/* Copyright (c) 2013 The University of Sheffield.
 * 
 * This file is part of StateChum.
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */
package statechum.analysis.learning;

import java.io.OutputStream;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;
import java.util.Map.Entry;
import java.util.TreeSet;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import statechum.Configuration;
import statechum.Label;
import statechum.StatechumXML;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.StatechumXML.SequenceIO;
import statechum.analysis.learning.observers.LearnerSimulator;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

public class PairOfPaths
{
	List<Label> R,Q;
	
	public List<Label> getQ()
	{
		return Q;
	}
	
	public List<Label> getR()
	{
		return R;
	}

	
	public static final String pairElement = "pair", pairCollectionElement="collection";
	
	public void WritePair(Element top, SequenceIO<statechum.Label> labelio)
	{
		Collection<List<Label>> elements = new LinkedList<List<Label>>();elements.add(R);if (Q != null) elements.add(Q);
		Element sequences = labelio.writeSequenceList(pairElement, elements);
		top.appendChild(sequences);		
	}
	
    public static void writePairs(List<PairOfPaths> pairs, Configuration configuration, OutputStream outputStream)
    {
    	DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		try
		{
			factory.setFeature(javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			Document doc = factory.newDocumentBuilder().newDocument();
			SequenceIO<statechum.Label> labelio = new StatechumXML.LabelSequenceWriter(doc,configuration, null);
			Element top = doc.createElement(pairCollectionElement);doc.appendChild(top);top.appendChild(AbstractPersistence.endl(doc));
			for(PairOfPaths p:pairs)
				p.WritePair(top,labelio);
			Transformer trans = TransformerFactory.newInstance().newTransformer();
			trans.transform(new DOMSource(doc),new StreamResult(outputStream));
		}
		catch(Exception e)
		{
			statechum.Helper.throwUnchecked("failed to write XML",e);
		}
    }

    public static List<PairOfPaths> readPairs(Reader inStream, Configuration configuration, ConvertALabel conv)
    {
    	List<PairOfPaths> outcome = new ArrayList<PairOfPaths>();
    	Document doc = LearnerSimulator.getDocumentOfXML(inStream);
    	NodeList nlist = doc.getChildNodes();
    	if (nlist.getLength() != 1)
    		throw new IllegalArgumentException("should have at most 1 child");
    	if (!nlist.item(0).getNodeName().equals(pairCollectionElement))
    		throw new IllegalArgumentException("invalid child element");
    	NodeList childElements = nlist.item(0).getChildNodes();
    	int length = childElements.getLength();
    	SequenceIO<statechum.Label> labelio = new StatechumXML.LabelSequenceWriter(doc,configuration, conv);
    	for(int i=0;i<length;++i)
    	{
    		if (childElements.item(i) instanceof Element)
    		{
        		Element n = (Element)childElements.item(i);
        		List<List<Label>> p = labelio.readSequenceList(n,pairElement);
        		PairOfPaths pair = new PairOfPaths();
        		assert p.size() > 0 && p.size() <= 2;
        		pair.R=p.get(0);
        		if (p.size() > 1)
        			pair.Q=p.get(1);
        		outcome.add(pair);
    		}
    	}
    	return outcome;
    }
    
	private PairOfPaths()
	{}
	
	/** Computes shortest paths from the initial state to the supplied states.
	 * If one of the components of the pair is null, it is used to indicate a single state rather than a pair. Used to record which red states have been chosen and verify that a particular pair represents the red state and not a pair to be selected.
	 */
	public PairOfPaths(LearnerGraph coregraph,PairScore pair)
	{
		Set<CmpVertex> statesOfInterest = new TreeSet<CmpVertex>();statesOfInterest.add(pair.getR());if (pair.getQ() != null) statesOfInterest.add(pair.getQ());
		Map<CmpVertex,LinkedList<Label>> stateToPath = convertSetOfStatesToPaths(coregraph, statesOfInterest);
		if (stateToPath == null)
			throw new IllegalArgumentException("failed to find paths to the supplied pair "+pair);// we should not reach here because the two states should exist in the graph
		if (pair.getQ() != null) Q=stateToPath.get(pair.getQ());
		R=stateToPath.get(pair.getR());
	}

	/** Given a collection of states, computes paths in the automaton from the initial state to states in the set.
	 * Does not use state cover computation for memory efficiency.
	 */
	public static Map<CmpVertex,LinkedList<Label>> convertSetOfStatesToPaths(LearnerGraph coregraph, Set<CmpVertex> statesOfInterestArg)
	{
		Map<CmpVertex,LinkedList<Label>> stateToPath = new HashMap<CmpVertex,LinkedList<Label>>();// not many of these hence not a HashSetWithSearch
		stateToPath.put(coregraph.getInit(), new LinkedList<Label>());
		
		Queue<CmpVertex> fringe = new LinkedList<CmpVertex>();
		Set<CmpVertex> statesInFringe = new HashSet<CmpVertex>();// in order not to iterate through the list all the time.
		fringe.add(coregraph.getInit());statesInFringe.add(coregraph.getInit());
		Set<CmpVertex> statesOfInterest = new TreeSet<CmpVertex>(statesOfInterestArg);// make a copy of the set, otherwise we might modify something like a keyset of our coregraph and mess up both the graph and the traversal process.
		int pathsLeft=statesOfInterest.size();
		while(!fringe.isEmpty())
		{
			CmpVertex currentState = fringe.remove();
			LinkedList<Label> currentPath = stateToPath.get(currentState);
			if (statesOfInterest.contains(currentState))
			{
				pathsLeft--;
				statesOfInterest.remove(currentState);
				if (pathsLeft <= 0)
					break;// finished
			}
			
			Map<Label,CmpVertex> targets = coregraph.transitionMatrix.get(currentState);
			if(targets != null && !targets.isEmpty())
				for(Entry<Label,CmpVertex> labelstate:targets.entrySet())
				{
					CmpVertex target = labelstate.getValue();
					if (!statesInFringe.contains(target))
					{
						@SuppressWarnings("unchecked")
						LinkedList<Label> newPath = (LinkedList<Label>)currentPath.clone();newPath.add(labelstate.getKey());
						stateToPath.put(target, newPath);
						fringe.offer(target);
						statesInFringe.add(target);

					}
				}
		}

		if (pathsLeft > 0)
			return null;//throw new IllegalArgumentException("failed to find paths to the supplied stack "+stack);// we should not reach here because all states should exist in the graph
		
		return stateToPath;
	}
	
	/** Given a stack of pairs of states, converts it to a stack of pairs of paths to those states. This is useful where state labelling may change but we 
	 * need to match pairs of states computed by {@link Learner#ChooseStatePairs(LearnerGraph)}. 
	 * 
	 * @param coregraph the graph in which to look for paths
	 * @param stack stack of pairs of states
	 * @return stack of pairs of paths
	 */
	public static Stack<PairOfPaths> convertStack(LearnerGraph coregraph,Stack<PairScore> stack)
	{
		Stack<PairOfPaths> outcome = new Stack<PairOfPaths>();
		if (!stack.isEmpty())
		{
			
			Set<CmpVertex> statesOfInterest = new HashSet<CmpVertex>();
			for(PairScore pair:stack)
			{
				statesOfInterest.add(pair.getQ());statesOfInterest.add(pair.getR());
			}
			
			Map<CmpVertex,LinkedList<Label>> stateToPath = convertSetOfStatesToPaths(coregraph, statesOfInterest);
			// Now populate the stack to return
			for(int i=0;i<stack.size();++i)
			{
				PairOfPaths pair = new PairOfPaths();pair.Q = stateToPath.get(stack.get(i).getQ());pair.R = stateToPath.get(stack.get(i).getR());
				assert pair.Q != null && pair.R != null;
				outcome.add(pair);
			}
		}
		return outcome;
	}
	
	/** Given a collection of pairs generated by chooseStatePairs, this method checks that the pair stored in this pair class is one of those in the list and has the best score.
	 * If this fails, throws an exception, otherwise adds this paper to the top of the stack.
	 * @param graph graph to process
	 * @param stack stack of values to go through, will destroy it completely and then add a single value.
	 */
	public void rebuildStack(LearnerGraph graph, Stack<PairScore> stack)
	{
		PairScore p=new PairScore(graph.getVertex(Q),graph.getVertex(R),0,0);
		if (p.firstElem == null || p.secondElem == null)
			throw new IllegalArgumentException("pair not found in graph");
		if (stack.isEmpty())
			throw new IllegalArgumentException("pair not found in stack or is not top in score");
		PairScore top = stack.pop();
		long topScore = top.getScore();
		while(top.getScore() == topScore && (top.getQ() != p.firstElem || top.getR() != p.secondElem) && !stack.isEmpty())
			top = stack.pop();// will throw if our pair is not found.
		if (top.getQ() != p.firstElem || top.getR() != p.secondElem || top.getScore() != topScore)
			throw new IllegalArgumentException("pair not found in stack or is not top in score");
		
		stack.clear();stack.push(top);
	}

}