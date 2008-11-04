/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.analysis.learning.rpnicore;

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.io.GraphMLFile;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.StatePair;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSetAutomaton;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

public class Transform {
	final LearnerGraph coregraph;
	
	/** Associates this object to ComputeStateScores it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	Transform(LearnerGraph g)
	{
		coregraph =g;
	}
	
	public static int HammingDistance(List<Boolean> A, List<Boolean> B)
	{
		if (A.size() != B.size())
			throw new IllegalArgumentException("sequences of different length passed");
		int distance = 0;
		Iterator<Boolean> A_Iter = A.iterator(), B_Iter = B.iterator();
		while(A_Iter.hasNext())
		{
			if (!A_Iter.next().equals(B_Iter.next())) ++distance;
		}
		return distance;
	}
	
	/** a marker for an initial state in a graphML file. */
	public static final String Initial = "Initial";
	
	/** Returns the ID of the node, prepending Initial as appropriate for the initial state. */
	protected String transformNodeName(CmpVertex node)
	{
		return (node == coregraph.init? Initial+" ":"")+node.getID().toString(); 
	}

	/** Writes a graph into a graphML file. All vertices are written. */
	public void writeGraphML(String name) throws IOException
	{
		FileWriter writer = new FileWriter(name);writeGraphML(writer);writer.close();
	}
	
	/** Graphml namespace */
	protected static final String graphmlNS="gml";
	
	/** Graphml top-level node tag. */
	public static final String graphmlNodeName = graphmlNS+":graphml";
	
	/** Graphml uri */
	protected static final String graphlmURI="http://graphml.graphdrawing.org/xmlns/graphml";	
	
	protected Element createStateNode(Document doc, CmpVertex node)
	{
		if (node.getID().toString().contains(Initial))
			throw new IllegalArgumentException("Invalid node name "+node);
		Element nodeElement = doc.createElementNS(graphmlNS,"node");
		nodeElement.setAttribute("id",node.getID().toString());
		nodeElement.setIdAttribute("id", true);
		nodeElement.setAttribute("VERTEX", transformNodeName(node));
		if (!node.isAccept()) nodeElement.setAttribute(JUConstants.ACCEPTED.name(),Boolean.toString(node.isAccept()));
		if (node.isHighlight()) nodeElement.setAttribute(JUConstants.HIGHLIGHT.name(),Boolean.toString(node.isHighlight()));
		if (node.getColour() != null) nodeElement.setAttribute(JUConstants.COLOUR.name(),node.getColour().name());
		return nodeElement;
	}
	
	public static Text endl(Document doc)
	{
		return doc.createTextNode("\n");		
	}
	
	public Element createGraphMLNode(Document doc)
	{
		Element graphElement = doc.createElementNS(graphlmURI,graphmlNodeName);
		Element graphTop = doc.createElementNS(graphmlNS,"graph");
		//graphElement.setAttributeNodeNS(doc.createAttributeNS("http://graphml.graphdrawing.org/xmlns/graphml", "gml:aaaschemaLocation"));
		graphTop.setAttribute("edgedefault", "directed");graphElement.appendChild(graphTop);
		graphTop.appendChild(endl(doc));
		for(Entry<CmpVertex,Map<String,CmpVertex>> vert:coregraph.transitionMatrix.entrySet())
		{
			graphTop.appendChild(createStateNode(doc, vert.getKey()));graphTop.appendChild(endl(doc));
		}
		for(Entry<CmpVertex,Map<String,CmpVertex>> vert:coregraph.transitionMatrix.entrySet())
			for(Entry<String,CmpVertex> transition:vert.getValue().entrySet())
			{
				Element edge = doc.createElementNS(graphmlNS,"edge");edge.setAttribute("source", vert.getKey().getID().toString());
				edge.setAttribute("target", transition.getValue().getID().toString());edge.setAttribute("directed", "true");
				edge.setAttribute("EDGE", transition.getKey());graphTop.appendChild(edge);
				graphTop.appendChild(endl(doc));
			}
		return graphElement;
	}
	
	/** Writes a graph into a graphML file. All vertices are written.
	 * 
	 * @throws IOException if an I/O error occurs or 
	 * any vertex has a substring "Initial" in it, because this substring is used to designate 
	 * an initial state in the graphml file. Most of the time, "Init" is used instead in the graphs.
	 * @throws ParserConfigurationException 
	 */
	public void writeGraphML(Writer writer) throws IOException
	{
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		Document doc = null;
		try
		{
			factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();doc.setXmlStandalone(true);
		}
		catch(ParserConfigurationException ex)
		{
			IOException parserEx = new IOException("configuration exception: "+ex);parserEx.initCause(ex);throw parserEx;
		}
		doc.appendChild(createGraphMLNode(doc));
		// based on http://www.exampledepot.com/egs/javax.xml.transform/WriteDom.html
		try {
			Transformer trans = TransformerFactory.newInstance().newTransformer();
			trans.transform(new DOMSource(doc),new StreamResult(writer));
		} catch (Exception e) {
			IOException ex = new IOException("failed to write out XML "+e);ex.initCause(e);
			throw ex;
		}
	}
	
	static class DOMExperimentGraphMLHandler extends ExperimentGraphMLHandler
	{
	    @Override
		public Graph getGraph() {
	        return super.getGraph();
	    }
		
	}

	/** Converts DOM collection of attributes to the SAX one.
	 * @param namedMap what to convert
	 * @return the SAX collection, ready to be passed to a SAX listener. 
	 */
	static protected Attributes Attributes_DOM_to_SAX(NamedNodeMap namedMap)
	{
		AttributesImpl collection = new AttributesImpl();
		if (namedMap != null)
			for(int i=0;i<namedMap.getLength();++i) 
			{
				org.w3c.dom.Node node = namedMap.item(i);
				collection.addAttribute(node.getNamespaceURI(), node.getLocalName(), node.getNodeName(), "attribute", node.getNodeValue());
			}
		return collection;
	}
	
	/** Given a node in a document, loads a graph from this node. 
	 * @param elem the graphml element to load 
	 */
	public static Graph loadGraph(Element elem)
	{
		if (!elem.getNodeName().equals(Transform.graphmlNodeName))
			throw new IllegalArgumentException("element does not start with graphml");
		NodeList graphs = elem.getElementsByTagName("graph");
		if (graphs.getLength() < 1)
			throw new IllegalArgumentException("absent graph element");
		if (graphs.getLength() > 1)
			throw new IllegalArgumentException("duplicate graph element");
		Element graphElement = (Element)graphs.item(0);

		DOMExperimentGraphMLHandler graphHandler = new DOMExperimentGraphMLHandler();
    	GraphMLFile graphmlFile = new GraphMLFile();
    	graphmlFile.setGraphMLFileHandler(graphHandler);
    	synchronized(LearnerGraph.syncObj)
    	{// multi-core execution understandably fails if I forget to sync on that object
	    	try
	    	{
		    	graphHandler.startElement(graphElement.getNamespaceURI(), graphElement.getLocalName(), graphElement.getNodeName(), Attributes_DOM_to_SAX(graphElement.getAttributes())); // so as to applease the lack of any clue Jung has about graphml namespaces
		    	NodeList nodes = graphElement.getChildNodes(); 
		    	for(int i=0;i<nodes.getLength();++i)
		    	{
					org.w3c.dom.Node node = nodes.item(i);
					if (node.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE)
						graphHandler.startElement(node.getNamespaceURI(), node.getLocalName(), node.getNodeName(), Attributes_DOM_to_SAX(node.getAttributes()));
		    	}
	    	}
	    	catch(SAXException e)
	    	{
	    		IllegalArgumentException ex = new IllegalArgumentException("failed to write out XML "+e);ex.initCause(e);
	    		throw ex;
	    	}
    	}
    	return graphHandler.getGraph();
	}
	
	/** Returns a state, randomly chosen according to the supplied random number generator. */
	public static CmpVertex pickRandomState(LearnerGraph g, Random rnd)
	{
		int nr = rnd.nextInt(g.transitionMatrix.size());
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:g.transitionMatrix.entrySet())
			if (nr-- == 0)
				return entry.getKey();
		
		throw new IllegalArgumentException("something wrong with the graph - expected state was not found");
	}

	/** Adds a reject transition to a randomly-chosen state, if possible (the chosen state has an input not in the current alphabet). */
	public void addRejectStateRandomly(Random rnd)
	{
		CmpVertex v =pickRandomState(coregraph, rnd);
		HashSet<String> possibilities = new HashSet<String>();possibilities.addAll(coregraph.learnerCache.getAlphabet());
		possibilities.removeAll(coregraph.transitionMatrix.get(v).keySet());
		Iterator<String> inputIt = possibilities.iterator();
		if (inputIt.hasNext())
		{
			CmpVertex newVertex = LearnerGraph.generateNewCmpVertex(coregraph.nextID(false), coregraph.config);
			newVertex.setAccept(false);
			coregraph.transitionMatrix.put(newVertex, new TreeMap<String,CmpVertex>());
			coregraph.transitionMatrix.get(v).put(inputIt.next(),newVertex);
		}
	}
	
	/** 
	 * Relabels graph, keeping NrToKeep original labels. All new ones are generated with
	 * prefix PrefixNew.
	 * 
	 * @param g graph to transform.
	 * @param NrToKeep number of labels to keep.
	 * @param PrefixNew prefix of new labels.
	 * @throws IllegalArgumentException if PrefixNew is a prefix of an existing vertex. The graph supplied is destroyed in this case.
	 */
	public static void relabel(LearnerGraph g, int NrToKeep, String PrefixNew)
	{
		Map<String,String> fromTo = new TreeMap<String,String>();
		int newLabelCnt = 0;
		TreeMap<CmpVertex,Map<String,CmpVertex>> newMatrix = new TreeMap<CmpVertex,Map<String,CmpVertex>>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:g.transitionMatrix.entrySet())
		{
			Map<String,CmpVertex> newRow = new TreeMap<String,CmpVertex>();
			for(Entry<String,CmpVertex> transition:entry.getValue().entrySet())
			{
				if (NrToKeep > 0 && !fromTo.containsKey(transition.getKey()))
				{
					NrToKeep--;fromTo.put(transition.getKey(),transition.getKey());// keep the label and reduce the counter.
				}
				else
					if (!fromTo.containsKey(transition.getKey()))
					{
						if(transition.getKey().startsWith(PrefixNew))
							throw new IllegalArgumentException("there is already a transition with prefix "+PrefixNew+" in the supplied graph");
						fromTo.put(transition.getKey(), PrefixNew+newLabelCnt++);
					}
				newRow.put(fromTo.get(transition.getKey()), transition.getValue());
			}
			newMatrix.put(entry.getKey(), newRow);
		}
		g.transitionMatrix = newMatrix;g.learnerCache.invalidate();
	}
	
	/** Adds all states and transitions from graph <em>what</em> to graph <em>g</em>.
	 * Very useful for renumbering nodes on graphs loaded from GraphML and such, because
	 * numerical node IDs can be useful. The current implementation does not require this
	 * because it is easy to build a map from existing identifiers to numbers.
	 * <em>WMethod.buildStateToIntegerMap()</em> does exactly this and the outcome is cached
	 * and used by <em>vertexToInt</em> and <em>vertexToIntNR</em>.
	 * <p>
	 * An example of using this method to renumber vertices is shown below:
	 * <pre>
	 * LearnerGraph grTmp = new LearnerGraph(g.config);
	 * CmpVertex newInit = addToGraph(grTmp,g);StatePair whatToMerge = new StatePair(grTmp.init,newInit);
	 * LinkedList<Collection<CmpVertex>> collectionOfVerticesToMerge = new LinkedList<Collection<CmpVertex>>();
	 * grTmp.pairscores.computePairCompatibilityScore_general(whatToMerge,collectionOfVerticesToMerge);
	 * LearnerGraph result = MergeStates.mergeAndDeterminize_general(grTmp, whatToMerge,collectionOfVerticesToMerge);
	 * WMethod.computeWSet(result);
	 * </pre>
	 * @param g target into which to merge what
	 * @param what graph to merge into g.
	 * @param whatToG maps original vertices to those included in the graph <em>g</em>.
	 * @return vertex in g corresponding to the initial vertex in what 
	 */ 
	public static CmpVertex addToGraph(LearnerGraph g, LearnerGraph what,Map<CmpVertex,CmpVertex> whatToG)
	{
		if (whatToG == null) whatToG = new TreeMap<CmpVertex,CmpVertex>();else whatToG.clear();
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:what.transitionMatrix.entrySet())
		{// The idea is to number the new states rather than to clone vertices.
		 // This way, new states get numerical IDs rather than retain the original (potentially text) IDs.
			CmpVertex newVert = g.transform.copyVertexUnderDifferentName(entry.getKey());
			whatToG.put(entry.getKey(),newVert);
		}

		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:what.transitionMatrix.entrySet())
		{
			Map<String,CmpVertex> row = new TreeMap<String,CmpVertex>();g.transitionMatrix.put(whatToG.get(entry.getKey()),row);
			for(Entry<String,CmpVertex> transition:entry.getValue().entrySet())
				row.put(transition.getKey(), whatToG.get(transition.getValue()));
		}
		g.learnerCache.invalidate();
		return whatToG.get(what.init);
	}

	/** In many cases we have a vertex from some graph we'd like to add to another graph, but we cannot
	 * just do a clone because we may encounter a name clash. This method takes care of this by creating 
	 * an entirely new vertex and copies all the attributes of an existing vertex to the new one.
	 * 
	 * @param what what to add to the current graph
	 * @return the newly-added vertex.
	 */
	public CmpVertex copyVertexUnderDifferentName(CmpVertex what)
	{
		CmpVertex newVert = LearnerGraph.generateNewCmpVertex(coregraph.nextID(what.isAccept()), coregraph.config);
		if (coregraph.findVertex(newVert.getID()) != null) throw new IllegalArgumentException("duplicate vertex with ID "+newVert.getID()+" in graph "+coregraph);
		assert !coregraph.transitionMatrix.containsKey(newVert) : "duplicate vertex "+newVert;
		newVert.setAccept(what.isAccept());
		newVert.setHighlight(what.isHighlight());
		newVert.setColour(what.getColour());
		coregraph.transitionMatrix.put(newVert,new TreeMap<String,CmpVertex>());
		return newVert;
	}
	
	/** Changes states labels on a graph to their numerical equivalents.
	 * 
	 * @param what graph to convert
	 * @return result of conversion.
	 */
	public static LearnerGraph convertToNumerical(LearnerGraph what)
	{
		LearnerGraph result = new LearnerGraph(what.config);result.init = null;result.transitionMatrix.clear();
		result.init = addToGraph(result, what, null);if (what.getName() != null) result.setName(what.getName());
		return result;
	}

	public static void addToGraph_tmp(LearnerGraph g, List<String> initPath, LearnerGraph what)
	{
		PTASequenceEngine engine = new PTASequenceEngine();
		engine.init(new PTASequenceSetAutomaton());		
		SequenceSet initSet = engine.new SequenceSet();initSet.setIdentity();initSet.crossWithSequence(initPath);
		
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:what.transitionMatrix.entrySet())
		{// for all states in our new machine, compute paths to them.
			SequenceSet result = engine.new SequenceSet();
			what.paths.computePathsSBetween(what.init, entry.getKey(), initSet, result);
			result.crossWithSet(entry.getValue().keySet());
		}
		
		// Now engine contains a sort of a transition cover of "what", except that only 
		// transitions present in "what" will be covered. All sequences in this set have
		// initPath prepended to them, so that I can merge the result into g directly.
		g.paths.augmentPTA(engine);
	}
	
	/** Inverts states' acceptance conditions. */
	public void invertStates()
	{
		for(CmpVertex vertex:coregraph.transitionMatrix.keySet())
			vertex.setAccept(!vertex.isAccept());
	}
	
	/** Useful where we aim to check that the learnt machine is the same as 
	 * original. To prevent erroneous mergers, negative information is supplied,
	 * which is incorporated into the final machine. This way, even if the
	 * original machine does not have reject-states, the outcome of merging
	 * will have them. Transitions to those negative states are obviously only
	 * added where there are no transitions in the original one, so if we take 
	 * the original machine and add transitions from all states to reject states 
	 * for undefined inputs (called <em>completeGraph()</em>), the outcome 
	 * of learning will have a subset of transitions to reject-states.
	 *<p>
	 * Throws {@link IllegalArgumentException} if the initial state points to a reject-state. 
	 * This makes sure that the outcome is never an empty graph.
	 * 
	 * @param what an automaton which states are to be removed.
	 * @param config this method makes a copy of an automaton first, hence a configuration is needed.
	 * @return an automaton reduced in the described way.
	 */
	public static LearnerGraph removeRejectStates(LearnerGraph what,Configuration config)
	{
		if (!what.init.isAccept()) throw new IllegalArgumentException("initial state cannot be a reject-state");
		LearnerGraph result = what.copy(config);
		
		// Since we'd like to modify a transition matrix, we iterate through states of the original machine and modify the result.
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:what.transitionMatrix.entrySet())
			if (!entry.getKey().isAccept()) result.transitionMatrix.remove(entry.getKey());// a copied state should be identical to the original one, so doing remove is appropriate 
			else
			{
				Map<String,CmpVertex> row = result.transitionMatrix.get(entry.getKey());
				for(Entry<String,CmpVertex> target:entry.getValue().entrySet())
					if (!target.getValue().isAccept()) row.remove(target.getKey());
			}
		
		return result;
	}
	
	/** Computes an alphabet of a given graph and adds transitions to a 
	 * reject state from all states A and inputs a from which there is no B such that A-a->B
	 * (A-a-#REJECT) gets added. Note: (1) such transitions are even added to reject vertices.
	 * (2) if such a vertex already exists, an IllegalArgumentException is thown.
	 * 
	 * @param reject the name of the reject state, to be added to the graph. No transitions are added from this state.
	 * @return true if any transitions have been added
	 */   
	public boolean completeGraph(VertexID reject)
	{
		if (coregraph.findVertex(reject) != null)
			throw new IllegalArgumentException("reject vertex named "+reject+" already exists");
		
		CmpVertex rejectVertex = null;
		
		// first pass - computing an alphabet
		Set<String> alphabet = coregraph.wmethod.computeAlphabet();
		
		// second pass - checking if any transitions need to be added and adding them.
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
		{
			Set<String> labelsToRejectState = new HashSet<String>();
			labelsToRejectState.addAll(alphabet);labelsToRejectState.removeAll(entry.getValue().keySet());
			if (!labelsToRejectState.isEmpty())
			{
				if (rejectVertex == null)
				{
					rejectVertex = LearnerGraph.generateNewCmpVertex(reject,coregraph.config);rejectVertex.setAccept(false);
				}
				Map<String,CmpVertex> row = entry.getValue();
				for(String rejLabel:labelsToRejectState)
					row.put(rejLabel, rejectVertex);
			}
		}

		if (rejectVertex != null)
			coregraph.transitionMatrix.put(rejectVertex,new TreeMap<String,CmpVertex>());
		
		coregraph.learnerCache.invalidate();
		return rejectVertex != null;
	}

	/** Given a state and a W set, computes a map from those sequences to booleans representing
	 * whether those sequences to true/false depending whether a specific can be followed from
	 * the given state. 
	 * @param state the state to compute applicability of paths from
	 * @param wSet the set of sequences to manipulate
	 * @return a list of booleans representing applicability of sequences.
	 */
	public static List<Boolean> wToBooleans(LearnerGraph g, CmpVertex state, Collection<List<String>> wSet)
	{
		List<Boolean> result = new LinkedList<Boolean>();
		for(List<String> seq:wSet)
			result.add(g.paths.tracePath(seq,state) == AbstractOracle.USER_ACCEPTED);
		return result;
	}
	
	Set<CmpVertex> fragileStates = new HashSet<CmpVertex>();
	
	/** Computes Hamming distances between elements of a W set and outputs 
	 * the description of the results, potentially including a lot of statistical information. 
	 * 
	 * @param produceStatistics whether to output lots of details.
	 * @return Hamming distances between vectors corresponding to responses of states 
	 * of our graph to sequences in the W set.
	 */
	public String ComputeHamming(boolean produceStatistics)
	{
		List<List<String>> wSet = new LinkedList<List<String>>();wSet.addAll(WMethod.computeWSet_reducedmemory(coregraph));
		Map<CmpVertex,List<Boolean>> bitVector = new TreeMap<CmpVertex,List<Boolean>>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> state:coregraph.transitionMatrix.entrySet())
			bitVector.put(state.getKey(),wToBooleans(coregraph,state.getKey(), wSet));
		int min=Integer.MAX_VALUE,max=0;double average = 0;
		Map<Integer,AtomicInteger> statistics = new HashMap<Integer,AtomicInteger>();
		Object stateToBitVector[] = bitVector.entrySet().toArray();
		for(int i=0;i< stateToBitVector.length;++i)
			for(int j=i+1;j<stateToBitVector.length;++j)
			{
				Entry<CmpVertex,List<Boolean>> vecI = (Entry<CmpVertex,List<Boolean>>) stateToBitVector[i],vecJ = (Entry<CmpVertex,List<Boolean>>)stateToBitVector[j];
				int h = HammingDistance(vecI.getValue(), vecJ.getValue());
				average+=h;
				if (min > h) min = h;
				if (max < h) max = h;
				AtomicInteger atomicH = statistics.get(h);if (atomicH == null) { atomicH = new AtomicInteger(1);statistics.put(h, atomicH); } else atomicH.addAndGet(1);
				if (h == 1) fragileStates.add(vecI.getKey());
			}
		String result =" ONE:"+statistics.get(1)+" ";
		for(Entry<Integer,AtomicInteger> pair:statistics.entrySet()) result+=" "+pair.getKey()+":"+pair.getValue();
		result+="\n";
		for(CmpVertex fragile:fragileStates) result+=" "+fragile;result+="\n";
		int counter =  bitVector.size()*(bitVector.size()-1)/2;
		String basicInfo = "Hamming distances min: "+min+" max: "+max;
		result+="\n"+basicInfo+" average: "+(average/counter);
		return produceStatistics?result:basicInfo;
	}

	/** Takes a graph and outputs vectors corresponding to responses of states 
	 * of our graph to sequences in the W set.
	 * 
	 * @param g the graph which states to examine
	 * @param wSet the W set to compute the response of g to.
	 */
	public static String getVectors(LearnerGraph g, Collection<List<String>> wSet)
	{
		String result = "";
		for(Entry<CmpVertex,Map<String,CmpVertex>> state:g.transitionMatrix.entrySet())
			result+="\n"+wToBooleans(g,state.getKey(), wSet);
		result+="\n";
		return result;
	}
	
	/** A graph may be completely random or built with a specific distribution of labels in mind.
	 * The likelyhood that a label will be used on a transition from some state is referred to
	 * as a <em>fill factor</em>. You can think of a graph as a glass which is filled with 
	 * transitions, but the idea is to distribute them such that each state has the same number 
	 * of outgoing and incoming transitions. This is the reason why populating a graph is compared to
	 * filling a glass with water - transitions are added in "levels".<p>
	 * A W set may consist of singleton sequences (a very common case), in this case we may compute
	 * a restriction of g to a subset of alphabet, using only inputs used in W. This function 
	 * computes a fill factor of the resulting graph - even if the original graph was built well,
	 * the fill factor of the considered subgraph may happen to be rather different.
	 *  
	 * @param g the graph to restrict
	 * @param wSet a collection of singleton sequences to restrict g to.
	 * @return the fill factor of the restriction.
	 */
	public static double getEffectiveFillRate(LearnerGraph g, Collection<List<String>> wSet)
	{
		int positives=0;
		for(Entry<CmpVertex,Map<String,CmpVertex>> state:g.transitionMatrix.entrySet())
			for(Boolean b:wToBooleans(g,state.getKey(), wSet))
				if (b.booleanValue()) ++positives;
		return ((double)positives)/(g.getStateNumber()*wSet.size());
	}
	
	// grep SUCCESS *|awk -F, 'BEGIN {a=0;count=0;} { a+=$3;++count;} END{print a/count;}'
	
	/** Adds all possible transitions to a graph and computes the likelyhood that the original
	 * W will not work in a modified graph. The implementation only adds transitions 
	 * with labels from the W set (all the other labels will not cause a W set to fail
	 * to distinguish states) and only adds loopback transitions in each state (target
	 * state does not affect applicability of W consisting of singleton sequences which
	 * is the only case considered below and an exception will be thrown if W fails to 
	 * consist of singletons).
	 * 
	 *  @return description of the results.
	 */ 
	public String checkWChanged()
	{
		String result = "";
		Collection<List<String>> wSet = WMethod.computeWSet_reducedmemory(coregraph);
		Set<String> Walphabet = new HashSet<String>();
		for(List<String> wSeq:wSet)
		{
			if (wSeq.size() != 1)
				throw new IllegalArgumentException("non-singleton W");
			Walphabet.add(wSeq.iterator().next());
		}
		Collection<String> alphabet = coregraph.wmethod.computeAlphabet();
		double fillFactor = getEffectiveFillRate(coregraph, wSet);//transitionsFromEveryState/alphabet.size();
		result+=getVectors(coregraph, wSet);
		double average = (1-fillFactor)*wSet.size()*coregraph.getStateNumber();
		int changeNumber = 0, total =0;
		Map<String,AtomicInteger> labelUsage = new HashMap<String,AtomicInteger>();for(String l:alphabet) labelUsage.put(l, new AtomicInteger());
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
		{
			Collection<String> newLabels = new HashSet<String>();newLabels.addAll(Walphabet);newLabels.removeAll(entry.getValue().keySet());
			int changesForThisState = 0;
			
			for(String lbl:entry.getValue().keySet()) labelUsage.get(lbl).addAndGet(1);
			
			for(String label:newLabels)
			{
				LearnerGraph newGraph = coregraph.copy(coregraph.config);
				CmpVertex currState = newGraph.findVertex(entry.getKey().getID());
				newGraph.transitionMatrix.get(currState).put(label, currState);
				String description = newGraph.wmethod.checkW_is_corrent_boolean(wSet);
				boolean changed = (description != null);
/*
				for(Entry<CmpVertex,Map<String,CmpVertex>> state:graph.transitionMatrix.entrySet())
				{
					LearnerGraph aGraph = graph.copy(graph.config);
					CmpVertex aState = aGraph.findVertex(entry.getKey().getName());
					aGraph.transitionMatrix.get(aState).put(label, aGraph.findVertex(state.getKey().getName()));
					if (changed != (aGraph.wmethod.checkW_is_corrent_boolean(wSet) != null))
						throw new IllegalArgumentException("inconsistent W set results");
				}
*/
				if (changed)
					++changesForThisState;
				++total;
			}
			changeNumber+=changesForThisState;
			result+="changes for "+entry.getKey().getID().toString()+" "+changesForThisState+" (max "+newLabels.size()+"), max for add/remove is "+Walphabet.size()+"\n";
		}
		double stateNumber = coregraph.getStateNumber();
		double wsize = wSet.size();
		double expectedNrOfChanges = wsize*2*fillFactor*(1-fillFactor)*Math.pow(fillFactor*fillFactor+(1-fillFactor)*(1-fillFactor), wsize-1)*
			stateNumber*(stateNumber-1)/2;
		result+="Distribution of labels: ";for(Entry<String,AtomicInteger> en:labelUsage.entrySet()) result+=" "+en.getValue();result+="\n";
		result+="Distribution of elements of W: ";for(String wElem:Walphabet) result+=" "+labelUsage.get(wElem);result+="\n";
		return Math.abs(expectedNrOfChanges-changeNumber)/changeNumber+"\n"+result+"W size: "+wSet.size()+" W changes: "+changeNumber+ " out of "+total+" (expected "+average+"), \nfill factor is "+fillFactor+"\n "+
			"Expected number of changes is: "+expectedNrOfChanges
		;
	}
	
	/** Given a tentative PTA and a maximal automaton, this method adds reject-traces from the maximal
	 * automaton to tentative PTA in such a way that no new positive paths are created. The exploration
	 * is performed by navigating a cross-product of states because loops in a tentative PTA may have
	 * to be unrolled before a reject-node can be added. Unrolling of loops may be disabled by setting
	 * unroll to false. Where the two automata contradict, an exception is thrown; it is possible to 
	 * override transitions in a tentative PTA by those from the maximal automaton if override is set
	 * to true, permitting <em>augmentFromMAX</em> to be used as a kind of AugmentMAX (<em>augmentPTA</em> 
	 * with <em>max</em> argument set to true). 
	 *   
	 * @param what tentative PTA to update
	 * @param from maximal automaton to update from
	 * @param override whether to replace parts of tentative PTA with those from maximal automaton if the two are in conflict.
	 * @param unroll whether to unroll loops - this produces are more accurate result at the expense of an increase of the state number in a tentative PTA.
	 * @param maxIsPartial whether a maximal automaton is partial, i.e. there may be sequences in our tentative PTA which are not reflected in a 
	 * maximal automaton. Given that the two are different graphs which may have different alphabets, it might be best to check for such an
	 * incompatibility at runtime here since such a case should never happen anyway (this would be so if our ltl2ba system did not produce a correct
	 * automaton). 
	 */
	public static void augmentFromMAX(LearnerGraph graph, LearnerGraph from, boolean override, boolean unroll, boolean maxIsPartial)
	{
		final Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
		final Map<StatePair,CmpVertex> pairsToGraphStates = new HashMap<StatePair,CmpVertex>();

		// Two sets are constructed so that I do not have to think about vertices which are shared between the two graphs regardless whether such a case is possible or not.
		final Set<CmpVertex> encounteredGraph = new HashSet<CmpVertex>(), encounteredMax = new HashSet<CmpVertex>();
		StatePair statePair = new StatePair(graph.init,from.init);
		currentExplorationBoundary.add(statePair);pairsToGraphStates.put(statePair, statePair.firstElem);
		encounteredGraph.add(statePair.firstElem);encounteredMax.add(statePair.secondElem);
		
		if (!statePair.firstElem.isAccept() && statePair.firstElem.isAccept())
			// tentative graph is reject but the maximal automaton is accept, nothing to do in this case
			return;
		
		if (statePair.firstElem.isAccept() != statePair.firstElem.isAccept())
		{// initial states are incompatible because the tentative automaton is accept and 
		 // the max automaton is reject (the other case handled above) 
		 // hence either override if possible and requested or throw.
			if (override)
			{
				graph.transitionMatrix.get(graph.init).clear();graph.init.setAccept(false);
			}
			else
				throw new IllegalArgumentException("incompatible labelling: maximal automaton is all-reject and tentative one is not");			
			
		}

		Map<String,CmpVertex> emptyTargets = new TreeMap<String,CmpVertex>();
		
		while(!currentExplorationBoundary.isEmpty())
		{
			statePair = currentExplorationBoundary.remove();
			assert statePair.secondElem == null || graph.transitionMatrix.containsKey(statePair.firstElem) : "state "+statePair.firstElem+" is not known to the first graph";
			assert from.transitionMatrix.containsKey(statePair.secondElem) : "state "+statePair.secondElem+" is not known to the second graph";
			assert statePair.firstElem.isAccept() == statePair.secondElem.isAccept() : "incompatible labelling of "+statePair;
						
			Map<String,CmpVertex> graphTargets = graph.transitionMatrix.get(statePair.firstElem), 
				maxTargets = statePair.secondElem == null? emptyTargets:from.transitionMatrix.get(statePair.secondElem);
				
			for(Entry<String,CmpVertex> labelstate:graphTargets.entrySet())
			{
				String label = labelstate.getKey();
				CmpVertex graphState = labelstate.getValue();// the original one
				CmpVertex maxState = maxTargets.get(label);

				if (!maxTargets.containsKey(label))
				{// this is the case where a transition in a tentative graph is not matched by any in a maximal automaton
					if (!maxIsPartial)
						throw new IllegalArgumentException("In state pair "+statePair+" transition labelled by "+label+" is not matched in a maximal automaton");
					maxState = null;
				}
				
				StatePair nextPair = new StatePair(graphState,maxState);
				// Now that we're making a step to a state pair where (graphState,maxState) pair has not been seen before,
				// it is quite possible that we have to clone the corresponding state in a tentative graph so as to ensure
				// that each state from a tentative graph is paired with no more than a single state in a maximal automaton 
				// (this corresponds to a construction of a cross-product of states).
				// A state of a tentative state can be unpaired if the maximal automaton is partial, 
				// i.e. it contains a number of counter-examples rather than all possible sequences. This is another
				// thing to check for in this method - if taking on LTL-derived graph this should be deemed an error.
				boolean shouldDescend = true;
				if (maxState != null && !unroll)
				{// if we have matched states, the decision to descend if not unrolling 
				 // loops depends on whether we've already seen a corresponding state in a max automaton
					shouldDescend = !encounteredMax.contains(maxState);
					if (shouldDescend) encounteredMax.add(maxState);// if not seen add it there
				}
				CmpVertex nextGraphVertex = pairsToGraphStates.get(nextPair);
				if (nextGraphVertex == null)
				{// not seen this pair already hence might have to clone.
					if (!encounteredGraph.contains(graphState))
					{// since we did not see this pair before, the first encountered vertex (graphState) is now a representative of the pair nextPair
						pairsToGraphStates.put(nextPair,graphState);
						nextGraphVertex = graphState;encounteredGraph.add(graphState);
					}
					else
					{// graphState already paired with one of the states in maximal automaton hence clone the state
						boolean accept = graphState.isAccept() && (maxState == null || maxState.isAccept());
						nextGraphVertex = LearnerGraph.generateNewCmpVertex(graph.nextID(accept), graph.config);
						if (graph.findVertex(nextGraphVertex.getID()) != null) throw new IllegalArgumentException("duplicate vertex with ID "+nextGraphVertex.getID()+" in graph "+graph);
						assert !graph.transitionMatrix.containsKey(nextGraphVertex) : "duplicate vertex "+nextGraphVertex;
						nextGraphVertex.setAccept(accept);
						nextGraphVertex.setHighlight(graphState.isHighlight());
						nextGraphVertex.setColour(graphState.getColour());
						graph.transitionMatrix.put(nextGraphVertex,new TreeMap<String,CmpVertex>());

						pairsToGraphStates.put(nextPair, nextGraphVertex);
						if (!unroll || !nextGraphVertex.isAccept()) shouldDescend = false;
					}
				}
				else
					shouldDescend = false;
				
				graphTargets.put(label,nextGraphVertex);// the only case when this is no-op is when we've reached the original representative (such as when loops are not unrolled).
				System.out.println(statePair+" - "+label+" -> "+nextPair+" ( represented by "+nextGraphVertex+" )");
				// Now proceed if we did not encounter this pair;
				// if not unrolling loops, we proceed if neither of the two states were met.
				if (shouldDescend)
				{// need to explore all transitions from the new state pair.
					currentExplorationBoundary.offer(nextPair);
					encounteredGraph.add(graphState);if (maxState != null) encounteredMax.add(maxState);
				}
			}
			
			for(Entry<String,CmpVertex> labelstate:maxTargets.entrySet())
			{
				String label = labelstate.getKey();
				if (!graphTargets.containsKey(label) && !labelstate.getValue().isAccept())
				{// a transition in a maximal automaton is not matched but leads to a reject-state hence direct to a reject-state adding it if necessary
					CmpVertex newVert = pairsToGraphStates.get(new StatePair(null,labelstate.getValue()));
					if (newVert == null)
					{
						newVert = graph.transform.copyVertexUnderDifferentName(labelstate.getValue());
						pairsToGraphStates.put(new StatePair(null,labelstate.getValue()), newVert);
					}
					graphTargets.put(label, newVert);
				}
			}
		}
	}

}
