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
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSetAutomaton;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

public class AddTransitions {
	private final LearnerGraph graph;
	
	public AddTransitions(LearnerGraph g)
	{
		graph =g;
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

	Map<CmpVertex,Map<CmpVertex,Integer>> vertexToNum = new HashMap<CmpVertex,Map<CmpVertex,Integer>>();
	
	public static void addAssignement(StringBuffer buffer, Integer A, Integer B, double value)
	{
		buffer.append("mat(");
		buffer.append(A);
		buffer.append(",");
		buffer.append(B);
		buffer.append(")=");buffer.append(value);buffer.append(";");
	}
	
	public final double valueK=0.9;
	
	public interface AddToMatrix
	{
		public void addMapping(Integer A, Integer B, double value);
	}
	
	public void addToBuffer(AddToMatrix resultReceiver, CmpVertex A, CmpVertex B)
	{
		Map<String,CmpVertex> rowB = graph.transitionMatrix.get(B);
		Set<String> outLabels = new HashSet<String>();outLabels.addAll(rowB.keySet());outLabels.addAll(graph.transitionMatrix.get(A).keySet());
		int outNumber = outLabels.size();
		Integer valueFirst = vertexToNum.get(B).get(A);if (valueFirst == null) valueFirst = vertexToNum.get(A).get(B);
		assert valueFirst != null;
		int matchedNumber = 0;
		Map<Integer,Double> targetCnt = new TreeMap<Integer,Double>();targetCnt.put(valueFirst, new Double(outNumber));
		for(Entry<String,CmpVertex> outLabel:graph.transitionMatrix.get(A).entrySet())
		{
			CmpVertex to = rowB.get(outLabel.getKey());
			if (to != null)
			{// matched pair of transitions
				++matchedNumber;
				Integer valueSecond = vertexToNum.get(outLabel.getValue()).get(to);if (valueSecond == null) valueSecond = vertexToNum.get(to).get(outLabel.getValue());
				assert valueSecond != null;
				Double current = targetCnt.get(valueSecond);
				if (current == null)
					targetCnt.put(valueSecond,-valueK);
				else
					targetCnt.put(valueSecond,current-valueK);
			}
		}

		for(Entry<Integer,Double> entry:targetCnt.entrySet())
			resultReceiver.addMapping(valueFirst,entry.getKey(),entry.getValue());
	}
	
	public void populatePairToNumber()
	{
		vertexToNum.clear();int num=0;
		for(Entry<CmpVertex,Map<String,CmpVertex>> entryA:graph.transitionMatrix.entrySet())
		{
			Map<CmpVertex,Integer> row = vertexToNum.get(entryA.getKey());
			if (row == null)
			{
				row = new TreeMap<CmpVertex,Integer>();vertexToNum.put(entryA.getKey(),row);
			}
			Iterator<Entry<CmpVertex,Map<String,CmpVertex>>> entryB_Iter=graph.transitionMatrix.entrySet().iterator();
			
			while(entryB_Iter.hasNext())
			{
				Entry<CmpVertex,Map<String,CmpVertex>> entryB = entryB_Iter.next();
				row.put(entryB.getKey(),++num);
				if (entryB == entryA) break;// only lower triangle of the matrix is processed.
			}
		}
	}
	
	public String toOctaveMatrix()
	{		
		final StringBuffer result = new StringBuffer();
		AddToMatrix resultAdder = new AddToMatrix()
		{
			public void addMapping(Integer A, Integer B, double value) {
				addAssignement(result, A, B, value);
			}
		};
		buildMatrix(resultAdder);
		return result.toString();
	}
	
	public void buildMatrix(AddToMatrix resultAdder)
	{		
		for(Entry<CmpVertex,Map<String,CmpVertex>> entryA:graph.transitionMatrix.entrySet())
			for(Entry<CmpVertex,Map<String,CmpVertex>> entryB:graph.transitionMatrix.entrySet())
				addToBuffer(resultAdder,entryA.getKey(),entryB.getKey());		
	}
	
	public static final String graphML_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns/graphml\"  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\nxsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns/graphml\">\n<graph edgedefault=\"directed\"/>\n";		

	protected static void writeNode(Writer writer, CmpVertex node, String namePrefix) throws IOException
	{
		writer.write("<node id=\""+node.getID().toString()+
				"\" VERTEX=\""+(namePrefix == null?"":namePrefix)+node.getID().toString()+"\"");
		if (!node.isAccept()) writer.write(" "+JUConstants.ACCEPTED+"="+node.isAccept());
		if (node.isHighlight()) writer.write(" "+JUConstants.HIGHLIGHT+"=true");
		writer.write("/>\n");
	}
	
	/** Writes a graph into a graphml file. All vertices are written. */
	public static void writeGraphML(LearnerGraph graph, String name) throws IOException
	{
		FileWriter writer = new FileWriter(name);
		writer.write(graphML_header);
		
		writeNode(writer, graph.init, "Initial ");
		for(Entry<CmpVertex,Map<String,CmpVertex>> vert:graph.transitionMatrix.entrySet())
			if (vert != graph.init)
				writeNode(writer, vert.getKey(), null);
		//<node id="1" VERTEX="Initial State 0" />
		
		//<edge source="21" target="19" directed="true" EDGE="a1" />
		for(Entry<CmpVertex,Map<String,CmpVertex>> vert:graph.transitionMatrix.entrySet())
			for(Entry<String,CmpVertex> transition:vert.getValue().entrySet())
				writer.write("<edge source=\""+vert.getKey().getID().toString()+
						"\" target=\""+transition.getValue().getID().toString()+
						"\" directed=\"true\" EDGE=\""+transition.getKey()+"\"/>\n");
		writer.write("</graph></graphml>\n");writer.close();
	}
	
	/** Returns a state, randomly chosen according to the supplied random number generator. */
	public static CmpVertex pickRandomState(LearnerGraph g, Random rnd)
	{
		int nr = rnd.nextInt(g.transitionMatrix.size());System.out.print(" st="+nr+" ");
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:g.transitionMatrix.entrySet())
			if (nr-- == 0)
				return entry.getKey();
		
		throw new IllegalArgumentException("something wrong with the graph - expected state was not found");
	}
	
	/** 
	 * Relabels graph, keeping NrToKeep original labels. All new ones are generated with
	 * prefix PrefixNew.
	 * 
	 * @param g graph to transform.
	 * @param NrToKeep number of labels to keep.
	 * @param PrefixNew prefix of new labels.
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
							fromTo.put(transition.getKey(), PrefixNew+newLabelCnt++);
				newRow.put(fromTo.get(transition.getKey()), transition.getValue());
			}
			newMatrix.put(entry.getKey(), newRow);
		}
		g.transitionMatrix = newMatrix;
	}
	
	/** Adds all states and transitions from graph <em>what</em> to graph <em>g</em>.
	 * 
	 * @param g target into which to merge what
	 * @param what graph to merge into g.
	 * @return vertex in g corresponding to the initial vertex in what 
	 */ 
	public static CmpVertex addToGraph(LearnerGraph g, LearnerGraph what)
	{
		Map<CmpVertex,CmpVertex> whatToG = new HashMap<CmpVertex,CmpVertex>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:what.transitionMatrix.entrySet())
			whatToG.put(entry.getKey(), 
					LearnerGraph.generateNewCmpVertex(g.nextID(entry.getKey().isAccept()), g.config));
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:what.transitionMatrix.entrySet())
		{
			Map<String,CmpVertex> row = new TreeMap<String,CmpVertex>();g.transitionMatrix.put(whatToG.get(entry.getKey()),row);
			for(Entry<String,CmpVertex> transition:entry.getValue().entrySet())
				row.put(transition.getKey(), whatToG.get(transition.getValue()));
		}					
		return whatToG.get(what.init);
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
			result.add(g.paths.tracePath(seq,state) == RPNIBlueFringeLearner.USER_ACCEPTED);
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
		List<List<String>> wSet = new LinkedList<List<String>>();wSet.addAll(WMethod.computeWSet(graph));
		Map<CmpVertex,List<Boolean>> bitVector = new TreeMap<CmpVertex,List<Boolean>>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> state:graph.transitionMatrix.entrySet())
			bitVector.put(state.getKey(),wToBooleans(graph,state.getKey(), wSet));
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
		Collection<List<String>> wSet = WMethod.computeWSet(graph);
		Set<String> Walphabet = new HashSet<String>();
		for(List<String> wSeq:wSet)
		{
			if (wSeq.size() != 1)
				throw new IllegalArgumentException("non-singleton W");
			Walphabet.add(wSeq.iterator().next());
		}
		Collection<String> alphabet = graph.wmethod.computeAlphabet();
		double fillFactor = getEffectiveFillRate(graph, wSet);//transitionsFromEveryState/alphabet.size();
		result+=getVectors(graph, wSet);
		double average = (1-fillFactor)*wSet.size()*graph.getStateNumber();
		int changeNumber = 0, total =0;
		Map<String,AtomicInteger> labelUsage = new HashMap<String,AtomicInteger>();for(String l:alphabet) labelUsage.put(l, new AtomicInteger());
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:graph.transitionMatrix.entrySet())
		{
			Collection<String> newLabels = new HashSet<String>();newLabels.addAll(Walphabet);newLabels.removeAll(entry.getValue().keySet());
			int changesForThisState = 0;
			
			for(String lbl:entry.getValue().keySet()) labelUsage.get(lbl).addAndGet(1);
			
			for(String label:newLabels)
			{
				LearnerGraph newGraph = graph.copy(graph.config);
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
		double stateNumber = graph.getStateNumber();
		double wsize = wSet.size();
		double expectedNrOfChanges = wsize*2*fillFactor*(1-fillFactor)*Math.pow(fillFactor*fillFactor+(1-fillFactor)*(1-fillFactor), wsize-1)*
			stateNumber*(stateNumber-1)/2;
		result+="Distribution of labels: ";for(Entry<String,AtomicInteger> en:labelUsage.entrySet()) result+=" "+en.getValue();result+="\n";
		result+="Distribution of elements of W: ";for(String wElem:Walphabet) result+=" "+labelUsage.get(wElem);result+="\n";
		return Math.abs(expectedNrOfChanges-changeNumber)/changeNumber+"\n"+result+"W size: "+wSet.size()+" W changes: "+changeNumber+ " out of "+total+" (expected "+average+"), \nfill factor is "+fillFactor+"\n "+
			"Expected number of changes is: "+expectedNrOfChanges
		;
	}
	
}
