/*
 * Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * StateChum. If not, see <http://www.gnu.org/licenses/>.
 */ 

package statechum.analysis.learning.rpnicore;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.Map.Entry;

import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.oracles.*;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

/** This class and its wholly-owned subsidiaries perform computation of scores, state merging and question generation. */
public class LearnerGraph implements Cloneable {
	/** The initial vertex. */
	CmpVertex init;
	
	/** Transition matrix. */
	Map<CmpVertex,Map<String,CmpVertex>> transitionMatrix = new TreeMap<CmpVertex,Map<String,CmpVertex>>();
			
	/** Stores all red-blue pairs; has to be backed by array for the optimal performance of the sort function. */
	protected List<PairScore> pairsAndScores;

	public static class Configuration implements Cloneable
	{
		protected int generalisationThreshold=0;
		protected int pairsMergedPerHypothesis=0;
		
		protected boolean bumpPositives = false, useCompatibilityScore = false;

		public int getGeneralisationThreshold() {
			return generalisationThreshold;
		}

		public void setGeneralisationThreshold(int generalisationThresholdArg) {
			this.generalisationThreshold = generalisationThresholdArg;
		}

		public int getPairsMergedPerHypothesis() {
			return pairsMergedPerHypothesis;
		}

		public void setPairsMergedPerHypothesis(int pairsMergedPerHypothesisArg) {
			this.pairsMergedPerHypothesis = pairsMergedPerHypothesisArg;
		}

		public boolean isBumpPositives() {
			return bumpPositives;
		}

		public void setBumpPositives(boolean bumpPositivesArg) {
			this.bumpPositives = bumpPositivesArg;
		}

		public boolean isUseCompatibilityScore() {
			return useCompatibilityScore;
		}

		public void setUseCompatibilityScore(boolean useCompatibilityScoreArg) {
			this.useCompatibilityScore = useCompatibilityScoreArg;
		}

		@Override
		public Object clone() {
			try {
				return super.clone();
			} catch (CloneNotSupportedException e) {
				IllegalArgumentException ex = new IllegalArgumentException("clone of Configuration failed - should not happen");
				ex.initCause(e);throw ex;
			}
		}
	};
	public final Configuration config;
	
	/** Used to switch on a variety of consistency checks. */
	public static boolean testMode = false;

	/** The initial size of the pairsAndScores array. */
	public static final int pairArraySize = 2000;
	
	/** Resets all the colour labelling to the initial value. */
	public void clearColours()
	{
		for(Vertex v:transitionMatrix.keySet())
			v.removeUserDatum(JUConstants.COLOUR);
		init.addUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
	}
	
	final public ComputeQuestions questions = new ComputeQuestions(this);
	final public PathRoutines paths = new PathRoutines(this);
	final public MergeStates merger = new MergeStates(this);
	final public PairScoreComputation pairscores = new PairScoreComputation(this);
	
	/** Initialises the class used to compute scores between states.
	 * 
	 * @param g the graph it will be used on 
	 */
	public LearnerGraph(DirectedSparseGraph g)
	{
		init = (CmpVertex)RPNIBlueFringeLearner.findInitial(g);
		config = new Configuration();
		
		pairsAndScores = new ArrayList<PairScore>(pairArraySize);//graphVertices.size()*graphVertices.size());
		for(CmpVertex v:(Set<CmpVertex>)g.getVertices())
		{
			transitionMatrix.put(v,new TreeMap<String,CmpVertex>());// using TreeMap makes everything predictable
			v.removeUserDatum(JUConstants.COLOUR);
		}
		init.addUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
	
		Iterator<DirectedSparseEdge> edgeIter = g.getEdges().iterator();
		while(edgeIter.hasNext())
		{	
			DirectedSparseEdge e = edgeIter.next();
			Map<String,CmpVertex> outgoing = transitionMatrix.get(e.getSource());
			// The line below aims to ensure that inputs are evaluated by computeStateScore in a specific order, which in conjunction with the visited set of computeStateScore permits emulating a bug in computeScore
			for(String label:(Collection<String>)e.getUserDatum(JUConstants.LABEL))
				outgoing.put(label, (CmpVertex)e.getDest());			
		}
	}
	
	protected LearnerGraph(Configuration conf)
	{
		config = (Configuration)conf.clone();
		initEmpty();
	}

	/** Constructs a graph with an initial state. */
	public LearnerGraph()
	{
		config = new Configuration();
		initPTA();
	}

	/**
	 * Augment every occurrence of the first label in the pair in the PTA
	 * with an edge to the second label in the pair, that is either accepted or not
	 */
	public void augmentPairs(StringPair pair, boolean accepted){
		Collection<CmpVertex> fromVertices = findVertices(pair.getFrom());
		for (CmpVertex vertex : fromVertices) {
			Collection<List<String>> tails = getTails(vertex, new ArrayList<String>(), new HashSet<List<String>>());
			for (List<String> list : tails) {
				addNegativeEdges(vertex, list, pair, accepted);
			}
		}
	}
	
	private void addNegativeEdges(CmpVertex fromVertex,List<String> tail, StringPair pair, boolean accepted){
		Stack callStack = new Stack();
		addVertex(fromVertex, accepted, pair.getTo());
		CmpVertex currentVertex = fromVertex;
		for(int i=0;i<tail.size();i++){
			String element = tail.get(i);
			currentVertex = transitionMatrix.get(currentVertex).get(element);
			if(element.equals("ret")&&!callStack.isEmpty()){
				callStack.pop();
				if(callStack.isEmpty())
					addVertex(currentVertex, accepted, pair.getTo());
			}
			else if (!element.equals("ret"))
				callStack.push(element);
			else if (element.equals("ret")&&callStack.isEmpty())
				return;
		}
	}
	
	private Collection<List<String>> getTails(CmpVertex vertex, ArrayList<String> currentList, Collection<List<String>> collection){
		Map<String,CmpVertex> successors = transitionMatrix.get(vertex);
		if(successors.isEmpty()){
			collection.add(currentList);
			return collection;
		}

		Iterator<String> keyIt = successors.keySet().iterator();
		while(keyIt.hasNext()){
			String key = keyIt.next();
			currentList.add(key);
			collection.addAll(getTails(successors.get(key),currentList,collection));
		}
		return collection;
	}
	
	/**
	 *returns set of vertices that are the destination of label
	 */
	private Collection<CmpVertex> findVertices(String label)
	{
		Collection<CmpVertex> vertices = new HashSet<CmpVertex>();
		Iterator<Map<String, CmpVertex>> outgoingEdgesIt = transitionMatrix.values().iterator();
		while(outgoingEdgesIt.hasNext()){
			Map<String,CmpVertex> edges = outgoingEdgesIt.next();
			if(edges.keySet().contains(label))
				vertices.add(edges.get(label));
		}
		return vertices;
	}
			
	public Vertex getVertex(List<String> seq)
	{
		Vertex result = init;
		Iterator<String> seqIt = seq.iterator();
		while(seqIt.hasNext() && result != null)
			result = transitionMatrix.get(result).get(seqIt.next());
		
		return result;
	}
	
	public Vertex getVertex(Vertex from, List<String> seq){
		Vertex result = from;
		Iterator<String> seqIt = seq.iterator();
		while(seqIt.hasNext() && result != null)
			result = transitionMatrix.get(result).get(seqIt.next());
		
		return result;
	}
	
	public int countEdges(){
		Iterator<Map<String,CmpVertex>> outIt = transitionMatrix.values().iterator();
		int counter = 0;
		while(outIt.hasNext()){
			Map current = outIt.next();
			counter = counter + current.keySet().size();
		}
		return counter;
	}
	
	/** Takes states associated with red in mergedVertices and finds a target state for a given input
	 * 
	 * @param mergedVertices vertices linked to r
	 * @param r the red state
	 * @param input the input to consider
	 * @return the target state, null if there is no transition with this input not only from r but also from all states associated to it
	 * using mergedVertices. 
	 */
	protected CmpVertex findNextRed(Map<CmpVertex,List<CmpVertex>> mergedVertices, Vertex r, String input)
	{
		CmpVertex target = null;
		List<CmpVertex> associatedVertices = mergedVertices.get(r);
		if (associatedVertices != null)
		{
			Iterator<CmpVertex> associatedIt = associatedVertices.iterator();
			while(associatedIt.hasNext() && target == null)
				target = transitionMatrix.get(associatedIt.next()).get(input);
		}
		return target;
	}

	/** Note: this clone is not very deep: the transition matrix is cloned and so is the configuration, 
	 * but states (vertices are not).
	 */
	@Override
	public Object clone()
	{
		LearnerGraph result = new LearnerGraph(config);
		result.init = init;
		result.transitionMatrix = new TreeMap<CmpVertex,Map<String,CmpVertex>>(); 
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:transitionMatrix.entrySet())
		{
			Map<String,CmpVertex> newValue = new TreeMap<String,CmpVertex>();
			newValue.putAll(entry.getValue());
			result.transitionMatrix.put(entry.getKey(),newValue);
		}
		return result;
	}

	/** Finds a vertex with a supplied name in a transition matrix.
	 */
	public CmpVertex findVertex(String name)
	{
		CmpVertex result = null;
		Iterator<Entry<CmpVertex,Map<String,CmpVertex>>> entryIt = transitionMatrix.entrySet().iterator();
		while(entryIt.hasNext() && result == null)
		{
			CmpVertex currentVert = entryIt.next().getKey();
			String vertName = (String)currentVert.getUserDatum(JUConstants.LABEL);
			if (vertName.equals(name))
				result = currentVert;
		}
		return result;
	}

	/** A very important object - this one is used when I wish to create new vertices or edges in a Jung graph.
	 * There are many threads which may wish to do that; the potential outcome is that a single thread may end up
	 * with multiple Vertices with the same ID, as repeatedly observed on 50-6. Holding a lock on this global object 
	 * when creating vertices/edges eliminates the potential of such racing, which occurs when public static int ID
	 * gets increased by Jung in the course of object creation.
	 */
	public static final Object syncObj = new Object();
	
	protected int vertPositiveID = 1;
	protected int vertNegativeID = 1;
	
	public enum IDMode { NONE, POSITIVE_NEGATIVE, POSITIVE_ONLY };
	
	protected IDMode id_mode = IDMode.NONE; // creation of new vertices is prohibited.
	
	public LearnerGraph setMode(IDMode m)
	{
		id_mode = m;return this;
	}
	
	/** Generates vertice IDs. */
	public String nextID(boolean accepted)
	{
		if (id_mode == IDMode.POSITIVE_ONLY)
			return "V"+vertPositiveID++;
		return (accepted?"P"+vertPositiveID++:"N"+vertNegativeID++);
	}
	
	/** This one is similar to the above but does not add a vertex to the graph - I need this behaviour when
	 * concurrently processing graphs. 
	 *  
	 * @param prevState the state from which to start the new edge
	 * @param accepted whether the vertex to add should be an accept one
	 * @param input the label of the edge
	 * @return the new vertex.
	 */
	CmpVertex addVertex(CmpVertex prevState, boolean accepted, String input)
	{
		assert Thread.holdsLock(syncObj);
		CmpVertex newVertex = new DeterministicDirectedSparseGraph.DeterministicVertex(nextID(accepted));
		newVertex.setUserDatum(JUConstants.ACCEPTED, accepted, UserData.SHARED);
		transitionMatrix.put(newVertex, new TreeMap<String,CmpVertex>());
		transitionMatrix.get(prevState).put(input,newVertex);
		return newVertex;
	}

	public void initPTA()
	{
		initEmpty();
		init = new DeterministicDirectedSparseGraph.DeterministicVertex("Init");
		init.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
		init.setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
		//graph.setUserDatum(JUConstants.TITLE, "Hypothesis machine", UserData.SHARED);
		transitionMatrix.put(init,new TreeMap<String,CmpVertex>());
	}
	
	public void initEmpty()
	{
		transitionMatrix.clear();
		pairsAndScores = new ArrayList<PairScore>(pairArraySize);
	}

	@Override
	public String toString()
	{
		return "states: "+transitionMatrix.size()+" (hash "+transitionMatrix.hashCode()+")";
	}
}

