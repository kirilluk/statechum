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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.Map.Entry;

import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.StringVertex;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.Configuration;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.Configuration.IDMode;
import statechum.analysis.learning.oracles.*;
import statechum.xmachine.model.testset.PTATestSequenceEngine.FSMAbstraction;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

/** This class and its wholly-owned subsidiaries perform computation 
 * of scores, state merging and question generation. 
 */
public class LearnerGraph {
	/** The initial vertex. */
	CmpVertex init;

	/** Transition matrix. */
	Map<CmpVertex,Map<String,CmpVertex>> transitionMatrix = new TreeMap<CmpVertex,Map<String,CmpVertex>>();
			
	/** Represents a slightly different view of this machine and used in W set generation. */
	public abstract class FSMImplementation implements FSMAbstraction
	{
		public Object getInitState() {
			return init;
		}
	
		public Object getNextState(Object currentState, String input) 
		{
			CmpVertex result = null;
			Map<String,CmpVertex> row = transitionMatrix.get(currentState);
			if (row != null)
				result = row.get(input);
			return result;
		}
	
		public boolean isAccept(Object currentState) 
		{
			return ((CmpVertex)currentState).isAccept();
		}

		/** Whether a sequence ending at a given vertex should be returned as a result of getData(). */
		abstract public boolean shouldBeReturned(Object elem);
	}
	
	class NonExistingPaths implements FSMAbstraction
	{
		private final CmpVertex red = init;
		
		public NonExistingPaths()
		{
		}
		
		public Object getInitState() {
			return red;
		}
	
		public final CmpVertex junkVertex = LearnerGraph.generateNewCmpVertex("JUNK",config);
				
		public Object getNextState(Object currentState, String input) 
		{
			CmpVertex result = null;
			Map<String,CmpVertex> row = transitionMatrix.get(currentState);
			if (row != null)
				result = row.get(input);
			if (result == null)
				result = junkVertex;

			return result;
		}
	
		public boolean isAccept(@SuppressWarnings("unused")	Object currentState) 
		{
			return true;
		}

		public boolean shouldBeReturned(Object elem) {
			return elem == junkVertex;
		}
	}
	
	/** Stores all red-blue pairs; has to be backed by array for 
	 * the optimal performance of the sort function. 
	 */
	protected List<PairScore> pairsAndScores;

	/** The configuration stores parameters used by a variety methods
	 * involved in learning;
	 */
	public final Configuration config;
	
	/** Used to switch on a variety of consistency checks. */
	public static boolean testMode = false;

	/** The initial size of the pairsAndScores array. */
	public static final int pairArraySize = 2000;
	
	/** Resets all the colour labelling to the initial value. */
	public void clearColours()
	{
		for(CmpVertex v:transitionMatrix.keySet())
			v.setColour(null);
		init.setColour(JUConstants.RED);
	}
	
	final public ComputeQuestions questions = new ComputeQuestions(this);
	final public PathRoutines paths = new PathRoutines(this);
	final public MergeStates merger = new MergeStates(this);
	final public PairScoreComputation pairscores = new PairScoreComputation(this);
	final public WMethod wmethod = new WMethod(this);
	
	/** Initialises the class used to compute scores between states.
	 *
	 * @param g the graph it will be used on 
	 */
	public LearnerGraph(DirectedSparseGraph g,Configuration conf)
	{//TODO: to check that all exceptions are thrown for appropriate graphs.
		config = conf;initEmpty();
		Map<Vertex,CmpVertex> origToCmp = new HashMap<Vertex,CmpVertex>();
		pairsAndScores = new ArrayList<PairScore>(pairArraySize);//graphVertices.size()*graphVertices.size());
		
		
		synchronized (LearnerGraph.syncObj) 
		{
			for(Vertex srcVert:(Set<Vertex>)g.getVertices())
			{
				CmpVertex vert = cloneCmpVertex(srcVert,config);
				origToCmp.put(srcVert, vert);

				if (findVertex(vert.getName()) != null)
					throw new IllegalArgumentException("multiple states with the same name "+vert.getName());
				
				transitionMatrix.put(vert,new TreeMap<String,CmpVertex>());// using TreeMap makes everything predictable
				if (DeterministicDirectedSparseGraph.isInitial(srcVert))// special case for the initial vertex.
				{
					Object property = srcVert.getUserDatum(JUConstants.INITIAL);
					if (!(property instanceof Boolean) || !((Boolean)property).booleanValue())
						throw new IllegalArgumentException("invalid init property");

					if (init != null)
						throw new IllegalArgumentException("vertices "+srcVert+" and "+init+" are both labelled as initial states");
					init = vert;
				}
			}
		} // synchronized (LearnerGraph.syncObj)

		if (init == null)
			throw new IllegalArgumentException("missing initial state");
		init.setColour(JUConstants.RED);

		Iterator<DirectedSparseEdge> edgeIter = g.getEdges().iterator();
		while(edgeIter.hasNext())
		{	
			DirectedSparseEdge edge = edgeIter.next();
			Map<String,CmpVertex> outgoing = transitionMatrix.get(origToCmp.get(edge.getSource()));
			assert origToCmp.containsKey(edge.getDest());// this cannot fail if we handle normal Jung graphs which will never let me add an edge with vertex not in the graph
			// The line below aims to ensure that inputs are evaluated by computeStateScore in a specific order, which in conjunction with the visited set of computeStateScore permits emulating a bug in computeScore
			createLabelToStateMap((Set<String>)edge.getUserDatum(JUConstants.LABEL),origToCmp.get(edge.getDest()),outgoing);
		}
	}
	
	/** Given a set of labels and a target state, this method adds to a supplied map an association 
	 * of every label with the specified target state.
	 * 
	 * @param labels labels
	 * @param to target state
	 * @param map a map associating state <i>to</i> with each of the labels. If this is <i>null</i>, a new map is created.
	 * @return an updated map.
	 */ 
	public static Map<String,CmpVertex> createLabelToStateMap(Collection<String> labels,CmpVertex to,Map<String,CmpVertex> map)
	{
		Map<String,CmpVertex> result = (map == null)? new LinkedHashMap<String,CmpVertex>() : map;
		for(String label:labels)
		{
			if (result.containsKey(label))
				throw new IllegalArgumentException("nondeterminism detected for label "+label);
			result.put(label,to);
		}
		return result;
	}

	public LearnerGraph(Configuration conf)
	{
		config = conf;
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
			
	public CmpVertex getVertex(List<String> seq)
	{
		CmpVertex result = init;
		Iterator<String> seqIt = seq.iterator();
		while(seqIt.hasNext() && result != null)
			result = transitionMatrix.get(result).get(seqIt.next());
		
		return result;
	}
	
	public CmpVertex getVertex(CmpVertex from, List<String> seq){
		CmpVertex result = from;
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

	/** Note: this clone is not necessarily deep: the transition matrix is 
	 * cloned and so is the configuration, but states (vertices) are not 
	 * if the configuration does not specify cloning; they are cloned 
	 * otherwise.
	 * 
	 * @param conf the configuration to use in the process of copying. 
	 */
	public LearnerGraph copy(Configuration copyConfiguration)
	{
		LearnerGraph result = new LearnerGraph(copyConfiguration);
		result.initEmpty();
		result.transitionMatrix = new TreeMap<CmpVertex,Map<String,CmpVertex>>(); 

		Map<CmpVertex,CmpVertex> oldToNew = new HashMap<CmpVertex,CmpVertex>();
		
		// First, clone vertices
		for(CmpVertex state:transitionMatrix.keySet())
		{
			CmpVertex newState = cloneCmpVertex(state, copyConfiguration);oldToNew.put(state, newState);
			result.transitionMatrix.put(newState, new TreeMap<String, CmpVertex>());
		}
		result.init = oldToNew.get(init);
		
		// Now clone edges.
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:transitionMatrix.entrySet())
		{
			Map<String,CmpVertex> row = result.transitionMatrix.get(oldToNew.get(entry.getKey()));
			for(Entry<String,CmpVertex> rowEntry:entry.getValue().entrySet())
				row.put(rowEntry.getKey(),oldToNew.get(rowEntry.getValue()));
		}
		return result;
	}

	/** Finds a vertex with a supplied name in a transition matrix.
	 * Important: do not change the acceptance condition on the returned vertex: it will mess up the
	 * transition matrix since hash code is dependent on acceptance.
	 */
	public CmpVertex findVertex(String name)
	{
		CmpVertex result = null;
		Iterator<Entry<CmpVertex,Map<String,CmpVertex>>> entryIt = transitionMatrix.entrySet().iterator();
		while(entryIt.hasNext() && result == null)
		{
			CmpVertex currentVert = entryIt.next().getKey();
			String vertName = currentVert.getName();
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

	/** Generates vertice IDs. */
	public String nextID(boolean accepted)
	{
		if (config.getMode() == IDMode.POSITIVE_ONLY)
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
		CmpVertex newVertex = generateNewCmpVertex(nextID(accepted),config);
		newVertex.setAccept(accepted);
		transitionMatrix.put(newVertex, new TreeMap<String,CmpVertex>());
		transitionMatrix.get(prevState).put(input,newVertex);
		return newVertex;
	}

	/** Creates a new vertex with the supplied name; the specific 
	 * type generated depends on the configuration supplied. 
	 * 
	 * @param name the name of the vertex to generate
	 * @param conf the configuration to use when deciding what to produce.
	 * @return the new vertex.
	 */
	public static CmpVertex generateNewCmpVertex(String name,Configuration conf)
	{
		synchronized(syncObj)
		{
			return conf.isLearnerUseStrings()? 
					new StringVertex(name):
					new DeterministicVertex(name);			
		}		
	}
	
	/** This is not quite like a real clone - it clones depending on the 
	 * global configuration, so it is possible to turn a DeterministicVertex 
	 * into a StringVertex and the other way around. Moreover, cloning a 
	 * deterministic vertex requires holding a lock which is currently located in this class.
	 * <p>
	 * Important: if configuration specifies "do not clone", this one simply returns the 
	 * vertex it has been passed (with one exception). 
	 * There is an exception to this rule: if the supplied vertex
	 * is not a CmpVertex and the configuration permits handling of such vertices, 
	 * it will always be cloned. 
	 * 
	 * @param vertToClone vertex to clone.
	 * @param conf configuration to use when cloning.
	 * @return the result of cloning.
	 */
	public static CmpVertex cloneCmpVertex(Object vertToClone,Configuration conf)
	{
		CmpVertex result = null;
		if (vertToClone instanceof CmpVertex)
		{// normal processing
			CmpVertex vert = (CmpVertex)vertToClone;
			if (!conf.isLearnerCloneGraph())
				result = vert;
			else
			{
				result = generateNewCmpVertex(vert.getName(),conf);
				result.setColour(vert.getColour());result.setAccept(vert.isAccept());result.setHighlight(vert.isHighlight());
			}
		}
		else
		{// we've been passed something which is not a CmpVertex
			if (!(vertToClone instanceof Vertex))
				throw new IllegalArgumentException("Cannot clone vertex which is neither a CmpVertex nor Vertex, what was passed is "+vertToClone);
			if (!conf.isAllowedToCloneNonCmpVertex())
				throw new IllegalArgumentException("Cannot clone a non-CmpVertex - prohibited using configuration");
			
			Vertex srcVert = (Vertex)vertToClone;
			if (!srcVert.containsUserDatumKey(JUConstants.LABEL))
				throw new IllegalArgumentException("vertex "+srcVert+" is not labelled");
			
			// Copying the attributes associated with the vertex
			result = generateNewCmpVertex((String)srcVert.getUserDatum(JUConstants.LABEL), conf);
			result.setAccept(DeterministicDirectedSparseGraph.isAccept(srcVert));
			if (srcVert.containsUserDatumKey(JUConstants.COLOUR))
				result.setColour((JUConstants)srcVert.getUserDatum(JUConstants.COLOUR));
			if (srcVert.containsUserDatumKey(JUConstants.HIGHLIGHT))
				result.setHighlight(true);
		}
		return result;
	}

	/** Converts a transition into an FSM structure, by taking a copy.
	 * 
	 * @param tTable table, where tTable[source][input]=targetstate
	 * @param vFrom the order in which elements from tTable are to be used.
	 * @param rejectNumber the value of an entry in a tTable which is used to denote an absence of a transition.
	 * @return the constructed transition structure.
	 */
	public static LearnerGraph convertTableToFSMStructure(final int [][]tTable, final int []vFrom, int rejectNumber, Configuration config)
	{
		if (vFrom.length == 0 || tTable.length == 0) throw new IllegalArgumentException("array is zero-sized");
		int alphabetSize = tTable[vFrom[0]].length;
		if (alphabetSize == 0) throw new IllegalArgumentException("alphabet is zero-sized");
		CmpVertex stateName[] = new CmpVertex[tTable.length];for(int i=0;i < tTable.length;++i) stateName[i]=new StringVertex("S"+i);
		String inputName[] = new String[alphabetSize];for(int i=0;i < alphabetSize;++i) inputName[i]="i"+i;
		LearnerGraph fsm = new LearnerGraph(config);fsm.initEmpty();
		fsm.init = stateName[vFrom[0]];
		Set<CmpVertex> statesUsed = new HashSet<CmpVertex>();
		for(int i=0;i<vFrom.length;++i)
		{
			int currentState = vFrom[i];
			if (currentState == rejectNumber) throw new IllegalArgumentException("reject number in vFrom");
			if (tTable[currentState].length != alphabetSize) throw new IllegalArgumentException("rows of inconsistent size");
			Map<String,CmpVertex> row = new LinkedHashMap<String,CmpVertex>();
			stateName[currentState].setAccept(true);
			for(int input=0;input < tTable[currentState].length;++input)
				if (tTable[currentState][input] != rejectNumber)
				{
					int nextState = tTable[currentState][input];
					if (nextState < 0 || nextState > tTable.length)
						throw new IllegalArgumentException("transition from state "+currentState+" leads to an invalid state "+nextState);
					row.put(inputName[input], stateName[nextState]);
					statesUsed.add(stateName[nextState]);
				}
			fsm.transitionMatrix.put(stateName[currentState], row);
		}
		statesUsed.removeAll(fsm.transitionMatrix.keySet());
		if (!statesUsed.isEmpty())
			throw new IllegalArgumentException("Some states in the transition table are not included in vFrom");
		return fsm;
	}
	
	/** Initialises this graph with a single-state PTA. */
	public void initPTA()
	{
		initEmpty(); 
		init = generateNewCmpVertex(config.getDefaultInitialPTAName(),config);
		init.setAccept(true);init.setColour(JUConstants.RED);
		
		transitionMatrix.put(init,new TreeMap<String,CmpVertex>());
	}
	
	/** Initialises this graph with an empty graph. */
	public void initEmpty()
	{
		transitionMatrix.clear();init=null;
		pairsAndScores = new ArrayList<PairScore>(pairArraySize);
	}

	@Override
	public String toString()
	{
		return "states: "+transitionMatrix.size()+" (hash "+transitionMatrix.hashCode()+")";
	}

	/** This one does not consider configuration or IDs - only states/transitions 
	 * are compared. I think this is best, however note that it is not a 
	 * congruence: doing an "augment pta" may hence add vertices with different
	 * numbers and hence previously identical graphs will become different.
	 * I think that despite this problem, it is important to be able to consider
	 * graphs with isomorphic transition diagrams to be the same.
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		//result = prime * result + ((config == null) ? 0 : config.hashCode());
		//result = prime * result + vertNegativeID;
		//result = prime * result + vertPositiveID;
		result = prime * result + (init == null?0:init.hashCode());
		result = prime * result + transitionMatrix.hashCode();
		return result;
	}

	/** This one does not compare configuration or IDs - only states/transitions 
	 * are compared. I think this is best, however note that it is not a 
	 * congruence: doing an "augment pta" may hence add vertices with different
	 * numbers and hence previously identical graphs will become different.
	 * I think that despite this problem, it is important to be able to consider
	 * graphs with isomorphic transition diagrams to be the same.
	 *    
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof LearnerGraph))
			return false;
		final LearnerGraph other = (LearnerGraph) obj;
		/*
		if (config == null) {
			if (other.config != null)
				return false;
		} else if (!config.equals(other.config))
			return false;
		if (vertNegativeID != other.vertNegativeID)
			return false;
		if (vertPositiveID != other.vertPositiveID)
			return false;
		*/
		if (init == null)
		{
			if (other.init != null)
				return false;
		}
		else
			if (!init.equals(other.init))
				return false;
		
		if (!transitionMatrix.equals(other.transitionMatrix)) // This is enough to check that the content of the matrices is the same.
			return false;
		return true;
	}
}

