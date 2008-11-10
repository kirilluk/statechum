/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

package statechum.analysis.learning.rpnicore;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.StringVertex;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.PairScore;
import statechum.model.testset.PTASequenceEngine.FSMAbstraction;
import edu.uci.ics.jung.graph.Graph;

/** This class and its wholly-owned subsidiaries perform computation 
 * of scores, state merging and question generation. 
 */
public class LearnerGraph extends AbstractTransitionMatrix<CmpVertex> 
{
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
			if (currentState == null)
				return true;// always return reject-nodes
			return ((CmpVertex)currentState).isAccept();// TODO: This line will ever return false because X-m testing is never used on graphs with explicit reject states 
		}

		/** Whether a sequence ending at a given vertex should be returned as a result of getData(). */
		abstract public boolean shouldBeReturned(Object elem);
	}
	
	class NonExistingPaths implements FSMAbstraction
	{
		private final CmpVertex red = init;
		
		public NonExistingPaths()
		{// nothing to initialise here
		}
		
		public Object getInitState() {
			return red;
		}
	
		public final CmpVertex junkVertex = AbstractTransitionMatrix.generateNewCmpVertex(new VertexID("JUNK"),config);
				
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

	/** Stores pairs of states which should either never be merged due to constraints which cannot 
	 * be easily expressed with counter-examples such as non-determinism related to intersection of
	 * domains of labels from them (those which can be expressed using labels should be added to the maximal
	 * automaton). 
	 */
	protected Map<CmpVertex,Set<CmpVertex>> incompatibles;

	/** Adds a supplied pair to the collection of incompatible elements. 
	 *  It is assumed that the two states belong to the graph; one would perhaps also not want to make them equal.
	 * @param A one of the vertices to add
	 * @param B another vertex to add.
	 */
	public void addToIncompatibles(CmpVertex A, CmpVertex B)
	{
		assert !A.getID().equals(B.getID());
		addToIncompatibles_internal(A, B);
		addToIncompatibles_internal(B, A);
	}

	private void addToIncompatibles_internal(CmpVertex A, CmpVertex B)
	{
		Set<CmpVertex> incSet = incompatibles.get(A);
		if (incSet == null)
		{
			incSet = new HashSet<CmpVertex>();incompatibles.put(A,incSet);
		}
		incSet.add(B);
	}
	
	/** The state corresponding to the red and blue states after the merge of which this graph was built. */
	protected CmpVertex stateLearnt = null;
	
	public CmpVertex getStateLearnt()
	{
		return stateLearnt;
	}
	
	/** Used to switch on a variety of consistency checks. */
	public static boolean testMode = false;

	/** The initial size of the pairsAndScores array. */
	public static final int pairArraySize = 2000;

	final public ComputeQuestions questions = new ComputeQuestions(this);
	final public PathRoutines paths = new PathRoutines(this);
	final public MergeStates merger = new MergeStates(this);
	final public PairScoreComputation pairscores = new PairScoreComputation(this);
	final public WMethod wmethod = new WMethod(this);
	final public Transform transform = new Transform(this);
	final public Linear linear = new Linear(this);
	final CachedData learnerCache = new CachedData(this);
	final public SootOracleSupport sootsupport = new SootOracleSupport(this);

	/** Constructs a StateChum graph from a Jung Graph
	 *
	 * @param g the graph to build StateChum graph from
	 * @param conf configuration to use
	 */
	public LearnerGraph(Graph g,Configuration conf)
	{
		super(conf);
		TransitionMatrixND matrixND=new TransitionMatrixND(g,conf);
		loadFrom(matrixND);
	}
	
	/** Constructs a StateChum graph from the supplied graph. An IllegalArgumentException is thrown if a non-deterministic choice is detected.
	 * Use <em>buildDeterministicGraph</em> for a proper conversion of a non-deterministic structure to
	 * a deterministic one.
	 *
	 * @param matrixND the matrix to build graph from.
	 */
	public LearnerGraph(TransitionMatrixND matrixND)
	{
		super(matrixND.config);
		loadFrom(matrixND);
	}
	
	/** Loads the data in the graph from the supplied graph. An IllegalArgumentException is thrown if a non-deterministic choice is detected.
	 * Use <em>buildDeterministicGraph</em> for a proper conversion of a non-deterministic structure to
	 * a deterministic one.
	 * 
	 * @param matrixND the matrix to build graph from.
	 */
	private void loadFrom(TransitionMatrixND matrixND)
	{
		initEmpty();
		setName(matrixND.getName());
		
		pairsAndScores = new ArrayList<PairScore>(pairArraySize);//graphVertices.size()*graphVertices.size());
		incompatibles = new HashMap<CmpVertex,Set<CmpVertex>>();
		
		// now we need to turn potentially non-deterministic choices of target states to deterministic ones.
		vertNegativeID = matrixND.vertNegativeID;vertPositiveID=matrixND.vertPositiveID;

		Map<CmpVertex,CmpVertex> oldToNew = new HashMap<CmpVertex,CmpVertex>();
		
		// First, clone vertices
		for(CmpVertex state:matrixND.transitionMatrix.keySet())
		{
			CmpVertex newState = cloneCmpVertex(state, config);oldToNew.put(state, newState);
			transitionMatrix.put(newState, createNewRow());
		}
		init = oldToNew.get(matrixND.init);
		
		// Now clone edges.
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:matrixND.transitionMatrix.entrySet())
			transitionMatrix.put(oldToNew.get(entry.getKey()),convertRowToDet(entry.getValue(), oldToNew, entry.getKey()));
	}

	/** Given a deterministic row represented in a non-deterministic form, converts that row to a deterministic form.
	 *  
	 * @param map what to convert
	 * @param oldToNew map used to convert vertices from those in the original map to those to be used in the result map. If null, no conversion is performed.
	 * @param from the source state of the outgoing transitions - used in an error message
	 * @return result of conversion 
	 */
	public Map<String,CmpVertex> convertRowToDet(Map<String,List<CmpVertex>> map, Map<CmpVertex,CmpVertex> oldToNew, CmpVertex from)
	{
		Map<String,CmpVertex> result = createNewRow();
		for(Entry<String,List<CmpVertex>> rowEntry:map.entrySet())
		{
			if (rowEntry.getValue().size() != 1)
				throw new IllegalArgumentException("non-deterministic or empty target state for transition from state "+from+" with label "+rowEntry.getKey());
			CmpVertex target = rowEntry.getValue().iterator().next();if (oldToNew != null) target = oldToNew.get(target);
			assert target != null;
			result.put(rowEntry.getKey(),target);
		}
		return result;
	}

	/** Sometimes, we might wish to use a pre-set value for the maxScore. 
	 * This is particularly useful for testing.
	 */ 
	public void setMaxScore(int score)
	{
		if (learnerCache.maxScore < 0) learnerCache.maxScore = transitionMatrix.size()*wmethod.computeAlphabet().size();
		if (learnerCache.maxScore > score)
			throw new IllegalArgumentException("cannot set the max score below the actual maximum");
		learnerCache.maxScore=score;
	}
	
	public LearnerGraph(Configuration conf)
	{
		super(conf);
		initPTA();
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

	public int countEdges()
	{
		Iterator<Map<String,CmpVertex>> outIt = transitionMatrix.values().iterator();
		int counter = 0;
		while(outIt.hasNext()){
			Map<String,CmpVertex> current = outIt.next();
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
		result.vertNegativeID = vertNegativeID;result.vertPositiveID=vertPositiveID;

		Map<CmpVertex,CmpVertex> oldToNew = new HashMap<CmpVertex,CmpVertex>();
		
		// First, clone vertices
		for(CmpVertex state:transitionMatrix.keySet())
		{
			CmpVertex newState = cloneCmpVertex(state, copyConfiguration);oldToNew.put(state, newState);
			result.transitionMatrix.put(newState, createNewRow());
		}
		result.init = oldToNew.get(init);
		
		// Now clone edges.
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:transitionMatrix.entrySet())
		{
			Map<String,CmpVertex> row = result.transitionMatrix.get(oldToNew.get(entry.getKey()));
			for(Entry<String,CmpVertex> rowEntry:entry.getValue().entrySet())
				row.put(rowEntry.getKey(),oldToNew.get(rowEntry.getValue()));
		}
		
		for(Entry<CmpVertex,Set<CmpVertex>> entry:incompatibles.entrySet())
			if (!result.incompatibles.containsKey(entry.getKey()))
			{
				for(CmpVertex vert:entry.getValue())
					result.addToIncompatibles(entry.getKey(),vert);
			}
		return result;
	}

	/** Initialises this graph with a single-state PTA. */
	@Override
	public void initPTA()
	{
		super.initPTA();
		learnerCache.invalidate();
	}
	
	/** Initialises this graph with an empty graph, but IDs of vertices are unchanged. */
	@Override
	public void initEmpty()
	{
		super.initEmpty();
		pairsAndScores = new ArrayList<PairScore>(pairArraySize);incompatibles=new HashMap<CmpVertex,Set<CmpVertex>>();
		learnerCache.invalidate();
	}

	/** Used to determine which states to consider/ignore during linear.
	 * Conceptually similar to <em>FilterPredicate</em> but for a different
	 * purpose and an argument of a different type.
	  */ 
	public interface StatesToConsider
	{
		/** Returns true if the state is to be considered
		 * 
		 * @param vert state
		 * @param graph the graph in which this state is contained.
		 * @return whether state is to be considered
		 */
		public boolean stateToConsider(CmpVertex vert);
	}
	
	/** Builds a map from vertices to number, for use with <em>vertexToInt</em>.
	 * 
	 * @param whatToConsider to reject interface determining vertices to reject.
	 * null means all states are to be considered.
	 * @param vertToIntMap from vertices to numbers (an inverse of the map returned).
	 * @return map from vertex number to vertices 
	 */
	CmpVertex[] buildStateToIntegerMap(StatesToConsider whatToConsider, Map<CmpVertex,Integer> vertToIntMap)
	{
		int size=0;for(CmpVertex vert:transitionMatrix.keySet()) 
			if (whatToConsider == null || whatToConsider.stateToConsider(vert)) size++;
		CmpVertex [] intToVertexMap = new CmpVertex[size];
		int num=0;
		for(CmpVertex vert:transitionMatrix.keySet())
			if (whatToConsider == null || whatToConsider.stateToConsider(vert))
			{
				if (intToVertexMap != null) intToVertexMap[num]=vert;// populate an inverse map
				vertToIntMap.put(vert, num++);// populate the forward map
			}
		assert num == size;
		return intToVertexMap;
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
		assert !transitionMatrix.containsKey(newVertex);
		newVertex.setAccept(accepted);
		transitionMatrix.put(newVertex, createNewRow());
		transitionMatrix.get(prevState).put(input,newVertex);
		return newVertex;
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
			if (tTable[currentState].length != alphabetSize) 
				throw new IllegalArgumentException("rows of inconsistent size");
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

	@Override
	public Map<String, CmpVertex> createNewRow() {
		return new TreeMap<String,CmpVertex>();// using TreeMap makes everything predictable
	}
}

