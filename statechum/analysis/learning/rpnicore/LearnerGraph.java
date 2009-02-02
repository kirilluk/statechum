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
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

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
public class LearnerGraph extends AbstractLearnerGraph<CmpVertex,LearnerGraphCachedData> 
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
			return ((CmpVertex)currentState).isAccept();// Note that this line will ever return false 
				// when FSMImplementation-derivative is used for test generation because X-m testing 
				// is never used on graphs with explicit reject states. 
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
	
		public final CmpVertex junkVertex = AbstractLearnerGraph.generateNewCmpVertex(new VertexID("JUNK"),config);
				
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
	protected ArrayList<PairScore> pairsAndScores;

	/** The initial size of the pairsAndScores array. */
	public static final int pairArraySize = 2000;

	final public ComputeQuestions questions = new ComputeQuestions(this);
	final public PathRoutines paths = new PathRoutines(this);
	final public MergeStates merger = new MergeStates(this);
	final public PairScoreComputation pairscores = new PairScoreComputation(this);
	final public WMethod wmethod = new WMethod(this);
	final public Transform transform = new Transform(this);
	final public Linear linear = new Linear(this);
	
	final public SootOracleSupport sootsupport = new SootOracleSupport(this);

	/** Constructs a StateChum graph from a Jung Graph
	 *
	 * @param g the graph to build StateChum graph from
	 * @param conf configuration to use
	 */
	public LearnerGraph(Graph g,Configuration conf)
	{
		this(new LearnerGraphND(g,conf),conf);
	}
	
	/** Constructs a StateChum graph from the supplied graph. An IllegalArgumentException is thrown if a non-deterministic choice is detected.
	 * Use <em>buildDeterministicGraph</em> for a proper conversion of a non-deterministic structure to
	 * a deterministic one.
	 *
	 * @param matrixND the matrix to build graph from.
	 * @param argConfig configuration to use
	 */
	@SuppressWarnings("unchecked") // unchecked conversions are fine here because copyGraphs works the same way regardless of argument types.
	public LearnerGraph(AbstractLearnerGraph matrixND, Configuration argConfig)
	{
		super(argConfig);
		AbstractLearnerGraph.copyGraphs(matrixND, this);
	}

	/** Sometimes, we might wish to use a pre-set value for the maxScore. 
	 * This is particularly useful for testing.
	 */ 
	public void setMaxScore(int score)
	{
		if (learnerCache.maxScore < 0) learnerCache.maxScore = transitionMatrix.size()*pathroutines.computeAlphabet().size();
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
		return getVertex(init,seq);
	}
	
	/** Follows the supplied sequence of transitions from the supplied vertex and returns the vertex reached. 
	 * 
	 * @param from vertex to start from
	 * @param seq sequence of labels to follow
	 * @return vertex reached, null if the supplied sequence does not exist.
	 */
	public CmpVertex getVertex(CmpVertex from, List<String> seq)
	{
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

	/** Initialises this graph with an empty graph, but IDs of vertices are unchanged. */
	@Override
	public void initEmpty()
	{
		super.initEmpty();
		pairsAndScores = new ArrayList<PairScore>(pairArraySize);
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

	@Override
	void addTransition(Map<String, CmpVertex> row, String input, CmpVertex target) 
	{
		if (row.containsKey(input)) throw new IllegalArgumentException("non-determinism detected for input "+input+" to state "+target);
			
		row.put(input, target);
	}

	@Override
	public LearnerGraphCachedData createCache() {
		return new LearnerGraphCachedData(this);
	}
/*
	@Override
	public Class<CmpVertex> getClassOfTargetType() {
		return CmpVertex.class;
	}
*/
	@Override
	Collection<CmpVertex> getTargets(final CmpVertex targ) 
	{
		return new Collection<CmpVertex>() {

			public boolean add(@SuppressWarnings("unused") CmpVertex e) {
				throw new UnsupportedOperationException("should not be used");
			}

			public boolean addAll(@SuppressWarnings("unused") Collection<? extends CmpVertex> c) {
				throw new UnsupportedOperationException("should not be used");
			}

			public void clear() {
				throw new UnsupportedOperationException("should not be used");
			}

			public boolean contains(Object o) {
				return targ.equals(o);
			}

			public boolean containsAll(@SuppressWarnings("unused") Collection<?> c) {
				throw new UnsupportedOperationException("should not be used");
			}

			public boolean isEmpty() {
				return false;
			}

			public Iterator<CmpVertex> iterator() {
				return new Iterator<CmpVertex>()
				{
					boolean elementReturned = false;
					public boolean hasNext() {
						return !elementReturned;
					}
	
					public CmpVertex next() {
						assert hasNext();elementReturned = true;
						return targ;
					}
	
					public void remove() {
						throw new UnsupportedOperationException("remove cannot be performed.");
					}
				};				
			}

			public boolean remove(@SuppressWarnings("unused") Object o) {
				throw new UnsupportedOperationException("should not be used");
			}

			public boolean removeAll(@SuppressWarnings("unused") Collection<?> c) {
				throw new UnsupportedOperationException("should not be used");
			}

			public boolean retainAll(@SuppressWarnings("unused") Collection<?> c) {
				throw new UnsupportedOperationException("should not be used");
			}

			public int size() {
				return 1;
			}

			public Object[] toArray() {
				throw new UnsupportedOperationException("should not be used");
			}

			public <T> T[] toArray(@SuppressWarnings("unused") T[] a) {
				throw new UnsupportedOperationException("should not be used");
			}
		};
	}

	@Override
	Map<CmpVertex, Map<String, CmpVertex>> createNewTransitionMatrix() {
		return new TreeMap<CmpVertex, Map<String, CmpVertex>>();
	}

	@Override
	void removeTransition(Map<String, CmpVertex> row, String input, @SuppressWarnings("unused") CmpVertex target) 
	{
		row.remove(input);
	}

	@Override
	public AbstractLearnerGraph<CmpVertex, LearnerGraphCachedData> copy(Configuration conf) 
	{
		AbstractLearnerGraph<CmpVertex, LearnerGraphCachedData> result = newInstance(conf);AbstractLearnerGraph.copyGraphs(this, result);return result;
	}

	@Override
	public AbstractLearnerGraph<CmpVertex, LearnerGraphCachedData> newInstance(Configuration conf) 
	{
		return new LearnerGraph(conf);
	}
}

