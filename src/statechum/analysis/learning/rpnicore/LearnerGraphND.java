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

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import statechum.collections.TreeMapWithSearch;
import statechum.*;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.collections.ArrayMapWithSearchPos;
import statechum.collections.MapWithSearch;

import java.util.*;

/** This is a non-deterministic graph. Strictly speaking, all the methods here are applicable to the
 * generalised graph too and thus could have been placed in <em>AbstractPathRoutines</em>. The reason
 * they are here is that there does not seem to be much use in running such methods on deterministic 
 * graphs. In case one needs to do this, the relevant routines can be moved.
 * 
 * @author kirill
 */
public class LearnerGraphND extends AbstractLearnerGraph<List<CmpVertex>,LearnerGraphNDCachedData>
{
	public LearnerGraphND(Configuration conf)
	{
		super(conf);
		transitionMatrix = createNewTransitionMatrix(new Pair<>(conf.getMaxAcceptStateNumber(), conf.getMaxRejectStateNumber()));
		setInit(null);
		initPTA();
	}

	/** Constructs a StateChum graph from a Jung Graph
	 *
	 * @param g the graph to build StateChum graph from
	 * @param conf configuration to use
	 */
	@SuppressWarnings("unchecked")
	public LearnerGraphND(Graph g,Configuration conf)
	{
		super(conf);
		initEmpty();
		Map<Vertex,CmpVertex> origToCmp = new HashMap<>(g.numVertices());
		if (g.containsUserDatumKey(JUConstants.TITLE))
			setName((String)g.getUserDatum(JUConstants.TITLE));
		Set<VertID> idSet = new HashSet<>();
		
		synchronized (AbstractLearnerGraph.syncObj) 
		{
			for(Vertex srcVert:(Set<Vertex>)g.getVertices())
			{
				CmpVertex vert;
				if (DeterministicDirectedSparseGraph.isInitial(srcVert))// special case for the initial vertex.
				{
					vert = cloneCmpVertex(srcVert,config);//generateNewCmpVertex(getDefaultInitialPTAName(),config);
					Object property = srcVert.getUserDatum(JUConstants.INITIAL);
					if (!(property instanceof Boolean) || !(Boolean) property)
						throw new IllegalArgumentException("invalid init property");

					if (getInit() != null)
						throw new IllegalArgumentException("vertices "+srcVert+" and "+getInit()+" are both labelled as initial states");
					setInit(vert);
				}
				else vert = cloneCmpVertex(srcVert,config);
				origToCmp.put(srcVert, vert);

				if (idSet.contains(vert))
					throw new IllegalArgumentException("multiple states with the same name "+vert.getStringId());
				idSet.add(vert);
				
				transitionMatrix.put(vert,createNewRow());
			}
		} // synchronized (LearnerGraph.syncObj)

		if (getInit() == null)
			throw new IllegalArgumentException("missing initial state");

		for (DirectedSparseEdge edge : (Iterable<DirectedSparseEdge>) g.getEdges()) {
			Map<Label, List<CmpVertex>> outgoing = transitionMatrix.get(origToCmp.get(edge.getSource()));
			assert origToCmp.containsKey(edge.getDest());// this cannot fail if we handle normal Jung graphs which will never let me add an edge with vertex not in the graph
			// The line below aims to ensure that inputs are evaluated by computeStateScore in a specific order, which in conjunction with the visited set of computeStateScore permits emulating a bug in computeScore
			createLabelToStateMap((Set<Label>) edge.getUserDatum(JUConstants.LABEL), origToCmp.get(edge.getDest()), outgoing);
		}
		
		setIDNumbers();
	}

	/** Ignores all reject-states. */
	public static class ignoreRejectStatesClass implements StatesToConsider
	{
		@Override
		public boolean stateToConsider(CmpVertex vert) {
			return vert.isAccept();
		}
	}
	
	public final static StatesToConsider ignoreRejectStates =new ignoreRejectStatesClass();

	/** Ignores all states with no outgoing transitions to accept-states.
	 * The main idea is to eliminate as many states as possible for the construction
	 * of a matrix. A pair consisting of an arbitrary state and a reject state 
	 * will never have positive scores, either INCOMPATIBLE (if the former is an accept state)
	 * or a zero (if the former is reject) because there are no outgoing transitions
	 * from reject states. The same can be said if the latter state is an accept one but
	 * without outgoing transitions - there will also be no matched transitions.
	 * If there are some outgoing transitions from a state, it cannot be ignored since
	 * any transitions leading to it (if there is none, we should've not included it in a graph
	 * anyway) may be matched by transitions from some other states and a score associated
	 * with the pair may be needed for computation of a score of a pair with matched transitions
	 * leading to this state.
	 */
	public static class ignoreZeroClass implements StatesToConsider
	{
		private final LearnerGraph graph;
		
		public ignoreZeroClass(LearnerGraph g)
		{
			graph=g;
		}
		
		@Override
		public boolean stateToConsider(CmpVertex vert) {
			return graph.transitionMatrix.get(vert).size() > 0;
		}
	}
	
	/** Does not ignore any states. */
	public static class ignoreNoneClass implements StatesToConsider
	{
		@Override
		public boolean stateToConsider(@SuppressWarnings("unused") CmpVertex vert) {
			return true;
		}
	}
	
	public final static StatesToConsider ignoreNone = new ignoreNoneClass();

	/** Converts a given matrix into a non-deterministic representation, needed for
	 * some methods. */
	@SuppressWarnings({ "unchecked", "rawtypes" }) // I aim to convert any kind of graph to this type.
	public LearnerGraphND(AbstractLearnerGraph matrixND, Configuration argConfig)
	{
		super(argConfig);
		AbstractLearnerGraph.copyGraphs(matrixND, this);
	}
		
	public static final JUConstants ltlColour = JUConstants.INF_AMBER;

	/** Puts together transitions from a different matrices and returns the result of addition, which
	 * is most likely non-deterministic.
	 */
	public static 
	<TARGET_A_TYPE,TARGET_B_TYPE,
	CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
	CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>>
	LearnerGraphND UniteTransitionMatrices(
			AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> matrixToAdd, 
			AbstractLearnerGraph<TARGET_B_TYPE,CACHE_B_TYPE> origGraph)
	{
		LearnerGraphND matrixResult = new LearnerGraphND(origGraph,origGraph.config);
		
		// given that all text identifiers go before (or after) numerical ones, we're not
		// going to hit a state clash if we simply generate state names
		// based on the existing IDs of origGraph. 
		Map<CmpVertex,CmpVertex> firstToSecond = new TreeMap<>();
		firstToSecond.put(matrixToAdd.getInit(), matrixResult.getInit());
		for(CmpVertex firstVertex:matrixToAdd.transitionMatrix.keySet())
			if (firstVertex != matrixToAdd.getInit())
			{// new vertices are generated using the new matrix, hence min/max vertex IDs remain valid.
				CmpVertex vert = AbstractLearnerGraph.generateNewCmpVertex(matrixResult.nextID(firstVertex.isAccept()), origGraph.config);
				DeterministicDirectedSparseGraph.copyVertexData(firstVertex, vert);
				firstToSecond.put(firstVertex, vert);
			}

		AbstractLearnerGraph.addAndRelabelGraphs(matrixToAdd, firstToSecond, matrixResult);
		return matrixResult;
	}

	@Override
	public MapWithSearch<Label,Label, List<CmpVertex>> createNewRow() {
		if (config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY)
			return new ArrayMapWithSearchPos<>();
		return new TreeMapWithSearch<>();
	}

	@Override
	public void addTransition(MapWithSearch<Label,Label, List<CmpVertex>> row, Label input, CmpVertex target) {
		List<CmpVertex> targets = row.get(input);
		if (targets == null)
		{
			targets = new LinkedList<>();row.put(input, targets);
		}
		else if (targets.contains(target)) throw new IllegalArgumentException("duplicate transition with input "+input+" to "+target);
		targets.add(target);
	}

	@Override
	public LearnerGraphNDCachedData createCache() {
		return new LearnerGraphNDCachedData(this);
	}

	@Override
	public Collection<CmpVertex> getTargets(List<CmpVertex> targ) {
		return targ;
	}

	@Override
	public void removeTransition(MapWithSearch<Label,Label, List<CmpVertex>> row, Label input,
			CmpVertex target) {
		List<CmpVertex> targets = row.get(input);
		if (targets != null)
		{
			targets.remove(target);if (targets.isEmpty()) row.remove(input);
		}
	}

	@Override
	public AbstractLearnerGraph<List<CmpVertex>, LearnerGraphNDCachedData> copy(Configuration conf) 
	{
		AbstractLearnerGraph<List<CmpVertex>, LearnerGraphNDCachedData> result = newInstance(conf);AbstractLearnerGraph.copyGraphs(this, result);return result;
	}

	@Override
	public AbstractLearnerGraph<List<CmpVertex>, LearnerGraphNDCachedData> newInstance(Configuration conf) 
	{
		return new LearnerGraphND(conf);
	}
	
}