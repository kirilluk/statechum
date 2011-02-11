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

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;

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
		transitionMatrix = new TreeMap<CmpVertex,Map<String,List<CmpVertex>>>();
		setInit(null);
		initPTA();
	}

	/** Constructs a StateChum graph from a Jung Graph
	 *
	 * @param g the graph to build StateChum graph from
	 * @param conf configuration to use
	 */
	public LearnerGraphND(Graph g,Configuration conf)
	{
		super(conf);
		initEmpty();
		Map<Vertex,CmpVertex> origToCmp = new HashMap<Vertex,CmpVertex>();
		if (g.containsUserDatumKey(JUConstants.TITLE))
			setName((String)g.getUserDatum(JUConstants.TITLE));
		Set<VertexID> idSet = new HashSet<VertexID>(); 
		
		synchronized (AbstractLearnerGraph.syncObj) 
		{
			for(Vertex srcVert:(Set<Vertex>)g.getVertices())
			{
				CmpVertex vert = null;
				if (DeterministicDirectedSparseGraph.isInitial(srcVert))// special case for the initial vertex.
				{
					vert = cloneCmpVertex(srcVert,config);//generateNewCmpVertex(getDefaultInitialPTAName(),config);
					Object property = srcVert.getUserDatum(JUConstants.INITIAL);
					if (!(property instanceof Boolean) || !((Boolean)property).booleanValue())
						throw new IllegalArgumentException("invalid init property");

					if (getInit() != null)
						throw new IllegalArgumentException("vertices "+srcVert+" and "+getInit()+" are both labelled as initial states");
					setInit(vert);
				}
				else vert = cloneCmpVertex(srcVert,config);
				origToCmp.put(srcVert, vert);

				if (idSet.contains(vert.getID()))
					throw new IllegalArgumentException("multiple states with the same name "+vert.getID());
				idSet.add(vert.getID());
				
				transitionMatrix.put(vert,createNewRow());
			}
		} // synchronized (LearnerGraph.syncObj)

		if (getInit() == null)
			throw new IllegalArgumentException("missing initial state");

		Iterator<DirectedSparseEdge> edgeIter = g.getEdges().iterator();
		while(edgeIter.hasNext())
		{	
			DirectedSparseEdge edge = edgeIter.next();
			Map<String,List<CmpVertex>> outgoing = transitionMatrix.get(origToCmp.get(edge.getSource()));
			assert origToCmp.containsKey(edge.getDest());// this cannot fail if we handle normal Jung graphs which will never let me add an edge with vertex not in the graph
			// The line below aims to ensure that inputs are evaluated by computeStateScore in a specific order, which in conjunction with the visited set of computeStateScore permits emulating a bug in computeScore
			createLabelToStateMap((Set<String>)edge.getUserDatum(JUConstants.LABEL),origToCmp.get(edge.getDest()),outgoing);
		}
		
		PairCompatibility<Vertex> compat = (PairCompatibility<Vertex>)g.getUserDatum(JUConstants.PAIR_COMPATIBILITY);
		if (compat != null)
			PairCompatibility.copyTo(compat, pairCompatibility, origToCmp);
		
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
	 * will never have positive scores, either INCOMPATIBLE (if the former is an accept)
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
	@SuppressWarnings("unchecked") // I aim to convert any kind of graph to this type.
	public LearnerGraphND(AbstractLearnerGraph matrixND, Configuration argConfig)
	{
		super(argConfig);
		AbstractLearnerGraph.copyGraphs(matrixND, this);
	}

	/** Converts the existing state-transition matrix into the one with 
	 * the signature of <em>transitionMatrixND</em>.
	 * 
	 * @param coregraph the graph to build <em>transitionMatrixND</em> from.
	 * @param filter the filter to use when deciding which states to consider and which to throw away.
	 * @param matrixND matrix to build
	 */
	public static <TARGET_A_TYPE,CACHE_A_TYPE extends CachedData<TARGET_A_TYPE,CACHE_A_TYPE>>
		void buildForward(AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> coregraph,StatesToConsider filter, LearnerGraphND matrixND)
	{
		for(Entry<CmpVertex,Map<String,TARGET_A_TYPE>> entry:coregraph.transitionMatrix.entrySet())
			if (filter.stateToConsider(entry.getKey()))
			{
				Map<String,List<CmpVertex>> entryForState = new TreeMap<String,List<CmpVertex>>();
				for(Entry<String,TARGET_A_TYPE> transition:entry.getValue().entrySet())
					for(CmpVertex targetState:coregraph.getTargets(transition.getValue()))
						if (filter.stateToConsider(targetState))
						{
							List<CmpVertex> targetList = new LinkedList<CmpVertex>();targetList.add(targetState);
							entryForState.put(transition.getKey(), targetList);
						}
				matrixND.transitionMatrix.put(entry.getKey(), entryForState);
			}
		
		// It cannot happen that some target states will not be included in the set
		// of source states because routines building LearnerGraph ensure
		// that all states are mentioned on the left-hand side
		// LearnerGraph's transition matrix.
		
		assert matrixND.findVertex(coregraph.getInit().getID()) != null : "initial state was filtered out";
		matrixND.setInit(coregraph.getInit());
	}
	
	/** Builds a (non-deterministic in general) transition matrix where all 
	 * transitions point in an opposite direction to the current one. 
	 * The matrix produced is used to scan  the state comparison matrix columnwise.
	 *
	 * @param graph the graph to build <em>transitionMatrixND</em> from.
	 * @param filter the filter to use when deciding which states 
	 * to consider and which to throw away.
	 * @param matrixND matrix to build
	 */
	public static <TARGET_A_TYPE,CACHE_A_TYPE extends CachedData<TARGET_A_TYPE,CACHE_A_TYPE>>
		void buildInverse(AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> graph,StatesToConsider filter, LearnerGraphND matrixND)
	{
		// First, we fill the map with empty entries - 
		// it is crucially important to fill in all the entries which can be accessed during the triangular exploration, 
		// otherwise holes will lead to the sequence of numbers explored to be discontinuous, causing a failure.
		for(Entry<CmpVertex,Map<String,TARGET_A_TYPE>> entry:graph.transitionMatrix.entrySet())
			if (filter.stateToConsider(entry.getKey()))
				matrixND.transitionMatrix.put(entry.getKey(),matrixND.createNewRow());
		
		for(Entry<CmpVertex,Map<String,TARGET_A_TYPE>> entry:graph.transitionMatrix.entrySet())
			if (filter.stateToConsider(entry.getKey()))
			{
				for(Entry<String,TARGET_A_TYPE> transition:entry.getValue().entrySet())
					for(CmpVertex target:graph.getTargets(transition.getValue()))
						if (filter.stateToConsider(target))
						{
							Map<String,List<CmpVertex>> row = matrixND.transitionMatrix.get(target);
							List<CmpVertex> sourceStates = row.get(transition.getKey());
							if (sourceStates == null)
							{
								sourceStates=new LinkedList<CmpVertex>();row.put(transition.getKey(), sourceStates);
							}
							sourceStates.add(entry.getKey());
						}
			}
		if (matrixND.transitionMatrix.isEmpty()) matrixND.setInit(null);
		else matrixND.setInit(graph.getInit());
	}

	/** Looks through all the states for the one matching the supplied name and sets the initial state to the found one.
	 * If the supplied state is not found, the initial state is reset to null.
	 * 
	 * @param initStateName name of the initial state.
	 */
	public void findInitialState(String initStateName)
	{
		CmpVertex initVertex = null;
		for(CmpVertex vert:transitionMatrix.keySet())
			if (vert.getID().toString().contains(initStateName))
			{
				initVertex = vert;break;
			}
		if (initVertex == null)
			throw new IllegalArgumentException("absent initial state");

		setInit(initVertex);
	}
	
	public static final JUConstants ltlColour = JUConstants.INF_AMBER;

	/** Puts together transitions from a different matrices and returns the result of addition, which
	 * is most likely non-deterministic.
	 */
	protected static 
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
		Map<CmpVertex,CmpVertex> firstToSecond = new TreeMap<CmpVertex,CmpVertex>();
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
	public
	Map<String, List<CmpVertex>> createNewRow() {
		return new TreeMap<String,List<CmpVertex>>();
	}

	@Override
	public void addTransition(Map<String, List<CmpVertex>> row, String input, CmpVertex target) {
		List<CmpVertex> targets = row.get(input);
		if (targets == null)
		{
			targets = new LinkedList<CmpVertex>();row.put(input, targets);
		}
		else if (targets.contains(target)) throw new IllegalArgumentException("duplicate transition with input "+input+" to "+target);
		targets.add(target);
	}

	@Override
	public LearnerGraphNDCachedData createCache() {
		return new LearnerGraphNDCachedData(this);
	}

	@Override
	Map<CmpVertex, Map<String, List<CmpVertex>>> createNewTransitionMatrix() {
		return new TreeMap<CmpVertex, Map<String, List<CmpVertex>>>();
	}

	@Override
	Collection<CmpVertex> getTargets(List<CmpVertex> targ) {
		return targ;
	}

	@Override
	public
	void removeTransition(Map<String, List<CmpVertex>> row, String input,
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