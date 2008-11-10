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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.rpnicore.LearnerGraph.StatesToConsider;

class TransitionMatrixND extends AbstractTransitionMatrix<List<CmpVertex>>
{
	public TransitionMatrixND(Configuration conf)
	{
		super(conf);
		transitionMatrix = new TreeMap<CmpVertex,Map<String,List<CmpVertex>>>();
		init = null;
	}

	/** Constructs a StateChum graph from a Jung Graph
	 *
	 * @param g the graph to build StateChum graph from
	 * @param conf configuration to use
	 */
	public TransitionMatrixND(Graph g,Configuration conf)
	{
		super(conf);
		initEmpty();
		Map<Vertex,CmpVertex> origToCmp = new HashMap<Vertex,CmpVertex>();
		if (g.containsUserDatumKey(JUConstants.TITLE))
			setName((String)g.getUserDatum(JUConstants.TITLE));
		Set<VertexID> idSet = new HashSet<VertexID>(); 
		
		synchronized (AbstractTransitionMatrix.syncObj) 
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

					if (init != null)
						throw new IllegalArgumentException("vertices "+srcVert+" and "+init+" are both labelled as initial states");
					init = vert;
				}
				else vert = cloneCmpVertex(srcVert,config);
				origToCmp.put(srcVert, vert);

				if (idSet.contains(vert.getID()))
					throw new IllegalArgumentException("multiple states with the same name "+vert.getID());
				idSet.add(vert.getID());
				
				transitionMatrix.put(vert,createNewRow());
			}
		} // synchronized (LearnerGraph.syncObj)

		if (init == null)
			throw new IllegalArgumentException("missing initial state");
		init.setColour(JUConstants.RED);

		Iterator<DirectedSparseEdge> edgeIter = g.getEdges().iterator();
		while(edgeIter.hasNext())
		{	
			DirectedSparseEdge edge = edgeIter.next();
			Map<String,List<CmpVertex>> outgoing = transitionMatrix.get(origToCmp.get(edge.getSource()));
			assert origToCmp.containsKey(edge.getDest());// this cannot fail if we handle normal Jung graphs which will never let me add an edge with vertex not in the graph
			// The line below aims to ensure that inputs are evaluated by computeStateScore in a specific order, which in conjunction with the visited set of computeStateScore permits emulating a bug in computeScore
			createLabelToStateMap((Set<String>)edge.getUserDatum(JUConstants.LABEL),origToCmp.get(edge.getDest()),outgoing);
		}
		
		setIDNumbers();
	}

	protected static class EqClass extends TreeSet<CmpVertex>
	{
		/**
		 * Serialization ID.
		 */
		private static final long serialVersionUID = 6174886417002882065L;
		
		private final CmpVertex dvertex;
		
		/** Each collection of states in a non-deterministic graph corresponds to a specific
		 * state in deterministic graph.
		 * 
		 * @param mergedVertex
		 */
		public EqClass(CmpVertex vertex)
		{
			dvertex = vertex;
		}
		
		public CmpVertex getVertex()
		{
			return dvertex;
		}
	}
	
	/** Ignores all reject-states. */
	public static class ignoreRejectStatesClass implements StatesToConsider
	{
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
		
		public boolean stateToConsider(CmpVertex vert) {
			return graph.transitionMatrix.get(vert).size() > 0;
		}
	}
	
	/** Does not ignore any states. */
	public static class ignoreNoneClass implements StatesToConsider
	{
		public boolean stateToConsider(@SuppressWarnings("unused") CmpVertex vert) {
			return true;
		}
	}
	
	public final static StatesToConsider ignoreNone = new ignoreNoneClass();

	/** Converts a deterministic matrix into a non-deterministic representation, needed for
	 * some methods. */
	public TransitionMatrixND(LearnerGraph graph)
	{
		super(graph.config);
		initEmpty();
		buildForward(graph,ignoreNone,this);
		init = graph.init;
	}

	/** Converts the existing state-transition matrix into the one with 
	 * the signature of <em>transitionMatrixND</em>.
	 * 
	 * @param coregraph the graph to build <em>transitionMatrixND</em> from.
	 * @param filter the filter to use when deciding which states to consider and which to throw away.
	 * @param matrixND matrix to build
	 */
	public static void buildForward(LearnerGraph coregraph,StatesToConsider filter, TransitionMatrixND matrixND)
	{
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
			if (filter.stateToConsider(entry.getKey()))
			{
				Map<String,List<CmpVertex>> entryForState = new TreeMap<String,List<CmpVertex>>();
				for(Entry<String,CmpVertex> transition:entry.getValue().entrySet())
					if (filter.stateToConsider(transition.getValue()))
					{
						List<CmpVertex> targetList = new LinkedList<CmpVertex>();targetList.add(transition.getValue());
						entryForState.put(transition.getKey(), targetList);
					}
				matrixND.transitionMatrix.put(entry.getKey(), entryForState);
			}
		
		// It cannot happen that some target states will not be included in the set
		// of source states because routines building LearnerGraph ensure
		// that all states are mentioned on the left-hand side
		// LearnerGraph's transition matrix.
		matrixND.init = coregraph.init;
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
	public static void buildInverse(TransitionMatrixND graph,StatesToConsider filter, TransitionMatrixND matrixND)
	{
		// First, we fill the map with empty entries - 
		// it is crucially important to fill in all the entries which can be accessed during the triangular exploration, 
		// otherwise holes will lead to the sequence of numbers explored to be discontinuous, causing a failure.
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:graph.transitionMatrix.entrySet())
			if (filter.stateToConsider(entry.getKey()))
				matrixND.transitionMatrix.put(entry.getKey(),matrixND.createNewRow());
		
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:graph.transitionMatrix.entrySet())
			if (filter.stateToConsider(entry.getKey()))
			{
				for(Entry<String,List<CmpVertex>> transition:entry.getValue().entrySet())
					for(CmpVertex target:transition.getValue())
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
		matrixND.init = graph.init;
	}

	/** Looks through all the states for the one matching the supplied name and sets the initial state to the found one.
	 * If the supplied state is not found, the initial state is reset to null.
	 * 
	 * @param initStateName name of the initial state.
	 */
	public void findInitialState(String initStateName)
	{// TODO: to unit-test this one
		CmpVertex initVertex = null;
		for(CmpVertex vert:transitionMatrix.keySet())
			if (vert.getID().toString().contains(initStateName))
			{
				initVertex = vert;break;
			}
		if (initVertex == null)
			throw new IllegalArgumentException("absent initial state");

		init = initVertex;
	}
	
	public static final JUConstants ltlColour = JUConstants.INF_AMBER;

	/** Takes the recorded non-deterministic transition matrix and turns it into
	 * a deterministic one, at the obviously exponential cost.
	 * 
	 * @return deterministic version of it.
	 */
	public LearnerGraph buildDeterministicGraph()
	{
		Map<Set<CmpVertex>,EqClass> equivalenceClasses = new HashMap<Set<CmpVertex>,EqClass>();
		/** Maps sets of target states to the corresponding known states. */
		LearnerGraph result = new LearnerGraph(config.copy());result.transitionMatrix.clear();
		CmpVertex newInitialState = AbstractTransitionMatrix.generateNewCmpVertex(result.nextID(init.isAccept()),config);
		newInitialState.setAccept(init.isAccept());newInitialState.setHighlight(init.isHighlight());newInitialState.setColour(init.getColour());
		EqClass initial = new EqClass(newInitialState);initial.add(init);
		result.init = initial.getVertex();
		//result.transitionMatrix.put(initial.getVertex(), result.createNewRow());
		Queue<EqClass> currentExplorationBoundary = new LinkedList<EqClass>();// FIFO queue containing equivalence classes to be explored

		currentExplorationBoundary.add(initial);equivalenceClasses.put(initial,initial);
		while(!currentExplorationBoundary.isEmpty())
		{
			EqClass currentClass = currentExplorationBoundary.remove();
			Map<String,EqClass> inputToTargetClass = new HashMap<String,EqClass>();
			for(CmpVertex vertex:currentClass)
			{
				for(Entry<String,List<CmpVertex>> transition:transitionMatrix.get(vertex).entrySet())
				{
					EqClass targets = inputToTargetClass.get(transition.getKey());
					if (targets == null)
					{
						boolean isAccept = transition.getValue().iterator().next().isAccept();
						targets = new EqClass(AbstractTransitionMatrix.generateNewCmpVertex(result.nextID(isAccept), config));
						targets.getVertex().setAccept(isAccept);
						inputToTargetClass.put(transition.getKey(),targets);
					}
					targets.addAll(transition.getValue());
				}
			}

			// Now I have iterated through all states in the current class and
			// assembled collections of states corresponding to destination classes.
			
			assert !result.transitionMatrix.containsKey(currentClass.getVertex()) : "duplicate state "+currentClass.getVertex();
			Map<String,CmpVertex> row = result.createNewRow();result.transitionMatrix.put(currentClass.getVertex(),row);
			// Now I need to iterate through those new classes and
			// 1. update the transition diagram.
			// 2. append those I've not yet seen to the exploration stack.
			for(Entry<String,EqClass> transition:inputToTargetClass.entrySet())
			{
				EqClass realTargetState = equivalenceClasses.get(transition.getValue());
				if (realTargetState == null)
				{// this is a new state
					realTargetState = transition.getValue();
					currentExplorationBoundary.offer(realTargetState);
					equivalenceClasses.put(realTargetState,realTargetState);
					
					// Now we need to set colours to Amber if all states are amber.
					boolean allAmber = true;
					for(CmpVertex vertex:realTargetState)
						if (vertex.getColour() != ltlColour)
							allAmber = false;
					if (allAmber) realTargetState.getVertex().setColour(ltlColour);
				}
								
				row.put(transition.getKey(), realTargetState.getVertex());
				//System.out.println("added "+currentClass+" ( "+currentClass.getVertex()+" ) - " + transition.getKey() + " -> "+realTargetState+" ( "+realTargetState.getVertex()+" )");
				// now check that accept conditions are compatible.
				for(CmpVertex v:realTargetState)
					if (v.isAccept() != realTargetState.getVertex().isAccept())
						throw new IllegalArgumentException("inconsistent labelling on transitions from "+currentClass+" - " + transition.getKey() + " -> "+realTargetState);
			}
		}

		result.setName("after making deterministic");
		//Visualiser.updateFrame(result, result);
		return result;
	}

	/** Puts together transitions from a different matrices and returns the result of addition, which
	 * is most likely non-deterministic.
	 */
	protected static TransitionMatrixND UniteTransitionMatrices(
			TransitionMatrixND matrixToAdd, LearnerGraph origGraph)
	{
		CmpVertex init = matrixToAdd.init;
		LearnerGraph grIds = new LearnerGraph(origGraph.config);
		grIds.vertNegativeID = origGraph.vertNegativeID;grIds.vertPositiveID=origGraph.vertPositiveID;
		
		// given that all text identifiers go before (or after) numerical ones, we're not
		// going to hit a state clash if we simply generate state names
		// based on the existing IDs of origGraph. 
		Map<CmpVertex,CmpVertex> firstToSecond = new TreeMap<CmpVertex,CmpVertex>();
		firstToSecond.put(init, origGraph.init);
		for(CmpVertex firstVertex:matrixToAdd.transitionMatrix.keySet())
			if (firstVertex != init)
			{
				CmpVertex vert = AbstractTransitionMatrix.generateNewCmpVertex(grIds.nextID(firstVertex.isAccept()), origGraph.config);
				vert.setAccept(firstVertex.isAccept());vert.setHighlight(firstVertex.isHighlight());vert.setColour(firstVertex.getColour());
				firstToSecond.put(firstVertex, vert);
			}

		TransitionMatrixND matrixResult = new TransitionMatrixND(origGraph);
		
		// Add the transitions.
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:matrixToAdd.transitionMatrix.entrySet())
		{
			CmpVertex entryKey = firstToSecond.get(entry.getKey());
			Map<String,List<CmpVertex>> row = matrixResult.transitionMatrix.get(entryKey);
			if (row == null)
			{
				row = new TreeMap<String,List<CmpVertex>>();matrixResult.transitionMatrix.put(entryKey, row);
			}
			for(Entry<String,List<CmpVertex>> transition:entry.getValue().entrySet())
			{
				List<CmpVertex> targets = row.get(transition.getKey());
				if (targets == null)
				{
					targets = new LinkedList<CmpVertex>();row.put(transition.getKey(), targets);
				}
				for(CmpVertex v:transition.getValue())
					targets.add(firstToSecond.get(v));
			}
		}
		
		return matrixResult;
	}

	@Override
	Map<String, List<CmpVertex>> createNewRow() {
		return new TreeMap<String,List<CmpVertex>>();
	}
	
}