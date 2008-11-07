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
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;

class TransitionMatrixND
{
	Map<CmpVertex,Map<String,List<CmpVertex>>> matrix;
	CmpVertex init;
	
	public TransitionMatrixND()
	{
		matrix = new TreeMap<CmpVertex,Map<String,List<CmpVertex>>>();
		init = null;
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
	
	/** Converts a deterministic matrix into a non-deterministic representation, needed for
	 * some methods. */
	protected static TransitionMatrixND convertToND(LearnerGraph graph)
	{
		TransitionMatrixND matrixResult = new TransitionMatrixND();
		LearnerGraphND.buildForward(graph,LearnerGraphND.ignoreNone,matrixResult);
		matrixResult.init = graph.init;
		return matrixResult;
	}

	/** Looks through all the states for the one matching the supplied name and sets the initial state to the found one.
	 * If the supplied state is not found, the initial state is reset to null.
	 * 
	 * @param initStateName name of the initial state.
	 */
	public void findInitialState(String initStateName)
	{// TODO: to unit-test this one
		CmpVertex initVertex = null;
		for(CmpVertex vert:matrix.keySet())
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
	 * @param config the configuration to use when creating the outcome.
	 * @return deterministic version of it.
	 */
	public LearnerGraph buildDeterministicGraph(Configuration config)
	{
		Map<Set<CmpVertex>,EqClass> equivalenceClasses = new HashMap<Set<CmpVertex>,EqClass>();
		/** Maps sets of target states to the corresponding known states. */
		LearnerGraph result = new LearnerGraph(config);result.transitionMatrix.clear();
		CmpVertex newInitialState = LearnerGraph.generateNewCmpVertex(result.nextID(init.isAccept()),config);
		newInitialState.setAccept(init.isAccept());newInitialState.setHighlight(init.isHighlight());newInitialState.setColour(init.getColour());
		EqClass initial = new EqClass(newInitialState);initial.add(init);
		result.init = initial.getVertex();
		//result.transitionMatrix.put(initial.getVertex(), new TreeMap<String,CmpVertex>());
		Queue<EqClass> currentExplorationBoundary = new LinkedList<EqClass>();// FIFO queue containing equivalence classes to be explored

		currentExplorationBoundary.add(initial);equivalenceClasses.put(initial,initial);
		while(!currentExplorationBoundary.isEmpty())
		{
			EqClass currentClass = currentExplorationBoundary.remove();
			Map<String,EqClass> inputToTargetClass = new HashMap<String,EqClass>();
			for(CmpVertex vertex:currentClass)
			{
				for(Entry<String,List<CmpVertex>> transition:matrix.get(vertex).entrySet())
				{
					EqClass targets = inputToTargetClass.get(transition.getKey());
					if (targets == null)
					{
						boolean isAccept = transition.getValue().iterator().next().isAccept();
						targets = new EqClass(LearnerGraph.generateNewCmpVertex(result.nextID(isAccept), config));
						targets.getVertex().setAccept(isAccept);
						inputToTargetClass.put(transition.getKey(),targets);
					}
					targets.addAll(transition.getValue());
				}
			}

			// Now I have iterated through all states in the current class and
			// assembled collections of states corresponding to destination classes.
			
			assert !result.transitionMatrix.containsKey(currentClass.getVertex()) : "duplicate state "+currentClass.getVertex();
			Map<String,CmpVertex> row = new TreeMap<String,CmpVertex>();result.transitionMatrix.put(currentClass.getVertex(),row);
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
		for(CmpVertex firstVertex:matrixToAdd.matrix.keySet())
			if (firstVertex != init)
			{
				CmpVertex vert = LearnerGraph.generateNewCmpVertex(grIds.nextID(firstVertex.isAccept()), origGraph.config);
				vert.setAccept(firstVertex.isAccept());vert.setHighlight(firstVertex.isHighlight());vert.setColour(firstVertex.getColour());
				firstToSecond.put(firstVertex, vert);
			}

		TransitionMatrixND matrixResult = TransitionMatrixND.convertToND(origGraph);
		
		// Add the transitions.
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:matrixToAdd.matrix.entrySet())
		{
			CmpVertex entryKey = firstToSecond.get(entry.getKey());
			Map<String,List<CmpVertex>> row = matrixResult.matrix.get(entryKey);
			if (row == null)
			{
				row = new TreeMap<String,List<CmpVertex>>();matrixResult.matrix.put(entryKey, row);
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
	
}