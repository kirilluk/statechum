/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

import java.awt.Color;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import statechum.ArrayOperations;
import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicEdge;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.StatePair;
import statechum.model.testset.PTAExploration;
import statechum.model.testset.PTASequenceEngine;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

public class PathRoutines {
	final LearnerGraph coregraph;
	
	/** Associates this object to ComputeStateScores it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	PathRoutines(LearnerGraph computeStateScores) 
	{
		coregraph = computeStateScores;
	}

	/** Computes the ratio of edges in the graph from non-amber states 
	 * to the total number of possible edges from non-amber states. 
	 */
	public double getExtentOfCompleteness()
	{
		int normalEdgeCount = 0, stateNumber=0;
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
			if (entry.getKey().isAccept() && entry.getKey().getColour() != JUConstants.AMBER) 
			{
				normalEdgeCount+=entry.getValue().size();++stateNumber;
			}
		
		if (stateNumber == 0 || coregraph.learnerCache.getAlphabet().size() == 0)
			return 0;
		return (double)normalEdgeCount/( stateNumber * coregraph.learnerCache.getAlphabet().size());
	}
	

	/** Computes some of the possible shortest paths from the supplied source state to the supplied target state 
	 * and returns a sequence of possible sets inputs which can be followed. In other words, 
	 * a choice of any input from each of the returned sets gives a possible path between
	 * the requested vertices.
	 * 
	 * @param vertSource the source state
	 * @param vertTarget the target state
	 * @return sequences of inputs to follow all paths found. Null if a path is not found and an empty list if the target vertex is the same as the source one
	 */	
	List<Collection<String>> COMPAT_computePathsSBetween(CmpVertex vertSource, CmpVertex vertTarget)
	{
		List<Collection<String>> sequenceOfSets = null;

		Set<CmpVertex> visitedStates = new HashSet<CmpVertex>();visitedStates.add(vertSource);
		LinkedList<CmpVertex> initPath = new LinkedList<CmpVertex>();initPath.add( vertSource );
		Queue<LinkedList<CmpVertex>> currentExplorationPath = new LinkedList<LinkedList<CmpVertex>>();// FIFO queue containing paths to states to be explored
		currentExplorationPath.add(initPath);
		LinkedList<CmpVertex> currentPath = null;CmpVertex currentVert = null;
		while(currentVert != vertTarget && !currentExplorationPath.isEmpty())
		{
			currentPath = currentExplorationPath.remove();
			currentVert = currentPath.getLast();
			if (currentVert != vertTarget)
				// we have not reached the red state, yet
				for(CmpVertex targetVertex:coregraph.transitionMatrix.get(currentVert).values())
					if (!visitedStates.contains(targetVertex))
					{
						LinkedList<CmpVertex> newPath = new LinkedList<CmpVertex>();newPath.addAll(currentPath);newPath.add(targetVertex);
						currentExplorationPath.offer(newPath);
						visitedStates.add(currentVert);
					}
		}

		if (currentVert == vertTarget && vertTarget != null)
		{// the path to the red state has been found.
			sequenceOfSets = new LinkedList<Collection<String>>();
			Iterator<CmpVertex> vertIt = currentPath.iterator();
			CmpVertex prevVert = vertIt.next();
			while(vertIt.hasNext())
			{
				currentVert = vertIt.next();
				List<String> inputsToMultWith = new LinkedList<String>();
				for(Entry<String,CmpVertex> entry:coregraph.transitionMatrix.get(prevVert).entrySet())
					if (entry.getValue() == currentVert)
						inputsToMultWith.add(entry.getKey());
				sequenceOfSets.add(inputsToMultWith);
				prevVert = currentVert;
			}
		}
		
		return sequenceOfSets;
	}


	/** Computes all possible shortest paths from the supplied source state to the 
	 * supplied target state and returns a PTA corresponding to them. The easiest 
	 * way to record the numerous computed paths is by using PTATestSequenceEngine-derived classes;
	 * this also permits one to trace them in some automaton and junk irrelevant ones.
	 * 
	 * @param vertSource the source state
	 * @param vertTarget the target state
	 * @param pathsToVertSource PTA of paths to enter vertSource, can be initialised with identity 
	 * or obtained using PTATestSequenceEngine-related operations.
	 * @param nodes of a PTA corresponding to the entered states, to which resulting nodes will be added (this method 
	 * cannot create an empty instance of a sequenceSet (which is why it has to be passed one), perhaps for a reason).
	 */	
	public void ORIGcomputePathsSBetween(CmpVertex vertSource, CmpVertex vertTarget,
			PTASequenceEngine.SequenceSet pathsToVertSource,
			PTASequenceEngine.SequenceSet result)
	{
		if (vertSource == null || vertTarget == null || pathsToVertSource == null)
			throw new IllegalArgumentException("null arguments to computePathsSBetween");
		if (GlobalConfiguration.getConfiguration().isAssertEnabled())
			if (!coregraph.transitionMatrix.containsKey(vertSource) || !coregraph.transitionMatrix.containsKey(vertTarget))
				throw new IllegalArgumentException("either source or target vertex is not in the graph");
		
		Set<CmpVertex> visitedStates = new HashSet<CmpVertex>();visitedStates.add(vertSource);
		
		// FIFO queue containing sequences of states labelling paths to states to be explored.
		// Important, after processing of each wave, we add a null, in order to know when
		// to stop when scanning to the end of the current wave when a path to the target state
		// has been found.
		Queue<List<CmpVertex>> currentExplorationPath = new LinkedList<List<CmpVertex>>();
		Queue<CmpVertex> currentExplorationState = new LinkedList<CmpVertex>();
		if (vertSource == vertTarget)
		{
			result.unite(pathsToVertSource);
			return;// nothing to do, return paths to an initial state.
		}
		
		currentExplorationPath.add(new LinkedList<CmpVertex>());currentExplorationState.add(vertSource);
		currentExplorationPath.offer(null);currentExplorationState.offer(null);// mark the end of the first (singleton) wave.
		CmpVertex currentVert = null;List<CmpVertex> currentPath = null;
		boolean pathFound = false;
		while(!currentExplorationPath.isEmpty())
		{
			currentVert = currentExplorationState.remove();currentPath = currentExplorationPath.remove();
			if (currentVert == null)
			{// we got to the end of a wave
				if (pathFound)
					break;// if we got to the end of a wave and the target vertex has been found on some paths in this wave, stop scanning.
				else
					if (currentExplorationPath.isEmpty())
						break;// we are at the end of the last wave, stop looping.
					else
					{// mark the end of a wave.
						currentExplorationPath.offer(null);currentExplorationState.offer(null);
					}
			}
			else
			{
				visitedStates.add(currentVert);
				for(CmpVertex nextVertex:coregraph.transitionMatrix.get(currentVert).values())
				{
					if (nextVertex == vertTarget)
					{// found the vertex we are looking for
						pathFound = true;
						// now we need to go through all our states in a path and update pathsToVertSource
						PTASequenceEngine.SequenceSet paths = pathsToVertSource;
						CmpVertex curr = vertSource;Collection<String> inputsToMultWith = new LinkedList<String>();
						
						// process all but one vertices
						for(CmpVertex tgt:currentPath)
						{// ideally, I'd update one at a time and merge results, but it seems the same (set union) if I did it by building a set of inputs and did a cross with it.
							inputsToMultWith.clear();
							for(Entry<String,CmpVertex> entry:coregraph.transitionMatrix.get(curr).entrySet())
								if (entry.getValue() == tgt)
									inputsToMultWith.add(entry.getKey());
							paths = paths.crossWithSet(inputsToMultWith);
							curr = tgt;
						}
						inputsToMultWith.clear();
						// now the last pass for the target vertex
						for(Entry<String,CmpVertex> entry:coregraph.transitionMatrix.get(curr).entrySet())
							if (entry.getValue() == nextVertex)
								inputsToMultWith.add(entry.getKey());
						result.unite( paths.crossWithSet(inputsToMultWith) );// update the result.
					}
					else
					if (!visitedStates.contains(nextVertex))
					{
						List<CmpVertex> newPath = new LinkedList<CmpVertex>();newPath.addAll(currentPath);newPath.add(nextVertex);
						currentExplorationPath.offer(newPath);currentExplorationState.offer(nextVertex);
					}
				}
			}
		}

		if (!pathFound)
			throw new IllegalArgumentException("path from state "+vertSource+" to state "+vertTarget+" was not found");
		
		return ;
	}

	public LearnerGraph augmentPTA(Collection<List<String>> strings, boolean accepted, boolean maximalAutomaton)
	{
		for(List<String> sequence:strings)
			augmentPTA(sequence, accepted,maximalAutomaton,null);
		return coregraph;
	}
	
	/** Given a path and whether it is an accept or reject-path adds this path to the graph. 
	 * An accept-path added in its entirety; reject-path has the last node as reject and the rest are accept ones.
	 * <p>
	 * The special case is when we add a path to a maximal automaton which is accepting a language which is not
	 * prefix-closed. For this reason, when a reject path is added which clashes with an existing path,
	 * the existing path is truncated at the new reject-node. Repeated addition of reject sequences can hence
	 * render parts of the state space inaccessible; this is to be expected and can be easily removed by computing
	 * a state cover (as a map of states->sequences) and removing all state not mentioned in it. 
	 * 
	 * @param sequence the path to add
	 * @param accepted whether the last element is accept or reject.
	 * @param maxAutomaton whether to interpret the current automaton as a maximal automaton 
	 * (where a reject-node overrides an accept one)
	 * @param newColour the colour to assign to all new vertices, usually this should be left at null.
	 * @return the current (updated) graph. 
	 */
	public LearnerGraph augmentPTA(List<String> sequence, boolean accepted, boolean maximalAutomaton, JUConstants newColour)
	{
		CmpVertex currentState = coregraph.init, prevState = null;
		Iterator<String> inputIt = sequence.iterator();
		String lastInput = null;
		int position = 0;
		while(inputIt.hasNext() && currentState != null)
		{
			if (!currentState.isAccept())
			{// not the last state and the already-reached state is not accept, while all prefixes of reject sequences should be accept ones. 
				currentState.setHighlight(true);
				throw new IllegalArgumentException("incompatible "+
						(accepted?"accept":"reject")+" labelling: "+sequence.subList(0, position)+" when trying to append "+sequence);
			}
			prevState = currentState;lastInput = inputIt.next();++position;
			
			currentState = coregraph.transitionMatrix.get(prevState).get(lastInput);
		}
		
		if (currentState == null)
		{// the supplied path does not exist in PTA, the first non-existing vertex is from state prevState with label lastInput

			synchronized (AbstractLearnerGraph.syncObj) 
			{
				while(inputIt.hasNext())
				{
					prevState = coregraph.addVertex(prevState, true, lastInput);prevState.setColour(newColour);
					prevState.setColour(newColour);prevState.setDepth(position++);
					lastInput = inputIt.next();
				}
				// at this point, we are at the end of the sequence. Last vertex is prevState and last input if lastInput
				CmpVertex newVertex = coregraph.addVertex(prevState, accepted, lastInput);
				newVertex.setColour(newColour);newVertex.setDepth(position++);
			}
			
		}
		else
		{// we reached the end of the string to add to the PTA, with currentState being the current PTA state.
			
			if (!maximalAutomaton)
			{
				if (currentState.isAccept() != accepted)
				{
					currentState.setHighlight(true);
					throw new IllegalArgumentException("incompatible "+(accepted?"accept":"reject")+" labelling: "+sequence.subList(0, position));
				}
			}
			else
			{// the case when a path is being added to a maximal automaton
				if (accepted && !currentState.isAccept())
				{
					currentState.setHighlight(true);
					throw new IllegalArgumentException("incompatible "+(accepted?"accept":"reject")+" labelling: "+sequence.subList(0, position));
				}

				if (!accepted && currentState.isAccept())
				{
					if (prevState != null)
						// truncate the current path, as long as it is not empty
						synchronized (AbstractLearnerGraph.syncObj) 
						{
							coregraph.removeTransition(coregraph.transitionMatrix.get(prevState), lastInput, currentState);
							CmpVertex newVertex = coregraph.addVertex(prevState, accepted, lastInput);
							newVertex.setColour(newColour);newVertex.setDepth(position++);
						}
					else
					{// for an empty path, set the current (i.e. initial state) to a reject-state and clear outgoing transitions.
						currentState.setAccept(false);coregraph.transitionMatrix.get(currentState).clear();
					}
				}
			}
		}
	
		coregraph.learnerCache.invalidate();
		return coregraph;
	}
	
	/** Adds all paths from a supplied PTA to the graph. Whether a path is an accept or a 
	 * reject one is determined by looking at that tail node of the PTA supplied. 
	 * An accept-path added in its entirety; reject-path has the last node as reject and the rest are accept ones.
	 * 
	 * @param graph the graph to update
	 */
	public LearnerGraph augmentPTA(PTASequenceEngine engine)
	{
		PTAExploration<CmpVertex> exploration = new PTAExploration<CmpVertex>(engine) {
			@Override
			public CmpVertex newUserObject() {
				return null;
			}
	
			public void addVertex(PTAExplorationNode currentNode,LinkedList<PTAExplorationNode> pathToInit, boolean accepted) 
			{
				CmpVertex ourVertex = null;
				if (pathToInit.isEmpty())
				{// processing the first vertex in a PTA
					ourVertex = coregraph.init;
				}
				else
				{// not the first vertex in a PTA
					PTAExplorationNode prevNode = pathToInit.getFirst();
					ourVertex = coregraph.transitionMatrix.get(prevNode.userObject).get(prevNode.getInput());
					if (ourVertex == null)
					{// this one will happily add lots of reject nodes along the same path, however this cannot
					 // happen because due to the way addVertex is used by nodeEntered and leafEntered
					 // accept == true everyone other than the last node in a path.
						synchronized (AbstractLearnerGraph.syncObj) 
						{
							ourVertex = coregraph.addVertex(prevNode.userObject, accepted, prevNode.getInput());
							ourVertex.setDepth(pathToInit.size());
						}
					}
				}
				currentNode.userObject = ourVertex;

				if (ourVertex.isAccept() != accepted)
				{ 
					ourVertex.setHighlight(true);
					boolean first = true;
					StringBuffer result = new StringBuffer();
					for(PTAExplorationNode node:pathToInit) { if (first) first=false;else result.insert(0,ArrayOperations.separator);result.insert(0,node.getInput()); }
					throw new IllegalArgumentException("incompatible "+(accepted?"accept":"reject")+" labelling: "+result);
				}					
			}
	
			@Override
			public void nodeEntered(PTAExplorationNode currentNode,LinkedList<PTAExplorationNode> pathToInit) {
				addVertex(currentNode, pathToInit,true);// all states which are not leaf (tail) nodes should be accept states.
			}
			
			@Override
			public void leafEntered(PTAExplorationNode currentNode,	LinkedList<PTAExplorationNode> pathToInit) 
			{
				addVertex(currentNode, pathToInit,currentNode.shouldBeReturned());// this is a leaf node, whether accept or 
					// not depends on its labelling by the PTA; this one is determined 
					// via shouldBeReturned method since the regular isAccept is reserved 
					// to determine whether paths in a PTA grow. 
			}
	
			@Override
			public void nodeLeft(
					@SuppressWarnings("unused") PTAExplorationNode currentNode,
					@SuppressWarnings("unused")	LinkedList<PTAExplorationNode> pathToInit) 
			{// nothing to do here but this method is needed to implement an interface.
			}
		};
		exploration.walkThroughAllPaths();
		coregraph.learnerCache.invalidate();
		return coregraph;
	}

	/** The original version of the routine which builds a Jung graph 
	 * corresponding to the state machine stored in transitionMatrix.
	 * This one does not support configuration and anything of its sort
	 * but is useful for testing of the "real" getGraph. 
	 * It should do the same as getGraph in the "clone graph" configuration.
	 *  
	 * @param the name to give to the graph to be built.
	 * @return constructed graph.
	 */
	public DirectedSparseGraph OrigGetGraph(String name)
	{
		DirectedSparseGraph result = null;
		synchronized (AbstractLearnerGraph.syncObj) 
		{
			result = new DirectedSparseGraph();
			if (name != null)
				result.setUserDatum(JUConstants.TITLE, name,UserData.SHARED);
			Map<CmpVertex,Map<CmpVertex,Set<String>>> flowgraph = coregraph.pathroutines.getFlowgraph();
			Map<CmpVertex,DeterministicVertex> oldToNew = new HashMap<CmpVertex,DeterministicVertex>();
			// add states
			for(Entry<CmpVertex,Map<CmpVertex,Set<String>>> entry:flowgraph.entrySet())
			{
				CmpVertex source = entry.getKey();
				DeterministicVertex vert = new DeterministicVertex(source.getID());
				if (coregraph.init == source)
					vert.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
				vert.setAccept(source.isAccept());
				vert.setColour(source.getColour());
				vert.setHighlight(source.isHighlight());
				result.addVertex(vert);
				oldToNew.put(source,vert);
			}
			
			// now add transitions
			for(Entry<CmpVertex,Map<CmpVertex,Set<String>>> entry:flowgraph.entrySet())
			{
				DeterministicVertex source = oldToNew.get(entry.getKey());
				for(Entry<CmpVertex,Set<String>> tgtEntry:entry.getValue().entrySet())
				{
					DeterministicVertex target = oldToNew.get(tgtEntry.getKey());
					DeterministicEdge e = new DeterministicEdge(source,target);
					e.addUserDatum(JUConstants.LABEL, tgtEntry.getValue(), UserData.CLONE);
					result.addEdge(e);
				}
			}
		}
		return result;
	}
	
	/** Calculates statistics on the learned machine
	 * 
	 * @param computeW whether to compute the W set - not a good idea on 13500 state PTA, for instance.
	 * @return statistics
	 */
	public String getStatistics(boolean computeW) {
		int edgeCounter = 0;
		for(Entry<CmpVertex,Map<String,CmpVertex>> v:coregraph.transitionMatrix.entrySet())
			edgeCounter+=v.getValue().size();
		
		LearnerGraph fsm = new LearnerGraph(coregraph.pathroutines.getGraph(), coregraph.config);
		
		String wsetDetails = "";
		try
		{
			if (computeW) wsetDetails = "Wset: "+WMethod.computeWSet_reducedmemory(fsm).size()+" seq";
		}
		catch (statechum.analysis.learning.rpnicore.WMethod.EquivalentStatesException e) {
			wsetDetails = e.toString();
		}
		return "vert: "+coregraph.transitionMatrix.keySet().size()+" edges: "+edgeCounter+" alphabet: "+fsm.pathroutines.computeAlphabet().size()+" unreachable: "+fsm.pathroutines.checkUnreachableStates()+" "+wsetDetails;
	}

	protected static void checkPTAConsistency(LearnerGraph original, CmpVertex blueState)
	{
		assert GlobalConfiguration.getConfiguration().isAssertEnabled() : "this one should not run when not under test";
		Queue<CmpVertex> currentBoundary = new LinkedList<CmpVertex>();// FIFO queue containing vertices to be explored
		Set<CmpVertex> ptaStates = new HashSet<CmpVertex>();
		currentBoundary.add( blueState );
		while(!currentBoundary.isEmpty())
		{
			CmpVertex current = currentBoundary.remove();
			if (ptaStates.contains(current))
				throw new IllegalArgumentException("PTA has multiple paths to "+current);
			ptaStates.add(current);
			if (GlobalConfiguration.getConfiguration().isAssertEnabled())
				if (!original.transitionMatrix.containsKey(current))
					throw new IllegalArgumentException("the original machine does not contain transitions for state "+current);
			
			for(Entry<String,CmpVertex> input_and_target:original.transitionMatrix.get(current).entrySet())
				currentBoundary.offer(input_and_target.getValue());
		}
		
		// now check that no existing states refer to PTA states
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:original.transitionMatrix.entrySet())
			if (!ptaStates.contains(entry.getKey()))
			{// this is a non-pta state, check where it points to
				for(Entry<String,CmpVertex> input_and_target:entry.getValue().entrySet())
					if (ptaStates.contains(input_and_target.getValue()) && input_and_target.getValue() != blueState)
						throw new IllegalArgumentException("non-pta state "+entry.getKey()+" ("+entry.getKey().getColour()+") refers to PTA state "+input_and_target.getValue()+", blue state is "+blueState);
			}									
	}
	
	/** Checks that non-red states form a tree, i.e. they have exactly one 
	 * incoming edge and there are no
	 * disconnected states. Arguments after mergeResult are used to check 
	 * what happened when the mergeResult fails to be a tree.
	 * 
	 * @param mergeResult merged automaton
	 * @param original the original one
	 * @param pair the pair being merged
	 * @param notRemoved set of PTA states which had transitions with new inputs entering 
	 * them from those PTA states which were merged with either red states or other PTA states. 
	 */
	protected static void checkPTAIsTree(LearnerGraph mergeResult,LearnerGraph original, 
			StatePair pair,Collection<CmpVertex> notRemoved)
	{
		assert GlobalConfiguration.getConfiguration().isAssertEnabled() : "this one should not run when not under test";
	
		// The first check: every state of a merged PTA contains only one incoming transition,
		// assuming that only those labelled RED can have multiple incoming transitions. Given that
		// merging routines merge PTA states _into_ the original ones, thus preserving the red colour,
		// those left with blue colour or without any have to be PTA parts. 
		Map<CmpVertex,AtomicInteger> hasIncoming = new HashMap<CmpVertex,AtomicInteger>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:mergeResult.transitionMatrix.entrySet())
			for(Entry<String,CmpVertex> targ:entry.getValue().entrySet())
			{
				if (!hasIncoming.containsKey(targ.getValue()))
					hasIncoming.put(targ.getValue(), new AtomicInteger(1));
				else
					hasIncoming.get(targ.getValue()).addAndGet(1);
			}
		for(Entry<CmpVertex,AtomicInteger> p:hasIncoming.entrySet())
			if (p.getValue().intValue() > 1 &&
					p.getKey().getColour() != null && p.getKey().getColour() != JUConstants.RED)
				throw new IllegalArgumentException("non-red vertex "+p.getKey()+" has multiple incoming transitions");
				
		// The second check: trying to find states which have become unreachable in the course of merging
		// but were not removed at the end of the merging process. 
		Queue<CmpVertex> currentBoundary = new LinkedList<CmpVertex>();// FIFO queue containing vertices to be explored
		Set<CmpVertex> visitedStates = new HashSet<CmpVertex>();
		currentBoundary.add( mergeResult.init );visitedStates.add(mergeResult.init);
		while(!currentBoundary.isEmpty())
		{
			CmpVertex current = currentBoundary.remove();
			for(Entry<String,CmpVertex> input_and_target:mergeResult.transitionMatrix.get(current).entrySet())
				if (!visitedStates.contains(input_and_target.getValue()))
				{
					visitedStates.add(input_and_target.getValue());
					currentBoundary.offer(input_and_target.getValue());
				}
		}
		
		Set<CmpVertex> unreachables = new HashSet<CmpVertex>();unreachables.addAll(mergeResult.transitionMatrix.keySet());
		unreachables.removeAll(visitedStates);
		if (!unreachables.isEmpty())
		{// some states appear unreachable, starting investigation.
			if( original == null)
				throw new IllegalArgumentException("vertices "+unreachables.toString()+" are unreachable");
			
			
			currentBoundary.clear();// FIFO queue containing vertices to be explored
			visitedStates.clear();
			currentBoundary.add( pair.getQ() );visitedStates.add(pair.getQ());
			while(!currentBoundary.isEmpty())
			{
				CmpVertex current = currentBoundary.remove();
				for(Entry<String,CmpVertex> input_and_target:original.transitionMatrix.get(current).entrySet())
					if (!visitedStates.contains(input_and_target.getValue()))
					{
						visitedStates.add(input_and_target.getValue());
						currentBoundary.offer(input_and_target.getValue());
					}
			}
			
			// now visitedStates contains a set of states originally reachable from the blue state before merging.
			
			Set<CmpVertex> remaining = new HashSet<CmpVertex>();remaining.addAll(unreachables);
			remaining.removeAll(visitedStates);// this one computes the set of states which are 
			// unreachable in the merged machine and do not belong to the original PTA (it is possible to compare
			// vertices directly because they all originate from the same big PTA we started from before 
			// merging was initiated; btw, even if not, they would have identical names, being CmpVertex and
			// most likely DeterministicVertex too).  
			if (remaining.isEmpty())
			{// all unreachable vertices came from the original PTA
				remaining.clear();remaining.addAll(unreachables);remaining.removeAll(notRemoved);
				// Now remaining is the set of unreachable vertices which should've been removed but apparently were not.
				// Possibly, they have incoming transitions (if a chunk of a tree became orphaned, it will surely
				// not get merged into anything).
				String response = "vertices "+unreachables.toString()+" are unreachable and all of them are PTA vertices; the following were for some reason not removed "+remaining+"\n";
				for(CmpVertex u:unreachables)
					response=response+" "+u+"("+hasIncoming.get(u)+")";
				System.out.println(response);
				
				CmpVertex InterestingUnreachableVertex = unreachables.iterator().next();
				List<String> seq = original.pathroutines.computePathsBetween(pair.getQ(), InterestingUnreachableVertex).iterator().next();
				System.out.println(seq);// dumps a seq from a blue state to the first unreachable vertex (after merging) which was on the PTA in the original machine.
				Map<CmpVertex,List<CmpVertex>> mergedVertices = new HashMap<CmpVertex,List<CmpVertex>>();
				if (original.pairscores.computePairCompatibilityScore_internal(pair,mergedVertices) < 0)
					throw new IllegalArgumentException("elements of the pair are incompatible in the original machine, no idea why they got merged in the first place");
				
				System.out.println(pair);
				
				// Now I could dump what was merged and where, in order to try to trace what happened.
				// This is done by traversing a branch of a tree (from the blue state to the unreachable one)
				// which is used by the merging algorithm and dumping the results.
				CmpVertex v = pair.getR();
				Iterator<String> seqIt = seq.iterator();
				while(seqIt.hasNext() && v != null)
				{
					String input = seqIt.next();
					System.out.print(v.toString()+" "+original.transitionMatrix.get(v).keySet()+" input "+input+" ");
					List<CmpVertex> extra = mergedVertices.get(v);
					if (extra != null) 
						for(CmpVertex ev:extra)
							System.out.print(" "+ev.toString()+" : "+original.transitionMatrix.get(ev).keySet());
					System.out.println();
					CmpVertex newV = original.transitionMatrix.get(v).get(input);
					if (newV != null) v=newV;
					else v=original.pairscores.findNextRed(mergedVertices,v,input);
				}
				System.out.println("final state is "+v);

				v=pair.getR();
				seqIt = seq.iterator();
				while(seqIt.hasNext() && v != null)
				{
					String input = seqIt.next();
					System.out.println(v.toString()+" "+mergeResult.transitionMatrix.get(v).keySet()+" input "+input+" ");
					v = mergeResult.transitionMatrix.get(v).get(input);
				}
				System.out.println("final state is "+v);
				
				throw new IllegalArgumentException(response);
				
			}
			
			throw new IllegalArgumentException("vertices "+unreachables.toString()+" are unreachable and "+remaining+" are non-PTA vertices");
				
		}
	}

	/** Navigates a path from the initial state and either returns 
	 * RPNIBlueFringeLearner.USER_ACCEPTED if it is a valid path or the
	 * position of the first element on the path which does not 
	 * exist.
	 * @param path path to traverse
	 * @param startState the state to start from
	 * @return either RPNIBlueFringeLearner.USER_ACCEPTED or the number
	 * of the first non-existing element.
	 */
	public int tracePath(List<String> path)
	{
		return tracePath(path,coregraph.init);
	}
	
	/** Navigates a path from the supplied state and either returns 
	 * RPNIBlueFringeLearner.USER_ACCEPTED if it is a valid path or the
	 * position of the first element on the path which does not 
	 * exist.
	 * @param path path to traverse
	 * @param startState the state to start from
	 * @return either RPNIBlueFringeLearner.USER_ACCEPTED or the number
	 * of the first non-existing element.
	 */
	public int tracePath(List<String> path, CmpVertex startState)
	{
		CmpVertex current = startState;
		if (current == null)
			return 0;// if we start from null (i.e. not found) state, fail immediately.
		int pos = -1;
		for(String label:path)
		{
			++pos;
			Map<String,CmpVertex> exitingTrans = coregraph.transitionMatrix.get(current);
			if (exitingTrans == null || (current = exitingTrans.get(label)) == null)
				return pos;
		}
		return current.isAccept()? AbstractOracle.USER_ACCEPTED:pos;
	}
	
	/** Traces a path in a graph and returns the entered state; null if a path does not exist.
	 * 
	 * @param path path to trace
	 * @return state which would be entered by the machine if it follows the given path. 
	 */
	public CmpVertex getVertex(List<String> path)
	{
		CmpVertex current = coregraph.init;
		int pos = -1;
		for(String label:path)
		{
			++pos;
			Map<String,CmpVertex> exitingTrans = coregraph.transitionMatrix.get(current);
			if (exitingTrans == null || (current = exitingTrans.get(label)) == null)
				return null;
		}
		return current;
	}
	
	/** Converts a given sequence into a fundamental test sequence.
	 * Only used by the old implementation of the W method, <em>computeOldTestSet</em>.
	 * 
	 * @return truncated sequence
	 */
	public List<String> truncateSequence(List<String> path)
	{
		int pos = tracePath(path);
		List<String> seq = path;
		assert pos == AbstractOracle.USER_ACCEPTED || pos < path.size();
		if (pos >= 0)
				seq = path.subList(0, pos+1);// up to a rejected position plus one
		return seq;
	}
	/** When adding transitions corresponding to state associations in order to check for graph
	 * equivalence (for testing only), this prefix is used. 
	 */
	public static final String associationPrefix = "_";//PAIRASSOCIATION_"; 
	
	/** Associations between states are like normal transitions (but should not be considered 
	 * in a number of cases such as augmentation, state merging and GD). In order to check
	 * equivalence of graphs for testing, it seems reasonable to replace all such associations with real
	 * transitions and run the usual procedure of checking.
	 * <p>
	 * This method performs such a conversion.
	 * 
	 * @param graph graph
	 * @param config
	 * @return
	 */
	public static <TARGET_A_TYPE,CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>>
		DirectedSparseGraph convertPairAssociationsToTransitions(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> graph,Configuration config)
	{
		Set<String> alphabet = graph.pathroutines.computeAlphabet();
		
		Configuration noClone = config.copy();noClone.setLearnerCloneGraph(false);
		LearnerGraphND result = new LearnerGraphND(graph,config);
		Set<CmpVertex> rowsProcessed = new HashSet<CmpVertex>();
		
		class TransitionAnnotationClass extends TreeMap<String,Map<String,Map<String,Color>>>
		{
			/**
			 * ID for serialisation
			 */
			private static final long serialVersionUID = 7920446745767201260L;

			public void putAssociation(CmpVertex stateA, CmpVertex stateB, String label, Color color)
			{
				putAssociation_internal(stateA, stateB, label, color);
				putAssociation_internal(stateB, stateA, label, color);
			}
			
			private void putAssociation_internal(CmpVertex stateFrom, CmpVertex stateTo, String label,Color color)
			{
				String fromString = stateFrom.getID().toString();
				Map<String,Map<String,Color>> lbl = get(fromString);
				if (lbl == null)
				{
					lbl = new TreeMap<String,Map<String,Color>>();put(fromString, lbl);
				}
				Map<String,Color> targetToColour = lbl.get(label);
				if (targetToColour == null)
				{// this is the first annotation for the specific target state
					targetToColour = new TreeMap<String,Color>();lbl.put(label,targetToColour);
				}
				
				targetToColour.put(stateTo.getID().toString(),color);
			}
		}

		TransitionAnnotationClass transitionAnnotation = new TransitionAnnotationClass();
		
		for(Entry<CmpVertex,Map<CmpVertex,PAIRCOMPATIBILITY>> entry:result.pairCompatibility.compatibility.entrySet())
		{
			rowsProcessed.add(entry.getKey());
			for(Entry<CmpVertex,PAIRCOMPATIBILITY> associations:entry.getValue().entrySet())
				if (!rowsProcessed.contains(associations.getKey()))
				{
					String label =associationPrefix+associations.getValue().name();
					if (alphabet.contains(label))
						throw new IllegalArgumentException("cannot use label "+label);

					result.addTransition(result.transitionMatrix.get(entry.getKey()), label, associations.getKey());
					result.addTransition(result.transitionMatrix.get(associations.getKey()), label, entry.getKey());
					transitionAnnotation.putAssociation(entry.getKey(), associations.getKey(), label, Color.YELLOW);
				}
		}
		
		DirectedSparseGraph gr = result.pathroutines.getGraph();
		
		gr.addUserDatum(JUConstants.EDGE, transitionAnnotation, UserData.SHARED);
		return gr;
	}
}
