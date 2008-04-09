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
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicEdge;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.StatePair;
import statechum.xmachine.model.testset.PTAExploration;
import statechum.xmachine.model.testset.PTASequenceSetAutomaton;
import statechum.xmachine.model.testset.PTASequenceEngine;
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

	public Collection<List<String>> computePathsToRed(CmpVertex red)
	{
		return computePathsBetween(coregraph.init, red);
	}
	
	/** Computes all possible shortest paths from the supplied source state to the supplied target state.
	 * If there are many paths of the same length, all of those paths are returned.
	 * 
	 * @param vertSource the source state
	 * @param vertTarget the target state
	 * @return sequences of inputs to follow all paths found.
	 */	
	Collection<List<String>> computePathsBetween(CmpVertex vertSource, CmpVertex vertTarget)
	{
		PTASequenceEngine engine = new PTASequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTASequenceEngine.SequenceSet initSet = engine.new SequenceSet();initSet.setIdentity(); 
		PTASequenceEngine.SequenceSet paths = engine.new SequenceSet();paths.setIdentity(); 
		computePathsSBetween(vertSource, vertTarget,initSet,paths);
		return engine.getData();
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
	public void computePathsSBetween(CmpVertex vertSource, CmpVertex vertTarget,
			PTASequenceEngine.SequenceSet pathsToVertSource,
			PTASequenceEngine.SequenceSet result)
	{
		if (!computePathsSBetweenBoolean(vertSource, vertTarget, pathsToVertSource, result))
			throw new IllegalArgumentException("path from state "+vertSource+" to state "+vertTarget+" was not found");
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
	 * @return false if a path cannot be found and true otherwise.
	 */	
	public boolean computePathsSBetweenBoolean(CmpVertex vertSource, CmpVertex vertTarget,
			PTASequenceEngine.SequenceSet pathsToVertSource,
			PTASequenceEngine.SequenceSet result)
	{
		if (vertSource == null || vertTarget == null || pathsToVertSource == null)
			throw new IllegalArgumentException("null arguments to computePathsSBetween");
		coregraph.buildCachedData();
		if (LearnerGraph.testMode)
			if (!coregraph.learnerCache.flowgraph.containsKey(vertSource) || !coregraph.learnerCache.flowgraph.containsKey(vertTarget))
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
			return true;// nothing to do, return paths to an initial state.
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
				for(Entry<CmpVertex,Set<String>> entry:coregraph.learnerCache.flowgraph.get(currentVert).entrySet())
				{
					if (entry.getKey() == vertTarget)
					{// found the vertex we are looking for
						pathFound = true;
						// now we need to go through all our states in a path and update pathsToVertSource
						PTASequenceEngine.SequenceSet paths = pathsToVertSource;currentPath.add(vertTarget);CmpVertex curr = vertSource;
						// process vertices
						for(CmpVertex tgt:currentPath)
						{// ideally, I'd update one at a time and merge results, but it seems the same (set union) if I did it by building a set of inputs and did a cross with it.
							paths = paths.crossWithSet(coregraph.learnerCache.flowgraph.get(curr).get(tgt));
							curr = tgt;
						}
						result.unite( paths );// update the result.
					}
					else
						if (!visitedStates.contains(entry.getKey()))
						{
							List<CmpVertex> newPath = new LinkedList<CmpVertex>();newPath.addAll(currentPath);newPath.add(entry.getKey());
							currentExplorationPath.offer(newPath);currentExplorationState.offer(entry.getKey());
						}
				}
			}
		}

		return pathFound;
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
		if (LearnerGraph.testMode)
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

	public LearnerGraph augmentPTA(Collection<List<String>> strings, boolean accepted)
	{
		for(List<String> sequence:strings)
			augmentPTA(sequence, accepted);
		return coregraph;
	}
	
	/** Given a path and whether it is an accept or reject-path adds this path to the graph. 
	 * An accept-path added in its entirety; reject-path has the last node as reject and the rest are accept ones.
	 * 
	 * @param sequence the path to add
	 * @param accepted whether the last element is accept or reject.
	 * @return the current (updated) graph. 
	 */
	public LearnerGraph augmentPTA(List<String> sequence, boolean accepted)
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
				throw new IllegalArgumentException("incompatible "+(accepted?"accept":"reject")+" labelling: "+sequence.subList(0, position));
			}
			prevState = currentState;lastInput = inputIt.next();++position;
			
			currentState = coregraph.transitionMatrix.get(prevState).get(lastInput);
		}
		
		if (currentState == null)
		{// the supplied path does not exist in PTA, the first non-existing vertex is from state prevState with label lastInput

			synchronized (LearnerGraph.syncObj) 
			{
				while(inputIt.hasNext())
				{
					prevState = coregraph.addVertex(prevState, true, lastInput);
					lastInput = inputIt.next();
				}
				// at this point, we are at the end of the sequence. Last vertex is prevState and last input if lastInput
				coregraph.addVertex(prevState, accepted, lastInput);
			}
			
		}
		else
		{// we reached the end of the PTA
			if (currentState.isAccept() != accepted)
			{
				currentState.setHighlight(true);
				throw new IllegalArgumentException("incompatible "+(accepted?"accept":"reject")+" labelling: "+sequence.subList(0, position));
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
					{
						synchronized (LearnerGraph.syncObj) 
						{
							ourVertex = coregraph.addVertex(prevNode.userObject, accepted, prevNode.getInput());
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
					throw new IllegalArgumentException("incompatible accept "+(accepted?"accept":"reject")+" labelling: "+result);
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
			{
			}
		};
		exploration.walkThroughAllPaths();
		coregraph.learnerCache.invalidate();
		return coregraph;
	}

	/** Builds a Jung graph corresponding to the state machine stored in transitionMatrix.
	 * Note that all states in our transition diagram (transitionMatrix) have Jung vertices associated with them (CmpVertex).
	 * 
	 * @return constructed graph.
	 */
	public DirectedSparseGraph getGraph()
	{
		return getGraph(null);
	}
	
	/** Builds a Jung graph corresponding to the state machine stored in transitionMatrix.
	 * Note that all states in our transition diagram (transitionMatrix) have Jung vertices associated with them (DeterministicVertex).
	 * The fact that we need to return a Jung graph implies that all nodes are always cloned;
	 * this way we do not have to check if we've been asked to keep the original nodes and
	 * keep them unless they were StringVertices.
	 * 
	 * @param the name to give to the graph to be built.
	 * @return constructed graph.
	 */
	public DirectedSparseGraph getGraph(String name)
	{
		DirectedSparseGraph result = null;
		Configuration cloneConfig = (Configuration)coregraph.config.clone();cloneConfig.setLearnerUseStrings(false);cloneConfig.setLearnerCloneGraph(true);
		synchronized (LearnerGraph.syncObj) 
		{
			result = new DirectedSparseGraph();
			if (name != null)
				result.setUserDatum(JUConstants.TITLE, name,UserData.SHARED);
			coregraph.buildCachedData();
			Map<CmpVertex,DeterministicVertex> oldToNew = new HashMap<CmpVertex,DeterministicVertex>();
			// add states
			for(Entry<CmpVertex,Map<CmpVertex,Set<String>>> entry:coregraph.learnerCache.flowgraph.entrySet())
			{
				CmpVertex source = entry.getKey();
				DeterministicVertex vert = (DeterministicVertex)LearnerGraph.cloneCmpVertex(source,cloneConfig);
				if (coregraph.init == source)
					vert.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
				result.addVertex(vert);
				oldToNew.put(source,vert);
			}
			
			// now add transitions
			for(Entry<CmpVertex,Map<CmpVertex,Set<String>>> entry:coregraph.learnerCache.flowgraph.entrySet())
			{
				DeterministicVertex source = oldToNew.get(entry.getKey());
				for(Entry<CmpVertex,Set<String>> tgtEntry:entry.getValue().entrySet())
				{
					CmpVertex targetOld = tgtEntry.getKey();
					assert coregraph.findVertex(targetOld.getID()) == targetOld : "was looking for vertex with name "+targetOld.getID()+", got "+coregraph.findVertex(targetOld.getID());
					DeterministicVertex target = oldToNew.get(targetOld);
					DeterministicEdge e = new DeterministicEdge(source,target);
					e.addUserDatum(JUConstants.LABEL, tgtEntry.getValue(), UserData.CLONE);
					result.addEdge(e);
				}
			}
		}
		return result;
	}

	/** Numerous methods using this class expect to be able to interpret the state 
	 * machine as a flowgraph, this method builds one.
	 * Always make sure this one is called after finished with making changes to
	 *  
	 */
	public Map<CmpVertex,Map<CmpVertex,Set<String>>> getFlowgraph()
	{
		Map<CmpVertex,Map<CmpVertex,Set<String>>> result = new TreeMap<CmpVertex,Map<CmpVertex,Set<String>>>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
		{
			Map<CmpVertex,Set<String>> targetStateToEdgeLabels = result.get(entry.getKey());
			if (targetStateToEdgeLabels == null)
			{
				targetStateToEdgeLabels = new TreeMap<CmpVertex,Set<String>>();
				result.put(entry.getKey(), targetStateToEdgeLabels);
			}
			
			for(Entry<String,CmpVertex> sv:entry.getValue().entrySet())
			{
				if (sv.getValue() == null)
					throw new IllegalArgumentException("Target vertex "+sv.getValue()+" is not in the transition table, referred to from vertex "+entry.getKey());
				Set<String> labels = targetStateToEdgeLabels.get(sv.getValue());
				if (labels != null)
					// there is an edge already with the same target state from the current vertice, update the label on it
					labels.add(sv.getKey());
				else
				{// add a new edge
					labels = new HashSet<String>();labels.add(sv.getKey());
					targetStateToEdgeLabels.put(sv.getValue(), labels);
				}
			}
		}
		
		return result;
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
		synchronized (LearnerGraph.syncObj) 
		{
			result = new DirectedSparseGraph();
			if (name != null)
				result.setUserDatum(JUConstants.TITLE, name,UserData.SHARED);
			Map<CmpVertex,Map<CmpVertex,Set<String>>> flowgraph = getFlowgraph();
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
		
		LearnerGraph fsm = new LearnerGraph(getGraph(), coregraph.config);
		
		String wsetDetails = "";
		try
		{
			if (computeW) wsetDetails = "Wset: "+WMethod.computeWSet(fsm).size()+" seq";
		}
		catch (statechum.analysis.learning.rpnicore.WMethod.EquivalentStatesException e) {
			wsetDetails = e.toString();
		}
		return "vert: "+coregraph.transitionMatrix.keySet().size()+" edges: "+edgeCounter+" alphabet: "+fsm.wmethod.computeAlphabet().size()+" unreachable: "+fsm.wmethod.checkUnreachableStates()+" "+wsetDetails;
	}

	protected static void checkPTAConsistency(LearnerGraph original, CmpVertex blueState)
	{
		assert LearnerGraph.testMode : "this one should not run when not under test";
		Queue<CmpVertex> currentBoundary = new LinkedList<CmpVertex>();// FIFO queue containing vertices to be explored
		Set<CmpVertex> ptaStates = new HashSet<CmpVertex>();
		currentBoundary.add( blueState );
		while(!currentBoundary.isEmpty())
		{
			CmpVertex current = currentBoundary.remove();
			if (ptaStates.contains(current))
				throw new IllegalArgumentException("PTA has multiple paths to "+current);
			ptaStates.add(current);
			if (LearnerGraph.testMode)
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
	
	/** Checks that non-red states form a tree, i.e. they have exactly one incoming edge and there are no
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
		assert LearnerGraph.testMode : "this one should not run when not under test";
	
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
				List<String> seq = original.paths.computePathsBetween(pair.getQ(), InterestingUnreachableVertex).iterator().next();
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

	/** Computes an alphabet of a given graph and adds transitions to a 
	 * reject state from all states A and inputs a from which there is no B such that A-a->B
	 * (A-a-#REJECT) gets added. Note: (1) such transitions are even added to reject vertices.
	 * (2) if such a vertex already exists, an IllegalArgumentException is thown.
	 * 
	 * @param reject the name of the reject state, to be added to the graph. No transitions are added from this state.
	 * @return true if any transitions have been added
	 */   
	public boolean completeGraph(VertexID reject)
	{
		if (coregraph.findVertex(reject) != null)
			throw new IllegalArgumentException("reject vertex named "+reject+" already exists");
		
		CmpVertex rejectVertex = null;
		
		// first pass - computing an alphabet
		Set<String> alphabet = coregraph.wmethod.computeAlphabet();
		
		// second pass - checking if any transitions need to be added and adding them.
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
		{
			Set<String> labelsToRejectState = new HashSet<String>();
			labelsToRejectState.addAll(alphabet);labelsToRejectState.removeAll(entry.getValue().keySet());
			if (!labelsToRejectState.isEmpty())
			{
				if (rejectVertex == null)
				{
					rejectVertex = LearnerGraph.generateNewCmpVertex(reject,coregraph.config);rejectVertex.setAccept(false);
				}
				Map<String,CmpVertex> row = entry.getValue();
				for(String rejLabel:labelsToRejectState)
					row.put(rejLabel, rejectVertex);
			}
		}

		if (rejectVertex != null)
			coregraph.transitionMatrix.put(rejectVertex,new TreeMap<String,CmpVertex>());
		
		coregraph.learnerCache.invalidate();
		return rejectVertex != null;
	}

	public int tracePath(List<String> path)
	{
		return tracePath(path,coregraph.init);
	}
	
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
		return current.isAccept()? RPNIBlueFringeLearner.USER_ACCEPTED:pos;
	}
	
	/** converts a given sequence into a fundamental test sequence.
	 * 
	 * @return truncated sequence
	 */
	public List<String> truncateSequence(List<String> path)
	{// TODO: I think this should only be used for testing of PTATestSequenceEngine and the like.
		int pos = tracePath(path);
		List<String> seq = path;
		assert(pos == RPNIBlueFringeLearner.USER_ACCEPTED || pos < path.size());
		if (pos >= 0)
				seq = path.subList(0, pos+1);// up to a rejected position plus one
		return seq;
	}
}
