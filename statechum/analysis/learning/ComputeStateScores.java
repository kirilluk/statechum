/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.analysis.learning;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.oracles.*;
import statechum.xmachine.model.testset.PTASequenceSet;
import statechum.xmachine.model.testset.PTASequenceSetAutomaton;
import statechum.xmachine.model.testset.PTATestSequenceEngine;
import statechum.xmachine.model.testset.WMethod;
import statechum.xmachine.model.testset.PTATestSequenceEngine.FSMAbstraction;
import statechum.xmachine.model.testset.PTATestSequenceEngine.sequenceSet;
import statechum.xmachine.model.testset.WMethod.EquivalentStatesException;
import static statechum.analysis.learning.RPNIBlueFringeLearner.isAccept;
import static statechum.analysis.learning.Visualiser.isGraphTransformationDebug;
import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

/** This class and its wholly-owned subsidiaries perform computation of scores, state merging and question generation. */
public class ComputeStateScores implements Cloneable {
	/** The initial vertex. */
	private CmpVertex init;
	
	protected Map<CmpVertex,Map<String,CmpVertex>> transitionMatrix = new TreeMap<CmpVertex,Map<String,CmpVertex>>();
			
	/** Stores all red-blue pairs; has to be backed by array for the optimal performance of the sort function. */
	protected List<PairScore> pairsAndScores;
	
	protected int generalisationThreshold;
	protected int pairsMergedPerHypothesis;
	
	protected boolean bumpPositives = false, useCompatibilityScore = false;

	/** Used to switch on a variety of consistency checks. */
	protected static boolean testMode = false;

	/** The initial size of the pairsAndScores array. */
	public static final int pairArraySize = 2000;
	
	public List<String> extractReds()
	{
		List<String> result = new LinkedList<String>();
		for(Vertex v:transitionMatrix.keySet())
			if (v.containsUserDatumKey(JUConstants.COLOUR)&& v.getUserDatum(JUConstants.COLOUR) == JUConstants.RED)
					result.add(v.getUserDatum(JUConstants.LABEL).toString());
		
		return result;
	}

	/** Resets all the colour labelling to the initial value. */
	public void clearColours()
	{
		for(Vertex v:transitionMatrix.keySet())
			v.removeUserDatum(JUConstants.COLOUR);
		init.addUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
	}
	
	/** Assigns red colour to all vertices in the supplied set
	 * 
	 * @param reds vertices to assign colours to.
	 */
	public void assignReds(Set<String> reds)
	{
		for(Vertex v:transitionMatrix.keySet())
		{
			v.removeUserDatum(JUConstants.COLOUR);
			if (reds.contains(v.getUserDatum(JUConstants.LABEL)))
				v.addUserDatum(JUConstants.COLOUR,JUConstants.RED, UserData.SHARED);
		}
	}
	
	/** Initialises the class used to compute scores between states.
	 * 
	 * @param g the graph it will be used on 
	 */
	public ComputeStateScores(DirectedSparseGraph g)
	{
		init = (CmpVertex)TestRpniLearner.findInitial(g);
		
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

	public ComputeStateScores(int newPairsMergedPerHypothesis)
	{
		pairsMergedPerHypothesis = newPairsMergedPerHypothesis;
		initPTA();
	}
	
	public static Vertex getTempRed_DijkstraShortestPath(DirectedSparseGraph model, Vertex r, DirectedSparseGraph temp){
		DijkstraShortestPath p = new DijkstraShortestPath(model);
		List<Edge> pathToRed = p.getPath(TestRpniLearner.findInitial(model), r);
		Vertex tempRed = null;
		if(!pathToRed.isEmpty()){
			List<String> pathToRedString = new LinkedList<String>();
			for(Edge e:pathToRed)
				pathToRedString.add( ((Collection<String>)e.getUserDatum(JUConstants.LABEL)).iterator().next() );
			tempRed = TestRpniLearner.getVertex(temp, pathToRedString);
		}
		else
			tempRed = TestRpniLearner.findInitial(temp);
		return tempRed;
	}
	
	Collection<List<String>> computePathsToRed(Vertex red)
	{
		return computePathsBetween(init, red);
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
		if(successors.keySet().isEmpty()){
			collection.add(currentList);
			return collection;
		}
		else{
			Iterator<String> keyIt = successors.keySet().iterator();
			while(keyIt.hasNext()){
				String key = keyIt.next();
				currentList.add(key);
				collection.addAll(getTails(successors.get(key),currentList,collection));
			}
		}
		return collection;
	}
	
	/**
	 *returns set of vertices that are the destination of label
	 */
	private Collection<CmpVertex> findVertices(String label){
		Collection<CmpVertex> vertices = new HashSet<CmpVertex>();
		Iterator<Map<String, CmpVertex>> outgoingEdgesIt = transitionMatrix.values().iterator();
		while(outgoingEdgesIt.hasNext()){
			Map<String,CmpVertex> edges = outgoingEdgesIt.next();
			if(edges.keySet().contains(label))
				vertices.add(edges.get(label));
		}
		return vertices;
	}
		
	/** Computes all possible shortest paths from the supplied source state to the supplied target state.
	 * If there are many paths of the same length, all of those paths are returned.
	 * 
	 * @param vertSource the source state
	 * @param vertTarget the target state
	 * @return sequences of inputs to follow all paths found.
	 */	
	Collection<List<String>> computePathsBetween(Vertex vertSource, Vertex vertTarget)
	{
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity(); 
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();paths.setIdentity(); 
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
	 * cannot create an empty instance of a sequenceSet, perhaps for a reason).
	 */	
	void computePathsSBetween(Vertex vertSource, Vertex vertTarget,
			PTATestSequenceEngine.sequenceSet pathsToVertSource,
			PTATestSequenceEngine.sequenceSet result)
	{
		if (vertSource == null || vertTarget == null || pathsToVertSource == null)
			throw new IllegalArgumentException("null arguments to computePathsSBetween");
		Set<Vertex> visitedStates = new HashSet<Vertex>();visitedStates.add(vertSource);
		
		// FIFO queue containing sequences of states labelling paths to states to be explored.
		// Important, after processing of each wave, we add a null, in order to know when
		// to stop when scanning to the end of the current wave when a path to the target state
		// has been found.
		Queue<List<Vertex>> currentExplorationPath = new LinkedList<List<Vertex>>();
		Queue<Vertex> currentExplorationState = new LinkedList<Vertex>();
		if (vertSource == vertTarget)
		{
			result.setIdentity();
			return ;// nothing to do, return an empty sequence.
		}
		
		currentExplorationPath.add(new LinkedList<Vertex>());currentExplorationState.add(vertSource);
		currentExplorationPath.offer(null);currentExplorationState.offer(null);// mark the end of the first (singleton) wave.
		Vertex currentVert = null;List<Vertex> currentPath = null;
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
				for(Vertex nextVertex:transitionMatrix.get(currentVert).values())
				{
					if (nextVertex == vertTarget)
					{// found the vertex we are looking for
						pathFound = true;
						// now we need to go through all our states in a path and update pathsToVertSource
						PTATestSequenceEngine.sequenceSet paths = pathsToVertSource;
						Vertex curr = vertSource;Collection<String> inputsToMultWith = new LinkedList<String>();
						
						// process all but one vertices
						for(Vertex tgt:currentPath)
						{// ideally, I'd update one at a time and merge results, but it seems the same (set union) if I did it by building a set of inputs and did a cross with it.
							inputsToMultWith.clear();
							for(Entry<String,CmpVertex> entry:transitionMatrix.get(curr).entrySet())
								if (entry.getValue() == tgt)
									inputsToMultWith.add(entry.getKey());
							paths = paths.crossWithSet(inputsToMultWith);
							curr = tgt;
						}
						inputsToMultWith.clear();
						// now the last pass for the target vertex
						for(Entry<String,CmpVertex> entry:transitionMatrix.get(curr).entrySet())
							if (entry.getValue() == nextVertex)
								inputsToMultWith.add(entry.getKey());
						result.unite( paths.crossWithSet(inputsToMultWith) );// update the result.
					}
					else
					if (!visitedStates.contains(nextVertex))
					{
						List<Vertex> newPath = new LinkedList<Vertex>();newPath.addAll(currentPath);newPath.add(nextVertex);
						currentExplorationPath.offer(newPath);currentExplorationState.offer(nextVertex);
					}
				}
			}
		}

		if (!pathFound)
			throw new IllegalArgumentException("path to state "+vertTarget+" was not found");
		
		return ;
	}

	private static class ExplorationElement
	{
		public final Vertex firstVertex, secondVertex;
		public final PTATestSequenceEngine.sequenceSet pathsInOriginal;
		
		public ExplorationElement(Vertex first, Vertex second, PTATestSequenceEngine.sequenceSet paths)
		{
			firstVertex=first;secondVertex=second;pathsInOriginal=paths;
		}
	}
	
	private static void buildQuestionsFromPair(
			ComputeStateScores original, Vertex originalRed, 
			ComputeStateScores merged, Vertex mergedRed, 
			PTATestSequenceEngine.sequenceSet pathsInOriginal)
	{
		Map<CmpVertex,List<String>> targetToInputSet = new TreeMap<CmpVertex,List<String>>();
		
		// now we build a sort of a "transition cover" from the tempRed state, in other words, take every vertex and 
		// build a path from tempRed to it, at the same time tracing it through the current machine.

		Set<Vertex> visitedStates = new HashSet<Vertex>();visitedStates.add(originalRed);
		Queue<ExplorationElement> currentExplorationBoundary = new LinkedList<ExplorationElement>();// FIFO queue containing vertices to be explored
		currentExplorationBoundary.add(new ExplorationElement(originalRed,mergedRed, pathsInOriginal));

		//List<String> nextInput = new ArrayList<String>(1);
		while(!currentExplorationBoundary.isEmpty())
		{
			ExplorationElement current = currentExplorationBoundary.remove();
			
			Map<String,CmpVertex> firstRow = merged.transitionMatrix.get(current.firstVertex),
				secondRow = original.transitionMatrix.get(current.secondVertex);
			targetToInputSet.clear();
			System.out.println("before multiplying: "+current.pathsInOriginal.getDebugData());
			PTATestSequenceEngine.sequenceSet multResult = current.pathsInOriginal.crossWithSet(firstRow.keySet());// transition cover of merged
			System.out.println("about to multiply by "+firstRow.keySet());
			System.out.println("after state cover: "+multResult.getDebugData());
			if (ComputeStateScores.testMode)
			{
				Set<String> moreOrigInputs = new HashSet<String>();moreOrigInputs.addAll(secondRow.keySet());
				moreOrigInputs.removeAll(firstRow.keySet());
				assert moreOrigInputs.isEmpty() : 
					"inconsistent merge: merged automaton has fewer paths, merged: "+firstRow.keySet()+
					", original: "+secondRow.keySet();
			}
			System.out.println("from states "+current.firstVertex+","+current.secondVertex);
			for(Entry<String,CmpVertex> entry:firstRow.entrySet())
				// When generating questions for PTA merging, we should be trying 
				// to enter all states of the merged automaton - 
				// traversing states of the original one generates long sequences 
				// and is likely to lead to numerous spurious questions being generated 
				// because for a group of states merged into one, the closest one to the 
				// initial state will be the most representative (the further from the initial 
				// state, the more sparse a PTA becomes). The important point here is that the first state
				// of a group of merged states is the one closest to the initial state in the PTA.
				// In contrast, where states in an arbitrary automaton are being merged, 
				// we might be better off checking states further away
				// because they may just happen to contain more structure.
				if (!visitedStates.contains(entry.getValue()))
				{
					List<String> inputs = targetToInputSet.get(entry.getValue());
					if (inputs == null)
					{
						inputs = new LinkedList<String>();targetToInputSet.put(entry.getValue(),inputs);
					}
					inputs.add(entry.getKey());
				}
			
			for(Entry<CmpVertex,List<String>> target:targetToInputSet.entrySet())
			{
				visitedStates.add(target.getKey());
				Vertex newMergedVertex = firstRow.get(target.getValue().iterator().next());
				PTATestSequenceEngine.sequenceSet stateCoverSoFar = current.pathsInOriginal.crossWithSet(target.getValue()); 
				currentExplorationBoundary.offer(new ExplorationElement(target.getKey(), newMergedVertex, 
						stateCoverSoFar));
				System.out.println("\tnew pair: "+target.getKey()+","+newMergedVertex+
						" inputs: "+target.getValue());
			}
		}
		
	}

	private static void buildQuestionsFromPair_Compatible(
			ComputeStateScores original, Vertex originalRed, 
			ComputeStateScores merged, Vertex mergedRed, 
			PTATestSequenceEngine.sequenceSet pathsInOriginal)
	{
		// now we build a sort of a "transition cover" from the tempRed state, in other words, take every vertex and 
		// build a path from tempRed to it, at the same time tracing it through the current machine.

		Set<Vertex> visitedStates = new HashSet<Vertex>();visitedStates.add(mergedRed);
		Queue<Vertex> currentExplorationBoundary = new LinkedList<Vertex>();// FIFO queue containing vertices to be explored
		Queue<PTATestSequenceEngine.sequenceSet> currentExplorationTargetStates = new LinkedList<PTATestSequenceEngine.sequenceSet>();
		currentExplorationBoundary.add(mergedRed);
		currentExplorationTargetStates.add(pathsInOriginal);

		Map<CmpVertex,List<String>> targetToInputSet = new TreeMap<CmpVertex,List<String>>();
		while(!currentExplorationBoundary.isEmpty())
		{
			Vertex currentVert = currentExplorationBoundary.remove();
			PTATestSequenceEngine.sequenceSet currentPaths = currentExplorationTargetStates.remove();
			targetToInputSet.clear();
			
			currentPaths.crossWithSet(merged.transitionMatrix.get(currentVert).keySet());
			for(Entry<String,CmpVertex> entry:merged.transitionMatrix.get(currentVert).entrySet())
				if (!visitedStates.contains(entry.getValue()))
				{
					List<String> inputs = targetToInputSet.get(entry.getValue());
					if (inputs == null)
					{
						inputs = new LinkedList<String>();targetToInputSet.put(entry.getValue(),inputs);
					}
					inputs.add(entry.getKey());
				}
			for(Entry<CmpVertex,List<String>> target:targetToInputSet.entrySet())
			{
				visitedStates.add(target.getKey());
				currentExplorationBoundary.offer(target.getKey());
				currentExplorationTargetStates.offer(currentPaths.crossWithSet(target.getValue()));
// KIRR: what is interesting is that even if crossWithSet delivers all-sink states (i.e. no transition we've taken from the 
// new red state is allowed by the original automaton), we still proceed to enumerate states. Another strange feature is 
// that we're taking shortest paths to all states in the new automaton, while there could be multiple different paths. 
// This means that it is possible that there would be paths to some states in the new automaton which will also be possible 
// in the original one, but will not be found because they are not the shortest ones. In turn, this means that many potential

			}
		}
	}

	private class NonExistingPaths implements FSMAbstraction
	{
		private final CmpVertex red;
		
		public NonExistingPaths(CmpVertex redState)
		{
			red = redState;
		}
		
		public Object getInitState() {
			return red;
		}
	
		public final CmpVertex junkVertex = new DeterministicDirectedSparseGraph.DeterministicVertex("JUNK");
				
		public Object getNextState(Object currentState, String input) 
		{
			Vertex result = null;
			Map<String,CmpVertex> row = transitionMatrix.get(currentState);
			if (row != null)
				result = row.get(input);
			if (result == null)
				result = junkVertex;

			return result;
		}
	
		public boolean isAccept(Object currentState) 
		{
			return true;
		}

		public boolean shouldBeReturned(Object elem) {
			return elem == junkVertex;
		}
	}

	// TODO to test with red = init, with and without loop around it (red=init and no loop is 3_1), with and without states which cannot be reached from a red state,
	// where a path in the original machine corresponding to a path in the merged one exists or not (tested with 3_1)
	public Collection<List<String>> computeQS(final StatePair pair, ComputeStateScores merged)
	{
		Vertex mergedRed = merged.findVertex((String)pair.getR().getUserDatum(JUConstants.LABEL));
		if (mergedRed == null)
			throw new IllegalArgumentException("failed to find the red state in the merge result");

		PTATestSequenceEngine engine = new PTATestSequenceEngine();
		engine.init(new NonExistingPaths((CmpVertex)init));
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		PTATestSequenceEngine.sequenceSet initp = engine.new sequenceSet();initp.setIdentity();

		merged.computePathsSBetween(merged.init,mergedRed, initp, paths);
		Collection<String> inputsToMultWith = new LinkedList<String>();
		for(Entry<String,CmpVertex> loopEntry:merged.transitionMatrix.get(mergedRed).entrySet())
			if (loopEntry.getValue() == mergedRed)
			{// Note an input corresponding to any loop in temp can be followed in the original machine, since
				// a loop in temp is either due to the merge or because it was there in the first place.
				inputsToMultWith.add(loopEntry.getKey());
			}
		paths.unite(paths.crossWithSet(inputsToMultWith));// the resulting path does a "transition cover" on all transitions leaving the red state.
		buildQuestionsFromPair_Compatible(this, pair.getR(), merged, mergedRed, paths);
		return engine.getData();
	}

	
	/** Computes scores by navigating a cross-product of this machine, with itself.
	 * 
	 *  @param the pair to compute a score for
	 *  @return the resulting score, reflecting compatibility.
	 */
	protected int computeStateScore(StatePair pair)
	{
		if (isAccept(pair.getR()) != isAccept(pair.getQ()))
			return -1;

		int score = 0;
		
		assert pair.getQ() != pair.getR();
		
		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
		currentExplorationBoundary.add(pair);
		
		while(!currentExplorationBoundary.isEmpty())
		{
			StatePair currentPair = currentExplorationBoundary.remove();
			Map<String,CmpVertex> targetRed = transitionMatrix.get(currentPair.getR()),
				targetBlue = transitionMatrix.get(currentPair.getQ());

			for(Entry<String,CmpVertex> redEntry:targetRed.entrySet())
			{
				Vertex nextBlueState = targetBlue.get(redEntry.getKey());
				if (nextBlueState != null)
				{// both states can make a transition
					if (isAccept(redEntry.getValue()) != isAccept(nextBlueState))
						return -1;// incompatible states
					
					++score;

					StatePair nextStatePair = new StatePair(nextBlueState,redEntry.getValue());
					currentExplorationBoundary.offer(nextStatePair);
				}
				// if the red can make a move, but the blue one cannot, ignore this case.
			}
		}
		
		if (bumpPositives && isAccept(pair.getQ()))
			score++;// bumpPositives is used to give an extra weight to state pairs which are both compatible and positive (i.e. discourage reject-reject state pairs).
		
		return score;
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
	
	public Stack<PairScore> chooseStatePairs()
	{
		pairsAndScores.clear();
		Set<Vertex> reds = new LinkedHashSet<Vertex>();
		for(Vertex v:transitionMatrix.keySet())
			if (v.containsUserDatumKey(JUConstants.COLOUR) && v.getUserDatum(JUConstants.COLOUR) == JUConstants.RED)
				reds.add(v);

		Queue<Vertex> currentExplorationBoundary = new LinkedList<Vertex>();// FIFO queue
		currentExplorationBoundary.addAll(reds);
		List<Vertex> BlueStatesConsideredSoFar = new LinkedList<Vertex>();
		while(!currentExplorationBoundary.isEmpty())
		{
			Vertex currentRed = currentExplorationBoundary.remove();
			for(Entry<String,CmpVertex> BlueEntry:transitionMatrix.get(currentRed).entrySet())
				if (!BlueEntry.getValue().containsUserDatumKey(JUConstants.COLOUR) || 
						BlueEntry.getValue().getUserDatum(JUConstants.COLOUR) == JUConstants.BLUE)
				{// the next vertex is not marked red, hence it has to become blue
					Vertex currentBlueState = BlueEntry.getValue();
											
					int numberOfCompatiblePairs = 0;
					for(Vertex oldRed:reds)
					{
						PairScore pair = obtainPair(currentBlueState,oldRed);
						if (pair.getScore() >= generalisationThreshold)
						{
							pairsAndScores.add(pair);
							++numberOfCompatiblePairs;
							if (testMode) checkPTAConsistency(this, currentBlueState);
						}
					}
					
					if (numberOfCompatiblePairs == 0)
					{// mark this blue node as red. 
						Vertex newRedNode = currentBlueState;
						newRedNode.setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
						reds.add(newRedNode);currentExplorationBoundary.add(newRedNode);
						BlueStatesConsideredSoFar.remove(newRedNode);
						
						// All future blue nodes will use this revised set of red states; the fact that
						// it is added to the exploration boundary ensures that it is considered when looking for more blue states.
						// Note that previously-considered blue states were not compared to this one (because it was blue before),
						// however previously-introduced red were - we're using the up-to-date reds set above.
						// For this reason, all we have to do is iterate over the old blue states and compare them to the
						// current one; none of those states may become red as a consequence since they are not 
						// red already, i.e. there is an entry about them in PairsAndScores
						for(Vertex oldBlue:BlueStatesConsideredSoFar)
						{
							PairScore pair = obtainPair(oldBlue,newRedNode);
							if (pair.getScore() >= generalisationThreshold)
							{
								pairsAndScores.add(pair);
								if (testMode) checkPTAConsistency(this, oldBlue);
							}
						}
					}
					else
					{// This node is a blue node and remains blue unlike the case above when it could become red.
						BlueStatesConsideredSoFar.add(BlueEntry.getValue());// add a blue one
						currentBlueState.setUserDatum(JUConstants.COLOUR, JUConstants.BLUE, UserData.SHARED);
					}							
				}
		}

		Collections.sort(pairsAndScores);// there is no point maintaining a sorted collection as we go since a single quicksort at the end will do the job

		Stack<PairScore> result = new Stack<PairScore>();
		if (pairsMergedPerHypothesis > 0)
		{
			int numberOfElements = Math.min(pairsAndScores.size(),pairsMergedPerHypothesis);
			result.addAll(pairsAndScores.subList(0, numberOfElements));
		}
/*		
		else
			if (bumpPositives && !pairsAndScores.isEmpty())
			{// here we only append elements with the same score as the max score
				
				ComputeStateScores.PairScore topPair = pairsAndScores.get(pairsAndScores.size()-1);result.add(topPair);
				
				for(int i=0;i< pairsAndScores.size();++i)
				{// we iterage until we get to the end; pairsAndScores is an array
					ComputeStateScores.PairScore p = pairsAndScores.get(i);
					if (p.getScore() == topPair.getScore()) result.add(p);
				}
			}
*/			
		else result.addAll(pairsAndScores);

		return result;
	}		

	protected PairScore obtainPair(Vertex blue, Vertex red)
	{
		int computedScore = -1, compatibilityScore =-1;StatePair pairToComputeFrom = new StatePair(blue,red);
		if (useCompatibilityScore)
		{
			computedScore = computePairCompatibilityScore(pairToComputeFrom);compatibilityScore=computedScore;
		}
		else
		{
			computedScore = computeStateScore(pairToComputeFrom);
			if (computedScore >= 0)
			{
				compatibilityScore=	computePairCompatibilityScore(pairToComputeFrom);
				if (compatibilityScore < 0)
					computedScore = -1;
			}
			if (testMode)
				assert computePairCompatibilityScore(pairToComputeFrom) <= computedScore;
		}
		
		return new PairScore(blue,red,computedScore, compatibilityScore);
	}
	
	/** If the supplied vertex is already known (its label is stored in the map), the one from the map is returned;
	 * otherwise a reasonable copy is made, it is then both returned and stored in the map.
	 * 
	 * @param newVertices the map from labels to new vertices
	 * @param g the graph which will have the new vertex added to it
	 * @param origVertex the vertex to copy
	 * @return a copy of the vertex
	 */
	private static CmpVertex copyVertex(Map<String,CmpVertex> newVertices, DirectedSparseGraph g,Vertex origVertex)
	{
		String vertName = (String)origVertex.getUserDatum(JUConstants.LABEL);
		CmpVertex newVertex = newVertices.get(vertName);
		if (newVertex == null) { 
			newVertex = new DeterministicDirectedSparseGraph.DeterministicVertex();
			newVertex.addUserDatum(JUConstants.LABEL, vertName, UserData.SHARED);
			newVertex.addUserDatum(JUConstants.ACCEPTED, isAccept(origVertex), UserData.SHARED);
			if (RPNIBlueFringeLearner.isInitial(origVertex))
				newVertex.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
			newVertices.put(vertName,newVertex);g.addVertex(newVertex);
		}
		return newVertex;
	}
	
	/** A fast graph copy, which only copies labels and accept labelling. Transition labels are cloned.
	 * This one only copies vertices which participate in transitions. 
	 */
	public static DirectedSparseGraph copy(Graph g)
	{
		DirectedSparseGraph result = new DirectedSparseGraph();
		Map<String,CmpVertex> newVertices = new TreeMap<String,CmpVertex>();
		for(DirectedSparseEdge e:(Set<DirectedSparseEdge>)g.getEdges())
		{
			CmpVertex newSrc = copyVertex(newVertices,result,e.getSource()),
				newDst = copyVertex(newVertices, result, e.getDest());
			DirectedSparseEdge newEdge = new DirectedSparseEdge(newSrc,newDst);
			newEdge.addUserDatum(JUConstants.LABEL, ((HashSet)e.getUserDatum(JUConstants.LABEL)).clone(), UserData.SHARED);
			result.addEdge(newEdge);
		}
		return result;
	}
	
	/** Takes states associated with red in mergedVertices and finds a target state for a given input
	 * 
	 * @param mergedVertices vertices linked to r
	 * @param r the red state
	 * @param input the input to consider
	 * @return the target state, null if there is no transition with this input not only from r but also from all states associated to it
	 * using mergedVertices. 
	 */
	protected Vertex findNextRed(Map<Vertex,List<Vertex>> mergedVertices, Vertex r, String input)
	{
		Vertex target = null;
		List<Vertex> associatedVertices = mergedVertices.get(r);
		if (associatedVertices != null)
		{
			Iterator<Vertex> associatedIt = associatedVertices.iterator();
			while(associatedIt.hasNext() && target == null)
				target = transitionMatrix.get(associatedIt.next()).get(input);
		}
		return target;
	}

	public int computePairCompatibilityScore(StatePair origPair)
	{
		Map<Vertex,List<Vertex>> mergedVertices = new HashMap<Vertex,List<Vertex>>();// for every vertex of the model, gives a set of PTA vertices which were joined to it, for those of them which lead to a new (PTA-only) state
		// note that PTA states may easily be merged with other PTA states, in which case they will feature as keys of this set.
		return computePairCompatibilityScore_internal(origPair, mergedVertices);
	}
	
	public static DirectedSparseGraph mergeAndDeterminize(Graph graphToMerge, StatePair pair)
	{
			DirectedSparseGraph g = (DirectedSparseGraph)graphToMerge.copy();
			Vertex newBlue = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, pair.getQ().getUserDatum(JUConstants.LABEL),g);
			Vertex newRed = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, pair.getR().getUserDatum(JUConstants.LABEL),g);
			pair = new StatePair(newBlue,newRed);
			Map<Vertex,List<Vertex>> mergedVertices = new HashMap<Vertex,List<Vertex>>();
			ComputeStateScores s=new ComputeStateScores(g);
			if (s.computePairCompatibilityScore_internal(pair,mergedVertices) < 0)
				throw new IllegalArgumentException("elements of the pair are incompatible");

			// make a loop
			Set<String> usedInputs = new HashSet<String>();
			for(DirectedSparseEdge e:(Set<DirectedSparseEdge>)newBlue.getInEdges())
			{
				Vertex source = e.getSource();
				Collection<String> existingLabels = (Collection<String>)e.getUserDatum(JUConstants.LABEL);
				g.removeEdge(e);

				// It is possible that there is already an edge between g.getSource Blue and newRed
				Iterator<DirectedSparseEdge> sourceOutIt = source.getOutEdges().iterator();
				Edge fromSourceToNewRed = null;
				while(sourceOutIt.hasNext() && fromSourceToNewRed == null)
				{
					DirectedSparseEdge out = sourceOutIt.next();if (out.getDest() == newRed) fromSourceToNewRed = out;
				}
				if (fromSourceToNewRed == null)
				{
					fromSourceToNewRed = new DirectedSparseEdge(source,newRed);
					fromSourceToNewRed.setUserDatum(JUConstants.LABEL, existingLabels, UserData.CLONE);// no need to clone this one since I'll delete the edge in a bit
					g.addEdge(fromSourceToNewRed);
				}
				else
					// there is already a transition from source to newRed, hence all we have to do is merge the new labels into it.
					((Collection<String>)fromSourceToNewRed.getUserDatum(JUConstants.LABEL)).addAll( existingLabels );
					
			}

			// now the elements of mergedVertices are in terms of the copied graph.
			for(Vertex vert:(Set<Vertex>)g.getVertices())
				if (mergedVertices.containsKey(vert))
				{// there are some vertices to merge with this one.
					usedInputs.clear();usedInputs.addAll(s.transitionMatrix.get(vert).keySet());
					for(Vertex toMerge:mergedVertices.get(vert))
					{// for every input, I'll have a unique target state - this is a feature of PTA
					 // For this reason, every if multiple branches of PTA get merged, there will be no loops or parallel edges.
					// As a consequence, it is safe to assume that each input/target state combination will lead to a new state.
						Set<String> inputsFrom_toMerge = s.transitionMatrix.get(toMerge).keySet();
						for(String input:inputsFrom_toMerge)
							if (!usedInputs.contains(input))
							{
								Set<String> labels = new HashSet<String>();labels.add(input);
								Vertex targetVert = s.transitionMatrix.get(toMerge).get(input);
								DirectedSparseEdge newEdge = new DirectedSparseEdge(vert,targetVert);
								newEdge.addUserDatum(JUConstants.LABEL, labels, UserData.CLONE);
								g.removeEdges(targetVert.getInEdges());g.addEdge(newEdge);
							}
						usedInputs.addAll(inputsFrom_toMerge);
					}
				}
			
			// now remove everything related to the PTA
			Queue<Vertex> currentExplorationBoundary = new LinkedList<Vertex>();// FIFO queue containing vertices to be explored
			currentExplorationBoundary.add( newBlue );
			while(!currentExplorationBoundary.isEmpty())
			{
				Vertex currentVert = currentExplorationBoundary.remove();
				Set<DirectedSparseEdge> outEdges = (Set<DirectedSparseEdge>)currentVert.getOutEdges();
				for(DirectedSparseEdge e:outEdges)
					currentExplorationBoundary.add(e.getDest());
				g.removeEdges(outEdges);
				g.removeVertex(currentVert);
			}
			return g;
	}
	
	@Override
	public Object clone() throws CloneNotSupportedException
	{
		ComputeStateScores result = (ComputeStateScores)super.clone();
		result.transitionMatrix = new TreeMap<CmpVertex,Map<String,CmpVertex>>(); 
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:transitionMatrix.entrySet())
		{
			Map<String,CmpVertex> newValue = new TreeMap<String,CmpVertex>();
			newValue.putAll(entry.getValue());
			result.transitionMatrix.put(entry.getKey(),newValue);
		}
		pairsAndScores = new ArrayList<PairScore>(pairArraySize);
		return result;
	}

	/** Finds a vertex with a supplied name in a transition matrix.
	 */
	protected Vertex findVertex(String name)
	{
		Vertex result = null;
		Iterator<Entry<CmpVertex,Map<String,CmpVertex>>> entryIt = transitionMatrix.entrySet().iterator();
		while(entryIt.hasNext() && result == null)
		{
			Vertex currentVert = entryIt.next().getKey();
			String vertName = (String)currentVert.getUserDatum(JUConstants.LABEL);
			if (vertName.equals(name))
				result = currentVert;
		}
		return result;
	}
	
	public static ComputeStateScores mergeAndDeterminize(ComputeStateScores original, StatePair pair)
	{
			if (testMode) { checkPTAConsistency(original, (CmpVertex)pair.getQ());checkPTAIsTree(original,null,null,null); }
			Map<Vertex,List<Vertex>> mergedVertices = new HashMap<Vertex,List<Vertex>>();
			ComputeStateScores result;
			try {
				result = (ComputeStateScores)original.clone();
			} catch (CloneNotSupportedException e) {
				IllegalArgumentException ex = new IllegalArgumentException("failed to clone the supplied machine");
				ex.initCause(e);throw ex;
			}
			if (testMode) checkPTAConsistency(result, (CmpVertex)pair.getQ());
			
			if (original.computePairCompatibilityScore_internal(pair,mergedVertices) < 0)
				throw new IllegalArgumentException("elements of the pair are incompatible");

			// make a loop
			for(Entry<CmpVertex,Map<String,CmpVertex>> entry:original.transitionMatrix.entrySet())
			{
				for(Entry<String,CmpVertex> rowEntry:entry.getValue().entrySet())
					if (rowEntry.getValue() == pair.getQ())	
						// the transition from entry.getKey() leads to the original blue state, record it to be rerouted.
						result.transitionMatrix.get(entry.getKey()).put(rowEntry.getKey(), (CmpVertex) pair.getR());
			}

			Set<Vertex> ptaVerticesUsed = new HashSet<Vertex>();
			Set<String> inputsUsed = new HashSet<String>();

			// I iterate over the elements of the original graph in order to be able to update the target one.
			for(Entry<CmpVertex,Map<String,CmpVertex>> entry:original.transitionMatrix.entrySet())
			{
				Vertex vert = entry.getKey();
				Map<String,CmpVertex> resultRow = result.transitionMatrix.get(vert);// the row we'll update
				if (mergedVertices.containsKey(vert))
				{// there are some vertices to merge with this one.
					
					inputsUsed.clear();inputsUsed.addAll(entry.getValue().keySet());// the first entry is either a "derivative" of a red state or a branch of PTA into which we are now merging more states.
					for(Vertex toMerge:mergedVertices.get(vert))
					{// for every input, I'll have a unique target state - this is a feature of PTA
					 // For this reason, every if multiple branches of PTA get merged, there will be no loops or parallel edges.
					// As a consequence, it is safe to assume that each input/target state combination will lead to a new state
					// (as long as this combination is the one _not_ already present from the corresponding red state).
						boolean somethingWasAdded = false;
						for(Entry<String,CmpVertex> input_and_target:original.transitionMatrix.get(toMerge).entrySet())
							if (!inputsUsed.contains(input_and_target.getKey()))
							{
								resultRow.put(input_and_target.getKey(), input_and_target.getValue());
								inputsUsed.add(input_and_target.getKey());
								ptaVerticesUsed.add(input_and_target.getValue());somethingWasAdded = true;
								// Since PTA is a tree, a tree rooted at ptaVerticesUsed will be preserved in a merged automaton, however 
								// other parts of a tree could be merged into it. In this case, each time there is a fork corresponding to 
								// a step by that other chunk which the current tree cannot follow, that step will end in a tree and a root
								// of that tree will be added to ptaVerticesUsed.
							}
						assert somethingWasAdded : "RedAndBlueToBeMerged was not set correctly at an earlier stage";
					}
				}
			}
			
			// now remove everything related to the PTA
			Queue<Vertex> currentExplorationBoundary = new LinkedList<Vertex>();// FIFO queue containing vertices to be explored
			currentExplorationBoundary.add( pair.getQ() );
			while(!currentExplorationBoundary.isEmpty())
			{
				Vertex currentVert = currentExplorationBoundary.remove();
				if (!ptaVerticesUsed.contains(currentVert))
				{// once a single used PTA vertex is found, all vertices from it (which do not have 
					// any transition leading to states in the red portion of the graph, by construction 
					// of ptaVerticesUsed) have been appended to the transition diagram and
					// hence we should not go through its target states.
					for(Entry<String,CmpVertex> input_and_target:original.transitionMatrix.get(currentVert).entrySet())
						currentExplorationBoundary.offer(input_and_target.getValue());

					result.transitionMatrix.remove(currentVert);// remove the vertex from the resulting transition table.
				}
			}
			
			if (testMode) checkPTAIsTree(result, original, pair,ptaVerticesUsed);
			return result;
	}

	protected static void checkPTAConsistency(ComputeStateScores original, Vertex blueState)
	{
		assert testMode : "this one should not run when not under test";
		Queue<Vertex> currentBoundary = new LinkedList<Vertex>();// FIFO queue containing vertices to be explored
		Set<Vertex> ptaStates = new HashSet<Vertex>();
		currentBoundary.add( blueState );
		while(!currentBoundary.isEmpty())
		{
			Vertex current = currentBoundary.remove();
			if (ptaStates.contains(current))
				throw new IllegalArgumentException("PTA has multiple paths to "+current);
			ptaStates.add(current);
			for(Entry<String,CmpVertex> input_and_target:original.transitionMatrix.get(current).entrySet())
				currentBoundary.offer(input_and_target.getValue());
		}
		
		// now check that no existing states refer to PTA states
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:original.transitionMatrix.entrySet())
			if (!ptaStates.contains(entry.getKey()))
			{// this is a non-pta state, check where it points to
				for(Entry<String,CmpVertex> input_and_target:entry.getValue().entrySet())
					if (ptaStates.contains(input_and_target.getValue()) && input_and_target.getValue() != blueState)
						throw new IllegalArgumentException("non-pta state "+entry.getKey()+" ("+entry.getKey().getUserDatum(JUConstants.COLOUR)+") refers to PTA state "+input_and_target.getValue()+", blue state is "+blueState);
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
	protected static void checkPTAIsTree(ComputeStateScores mergeResult,ComputeStateScores original, 
			StatePair pair,Collection<Vertex> notRemoved)
	{
		assert testMode : "this one should not run when not under test";
	
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
					p.getKey().containsUserDatumKey(JUConstants.COLOUR) && p.getKey().getUserDatum(JUConstants.COLOUR) != JUConstants.RED)
				throw new IllegalArgumentException("non-red vertex "+p.getKey()+" has multiple incoming transitions");
				
		// The second check: trying to find states which have become unreachable in the course of merging
		// but were not removed at the end of the merging process. 
		Queue<Vertex> currentBoundary = new LinkedList<Vertex>();// FIFO queue containing vertices to be explored
		Set<Vertex> visitedStates = new HashSet<Vertex>();
		currentBoundary.add( mergeResult.init );visitedStates.add(mergeResult.init);
		while(!currentBoundary.isEmpty())
		{
			Vertex current = currentBoundary.remove();
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
				Vertex current = currentBoundary.remove();
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
				for(Vertex u:unreachables)
					response=response+" "+u+"("+hasIncoming.get(u)+")";
				System.out.println(response);
				
				Vertex InterestingUnreachableVertex = unreachables.iterator().next();
				List<String> seq = original.computePathsBetween(pair.getQ(), InterestingUnreachableVertex).iterator().next();
				System.out.println(seq);// dumps a seq from a blue state to the first unreachable vertex (after merging) which was on the PTA in the original machine.
				Map<Vertex,List<Vertex>> mergedVertices = new HashMap<Vertex,List<Vertex>>();
				if (original.computePairCompatibilityScore_internal(pair,mergedVertices) < 0)
					throw new IllegalArgumentException("elements of the pair are incompatible in the original machine, no idea why they got merged in the first place");
				
				System.out.println(pair);
				
				// Now I could dump what was merged and where, in order to try to trace what happened.
				// This is done by traversing a branch of a tree (from the blue state to the unreachable one)
				// which is used by the merging algorithm and dumping the results.
				Vertex v = pair.getR();
				Iterator<String> seqIt = seq.iterator();
				while(seqIt.hasNext() && v != null)
				{
					String input = seqIt.next();
					System.out.print(v.toString()+" "+original.transitionMatrix.get(v).keySet()+" input "+input+" ");
					List<Vertex> extra = mergedVertices.get(v);
					if (extra != null) 
						for(Vertex ev:extra)
							System.out.print(" "+ev.toString()+" : "+original.transitionMatrix.get(ev).keySet());
					System.out.println();
					Vertex newV = original.transitionMatrix.get(v).get(input);
					if (newV != null) v=newV;
					else v=original.findNextRed(mergedVertices,v,input);
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
			else
				throw new IllegalArgumentException("vertices "+unreachables.toString()+" are unreachable and "+remaining+" are non-PTA vertices");
				
		}
	}
	
	/** After merging, a graph may exhibit non-determinism, in which case it is made deterministic
	 * by merging nodes. For instance, for A->B and A->C being a non-deterministic choice at node A, 
	 * nodes B and C are to
	 * be merged. The idea is to keep merging such states until it either has to do a contradictory 
	 * merge (accept and reject states) or no merged states exhibit non-determinism. This function
	 * performs a `virtual' merge and populates the list of possible mergers.
	 * It also computes a compatibility score between a pair of states.
	 * 
	 * The scores computed are different from what computeScore returns since in the situation when a graph
	 * is a straight line, computeScore happily matches PTA with itself while this one has to know
	 * when we've looped (so as to effectively check for compatibility) and hence it cannot do that. 
	 * 
	 * @param model
	 * @param mergedVertices records which vertices have to be merged.
	 * @return a pair of states to be merged or null if the graph is deterministic.
	 */
	protected int computePairCompatibilityScore_internal(StatePair origPair,Map<Vertex,List<Vertex>> mergedVertices) 
	{
		mergedVertices.clear();// for every vertex of the model, gives a set of PTA vertices which were joined to it, for those of them which lead to a new (PTA-only) state
			// note that PTA states may easily be merged with other PTA states, in which case they will feature as keys of this set.
		
		int score = -1;// compatibility score between states in the pair
		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue containing pairs to be explored
		Queue<Boolean> currentRedFromPta = new LinkedList<Boolean>();// FIFO queue containing true if the red node comes from a branch of a PTA which has been previously already merged into the machine
		currentExplorationBoundary.add(origPair);currentRedFromPta.add(false);
		
		
		while(!currentExplorationBoundary.isEmpty())
		{
			StatePair currentPair = currentExplorationBoundary.remove();Boolean redFromPta = currentRedFromPta.remove();
			boolean RedAndBlueToBeMerged = false;// this one is set to true if states in the current pair have to be merged. 
			// This will be so for all state pairs where a blue node can 
			// make moves which the red one cannot match. The term "merged" does not refer to whether 
			// two nodes are actually merged - they have to be anyway, however if there are sequences of 
			// nodes with identical moves, PTA nodes do not contribute to anything - we only need
			// to consider those which branch. mergedVertices is only updated when we find a blue vertex which 
			// can accept input a red node cannot accept. 
			
			if (isAccept(currentPair.getQ()) != isAccept(currentPair.getR()))
				return -1;// incompatible states
			if (!redFromPta.booleanValue())
				++score;
			Map<String,CmpVertex> targetBlue = transitionMatrix.get(currentPair.getQ());

			for(Entry<String,CmpVertex> blueEntry:targetBlue.entrySet())
			{
				Vertex nextRedState = transitionMatrix.get(currentPair.getR()).get(blueEntry.getKey());
				if (nextRedState != null)
				{// both states can make a transition - this would be the case of "non-determinism" for Merge&Determinize
					
					// PTA does not have loops, but the original automaton has
					// and one of those loops is not on the transition diagram, namely the one related to B=A
					if (nextRedState == origPair.getQ())
					{
						nextRedState = origPair.getR(); // emulates the new loop
						redFromPta = !useCompatibilityScore; // and since the original score computation algorithm cannot do this, we pretend to be unable to do this either
					}

					StatePair nextStatePair = new StatePair(blueEntry.getValue(),nextRedState);
					currentExplorationBoundary.offer(nextStatePair);currentRedFromPta.offer(redFromPta);
				}
				else
				{// the current red state cannot make a transition, perhaps PTA states associated with it can
					nextRedState = findNextRed(mergedVertices,currentPair.getR(),blueEntry.getKey());
					if (nextRedState != null)
					{// both states can make a transition - this would be the case of "non-determinism" for Merge&Determinize
					 // The red state is the one originally from a previously-merged PTA branch, so here we are merging PTA with itself. 

						// Since we are merging PTA with itself and PTA does not have loops, we cannot reenter the original blue state. Moreover,
						// since we called findNextRed, we are looking at transitions from the PTA states. For this reason, we cannot enter the 
						// blue state since PTA does not have loops.
						assert nextRedState != origPair.getQ() : "inconsistent PTA";
						
						StatePair nextStatePair = new StatePair(blueEntry.getValue(),nextRedState);
						currentExplorationBoundary.offer(nextStatePair);currentRedFromPta.offer(!useCompatibilityScore);// from now on, no increments to the score
					}
					else
					{
						// If the blue can make a move, but the red one cannot, it means that the blue vertex has 
						// transitions with labels which are not contained in the set of labels on 
						// transitions from the red state. For this reason, we have to merge the current blue vertex with the current red one.
						RedAndBlueToBeMerged = true;
						
						// there is no point exploring further since the transition leaving the blue state is not matched to any red one.
					}
				}
			}
			
			if (RedAndBlueToBeMerged)
			{// if the current pair of states is to be merged, do it (i.e. record them as merged).
				List<Vertex> redMerged = mergedVertices.get(currentPair.getR());
				if (redMerged == null)
				{
					redMerged = new LinkedList<Vertex>();mergedVertices.put(currentPair.getR(), redMerged);
				}
				redMerged.add(currentPair.getQ());
			}
		}
		return score;
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
	
	public ComputeStateScores setMode(IDMode m)
	{
		id_mode = m;return this;
	}
	
	/** Generates vertice IDs. */
	public String nextID(boolean accepted)
	{
		if (id_mode == IDMode.POSITIVE_ONLY)
			return "V"+vertPositiveID++;
		else return (accepted?"P"+vertPositiveID++:"N"+vertNegativeID++);
	}
	
	/** This one is similar to the above but does not add a vertex to the graph - I need this behaviour when
	 * concurrently processing graphs. 
	 *  
	 * @param prevState the state from which to start the new edge
	 * @param accepted whether the vertex to add should be an accept one
	 * @param input the label of the edge
	 * @return the new vertex.
	 */
	private CmpVertex addVertex(CmpVertex prevState, boolean accepted, String input)
	{
		assert Thread.holdsLock(syncObj);
		CmpVertex newVertex = new DeterministicDirectedSparseGraph.DeterministicVertex();
		newVertex.addUserDatum(JUConstants.LABEL, 
				nextID(accepted), 
				UserData.SHARED);
		newVertex.setUserDatum(JUConstants.ACCEPTED, accepted, UserData.SHARED);
		transitionMatrix.put(newVertex, new TreeMap<String,CmpVertex>());
		transitionMatrix.get(prevState).put(input,newVertex);
		return newVertex;
	}

	public void initPTA()
	{
		transitionMatrix.clear();
		init = new DeterministicDirectedSparseGraph.DeterministicVertex();
		init.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
		init.addUserDatum(JUConstants.LABEL, "Init", UserData.SHARED);
		init.setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
		//graph.setUserDatum(JUConstants.TITLE, "Hypothesis machine", UserData.SHARED);
		transitionMatrix.put(init,new TreeMap<String,CmpVertex>());
		pairsAndScores = new ArrayList<PairScore>(pairArraySize);
	}
	
	public ComputeStateScores augmentPTA(Collection<List<String>> strings, boolean accepted)
	{
		for(List<String> sequence:strings)
			augmentPTA(sequence, accepted);
		return this;
	}
	
	public ComputeStateScores augmentPTA(List<String> sequence, boolean accepted)
	{
		CmpVertex currentState = init, prevState = null;
		Iterator<String> inputIt = sequence.iterator();
		String lastInput = null;
		int position = 0;
		while(inputIt.hasNext() && currentState != null)
		{
			if (!isAccept(currentState))
			{// not the last state and the already-reached state is not accept, while all prefixes of reject sequences should be accept ones. 
				currentState.addUserDatum(JUConstants.HIGHLIGHT, "whatever", UserData.SHARED);
				throw new IllegalArgumentException("incompatible "+(accepted?"accept":"reject")+" labelling: "+sequence.subList(0, position));
			}
			prevState = currentState;lastInput = inputIt.next();++position;
			
			currentState = transitionMatrix.get(prevState).get(lastInput);
		}
		
		if (currentState == null)
		{// the supplied path does not exist in PTA, the first non-existing vertex is from state prevState with label lastInput

			synchronized (syncObj) {
				while(inputIt.hasNext())
				{
					prevState = addVertex(prevState, true, lastInput);
					lastInput = inputIt.next();
				}
				// at this point, we are at the end of the sequence. Last vertex is prevState and last input if lastInput
				addVertex(prevState, accepted, lastInput);
			}
			
		}
		else
		{// we reached the end of the PTA
			if (isAccept(currentState) != accepted)
			{
				currentState.addUserDatum(JUConstants.HIGHLIGHT, "whatever", UserData.SHARED);
				throw new IllegalArgumentException("incompatible "+(accepted?"accept":"reject")+" labelling: "+sequence.subList(0, position));
			}
			
		}
	
		return this;
	}
	
	/** Builds a Jung graph corresponding to the state machine stored in transitionMatrix.
	 * Note that all states in our transition diagram (transitionMatrix) have Jung vertices associated with them (CmpVertex).
	 * 
	 * @return constructed graph.
	 */
	public DirectedSparseGraph getGraph()
	{
		DirectedSparseGraph result = null;
		synchronized (syncObj) 
		{
			result = new DirectedSparseGraph();
			result.setUserDatum(JUConstants.TITLE, "the graph from ComputeStateScores",UserData.SHARED);
			Map<Vertex,Vertex> oldVertexToNewVertex = new HashMap<Vertex,Vertex>();
			Map<Vertex,Set<String>> targetStateToEdgeLabels = new LinkedHashMap<Vertex,Set<String>>();
			for(Entry<CmpVertex,Map<String,CmpVertex>> entry:transitionMatrix.entrySet())
				oldVertexToNewVertex.put(entry.getKey(), (Vertex)entry.getKey().copy(result));
			
			for(Entry<CmpVertex,Map<String,CmpVertex>> entry:transitionMatrix.entrySet())
			{
				targetStateToEdgeLabels.clear();
				for(Entry<String,CmpVertex> sv:entry.getValue().entrySet())
				{
					Set<String> labels = targetStateToEdgeLabels.get(sv.getValue());
					if (labels != null)
					// there is an edge already with the same target state from the current vertice, update the label on it
						labels.add(sv.getKey());
					else
					{// add a new edge
						Vertex src = oldVertexToNewVertex.get(entry.getKey()), dst = oldVertexToNewVertex.get(sv.getValue());
						if (src == null)
							throw new IllegalArgumentException("Source vertex "+entry.getKey()+" is not in the transition table");
						if (dst == null)
							throw new IllegalArgumentException("Target vertex "+sv.getValue()+" is not in the transition table, referred to from vertex "+entry.getKey());
						DirectedSparseEdge e = new DirectedSparseEdge(src,dst);
						labels = new HashSet<String>();labels.add(sv.getKey());
						targetStateToEdgeLabels.put(sv.getValue(), labels);
						e.addUserDatum(JUConstants.LABEL, labels, UserData.CLONE);
						result.addEdge(e);			
					}
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
	public void buildFlowgraph()
	{
		
	}
	
	/** Calculates statistics on the learned machine
	 * 
	 * @param computeW whether to compute the W set - not a good idea on 13500 state PTA, for instance.
	 * @return statistics
	 */
	public String getStatistics(boolean computeW) {
		int edgeCounter = 0;
		for(Entry<CmpVertex,Map<String,CmpVertex>> v:transitionMatrix.entrySet())
			edgeCounter+=v.getValue().size();
		
		FSMStructure fsm = WMethod.getGraphData(getGraph());
		
		String wsetDetails = "";
		try
		{
			if (computeW) wsetDetails = "Wset: "+WMethod.computeWSet(fsm).size()+" seq";
		}
		catch (EquivalentStatesException e) {
			wsetDetails = e.toString();
		}
		return "vert: "+transitionMatrix.keySet().size()+" edges: "+edgeCounter+" alphabet: "+WMethod.computeAlphabet(fsm).size()+" unreachable: "+WMethod.checkUnreachableStates(fsm)+" "+wsetDetails;
	}
	
	
	@Override
	public String toString()
	{
		return "states: "+transitionMatrix.size()+" (hash "+transitionMatrix.hashCode()+")";
	}

	public void bumpPositive() {
		bumpPositives  = true;
	}
	
	public void useCompatibilityScore()
	{
		useCompatibilityScore = true;
	}
}

