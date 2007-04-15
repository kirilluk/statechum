/**
 * 
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

import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.PTATestSequenceEngine;
import statechum.xmachine.model.testset.PrefixFreeCollection;
import statechum.xmachine.model.testset.SlowPrefixFreeCollection;
import statechum.xmachine.model.testset.WMethod;
import statechum.xmachine.model.testset.PTATestSequenceEngine.FSMAbstraction;
import statechum.xmachine.model.testset.WMethod.EquivalentStatesException;
import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class computeStateScores implements Cloneable {
	private DirectedSparseGraph graph;
	/** The initial vertex. */
	private Vertex init;
	
	private Map<Vertex,Map<String,Vertex>> transitionMatrix = new LinkedHashMap<Vertex,Map<String,Vertex>>();
			
	/** Stores all red-blue pairs; has to be backed by array for the optimal performance of the sort function. */
	protected List<computeStateScores.PairScore> pairsAndScores;
	
	protected int generalisationThreshold;
	protected int pairsMergedPerHypothesis;

	public static final int pairArraySize = 2000;
	
	public List<String> extractReds()
	{
		List<String> result = new LinkedList<String>();
		for(Vertex v:transitionMatrix.keySet())
			if (v.containsUserDatumKey("colour")&& v.getUserDatum("colour").equals("red"))
					result.add(v.getUserDatum(JUConstants.LABEL).toString());
		
		return result;
	}
	
	public void assignReds(Set<String> reds)
	{
		for(Vertex v:transitionMatrix.keySet())
		{
			v.removeUserDatum("colour");
			if (reds.contains(v.getUserDatum(JUConstants.LABEL)))
				v.addUserDatum("colour","red", UserData.SHARED);
		}
	}
	
	/** Initialises the class used to compute scores between states.
	 * 
	 * @param g the graph it will be used on 
	 * @param sinkVertexName the name for a sink vertex, to be different from names of all vertices on the graph
	 */
	public computeStateScores(DirectedSparseGraph g,String sinkVertexName)
	{
		graph = g;
		init = TestRpniLearner.findVertex(JUConstants.PROPERTY, JUConstants.INIT, graph);
		
		pairsAndScores = new ArrayList<computeStateScores.PairScore>(pairArraySize);//graphVertices.size()*graphVertices.size());
		for(Vertex v:(Set<Vertex>)g.getVertices())
		{
			assert !sinkVertexName.equals(v);
			transitionMatrix.put(v,new TreeMap<String,Vertex>());// using TreeMap makes everything predictable
			v.removeUserDatum("colour");
		}
		init.addUserDatum("colour", "red", UserData.SHARED);
	
		Iterator<DirectedSparseEdge> edgeIter = g.getEdges().iterator();
		while(edgeIter.hasNext())
		{	
			DirectedSparseEdge e = edgeIter.next();
			Map<String,Vertex> outgoing = transitionMatrix.get(e.getSource());
			// The line below aims to ensure that inputs are evaluated by computeStateScore in a specific order, which in conjunction with the visited set of computeStateScore permits emulating a bug in computeScore
			for(String label:(HashSet<String>)e.getUserDatum(JUConstants.LABEL))
				outgoing.put(label, e.getDest());			
		}
	}

	public computeStateScores(int newGeneralisationThreshold, int newPairsMergedPerHypothesis)
	{
		generalisationThreshold = newGeneralisationThreshold;
		pairsMergedPerHypothesis = newPairsMergedPerHypothesis;
		
		graph = null;
		init = new DirectedSparseVertex();
		init.addUserDatum("property", "init", UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);
		init.addUserDatum(JUConstants.LABEL, "Init", UserData.SHARED);
		init.setUserDatum("colour", "red", UserData.SHARED);
		//graph.setUserDatum(JUConstants.TITLE, "Hypothesis machine", UserData.SHARED);
		transitionMatrix.put(init,new TreeMap<String,Vertex>());
		pairsAndScores = new ArrayList<computeStateScores.PairScore>(pairArraySize);
	}
	
	public static Vertex getTempRed_DijkstraShortestPath(DirectedSparseGraph model, Vertex r, DirectedSparseGraph temp){
		DijkstraShortestPath p = new DijkstraShortestPath(model);
		List<Edge> pathToRed = p.getPath(TestRpniLearner.findVertex(JUConstants.PROPERTY, JUConstants.INIT,model), r);
		Vertex tempRed = null;
		if(!pathToRed.isEmpty()){
			List<String> pathToRedString = new LinkedList<String>();
			for(Edge e:pathToRed)
				pathToRedString.add( ((Collection<String>)e.getUserDatum(JUConstants.LABEL)).iterator().next() );
			tempRed = TestRpniLearner.getVertex(temp, pathToRedString);
		}
		else
			tempRed = TestRpniLearner.findVertex(JUConstants.PROPERTY, JUConstants.INIT, temp);
		return tempRed;
	}
	
	public Vertex getTempRed_internal(Vertex r, DirectedSparseGraph temp){
		Queue<Vertex> currentExplorationBoundary = new LinkedList<Vertex>();// FIFO queue
		HashSet<Vertex> visitedVertices = new HashSet<Vertex>();visitedVertices.add(init);
		currentExplorationBoundary.add(init);
		Vertex tempInit = TestRpniLearner.findVertex(JUConstants.PROPERTY, JUConstants.INIT, temp);
		Queue<Vertex> currentTempExploration = new LinkedList<Vertex>();currentTempExploration.add(tempInit);
		while(!currentExplorationBoundary.isEmpty())
		{
			Vertex currentVertex = currentExplorationBoundary.remove();Vertex currentTemp = currentTempExploration.remove();
			if (currentVertex == r)
				return currentTemp;
			Set<Edge> tempOutgoing = (Set<Edge>)currentTemp.getOutEdges();

			for(DirectedSparseEdge outgoing:(Set<DirectedSparseEdge>)currentVertex.getOutEdges())
			{// after merging, the languages increases and transitions are not split, 
			 // hence for every transition in the original graph, there would be the same or bigger transition in the temp graph 
				Vertex nextVertex = outgoing.getDest();
				if (!visitedVertices.contains(nextVertex))
				{// now find the corresponding transition in the temp graph
					Vertex tempNextVertex = RPNIBlueFringeLearner.getEdgeWithLabel(tempOutgoing, 
							((Set<String>)outgoing.getUserDatum(JUConstants.LABEL)).iterator().next()).getDest();
					currentExplorationBoundary.offer(nextVertex);visitedVertices.add(nextVertex);currentTempExploration.offer(tempNextVertex);
				}
			}
		}
		throw new IllegalArgumentException("failed to find a red vertex");
	}
	
	// TODO to test with red = init, with and without loop around it (red=init and no loop is 3_1), with and without states which cannot be reached from a red state,
	// where a path in the original machine corresponding to a path in the merged one exists or not (tested with 3_1)
	public List<List<String>> computeQS(StatePair pair, DirectedSparseGraph temp)
	{
		DijkstraShortestPath shortestPathDijkstra = new DijkstraShortestPath(temp);
		List<List<String>> sp = new LinkedList<List<String>>();sp.add(new LinkedList<String>());

		Vertex tempInit = TestRpniLearner.findVertex(JUConstants.PROPERTY, JUConstants.INIT, temp),
		tempRed = getTempRed_internal(pair.getR(), temp);
		for(Edge e: (List<Edge>)shortestPathDijkstra.getPath(tempInit, tempRed))
			sp = WMethod.crossWithSet_One(sp, (HashSet<String>)e.getUserDatum(JUConstants.LABEL));
		Edge loopEdge = TestRpniLearner.findEdge(tempRed, tempRed);
		List<List<String>> prefixOfAllPathsFromRedState = new LinkedList<List<String>>();prefixOfAllPathsFromRedState.add(new LinkedList<String>());
		if(loopEdge!=null)
			prefixOfAllPathsFromRedState.addAll(WMethod.makeSingleton((Collection<String>)loopEdge.getUserDatum(JUConstants.LABEL)));

		PrefixFreeCollection partialQuestions = new SlowPrefixFreeCollection(); 

		// now we build a sort of a "transition cover" from the tempRed state, in other words, take every vertex and 
		// build a path from tempRed to it
		for(Vertex v:(Set<Vertex>)temp.getVertices())
		{
			List<Edge> pathInTemp = (List<Edge>)shortestPathDijkstra.getPath(tempRed, v);
			// now we need to check whether the expected state is actually reached by the provided path - if a path is empty, either v = tempRed or v is unreachable
			if (!pathInTemp.isEmpty() || v == tempRed)
			{
				for(List<String> loopPrefix:prefixOfAllPathsFromRedState)
				{
					List<List<String>> fullPath = new LinkedList<List<String>>();LinkedList<String> fullPathInitialSeq = new LinkedList<String>();fullPathInitialSeq.addAll(loopPrefix);fullPath.add(fullPathInitialSeq);
					Vertex origState = loopPrefix.isEmpty()?pair.getR():transitionMatrix.get(pair.getR()).get(loopPrefix.get(0));// the first element always exists
					Iterator<Edge> pathIt = pathInTemp.iterator();
					// Now we trace this path in the original machine, following the existing matrix
					while(pathIt.hasNext() && origState != null)
					{
						Collection<String> labels = (Collection<String>)pathIt.next().getUserDatum(JUConstants.LABEL);
						String input = labels.iterator().next();
						fullPath = WMethod.crossWithSet_One(fullPath, labels);
						origState = transitionMatrix.get(origState).get(input);
					}
					while(pathIt.hasNext())
						fullPath = WMethod.crossWithSet_One(fullPath, (Collection<String>)pathIt.next().getUserDatum(JUConstants.LABEL));
					
					Map<String,Vertex> row = origState == null? null:transitionMatrix.get(origState);
					// now we know the state in the original graph which corresponds to v
					for(Edge e:(Set<Edge>)v.getOutEdges())
						for(String input:(Collection<String>)e.getUserDatum(JUConstants.LABEL))
							if (row == null || !row.containsKey(input))
							{
								for(List<String> path:fullPath)
								{
									List<String> ending = new LinkedList<String>();ending.addAll(path);ending.add(input);partialQuestions.addSequence(ending);
								}
							}
				}
			}
		}
		return WMethod.cross(sp, partialQuestions.getData());
	}

	Collection<List<String>> computePathsToRed(Vertex red)
	{
		return computePathsBetween(init, red);
	}
	
	/** Computes all possible shortest paths from the supplied source state to the supplied target state 
	 * 
	 * @param vertSource the source state
	 * @param vertTarget the target state
	 * @return sequences of inputs to follow all paths found.
	 */	
	Collection<List<String>> computePathsBetween(Vertex vertSource, Vertex vertTarget)
	{
		List<List<String>> sp = new LinkedList<List<String>>();

		Set<Vertex> visitedStates = new HashSet<Vertex>();visitedStates.add(vertSource);
		LinkedList<Vertex> initPath = new LinkedList<Vertex>();initPath.add( vertSource );
		Queue<LinkedList<Vertex>> currentExplorationPath = new LinkedList<LinkedList<Vertex>>();// FIFO queue containing paths to states to be explored
		currentExplorationPath.add(initPath);
		LinkedList<Vertex> currentPath = null;Vertex currentVert = null;
		while(currentVert != vertTarget && !currentExplorationPath.isEmpty())
		{
			currentPath = currentExplorationPath.remove();
			currentVert = currentPath.getLast();
			if (currentVert != vertTarget)
				// we have not reached the red state, yet
				for(Vertex targetVertex:transitionMatrix.get(currentVert).values())
					if (!visitedStates.contains(targetVertex))
					{
						LinkedList<Vertex> newPath = new LinkedList<Vertex>();newPath.addAll(currentPath);newPath.add(targetVertex);
						currentExplorationPath.offer(newPath);
						visitedStates.add(currentVert);
					}
		}

		if (currentVert == vertTarget && vertTarget != null)
		{// the path to the red state has been found.
			sp.add(new LinkedList<String>());// add a singleton path.
			Iterator<Vertex> vertIt = currentPath.iterator();
			Vertex prevVert = vertIt.next();
			List<String> inputsToMultWith = new LinkedList<String>();
			while(vertIt.hasNext())
			{
				currentVert = vertIt.next();
				inputsToMultWith.clear();
				for(Entry<String,Vertex> entry:transitionMatrix.get(prevVert).entrySet())
					if (entry.getValue() == currentVert)
						inputsToMultWith.add(entry.getKey());
				sp = WMethod.crossWithSet(sp, inputsToMultWith);
				prevVert = currentVert;
			}
		}
		return sp;
	}

	private static void buildQuestionsFromPair(computeStateScores temp, Vertex initialRed, PTATestSequenceEngine.sequenceSet initialBlueStates)
	{
		// now we build a sort of a "transition cover" from the tempRed state, in other words, take every vertex and 
		// build a path from tempRed to it, at the same time tracing it through the current machine.

		Set<Vertex> visitedStates = new HashSet<Vertex>();visitedStates.add(initialRed);
		Queue<Vertex> currentExplorationBoundary = new LinkedList<Vertex>();// FIFO queue containing vertices to be explored
		Queue<PTATestSequenceEngine.sequenceSet> currentExplorationTargetStates = new LinkedList<PTATestSequenceEngine.sequenceSet>();
		currentExplorationBoundary.add(initialRed);
		currentExplorationTargetStates.add(initialBlueStates);

		Map<Vertex,List<String>> targetToInputSet = new LinkedHashMap<Vertex,List<String>>();
		while(!currentExplorationBoundary.isEmpty())
		{
			Vertex currentVert = currentExplorationBoundary.remove();
			PTATestSequenceEngine.sequenceSet currentPaths = currentExplorationTargetStates.remove();
			targetToInputSet.clear();
			
			currentPaths.crossWithSet(temp.transitionMatrix.get(currentVert).keySet());
			for(Entry<String,Vertex> entry:temp.transitionMatrix.get(currentVert).entrySet())
				if (!visitedStates.contains(entry.getValue()))
				{
					List<String> inputs = targetToInputSet.get(entry.getValue());
					if (inputs == null)
					{
						inputs = new LinkedList<String>();targetToInputSet.put(entry.getValue(),inputs);
					}
					inputs.add(entry.getKey());
				}
			for(Entry<Vertex,List<String>> target:targetToInputSet.entrySet())
			{
				visitedStates.add(target.getKey());
				currentExplorationBoundary.offer(target.getKey());
				currentExplorationTargetStates.offer(currentPaths.crossWithSet(target.getValue()));
			}
		}
		
	}
	
	private class NonExistingPaths implements FSMAbstraction
	{
		private final Vertex red;
		
		public NonExistingPaths(Vertex redState)
		{
			red = redState;
		}
		
		public Object getInitState() {
			return red;
		}
	
		public final Vertex junkVertex = new DirectedSparseVertex();
		
		public Object getNextState(Object currentState, String input) 
		{
			Vertex result = null;
			Map<String,Vertex> row = transitionMatrix.get(currentState);
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
		
	public List<List<String>> computeQS(final StatePair pair, computeStateScores temp)
	{
		Vertex tempRed = temp.findVertex((String)pair.getR().getUserDatum(JUConstants.LABEL));
		Collection<List<String>> sp = temp.computePathsToRed(tempRed);
		if (sp == null || sp.isEmpty())
			throw new IllegalArgumentException("failed to find the red state in the merge result");
	
		PTATestSequenceEngine engine = new PTATestSequenceEngine();
		engine.init(new NonExistingPaths(pair.getR()));
		
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();paths.setIdentity();
		buildQuestionsFromPair(temp, tempRed, paths);
		for(Entry<String,Vertex> loopEntry:temp.transitionMatrix.get(tempRed).entrySet())
			if (loopEntry.getValue() == tempRed)
			{// Note an input corresponding to any loop in temp can be followed in the original machine, since
				// a loop in temp is either due to the merge or because it was there in the first place.
				List<String> initialSeq = new LinkedList<String>();initialSeq.add(loopEntry.getKey());
				buildQuestionsFromPair(temp, loopEntry.getValue(),paths.crossWithSet(initialSeq));
			}
		return WMethod.cross(sp, engine.getData());
	}

	
	
	/** Computes scores by navigating a cross-product of this machine, with itself.
	 * 
	 *  @param the pair to compute a score for
	 *  @return the resulting score, reflecting compatibility.
	 */
	protected int computeStateScore(StatePair pair)
	{
		if (TestRpniLearner.isAccept(pair.getR()) != TestRpniLearner.isAccept(pair.getQ()))
			return -1;
		//System.out.println("computing scores for "+pair);
		int score = 0;
		
		assert pair.getQ() != pair.getR();
		
		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
		currentExplorationBoundary.add(pair);
		
		while(!currentExplorationBoundary.isEmpty())
		{
			StatePair currentPair = currentExplorationBoundary.remove();
			Map<String,Vertex> targetRed = transitionMatrix.get(currentPair.getR()),
				targetBlue = transitionMatrix.get(currentPair.getQ());

			for(Entry<String,Vertex> redEntry:targetRed.entrySet())
			{
				Vertex nextBlueState = targetBlue.get(redEntry.getKey());
				if (nextBlueState != null)
				{// both states can make a transition
					// if the red side is currently in the sink vertex, i.e. we are effectively calculating a set of questions, do not report inconsistency or increment the score
						if (TestRpniLearner.isAccept(redEntry.getValue()) != TestRpniLearner.isAccept(nextBlueState))
							return -1;// incompatible states
					
					++score;

					if (((DirectedSparseVertex)nextBlueState).numSuccessors() > 0)
					{
						StatePair nextStatePair = new StatePair(nextBlueState,redEntry.getValue());
						currentExplorationBoundary.offer(nextStatePair);
					}
				}
				// if the red can make a move, but the blue one cannot, ignore this case.
			}
		}		
		return score;
	}
	
	public static class PairScore extends StatePair implements Comparable
	{
		protected final int score;

		public PairScore(Vertex q, Vertex r, int sc) {
			super(q, r);
			score = sc;
		}
		
		public int getScore() {
			return score;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int PRIME = 31;
			int result = super.hashCode();
			result = PRIME * result + score;
			return result;
		}

		public int compareTo(Object b){
			computeStateScores.PairScore pB = (computeStateScores.PairScore)b;
			if (score != pB.score)
				return score < pB.score? -1:1;
			return super.compareTo(b);
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (!super.equals(obj))
				return false;
			if (!(obj instanceof PairScore))
				return false;
			final computeStateScores.PairScore other = (computeStateScores.PairScore) obj;
			if (score != other.score)
				return false;
			return true;
		}
		
		public String toString(){
			return "[ "+getQ().getUserDatum(JUConstants.LABEL)+", "+getR().getUserDatum(JUConstants.LABEL)+" : "+score+" ]";
		}
	}

	public Vertex getVertex(List<String> seq)
	{
		Vertex result = init;
		Iterator<String> seqIt = seq.iterator();
		while(seqIt.hasNext() && result != null)
			result = transitionMatrix.get(result).get(seqIt.next());
		
		return result;
	}
	
	
	protected Stack<StatePair> chooseStatePairs()
	{
		pairsAndScores.clear();
		Set<Vertex> reds = new LinkedHashSet<Vertex>();
		for(Vertex v:transitionMatrix.keySet())
			if (v.containsUserDatumKey("colour") && v.getUserDatum("colour").equals("red"))
				reds.add(v);

		Queue<Vertex> currentExplorationBoundary = new LinkedList<Vertex>();// FIFO queue
		currentExplorationBoundary.addAll(reds);
		List<Vertex> BlueStatesConsideredSoFar = new LinkedList<Vertex>();
		while(!currentExplorationBoundary.isEmpty())
		{
			Vertex currentRed = currentExplorationBoundary.remove();
			for(Entry<String,Vertex> BlueEntry:transitionMatrix.get(currentRed).entrySet())
				if (!BlueEntry.getValue().containsUserDatumKey("colour") || BlueEntry.getValue().getUserDatum("colour").equals("blue"))
				{// the next vertex is not marked red, hence it has to become blue
					
					Vertex currentBlueState = BlueEntry.getValue();
					int numberOfCompatiblePairs = 0;
					for(Vertex oldRed:reds)
					{
						StatePair pairToComputeFrom = new StatePair(currentBlueState,oldRed);
						int computedScore = computeStateScore(pairToComputeFrom);
						if (computedScore >= 0 &&
								computePairCompatibilityScore(pairToComputeFrom) < 0)
									computedScore = -1;
						computeStateScores.PairScore pair = new PairScore(currentBlueState,oldRed,computedScore);
						if (pair.getScore() >= generalisationThreshold)
						{
							pairsAndScores.add(pair);
							++numberOfCompatiblePairs;
						}
					}
					
					if (numberOfCompatiblePairs == 0)
					{// mark this blue node as red. 
						Vertex newRedNode = currentBlueState;
						newRedNode.setUserDatum("colour", "red", UserData.SHARED);
						reds.add(newRedNode);currentExplorationBoundary.add(newRedNode);

						// All future blue nodes will use this revised set of red states; the fact that
						// it is added to the exploration boundary ensures that it is considered when looking for more blue states.
						// Note that previously-considered blue states were not compared to this one,
						// however previously-introduced red were - we're using the up-to-date reds set above.
						// For this reason, all we have to do is iterate over the old blue states and compare them to the
						// current one; none of those states may become red as a consequence since they are not 
						// red already, i.e. there is an entry about them in PairsAndScores
						for(Vertex oldBlue:BlueStatesConsideredSoFar)
						{
							StatePair newPair = new StatePair(oldBlue,newRedNode);
							int computedScore = computeStateScore(newPair);
							if (computedScore >= 0 &&
									computePairCompatibilityScore(newPair) < 0)
										computedScore = -1;

							computeStateScores.PairScore pair = new PairScore(oldBlue,newRedNode,computedScore);
							if (pair.getScore() >= generalisationThreshold)
								pairsAndScores.add(pair);
						}
					}
					else
					{// This node is a blue node
						BlueStatesConsideredSoFar.add(BlueEntry.getValue());// add a blue one
						currentBlueState.setUserDatum("colour", "blue", UserData.SHARED);
					}							
				}
		}

		Collections.sort(pairsAndScores);// there is no point maintaining a sorted collection as we go since a single quicksort at the end will do the job
				//, new Comparator(){	public int compare(Object o1, Object o2) { return ((PairScore)o2).compareTo(o1); }
		Stack<StatePair> result = new Stack<StatePair>();
		if (pairsMergedPerHypothesis > 0)
		{
			int numberOfElements = Math.min(pairsAndScores.size(),pairsMergedPerHypothesis);
			result.addAll(pairsAndScores.subList(0, numberOfElements));
		}
		else result.addAll(pairsAndScores);

		return result;
	}		

	/** If the supplied vertex is already known (its label is stored in the map), the one from the map is returned;
	 * otherwise a reasonable copy is made, it is then both returned and stored in the map.
	 * 
	 * @param newVertices the map from labels to new vertices
	 * @param g the graph which will have the new vertex added to it
	 * @param origVertex the vertex to copy
	 * @return a copy of the vertex
	 */
	private static Vertex copyVertex(Map<String,Vertex> newVertices, DirectedSparseGraph g,Vertex origVertex)
	{
		String vertName = (String)origVertex.getUserDatum(JUConstants.LABEL);
		Vertex newVertex = newVertices.get(vertName);
		if (newVertex == null) { 
			newVertex = new DirectedSparseVertex();
			newVertex.addUserDatum(JUConstants.LABEL, vertName, UserData.SHARED);
			newVertex.addUserDatum(JUConstants.ACCEPTED, TestRpniLearner.isAccept(origVertex)? "true":"false", UserData.SHARED);
			Object property = origVertex.getUserDatum(JUConstants.PROPERTY);
			if (property != null) newVertex.addUserDatum(JUConstants.PROPERTY, property, UserData.SHARED);
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
		Map<String,Vertex> newVertices = new HashMap<String,Vertex>();
		for(DirectedSparseEdge e:(Set<DirectedSparseEdge>)g.getEdges())
		{
			Vertex newSrc = copyVertex(newVertices,result,e.getSource()),
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
	private Vertex findNextRed(Map<Vertex,List<Vertex>> mergedVertices, Vertex r, String input)
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
			computeStateScores s=new computeStateScores(g,"SINK");
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
		computeStateScores result = (computeStateScores)super.clone();
		result.transitionMatrix = new LinkedHashMap<Vertex,Map<String,Vertex>>(); 
		for(Entry<Vertex,Map<String,Vertex>> entry:transitionMatrix.entrySet())
			result.transitionMatrix.put(entry.getKey(),(Map<String,Vertex>)((TreeMap<String,Vertex>)entry.getValue()).clone());
		pairsAndScores = new ArrayList<computeStateScores.PairScore>(pairArraySize);
		return result;
	}

	/** Finds a vertex with a supplied name in a transition matrix.
	 */
	protected Vertex findVertex(String name)
	{
		Vertex result = null;
		Iterator<Entry<Vertex,Map<String,Vertex>>> entryIt = transitionMatrix.entrySet().iterator();
		while(entryIt.hasNext() && result == null)
		{
			Vertex currentVert = entryIt.next().getKey();
			String vertName = (String)currentVert.getUserDatum(JUConstants.LABEL);
			if (vertName.equals(name))
				result = currentVert;
		}
		return result;
	}
	
	public static computeStateScores mergeAndDeterminize(computeStateScores original, StatePair pair)
	{
			Map<Vertex,List<Vertex>> mergedVertices = new HashMap<Vertex,List<Vertex>>();
			computeStateScores result;
			try {
				result = (computeStateScores)original.clone();
			} catch (CloneNotSupportedException e) {
				IllegalArgumentException ex = new IllegalArgumentException("failed to clone the supplied machine");
				ex.initCause(e);throw ex;
			}
			
			if (original.computePairCompatibilityScore_internal(pair,mergedVertices) < 0)
				throw new IllegalArgumentException("elements of the pair are incompatible");

			// make a loop
			Set<String> reroutedInputs = new HashSet<String>();
			for(Entry<Vertex,Map<String,Vertex>> entry:result.transitionMatrix.entrySet())
			{
				reroutedInputs.clear();// for each state, this stores inputs which should have their transitions re-routed to the red state 
			
				for(Entry<String,Vertex> rowEntry:entry.getValue().entrySet())
					if (rowEntry.getValue() == pair.getQ())	
						// the transition from entry.getKey() leads to the original blue state, record it to be rerouted.
						reroutedInputs.add(rowEntry.getKey());
				for(String reroutedInput:reroutedInputs)
					entry.getValue().put(reroutedInput, pair.getR());
			}

			List<Vertex> ptaVerticesUsed = new LinkedList<Vertex>();
			Set<String> inputsUsed = new HashSet<String>();

			// I iterate over the elements of the original graph in order to be able to update the target one.
			for(Entry<Vertex,Map<String,Vertex>> entry:original.transitionMatrix.entrySet())
			{
				Vertex vert = entry.getKey();
				Map<String,Vertex> resultRow = result.transitionMatrix.get(entry.getKey());// the row we'll update
				if (mergedVertices.containsKey(vert))
				{// there are some vertices to merge with this one.
					inputsUsed.clear();inputsUsed.addAll(entry.getValue().keySet());
					for(Vertex toMerge:mergedVertices.get(vert))
					{// for every input, I'll have a unique target state - this is a feature of PTA
					 // For this reason, every if multiple branches of PTA get merged, there will be no loops or parallel edges.
					// As a consequence, it is safe to assume that each input/target state combination will lead to a new state.
						for(Entry<String,Vertex> input_and_target:original.transitionMatrix.get(toMerge).entrySet())
							if (!inputsUsed.contains(input_and_target.getKey()))
							{
								resultRow.put(input_and_target.getKey(), input_and_target.getValue());
								ptaVerticesUsed.add(input_and_target.getValue());
							}
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
					// Note that only some vertices 
					for(Entry<String,Vertex> input_and_target:original.transitionMatrix.get(currentVert).entrySet())
						currentExplorationBoundary.offer(input_and_target.getValue());

					result.transitionMatrix.remove(currentVert);// remove the vertex from the resulting transition table.
				}
			}
			
			return result;
	}
	
	/** After merging, a graph may exhibit non-determinism, in which case it is made deterministic
	 * by merging nodes. For instance, for A->B and A->C being a non-deterministic choice at node A, 
	 * nodes B and C are to
	 * be merged. The idea is to keep merging such states until it either has to do a contradictory 
	 * merge (accept and reject states) or no merged states exhibit non-determinism. This function
	 * performs a `virtual' merge and populates the list of possible mergers.
	 * It also computes a compability score between a pair of states.
	 * 
	 * The scores computed are different from what computeScore returns since in the situation when a graph
	 * is a straight line, computeScore happily matches PTA with itself while this one has to know
	 * when we've looped (so as to effectively check for compatibility) and hence it cannot do that. 
	 * 
	 * @param model
	 * @param mergedVertices records which vertices have to be merged.
	 * @return a pair of states to be merged or null if the graph is deterministic.
	 */
	private int computePairCompatibilityScore_internal(StatePair origPair,Map<Vertex,List<Vertex>> mergedVertices) 
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
			// make moves which the red one cannot match. The aim is to avoid unnecessary mergers such as
			// when a blue state can make multiple moves which the red node cannot match.
			
			if (TestRpniLearner.isAccept(currentPair.getQ()) != TestRpniLearner.isAccept(currentPair.getR()))
				return -1;// incompatible states
			if (!redFromPta.booleanValue())
				++score;
			Map<String,Vertex> targetBlue = transitionMatrix.get(currentPair.getQ());

			for(Entry<String,Vertex> blueEntry:targetBlue.entrySet())
			{
				Vertex nextRedState = transitionMatrix.get(currentPair.getR()).get(blueEntry.getKey());
				if (nextRedState != null)
				{// both states can make a transition - this would be the case of "non-determinism" for Merge&Determinize
					
					// PTA does not have loops, but the original automaton has
					// and one of those loops is not on the transition diagram, namely the one related to B=A
					if (nextRedState == origPair.getQ())
					{
						nextRedState = origPair.getR(); // emulates the new loop
						redFromPta = true; // and since the original score computation algorithm cannot do this, we pretend to be unable to do this either
					}

					StatePair nextStatePair = new StatePair(blueEntry.getValue(),nextRedState);
					currentExplorationBoundary.offer(nextStatePair);currentRedFromPta.offer(redFromPta);
				}
				else
				{// the current state cannot make a transition, perhaps PTA states associated with it can
					nextRedState = findNextRed(mergedVertices,currentPair.getR(),blueEntry.getKey());
					if (nextRedState != null)
					{// both states can make a transition - this would be the case of "non-determinism" for Merge&Determinize
					 // The red state is the one originally from a previosly-merged PTA branch
						
						// PTA does not have loops, but the original automaton has
						// and one of those loops is not on the transition diagram, namely the one related to B=A
						if (nextRedState == origPair.getQ())
							nextRedState = origPair.getR(); // emulates the new loop
						
						StatePair nextStatePair = new StatePair(blueEntry.getValue(),nextRedState);
						currentExplorationBoundary.offer(nextStatePair);currentRedFromPta.offer(true);// from now on, no increments to the score
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
	protected static final Object syncObj = new Object();
	
	/** This one is similar to the above but does not add a vertex to the graph - I need this behaviour when
	 * concurrently processing graphs. 
	 *  
	 * @param prevState the state from which to start the new edge
	 * @param accepted whether the vertex to add should be an accept one
	 * @param input the label of the edge
	 * @return
	 */
	private Vertex addVertex(Vertex prevState, boolean accepted, String input)
	{
		Vertex newVertex = new DirectedSparseVertex();newVertex.addUserDatum(JUConstants.LABEL, newVertex.toString(), UserData.SHARED);
		newVertex.setUserDatum(JUConstants.ACCEPTED, ""+accepted, UserData.SHARED);
		transitionMatrix.put(newVertex, new TreeMap<String,Vertex>());
		transitionMatrix.get(prevState).put(input,newVertex);
		return newVertex;
	}

	public computeStateScores augmentPTA(Collection<List<String>> strings, boolean accepted)
	{
		for(List<String> sequence:strings)
		{
			Vertex currentState = init, prevState = null;
			Iterator<String> inputIt = sequence.iterator();
			String lastInput = null;
			int position = 0;
			while(inputIt.hasNext() && currentState != null)
			{
				if (!TestRpniLearner.isAccept(currentState))
				{// not the last state and the already-reached state is not accept, while all prefixes of reject sequences should be accept ones. 
					currentState.addUserDatum("pair", "whatever", UserData.SHARED);
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
				if (TestRpniLearner.isAccept(currentState) != accepted)
				{
					currentState.addUserDatum("pair", "whatever", UserData.SHARED);
					throw new IllegalArgumentException("incompatible "+(accepted?"accept":"reject")+" labelling: "+sequence.subList(0, position));
				}
				
			}
		}
		
		return this;
	}
	
	/** Certain methods does not add any edges to the graph, for performance reasons. 
	 * This method adds all relevant edges.
	 * 
	 * @return the current graph (with all edges added).
	 */
	public DirectedSparseGraph getGraph()
	{
		DirectedSparseGraph result = null;
		synchronized (syncObj) 
		{
			result = new DirectedSparseGraph();
			result.setUserDatum(JUConstants.TITLE, "the graph from computeStateScores",UserData.SHARED);
			Map<Vertex,Vertex> oldVertexToNewVertex = new HashMap<Vertex,Vertex>();
			Map<Vertex,Set<String>> targetStateToEdgeLabels = new LinkedHashMap<Vertex,Set<String>>();
			for(Entry<Vertex,Map<String,Vertex>> entry:transitionMatrix.entrySet())
				oldVertexToNewVertex.put(entry.getKey(), (Vertex)entry.getKey().copy(result));
			
			for(Entry<Vertex,Map<String,Vertex>> entry:transitionMatrix.entrySet())
			{
				targetStateToEdgeLabels.clear();
				for(Entry<String,Vertex> sv:entry.getValue().entrySet())
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
							throw new IllegalArgumentException("Target vertex "+sv.getValue()+" is not in the transition table");
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

	public String getStatistics() {
		int edgeCounter = 0;
		for(Entry<Vertex,Map<String,Vertex>> v:transitionMatrix.entrySet())
			++edgeCounter;
		FSMStructure fsm = WMethod.getGraphData(getGraph());
		
		String wsetDetails = "";
		try
		{
			wsetDetails = "Wset: "+WMethod.computeWSet(fsm).size()+" seq";
		}
		catch (EquivalentStatesException e) {
			wsetDetails = e.toString();
		}
		return "vert: "+transitionMatrix.keySet().size()+" edges: "+edgeCounter+" alphabet: "+WMethod.computeAlphabet(fsm).size()+" unreachable: "+WMethod.checkUnreachableStates(fsm)+" "+wsetDetails;
	}
	
}

