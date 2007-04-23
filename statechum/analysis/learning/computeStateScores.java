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
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.text.Position;

import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.PTATestSequenceEngine;
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
	private CmpVertex init;
	
	private Map<CmpVertex,Map<String,CmpVertex>> transitionMatrix = new TreeMap<CmpVertex,Map<String,CmpVertex>>();
			
	/** Stores all red-blue pairs; has to be backed by array for the optimal performance of the sort function. */
	protected List<computeStateScores.PairScore> pairsAndScores;
	
	protected int generalisationThreshold;
	protected int pairsMergedPerHypothesis;
	
	private boolean bumpPositives = false, useCompatibilityScore = false;

	/** Used to switch on a variety of consistency checks. */
	protected static boolean testMode = false;

	public static final int pairArraySize = 2000;
	
	public List<String> extractReds()
	{
		List<String> result = new LinkedList<String>();
		for(Vertex v:transitionMatrix.keySet())
			if (v.containsUserDatumKey("colour")&& v.getUserDatum("colour").equals("red"))
					result.add(v.getUserDatum(JUConstants.LABEL).toString());
		
		return result;
	}

	/** Resets all the colour labelling to the initial value. */
	public void clearColours()
	{
		for(Vertex v:transitionMatrix.keySet())
			v.removeUserDatum("colour");
		init.addUserDatum("colour", "red", UserData.SHARED);
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
		init = (CmpVertex)TestRpniLearner.findVertex(JUConstants.PROPERTY, JUConstants.INIT, graph);
		
		pairsAndScores = new ArrayList<computeStateScores.PairScore>(pairArraySize);//graphVertices.size()*graphVertices.size());
		for(CmpVertex v:(Set<CmpVertex>)g.getVertices())
		{
			assert !sinkVertexName.equals(v);
			transitionMatrix.put(v,new TreeMap<String,CmpVertex>());// using TreeMap makes everything predictable
			v.removeUserDatum("colour");
		}
		init.addUserDatum("colour", "red", UserData.SHARED);
	
		Iterator<DirectedSparseEdge> edgeIter = g.getEdges().iterator();
		while(edgeIter.hasNext())
		{	
			DirectedSparseEdge e = edgeIter.next();
			Map<String,CmpVertex> outgoing = transitionMatrix.get(e.getSource());
			// The line below aims to ensure that inputs are evaluated by computeStateScore in a specific order, which in conjunction with the visited set of computeStateScore permits emulating a bug in computeScore
			for(String label:(HashSet<String>)e.getUserDatum(JUConstants.LABEL))
				outgoing.put(label, (CmpVertex)e.getDest());			
		}
	}

	public computeStateScores(int newGeneralisationThreshold, int newPairsMergedPerHypothesis)
	{
		generalisationThreshold = newGeneralisationThreshold;
		pairsMergedPerHypothesis = newPairsMergedPerHypothesis;
		graph = null;
		initPTA();
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
		List<Collection<String>> sequenceOfSets = computePathsSBetween(vertSource, vertTarget);
		if (sequenceOfSets == null) 
			return sp;
		sp.add(new LinkedList<String>());
		for(Collection<String> inputsToMultWith:sequenceOfSets)
			sp = WMethod.crossWithSet(sp, inputsToMultWith);
		return sp;
	}
	
	/** Computes all possible shortest paths from the supplied source state to the supplied target state 
	 * and returns a sequence of possible sets inputs which can be followed. In other words, 
	 * a choice of any input from each of the returned sets gives a possible path between
	 * the requested vertices.
	 * 
	 * @param vertSource the source state
	 * @param vertTarget the target state
	 * @return sequences of inputs to follow all paths found. Null if a path is not found and an empty list if the target vertex is the same as the source one
	 */	
	List<Collection<String>> computePathsSBetween(Vertex vertSource, Vertex vertTarget)
	{
		List<Collection<String>> sequenceOfSets = null;

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
			sequenceOfSets = new LinkedList<Collection<String>>();
			Iterator<Vertex> vertIt = currentPath.iterator();
			Vertex prevVert = vertIt.next();
			while(vertIt.hasNext())
			{
				currentVert = vertIt.next();
				List<String> inputsToMultWith = new LinkedList<String>();
				for(Entry<String,CmpVertex> entry:transitionMatrix.get(prevVert).entrySet())
					if (entry.getValue() == currentVert)
						inputsToMultWith.add(entry.getKey());
				sequenceOfSets.add(inputsToMultWith);
				prevVert = currentVert;
			}
		}
		
		return sequenceOfSets;
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

		Map<CmpVertex,List<String>> targetToInputSet = new TreeMap<CmpVertex,List<String>>();
		while(!currentExplorationBoundary.isEmpty())
		{
			Vertex currentVert = currentExplorationBoundary.remove();
			PTATestSequenceEngine.sequenceSet currentPaths = currentExplorationTargetStates.remove();
			targetToInputSet.clear();
			
			currentPaths.crossWithSet(temp.transitionMatrix.get(currentVert).keySet());
			for(Entry<String,CmpVertex> entry:temp.transitionMatrix.get(currentVert).entrySet())
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
	public Collection<List<String>> computeQS(final StatePair pair, computeStateScores temp)
	{
		Vertex tempRed = temp.findVertex((String)pair.getR().getUserDatum(JUConstants.LABEL));
		PTATestSequenceEngine engine = new PTATestSequenceEngine();
		engine.init(new NonExistingPaths((CmpVertex)init));
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();paths.setIdentity();

		List<Collection<String>> sequenceOfSets = temp.computePathsSBetween(temp.init,tempRed);
		if (sequenceOfSets == null)
			throw new IllegalArgumentException("failed to find the red state in the merge result");
		for(Collection<String> inputsToMultWith:sequenceOfSets)
			paths = paths.crossWithSet(inputsToMultWith);

		buildQuestionsFromPair(temp, tempRed, paths);
		for(Entry<String,CmpVertex> loopEntry:temp.transitionMatrix.get(tempRed).entrySet())
			if (loopEntry.getValue() == tempRed)
			{// Note an input corresponding to any loop in temp can be followed in the original machine, since
				// a loop in temp is either due to the merge or because it was there in the first place.
				List<String> initialSeq = new LinkedList<String>();initialSeq.add(loopEntry.getKey());
				buildQuestionsFromPair(temp, loopEntry.getValue(),paths.crossWithSet(initialSeq));
			}
		return engine.getData();
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
					// if the red side is currently in the sink vertex, i.e. we are effectively calculating a set of questions, do not report inconsistency or increment the score
						if (TestRpniLearner.isAccept(redEntry.getValue()) != TestRpniLearner.isAccept(nextBlueState))
							return -1;// incompatible states
					
					++score;

					StatePair nextStatePair = new StatePair(nextBlueState,redEntry.getValue());
					currentExplorationBoundary.offer(nextStatePair);
				}
				// if the red can make a move, but the blue one cannot, ignore this case.
			}
		}
		
		if (bumpPositives && TestRpniLearner.isAccept(pair.getQ()))
			score++;
		
		return score;
	}
	
	public static class PairScore extends StatePair implements Comparable
	{
		private final int score;

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
			return "[ "+getQ().getUserDatum(JUConstants.LABEL)+"("+TestRpniLearner.isAccept(getQ())+"), "+getR().getUserDatum(JUConstants.LABEL)+"("+TestRpniLearner.isAccept(getR())+") : "+score+" ]";
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
	
	
	protected Stack<computeStateScores.PairScore> chooseStatePairs()
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
			for(Entry<String,CmpVertex> BlueEntry:transitionMatrix.get(currentRed).entrySet())
				if (!BlueEntry.getValue().containsUserDatumKey("colour") || BlueEntry.getValue().getUserDatum("colour").equals("blue"))
				{// the next vertex is not marked red, hence it has to become blue
					Vertex currentBlueState = BlueEntry.getValue();
											
					int numberOfCompatiblePairs = 0;
					for(Vertex oldRed:reds)
					{
						computeStateScores.PairScore pair = obtainPair(currentBlueState,oldRed);
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
						newRedNode.setUserDatum("colour", "red", UserData.SHARED);
						reds.add(newRedNode);currentExplorationBoundary.add(newRedNode);
						BlueStatesConsideredSoFar.remove(newRedNode);
						
						// All future blue nodes will use this revised set of red states; the fact that
						// it is added to the exploration boundary ensures that it is considered when looking for more blue states.
						// Note that previously-considered blue states were not compared to this one,
						// however previously-introduced red were - we're using the up-to-date reds set above.
						// For this reason, all we have to do is iterate over the old blue states and compare them to the
						// current one; none of those states may become red as a consequence since they are not 
						// red already, i.e. there is an entry about them in PairsAndScores
						for(Vertex oldBlue:BlueStatesConsideredSoFar)
						{
							computeStateScores.PairScore pair = obtainPair(oldBlue,newRedNode);
							if (pair.getScore() >= generalisationThreshold)
							{
								pairsAndScores.add(pair);
								if (testMode) checkPTAConsistency(this, oldBlue);
							}
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
		Stack<computeStateScores.PairScore> result = new Stack<computeStateScores.PairScore>();
		if (pairsMergedPerHypothesis > 0)
		{
			int numberOfElements = Math.min(pairsAndScores.size(),pairsMergedPerHypothesis);
			result.addAll(pairsAndScores.subList(0, numberOfElements));
		}
/*		
		else
			if (bumpPositives && !pairsAndScores.isEmpty())
			{// here we only append elements with the same score as the max score
				
				computeStateScores.PairScore topPair = pairsAndScores.get(pairsAndScores.size()-1);result.add(topPair);
				
				for(int i=0;i< pairsAndScores.size();++i)
				{// we iterage until we get to the end; pairsAndScores is an array
					computeStateScores.PairScore p = pairsAndScores.get(i);
					if (p.getScore() == topPair.getScore()) result.add(p);
				}
			}
*/			
		else result.addAll(pairsAndScores);

		return result;
	}		

	private PairScore obtainPair(Vertex blue, Vertex red)
	{
		int computedScore = -1;StatePair pairToComputeFrom = new StatePair(blue,red);
		if (useCompatibilityScore)
			computedScore = computePairCompatibilityScore(pairToComputeFrom);
		else
		{
			computedScore = computeStateScore(pairToComputeFrom);
			if (computedScore >= 0 &&
				computePairCompatibilityScore(pairToComputeFrom) < 0)
					computedScore = -1;
		}
		
		return new PairScore(blue,red,computedScore);
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
		result.transitionMatrix = new TreeMap<CmpVertex,Map<String,CmpVertex>>(); 
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:transitionMatrix.entrySet())
		{
			Map<String,CmpVertex> newValue = new TreeMap<String,CmpVertex>();
			newValue.putAll(entry.getValue());
			result.transitionMatrix.put(entry.getKey(),newValue);
		}
		pairsAndScores = new ArrayList<computeStateScores.PairScore>(pairArraySize);
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
	
	public static computeStateScores mergeAndDeterminize(computeStateScores original, StatePair pair)
	{
			if (testMode) { checkPTAConsistency(original, (CmpVertex)pair.getQ());checkPTAIsTree(original,null,null,null); }
			Map<Vertex,List<Vertex>> mergedVertices = new HashMap<Vertex,List<Vertex>>();
			computeStateScores result;
			try {
				result = (computeStateScores)original.clone();
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
					
					inputsUsed.clear();inputsUsed.addAll(entry.getValue().keySet());
					for(Vertex toMerge:mergedVertices.get(vert))
					{// for every input, I'll have a unique target state - this is a feature of PTA
					 // For this reason, every if multiple branches of PTA get merged, there will be no loops or parallel edges.
					// As a consequence, it is safe to assume that each input/target state combination will lead to a new state.
						boolean somethingWasAdded = false;
						for(Entry<String,CmpVertex> input_and_target:original.transitionMatrix.get(toMerge).entrySet())
							if (!inputsUsed.contains(input_and_target.getKey()))
							{
								resultRow.put(input_and_target.getKey(), input_and_target.getValue());
								inputsUsed.add(input_and_target.getKey());
								ptaVerticesUsed.add(input_and_target.getValue());somethingWasAdded = true;
							}
						assert somethingWasAdded;
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
					for(Entry<String,CmpVertex> input_and_target:original.transitionMatrix.get(currentVert).entrySet())
						currentExplorationBoundary.offer(input_and_target.getValue());

					result.transitionMatrix.remove(currentVert);// remove the vertex from the resulting transition table.
				}
			}
			
			if (testMode) checkPTAIsTree(result, original, pair,ptaVerticesUsed);
			return result;
	}

	protected static void checkPTAConsistency(computeStateScores original, Vertex blueState)
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
						throw new IllegalArgumentException("non-pta state "+entry.getKey()+" ("+entry.getKey().getUserDatum("colour")+") refers to PTA state "+input_and_target.getValue()+", blue state is "+blueState);
			}									
	}
	
	/** Checks that non-red states form a tree, i.e. they have exactly one incoming edge and there are no
	 * disconnected states. Arguments after pta are used to check what happened when the pta fails to be
	 * a tree.
	 * 
	 * @param pta
	 */
	protected static void checkPTAIsTree(computeStateScores pta,computeStateScores original, StatePair pair,Collection<Vertex> notRemoved)
	{
		assert testMode : "this one should not run when not under test";
		Map<CmpVertex,AtomicInteger> hasIncoming = new HashMap<CmpVertex,AtomicInteger>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:pta.transitionMatrix.entrySet())
			for(Entry<String,CmpVertex> targ:entry.getValue().entrySet())
			{
				if (!hasIncoming.containsKey(targ.getValue()))
					hasIncoming.put(targ.getValue(), new AtomicInteger(1));
				else
					hasIncoming.get(targ.getValue()).addAndGet(1);
			}
		for(Entry<CmpVertex,AtomicInteger> p:hasIncoming.entrySet())
			if (p.getValue().intValue() > 1 &&
					p.getKey().containsUserDatumKey("colour") && !p.getKey().getUserDatum("colour").equals("red"))
				throw new IllegalArgumentException("non-red vertex "+p.getKey()+" has multiple incoming transitions");
				
		Queue<Vertex> currentBoundary = new LinkedList<Vertex>();// FIFO queue containing vertices to be explored
		Set<Vertex> visitedStates = new HashSet<Vertex>();
		currentBoundary.add( pta.init );visitedStates.add(pta.init);
		while(!currentBoundary.isEmpty())
		{
			Vertex current = currentBoundary.remove();
			for(Entry<String,CmpVertex> input_and_target:pta.transitionMatrix.get(current).entrySet())
				if (!visitedStates.contains(input_and_target.getValue()))
				{
					visitedStates.add(input_and_target.getValue());
					currentBoundary.offer(input_and_target.getValue());
				}
		}
		
		Set<CmpVertex> unreachables = new HashSet<CmpVertex>();unreachables.addAll(pta.transitionMatrix.keySet());
		unreachables.removeAll(visitedStates);
		if (!unreachables.isEmpty())
		{
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
			Set<CmpVertex> remaining = new HashSet<CmpVertex>();remaining.addAll(unreachables);
			remaining.removeAll(visitedStates);
			if (remaining.isEmpty())
			{
				remaining.clear();remaining.addAll(unreachables);remaining.removeAll(notRemoved);
				String response = "vertices "+unreachables.toString()+" are unreachable and all of them are PTA vertices; the following were for some reason not removed "+remaining+"\n";
				for(Vertex u:unreachables)
					response=response+" "+u+"("+hasIncoming.get(u)+")";
				System.out.println(response);
				Vertex InterestingUnreachableVertex = unreachables.iterator().next();
				List<String> seq = original.computePathsBetween(pair.getQ(), InterestingUnreachableVertex).iterator().next();
				System.out.println(seq);
				Map<Vertex,List<Vertex>> mergedVertices = new HashMap<Vertex,List<Vertex>>();
				if (original.computePairCompatibilityScore_internal(pair,mergedVertices) < 0)
					throw new IllegalArgumentException("elements of the pair are incompatible");
				
				System.out.println(pair);
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
					System.out.println(v.toString()+" "+pta.transitionMatrix.get(v).keySet()+" input "+input+" ");
					v = pta.transitionMatrix.get(v).get(input);
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
	
	protected int vertPositiveID = 1;
	protected int vertNegativeID = 1;
	
	public enum IDMode { NONE, POSITIVE_NEGATIVE, POSITIVE_ONLY };
	
	protected IDMode id_mode = IDMode.NONE; // creation of new vertices is prohibited.
	
	public computeStateScores setMode(IDMode m)
	{
		id_mode = m;return this;
	}
	
	/** Generates vertice IDs. */
	public String nextID(boolean accepted)
	{
		if (id_mode == IDMode.POSITIVE_NEGATIVE)
			return (accepted?"P"+vertPositiveID++:"N"+vertNegativeID++);
		else
			if (id_mode == IDMode.POSITIVE_ONLY)
				return "V"+vertPositiveID++;
			else
				throw new IllegalArgumentException("unknown vertex id allocation mode");
	}
	
	/** This one is similar to the above but does not add a vertex to the graph - I need this behaviour when
	 * concurrently processing graphs. 
	 *  
	 * @param prevState the state from which to start the new edge
	 * @param accepted whether the vertex to add should be an accept one
	 * @param input the label of the edge
	 * @return
	 */
	private CmpVertex addVertex(CmpVertex prevState, boolean accepted, String input)
	{
		assert Thread.holdsLock(syncObj);
		CmpVertex newVertex = new DeterministicDirectedSparseGraph.DeterministicVertex();
		newVertex.addUserDatum(JUConstants.LABEL, 
				nextID(accepted), 
				UserData.SHARED);
		newVertex.setUserDatum(JUConstants.ACCEPTED, ""+accepted, UserData.SHARED);
		transitionMatrix.put(newVertex, new TreeMap<String,CmpVertex>());
		transitionMatrix.get(prevState).put(input,newVertex);
		return newVertex;
	}

	public void initPTA()
	{
		transitionMatrix.clear();
		init = new DeterministicDirectedSparseGraph.DeterministicVertex();
		init.addUserDatum("property", "init", UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);
		init.addUserDatum(JUConstants.LABEL, "Init", UserData.SHARED);
		init.setUserDatum("colour", "red", UserData.SHARED);
		//graph.setUserDatum(JUConstants.TITLE, "Hypothesis machine", UserData.SHARED);
		transitionMatrix.put(init,new TreeMap<String,CmpVertex>());
		pairsAndScores = new ArrayList<computeStateScores.PairScore>(pairArraySize);
	}
	
	public computeStateScores augmentPTA(Collection<List<String>> strings, boolean accepted)
	{
		for(List<String> sequence:strings)
			augmentPTA(sequence, accepted);
		return this;
	}
	
	public computeStateScores augmentPTA(List<String> sequence, boolean accepted)
	{
		CmpVertex currentState = init, prevState = null;
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
		return "trans: "+transitionMatrix.size()+" (hash "+transitionMatrix.hashCode()+")";
	}

	public void bumpPositive() {
		bumpPositives  = true;
	}
	
	public void useCompatibilityScore()
	{
		useCompatibilityScore = true;
	}
}

