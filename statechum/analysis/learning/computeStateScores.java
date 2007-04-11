/**
 * 
 */
package statechum.analysis.learning;

import java.awt.Frame;
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
import statechum.xmachine.model.testset.HashBucketPrefixFreeCollection;
import statechum.xmachine.model.testset.PrefixFreeCollection;
import statechum.xmachine.model.testset.SlowPrefixFreeCollection;
import statechum.xmachine.model.testset.WMethod;
import sun.util.calendar.JulianCalendar;
import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class computeStateScores {
	private final Vertex sinkVertex = new DirectedSparseVertex();
	private final Set<String> alphabet;
	private final DirectedSparseGraph graph;
	/** The initial vertex. */
	private final Vertex init;
	
	private final Map<Vertex,Map<String,Vertex>> transitionMatrix = new LinkedHashMap<Vertex,Map<String,Vertex>>();
			
	/** Initialises the class used to compute scores between states.
	 * 
	 * @param g the graph it will be used on 
	 * @param sinkVertexName the name for a sink vertex, to be different from names of all vertices on the graph
	 */
	public computeStateScores(DirectedSparseGraph g,String sinkVertexName)
	{
		graph = g;
		init = TestRpniLearner.findVertex(JUConstants.PROPERTY, JUConstants.INIT, graph);
		
		Set<Vertex> graphVertices = graph.getVertices();
		pairsAndScores = new ArrayList<computeStateScores.PairScore>(graphVertices.size()*graphVertices.size());
		for(Vertex v:graphVertices)
		{
			assert !sinkVertexName.equals(v);
			transitionMatrix.put(v,new TreeMap<String,Vertex>());// using TreeMap makes everything predictable  
		}
		alphabet = WMethod.computeAlphabet(g);
		sinkVertex.addUserDatum(JUConstants.LABEL, sinkVertexName, UserData.SHARED);
		sinkVertex.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);
		Map<String,Vertex> outgoingSink = new TreeMap<String,Vertex>();
		for(String input:alphabet)
			outgoingSink.put(input, sinkVertex);
		transitionMatrix.put(sinkVertex, outgoingSink);
		
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
	
	// a heavily-reduced version of the above, tailured for score computation only
	protected int computeStateScore(StatePair pair)
	{
		if (TestRpniLearner.isAccept(pair.getR()) != TestRpniLearner.isAccept(pair.getQ()))
			return -1;
		//System.out.println("computing scores for "+pair);
		int score = 0;
		
		assert pair.getQ() != pair.getR();
		assert pair.getQ().getGraph() == graph && pair.getR().getGraph() == graph; 
		
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
	
	/** Stores all red-blue pairs; has to be backed by array for the optimal performance of the sort function. */
	protected final List<computeStateScores.PairScore> pairsAndScores;
	
	protected int generalisationThreshold;
	protected int pairsMergedPerHypothesis;
	
	protected Stack<StatePair> chooseStatePairs()
	{
		pairsAndScores.clear();
		Set<Vertex> reds = new LinkedHashSet<Vertex>();reds.addAll(RPNIBlueFringeLearner.findVertices("colour", "red", graph));
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
						if (computedScore > 0 &&
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
						currentBlueState.setUserDatum("colour", "red", UserData.SHARED);
						reds.add(BlueEntry.getValue());currentExplorationBoundary.add(BlueEntry.getValue());

						// All future blue nodes will use this revised set of red states; the fact that
						// it is added to the exploration boundary ensures that it is considered when looking for more blue states.
						// Note that previously-considered blue states were not compared to this one,
						// however previously-introduced red were - we're using the up-to-date reds set above.
						// For this reason, all we have to do is iterate over the old blue states and compare them to the
						// current one; none of those states may become red as a consequence since they are not red already, i.e. there is an entry about them in PairsAndScores
						for(Vertex oldBlue:BlueStatesConsideredSoFar)
						{
							computeStateScores.PairScore pair = new PairScore(currentBlueState, oldBlue,computeStateScore(new StatePair(currentBlueState,oldBlue)));
							if (pair.getScore() >= generalisationThreshold)
								pairsAndScores.add(pair);
						}
					}
					else
					{
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
			Queue<Vertex> currentExplorationBoundary = new LinkedList<Vertex>();// FIFO queue containing pairs to be explored
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
	
	/** After merging, a graph may exhibit non-determinism, in which case it is made deterministic
	 * by merging nodes. For instance, for A->B and A->C being a non-deterministic choice at node A, 
	 * nodes B and C are to
	 * be merged. This function keeps merging such states until it either has to do a contradictory merge (accept and reject states)
	 * or no merged states exhibit non-determinism.
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


	/** Creates a new vertex and an edge and and adds both to the graph. 
	 * 
	 * @param g the graph to modify
	 * @param prevState the state from which to start the new edge
	 * @param accepted whether the vertex to add should be an accept one
	 * @param input the label of the edge
	 * @return
	 */
	private static Vertex addVertex(DirectedSparseGraph g, Vertex prevState, boolean accepted, String input)
	{
		Vertex newVertex = new DirectedSparseVertex();newVertex.addUserDatum(JUConstants.LABEL, newVertex.toString(), UserData.SHARED);
		newVertex.setUserDatum(JUConstants.ACCEPTED, ""+accepted, UserData.SHARED);
		g.addVertex(newVertex);
		DirectedSparseEdge e = new DirectedSparseEdge(prevState, newVertex);
		Set<String> labels = new HashSet<String>();labels.add(input);
		e.addUserDatum(JUConstants.LABEL, labels, UserData.CLONE);
		g.addEdge(e);
		return newVertex;
	}
	
	/** Adds a given set of sequences to a PTA, with a specific accept-reject labelling.
	 * 
	 * @param pta
	 * @param strings sequences to be added
	 * @param accepted whether sequences are accept or reject ones.
	 * @return the result of adding.
	 */ 
	public static DirectedSparseGraph augmentPTA(DirectedSparseGraph pta, Collection<List<String>> strings, boolean accepted)
	{
		Vertex init = RPNIBlueFringeLearner.findVertex(JUConstants.PROPERTY, JUConstants.INIT,pta);
		
		Iterator<List<String>> stringsIt = strings.iterator();
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
				DirectedSparseEdge followingEdge = RPNIBlueFringeLearner.getEdgeWithLabel(currentState.getOutEdges(),lastInput);
				if (followingEdge != null)
					currentState = followingEdge.getDest();
				else
					currentState = null;
			}
			
			if (currentState == null)
			{// the supplied path does not exist in PTA, the first non-existing vertex is from state prevState with label lastInput
				while(inputIt.hasNext())
				{
					prevState = addVertex(pta, prevState, true, lastInput);
					lastInput = inputIt.next();
				}
				// at this point, we are at the end of the sequence. Last vertex is prevState and last input if lastInput
				addVertex(pta, prevState, accepted, lastInput);
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
		return pta;
	}
}

