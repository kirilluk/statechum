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
			transitionMatrix.put(v,new TreeMap<String,Vertex>());// using TreeMap makes everything more predictable
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
			sp = WMethod.cross(sp, WMethod.makeSingleton((HashSet<String>)e.getUserDatum(JUConstants.LABEL)));
		Edge loopEdge = TestRpniLearner.findEdge(tempRed, tempRed);
		List<List<String>> prefixOfAllPathsFromRedState = new LinkedList<List<String>>();prefixOfAllPathsFromRedState.add(new LinkedList<String>());
		if(loopEdge!=null)
			prefixOfAllPathsFromRedState.addAll(WMethod.makeSingleton((HashSet<String>)loopEdge.getUserDatum(JUConstants.LABEL)));

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
					List<List<String>> fullPath = new LinkedList<List<String>>();fullPath.add(loopPrefix);
					Vertex origState = loopPrefix.isEmpty()?pair.getR():transitionMatrix.get(pair.getR()).get(loopPrefix.get(0));// the first element always exists
					Iterator<Edge> pathIt = pathInTemp.iterator();
					// Now we trace this path in the original machine, following the existing matrix
					while(pathIt.hasNext() && origState != null)
					{
						Collection<String> labels = (Collection<String>)pathIt.next().getUserDatum(JUConstants.LABEL);
						String input = labels.iterator().next();
						fullPath = WMethod.cross(fullPath, WMethod.makeSingleton(labels));
						origState = transitionMatrix.get(origState).get(input);
					}
					while(pathIt.hasNext())
						fullPath = WMethod.cross(fullPath, WMethod.makeSingleton(((Collection<String>)pathIt.next().getUserDatum(JUConstants.LABEL))));
					
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
		//Map<Vertex,Set<String>> visited = new HashMap<Vertex,Set<String>>();
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
						computeStateScores.PairScore pair = new PairScore(currentBlueState,oldRed,computeStateScore(new StatePair(currentBlueState,oldRed)));
						if (pair.getScore() >= generalisationThreshold && pairCompatible(graph, pair))
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
							if (pair.getScore() >= generalisationThreshold && pairCompatible(graph, pair))
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
	
	public static DirectedSparseGraph mergeAndDeterminize(Graph g, StatePair pair) throws IncompatibleMergeException{
		DirectedSparseGraph original = copy(g);
		Vertex q = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, pair.getQ().getUserDatum(JUConstants.LABEL),original);
		Vertex qDash = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, pair.getR().getUserDatum(JUConstants.LABEL),original);
		assert q != null && qDash != null;
		pair = new StatePair(q,qDash);
		DirectedSparseGraph temp = RPNIBlueFringeLearner.merge((DirectedSparseGraph)original, pair);
		StatePair mergable = findMergablePair(temp);
		while(mergable!=null){
			temp=RPNIBlueFringeLearner.merge(temp, mergable);
			mergable = findMergablePair(temp);
		}
		return temp;
	}
	
	public static boolean pairCompatible(Graph g, StatePair pair){
		try
		{
			mergeAndDeterminize(g, pair);
		}
		catch(IncompatibleMergeException e)
		{
			return false;
		}
		
		return true;
	}

	/** Thrown when a positive node is being merged with a negative one. */
	public static class IncompatibleMergeException extends Error
	{

		/**
		 *  ID for serialization.
		 */
		private static final long serialVersionUID = -5237773645955967247L;
		
		public IncompatibleMergeException(StatePair pair)
		{
			super("two states "+pair+"should not be merged");
		}
	}
	
	/** After merging, a graph may exhibit non-determinism, in which case it is made deterministic
	 * by merging nodes. For instance, for A->B and A->C being a non-deterministic choice at node A, 
	 * nodes B and C are to
	 * be merged. This function identifies such a (B,C).
	 * 
	 * @param model
	 * @return a pair of states to be merged or null if the graph is deterministic.
	 * @throws IncompatibleMergeException if states being merged are not compatible.
	 */
	public static StatePair findMergablePair(DirectedSparseGraph model) throws IncompatibleMergeException{
		Queue<Vertex> currentExplorationBoundary = new LinkedList<Vertex>();// FIFO queue
		Vertex initVertex = RPNIBlueFringeLearner.findVertex(JUConstants.PROPERTY, JUConstants.INIT, model);
		HashSet<Vertex> visitedVertices = new HashSet<Vertex>();visitedVertices.add(initVertex);
		currentExplorationBoundary.add(initVertex);
		Map<String,DirectedSparseEdge> doneLabels = new HashMap<String,DirectedSparseEdge>();

		while(!currentExplorationBoundary.isEmpty())
		{
			Vertex currentVertex = currentExplorationBoundary.remove();
			doneLabels.clear();
			Set<DirectedSparseEdge> edges = currentVertex.getOutEdges();
			Iterator<DirectedSparseEdge> edgeIt = edges.iterator();
			while(edgeIt.hasNext())
			{
				DirectedSparseEdge e = edgeIt.next();// for every edge ...
				Set<String> labels = (Set<String>)e.getUserDatum(JUConstants.LABEL);
				Iterator<String> labelIt = labels.iterator();
				while(labelIt.hasNext()){
					String label = labelIt.next();// ... and every label on it
					DirectedSparseEdge eDash = doneLabels.get(label); 
					if(eDash==null)
						doneLabels.put(label, e);
					else {
						StatePair p = null;
						if(eDash.getDest().getUserDatum("property")!=null)
							p = new StatePair(e.getDest(), eDash.getDest());
						else
							p = new StatePair(eDash.getDest(),e.getDest());

						if(TestRpniLearner.isAccept(e.getDest()) != TestRpniLearner.isAccept(eDash.getDest())) // KIRR: strange - the two should never be different if the original pair to choose was selected properly
							throw new IncompatibleMergeException(p);
						else
							return p;
					}
				}
				if (!visitedVertices.contains(e.getDest()))
				{
					visitedVertices.add(e.getDest());currentExplorationBoundary.offer(e.getDest());
				}
			}
		}
		return null;
	}
}