/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
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

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.algorithms.shortestpath.DijkstraDistance;

import java.util.*;
import java.util.Map.Entry;

import statechum.DeterministicDirectedSparseGraph;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSet;
import statechum.model.testset.PTASequenceSetAutomaton;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

public class RandomPathGenerator {
	
	final protected LearnerGraph g;
	final CmpVertex initialState;
	int pathLength;
	
	final Random randomNumberGenerator;
	
	/** An array representation of the transition matrix of the graph, needed for fast computation of random walks. */
	private Map<CmpVertex,ArrayList<Entry<Label,CmpVertex>>> transitions = new TreeMap<CmpVertex,ArrayList<Entry<Label,CmpVertex>>>();
	/** For each state, stores inputs not accepted from it, needed for fast computation of random walks. */
	private Map<CmpVertex,ArrayList<Label>> inputsRejected = new TreeMap<CmpVertex,ArrayList<Label>>();
	
	/** The random number generator passed in is used to generate walks; one can pass a mock in order to 
	 * produce walks devised by a tester. Note that the object will be modified in the course of walks thanks
	 * to java's Random being non-serialisable.
	 *  
	 * @param baseGraph the graph to operate on
	 * @param random the random number generator.
	 * @param extra the length of paths will be diameter plus this value.
	 * @param initial the state to start the traversal with. null means use the initial state of <em>graph</em>.
	 * @param alphabet in some cases we would want to generate walks using a different alphabet than the one visible in a graph. 
	 * For instance, if we take a single state of a graph, only a portion of the whole 
	 * graph will be accessible and this will limit visible alphabet while the alphabet for the whole has to be used.
	 * Alphabet obtained from the supplied graph if <em>null</em>.
	 */ 
	public RandomPathGenerator(LearnerGraph graph, Random random, int extra, CmpVertex initial) 
	{
		this(graph,random,extra,initial,null);
	}
	
	/** The random number generator passed in is used to generate walks; one can pass a mock in order to 
	 * produce walks devised by a tester. Note that the object will be modified in the course of walks thanks
	 * to java's Random being non-serialisable.
	 *  
	 * @param baseGraph the graph to operate on
	 * @param random the random number generator.
	 * @param extra the length of paths will be diameter plus this value.
	 * @param initial the state to start the traversal with. null means use the initial state of <em>graph</em>.
	 * @param alphabet in some cases we would want to generate walks using a different alphabet than the one visible in a graph. 
	 * For instance, if we take a single state of a graph, only a portion of the whole 
	 * graph will be accessible and this will limit visible alphabet while the alphabet for the whole has to be used.
	 * Alphabet obtained from the supplied graph if <em>null</em>.
	 */ 
	public RandomPathGenerator(LearnerGraph graph, Random random, int extra, CmpVertex initial,Set<Label> alphabetArg)
	{
		g = graph;randomNumberGenerator = random;
		if (initial != null) 
		{ 
			g.verifyVertexInGraph(initial);initialState=initial; 
		}
		else initialState=g.getInit();
		pathLength = diameter(g)+extra;
		
		transitions.clear();inputsRejected.clear();
		/** The alphabet of the graph. */
		Set<Label> alphabet = null;
		if (alphabetArg == null)
			alphabet = g.pathroutines.computeAlphabet();
		else
		{// check that the supplied actual alphabet includes the one in the graph 
			Set<Label> origAlphabet = g.pathroutines.computeAlphabet();
			origAlphabet.removeAll(alphabetArg);if(!origAlphabet.isEmpty()) throw new IllegalArgumentException("the supplied alphabet does not include the one of the graph, "+origAlphabet+" elements are new to the graph");
			alphabet=alphabetArg;
		}
 		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
		{
			ArrayList<Entry<Label,CmpVertex>> row = new ArrayList<Entry<Label,CmpVertex>>();row.addAll(entry.getValue().entrySet());
			transitions.put(entry.getKey(), row);
			
			Set<Label> negatives = new LinkedHashSet<Label>();
			negatives.addAll(alphabet);negatives.removeAll(entry.getValue().keySet());
			ArrayList<Label> rejects = new ArrayList<Label>();rejects.addAll(negatives);
			inputsRejected.put(entry.getKey(), rejects);
		}
		initAllSequences();
	}

	/** If true, every walk should avoid visiting an initial state but all positive ones should terminate at it.*/
	protected boolean walksShouldLeadToInitialState = false;

	public void setWalksShouldLeadToInitialState()
	{
		walksShouldLeadToInitialState = true;
		constructShortestPathsToInitAndLongestPathsAvoidingInit();
	}
	
	/** Returns the maximal length of sequences that will be generated by default (that is, without a user-defined {@link RandomLengthGenerator}. */
	public int getPathLength()
	{
		return pathLength;
	}
	
	public void setPathLength(int value)
	{
		if (value <= 0) throw new IllegalArgumentException("path length has to be positive");
		pathLength = value;
	}
	
	public static int diameter(LearnerGraph graph)
	{// Decided not to rewrite using a flowgraph or not, given that this is only used once per experiment. 
		DirectedSparseGraph g = graph.pathroutines.getGraph();
		DijkstraDistance dd = new DijkstraDistance(g);
		@SuppressWarnings("unchecked")
		Collection<Double> distances = dd.getDistanceMap(DeterministicDirectedSparseGraph.findInitial(g)).values();
		Double result =-1.;
		for(Double distance:distances) if (result<distance) result=distance;
		return result.intValue();
	}

	/** All new states will be given this %% and accept condition. Used to change names for states, 
	 * in order to ensure that tail states have special names 
	 * (i.e. %% of a full set of walk they belong to), 
	 * which can subsequently be used to filter the PTA.
	 */
	public class StateName
	{
		public final int percent;
		public final boolean accept;
		
		public StateName(int per,boolean ac)
		{
			percent = per;accept=ac;
		}
	}
	
	StateName tag;
	
	class PercentLabelledPTA extends PTASequenceSetAutomaton
	{
		// The automaton to back a PTA tree containing all walks generated.
		@Override
		public Object getTheOnlyState() {
			return tag;
		}
		
		@Override
		public boolean shouldBeReturned(Object elem) {
			if (elem == null) // the reject-node of a PTA engine
				return false;
			return ((StateName)elem).accept;
		}
	}
	
	protected PTASequenceSet allSequences = null,extraSequences = null;
	
	/** Generates a random walk through the graph. Since it attempts to add elements to a graph
	 * in the course of checking whether they belong to it, it should be used to add
	 * elements longest-first (this is also important to prevent short paths blocking walks
	 * if an automaton has a narrow path leading to some parts of it). 
	 * A path is only generated if it is not contained in the <em>allSequences</em> collection.
	 *  
	 * @param walkLength the length of a walk. 
	 * @param prefixLen the length of the path to check for existence in a PTA. This is 
	 * useful for generation of negative paths such that for each such path,
	 * a positive prefix of it is unique in a PTA. 
	 * @param positive whether to generate a positive walk.
	 * The member collection <em>allSequences</em> contains all sequences generated so far.
	 * @param prefixForAllSequences prefix of a sequence to generate. Its length is not included in the length of the walk. Can be null in which case prefix is ignored.
	 * @return the path computed. Throws IllegalArgument exception if arguments are wrong. 
	 * Returns null if the requested number of paths cannot be computed (because graph does not 
	 * have enough transitions).
	 */
	List<Label> generateRandomWalk(int walkLength, int prefixLen, boolean positive, List<Label> prefixForAllSequences)
	{
		if (walkLength < 1) 
			throw new IllegalArgumentException("cannot generate paths with length less than one");
		if (prefixLen < 0 || prefixLen > walkLength)
			throw new IllegalArgumentException("invalid prefix length");
		int prefixForAllSequencesLength = prefixForAllSequences == null?0:prefixForAllSequences.size();
		if (prefixForAllSequencesLength > prefixLen)
			throw new IllegalArgumentException("prefix to add to all sequences should be shorter than the length of prefix to check for existence in the list of generated paths");

		List<Label> outcome = null;
		if (walksShouldLeadToInitialState)
			outcome = generateRandomWalkLeadingToTheInitialState(walkLength, prefixLen, positive, prefixForAllSequences);
		else
			outcome = generateUnrestrictedRandomWalk(walkLength, prefixLen, positive, prefixForAllSequences);
		
		return outcome;
	}
	
	List<Label> generateUnrestrictedRandomWalk(int walkLength, int prefixLen, boolean positive, List<Label> prefixForAllSequences)
	{
		int generationAttempt = 0;
		int prefixForAllSequencesLength = prefixForAllSequences == null?0:prefixForAllSequences.size();
		List<Label> path = new ArrayList<Label>(walkLength+prefixForAllSequencesLength);
		
		do
		{
			path.clear();if (prefixForAllSequences != null) path.addAll(prefixForAllSequences);
			CmpVertex current = initialState;

			int positiveLength = positive?walkLength:walkLength-1;// this is how many elements to add to what we already have (prefixForAllSequencesLength).
			if (positiveLength>0)
			{// if we are asked to generate negative paths of length 1, we cannot start with anything positive.
				for(int i=0;i<positiveLength;i++)
				{
					ArrayList<Entry<Label,CmpVertex>> row = transitions.get(current);
					if(row.isEmpty())
						break;// cannot make a transition
					Entry<Label,CmpVertex> inputState = row.get(randomNumberGenerator.nextInt(row.size()));
					path.add(inputState.getKey());current = inputState.getValue();
				}
			}
			
			if (path.size() == prefixForAllSequencesLength+positiveLength && !positive)
			{// successfully generated a positive path of the requested length, append a negative transition.
				// In the situation where we'd like to generate both negatives and 
				// one element shorter positives, we'd have to copy our positive 
				// and then append a negative to the copy. It takes as long to take 
				// all negatives and make copy of all but one elements, given that 
				// they are ArrayLists. 
				ArrayList<Label> rejects = inputsRejected.get(current);
				if (!rejects.isEmpty())
					path.add(rejects.get(randomNumberGenerator.nextInt(rejects.size())));
			}
			
			generationAttempt++;
				
			if (generationAttempt > g.config.getRandomPathAttemptThreshold())
				return null;
		}
		while(prefixLen > 0 && (path.size() < prefixForAllSequencesLength+walkLength || allSequences.contains(path.subList(0, prefixForAllSequencesLength+prefixLen))));
		return path;
	}
	
	/** Constructs the sets necessary to construct walks that avoid an initial state for negative walks and those that terminate at an initial state for positive walks.
	 * This method rebuilds the <b>transitions</b> map. 
	 */ 
	protected Map<CmpVertex,List<Label>> constructShortestPathsToInitAndLongestPathsAvoidingInit()
	{
		if (shortestPathsIntoInit == null)
		{
			LearnerGraphND inverse = new LearnerGraphND(g.config);
			AbstractPathRoutines.buildInverse(g,LearnerGraphND.ignoreNone,inverse);
			shortestPathsIntoInit = new TreeMap<CmpVertex,List<Label>>();
			for(Entry<CmpVertex,List<Label>> path:inverse.pathroutines.computeShortPathsToAllStates().entrySet())
				if (path.getValue().size() == 0)
					shortestPathsIntoInit.put(path.getKey(),Collections.<Label>emptyList());
				else
				{
					Label[] invertedPath = new Label[path.getValue().size()];
					int i=path.getValue().size()-1;
					for(Label elem:path.getValue())
						invertedPath[i--]=elem;
					shortestPathsIntoInit.put(path.getKey(),Arrays.asList(invertedPath));
				}
			
			lengthOfShortestPathsIntoInit = new TreeMap<CmpVertex,Integer>();
			for(Entry<CmpVertex,List<Label>> entry:shortestPathsIntoInit.entrySet())
				lengthOfShortestPathsIntoInit.put(entry.getKey(), entry.getValue().size());

			LearnerGraph withoutInitial = new LearnerGraph(g.config);withoutInitial.initEmpty();
			Queue<CmpVertex> loopsWave = new LinkedList<CmpVertex>();
			Map<CmpVertex,Set<CmpVertex>> vertexToThoseReachableFromIt = new TreeMap<CmpVertex,Set<CmpVertex>>(), vertexToThoseFromWhichItIsReachable = new TreeMap<CmpVertex,Set<CmpVertex>>();

			transitions.clear();
			Queue<CmpVertex> lengthWave = new LinkedList<CmpVertex>();
			longestPathsNotLeadingToInit = new TreeMap<CmpVertex,Integer>();

			for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:g.transitionMatrix.entrySet())
			{// transitions from the initial state are included but those leading to it are not.
				ArrayList<Entry<Label,CmpVertex>> row = new ArrayList<Entry<Label,CmpVertex>>();
				Map<Label,CmpVertex> trs = withoutInitial.createNewRow();withoutInitial.transitionMatrix.put(entry.getKey(), trs);

				for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
					if (transition.getValue() != g.getInit()) // filter out initial states as target ones in order to ensure walks do not go through the initial state.
					{
						row.add(transition);
						trs.put(transition.getKey(), transition.getValue());// we can afford not creating rows for the encountered states because those will be constructed when we meet those nodes as part of exploration of transitionMatrix.
					}
					else
						if (entry.getKey() != g.getInit())
						{
							lengthWave.offer(entry.getKey());longestPathsNotLeadingToInit.put(entry.getKey(),0);
						}
				transitions.put(entry.getKey(), row);
			}
			
			// now for each state we compute the min and max length of a path from it to an initial state, in order for generated traces to match the expected profile.
			LearnerGraphND withoutInitialInverse = new LearnerGraphND(g.config);
			AbstractPathRoutines.buildInverse(withoutInitial,LearnerGraphND.ignoreNone,withoutInitialInverse);
			
			loopsWave.offer(g.getInit());
			vertexToThoseFromWhichItIsReachable.put(g.getInit(),new LinkedHashSet<CmpVertex>());vertexToThoseFromWhichItIsReachable.get(g.getInit()).add(g.getInit());
			vertexToThoseReachableFromIt.put(g.getInit(),new LinkedHashSet<CmpVertex>());
			while(!loopsWave.isEmpty())
			{
				CmpVertex vert = loopsWave.remove();
				Set<CmpVertex> statesLeadingToCurrentOne = vertexToThoseFromWhichItIsReachable.get(vert), statesReachableFromCurrent = vertexToThoseReachableFromIt.get(vert);
				
				for(Entry<CmpVertex,Set<Label>> transition:withoutInitial.pathroutines.getFlowgraph().get(vert).entrySet())
				{
					CmpVertex v=transition.getKey();
					statesReachableFromCurrent.add(v);

					Set<CmpVertex> statesReachableFromNew = vertexToThoseReachableFromIt.get(v);

					if (statesReachableFromNew == null)
					{// we've not seen this vertex yet, add it into our sets
						Set<CmpVertex> statesLeadingToNew = new LinkedHashSet<CmpVertex>();statesLeadingToNew.add(v);vertexToThoseFromWhichItIsReachable.put(v,statesLeadingToNew);
						statesReachableFromNew = new LinkedHashSet<CmpVertex>(); vertexToThoseReachableFromIt.put(v,statesReachableFromNew);
						loopsWave.offer(v);// make sure we explore all paths reachable from it.
					}
						
					// If we have reached a vertex that has already been explored, update the data associated with this vertex and all those that can be reached from it.
					// Where it is a new vertex, we have already added some data about it and all we need to complete it is a few things.
					statesReachableFromCurrent.addAll(statesReachableFromNew);
					for(CmpVertex reachable:statesReachableFromNew) vertexToThoseFromWhichItIsReachable.get(reachable).addAll(statesLeadingToCurrentOne);
					vertexToThoseFromWhichItIsReachable.get(v).addAll(statesLeadingToCurrentOne);
				
				}
				for(CmpVertex leadingTo:statesLeadingToCurrentOne) vertexToThoseReachableFromIt.get(leadingTo).addAll(statesReachableFromCurrent);
			}

			// At this point, we know where loops are, so the goal is to calculate maximal length paths. This one does a different exploration to the loop exploration 
			// above (it can follow previously explored states if this increases the length of maximal possible paths), hence we do it separately.
			while(!lengthWave.isEmpty())
			{
				CmpVertex vert = lengthWave.remove();int currentLen = longestPathsNotLeadingToInit.get(vert);
				if (vertexToThoseReachableFromIt.get(vert).contains(vert))
				{
					for(CmpVertex v:vertexToThoseFromWhichItIsReachable.get(vert))
						longestPathsNotLeadingToInit.put(v,Integer.MAX_VALUE);
				}
				
				for(Entry<CmpVertex,Set<Label>> transition:withoutInitialInverse.pathroutines.getFlowgraph().get(vert).entrySet())
				{
					CmpVertex v=transition.getKey();
					int nextValue = -1;
					if (longestPathsNotLeadingToInit.containsKey(v))
						nextValue = longestPathsNotLeadingToInit.get(v);
					if (nextValue < currentLen+1) // stops the exploration where we find a vertex from which a longer path (incl. infinite) is possible.
					{
						longestPathsNotLeadingToInit.put(v,currentLen+1);lengthWave.offer(v);
					}
				}				
			}
			
			// At this point, all entries longestPathsNotLeadingToInit that do not have an assigned value are the states of the automaton that have no path leading to the initial state.
			// This should not happen by construction but tests verify this.
		}
		
		return shortestPathsIntoInit;
	}
	
	private ArrayList<Entry<Label,CmpVertex>> currentRow = null;
	
	/** lengthMin is the shortest path permitted, this can be negative if we have already gotten as long a path as we like. */
	private Entry<Label,CmpVertex> pickNextState(CmpVertex currentState,int lengthMin, int lengthMax)
	{
		if (currentRow == null) currentRow = new ArrayList<Entry<Label,CmpVertex>>();else currentRow.clear();
		for(Entry<Label,CmpVertex> transition:transitions.get(currentState))
		{
			Integer shortestPathToInit = lengthOfShortestPathsIntoInit.get(transition.getValue());
			if (shortestPathToInit == null)
				throw new IllegalArgumentException("there is no path to the initial state ("+g.getInit()+") from the "+transition.getValue()+" state");
			int newLength = 1+shortestPathToInit;
			if (newLength <= lengthMax && longestPathsNotLeadingToInit.get(transition.getValue()) >= lengthMin) // the comparison will do NullPointer if the initial state is not reachable from some vertices, but this is ensure by construction
				// we should consider this transition if we will not be forced to take a long way to the initial state at the end ...
				// ... and additionally there has to be a way to reach the desired way, no point visiting deadends.
				currentRow.add(transition);
		}
		if(currentRow.isEmpty())
			return null;
		return currentRow.get(randomNumberGenerator.nextInt(currentRow.size()));
	}
	
	/** Shortest paths from every state of our graph to the initial state. */
	protected Map<CmpVertex,List<Label>> shortestPathsIntoInit = null;
	/** Length of the shortest paths from every state of our graph to the initial state. */
	protected Map<CmpVertex,Integer> lengthOfShortestPathsIntoInit = null;
	/** How long are the longest possible paths from each state that do not enter Init. */
	Map<CmpVertex,Integer> longestPathsNotLeadingToInit = null;
	/** How imprecise in walk generation we are prepared to be. */
	protected final int deltaInGenerationOfWalkLeadingToTheInitialState = 1;

	/** Here prefixForAllSequences is included in the length of the walk. */
	protected List<Label> generateRandomWalkLeadingToTheInitialState(int walkLength, int prefixLen, boolean positive, List<Label> prefixForAllSequences)
	{
		int prefixForAllSequencesLength = prefixForAllSequences == null?0:prefixForAllSequences.size();
		
		int generationAttempt = 0;
		List<Label> path = new ArrayList<Label>(walkLength+deltaInGenerationOfWalkLeadingToTheInitialState);
		boolean generationFailed = false;
		do
		{
			generationFailed = false;
			path.clear();if (prefixForAllSequences != null) path.addAll(prefixForAllSequences);
			CmpVertex current = initialState;

			int positiveLength = positive?walkLength:walkLength-1;// this is how many elements to add to what we already have (prefixForAllSequencesLength).
			if (positiveLength>0)
			{// If we are asked to generate negative paths of length 1, we cannot start with anything positive.
				
			 // Where we are offered a prefix longer than a maximal sequence we could generate, generate nothing positive.
				int currentLength = prefixForAllSequencesLength+(positive?lengthOfShortestPathsIntoInit.get(current):0);// for a positive trace, the length is the one we obtain by the walk plus the length of a path to init, for negative traces, this is just zero since we are not aiming to reach init. 
				while(currentLength < positiveLength)
				{
					Entry<Label,CmpVertex> inputState = null;
					//if (currentLength >= positiveLength)
					//	break;// we got a path that is long enough, stop now. The current path is not too long (beyond positiveLength+delta) because in that case pickNextState would have returned null and we would have stopped before making a step.
					
					// Our current length is positiveLength-currentLength short, the subsequent step may get us closer to the target or we may overshoot. 
					// In case of overshoot, the step may get us further away from the target, hence we should not take the step even if it is within delta of the target.
					// This is the reason why lengthMax should be set accordingly in the call to pickNextState below.
					// The absolute max that should be added is positiveLength+deltaInGenerationOfWalkLeadingToTheInitialState-i
					// The desirable one is where positiveLength-currentLength-1 is used instead of deltaInGenerationOfWalkLeadingToTheInitialState, if smaller.
					// The shortest path to init is positiveLength-i-deltaInGenerationOfWalkLeadingToTheInitialState
					int maxLength = positive? positiveLength-path.size()+ Math.min(deltaInGenerationOfWalkLeadingToTheInitialState, positiveLength-currentLength-1) : Integer.MAX_VALUE;// for negative paths, we are not constrained by the maximal length because there is no need to enter an initial sate
					inputState = pickNextState(current,positiveLength-path.size()-deltaInGenerationOfWalkLeadingToTheInitialState, maxLength);
					if (inputState == null)
						// any subsequent step is either impossible (no more transitions from this state, this should be impossible because of the way the automata are generated),
						// or a subsequent step leads us to an extremely long path, hence our only choice is to stop now, possibly way short of target.
						break;
					path.add(inputState.getKey());current = inputState.getValue();
					currentLength = path.size()+(positive?lengthOfShortestPathsIntoInit.get(current):0);
				}
				
				if (positive)
				{// add a walk to the initial state to the path
					path.addAll(shortestPathsIntoInit.get(current));
				}
			}
			
			if (!positive)
			{// successfully generated a positive path of the requested length, append a negative transition.
			 // In the situation where we'd like to generate both negatives and 
			 // one element shorter positives, we'd have to copy our positive 
			 // and then append a negative to the copy. It takes as long to take 
			 // all negatives and make copy of all but one element, given that 
			 // they are ArrayLists. 
				ArrayList<Label> rejects = inputsRejected.get(current);
				if (!rejects.isEmpty())
					path.add(rejects.get(randomNumberGenerator.nextInt(rejects.size())));
				else
					generationFailed = true;// report a failure to generate a negative sequence.
			}
			
			generationAttempt++;
				
			if (generationAttempt > g.config.getRandomPathAttemptThreshold())
				return null;
		}
		while(generationFailed || path.size() < walkLength-deltaInGenerationOfWalkLeadingToTheInitialState || path.size() > walkLength+deltaInGenerationOfWalkLeadingToTheInitialState
				|| (prefixLen > 0 && allSequences.contains(path.subList(0, Math.min(path.size(),prefixLen)))));
		// we terminate if the path is at most deltaInGenerationOfWalkLeadingToTheInitialState short of the expected length. 
		// The use of Math.min is to account for paths constructed being shorter than prefixForAllSequencesLength+prefixLen
		
		return path;
	}
	
	class PercentFilter implements FilterPredicate
	{
		private final int filter; 
		public PercentFilter(int percent)
		{
			filter=percent;
		}
		
		@Override
		public boolean shouldBeReturned(Object name) {
			return ((StateName)name).percent <= filter;
		}
		
	}
	
	class PercentIntervalFilter implements FilterPredicate
	{
		private final int filter; 
		public PercentIntervalFilter(int percent)
		{
			filter=percent;
		}
		
		@Override
		public boolean shouldBeReturned(Object name) {
			return ((StateName)name).percent == filter;
		}
		
	}
	
	/** Returns a PTA consisting of chunks number 0 .. upToChunk (inclusive).
	 * 
	 * @param upToChunk how many chunks to combine before returning the result.
	 * @return the result. 
	 */
	public PTASequenceEngine getAllSequencesPercentageInterval(int chunk)
	{
		if (chunk < 0 || chunk >= getChunkNumber())
			throw new IllegalArgumentException("chunk number "+chunk+" is out of range 0.."+getChunkNumber());
		
		return allSequences.filter(new PercentIntervalFilter(chunk));
	}
	
	/** Returns a PTA consisting of chunk.
	 * 
	 * @param chunk which chunk to return.
	 * @return the result. 
	 */
	public PTASequenceEngine getExtraSequencesPercentageInterval(int chunk)
	{
		if (chunk < 0 || chunk >= getChunkNumber())
			throw new IllegalArgumentException("chunk number "+chunk+" is out of range 0.."+getChunkNumber());
		
		return extraSequences.filter(new PercentIntervalFilter(chunk));
	}
	
	/** Returns a PTA consisting of chunks number 0 .. upToChunk (inclusive).
	 * 
	 * @param upToChunk how many chunks to combine before returning the result.
	 * @return the result. 
	 */
	public PTASequenceEngine getAllSequences(int upToChunk)
	{
		if (upToChunk < 0 || upToChunk >= getChunkNumber())
			throw new IllegalArgumentException("chunk number "+upToChunk+" is out of range 0.."+getChunkNumber());
		
		return allSequences.filter(new PercentFilter(upToChunk));
	}
	
	/** Returns a PTA consisting of chunks number 0 .. upToChunk (inclusive).
	 * 
	 * @param upToChunk how many chunks to combine before returning the result.
	 * @return the result. 
	 */
	public PTASequenceEngine getExtraSequences(int upToChunk)
	{
		if (upToChunk < 0 || upToChunk >= getChunkNumber())
			throw new IllegalArgumentException("chunk number "+upToChunk+" is out of range 0.."+getChunkNumber());
		
		return extraSequences.filter(new PercentFilter(upToChunk));
	}

	/** Generates positive and negative paths where negatives are just 
	 * positives with an extra element added at the end.
	 * If the requested length of sequences is 1, no positives are generated.
	 *  
	 * Data added is split into a number of parts, with a specific 
	 * number of sequences per chunk.
	 * 
	 * @param numberPerChunk number of sequences per chunk.
	 * @param chunks the number of chunks to generate.
	 * @param length the maximal length of paths, minimal is 1.
	 */
	public void generatePosNeg(int numberPerChunk, int chunks)
	{
		if (pathLength < 1)
			throw new IllegalArgumentException("Cannot generate paths with length less than 1");
		if (numberPerChunk % 2 != 0)
			throw new IllegalArgumentException("Number of sequences per chunk must be even");
		chunksGenerated = 0;
		int seqNumber = chunks*numberPerChunk/2;
		int distribution [] = new int[seqNumber];
		RandomLengthGenerator rnd = new RandomLengthGenerator(){

			@Override
			public int getLength() {
				if (pathLength > 1)
					return randomNumberGenerator.nextInt(pathLength-1)+2;// the shortest length is 2
				return 1;
			}

			@Override
			public int getPrefixLength(int len) {
				return len-1;
			}
			
		};
		for(int i=0;i < seqNumber;++i)
			distribution[i]= rnd.getLength();
		Arrays.sort(distribution);
		initAllSequences();
		StateName [] positives = new StateName[chunks], negatives = new StateName[chunks];
		for(int i=0;i< chunks;++i) { positives[i]=new StateName(i,true);negatives[i]=new StateName(i,false); }

		for(int i=seqNumber-1;i>=0;--i)
		{
			tag = negatives[i % chunks];
			List<Label> path = generateRandomWalkWithFudge(distribution[i],rnd,false,null);
			if (path == null)
				throw new IllegalArgumentException("failed to generate a negative"+
						" path of length "+distribution[i]+" (prefix length "+rnd.getPrefixLength(distribution[i])+") after even after trying to fudge it "+
						g.config.getRandomPathAttemptFudgeThreshold()+" times");
			allSequences.add(path);
			tag = positives[i % chunks];
			int prefixLength = rnd.getPrefixLength(path.size());
			if (prefixLength > 0)
				extraSequences.add(path.subList(0, rnd.getPrefixLength(path.size())));// all positives go there
		}
		chunksGenerated = chunks;
	}
	
	public int getChunkNumber()
	{
		return chunksGenerated;
	}
	
	protected int chunksGenerated = 0;
	
	public interface RandomLengthGenerator 
	{
		public int getLength();
		public int getPrefixLength(int len);
	}

	/** Initialises the collection of data. Used to reset the whole thing before
	 * generating walks.
	 */
	protected void initAllSequences()
	{
		tag = new StateName(0,false);
		allSequences = new PTASequenceSet(new PercentLabelledPTA());extraSequences = new PTASequenceSet(new PercentLabelledPTA());
	}
	/** Generates random positive and negative paths. 
	 * Data added is split into a number of parts, with a specific number of sequences per chunk.
	 * 
	 * @param numberPerChunk number of sequences per chunk.
	 * @param chunks the number of chunks to generate.
	 */
	public void generateRandomPosNeg(int numberPerChunk, int chunks)
	{
		generateRandomPosNeg(numberPerChunk,chunks,true);
	}
	
	/** Generates random positive and negative paths. 
	 * Data added is split into a number of parts, with a specific number of sequences per chunk.
	 * 
	 * @param numberPerChunk number of sequences per chunk.
	 * @param chunks the number of chunks to generate.
	 * @param exceptionOnFailure whether to throw an exception if paths cannot be generated.  
	 */
	public void generateRandomPosNeg(int numberPerChunk, int chunks, boolean exceptionOnFailure)
	{
		generateRandomPosNeg(numberPerChunk,chunks,exceptionOnFailure,null);
	}

	/** Generates random positive and negative paths. 
	 * Data added is split into a number of parts, with a specific number of sequences per chunk.
	 * 
	 * @param numberPerChunk number of sequences per chunk.
	 * @param chunks the number of chunks to generate.
	 * @param exceptionOnFailure whether to throw an exception if paths cannot be generated.  
	 */
	public void generateRandomPosNeg(int numberPerChunk, int chunks, boolean exceptionOnFailure, RandomLengthGenerator rnd)
	{
		generateRandomPosNeg(numberPerChunk, chunks, exceptionOnFailure, rnd, true, true, null,null);
	}
	
	/** Generates random positive and negative paths. 
	 * Data added is split into a number of parts, with a specific number of sequences per chunk.
	 * 
	 * @param numberPerChunk number of sequences per chunk.
	 * @param chunks the number of chunks to generate.
	 * @param exceptionOnFailure whether to throw an exception if paths cannot be generated.  
	 * @param attemptPositive whether to attempt to generate positive sequences. If false, positives are not generated (but the number of negatives if requested is still numberPerChunk/2).
	 * @param attemptNegative whether to attempt to generate positive sequences. If false, negatives are not generated (but the number of positives if requested is still numberPerChunk/2).
	 * @param initialSet if non-null, the collection to initialise our sequences with. Useful for the purpose of augmenting an existing set with new sequences that are supposed to be different from the existing ones.
	 * @param initial path if non-null, all sequences start with this path. Very useful where initial state is set to one of the states of a graph and we generate paths starting from that state. 
	 */
	public void generateRandomPosNeg(int numberPerChunk, int chunks, boolean exceptionOnFailure, RandomLengthGenerator argRnd, boolean attemptPositive, boolean attemptNegative, Collection<List<Label>> initialSet, List<Label> prefix)
	{
		if (pathLength < 1)
			throw new IllegalArgumentException("Cannot generate paths of length less than 1");
		if (numberPerChunk % 2 != 0)
			throw new IllegalArgumentException("Number of sequences per chunk must be even");
		chunksGenerated = 0;

		int seqNumber = chunks*numberPerChunk/2;
		int distribution [] = new int[seqNumber];
		
		RandomLengthGenerator rnd = argRnd;
		if (rnd == null) rnd = new RandomLengthGenerator(){

			@Override
			public int getLength() {
				return randomNumberGenerator.nextInt(pathLength)+1;
			}

			@Override
			public int getPrefixLength(int len) {
				return len;
			}
			
		};

		for(int i=0;i < seqNumber;++i)
			distribution[i]=rnd.getLength();
		Arrays.sort(distribution);
		initAllSequences();
		if (initialSet != null) for (List<Label> seq:initialSet) allSequences.add(seq);

		StateName [] positives = new StateName[chunks], negatives = new StateName[chunks];
		for(int i=0;i< chunks;++i) { positives[i]=new StateName(i,true);negatives[i]=new StateName(i,false); }

		for(int i=seqNumber-1;i>=0;--i)
		{
			tag = negatives[i % chunks];
			List<Label> path = null;
			
			if (attemptNegative)
			{
				path = generateRandomWalkWithFudge(distribution[i],rnd,false, prefix);
				if (path != null)
					allSequences.add(path);
				else
					if (exceptionOnFailure)
						throw new IllegalArgumentException("failed to generate a negative"+
							" path of length "+distribution[i]+" (prefix length "+rnd.getPrefixLength(distribution[i])+") after even after trying to fudge it "+
							g.config.getRandomPathAttemptFudgeThreshold()+" times");
			}
			tag = positives[i % chunks];
			
			if (attemptPositive)
			{
				path=generateRandomWalkWithFudge(distribution[i],rnd,true, prefix);
				if (path != null)
					allSequences.add(path);
				else
					if (exceptionOnFailure)
						throw new IllegalArgumentException("failed to generate a positive"+
							" path of length "+distribution[i]+" (prefix length "+rnd.getPrefixLength(distribution[i])+") after even after trying to fudge it "+
							g.config.getRandomPathAttemptFudgeThreshold()+" times");
			}
		}
		chunksGenerated = chunks;
	}
	
	/** Counts the number of times sequence length was revised during
	 * sequence generation. 0 means that the length was never revised. 
	 */
	private List<String> fudgeDetails = new LinkedList<String>();
	
	public List<String> getFudgeDetails()
	{
		return fudgeDetails;
	}
	
	/** Generates a walk, but if none can be produced for a given sequence length, 
	 * attempts to randomly choose a different length.
	 * @param origWalkLength the length of the walk to start with, it is subsequently updated when walks of the specified length cannot be generated.
	 * @param rnd random generator to use for selection of inputs
	 * @param positive whether the path generated should be present or absent in an automaton of interest
	 * @param prefix prefix of a sequence to generate. Its length is not included in the length of the walk. Can be null in which case prefix is ignored.
	 */
	List<Label> generateRandomWalkWithFudge(int origWalkLength, RandomLengthGenerator rnd,boolean positive, List<Label> prefix)
	{
		List<Label> path = generateRandomWalk(origWalkLength, rnd.getPrefixLength(origWalkLength), positive, prefix);
		if (path != null)
		{
			assert !allSequences.containsAsLeaf(path);assert !extraSequences.containsAsLeaf(path);
			return path;
		}
		for(int i=1;i<g.config.getRandomPathAttemptFudgeThreshold();++i)
		{
			int revisedWalkLength = rnd.getLength();
			path = generateRandomWalk(revisedWalkLength, rnd.getPrefixLength(revisedWalkLength), positive, prefix);
			if (path != null)
			{
				boolean notPrefix = !allSequences.extendsLeaf(path) && !extraSequences.extendsLeaf(path);
				fudgeDetails.add(origWalkLength+","+rnd.getPrefixLength(origWalkLength)+" "+(positive?"positive":"negative")+"->"+revisedWalkLength+","+rnd.getPrefixLength(revisedWalkLength)+
						" "+(notPrefix?"done":"ATTEMPT FAILED"));
				if (notPrefix)
					return path;
			}
		}
		return null;
	}

	
}
