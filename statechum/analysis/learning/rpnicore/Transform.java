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

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID.VertKind;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.LearnerGraph.NonExistingPaths;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod.EquivalentStatesException;
import statechum.apps.QSMTool;

public class Transform 
{
	final LearnerGraph coregraph;
	
	/** Associates this object to LearnerGraph it is using for data to operate on. 
	 * Important: the constructor should not access any data in LearnerGraph 
	 * because it is usually invoked during the construction phase of LearnerGraph 
	 * when no data is yet available.
	 */
	Transform(LearnerGraph g)
	{
		coregraph = g;
	}
	
	public static int HammingDistance(List<Boolean> A, List<Boolean> B)
	{
		if (A.size() != B.size())
			throw new IllegalArgumentException("sequences of different length passed");
		int distance = 0;
		Iterator<Boolean> A_Iter = A.iterator(), B_Iter = B.iterator();
		while(A_Iter.hasNext())
		{
			if (!A_Iter.next().equals(B_Iter.next())) ++distance;
		}
		return distance;
	}
	
	/** Adds a reject transition to a randomly-chosen state, if possible (the chosen state has an input not in the current alphabet). */
	public void addRejectStateRandomly(Random rnd)
	{
		CmpVertex v =coregraph.pathroutines.pickRandomState(rnd);
		HashSet<String> possibilities = new HashSet<String>();possibilities.addAll(coregraph.learnerCache.getAlphabet());
		possibilities.removeAll(coregraph.transitionMatrix.get(v).keySet());
		Iterator<String> inputIt = possibilities.iterator();
		if (inputIt.hasNext())
		{
			CmpVertex newVertex = AbstractLearnerGraph.generateNewCmpVertex(coregraph.nextID(false), coregraph.config);
			newVertex.setAccept(false);
			coregraph.transitionMatrix.put(newVertex, coregraph.createNewRow());
			coregraph.transitionMatrix.get(v).put(inputIt.next(),newVertex);
		}
	}
	
	/** Given a state and a W set, computes a map from those sequences to booleans representing
	 * whether those sequences to true/false depending whether a specific can be followed from
	 * the given state. 
	 * @param state the state to compute applicability of paths from
	 * @param wSet the set of sequences to manipulate
	 * @return a list of booleans representing applicability of sequences.
	 */
	public static List<Boolean> wToBooleans(LearnerGraph g, CmpVertex state, Collection<List<String>> wSet)
	{
		List<Boolean> result = new LinkedList<Boolean>();
		for(List<String> seq:wSet)
			result.add(g.paths.tracePath(seq,state) == AbstractOracle.USER_ACCEPTED);
		return result;
	}
	
	Set<CmpVertex> fragileStates = new HashSet<CmpVertex>();
	
	/** Computes Hamming distances between elements of a W set and outputs 
	 * the description of the results, potentially including a lot of statistical information. 
	 * 
	 * @param produceStatistics whether to output lots of details.
	 * @return Hamming distances between vectors corresponding to responses of states 
	 * of our graph to sequences in the W set.
	 */
	public String ComputeHamming(boolean produceStatistics)
	{
		List<List<String>> wSet = new LinkedList<List<String>>();wSet.addAll(WMethod.computeWSet_reducedmemory(coregraph));
		Map<CmpVertex,List<Boolean>> bitVector = new TreeMap<CmpVertex,List<Boolean>>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> state:coregraph.transitionMatrix.entrySet())
			bitVector.put(state.getKey(),wToBooleans(coregraph,state.getKey(), wSet));
		int min=Integer.MAX_VALUE,max=0;double average = 0;
		Map<Integer,AtomicInteger> statistics = new HashMap<Integer,AtomicInteger>();
		Object stateToBitVector[] = bitVector.entrySet().toArray();
		for(int i=0;i< stateToBitVector.length;++i)
			for(int j=i+1;j<stateToBitVector.length;++j)
			{
				Entry<CmpVertex,List<Boolean>> vecI = (Entry<CmpVertex,List<Boolean>>) stateToBitVector[i],vecJ = (Entry<CmpVertex,List<Boolean>>)stateToBitVector[j];
				int h = HammingDistance(vecI.getValue(), vecJ.getValue());
				average+=h;
				if (min > h) min = h;
				if (max < h) max = h;
				AtomicInteger atomicH = statistics.get(h);if (atomicH == null) { atomicH = new AtomicInteger(1);statistics.put(h, atomicH); } else atomicH.addAndGet(1);
				if (h == 1) fragileStates.add(vecI.getKey());
			}
		String result =" ONE:"+statistics.get(1)+" ";
		for(Entry<Integer,AtomicInteger> pair:statistics.entrySet()) result+=" "+pair.getKey()+":"+pair.getValue();
		result+="\n";
		for(CmpVertex fragile:fragileStates) result+=" "+fragile;result+="\n";
		int counter =  bitVector.size()*(bitVector.size()-1)/2;
		String basicInfo = "Hamming distances min: "+min+" max: "+max;
		result+="\n"+basicInfo+" average: "+(average/counter);
		return produceStatistics?result:basicInfo;
	}

	/** Takes a graph and outputs vectors corresponding to responses of states 
	 * of our graph to sequences in the W set.
	 * 
	 * @param g the graph which states to examine
	 * @param wSet the W set to compute the response of g to.
	 */
	public static String getVectors(LearnerGraph g, Collection<List<String>> wSet)
	{
		String result = "";
		for(Entry<CmpVertex,Map<String,CmpVertex>> state:g.transitionMatrix.entrySet())
			result+="\n"+wToBooleans(g,state.getKey(), wSet);
		result+="\n";
		return result;
	}
	
	/** A graph may be completely random or built with a specific distribution of labels in mind.
	 * The likelyhood that a label will be used on a transition from some state is referred to
	 * as a <em>fill factor</em>. You can think of a graph as a glass which is filled with 
	 * transitions, but the idea is to distribute them such that each state has the same number 
	 * of outgoing and incoming transitions. This is the reason why populating a graph is compared to
	 * filling a glass with water - transitions are added in "levels".<p>
	 * A W set may consist of singleton sequences (a very common case), in this case we may compute
	 * a restriction of g to a subset of alphabet, using only inputs used in W. This function 
	 * computes a fill factor of the resulting graph - even if the original graph was built well,
	 * the fill factor of the considered subgraph may happen to be rather different.
	 *  
	 * @param g the graph to restrict
	 * @param wSet a collection of singleton sequences to restrict g to.
	 * @return the fill factor of the restriction.
	 */
	public static double getEffectiveFillRate(LearnerGraph g, Collection<List<String>> wSet)
	{
		int positives=0;
		for(Entry<CmpVertex,Map<String,CmpVertex>> state:g.transitionMatrix.entrySet())
			for(Boolean b:wToBooleans(g,state.getKey(), wSet))
				if (b.booleanValue()) ++positives;
		return ((double)positives)/(g.getStateNumber()*wSet.size());
	}
	
	// grep SUCCESS *|awk -F, 'BEGIN {a=0;count=0;} { a+=$3;++count;} END{print a/count;}'
	
	/** Adds all possible transitions to a graph and computes the likelyhood that the original
	 * W will not work in a modified graph. The implementation only adds transitions 
	 * with labels from the W set (all the other labels will not cause a W set to fail
	 * to distinguish states) and only adds loopback transitions in each state (target
	 * state does not affect applicability of W consisting of singleton sequences which
	 * is the only case considered below and an exception will be thrown if W fails to 
	 * consist of singletons).
	 * 
	 *  @return description of the results.
	 */ 
	public String checkWChanged()
	{
		String result = "";
		Collection<List<String>> wSet = WMethod.computeWSet_reducedmemory(coregraph);
		Set<String> Walphabet = new HashSet<String>();
		for(List<String> wSeq:wSet)
		{
			if (wSeq.size() != 1)
				throw new IllegalArgumentException("non-singleton W");
			Walphabet.add(wSeq.iterator().next());
		}
		Collection<String> alphabet = coregraph.pathroutines.computeAlphabet();
		double fillFactor = getEffectiveFillRate(coregraph, wSet);//transitionsFromEveryState/alphabet.size();
		result+=getVectors(coregraph, wSet);
		double average = (1-fillFactor)*wSet.size()*coregraph.getStateNumber();
		int changeNumber = 0, total =0;
		Map<String,AtomicInteger> labelUsage = new HashMap<String,AtomicInteger>();for(String l:alphabet) labelUsage.put(l, new AtomicInteger());
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
		{
			Collection<String> newLabels = new HashSet<String>();newLabels.addAll(Walphabet);newLabels.removeAll(entry.getValue().keySet());
			int changesForThisState = 0;
			
			for(String lbl:entry.getValue().keySet()) labelUsage.get(lbl).addAndGet(1);
			
			for(String label:newLabels)
			{
				LearnerGraph newGraph = new LearnerGraph(coregraph,coregraph.config);
				CmpVertex currState = newGraph.findVertex(entry.getKey().getID());
				newGraph.transitionMatrix.get(currState).put(label, currState);
				String description = newGraph.wmethod.checkW_is_corrent_boolean(wSet);
				boolean changed = (description != null);
/*
				for(Entry<CmpVertex,Map<String,CmpVertex>> state:graph.transitionMatrix.entrySet())
				{
					LearnerGraph aGraph = graph.copy(graph.config);
					CmpVertex aState = aGraph.findVertex(entry.getKey().getName());
					aGraph.transitionMatrix.get(aState).put(label, aGraph.findVertex(state.getKey().getName()));
					if (changed != (aGraph.wmethod.checkW_is_corrent_boolean(wSet) != null))
						throw new IllegalArgumentException("inconsistent W set results");
				}
*/
				if (changed)
					++changesForThisState;
				++total;
			}
			changeNumber+=changesForThisState;
			result+="changes for "+entry.getKey().getID().toString()+" "+changesForThisState+" (max "+newLabels.size()+"), max for add/remove is "+Walphabet.size()+"\n";
		}
		double stateNumber = coregraph.getStateNumber();
		double wsize = wSet.size();
		double expectedNrOfChanges = wsize*2*fillFactor*(1-fillFactor)*Math.pow(fillFactor*fillFactor+(1-fillFactor)*(1-fillFactor), wsize-1)*
			stateNumber*(stateNumber-1)/2;
		result+="Distribution of labels: ";for(Entry<String,AtomicInteger> en:labelUsage.entrySet()) result+=" "+en.getValue();result+="\n";
		result+="Distribution of elements of W: ";for(String wElem:Walphabet) result+=" "+labelUsage.get(wElem);result+="\n";
		return Math.abs(expectedNrOfChanges-changeNumber)/changeNumber+"\n"+result+"W size: "+wSet.size()+" W changes: "+changeNumber+ " out of "+total+" (expected "+average+"), \nfill factor is "+fillFactor+"\n "+
			"Expected number of changes is: "+expectedNrOfChanges
		;
	}
	
	/** Given a tentative PTA and a maximal automaton, this method adds reject-traces from the maximal
	 * automaton to tentative PTA in such a way that no new positive paths are created. The exploration
	 * is performed by navigating a cross-product of states because loops in a tentative PTA may have
	 * to be unrolled before a reject-node can be added. Unrolling of loops may be disabled by setting
	 * unroll to false. Where the two automata contradict, an exception is thrown; it is possible to 
	 * override transitions in a tentative PTA by those from the maximal automaton if override is set
	 * to true, permitting <em>augmentFromMAX</em> to be used as a kind of AugmentMAX (<em>augmentPTA</em> 
	 * with <em>max</em> argument set to true). 
	 *   
	 * @param what tentative PTA to update
	 * @param from maximal automaton to update from
	 * @param override whether to replace parts of tentative PTA with those from maximal automaton if the two are in conflict.
	 * @param maxIsPartial whether a maximal automaton is partial, i.e. there may be sequences in our tentative PTA which are not reflected in a 
	 * maximal automaton. Given that the two are different graphs which may have different alphabets, it might be best to check for such an
	 * incompatibility at runtime here since such a case should never happen anyway (this would be so if our ltl2ba system did not produce a correct
	 * automaton).
	 * @param config configuration to use for construction of a new graph.
	 * @param checkWasModified used only for testing (should be false otherwise). When set to true this method checks that 
	 * the graphModified variable reflects the equivalence of the original and new graphs.
	 * @return the result of updating graph with the maximal automaton if the graph was modified; null otherwise. The original graph is untouched. 
	 */
	public static LearnerGraph augmentFromMAX(LearnerGraph graph, LearnerGraph from, boolean override, 
			boolean maxIsPartial, Configuration config, boolean checkWasModified)
	{
		final Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
		final Map<StatePair,CmpVertex> pairsToGraphStates = new HashMap<StatePair,CmpVertex>();
		LearnerGraph result = new LearnerGraph(config);result.initEmpty();
		// Two sets are constructed so that I do not have to think about vertices which are shared between the two graphs regardless whether such a case is possible or not.
		final Set<CmpVertex> encounteredGraph = new HashSet<CmpVertex>();
		StatePair statePair = new StatePair(graph.init,from.init);
		encounteredGraph.add(statePair.firstElem);
		result.init = AbstractLearnerGraph.cloneCmpVertex(graph.init, config);
		result.transitionMatrix.put(result.init, result.createNewRow());
		pairsToGraphStates.put(statePair, result.init);
		boolean graphModified = false;
		if (statePair.firstElem.isAccept() && !statePair.secondElem.isAccept())
		{// initial states are incompatible because the tentative automaton is accept and 
		 // the max automaton is reject, hence either override if possible and requested or throw.
			if (override)
			{
				result.init.setAccept(false);graphModified = true;
			}
			else
				throw new IllegalArgumentException("incompatible labelling: maximal automaton is all-reject and tentative one is not");			
			
		}

		if (statePair.firstElem.isAccept() == statePair.secondElem.isAccept())
			currentExplorationBoundary.add(statePair);

		Map<String,CmpVertex> emptyTargets = result.createNewRow();
		
		while(!currentExplorationBoundary.isEmpty())
		{
			statePair = currentExplorationBoundary.remove();
			assert graph.transitionMatrix.containsKey(statePair.firstElem) : "state "+statePair.firstElem+" is not known to the first graph";
			assert statePair.secondElem == null || from.transitionMatrix.containsKey(statePair.secondElem) : "state "+statePair.secondElem+" is not known to the second graph";
			assert statePair.secondElem == null || statePair.firstElem.isAccept() == statePair.secondElem.isAccept() : "incompatible labelling of "+statePair;
						
			Map<String,CmpVertex> graphTargets = graph.transitionMatrix.get(statePair.firstElem), 
				maxTargets = statePair.secondElem == null? emptyTargets:from.transitionMatrix.get(statePair.secondElem);
			CmpVertex currentRepresentative = pairsToGraphStates.get(statePair);assert currentRepresentative != null;
			for(Entry<String,CmpVertex> labelstate:graphTargets.entrySet())
			{
				String label = labelstate.getKey();
				CmpVertex graphState = labelstate.getValue();// the original one
				CmpVertex maxState = maxTargets.get(label);

				if (maxState == null)
				{// this is the case where a transition in a tentative graph is not matched by any in a maximal automaton
					if (!maxIsPartial)
						throw new IllegalArgumentException("In state pair "+statePair+" transition labelled by "+label+" is not matched in a maximal automaton");
				}
				
				StatePair nextPair = new StatePair(graphState,maxState);
				// Now that we're making a step to a state pair where (graphState,maxState) pair has not been seen before,
				// it is quite possible that we have to clone the corresponding state in a tentative graph so as to ensure
				// that each state from a tentative graph is paired with no more than a single state in a maximal automaton 
				// (this corresponds to a construction of a cross-product of states).
				// A state of a tentative state can be unpaired if the maximal automaton is partial, 
				// i.e. it contains a number of counter-examples rather than all possible sequences. This is another
				// thing to check for in this method - if taking of an LTL-derived graph this should be deemed an error.
				boolean shouldDescend = true;
				CmpVertex nextGraphVertex = pairsToGraphStates.get(nextPair);// get a state representing the next pair of states
				if (nextGraphVertex == null)
				{// not seen this pair already hence might have to clone.
					if (!encounteredGraph.contains(graphState))
					{// since we did not see this pair before, the first encountered 
					 // vertex (graphState) is now a representative of the pair nextPair
						nextGraphVertex = AbstractLearnerGraph.cloneCmpVertex(graphState, config);encounteredGraph.add(graphState);
						pairsToGraphStates.put(nextPair,nextGraphVertex);
						result.transitionMatrix.put(nextGraphVertex, result.createNewRow());
						shouldDescend = nextGraphVertex.isAccept();
					}
					else
					{// graphState already paired with one of the states in maximal automaton hence clone the state
						boolean accept = graphState.isAccept() && (maxState == null || maxState.isAccept());// do not descend if the next state is reject or the next state in a maximal automaton is reject
						
						if (graphState.isAccept() != accept)
						{// tentative automaton reaches an accept state but the maximal automaton gets into reject-state 
							if (!override)
								throw new IllegalArgumentException("incompatible labelling: maximal automaton chops off some paths in a tentative automaton");
							graphModified=true;
						}
						nextGraphVertex = AbstractLearnerGraph.generateNewCmpVertex(result.nextID(accept), config);
						if (GlobalConfiguration.getConfiguration().isAssertEnabled() && result.findVertex(nextGraphVertex.getID()) != null) throw new IllegalArgumentException("duplicate vertex with ID "+nextGraphVertex.getID()+" in graph "+result);
						DeterministicDirectedSparseGraph.copyVertexData(graphState, nextGraphVertex);nextGraphVertex.setAccept(accept);
						result.transitionMatrix.put(nextGraphVertex,result.createNewRow());
						
						pairsToGraphStates.put(nextPair, nextGraphVertex);
						if (!accept) shouldDescend = false;
					}
				}
				else // already seen the next pair hence no need to descend
					shouldDescend = false;
				
				result.transitionMatrix.get(currentRepresentative).put(label,nextGraphVertex);

				if (shouldDescend)
				// need to explore all transitions from the new state pair.
					currentExplorationBoundary.offer(nextPair);
				
			}
			
			for(Entry<String,CmpVertex> labelstate:maxTargets.entrySet())
			{
				String label = labelstate.getKey();
				if (!graphTargets.containsKey(label) && !labelstate.getValue().isAccept())
				{// a transition in a maximal automaton is not matched but leads to a reject-state hence direct to a reject-state adding it if necessary
					CmpVertex newVert = pairsToGraphStates.get(new StatePair(null,labelstate.getValue()));
					if (newVert == null)
					{
						newVert = result.copyVertexUnderDifferentName(labelstate.getValue());
						pairsToGraphStates.put(new StatePair(null,labelstate.getValue()), newVert);
					}
					result.transitionMatrix.get(currentRepresentative).put(label, newVert);graphModified=true;
				}
			}
		}
		
		if (GlobalConfiguration.getConfiguration().isAssertEnabled() && checkWasModified)
		{// if graphModified is true the graph should not have been modified and vice-versa
			DifferentFSMException ex = WMethod.checkM(graph, result);
			assert graphModified == (ex != null);
		}
		return graphModified?result:null;
	}

	/** We explore a cross-product of a 
	 * <ul>
	 * <li>union of a tentative automaton with one of the "then automata" and </li>
	 * <li>property automaton.</li>
	 * </ul>
	 * Although there could be numerous instances of "then" parts, exploration
	 * will not discover anything new if the three states of graphState, thenState and propertyState 
	 * have been visited before. This is why there is no record of which exactly instance is being
	 * explored.
	 */
	public static class ExplorationElement
	{
		/** How far from the last existing state in a tentative automaton we've gone. */
		public final int depth;
		public final CmpVertex graphState, thenState, propertyState;

		public ExplorationElement(CmpVertex graphS, CmpVertex thenS, CmpVertex propertyS, int currDepth)
		{
			graphState = graphS;thenState = thenS;propertyState = propertyS;depth = currDepth;
		}
		
		/**
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result
					+ ((graphState == null) ? 0 : graphState.hashCode());
			result = prime * result
					+ ((propertyState == null) ? 0 : propertyState.hashCode());
			result = prime * result
					+ ((thenState == null) ? 0 : thenState.hashCode());
			return result;
		}

		/**
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj instanceof ExplorationElement))
				return false;
			ExplorationElement other = (ExplorationElement) obj;
			if (graphState == null) {
				if (other.graphState != null)
					return false;
			} else if (!graphState.equals(other.graphState))
				return false;
			if (propertyState == null) {
				if (other.propertyState != null)
					return false;
			} else if (!propertyState.equals(other.propertyState))
				return false;
			if (thenState == null) {
				if (other.thenState != null)
					return false;
			} else if (!thenState.equals(other.thenState))
				return false;
			return true;
		}
	}
	
	/** Can be used both to add new transitions to the graph (at most <em>howMayToAdd</em> waves) and to check if the
	 * property answers the supplied questions.
	 * 
	 * @param graph graph to consider and perhaps modify
	 * @param questionPaths PTA with questions, ignored if null, otherwise answered questions are marked.
	 * @param ifthenGraph property automaton to consider.
	 * @param howManyToAdd how many waves of transitions to add to the graph. 
	 * This means that paths of at most <em>howMayToAdd</em> transitions will be added. 
	 * If this value if not positive, the graph remains unchanged.
	 */
	public static void augmentFromIfThenAutomaton(LearnerGraph graph, NonExistingPaths questionPaths, LearnerGraph ifthenGraph,  
			int howManyToAdd) throws IncompatibleStatesException
	{
		for(CmpVertex state:graph.transitionMatrix.keySet())
			if (state.getID().getKind() == VertKind.NONEXISTING)
				throw new IllegalArgumentException("a graph cannot contain non-existing vertices");
		
		final Queue<ExplorationElement> currentExplorationBoundary = new LinkedList<ExplorationElement>();// FIFO queue
		final Set<ExplorationElement> visited = new HashSet<ExplorationElement>();
		ExplorationElement explorationElement = new ExplorationElement(graph.init,null,ifthenGraph.init,0);
		currentExplorationBoundary.add(explorationElement);
		
		while(!currentExplorationBoundary.isEmpty())
		{
			explorationElement = currentExplorationBoundary.remove();
			assert explorationElement.graphState == null || graph.transitionMatrix.containsKey(explorationElement.graphState) : "state "+explorationElement.graphState+" is not known to the tentative automaton";
			assert explorationElement.propertyState == null || ifthenGraph.transitionMatrix.containsKey(explorationElement.propertyState) : "state "+explorationElement.propertyState+" is not known to the property graph";
			if (explorationElement.thenState != null && explorationElement.graphState != null &&
					explorationElement.thenState.isAccept() != explorationElement.graphState.isAccept())
				throw new IncompatibleStatesException("cannot merge a tentative state "+explorationElement.graphState+" with THEN state "+explorationElement.thenState);
						
			// There are eight combinations of null/non-null values of the current states in total,
			// 	graph	THEN 	property|	consider labels	| 	meaning
			// 	.		.		.		|	graph & THEN	|	proceed with matching of a graph and then to a property automaton
			//	.		.		null	|	graph & THEN	|	proceed with matching of a graph and then to a property automaton,
			//							|					|	expecting the property automaton to be all-accept.
			//	.		null	.		|	graph			|	proceed with matching to a property automaton
			//	.		null	null	|	-				|	ignore
			// 	null	.		.		|	THEN			|	ignore - no point matching to a property automaton
			//	null	.		null	|	THEN			|	ignore - no point extending
			//	null	null	.		|	-				|	ignore
			//	null	null	null	|	-				|	ignore

			if (explorationElement.graphState != null && explorationElement.thenState == null && explorationElement.propertyState == null)
				continue;// strictly speaking, this is an optimisation: without this check I'll explore all states of a tentative graph, doing nothing for all of them 
			
			if (explorationElement.propertyState != null)
			{
				Map<CmpVertex,JUConstants.PAIRCOMPATIBILITY> compatibility = ifthenGraph.pairCompatibility.compatibility.get(explorationElement.propertyState);
				if (compatibility != null)
					for(Entry<CmpVertex,JUConstants.PAIRCOMPATIBILITY> entry:compatibility.entrySet())
						if (entry.getValue() == JUConstants.PAIRCOMPATIBILITY.THEN)
						{// we hit a match-state, add next states
							ExplorationElement nextExplorationElement = new ExplorationElement(explorationElement.graphState,entry.getKey(),explorationElement.propertyState, explorationElement.depth);
							if (!visited.contains(nextExplorationElement))
							{// not seen this triple already
								visited.add(nextExplorationElement);currentExplorationBoundary.offer(nextExplorationElement);
							}
						}
			}
			
			Map<String,CmpVertex> graphTargets = null;
			if (explorationElement.graphState != null)
			{
				if (questionPaths == null)
					graphTargets = graph.transitionMatrix.get(explorationElement.graphState);
				else
				{
					graphTargets = questionPaths.getNonExistingTransitionMatrix().get(explorationElement.graphState);
					if (graphTargets == null) // the current state is normal rather than partially or completely non-existent.
						graphTargets = graph.transitionMatrix.get(explorationElement.graphState);
					questionPaths.nonExistingVertices.remove(explorationElement.graphState);// we may attempt to remove an element which exists but it does matter since removing an element from a collection not containing that element is fine 
				}
			}

			Map<String,CmpVertex> propertyTargets = explorationElement.propertyState == null?null:ifthenGraph.transitionMatrix.get(explorationElement.propertyState),
				thenTargets = explorationElement.thenState == null?null:ifthenGraph.transitionMatrix.get(explorationElement.thenState);
				
			if (graphTargets != null)
			{
				// exploring the graph
				for(Entry<String,CmpVertex> labelstate:graphTargets.entrySet())
				{
					String label = labelstate.getKey();
					CmpVertex nextGraphState = labelstate.getValue();
					final CmpVertex nextPropertyState = propertyTargets == null?null:propertyTargets.get(label);
					final CmpVertex nextThenState = thenTargets == null?null:thenTargets.get(label);
					int depth = explorationElement.depth;
					
					if (nextGraphState == null && nextThenState != null && depth < howManyToAdd)
					{// the graph cannot make a transition but THEN machine can, hence we add a new transition to the graph
						if (!explorationElement.graphState.isAccept())
							throw new IncompatibleStatesException("cannot extend a reject state "+explorationElement.graphState+" with THEN state "+explorationElement.thenState);
						
						nextGraphState = AbstractLearnerGraph.generateNewCmpVertex(graph.nextID(nextThenState.isAccept()), graph.config);
						if (GlobalConfiguration.getConfiguration().isAssertEnabled() && graph.findVertex(nextGraphState.getID()) != null) throw new IllegalArgumentException("duplicate vertex with ID "+nextGraphState.getID()+" in graph "+graph);
						DeterministicDirectedSparseGraph.copyVertexData(nextThenState, nextGraphState);nextGraphState.setAccept(nextThenState.isAccept());
						graph.transitionMatrix.put(nextGraphState,graph.createNewRow());graphTargets.put(label, nextGraphState);
						++depth;// we made one more transition
					}

					ExplorationElement nextExplorationElement = new ExplorationElement(nextGraphState,nextThenState,nextPropertyState,depth);
					if (!visited.contains(nextExplorationElement))
					{// not seen this triple already
						visited.add(nextExplorationElement);currentExplorationBoundary.offer(nextExplorationElement);
					}
				}
			
				if (thenTargets != null) // Exploring the added "THEN" graph
					for(Entry<String,CmpVertex> labelstate:thenTargets.entrySet())
					{
						String label = labelstate.getKey();
						CmpVertex nextGraphState = labelstate.getValue();
						final CmpVertex nextPropertyState = labelstate.getValue();
						final CmpVertex nextThenState = thenTargets == null?null:thenTargets.get(label);
						
						int depth = explorationElement.depth;
						
						if (nextGraphState == null && nextThenState != null && depth < howManyToAdd)
						{// the graph cannot make a transition but THEN machine can, hence we add a new transition to the graph
							if (!explorationElement.graphState.isAccept())
								throw new IncompatibleStatesException("cannot extend a reject state "+explorationElement.graphState+" with THEN state "+explorationElement.thenState);

							nextGraphState = AbstractLearnerGraph.generateNewCmpVertex(graph.nextID(nextThenState.isAccept()), graph.config);
							if (GlobalConfiguration.getConfiguration().isAssertEnabled() && graph.findVertex(nextGraphState.getID()) != null) throw new IllegalArgumentException("duplicate vertex with ID "+nextGraphState.getID()+" in graph "+graph);
							DeterministicDirectedSparseGraph.copyVertexData(nextThenState, nextGraphState);nextGraphState.setAccept(nextThenState.isAccept());
							graph.transitionMatrix.put(nextGraphState,graph.createNewRow());graphTargets.put(label, nextGraphState);
							++depth;// we made one more transition
						}
	
						ExplorationElement nextExplorationElement = new ExplorationElement(nextGraphState,nextThenState,nextPropertyState,depth);
						if (!visited.contains(nextExplorationElement))
						{// not seen this triple already (note that matched states added in the labelstate:graphTargets.entrySet() loop 
						 // will be ignored here, including the state which have just been added above).
							visited.add(nextExplorationElement);currentExplorationBoundary.offer(nextExplorationElement);
						}
					}
			}
			
			// There is no point iterating through transitions possible for a property graph which are not matched
			// in a tentative automaton - those transitions are ignored.
		}// while(!currentExplorationBoundary.isEmpty())
	}
	
	/** Given a deterministic version of a maximal automaton, this method converts it to an if-then
	 * automaton which can be used to augment graphs or to answer questions.
	 * States are not cloned.
	 * 
	 * @param ltl what to convert
	 * @return the result of conversion
	 */
	public static LearnerGraph ltlToIfThenAutomaton(LearnerGraph ltl)
	{
		LearnerGraph result = new LearnerGraph(ltl,ltl.config);
		
		Map<Set<String>,CmpVertex> rejectInputsToRejectGraph = new HashMap<Set<String>,CmpVertex>();
		
		// first pass - computing an alphabet
		Set<String> alphabet = result.learnerCache.getAlphabet();
		Map<CmpVertex,Map<String,CmpVertex>> extraRows = ltl.createNewTransitionMatrix();
		// second pass - checking if any transitions need to be added and adding them.
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:result.transitionMatrix.entrySet())
		{
			Set<String> labelsRejected = new TreeSet<String>();
			labelsRejected.addAll(alphabet);labelsRejected.removeAll(entry.getValue().keySet());
			if (!labelsRejected.isEmpty())
			{
				CmpVertex thenGraph = rejectInputsToRejectGraph.get(labelsRejected);
				if (thenGraph == null)
				{// create a THEN graph which rejects all transitions with inputs rejected from entry.getKey() state.
					thenGraph = AbstractLearnerGraph.generateNewCmpVertex(result.nextID(true), result.config);
					Map<String,CmpVertex> row = result.createNewRow();
					extraRows.put(thenGraph, row);
					for(String rejectInput:labelsRejected)
					{
						CmpVertex rejectState = AbstractLearnerGraph.generateNewCmpVertex(result.nextID(false), result.config);
						rejectState.setAccept(false);
						extraRows.put(rejectState, result.createNewRow());
						row.put(rejectInput,rejectState);
					}
					rejectInputsToRejectGraph.put(labelsRejected,thenGraph);
				}
				result.addToCompatibility(entry.getKey(), thenGraph, PAIRCOMPATIBILITY.THEN);
			}
		}
		result.transitionMatrix.putAll(extraRows);
		return result;
	}
	
	public static Collection<LearnerGraph> buildIfThenAutomata(Collection<String> ltl, LearnerGraph graph, Configuration config)
	{
		Collection<LearnerGraph> ifthenAutomata = new LinkedList<LearnerGraph>();
		LTL_to_ba converter = new LTL_to_ba(config);
		converter.ltlToBA(ltl, graph);
		try {
			LearnerGraph ltlAutomaton = Transform.ltlToIfThenAutomaton(converter.getLTLgraph().pathroutines.buildDeterministicGraph());
			ltlAutomaton.setName("LTL");
			ifthenAutomata.add(ltlAutomaton);
		} catch (IncompatibleStatesException e) {
			Helper.throwUnchecked("failed to construct an if-then automaton from ltl", e);
		}
		
		for(String property:ltl)
			if (property.startsWith(QSMTool.cmdIFTHENAUTOMATON))
			{
				String automatonAndName = property.substring(0, QSMTool.cmdIFTHENAUTOMATON.length()+1).trim();
				int endOfName = automatonAndName.indexOf(' ');
				if (endOfName < 1)
					throw new IllegalArgumentException("missing automata name from"+automatonAndName);
				LearnerGraph propertyAutomaton = new LearnerGraph(
						TestFSMAlgo.buildGraph(automatonAndName.substring(endOfName).trim(),automatonAndName.substring(0, endOfName).trim()),config);
				ifthenAutomata.add(propertyAutomaton);
			}
		return ifthenAutomata;
	}

	public static class TraversalStatistics
	{
		public final int Nx,Tx,matched;
		
		public TraversalStatistics(int argNX,int argTX,int argMatched)
		{
			Nx=argNX;Tx=argTX;matched=argMatched;
		}
	}
	
	/** Counts the number of transitions in common between the two graphs.
	 * The small graph should be contained in the large one. 
	 */
	public static TraversalStatistics countSharedTransitions(LearnerGraph big, LearnerGraph small)
	{
		CmpVertex stateBig = big.init, stateSmall = small.init;
		LearnerGraph transitionCounter = new LearnerGraph(big,big.config);
		Set<CmpVertex> loopsInBig = new TreeSet<CmpVertex>();// contains states with self-loops of big which are not self-loops of B 
		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
		int matchedTransitionCounter = 0;
		Set<StatePair> statesAddedToBoundary = new HashSet<StatePair>();
		currentExplorationBoundary.add(new StatePair(stateBig,stateSmall));statesAddedToBoundary.add(new StatePair(stateBig,stateSmall));

		while(!currentExplorationBoundary.isEmpty())
		{
			StatePair statePair = currentExplorationBoundary.remove();
			assert big.transitionMatrix.containsKey(statePair.firstElem) : "state "+statePair.firstElem+" is not known to the first graph";
			assert small.transitionMatrix.containsKey(statePair.secondElem) : "state "+statePair.secondElem+" is not known to the second graph";
			if (statePair.firstElem.isAccept() != statePair.secondElem.isAccept())
				throw new DifferentFSMException("states "+statePair.firstElem+" and " + statePair.secondElem+" have a different acceptance labelling between the machines");

			Map<String,CmpVertex> targetsBig = big.transitionMatrix.get(statePair.firstElem);
			Map<String,CmpVertex> targetsSmall = small.transitionMatrix.get(statePair.secondElem);
					
			for(Entry<String,CmpVertex> labelstate:targetsSmall.entrySet())
			{
				String label = labelstate.getKey();
				if (!targetsBig.containsKey(label))
					throw new IllegalArgumentException("small graph is not contained in the large one, from "+statePair+
							" unmatched transition "+label+" to (nothing_in_big,"+labelstate.getValue()+")");
				++matchedTransitionCounter;
				transitionCounter.transitionMatrix.get(statePair.firstElem).remove(label);
				CmpVertex nextSmall = labelstate.getValue();
				CmpVertex nextBig = targetsBig.get(label);
				if (nextBig.equals(statePair.firstElem) &&
						!nextSmall.equals(statePair.secondElem))
					loopsInBig.add(nextBig);
				
				StatePair nextPair = new StatePair(nextBig,nextSmall);

				if (!statesAddedToBoundary.contains(nextPair))
				{
					currentExplorationBoundary.offer(nextPair);
					statesAddedToBoundary.add(nextPair);
				}
			}
		}
		
		return new TraversalStatistics(transitionCounter.countEdges(),loopsInBig.size(),matchedTransitionCounter);
	}
	
	public static double QuanteKoschkeDifference(LearnerGraph A, LearnerGraph B)
	{
		LearnerGraph automaton = null;
		try {
			automaton = LearnerGraphND.UniteTransitionMatrices(new LearnerGraphND(A,A.config),B).pathroutines.buildDeterministicGraph();
		} catch (IncompatibleStatesException e) {
			Helper.throwUnchecked("failed to build a deterministic version of a union", e);
		}
		//Visualiser.updateFrame(A, B);
		boolean minimal = false;
		while(!minimal)
			try
			{
				minimal = false;
				//Visualiser.updateFrame(automaton, null);
				WMethod.computeWSet_reducedmemory(automaton);
				minimal = true;
			}
			catch(EquivalentStatesException ex)
			{
				System.out.println("merging "+ex.getA()+" and "+ex.getB());
				automaton = MergeStates.mergeAndDeterminize_general(automaton, new StatePair(ex.getA(),ex.getB()));
			}
		//Visualiser.updateFrame(automaton, null);
		TraversalStatistics UA= countSharedTransitions(automaton, A), UB = countSharedTransitions(automaton, B);
		double DuA = ((double)UA.Nx+UA.Tx)/(UA.matched+automaton.countEdges()),
			DuB = ((double)UB.Nx+UB.Tx)/(UB.matched+automaton.countEdges());
		
		return (DuA+DuB)/2;
	}
}
