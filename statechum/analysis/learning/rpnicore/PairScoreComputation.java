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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;

public class PairScoreComputation {
	final LearnerGraph coregraph;
	
	/** Associates this object to ComputeStateScores it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	PairScoreComputation(LearnerGraph computeStateScores) 
	{
		coregraph = computeStateScores;
	}

	public Stack<PairScore> chooseStatePairs()
	{
		coregraph.pairsAndScores.clear();
		Set<CmpVertex> reds = new LinkedHashSet<CmpVertex>();
		for(CmpVertex v:coregraph.transitionMatrix.keySet())
			if (v.getColour() == JUConstants.RED)
				reds.add(v);

		Queue<CmpVertex> currentExplorationBoundary = new LinkedList<CmpVertex>();// FIFO queue
		currentExplorationBoundary.addAll(reds);
		List<CmpVertex> BlueStatesConsideredSoFar = new LinkedList<CmpVertex>();
		while(!currentExplorationBoundary.isEmpty())
		{
			CmpVertex currentRed = currentExplorationBoundary.remove();
			for(Entry<String,CmpVertex> BlueEntry:coregraph.transitionMatrix.get(currentRed).entrySet())
				if (BlueEntry.getValue().getColour() == null || 
						BlueEntry.getValue().getColour() == JUConstants.BLUE)
				{// the next vertex is not marked red, hence it has to become blue
					CmpVertex currentBlueState = BlueEntry.getValue();
											
					int numberOfCompatiblePairs = 0;
					for(CmpVertex oldRed:reds)
					{
						PairScore pair = obtainPair(currentBlueState,oldRed);
						if (pair.getScore() >= coregraph.config.getGeneralisationThreshold())
						{
							coregraph.pairsAndScores.add(pair);
							++numberOfCompatiblePairs;
							if (LearnerGraph.testMode) PathRoutines.checkPTAConsistency(coregraph, currentBlueState);
						}
					}
					
					if (numberOfCompatiblePairs == 0)
					{// mark this blue node as red. 
						CmpVertex newRedNode = currentBlueState;
						newRedNode.setColour(JUConstants.RED);
						reds.add(newRedNode);currentExplorationBoundary.add(newRedNode);
						BlueStatesConsideredSoFar.remove(newRedNode);
						
						// All future blue nodes will use this revised set of red states; the fact that
						// it is added to the exploration boundary ensures that it is considered when looking for more blue states.
						// Note that previously-considered blue states were not compared to this one (because it was blue before),
						// however previously-introduced red were - we're using the up-to-date reds set above.
						// For this reason, all we have to do is iterate over the old blue states and compare them to the
						// current one; none of those states may become red as a consequence since they are not 
						// red already, i.e. there is an entry about them in PairsAndScores
						for(CmpVertex oldBlue:BlueStatesConsideredSoFar)
						{
							PairScore pair = obtainPair(oldBlue,newRedNode);
							if (pair.getScore() >= coregraph.config.getGeneralisationThreshold())
							{
								coregraph.pairsAndScores.add(pair);
								if (LearnerGraph.testMode) PathRoutines.checkPTAConsistency(coregraph, oldBlue);
							}
						}
					}
					else
					{// This node is a blue node and remains blue unlike the case above when it could become red.
						BlueStatesConsideredSoFar.add(BlueEntry.getValue());// add a blue one
						currentBlueState.setColour(JUConstants.BLUE);
					}							
				}
		}

		Collections.sort(coregraph.pairsAndScores);// there is no point maintaining a sorted collection as we go since a single quicksort at the end will do the job

		Stack<PairScore> result = new Stack<PairScore>();
		if (coregraph.config.getPairsMergedPerHypothesis() > 0)
		{
			int numberOfElements = Math.min(coregraph.pairsAndScores.size(),coregraph.config.getPairsMergedPerHypothesis());
			result.addAll(coregraph.pairsAndScores.subList(0, numberOfElements));
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
		else result.addAll(coregraph.pairsAndScores);

		return result;
	}		

	protected PairScore obtainPair(CmpVertex blue, CmpVertex red)
	{
		coregraph.buildCachedData();
		int computedScore = -1, compatibilityScore =-1;StatePair pairToComputeFrom = new StatePair(blue,red);
		if (coregraph.config.getLearnerScoreMode() == Configuration.ScoreMode.COMPATIBILITY)
		{
			computedScore = computePairCompatibilityScore(pairToComputeFrom);compatibilityScore=computedScore;
		}
		else
		{
			computedScore = coregraph.pairscores.computeStateScore(pairToComputeFrom);
			if (computedScore >= 0)
			{
				compatibilityScore=	computePairCompatibilityScore(pairToComputeFrom);
				if (compatibilityScore < 0)
					computedScore = -1;
			}
			if (LearnerGraph.testMode)
				assert computePairCompatibilityScore(pairToComputeFrom) <= computedScore;
		}
		
		return new PairScore(blue,red,computedScore, compatibilityScore);
	}

	public int computePairCompatibilityScore(StatePair origPair)
	{
		Map<CmpVertex,List<CmpVertex>> mergedVertices = new HashMap<CmpVertex,List<CmpVertex>>();// for every vertex of the model, gives a set of PTA vertices which were joined to it, for those of them which lead to a new (PTA-only) state
		// note that PTA states may easily be merged with other PTA states, in which case they will feature as keys of this set.
		return computePairCompatibilityScore_internal(origPair, mergedVertices);
	}
	
	
	/** Takes states associated with red in mergedVertices and finds a target state for a given input
	 * 
	 * @param mergedVertices vertices linked to r
	 * @param r the red state
	 * @param input the input to consider
	 * @return the target state, null if there is no transition with this input not only from r but also from all states associated to it
	 * using mergedVertices. 
	 */
	protected CmpVertex findNextRed(Map<CmpVertex,List<CmpVertex>> mergedVertices, CmpVertex r, String input)
	{
		CmpVertex target = null;
		List<CmpVertex> associatedVertices = mergedVertices.get(r);
		if (associatedVertices != null)
		{
			Iterator<CmpVertex> associatedIt = associatedVertices.iterator();
			while(associatedIt.hasNext() && target == null)
				target = coregraph.transitionMatrix.get(associatedIt.next()).get(input);
		}
		return target;
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
	protected int computePairCompatibilityScore_internal(StatePair origPair,Map<CmpVertex,List<CmpVertex>> mergedVertices) 
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
			
			if (currentPair.getQ().isAccept() != currentPair.getR().isAccept())
				return -1;// incompatible states
			if (!redFromPta.booleanValue())
				++score;
			Map<String,CmpVertex> targetBlue = coregraph.transitionMatrix.get(currentPair.getQ());

			for(Entry<String,CmpVertex> blueEntry:targetBlue.entrySet())
			{
				CmpVertex nextRedState = coregraph.transitionMatrix.get(currentPair.getR()).get(blueEntry.getKey());
				if (nextRedState != null)
				{// both states can make a transition - this would be the case of "non-determinism" for Merge&Determinize
					
					// PTA does not have loops, but the original automaton has
					// and one of those loops is not on the transition diagram, namely the one related to B=A
					if (nextRedState == origPair.getQ())
					{
						nextRedState = origPair.getR(); // emulates the new loop
						redFromPta = coregraph.config.getLearnerScoreMode() != Configuration.ScoreMode.COMPATIBILITY; // and since the original score computation algorithm cannot do this, we pretend to be unable to do this either
						// The problem is that since we effectively merge the
						// states at this point, a loop introduced by merging
						// adjacent states may suck many PTA states into it, 
						// so that two transitions which would not normally be
						// near each other will be merged. For this reason, it
						// is possible that our score computation will deliver
						// a higher value that the conventional matching 
						// (where in the considered situation we'll be 
						// matching PTA with itself and PTA may be sparse).
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
						currentExplorationBoundary.offer(nextStatePair);currentRedFromPta.offer(coregraph.config.getLearnerScoreMode() != Configuration.ScoreMode.COMPATIBILITY);// from now on, no increments to the score
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
				List<CmpVertex> redMerged = mergedVertices.get(currentPair.getR());
				if (redMerged == null)
				{
					redMerged = new LinkedList<CmpVertex>();mergedVertices.put(currentPair.getR(), redMerged);
				}
				redMerged.add(currentPair.getQ());
			}
		}
		return score;
	}

	/** Similar to computePairCompatibilityScore_internal but can operate 
	 * on arbitrary graphs rather than just a graph and a PTA.
	 * 
	 *  @param origPair the pair to compute a score of in this graph.
	 *  @param mergedVertices collection of sets of merged vertices. Singleton sets reflect those which were not merged with any other.
	 */ 
	public int computePairCompatibilityScore_general(StatePair origPair,Collection<Collection<CmpVertex>> mergedVertices) 
	{
		class EquivalenceClass implements Comparable<EquivalenceClass>
		{
			/** The list of outgoing transitions from this equivalence class. */ 
			private List<Entry<String,CmpVertex>> outgoingTransitions = new ArrayList<Entry<String,CmpVertex>>(3);
			
			/** The ID of this equivalence class - not really necessary but handy. */
			int ClassNumber;
			
			public EquivalenceClass(int number)
			{
				ClassNumber=number;
			}
			
			public int getNumber()
			{
				return ClassNumber;
			}
			
			/** Returns transitions leaving states contained in this equivalence class. */ 
			public List<Entry<String,CmpVertex>> getOutgoing()
			{
				return outgoingTransitions;
			}
			
			/** Adds transitions from the supplied collection.
			 * 
			 * @param from transitions to add from.
			 */
			public void addFrom(Collection<Entry<String,CmpVertex>> from)
			{
				outgoingTransitions.addAll(from);
			}
			
			/** Adds the contents of the supplied argument to outgoing transitions 
			 * of this class and makes the supplied class equal to this class.
			 * 
			 * @param to
			 */
			public void setTo(EquivalenceClass to)
			{
				outgoingTransitions.addAll(to.outgoingTransitions);
				to.outgoingTransitions=outgoingTransitions;to.ClassNumber=ClassNumber;
			}

			public int compareTo(EquivalenceClass o) {
				return ClassNumber - o.ClassNumber;
			}

			/* (non-Javadoc)
			 * @see java.lang.Object#hashCode()
			 */
			@Override
			public int hashCode() {
				final int prime = 31;
				int result = 1;
				result = prime * result + ClassNumber;
				return result;
			}

			/* (non-Javadoc)
			 * @see java.lang.Object#equals(java.lang.Object)
			 */
			@Override
			public boolean equals(Object obj) {
				if (this == obj)
					return true;
				if (obj == null)
					return false;
				if (!(obj instanceof EquivalenceClass))
					return false;
				final EquivalenceClass other = (EquivalenceClass) obj;
				if (ClassNumber != other.ClassNumber)
					return false;
				return true;
			}
			
			public String toString()
			{
				return "[ "+ClassNumber+" -> "+outgoingTransitions+" ]";
			}
		}
		
		if (mergedVertices != null)	mergedVertices.clear();
		int equivalenceClassNumber = 0;
		Map<CmpVertex,EquivalenceClass> stateToEquivalenceClass = new TreeMap<CmpVertex,EquivalenceClass>();
		int score = 0;// compatibility score between states in the pair
		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue containing pairs to be explored
		currentExplorationBoundary.add(origPair);
		
		while(!currentExplorationBoundary.isEmpty())
		{
			StatePair currentPair = currentExplorationBoundary.remove();
			if (currentPair.firstElem.isAccept() != currentPair.secondElem.isAccept())
			{
				score = -1;
				break;// incompatibility
			}
			EquivalenceClass firstClass = stateToEquivalenceClass.get(currentPair.firstElem);
			EquivalenceClass secondClass= stateToEquivalenceClass.get(currentPair.secondElem);
			EquivalenceClass equivalenceClass = null;
			if (firstClass == null)
			{
				if (secondClass == null)
				{// a new pair has been discovered, populate from the current transition matrix.
					equivalenceClass = new EquivalenceClass(equivalenceClassNumber++);
					equivalenceClass.addFrom(coregraph.transitionMatrix.get(currentPair.firstElem).entrySet());
					equivalenceClass.addFrom(coregraph.transitionMatrix.get(currentPair.secondElem).entrySet());
					stateToEquivalenceClass.put(currentPair.firstElem,equivalenceClass);
					stateToEquivalenceClass.put(currentPair.secondElem,equivalenceClass);
				}
				else
				{// first is null, second is not, record first as a member of the equivalence class the second one belongs to.
					equivalenceClass = secondClass;
					equivalenceClass.addFrom(coregraph.transitionMatrix.get(currentPair.firstElem).entrySet());
					stateToEquivalenceClass.put(currentPair.firstElem,equivalenceClass);
				}
			}
			else
			{
				if (secondClass == null)
				{// second is null, first is not, record second as a member of the equivalence class the first one belongs to.
					equivalenceClass = firstClass;
					equivalenceClass.addFrom(coregraph.transitionMatrix.get(currentPair.secondElem).entrySet());
					stateToEquivalenceClass.put(currentPair.secondElem,equivalenceClass);
				}
				else
					if (firstClass != secondClass)
					{
						// if the two are the same, we've seen this pair before - ignore this case
						// neither are null, hence it looks like we have to merge the two equivalent classes - doing this via inplace update
						// TODO: to write a test to explore this case
						equivalenceClass = firstClass;
						equivalenceClass.setTo(secondClass);// merge equivalence classes
					}
			}

			
			// We reconsider every equivalence class which has changed. However, there may be still
			// pairs from the past which may have originally belonged to the two classes which 
			// got merged an considered early. Since the equivalence class did not change, we 
			// do not need to consider it here.
			if (equivalenceClass != null)
			{
				// Now we have a single equivalence class in the form of a list of string-next vertex entries, explore all matching transitions.
				ListIterator<Entry<String,CmpVertex>> firstTransitionIter = equivalenceClass.getOutgoing().listIterator();
				while(firstTransitionIter.hasNext())
				{// quadratic-time search is not nice, but I'd like to ensure that even if I end up with more than two
					// transitions from the same equivalence class with the same label, I can handle this case.
					Entry<String,CmpVertex> firstTransition=firstTransitionIter.next();
					ListIterator<Entry<String,CmpVertex>> secondTransitionIter = equivalenceClass.getOutgoing().listIterator(firstTransitionIter.nextIndex());
					while(secondTransitionIter.hasNext())
					{
						Entry<String,CmpVertex> secondTransition = secondTransitionIter.next();
						if (firstTransition.getKey().equals(secondTransition.getKey()))
						{
							EquivalenceClass fClass = stateToEquivalenceClass.get(firstTransition.getValue());
							EquivalenceClass sClass= stateToEquivalenceClass.get(secondTransition.getValue());
							if (fClass == null || sClass == null || !fClass.equals(sClass))
							{// this is the case when a pair of states will have to be merged.
								StatePair nextPair = new StatePair(firstTransition.getValue(),secondTransition.getValue());
								currentExplorationBoundary.offer(nextPair);
								++score;// every matched pair increments the score.
							}
						}
					}
				}
			}
		}
		
		assert stateToEquivalenceClass.size() > 0;
		//System.out.println(stateToEquivalenceClass);
		if (score >=0 && mergedVertices != null)
		{// merge successful - collect vertices from the equivalence classes
			Map<Integer,Collection<CmpVertex>> classToVertices = new TreeMap<Integer,Collection<CmpVertex>>();
			for(Entry<CmpVertex,EquivalenceClass> entry:stateToEquivalenceClass.entrySet())
			{
				int eqClass = entry.getValue().getNumber();Collection<CmpVertex> merged = classToVertices.get(eqClass);
				if (merged == null)
				{
					merged = new LinkedList<CmpVertex>();classToVertices.put(eqClass, merged);
				}
				merged.add(entry.getKey());
			}
			mergedVertices.addAll(classToVertices.values());
			Collection<CmpVertex> verticesNotMerged = new HashSet<CmpVertex>();verticesNotMerged.addAll(coregraph.transitionMatrix.keySet());
			verticesNotMerged.removeAll(stateToEquivalenceClass.keySet());
			for(CmpVertex vert:verticesNotMerged)
				mergedVertices.add(Arrays.asList(new CmpVertex[]{vert}));
		}
		
		return score;
	}
	
	/** Computes scores by navigating a cross-product of this machine, with itself.
	 * 
	 *  @param the pair to compute a score for
	 *  @return the resulting score, reflecting compatibility.
	 */
	public int computeStateScore(StatePair pair)
	{
		if (pair.getR().isAccept() != pair.getQ().isAccept())
			return -1;

		int score = 0;
		int currentExplorationDepth=1;
		assert pair.getQ() != pair.getR();
		boolean foundKTail = false;
		
		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
		currentExplorationBoundary.add(pair);currentExplorationBoundary.offer(null);
		
		while(!foundKTail)
		{
			StatePair currentPair = currentExplorationBoundary.remove();
			if (currentPair == null)
			{// we got to the end of a wave
				if (currentExplorationBoundary.isEmpty())
					break;// we are at the end of the last wave, stop looping.

				// mark the end of a wave.
				currentExplorationBoundary.offer(null);currentExplorationDepth++;
			}
			else
			{
				Map<String,CmpVertex> targetRed = coregraph.transitionMatrix.get(currentPair.getR()),
					targetBlue = coregraph.transitionMatrix.get(currentPair.getQ());
	
				for(Entry<String,CmpVertex> redEntry:targetRed.entrySet())
				{
					CmpVertex nextBlueState = targetBlue.get(redEntry.getKey());
					if (nextBlueState != null)
					{// both states can make a transition
						if (redEntry.getValue().isAccept() != nextBlueState.isAccept())
							return -1;// incompatible states
						
						if (coregraph.config.getLearnerScoreMode() == Configuration.ScoreMode.KTAILS &&
								currentExplorationDepth >= coregraph.config.getKlimit())
						{
							foundKTail = true;
							break;// we found a path of the "currentExplorationDepth" length and using the KTAILS method, hence stop the loop.
						}

						++score;
	
						StatePair nextStatePair = new StatePair(nextBlueState,redEntry.getValue());
						currentExplorationBoundary.offer(nextStatePair);
					}
					// if the red can make a move, but the blue one cannot, ignore this case.
				}
			}
		}
		
		if (coregraph.config.isBumpPositives() && pair.getQ().isAccept())
			score++;// bumpPositives is used to give an extra weight to state pairs which are both compatible and positive (i.e. discourage reject-reject state pairs).
		
		if (foundKTail)
		{// If we are operating in a k-tails mode, report a very high number if we found a k-tail.
			coregraph.buildCachedData();
			score = coregraph.learnerCache.maxScore+1;
		}
		return score;
	}
}
