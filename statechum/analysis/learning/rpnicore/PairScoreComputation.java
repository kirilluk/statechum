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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph.StatesToConsider;
import statechum.analysis.learning.rpnicore.GDLearnerGraph.DetermineDiagonalAndRightHandSide;
import statechum.analysis.learning.rpnicore.GDLearnerGraph.HandleRow;

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
							if (GlobalConfiguration.getConfiguration().isAssertEnabled()) PathRoutines.checkPTAConsistency(coregraph, currentBlueState);
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
								if (GlobalConfiguration.getConfiguration().isAssertEnabled()) PathRoutines.checkPTAConsistency(coregraph, oldBlue);
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

		return getSortedPairsAndScoresStackFromUnsorted();
	}		

	/** Used to sort the collection of pairs and scores and do the filtering if needed. */
	Stack<PairScore>  getSortedPairsAndScoresStackFromUnsorted()
	{
		Collections.sort(coregraph.pairsAndScores);// there is no point maintaining a sorted collection as we go since a single quicksort at the end will do the job

		Stack<PairScore> result = new Stack<PairScore>();
		if (coregraph.config.getPairsMergedPerHypothesis() > 0)
		{
			int numberOfElements = Math.min(coregraph.pairsAndScores.size(),coregraph.config.getPairsMergedPerHypothesis());
			result.addAll(coregraph.pairsAndScores.subList(0, numberOfElements));
		}
		else result.addAll(coregraph.pairsAndScores);

		return result;		
	}
	
	protected PairScore obtainPair(CmpVertex blue, CmpVertex red)
	{
		if (coregraph.learnerCache.maxScore < 0) coregraph.learnerCache.maxScore = coregraph.transitionMatrix.size()*coregraph.pathroutines.computeAlphabet().size();
		int computedScore = -1, compatibilityScore =-1;StatePair pairToComputeFrom = new StatePair(blue,red);
		if (coregraph.config.getLearnerScoreMode() == Configuration.ScoreMode.COMPATIBILITY)
		{
			computedScore = computePairCompatibilityScore(pairToComputeFrom);compatibilityScore=computedScore;
		}
		else		
		if (coregraph.config.getLearnerScoreMode() == Configuration.ScoreMode.GENERAL)
		{
			LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			computedScore = computePairCompatibilityScore_general(pairToComputeFrom, collectionOfVerticesToMerge);compatibilityScore=computedScore;
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
			if (computedScore <= coregraph.learnerCache.maxScore &&
					coregraph.config.getLearnerScoreMode()==Configuration.ScoreMode.KTAILS)
					    computedScore = -1; 
			if (GlobalConfiguration.getConfiguration().isAssertEnabled())
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

			if (!AbstractLearnerGraph.checkCompatible(currentPair.getQ(),currentPair.getR(),coregraph.incompatibles))
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

	static public class StringVertexPair extends Pair<String,CmpVertex> implements Comparable<StringVertexPair>
	{
		public StringVertexPair(String st,CmpVertex v)
		{
			super(st,v);
		}
		
		public int compareTo(StringVertexPair o) {
			//return firstElem.compareTo(o.firstElem);
			return super.compareTo(o);
		}
		
		@Override
		public String toString()
		{
			return "["+firstElem+"->"+secondElem+"]";
		}
	}

	/** Similar to computePairCompatibilityScore_internal but can operate 
	 * on arbitrary graphs rather than just a graph and a PTA.
	 * 
	 *  @param origPair the pair to compute a score of in this graph.
	 *  @param mergedVertices collection of sets of merged vertices. Singleton sets reflect those which were not merged with any other.
	 */ 
	public int computePairCompatibilityScore_general(StatePair origPair,Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices) 
	{
		mergedVertices.clear();
		int equivalenceClassNumber = 0;
		Map<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> stateToEquivalenceClass = new TreeMap<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		int score = 0;// compatibility score between states in the pair
		
		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue containing pairs to be explored
		currentExplorationBoundary.add(origPair);
		while(!currentExplorationBoundary.isEmpty())
		{
			StatePair currentPair = currentExplorationBoundary.remove();


			AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> firstClass = stateToEquivalenceClass.get(currentPair.firstElem);
			AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> secondClass= stateToEquivalenceClass.get(currentPair.secondElem);
			AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> equivalenceClass = null;
			
			try
			{
				if (firstClass == null)
				{
					if (secondClass == null)
					{// a new pair has been discovered, populate from the current transition matrix.
						equivalenceClass = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(equivalenceClassNumber++,coregraph);
						assert coregraph.transitionMatrix.containsKey(currentPair.firstElem) : " state "+currentPair.firstElem+" is not in the graph";
						assert coregraph.transitionMatrix.containsKey(currentPair.secondElem) : " state "+currentPair.firstElem+" is not in the graph";
						equivalenceClass.addFrom(currentPair.secondElem,coregraph.transitionMatrix.get(currentPair.secondElem).entrySet());
						equivalenceClass.mergeWith(currentPair.firstElem,coregraph.transitionMatrix.get(currentPair.firstElem).entrySet());
						stateToEquivalenceClass.put(currentPair.firstElem,equivalenceClass);
						stateToEquivalenceClass.put(currentPair.secondElem,equivalenceClass);
					}
					else
					{// first is null, second is not, record first as a member of the equivalence class the second one belongs to.
						equivalenceClass = secondClass;
						equivalenceClass.mergeWith(currentPair.firstElem,coregraph.transitionMatrix.get(currentPair.firstElem).entrySet());
						stateToEquivalenceClass.put(currentPair.firstElem,equivalenceClass);
					}
				}
				else
				{
					if (secondClass == null)
					{// second is null, first is not, record second as a member of the equivalence class the first one belongs to.
						equivalenceClass = firstClass;
						equivalenceClass.mergeWith(currentPair.secondElem,coregraph.transitionMatrix.get(currentPair.secondElem).entrySet());
						stateToEquivalenceClass.put(currentPair.secondElem,equivalenceClass);
					}
					else
						if (firstClass.getNumber() != secondClass.getNumber())
						{
							// if the two are the same, we've seen this pair before - ignore this case
							// neither are null, hence it looks like we have to merge the two equivalent classes - doing this via inplace update
							// Tested by testPairCompatible_general_C()
							equivalenceClass = firstClass;
							equivalenceClass.mergeWith(secondClass);// merge equivalence classes
							// I cannot keep secondClass in the table because a number of states point to it.
							// Subsequently, I may wish to merge the first class with another one, but there is
							// nothing to suggest that the secondClass which I just merged in, has to be merged into
							// that "another one". For this reason, I keep a collection of all states in each 
							// equivalence class and remap stateToEquivalenceClass when equivalence classes are merged.
							
							for(CmpVertex vert:secondClass.getStates())
								stateToEquivalenceClass.put(vert,equivalenceClass);
							if (GlobalConfiguration.getConfiguration().isAssertEnabled())
								for(Entry<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> entry:stateToEquivalenceClass.entrySet())
									assert entry.getValue().getNumber() != secondClass.getNumber();
						}
				}
			} catch (IncompatibleStatesException e) {
				score = -1;break;// encountered incompatible states
			}

			//if (equivalenceClass != null)
			//	System.out.println("considering: "+equivalenceClass+ "("+equivalenceClass.getNumber()+") current: "+equivalenceClass.getOutgoing()+", new : "+equivalenceClass.getNewOutgoing());
			// We reconsider every equivalence class which has changed which is the case if equivalenceClass != null. 
			// Note there may be still pairs from the past which may have originally 
			// belonged to the two classes which got subsequently merged. These pairs 
			// will remain on the exploration stack and will be ignored below.
			if (equivalenceClass != null)
			{
				// Now we have a single equivalence class in the form of a list of <label,next vertex> entries, explore all matching transitions,
				// which correspond to sequences where the first element is the same (we are using sorted sets).
				Iterator<StringVertexPair> firstTransitionIter = equivalenceClass.getOutgoing().iterator();

				while(firstTransitionIter.hasNext())
				{
					StringVertexPair firstTransition=firstTransitionIter.next();
					AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> fClass = stateToEquivalenceClass.get (firstTransition.secondElem);

					// For each input, we need to consider every pair of next states; the loop below explores a triangle.
					// In a way, we merge all appropriate equivalence classes, pairwise. Might be better to do this
					// in one go. 
					// The idea of tailSet is to pick all input/target state pairs for the input we considering and inputs
					// lexicographically higher; once we got to the end of the collection of inputs,
					// !firstTransition.firstElem.equals(secondTransition.firstElem) line will stop the iteration.
					StringVertexPair firstInput = new StringVertexPair(firstTransition.firstElem,null);
					Iterator<StringVertexPair> secondTransitionIter = equivalenceClass.getNewOutgoing().tailSet(firstInput).iterator();
					while (secondTransitionIter.hasNext())
					{
						StringVertexPair secondTransition = secondTransitionIter.next();
						if (!firstTransition.firstElem.equals(secondTransition.firstElem))
							break;// go to the end of the sequence of outgoing transitions with the same label.
						//System.out.println("transition " + firstTransition.firstElem+" to "+new StatePair(firstTransition.secondElem,secondTransition.secondElem));
						AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> sClass = stateToEquivalenceClass.get(secondTransition.secondElem);
						if (fClass == null || sClass == null || !fClass.equals(sClass))
						{// this is the case when a pair of states will have to be merged.
							StatePair nextPair = new StatePair(firstTransition.secondElem,secondTransition.secondElem);
							currentExplorationBoundary.offer(nextPair);
							++score;// every matched pair increments the score.
						}
					}
				}
				equivalenceClass.populate();
			}
		}
		
		assert score < 0 || stateToEquivalenceClass.size() > 0;

		if (score >=0)
		{// merge successful - collect vertices from the equivalence classes
			mergedVertices.clear();Set<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedCollection = new TreeSet<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();mergedCollection.addAll(stateToEquivalenceClass.values());
			mergedVertices.addAll(mergedCollection);
			Collection<CmpVertex> verticesNotMerged = new HashSet<CmpVertex>();verticesNotMerged.addAll(coregraph.transitionMatrix.keySet());
			verticesNotMerged.removeAll(stateToEquivalenceClass.keySet());
			for(CmpVertex vert:verticesNotMerged)
			{
				AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(equivalenceClassNumber++,coregraph);
				try {
					eqClass.addFrom(vert, coregraph.transitionMatrix.get(vert).entrySet());
				} catch (IncompatibleStatesException e) {
					assert false;// this should never happen because we are adding single states which cannot be incompatible to anything.
				}
				mergedVertices.add(eqClass);
			}
			score=coregraph.transitionMatrix.size()-mergedVertices.size();
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
		if (!AbstractLearnerGraph.checkCompatible(pair.getR(),pair.getQ(),coregraph.incompatibles))
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
						if (!AbstractLearnerGraph.checkCompatible(redEntry.getValue(),nextBlueState,coregraph.incompatibles))
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
			if (coregraph.learnerCache.maxScore < 0) coregraph.learnerCache.maxScore = coregraph.transitionMatrix.size()*coregraph.pathroutines.computeAlphabet().size();
			score = coregraph.learnerCache.maxScore+1;
		}
		return score;
	}

	/** Computes a stack of states with scores over a given threshold, using Linear. 
	 * States which are filtered out by GDLearnerGraph's filter are ignored.
	 * The outcome is not sorted - this internal routine is used by 
	 * chooseStatePairs_filtered and chooseStatePairs.
	 * 
	 * @param threshold the threshold to use, prior to scaling.
	 * @param scale We are using floating-point numbers here but compatibility scores are integers, hence we scale them before truncating into integers.
	 * @param ThreadNumber the number of CPUs to use
	 * @param ddrh class to compute diagonal and right-hand side in state comparisons
	 * @param filter determines the states to filter out.
	 */
	public void chooseStatePairs_internal(double threshold, double scale, int ThreadNumber, 
			final Class<? extends DetermineDiagonalAndRightHandSide> ddrh, StatesToConsider filter)
	{
		GDLearnerGraph ndGraph = new GDLearnerGraph(coregraph, filter, false);
		final int [] incompatiblePairs = new int[ndGraph.getStateNumber()*(ndGraph.getStateNumber()+1)/2];for(int i=0;i<incompatiblePairs.length;++i) incompatiblePairs[i]=GDLearnerGraph.PAIR_OK;
		final int pairsNumber = ndGraph.findIncompatiblePairs(incompatiblePairs,ThreadNumber);
		LSolver solver = ndGraph.buildMatrix_internal(incompatiblePairs, pairsNumber, ThreadNumber,ddrh);
		solver.solve();
		solver.freeAllButResult();// deallocate memory before creating a large array.
		coregraph.pairsAndScores.clear();
		// now fill in the scores in the array.
		for(int i=0;i<incompatiblePairs.length;++i)
		{
			int index = incompatiblePairs[i];
			if (index >= 0) 
			{
				double value = solver.j_x[incompatiblePairs[i]];
				if (value > threshold) coregraph.pairsAndScores.add(ndGraph.getPairScore(i, (int)(scale*value), 0));
			}
			else // PAIR_INCOMPATIBLE
				if (threshold < GDLearnerGraph.PAIR_INCOMPATIBLE) coregraph.pairsAndScores.add(ndGraph.getPairScore(i, (int)(scale*GDLearnerGraph.PAIR_INCOMPATIBLE), 0));
		}
	}
	/** Returns a stack of states with scores over a given threshold, using Linear. 
	 * States which are filtered out by GDLearnerGraph's filter are ignored.
	 * 
	 * @param threshold the threshold to use, prior to scaling.
	 * @param scale We are using floating-point numbers here but compatibility scores are integers, hence we scale them before truncating into integers.
	 * @param ThreadNumber the number of CPUs to use
	 * @param ddrh class to compute diagonal and right-hand side in state comparisons
	 * @param filter determines the states to filter out.
	 * @return
	 */
	public Stack<PairScore> chooseStatePairs_filtered(double threshold, double scale, int ThreadNumber, 
			final Class<? extends DetermineDiagonalAndRightHandSide> ddrh, StatesToConsider filter)
	{
		chooseStatePairs_internal(threshold, scale, ThreadNumber, ddrh, filter);
		return coregraph.pairscores.getSortedPairsAndScoresStackFromUnsorted();
	}

	/** Returns a stack of states with scores over a given threshold, using Linear. 
	 * States which are filtered out by GDLearnerGraph's filter are initially ignored and subsequently 
	 * added. 
	 * 
	 * @param threshold the threshold to use, prior to scaling.
	 * @param scale We are using floating-point numbers here but compatibility scores are integers, hence we scale them before truncating into integers.
	 * @param ThreadNumber the number of CPUs to use
	 * @param ddrh class to compute diagonal and right-hand side in state comparisons
	 * @param filter determines the states to filter out at the initial stage; all state pairs which 
	 * were filtered out are subsequently appended to the end of the stack returned.
	 * @return
	 */
	public Stack<PairScore> chooseStatePairs(final double threshold, final double scale, final int ThreadNumber, 
			final Class<? extends DetermineDiagonalAndRightHandSide> ddrh, final StatesToConsider filter)
	{
		chooseStatePairs_internal(threshold, scale, ThreadNumber, ddrh, filter);
		if (threshold <= 0)
		{
			List<HandleRow<CmpVertex>> handlerList = new LinkedList<HandleRow<CmpVertex>>();
			final List<PairScore> resultsPerThread [] = new List[ThreadNumber];
			for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
			{
				resultsPerThread[threadCnt]=new LinkedList<PairScore>();
				handlerList.add(new HandleRow<CmpVertex>()
				{
					public void init(@SuppressWarnings("unused") int threadNo) {
						// No per-thread initialisation is needed.
					}
	
					public void handleEntry(Entry<CmpVertex, Map<String, CmpVertex>> entryA, int threadNo) 
					{
						// Now iterate through states
						Iterator<Entry<CmpVertex,Map<String,CmpVertex>>> stateB_It = coregraph.transitionMatrix.entrySet().iterator();
						while(stateB_It.hasNext())
						{
							Entry<CmpVertex,Map<String,CmpVertex>> stateB = stateB_It.next();// stateB should not have been filtered out by construction of matrixInverse
							if (!filter.stateToConsider(entryA.getKey()) ||
									!filter.stateToConsider(stateB.getKey()))
							{
								int score = 0;

								if (!AbstractLearnerGraph.checkCompatible(stateB.getKey(),entryA.getKey(),coregraph.incompatibles)) score=GDLearnerGraph.PAIR_INCOMPATIBLE;

								if (score>threshold)
									resultsPerThread[threadNo].add(new PairScore(entryA.getKey(),stateB.getKey(),(int)(scale*score),0));

								if (stateB.getKey().equals(entryA.getKey())) break; // we only process a triangular subset.
							}
						}// B-loop
					}
				});
			}
			GDLearnerGraph.performRowTasks(handlerList, ThreadNumber, coregraph.transitionMatrix,LearnerGraphND.ignoreNone,
					GDLearnerGraph.partitionWorkLoadTriangular(ThreadNumber,coregraph.transitionMatrix.size()));
			for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
				coregraph.pairsAndScores.addAll(resultsPerThread[threadCnt]);
		}
				
		return coregraph.pairscores.getSortedPairsAndScoresStackFromUnsorted();
	}
}
