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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Stack;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph.StatesToConsider;
import statechum.analysis.learning.linear.GDLearnerGraph;
import statechum.analysis.learning.linear.GDLearnerGraph.DetermineDiagonalAndRightHandSide;
import statechum.analysis.learning.linear.GDLearnerGraph.DetermineDiagonalAndRightHandSideInterface;
import statechum.analysis.learning.linear.GDLearnerGraph.HandleRow;
import statechum.analysis.learning.linear.GDLearnerGraph.StateBasedRandom;
import statechum.analysis.learning.rpnicore.LSolver;
import statechum.collections.ArrayMapWithSearch;
import statechum.collections.ArrayMapWithSearchPos;
import statechum.collections.HashMapWithSearch;

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

	
	/** Makes it possible to register callbacks for score computation. Currently used with Markov but should be useful for Weka. */ 
	public static interface ScoreComputationCallback
	{
		public void initComputation(LearnerGraph graph);
		public long overrideScoreComputation(PairScore p);
	}

	public static interface RedNodeSelectionProcedure extends ScoreComputationCallback
	{
		/** Given a graph, the current collection of red nodes and those not compatible with any current red nodes, this function is supposed to decide which of the blue nodes to promote to red.
		 * 
		 * @param coregraph graph to work with.
		 * @param reds nodes currently coloured red.
		 * @param tentativeRedNodes blue nodes that are subject to promotion.
		 * @return the node to promote.
		 */
		CmpVertex selectRedNode(LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes);
		
		/** Given a collection of pairs, it may happen that none of these pairs selected so far are valid mergers (compared to the reference automaton). 
		 * This means that all blue states mentioned in these pairs are not compatible with all the existing red states 
		 * and hence at least one of them should be considered red. This method is called whenever there are any red-blue pairs available in order to check whether such a problem actually occurred.
		 * Since the idea is to return a state to colour red, the prototype for this method is similar to the prototype for the {@link #selectRedNode}.
		 */
		CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, Collection<CmpVertex> reds, List<PairScore> pairs);
	}
	

	public Stack<PairScore> chooseStatePairs(RedNodeSelectionProcedure decisionProcedure)
	{
		coregraph.pairsAndScores.clear();
		if (decisionProcedure != null) decisionProcedure.initComputation(coregraph);
		Collection<CmpVertex> reds = new LinkedList<CmpVertex>();// was: new LinkedHashSet<CmpVertex>();
		for(CmpVertex v:coregraph.transitionMatrix.keySet())
			if (v.getColour() == JUConstants.RED)
				reds.add(v);

		Queue<CmpVertex> currentExplorationBoundary = new LinkedList<CmpVertex>();// FIFO queue
		Collection<CmpVertex> RedStatesFound = new LinkedList<CmpVertex>();
		
		do
		{
			RedStatesFound.clear();coregraph.pairsAndScores.clear();
			currentExplorationBoundary.addAll(reds);
			if (coregraph.additionalExplorationRoot != null) currentExplorationBoundary.addAll(coregraph.additionalExplorationRoot);
			while(!currentExplorationBoundary.isEmpty())
			{
				CmpVertex currentRed = currentExplorationBoundary.remove();
	
				for(Entry<Label,CmpVertex> BlueEntry:coregraph.transitionMatrix.get(currentRed).entrySet())
					if (BlueEntry.getValue().getColour() == null || 
							BlueEntry.getValue().getColour() == JUConstants.BLUE)
					{// the next vertex is not marked red, hence it has to become blue
						CmpVertex currentBlueState = BlueEntry.getValue();
						
						int numberOfCompatiblePairs = 0;
						for(CmpVertex oldRed:reds)
						{
							PairScore pair = obtainPair(currentBlueState,oldRed,decisionProcedure);
							if (pair.getScore() >= coregraph.config.getGeneralisationThreshold())
							{
								coregraph.pairsAndScores.add(pair);
								++numberOfCompatiblePairs;
								if (GlobalConfiguration.getConfiguration().isAssertEnabled() && coregraph.config.getDebugMode()) PathRoutines.checkPTAConsistency(coregraph, currentBlueState);
							}
						}
						
						if (numberOfCompatiblePairs == 0)
							RedStatesFound.add(currentBlueState);
							
						// This node is current a blue node and remains blue until I decide which of the currently potentially red nodes become red.
						currentBlueState.setColour(JUConstants.BLUE);
					}
			}
	
			// Now that we have a collection of all potentially red vertices, pick one to make red and then then check if others can remain blue.
			CmpVertex newRedNode = null;
			if (!RedStatesFound.isEmpty())
			{
				if (RedStatesFound.size() > 1 && decisionProcedure != null)
					newRedNode = decisionProcedure.selectRedNode(coregraph, reds, RedStatesFound);
				else
					newRedNode = RedStatesFound.iterator().next();
				// mark this blue node as red and rebuild a collection of blue and potentially red states. 
				newRedNode.setColour(JUConstants.RED);
				reds.add(newRedNode);
			}
			else
				if (!coregraph.pairsAndScores.isEmpty() && decisionProcedure != null)
				{// the pairs chosen so far might all be the wrong ones, hence we could attempt to avoid the disaster if we can do something clever and whoever registered a decision procedure is given a chance to do it.  
					newRedNode = decisionProcedure.resolvePotentialDeadEnd(coregraph, reds, coregraph.pairsAndScores);
					if (newRedNode != null)
					{
						newRedNode.setColour(JUConstants.RED);
						reds.add(newRedNode);RedStatesFound.add(newRedNode);
					}
				}
		}
		while(!RedStatesFound.isEmpty());

		return getSortedPairsAndScoresStackFromUnsorted();
	}		

	/** Used to sort the collection of pairs and scores and do the filtering if needed. */
	public Stack<PairScore>  getSortedPairsAndScoresStackFromUnsorted()
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
	
	public PairScore obtainPair(CmpVertex blue, CmpVertex red, ScoreComputationCallback scoreComputationOverride)
	{
		long computedScore = -1, compatibilityScore =-1;StatePair pairToComputeFrom = new StatePair(blue,red);
		if (coregraph.config.getLearnerScoreMode() == Configuration.ScoreMode.COMPATIBILITY)
		{
			computedScore = computePairCompatibilityScore(pairToComputeFrom);compatibilityScore=computedScore;
		}
		else		
		if (coregraph.config.getLearnerScoreMode() == Configuration.ScoreMode.GENERAL)
		{
			Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			computedScore = computePairCompatibilityScore_general(pairToComputeFrom,null,collectionOfVerticesToMerge);compatibilityScore=computedScore;
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
			
			if (coregraph.config.getLearnerScoreMode()==Configuration.ScoreMode.KTAILS)
				assert coregraph.learnerCache.maxScore > 0;
				//if (coregraph.learnerCache.maxScore < 0) coregraph.learnerCache.maxScore = coregraph.transitionMatrix.size()*coregraph.pathroutines.computeAlphabet().size();

			if (computedScore <= coregraph.learnerCache.maxScore &&
					coregraph.config.getLearnerScoreMode()==Configuration.ScoreMode.KTAILS)
					    computedScore = -1; 
			else
			if (GlobalConfiguration.getConfiguration().isAssertEnabled())
			{
				int compatScore = computePairCompatibilityScore(pairToComputeFrom);
				assert compatScore <= computedScore;
			}
		}

		if (blue.isAccept() && computedScore < coregraph.config.getRejectPositivePairsWithScoresLessThan())
			computedScore = -1;
		
		if (computedScore >= 0 && scoreComputationOverride != null)
			computedScore = scoreComputationOverride.overrideScoreComputation(new PairScore(blue,red,computedScore, compatibilityScore));
		return new PairScore(blue,red,computedScore, compatibilityScore);
	}

	public int computePairCompatibilityScore(StatePair origPair)
	{
		Map<CmpVertex,List<CmpVertex>> mergedVertices = coregraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,List<CmpVertex>>():
				new HashMapWithSearch<CmpVertex,List<CmpVertex>>(coregraph.getStateNumber());
		// for every vertex of the model, gives a set of PTA vertices which were joined to it, for those of them which lead to a new (PTA-only) state
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
	protected CmpVertex findNextRed(Map<CmpVertex,List<CmpVertex>> mergedVertices, CmpVertex r, Label input)
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
	public int computePairCompatibilityScore_internal(StatePair origPair,Map<CmpVertex,List<CmpVertex>> mergedVertices) 
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

			if (!AbstractLearnerGraph.checkCompatible(currentPair.getQ(),currentPair.getR(),coregraph.pairCompatibility))
				return -1;// incompatible states
			if (!redFromPta.booleanValue())
				++score;
			Map<Label,CmpVertex> targetBlue = coregraph.transitionMatrix.get(currentPair.getQ());

			for(Entry<Label,CmpVertex> blueEntry:targetBlue.entrySet())
			{
				CmpVertex nextRedState = coregraph.transitionMatrix.get(currentPair.getR()).get(blueEntry.getKey());
				if (nextRedState != null)
				{// both states can make a transition - this would be the case of "non-determinism" for Merge&Determinize
					boolean newRedFromPta = redFromPta;
					
					// PTA does not have loops, but the original automaton has
					// and one of those loops is not on the transition diagram, namely the one related to B=A
					if (nextRedState == origPair.getQ())
					{
						nextRedState = origPair.getR(); // emulates the new loop
						newRedFromPta = coregraph.config.getLearnerScoreMode() != Configuration.ScoreMode.COMPATIBILITY; // and since the original score computation algorithm cannot do this, we pretend to be unable to do this either
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

					if (coregraph.config.getScoreCompatibilityScoreComputationBugEmulation())
						redFromPta = newRedFromPta;
					StatePair nextStatePair = new StatePair(blueEntry.getValue(),nextRedState);
					currentExplorationBoundary.offer(nextStatePair);currentRedFromPta.offer(newRedFromPta);
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

	static class AMEquivalenceClassMergingDetails
	{
		int nextEquivalenceClass;
	}
	
	/** Merges the equivalence classes associated with the supplied pair.
	 * It is important to point out that classes merged can be incomplete, in that they will contain  
	 */
	private boolean mergePair(StatePair currentPair,Map<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> stateToEquivalenceClass, AMEquivalenceClassMergingDetails mergingDetails) throws IncompatibleStatesException
	{
		AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> firstClass = stateToEquivalenceClass.get(currentPair.firstElem);
		AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> secondClass= stateToEquivalenceClass.get(currentPair.secondElem);
		AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> equivalenceClass = null;

		boolean singleton = true;
		if (firstClass == null)
		{
			if (secondClass == null)
			{// a new pair has been discovered, populate from the current transition matrix.
				equivalenceClass = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(mergingDetails.nextEquivalenceClass++,coregraph);
				assert coregraph.transitionMatrix.containsKey(currentPair.firstElem) : " state "+currentPair.firstElem+" is not in the graph";
				assert coregraph.transitionMatrix.containsKey(currentPair.secondElem) : " state "+currentPair.firstElem+" is not in the graph";
				singleton &= equivalenceClass.mergeWith(currentPair.secondElem,coregraph.transitionMatrix.get(currentPair.secondElem).entrySet());
				singleton &= equivalenceClass.mergeWith(currentPair.firstElem,coregraph.transitionMatrix.get(currentPair.firstElem).entrySet());
				stateToEquivalenceClass.put(currentPair.firstElem,equivalenceClass);
				stateToEquivalenceClass.put(currentPair.secondElem,equivalenceClass);
			}
			else
			{// first is null, second is not, record first as a member of the equivalence class the second one belongs to.
				equivalenceClass = secondClass;
				singleton &= equivalenceClass.mergeWith(currentPair.firstElem,coregraph.transitionMatrix.get(currentPair.firstElem).entrySet());
				stateToEquivalenceClass.put(currentPair.firstElem,equivalenceClass);
			}
		}
		else
		{
			if (secondClass == null)
			{// second is null, first is not, record second as a member of the equivalence class the first one belongs to.
				equivalenceClass = firstClass;
				singleton &= equivalenceClass.mergeWith(currentPair.secondElem,coregraph.transitionMatrix.get(currentPair.secondElem).entrySet());
				stateToEquivalenceClass.put(currentPair.secondElem,equivalenceClass);
			}
			else
				if (firstClass.getNumber() != secondClass.getNumber())
				{
					// if the two are the same, we've seen this pair before - ignore this case.
					// Neither are null, hence it looks like we have to merge the two equivalent classes - doing this via inplace update
					// is tested by {@link TestRpniLearner#testPairCompatible_general_C()}
					equivalenceClass = firstClass;
					singleton &= equivalenceClass.mergeWith(secondClass);// merge equivalence classes
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
				else
				{
					// We've seen this one already and most likely successfully merged, but not necessarily explored all matching transitions 
					// (this case would only happen where there is a list of pairs to merge and we do the merging before exploring the matching 
					// transitions out of the merged equivalence classes).
					equivalenceClass = firstClass;singleton = false;
					assert firstClass == secondClass;// these should correspond to the same object if we have completed merging them.
				}
		}
		
		return singleton;
	}
	
	
	/** Similar to computePairCompatibilityScore_internal but can operate 
	 * on arbitrary graphs rather than just a graph and a PTA.
	 * 
	 *  @param pairToMerge pair to merge
	 *  @param pairsToMerge more pairs to merge in the process of computation. Can be null.
	 *  @param mergedVertices collection of sets of merged vertices. Singleton sets reflect those which were not merged with any other.
	 */ 
	public int computePairCompatibilityScore_general(StatePair pairToMerge, Collection<StatePair> pairsToMerge, Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices) 
	{
		int score=-1;
		
		AMEquivalenceClassMergingDetails mergingDetails = new AMEquivalenceClassMergingDetails();mergingDetails.nextEquivalenceClass = 0;
		Map<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> stateToEquivalenceClass = 
				coregraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
						new ArrayMapWithSearch<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>():
				new HashMapWithSearch<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>(5);// these are going to be small sets, no point creating really big ones.
		boolean compatible = true;
		Queue<AMEquivalenceClass<CmpVertex, LearnerGraphCachedData>> currentExplorationBoundary = new LinkedList<AMEquivalenceClass<CmpVertex, LearnerGraphCachedData>>();// FIFO queue containing pairs to be explored
		//ArrayList<AMEquivalenceClass<CmpVertex, LearnerGraphCachedData>> setOfEquivalenceClassesOnStack = new ArrayList<AMEquivalenceClass<CmpVertex, LearnerGraphCachedData>>();
		ArrayMapWithSearchPos<AMEquivalenceClass<CmpVertex, LearnerGraphCachedData>, AMEquivalenceClass<CmpVertex, LearnerGraphCachedData>> setOfEquivalenceClassesOnStack  =
				new ArrayMapWithSearchPos<AMEquivalenceClass<CmpVertex, LearnerGraphCachedData>, AMEquivalenceClass<CmpVertex, LearnerGraphCachedData>>();
		try
		{
			if (pairToMerge != null) 
			{
				if (!mergePair(pairToMerge,stateToEquivalenceClass,mergingDetails))
				{
					AMEquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = stateToEquivalenceClass.get(pairToMerge.firstElem);
					currentExplorationBoundary.add(eqClass);// in order to explore matching transitions
					setOfEquivalenceClassesOnStack.put(eqClass, eqClass);
				}
			}
			if (pairsToMerge != null)
				for(StatePair pair:pairsToMerge)
				{
					if (!mergePair(pair,stateToEquivalenceClass,mergingDetails))
					{// add pairs one after one to avoid creating a huge stack of pairs.
						AMEquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = stateToEquivalenceClass.get(pair.firstElem);
						currentExplorationBoundary.add(eqClass);// in order to explore matching transitions
						setOfEquivalenceClassesOnStack.put(eqClass, eqClass);
					}
				}
			
			while(!currentExplorationBoundary.isEmpty())
			{
				AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> equivalenceClass = currentExplorationBoundary.remove();setOfEquivalenceClassesOnStack.remove(equivalenceClass);
				
				// We reconsider every equivalence class that has outgoing transitions with the same labels leading to different equivalence classes.  
				// Note there may be still pairs from the past which may have originally 
				// belonged to the two classes which got subsequently merged. These pairs 
				// will remain on the exploration stack and will be ignored below.				
				
				// Now we have a single equivalence class in the form of a list of <label,next vertex> entries, explore all matching transitions,
				// which correspond to sequences where the first element is the same.
				boolean singleton = true;
				do
				{
					singleton = true;
					for(Label lbl:equivalenceClass.getOutgoing().keySet().toArray(new Label[0]))
					{
						ArrayList<CmpVertex> targets = equivalenceClass.getOutgoing().get(lbl);
						CmpVertex firstVertex = targets.get(0);
						if (targets.size() > 1)
						{
							singleton = false;/*
							AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> firstEquivalenceClass = stateToEquivalenceClass.get(firstVertex);
							int firstEqNumber = firstEquivalenceClass.getNumber();*/
							int i=1;
							while(i<targets.size()) // here we benefit from the ability to iterate over a collection that may be updated as we iterate through it.
							{
								CmpVertex target = targets.get(i);
								if (!mergePair(new StatePair(firstVertex,target), stateToEquivalenceClass,mergingDetails))
								{
									AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> firstEquivalenceClass = stateToEquivalenceClass.get(firstVertex);
									if (setOfEquivalenceClassesOnStack.get(firstEquivalenceClass) == null)
									{// if a merge added something and the equivalence class is not already on the stack, add it.
										currentExplorationBoundary.offer(firstEquivalenceClass);// this may cause elements to be added to the collection of transitions in the considered equivalence classes and possibly even to the component of it denoted by targets.
										setOfEquivalenceClassesOnStack.put(firstEquivalenceClass, firstEquivalenceClass);
									}
								}
								++i;
							}
							
							// at this point, we are finished merging all in the targets set, clear it
							targets.clear();targets.add(firstVertex);// all equivalence classes are merged into that which the first vertex belongs to.
						}
					}
				} while(!singleton);// typically, we would have cleared all elements of the pending sets, however it may be that our mergers would 
					// introduce new elements to them. This is why we have to undergo a completely empty pass to confirm that it is all done and we 
					// need to do no more work for this equivalence class at this stage. We may need to revisit it in future, however.
			}
		} catch (IncompatibleStatesException e) {				
			compatible = false;// encountered incompatible states
		}
		assert !compatible || stateToEquivalenceClass.size() > 0 || (pairToMerge == null && (pairsToMerge == null || pairsToMerge.isEmpty()));
				
		if (compatible)
		{// merge successful - collect vertices from the equivalence classes
			mergedVertices.clear();
			for(CmpVertex vert:coregraph.transitionMatrix.keySet())
			{
				AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = stateToEquivalenceClass.get(vert);
				if (eqClass == null)
				{
					eqClass = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(mergingDetails.nextEquivalenceClass++,coregraph);
					try {
						eqClass.mergeWith(vert, coregraph.transitionMatrix.get(vert).entrySet());
					} catch (IncompatibleStatesException e) {
						assert false;// this should never happen because we are adding single states which cannot be incompatible to anything.
					}
					mergedVertices.add(eqClass);
				}
				else
				{// this is an existing equivalence class. 
				// After emptying the "frontline" stack, we are left with an empty setOfEquivalenceClassesOnStack because additions/removals from the stack are matched by the corresponding operations with setOfEquivalenceClassesOnStack.
				// We are hence at liberty to use setOfEquivalenceClassesOnStack to store equivalence classes that we have come across, in order to add only one entry to mergedVertices for each of them.
					if (setOfEquivalenceClassesOnStack.get(eqClass) == null)
					{
						setOfEquivalenceClassesOnStack.put(eqClass,eqClass);mergedVertices.add(eqClass);
					}
							
				}
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
	public long computeStateScore(StatePair pair)
	{
		if (!AbstractLearnerGraph.checkCompatible(pair.getR(),pair.getQ(),coregraph.pairCompatibility))
			return -1;

		long score = 0;
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
				Map<Label,CmpVertex> targetRed = coregraph.transitionMatrix.get(currentPair.getR()),
					targetBlue = coregraph.transitionMatrix.get(currentPair.getQ());
	
				for(Entry<Label,CmpVertex> redEntry:targetRed.entrySet())
				{
					CmpVertex nextBlueState = targetBlue.get(redEntry.getKey());
					if (nextBlueState != null)
					{// both states can make a transition
						if (!AbstractLearnerGraph.checkCompatible(redEntry.getValue(),nextBlueState,coregraph.pairCompatibility))
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
	 * @param randomWalkGenerator random number generator to be used in walk generation.
	 */
	public void chooseStatePairs_internal(double threshold, double scale, int ThreadNumber, 
			final Class<? extends DetermineDiagonalAndRightHandSideInterface> ddrh, StatesToConsider filter, StateBasedRandom randomWalkGenerator)
	{
		GDLearnerGraph ndGraph = new GDLearnerGraph(coregraph, filter, false);
		switch(coregraph.config.getGdScoreComputationAlgorithm())
		{
		case SCORE_RANDOMPATHS:
		case SCORE_TESTSET:
			// build (1) deterministic machines for each state and (2) walks from each state. 
			ndGraph.computeWalkSequences(randomWalkGenerator, ThreadNumber);
			break;
		case SCORE_LINEAR:
			break;
		default:
			throw new IllegalArgumentException("computation algorithm "+coregraph.config.getGdScoreComputationAlgorithm()+" is not currently supported");
		}
		
		final int [] incompatiblePairs = new int[ndGraph.getStateNumber()*(ndGraph.getStateNumber()+1)/2];for(int i=0;i<incompatiblePairs.length;++i) incompatiblePairs[i]=GDLearnerGraph.PAIR_OK;
		final int pairsNumber = ndGraph.findIncompatiblePairs(incompatiblePairs,ThreadNumber);
		LSolver solver = ndGraph.buildMatrix_internal(incompatiblePairs, pairsNumber, ThreadNumber,ddrh);
		solver.solve(ThreadNumber);
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
	 * @param randomWalkGenerator random number generator to be used in walk generation.
	 * @return
	 */
	public Stack<PairScore> chooseStatePairs_filtered(double threshold, double scale, int ThreadNumber, 
			final Class<? extends DetermineDiagonalAndRightHandSideInterface> ddrh, StatesToConsider filter, StateBasedRandom randomWalkGenerator)
	{
		chooseStatePairs_internal(threshold, scale, ThreadNumber, ddrh, filter,randomWalkGenerator);
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
			final Class<? extends DetermineDiagonalAndRightHandSide> ddrh, final StatesToConsider filter,StateBasedRandom randomWalkGenerator)
	{
		chooseStatePairs_internal(threshold, scale, ThreadNumber, ddrh, filter,randomWalkGenerator);
		if (threshold <= 0)
		{
			List<HandleRow<CmpVertex>> handlerList = new LinkedList<HandleRow<CmpVertex>>();
			@SuppressWarnings("unchecked")
			final List<PairScore> resultsPerThread [] = new List[ThreadNumber];
			for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
			{
				resultsPerThread[threadCnt]=new LinkedList<PairScore>();
				handlerList.add(new HandleRow<CmpVertex>()
				{
					@Override
					public void init(@SuppressWarnings("unused") int threadNo) {
						// No per-thread initialisation is needed.
					}
	
					@Override
					public void handleEntry(Entry<CmpVertex, Map<Label, CmpVertex>> entryA, int threadNo) 
					{
						// Now iterate through states
						Iterator<Entry<CmpVertex,Map<Label,CmpVertex>>> stateB_It = coregraph.transitionMatrix.entrySet().iterator();
						while(stateB_It.hasNext())
						{
							Entry<CmpVertex,Map<Label,CmpVertex>> stateB = stateB_It.next();// stateB should not have been filtered out by construction of matrixInverse
							if (!filter.stateToConsider(entryA.getKey()) ||
									!filter.stateToConsider(stateB.getKey()))
							{
								int score = 0;

								if (!AbstractLearnerGraph.checkCompatible(stateB.getKey(),entryA.getKey(),coregraph.pairCompatibility)) score=GDLearnerGraph.PAIR_INCOMPATIBLE;

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
			// now collect the results of processing
			for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
				coregraph.pairsAndScores.addAll(resultsPerThread[threadCnt]);
		}
				
		return coregraph.pairscores.getSortedPairsAndScoresStackFromUnsorted();
	}
}
