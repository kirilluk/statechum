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

import java.util.*;
import java.util.Map.Entry;

import statechum.collections.HashMapWithSearch;
import statechum.*;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph.StatesToConsider;
import statechum.analysis.learning.linear.GDLearnerGraph;
import statechum.analysis.learning.linear.GDLearnerGraph.DetermineDiagonalAndRightHandSideInterface;
import statechum.analysis.learning.linear.GDLearnerGraph.HandleRow;
import statechum.analysis.learning.linear.GDLearnerGraph.StateBasedRandom;
import statechum.collections.ArrayMapWithSearchPos;
import statechum.collections.MapWithSearch;

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
	public interface ScoreComputationCallback
	{
		void initComputation(LearnerGraph graph);
		long overrideScoreComputation(PairScore p);
	}

	public interface RedNodeSelectionProcedure extends ScoreComputationCallback
	{
		/** Used in order to explore the surroundings of red states in graphs that are not built from a PTA. If it returns <em>null</em>, the default is used. 
		 * This is not a map in order to permit transitions with the same label to lead to multiple states.
		 * 
		 * @param currentRed supplied state
		 * @return a Map of labels to the following states.
		 */
		Collection<Entry<Label,CmpVertex>> getSurroundingTransitions(CmpVertex currentRed);
		
		/** Given a graph, the current collection of red nodes and those not compatible with any 
		 * current red nodes, this function is supposed to decide which of the blue nodes to promote to red.
		 * 
		 * @param coregraph graph to work with.
		 * @param reds nodes currently coloured red.
		 * @param tentativeRedNodes blue nodes that are subject to promotion.
		 * @return the node to promote.
		 */
		CmpVertex selectRedNode(LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes);
		
		/** Given a collection of pairs selected so far, it may happen that none of these 
		 * pairs are valid mergers (compared to the reference automaton). 
		 * This means that all blue states mentioned in these pairs are not compatible 
		 * with all the existing red states and hence at least one of them should be 
		 * considered red. This method is called whenever there are any red-blue pairs 
		 * available in order to check whether such a problem actually occurred.
		 * Since the idea is to return a state to colour red, the prototype for 
		 * this method is similar to the prototype for the {@link #selectRedNode}.
		 */
		CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, Collection<CmpVertex> reds, List<PairScore> pairs);
	}
	

	public Stack<PairScore> chooseStatePairs(RedNodeSelectionProcedure decisionProcedure)
	{
		coregraph.pairsAndScores.clear();
		if (decisionProcedure != null) decisionProcedure.initComputation(coregraph);
		Collection<CmpVertex> reds = new ArrayList<>();// was: new LinkedHashSet<CmpVertex>();
		for(CmpVertex v:coregraph.transitionMatrix.keySet())
			if (v.getColour() == JUConstants.RED)
				reds.add(v);
		//System.out.println("choose state pairs with "+reds.size()+" red states");
		
		Queue<CmpVertex> currentExplorationBoundary = new LinkedList<>();// FIFO queue
		Collection<CmpVertex> RedStatesFound = new ArrayList<>();
		
		do
		{
			RedStatesFound.clear();coregraph.pairsAndScores.clear();
			currentExplorationBoundary.addAll(reds);
			//System.out.println("iterating through loop with "+reds.size()+" red states");
			while(!currentExplorationBoundary.isEmpty())
			{
				CmpVertex currentRed = currentExplorationBoundary.remove();
	
				Collection<Entry<Label,CmpVertex>> surrounding = decisionProcedure == null?null:decisionProcedure.getSurroundingTransitions(currentRed);
				if (surrounding == null) surrounding = coregraph.transitionMatrix.get(currentRed).entrySet();
				for(Entry<Label,CmpVertex> BlueEntry:surrounding)
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
			CmpVertex newRedNode;
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
		return getSortedPairsAndScoresStackFromUnsorted(coregraph.pairsAndScores, coregraph.config);
	}
	
	/** Used to sort the collection of pairs and scores and do the filtering if needed. */
	public static Stack<PairScore>  getSortedPairsAndScoresStackFromUnsorted(ArrayList<PairScore> pairsAndScores, Configuration config)
	{
		Collections.sort(pairsAndScores);// there is no point maintaining a sorted collection as we go since a single quicksort at the end will do the job

		Stack<PairScore> result = new Stack<>();
		if (config.getPairsMergedPerHypothesis() > 0)
		{
			int numberOfElements = Math.min(pairsAndScores.size(),config.getPairsMergedPerHypothesis());
			result.addAll(pairsAndScores.subList(0, numberOfElements));
		}
		else result.addAll(pairsAndScores);

		return result;		
	}
	
	public PairScore obtainPair(CmpVertex blue, CmpVertex red, ScoreComputationCallback scoreComputationOverride)
	{
		long computedScore = -1, compatibilityScore =-1;StatePair pairToComputeFrom = new StatePair(blue,red);
		switch(coregraph.config.getLearnerScoreMode())
		{
			case ONLYOVERRIDE:
				computedScore = scoreComputationOverride.overrideScoreComputation(new PairScore(blue,red,0, 0));compatibilityScore=computedScore;
				return new PairScore(blue,red,computedScore, compatibilityScore);
			case COMPATIBILITY:
				computedScore = computePairCompatibilityScore(pairToComputeFrom);compatibilityScore=computedScore;
				break;
			case GENERAL:
			{
				Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
				computedScore = computePairCompatibilityScore_general(pairToComputeFrom,null,collectionOfVerticesToMerge, true);compatibilityScore=computedScore;
				break;
			}
			case GENERAL_NOFULLMERGE:
			{
				Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
				computedScore = computePairCompatibilityScore_general(pairToComputeFrom,null,collectionOfVerticesToMerge, false);compatibilityScore=computedScore;
				break;
			}
			case GENERAL_PLUS_NOFULLMERGE:
			{
				Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
				computedScore = computePairCompatibilityScore_general(pairToComputeFrom,null,collectionOfVerticesToMerge, false);compatibilityScore=computedScore;
	
				if (computedScore >= 0)
				{
					computedScore = 0;
					for(EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:collectionOfVerticesToMerge)
						if (eqClass.getRepresentative().isAccept())
							computedScore += eqClass.getStates().size()-1;					
				}
				break;
			}
			case KTAILS:
			{// computeStateScore cannot be used here because it will see that we want to do KTails and will not evaluate whether a merge is feasible, hence later causing an experiment to fail with "elements of the pair are incompatible"
				Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
				computedScore = computePairCompatibilityScore_general(pairToComputeFrom,null,collectionOfVerticesToMerge, false);
				if (computedScore >= 0)
					computedScore = coregraph.pairscores.computeStateScoreKTails(pairToComputeFrom,false);
				break;
			}
			case KTAILS_ANY:
			{// computeStateScore cannot be used here because it will see that we want to do KTails and will not evaluate whether a merge is feasible, hence later causing an experiment to fail with "elements of the pair are incompatible"
				Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
				computedScore = computePairCompatibilityScore_general(pairToComputeFrom,null,collectionOfVerticesToMerge, false);
				if (computedScore >= 0)
					computedScore = coregraph.pairscores.computeStateScoreKTails(pairToComputeFrom,true);
				break;
			}
			default:
				computedScore = coregraph.pairscores.computeStateScore(pairToComputeFrom);
				if (computedScore >= 0)
				{
					compatibilityScore=	computePairCompatibilityScore(pairToComputeFrom);
					if (compatibilityScore < 0)
						computedScore = -1;
				}
				
				else
				if (GlobalConfiguration.getConfiguration().isAssertEnabled())
				{
					int compatScore = computePairCompatibilityScore(pairToComputeFrom);
					assert compatScore <= computedScore;
				}
				break;
		}

		if (blue.isAccept() && computedScore < coregraph.config.getRejectPositivePairsWithScoresLessThan())
			computedScore = -1;

		if (computedScore >= 0 && scoreComputationOverride != null)
			computedScore = scoreComputationOverride.overrideScoreComputation(new PairScore(blue,red,computedScore, compatibilityScore));
		return new PairScore(blue,red,computedScore, compatibilityScore);
	}

	public int computePairCompatibilityScore(StatePair origPair)
	{
		Map<CmpVertex,List<CmpVertex>> mergedVertices = AbstractLearnerGraph.constructMap(coregraph.config,coregraph);
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
	public CmpVertex findNextRed(Map<CmpVertex,List<CmpVertex>> mergedVertices, CmpVertex r, Label input)
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
	 * @param origPair the pair to start resolving non-determinism with
	 * @param mergedVertices records which vertices have to be merged.
	 * @return a pair of states to be merged or null if the graph is deterministic.
	 */
	public int computePairCompatibilityScore_internal(StatePair origPair,Map<CmpVertex,List<CmpVertex>> mergedVertices) 
	{
		mergedVertices.clear();// for every vertex of the model, gives a set of PTA vertices which were joined to it, for those of them which lead to a new (PTA-only) state
			// note that PTA states may easily be merged with other PTA states, in which case they will feature as keys of this set.
		
		int score = -1;// compatibility score between states in the pair
		Queue<StatePair> currentExplorationBoundary = new LinkedList<>();// FIFO queue containing pairs to be explored
		Queue<Boolean> currentRedFromPta = new LinkedList<>();// FIFO queue containing true if the red node comes from a branch of a PTA which has been previously already merged into the machine
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
			if (!redFromPta)
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
				List<CmpVertex> redMerged = mergedVertices.computeIfAbsent(currentPair.getR(), k -> new LinkedList<>());
				redMerged.add(currentPair.getQ());
			}
		}
		return score;
	}

	public static class AMEquivalenceClassMergingDetails
	{
		public int nextEquivalenceClass;
	}
	
	/** Merges the equivalence classes associated with the supplied pair.
	 * It is important to point out that classes merged can be incomplete, in that they will contain  
	 */
	public boolean mergePair(StatePair currentPair,Map<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> stateToEquivalenceClass, AMEquivalenceClassMergingDetails mergingDetails) throws IncompatibleStatesException
	{
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> firstClass = stateToEquivalenceClass.get(currentPair.firstElem);
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> secondClass= stateToEquivalenceClass.get(currentPair.secondElem);
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> equivalenceClass;

		boolean singleton = true;
		if (firstClass == null)
		{
			if (secondClass == null)
			{// a new pair has been discovered, populate from the current transition matrix.
				equivalenceClass = new AMEquivalenceClass<>(mergingDetails.nextEquivalenceClass++, coregraph);
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
						for(Entry<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> entry:stateToEquivalenceClass.entrySet())
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
	 *  @param fullMergedVertices whether to add all states to equivalence classes, even for states that correspond to singleton equivalence classes. Makes everything slow but may be needed for question generation as well as for SMT.
	 */ 
	public int computePairCompatibilityScore_general(StatePair pairToMerge, Collection<StatePair> pairsToMerge,
													 Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices, boolean fullMergedVertices)
	{
		int score=-1;
		
		AMEquivalenceClassMergingDetails mergingDetails = new AMEquivalenceClassMergingDetails();mergingDetails.nextEquivalenceClass = 0;

		Map<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> stateToEquivalenceClass = 
				new HashMapWithSearch<VertID,CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>(coregraph.config.getMaxAcceptStateNumber()+coregraph.config.getMaxRejectStateNumber());// these are going to be small sets, no point creating really big ones.
		boolean compatible = true;
		Queue<EquivalenceClass<CmpVertex, LearnerGraphCachedData>> currentExplorationBoundary = new LinkedList<>();// FIFO queue containing pairs to be explored
		ArrayMapWithSearchPos<EquivalenceClass<CmpVertex, LearnerGraphCachedData>,EquivalenceClass<CmpVertex, LearnerGraphCachedData>, EquivalenceClass<CmpVertex, LearnerGraphCachedData>> setOfEquivalenceClassesOnStack  =
				new ArrayMapWithSearchPos<>();
		
		// For large graphs, there are usually few states participate, compared to graph size, I've seen at most 10% and most often it is a handful out of a possibly million vertices. Hence use TreeMap
		//Map<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> stateToEquivalenceClass = new TreeMap<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
/*
		Map<EquivalenceClass<CmpVertex, LearnerGraphCachedData>,EquivalenceClass<CmpVertex, LearnerGraphCachedData>>  setOfEquivalenceClassesOnStack = 
				new TreeMap<EquivalenceClass<CmpVertex, LearnerGraphCachedData>,EquivalenceClass<CmpVertex, LearnerGraphCachedData>>();
*/
		if (pairToMerge != null && !AbstractLearnerGraph.checkCompatible(pairToMerge.getQ(),pairToMerge.getR(),coregraph.pairCompatibility))
			return -1;// incompatible states, perform a quick bailout.

		try
		{
			if (pairToMerge != null) 
			{
				if (!mergePair(pairToMerge,stateToEquivalenceClass,mergingDetails))
				{
					EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = stateToEquivalenceClass.get(pairToMerge.firstElem);
					currentExplorationBoundary.add(eqClass);// in order to explore matching transitions
					setOfEquivalenceClassesOnStack.put(eqClass, eqClass);
				}
			}
			if (pairsToMerge != null)
				for(StatePair pair:pairsToMerge)
				{
					if (!mergePair(pair,stateToEquivalenceClass,mergingDetails))
					{// add pairs one after one to avoid creating a huge stack of pairs.
						EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = stateToEquivalenceClass.get(pair.firstElem);
						if (!setOfEquivalenceClassesOnStack.containsKey(eqClass))
						{
							currentExplorationBoundary.add(eqClass);// in order to explore matching transitions
							setOfEquivalenceClassesOnStack.put(eqClass, eqClass);
						}
					}
				}
			
			while(!currentExplorationBoundary.isEmpty())
			{
				EquivalenceClass<CmpVertex, LearnerGraphCachedData> equivalenceClass = currentExplorationBoundary.remove();setOfEquivalenceClassesOnStack.remove(equivalenceClass);
				
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
						Object targetsSlot = equivalenceClass.getOutgoing().get(lbl);
						
						if (!(targetsSlot instanceof CmpVertex) && ((List<CmpVertex>)targetsSlot).size() > 1) // checks if the collection is a singleton or not.
						{
							List<CmpVertex> targets = (List<CmpVertex>)targetsSlot;
							CmpVertex firstVertex = targets.get(0);
							
							EquivalenceClass<CmpVertex,LearnerGraphCachedData> firstEquivalenceClass = stateToEquivalenceClass.get(firstVertex);
							if (firstEquivalenceClass == null)
							{// the first outgoing transition is not associated to a known equivalence class.
								firstEquivalenceClass = new AMEquivalenceClass<>(mergingDetails.nextEquivalenceClass++, coregraph);
								firstEquivalenceClass.mergeWith(firstVertex, coregraph.transitionMatrix.get(firstVertex).entrySet());
								stateToEquivalenceClass.put(firstVertex, firstEquivalenceClass);
								currentExplorationBoundary.offer(firstEquivalenceClass);// this may cause elements to be added to the collection of transitions in the considered equivalence classes and possibly even to the component of it denoted by targets.
								setOfEquivalenceClassesOnStack.put(firstEquivalenceClass, firstEquivalenceClass);
							}
							
							int i=1;
							while(i<targets.size()) // here we benefit from the ability to iterate over a collection that may be updated as we iterate through it. Such an update will only add new vertices to the end and we'll iterate through them in due course.
							{
								CmpVertex target = targets.get(i);
								if (firstEquivalenceClass != stateToEquivalenceClass.get(target))
								{// only consider merging when not already merged. mergePair does not handle this case because it is also called during the initial construction
									
									if (!mergePair(new StatePair(firstVertex,target), stateToEquivalenceClass,mergingDetails)) // this is where the targets collection may get updated
									{// outcome not a singleton
										singleton = false;
										if (setOfEquivalenceClassesOnStack.get(firstEquivalenceClass) == null)
										{// if a merge added something and the equivalence class is not already on the stack, add it.
											currentExplorationBoundary.offer(firstEquivalenceClass);// this may cause elements to be added to the collection of transitions in the considered equivalence classes and possibly even to the component of it denoted by targets.
											setOfEquivalenceClassesOnStack.put(firstEquivalenceClass, firstEquivalenceClass);
										}
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
			if(fullMergedVertices)
			{
				for(CmpVertex vert:coregraph.transitionMatrix.keySet())
				{
					EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = stateToEquivalenceClass.get(vert);
					if (eqClass == null)
					{
						eqClass = new AMEquivalenceClass<>(mergingDetails.nextEquivalenceClass++, coregraph);
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
			else
			{
				score = 0;
				setOfEquivalenceClassesOnStack.clear();
				for(Entry<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> entry:stateToEquivalenceClass.entrySet())
					if (!setOfEquivalenceClassesOnStack.containsKey(entry.getValue()))
					{
						setOfEquivalenceClassesOnStack.put(entry.getValue(),entry.getValue());
						mergedVertices.add(entry.getValue());
						score+=entry.getValue().getStates().size()-1;
					}
			}				
		}

		return score;
	}
	
	/** Computes scores by navigating a cross-product of this machine, with itself.
	 * 
	 *  @param pair the pair to compute a score for
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
		
		Queue<StatePair> currentExplorationBoundary = new LinkedList<>();// FIFO queue
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
			if (coregraph.learnerCache.maxScore < 0) coregraph.learnerCache.maxScore = (long) coregraph.transitionMatrix.size() *coregraph.pathroutines.computeAlphabet().size();
			score = coregraph.learnerCache.maxScore+1;
		}
		return score;
	}

	/** Computes scores by navigating a cross-product of this machine, with itself. Implements the k-tails method.
	 * 
	 *  @param pair the pair to compute a score for
	 *  @param anyPath whether any path matching is good enough or all paths should match.
	 *  @return the resulting score, reflecting compatibility.
	 */
	public long computeStateScoreKTails(StatePair pair, boolean anyPath)
	{
		return LearningAlgorithms.computeStateScoreKTails(coregraph,pair,coregraph.config.getKlimit(), anyPath);
	}
	

	public long computeScoreSicco(StatePair pair, boolean recursive)
	{
		assert pair.getQ() != pair.getR();
		assert coregraph.transitionMatrix.containsKey(pair.firstElem);
		assert coregraph.transitionMatrix.containsKey(pair.secondElem);
		Map<CmpVertex,List<CmpVertex>> mergedVertices = AbstractLearnerGraph.constructMap(coregraph.config,coregraph);
		Configuration shallowCopy = coregraph.config.copy();shallowCopy.setLearnerCloneGraph(false);

		long pairScore = coregraph.pairscores.computePairCompatibilityScore_internal(pair,mergedVertices);
		if (pairScore < 0)
			return -1;

		Set<Label> inputsUsed = new HashSet<>();

		// I iterate over the elements of the original graph in order to be able to update the target one.
		for(Entry<CmpVertex,MapWithSearch<Label,Label,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
			if ( (recursive || entry.getKey() == pair.getR()) && entry.getKey().getColour() == JUConstants.RED)
			{// only checks for specific state of interest if we are supposed to be non-recursive.
				CmpVertex vert = entry.getKey();
				if (mergedVertices.containsKey(vert))
				{// there are some vertices to merge with this one.
					inputsUsed.clear();inputsUsed.addAll(entry.getValue().keySet());// the first entry is either a "derivative" of a red state or a branch of PTA into which we are now merging more states.
					for(CmpVertex toMerge:mergedVertices.get(vert))
					{// for every input, I'll have a unique target state - this is a feature of PTA
					 // For this reason, every if multiple branches of PTA get merged, there will be no loops or parallel edges.
					// As a consequence, it is safe to assume that each input/target state combination will lead to a new state
					// (as long as this combination is the one _not_ already present from the corresponding red state).
						for(Entry<Label,CmpVertex> input_and_target:coregraph.transitionMatrix.get(toMerge).entrySet())
							if (input_and_target.getValue().isAccept() && !inputsUsed.contains(input_and_target.getKey()))
								return -1;// where anything is added that is not part of the original set of inputs, block the merge.
					}
				}
			}

		return pairScore;
	}

	public enum SiccoGeneralScoring { S_ONEPAIR, S_RED }
	
	/** This is similar in spirit to Sicco score computation but capable of handling arbitrary state mergers. 
	 * The method does not intend to compute a positive score since it is expected to be used to reject incompatible ones and will return 0 if provided with an empty set of equivalence 
	 * classes (which means that computation of scores returned -1 and hence did not populate equivalence classes). 
	 * 
	 * In a similar way to ordinary Sicco score computation, there are three modes,
	 * <ul>
	 * <li>Only look at the current pair to merge and the states that got merged into it (requested with howToScore == SiccoGeneralScoring.S_ONEPAIR). </li>
	 * <li>Look at mergers of any state into a red state (if there are multiple red states being merged together, this will do a union of their outgoing transitions).</li>
	 * </ul>
	 * Unlike the score computation that relies on mergers between a branch of a tree and a graph, 
	 * this scoring routine cannot tell whether any node comes from a tree or from the main graph (except where they are labelled red or blue).
	 * This is why it cannot do an equivalent of 'recursive' computation where one follows a branch and checks states against those in the main graph. On the positive side, it can be used
	 * for arbitrary mergers in a graph, something that typical Sicco score computation cannot handle.
	 * 
	 * There is no provision for 'blue' states because during score computation, it is not known which states are going to be blue or will immediately become red.
	 * 
	 * @param pair a pair of states to be merged, only used for where checking is limited to the first pair. In reality it matches all red states that got merged into the one provided with all blue ones that got merged with those red ones.
	 * @param mergedVertices describes vertices that are to form clusters of new states. This does not have to be a total map: clusters of states not in the collection are ignored since they are assumed to be singletons.
	 * @param howToScore the scoring method to use.
	 * @return the (negative) number of transitions that will be new to the red part of the graph.
	 */
	public long computeSiccoRejectScoreGeneral(StatePair pair, Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices, SiccoGeneralScoring howToScore)
	{
		if (howToScore == SiccoGeneralScoring.S_ONEPAIR)
		{
			if (pair == null)
				throw new IllegalArgumentException("when looking for a score from a single pair, this pair should be passed as an argument");
		}
		
		long outcome = 0;
		Set<Label> outgoingRed = new TreeSet<>(), outgoingNew = new TreeSet<>();
		for(EquivalenceClass<CmpVertex,LearnerGraphCachedData> eq:mergedVertices)
			if (howToScore != SiccoGeneralScoring.S_ONEPAIR || eq.getStates().contains(pair.getR()))
			{
				outgoingRed.clear();outgoingNew.clear();
				
				if (howToScore == SiccoGeneralScoring.S_ONEPAIR)
				{
					if (!eq.getStates().contains(pair.getQ())) 
						throw new IllegalArgumentException("invalid merge: pair "+pair+ " should have been merged but states in the pair are in distinct equivalence classes");
				}
				boolean redFound = false;
				for(CmpVertex v:eq.getStates())
					if ( v.getColour() == JUConstants.RED )
					{// collects labels of all transitions from the RED states merged together. 
						outgoingRed.addAll(coregraph.transitionMatrix.get(v).keySet());
						redFound = true;
					}
				if (redFound) // only consider mergers into a red state, otherwise we do not know which is being merged into which.
					for(CmpVertex v:eq.getStates())
						if (v.getColour() != JUConstants.RED)
						{
							for(Map.Entry<Label,CmpVertex> entry:coregraph.transitionMatrix.get(v).entrySet())
								if (entry.getValue().isAccept() && !outgoingRed.contains(entry.getKey()) && !outgoingNew.contains(entry.getKey()))
								{// each outgoing label that is new has to be counted only once.
									--outcome;outgoingNew.add(entry.getKey());
								}
						}
			}
		
		return outcome;
	}

	/** Similar to {@link PairScoreComputation#computeSiccoRejectScoreGeneral(StatePair, Collection, SiccoGeneralScoring)} 
	 * but does not count the number of unmatched transitions returning -1 when the first one is met. 
	 */
	public long computeSiccoRejectScoreGeneral_fastreturn(StatePair pair, Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices, SiccoGeneralScoring howToScore)
	{
		if (howToScore == SiccoGeneralScoring.S_ONEPAIR)
		{
			if (pair == null)
				throw new IllegalArgumentException("when looking for a score from a single pair, this pair should be passed as an argument");
		}
		
		Set<Label> outgoingRed = new TreeSet<>();
		for(EquivalenceClass<CmpVertex,LearnerGraphCachedData> eq:mergedVertices)
			if (howToScore != SiccoGeneralScoring.S_ONEPAIR || eq.getStates().contains(pair.getR()))
			{
				outgoingRed.clear();
				
				if (howToScore == SiccoGeneralScoring.S_ONEPAIR)
				{
					if (!eq.getStates().contains(pair.getQ())) 
						throw new IllegalArgumentException("invalid merge: pair "+pair+ " should have been merged but states in the pair are in distinct equivalence classes");
				}
				boolean redFound = false;
				for(CmpVertex v:eq.getStates())
					if ( v.getColour() == JUConstants.RED )
					{// collects labels of all transitions from the RED states merged together. 
						outgoingRed.addAll(coregraph.transitionMatrix.get(v).keySet());
						redFound = true;
					}
				if (redFound) // only consider mergers into a red state, otherwise we do not know which is being merged into which.
					for(CmpVertex v:eq.getStates())
						if (v.getColour() != JUConstants.RED)
						{
							for(Map.Entry<Label,CmpVertex> entry:coregraph.transitionMatrix.get(v).entrySet())
								if (entry.getValue().isAccept() && !outgoingRed.contains(entry.getKey()))
								{// each outgoing label that is new has to be counted only once.
									return -1;
								}
						}
			}
		
		return 0;
	}

	public static class LinearScoring<TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>>
	{
		protected final AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> coregraph;
		protected final GDLearnerGraph ndGraph;
		protected final int [] pairToScore;
		protected final LSolver solver;
		protected final StatesToConsider filter;
		protected final int threadNumber;
		
		/** Permits one to compute scores of state using Linear. 
		 * States which are filtered out by GDLearnerGraph's filter are ignored.
		 * The outcome is not sorted - this internal routine is used by 
		 * chooseStatePairs_filtered and chooseStatePairs.
		 * 
		 * @param graph graph to use for score computation.
		 * @param ThreadNumber the number of CPUs to use
		 * @param ddrh class to compute diagonal and right-hand side in state comparisons
		 * @param filterToUse determines the states to filter out.
		 * @param randomWalkGenerator random number generator to be used in walk generation.
		 */
		public LinearScoring(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph,int ThreadNumber, 
				final Class<? extends DetermineDiagonalAndRightHandSideInterface> ddrh, final StatesToConsider filterToUse, StateBasedRandom randomWalkGenerator)
		{
			threadNumber = ThreadNumber;filter = filterToUse;coregraph = graph;
			ndGraph = new GDLearnerGraph(coregraph, filter, false);
			switch(coregraph.config.getGdScoreComputationAlgorithm())
			{
			case SCORE_RANDOMPATHS:
			case SCORE_TESTSET:
				// build (1) deterministic machines for each state and (2) walks from each state. 
				ndGraph.computeWalkSequences(randomWalkGenerator, threadNumber);
				break;
			case SCORE_LINEAR:
				break;
			default:
				throw new IllegalArgumentException("computation algorithm "+coregraph.config.getGdScoreComputationAlgorithm()+" is not currently supported");
			}
			
			pairToScore = new int[ndGraph.getStateNumber()*(ndGraph.getStateNumber()+1)/2];for(int i=0;i<pairToScore.length;++i) pairToScore[i]=GDLearnerGraph.PAIR_OK;
			final int pairsNumber = ndGraph.findIncompatiblePairs(pairToScore,threadNumber);
			solver = ndGraph.buildMatrix_internal(pairToScore, pairsNumber, threadNumber,ddrh);
			solver.solve(threadNumber);
			solver.freeAllButResult();// deallocate memory before creating a large array.
		}
		
		/** Computes a stack of states with scores over a given threshold, using Linear. 
		 * States which are filtered out by GDLearnerGraph's filter are ignored.
		 * The outcome is not sorted - this internal routine is used by 
		 * chooseStatePairs_filtered and chooseStatePairs.
		 * 
		 * @param threshold the threshold to use, prior to scaling.
		 * @param scale We are using floating-point numbers here but compatibility scores are integers, hence we scale them before truncating into integers.
		 */
		public ArrayList<PairScore>	chooseStatePairs_internal(final double threshold, final double scale)
		{
			ArrayList<PairScore> pairsAndScores = new ArrayList<>(pairToScore.length);

			List<HandleRow<List<CmpVertex>>> handlerList = new LinkedList<>();
			@SuppressWarnings("unchecked")
			final List<PairScore> resultsPerThread [] = new List[threadNumber];
			for(int threadCnt=0;threadCnt<threadNumber;++threadCnt)
			{
				resultsPerThread[threadCnt]= new LinkedList<>();
				handlerList.add(new HandleRow<List<CmpVertex>>()
				{
					@Override
					public void init(@SuppressWarnings("unused") int threadNo) {
						// No per-thread initialisation is needed.
					}

					@Override
					public void handleEntry(Entry<CmpVertex, MapWithSearch<Label,Label, List<CmpVertex>>> entryA, int threadNo) 
					{
						// Now iterate through states
						Iterator<Entry<CmpVertex,MapWithSearch<Label,Label,List<CmpVertex>>>> stateB_It = ndGraph.matrixForward.transitionMatrix.entrySet().iterator();
						while(stateB_It.hasNext())
						{
							Entry<CmpVertex,MapWithSearch<Label,Label,List<CmpVertex>>> stateB = stateB_It.next();// stateB should not have been filtered out by construction of matrixInverse
							int currentStatePair = pairToScore[ndGraph.vertexToIntNR(stateB.getKey(), entryA.getKey())];
							if (currentStatePair >= 0)
							{
								double score = solver.j_x[currentStatePair];

								if (score > threshold)
									resultsPerThread[threadNo].add(new PairScore(entryA.getKey(), stateB.getKey(), (int) (scale * score), 0));

							}
							else
								if (GDLearnerGraph.PAIR_INCOMPATIBLE > threshold)
									resultsPerThread[threadNo].add(new PairScore(entryA.getKey(),stateB.getKey(),(int)(scale*GDLearnerGraph.PAIR_INCOMPATIBLE),0));
							
							if (stateB.getKey().equals(entryA.getKey())) break; // we only process a triangular subset.
						}// B-loop
					}
				});
			}
			GDLearnerGraph.performRowTasks(handlerList, threadNumber, ndGraph.matrixForward.transitionMatrix,filter,
					GDLearnerGraph.partitionWorkLoadTriangular(threadNumber,ndGraph.matrixForward.transitionMatrix.size()));
			// now collect the results of processing
			for(int threadCnt=0;threadCnt<threadNumber;++threadCnt)
				pairsAndScores.addAll(resultsPerThread[threadCnt]);
			
			return pairsAndScores;
		}
		
		/** Returns a score for a provided pair of states. */
		public long scoreForPair(CmpVertex a, CmpVertex b,final double scale)
		{
			int currentStatePair = pairToScore[ndGraph.vertexToIntNR(a, b)];
			if (currentStatePair < 0)
				return -1;
			return (long)(solver.j_x[currentStatePair]*scale);
		}
		
		/** Returns a stack of states with scores over a given threshold, using Linear. 
		 * States which are filtered out by GDLearnerGraph's filter are ignored.
		 * 
		 * @param threshold the threshold to use, prior to scaling.
		 * @param scale We are using floating-point numbers here but compatibility scores are integers, hence we scale them before truncating into integers.
		 */
		public Stack<PairScore>
			chooseStatePairs_filtered(double threshold, double scale)
		{
			ArrayList<PairScore> pairAndScores = chooseStatePairs_internal(threshold, scale);
			return PairScoreComputation.getSortedPairsAndScoresStackFromUnsorted(pairAndScores,coregraph.config);
		}
		
		/** Returns a stack of states with scores over a given threshold, using Linear. 
		 * States which are filtered out by GDLearnerGraph's filter are initially ignored and subsequently 
		 * added. 
		 * 
		 * @param threshold the threshold to use, prior to scaling.
		 * @param scale We are using floating-point numbers here but compatibility scores are integers, hence we scale them before truncating into integers.
		 */
		public Stack<PairScore>	chooseStatePairs(final double threshold, final double scale)
		{
			ArrayList<PairScore> pairsAndScores = chooseStatePairs_internal(threshold, scale);
			if (threshold <= 0)
			{
				List<HandleRow<TARGET_TYPE>> handlerList = new LinkedList<>();
				@SuppressWarnings("unchecked")
				final List<PairScore> resultsPerThread [] = new List[threadNumber];
				for(int threadCnt=0;threadCnt<threadNumber;++threadCnt)
				{
					resultsPerThread[threadCnt]= new LinkedList<>();
					handlerList.add(new HandleRow<TARGET_TYPE>()
					{
						@Override
						public void init(@SuppressWarnings("unused") int threadNo) {
							// No per-thread initialisation is needed.
						}
		
						@Override
						public void handleEntry(Entry<CmpVertex, MapWithSearch<Label,Label, TARGET_TYPE>> entryA, int threadNo)
						{
							// Now iterate through states
							Iterator<Entry<CmpVertex,MapWithSearch<Label,Label,TARGET_TYPE>>> stateB_It = coregraph.transitionMatrix.entrySet().iterator();
							while(stateB_It.hasNext())
							{
								Entry<CmpVertex,MapWithSearch<Label,Label,TARGET_TYPE>> stateB = stateB_It.next();// stateB should not have been filtered out by construction of matrixInverse
								if (!filter.stateToConsider(entryA.getKey()) ||
										!filter.stateToConsider(stateB.getKey()))
								{// the above condition picks vertices that have previously been ignored.
									int score = 0;

									if (!AbstractLearnerGraph.checkCompatible(stateB.getKey(),entryA.getKey(),coregraph.pairCompatibility)) score=GDLearnerGraph.PAIR_INCOMPATIBLE;

									if (score>threshold) // note that we only get here if threshold <= 0 (as per condition at the top of chooseStatePairs)
										resultsPerThread[threadNo].add(new PairScore(entryA.getKey(),stateB.getKey(),(int)(scale*score),0));

									if (stateB.getKey().equals(entryA.getKey())) break; // we only process a triangular subset.
								}
							}// B-loop
						}
					});
				}
				GDLearnerGraph.performRowTasks(handlerList, threadNumber, coregraph.transitionMatrix,LearnerGraphND.ignoreNone,
						GDLearnerGraph.partitionWorkLoadTriangular(threadNumber,coregraph.transitionMatrix.size()));
				// now collect the results of processing
				for(int threadCnt=0;threadCnt<threadNumber;++threadCnt)
					pairsAndScores.addAll(resultsPerThread[threadCnt]);
			}
					
			return PairScoreComputation.getSortedPairsAndScoresStackFromUnsorted(pairsAndScores,coregraph.config);
		}
	}

}
