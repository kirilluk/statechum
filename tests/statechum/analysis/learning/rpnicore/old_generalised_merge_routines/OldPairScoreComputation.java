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

package statechum.analysis.learning.rpnicore.old_generalised_merge_routines;

import harmony.collections.HashMapWithSearch;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.GlobalConfiguration;
import statechum.Label;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.collections.ArrayMapWithSearchPos;
import statechum.collections.MapWithSearch;

import java.util.*;
import java.util.Map.Entry;

public class OldPairScoreComputation 
{
	final LearnerGraph coregraph;
	
	/** Associates this object to ComputeStateScores it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	public OldPairScoreComputation(LearnerGraph computeStateScores) 
	{
		coregraph = computeStateScores;
	}

	static class OldEquivalenceClassMergingDetails
	{
		int nextEquivalenceClass;
	}
	
	/** Merges the equivalence classes associated with the supplied pair.
	 * It is important to point out that classes merged can be incomplete, in that they will contain  
	 */
	private boolean mergePair(StatePair currentPair,Map<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> stateToEquivalenceClass, OldEquivalenceClassMergingDetails mergingDetails) throws IncompatibleStatesException
	{
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> firstClass = stateToEquivalenceClass.get(currentPair.firstElem);
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> secondClass= stateToEquivalenceClass.get(currentPair.secondElem);
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> equivalenceClass = null;

		boolean singleton = true;
		if (firstClass == null)
		{
			if (secondClass == null)
			{// a new pair has been discovered, populate from the current transition matrix.
				equivalenceClass = new OldEquivalenceClass<>(mergingDetails.nextEquivalenceClass++, coregraph);
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
	
	
	/** Merges the supplied pair of states states of the supplied machine. 
	 * Returns the result of merging and populates the collection containing equivalence classes.
	 *  
	 * @param original the machine in which to merge two states
	 * @param pair the states to merge
	 * @return result of merging, which is a shallow copy of the original LearnerGraph.
	 * In addition, mergedStates of the graph returned is set to equivalence classes 
	 * relating original and merged states.
	 */
	public static LearnerGraph mergeAndDeterminize_general(LearnerGraph original, StatePair pair)
	{
		assert original.transitionMatrix.containsKey(pair.firstElem);
		assert original.transitionMatrix.containsKey(pair.secondElem);
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new LinkedList<>();
		if (new OldPairScoreComputation(original).computePairCompatibilityScore_general(pair,null,mergedVertices) > 0)
			return OldMergeStates.mergeCollectionOfVertices(original,pair.getR(),mergedVertices);

		return null;
	}

	/** Similar to computePairCompatibilityScore_internal but can operate 
	 * on arbitrary graphs rather than just a graph and a PTA.
	 * 
	 *  @param pairToMerge pair to merge
	 *  @param pairsToMerge more pairs to merge in the process of computation. Can be null.
	 *  @param mergedVertices collection of sets of merged vertices. Singleton sets reflect those which were not merged with any other.
	 */ 
	public int computePairCompatibilityScore_general(StatePair pairToMerge, Collection<StatePair> pairsToMerge, Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices) 
	{
		int score=-1;
		
		OldEquivalenceClassMergingDetails mergingDetails = new OldEquivalenceClassMergingDetails();mergingDetails.nextEquivalenceClass = 0;
		MapWithSearch<VertID,CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> stateToEquivalenceClass =
				new HashMapWithSearch<>(5);// these are going to be small sets, no point creating really big ones.
		boolean compatible = true;
		Queue<EquivalenceClass<CmpVertex, LearnerGraphCachedData>> currentExplorationBoundary = new LinkedList<>();// FIFO queue containing pairs to be explored
		//ArrayList<OldEquivalenceClass<CmpVertex, LearnerGraphCachedData>> setOfEquivalenceClassesOnStack = new ArrayList<OldEquivalenceClass<CmpVertex, LearnerGraphCachedData>>();
		ArrayMapWithSearchPos<EquivalenceClass<CmpVertex, LearnerGraphCachedData>, EquivalenceClass<CmpVertex, LearnerGraphCachedData>, EquivalenceClass<CmpVertex, LearnerGraphCachedData>> setOfEquivalenceClassesOnStack  =
				new ArrayMapWithSearchPos<>();
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
						currentExplorationBoundary.add(eqClass);// in order to explore matching transitions
						setOfEquivalenceClassesOnStack.put(eqClass, eqClass);
					}
				}
			
			while(!currentExplorationBoundary.isEmpty())
			{
				EquivalenceClass<CmpVertex,LearnerGraphCachedData> equivalenceClass = currentExplorationBoundary.remove();setOfEquivalenceClassesOnStack.remove(equivalenceClass);
				
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
						@SuppressWarnings("unchecked")
						ArrayList<CmpVertex> targets = (ArrayList<CmpVertex>)equivalenceClass.getOutgoing().get(lbl);
						CmpVertex firstVertex = targets.get(0);
						if (targets.size() > 1)
						{
							singleton = false;/*
							OldEquivalenceClass<CmpVertex,LearnerGraphCachedData> firstEquivalenceClass = stateToEquivalenceClass.get(firstVertex);
							int firstEqNumber = firstEquivalenceClass.getNumber();*/
							int i=1;
							while(i<targets.size()) // here we benefit from the ability to iterate over a collection that may be updated as we iterate through it.
							{
								CmpVertex target = targets.get(i);
								if (!mergePair(new StatePair(firstVertex,target), stateToEquivalenceClass,mergingDetails))
								{
									EquivalenceClass<CmpVertex,LearnerGraphCachedData> firstEquivalenceClass = stateToEquivalenceClass.get(firstVertex);
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
				EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = stateToEquivalenceClass.get(vert);
				if (eqClass == null)
				{
					eqClass = new OldEquivalenceClass<>(mergingDetails.nextEquivalenceClass++, coregraph);
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
}
