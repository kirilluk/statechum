/* Copyright (c) 2016 The University of Sheffield.
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
package statechum.analysis.learning.experiments.MarkovEDSM;

import java.util.*;

import statechum.JUConstants;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairOfPaths;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.MarkovClassifierLG;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;

/** Constructs pre-merged graph. */
public class PerformFirstMerge
{
	public LearnerGraph ptaToUseForInference = null;
	public Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA=null;
	public CmpVertex vertexWithMostTransitions = null;
	public boolean correctCentre = true;
	public int centrePathNumber = 0;
	
	public PerformFirstMerge()
	{}
	
	/** 
	 * Builds an instance of the outcome of first merge. 
	 * @param pta PTA to start with.
	 * @param referenceGraph reference graph, used to report whether the decision made by this routine was correct.
	 * @param par parameters to use for making the first merge.
	 */
	public void buildFirstGraph(LearnerGraph pta, LearnerGraph referenceGraph, MarkovParameters par,MarkovModel m, final ConsistencyChecker checker)
	{
		final MarkovClassifierLG ptaClassifier = new MarkovClassifierLG(m,pta,null);
		final List<List<Label>> pathsToMerge=ptaClassifier.identifyPathsToMerge(checker,par.useAverageOrMax,par.divisorForPathCount,par.expectedWLen);
		centrePathNumber = pathsToMerge.size();
		// These vertices are merged first and then the learning start from the root as normal.
		// The reason to learn from the root is a memory cost. if we learn from the middle, we can get a better results
		verticesToMergeBasedOnInitialPTA=ptaClassifier.buildVerticesToMergeForPaths(pathsToMerge);
		
		// now check if we got the 'verticesToMergeBasedOnInitialPTA' correct using the reference graph (if reference is provided).
		if (referenceGraph != null)
		{
			for(Set<CmpVertex> collection:verticesToMergeBasedOnInitialPTA)
			{// we need to check that all states in the collection are associated with the same state in the reference graph.
				correctCentre = checkSetOfStatesAgainstReference(pta,collection,referenceGraph);
				if (!correctCentre)
					break;
			}
		}			
		List<StatePair> pairsListInitialMerge = ptaClassifier.buildVerticesToMergeForPath(pathsToMerge);
		LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMergeInitialMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		int scoreInitialMerge = pta.pairscores.computePairCompatibilityScore_general(null, pairsListInitialMerge, verticesToMergeInitialMerge, false);
		assert scoreInitialMerge >= 0;
		ptaToUseForInference = MergeStates.mergeCollectionOfVertices(pta, null, verticesToMergeInitialMerge, null,true);
		vertexWithMostTransitions = WaveBlueFringe.findVertexWithMostTransitions(ptaToUseForInference,MarkovClassifier.computeInverseGraph(pta),par.whichMostConnectedVertex);
		if (par.useMostConnectedVertexToStartLearning)
		{
			ptaToUseForInference.clearColours();ptaToUseForInference.getInit().setColour(null);vertexWithMostTransitions.setColour(JUConstants.RED);
		}
		
	}

	/** Given a collection of states in a PTA, checks that they all correspond to the same state of a reference graph.
	 */
	public static boolean checkSetOfStatesAgainstReference(LearnerGraph pta, Collection<CmpVertex> statesOfInterestArg, LearnerGraph referenceGraph)
	{
		Queue<CmpVertex> referenceState = new LinkedList<>();
		Queue<CmpVertex> fringe = new LinkedList<>();
		Set<CmpVertex> statesInFringe = new HashSet<>();// in order not to iterate through the list all the time.
		fringe.add(pta.getInit());referenceState.add(referenceGraph.getInit());
		Set<CmpVertex> statesOfInterest = new TreeSet<>(statesOfInterestArg);// make a copy of the set, otherwise we might modify something like a keyset of our coregraph and mess up both the graph and the traversal process.
		int pathsLeft=statesOfInterest.size();

		CmpVertex expectedStateInReference = null;
		while(!fringe.isEmpty())
		{
			CmpVertex currentState = fringe.remove();
			CmpVertex stateInReference = referenceState.remove();
			if (statesOfInterest.contains(currentState))
			{
				pathsLeft--;
				statesOfInterest.remove(currentState);

				if (expectedStateInReference == null) // first time we meet this state
					expectedStateInReference = stateInReference;
				else
					if (!expectedStateInReference.equals(stateInReference)) // one of the states from statesOfInterestArg does not match a different state, report this.
						return false;

				if (pathsLeft <= 0)
					break;// finished
			}

			Map<Label,CmpVertex> targets = pta.transitionMatrix.get(currentState);
			Map<Label,CmpVertex> targetInReference = referenceGraph.transitionMatrix.get(stateInReference);
			if(targets != null && !targets.isEmpty())
				for(Map.Entry<Label,CmpVertex> labelstate:targets.entrySet())
				{
					CmpVertex target = labelstate.getValue();
					if (!statesInFringe.contains(target))
					{
						fringe.offer(target);
						CmpVertex newStateInReference = targetInReference.get(labelstate.getKey());
						if (newStateInReference == null)
							throw new IllegalArgumentException("Transition "+ labelstate.getValue()+" does not exist from reference state "+stateInReference);
						referenceState.add(newStateInReference);
					}
				}
		}

		if (pathsLeft > 0)
			throw new IllegalArgumentException("checkSetOfStatesAgainstReference was supplied with a collection of states some of which are not reachable");

		return true;
	}
}