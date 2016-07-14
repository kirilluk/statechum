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

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import statechum.JUConstants;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairOfPaths;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
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
		final MarkovClassifier ptaClassifier = new MarkovClassifier(m,pta);
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
				Map<CmpVertex,LinkedList<Label>> vertToPaths =  PairOfPaths.convertSetOfStatesToPaths(pta,collection);// this obtains all the paths to reach states of interest, we then trace them in the reference graph. 
				CmpVertex expectedState = null;
				for(CmpVertex vertFromCollection:collection)
				{
					List<Label> path = vertToPaths.get(vertFromCollection);
					CmpVertex obtainedVertex = referenceGraph.getVertex(path);
					if (expectedState == null)
						expectedState = obtainedVertex;
					else
						if (expectedState != obtainedVertex)
						{
							correctCentre = false;break;// our paths do not correctly identify states of the expected graph
						}
				}
				
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
}