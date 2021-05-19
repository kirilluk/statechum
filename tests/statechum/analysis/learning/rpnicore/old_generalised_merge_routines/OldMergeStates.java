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

import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.TreeMap;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.GlobalConfiguration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.collections.ArrayMapWithSearch;
import statechum.Label;

public class OldMergeStates 
{
	final LearnerGraph coregraph;
	
	/** Associates this object to ComputeStateScores it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	OldMergeStates(LearnerGraph computeStateScores) 
	{
		coregraph = computeStateScores;
	}

	/** Merges the supplied pair of states states of the supplied machine. 
	 * Returns the result of merging and populates the collection containing equivalence classes.
	 *  
	 * @param original the machine in which to merge two states
	 * @param redVertex the vertex from the original graph corresponding to <em>result.learnerCache.stateLearnt</em> 
	 * of the new one.
	 * @return result of merging, which is a shallow copy of the original LearnerGraph.
	 * In addition, mergedStates of the graph returned is set to equivalence classes 
	 * relating original and merged states.
	 */
	public static LearnerGraph mergeCollectionOfVertices(LearnerGraph original,CmpVertex redVertex,
			Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices)
	{
		LearnerGraph result = new LearnerGraph(original.config);result.initEmpty();
		Configuration cloneConfig = result.config.copy();cloneConfig.setLearnerCloneGraph(true);
		LearnerGraph configHolder = new LearnerGraph(cloneConfig);
		
		// Build a map from old vertices to the corresponding equivalence classes
		Map<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> origToNew = AbstractLearnerGraph.constructMap(cloneConfig,original);
                Map<VertID,Collection<VertID>> mergedToHard = new TreeMap<VertID,Collection<VertID>>();
		for(EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:mergedVertices)
		{
			eqClass.constructMergedVertex(configHolder,false,true);
                        Collection<VertID> hardVertices = new LinkedList<VertID>();mergedToHard.put(eqClass.getMergedVertex(), hardVertices);
			for(CmpVertex v:eqClass.getStates())
            {
                origToNew.put(v, eqClass);
                Map<VertID,Collection<VertID>> hardOrig = original.learnerCache.getMergedToHardFacts();
                if (hardOrig != null && hardOrig.containsKey(v))
                {
                    hardVertices.addAll(hardOrig.get(v));
                }
                else
                    hardVertices.add(v);
            }
		}
		result.setInit(origToNew.get(original.getInit()).getMergedVertex());
		result.vertNegativeID = original.vertNegativeID;result.vertPositiveID=original.vertPositiveID;
		Queue<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> currentExplorationBoundary = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();// FIFO queue containing vertices to be explored
		currentExplorationBoundary.add(origToNew.get(original.getInit()));
		Map<EquivalenceClass<CmpVertex,LearnerGraphCachedData>,Boolean> visitedEqClasses = new ArrayMapWithSearch<EquivalenceClass<CmpVertex,LearnerGraphCachedData>,Boolean>();
		Boolean trueValue = Boolean.valueOf(true);
		while(!currentExplorationBoundary.isEmpty())
		{// In order to build a new transition diagram consisting of equivalence classes, I need to
		 // navigate the existing transition diagram, in its entirety.
			EquivalenceClass<CmpVertex,LearnerGraphCachedData> current = currentExplorationBoundary.remove();
			Map<Label,CmpVertex> row = result.transitionMatrix.get(current.getMergedVertex());
			if (row == null)
			{
				row = result.createNewRow();result.transitionMatrix.put(current.getMergedVertex(), row);
			}

			for(CmpVertex equivalentVertex:current.getStates())
				for(Entry<Label,CmpVertex> entry:original.transitionMatrix.get(equivalentVertex).entrySet())
				{
					EquivalenceClass<CmpVertex,LearnerGraphCachedData> nextClass = origToNew.get(entry.getValue());
					if (null == visitedEqClasses.get(nextClass))
					{// have not yet visited this class
						currentExplorationBoundary.offer(nextClass);
						visitedEqClasses.put(nextClass,trueValue);
					}
					if (GlobalConfiguration.getConfiguration().isAssertEnabled() && row.containsKey(entry.getKey()))
						assert row.get(entry.getKey()) == nextClass.getMergedVertex();
					row.put(entry.getKey(), nextClass.getMergedVertex());
				}	
		}
		statechum.analysis.learning.rpnicore.AMEquivalenceClass.populateCompatible(result, mergedVertices);
		result.setLayoutOptions(original.getLayoutOptions().copy());
		result.learnerCache.invalidate();result.learnerCache.setMergedStates(mergedVertices);result.learnerCache.setMergedToHardFacts(mergedToHard);
		if (redVertex != null)
			result.learnerCache.setStateLearnt(origToNew.get(redVertex).getMergedVertex());
		
		result.pathroutines.updateDepthLabelling();
		return result;
	}
}
