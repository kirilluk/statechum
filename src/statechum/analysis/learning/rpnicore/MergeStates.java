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
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.StatePair;
import statechum.collections.ArrayMapWithSearch;
import statechum.collections.HashMapWithSearch;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;
import statechum.Label;

public class MergeStates {
	final LearnerGraph coregraph;
	
	/** Associates this object to ComputeStateScores it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	MergeStates(LearnerGraph computeStateScores) 
	{
		coregraph = computeStateScores;
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
		Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		return mergeAndDeterminize_general(original,pair,mergedVertices);
	}
	
	/** Merges the supplied pair of states states of the supplied machine. 
	 * Returns the result of merging and populates the collection containing equivalence classes. Most importantly, this does not update a map detailing which vertices are merged from which and depth is also ignored.
	 * The benefit of this routine is relatively fast execution, around 4x.
	 *  
	 * @param original the machine in which to merge two states
	 * @return result of merging, which is a shallow copy of the original LearnerGraph.
	 * In addition, mergedStates of the graph returned is set to equivalence classes 
	 * relating original and merged states.
	 */
	public static LearnerGraph mergeCollectionOfVerticesNoUpdateOfAuxiliaryInformation(LearnerGraph original,
			Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices)
	{
		LearnerGraph result = new LearnerGraph(original.config);result.initEmpty();
		// Build a map from old vertices to the corresponding equivalence classes
		Map<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> origToNew = original.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>(original.vertPositiveID,original.vertNegativeID)
				:new HashMapWithSearch<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>(original.vertPositiveID+original.vertNegativeID);

		for(AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:mergedVertices)
		{
			eqClass.constructMergedVertex(result,false,true);
			for(CmpVertex v:eqClass.getStates())
                origToNew.put(v, eqClass);
		}
		
		result.setInit(origToNew.get(original.getInit()).getMergedVertex());
		result.vertNegativeID = original.vertNegativeID;result.vertPositiveID=original.vertPositiveID;
		
		for(AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:mergedVertices)
		{
			Map<Label,CmpVertex> row = result.transitionMatrix.get(eqClass.getMergedVertex());
			for(Entry<Label,Object> outgoing:eqClass.getOutgoing().entrySet())
			{
				Object value = outgoing.getValue();if (!(value instanceof CmpVertex)) value = ((ArrayList<CmpVertex>)value).get(0);
				row.put(outgoing.getKey(), origToNew.get(value).getMergedVertex());
			}
		}		

		return result;
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
			Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices)
	{
		LearnerGraph result = new LearnerGraph(original.config);result.initEmpty();
		Configuration cloneConfig = result.config.copy();cloneConfig.setLearnerCloneGraph(true);
		LearnerGraph configHolder = new LearnerGraph(cloneConfig);
		System.out.println(new Date()+" merge started");
		// Build a map from old vertices to the corresponding equivalence classes
		Map<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> origToNew = original.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>(original.vertPositiveID,original.vertNegativeID):
				new HashMapWithSearch<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>(original.vertPositiveID+original.vertNegativeID);
				
        Map<VertID,Collection<VertID>> mergedToHard = original.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
        		new ArrayMapWithSearch<VertID,Collection<VertID>>(original.vertPositiveID,original.vertNegativeID):new TreeMap<VertID,Collection<VertID>>();
		for(AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:mergedVertices)
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
		System.out.println(new Date()+" orig built");
		result.setInit(origToNew.get(original.getInit()).getMergedVertex());
		result.vertNegativeID = original.vertNegativeID;result.vertPositiveID=original.vertPositiveID;
		Queue<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> currentExplorationBoundary = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();// FIFO queue containing vertices to be explored
		currentExplorationBoundary.add(origToNew.get(original.getInit()));
		Map<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>,Boolean> visitedEqClasses = new ArrayMapWithSearch<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>,Boolean>();
		Boolean trueValue = new Boolean(true);
		while(!currentExplorationBoundary.isEmpty())
		{// In order to build a new transition diagram consisting of equivalence classes, I need to
		 // navigate the existing transition diagram, in its entirety.
			AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> current = currentExplorationBoundary.remove();
			Map<Label,CmpVertex> row = result.transitionMatrix.get(current.getMergedVertex());
			if (row == null)
			{
				row = result.createNewRow();result.transitionMatrix.put(current.getMergedVertex(), row);
			}

			for(CmpVertex equivalentVertex:current.getStates())
				for(Entry<Label,CmpVertex> entry:original.transitionMatrix.get(equivalentVertex).entrySet())
				{
					AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> nextClass = origToNew.get(entry.getValue());
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
		System.out.println(new Date()+" transitions constructed");
		
		AMEquivalenceClass.populateCompatible(result, mergedVertices);
		result.layoutOptions = original.layoutOptions.copy();
		result.learnerCache.invalidate();result.learnerCache.setMergedStates(mergedVertices);result.learnerCache.mergedToHardFacts=mergedToHard;
		if (redVertex != null)
			result.learnerCache.stateLearnt=origToNew.get(redVertex).getMergedVertex();
		
		result.pathroutines.updateDepthLabelling();
		System.out.println(new Date()+" merge complete, "+result.getAcceptStateNumber()+" states");

		return result;
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
	public static LearnerGraph mergeAndDeterminize_general(LearnerGraph original, StatePair pair, Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices)
	{

		if (original.pairscores.computePairCompatibilityScore_general(pair,null,mergedVertices) < 0)
		{/*
			try {
			
				String failName= new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.RESOURCES),"failedmerge_"+pair.getR().getID()+"_"+pair.getQ().getID()+".xml").getAbsolutePath();
				original.storage.writeGraphML(failName);
			} catch (IOException e) {
				System.out.println("failed to write error file");
				e.printStackTrace();
			}
			*/	
			throw new IllegalArgumentException("elements of the pair "+pair+" are incompatible, orig score was "+original.pairscores.computePairCompatibilityScore(pair));
		}
		return mergeCollectionOfVertices(original,pair.getR(),mergedVertices);
	}

	/**
	 * Mergers a pair of states QSM-like. Does not merge compatibility information, use generalised merger if that is needed..
	 *  
	 * @param original the graph in which to merge a pair of states
	 * @param pair states to merge
	 * @return the outcome of the merger.
	 */
	public static LearnerGraph mergeAndDeterminize(LearnerGraph original,StatePair pair)
	{
		assert pair.getQ() != pair.getR();
		if (GlobalConfiguration.getConfiguration().isAssertEnabled() && original.config.getDebugMode()) { PathRoutines.checkPTAConsistency(original, pair.getQ());PathRoutines.checkPTAIsTree(original,null,null,null); }
		assert original.transitionMatrix.containsKey(pair.firstElem);
		assert original.transitionMatrix.containsKey(pair.secondElem);
		Map<CmpVertex,List<CmpVertex>> mergedVertices = original.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,List<CmpVertex>>(original.vertPositiveID,original.vertNegativeID):
				new HashMapWithSearch<CmpVertex,List<CmpVertex>>(original.vertPositiveID+original.vertNegativeID);
		Configuration shallowCopy = original.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph result = new LearnerGraph(original,shallowCopy);
		assert result.transitionMatrix.containsKey(pair.firstElem);
		assert result.transitionMatrix.containsKey(pair.secondElem);
		if (GlobalConfiguration.getConfiguration().isAssertEnabled() && original.config.getDebugMode()) PathRoutines.checkPTAConsistency(result, pair.getQ());
		
		if (original.pairscores.computePairCompatibilityScore_internal(pair,mergedVertices) < 0)
			throw new IllegalArgumentException("elements of the pair are incompatible");

		// make a loop
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:original.transitionMatrix.entrySet())
		{
			for(Entry<Label,CmpVertex> rowEntry:entry.getValue().entrySet())
				if (rowEntry.getValue() == pair.getQ())	
					// the transition from entry.getKey() leads to the original blue state, record it to be rerouted.
					result.transitionMatrix.get(entry.getKey()).put(rowEntry.getKey(), pair.getR());
		}

		Set<CmpVertex> ptaVerticesUsed = new HashSet<CmpVertex>();
		Set<Label> inputsUsed = new HashSet<Label>();

		// I iterate over the elements of the original graph in order to be able to update the target one.
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:original.transitionMatrix.entrySet())
		{
			CmpVertex vert = entry.getKey();
			Map<Label,CmpVertex> resultRow = result.transitionMatrix.get(vert);// the row we'll update
			if (mergedVertices.containsKey(vert))
			{// there are some vertices to merge with this one.
				inputsUsed.clear();inputsUsed.addAll(entry.getValue().keySet());// the first entry is either a "derivative" of a red state or a branch of PTA into which we are now merging more states.
				for(CmpVertex toMerge:mergedVertices.get(vert))
				{// for every input, I'll have a unique target state - this is a feature of PTA
				 // For this reason, every if multiple branches of PTA get merged, there will be no loops or parallel edges.
				// As a consequence, it is safe to assume that each input/target state combination will lead to a new state
				// (as long as this combination is the one _not_ already present from the corresponding red state).
					boolean somethingWasAdded = false;
					for(Entry<Label,CmpVertex> input_and_target:original.transitionMatrix.get(toMerge).entrySet())
						if (!inputsUsed.contains(input_and_target.getKey()))
						{
							resultRow.put(input_and_target.getKey(), input_and_target.getValue());
							inputsUsed.add(input_and_target.getKey());
							ptaVerticesUsed.add(input_and_target.getValue());somethingWasAdded = true;
							// Since PTA is a tree, a tree rooted at ptaVerticesUsed will be preserved in a merged automaton, however 
							// other parts of a tree could be merged into it. In this case, each time there is a fork corresponding to 
							// a step by that other chunk which the current tree cannot follow, that step will end in a tree and a root
							// of that tree will be added to ptaVerticesUsed.
						}
					assert somethingWasAdded : "RedAndBlueToBeMerged was not set correctly at an earlier stage";
				}
			}
		}
		
		// now remove everything related to the PTA
		Queue<CmpVertex> currentExplorationBoundary = new LinkedList<CmpVertex>();// FIFO queue containing vertices to be explored
		currentExplorationBoundary.add( pair.getQ() );
		while(!currentExplorationBoundary.isEmpty())
		{
			CmpVertex currentVert = currentExplorationBoundary.remove();
			if (!ptaVerticesUsed.contains(currentVert))
			{// once a single used PTA vertex is found, all vertices from it (which do not have 
				// any transition leading to states in the red portion of the graph, by construction 
				// of ptaVerticesUsed) have been appended to the transition diagram and
				// hence we should not go through its target states.
				for(Entry<Label,CmpVertex> input_and_target:original.transitionMatrix.get(currentVert).entrySet())
					currentExplorationBoundary.offer(input_and_target.getValue());

				result.transitionMatrix.remove(currentVert);// remove the vertex from the resulting transition table.
			}
		}
		
		if (GlobalConfiguration.getConfiguration().isAssertEnabled() && original.config.getDebugMode()) PathRoutines.checkPTAIsTree(result, original, pair,ptaVerticesUsed);
		
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:result.transitionMatrix.entrySet())
			for(CmpVertex target:entry.getValue().values())
				if (!result.transitionMatrix.containsKey(target))
					throw new IllegalArgumentException("vertex "+target+" is not known in a transformed graph");
		result.learnerCache.invalidate();
		return result;
	}
	
	@SuppressWarnings("unchecked")
	public static DirectedSparseGraph mergeAndDeterminize(Graph graphToMerge, StatePair pair, Configuration conf)
	{
			DirectedSparseGraph g = (DirectedSparseGraph)graphToMerge.copy();
			DeterministicVertex newBlue = DeterministicDirectedSparseGraph.findVertexNamed(pair.getQ(),g);
			DeterministicVertex newRed = DeterministicDirectedSparseGraph.findVertexNamed(pair.getR(),g);
			Map<CmpVertex,List<CmpVertex>> mergedVertices = conf.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY? 
					new ArrayMapWithSearch<CmpVertex,List<CmpVertex>>(g.numVertices(),g.numVertices()):
					new HashMapWithSearch<CmpVertex,List<CmpVertex>>(g.numVertices());
			
			// Special configuration is necessary to ensure that computePairCompatibilityScore_internal
			// builds mergedVertices using g's vertices rather than StringVertices or clones of g's vertices.
			Configuration VertexCloneConf = conf.copy();VertexCloneConf.setLearnerUseStrings(false);VertexCloneConf.setLearnerCloneGraph(false);
			LearnerGraph s=new LearnerGraph(g,VertexCloneConf);
			if (s.pairscores.computePairCompatibilityScore_internal(new StatePair(s.findVertex(pair.getQ()),s.findVertex(pair.getR())),mergedVertices) < 0)
				throw new IllegalArgumentException("elements of the pair are incompatible");

			// make a loop
			Set<Label> usedInputs = new HashSet<Label>();
			for(DirectedSparseEdge e:(Set<DirectedSparseEdge>)newBlue.getInEdges())
			{
				Vertex source = e.getSource();
				Collection<Label> existingLabels = (Collection<Label>)e.getUserDatum(JUConstants.LABEL);
				g.removeEdge(e);

				// It is possible that there is already an edge between g.getSource Blue and newRed
				Iterator<DirectedSparseEdge> sourceOutIt = source.getOutEdges().iterator();
				Edge fromSourceToNewRed = null;
				while(sourceOutIt.hasNext() && fromSourceToNewRed == null)
				{
					DirectedSparseEdge out = sourceOutIt.next();if (out.getDest() == newRed) fromSourceToNewRed = out;
				}
				if (fromSourceToNewRed == null)
				{
					fromSourceToNewRed = new DirectedSparseEdge(source,newRed);
					fromSourceToNewRed.setUserDatum(JUConstants.LABEL, existingLabels, UserData.CLONE);// no need to clone this one since I'll delete the edge in a bit
					g.addEdge(fromSourceToNewRed);
				}
				else
					// there is already a transition from source to newRed, hence all we have to do is merge the new labels into it.
					((Collection<Label>)fromSourceToNewRed.getUserDatum(JUConstants.LABEL)).addAll( existingLabels );
					
			}

			// now the elements of mergedVertices are in terms of the copied graph.
			for(Vertex vert:(Set<Vertex>)g.getVertices())
				if (mergedVertices.containsKey(vert))
				{// there are some vertices to merge with this one.
					usedInputs.clear();usedInputs.addAll(s.transitionMatrix.get(vert).keySet());
					for(CmpVertex toMerge:mergedVertices.get(vert))
					{// for every input, I'll have a unique target state - this is a feature of PTA
					 // For this reason, every if multiple branches of PTA get merged, there will be no loops or parallel edges.
					// As a consequence, it is safe to assume that each input/target state combination will lead to a new state.
						Set<Label> inputsFrom_toMerge = s.transitionMatrix.get(toMerge).keySet();
						for(Label input:inputsFrom_toMerge)
							if (!usedInputs.contains(input))
							{
								Set<Label> labels = new HashSet<Label>();
                                                                labels.add(input);
								DeterministicVertex targetVert = (DeterministicVertex)s.transitionMatrix.get(toMerge).get(input);
								DirectedSparseEdge newEdge = new DirectedSparseEdge(vert,targetVert);
								newEdge.addUserDatum(JUConstants.LABEL, labels, UserData.CLONE);
								g.removeEdges(targetVert.getInEdges());g.addEdge(newEdge);
							}
						usedInputs.addAll(inputsFrom_toMerge);
					}
				}
			
			// now remove everything related to the PTA
			Queue<Vertex> currentExplorationBoundary = new LinkedList<Vertex>();// FIFO queue containing vertices to be explored
			currentExplorationBoundary.add( newBlue );
			while(!currentExplorationBoundary.isEmpty())
			{
				Vertex currentVert = currentExplorationBoundary.remove();
				Set<DirectedSparseEdge> outEdges = currentVert.getOutEdges();
				for(DirectedSparseEdge e:outEdges)
					currentExplorationBoundary.add(e.getDest());
				g.removeEdges(outEdges);
				g.removeVertex(currentVert);
			}
			
			return g;
	}	
}
