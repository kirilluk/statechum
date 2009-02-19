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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

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
	
	/** Used to populate <em>result.learnerCache.setMergedStates</em> with dummy values. 
	public void buildDummyMergeResult()
	{
		int i=0;
		Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		for(CmpVertex vertex:coregraph.transitionMatrix.keySet())
		{
			AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(i++,coregraph);
			try {
				eqClass.addFrom(vertex, null);
			} catch (IncompatibleStatesException e) {
				Helper.throwUnchecked("failed to construct an AMEquivalenceClass with a single node", e);
			}
			mergedVertices.add(eqClass);
		}	
		coregraph.learnerCache.setMergedStates(mergedVertices);
	}
	*/
	
	/** Merges the supplied pair of states states of the supplied machine. 
	 * Returns the result of merging and populates the collection containing equivalence classes.
	 *  
	 * @param original the machine in which to merge two states
	 * @param pair the states to merge
	 * @return result of merging, which is a shallow copy of the original LearnerGraph.
	 * In addition, mergedStates of the graph returned is set to equivalence classes 
	 * relating original and merged states.
	 */
	public static LearnerGraph mergeAndDeterminize_general(LearnerGraph original, StatePair pair,
			Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices)
	{
		LearnerGraph result = new LearnerGraph(original.config);result.initEmpty();
/*		result.initEmpty();
		result.transitionMatrix = result.createNewTransitionMatrix();
*/
		if (original.pairscores.computePairCompatibilityScore_general(pair,mergedVertices) < 0)
		{/*
			try {
				String failName= new File("resources","failedmerge_"+pair.getR().getID()+"_"+pair.getQ().getID()+".xml").getAbsolutePath();
				original.storage.writeGraphML(failName);
			} catch (IOException e) {
				System.out.println("failed to write error file");
				e.printStackTrace();
			}
			*/	
			throw new IllegalArgumentException("elements of the pair "+pair+" are incompatible, orig score was "+original.pairscores.computePairCompatibilityScore(pair));
		}
		
		Configuration cloneConfig = result.config.copy();cloneConfig.setLearnerCloneGraph(true);
		LearnerGraph configHolder = new LearnerGraph(cloneConfig);
		// Build a map from old vertices to the corresponding equivalence classes
		Map<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> origToNew = new HashMap<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		for(AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:mergedVertices)
		{
			eqClass.constructMergedVertex(configHolder,false,true);
			for(CmpVertex v:eqClass.getStates())
				origToNew.put(v, eqClass);
		}
		result.init = origToNew.get(original.init).getMergedVertex();
		result.vertNegativeID = original.vertNegativeID;result.vertPositiveID=original.vertPositiveID;
		Queue<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> currentExplorationBoundary = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();// FIFO queue containing vertices to be explored
		currentExplorationBoundary.add(origToNew.get(original.init));
		Set<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> visitedEqClasses = new HashSet<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		while(!currentExplorationBoundary.isEmpty())
		{// In order to build a new transition diagram consisting of equivalence classes, I need to
		 // navigate the existing transition diagram, in its entirety.
			AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> current = currentExplorationBoundary.remove();
			Map<String,CmpVertex> row = result.transitionMatrix.get(current.getMergedVertex());
			if (row == null)
			{
				row = result.createNewRow();result.transitionMatrix.put(current.getMergedVertex(), row);
			}

			for(CmpVertex equivalentVertex:current.getStates())
				for(Entry<String,CmpVertex> entry:original.transitionMatrix.get(equivalentVertex).entrySet())
				{
					AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> nextClass = origToNew.get(entry.getValue());
					if (!visitedEqClasses.contains(nextClass))
					{// have not yet visited this class
						currentExplorationBoundary.offer(nextClass);
						visitedEqClasses.add(nextClass);
					}
					if (GlobalConfiguration.getConfiguration().isAssertEnabled() && row.containsKey(entry.getKey()))
						assert row.get(entry.getKey()) == nextClass.getMergedVertex();
					row.put(entry.getKey(), nextClass.getMergedVertex());
				}	
		}
		AMEquivalenceClass.populateCompatible(result, mergedVertices);
		result.learnerCache.invalidate();result.learnerCache.setMergedStates(mergedVertices);
		result.learnerCache.stateLearnt=origToNew.get(pair.getR()).getMergedVertex();
		if (original.learnerCache.getVertexToEqClass() != null) 
		{// only build a set if there was one built earlier. This way I can ensure that a graph not built 
		 // via a sequence of mergers will not have such a map (because we need traceability in order
		 // to keep the map up to date).
			original.merger.buildVertexToEqClassMap(null);// update the map in the original with the vertices which may have been added between its construction and now
			result.merger.buildVertexToEqClassMap(original.learnerCache.getVertexToEqClass());
		}
		return result;
	}
	
	/** Each time a merge happens, we need to rebuild a map from merged vertices to collections 
	 * of original vertices they correspond to. This is the purpose of this method.
	 * <p>
	 * If <em>previousMap</em> is null, the current map is updated with vertices not 
	 * mentioned in the map (or built anew if it does not exist).
	 * <p>
	 * There is no waste in using CmpVertex-vertices because they are part of the initial PTA and
	 * hence kept in memory anyway.
	 */
	public void buildVertexToEqClassMap(Map<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> previousMap)
	{
		// First, we build a collection of states of the original PTA which correspond to the each merged vertex.
		Map<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> newVertexToEqClass = new TreeMap<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		
		if (previousMap == null)
		{// either the case when we get here for the first time (as well as right after a reset)
		 // or when vertices have been added to the graph and we need to update the map.	
			if (coregraph.learnerCache.getVertexToEqClass() != null)
				newVertexToEqClass = coregraph.learnerCache.getVertexToEqClass();// we are updating the map here.
			int i=newVertexToEqClass.size();// ensure that if we add new vertices, their IDs will be unique by construction below.
			
			for(CmpVertex vertex:coregraph.transitionMatrix.keySet())
				if (!newVertexToEqClass.containsKey(vertex))
				{// vertex not in the collection
					AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(i++,coregraph);
					try {
						eqClass.addFrom(vertex, null);
					} catch (IncompatibleStatesException e) {
						Helper.throwUnchecked("failed to construct an AMEquivalenceClass with a single node", e);
					}
	
					newVertexToEqClass.put(eqClass.getRepresentative(),eqClass);
				}
		}
		else // after a previous successful merge 
			for(AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:coregraph.learnerCache.getMergedStates())
			{
				AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> combinedEqClass = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(eqClass.getNumber(),coregraph);
				for(CmpVertex state:eqClass.getStates())
					try 
					{
						combinedEqClass.mergeWith(previousMap.get(state));
					} catch (IncompatibleStatesException e) {
						Helper.throwUnchecked("failed to construct a collection of states which have previously been merged successfully", e);
					}
				newVertexToEqClass.put(eqClass.getMergedVertex(),combinedEqClass);
			}
		coregraph.learnerCache.vertexToEqClass = newVertexToEqClass;
		
		if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.ASSERT)))
		{
			Map<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> vertexToCollection = new TreeMap<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			for(Entry<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> eqClass:coregraph.learnerCache.vertexToEqClass.entrySet())
			{
				for(CmpVertex vert:eqClass.getValue().getStates())
				{
					AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> existingClass = vertexToCollection.get(vert);
					if (existingClass != null)
						throw new IllegalArgumentException("classes "+existingClass+" and "+eqClass.getValue()+" share vertex "+vert);
					vertexToCollection.put(vert,eqClass.getValue());
				}
			}
			
			{
				Set<CmpVertex> verticesInGraph = new TreeSet<CmpVertex>();verticesInGraph.addAll(coregraph.transitionMatrix.keySet());
				verticesInGraph.removeAll(coregraph.learnerCache.getVertexToEqClass().keySet());
				if (!verticesInGraph.isEmpty())
					throw new IllegalArgumentException("vertices such as "+verticesInGraph+" do not feature in the vertex to collection map");
			}
			
			{
				Set<CmpVertex> verticesInCollection = new TreeSet<CmpVertex>();verticesInCollection.addAll(coregraph.learnerCache.getVertexToEqClass().keySet());
				verticesInCollection.removeAll(coregraph.transitionMatrix.keySet());
				if (!verticesInCollection.isEmpty())
					throw new IllegalArgumentException("vertices from the vertex to collection map "+verticesInCollection+" do not feature in the graph");
			}
			// the computed number of vertices cannot be matched to that of the 
			// original PTA because graphs can be updated using IF-THEN automata.
		}
	}
	
	public static LearnerGraph mergeAndDeterminize(LearnerGraph original,StatePair pair)
	{
		if (GlobalConfiguration.getConfiguration().isAssertEnabled()) { PathRoutines.checkPTAConsistency(original, pair.getQ());PathRoutines.checkPTAIsTree(original,null,null,null); }
		assert original.transitionMatrix.containsKey(pair.firstElem);
		assert original.transitionMatrix.containsKey(pair.secondElem);
		Map<CmpVertex,List<CmpVertex>> mergedVertices = new HashMap<CmpVertex,List<CmpVertex>>();
		Configuration shallowCopy = original.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph result = new LearnerGraph(original,shallowCopy);
		assert result.transitionMatrix.containsKey(pair.firstElem);
		assert result.transitionMatrix.containsKey(pair.secondElem);
		if (GlobalConfiguration.getConfiguration().isAssertEnabled()) PathRoutines.checkPTAConsistency(result, pair.getQ());
		
		if (original.pairscores.computePairCompatibilityScore_internal(pair,mergedVertices) < 0)
			throw new IllegalArgumentException("elements of the pair are incompatible");

		// make a loop
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:original.transitionMatrix.entrySet())
		{
			for(Entry<String,CmpVertex> rowEntry:entry.getValue().entrySet())
				if (rowEntry.getValue() == pair.getQ())	
					// the transition from entry.getKey() leads to the original blue state, record it to be rerouted.
					result.transitionMatrix.get(entry.getKey()).put(rowEntry.getKey(), pair.getR());
		}

		Set<CmpVertex> ptaVerticesUsed = new HashSet<CmpVertex>();
		Set<String> inputsUsed = new HashSet<String>();

		// I iterate over the elements of the original graph in order to be able to update the target one.
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:original.transitionMatrix.entrySet())
		{
			CmpVertex vert = entry.getKey();
			Map<String,CmpVertex> resultRow = result.transitionMatrix.get(vert);// the row we'll update
			if (mergedVertices.containsKey(vert))
			{// there are some vertices to merge with this one.
				
				inputsUsed.clear();inputsUsed.addAll(entry.getValue().keySet());// the first entry is either a "derivative" of a red state or a branch of PTA into which we are now merging more states.
				for(CmpVertex toMerge:mergedVertices.get(vert))
				{// for every input, I'll have a unique target state - this is a feature of PTA
				 // For this reason, every if multiple branches of PTA get merged, there will be no loops or parallel edges.
				// As a consequence, it is safe to assume that each input/target state combination will lead to a new state
				// (as long as this combination is the one _not_ already present from the corresponding red state).
					boolean somethingWasAdded = false;
					for(Entry<String,CmpVertex> input_and_target:original.transitionMatrix.get(toMerge).entrySet())
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
				for(Entry<String,CmpVertex> input_and_target:original.transitionMatrix.get(currentVert).entrySet())
					currentExplorationBoundary.offer(input_and_target.getValue());

				result.transitionMatrix.remove(currentVert);// remove the vertex from the resulting transition table.
			}
		}
		
		if (GlobalConfiguration.getConfiguration().isAssertEnabled()) PathRoutines.checkPTAIsTree(result, original, pair,ptaVerticesUsed);
		
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:result.transitionMatrix.entrySet())
			for(CmpVertex target:entry.getValue().values())
				if (!result.transitionMatrix.containsKey(target))
					throw new IllegalArgumentException("vertex "+target+" is not known in a transformed graph");
		result.learnerCache.invalidate();
		return result;
	}
	
	public static DirectedSparseGraph mergeAndDeterminize(Graph graphToMerge, StatePair pair, Configuration conf)
	{
			DirectedSparseGraph g = (DirectedSparseGraph)graphToMerge.copy();
			DeterministicVertex newBlue = DeterministicDirectedSparseGraph.findVertexNamed(pair.getQ().getID(),g);
			DeterministicVertex newRed = DeterministicDirectedSparseGraph.findVertexNamed(pair.getR().getID(),g);
			Map<CmpVertex,List<CmpVertex>> mergedVertices = new HashMap<CmpVertex,List<CmpVertex>>();
			
			// Special configuration is necessary to ensure that computePairCompatibilityScore_internal
			// builds mergedVertices using g's vertices rather than StringVertices or clones of g's vertices.
			Configuration VertexCloneConf = conf.copy();VertexCloneConf.setLearnerUseStrings(false);VertexCloneConf.setLearnerCloneGraph(false);
			LearnerGraph s=new LearnerGraph(g,VertexCloneConf);
			if (s.pairscores.computePairCompatibilityScore_internal(new StatePair(s.findVertex(pair.getQ().getID()),s.findVertex(pair.getR().getID())),mergedVertices) < 0)
				throw new IllegalArgumentException("elements of the pair are incompatible");

			// make a loop
			Set<String> usedInputs = new HashSet<String>();
			for(DirectedSparseEdge e:(Set<DirectedSparseEdge>)newBlue.getInEdges())
			{
				Vertex source = e.getSource();
				Collection<String> existingLabels = (Collection<String>)e.getUserDatum(JUConstants.LABEL);
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
					((Collection<String>)fromSourceToNewRed.getUserDatum(JUConstants.LABEL)).addAll( existingLabels );
					
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
						Set<String> inputsFrom_toMerge = s.transitionMatrix.get(toMerge).keySet();
						for(String input:inputsFrom_toMerge)
							if (!usedInputs.contains(input))
							{
								Set<String> labels = new HashSet<String>();labels.add(input);
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
