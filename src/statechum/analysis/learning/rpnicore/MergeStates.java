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
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;
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
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
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
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		return mergeAndDeterminize_general(original,pair,mergedVertices);
	}
	
	/** Adds all transitions from the provided vertex (current) to the transition matrix row (row). A target vertex for each transition is either a merged vertex from an equivalence class
	 * (if an entry exists in origToNew) or an actual CmpVertex (equivalence classes does not contain entries where a state was not merged).
	 * Some of the mentioned states have been seen before and either were or will be processed. Those that were seen would be a member of visitedTargetStates and where they were not seen,
	 * they need to be and hence added to currentExplorationBoundary.
	 * 
	 * @param row the row of a transition matrix to update
	 * @param current the state in the original (before merge) graph being considered.
	 * @param original the graph before merge.
	 * @param visitedTargetStates states in the merged graph that were earlier scheduled for processing. This is represented as a Map because this gives me flexibility to use ArrayMapWithSearch as a set and
	 * in addition, Java implements Sets as Maps from elements to themselves.
	 * @param origToEqClass a map from vertices to equivalence classes. This is a partial function since where a vertex is not merged there is no point storing it in this map 
	 * (significant for a million-vertex graph where a merge only affects a handful of vertices). 
	 * @param currentExplorationBoundary contains vertices to process later on in the loop inside mergeCollectionOfVertices.
	 * @param verticesToConsider if not null, only transitions leading to/from these vertices in the original graph will be considered. 
	 * This is necessary in order to  
	 */
	private static void processVertex(Map<Label,CmpVertex> row,CmpVertex current,LearnerGraph original,LearnerGraph result,Map<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> origToEqClass,
			Map<CmpVertex,CmpVertex> visitedTargetStates,Queue<CmpVertex> currentExplorationBoundary,Collection<CmpVertex> verticesToConsider, Map<VertID,Collection<VertID>> mergedToHard)
	{
		Map<Label,CmpVertex> transitionsOutOfState = original.transitionMatrix.get(current);
		for(Entry<Label,CmpVertex> entry:transitionsOutOfState.entrySet())
		{
			CmpVertex targetState = entry.getValue();
			EquivalenceClass<CmpVertex, LearnerGraphCachedData> nextClass = origToEqClass.get(targetState);
			if (nextClass != null)
			// next state belongs to an equivalence class
				targetState = nextClass.getRepresentative();
			
			CmpVertex targetInResultGraph = visitedTargetStates.get(targetState);// this returns the vertex in the target graph. For vertices built from eq classes, it is the same as mergedVertex, 
				// for others (those not in eqClasses, corresponding to singleton equivalence classes) it will be a clone of that vertex that is used in the target graph.
			if (nextClass != null && targetInResultGraph != null)
				if (targetInResultGraph != nextClass.getMergedVertex())
				{
					assert false:"vertex in target graph is not the same as a merged vertex";// using assert false so that a breakpoint can be set on this line.
				}

			if (targetInResultGraph == null)
			{// have not yet visited this class
				if (nextClass != null)
					targetInResultGraph = nextClass.getMergedVertex();// class not null, hence target vertex has already been built.
				else
					targetInResultGraph = AMEquivalenceClass.constructMergedVertexFrom(targetState,targetState.getColour(),result,false,true);// cloning vertex here.
				if (result.transitionMatrix.containsKey(targetInResultGraph))
					assert false : "vertex "+targetInResultGraph+" already exists in the target graph";
				if (mergedToHard != null && nextClass == null)
					updateHardFacts(targetState,targetInResultGraph,original,mergedToHard);
				result.transitionMatrix.put(targetInResultGraph, result.createNewRow());
				visitedTargetStates.put(targetState,targetInResultGraph);
				if (verticesToConsider == null || verticesToConsider.contains(targetState))
					currentExplorationBoundary.offer(targetState);// inconsistency is computed per-vertex, therefore if outgoing transitions lead to states that we do not intend to explore, we still need to add them, it is just that we do not need to add subsequent transitions to them. 
			}
			if (GlobalConfiguration.getConfiguration().isAssertEnabled() && row.containsKey(entry.getKey()))
				if (row.get(entry.getKey()) != targetInResultGraph)
					assert false:"every time we meet a target state, we should use the same object as a target state";// using assertfalse so that a breakpoint can be set on this line.
			row.put(entry.getKey(), targetInResultGraph);
		}	
	}

	/** Updates the hard-facts map for the constructed vertex that corresponds to a singleton equivalence class for a merge. */
	private static void updateHardFacts(CmpVertex currentInOrigin, CmpVertex currentInTarget, LearnerGraph original, Map<VertID,Collection<VertID>> mergedToHard)
	{
		Collection<VertID> hardVertices = new ArrayList<VertID>();mergedToHard.put(currentInTarget, hardVertices);
		Map<VertID,Collection<VertID>> hardOrig = original.learnerCache.getMergedToHardFacts();
        if (hardOrig != null && hardOrig.containsKey(currentInOrigin))
        {
            hardVertices.addAll(hardOrig.get(currentInOrigin));
        }
        else
            hardVertices.add(currentInOrigin);
	}
	
	/** The purpose of this method is to clone all vertices from a specified collection and add them to the exploration boundary. Think of it as constructing a graph with multiple 
	 * initial states. This is not far from true - when evaluating inconsistencies, it is necessary to build a subset of a target graph, only containing vertices contributing to a 
	 * difference in inconsistencies between the original and the merged graph.
	 * 
	 * @param vertices start states to explore
	 * @param result graph to be constructed.
	 * @param originalVertexToClonedVertex maps vertices in the original graph to those in the constructed one.
	 * @param origToNew for equivalence classes that are not singletons, maps vertices from the original graph to equivalence classes containing them. Vertices that are singletons do not usually feature in the collection of equivalence classes and hence are not in origToNew
	 * @return a list of vertices to explore from in the original graph.
	 */
	private static Queue<CmpVertex> addVerticesAndConstructExplorationBoundary(Collection<CmpVertex> vertices, LearnerGraph result, Map<CmpVertex,CmpVertex> originalVertexToClonedVertex, Map<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> origToNew)
	{
		Queue<CmpVertex> currentExplorationBoundary = new LinkedList<CmpVertex>();// FIFO queue containing vertices to be explored
		for(CmpVertex v:vertices)
		{
			CmpVertex vOrig = v, vertexResult = null;
			EquivalenceClass<CmpVertex, LearnerGraphCachedData> vEqClass = origToNew.get(v);
			if (vEqClass != null)
			{
				vOrig = vEqClass.getRepresentative();vertexResult = vEqClass.getMergedVertex();
			}
			else
				vertexResult = AMEquivalenceClass.constructMergedVertexFrom(v,v.getColour(),result,false,true);// cloning vertex here.

			if (!result.transitionMatrix.containsKey(vertexResult))
			{// the corresponding equivalence class is not known in the graph being constructed, hence add it.
				result.transitionMatrix.put(vertexResult, result.createNewRow());
				currentExplorationBoundary.add(vOrig);
			}
			originalVertexToClonedVertex.put(vOrig,vertexResult);
		}
		
		return currentExplorationBoundary;
	}
	
	/** Merges the supplied pair of states states of the supplied machine. 
	 * Returns the result of merging and populates the collection containing equivalence classes.
	 *  
	 * @param original the machine in which to merge two states
	 * @param redVertex the vertex from the original graph corresponding to <em>result.learnerCache.stateLearnt</em> 
	 * of the new one.
	 * @param limitVerticesTo if not null, vertices in the target graph will be limited to those that start or end at vertices in this set. This is used to construct a subset of a merged graph for computation of inconsistency scores.
	 * @param updateAuxInformation if false, does not update any auxiliary information such as depth, mergedtohard, compatible vertices etc.
	 * @return result of merging, which is a shallow copy of the original LearnerGraph.
	 * In addition, mergedStates of the graph returned is set to equivalence classes 
	 * relating original and merged states.
	 */
	public static LearnerGraph mergeCollectionOfVertices(LearnerGraph original,CmpVertex redVertex, Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices, Collection<CmpVertex> limitVerticesTo, boolean updateAuxInformation)
	{
		LearnerGraph result = new LearnerGraph(original.config);result.initEmpty();
		Configuration cloneConfig = result.config.copy();cloneConfig.setLearnerCloneGraph(true);
		LearnerGraph configHolder = new LearnerGraph(cloneConfig);
		// Build a map from old vertices to the corresponding equivalence classes
		Map<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> origToEqClass = new HashMap<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
				/*original.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>(original.vertPositiveID,original.vertNegativeID):
				new HashMapWithSearch<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>(original.vertPositiveID+original.vertNegativeID);*/
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedForCompatibility = null;
		
        Map<VertID,Collection<VertID>> mergedToHard = null;
        if (updateAuxInformation)
        {
        	mergedToHard = AbstractLearnerGraph.constructMap(cloneConfig,original);
        	mergedForCompatibility = new ArrayList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();mergedForCompatibility.addAll(mergedVertices);
        }
		for(EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:mergedVertices)
		{
			eqClass.constructMergedVertex(configHolder,false,true);
			for(CmpVertex v:eqClass.getStates())
                origToEqClass.put(v, eqClass);
			
			if (updateAuxInformation)
			{
	            Collection<VertID> hardVertices = new ArrayList<VertID>();mergedToHard.put(eqClass.getMergedVertex(), hardVertices);
				for(CmpVertex v:eqClass.getStates())
	            {
	                Map<VertID,Collection<VertID>> hardOrig = original.learnerCache.getMergedToHardFacts();
	                if (hardOrig != null && hardOrig.containsKey(v))
	                {
	                    hardVertices.addAll(hardOrig.get(v));
	                }
	                else
	                    hardVertices.add(v);
	            }
			}
		}
		
		// This map associates vertices in the original graph to those in the cloned one. It is populated when new vertices are explored and therefore doubles as a 'visited' set.
		Map<CmpVertex,CmpVertex> originalVertexToClonedVertex = original.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,CmpVertex>(original.vertPositiveID,original.vertNegativeID):
					new HashMap<CmpVertex,CmpVertex>();
				
		Collection<CmpVertex> verticesToStartFrom = limitVerticesTo;
		if (limitVerticesTo == null)
			verticesToStartFrom = Arrays.asList(new CmpVertex[]{original.getInit()});
		Queue<CmpVertex> currentExplorationBoundary = addVerticesAndConstructExplorationBoundary(verticesToStartFrom,result,originalVertexToClonedVertex,origToEqClass);
		if (limitVerticesTo == null)
		{
			CmpVertex vOrig = original.getInit();
			EquivalenceClass<CmpVertex, LearnerGraphCachedData> vEqClass = origToEqClass.get(vOrig);
			if (vEqClass != null)
				vOrig = vEqClass.getRepresentative();
			result.setInit(originalVertexToClonedVertex.get(vOrig));
		}
		result.vertNegativeID = original.vertNegativeID;result.vertPositiveID=original.vertPositiveID;
		while(!currentExplorationBoundary.isEmpty())
		{// In order to build a new transition diagram consisting of equivalence classes, I need to
		 // navigate the existing transition diagram, in its entirety.
			CmpVertex current = currentExplorationBoundary.remove();
			Map<Label,CmpVertex> row = result.transitionMatrix.get(originalVertexToClonedVertex.get(current));
			EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = origToEqClass.get(current);
			assert row != null;

			if (eqClass != null)
			{// current vertex is a member of an equivalence class
				for(CmpVertex equivalentVertex:eqClass.getStates()) // eqClass.getOutgoing() will return a singleton at this stage because they are built to do this by the generalised score computation.
					processVertex(row,equivalentVertex,original,result,origToEqClass,originalVertexToClonedVertex,currentExplorationBoundary,limitVerticesTo,mergedToHard);
			}
			else
			{// current vertex is standalone (that is, a singleton equivalence class) 
				processVertex(row,current,original,result,origToEqClass,originalVertexToClonedVertex,currentExplorationBoundary,limitVerticesTo,mergedToHard);
				if (mergedForCompatibility != null)
				{// add a singleton equivalence class with this state 
					Map<CmpVertex,JUConstants.PAIRCOMPATIBILITY> vertexToValue = original.pairCompatibility.compatibility.get(current);
					if (vertexToValue != null)
					{// if we have any vertices associated with the current one
						Collection<CmpVertex> incompatibleVertices = new LinkedList<CmpVertex>();
						for(Entry<CmpVertex,JUConstants.PAIRCOMPATIBILITY> entry:vertexToValue.entrySet())
							if (entry.getValue() == JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE)
								incompatibleVertices.add(entry.getKey());
						mergedForCompatibility.add(new EquivalenceClassForConstructionOfCompatibility(current, originalVertexToClonedVertex.get(current), incompatibleVertices));
					}
				}
			}
		}
		
		result.layoutOptions = original.layoutOptions.copy();result.learnerCache.invalidate();
		if (redVertex != null)
		{
			CmpVertex vertInSource = redVertex;
			EquivalenceClass<CmpVertex, LearnerGraphCachedData> redEqClass = origToEqClass.get(redVertex);
			if (redEqClass != null)
				vertInSource=redEqClass.getMergedVertex();
			result.learnerCache.stateLearnt=originalVertexToClonedVertex.get(vertInSource);
		}

		if (updateAuxInformation)
		{
			AMEquivalenceClass.populateCompatible(result, mergedForCompatibility);
			result.learnerCache.setMergedStates(mergedVertices);result.learnerCache.mergedToHardFacts=mergedToHard;
		}

		return result;
	}

	/** This is a very simplified version of an equivalence class, only for the use with {@link AMEquivalenceClass#populateCompatible(AbstractLearnerGraph, Collection)} 
	 * routine. Unlike a real equivalence class, it store much less information and hence takes up less space. 
	 */
	public static class EquivalenceClassForConstructionOfCompatibility implements EquivalenceClass<CmpVertex,LearnerGraphCachedData>
	{
		private final CmpVertex representative,merged;
		private final Collection<CmpVertex> incompatibleVertices;
		private final Set<CmpVertex> states = new TreeSet<CmpVertex>();

		public EquivalenceClassForConstructionOfCompatibility(CmpVertex representative, CmpVertex merged, Collection<CmpVertex> incompatibleVertices)
		{
			this.representative = representative;this.merged = merged;this.incompatibleVertices = incompatibleVertices;states.add(representative);
		}
		@Override
		public int getNumber() {
			throw new UnsupportedOperationException();
		}

		@Override
		public Map<Label, Object> getOutgoing() {
			throw new UnsupportedOperationException();
		}

		@Override
		public Set<CmpVertex> getStates() {
			return states;
		}

		@Override
		public CmpVertex getRepresentative() {
			return representative;
		}

		@SuppressWarnings("unused")
		@Override
		public boolean mergeWith(CmpVertex vert, Collection<Entry<Label, CmpVertex>> from) throws IncompatibleStatesException 
		{
			throw new UnsupportedOperationException();
		}

		@SuppressWarnings("unused")
		@Override
		public boolean mergeWith(EquivalenceClass<CmpVertex, LearnerGraphCachedData> to) throws IncompatibleStatesException 
		{
			throw new UnsupportedOperationException();
		}

		@SuppressWarnings("unused")
		@Override
		public int compareTo(EquivalenceClass<CmpVertex, LearnerGraphCachedData> o) {
			throw new UnsupportedOperationException();
		}

		@Override
		public CmpVertex getMergedVertex() {
			return merged;
		}

		@Override
		public Collection<CmpVertex> incompatibleStates() {
			return incompatibleVertices;
		}

		@SuppressWarnings("unused")
		@Override
		public <TARGET_C_TYPE, CACHE_C_TYPE extends CachedData<TARGET_C_TYPE, CACHE_C_TYPE>> void constructMergedVertex(
				AbstractLearnerGraph<TARGET_C_TYPE, CACHE_C_TYPE> graph, boolean useDifferentNameIfAlreadyExist,
				boolean setOrigState) {
			throw new UnsupportedOperationException();
		}

		@Override
		public int toInt() {
			throw new UnsupportedOperationException();
		}
		
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
	public static LearnerGraph mergeAndDeterminize_general(LearnerGraph original, StatePair pair, Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices)
	{

		if (original.pairscores.computePairCompatibilityScore_general(pair,null,mergedVertices, true) < 0)
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
		return mergeCollectionOfVertices(original,pair.getR(),mergedVertices,null,true);
	}

	/**
	 * Mergers a pair of states QSM-like. Does not merge compatibility information, use generalised merger if that is needed.
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
				DeterministicVertex source = (DeterministicVertex) e.getSource();
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
					fromSourceToNewRed = AbstractLearnerGraph.generateNewJungEdge(source,newRed);
					fromSourceToNewRed.setUserDatum(JUConstants.LABEL, existingLabels, UserData.CLONE);// no need to clone this one since I'll delete the edge in a bit
					g.addEdge(fromSourceToNewRed);
				}
				else
					// there is already a transition from source to newRed, hence all we have to do is merge the new labels into it.
					((Collection<Label>)fromSourceToNewRed.getUserDatum(JUConstants.LABEL)).addAll( existingLabels );
					
			}

			// now the elements of mergedVertices are in terms of the copied graph.
			for(DeterministicVertex vert:(Set<DeterministicVertex>)g.getVertices())
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
								DirectedSparseEdge newEdge = AbstractLearnerGraph.generateNewJungEdge(vert,targetVert);
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
