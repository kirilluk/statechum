/*
 * Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

import java.io.File;
import java.io.IOException;
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
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
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

	/** Verifies that vertices with the same name have the same colour in the two graphs,
	 * in addition to checking for isomorphism of the graphs.
	 * Used for consistency checking.
	 */
	public static void verifySameMergeResults(LearnerGraph A, LearnerGraph B)
	{
		WMethod.checkM(A, B);
		for(Entry<CmpVertex,LinkedList<String>> entry:A.wmethod.computeShortPathsToAllStates().entrySet())
		{
			CmpVertex Bstate = B.getVertex(entry.getValue());
			CmpVertex Astate = entry.getKey();
			if (Bstate.getColour() != Astate.getColour())
				throw new DifferentFSMException("states "+ Astate + " (" +
						((Astate.getColour() == null)?"no color":Astate.getColour())+") and "+Bstate+" ("+
						((Bstate.getColour() == null)?"no color":Bstate.getColour())+") have different colours");
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
	public static LearnerGraph mergeAndDeterminize_general(LearnerGraph original, StatePair pair)
	{
		assert original.transitionMatrix.containsKey(pair.firstElem);
		assert original.transitionMatrix.containsKey(pair.secondElem);
		Collection<Collection<CmpVertex>> mergedVertices = new LinkedList<Collection<CmpVertex>>();
		return mergeAndDeterminize_general(original,pair,mergedVertices);
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
	public static LearnerGraph mergeAndDeterminize_general(LearnerGraph original, StatePair pair,
			Collection<Collection<CmpVertex>> mergedVertices)
	{
		LearnerGraph result = new LearnerGraph(original.config);
		result.initEmpty();
		result.transitionMatrix = new TreeMap<CmpVertex,Map<String,CmpVertex>>(); 

		if (original.pairscores.computePairCompatibilityScore_general(pair,mergedVertices) < 0)
		{
			try {
				String failName= new File("resources","failedmerge_"+pair.getR().getID()+"_"+pair.getQ().getID()+".xml").getAbsolutePath();
				original.transform.writeGraphML(failName);
			} catch (IOException e) {
				System.out.println("failed to write error file");
				e.printStackTrace();
			}
			throw new IllegalArgumentException("elements of the pair "+pair+" are incompatible, orig score was "+original.pairscores.computePairCompatibilityScore(pair));
		}
		Collection<AMEquivalenceClass> mergedStates = new LinkedList<AMEquivalenceClass>();
		Configuration cloneConfig = result.config.copy();cloneConfig.setLearnerCloneGraph(true);
		// Build a map from old vertices to the corresponding equivalence classes
		Map<CmpVertex,AMEquivalenceClass> origToNew = new HashMap<CmpVertex,AMEquivalenceClass>();
		for(Collection<CmpVertex> eqClass:mergedVertices)
		{
			AMEquivalenceClass equivalenceClass = new AMEquivalenceClass(eqClass);
			equivalenceClass.mergedVertex=LearnerGraph.cloneCmpVertex(equivalenceClass.mergedVertex, cloneConfig);equivalenceClass.computeMergedColour();
			mergedStates.add(equivalenceClass);
			
			for(CmpVertex v:eqClass)
				origToNew.put(v, equivalenceClass);
		}
		result.init = origToNew.get(original.init).mergedVertex;
		result.vertNegativeID = original.vertNegativeID;result.vertPositiveID=original.vertPositiveID;
		result.stateLearnt=origToNew.get(pair.getR()).mergedVertex;
		Queue<AMEquivalenceClass> currentExplorationBoundary = new LinkedList<AMEquivalenceClass>();// FIFO queue containing vertices to be explored
		currentExplorationBoundary.add(origToNew.get(original.init));
		Set<AMEquivalenceClass> visitedEqClasses = new HashSet<AMEquivalenceClass>();
		while(!currentExplorationBoundary.isEmpty())
		{// In order to build a new transition diagram consisting of equivalence classes, I need to
		 // navigate the existing transition diagram, in its entirety.
			AMEquivalenceClass current = currentExplorationBoundary.remove();
			Map<String,CmpVertex> row = result.transitionMatrix.get(current.mergedVertex);
			if (row == null)
			{
				row = new TreeMap<String,CmpVertex>();result.transitionMatrix.put(current.mergedVertex, row);
			}

			for(CmpVertex equivalentVertex:current.vertices)
				for(Entry<String,CmpVertex> entry:original.transitionMatrix.get(equivalentVertex).entrySet())
				{
					AMEquivalenceClass nextClass = origToNew.get(entry.getValue());
					if (!visitedEqClasses.contains(nextClass))
					{// have not yet visited this class
						currentExplorationBoundary.offer(nextClass);
						visitedEqClasses.add(nextClass);
					}
					if (LearnerGraph.testMode && row.containsKey(entry.getKey()))
						assert row.get(entry.getKey()) == nextClass.mergedVertex;
					row.put(entry.getKey(), nextClass.mergedVertex);
				}	
		}
		result.learnerCache.invalidate();result.learnerCache.setMergedStates(mergedStates);
		return result;
	}
	
	public static LearnerGraph mergeAndDeterminize(LearnerGraph original,StatePair pair)
	{
		if (LearnerGraph.testMode) { PathRoutines.checkPTAConsistency(original, pair.getQ());PathRoutines.checkPTAIsTree(original,null,null,null); }
		assert original.transitionMatrix.containsKey(pair.firstElem);
		assert original.transitionMatrix.containsKey(pair.secondElem);
		Map<CmpVertex,List<CmpVertex>> mergedVertices = new HashMap<CmpVertex,List<CmpVertex>>();
		LearnerGraph result;
		Configuration shallowCopy = original.config.copy();shallowCopy.setLearnerCloneGraph(false);
		result = original.copy(shallowCopy);
		assert result.transitionMatrix.containsKey(pair.firstElem);
		assert result.transitionMatrix.containsKey(pair.secondElem);
		if (LearnerGraph.testMode) PathRoutines.checkPTAConsistency(result, pair.getQ());
		
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
		
		if (LearnerGraph.testMode) PathRoutines.checkPTAIsTree(result, original, pair,ptaVerticesUsed);
		
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
