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

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;

import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.StatePair;
import statechum.xmachine.model.testset.PTATestSequenceEngine;

public class ComputeQuestions {
	final LearnerGraph coregraph;
	
	/** Associates this object to ComputeStateScores it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	ComputeQuestions(LearnerGraph computeStateScores) 
	{
		coregraph = computeStateScores;
	}

	private static class ExplorationElement
	{
		public final Pair<CmpVertex,CmpVertex> pair;
		public final PTATestSequenceEngine.sequenceSet pathsInOriginal;
		
		public ExplorationElement(CmpVertex first, CmpVertex second, PTATestSequenceEngine.sequenceSet paths)
		{
			pair = new Pair<CmpVertex, CmpVertex>(first,second);pathsInOriginal=paths;
		}
	}
	
	private static void buildQuestionsFromPair(
			LearnerGraph original, CmpVertex originalRed, 
			LearnerGraph merged, CmpVertex mergedRed, 
			PTATestSequenceEngine.sequenceSet pathsInOriginal)
	{
		Map<CmpVertex,List<String>> targetToInputSet = new TreeMap<CmpVertex,List<String>>();
		
		// now we build a sort of a "transition cover" from the tempRed state, in other words, take every vertex and 
		// build a path from tempRed to it, at the same time tracing it through the current machine.

		Set<CmpVertex> visitedStates = new HashSet<CmpVertex>();visitedStates.add(originalRed);
		Queue<ExplorationElement> currentExplorationBoundary = new LinkedList<ExplorationElement>();// FIFO queue containing vertices to be explored
		currentExplorationBoundary.add(new ExplorationElement(originalRed,mergedRed, pathsInOriginal));

		//List<String> nextInput = new ArrayList<String>(1);
		while(!currentExplorationBoundary.isEmpty())
		{
			ExplorationElement current = currentExplorationBoundary.remove();
			
			Map<String,CmpVertex> firstRow = merged.transitionMatrix.get(current.pair.firstElem),
				secondRow = original.transitionMatrix.get(current.pair.secondElem);
			targetToInputSet.clear();
			System.out.println("before multiplying: "+current.pathsInOriginal.getDebugData());
			PTATestSequenceEngine.sequenceSet multResult = current.pathsInOriginal.crossWithSet(firstRow.keySet());// transition cover of merged
			System.out.println("about to multiply by "+firstRow.keySet());
			System.out.println("after state cover: "+multResult.getDebugData());
			if (LearnerGraph.testMode)
			{
				Set<String> moreOrigInputs = new HashSet<String>();moreOrigInputs.addAll(secondRow.keySet());
				moreOrigInputs.removeAll(firstRow.keySet());
				assert moreOrigInputs.isEmpty() : 
					"inconsistent merge: merged automaton has fewer paths, merged: "+firstRow.keySet()+
					", original: "+secondRow.keySet();
			}
			System.out.println("from states "+current.pair.firstElem+","+current.pair.secondElem);
			for(Entry<String,CmpVertex> entry:firstRow.entrySet())
				// When generating questions for PTA merging, we should be trying 
				// to enter all states of the merged automaton - 
				// traversing states of the original one generates long sequences 
				// and is likely to lead to numerous spurious questions being generated 
				// because for a group of states merged into one, the closest one to the 
				// initial state will be the most representative (the further from the initial 
				// state, the more sparse a PTA becomes). The important point here is that the first state
				// of a group of merged states is the one closest to the initial state in the PTA.
				// In contrast, where states in an arbitrary automaton are being merged, 
				// we might be better off checking states further away
				// because they may just happen to contain more structure.
				if (!visitedStates.contains(entry.getValue()))
				{
					List<String> inputs = targetToInputSet.get(entry.getValue());
					if (inputs == null)
					{
						inputs = new LinkedList<String>();targetToInputSet.put(entry.getValue(),inputs);
					}
					inputs.add(entry.getKey());
				}
			
			for(Entry<CmpVertex,List<String>> target:targetToInputSet.entrySet())
			{
				visitedStates.add(target.getKey());
				CmpVertex newMergedVertex = firstRow.get(target.getValue().iterator().next());
				PTATestSequenceEngine.sequenceSet stateCoverSoFar = current.pathsInOriginal.crossWithSet(target.getValue()); 
				currentExplorationBoundary.offer(new ExplorationElement(target.getKey(), newMergedVertex, 
						stateCoverSoFar));
				System.out.println("\tnew pair: "+target.getKey()+","+newMergedVertex+
						" inputs: "+target.getValue());
			}
		}
		
	}

	/** Given a result of merging, computes a set of questions in the way it was implemented in 2007.
	 * 
	 * @param mergedRed
	 * @param pathsInOriginal
	 */ 
	private void buildQuestionsFromPair_Compatible(
			CmpVertex mergedRed,PTATestSequenceEngine.sequenceSet pathsInOriginal)
	{
		// now we build a sort of a "transition cover" from the tempRed state, in other words, take every vertex and 
		// build a path from tempRed to it, at the same time tracing it through the current machine.

		Set<CmpVertex> visitedStates = new HashSet<CmpVertex>();visitedStates.add(mergedRed);
		Queue<CmpVertex> currentExplorationBoundary = new LinkedList<CmpVertex>();// FIFO queue containing vertices to be explored
		Queue<PTATestSequenceEngine.sequenceSet> currentExplorationTargetStates = new LinkedList<PTATestSequenceEngine.sequenceSet>();
		currentExplorationBoundary.add(mergedRed);
		currentExplorationTargetStates.add(pathsInOriginal);

		Map<CmpVertex,List<String>> targetToInputSet = new TreeMap<CmpVertex,List<String>>();
		while(!currentExplorationBoundary.isEmpty())
		{
			CmpVertex currentVert = currentExplorationBoundary.remove();
			PTATestSequenceEngine.sequenceSet currentPaths = currentExplorationTargetStates.remove();
			targetToInputSet.clear();
			
			currentPaths.crossWithSet(coregraph.transitionMatrix.get(currentVert).keySet());
			for(Entry<String,CmpVertex> entry:coregraph.transitionMatrix.get(currentVert).entrySet())
				if (!visitedStates.contains(entry.getValue()))
				{
					List<String> inputs = targetToInputSet.get(entry.getValue());
					if (inputs == null)
					{
						inputs = new LinkedList<String>();targetToInputSet.put(entry.getValue(),inputs);
					}
					inputs.add(entry.getKey());
				}
			for(Entry<CmpVertex,List<String>> target:targetToInputSet.entrySet())
			{
				visitedStates.add(target.getKey());
				currentExplorationBoundary.offer(target.getKey());
				currentExplorationTargetStates.offer(currentPaths.crossWithSet(target.getValue()));
// KIRR: what is interesting is that even if crossWithSet delivers all-sink states (i.e. no transition we've taken from the 
// new red state is allowed by the original automaton), we still proceed to enumerate states. Another strange feature is 
// that we're taking shortest paths to all states in the new automaton, while there could be multiple different paths. 
// This means that it is possible that there would be paths to some states in the new automaton which will also be possible 
// in the original one, but will not be found because they are not the shortest ones. In turn, this means that many potential

			}
		}
	}

	// TODO to test with red = init, with and without loop around it (red=init and no loop is 3_1), with and without states which cannot be reached from a red state,
	// where a path in the original machine corresponding to a path in the merged one exists or not (tested with 3_1)
	/** Given a pair of states merged in a graph and the result of merging, 
	 * this method determines questions to ask.
	 * 
	 */
	public static Collection<List<String>> computeQS(final StatePair pair, LearnerGraph original, LearnerGraph merged)
	{
		CmpVertex mergedRed = merged.findVertex(pair.getR().toString());
		if (mergedRed == null)
			throw new IllegalArgumentException("failed to find the red state in the merge result");

		PTATestSequenceEngine engine = new PTATestSequenceEngine();
		engine.init(original.new NonExistingPaths());
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		PTATestSequenceEngine.sequenceSet initp = engine.new sequenceSet();initp.setIdentity();

		merged.paths.computePathsSBetween(merged.init,mergedRed, initp, paths);
		
		Collection<String> inputsToMultWith = new LinkedList<String>();
		for(Entry<String,CmpVertex> loopEntry:merged.transitionMatrix.get(mergedRed).entrySet())
			if (loopEntry.getValue() == mergedRed)
			{// Note an input corresponding to any loop in temp can be followed in the original machine, since
				// a loop in temp is either due to the merge or because it was there in the first place.
				inputsToMultWith.add(loopEntry.getKey());
			}
		paths.unite(paths.crossWithSet(inputsToMultWith));// the resulting path does a "transition cover" on all transitions leaving the red state.
		merged.questions.buildQuestionsFromPair_Compatible(mergedRed, paths);
		return engine.getData();
	}

}
