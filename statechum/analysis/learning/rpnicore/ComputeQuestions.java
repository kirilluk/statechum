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

import statechum.ArrayOperations;
import statechum.Configuration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.StatePair;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

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

	public interface QuestionConstructor 
	{
		/** Constructs an engine which is used to store trees of sequences representing questions to ask. */
		public PTASequenceEngine constructEngine(LearnerGraph original, LearnerGraph learnt);
		
		/** Called for each state in the merged machine. Expected to update the collection of questions.
		 * 
		 * @param state the equivalence class to process
		 * @param original the original graph
		 * @param learnt the result of merging
		 * @param pairOrig the pair of states which was merged in the original graph
		 * @param stateLearnt the state in the merged graph corresponding to the red 
		 * and blue states of the original graph.
		 */
		public void addQuestionsForState(AMEquivalenceClass state, LearnerGraph original, LearnerGraph learnt, 
				StatePair pairOrig,CmpVertex stateLearnt,MergeData data);
	}
	
	/** We may be able to supply question generate with lots of different pieces of data, but
	 * not all of it will be needed - no point wasting time computing irrelevant stuff.
	 */
	public interface MergeData
	{
		/** Returns shortest paths in the original graph to the blue state. */
		public SequenceSet getPathsToBlue();
		/** Returns shortest paths in the original graph to the red state. */
		public SequenceSet getPathsToRed();
		/** Returns shortest paths in the learnt graph to the stateLearnt state. */
		public SequenceSet getPathsToLearnt();
	}
	
	public static List<List<String>> computeQS_general(final StatePair pairToMerge, 
			final LearnerGraph original, final LearnerGraph learnt,final QuestionConstructor qConstructor)
	{
		final PTASequenceEngine engine = qConstructor.constructEngine(original, learnt);
		
		final SequenceSet identity = engine.new SequenceSet();identity.setIdentity();
		for(AMEquivalenceClass eq:learnt.learnerCache.getMergedStates())
			qConstructor.addQuestionsForState(eq, original, learnt, pairToMerge, 
					learnt.stateLearnt,new MergeData(){
				public SequenceSet getPathsToBlue() 
				{
					SequenceSet toBlue = engine.new SequenceSet();
					original.paths.computePathsSBetween(original.init, pairToMerge.getQ(), identity, toBlue);
					return toBlue;
				}

				public SequenceSet getPathsToRed() 
				{
					SequenceSet toRed = engine.new SequenceSet();
					original.paths.computePathsSBetween(original.init, pairToMerge.getR(), identity, toRed);
					return toRed;
				}

				public SequenceSet getPathsToLearnt() 
				{
					SequenceSet toLearnt = engine.new SequenceSet();
					learnt.paths.computePathsSBetween(learnt.init, learnt.stateLearnt, identity, toLearnt);
					return toLearnt;
				}

			});
		
		return engine.getData();
	}
	
	/** Replicates QSM question generator using the new question generation framework. */
	static public class QSMQuestionGenerator implements QuestionConstructor
	{
		private PTASequenceEngine engine = null;
		private Map<CmpVertex,PTASequenceEngine.SequenceSet> fanout = null;
		
		public PTASequenceEngine constructEngine(LearnerGraph original, @SuppressWarnings("unused") LearnerGraph learnt) 
		{
			engine = new PTASequenceEngine();
			engine.init(original.new NonExistingPaths());
			return engine;
		}

		public void addQuestionsForState(AMEquivalenceClass state, 
				LearnerGraph original, LearnerGraph learnt, 
				@SuppressWarnings("unused") StatePair pairOrig, CmpVertex stateLearnt,
				MergeData data) 
		{
			if (fanout == null)
			{// Initialisation
				Collection<String> inputsToMultWith = new LinkedList<String>();
				for(Entry<String,CmpVertex> loopEntry:learnt.transitionMatrix.get(stateLearnt).entrySet())
					if (loopEntry.getValue() == stateLearnt)
					{// Note an input corresponding to any loop in temp can be followed in the original machine, since
					 // a loop in temp is either due to the merge or because it was there in the first place.
						inputsToMultWith.add(loopEntry.getKey());
					}
				SequenceSet pathsToMergedRed = data.getPathsToLearnt();
				pathsToMergedRed.unite(pathsToMergedRed.crossWithSet(inputsToMultWith));// the resulting path does a "transition cover" on all transitions leaving the red state.
				
				// Now we limit the number of elements in pathsToMerged to the value specified in the configuration.
				// This will not affect the underlying graph, but it does not really matter since all
				// elements in that graph are accept-states by construction of pathsToMergedRed and hence
				// not be returned.
				pathsToMergedRed.limitTo(original.config.getQuestionPathUnionLimit());
				
				fanout = learnt.paths.computePathsSBetween_All(stateLearnt, engine, pathsToMergedRed);
			}
						
			SequenceSet pathsToCurrentState = fanout.get(state.mergedVertex);
			if (pathsToCurrentState != null)
			{
				assert state.mergedVertex.getColour() != JUConstants.AMBER;
				
				// if a path from the merged red state to the current one can be found, update the set of questions. 
				pathsToCurrentState.crossWithSet(learnt.transitionMatrix.get(state.mergedVertex).keySet());
				// Note that we do not care what the result of crossWithSet is - for those states which 
				// do not exist in the underlying graph, reject vertices will be added by the engine and
				// hence will be returned when we do a .getData() on the engine.
			}
		}
		
	}
	/** Improves on the QSM question generator by using a real loop rather than a single-transition loop. */
	static public class QSMQuestionGeneratorImproved implements QuestionConstructor
	{
		private PTASequenceEngine engine = null;
		private Map<CmpVertex,PTASequenceEngine.SequenceSet> fanout = null;

		public PTASequenceEngine constructEngine(LearnerGraph original, @SuppressWarnings("unused") LearnerGraph learnt) 
		{
			engine = new PTASequenceEngine();
			engine.init(original.new NonExistingPaths());
			return engine;
		}

		public void addQuestionsForState(AMEquivalenceClass state, 
				LearnerGraph original, LearnerGraph learnt, 
				StatePair pairOrig, CmpVertex stateLearnt,
				MergeData data) 
		{
			if (fanout == null)
			{// Initialisation
				SequenceSet pathsToRed = data.getPathsToLearnt();
				SequenceSet pathsToMergedRed=engine.new SequenceSet();pathsToMergedRed.unite(pathsToRed);
				original.paths.computePathsSBetweenBoolean(pairOrig.getR(), pairOrig.getQ(), pathsToRed, pathsToMergedRed);
				
				// Now we limit the number of elements in pathsToMerged to the value specified in the configuration.
				// This will not affect the underlying graph, but it does not really matter since all
				// elements in that graph are accept-states by construction of pathsToMergedRed and hence
				// not be returned.
				pathsToMergedRed.limitTo(original.config.getQuestionPathUnionLimit());
				
				fanout = learnt.paths.computePathsSBetween_All(stateLearnt, engine, pathsToMergedRed);
			}
						
			SequenceSet pathsToCurrentState = fanout.get(state.mergedVertex);
			if (pathsToCurrentState != null)
				// if a path from the merged red state to the current one can be found, update the set of questions. 
				pathsToCurrentState.crossWithSet(learnt.transitionMatrix.get(state.mergedVertex).keySet());
				// Note that we do not care what the result of crossWithSet is - for those states which 
				// do not exist in the underlying graph, reject vertices will be added by the engine and
				// hence will be returned when we do a .getData() on the engine.
		}
		
	}

	/** The question generator which should work for all possible kinds of mergers. */
	static public class SymmetricQuestionGenerator implements QuestionConstructor
	{
		private PTASequenceEngine engine = null;
		private Map<CmpVertex,PTASequenceEngine.SequenceSet> fanout = null;

		public PTASequenceEngine constructEngine(LearnerGraph original, @SuppressWarnings("unused") LearnerGraph learnt) 
		{
			engine = new PTASequenceEngine();
			engine.init(original.new NonExistingPaths());
			return engine;
		}

		public void addQuestionsForState(AMEquivalenceClass state, 
				LearnerGraph original, LearnerGraph learnt, 
				@SuppressWarnings("unused") StatePair pairOrig, @SuppressWarnings("unused") CmpVertex stateLearnt,
				@SuppressWarnings("unused") MergeData data) 
		{
			if (fanout == null)
			{
				SequenceSet pathsToInitState = engine.new SequenceSet();pathsToInitState.setIdentity();
				fanout = original.paths.computePathsSBetween_All(original.init, engine, pathsToInitState);
			}
			
			for(CmpVertex vert:state.vertices)
			{
				SequenceSet pathsToCurrentState = fanout.get(vert);
				if (pathsToCurrentState != null)
				{
					pathsToCurrentState.limitTo(original.config.getQuestionPathUnionLimit());
					pathsToCurrentState.crossWithSet(learnt.transitionMatrix.get(state.mergedVertex).keySet());// attempt all possible continuation vertices
				}
			}
		}
		
	}
	
	/** Given a result of merging, computes a set of questions in the way it was implemented in 2007.
	 * 
	 * @param mergedRed
	 * @param pathsInOriginal
	 */ 
	private void buildQuestionsFromPair_Compatible(
			CmpVertex mergedRed,PTASequenceEngine.SequenceSet pathsInOriginal)
	{
		// now we build a sort of a "transition cover" from the tempRed state, in other words, take every vertex and 
		// build a path from tempRed to it, at the same time tracing it through the current machine.

		Set<CmpVertex> visitedStates = new HashSet<CmpVertex>();visitedStates.add(mergedRed);
		Queue<CmpVertex> currentExplorationBoundary = new LinkedList<CmpVertex>();// FIFO queue containing vertices to be explored
		Queue<PTASequenceEngine.SequenceSet> currentExplorationTargetStates = new LinkedList<PTASequenceEngine.SequenceSet>();
		currentExplorationBoundary.add(mergedRed);
		currentExplorationTargetStates.add(pathsInOriginal);

		Map<CmpVertex,List<String>> targetToInputSet = new TreeMap<CmpVertex,List<String>>();
		while(!currentExplorationBoundary.isEmpty())
		{
			CmpVertex currentVert = currentExplorationBoundary.remove();
			PTASequenceEngine.SequenceSet currentPaths = currentExplorationTargetStates.remove();
			targetToInputSet.clear();
			
			currentPaths.crossWithSet(coregraph.transitionMatrix.get(currentVert).keySet());
			for(Entry<String,CmpVertex> entry:coregraph.transitionMatrix.get(currentVert).entrySet())
				if (!visitedStates.contains(entry.getValue()) && entry.getValue().getColour() != JUConstants.AMBER)
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
// in the original one, but will not be found because they are not the shortest ones. 
			}
		}
	}

	// TODO to test with red = init, with and without loop around it (red=init and no loop is 3_1), with and without states which cannot be reached from a red state,
	// where a path in the original machine corresponding to a path in the merged one exists or not (tested with 3_1)
	/** Given a pair of states merged in a graph and the result of merging, 
	 * this method determines questions to ask.
	 */
	public static List<List<String>> computeQS_orig(final StatePair pair, LearnerGraph original, LearnerGraph merged)
	{
		CmpVertex mergedRed = merged.findVertex(pair.getR().getID());
		if (mergedRed == null)
			throw new IllegalArgumentException("failed to find the red state in the merge result");
		
		PTASequenceEngine engine = new PTASequenceEngine();
		engine.init(original.new NonExistingPaths());
		PTASequenceEngine.SequenceSet paths = engine.new SequenceSet();
		PTASequenceEngine.SequenceSet initp = engine.new SequenceSet();initp.setIdentity();

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
	
	// TODO to test with red = init, with and without loop around it (red=init and no loop is 3_1), with and without states which cannot be reached from a red state,
	// where a path in the original machine corresponding to a path in the merged one exists or not (tested with 3_1)
	/** Given a pair of states merged in a graph and the result of merging, 
	 * this method determines questions to ask.
	 */
	public static List<List<String>> computeQS_origReduced(final StatePair pair, LearnerGraph original, LearnerGraph merged)
	{
		CmpVertex mergedRed = merged.findVertex(pair.getR().getID());
		if (mergedRed == null)
			throw new IllegalArgumentException("failed to find the red state in the merge result");
		
		PTASequenceEngine engine = new PTASequenceEngine();
		engine.init(original.new NonExistingPaths());
		PTASequenceEngine.SequenceSet paths = engine.new SequenceSet();
		PTASequenceEngine.SequenceSet initp = engine.new SequenceSet();initp.setIdentity();

		List<Collection<String>> sequenceOfSets = merged.paths.computePathsSBetween(merged.init,mergedRed);
		if (sequenceOfSets == null)
			throw new IllegalArgumentException("failed to find the red state in the merge result");
		for(Collection<String> inputsToMultWith:sequenceOfSets)
			initp = initp.crossWithSet(inputsToMultWith);
		paths.unite(initp);
		//merged.paths.computePathsSBetweenBooleanReduced(merged.init,mergedRed, initp, paths);
		
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
	
	public static Collection<List<String>> computeQS_getpartA(final StatePair pair, LearnerGraph original, LearnerGraph merged)
	{
		CmpVertex mergedRed = merged.findVertex(pair.getR().getID());
		if (mergedRed == null)
			throw new IllegalArgumentException("failed to find the red state in the merge result");
		
		PTASequenceEngine engine = new PTASequenceEngine();
		engine.init(original.new NonExistingPaths());
		PTASequenceEngine.SequenceSet initp = engine.new SequenceSet();initp.setIdentity();
		merged.questions.buildQuestionsFromPair_Compatible(mergedRed, initp);
		return engine.getData(PTASequenceEngine.truePred);
	}
	
	/** Given a pair of states merged in a graph and the result of merging, 
	 * this method determines questions to ask.
	 */
	public static List<List<String>> computeQS(final StatePair pair, LearnerGraph original, LearnerGraph merged)
	{
		List<List<String>> questions = null;
		if (original.config.getQuestionGenerator() == Configuration.QuestionGeneratorKind.ORIGINAL)
			questions = computeQS_orig(new StatePair(merged.getStateLearnt(),merged.getStateLearnt()), original, merged);
		else
		{
			QuestionConstructor qConstructor=null;
			switch(original.config.getQuestionGenerator())
			{
				case CONVENTIONAL: qConstructor=new QSMQuestionGenerator();break;
				case CONVENTIONAL_IMPROVED:qConstructor=new QSMQuestionGeneratorImproved();break; 
				case SYMMETRIC:qConstructor=new SymmetricQuestionGenerator();break;
				case ORIGINAL:assert false;break;// should not be reached because it is handled at the top of this routine.
			}
			questions = computeQS_general(pair, original, merged, qConstructor);
		}
		return ArrayOperations.sort(questions);
	}
}
