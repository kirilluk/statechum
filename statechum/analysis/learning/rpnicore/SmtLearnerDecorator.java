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
import java.util.Iterator;
import java.util.List;
import java.util.Stack;

import statechum.JUConstants;
import statechum.Pair;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.observers.DummyLearner;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.rpnicore.LabelRepresentation.AbstractState;
import statechum.model.testset.PTASequenceEngine;

public class SmtLearnerDecorator extends DummyLearner 
{
	protected final LabelRepresentation lbl;
	
	public SmtLearnerDecorator(Learner learner,LabelRepresentation labels) {
		super(learner);lbl=labels;
	}

	/**
	 * This one rebuilds a map from operations to values previously used in them, hence
	 * whenever a learner has new traces confirmed, the outcome in terms of values of low-level
	 * functions used can immediately be used.
	 * 
	 * @see statechum.analysis.learning.observers.DummyLearner#AugmentPTA(statechum.analysis.learning.rpnicore.LearnerGraph, statechum.analysis.learning.observers.Learner.RestartLearningEnum, java.util.List, boolean, statechum.JUConstants)
	 */
	@Override
	public void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind,
			List<String> sequence, boolean accepted, JUConstants newColour) {

		decoratedLearner.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
		lbl.buildVertexToAbstractStateMap(pta, null,true);
	}

	/**
	 * @see statechum.analysis.learning.observers.DummyLearner#AddConstraints(statechum.analysis.learning.rpnicore.LearnerGraph, statechum.analysis.learning.rpnicore.LearnerGraph, java.lang.StringBuffer)
	 */
	@Override
	public boolean AddConstraints(LearnerGraph graph, LearnerGraph outcome,	StringBuffer counterExampleHolder) 
	{
		boolean result = decoratedLearner.AddConstraints(graph, outcome, counterExampleHolder);
		if (!result) return false;
		lbl.buildVertexToAbstractStateMap(outcome, null,false);// do not rebuild maps.
		return lbl.checkConsistency(outcome,graph.config) == null;
	}
	
	/**
	 * @see statechum.analysis.learning.observers.DummyLearner#CheckWithEndUser(statechum.analysis.learning.rpnicore.LearnerGraph, java.util.List, int, java.lang.Object[])
	 */
	@Override
	public Pair<Integer, String> CheckWithEndUser(LearnerGraph graph,
			List<String> question, int responseForNoRestart, List<Boolean> acceptedElements, Object[] options) 
	{
		int smtAnswer = lbl.CheckWithEndUser(question);
		System.err.println("question: "+question+" expected for no restart: "+responseForNoRestart+" smt returned: "+smtAnswer);
		if (smtAnswer != responseForNoRestart)
			return new Pair<Integer, String>(smtAnswer,null);
		
		return decoratedLearner.CheckWithEndUser(graph, question, responseForNoRestart, acceptedElements, options);
	}
	
	@Override
	public LearnerGraph MergeAndDeterminize(LearnerGraph original,StatePair pair) 
	{
		LearnerGraph result = decoratedLearner.MergeAndDeterminize(original, pair);
		lbl.buildVertexToAbstractStateMap(result, original,false);// update the map from vertices to the corresponding collections of abstract states
		return result;
	}

	/**
	 * @see statechum.analysis.learning.observers.DummyLearner#ChooseStatePairs(statechum.analysis.learning.rpnicore.LearnerGraph)
	 */
	@Override
	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) 
	{
		decoratedLearner.ChooseStatePairs(graph);
		
		// Now go through the pairs and update the scores.
		for(int i=0;i<graph.pairsAndScores.size();++i)
		{
			PairScore pair = graph.pairsAndScores.get(i);
			boolean finished = false, statesIntersect = false;// using these two variables I can choose whether to check for intersection or non-intersection.
			if (pair.firstElem.isAccept() && pair.secondElem.isAccept())
			{
				Iterator<AbstractState> 
					stateA_iter = graph.getVertexToAbstractState().get(pair.firstElem).iterator(),
					stateB_iter = graph.getVertexToAbstractState().get(pair.secondElem).iterator();
				while(stateA_iter.hasNext() && !finished)
				{
					AbstractState stateA = stateA_iter.next();
					while(stateB_iter.hasNext() && !finished)
						if (lbl.abstractStatesCompatible(stateA, stateB_iter.next()))
						{
							finished = true;statesIntersect = true;
						}
				}
			}
			if (statesIntersect)
			{
				System.err.println("bumped score for "+new StatePair(pair.firstElem, pair.secondElem));
				graph.pairsAndScores.set(i, new PairScore(pair.firstElem, pair.secondElem,
						pair.getScore()+1,pair.getAnotherScore()));
			}
			else
				System.err.println("no bump for "+new StatePair(pair.firstElem, pair.secondElem));
		}
		return graph.pairscores.getSortedPairsAndScoresStackFromUnsorted();
	}

	@Override
	public LearnerGraph init(Collection<List<String>> plus,	Collection<List<String>> minus) 
	{
		LearnerGraph result= decoratedLearner.init(plus, minus);
		lbl.buildVertexToAbstractStateMap(result, null, true);// construct the initial version of the 
		// map associating vertices with those these vertices were built from; this map is subsequently 
		// updated when a merged automaton is built.
		return result;
	}

	@Override
	public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
	{
		LearnerGraph result= decoratedLearner.init(engine, plusSize, minusSize);
		lbl.buildVertexToAbstractStateMap(result, null, true);// construct the initial version of the 
		// map associating vertices with those these vertices were built from; this map is subsequently 
		// updated when a merged automaton is built.
		return result;
	}

	/**
	 * @see statechum.analysis.learning.observers.DummyLearner#Restart(statechum.analysis.learning.observers.Learner.RestartLearningEnum)
	 */
	@Override
	public void Restart(RestartLearningEnum mode) 
	{
		decoratedLearner.Restart(mode);
	}
}
