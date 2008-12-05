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
import java.util.List;
import java.util.Stack;

import statechum.JUConstants;
import statechum.Pair;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.observers.DummyLearner;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.Smt;
import statechum.model.testset.PTASequenceEngine;

public class SmtLearnerDecorator extends DummyLearner {

	protected final LabelRepresentation lbl;
	protected final Smt solver;
	
	public SmtLearnerDecorator(Learner learner,LabelRepresentation labels) {
		super(learner);lbl=labels;
		
		Smt.loadLibrary();Smt.closeStdOut();solver = new Smt();
	}

	/* (non-Javadoc)
	 * @see statechum.analysis.learning.observers.DummyLearner#AugmentPTA(statechum.analysis.learning.rpnicore.LearnerGraph, statechum.analysis.learning.observers.Learner.RestartLearningEnum, java.util.List, boolean, statechum.JUConstants)
	 */
	@Override
	public void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind,
			List<String> sequence, boolean accepted, JUConstants newColour) {

		decoratedLearner.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
		if (ptaKind == RestartLearningEnum.restartHARD)
			lbl.AugmentAbstractStates(solver,sequence, pta,accepted);
	}

	/* (non-Javadoc)
	 * @see statechum.analysis.learning.observers.DummyLearner#CheckWithEndUser(statechum.analysis.learning.rpnicore.LearnerGraph, java.util.List, int, java.lang.Object[])
	 */
	@Override
	public Pair<Integer, String> CheckWithEndUser(LearnerGraph graph,
			List<String> question, int responseForNoRestart, Object[] options) 
	{
		int smtAnswer = lbl.CheckWithEndUser(solver, graph, question);
		System.err.println("question: "+question+" expected for no restart: "+responseForNoRestart+" smt returned: "+smtAnswer);
		if (smtAnswer != responseForNoRestart)
			return new Pair<Integer, String>(smtAnswer,null);
		
		return decoratedLearner.CheckWithEndUser(graph, question, responseForNoRestart, options);
	}

	/* (non-Javadoc)
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
			if (lbl.abstractStatesCompatible(solver, LabelRepresentation.getID(pair.firstElem), LabelRepresentation.getID(pair.secondElem)))
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
		lbl.mapVerticesToAbstractStates(result);return result;
	}

	@Override
	public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
	{
		LearnerGraph result= decoratedLearner.init(engine, plusSize, minusSize);
		lbl.mapVerticesToAbstractStates(result);return result;
	}
}
