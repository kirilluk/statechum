/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
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

package statechum.analysis.learning.observers;

import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Stack;

import statechum.Helper;
import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;

import statechum.analysis.learning.Learner;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.*;
import statechum.model.testset.PTASequenceEngine;

public class ClusteringTrackerDecorator extends LearnerDecorator
{
	int counter = 0;

	public ClusteringTrackerDecorator(Learner learner)
	{
		super(learner);
	}
	
	@Override
	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) 
	{
		try {
			statechum.analysis.learning.util.ScoreMatrixOutput.writeMatrix(graph, "stage"+Integer.toString(counter));
		} catch (IOException e) {
			Helper.throwUnchecked("failed to write output score matrix", e);
		}
		counter++;
		return decoratedLearner.ChooseStatePairs(graph);
	}
	
	@Override
	public LearnerGraph learnMachine(Collection<List<Label>> sPlus,  Collection<List<Label>> sMinus){
		init(sPlus,sMinus);
		return learnMachine();
	}

	@Override
	public LearnerGraph learnMachine()
	{
		LearnerGraph result = decoratedLearner.learnMachine();
		return result;
	}

	@Override
	public String getResult()
	{
		return decoratedLearner.getResult();
	}

	@Override
	public Pair<Integer, String> CheckWithEndUser(LearnerGraph graph, List<Label> question, int responseForNoRestart, List<Boolean> acceptedElements, PairScore pairBeingMerged, Object[] options) {
		return decoratedLearner.CheckWithEndUser(graph, question, responseForNoRestart, acceptedElements, pairBeingMerged, options);
	}

	@Override
	public List<List<Label>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) {
		return decoratedLearner.ComputeQuestions(pair, original, temp);
	}

	@Override
	public List<List<Label>> RecomputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) {
		return decoratedLearner.RecomputeQuestions(pair, original, temp);
	}


	@Override
	public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) {
		return decoratedLearner.MergeAndDeterminize(original, pair);
	}


	@Override
	public void Restart(RestartLearningEnum mode) {
		decoratedLearner.Restart(mode);
	}


	@Override
	public LearnerGraph init(Collection<List<Label>> plus,	Collection<List<Label>> minus) {
		return decoratedLearner.init(plus, minus);
	}
	
	@Override
	public LearnerGraph init(PTASequenceEngine en, int plus, int minus) {
		return decoratedLearner.init(en, plus, minus);
	}

	@Override
	public void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind,
			List<Label> sequence, boolean accepted, JUConstants newColour) {
		decoratedLearner.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
	}

	@Override
	public boolean AddConstraints(LearnerGraph graph,LearnerGraph outcome,StringBuffer counterExampleHolder) {
		return decoratedLearner.AddConstraints(graph,outcome,counterExampleHolder);
	}
}
