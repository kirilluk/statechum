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

/**
 * This decorator lets you supply the target model so that questions are answered automatically
 */

import java.util.Collection;
import java.util.List;
import java.util.Stack;

import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.model.testset.PTASequenceEngine;

public class MachineOracleDecorator extends LearnerDecorator {

	LearnerGraph target = null;
	int restarts=0;int questions=0;
	
	public MachineOracleDecorator(Learner learner, LearnerGraph argTarget) {
		super(learner);
		this.target=argTarget;
	}

	@Override 
	public boolean AddConstraints(LearnerGraph graph, LearnerGraph outcome, StringBuffer counterExampleHolder) {
		return decoratedLearner.AddConstraints(graph,outcome,counterExampleHolder);
	}

	@Override 
	public void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind,
			List<Label> sequence, boolean accepted, JUConstants newColour) {
			decoratedLearner.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
	}

	@Override 
	public Pair<Integer, String> CheckWithEndUser(@SuppressWarnings("unused") LearnerGraph graph,
			List<Label> question, @SuppressWarnings("unused") int expectedAccept, 
			@SuppressWarnings("unused") List<Boolean> acceptedElements,
			@SuppressWarnings("unused") PairScore pairBeingMerged,
			@SuppressWarnings("unused") Object[] options) {
		questions++;
		int answer = target.paths.tracePathPrefixClosed(question);
		return new Pair<Integer, String>(answer,null);
	}

	@Override 
	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) 
	{
		return decoratedLearner.ChooseStatePairs(graph);
	}

	@Override 
	public List<List<Label>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) 
	{
		return decoratedLearner.ComputeQuestions(pair, original, temp);
	}

	@Override 
	public List<List<Label>> RecomputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) 
	{
		return decoratedLearner.RecomputeQuestions(pair, original, temp);
	}

	@Override 
	public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
	{
		return decoratedLearner.MergeAndDeterminize(original, pair);
	}

	@Override 
	public void Restart(RestartLearningEnum mode) {
		restarts++;
		decoratedLearner.Restart(mode);

	}

	@Override 
	public String getResult() {
		return decoratedLearner.getResult()+","+questions+","+restarts;
	}

	@Override 
	public LearnerGraph init(Collection<List<Label>> plus,	Collection<List<Label>> minus) {
		return decoratedLearner.init(plus,minus);
	}

	@Override 
	public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) {
		return decoratedLearner.init(engine,plusSize,minusSize);
	}

}
