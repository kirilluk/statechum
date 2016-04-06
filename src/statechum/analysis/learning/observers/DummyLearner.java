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
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.model.testset.PTASequenceEngine;

/** An implementation of <em>LearnerDecorator</em> which does nothing and
 * hence makes it easy to create instances of <em>LearnerDecorator</em>.
 * 
 * @author kirill
 */
public class DummyLearner extends LearnerDecorator 
{
	public DummyLearner(Learner learner) 
	{
		super(learner);
	}

	@Override 
	public void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind, List<Label> sequence,
			boolean accepted, JUConstants newColour) 
	{
		decoratedLearner.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
	}

	@Override 
	public Pair<Integer, String> CheckWithEndUser(LearnerGraph graph, List<Label> question, int responseForNoRestart, List<Boolean> acceptedElements, PairScore pairBeingMerged, Object[] options) 
	{
		return decoratedLearner.CheckWithEndUser(graph, question, responseForNoRestart, acceptedElements, pairBeingMerged, options);
	}

	@Override 
	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) {
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
	public LearnerGraph MergeAndDeterminize(LearnerGraph original,StatePair pair) 
	{
		return decoratedLearner.MergeAndDeterminize(original, pair);
	}

	@Override 
	public void Restart(RestartLearningEnum mode) 
	{
		decoratedLearner.Restart(mode);
	}

	@Override 
	public String getResult() 
	{
		return decoratedLearner.getResult();
	}

	@Override 
	public LearnerGraph init(Collection<List<Label>> plus,	Collection<List<Label>> minus) 
	{
		return decoratedLearner.init(plus, minus);
	}

	@Override 
	public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
	{
		return decoratedLearner.init(engine, plusSize, minusSize);
	}
	
	@Override
	public LearnerGraph learnMachine(Collection<List<Label>> plus, Collection<List<Label>> minus)
	{
		return decoratedLearner.learnMachine(plus,minus);
	}
	
	@Override
	public LearnerGraph learnMachine(PTASequenceEngine engine, int plusSize, int minusSize)
	{
		return decoratedLearner.learnMachine(engine,plusSize,minusSize);
	}

	@Override 
	public boolean AddConstraints(LearnerGraph graph, LearnerGraph outcome, StringBuffer counterExampleHolder) 
	{
		return decoratedLearner.AddConstraints(graph,outcome,counterExampleHolder);
	}

	@Override
	public ConvertALabel getLabelConverter() 
	{
		if (decoratedLearner == null)
			return super.getLabelConverter();
		return decoratedLearner.getLabelConverter();
	}
}