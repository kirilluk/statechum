/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

statechum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/
package statechum.analysis.learning.observers;

import java.util.Collection;
import java.util.List;
import java.util.Stack;

import statechum.JUConstants;
import statechum.Pair;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
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

	public void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind, List<String> sequence,
			boolean accepted, JUConstants newColour) 
	{
		decoratedLearner.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
	}

	public Pair<Integer, String> CheckWithEndUser(LearnerGraph graph, List<String> question, int responseForNoRestart, Object[] options) 
	{
		return decoratedLearner.CheckWithEndUser(graph, question, responseForNoRestart, options);
	}

	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) {
		return decoratedLearner.ChooseStatePairs(graph);
	}

	public List<List<String>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) 
	{
		return decoratedLearner.ComputeQuestions(pair, original, temp);
	}

	public LearnerGraph MergeAndDeterminize(LearnerGraph original,StatePair pair) 
	{
		return decoratedLearner.MergeAndDeterminize(original, pair);
	}

	public void Restart(RestartLearningEnum mode) 
	{
		decoratedLearner.Restart(mode);
	}

	public String getResult() 
	{
		return decoratedLearner.getResult();
	}

	public LearnerGraph init(Collection<List<String>> plus,	Collection<List<String>> minus) 
	{
		return decoratedLearner.init(plus, minus);
	}

	public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
	{
		return decoratedLearner.init(engine, plusSize, minusSize);
	}
	
	@Override
	public LearnerGraph learnMachine(Collection<List<String>> plus, Collection<List<String>> minus)
	{
		return decoratedLearner.learnMachine(plus,minus);
	}
	
	@Override
	public LearnerGraph learnMachine(PTASequenceEngine engine, int plusSize, int minusSize)
	{
		return decoratedLearner.learnMachine(engine,plusSize,minusSize);
	}

	public LearnerGraph AddConstraints(LearnerGraph graph) 
	{
		return decoratedLearner.AddConstraints(graph);
	}
}