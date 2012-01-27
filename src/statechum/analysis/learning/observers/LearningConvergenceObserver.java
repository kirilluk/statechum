/* Copyright (C) 2012 The University of Sheffield.
 * 
 * This file is part of StateChum.
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

import statechum.JUConstants;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.model.testset.PTASequenceEngine;

public class LearningConvergenceObserver extends DummyLearner {

	public LearningConvergenceObserver(Learner learner) {
		super(learner);
	}
	
	private double lastProgress=-1;
	
	@Override 
	public List<List<Label>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) 
	{
		computeProgress(original);
		return decoratedLearner.ComputeQuestions(pair, original, temp);
	}

	@Override 
	public List<List<Label>> RecomputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) 
	{
		computeProgress(original);
		return decoratedLearner.RecomputeQuestions(pair, original, temp);
	}

	private void computeProgress(LearnerGraph graph)
	{
		double outcome = 0.;
		int redStates = 0;
		for(CmpVertex v:graph.transitionMatrix.keySet())
			if (v.getColour() == JUConstants.RED)
				++redStates;
		
		if (!graph.transitionMatrix.isEmpty()) outcome = ((double)redStates)/graph.transitionMatrix.size();
		lastProgress = outcome;
	}
	
	public List<Double> progressObserved = new java.util.LinkedList<Double>();
	
	protected void logProgress()
	{
		assert lastProgress >= 0.;
		progressObserved.add(lastProgress);
		System.out.println(lastProgress);
		lastProgress=-1;
	}
	
	@Override 
	public void Restart(RestartLearningEnum mode) 
	{
		if (mode == RestartLearningEnum.restartHARD || 
				mode == RestartLearningEnum.restartSOFT)
		{
			logProgress();
		}
		decoratedLearner.Restart(mode);
	}

	@Override
	public LearnerGraph learnMachine(Collection<List<Label>> plus, Collection<List<Label>> minus)
	{
		LearnerGraph result = decoratedLearner.learnMachine(plus,minus);
		computeProgress(result);logProgress();
		return result;
	}
	
	@Override
	public LearnerGraph learnMachine(PTASequenceEngine engine, int plusSize, int minusSize)
	{
		LearnerGraph result = decoratedLearner.learnMachine(engine,plusSize,minusSize);
		computeProgress(result);logProgress();
		return result;
	}
	
	@Override
	public LearnerGraph learnMachine()
	{
		LearnerGraph result = decoratedLearner.learnMachine();
		computeProgress(result);logProgress();
		return result;
	}

}
