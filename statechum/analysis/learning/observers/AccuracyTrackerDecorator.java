/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
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
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import statechum.JUConstants;
import statechum.Pair;

import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.PrecisionRecall.PosNegPrecisionRecall;
import statechum.analysis.learning.rpnicore.*;
import statechum.apps.CompareGraphs;
import statechum.model.testset.PTASequenceEngine;

public class AccuracyTrackerDecorator extends LearnerDecorator
{
	protected Collection<List<String>> tests;
	protected LearnerGraph specfsm;
	protected List<List<ResultsContainer>> results;
	protected List<ResultsContainer> currentResults = null;

	public AccuracyTrackerDecorator(Learner learner, LearnerGraph target, Collection<List<String>> argTests){
		super(learner);

		results = new LinkedList<List<ResultsContainer>>();
		tests = argTests;
		specfsm = target;
	}

	
	public void trackResults(LearnerGraph graph) {
		PosNegPrecisionRecall pr = CompareGraphs.compare(tests, specfsm, graph);
		double accuracy = CompareGraphs.computeAccuracy(graph, specfsm, tests);
		ResultsContainer result = new ResultsContainer(accuracy, pr);
		currentResults.add(result);
	}
	
	public String resultsToString(){
		String returnString = new String();
		
		for (List<ResultsContainer> trajectory : results) {
			String trajectoryString = new String();
			for (ResultsContainer container : trajectory) {
				String resultString = container.getAccuracy()+","+
					container.getPr().getPosprecision()+","+
					container.getPr().getPosrecall()+","+
					container.getPr().getNegprecision()+","+
					container.getPr().getNegrecall()+"\n";
				trajectoryString = trajectoryString+resultString;
			}
			returnString = returnString + "\n"+trajectoryString;
		}
		return returnString;
	}
	
	public LearnerGraph learnMachine(Collection<List<String>> sPlus,  Collection<List<String>> sMinus){
		init(sPlus,sMinus);
		return learnMachine();
	}

	public LearnerGraph learnMachine()
	{
		currentResults = new LinkedList<ResultsContainer>();
		LearnerGraph result = decoratedLearner.learnMachine();
		results.add(currentResults);currentResults = new LinkedList<ResultsContainer>();
		return result;
	}

	public String getResult()
	{
		return decoratedLearner.getResult() + resultsToString();
	}

	public class ResultsContainer {
		private double accuracy;
		private PosNegPrecisionRecall posNegPrecisionRecall;
		
		public ResultsContainer(double acc, PosNegPrecisionRecall pr){
			accuracy = acc;
			posNegPrecisionRecall = pr;
		}
		
		public double getAccuracy() {
			return accuracy;
		}
		
		public PosNegPrecisionRecall getPr() {
			return posNegPrecisionRecall;
		}
		

	}

	public Pair<Integer, String> CheckWithEndUser(LearnerGraph graph, List<String> question, Object[] options) {
		return decoratedLearner.CheckWithEndUser(graph, question, options);
	}

	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) {
		trackResults(graph);
		return decoratedLearner.ChooseStatePairs(graph);
	}


	public List<List<String>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) {
		return decoratedLearner.ComputeQuestions(pair, original, temp);
	}


	public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) {
		return decoratedLearner.MergeAndDeterminize(original, pair);
	}


	public void Restart(RestartLearningEnum mode) {
		decoratedLearner.Restart(mode);
		if (mode != RestartLearningEnum.restartNONE)
		{
			results.add(currentResults);currentResults = new LinkedList<ResultsContainer>();
		}
	}


	public LearnerGraph init(Collection<List<String>> plus,	Collection<List<String>> minus) {
		return decoratedLearner.init(plus, minus);
	}
	
	public LearnerGraph init(PTASequenceEngine en, int plus, int minus) {
		return decoratedLearner.init(en, plus, minus);
	}


	public void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind,
			List<String> sequence, boolean accepted, JUConstants newColour) {
		decoratedLearner.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
	}
}
