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
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Stack;

import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;

import statechum.analysis.learning.Learner;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.PrecisionRecall.PosNegPrecisionRecall;
import statechum.analysis.learning.rpnicore.*;
import statechum.apps.CompareGraphs;
import statechum.model.testset.PTASequenceEngine;

public class AccuracyTrackerDecorator extends LearnerDecorator
{
	protected Collection<List<Label>> tests, samples;
	protected LearnerGraph specfsm;
	protected List<List<ResultsContainer>> results;
	protected List<ResultsContainer> currentResults = null;

	public AccuracyTrackerDecorator(Learner learner, LearnerGraph target)
	{
		super(learner);

		results = new LinkedList<List<ResultsContainer>>();
		tests = target.wmethod.getFullTestSet(4);
		RandomPathGenerator rpg = new RandomPathGenerator(target, new Random(100),5,null);
		double nSquared = (target.getStateNumber()*target.getStateNumber())/2;
		int number = (int)(nSquared + (nSquared%2));
		rpg.generateRandomPosNeg(number, 1);
		
		samples = rpg.getAllSequences(0).getData(PTASequenceEngine.truePred);
		specfsm = target;
	}

	
	public void trackResults(LearnerGraph graph) {
		PosNegPrecisionRecall pr = CompareGraphs.compare(tests, specfsm, graph);
		double accuracy = CompareGraphs.computeAccuracy(graph, specfsm, samples);
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
	
	@Override
	public LearnerGraph learnMachine(Collection<List<Label>> sPlus,  Collection<List<Label>> sMinus)
	{
		init(sPlus,sMinus);
		return learnMachine();
	}

	@Override
	public LearnerGraph learnMachine()
	{
		currentResults = new LinkedList<ResultsContainer>();
		LearnerGraph result = decoratedLearner.learnMachine();
		results.add(currentResults);currentResults = new LinkedList<ResultsContainer>();
		return result;
	}

	@Override 
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

	@Override 
	public Pair<Integer, String> CheckWithEndUser(LearnerGraph graph, List<Label> question, int responseForNoRestart, List<Boolean> acceptedElements, PairScore pairBeingMerged, Object[] options) {
		return decoratedLearner.CheckWithEndUser(graph, question, responseForNoRestart, acceptedElements, pairBeingMerged, options);
	}

	@Override 
	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) {
		trackResults(graph);
		return decoratedLearner.ChooseStatePairs(graph);
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
		if (mode != RestartLearningEnum.restartNONE)
		{
			results.add(currentResults);currentResults = new LinkedList<ResultsContainer>();
		}
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
	public boolean AddConstraints(LearnerGraph graph, LearnerGraph outcome, StringBuffer counterExampleHolder) {
		return decoratedLearner.AddConstraints(graph,outcome,counterExampleHolder);
	}
}
