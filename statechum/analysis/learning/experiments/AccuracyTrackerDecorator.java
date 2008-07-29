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

package statechum.analysis.learning.experiments;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Observable;
import java.util.Observer;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import statechum.analysis.learning.*;


import statechum.analysis.learning.rpnicore.*;
import statechum.model.testset.PTASequenceEngine;

public class AccuracyTrackerDecorator extends LearnerDecorator implements Observer{

	protected Collection<List<String>> tests;
	protected LearnerGraph specfsm;
	protected List<List<ResultsContainer>> results;
	protected List<ResultsContainer> currentResults;
	
	public AccuracyTrackerDecorator(Observable decoratedLearner, LearnerGraph target, Collection<List<String>> tests){
		super((Learner)decoratedLearner);
		decoratedLearner.addObserver(AccuracyTrackerDecorator.this);
		results = new ArrayList<List<ResultsContainer>>();
		this.tests = tests;
		this.specfsm = target;
		this.currentResults = new ArrayList<ResultsContainer>();
	}

	
	public void trackResults(LearnerState ls) {
		int iterations = ls.getIterations();
		LearnerGraph lg = ls.getResult();
		PosNegPrecisionRecall pr = CompareGraphs.compare(tests, specfsm, lg);
		double accuracy = CompareGraphs.computeAccuracy(lg, specfsm, tests);
		ResultsContainer result = new ResultsContainer(accuracy, pr);
		if(iterations == 1){
			if(currentResults!=null){
				results.add(currentResults);
			}
			currentResults = new ArrayList<ResultsContainer>();
		}
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
	

	public DirectedSparseGraph learnMachine() {
		DirectedSparseGraph learnt = decoratedLearner.learnMachine();
		results.add(currentResults);
		return learnt; 
	}

	public String getResult(){
		return decoratedLearner.getResult() + resultsToString();
	}



	public class ResultsContainer {
		private double accuracy;
		private PosNegPrecisionRecall pr;
		
		public ResultsContainer(double accuracy, PosNegPrecisionRecall pr){
			this.accuracy = accuracy;
			this.pr = pr;
		}
		
		public double getAccuracy() {
			return accuracy;
		}
		
		public PosNegPrecisionRecall getPr() {
			return pr;
		}
		

	}



	public void init(PTASequenceEngine en, int plus, int minus) {
		this.decoratedLearner.init(en, plus, minus);
		
	}



	public void update(Observable o, Object arg) {
		if(arg instanceof LearnerState)
			trackResults((LearnerState) arg);
		
	}
}
