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

import java.awt.Frame;
import java.util.Random;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Collection;

import statechum.Configuration;
import statechum.analysis.learning.RPNIBlueFringeLearnerTestComponentOpt;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.model.testset.*;

public class LearnerAccuracyTracker extends
		RPNIBlueFringeLearnerTestComponentOpt {
	
	

	protected Collection<List<String>> tests;
	protected LearnerGraph specfsm;
	protected List<List<ResultsContainer>> results;
	protected List<ResultsContainer> currentResults;
	
	public LearnerAccuracyTracker(Frame parent, Configuration c, LearnerGraph target, Collection<List<String>> tests){
		super(parent,c);
		results = new ArrayList<List<ResultsContainer>>();
		config.setDebugMode(true);
		
		this.tests = tests;
		
		this.specfsm = target;
		this.currentResults = new ArrayList<ResultsContainer>();
	}

	@Override
	protected void debugAction(LearnerGraph lg, int iterations) {
		super.debugAction(lg, iterations);
		PosNegPrecisionRecall pr = CompareGraphs.compare(specfsm, lg);
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
	
	
	
	
	

}
