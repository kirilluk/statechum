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
package statechum.analysis.learning.experiments;


import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.*;
import statechum.analysis.learning.observers.*;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.apps.QSMTool;


public class ManualAccuracyTracker extends QSMTool {
	
	public static void main(String[] args){
		ManualAccuracyTracker tool = new ManualAccuracyTracker();tool.loadConfig(args[0]);tool.runExperiment();
	}
	
	@Override
	public void runExperiment()
	{
		if(active)
			learnerInitConfiguration.config.setAskQuestions(true);
		if(!learnerInitConfiguration.ltlSequences.isEmpty())
			learnerInitConfiguration.config.setUseLTL(true);
		String target = "B-initialise->C-receiveDown->C-sendHalt->E-receiveDown->C-receiveHalt->D\nA-receiveDown->C-monitorHigherPriorityNodes->A-receiveHalt->D-receiveHalt->" +
		"D-receiveDown->D-sendAck->F-receiveDown->F-receiveHalt->F\nE-receiveAck->G-announceLeadership->C";
		LearnerGraph targetMachine = new LearnerGraph(TestFSMAlgo.buildGraph(target, "Target"), learnerInitConfiguration.config);
		RPNILearner l = new RPNIUniversalLearner(null, new LearnerEvaluationConfiguration(null,null,learnerInitConfiguration.config,learnerInitConfiguration.ltlSequences,null));
		AccuracyTrackerDecorator atd = new AccuracyTrackerDecorator(l,targetMachine);
		atd.init(sPlus, sMinus);
		LearnerGraph learned = atd.learnMachine();
		statechum.analysis.learning.util.OutputUtil.generateDotOutput(learned.pathroutines.getGraph());
		System.out.println(atd.getResult());
	}
	
	

}
