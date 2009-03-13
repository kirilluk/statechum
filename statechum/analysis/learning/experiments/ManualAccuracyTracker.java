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


import java.util.HashSet;
import java.util.List;
import java.util.Random;

import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.*;
import statechum.analysis.learning.observers.*;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.apps.QSMTool;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;


public class ManualAccuracyTracker extends QSMTool {
	
	public static void main(String[] args){
		ManualAccuracyTracker tool = new ManualAccuracyTracker();tool.runExperiment();
	}
	
	@Override
	public void runExperiment()
	{
		String AutoName = System.getProperty(statechum.GlobalConfiguration.G_PROPERTIES.VIZ_AUTOFILENAME.name());
		if (AutoName != null) learnerInitConfiguration.config.setAutoAnswerFileName(AutoName);
		active = true;
		learnerInitConfiguration.config.setQuestionPathUnionLimit(1);
		//learnerInitConfiguration.config.setUseConstraints(true);
		learnerInitConfiguration.config.setHowManyStatesToAddFromIFTHEN(1);
		setSimpleConfiguration(learnerInitConfiguration.config, active, k);
		//if(learnerInitConfiguration.ltlSequences!=null && !learnerInitConfiguration.ltlSequences.isEmpty())
			learnerInitConfiguration.config.setUseLTL(true);
		learnerInitConfiguration.config.setDebugMode(true);
		String target = "B-initialise->C-receiveDown->C-sendHalt->E-receiveDown->C-receiveHalt->D\nA-receiveDown->C-monitorHigherPriorityNodes->A-receiveHalt->D-receiveHalt->" +
		"D-receiveDown->D-sendAck->F-receiveDown->F-receiveHalt->F\nE-receiveAck->G-announceLeadership->C";
		
		//String target = "q0-initialise->q1-connect->q2-login->q3-setfiletype->q4-rename->q6-storefile->q5-setfiletype->q4-storefile->q7-appendfile->q5-setfiletype->q4\nq3-makedir->q8-makedir->q8-logout->q16-disconnect->q17\nq3-changedirectory->q9-listfiles->q10-delete->q10-changedirectory->q9\nq10-appendfile->q11-logout->q16\nq3-storefile->q11\nq3-listfiles->q13-retrievefile->q13-logout->q16\nq13-changedirectory->q14-listfiles->q13\nq7-logout->q16\nq6-logout->q16";
		
		LearnerGraph targetMachine = new LearnerGraph(FsmParser.buildGraph(target, "Target"), learnerInitConfiguration.config);
		
		
		int sampleSize = (targetMachine.countEdges()*2);
		int percentPerChunk = 10;
		int nrPerChunk = sampleSize/(100/percentPerChunk);nrPerChunk+=nrPerChunk % 2;// make the number even
		
		
		RandomPathGenerator rpg = new RandomPathGenerator(targetMachine, new Random(100),5);// the seed for Random should be the same for each file
		rpg.generatePosNeg(2*nrPerChunk , 100/percentPerChunk);
		for(int i=0;i<10;i++){
			final PTASequenceEngine samples = rpg.getAllSequences(i);
			
			PTASequenceEngine.FilterPredicate posPredicate = samples.getFSM_filterPredicate();
			PTASequenceEngine.FilterPredicate negPredicate = new FilterPredicate() {
				FilterPredicate origFilter = samples.getFSM_filterPredicate();
				public boolean shouldBeReturned(Object name) {
					return !origFilter.shouldBeReturned(name);
				}
			};
			
			sPlus = new HashSet<List<String>>();
			sMinus = new HashSet<List<String>>();
			sPlus.addAll(samples.getData(posPredicate));
			sMinus.addAll(samples.getData(negPredicate));
			RPNILearner l = new RPNIUniversalLearner(null, new LearnerEvaluationConfiguration(null,null,learnerInitConfiguration.config,learnerInitConfiguration.ifthenSequences,null));
			AccuracyTrackerDecorator atd = new  AccuracyTrackerDecorator(new MachineOracleDecorator(l,targetMachine),targetMachine);
			//AccuracyTrackerDecorator atd = new  AccuracyTrackerDecorator(l,targetMachine);
			Learner autoAns = new AutoAnswers(atd);
			autoAns.init(sPlus, sMinus);
			
			LearnerGraph learned = autoAns.learnMachine();
			//statechum.analysis.learning.util.OutputUtil.generateDotOutput(learned.pathroutines.getGraph());
			System.out.println(atd.getResult());
			
			System.out.println("------------");
		}
	}
	
	

}
