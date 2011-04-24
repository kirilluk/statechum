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

import statechum.Label;
import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.*;
import statechum.analysis.learning.util.OutputUtil;
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
		System.out.println("autoname is "+AutoName);
		active = true;
		learnerInitConfiguration.config.setQuestionPathUnionLimit(1);
		learnerInitConfiguration.config.setUseConstraints(true);
		learnerInitConfiguration.config.setHowManyStatesToAddFromIFTHEN(1);
		learnerInitConfiguration.config.setUseLTL(true);
		learnerInitConfiguration.config.setDebugMode(true);
		learnerInitConfiguration.config.setUseSpin(false);
		setSimpleConfiguration(learnerInitConfiguration.config, active, k);
		//if(learnerInitConfiguration.ltlSequences!=null && !learnerInitConfiguration.ltlSequences.isEmpty())
		
		//String target = "B-initialise->C-receiveDown->C-sendHalt->E-receiveDown->C-receiveHalt->D\nA-receiveDown->C-monitorHigherPriorityNodes->A-receiveHalt->D-receiveHalt->" +
		//"D-receiveDown->D-sendAck->F-receiveDown->F-receiveHalt->F\nE-receiveAck->G-announceLeadership->C";
		
		String target =  "X-initialise->B-fail_close->A-check_updates->A-update->H-failed_get_use_old->I-succeed_use_remove_pending->F-write_to_cm_sim->D-set_wind_altimeter->A\nB-success_ctas_use_new_weather->C-succeed_use->D\nC-failed_use->A\nI-failed_use->E-write_to_cm_sim->A\nG-succeed_use_remove_pending->F\nG-failed_use_remove_pending->E\nH-success_ctas_use_new_weather->G";
		
		LearnerGraph targetMachine = FsmParser.buildLearnerGraph(target, "Target", learnerInitConfiguration.config);
		
		int sampleSize = (targetMachine.pathroutines.countEdges()*2);
		int percentPerChunk = 10;
		int nrPerChunk = sampleSize/(100/percentPerChunk);nrPerChunk+=nrPerChunk % 2;// make the number even
		
		
		RandomPathGenerator rpg = new RandomPathGenerator(targetMachine, new Random(100),5,null);// the seed for Random should be the same for each file
		rpg.generatePosNeg(2*nrPerChunk , 100/percentPerChunk);
		for(int i=0;i<10;i++){
			final PTASequenceEngine samples = rpg.getAllSequences(i);
			
			PTASequenceEngine.FilterPredicate posPredicate = samples.getFSM_filterPredicate();
			PTASequenceEngine.FilterPredicate negPredicate = new FilterPredicate() {
				FilterPredicate origFilter = samples.getFSM_filterPredicate();
				public @Override boolean shouldBeReturned(Object name) {
					return !origFilter.shouldBeReturned(name);
				}
			};
			
			sPlus = new HashSet<List<Label>>();
			sMinus = new HashSet<List<Label>>();
			sPlus.addAll(samples.getData(posPredicate));
			sMinus.addAll(samples.getData(negPredicate));
			/*
			RPNIUniversalLearner lr=new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,learnerInitConfiguration.config,learnerInitConfiguration.ifthenSequences,null));
			LearnerGraph ifthen = Transform.buildIfThenAutomata(learnerInitConfiguration.ifthenSequences, lr.init(sPlus, sMinus), 
					learnerInitConfiguration.config).toArray(new LearnerGraph[0])[0];
			Visualiser.updateFrame(ifthen, null);
			Visualiser.waitForKey();
			*/
			RPNILearner l = new RPNIUniversalLearner(null, new LearnerEvaluationConfiguration(null,null,learnerInitConfiguration.config,learnerInitConfiguration.ifthenSequences,null));
			//AccuracyTrackerDecorator atd = new  AccuracyTrackerDecorator(new MachineOracleDecorator(l,targetMachine),targetMachine);
			AccuracyTrackerDecorator atd = new  AccuracyTrackerDecorator(l,targetMachine);
			//atd.init(sPlus, sMinus);
			OutputUtil.generateDotOutput(targetMachine.pathroutines.getGraph(),"dotOutput.dot");
			Learner autoAns = new AutoAnswers(atd);
			autoAns.init(sPlus, sMinus);
			/*
			for(List<String> st:sPlus)
			{
				System.out.print("+ ");
				for(String elem:st)
					System.out.print(" "+elem);
				System.out.println();
			}
			
			for(List<String> st:sMinus)
			{
				System.out.print("- ");
				for(String elem:st)
					System.out.print(" "+elem);
				System.out.println();
			}
			*/
			//LearnerGraph learned = autoAns.learnMachine();
			//statechum.analysis.learning.util.OutputUtil.generateDotOutput(learned.pathroutines.getGraph());
			System.out.println(atd.getResult());
			
			System.out.println("------------");
		}
	}
	
	

}
