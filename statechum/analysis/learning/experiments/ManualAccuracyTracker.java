package statechum.analysis.learning.experiments;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import statechum.Configuration;
import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.*;
import statechum.analysis.learning.observers.*;
import statechum.apps.QSMTool;


public class ManualAccuracyTracker {
	
	public static void main(String[] args){
		
		Set<List<String>> sPlus = new HashSet<List<String>>();
		Set<List<String>> sMinus = new HashSet<List<String>>();
		Set<String> ltl = new HashSet<String>();
		boolean active = true;
		try {
			BufferedReader in = new BufferedReader(new FileReader(args[0]));
			String fileString;
			String activePassive = in.readLine();
			if (activePassive.trim().equalsIgnoreCase("passive"))
				active = false;
			while ((fileString = in.readLine()) != null) {
				QSMTool.process(fileString, sPlus, sMinus, ltl);
			}
			in.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		Configuration config = Configuration.getDefaultConfiguration().copy();
		config.setQuestionPathUnionLimit(1);
		if(active)
			config.setAskQuestions(true);
		if(!ltl.isEmpty())
			config.setUseSpin(true);
		String target = "B-initialise->C-receiveDown->C-sendHalt->E-receiveDown->C-receiveHalt->D\nA-receiveDown->C-monitorHigherPriorityNodes->A-receiveHalt->D-receiveHalt->" +
		"D-receiveDown->D-sendAck->F-receiveDown->F-receiveHalt->F\nE-receiveAck->G-announceLeadership->C";
		LearnerGraph targetMachine = new LearnerGraph(TestFSMAlgo.buildGraph(target, "Target"), config);
		//statechum.analysis.learning.util.OutputUtil.generateDotOutput(targetMachine.paths.getGraph());
		
		RPNILearner l = new RPNIUniversalLearner(null, ltl,config);
		AccuracyTrackerDecorator atd = new AccuracyTrackerDecorator(l,targetMachine);
		atd.init(sPlus, sMinus);
		atd.learnMachine();
		System.out.println(atd.getResult());
	}
	
	

}
