package statechum.analysis.learning.experiments;


import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.*;
import statechum.analysis.learning.observers.*;
import statechum.apps.QSMTool;


public class ManualAccuracyTracker extends QSMTool {
	
	public static void main(String[] args){
		ManualAccuracyTracker tool = new ManualAccuracyTracker();tool.loadConfig(args[0]);tool.runExperiment();
	}
	
	@Override
	public void runExperiment()
	{
		config.setQuestionPathUnionLimit(1);
		if(active)
			config.setAskQuestions(true);
		if(!ltl.isEmpty())
			config.setUseSpin(true);
		String target = "B-initialise->C-receiveDown->C-sendHalt->E-receiveDown->C-receiveHalt->D\nA-receiveDown->C-monitorHigherPriorityNodes->A-receiveHalt->D-receiveHalt->" +
		"D-receiveDown->D-sendAck->F-receiveDown->F-receiveHalt->F\nE-receiveAck->G-announceLeadership->C";
		LearnerGraph targetMachine = new LearnerGraph(TestFSMAlgo.buildGraph(target, "Target"), config);
		RPNILearner l = new RPNIUniversalLearner(null, ltl,config);
		AccuracyTrackerDecorator atd = new AccuracyTrackerDecorator(l,targetMachine);
		atd.init(sPlus, sMinus);
		LearnerGraph learned = atd.learnMachine();
		statechum.analysis.learning.util.OutputUtil.generateDotOutput(learned.paths.getGraph());
		System.out.println(atd.getResult());
	}
	
	

}
