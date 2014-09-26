package statechum.analysis.learning.experiments.PairSelection;

import java.util.LinkedList;
import java.util.List;

import statechum.Configuration;
import statechum.Configuration.LABELKIND;
import statechum.Label;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.PairSelection.Cav2014.EDSMReferenceLearner;
import statechum.analysis.learning.experiments.PairSelection.Cav2014.LearnerMarkovPassive;
import statechum.analysis.learning.experiments.PairSelection.Cav2014.RedPriorityOverBluePairSelectionRoutine;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.apps.QSMTool;

public class LearnFromTracesUsingExperimentalLearners {

	public LearnFromTracesUsingExperimentalLearners() {
	}

	
	public static class TraceLoader extends QSMTool
	{
		public TraceLoader(Configuration c,ConvertALabel converter)
		{
			learnerInitConfiguration.config = c;learnerInitConfiguration.setLabelConverter(converter);
		}
		
		public LearnerGraph getPTA()
		{
			LearnerGraph outcome = new LearnerGraph(learnerInitConfiguration.config);outcome.paths.augmentPTA(sPlus, true, false);outcome.paths.augmentPTA(sMinus, false, false);return outcome;
		}
	}
	
	public static void main(String []args)
	{
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setAskQuestions(false);config.setDebugMode(false);config.setGdLowToHighRatio(0.7);config.setRandomPathAttemptFudgeThreshold(1000);
		config.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);config.setLabelKind(LABELKIND.LABEL_STRING);
		final ConvertALabel converter = new Transform.InternStringLabel();
		MarkovModel m=new MarkovModel(3,true,true,false);
		
		TraceLoader tool = new TraceLoader(config,converter);
		tool.loadConfig(args[0]);
		LearnerGraph pta = tool.getPTA();
		
		new MarkovClassifier(m, pta).updateMarkov(false);
		LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);learnerEval.setLabelConverter(converter);
		LearnerMarkovPassive learnerOfPairs = new LearnerMarkovPassive(learnerEval,null,pta);learnerOfPairs.setMarkovModel(m);

		learnerOfPairs.setScoreComputationOverride(new RedPriorityOverBluePairSelectionRoutine(m));
		System.out.println("PTA states: "+pta.getStateNumber());
		System.out.println("Alphabet of "+pta.getCache().getAlphabet().size()+" : "+pta.getCache().getAlphabet());

		final Configuration deepCopy = pta.config.copy();deepCopy.setLearnerCloneGraph(true);
		LearnerGraph ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);
		
		LearnerGraph actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		LearnerGraph edsm2Outcome = new EDSMReferenceLearner(learnerEval,ptaCopy,1).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		System.out.println("Outcome states: "+edsm2Outcome.getStateNumber());
		Visualiser.updateFrame(actualAutomaton, edsm2Outcome);
		Visualiser.waitForKey();
		
	}
}
