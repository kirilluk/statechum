package statechum.analysis.learning.experiments.EvaluationOfLearners;

import ext_lib.collections.HashMapWithSearch;
import statechum.*;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.FsmParserDot;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.collections.MapWithSearch;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.Map;
import java.util.TreeMap;

public class LearnMealy {
    public static ProgressDecorator.LearnerEvaluationConfiguration constructLearnerInitConfiguration()
    {
        Configuration config = Configuration.getDefaultConfiguration().copy();config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        ProgressDecorator.LearnerEvaluationConfiguration learnerInitConfiguration = new ProgressDecorator.LearnerEvaluationConfiguration(config);
        final Configuration learnerConfig = learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
        learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);
        learnerConfig.setTransitionMatrixImplType(Configuration.STATETREE.STATETREE_ARRAY);
        learnerConfig.setAlwaysUseTheSameMatrixType(false);// permits computations to switch transition matrix type depending on matrix size.
        learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);
        learnerConfig.setLearnerScoreMode(Configuration.ScoreMode.GENERAL_NOFULLMERGE);
        learnerInitConfiguration.config.setUseConstraints(false);// do not use if-then during learning (enough to augment once)
        return learnerInitConfiguration;
    }

    public static void main(String[] args) throws IOException {// -ea -Xmx1600m -Xms800m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 -DVIZ_CONFIG=kirill_tmp
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.CLOSE_TERMINATE, "true");
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.ESC_TERMINATE, "false");
        final Configuration configAtomicPairs = Configuration.getDefaultConfiguration().copy();
        configAtomicPairs.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        String referenceDot = Helper.loadFile(new File(args[0]));
        String ptaDot = Helper.loadFile(new File(args[1]));
        MapWithSearch<String,String,Integer> useExistingNumbering = new HashMapWithSearch<>(20);
        Map<LabelInputOutput,Integer> labelToNumber = new TreeMap<>();
        LearnerGraph referenceGraph = FsmParserDot.buildLearnerGraph(referenceDot, configAtomicPairs, null, true, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND).
                transform.numberOutputsAndStates(false,null,useExistingNumbering,labelToNumber);
        LearnerGraph pta = FsmParserDot.buildLearnerGraph(ptaDot, configAtomicPairs, null, true, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND).
                transform.numberOutputsAndStates(false,null,useExistingNumbering,labelToNumber);
        System.out.println("pta size: "+pta.transitionMatrix.size());
        ProgressDecorator.LearnerEvaluationConfiguration learnerInitConfiguration = constructLearnerInitConfiguration();

        UASExperiment.ScoringModeScore scoringMethod = new UASExperiment.ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE_MEALY, LearningAlgorithms.ScoringToApply.SCORING_EDSM);
        LearningAlgorithms.StateMergingStatistics redReducer = LearningAlgorithms.ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown.constructReducerIfUsingSiccoScoring(referenceGraph,scoringMethod.scoringMethod);

        Visualiser graphVisualiser= new Visualiser(0);
//        LearnerGraph upperGraph = referenceGraph.transform.trimGraph(3, referenceGraph.config);
//        graphVisualiser.updateFrame(upperGraph, null);
//        Visualiser.waitForKey();
        pta.getInit().setColour(JUConstants.RED);
//        LearnerGraph trimmed = pta.transform.trimGraph(2,pta.config);graphVisualiser.update(null,trimmed);Visualiser.waitForKey();

        Learner learner = LearningAlgorithms.constructLearner(learnerInitConfiguration, pta,scoringMethod.scoringMethod, scoringMethod.scoringForEDSM, redReducer);
        LearnerGraph learntGraph = learner.learnMachine(new LinkedList<>(), new LinkedList<>());
        LearnerGraph actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
        LearnerGraph referenceAsIOPairs = referenceGraph.transform.convertToIOPairs();

        System.out.println("learnt size: "+actualAutomaton.transitionMatrix.size());
        System.out.println("missed: "+redReducer.reportMissedMergers());
        System.out.println("invalid : "+redReducer.reportInvalidMergers());
        PairQualityLearner.DifferenceToReferenceDiff diffMeasure = PairQualityLearner.DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceAsIOPairs, actualAutomaton, learnerInitConfiguration.config, 1);
//        PairQualityLearner.DifferenceToReferenceLanguageBCR bcrMeasure = PairQualityLearner.DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceAsIOPairs, actualAutomaton,learnerInitConfiguration.testSet);
        System.out.println(diffMeasure.getValue());

//        DirectedSparseGraph gr = DifferenceVisualiser.ChangesToGraph.computeVisualisationParameters(Synapse.StatechumProcess.constructFSM(referenceAsIOPairs),
//                DifferenceVisualiser.ChangesToGraph.computeGD(referenceAsIOPairs, actualAutomaton,configAtomicPairs));
//        graphVisualiser.update(null,gr);
//        LearnerGraph lowerGraph = actualAutomaton.transform.trimGraph(3, actualAutomaton.config);
        Visualiser.updateFrame(referenceAsIOPairs, actualAutomaton);
        Visualiser.waitForKey();
    }
}
