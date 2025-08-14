package statechum.analysis.learning.experiments.EvaluationOfLearners;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import ext_lib.collections.HashMapWithSearch;
import statechum.*;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.Erlang.Synapse;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.linear.DifferenceVisualiser;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.FsmParserDot;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.collections.MapWithSearch;
import statechum.model.testset.PTASequenceEngine;

import java.io.File;
import java.io.IOException;
import java.util.*;

public class LearnMealy {
    public static ProgressDecorator.LearnerEvaluationConfiguration constructLearnerInitConfiguration() {
        Configuration config = Configuration.getDefaultConfiguration().copy();
        config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        ProgressDecorator.LearnerEvaluationConfiguration learnerInitConfiguration = new ProgressDecorator.LearnerEvaluationConfiguration(config);
        final Configuration learnerConfig = learnerInitConfiguration.config;
        learnerConfig.setGeneralisationThreshold(0);
        learnerConfig.setGdFailOnDuplicateNames(false);
        learnerConfig.setGdLowToHighRatio(0.75);
        learnerConfig.setGdKeyPairThreshold(0.5);
        learnerConfig.setTransitionMatrixImplType(Configuration.STATETREE.STATETREE_ARRAY);
        learnerConfig.setAlwaysUseTheSameMatrixType(false);// permits computations to switch transition matrix type depending on matrix size.
        learnerConfig.setAskQuestions(false);
        learnerConfig.setDebugMode(false);
        learnerConfig.setLearnerScoreMode(Configuration.ScoreMode.GENERAL_NOFULLMERGE);
        learnerInitConfiguration.config.setUseConstraints(false);// do not use if-then during learning (enough to augment once)
        return learnerInitConfiguration;
    }

    public static void mainA(String[] args) throws IOException {// -ea -Xmx1600m -Xms800m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 -DVIZ_CONFIG=kirill_tmp
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.CLOSE_TERMINATE, "true");
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.ESC_TERMINATE, "false");
        final Configuration configAtomicPairs = Configuration.getDefaultConfiguration().copy();
        configAtomicPairs.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        String referenceDot = Helper.loadFile(new File(args[0]));
        String ptaDot = Helper.loadFile(new File(args[1]));
        MapWithSearch<String, String, Integer> useExistingNumbering = new HashMapWithSearch<>(20);
        Map<LabelInputOutput, Integer> labelToNumber = new TreeMap<>();
        LearnerGraph referenceGraph = FsmParserDot.buildLearnerGraph(referenceDot, configAtomicPairs, null, true, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND).
                transform.numberOutputsAndStates(false, null, useExistingNumbering, labelToNumber);
        LearnerGraph a = FsmParserDot.buildLearnerGraph(Helper.loadFile(new File(args[2])), configAtomicPairs, null, true, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND).
                transform.numberOutputsAndStates(false, null, useExistingNumbering, labelToNumber);
        LearnerGraph pta = FsmParserDot.buildLearnerGraph(ptaDot, configAtomicPairs, null, true, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND).
                transform.numberOutputsAndStates(false, null, useExistingNumbering, labelToNumber);
        System.out.println("pta size: " + pta.transitionMatrix.size());
        ProgressDecorator.LearnerEvaluationConfiguration learnerInitConfiguration = constructLearnerInitConfiguration();

        UASExperiment.ScoringModeScore scoringMethod = new UASExperiment.ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE_MEALY, LearningAlgorithms.ScoringToApply.SCORING_EDSM);
        LearningAlgorithms.StateMergingStatistics redReducer = LearningAlgorithms.ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown.constructReducerIfUsingSiccoScoring(referenceGraph, scoringMethod.scoringMethod);

        Visualiser graphVisualiser = new Visualiser(0);
//        LearnerGraph upperGraph = referenceGraph.transform.trimGraph(3, referenceGraph.config);
//        graphVisualiser.updateFrame(upperGraph, null);
//        Visualiser.waitForKey();
        pta.getInit().setColour(JUConstants.RED);
//        LearnerGraph trimmed = pta.transform.trimGraph(2,pta.config);graphVisualiser.update(null,trimmed);Visualiser.waitForKey();

        Learner learner = LearningAlgorithms.constructLearner(learnerInitConfiguration, pta, scoringMethod.scoringMethod, scoringMethod.scoringForEDSM, redReducer);
        LearnerGraph learntGraph = learner.learnMachine(new LinkedList<>(), new LinkedList<>());
        LearnerGraph actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);

//        actualAutomaton = a;

        System.out.println("learnt size: " + actualAutomaton.transitionMatrix.size());
        System.out.println("missed: " + redReducer.reportMissedMergers());
        System.out.println("invalid : " + redReducer.reportInvalidMergers());
        PairQualityLearner.DifferenceToReferenceDiff diffMeasure = PairQualityLearner.DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
//        PairQualityLearner.DifferenceToReferenceLanguageBCR bcrMeasure = PairQualityLearner.DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceAsIOPairs, actualAutomaton,learnerInitConfiguration.testSet);
        System.out.println("Learnt against reference : " + diffMeasure.getValue());
        {
            PairQualityLearner.DifferenceToReferenceDiff diffAgainstA = PairQualityLearner.DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, a, learnerInitConfiguration.config, 1);
            System.out.println("A against reference :" + diffAgainstA.getValue());
        }
        DirectedSparseGraph gr = DifferenceVisualiser.ChangesToGraph.computeVisualisationParameters(Synapse.StatechumProcess.constructFSM(referenceGraph),
                DifferenceVisualiser.ChangesToGraph.computeGD(referenceGraph, actualAutomaton, configAtomicPairs));
        graphVisualiser.update(null, gr);
        Visualiser.waitForKey();
//        LearnerGraph lowerGraph = actualAutomaton.transform.trimGraph(3, actualAutomaton.config);
        Visualiser.updateFrame(referenceGraph, actualAutomaton);
        Visualiser.waitForKey();
    }

    public static void mainB(String[] args) throws IOException {// -ea -Xmx1600m -Xms800m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 -DVIZ_CONFIG=kirill_tmp
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.CLOSE_TERMINATE, "true");
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.ESC_TERMINATE, "false");
        final Configuration configAtomicPairs = Configuration.getDefaultConfiguration().copy();
        configAtomicPairs.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        String referenceDot = Helper.loadFile(new File(args[0]));

        MapWithSearch<String, String, Integer> useExistingNumbering = new HashMapWithSearch<>(20);
        Map<LabelInputOutput, Integer> labelToNumber = new TreeMap<>();
        LearnerGraph referenceGraph = FsmParserDot.buildLearnerGraph(referenceDot, configAtomicPairs, null, true, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND).
                transform.numberOutputsAndStates(false, null, useExistingNumbering, labelToNumber);

        final LearnerGraph pta = new LearnerGraph(configAtomicPairs);
        int traceQuantity = 50;
        int traceLengthMult = 2;
        final int tracesToGenerate = LearningSupportRoutines.makeEven(referenceGraph.getStateNumber() * traceQuantity);
        final RandomPathGenerator generator = new RandomPathGenerator(referenceGraph, new Random(0), 5, referenceGraph.getInit());
        generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomPathGenerator.RandomLengthGenerator() {

            @Override
            public int getLength() {
                return traceLengthMult * referenceGraph.getStateNumber();
            }

            @Override
            public int getPrefixLength(int len) {
                return len;
            }
        }, true, true, null, null);
        pta.paths.augmentPTA(generator.getAllSequences(0));
        pta.clearColours();

        System.out.println("pta size: " + pta.transitionMatrix.size());
        ProgressDecorator.LearnerEvaluationConfiguration learnerInitConfiguration = constructLearnerInitConfiguration();

        UASExperiment.ScoringModeScore scoringMethod = new UASExperiment.ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE_MEALY, LearningAlgorithms.ScoringToApply.SCORING_EDSM);
        LearningAlgorithms.StateMergingStatistics redReducer = LearningAlgorithms.ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown.constructReducerIfUsingSiccoScoring(referenceGraph, scoringMethod.scoringMethod);

        Visualiser graphVisualiser = new Visualiser(0);
//        LearnerGraph upperGraph = referenceGraph.transform.trimGraph(3, referenceGraph.config);
//        graphVisualiser.updateFrame(upperGraph, null);
//        Visualiser.waitForKey();
        pta.getInit().setColour(JUConstants.RED);
//        LearnerGraph trimmed = pta.transform.trimGraph(2,pta.config);graphVisualiser.update(null,trimmed);Visualiser.waitForKey();

        Learner learner = LearningAlgorithms.constructLearner(learnerInitConfiguration, pta, scoringMethod.scoringMethod, scoringMethod.scoringForEDSM, redReducer);
        LearnerGraph learntGraph = learner.learnMachine(new LinkedList<>(), new LinkedList<>());
        LearnerGraph actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);

        System.out.println("learnt size: " + actualAutomaton.transitionMatrix.size());
        System.out.println("missed: " + redReducer.reportMissedMergers());
        System.out.println("invalid : " + redReducer.reportInvalidMergers());
        PairQualityLearner.DifferenceToReferenceDiff diffMeasure = PairQualityLearner.DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
//        PairQualityLearner.DifferenceToReferenceLanguageBCR bcrMeasure = PairQualityLearner.DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceAsIOPairs, actualAutomaton,learnerInitConfiguration.testSet);
        System.out.println("Learnt against reference : " + diffMeasure.getValue());
        DirectedSparseGraph gr = DifferenceVisualiser.ChangesToGraph.computeVisualisationParameters(Synapse.StatechumProcess.constructFSM(referenceGraph),
                DifferenceVisualiser.ChangesToGraph.computeGD(referenceGraph, actualAutomaton, configAtomicPairs));
        graphVisualiser.update(null, gr);
        Visualiser.waitForKey();
    }

    protected static String extractString(OtpErlangTuple tuple, int position) {
        return ((OtpErlangString) tuple.elementAt(position)).stringValue();
    }

    public static void mainCurrentSTatistic(String[] args) throws IOException {// -ea -Xmx1600m -Xms800m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 -DVIZ_CONFIG=kirill_tmp
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.CLOSE_TERMINATE, "true");
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.ESC_TERMINATE, "false");

        OtpErlangTuple taskRoot = (OtpErlangTuple) ErlangLabel.parseText(Helper.loadFile(new File(args[0])));
        File path = new File(extractString(taskRoot, 0));
        for (OtpErlangObject taskAsObject : (OtpErlangList) taskRoot.elementAt(1)) {
            OtpErlangTuple task = (OtpErlangTuple) taskAsObject;
            File pathToTask = new File(path, extractString(task, 0));
            final Configuration configAtomicPairs = Configuration.getDefaultConfiguration().copy();
            configAtomicPairs.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
            File referenceAutomatonPath = new File(pathToTask, extractString(task, 1));
            String referenceDot = Helper.loadFile(referenceAutomatonPath);
            String ptaDot = Helper.loadFile(new File(new File(pathToTask, extractString(task, 2)), extractString(task, 3)));
            String ptaLearntFromPta = Helper.loadFile(new File(new File(pathToTask, extractString(task, 2)), extractString(task, 4)));
            System.out.println("Learning " + extractString(task, 1) + " using " + extractString(task, 2));
            MapWithSearch<String, String, Integer> useExistingNumbering = new HashMapWithSearch<>(20);
            Map<LabelInputOutput, Integer> labelToNumber = new TreeMap<>();
            LearnerGraph referenceGraph = FsmParserDot.buildLearnerGraph(referenceDot, configAtomicPairs, null, true, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND).
                    transform.numberOutputsAndStates(false, null, useExistingNumbering, labelToNumber);
            LearnerGraph learntAutomatonFromPta = FsmParserDot.buildLearnerGraph(ptaLearntFromPta, configAtomicPairs, null, true, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND).
                    transform.numberOutputsAndStates(false, null, useExistingNumbering, labelToNumber);
            LearnerGraph pta = FsmParserDot.buildLearnerGraph(ptaDot, configAtomicPairs, null, true, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND).
                    transform.numberOutputsAndStates(false, null, useExistingNumbering, labelToNumber);
            System.out.println("pta size: " + pta.transitionMatrix.size());
            ProgressDecorator.LearnerEvaluationConfiguration learnerInitConfiguration = constructLearnerInitConfiguration();

            UASExperiment.ScoringModeScore scoringMethod = new UASExperiment.ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE_MEALY, LearningAlgorithms.ScoringToApply.SCORING_EDSM);
            LearningAlgorithms.StateMergingStatistics redReducer = LearningAlgorithms.ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown.constructReducerIfUsingSiccoScoring(referenceGraph, scoringMethod.scoringMethod);

            Visualiser graphVisualiser = new Visualiser(0);
//        LearnerGraph upperGraph = referenceGraph.transform.trimGraph(3, referenceGraph.config);
//        graphVisualiser.updateFrame(upperGraph, null);
//        Visualiser.waitForKey();
            pta.getInit().setColour(JUConstants.RED);
//        LearnerGraph trimmed = pta.transform.trimGraph(2,pta.config);graphVisualiser.update(null,trimmed);Visualiser.waitForKey();

            Learner learner = LearningAlgorithms.constructLearner(learnerInitConfiguration, pta, scoringMethod.scoringMethod, scoringMethod.scoringForEDSM, redReducer);
            LearnerGraph learntGraph = learner.learnMachine(new LinkedList<>(), new LinkedList<>());
            LearnerGraph actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);

            System.out.println("learnt size: " + actualAutomaton.transitionMatrix.size());
            System.out.println("missed: " + redReducer.reportMissedMergers());
            System.out.println("invalid : " + redReducer.reportInvalidMergers());
            PairQualityLearner.DifferenceToReferenceDiff diffMeasure = PairQualityLearner.DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
//        PairQualityLearner.DifferenceToReferenceLanguageBCR bcrMeasure = PairQualityLearner.DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceAsIOPairs, actualAutomaton,learnerInitConfiguration.testSet);
            System.out.println("Statechum-edsm against reference : " + diffMeasure.getValue());
            {
                PairQualityLearner.DifferenceToReferenceDiff diffAgainstA = PairQualityLearner.DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, learntAutomatonFromPta, learnerInitConfiguration.config, 1);
                System.out.println("Python-edsm against reference :" + diffAgainstA.getValue());
            }
        }
    }

    public static void main(String[] args) throws IOException {// -ea -Xmx1600m -Xms800m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 -DVIZ_CONFIG=kirill_tmp
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.CLOSE_TERMINATE, "true");
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.ESC_TERMINATE, "false");
        final Configuration configAtomicPairs = Configuration.getDefaultConfiguration().copy();
        configAtomicPairs.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);

        OtpErlangTuple taskRoot = (OtpErlangTuple) ErlangLabel.parseText(Helper.loadFile(new File(args[0])));
        File path = new File(extractString(taskRoot, 0));
        Map<String,File> sourceFiles = new TreeMap<>();
        for (OtpErlangObject taskAsObject : (OtpErlangList) taskRoot.elementAt(1)) {
            OtpErlangTuple task = (OtpErlangTuple) taskAsObject;
            File pathToTask = new File(path, extractString(task, 0));
            File referenceAutomatonPath = new File(pathToTask, extractString(task, 1));

            sourceFiles.put(extractString(task, 0),referenceAutomatonPath);
        }

        for(Map.Entry<String,File> sourcePath:sourceFiles.entrySet()) {
            System.out.println("************** Processing : "+sourcePath.getKey()+" **************");
            String referenceDot = Helper.loadFile(sourcePath.getValue());
            MapWithSearch<String, String, Integer> useExistingNumbering = new HashMapWithSearch<>(20);
            Map<LabelInputOutput, Integer> labelToNumber = new TreeMap<>();
            LearnerGraph referenceGraph = FsmParserDot.buildLearnerGraph(referenceDot, configAtomicPairs, null, true, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND).
                    transform.numberOutputsAndStates(false, null, useExistingNumbering, labelToNumber);

            final LearnerGraph pta = new LearnerGraph(configAtomicPairs);
            int maxTraceQuantity = 5000;
            for(int traceQuantity = 50;traceQuantity < maxTraceQuantity;traceQuantity*=2) {
                int traceLengthMult = 2;
                final int tracesToGenerate = LearningSupportRoutines.makeEven(referenceGraph.getStateNumber() * traceQuantity);
                final RandomPathGenerator generator = new RandomPathGenerator(referenceGraph, new Random(0), 5, referenceGraph.getInit());
                generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomPathGenerator.RandomLengthGenerator() {

                    @Override
                    public int getLength() {
                        return traceLengthMult * referenceGraph.getStateNumber();
                    }

                    @Override
                    public int getPrefixLength(int len) {
                        return len;
                    }
                }, true, true, null, null);
                pta.paths.augmentPTA(generator.getAllSequences(0));
                pta.clearColours();

                System.out.println("Trace quantity :"+traceQuantity+", PTA size: " + pta.transitionMatrix.size());
                ProgressDecorator.LearnerEvaluationConfiguration learnerInitConfiguration = constructLearnerInitConfiguration();

                UASExperiment.ScoringModeScore scoringMethod = new UASExperiment.ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE_MEALY, LearningAlgorithms.ScoringToApply.SCORING_EDSM);
                LearningAlgorithms.StateMergingStatistics redReducer = LearningAlgorithms.ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown.constructReducerIfUsingSiccoScoring(referenceGraph, scoringMethod.scoringMethod);

                pta.getInit().setColour(JUConstants.RED);

                Learner learner = LearningAlgorithms.constructLearner(learnerInitConfiguration, pta, scoringMethod.scoringMethod, scoringMethod.scoringForEDSM, redReducer);
                LearnerGraph learntGraph = learner.learnMachine(new LinkedList<>(), new LinkedList<>());
                LearnerGraph actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);

                System.out.println("learnt size: " + actualAutomaton.transitionMatrix.size());
                System.out.println("missed: " + redReducer.reportMissedMergers());
                System.out.println("invalid : " + redReducer.reportInvalidMergers());
                PairQualityLearner.DifferenceToReferenceDiff diffMeasure = PairQualityLearner.DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
                System.out.println("Learnt against reference : " + diffMeasure.getValue());
                if (diffMeasure.getValue() > 0.999)
                    break;
            }
        }
    }
}
