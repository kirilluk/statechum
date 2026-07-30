package statechum.analysis.learning.experiments.MarkovEDSM;

import com.ericsson.otp.erlang.*;
import statechum.Pair;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.*;
import java.util.*;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.constructResultsCollector;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.directoryExperimentStatistics;
import static statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment.sanitiseFileName;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovScoreVsInconsistency {
    public static final String description = "score_vs_inconsistency";

    public static class MarkovLearningStatisticsParameters extends MarkovLearningParameters {

        public MarkovLearningStatisticsParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample);
        }

        @Override
        public String getSubExperimentName() {
            return description;
        }
    }

    public static String learnStatistics = "statistics";

    public static CSVExperimentResult runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) throws FileNotFoundException {
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + description+"-results.csv"), "results.csv");
        boolean aveOrMax = true;// average divide by the divisor

        boolean [] penaliseMissingPathsValues = {true,false};
        int alphabetMultiplier = 2;
        boolean pathsOrSets = true;
        int [] chunkSizeValues = new int[]{2, 3, 4};
        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample) {
                    for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{learningGroup.getTracesLengthmultBaseline(states)})
                    {
                        int traceQuantityToUse = traces_lengthmult.firstElem;
                        for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample)
                            for (final int chunkSizeToEvaluate : chunkSizeValues)
                            for (final boolean penaliseMissingPaths:penaliseMissingPathsValues)
                            {
                                LearningAlgorithms.ScoringToApply learnerKind = LearningAlgorithms.ScoringToApply.SCORING_ORACLE_STATISTICS;
                                double weightOfInconsistencies = 1.0;
                                ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                ev.config = learningGroup.eval.config.copy();
                                ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                MarkovLearningStatisticsParameters parameters = new MarkovLearningStatisticsParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
                                parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
                                parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, alphabetMultiplier);
                                parameters.markovParameters.setMarkovParameters(0, chunkSizeToEvaluate, pathsOrSets,
                                        new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies, 0), penaliseMissingPaths, aveOrMax, 0, 0, 0);
                                parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovExperiment.MarkovLearnerRunner(parameters, ev);
                                learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                learningGroup.experimentRunner.submitTask(learnerRunner);
                            }
                    }
                }
            }

        learningGroup.experimentRunner.collectOutcomeOfExperiments(constructResultsCollector(resultCSV));

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
            for (int states : learningGroup.statesToUse)
                for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
//                    String experimentName = learningGroup.outPathPrefix + "statistics_"+states+"_"+perStateSquaredDensity100+"_";
//                    final Map<Integer,ScatterPlot> gr_ScoreVsInconsistency = new TreeMap<>();
//                    for (final int chunkSizeToEvaluate : chunkSizeValues) {
//                        gr_ScoreVsInconsistency.put(chunkSizeToEvaluate, new ScatterPlot("Inconsistency", "Score",
//                                new File(learningGroup.outPathPrefix + "statistics_" + states + "_" + perStateSquaredDensity100 + "_" + chunkSizeToEvaluate + "_"+penaliseMissingPaths+"_score_vs_inconsistency.pdf")));
//                    }
                    for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample) {
                        for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{new Pair(states, 2 * states)}) {
                            int traceQuantityToUse = traces_lengthmult.firstElem;
                            for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample)
                                for (final int chunkSizeToEvaluate : chunkSizeValues)
                                for (final boolean penaliseMissingPaths:penaliseMissingPathsValues)
                                {
                                    LearningAlgorithms.ScoringToApply learnerKind = LearningAlgorithms.ScoringToApply.SCORING_ORACLE_STATISTICS;
                                    double weightOfInconsistencies = 1.0;
                                    ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                    ev.config = learningGroup.eval.config.copy();
                                    ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                    MarkovLearningStatisticsParameters parameters = new MarkovLearningStatisticsParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
                                    parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
                                    parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, alphabetMultiplier);
                                    parameters.markovParameters.setMarkovParameters(0, chunkSizeToEvaluate, pathsOrSets,
                                            new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies, 0), penaliseMissingPaths, aveOrMax, 0, 0, 0);
                                    parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                    String pathName = learningGroup.outPathPrefix + directoryExperimentStatistics+sanitiseFileName(parameters.getSubExperimentName())+"-"+
                                            sanitiseFileName(parameters.getRowID());
                                    String statisticsFileName = SGE_ExperimentRunner.RunSubExperiment.constructFileName(
                                            learningGroup.outPathPrefix + directoryExperimentStatistics,learnStatistics,parameters);
                                    String fileContents = null;
                                    try (BufferedReader statisticsFile = new BufferedReader(new FileReader(statisticsFileName))) {
                                        fileContents = statisticsFile.readLine();
                                    } catch (IOException e) {
                                        // ignore error, we'll know that file was not read because fileContents will be null.
                                    }
                                    if (fileContents != null) {
                                        ScatterPlot gr_ScoreVsInconsistency = new ScatterPlot("Inconsistency", "Score",
                                           new File(pathName + File.separator+"statistics_" + states + "_" + perStateSquaredDensity100 + "_" + chunkSizeToEvaluate + "_"+penaliseMissingPaths+"_score_vs_inconsistency.pdf"));
                                        List<PairQualityLearner.PairScoreValue> values = new ArrayList<>();
                                        OtpErlangObject listOfPairsAsObject = ErlangLabel.parseText(fileContents);
                                        if (!(listOfPairsAsObject instanceof OtpErlangList))
                                            throw new IllegalArgumentException(statisticsFileName + " is not a list of type OtpErlangLists, got " + listOfPairsAsObject.getClass().getName());
                                        OtpErlangList listOfPairs = (OtpErlangList) listOfPairsAsObject;
                                        for (int i = 0; i < listOfPairs.arity(); ++i) {
                                            OtpErlangTuple pair = (OtpErlangTuple) listOfPairs.elementAt(i);
                                            boolean validMerge = ((OtpErlangBoolean) pair.elementAt(0)).booleanValue();
                                            long score = ((OtpErlangLong) pair.elementAt(1)).longValue();
                                            long inconsistency = ((OtpErlangLong) pair.elementAt(2)).longValue();
                                            values.add(new PairQualityLearner.PairScoreValue(validMerge, score, inconsistency));
                                            if (score < 100 && inconsistency < 1000)
                                                gr_ScoreVsInconsistency.add((double) inconsistency, (double) score, validMerge ? "green" : "red", null);
//                                                gr_ScoreVsInconsistency.get(chunkSizeToEvaluate).add((double) inconsistency, (double) score, validMerge ? "green" : "red", null);
                                        }

                                        gr_ScoreVsInconsistency.reportResults(learningGroup.gr);
                                        DrawGraphs.LogisticRegression regression = new DrawGraphs.LogisticRegression(values,"fit","pairvalues");
                                        System.out.println(pathName+" , "+ states + "_" + perStateSquaredDensity100 + "_" + chunkSizeToEvaluate + "_"+penaliseMissingPaths+" : "+regression.reportNormalisedCoefficients());
                                        ConfusionMatrix confUsingLogisticRegression = regression.computeConfusionMatrix(values);
                                        System.out.println("Logistic regression: "+confUsingLogisticRegression+" F1="+confUsingLogisticRegression.fMeasure()+", BCR="+confUsingLogisticRegression.BCR());
                                        for(double consideredWeightOfInconsistencies:new double[]{0.5,1.,2.,3.})
                                        for(double offset:new double[]{0,0.25,0.5,1})
                                        {
                                            ConfusionMatrix confGivenWeight = LogisticRegression.computeConfusionMatrixGivenWeightOfInconsistencies(values,consideredWeightOfInconsistencies,offset);
                                            System.out.println("Score-"+consideredWeightOfInconsistencies+"*inconsistency >= "+offset+": "+confGivenWeight+" F1="+confGivenWeight.fMeasure()+", BCR="+confGivenWeight.BCR());
                                        }
                                    }
                                }
                        }
                    }
//                    for (final int chunkSizeToEvaluate : chunkSizeValues) {
//                        gr_ScoreVsInconsistency.get(chunkSizeToEvaluate).reportResults(learningGroup.gr);
//                    }
                }
        }
        return resultCSV;
    }
}
