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
import java.util.concurrent.atomic.AtomicInteger;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.directoryExperimentStatistics;
import static statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment.sanitiseFileName;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovScoreVsInconsistency {

    public static class MarkovLearningStatisticsParameters extends MarkovLearningParameters {

        public MarkovLearningStatisticsParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample, int argSeed) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample, argSeed);
        }

        @Override
        public String getSubExperimentName() {
            return "score_vs_inconsistency";
        }
    }

    public static String learnStatistics = "statistics";

    public static CSVExperimentResult runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) throws FileNotFoundException {
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + "results.csv"));
        boolean aveOrMax = true;// average divide by the divisor

        boolean [] penaliseMissingPathsValues = {true,false};
        int alphabetMultiplier = 2;
        boolean pathsOrSets = true;
        int [] densities = new int[]{ 0 };
        int [] chunkSizeValues = new int[]{3, 4};
        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : densities) {
                for (int sample = 0,seedForFSM = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample, ++seedForFSM) {
                    for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{new Pair(states, 2*states )})
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

                                MarkovLearningStatisticsParameters parameters = new MarkovLearningStatisticsParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample, seedForFSM);
                                parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
                                parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, alphabetMultiplier);
                                parameters.markovParameters.setMarkovParameters(0, chunkSizeToEvaluate, pathsOrSets, weightOfInconsistencies, penaliseMissingPaths, aveOrMax, 0, 0, 0);
                                parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovExperiment.MarkovLearnerRunner(parameters, ev);
                                learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                learningGroup.experimentRunner.submitTask(learnerRunner);
                            }
                    }
                }
            }

        learningGroup.experimentRunner.collectOutcomeOfExperiments(new SGE_ExperimentRunner.processSubExperimentResult<MarkovLearningParameters, ExperimentResult<MarkovLearningParameters>>() {

            @Override
            public void processSubResult(ExperimentResult<MarkovLearningParameters> result, SGE_ExperimentRunner.RunSubExperiment<MarkovLearningParameters, ExperimentResult<MarkovLearningParameters>> experimentrunner) throws
                    IOException {// in these experiments, samples are singleton sequences because we run each of them in a separate process, in order to increase the efficiency with which all tasks are split between CPUs in an iceberg grid.
                PairQualityLearner.SampleData sm = result.samples.get(0);
                PairQualityLearner.ScoresForGraph data = sm.actualLearner;

                StringBuffer csvLine = new StringBuffer();
                csvLine.append(data.whetherLearningSuccessfulOrAborted);
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceBCR.getValue());// 1
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceStructural.getValue());// 2
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.invalidMergersNearRoot);// 3
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.missedMergersNearRoot); // 4
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.invalidMergersFarFromRoot);// 5
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.missedMergersFarFromRoot); // 6
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.validMergers); // 7
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.nrOfstates.getValue());// 8
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.inconsistencyReference);// 9
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistency);// 10

                if (result.parameters.learnerToUse.isMarkov()) {
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistencyAverage);// 11
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistencySD);// 12
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistencyAlwaysPositive);// 13
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.fractionOfStatesIdentifiedBySingletons);// 14
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovTransitionPrecision);// 15
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovTransitionRecall);// 16
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovHolePrecision);// 17
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovHoleRecall);// 18
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.comparisonsPerformed);// 19
                }

                if (result.parameters.markovParameters.useCentreVertex) {
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(sm.centreCorrect);
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(sm.centrePathNumber);
                }
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.referenceGraph.pathroutines.computeAlphabet().size());
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(100. * ConfusionMatrix.divide(sm.referenceGraph.pathroutines.countEdges(),sm.referenceGraph.getStateNumber()*sm.referenceGraph.getStateNumber())));
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.transitionsSampled);
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(data.executionTime / 1000000000.));// execution time is in nanoseconds, we only need seconds.
                experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());

                List<OtpErlangObject> pairs = new ArrayList<>();
                for(PairQualityLearner.PairScoreValue value:data.mergeStatistics)
                    pairs.add(new OtpErlangTuple(new OtpErlangObject[]{
                            new OtpErlangBoolean(value.validMerge),new OtpErlangLong(value.score),new OtpErlangLong(value.inconsistency)}));
//                System.out.println("Pairs : "+pairs.size()+" runtime: "+Math.round(data.executionTime / 1000000000.));
                String statisticsFileName = SGE_ExperimentRunner.RunSubExperiment.constructFileName(learningGroup.outPathPrefix + directoryExperimentStatistics,learnStatistics,result.parameters);
                try (FileWriter statisticsFile = new FileWriter(statisticsFileName)) {
                    statisticsFile.write(ErlangLabel.dumpErlangObject(new OtpErlangList(pairs.toArray(new OtpErlangObject[0]))));
                }
            }

            @Override
            public SGEExperimentResult[] getGraphs() {
                return new SGEExperimentResult[]{resultCSV};
            }

        });
        int referencePreset = 0;
        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
            for (int states : learningGroup.statesToUse)
                for (int perStateSquaredDensity100 : densities) {
//                    String experimentName = learningGroup.outPathPrefix + "statistics_"+states+"_"+perStateSquaredDensity100+"_";
//                    final Map<Integer,ScatterPlot> gr_ScoreVsInconsistency = new TreeMap<>();
//                    for (final int chunkSizeToEvaluate : chunkSizeValues) {
//                        gr_ScoreVsInconsistency.put(chunkSizeToEvaluate, new ScatterPlot("Inconsistency", "Score",
//                                new File(learningGroup.outPathPrefix + "statistics_" + states + "_" + perStateSquaredDensity100 + "_" + chunkSizeToEvaluate + "_"+penaliseMissingPaths+"_score_vs_inconsistency.pdf")));
//                    }
                    for (int sample = 0,seedForFSM = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample, ++seedForFSM) {
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

                                    MarkovLearningStatisticsParameters parameters = new MarkovLearningStatisticsParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample, seedForFSM);
                                    parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
                                    parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, alphabetMultiplier);
                                    parameters.markovParameters.setMarkovParameters(0, chunkSizeToEvaluate, pathsOrSets, weightOfInconsistencies, penaliseMissingPaths, aveOrMax, 0, 0, 0);
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

                                        OtpErlangObject listOfPairsAsObject = ErlangLabel.parseText(fileContents);
                                        if (!(listOfPairsAsObject instanceof OtpErlangList))
                                            throw new IllegalArgumentException(statisticsFileName + " is not a list of type OtpErlangLists, got " + listOfPairsAsObject.getClass().getName());
                                        OtpErlangList listOfPairs = (OtpErlangList) listOfPairsAsObject;
                                        for (int i = 0; i < listOfPairs.arity(); ++i) {
                                            OtpErlangTuple pair = (OtpErlangTuple) listOfPairs.elementAt(i);
                                            boolean validMerge = ((OtpErlangBoolean) pair.elementAt(0)).booleanValue();
                                            long score = ((OtpErlangLong) pair.elementAt(1)).longValue();
                                            long inconsistency = ((OtpErlangLong) pair.elementAt(2)).longValue();

                                            if (score < 100 && inconsistency < 1000)
                                                gr_ScoreVsInconsistency.add((double) inconsistency, (double) score, validMerge ? "green" : "red", null);
//                                                gr_ScoreVsInconsistency.get(chunkSizeToEvaluate).add((double) inconsistency, (double) score, validMerge ? "green" : "red", null);
                                        }

                                        gr_ScoreVsInconsistency.reportResults(learningGroup.gr);
                                    }
                                }
                        }
                    }
//                    for (final int chunkSizeToEvaluate : chunkSizeValues) {
//                        gr_ScoreVsInconsistency.get(chunkSizeToEvaluate).reportResults(learningGroup.gr);
//                    }
                }
        }

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            for (int states : learningGroup.statesToUse)
                for (int perStateSquaredDensity100 : densities) {

                }
        }
        return resultCSV;
    }
}
