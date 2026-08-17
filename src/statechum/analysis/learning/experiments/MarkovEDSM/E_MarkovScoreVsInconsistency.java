package statechum.analysis.learning.experiments.MarkovEDSM;

import com.ericsson.otp.erlang.*;
import statechum.Pair;
import statechum.ProgressIndicator;
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

    enum BEST_WORST_BCR {RESULT_MARKOV_BEST, RESULT_MARKOV_WORST, RESULT_LOGISTIC_BEST, RESULT_LOGISTIC_WORST}
    public static class BCRAndValues {
        double bcr;
        List<PairQualityLearner.PairScoreValue> values;
        String description = null;

        public BCRAndValues(double bcr) {
            this.bcr = bcr;this.values = null;description = null;
        }

        public void assignIfBetter(double bcr, List<PairQualityLearner.PairScoreValue> values, String description) {
            if (bcr > this.bcr) {
                this.bcr = bcr;
                this.values = values;
                this.description = description;
            }
        }

        public void assignIfWorse(double bcr, List<PairQualityLearner.PairScoreValue> values, String description) {
            if (bcr < this.bcr) {
                this.bcr = bcr;
                this.values = values;
                this.description = description;
            }
        }

    }
    public static CSVExperimentResult runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) throws FileNotFoundException {
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + description+"-results.csv"), "results.csv");
        boolean aveOrMax = true;// average divide by the divisor

        boolean [] penaliseMissingPathsValues = {true};//,false};
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

        final SGE_ExperimentRunner.processSubExperimentResult<MarkovLearningParameters, ExperimentResult<MarkovLearningParameters>> resultsCollector =
            constructResultsCollector(resultCSV);
        learningGroup.experimentRunner.collectOutcomeOfExperiments(new SGE_ExperimentRunner.processSubExperimentResult<MarkovLearningParameters, ExperimentResult<MarkovLearningParameters>>() {

            @Override
            public void processSubResult(ExperimentResult<MarkovLearningParameters> result, SGE_ExperimentRunner.RunSubExperiment<MarkovLearningParameters, ExperimentResult<MarkovLearningParameters>> experimentrunner) throws
                    IOException {// in these experiments, samples are singleton sequences because we run each of them in a separate process, in order to increase the efficiency with which all tasks are split between CPUs in an iceberg grid.
                PairQualityLearner.SampleData sm = result.samples.get(0);
                PairQualityLearner.ScoresForGraph data = sm.actualLearner;

                resultsCollector.processSubResult(result,experimentrunner);

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
                return resultsCollector.getGraphs();
            }
        });

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
            Map<String,ScatterPlot> pathToScatterPlot = new HashMap<>();
            Map<Integer,double []> chunkLenToWeights = new TreeMap<>();
            chunkLenToWeights.put(2,new double[]{1.0,2.0,3.0});
            chunkLenToWeights.put(3,new double[]{0.5,1.0,2.0});
            chunkLenToWeights.put(4,new double[]{0.25,0.5,1.0});
            Map<Pair<Integer,Double>,DrawGraphs.SquareBagPlot> multToBCR = new  TreeMap<>();
            for(int chunkLen: chunkSizeValues)
                for(double weight:chunkLenToWeights.get(chunkLen))
                    multToBCR.put(new Pair<>(chunkLen,weight),new DrawGraphs.SquareBagPlot("BCR, logistic regression", "BCR, "+weight,
                        new File(learningGroup.outPathPrefix + File.separator + description + "bcr_logistic_ch="+chunkLen+"_vs_mult+"+weight+".pdf"), 0.5, 1, true));

            int numberOfPoints = 0;
            for (int states_C : learningGroup.statesToUse)
                for (int ignoredA : MarkovExperiment.densityFromStateNumber(states_C))
                    for (int sample_C = 0; sample_C < learningGroup.fsmSamplesPerStateNumber; ++sample_C)
                        for (final Pair<Integer, Integer> ignoredB : new Pair[]{learningGroup.getTracesLengthmultBaseline(states_C)})
                            for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample)
                                for (final int ignoredC : chunkSizeValues)
                                    for (final boolean ignoredD:penaliseMissingPathsValues)
                                        ++numberOfPoints;

            Map<Pair<Integer,Double>,Map<BEST_WORST_BCR,BCRAndValues>> bcrKindForChunkLenAndWeight = new HashMap<>();

            ProgressIndicator progress = new ProgressIndicator("Reporting results",numberOfPoints);
            for (int states : learningGroup.statesToUse)
                for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                    for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample) {
                        for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{learningGroup.getTracesLengthmultBaseline(states)}) {
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
                                        }

                                        DrawGraphs.LogisticRegression regression = new DrawGraphs.LogisticRegression(values,"fit","pairvalues");
//                                        System.out.println(pathName+" , "+ states + "_" + perStateSquaredDensity100 + "_" + chunkSizeToEvaluate + "_"+penaliseMissingPaths+" : "+regression.reportNormalisedCoefficients());
                                        ConfusionMatrix confUsingLogisticRegression = regression.computeConfusionMatrix(values);
//                                        System.out.println("Logistic regression: "+confUsingLogisticRegression+" F1="+confUsingLogisticRegression.fMeasure()+", BCR="+confUsingLogisticRegression.BCR());


                                        // Now go through the plots to populate and only add to the plot with the right parameters.
                                        for(Map.Entry<Pair<Integer,Double>,SquareBagPlot> mult_and_plot:multToBCR.entrySet())
                                            if (mult_and_plot.getKey().firstElem == chunkSizeToEvaluate) {
                                                ConfusionMatrix confGivenWeight = LogisticRegression.computeConfusionMatrixGivenWeightOfInconsistencies(values,mult_and_plot.getKey().secondElem,0.0);
                                                mult_and_plot.getValue().add(confUsingLogisticRegression.BCR(), confGivenWeight.BCR());
                                                Map<BEST_WORST_BCR,BCRAndValues> valuesForBCRKind = bcrKindForChunkLenAndWeight.computeIfAbsent(new Pair<>(chunkSizeToEvaluate,mult_and_plot.getKey().secondElem),
                                                        p->new TreeMap<>());
                                                valuesForBCRKind.computeIfAbsent(BEST_WORST_BCR.RESULT_LOGISTIC_BEST, k -> new BCRAndValues(0)).
                                                        assignIfBetter(confUsingLogisticRegression.BCR(),values,regression.reportCoefficients());
                                                valuesForBCRKind.computeIfAbsent(BEST_WORST_BCR.RESULT_LOGISTIC_WORST, k -> new BCRAndValues(2.0)).
                                                        assignIfWorse(confUsingLogisticRegression.BCR(),values,regression.reportCoefficients());
                                                valuesForBCRKind.computeIfAbsent(BEST_WORST_BCR.RESULT_MARKOV_BEST, k -> new BCRAndValues(0)).
                                                        assignIfBetter(confGivenWeight.BCR(),values,regression.reportCoefficients());
                                                valuesForBCRKind.computeIfAbsent(BEST_WORST_BCR.RESULT_MARKOV_WORST, k -> new BCRAndValues(2.0)).
                                                        assignIfWorse(confGivenWeight.BCR(),values,regression.reportCoefficients());
                                            }
                                    }
                                    progress.next();
                                }
                        }
                    }
                }
            for(SquareBagPlot plot:multToBCR.values())
                plot.reportResults(learningGroup.gr);
            for(Map.Entry<Pair<Integer,Double>,Map<BEST_WORST_BCR,BCRAndValues>> valuesForBCRKindEntry:bcrKindForChunkLenAndWeight.entrySet()) {
                for(Map.Entry<BEST_WORST_BCR,BCRAndValues> kindOfBcr_Values:valuesForBCRKindEntry.getValue().entrySet()) {
                    ScatterPlot gr_ScoreVsInconsistencyEachLearn = new ScatterPlot("Inconsistency", "Score",
                            new File(learningGroup.outPathPrefix + File.separator + description +
                                    "bcr_logistic_ch=" + valuesForBCRKindEntry.getKey().firstElem + "_vs_mult+" +
                                    valuesForBCRKindEntry.getKey().secondElem + "_"+kindOfBcr_Values.getKey()+"(bcr="+kindOfBcr_Values.getValue().bcr+","+kindOfBcr_Values.getValue().description+").pdf"));
                    for(PairQualityLearner.PairScoreValue pairScores:kindOfBcr_Values.getValue().values) {
                        long score = pairScores.score, inconsistency = pairScores.inconsistency;
                        if (score < 100 && inconsistency < 1000)
                            gr_ScoreVsInconsistencyEachLearn.add((double) inconsistency, (double) score, pairScores.validMerge ? "green" : "red", null);
                    }
                    gr_ScoreVsInconsistencyEachLearn.reportResults(learningGroup.gr);
                }
            }
        }
        return resultCSV;
    }
}
