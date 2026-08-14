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
            for(double value:chunkLenToWeights.get(chunkLen))
                multToBCR.put(new Pair<>(chunkLen,value),new DrawGraphs.SquareBagPlot("BCR, logistic regression", "BCR, "+value,
                    new File(learningGroup.outPathPrefix + File.separator + description + "bcr_logistic_ch="+chunkLen+"_vs_mult+"+value+".pdf"), 0.5, 1, true));

//            final DrawGraphs.SquareBagPlot bcrLogisticRegressionVsMultiplicationBest = new DrawGraphs.SquareBagPlot("BCR, logistic regression", "BCR, 1.0",
//                    new File(learningGroup.outPathPrefix + File.separator + description + "bcr_logistic_vs_mult_best.pdf"), 0.5, 1, true);

            int numberOfPoints = 0;
            for (int states_C : learningGroup.statesToUse)
                for (int ignoredA : MarkovExperiment.densityFromStateNumber(states_C))
                    for (int sample_C = 0; sample_C < learningGroup.fsmSamplesPerStateNumber; ++sample_C)
                        for (final Pair<Integer, Integer> ignoredB : new Pair[]{learningGroup.getTracesLengthmultBaseline(states_C)})
                            for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample)
                                for (final int ignoredC : chunkSizeValues)
                                    for (final boolean ignoredD:penaliseMissingPathsValues)
                                        ++numberOfPoints;

            ProgressIndicator progress = new ProgressIndicator("Reporting results",numberOfPoints);
            for (int states : learningGroup.statesToUse)
                for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
//                    String experimentName = learningGroup.outPathPrefix + "statistics_"+states+"_"+perStateSquaredDensity100+"_";
//                    final Map<Integer,ScatterPlot> gr_ScoreVsInconsistency = new TreeMap<>();
//                    for (final int chunkSizeToEvaluate : chunkSizeValues) {
//                        gr_ScoreVsInconsistency.put(chunkSizeToEvaluate, new ScatterPlot("Inconsistency", "Score",
//                                new File(learningGroup.outPathPrefix + "statistics_" + states + "_" + perStateSquaredDensity100 + "_" + chunkSizeToEvaluate + "_"+penaliseMissingPaths+"_score_vs_inconsistency.pdf")));
//                    }
                    for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample) {
                        for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{learningGroup.getTracesLengthmultBaseline(states)}) {
                            int traceQuantityToUse = traces_lengthmult.firstElem;
                            for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample)
                                for (final int chunkSizeToEvaluate : chunkSizeValues)
                                for (final boolean penaliseMissingPaths:penaliseMissingPathsValues)
                                {
                                    progress.next();
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
                                        String pathSuffix = "statistics_" + states + "_" + perStateSquaredDensity100 + "_" + chunkSizeToEvaluate + "_"+penaliseMissingPaths+"_score_vs_inconsistency.pdf";
                                        // Created for each learning attempt
//                                        ScatterPlot gr_ScoreVsInconsistencyEachLearn = new ScatterPlot("Inconsistency", "Score",
//                                                new File(pathName + File.separator+pathSuffix));
//                                        ScatterPlot gr_ScoreVsInconsistency = pathToScatterPlot.computeIfAbsent(pathSuffix,suffix -> new ScatterPlot("Inconsistency", "Score",
//                                                new File(learningGroup.outPathPrefix + File.separator+ sanitiseFileName(parameters.getSubExperimentName())+suffix)));

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
//                                            if (score < 100 && inconsistency < 1000)
//                                                gr_ScoreVsInconsistencyEachLearn.add((double) inconsistency, (double) score, validMerge ? "green" : "red", null);
//                                                gr_ScoreVsInconsistency.add((double) inconsistency, (double) score, validMerge ? "green" : "red", null);
//                                                gr_ScoreVsInconsistency.get(chunkSizeToEvaluate).add((double) inconsistency, (double) score, validMerge ? "green" : "red", null);
                                        }

//                                        gr_ScoreVsInconsistencyEachLearn.reportResults(learningGroup.gr);


                                        DrawGraphs.LogisticRegression regression = new DrawGraphs.LogisticRegression(values,"fit","pairvalues");
//                                        System.out.println(pathName+" , "+ states + "_" + perStateSquaredDensity100 + "_" + chunkSizeToEvaluate + "_"+penaliseMissingPaths+" : "+regression.reportNormalisedCoefficients());
                                        ConfusionMatrix confUsingLogisticRegression = regression.computeConfusionMatrix(values);
//                                        System.out.println("Logistic regression: "+confUsingLogisticRegression+" F1="+confUsingLogisticRegression.fMeasure()+", BCR="+confUsingLogisticRegression.BCR());
//                                        double bestBcr=0.0;
                                        /*
                                        for(double consideredWeightOfInconsistencies:new double[]{0.25,0.5,1.,2.})
                                            for(double offset:new double[]{0})//,0.25,0.5,1})
                                            {
                                                ConfusionMatrix confGivenWeight = LogisticRegression.computeConfusionMatrixGivenWeightOfInconsistencies(values,consideredWeightOfInconsistencies,offset);
//                                                if (confGivenWeight.BCR() > bestBcr) bestBcr = confGivenWeight.BCR();
                                                for(Map.Entry<Pair<Integer,Double>,SquareBagPlot> mult_and_plot:multToBCR.entrySet())
                                                    if (mult_and_plot.getKey().firstElem == chunkSizeToEvaluate && Math.abs(consideredWeightOfInconsistencies - mult_and_plot.getKey().secondElem) < 1e-7)
                                                        mult_and_plot.getValue().add(confUsingLogisticRegression.BCR(), confGivenWeight.BCR());
    //                                            System.out.println("Score-"+consideredWeightOfInconsistencies+"*inconsistency >= "+offset+": "+confGivenWeight+" F1="+confGivenWeight.fMeasure()+", BCR="+confGivenWeight.BCR());
                                            }
//                                        bcrLogisticRegressionVsMultiplicationBest.add(confUsingLogisticRegression.BCR(), bestBcr);

                                        */
                                        for(Map.Entry<Pair<Integer,Double>,SquareBagPlot> mult_and_plot:multToBCR.entrySet())
                                            if (mult_and_plot.getKey().firstElem == chunkSizeToEvaluate) {
                                                ConfusionMatrix confGivenWeight = LogisticRegression.computeConfusionMatrixGivenWeightOfInconsistencies(values,mult_and_plot.getKey().secondElem,0.0);
                                                mult_and_plot.getValue().add(confUsingLogisticRegression.BCR(), confGivenWeight.BCR());
                                            }
                                    }
                                }
                        }
                    }
//                    for (final int chunkSizeToEvaluate : chunkSizeValues) {
//                        gr_ScoreVsInconsistency.get(chunkSizeToEvaluate).reportResults(learningGroup.gr);
//                    }
                }
//            for(ScatterPlot plot:pathToScatterPlot.values())
//                plot.reportResults(learningGroup.gr);
            for(SquareBagPlot plot:multToBCR.values())
                plot.reportResults(learningGroup.gr);
//            bcrLogisticRegressionVsMultiplicationBest.reportResults(learningGroup.gr);
        }
        return resultCSV;
    }
}
