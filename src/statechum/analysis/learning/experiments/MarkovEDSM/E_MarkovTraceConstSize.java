package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Pair;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.io.IOException;
import java.util.*;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.constructResultsCollector;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovTraceConstSize {

    public static final String description = "constant_size";

    public static class MarkovAlphabetLearningParameters extends MarkovLearningParameters {

        public MarkovAlphabetLearningParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample);
        }

        @Override
        public String getSubExperimentName() {
            return description;
        }
    }

    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        int[] learnerExperiment = new int[]{0};//0,1,2,3
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + description+"-results.csv"), "results.csv");
        boolean aveOrMax = true;// average divide by the divisor
        boolean penaliseMissingPaths = true;

        boolean pathsOrSets = true;
        List<Integer> traceLenMultValues = new LinkedList<>();
        for(int i=4;i <= MarkovExperiment.LearningExperimentGroupParameters.datasetSize;i <<= 1)
            traceLenMultValues.add(i);
        double alphabetMultiplier = 2;
        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample)
                    for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample)
                        for (int traceLenMultV:traceLenMultValues) {
                            int scalingFactor = learningGroup.getScalingFactor(states);

                            int traceLenMult = traceLenMultV * scalingFactor;
                            int traceQuantityToUse = MarkovExperiment.LearningExperimentGroupParameters.datasetSize * scalingFactor/ traceLenMult;
                            for (final int preset : learnerExperiment)
                                for (LearningAlgorithms.ScoringToApply learnerKind :
                                        preset == 0 ?// this is the only case where we can apply PTA-based merging algorithms, two other presets handle merging vertices in a connected graph
                                                new LearningAlgorithms.ScoringToApply[]{
                                                        LearningAlgorithms.ScoringToApply.SCORING_MARKOV,
                                                        LearningAlgorithms.ScoringToApply.SCORING_VH
                                                } :
                                                new LearningAlgorithms.ScoringToApply[]{
                                                        LearningAlgorithms.ScoringToApply.SCORING_MARKOV
                                                })
                                    for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? new int[]{3, 4} : new int[]{2})
                                        for (double weightOfInconsistencies : learnerKind.isMarkov() ? new double[]{0.5, 1.0, 2.0} : new double[]{1.0})
                                            for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 1)} : new Pair[]{new Pair(1, 1), new Pair(1, 2), new Pair(2, 4)}) {
                                                int wlen = wlen_divisor.firstElem, divisor = wlen_divisor.secondElem;
                                                ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                                ev.config = learningGroup.eval.config.copy();
                                                ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                                MarkovAlphabetLearningParameters parameters = new MarkovAlphabetLearningParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
                                                parameters.setTraceLengthMultiplier(traceLenMult);
                                                parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, alphabetMultiplier);
                                                parameters.markovParameters.setMarkovParameters(preset, chunkSizeToEvaluate, pathsOrSets,
                                                        new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies, 0), penaliseMissingPaths, aveOrMax, divisor, 0, wlen);
                                                parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                                MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovExperiment.MarkovLearnerRunner(parameters, ev);
                                                learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                                learningGroup.experimentRunner.submitTask(learnerRunner);
                                            }
                        }
           }

        learningGroup.experimentRunner.collectOutcomeOfExperiments(constructResultsCollector(resultCSV));


        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            for (int states : learningGroup.statesToUse) {
                final RBoxPlot<String> gr_BestStructuralForLengthMultiplier = new RBoxPlot<>("Trace length multiplier", "Structural Score, EDSM-Markov learner",
                        new File(learningGroup.outPathPrefix + description + "_" + states + "_constsize_mult_structural.pdf"));
                final Map<Integer, SquareBagPlot> gr_StructuralDiffBestMap = new TreeMap<>();
                Map<Integer, FilterCollectionOfResultsForBestPerformingLearner> learnerToHowOftenBestForAllMultipliers = new TreeMap<>();

                for (final int traceLenMult : traceLenMultValues) {
                    // Now select the best result from all those available
                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                        String[] elems = rowEntry.getKey().split("[_=]");
                        assert elems[20].equals("tM");
                        if (Double.parseDouble(elems[21]) == traceLenMult) {
                            final MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport();
                            gr_StructuralDiffBestMap.computeIfAbsent(traceLenMult, aDouble ->
                                    new SquareBagPlot("Structural score, VH", "Structural Score, EDSM-Markov learner",
                                            new File(learningGroup.outPathPrefix + description + "_" + states + "_constant_size_tracelen=" + traceLenMult + "_constsize_VH_structuraldiffBest.pdf"), 0, 1, true));

                            FilterCollectionOfResultsForBestPerformingLearner report = new FilterCollectionOfResultsForBestPerformingLearner(states, -1, resultCSV);
                            report.getResultForBestPerformingMarkovLearner(gr_StructuralDiffBestMap.get(traceLenMult), null);
                            learnerToHowOftenBestForAllMultipliers.computeIfAbsent(traceLenMult,aInteger -> report);

                            String Y_VH = getValueFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_VH + "-0");
                            if (Y_VH != null) {
                                double vh_score = Double.parseDouble(obtainValueFromCell(Y_VH, 2));
                                gr_StructuralDiffBestMap.get(traceLenMult).add(vh_score, bestLearningResult.structural, null, null);
                                StringBuilder sb = new StringBuilder();
                                Formatter formatter = new Formatter(sb, Locale.US);
                                formatter.format("%3d", traceLenMult);
                                gr_BestStructuralForLengthMultiplier.add(sb + "_M", bestLearningResult.structural);
                                gr_BestStructuralForLengthMultiplier.add(sb + "_S", vh_score);
                            } else
                                System.out.println("WARNING: missing VH-value for " + rowEntry.getKey());
                        }

                    }
                }

                for (final int traceLenMult : traceLenMultValues) {
                    System.out.println("traceLenMult Multiplier: " + traceLenMult);

                    gr_StructuralDiffBestMap.get(traceLenMult).reportResults(learningGroup.gr);
                    learnerToHowOftenBestForAllMultipliers.get(traceLenMult).reportResults();
                }
                gr_BestStructuralForLengthMultiplier.reportResults(learningGroup.gr);
            }
        }
    }
}
