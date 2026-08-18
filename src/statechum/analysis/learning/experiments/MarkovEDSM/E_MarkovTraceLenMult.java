package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Pair;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.util.*;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.LearningExperimentGroupParameters.getTraceLenMultValues;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovTraceLenMult {
    public static final String description = "tracelenMult";

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
        boolean pathsOrSets = true;
        boolean penaliseMissingPaths = true;
        List<Integer> traceLenMultValues = getTraceLenMultValues();
        double alphabetMultiplier = 2;
        final int chunkSizeToEvaluate = 3;
        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample)
                    for (int traceLenMultValue:traceLenMultValues) {
                        int traceLenMult = traceLenMultValue * learningGroup.getScalingFactor(states);
                        int traceQuantityToUse = learningGroup.getTracesLengthmultBaseline(states).firstElem;
                        for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample)
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
//                                    for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? new int[]{3} : new int[]{2})
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

        final String numberFormat = "%3d";
        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            Set<RESULT_VALUES> validityOfCells = obtainValidityOfCellValues(resultCSV);checkFullTransitionCoverageAttained(resultCSV, validityOfCells);
            for (int states : learningGroup.statesToUse) {
                final RBoxPlot<String> gr_BestStructuralForLengthMultiplier = new RBoxPlot<>("Trace length multiplier", "Structural Score, EDSM-Markov",
                        new File(learningGroup.outPathPrefix + description + "_" + states + "_lengthmult_structural.pdf"));
                gr_BestStructuralForLengthMultiplier.setupForTwoLineXLabels();

                final Map<Integer, SquareBagPlot> gr_StructuralDiffBestMap = new TreeMap<>();
                Map<Integer, FilterCollectionOfResultsForBestPerformingLearner> learnerToHowOftenBestForAllMultipliers = new TreeMap<>();

                for (final int traceLenMultValue : traceLenMultValues) {
                    int traceLenMult = traceLenMultValue * learningGroup.getScalingFactor(states);
                    // Now select the best result from all those available
                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                        MarkovLearningParameters rowValues = parseMarkovParametersRowFromCSV(rowEntry.getKey());
                        if (rowValues.traceLengthMultiplier == traceLenMult) {
                            gr_StructuralDiffBestMap.computeIfAbsent(traceLenMult, aDouble ->
                                    new SquareBagPlot("Structural score, VH", "Structural Score, EDSM-Markov",
                                            new File(learningGroup.outPathPrefix + description + "_" + states + "_tracemult_tracelen=" + traceLenMult + "_VH_structuraldiffBest.pdf"), 0, 1, true));
                            gr_StructuralDiffBestMap.get(traceLenMult).setOtherOptions("las=2");
                        }
                    }

                    FilterCollectionOfResultsForBestPerformingLearner report = new FilterCollectionOfResultsForBestPerformingLearner(states, -1,
                            rowHeader -> rowHeader.traceLengthMultiplier == traceLenMult,
                            columnParse -> columnParse.parameters.chunkLen == chunkSizeToEvaluate,
                            resultCSV,validityOfCells);
                    report.getResultForBestPerformingMarkovLearner(gr_StructuralDiffBestMap.get(traceLenMult), null,
                            (pair) -> {
                                double markov = pair.firstElem, vh_score = pair.secondElem;
                                StringBuilder sb = new StringBuilder();
                                Formatter formatter = new Formatter(sb, Locale.US);
                                formatter.format(numberFormat, traceLenMult);
                                gr_BestStructuralForLengthMultiplier.add("M\n"+sb, markov);
                                gr_BestStructuralForLengthMultiplier.add("VH\n"+sb, vh_score);
                            }, null);
                    learnerToHowOftenBestForAllMultipliers.computeIfAbsent(traceLenMult, aDouble -> report);
                }

                List<String> ordering = new LinkedList<>();
                for (final int traceLenMultValue : traceLenMultValues) {
                    int traceLenMult = traceLenMultValue * learningGroup.getScalingFactor(states);
                    StringBuilder sb = new StringBuilder();
                    Formatter formatter = new Formatter(sb, Locale.US);
                    formatter.format(numberFormat, traceLenMult);
                    ordering.add("M\n"+sb);
                    ordering.add("VH\n"+sb);
                }
                gr_BestStructuralForLengthMultiplier.setOrderingOfLabels(ordering);

                for (final int traceLenMultValue : traceLenMultValues) {
                    int traceLenMult = traceLenMultValue * learningGroup.getScalingFactor(states);
                    gr_StructuralDiffBestMap.get(traceLenMult).reportResults(learningGroup.gr);
                }
                gr_BestStructuralForLengthMultiplier.reportResults(learningGroup.gr);
            }
        }
    }
}
