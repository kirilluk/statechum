package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Pair;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.util.*;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_TIMEOUT;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovAlphabet {
    public static final String description = "alphabet";

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
        double [] alphabetMultValues = new double [] {0.5,1, 2, 4};
        final int chunkSizeToEvaluate = 3;
        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample)
                    for(final double alphabetMultiplier:alphabetMultValues) {
                        for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{learningGroup.getTracesLengthmultBaseline(states)}) {
                            int traceQuantityToUse = traces_lengthmult.firstElem;
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
//                                        for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? new int[]{3} : new int[]{2})
                                            for (double weightOfInconsistencies : learnerKind.isMarkov() ? new double[]{0.5, 1.0, 2.0} : new double[]{1.0})
                                                for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 1)} : new Pair[]{new Pair(1, 1), new Pair(1, 2), new Pair(2, 4)}) {
                                                    int wlen = wlen_divisor.firstElem, divisor = wlen_divisor.secondElem;
                                                    ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                                    ev.config = learningGroup.eval.config.copy();
                                                    ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                                    MarkovAlphabetLearningParameters parameters = new MarkovAlphabetLearningParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
                                                    parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
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
            }

        final String numberFormat = "%-3.1f";
        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            Set<RESULT_VALUES> validityOfCells = obtainValidityOfCellValues(resultCSV);checkFullTransitionCoverageAttained(resultCSV, validityOfCells);
            for (int states : learningGroup.statesToUse) {
                final RBoxPlot<String> gr_BestStructuralForAlphabet = new RBoxPlot<>("Alphabet multiplier and learner", "Structural Score, EDSM-Markov",
                        new File(learningGroup.outPathPrefix + description + "_" + states + "_alphabetmult_structural.pdf"));
//                gr_BestStructuralForAlphabet.setOtherOptions("las=1");
                gr_BestStructuralForAlphabet.setXLine(3);
                gr_BestStructuralForAlphabet.setYLine(2);
                gr_BestStructuralForAlphabet.setMargins(4,3,0.2,0.2);
                gr_BestStructuralForAlphabet.setMgpLabelX(0);
                gr_BestStructuralForAlphabet.setMgpLabelY(0.7);
                gr_BestStructuralForAlphabet.configureTextLabels(-0.15,1,0.5);
                final Map<Double, SquareBagPlot> gr_StructuralDiffBestMap = new TreeMap<>();
                Map<Double, FilterCollectionOfResultsForBestPerformingLearner> learnerToHowOftenBestForAllMultipliers = new TreeMap<>();

                for (final double alphabetMultiplier : alphabetMultValues) {
                    final DrawGraphs.RBagPlot gr_StructuralVsInconsistency = new DrawGraphs.RBagPlot("Inconsistency Learnt", "Structural Score",
                            new File(learningGroup.outPathPrefix + description + "_" + states + "alphabet_alphabetmult=" + alphabetMultiplier + "_inconsistency_structural.pdf"));

                    // Now select the best result from all those available
                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                        MarkovLearningParameters rowValues = parseMarkovParametersRowFromCSV(rowEntry.getKey());
                        if (rowValues.alphabetMultiplier == alphabetMultiplier && rowValues.states == states) {

                            gr_StructuralDiffBestMap.computeIfAbsent(alphabetMultiplier, aDouble ->
                                        new SquareBagPlot("Structural score, VH", "Structural Score, EDSM-Markov",
                                                new File(learningGroup.outPathPrefix + description + "_" + states + "alphabet_alphabetmult=" + alphabetMultiplier + "_VH_structuraldiffBest.pdf"), 0, 1, true)
                                );

                            getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), validityOfCells,
                                    (column, columnText, Y) -> {
                                        boolean learntOK = obtainStringValueFromCell(Y, RESULT_VALUES.E_SUCCESS, column).equals(LEARNING_OK.name);
                                        if (learntOK)
                                            gr_StructuralVsInconsistency.add(obtainDoubleValueFromCell(Y,RESULT_VALUES.E_INCONSISTENCY_LEARNT,column),
                                                    obtainDoubleValueFromCell(Y,RESULT_VALUES.E_DIFF,column));
                                    });
                        }
                    }

                    gr_StructuralVsInconsistency.reportResults(learningGroup.gr);

                    FilterCollectionOfResultsForBestPerformingLearner report = new FilterCollectionOfResultsForBestPerformingLearner(states, -1,
                            rowHeader -> rowHeader.alphabetMultiplier == alphabetMultiplier,
                            columnParse -> columnParse.parameters.chunkLen == chunkSizeToEvaluate,
                            resultCSV,validityOfCells);

                    report.getResultForBestPerformingMarkovLearner(gr_StructuralDiffBestMap.get(alphabetMultiplier), null,
                            (pair) -> {
                                double markov = pair.firstElem, vh_score = pair.secondElem;
                                StringBuilder sb = new StringBuilder();
                                Formatter formatter = new Formatter(sb, Locale.US);
                                formatter.format(numberFormat, alphabetMultiplier);
                                gr_BestStructuralForAlphabet.add("M\n"+sb, markov);
                                gr_BestStructuralForAlphabet.add("VH\n"+sb, vh_score);
                            }, null);
                    learnerToHowOftenBestForAllMultipliers.computeIfAbsent(alphabetMultiplier, aDouble -> report);
                }

                List<String> ordering = new LinkedList<>();
                for(final double alphabetMultiplier : alphabetMultValues) {
                    StringBuilder sb = new StringBuilder();
                    Formatter formatter = new Formatter(sb, Locale.US);
                    formatter.format(numberFormat, alphabetMultiplier);
                    ordering.add("M\n"+sb);
                    ordering.add("VH\n"+sb);
                }
                gr_BestStructuralForAlphabet.setOrderingOfLabels(ordering);

                for (final double alphabetMultiplier : alphabetMultValues)
                    gr_StructuralDiffBestMap.get(alphabetMultiplier).reportResults(learningGroup.gr);

                gr_BestStructuralForAlphabet.reportResults(learningGroup.gr);
            }
        }
    }
}
