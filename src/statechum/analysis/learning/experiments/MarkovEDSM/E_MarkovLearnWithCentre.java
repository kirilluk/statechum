package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Pair;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.util.*;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.RESULT_VALUES.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersColumnFromCSV;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovLearnWithCentre {
    public static final String description = "usingcentre";

    public static class MarkovLearningWithCentreParameters extends MarkovLearningParameters {

        public MarkovLearningWithCentreParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample);
        }

        @Override
        public String getSubExperimentName() {
            return description;
        }
    }

    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        int[] learnerExperiment = new int[]{0,1,2,3,4};
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + description+"-results.csv"), "results.csv");
        boolean aveOrMax = true;// average divide by the divisor
        boolean penaliseMissingPaths = true;
        int alphabetMultiplier = 2;
        boolean pathsOrSets = true;

        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample)
                {
                    for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{
                            learningGroup.getTracesLengthmultBaseline(states),
                                new Pair(1, MarkovExperiment.LearningExperimentGroupParameters.datasetSize*learningGroup.getScalingFactor(states))})
                    {
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
                                {
                                    int chunkSizeToEvaluate = 3;
//                                    double weightOfInconsistencies = 2.0;// good value for learning with 10 states
                                    double weightOfInconsistencies = 0.5;// good value for learning with 20 states
                                    for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 1)} : new Pair[]{new Pair(1, 1), new Pair(1, 2), new Pair(2, 4)}) {
                                        int wlen = wlen_divisor.firstElem, divisor = wlen_divisor.secondElem;
                                        ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                        ev.config = learningGroup.eval.config.copy();
                                        ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                        MarkovLearningParameters parameters = new MarkovLearningWithCentreParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
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
                }
            }

        learningGroup.experimentRunner.collectOutcomeOfExperiments(constructResultsCollector(resultCSV));

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            for (int states : learningGroup.statesToUse) {
                final RBoxPlot<String> gr_BestStructuralForDifferentPreset = new RBoxPlot<>("Trace length number and learner", "Structural Score",
                        new File(learningGroup.outPathPrefix + description+"_"+states + "_centre-learner_structural.pdf"));
                gr_BestStructuralForDifferentPreset.setOtherOptions("las=2");
                for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{
                        learningGroup.getTracesLengthmultBaseline(states),
                        new Pair(1, MarkovExperiment.LearningExperimentGroupParameters.datasetSize*learningGroup.getScalingFactor(states))}) {

                    int traceQuantityToUse = traces_lengthmult.firstElem;
                    final RBoxPlot<String> gr_PresetPerformance = new RBoxPlot<>("", "Structural Score",
                            new File(learningGroup.outPathPrefix + description+"_"+states + "_centre-learner_tracenum=" + traceQuantityToUse + "_structural.pdf"));
                    gr_PresetPerformance.setOtherOptions("las=2");
                    gr_PresetPerformance.setOrderingOfLabels(Arrays.asList(new String[]{"Best", "Markov", "M_Both", "M_Forward", "R_Forward", "R_Both"}));
                    final SquareBagPlot gr_StructuralDiffBest = new SquareBagPlot("Structural score, VH", "Structural Score",
                            new File(learningGroup.outPathPrefix + description+"_"+states + "_centre-learner_tracenum=" + traceQuantityToUse + "_VH_structuraldiffBest.pdf"), 0, 1, true);

                    String[] presetDescription = new String[]{"Markov", "M_Both", "R_Forward", "R_Both", "M_Forward"};

                    // Now select the best result from all those available
                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                        MarkovLearningParameters rowValues = parseMarkovParametersRowFromCSV(rowEntry.getKey());
                        if (rowValues.traceQuantity == traceQuantityToUse) {
                            // we are looking at specific rows
                            final Map<Integer, MarkovExperiment.LearningReport> bestLearningResultForThisRowAndAllPresets = new TreeMap<>();

                            MarkovExperiment.LearningReport bestLearningResultForThisRow = new MarkovExperiment.LearningReport();
                            for (final int preset : learnerExperiment) {
                                bestLearningResultForThisRowAndAllPresets.computeIfAbsent(preset, integer -> new MarkovExperiment.LearningReport());
                                MarkovExperiment.LearningReport bestLearningResultForThisRowAndPreset = bestLearningResultForThisRowAndAllPresets.get(preset);

                                getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV),
                                        (column, columnText, Y) -> {
                                            // Here columnText is the description of the learner used, Y is the values reported by processSubResult above.
                                            boolean learntOK = obtainStringValueFromCell(Y, RESULT_VALUES.E_SUCCESS, column).equals(LEARNING_OK.name);
                                            boolean alwaysPositive = obtainBooleanValueFromCell(Y, E_INCONSISTENCY_ALWAYSPOSITIVE,column);
                                            double bcr = obtainDoubleValueFromCell(Y, E_BCR,column);
                                            double structural = obtainDoubleValueFromCell(Y, E_DIFF,column);
                                            long inconsistency = obtainLongValueFromCell(Y, E_INCONSISTENCY_LEARNT,column);

                                            MarkovLearningParameters.ColumnParseOutcome columnValues=parseMarkovParametersColumnFromCSV(columnText);
        //                                    String[] columnValues = columnText.split("[_=]");
                                            if (learntOK && columnValues.learner == LearningAlgorithms.ScoringToApply.SCORING_MARKOV && columnValues.parameters.preset == preset) {
                                                // Now at the columns of interest (specific preset but different parameter of Markov)
                                                MarkovExperiment.LearningReport report = new MarkovExperiment.LearningReport(bcr, structural, inconsistency, alwaysPositive, columnText,Y, column);
                                                bestLearningResultForThisRowAndPreset.updateIfValueBetter(report);
                                                bestLearningResultForThisRow.updateIfValueBetter(report);
                                            }
                                        });
                            }

                            ColumnAndValue Y_VH = getValueFromMapGivenSelector(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_VH));
                            double vh_score = obtainDoubleValueFromCell(Y_VH.value, E_DIFF,Y_VH.column);
                            if (Y_VH != null)
                                gr_StructuralDiffBest.add(vh_score, bestLearningResultForThisRow.structural, null, null);
                            else
                                System.out.println("WARNING: missing VH-value for " + rowEntry.getKey());


                            StringBuilder sb = new StringBuilder();
                            Formatter formatter = new Formatter(sb, Locale.US);
                            formatter.format("%1d", traceQuantityToUse);
                            gr_BestStructuralForDifferentPreset.add(sb + "_M", bestLearningResultForThisRowAndAllPresets.get(0).structural);
                            gr_BestStructuralForDifferentPreset.add(sb + "_MC", bestLearningResultForThisRow.structural);
                            gr_BestStructuralForDifferentPreset.add(sb + "_S", vh_score);
                            for (Map.Entry<Integer, MarkovExperiment.LearningReport> entry : bestLearningResultForThisRowAndAllPresets.entrySet())
                                gr_PresetPerformance.add(presetDescription[entry.getKey()], entry.getValue().structural);
                            gr_PresetPerformance.add("Best", bestLearningResultForThisRow.structural);

                        }
                    }
                    gr_StructuralDiffBest.reportResults(learningGroup.gr);
                    gr_PresetPerformance.reportResults(learningGroup.gr);
                }
                List<String> labelValuesForComparativeAnalysis = new LinkedList<>();
                for (int traceQuantityToUse : new int[]{8, 1}) {
                    StringBuilder sb = new StringBuilder();
                    Formatter formatter = new Formatter(sb, Locale.US);
                    formatter.format("%1d", traceQuantityToUse);
                    labelValuesForComparativeAnalysis.add(sb + "_M");
                    labelValuesForComparativeAnalysis.add(sb + "_MC");
                    labelValuesForComparativeAnalysis.add(sb + "_S");
                }
                gr_BestStructuralForDifferentPreset.setOrderingOfLabels(labelValuesForComparativeAnalysis);
                gr_BestStructuralForDifferentPreset.reportResults(learningGroup.gr);
            }
        }
    }
}
