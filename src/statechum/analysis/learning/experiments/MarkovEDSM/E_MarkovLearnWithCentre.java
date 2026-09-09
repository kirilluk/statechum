package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Pair;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.RESULT_VALUES.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersColumnFromCSV;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_TIMEOUT;

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
        final DatapointsCollection resultCSV = new DatapointsCollection(learningGroup.outPathPrefix, learningGroup.copyToPrefix, learningGroup.moveToPrefix, description, true);
        MarkovExperiment.PreGeneratePTA tasks = new MarkovExperiment.PreGeneratePTA(learningGroup.phase, learningGroup.experimentRunner);
        boolean aveOrMax = true;// average divide by the divisor
        boolean penaliseMissingPaths = true;
        int alphabetMultiplier = 2;
        boolean pathsOrSets = true;
        final int chunkSizeToEvaluate = 3;
//        final double weightOfInconsistencies = 0.5;
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
                                                        LearningAlgorithms.ScoringToApply.SCORING_HV
                                                } :
                                                new LearningAlgorithms.ScoringToApply[]{
                                                        LearningAlgorithms.ScoringToApply.SCORING_MARKOV
                                                })
                                for (double weightOfInconsistencies : learnerKind.isMarkov() ? new double[]{0.5,1.0}: new double[]{1.0})
                                {
                                    for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 4)} : new Pair[]{new Pair(1, 8), new Pair(2, 8)}) {
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
                                        parameters.disableReportMergeStatisticsWhenSolutionIsKnown();
                                        MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovExperiment.MarkovLearnerRunner(learningGroup.outPathPrefix, parameters, ev);
                                        learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                        tasks.submitTask(learnerRunner);
                                    }
                                }
                    }
                }
            }

        tasks.generatePTAAndSubmitTasks();// this will generate PTAs and submit tasks to the runner as needed.
        learningGroup.experimentRunner.collectOutcomeOfExperiments(constructResultsCollector(resultCSV));

        final String numberFormat = "%1d";
        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            Set<RESULT_VALUES> validityOfCells = obtainValidityOfCellValues(description,resultCSV);checkFullTransitionCoverageAttained(description, resultCSV, validityOfCells);

            // Obtain the smallest timeout value used anywhere (used as a cap on reported time).
            AtomicInteger timeoutValueObtained = new AtomicInteger(Integer.MAX_VALUE);
            for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet())
                getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), validityOfCells,
                        (column, columnText, Y) -> {
                            boolean learntTimeout = obtainStringValueFromCell(Y, RESULT_VALUES.E_SUCCESS, column).equals(LEARNING_TIMEOUT.name);
                            if (learntTimeout) {
                                int runtime = (int) Math.round(obtainDoubleValueFromCell(Y, E_RUNTIME, column));
                                timeoutValueObtained.accumulateAndGet(runtime, (a, b) -> Math.min(a, b));
                            }
                        });

            final int timeCapForFasterLearning = 20;// 20 sec

            for (int states : learningGroup.statesToUse) {
                final RBoxPlot<String> gr_BestStructuralForDifferentPreset = new RBoxPlot<>("Trace length number and learner", "Structural Score, EM",
                        new File(learningGroup.outPathPrefix + File.separator + description+"_"+states + "_centre-learner_structural.pdf"));
                gr_BestStructuralForDifferentPreset.setupForTwoLineXLabels();
                Map<Double,RBoxPlot<String>> structuralForWeightAndPreset = new HashMap<>();

                for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{
                        learningGroup.getTracesLengthmultBaseline(states),
                        new Pair(1, MarkovExperiment.LearningExperimentGroupParameters.datasetSize*learningGroup.getScalingFactor(states))}) {

                    int traceQuantityToUse = traces_lengthmult.firstElem;
                    int traceLength =  traces_lengthmult.secondElem * states;
                    final RBoxPlot<String> gr_PresetPerformanceBest = new RBoxPlot<>("Number of traces and learner", "Structural Score, EM",
                            new File(learningGroup.outPathPrefix + File.separator + description+"_"+states + "_centre-learner_tracenum=" + traceQuantityToUse + "_tracelength="+traceLength+"_structural.pdf"));
                    gr_PresetPerformanceBest.setupForTwoLineXLabels();
                    gr_PresetPerformanceBest.setOrderingOfLabels(Arrays.asList("EM", "M\nB", "M\nF", "R\nF", "R\nB"));
                    final RBoxPlot<String> gr_PresetTimeCappedPerformanceBest = new RBoxPlot<>("Number of traces and learner", "Structural Score, EM",
                            new File(learningGroup.outPathPrefix + File.separator + description+"_"+states + "_centre-learner_tracenum=" + traceQuantityToUse + "_tracelength="+traceLength+"_timecapped_structural.pdf"));
                    gr_PresetTimeCappedPerformanceBest.setupForTwoLineXLabels();
                    gr_PresetTimeCappedPerformanceBest.setOrderingOfLabels(Arrays.asList("EM", "M\nB", "M\nF", "R\nF", "R\nB"));
                    final RBoxPlot<String> gr_PresetRuntimeBest = new RBoxPlot<>("Number of traces and learner", "Runtime, sec",
                            new File(learningGroup.outPathPrefix + File.separator + description+"_"+states + "_centre-learner_tracenum=" + traceQuantityToUse + "_tracelength="+traceLength+"_runtime.pdf"));
                    gr_PresetRuntimeBest.setupForTwoLineXLabels();
                    gr_PresetRuntimeBest.setOrderingOfLabels(Arrays.asList("EM", "M\nB", "M\nF", "R\nF", "R\nB"));
                    gr_PresetRuntimeBest.setYLine(3);
                    gr_PresetRuntimeBest.setMargins(4,4,0.2,0.2);
                    final RBoxPlot<String> gr_PresetTimeCappedRuntimeBest = new RBoxPlot<>("Number of traces and learner", "Runtime, sec",
                            new File(learningGroup.outPathPrefix + File.separator + description+"_"+states + "_centre-learner_tracenum=" + traceQuantityToUse + "_tracelength="+traceLength+"_timecapped_runtime.pdf"));
                    gr_PresetTimeCappedRuntimeBest.setupForTwoLineXLabels();
                    gr_PresetTimeCappedRuntimeBest.setOrderingOfLabels(Arrays.asList("EM", "M\nB", "M\nF", "R\nF", "R\nB"));
                    gr_PresetTimeCappedRuntimeBest.setYLine(3);
                    gr_PresetTimeCappedRuntimeBest.setMargins(4,4,0.2,0.2);
                    final RBoxPlot<String> gr_PresetRuntimeBestCapped = new RBoxPlot<>("Number of traces and learner", "Runtime, sec",
                            new File(learningGroup.outPathPrefix + File.separator + description+"_"+states + "_centre-learner_tracenum=" + traceQuantityToUse + "_tracelength="+traceLength+"_runtime_capped.pdf"));
                    gr_PresetRuntimeBestCapped.setupForTwoLineXLabels();
                    gr_PresetRuntimeBestCapped.setOrderingOfLabels(Arrays.asList("EM", "M\nB", "M\nF", "R\nF", "R\nB"));
                    gr_PresetRuntimeBestCapped.setYLine(3);
                    gr_PresetRuntimeBestCapped.setMargins(4,4,0.2,0.2);
//                    gr_PresetPerformanceBest.configureTextLabels(-0.42,0,0);
//                    gr_PresetPerformanceBest.setLabelsAuto(RGraph.PLOT_X_LABELS.XLABELS_TEXT_MANUAL);
//                    gr_PresetPerformanceBest.setXLine(5);
//                    gr_PresetPerformanceBest.setMargins(6,3,0.2,0.2);

                    final Map<Double,RBoxPlot<String>> weightToResults = new HashMap<>();

                    String[] presetDescription = new String[]{"EM", "M\nB", "R\nF", "R\nB", "M\nF"};

                    // Now select the best result from all those available
                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                        MarkovLearningParameters rowValues = parseMarkovParametersRowFromCSV(rowEntry.getKey());
                        if (rowValues.traceQuantity == traceQuantityToUse && rowValues.states == states) {
                            // we are looking at specific rows
                            final Map<Integer, MarkovExperiment.LearningReport> bestLearningResultForThisRowAndAllPresets = new TreeMap<>(),
                                    bestTimeCappedLearningResultForThisRowAndAllPresets = new TreeMap<>();
                            final Map<Integer, AtomicInteger> runtimeBestLearningResultForThisRowAndAllPresets = new TreeMap<>(),
                                    runtimeBestCappedLearningResultForThisRowAndAllPresets = new TreeMap<>(),
                                    attemptsForThisRowAndAllPresets = new TreeMap<>();
                            final Map<Double,Map<Integer, MarkovExperiment.LearningReport>> learningResultForThisRowAndAllWeightsAndPresets = new TreeMap<>();
                            for (final int preset : learnerExperiment) {
                                MarkovExperiment.LearningReport bestLearningResultForThisRowAndPreset = bestLearningResultForThisRowAndAllPresets
                                        .computeIfAbsent(preset, integer -> new MarkovExperiment.LearningReport());
                                MarkovExperiment.LearningReport bestTimeCappedLearningResultForThisRowAndPreset = bestTimeCappedLearningResultForThisRowAndAllPresets
                                        .computeIfAbsent(preset, integer -> new MarkovExperiment.LearningReport());

                                getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV),validityOfCells,
                                        (column, columnText, Y) -> {
                                            // Here columnText is the description of the learner used, Y is the values reported by processSubResult above.
                                            boolean learntOK = obtainStringValueFromCell(Y, RESULT_VALUES.E_SUCCESS, column).equals(LEARNING_OK.name);
                                            boolean alwaysPositive = obtainBooleanValueFromCell(Y, E_INCONSISTENCY_ALWAYSPOSITIVE,column);
                                            double bcr = obtainDoubleValueFromCell(Y, E_BCR,column);
                                            double structural = obtainDoubleValueFromCell(Y, E_DIFF,column);
                                            long inconsistency = obtainLongValueFromCell(Y, E_INCONSISTENCY_LEARNT,column);

                                            MarkovLearningParameters.ColumnParseOutcome columnValues=parseMarkovParametersColumnFromCSV(columnText,validityOfCells);

                                            if (columnValues.learner == LearningAlgorithms.ScoringToApply.SCORING_MARKOV && columnValues.parameters.preset == preset) {
                                                // Now at the columns of interest (specific preset but different parameter of Markov)
                                                MarkovExperiment.LearningReport report = new MarkovExperiment.LearningReport(bcr, structural, inconsistency, alwaysPositive, columnText,Y, column);
                                                int experimentRuntime = (int)E_MarkovCaseStudies.capToTimeout(
                                                        obtainDoubleValueFromCell(Y, E_RUNTIME,column),timeoutValueObtained);
                                                if (learntOK)
                                                    bestLearningResultForThisRowAndPreset.updateIfValueBetter(report);
                                                // update runtime regardless of success
                                                runtimeBestLearningResultForThisRowAndAllPresets.computeIfAbsent(preset, integer -> new AtomicInteger(0)).
                                                        addAndGet(experimentRuntime);
                                                learningResultForThisRowAndAllWeightsAndPresets.computeIfAbsent(columnValues.parameters.weightOfInconsistencies.weight, w -> new HashMap<>())
                                                        .computeIfAbsent(preset, p -> new MarkovExperiment.LearningReport()).updateIfValueBetter(report);

                                                // Now evaluate a hypothetical timecapped learner.
                                                if (learntOK && experimentRuntime < timeCapForFasterLearning)
                                                    bestTimeCappedLearningResultForThisRowAndPreset.updateIfValueBetter(report);
                                                runtimeBestCappedLearningResultForThisRowAndAllPresets.computeIfAbsent(preset, integer -> new AtomicInteger(0)).
                                                        addAndGet(Math.min(timeCapForFasterLearning,experimentRuntime));

                                                attemptsForThisRowAndAllPresets.computeIfAbsent(preset, integer -> new AtomicInteger(0)).
                                                        incrementAndGet();
                                            }
                                        });
                            }

//                            final int preset_M_Both = 1;
//                            if (bestLearningResultForThisRow.structural < bestLearningResultForThisRowAndAllPresets.get(preset_M_Both).structural) {
//                                System.out.print(rowEntry.getKey()+" : "+
//                                        bestLearningResultForThisRow.structural + "(inconsistency "+bestLearningResultForThisRow.inconsistency+
//                                        " ), preset "+presetDescription[preset_M_Both]+" gives "+
//                                        bestLearningResultForThisRowAndAllPresets.get(preset_M_Both).structural +
//                                        "(inconsistency "+bestLearningResultForThisRowAndAllPresets.get(preset_M_Both).inconsistency+" )");
//                            }

                            for(Map.Entry<Integer,AtomicInteger> entry:attemptsForThisRowAndAllPresets.entrySet())
                                if (entry.getKey() == 0) // EM
                                    assert entry.getValue().get() == 2:"unexpected number of attempts for EM";
                                else
                                    assert entry.getValue().get() == 4:"unexpected number of attempts for preset "+presetDescription[entry.getKey()];

                            ColumnAndValue Y_HV = getValueFromMapGivenSelector(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_HV),validityOfCells);
                            Double hv_score = Y_HV != null? obtainDoubleValueFromCell(Y_HV.value, E_DIFF,Y_HV.column): null;

                            StringBuilder sb = new StringBuilder();
                            Formatter formatter = new Formatter(sb, Locale.US);
                            formatter.format(numberFormat+","+numberFormat, traceQuantityToUse,traceLength);
                            gr_BestStructuralForDifferentPreset.add("EM\n"+sb, bestLearningResultForThisRowAndAllPresets.get(0).structural);// EM
                            gr_BestStructuralForDifferentPreset.add("EMC\n"+sb, bestLearningResultForThisRowAndAllPresets.get(1).structural);// M_BOTH
                            if (hv_score!= null)
                                gr_BestStructuralForDifferentPreset.add("HV\n"+sb, hv_score);
                            for (Map.Entry<Integer, MarkovExperiment.LearningReport> entry : bestLearningResultForThisRowAndAllPresets.entrySet()) {
                                gr_PresetPerformanceBest.add(presetDescription[entry.getKey()], entry.getValue().structural);
                                gr_PresetRuntimeBest.add(presetDescription[entry.getKey()], (double) runtimeBestLearningResultForThisRowAndAllPresets.get(entry.getKey()).get());
                                gr_PresetRuntimeBestCapped.add(presetDescription[entry.getKey()], (double) Math.min(runtimeBestLearningResultForThisRowAndAllPresets.get(entry.getKey()).get(), 3600));
                            }
                            for (Map.Entry<Integer, MarkovExperiment.LearningReport> entry : bestTimeCappedLearningResultForThisRowAndAllPresets.entrySet()) {
                                gr_PresetTimeCappedPerformanceBest.add(presetDescription[entry.getKey()], entry.getValue().structural);
                                gr_PresetTimeCappedRuntimeBest.add(presetDescription[entry.getKey()], (double)runtimeBestCappedLearningResultForThisRowAndAllPresets.get(entry.getKey()).get());
                            }
//                            gr_PresetPerformanceBest.add("Best", bestLearningResultForThisRow.structural);

                            // For a given weight, weightToPresetToValues maps preset to outcome.
                            for(Map.Entry<Double,Map<Integer, MarkovExperiment.LearningReport>> weightToPresetToValues:learningResultForThisRowAndAllWeightsAndPresets.entrySet()){
                                RBoxPlot<String> gr_PresetForWeight = structuralForWeightAndPreset.computeIfAbsent(weightToPresetToValues.getKey(), w-> {
                                    RBoxPlot<String> gr_PresetPerformance = new RBoxPlot<>("Number of traces and learner", "Structural Score, EM",
                                            new File(learningGroup.outPathPrefix + File.separator + description+"_"+states + "_centre-learner_weight="+weightToPresetToValues.getKey()+"_structural.pdf"));
                                    gr_PresetPerformance.setupForTwoLineXLabels();
                                    return gr_PresetPerformance;
                                });
                                gr_PresetForWeight.add("EM\n"+sb, weightToPresetToValues.getValue().get(0).structural);// EM
                                gr_PresetForWeight.add("EMC\n"+sb, weightToPresetToValues.getValue().get(1).structural);// M_BOTH, defined in MarkovParameters
                                if (hv_score!= null)
                                    gr_PresetForWeight.add("HV\n"+sb, hv_score);

                                RBoxPlot<String> gr_PresetPerformanceForWeight = weightToResults.computeIfAbsent(weightToPresetToValues.getKey(), w-> {
                                            RBoxPlot<String> gr_graph = new RBoxPlot<>("Number of traces and learner (weight =" + weightToPresetToValues.getKey() + " )", "Structural Score, EM",
                                                    new File(learningGroup.outPathPrefix + File.separator + description + "_" + states + "_centre-learner_tracenum=" + traceQuantityToUse + "_tracelen="+traceLength+"_weight=" + weightToPresetToValues.getKey() + "_structural.pdf"));
                                            gr_graph.setupForTwoLineXLabels();
                                            return gr_graph;
                                        });
                                for(Map.Entry<Integer, MarkovExperiment.LearningReport> presetToValues:weightToPresetToValues.getValue().entrySet())
                                    gr_PresetPerformanceForWeight.add(presetDescription[presetToValues.getKey()], presetToValues.getValue().structural);
                            }
                        }
                    }
                    gr_PresetPerformanceBest.reportResults(learningGroup.gr);
                    gr_PresetRuntimeBest.reportResults(learningGroup.gr);
                    gr_PresetRuntimeBestCapped.reportResults(learningGroup.gr);

                    gr_PresetTimeCappedPerformanceBest.reportResults(learningGroup.gr);
                    gr_PresetTimeCappedRuntimeBest.reportResults(learningGroup.gr);
                    for(Map.Entry<Double,RBoxPlot<String>> entry:weightToResults.entrySet())
                        entry.getValue().reportResults(learningGroup.gr);
                }
                List<String> labelValuesForComparativeAnalysis = new LinkedList<>();
                for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{
                        learningGroup.getTracesLengthmultBaseline(states),
                        new Pair(1, MarkovExperiment.LearningExperimentGroupParameters.datasetSize*learningGroup.getScalingFactor(states))}) {

                    int traceQuantityToUse = traces_lengthmult.firstElem;
                    int traceLength =  traces_lengthmult.secondElem * states;
                    StringBuilder sb = new StringBuilder();
                    Formatter formatter = new Formatter(sb, Locale.US);
                    formatter.format(numberFormat+","+numberFormat, traceQuantityToUse,traceLength);
                    labelValuesForComparativeAnalysis.add("EM\n"+sb);
                    labelValuesForComparativeAnalysis.add("EMC\n"+sb);
                    labelValuesForComparativeAnalysis.add("HV\n"+sb);
                }
                gr_BestStructuralForDifferentPreset.setOrderingOfLabels(labelValuesForComparativeAnalysis);
                gr_BestStructuralForDifferentPreset.reportResults(learningGroup.gr);
            }
        }
//        resultCSV.moveFiles();
    }
}
