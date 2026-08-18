package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Configuration;
import statechum.Pair;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.RESULT_VALUES.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.getAllValuesFromMapGivenRegexp;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.obtainDoubleValueFromCell;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.obtainStringValueFromCell;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_TIMEOUT;

public class E_MarkovFanTempMonitor600 extends E_MarkovCaseStudies {

    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        int[] learnerExperiment = new int[]{0, 1};
        final DrawGraphs.CSVExperimentResult resultCSV = new DrawGraphs.CSVExperimentResult(new File(learningGroup.outPathPrefix + "casestudy_fantempmonitor600-results.csv"), "results.csv");
        boolean aveOrMax = true;// average divide by the divisor
        boolean pathsOrSets = true, penaliseMissingPaths = true;
//        String pathToCaseStudyFiles = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_CASESTUDIES);
//        if (null == pathToCaseStudyFiles ||  pathToCaseStudyFiles.isEmpty())
//            throw new RuntimeException("Cannot load any case studies: path to case studies is not defined");
//        if (!Files.exists(Paths.get(pathToCaseStudyFiles)))
//            throw new RuntimeException("Cannot load any case studies: path to case studies does not exist "+pathToCaseStudyFiles);

        long timeout = 1800000L * 9L;// // for case studies, set timeout to 4.5 hours - the one that runs that long is centre-based computations for FanTempMonitor with 676 traces that do not produce brilliant results anyway (comparable to learning without centre since the PTA is dense enough for normal learning).

        E_MarkovCaseStudies.fillInCaseStudyExperimentParameters(learningGroup);

        final int casestudy = 5;// TempFanMonitor_A
        for (final int preset : learnerExperiment)
            for (final Pair<Integer, Integer> traces_lengthmult : caseStudyInformationMap.get(casestudy).traces_and_lengths) {
                int states = caseStudyInformationMap.get(casestudy).referenceGraph.getStateNumber();
                int traceQuantityToUse = traces_lengthmult.firstElem;
                for (int trainingSample = 0; trainingSample < caseStudyInformationMap.get(casestudy).trainingSamplesPerFSM; ++trainingSample)
                    for (LearningAlgorithms.ScoringToApply learnerKind :
                            preset == 0 ?// this is the only case where we can apply PTA-based merging algorithms, two other presets handle merging vertices in a connected graph
                                    new LearningAlgorithms.ScoringToApply[]{
                                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV,
                                            LearningAlgorithms.ScoringToApply.SCORING_EDSM_1, LearningAlgorithms.ScoringToApply.SCORING_EDSM_2, LearningAlgorithms.ScoringToApply.SCORING_EDSM_4,
                                            LearningAlgorithms.ScoringToApply.SCORING_PTAK_1, LearningAlgorithms.ScoringToApply.SCORING_PTAK_2,
                                            LearningAlgorithms.ScoringToApply.SCORING_VH
                                    } :
                                    new LearningAlgorithms.ScoringToApply[]{
                                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV
                                    })
                        for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? caseStudyInformationMap.get(casestudy).chunkSizesToEvaluate : new int[]{2})
                            for (double weightOfInconsistencies : learnerKind.isMarkov() ?
                                    caseStudyInformationMap.get(casestudy).chunkLenToWeights.get(chunkSizeToEvaluate)
                                    //new double[]{0.25,0.5,1.0, 2.0, 3.0, 4.0, 6.0, 8.0}
                                    : new double[]{1.0})
                                for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 1)} :
                                        new Pair[]{new Pair(1, 2), new Pair(1, 4), new Pair(2, 4), new Pair(2, 8)}) {
                                    ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                    ev.config = learningGroup.eval.config.copy();
                                    ev.config.setOverride_maximalNumberOfStates(states * 2);//LearningAlgorithms.maxStateNumberMultiplier);
                                    if (learnerKind.isMarkov())
                                        ev.config.setLearnerScoreMode(Configuration.ScoreMode.ONLYOVERRIDE);
                                    // For some case studies (FanTempController_T) there is a large amount of data - need Array-based data structures
                                    ev.config.setTransitionMatrixImplType(caseStudyInformationMap.get(casestudy).transitionMatrixImplType);
                                    ev.config.setTimeOut(timeout);
                                    MarkovLearningBaselineParameters parameters = new MarkovLearningBaselineParameters(learnerKind, states, 0, 0, casestudy, trainingSample);
                                    parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
                                    parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, 0);
                                    parameters.markovParameters.setMarkovParameters(preset, chunkSizeToEvaluate, pathsOrSets,
                                            new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies, 0), penaliseMissingPaths, aveOrMax, wlen_divisor.secondElem, 0, wlen_divisor.firstElem);
                                    parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                    parameters.disableReportMergeStatisticsWhenSolutionIsKnown();
//                                            parameters.setWalkType(RandomPathGenerator.WALKTYPE.WALKTYPE_AIMFORTRANSITIONCOVER_PREFERNONLOOP,0.6, 10);
                                    MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovLearnerRunnerForCaseStudies(parameters, ev);
                                    learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                    learningGroup.experimentRunner.submitTask(learnerRunner);
                                }
            }

        learningGroup.experimentRunner.collectOutcomeOfExperiments(constructResultsCollector(resultCSV));

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            Set<MarkovExperiment.RESULT_VALUES> validityOfCells = obtainValidityOfCellValues(resultCSV);
            List<List<String>> outputStatistics = new ArrayList<>();
            outputStatistics.add(new ArrayList<>(Arrays.asList("Case study", "States", "Alphabet", "Traces", "T. Length", "Centre", "P.Len", "Diff, M", "BCR, M", "Diff, VH", "BCR, VH", "A12", "A12 lo", "A12 hi", "Wilcoxon")));
            for (Map.Entry<Integer, CaseStudyInformation> entryForCaseStudy : caseStudyInformationMap.entrySet()) {

                // We need to compute the smallest runtime that was deemed to be a timeout. It is subsequently used as a cap
                // on the timeout values because in different experiments different timeouts were used (and time was also measured less precisely
                // in that some timeouts were only detected long past their timeout values).
                AtomicInteger timeoutValueObtained = new AtomicInteger(Integer.MAX_VALUE);
                for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                    MarkovLearningParameters rowHeader = parseMarkovParametersRowFromCSV(rowEntry.getKey());
                    if (rowHeader.sample == entryForCaseStudy.getKey()) {
                        getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), validityOfCells,
                                (column, columnText, Y) -> {
                                    boolean learntTimeout = obtainStringValueFromCell(Y, RESULT_VALUES.E_SUCCESS, column).equals(LEARNING_TIMEOUT.name);
                                    if (learntTimeout) {
                                        int runtime = (int) Math.round(obtainDoubleValueFromCell(Y, E_RUNTIME, column));
                                        timeoutValueObtained.accumulateAndGet(runtime, (a, b) -> Math.min(a, b));
                                    }
                                });
                    }
                }

                final DrawGraphs.RBoxPlot<String> gr_AveForLargeNumberOfTraces = new DrawGraphs.RBoxPlot<String>("Diff, without centre", "Diff, using centre",
                        new File(learningGroup.outPathPrefix + "casestudies_" + entryForCaseStudy.getValue().name + "_structure_600_with_and_without_centre.pdf"));
                gr_AveForLargeNumberOfTraces.setupForTwoLineXLabels();

                for (final int chunkSizeToEvaluate : entryForCaseStudy.getValue().chunkSizesToEvaluate) {
                    Pair<Integer, Integer>[] traces_and_lengths = entryForCaseStudy.getValue().traces_and_lengths;

                    final DrawGraphs.RBagPlot gr_RuntimeVsComparisonsSlow = new DrawGraphs.RBagPlot("Comparisons, log10", "Runtime, log10",
                            new File(learningGroup.outPathPrefix + "casestudies_" + entryForCaseStudy.getValue().name + "_chunklen=" + chunkSizeToEvaluate + ",runtime_vs_comparisons_SLOW.pdf"));
                    final DrawGraphs.RBagPlot gr_RuntimeVsComparisonsBefore600 = new DrawGraphs.RBagPlot("Comparisons, log10", "Runtime, log10",
                            new File(learningGroup.outPathPrefix + "casestudies_" + entryForCaseStudy.getValue().name + "_chunklen=" + chunkSizeToEvaluate + ",runtime_vs_comparisons_Before600.pdf"));
                    gr_RuntimeVsComparisonsSlow.setLabelsAuto(DrawGraphs.RGraph.PLOT_X_LABELS.XLABELS_R);
                    gr_RuntimeVsComparisonsSlow.setMargins(3, 4, 0.2, 0.2);
                    gr_RuntimeVsComparisonsSlow.setYLine(4);
                    gr_RuntimeVsComparisonsBefore600.setLabelsAuto(DrawGraphs.RGraph.PLOT_X_LABELS.XLABELS_R);


                    for (final boolean useCentre : new boolean[]{false, true})
                        for (final Pair<Integer, Integer> traces_lengthmult : traces_and_lengths)
                            if (traces_lengthmult.firstElem > 600) {

                                // Now select the non-Markov result from all those available
                                for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                                    MarkovLearningParameters rowHeader = parseMarkovParametersRowFromCSV(rowEntry.getKey());
                                    if (rowHeader.traceQuantity == traces_lengthmult.firstElem && rowHeader.sample == entryForCaseStudy.getKey()) {
                                        // Evaluate runtime of length Markov learning
                                        getAllValuesFromMapGivenRegexp(rowEntry.getValue(),
                                                column ->
                                                        (column.parameters.preset > 0) == useCentre &&
                                                                column.parameters.chunkLen == chunkSizeToEvaluate &&
                                                                column.learner == LearningAlgorithms.ScoringToApply.SCORING_MARKOV,
                                                validityOfCells,
                                                (column, columnText, Y) -> {
                                                    double runtime = capToTimeout(obtainDoubleValueFromCell(Y, E_RUNTIME, column), timeoutValueObtained);// cap runtime to timeout, esp since earlier experimental runs could run longer than 4.5 hours (esp because they were not as frequently checking for a timeout).
                                                    boolean learntOK = obtainStringValueFromCell(Y, RESULT_VALUES.E_SUCCESS, column).equals(LEARNING_OK.name);
                                                    ResultsXAxis xValue = new ResultsXAxis(column.learner, rowHeader.traceQuantity, chunkSizeToEvaluate, useCentre);
                                                    if (xValue.filter(entryForCaseStudy.getValue().name)) {
                                                        if (runtime >= 1.0)
                                                            runtime = Math.log10(runtime);

                                                        double comparisons = obtainDoubleValueFromCell(Y, E_MARKOV_COMPARISONSPERFORMED, column);
                                                        if (comparisons > 1.0)
                                                            comparisons = Math.log10(comparisons);
                                                        if (rowHeader.traceQuantity < 600 || !useCentre)
                                                            gr_RuntimeVsComparisonsBefore600.add(comparisons, runtime);
                                                        else
                                                            gr_RuntimeVsComparisonsSlow.add(comparisons, runtime);
                                                    }

                                                    if (learntOK)
                                                        gr_AveForLargeNumberOfTraces.add(xValue.toString(), obtainDoubleValueFromCell(Y, E_DIFF, column));

                                                });
                                    }
                                }
                            }
                    gr_RuntimeVsComparisonsSlow.reportResults(learningGroup.gr);
                    gr_RuntimeVsComparisonsBefore600.reportResults(learningGroup.gr);
                }
                gr_AveForLargeNumberOfTraces.reportResults(learningGroup.gr);
            }
        }
    }
}
