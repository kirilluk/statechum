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

public class E_MarkovFanTempMonitor676 extends E_MarkovCaseStudies {

    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        int[] learnerExperiment = new int[]{0, 1};
        final DrawGraphs.CSVExperimentResult resultCSV = new DrawGraphs.CSVExperimentResult(new File(learningGroup.outPathPrefix + File.separator + "casestudy_fantempmonitor600-results.csv"), "results.csv");
        resultCSV.setCheckCellHeaderCompatibility((oldParameters, newParameters) ->{
            if (oldParameters.equals(newParameters))
                return true;

            if (oldParameters.size() == 30 && newParameters.size() == oldParameters.size()) {
                final int position = 20;
                if (
                        (oldParameters.get(position).equals("relIncLrnt") && newParameters.get(position).equals("I_LntC")) ||
                        (newParameters.get(position).equals("relIncLrnt") && oldParameters.get(position).equals("I_LntC"))) // handle re-purposing field relIncLrnt as I_LntC
                    return true;
            }
            return false;
            }
        );
        boolean aveOrMax = true;// average divide by the divisor
        boolean pathsOrSets = true, penaliseMissingPaths = true;
//        String pathToCaseStudyFiles = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_CASESTUDIES);
//        if (null == pathToCaseStudyFiles ||  pathToCaseStudyFiles.isEmpty())
//            throw new RuntimeException("Cannot load any case studies: path to case studies is not defined");
//        if (!Files.exists(Paths.get(pathToCaseStudyFiles)))
//            throw new RuntimeException("Cannot load any case studies: path to case studies does not exist "+pathToCaseStudyFiles);

        long timeout = 1800000L * 2L;// // for case studies, set timeout to 1 hour - the one that runs that long is centre-based computations for FanTempMonitor with 676 traces that do not produce brilliant results anyway (comparable to learning without centre since the PTA is dense enough for normal learning).

        E_MarkovCaseStudies.fillInCaseStudyExperimentParameters(learningGroup);

        final int casestudy = 5;// TempFanMonitor_A
        assert caseStudyInformationMap.get(casestudy).name.equals(caseStudyFanTempMonitor) : "Got case study "+caseStudyInformationMap.get(casestudy).name+" , expected : "+caseStudyFanTempMonitor;
        Pair<Integer, Integer>[] traces_and_lengths_for_TempFanMonitor_A = caseStudyInformationMap.get(casestudy).traces_and_lengths;
        Pair<Integer, Integer> traces_lengthmult = traces_and_lengths_for_TempFanMonitor_A[traces_and_lengths_for_TempFanMonitor_A.length-1];
        assert traces_lengthmult.firstElem == 676 : "invalid number of traces : expected 676, got " + traces_lengthmult.firstElem;

        for (final int preset : learnerExperiment) {
                int states = caseStudyInformationMap.get(casestudy).referenceGraph.getStateNumber();
                int traceQuantityToUse = traces_lengthmult.firstElem;
                for (int trainingSample = 0; trainingSample < caseStudyInformationMap.get(casestudy).trainingSamplesPerFSM; ++trainingSample)
                    for (LearningAlgorithms.ScoringToApply learnerKind :
                            preset == 0 ?// this is the only case where we can apply PTA-based merging algorithms, two other presets handle merging vertices in a connected graph
                                    new LearningAlgorithms.ScoringToApply[]{
                                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV,
                                            LearningAlgorithms.ScoringToApply.SCORING_EDSM_1, LearningAlgorithms.ScoringToApply.SCORING_EDSM_2, LearningAlgorithms.ScoringToApply.SCORING_EDSM_4,
                                            LearningAlgorithms.ScoringToApply.SCORING_PTAK_1, LearningAlgorithms.ScoringToApply.SCORING_PTAK_2,
                                            LearningAlgorithms.ScoringToApply.SCORING_HV
                                    } :
                                    new LearningAlgorithms.ScoringToApply[]{
                                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV
                                    })
                        for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? caseStudyInformationMap.get(casestudy).chunkSizesToEvaluate : new int[]{2})
                            for (double weightOfInconsistencies : learnerKind.isMarkov() ?
                                    caseStudyInformationMap.get(casestudy).chunkLenToWeights.get(chunkSizeToEvaluate)
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
                                    MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovLearnerRunnerForCaseStudies(learningGroup.outPathPrefix, parameters, ev);
                                    learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                    learningGroup.experimentRunner.submitTask(learnerRunner);
                                }
            }

            learningGroup.experimentRunner.collectOutcomeOfExperiments(constructResultsCollector(resultCSV));

            if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
                Set<MarkovExperiment.RESULT_VALUES> validityOfCells = obtainValidityOfCellValues(description,resultCSV);checkFullTransitionCoverageAttained(description, resultCSV, validityOfCells);
                List<List<String>> outputStatistics = new ArrayList<>();
                outputStatistics.add(new ArrayList<>(Arrays.asList("Case study", "States", "Alphabet", "Traces", "T. Length", "Centre", "P.Len", "Diff, M", "BCR, M", "Diff, HV", "BCR, HV", "A12", "A12 lo", "A12 hi", "Wilcoxon")));
                for (Map.Entry<Integer, CaseStudyInformation> entryForCaseStudy : caseStudyInformationMap.entrySet())
                    if (entryForCaseStudy.getKey() == casestudy)
                    {

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
                                            if (learntTimeout && entryForCaseStudy.getValue().name.equals(caseStudyFanTempMonitor)) {
                                                int runtime = (int) Math.round(obtainDoubleValueFromCell(Y, E_RUNTIME, column));
                                                timeoutValueObtained.accumulateAndGet(runtime, (a, b) -> Math.min(a, b));
                                            }
                                        });
                            }
                        }

                        assert timeoutValueObtained.get() < 3800 : "Timeout for 676 traces is an hour, but we got minimal timeout of "+timeoutValueObtained.get();

                        final DrawGraphs.RBoxPlot<String> gr_AveForLargeNumberOfTraces = new DrawGraphs.RBoxPlot<String>("Learning strategy", "Structural difference",
                                new File(learningGroup.outPathPrefix + File.separator + "casestudies_" + entryForCaseStudy.getValue().name + "_676_structure.pdf"));
                        gr_AveForLargeNumberOfTraces.setupForTwoLineXLabels();

                        final DrawGraphs.RBoxPlot<String> gr_PerformanceOfLearners = new DrawGraphs.RBoxPlot<>("", "Structural Score",
                                new File(learningGroup.outPathPrefix + File.separator + description + "_" + entryForCaseStudy.getValue().name + "_676_learner_structural.pdf"));
                        gr_PerformanceOfLearners.setupForTwoLineXLabels();
                        gr_PerformanceOfLearners.setMargins(3, 3, 0.2, 0.2);

                        final DrawGraphs.RBoxPlot<String> gr_CentreCorrectPercentage = new DrawGraphs.RBoxPlot<>("", "%% success of centre identification",
                                new File(learningGroup.outPathPrefix + File.separator + description + "_" + entryForCaseStudy.getValue().name + "_676_learner_centrecorrect.pdf"));
                        gr_CentreCorrectPercentage.setupForTwoLineXLabels();
                        gr_CentreCorrectPercentage.setMargins(4, 3, 0.2, 0.2);

                        final DrawGraphs.RBoxPlot<String> gr_SuccessPercentage = new DrawGraphs.RBoxPlot<>("", "%% success",
                                new File(learningGroup.outPathPrefix + File.separator + description + "_" + entryForCaseStudy.getValue().name + "_676_learner_successpercentage.pdf"));
                        gr_SuccessPercentage.setupForTwoLineXLabels();
                        gr_SuccessPercentage.setMargins(3, 3, 0.2, 0.2);

                        for (final int chunkSizeToEvaluate : entryForCaseStudy.getValue().chunkSizesToEvaluate) {
                            final DrawGraphs.RBagPlot gr_RuntimeVsComparisons676Centre = new DrawGraphs.RBagPlot("Comparisons, log10", "Runtime, log10",
                                    new File(learningGroup.outPathPrefix + File.separator + "casestudies_" + entryForCaseStudy.getValue().name + "_676_chunklen=" + chunkSizeToEvaluate + ",runtime_vs_comparisons.pdf"));
                            gr_RuntimeVsComparisons676Centre.setLabelsAuto(DrawGraphs.RGraph.PLOT_X_LABELS.XLABELS_R);
                            gr_RuntimeVsComparisons676Centre.setMargins(3, 4, 0.2, 0.2);
                            gr_RuntimeVsComparisons676Centre.setYLine(4);

                            final boolean useCentre = true;
                            Map<ResultsXAxis, AtomicInteger> countsTotal = new HashMap<>();
                            Map<ResultsXAxis, AtomicInteger> countsSuccess = new HashMap<>();
                            Map<ResultsXAxis, AtomicInteger> centreCorrect = new HashMap<>();
                            final AtomicInteger diffReported = new AtomicInteger(0), bcrReported = new AtomicInteger(0);
                            // Now select the non-Markov result from all those available
                            for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                                MarkovLearningParameters rowHeader = parseMarkovParametersRowFromCSV(rowEntry.getKey());
                                if (rowHeader.traceQuantity == traces_lengthmult.firstElem && rowHeader.sample == entryForCaseStudy.getKey()) {
                                    // Evaluate runtime and success rate of Markov learning
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
                                                    double runtimeLog10 = runtime;
                                                    if (runtimeLog10 >= 1.0)
                                                        runtimeLog10 = Math.log10(runtimeLog10);

                                                    double comparisons = obtainDoubleValueFromCell(Y, E_MARKOV_COMPARISONSPERFORMED, column);
                                                    if (comparisons > 1.0)
                                                        comparisons = Math.log10(comparisons);
                                                    if (rowHeader.traceQuantity > 600 && useCentre)
                                                        gr_RuntimeVsComparisons676Centre.add(comparisons, runtimeLog10);

                                                    countsTotal.computeIfAbsent(xValue, k -> new AtomicInteger(0)).incrementAndGet();
                                                    if (learntOK && rowHeader.traceQuantity > 600 && useCentre) {
                                                        gr_AveForLargeNumberOfTraces.add(xValue.toString(), obtainDoubleValueFromCell(Y, E_DIFF, column));
                                                        countsSuccess.computeIfAbsent(xValue, k -> new AtomicInteger(0)).incrementAndGet();
                                                        boolean centreCorrectValue = obtainBooleanValueFromCell(Y, E_CENTRE_CORRECT, column);
                                                        if (centreCorrectValue)
                                                            centreCorrect.computeIfAbsent(xValue, k -> new AtomicInteger(0)).incrementAndGet();
                                                    }
                                                }
                                            });
                                }
                            }

                            for (Map.Entry<ResultsXAxis, AtomicInteger> entry : countsTotal.entrySet()) {
                                int value = countsSuccess.containsKey(entry.getKey()) ? countsSuccess.get(entry.getKey()).intValue() : 0;
                                gr_SuccessPercentage.add(entry.getKey().toString(), (double) Math.round(100. * (double) value / entry.getValue().intValue()));
                            }
                            for (Map.Entry<ResultsXAxis, AtomicInteger> entry : countsTotal.entrySet())
                            {
                                int value = centreCorrect.containsKey(entry.getKey())? centreCorrect.get(entry.getKey()).get():0;
                                gr_CentreCorrectPercentage.add(entry.getKey().toString(), (double) Math.round(100. * (double) value / entry.getValue().intValue()));
                            }
                            // Now evaluate results
                            FilterCollectionOfResultsForBestPerformingLearner report = new FilterCollectionOfResultsForBestPerformingLearner(-1, -1,
                                    rowHeader -> rowHeader.traceQuantity == traces_lengthmult.firstElem && rowHeader.sample == entryForCaseStudy.getKey(),
                                    columnParse -> (columnParse.parameters.preset > 0) == useCentre && columnParse.parameters.chunkLen == chunkSizeToEvaluate &&
                                            new ResultsXAxis(LearningAlgorithms.ScoringToApply.SCORING_MARKOV, traces_lengthmult.firstElem, chunkSizeToEvaluate, useCentre).filter(entryForCaseStudy.getValue().name),
                                    resultCSV, validityOfCells);

                            report.getResultForBestPerformingMarkovLearner(null, null,
                                    (pair) -> {
                                        ResultsXAxis xValue = new ResultsXAxis(LearningAlgorithms.ScoringToApply.SCORING_MARKOV, traces_lengthmult.firstElem, chunkSizeToEvaluate, useCentre);
                                        if (xValue.addToPlot(entryForCaseStudy.getValue().name))
                                            gr_PerformanceOfLearners.add(xValue.toString(), pair.firstElem, DrawGraphs.defaultColour, null);
                                    },
                                    null
                            );



                            gr_RuntimeVsComparisons676Centre.reportResults(learningGroup.gr);
                        }
                        gr_AveForLargeNumberOfTraces.reportResults(learningGroup.gr);
                        gr_PerformanceOfLearners.reportResults(learningGroup.gr);
                        gr_CentreCorrectPercentage.reportResults(learningGroup.gr);
                        gr_SuccessPercentage.reportResults(learningGroup.gr);
                    }
        }
    }
}
