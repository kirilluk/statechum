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
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.RESULT_VALUES.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersColumnFromCSV;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovBaselineLearn {
    public static final String description = "baselineP";

    public static class MarkovLearningBaselineParameters extends MarkovLearningParameters {

        public MarkovLearningBaselineParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample);
        }

        @Override
        public String getSubExperimentName() {
            return description;
        }
    }

    public static DrawGraphs.CSVExperimentResult runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        final DrawGraphs.CSVExperimentResult resultCSV = new DrawGraphs.CSVExperimentResult(new File(learningGroup.outPathPrefix + description+"-results.csv"), "results.csv");
        boolean aveOrMax = true;// average divide by the divisor
        boolean penaliseMissingPaths = true;

        int alphabetMultiplier = 2;
        boolean pathsOrSets = true;

        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample) {
                    for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{learningGroup.getTracesLengthmultBaseline(states)})
                    {
                        int traceQuantityToUse = traces_lengthmult.firstElem;
                        for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample)
                            for (LearningAlgorithms.ScoringToApply learnerKind :
                                    new LearningAlgorithms.ScoringToApply[]{
                                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV,
                                            LearningAlgorithms.ScoringToApply.SCORING_EDSM_1, LearningAlgorithms.ScoringToApply.SCORING_EDSM_2, LearningAlgorithms.ScoringToApply.SCORING_EDSM_4,
                                            LearningAlgorithms.ScoringToApply.SCORING_PTAK_1, LearningAlgorithms.ScoringToApply.SCORING_PTAK_2,
                                            LearningAlgorithms.ScoringToApply.SCORING_VH
                                    })
                            {
                                int chunkSizeToEvaluate = 3;
//                                double weightOfInconsistencies = 1.0;// good for 10 states
                                double weightOfInconsistencies = 0.5;// good for 20 states
                                ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                ev.config = learningGroup.eval.config.copy();
                                ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                MarkovLearningBaselineParameters parameters = new MarkovLearningBaselineParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
                                parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
                                parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, alphabetMultiplier);
                                parameters.markovParameters.setMarkovParameters(0, chunkSizeToEvaluate, pathsOrSets,
                                        new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies, 0), penaliseMissingPaths, aveOrMax, 0, 0, 0);
                                parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                parameters.disableReportMergeStatisticsWhenSolutionIsKnown();
                                MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovExperiment.MarkovLearnerRunner(parameters, ev);
                                learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                learningGroup.experimentRunner.submitTask(learnerRunner);
                            }
                    }
                }
        }

        learningGroup.experimentRunner.collectOutcomeOfExperiments(constructResultsCollector(resultCSV));

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
            Set<RESULT_VALUES> validityOfCells = obtainValidityOfCellValues(resultCSV);
            for (int states : learningGroup.statesToUse)
                for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                    String presetStr = "";
                    String experimentName = learningGroup.outPathPrefix + description+"_"+states+"_"+perStateSquaredDensity100+"_";
                    DataSelection source = new DataSelection(resultCSV,states,perStateSquaredDensity100,validityOfCells);

                    final DrawGraphs.RBagPlot gr_StructuralVsInconsistency = new DrawGraphs.RBagPlot("Inconsistency Learnt", "Structural Score", new File(experimentName + "inconsistency_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_BCR_vs_structural = new DrawGraphs.RBagPlot("BCR", "Structural Score", new File(experimentName + "bcr_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_TotalMergersVsStructuralScore = new DrawGraphs.RBagPlot("Total mergers", "Structural Score", new File(experimentName + "totalmergers_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_MistakesNearRootVsStructuralScore = new DrawGraphs.RBagPlot("Mistakes near root", "Structural Score", new File(experimentName  + "mistakes_nearroot_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_InvalidMergersNearRootVsStructuralScore = new DrawGraphs.RBagPlot("Invalid mergers near root", "Structural Score", new File(experimentName + "invalidmergers_nearroot_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_MissedMergersNearRootVsStructuralScore = new DrawGraphs.RBagPlot("Missed Mergers near root", "Structural Score", new File(experimentName + "missedmergers_nearroot_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_BCRVsInconsistency = new DrawGraphs.RBagPlot("Inconsistency Learnt", "BCR Score, EDSM-Markov", new File(experimentName + "inconsistency_bcr.pdf"));
                    final DrawGraphs.SquareBagPlot gr_StructuralDiff = new DrawGraphs.SquareBagPlot("Structural score, VH", "Structural Score, EDSM-Markov", new File(experimentName + "VH_structuraldiff.pdf"), 0, 1, true);
                    final DrawGraphs.SquareBagPlot gr_StructuralDiffLowDensity =
                        (perStateSquaredDensity100 == 0)?
                            new DrawGraphs.SquareBagPlot("Structural score, VH", "Structural Score, EDSM-Markov", new File(experimentName + "lowdensity_VH_structuraldiff.pdf"), 0, 1, true)
                            :null;
                    final DrawGraphs.RBagPlot gr_MarkovTransitionPrecisionStructuralDiff = new DrawGraphs.RBagPlot("Transition precision Markov", "Structural Score, EDSM-Markov", new File(experimentName + "markovtransitionprecision_structuraldiff.pdf"));
                    final DrawGraphs.RBagPlot gr_MarkovHoleRecallStructuralDiff = new DrawGraphs.RBagPlot("Hole recall Markov", "Structural Score, EDSM-Markov", new File(experimentName + "markovholerecall_structuraldiff.pdf"));
                    final DrawGraphs.RBagPlot gr_Inconsistencies_and_SD = new DrawGraphs.RBagPlot("Inconsistency, average", "Inconsistency, SD", new File(experimentName + "_inconsistencies_sd.pdf"));
                    final DrawGraphs.RBoxPlot<String> gr_PosnegNegativeInconsistencies_Structural = new DrawGraphs.RBoxPlot<>("Inconsistency always positive", "Structural difference", new File(experimentName + "posneginconsistencies_structuraldiff.pdf"));
//                    gr_PosnegNegativeInconsistencies_Structural.setupForOneLineXLabels();
//                    gr_PosnegNegativeInconsistencies_Structural.configureTextLabels(0.62,1,0.5);// xoffset is the vertical placement of x label. We have values from around 0.5 to 1.0 hence use a value smaller than the lowest value.
                    gr_PosnegNegativeInconsistencies_Structural.setXLine(2.5);
                    if (states > 20) {
                        gr_PosnegNegativeInconsistencies_Structural.setYLine(2.8);
                        gr_PosnegNegativeInconsistencies_Structural.setMargins(3.5,3.8,0.2,0.2);
                    }
                    else {
                        gr_PosnegNegativeInconsistencies_Structural.setYLine(2.5);
                        gr_PosnegNegativeInconsistencies_Structural.setMargins(3.5, 3.5, 0.2, 0.2);
                    }
                    final DrawGraphs.SquareBagPlot gr_BCR = new DrawGraphs.SquareBagPlot("BCR, VH", "BCR, EDSM-Markov", new File(experimentName + "_trace_bcr.pdf"), 0.5, 1, true);
                    final DrawGraphs.SquareBagPlot gr_DiffAgainstKtails1 = new DrawGraphs.SquareBagPlot("Structural Score, K-tails,1", "Structural Score, EDSM-Markov", new File(experimentName + "kt_1_markov.pdf"), 0, 1, true);
                    final DrawGraphs.SquareBagPlot gr_DiffAgainstKtails2 = new DrawGraphs.SquareBagPlot("Structural Score, K-tails,1", "Structural Score, EDSM-Markov", new File(experimentName + "kt_2_markov.pdf"), 0, 1, true);
                    final DrawGraphs.SquareBagPlot gr_DiffAgainstEDSM_1 = new DrawGraphs.SquareBagPlot("Structural Score, EDSM-1", "Structural Score, EDSM-Markov", new File(experimentName + "edsm-1_markov.pdf"), 0, 1, true);
                    final DrawGraphs.SquareBagPlot gr_DiffAgainstEDSM_2 = new DrawGraphs.SquareBagPlot("Structural Score, EDSM-2", "Structural Score, EDSM-Markov", new File(experimentName + "edsm-2_markov.pdf"), 0, 1, true);
//                    final DrawGraphs.SquareBagPlot BCRAgainstKtails1 = new DrawGraphs.SquareBagPlot("BCR, K-tails,1", "BCR, EDSM-Markov", new File(experimentName + "kt_1_markov_bcr.pdf"), 0.5, 1, true);
//                    final DrawGraphs.SquareBagPlot BCRAgainstKtails2 = new DrawGraphs.SquareBagPlot("BCR, K-tails,1", "BCR, EDSM-Markov", new File(experimentName + "kt_2_markov_bcr.pdf"), 0.5, 1, true);
//                    final DrawGraphs.SquareBagPlot BCRAgainstEDSM_1 = new DrawGraphs.SquareBagPlot("BCR, EDSM-1", "BCR, EDSM-Markov", new File(experimentName + "edsm-1_markov_bcr.pdf"), 0.5, 1, true);
//                    final DrawGraphs.SquareBagPlot BCRAgainstEDSM_2 = new DrawGraphs.SquareBagPlot("BCR, EDSM-2", "BCR, EDSM-Markov", new File(experimentName + "edsm-2_markov_bcr.pdf"), 0.5, 1, true);

                    final DrawGraphs.WilcoxonPairedTest Wilcoxon_test_Structural = new DrawGraphs.WilcoxonPairedTest(new File(experimentName + "Wilcoxon_t_str.csv"));
                    final DrawGraphs.WilcoxonPairedTest Wilcoxon_Test_BCR = new DrawGraphs.WilcoxonPairedTest(new File(experimentName + "Wilcoxon_t_bcr.csv"));
                    final DrawGraphs.Mann_Whitney_U_Test Mann_Whitney_U_Test_BCR = new DrawGraphs.Mann_Whitney_U_Test(new File(experimentName + "Mann_Whitney_U_Test_BCR.csv"));
                    final DrawGraphs.Mann_Whitney_U_Test Mann_Whitney_U_Test_Structural = new DrawGraphs.Mann_Whitney_U_Test(new File(experimentName + "Whitney_U_Test_str.csv"));
                    final DrawGraphs.Kruskal_Wallis Kruskal_Wallis_Test_BCR = new DrawGraphs.Kruskal_Wallis(new File(experimentName + "Kruskal_Wallis_Test_BCR.csv"));
                    final DrawGraphs.Kruskal_Wallis Kruskal_Wallis_Test_Structural = new DrawGraphs.Kruskal_Wallis(new File(experimentName + "Kruskal_Wallis_Test_str.csv"));
                    // names of columns include parameters used with learners, here we ignore that and pick those that match learner names
                    spreadsheetToBagPlotNoZeroYValues(gr_StructuralVsInconsistency, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_INCONSISTENCY_LEARNT,
                            new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, null, null);
                    spreadsheetToBagPlotNoZeroYValues(gr_BCR_vs_structural, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_BCR,
                            new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, null, null);
                    spreadsheetToBagPlotNoZeroYValues(gr_BCRVsInconsistency, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_INCONSISTENCY_LEARNT,
                            new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_BCR, null, null);
                    spreadsheetToBagPlot(gr_StructuralDiff, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_VH), E_DIFF,
                            new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, null, null);
                    spreadsheetToBagPlot(gr_MarkovTransitionPrecisionStructuralDiff, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_MARKOV_TRANSITION_PRECISION,
                            new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, null, null);
                    spreadsheetToBagPlot(gr_MarkovHoleRecallStructuralDiff, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_MARKOV_HOLE_RECALL,
                            new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, null, null);

                    spreadsheetToBagPlot(gr_Inconsistencies_and_SD, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_INCONSISTENCY_AVERAGE,
                            new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_INCONSISTENCY_SD, null, null);

                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                        MarkovLearningParameters rowValues = parseMarkovParametersRowFromCSV(rowEntry.getKey());

                        if (rowValues.perStateSquaredDensityMultipliedBy100 == perStateSquaredDensity100 && rowValues.states == states)
                            getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), validityOfCells, (column, columnText, Y) -> {
                                boolean alwaysPositive = obtainBooleanValueFromCell(Y, E_INCONSISTENCY_ALWAYSPOSITIVE,column);
                                double value = obtainDoubleValueFromCell(Y, E_DIFF,column);
//                                gr_StructuralVsInconsistency.add(Double.parseDouble(obtainValueFromCell(X, cellWithinX)), value, colour, label);

                                ColumnAndValue Y_VH = getValueFromMapGivenSelector(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_VH),validityOfCells);
                                if (gr_StructuralDiffLowDensity != null)
                                    gr_StructuralDiffLowDensity.add(
                                            obtainDoubleValueFromCell(Y_VH.value, E_DIFF,Y_VH.column),
                                            obtainDoubleValueFromCell(Y, E_DIFF,column), null, null);
                                gr_PosnegNegativeInconsistencies_Structural.add(Boolean.toString(alwaysPositive), value, null, null);
                                gr_TotalMergersVsStructuralScore.add(
                                        obtainDoubleValueFromCell(Y, E_ERR_INVALID_NEARROOT,column) + obtainDoubleValueFromCell(Y, E_ERR_INVALID_FARFROMROOT,column) + obtainDoubleValueFromCell(Y, E_VALIDMERGERS,column),
                                        obtainDoubleValueFromCell(Y, E_DIFF,column), null, null);
                                gr_MistakesNearRootVsStructuralScore.add(
                                        obtainDoubleValueFromCell(Y, E_ERR_INVALID_NEARROOT,column) + obtainDoubleValueFromCell(Y, E_ERR_MISSED_NEARROOT,column),
                                        obtainDoubleValueFromCell(Y, E_DIFF,column), null, null);
                                gr_InvalidMergersNearRootVsStructuralScore.add(
                                        obtainDoubleValueFromCell(Y, E_ERR_INVALID_NEARROOT,column),
                                        obtainDoubleValueFromCell(Y, E_DIFF,column), null, null);
                                gr_MissedMergersNearRootVsStructuralScore.add(
                                        obtainDoubleValueFromCell(Y, E_ERR_MISSED_NEARROOT,column),
                                        obtainDoubleValueFromCell(Y, E_DIFF,column), null, null);
                        });
                    }

                    spreadsheetToBagPlot(gr_BCR, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_VH), E_BCR, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_BCR, null, null);
                    spreadsheetToBagPlot(gr_DiffAgainstKtails1, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_PTAK_1), E_DIFF, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, null, null);
                    spreadsheetToBagPlot(gr_DiffAgainstKtails2, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_PTAK_2), E_DIFF, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, null, null);
                    spreadsheetToBagPlot(gr_DiffAgainstEDSM_1, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_EDSM_1), E_DIFF, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, null, null);
                    spreadsheetToBagPlot(gr_DiffAgainstEDSM_2, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_EDSM_2), E_DIFF, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, null, null);

                    spreadsheetAsDouble(Wilcoxon_Test_BCR, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_BCR, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_VH), E_BCR);
                    spreadsheetAsDouble(Wilcoxon_test_Structural, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_VH), E_DIFF);
                    spreadsheetAsDouble(Mann_Whitney_U_Test_BCR, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_BCR, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_VH), E_BCR);
                    spreadsheetAsDouble(Mann_Whitney_U_Test_Structural, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_VH), E_DIFF);
                    spreadsheetAsDouble(Kruskal_Wallis_Test_BCR, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_BCR, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_VH), E_BCR);
                    spreadsheetAsDouble(Kruskal_Wallis_Test_Structural, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_VH), E_DIFF);

                    for (@SuppressWarnings("rawtypes") DrawGraphs.RExperimentResult result : new DrawGraphs.RExperimentResult[]{gr_StructuralVsInconsistency, gr_BCRVsInconsistency,
                            gr_MarkovTransitionPrecisionStructuralDiff, gr_MarkovHoleRecallStructuralDiff, gr_StructuralDiff, gr_BCR_vs_structural,
                            gr_Inconsistencies_and_SD, gr_PosnegNegativeInconsistencies_Structural, gr_TotalMergersVsStructuralScore,
                            gr_MistakesNearRootVsStructuralScore, gr_MissedMergersNearRootVsStructuralScore, gr_InvalidMergersNearRootVsStructuralScore,
                            gr_BCR, gr_DiffAgainstKtails1, gr_DiffAgainstKtails2, gr_DiffAgainstEDSM_1, gr_DiffAgainstEDSM_2,
                            Wilcoxon_Test_BCR, Wilcoxon_test_Structural, Mann_Whitney_U_Test_BCR, Mann_Whitney_U_Test_Structural, Kruskal_Wallis_Test_Structural, Kruskal_Wallis_Test_BCR}) {
                        result.reportResults(learningGroup.gr);
                    }
                    if (gr_StructuralDiffLowDensity != null)
                        gr_StructuralDiffLowDensity.reportResults(learningGroup.gr);
                }
        }

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            Set<RESULT_VALUES> validityOfCells = obtainValidityOfCellValues(resultCSV);
            for (int states : learningGroup.statesToUse)
                for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                    FilterCollectionOfResultsForBestPerformingLearner report = new FilterCollectionOfResultsForBestPerformingLearner(states,perStateSquaredDensity100,resultCSV,validityOfCells);
                    final DrawGraphs.SquareBagPlot gr_StructuralDiffBest = new DrawGraphs.SquareBagPlot("Structural score, VH", "Structural Score, EDSM-Markov",
                            new File(learningGroup.outPathPrefix + description+"_" + states + "_" + perStateSquaredDensity100 + "_VH_structuraldiffBest.pdf"), 0, 1, true);
                    final RBoxPlot<String> gr_PerformanceOfLearners = new RBoxPlot<>("", "Structural Score",
                            new File(learningGroup.outPathPrefix + description+"_" + states + "_" + perStateSquaredDensity100 + "_baseline_learner_structural.pdf"));
                    final RBoxPlot<String> gr_RuntimeOfLearners = new RBoxPlot<>("", "Runtime, seconds",
                            new File(learningGroup.outPathPrefix + description+"_" + states + "_" + perStateSquaredDensity100 + "_baseline_learner_runtime.pdf"));
                    gr_PerformanceOfLearners.setOtherOptions("las=2");
                    gr_RuntimeOfLearners.setupForTwoLineXLabels();
                    gr_RuntimeOfLearners.configureTextLabels(-3.5,1,0.5);
                    gr_RuntimeOfLearners.setMargins(3,3,0.2,0.2);
                    report.getResultForBestPerformingMarkovLearner(gr_StructuralDiffBest, null, null, null);
//                gr_PerformanceOfLearners.add("MARKOV",bestLearningResult.structural, null, null);
                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                        MarkovLearningParameters rowValues = parseMarkovParametersRowFromCSV(rowEntry.getKey());
                        if (rowValues.perStateSquaredDensityMultipliedBy100 == perStateSquaredDensity100 && rowValues.states == states) {
                            for (Map.Entry<String, String> entry : rowEntry.getValue().entrySet()) {
                                MarkovLearningParameters.ColumnParseOutcome column=parseMarkovParametersColumnFromCSV(entry.getKey(),validityOfCells);

                                boolean learntOK = obtainStringValueFromCell(entry.getValue(), E_SUCCESS,column).equals(LEARNING_OK.name);
//                                        int cellForRuntime = entry.getValue().split(",").length - 1;
                                gr_PerformanceOfLearners.add(column.learner.reportedName, obtainDoubleValueFromCell(entry.getValue(), E_DIFF,column), null, null);
//                                        gr_RuntimeOfLearners.add(column.learner.reportedName + (learntOK ? "-OK" : "Err"), obtainDoubleValueFromCell(entry.getValue(), E_RUNTIME,column), learntOK ? null : "red", null);
                                gr_RuntimeOfLearners.add(column.learner.reportedName.replace('@','\n'), obtainDoubleValueFromCell(entry.getValue(), E_RUNTIME,column),null, null);
                            }
                        }
                    }
                    gr_StructuralDiffBest.reportResults(learningGroup.gr);
                    gr_PerformanceOfLearners.reportResults(learningGroup.gr);
                    gr_RuntimeOfLearners.reportResults(learningGroup.gr);
//                    report.reportResults();
                }
        }
        return resultCSV;
    }

}

