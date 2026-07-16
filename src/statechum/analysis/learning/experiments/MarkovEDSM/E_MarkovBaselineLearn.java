package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Pair;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.DrawGraphs.obtainValueFromCell;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovBaselineLearn {

    public static class MarkovLearningBaselineParameters extends MarkovLearningParameters {

        public MarkovLearningBaselineParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample);
        }

        @Override
        public String getSubExperimentName() {
            return "baseline";
        }
    }

    public static DrawGraphs.CSVExperimentResult runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        final DrawGraphs.CSVExperimentResult resultCSV = new DrawGraphs.CSVExperimentResult(new File(learningGroup.outPathPrefix + "results.csv"));
        boolean aveOrMax = true;// average divide by the divisor
        boolean penaliseMissingPaths = true;

        int alphabetMultiplier = 2;
        boolean pathsOrSets = true;
        int [] densities = new int[]{ 0, 20 };

        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : densities) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample) {
                    for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{new Pair(states, 2*states )})
                    {
                        int traceQuantityToUse = traces_lengthmult.firstElem;
                        for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample)
                            for (LearningAlgorithms.ScoringToApply learnerKind :
                                    new LearningAlgorithms.ScoringToApply[]{
                                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV,
//                                            LearningAlgorithms.ScoringToApply.SCORING_EDSM_1, LearningAlgorithms.ScoringToApply.SCORING_EDSM_2, LearningAlgorithms.ScoringToApply.SCORING_EDSM_4,
//                                            LearningAlgorithms.ScoringToApply.SCORING_PTAK_1, LearningAlgorithms.ScoringToApply.SCORING_PTAK_2,
                                            LearningAlgorithms.ScoringToApply.SCORING_VH
                                    })
                            // LEARNER_EDSMMARKOV("edsm_markov"),LEARNER_EDSM2("edsm_2"),LEARNER_EDSM4("edsm_4"),LEARNER_KTAILS_PTA1("kpta=1"),LEARNER_KTAILS_PTA2("kpta=2"),LEARNER_KTAILS_1("k=1"), LEARNER_KTAILS_2("k=2"),LEARNER_SICCO("SV");

//                                for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? new int[]{3, 4} : new int[]{2})
//                                    for (double weightOfInconsistencies : learnerKind.isMarkov() ?
//                                            new double[]{0.25, 0.5, 1.0, 2.0, 4.0, 8.0}
//                                            : new double[]{1.0})
                            {
                                int chunkSizeToEvaluate = 3;
                                double weightOfInconsistencies = 1.0;
                                ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                ev.config = learningGroup.eval.config.copy();
                                ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                MarkovLearningBaselineParameters parameters = new MarkovLearningBaselineParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
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

        learningGroup.experimentRunner.collectOutcomeOfExperiments(new SGE_ExperimentRunner.processSubExperimentResult<MarkovLearningParameters, ExperimentResult<MarkovLearningParameters>>() {

            @Override
            public void processSubResult(ExperimentResult<MarkovLearningParameters> result, SGE_ExperimentRunner.RunSubExperiment<MarkovLearningParameters, ExperimentResult<MarkovLearningParameters>> experimentrunner) throws
                    IOException {// in these experiments, samples are singleton sequences because we run each of them in a separate process, in order to increase the efficiency with which all tasks are split between CPUs in an iceberg grid.
                PairQualityLearner.SampleData sm = result.samples.get(0);
                PairQualityLearner.ScoresForGraph data = sm.actualLearner;

                StringBuffer csvLine = new StringBuffer();
                csvLine.append(data.whetherLearningSuccessfulOrAborted);
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceBCR.getValue());// 1
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceStructural.getValue());// 2
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.invalidMergersNearRoot);// 3
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.missedMergersNearRoot); // 4
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.invalidMergersFarFromRoot);// 5
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.missedMergersFarFromRoot); // 6
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.validMergers); // 7
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.nrOfstates.getValue());// 8
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.inconsistencyReference);// 9
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistency);// 10

                if (result.parameters.learnerToUse.isMarkov()) {
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistencyAverage);// 11
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistencySD);// 12
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistencyAlwaysPositive);// 13
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.fractionOfStatesIdentifiedBySingletons);// 14
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovTransitionPrecision);// 15
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovTransitionRecall);// 16
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovHolePrecision);// 17
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovHoleRecall);// 18
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.relativeInconsistencyForReferenceGraph);// 19
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.relativeInconsistency);// 20
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.comparisonsPerformed);// 21
                }

                if (result.parameters.markovParameters.useCentreVertex) {
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(sm.centreCorrect);
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(sm.centrePathNumber);
                }
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.referenceGraph.pathroutines.computeAlphabet().size());
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(100. * ConfusionMatrix.divide(sm.referenceGraph.pathroutines.countEdges(),sm.referenceGraph.getStateNumber()*sm.referenceGraph.getStateNumber())));
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.transitionsSampled);
                CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(data.executionTime / 1000000000.));// execution time is in nanoseconds, we only need seconds.
                experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
            }

            @Override
            public DrawGraphs.SGEExperimentResult[] getGraphs() {

                return new DrawGraphs.SGEExperimentResult[]{resultCSV};
            }

        });
        int referencePreset = 0;
        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
            for (int states : learningGroup.statesToUse)
                for (int perStateSquaredDensity100 : densities) {
                    String presetStr = "";
                    String referencePresetStr = "-" + referencePreset;
                    String experimentName = learningGroup.outPathPrefix + "baseline_"+states+"_"+perStateSquaredDensity100+"_";
                    DataSelection source = new DataSelection(resultCSV,states,perStateSquaredDensity100);

                    final DrawGraphs.RBagPlot gr_StructuralVsInconsistency = new DrawGraphs.RBagPlot("Inconsistency Learnt", "Structural Score", new File(experimentName + "inconsistency_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_BCR_vs_structural = new DrawGraphs.RBagPlot("BCR", "Structural Score", new File(experimentName + "bcr_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_TotalMergersVsStructuralScore = new DrawGraphs.RBagPlot("Total mergers", "Structural Score", new File(experimentName + "totalmergers_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_MistakesNearRootVsStructuralScore = new DrawGraphs.RBagPlot("Mistakes near root", "Structural Score", new File(experimentName  + "mistakes_nearroot_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_InvalidMergersNearRootVsStructuralScore = new DrawGraphs.RBagPlot("Invalid mergers near root", "Structural Score", new File(experimentName + "invalidmergers_nearroot_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_MissedMergersNearRootVsStructuralScore = new DrawGraphs.RBagPlot("Missed Mergers near root", "Structural Score", new File(experimentName + "missedmergers_nearroot_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_BCRVsInconsistency = new DrawGraphs.RBagPlot("Inconsistency Learnt", "BCR Score, EDSM-Markov", new File(experimentName + "inconsistency_bcr.pdf"));
                    final DrawGraphs.SquareBagPlot gr_StructuralDiff = new DrawGraphs.SquareBagPlot("Structural score, Sicco", "Structural Score, EDSM-Markov", new File(experimentName + "sicco_structuraldiff.pdf"), 0, 1, true);
                    final DrawGraphs.SquareBagPlot gr_StructuralDiffLowDensity = new DrawGraphs.SquareBagPlot("Structural score, Sicco", "Structural Score, EDSM-Markov", new File(experimentName + "lowdensity_sicco_structuraldiff.pdf"), 0, 1, true);
                    final DrawGraphs.RBagPlot gr_MarkovTransitionPrecisionStructuralDiff = new DrawGraphs.RBagPlot("Transition precision Markov", "Structural Score, EDSM-Markov", new File(experimentName + "markovtransitionprecision_structuraldiff.pdf"));
                    final DrawGraphs.RBagPlot gr_MarkovHolePrecisionStructuralDiff = new DrawGraphs.RBagPlot("Hole precision Markov", "Structural Score, EDSM-Markov", new File(experimentName + "markovholeprecision_structuraldiff.pdf"));
                    final DrawGraphs.RBagPlot gr_Inconsistencies_and_SD = new DrawGraphs.RBagPlot("Inconsistency, average", "Inconsistency, SD", new File(experimentName + "_inconsistencies_sd.pdf"));
                    final DrawGraphs.RBoxPlot<String> gr_PosnegNegativeInconsistencies_Structural = new DrawGraphs.RBoxPlot<>("Inconsistency always positive", "Structural difference", new File(experimentName + "posneginconsistencies_structuraldiff.pdf"));
                    final DrawGraphs.SquareBagPlot gr_BCR = new DrawGraphs.SquareBagPlot("BCR, Sicco", "BCR, EDSM-Markov", new File(experimentName + "_trace_bcr.pdf"), 0.5, 1, true);
                    final DrawGraphs.SquareBagPlot BCRAgainstKtails = new DrawGraphs.SquareBagPlot("BCR, K-tails,1", "BCR, EDSM-Markov", new File(experimentName + "kt_markov_bcr.pdf"), 0.5, 1, true);
                    final DrawGraphs.SquareBagPlot BCRAgainstEDSM_1 = new DrawGraphs.SquareBagPlot("BCR, EDSM-1", "BCR, EDSM-Markov", new File(experimentName + "edsm-1_markov_bcr.pdf"), 0.5, 1, true);
                    final DrawGraphs.SquareBagPlot BCRAgainstEDSM_2 = new DrawGraphs.SquareBagPlot("BCR, EDSM-2", "BCR, EDSM-Markov", new File(experimentName + "edsm-2_markov_bcr.pdf"), 0.5, 1, true);

                    final DrawGraphs.WilcoxonPairedTest Wilcoxon_test_Structural = new DrawGraphs.WilcoxonPairedTest(new File(experimentName + "Wilcoxon_t_str.csv"));
                    final DrawGraphs.WilcoxonPairedTest Wilcoxon_Test_BCR = new DrawGraphs.WilcoxonPairedTest(new File(experimentName + "Wilcoxon_t_bcr.csv"));
                    final DrawGraphs.Mann_Whitney_U_Test Mann_Whitney_U_Test_BCR = new DrawGraphs.Mann_Whitney_U_Test(new File(experimentName + "Mann_Whitney_U_Test_BCR.csv"));
                    final DrawGraphs.Mann_Whitney_U_Test Mann_Whitney_U_Test_Structural = new DrawGraphs.Mann_Whitney_U_Test(new File(experimentName + "Whitney_U_Test_str.csv"));
                    final DrawGraphs.Kruskal_Wallis Kruskal_Wallis_Test_BCR = new DrawGraphs.Kruskal_Wallis(new File(experimentName + "Kruskal_Wallis_Test_BCR.csv"));
                    final DrawGraphs.Kruskal_Wallis Kruskal_Wallis_Test_Structural = new DrawGraphs.Kruskal_Wallis(new File(experimentName + "Kruskal_Wallis_Test_str.csv"));
                    // names of columns include parameters used with learners, here we ignore that and pick those that match learner names
                    DrawGraphs.spreadsheetToBagPlotNoZeroYValues(gr_StructuralVsInconsistency, source, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 10,
                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);
                    DrawGraphs.spreadsheetToBagPlotNoZeroYValues(gr_BCR_vs_structural, source, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 1,
                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);
                    DrawGraphs.spreadsheetToBagPlotNoZeroYValues(gr_BCRVsInconsistency, source, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 10,
                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, null, null);
                    DrawGraphs.spreadsheetToBagPlot(gr_StructuralDiff, source, LearningAlgorithms.ScoringToApply.SCORING_VH + referencePresetStr, 2,
                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);
                    DrawGraphs.spreadsheetToBagPlot(gr_MarkovTransitionPrecisionStructuralDiff, source, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 15,
                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);
                    DrawGraphs.spreadsheetToBagPlot(gr_MarkovHolePrecisionStructuralDiff, source, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 16,
                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);

                    DrawGraphs.spreadsheetToBagPlot(gr_Inconsistencies_and_SD, source, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 11,
                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 12, null, null);

                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                        String[] rowValues = rowEntry.getKey().split("[_=]");
                        assert rowValues[10].equals("d");
                        assert rowValues[6].equals("S");

                        if (Double.parseDouble(rowValues[11]) == perStateSquaredDensity100 && Integer.parseInt(rowValues[7]) == states)
                            getAllValuesFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, (columnText, Y) -> {
                                boolean alwaysPositive = Boolean.parseBoolean(obtainValueFromCell(Y, 13));
                                double value = Double.parseDouble(obtainValueFromCell(Y, 2));
//                                gr_StructuralVsInconsistency.add(Double.parseDouble(obtainValueFromCell(X, cellWithinX)), value, colour, label);


                                String Y_Sicco = getValueFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_VH + "-0");
                                gr_StructuralDiffLowDensity.add(
                                        Double.parseDouble(obtainValueFromCell(Y_Sicco, 2)),
                                        Double.parseDouble(obtainValueFromCell(Y, 2)), null, null);
                                gr_PosnegNegativeInconsistencies_Structural.add(Boolean.toString(alwaysPositive), value, null, null);
                                gr_TotalMergersVsStructuralScore.add(
                                        Double.parseDouble(obtainValueFromCell(Y, 3)) + Double.parseDouble(obtainValueFromCell(Y, 5)) + Double.parseDouble(obtainValueFromCell(Y, 7)),
                                        Double.parseDouble(obtainValueFromCell(Y, 2)), null, null);
                                gr_MistakesNearRootVsStructuralScore.add(
                                        Double.parseDouble(obtainValueFromCell(Y, 3)) + Double.parseDouble(obtainValueFromCell(Y, 4)),
                                        Double.parseDouble(obtainValueFromCell(Y, 2)), null, null);
                                gr_InvalidMergersNearRootVsStructuralScore.add(
                                        Double.parseDouble(obtainValueFromCell(Y, 3)),
                                        Double.parseDouble(obtainValueFromCell(Y, 2)), null, null);
                                gr_MissedMergersNearRootVsStructuralScore.add(
                                        Double.parseDouble(obtainValueFromCell(Y, 4)),
                                        Double.parseDouble(obtainValueFromCell(Y, 2)), null, null);
                        });
                    }

                    DrawGraphs.spreadsheetToBagPlot(gr_BCR, source, LearningAlgorithms.ScoringToApply.SCORING_VH + referencePresetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, null, null);
                    DrawGraphs.spreadsheetToBagPlot(BCRAgainstKtails, source, LearningAlgorithms.ScoringToApply.SCORING_PTAK_1 + referencePresetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, null, null);
                    DrawGraphs.spreadsheetToBagPlot(BCRAgainstEDSM_1, source, LearningAlgorithms.ScoringToApply.SCORING_EDSM_1 + referencePresetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, null, null);
                    DrawGraphs.spreadsheetToBagPlot(BCRAgainstEDSM_2, source, LearningAlgorithms.ScoringToApply.SCORING_EDSM_2 + referencePresetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, null, null);

                    DrawGraphs.spreadsheetAsDouble(Wilcoxon_Test_BCR, source, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_VH + referencePresetStr, 1);
                    DrawGraphs.spreadsheetAsDouble(Wilcoxon_test_Structural, source, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, LearningAlgorithms.ScoringToApply.SCORING_VH + referencePresetStr, 2);
                    DrawGraphs.spreadsheetAsDouble(Mann_Whitney_U_Test_BCR, source, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_VH + referencePresetStr, 1);
                    DrawGraphs.spreadsheetAsDouble(Mann_Whitney_U_Test_Structural, source, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, LearningAlgorithms.ScoringToApply.SCORING_VH + referencePresetStr, 2);
                    DrawGraphs.spreadsheetAsDouble(Kruskal_Wallis_Test_BCR, source, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_VH + referencePresetStr, 1);
                    DrawGraphs.spreadsheetAsDouble(Kruskal_Wallis_Test_Structural, source, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, LearningAlgorithms.ScoringToApply.SCORING_VH + referencePresetStr, 2);

                    for (@SuppressWarnings("rawtypes") DrawGraphs.RExperimentResult result : new DrawGraphs.RExperimentResult[]{gr_StructuralVsInconsistency, gr_BCRVsInconsistency,
                            gr_MarkovTransitionPrecisionStructuralDiff, gr_MarkovHolePrecisionStructuralDiff, gr_StructuralDiff, gr_StructuralDiffLowDensity, gr_BCR_vs_structural,
                            gr_Inconsistencies_and_SD, gr_PosnegNegativeInconsistencies_Structural, gr_TotalMergersVsStructuralScore,
                            gr_MistakesNearRootVsStructuralScore, gr_MissedMergersNearRootVsStructuralScore, gr_InvalidMergersNearRootVsStructuralScore,
                            gr_BCR, BCRAgainstKtails, BCRAgainstEDSM_1, BCRAgainstEDSM_2,
                            Wilcoxon_Test_BCR, Wilcoxon_test_Structural, Mann_Whitney_U_Test_BCR, Mann_Whitney_U_Test_Structural, Kruskal_Wallis_Test_Structural, Kruskal_Wallis_Test_BCR}) {
                        result.reportResults(learningGroup.gr);
                    }
                }
        }

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            for (int states : learningGroup.statesToUse)
                for (int perStateSquaredDensity100 : densities) {
                    Map<String, AtomicInteger> learnerToHowOftenBest = new HashMap<>();
                    final DrawGraphs.SquareBagPlot gr_StructuralDiffBest = new DrawGraphs.SquareBagPlot("Structural score, Sicco", "Structural Score, EDSM-Markov",
                            new File(learningGroup.outPathPrefix + "baseline_" + states + "_" + perStateSquaredDensity100 + "_sicco_structuraldiffBest.pdf"), 0, 1, true);
                    final RBoxPlot<String> gr_PerformanceOfLearners = new RBoxPlot<>("", "Structural Score",
                            new File(learningGroup.outPathPrefix + "baseline_" + states + "_" + perStateSquaredDensity100 + "_baseline_learner_structural.pdf"));
                    gr_PerformanceOfLearners.setOtherOptions("las=2");
                    // Now select the best result from all those available
                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                        final MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport();
                        String[] rowValues = rowEntry.getKey().split("[_=]");
                        assert rowValues[10].equals("d");
                        assert rowValues[6].equals("S");

                        if (Double.parseDouble(rowValues[11]) == perStateSquaredDensity100 && Integer.parseInt(rowValues[7]) == states) {
                            getAllValuesFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_MARKOV.toString(), (columnText, Y) -> {
                                boolean learntOK = obtainValueFromCell(Y, 0).equals("L_OK");
                                boolean alwaysPositive = Boolean.parseBoolean(obtainValueFromCell(Y, 13));
                                double bcr = Double.parseDouble(obtainValueFromCell(Y, 1));
                                double structural = Double.parseDouble(obtainValueFromCell(Y, 2));
                                long inconsistency = Long.parseLong(obtainValueFromCell(Y, 10));

                                if (learntOK)
                                    bestLearningResult.updateIfValueBetter(new MarkovExperiment.LearningReport(bcr, structural, inconsistency, alwaysPositive, columnText));
                            });
                            learnerToHowOftenBest.computeIfAbsent(bestLearningResult.descr, s -> new AtomicInteger(0));
                            learnerToHowOftenBest.get(bestLearningResult.descr).addAndGet(1);
                            String Y_Sicco = getValueFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_VH + "-0");
                            if (Y_Sicco != null)
                                gr_StructuralDiffBest.add(Double.parseDouble(obtainValueFromCell(Y_Sicco, 2)), bestLearningResult.structural, null, null);
                            else
                                System.out.println("WARNING: missing Sicco-value for " + rowEntry.getKey());

//                gr_PerformanceOfLearners.add("MARKOV",bestLearningResult.structural, null, null);

                            for (Map.Entry<String, String> entry : rowEntry.getValue().entrySet())
                                if (!entry.getKey().startsWith(LearningAlgorithms.ScoringToApply.SCORING_MARKOV.toString())) {
                                    String[] learnerKind = entry.getKey().split("[-]");
                                    gr_PerformanceOfLearners.add(learnerKind[0], Double.parseDouble(obtainValueFromCell(entry.getValue(), 2)), null, null);
                                }
                        }
                    }
                    gr_StructuralDiffBest.reportResults(learningGroup.gr);
                    gr_PerformanceOfLearners.reportResults(learningGroup.gr);
                    List<String> learners = new ArrayList<>(learnerToHowOftenBest.keySet());
                    learners.sort((o1, o2) ->
                            learnerToHowOftenBest.get(o2).get() - learnerToHowOftenBest.get(o1).get());
                    System.out.println("States: "+states+" density: "+perStateSquaredDensity100);
                    for (String l : learners)
                        System.out.println(l + " -> " + learnerToHowOftenBest.get(l).get());
                }
        }
        return resultCSV;
    }
}
