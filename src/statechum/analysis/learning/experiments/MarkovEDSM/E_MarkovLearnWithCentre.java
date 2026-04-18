package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Pair;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import static statechum.analysis.learning.DrawGraphs.*;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovLearnWithCentre {

    public static class MarkovLearningWithCentreParameters extends MarkovLearningParameters {

        public MarkovLearningWithCentreParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample, int argSeed) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample, argSeed);
        }

        @Override
        public String getSubExperimentName() {
            return "usingcentre";
        }
    }

    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        int[] learnerExperiment = new int[]{0,1,2,3};
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + "results.csv"));
        final int statesMax = learningGroup.statesToUse[learningGroup.statesToUse.length-1];// reflects the size of the largest FSM that will be generated.
        boolean aveOrMax = true;// average divide by the divisor

        int alphabetMultiplier = 2;
        boolean pathsOrSets = true;

        int seedForFSM = 0;
        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : new int[]{0, 30}) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample, ++seedForFSM)
                    for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{new Pair(8*states/learningGroup.statesToUse[0], 32),new Pair(1,256*states/learningGroup.statesToUse[0])})
                    {
                        int traceQuantityToUse = traces_lengthmult.firstElem;
                        for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample)
                            for (final int preset : learnerExperiment)
                                for (LearningAlgorithms.ScoringToApply learnerKind :
                                        preset == 0 ?// this is the only case where we can apply PTA-based merging algorithms, two other presets handle merging vertices in a connected graph
                                                new LearningAlgorithms.ScoringToApply[]{
                                                        LearningAlgorithms.ScoringToApply.SCORING_MARKOV,
//														ScoringToApply.SCORING_EDSM_1, ScoringToApply.SCORING_EDSM_2, ScoringToApply.SCORING_EDSM_4,
//														ScoringToApply.SCORING_PTAK_1, ScoringToApply.SCORING_PTAK_2,
                                                        LearningAlgorithms.ScoringToApply.SCORING_SICCO
                                                } :
                                                new LearningAlgorithms.ScoringToApply[]{
                                                        LearningAlgorithms.ScoringToApply.SCORING_MARKOV
//														ScoringToApply.SCORING_EDSM_1, ScoringToApply.SCORING_EDSM_2
                                                })
                                    // LEARNER_EDSMMARKOV("edsm_markov"),LEARNER_EDSM2("edsm_2"),LEARNER_EDSM4("edsm_4"),LEARNER_KTAILS_PTA1("kpta=1"),LEARNER_KTAILS_PTA2("kpta=2"),LEARNER_KTAILS_1("k=1"), LEARNER_KTAILS_2("k=2"),LEARNER_SICCO("SV");
                                {
                                    int chunkSizeToEvaluate = 3;
                                    double weightOfInconsistencies = 2.0;
                                    for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 1)} : new Pair[]{new Pair(1, 1), new Pair(1, 2), new Pair(2, 4)}) {
                                        int wlen = wlen_divisor.firstElem, divisor = wlen_divisor.secondElem;
                                        ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                        ev.config = learningGroup.eval.config.copy();
                                        ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                        MarkovLearningParameters parameters = new MarkovLearningWithCentreParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample, seedForFSM);
                                        parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
                                        parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, statesMax, alphabetMultiplier);
                                        parameters.markovParameters.setMarkovParameters(preset, chunkSizeToEvaluate, pathsOrSets, weightOfInconsistencies, aveOrMax, divisor, 0, wlen);
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
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.comparisonsPerformed);// 19
                }

                if (result.parameters.markovParameters.useCentreVertex) {
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(sm.centreCorrect);
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(sm.centrePathNumber);
                }
                CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(sm.transitionsSampled);
                CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(Math.round(data.executionTime / 1000000000.));// execution time is in nanoseconds, we only need seconds.
                experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
            }

            @Override
            public SGEExperimentResult[] getGraphs() {

                return new SGEExperimentResult[]{resultCSV};
            }

        });
        int referencePreset = 0;
        for (final int preset : learnerExperiment) {
            if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
                String presetStr = "-" + preset;
                String referencePresetStr = "-" + referencePreset;
                String experimentName = learningGroup.outPathPrefix + "with_centre_"+"preset_" + preset + "_";
                final RBagPlot gr_StructuralVsInconsistency = new RBagPlot("Inconsistency Learnt", "Structural Score, EDSM-Markov learner", new File(experimentName + statesMax + "_inconsistency_structural.pdf"));
                final RBagPlot gr_TotalMergersVsStructuralScore = new RBagPlot("Total mergers", "Structural Score, EDSM-Markov learner", new File(experimentName + statesMax + "_totalmergers_structural.pdf"));
                final RBagPlot gr_MistakesNearRootVsStructuralScore = new RBagPlot("Mistakes near root", "Structural Score, EDSM-Markov learner", new File(experimentName + statesMax + "_mistakesnearroot_structural.pdf"));
                final RBagPlot gr_BCRVsInconsistency = new RBagPlot("Inconsistency Learnt", "BCR Score, EDSM-Markov learner", new File(experimentName + statesMax + "_inconsistency_bcr.pdf"));
                final SquareBagPlot gr_StructuralDiff = new SquareBagPlot("Structural score, Sicco", "Structural Score, EDSM-Markov learner", new File(experimentName + statesMax + "_sicco_structuraldiff.pdf"), 0, 1, true);
                final RBagPlot gr_MarkovTransitionPrecisionStructuralDiff = new RBagPlot("Transition precision Markov", "Structural Score, EDSM-Markov learner", new File(experimentName + statesMax + "_markovtransitionprecision_structuraldiff.pdf"));
                final RBagPlot gr_MarkovHolePrecisionStructuralDiff = new RBagPlot("Hole precision Markov", "Structural Score, EDSM-Markov learner", new File(experimentName + statesMax + "_markovholeprecision_structuraldiff.pdf"));
                final RBagPlot gr_Inconsistencies_and_SD = new RBagPlot("Inconsistency, average", "Inconsistency, SD", new File(experimentName + statesMax + "_inconsistencies_sd.pdf"));
                final RBoxPlot<String> gr_PosnegNegativeInconsistencies_Structural = new RBoxPlot<>("Inconsistency always positive", "Structural difference", new File(experimentName + statesMax + "_posneginconsistencies_structuraldiff.pdf"));
                final SquareBagPlot gr_BCR = new SquareBagPlot("BCR, Sicco", "BCR, EDSM-Markov learner", new File(experimentName + statesMax + "_trace_bcr.pdf"), 0.5, 1, true);
                final SquareBagPlot BCRAgainstKtails = new SquareBagPlot("BCR, K-tails,1", "BCR, EDSM-Markov learner", new File(experimentName + "_" + statesMax + "_kt_markov_bcr.pdf"), 0.5, 1, true);
                final SquareBagPlot BCRAgainstEDSM_1 = new SquareBagPlot("BCR, EDSM-1", "BCR, EDSM-Markov learner", new File(experimentName + "_" + statesMax + "_edsm-1_markov_bcr.pdf"), 0.5, 1, true);
                final SquareBagPlot BCRAgainstEDSM_2 = new SquareBagPlot("BCR, EDSM-2", "BCR, EDSM-Markov learner", new File(experimentName + "_" + statesMax + "_edsm-2_markov_bcr.pdf"), 0.5, 1, true);

                final WilcoxonPairedTest Wilcoxon_test_Structural = new WilcoxonPairedTest(new File(experimentName + "Wilcoxon_t_str.csv"));
                final WilcoxonPairedTest Wilcoxon_Test_BCR = new WilcoxonPairedTest(new File(experimentName + "Wilcoxon_t_bcr.csv"));
                final Mann_Whitney_U_Test Mann_Whitney_U_Test_BCR = new Mann_Whitney_U_Test(new File(experimentName + "Mann_Whitney_U_Test_BCR.csv"));
                final Mann_Whitney_U_Test Mann_Whitney_U_Test_Structural = new Mann_Whitney_U_Test(new File(experimentName + "Whitney_U_Test_str.csv"));
                final Kruskal_Wallis Kruskal_Wallis_Test_BCR = new Kruskal_Wallis(new File(experimentName + "Kruskal_Wallis_Test_BCR.csv"));
                final Kruskal_Wallis Kruskal_Wallis_Test_Structural = new Kruskal_Wallis(new File(experimentName + "Kruskal_Wallis_Test_str.csv"));
                // names of columns include parameters used with learners, here we ignore that and pick those that match learner names
                DrawGraphs.spreadsheetToBagPlotNoZeroYValues(gr_StructuralVsInconsistency, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 10, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);
                DrawGraphs.spreadsheetToBagPlotNoZeroYValues(gr_BCRVsInconsistency, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 10, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, null, null);
                DrawGraphs.spreadsheetToBagPlot(gr_StructuralDiff, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_SICCO + referencePresetStr, 2, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);
                DrawGraphs.spreadsheetToBagPlot(gr_MarkovTransitionPrecisionStructuralDiff, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 15, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);
                DrawGraphs.spreadsheetToBagPlot(gr_MarkovHolePrecisionStructuralDiff, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 16, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);

                DrawGraphs.spreadsheetToBagPlot(gr_Inconsistencies_and_SD, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 11, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 12, null, null);
                for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                    getAllValuesFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, (columnText, Y) -> {
                        boolean alwaysPositive = Boolean.parseBoolean(obtainValueFromCell(Y, 13));
                        double value = Double.parseDouble(obtainValueFromCell(Y, 2));

                        gr_PosnegNegativeInconsistencies_Structural.add(Boolean.toString(alwaysPositive), value, null, null);
                        gr_TotalMergersVsStructuralScore.add(
                                Double.parseDouble(obtainValueFromCell(Y, 3)) + Double.parseDouble(obtainValueFromCell(Y, 5)) + Double.parseDouble(obtainValueFromCell(Y, 7)),
                                Double.parseDouble(obtainValueFromCell(Y, 2)), null, null);
                        gr_MistakesNearRootVsStructuralScore.add(
                                Double.parseDouble(obtainValueFromCell(Y, 3)) + Double.parseDouble(obtainValueFromCell(Y, 4)),
                                Double.parseDouble(obtainValueFromCell(Y, 2)), null, null);
                    });
                }

                DrawGraphs.spreadsheetToBagPlot(gr_BCR, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_SICCO + referencePresetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, null, null);
                DrawGraphs.spreadsheetToBagPlot(BCRAgainstKtails, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_PTAK_1 + referencePresetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, null, null);
                DrawGraphs.spreadsheetToBagPlot(BCRAgainstEDSM_1, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_EDSM_1 + referencePresetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, null, null);
                DrawGraphs.spreadsheetToBagPlot(BCRAgainstEDSM_2, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_EDSM_2 + referencePresetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, null, null);

                DrawGraphs.spreadsheetAsDouble(Wilcoxon_Test_BCR, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_SICCO + referencePresetStr, 1);
                DrawGraphs.spreadsheetAsDouble(Wilcoxon_test_Structural, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, LearningAlgorithms.ScoringToApply.SCORING_SICCO + referencePresetStr, 2);
                DrawGraphs.spreadsheetAsDouble(Mann_Whitney_U_Test_BCR, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_SICCO + referencePresetStr, 1);
                DrawGraphs.spreadsheetAsDouble(Mann_Whitney_U_Test_Structural, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, LearningAlgorithms.ScoringToApply.SCORING_SICCO + referencePresetStr, 2);
                DrawGraphs.spreadsheetAsDouble(Kruskal_Wallis_Test_BCR, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_SICCO + referencePresetStr, 1);
                DrawGraphs.spreadsheetAsDouble(Kruskal_Wallis_Test_Structural, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, LearningAlgorithms.ScoringToApply.SCORING_SICCO + referencePresetStr, 2);
                final AtomicLong comparisonsPerformed = new AtomicLong(0);
				/*
				DrawGraphs.spreadsheetAsString((A, B) -> {
					try {
						comparisonsPerformed.addAndGet(Long.parseLong(A));
					}
					catch(NumberFormatException e) {
						System.out.println("Failed to convert "+e);
					}
				},resultCSV,ScoringToApply.SCORING_MARKOV+presetStr,3,ScoringToApply.SCORING_MARKOV+presetStr,3);
					*/
                for (@SuppressWarnings("rawtypes") RExperimentResult result : new RExperimentResult[]{gr_StructuralVsInconsistency, gr_BCRVsInconsistency,
                        gr_MarkovTransitionPrecisionStructuralDiff, gr_MarkovHolePrecisionStructuralDiff, gr_StructuralDiff,
                        gr_Inconsistencies_and_SD, gr_PosnegNegativeInconsistencies_Structural, gr_TotalMergersVsStructuralScore, gr_MistakesNearRootVsStructuralScore,
                        gr_BCR, BCRAgainstKtails, BCRAgainstEDSM_1, BCRAgainstEDSM_2,
                        Wilcoxon_Test_BCR, Wilcoxon_test_Structural, Mann_Whitney_U_Test_BCR, Mann_Whitney_U_Test_Structural, Kruskal_Wallis_Test_Structural, Kruskal_Wallis_Test_BCR}) {
                    result.reportResults(learningGroup.gr);
                }
                if (learningGroup.experimentRunner.isInteractive())
                    System.out.println("\nLOG of comparisons performed: " + Math.log10(comparisonsPerformed.doubleValue()) + "\n");
            }
        }


        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            Map<String, AtomicInteger> learnerToHowOftenBest = new HashMap<>();
            final SquareBagPlot gr_StructuralDiffBest = new SquareBagPlot("Structural score, Sicco", "Structural Score, EDSM-Markov learner", new File(learningGroup.outPathPrefix + "_withcentre_" + statesMax + "_sicco_structuraldiffBest.pdf"), 0, 1, true);

            // Now select the best result from all those available
            for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                final MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport();

                getAllValuesFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_MARKOV.toString(), (columnText, Y) -> {
                    boolean learntOK = obtainValueFromCell(Y, 0).equals("L_OK");
                    boolean alwaysPositive = Boolean.parseBoolean(obtainValueFromCell(Y, 13));
                    double bcr = Double.parseDouble(obtainValueFromCell(Y, 1));
                    double structural = Double.parseDouble(obtainValueFromCell(Y, 2));
                    long inconsistency = Long.parseLong(obtainValueFromCell(Y, 10));

                    if (learntOK && alwaysPositive && (bestLearningResult.inconsistency < 0 || inconsistency < bestLearningResult.inconsistency)) {
                        bestLearningResult.bcr = bcr;
                        bestLearningResult.structural = structural;
                        bestLearningResult.inconsistency = inconsistency;
                        bestLearningResult.descr = columnText;
                    }
                });
                learnerToHowOftenBest.computeIfAbsent(bestLearningResult.descr, s -> new AtomicInteger(0));
                learnerToHowOftenBest.get(bestLearningResult.descr).addAndGet(1);
                String Y_Sicco = getValueFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_SICCO + "-0");
                if (Y_Sicco != null)
                    gr_StructuralDiffBest.add(Double.parseDouble(obtainValueFromCell(Y_Sicco, 2)), bestLearningResult.structural, null, null);
                else
                    System.out.println("WARNING: missing Sicco-value for " + rowEntry.getKey());
            }
            gr_StructuralDiffBest.reportResults(learningGroup.gr);
            List<String> learners = new ArrayList<>(learnerToHowOftenBest.keySet());
            learners.sort((o1, o2) ->
                    learnerToHowOftenBest.get(o2).get() - learnerToHowOftenBest.get(o1).get());
            for (String l : learners)
                System.out.println(l + " -> " + learnerToHowOftenBest.get(l).get());
        }
    }
}
