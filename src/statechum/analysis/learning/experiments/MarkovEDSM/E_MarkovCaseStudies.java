package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.Pair;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.mutation.DiffExperiments;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.FsmParserDot;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.rpnicore.FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND;
import static statechum.analysis.learning.rpnicore.FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovCaseStudies {
    public static String [] caseStudies = new String[] {"coffeemachine"};//,"OpenSSH-8.8p1","TCP_Linux_Client","tls-1.2-openssl-1.1.1","xraypowercontrol - learnresult6"};


    public static class MarkovLearningBaselineParameters extends MarkovLearningParameters {

        public MarkovLearningBaselineParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample, int argSeed) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample, argSeed);
        }

        @Override
        public String getSubExperimentName() {
            return "casestudies";
        }
    }

    public static class MarkovLearnerRunnerForCaseStudies extends MarkovExperiment.MarkovLearnerRunner {

        public MarkovLearnerRunnerForCaseStudies(MarkovLearningBaselineParameters parameters, ProgressDecorator.LearnerEvaluationConfiguration ev) {
            super(parameters, ev);
        }

        /** Constructs a reference graph and assigns it to member variable <pre>referenceGraph</pre>. This is a separate method to permit overriding by subclasses.
         */
        public void generateReferenceFSM()
        {
            Configuration dotConfig = learnerInitConfiguration.config.copy();dotConfig.setLabelKind(Configuration.LABELKIND.LABEL_STRING);
            String referenceDot;
            try {
                referenceDot = Helper.loadFile(new File(
                        GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_CASESTUDIES)+
                                File.separator+caseStudies[par.sample]+".dot"));
            } catch (IOException e) {
                throw new RuntimeException("Failed to load graph "+e);
            }
            referenceGraph = FsmParserDot.buildLearnerGraph(referenceDot,dotConfig,
                    learnerInitConfiguration.getLabelConverter(), true,true,USE_START0);
        }
    }

    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        int[] learnerExperiment = new int[]{0,1,2,3,4};
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + "results_casestudies.csv"));
        boolean aveOrMax = true;// average divide by the divisor

        boolean pathsOrSets = true;
        String pathToCaseStudyFiles = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_CASESTUDIES);
        if (null == pathToCaseStudyFiles ||  pathToCaseStudyFiles.isEmpty())
            throw new RuntimeException("Cannot load any case studies: path to case studies is not defined");
        if (!Files.exists(Paths.get(pathToCaseStudyFiles)))
            throw new RuntimeException("Cannot load any case studies: path to case studies does not exist "+pathToCaseStudyFiles);

        for (int casestudy=0; casestudy<caseStudies.length; casestudy++) {
            System.out.println("Loading " + caseStudies[casestudy]);
            Configuration dotConfig = learningGroup.eval.config.copy();
            dotConfig.setLabelKind(Configuration.LABELKIND.LABEL_STRING);
            String referenceDot;
            try {
                referenceDot = Helper.loadFile(new File(pathToCaseStudyFiles + File.separator + caseStudies[casestudy] + ".dot"));
            } catch (IOException e) {
                throw new RuntimeException("Failed to load graph " + e);
            }
            LearnerGraph reference = FsmParserDot.buildLearnerGraph(referenceDot, dotConfig,
                    new Transform.InternStringLabel(), true, true, USE_START0);
            int states = reference.getStateNumber();
            for (final int preset : learnerExperiment)
                for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{new Pair(1, reference.getCache().getAlphabet().size() * states * states)}) {
                    int traceQuantityToUse = traces_lengthmult.firstElem;
                    for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample)
                        for (LearningAlgorithms.ScoringToApply learnerKind :
                                preset == 0 ?// this is the only case where we can apply PTA-based merging algorithms, two other presets handle merging vertices in a connected graph
                                        new LearningAlgorithms.ScoringToApply[]{
                                                LearningAlgorithms.ScoringToApply.SCORING_MARKOV,
                                                LearningAlgorithms.ScoringToApply.SCORING_EDSM_1, LearningAlgorithms.ScoringToApply.SCORING_EDSM_2, LearningAlgorithms.ScoringToApply.SCORING_EDSM_4,
                                                LearningAlgorithms.ScoringToApply.SCORING_PTAK_1, LearningAlgorithms.ScoringToApply.SCORING_PTAK_2,
                                                LearningAlgorithms.ScoringToApply.SCORING_SICCO
                                        } :
                                        new LearningAlgorithms.ScoringToApply[]{
                                                LearningAlgorithms.ScoringToApply.SCORING_MARKOV
                                        })
                            for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? new int[]{2, 3, 4} : new int[]{2})
                                for (double weightOfInconsistencies : learnerKind.isMarkov() ?
                                        new double[]{0.25, 0.5, 1.0, 1.5, 2.0, 4.0, 8.0}
                                        : new double[]{1.0})
                                    for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 1)} : new Pair[]{new Pair(1, 1), new Pair(1, 2), new Pair(2, 4)}) {
                                        {
                                            ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                            ev.config = learningGroup.eval.config.copy();
                                            ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                            MarkovLearningBaselineParameters parameters = new MarkovLearningBaselineParameters(learnerKind, states, 0, 0, casestudy, trainingSample, 0);
                                            parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
                                            parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, 0);
                                            parameters.markovParameters.setMarkovParameters(preset, chunkSizeToEvaluate, pathsOrSets, weightOfInconsistencies, aveOrMax, wlen_divisor.secondElem, 0, wlen_divisor.firstElem);
                                            parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                            MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovLearnerRunnerForCaseStudies(parameters, ev);
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
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.centreCorrect);
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.centrePathNumber);
                    }
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.transitionsSampled);
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(data.executionTime / 1000000000.));// execution time is in nanoseconds, we only need seconds.
                    experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
                }

                @Override
                public SGEExperimentResult[] getGraphs()
                {
                    return new SGEExperimentResult[]{resultCSV};
                }

            });
            int referencePreset = 0;
            if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
                for (final int preset : learnerExperiment) {
                    String presetStr = "casestudies-" + preset;
                    String referencePresetStr = "-" + referencePreset;
                    String experimentName = learningGroup.outPathPrefix + "preset_" + 0 + "_";
                    final RBagPlot gr_StructuralVsInconsistency = new RBagPlot("Inconsistency Learnt", "Structural Score", new File(experimentName + "_inconsistency_structural.pdf"));
                    final RBagPlot gr_BCR_vs_structural = new RBagPlot("BCR", "Structural Score", new File(experimentName + "_bcr_structural.pdf"));
                    final RBagPlot gr_TotalMergersVsStructuralScore = new RBagPlot("Total mergers", "Structural Score", new File(experimentName + "_totalmergers_structural.pdf"));
                    final RBagPlot gr_MistakesNearRootVsStructuralScore = new RBagPlot("Mistakes near root", "Structural Score", new File(experimentName + "_mistakes_nearroot_structural.pdf"));
                    final RBagPlot gr_InvalidMergersNearRootVsStructuralScore = new RBagPlot("Invalid mergers near root", "Structural Score", new File(experimentName + "_invalidmergers_nearroot_structural.pdf"));
                    final RBagPlot gr_MissedMergersNearRootVsStructuralScore = new RBagPlot("Missed Mergers near root", "Structural Score", new File(experimentName + "_missedmergers_nearroot_structural.pdf"));
                    final RBagPlot gr_BCRVsInconsistency = new RBagPlot("Inconsistency Learnt", "BCR Score, EDSM-Markov learner", new File(experimentName + "_inconsistency_bcr.pdf"));
                    final SquareBagPlot gr_StructuralDiff = new SquareBagPlot("Structural score, Sicco", "Structural Score, EDSM-Markov learner", new File(experimentName + "_sicco_structuraldiff.pdf"), 0, 1, true);
                    final RBagPlot gr_MarkovTransitionPrecisionStructuralDiff = new RBagPlot("Transition precision Markov", "Structural Score, EDSM-Markov learner", new File(experimentName + "_markovtransitionprecision_structuraldiff.pdf"));
                    final RBagPlot gr_MarkovHolePrecisionStructuralDiff = new RBagPlot("Hole precision Markov", "Structural Score, EDSM-Markov learner", new File(experimentName + "_markovholeprecision_structuraldiff.pdf"));
                    final RBagPlot gr_Inconsistencies_and_SD = new RBagPlot("Inconsistency, average", "Inconsistency, SD", new File(experimentName + "_inconsistencies_sd.pdf"));
                    final RBoxPlot<String> gr_PosnegNegativeInconsistencies_Structural = new RBoxPlot<>("Inconsistency always positive", "Structural difference", new File(experimentName + "_posneginconsistencies_structuraldiff.pdf"));
                    final SquareBagPlot gr_BCR = new SquareBagPlot("BCR, Sicco", "BCR, EDSM-Markov learner", new File(experimentName + "_trace_bcr.pdf"), 0.5, 1, true);
                    final SquareBagPlot BCRAgainstKtails = new SquareBagPlot("BCR, K-tails,1", "BCR, EDSM-Markov learner", new File(experimentName + "_" + "_kt_markov_bcr.pdf"), 0.5, 1, true);
                    final SquareBagPlot BCRAgainstEDSM_1 = new SquareBagPlot("BCR, EDSM-1", "BCR, EDSM-Markov learner", new File(experimentName + "_" + "_edsm-1_markov_bcr.pdf"), 0.5, 1, true);
                    final SquareBagPlot BCRAgainstEDSM_2 = new SquareBagPlot("BCR, EDSM-2", "BCR, EDSM-Markov learner", new File(experimentName + "_" + "_edsm-2_markov_bcr.pdf"), 0.5, 1, true);

                    final WilcoxonPairedTest Wilcoxon_test_Structural = new WilcoxonPairedTest(new File(experimentName + "Wilcoxon_t_str.csv"));
                    final WilcoxonPairedTest Wilcoxon_Test_BCR = new WilcoxonPairedTest(new File(experimentName + "Wilcoxon_t_bcr.csv"));
                    final Mann_Whitney_U_Test Mann_Whitney_U_Test_BCR = new Mann_Whitney_U_Test(new File(experimentName + "Mann_Whitney_U_Test_BCR.csv"));
                    final Mann_Whitney_U_Test Mann_Whitney_U_Test_Structural = new Mann_Whitney_U_Test(new File(experimentName + "Whitney_U_Test_str.csv"));
                    final Kruskal_Wallis Kruskal_Wallis_Test_BCR = new Kruskal_Wallis(new File(experimentName + "Kruskal_Wallis_Test_BCR.csv"));
                    final Kruskal_Wallis Kruskal_Wallis_Test_Structural = new Kruskal_Wallis(new File(experimentName + "Kruskal_Wallis_Test_str.csv"));
                    // names of columns include parameters used with learners, here we ignore that and pick those that match learner names
                    DrawGraphs.spreadsheetToBagPlotNoZeroYValues(gr_StructuralVsInconsistency, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 10, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);
                    DrawGraphs.spreadsheetToBagPlotNoZeroYValues(gr_BCR_vs_structural, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 1, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);
                    DrawGraphs.spreadsheetToBagPlotNoZeroYValues(gr_BCRVsInconsistency, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 10, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 1, null, null);
                    DrawGraphs.spreadsheetToBagPlot(gr_StructuralDiff, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_SICCO + referencePresetStr, 2, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);
                    DrawGraphs.spreadsheetToBagPlot(gr_MarkovTransitionPrecisionStructuralDiff, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 15, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);
                    DrawGraphs.spreadsheetToBagPlot(gr_MarkovHolePrecisionStructuralDiff, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + referencePresetStr, 16, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 2, null, null);

                    DrawGraphs.spreadsheetToBagPlot(gr_Inconsistencies_and_SD, resultCSV, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 11, LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, 12, null, null);
                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                        String[] rowValues = rowEntry.getKey().split("[_=]");

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
                            gr_InvalidMergersNearRootVsStructuralScore.add(
                                    Double.parseDouble(obtainValueFromCell(Y, 3)),
                                    Double.parseDouble(obtainValueFromCell(Y, 2)), null, null);
                            gr_MissedMergersNearRootVsStructuralScore.add(
                                    Double.parseDouble(obtainValueFromCell(Y, 4)),
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

                    for (@SuppressWarnings("rawtypes") RExperimentResult result : new RExperimentResult[]{gr_StructuralVsInconsistency, gr_BCRVsInconsistency,
                            gr_MarkovTransitionPrecisionStructuralDiff, gr_MarkovHolePrecisionStructuralDiff, gr_StructuralDiff, gr_BCR_vs_structural,
                            gr_Inconsistencies_and_SD, gr_PosnegNegativeInconsistencies_Structural, gr_TotalMergersVsStructuralScore,
                            gr_MistakesNearRootVsStructuralScore, gr_MissedMergersNearRootVsStructuralScore, gr_InvalidMergersNearRootVsStructuralScore,
                            gr_BCR, BCRAgainstKtails, BCRAgainstEDSM_1, BCRAgainstEDSM_2,
                            Wilcoxon_Test_BCR, Wilcoxon_test_Structural, Mann_Whitney_U_Test_BCR, Mann_Whitney_U_Test_Structural, Kruskal_Wallis_Test_Structural, Kruskal_Wallis_Test_BCR}) {
                        result.reportResults(learningGroup.gr);
                    }
                }
            }


            if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
                Map<String, AtomicInteger> learnerToHowOftenBest = new HashMap<>();
                final SquareBagPlot gr_StructuralDiffBest = new SquareBagPlot("Structural score, Sicco", "Structural Score, EDSM-Markov learner", new File(learningGroup.outPathPrefix + "_casestudies" + "_sicco_structuraldiffBest.pdf"), 0, 1, true);
                final RBoxPlot<String> gr_PerformanceOfLearners = new RBoxPlot<>("", "Structural Score",
                        new File(learningGroup.outPathPrefix + "_casestudies_learner_structural.pdf"));
                gr_PerformanceOfLearners.setOtherOptions("las=2");
                // Now select the best result from all those available
                for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                    final MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport();

                    getAllValuesFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_MARKOV.toString(), (columnText, Y) -> {
                        boolean learntOK = obtainValueFromCell(Y, 0).equals("L_OK");
                        boolean alwaysPositive = Boolean.parseBoolean(obtainValueFromCell(Y, 13));
                        double bcr = Double.parseDouble(obtainValueFromCell(Y, 1));
                        double structural = Double.parseDouble(obtainValueFromCell(Y, 2));
                        long inconsistency = Long.parseLong(obtainValueFromCell(Y, 10));

                        if (learntOK && alwaysPositive)
                            bestLearningResult.updateIfValueBetter(new MarkovExperiment.LearningReport(bcr, structural, inconsistency, columnText));
                    });
                    learnerToHowOftenBest.computeIfAbsent(bestLearningResult.descr, s -> new AtomicInteger(0));
                    learnerToHowOftenBest.get(bestLearningResult.descr).addAndGet(1);
                    String Y_Sicco = getValueFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_SICCO + "-0");
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
                gr_StructuralDiffBest.reportResults(learningGroup.gr);
                gr_PerformanceOfLearners.reportResults(learningGroup.gr);
                List<String> learners = new ArrayList<>(learnerToHowOftenBest.keySet());
                learners.sort((o1, o2) ->
                        learnerToHowOftenBest.get(o2).get() - learnerToHowOftenBest.get(o1).get());
                for (String l : learners)
                    System.out.println(l + " -> " + learnerToHowOftenBest.get(l).get());
            }
        }
}
