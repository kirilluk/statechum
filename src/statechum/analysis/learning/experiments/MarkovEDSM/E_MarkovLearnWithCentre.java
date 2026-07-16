package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Pair;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.io.IOException;
import java.util.*;

import static statechum.analysis.learning.DrawGraphs.*;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovLearnWithCentre {

    public static class MarkovLearningWithCentreParameters extends MarkovLearningParameters {

        public MarkovLearningWithCentreParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample);
        }

        @Override
        public String getSubExperimentName() {
            return "usingcentre";
        }
    }

    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        int[] learnerExperiment = new int[]{0,1,2,3,4};
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + "results.csv"));
        final int statesMax = learningGroup.statesToUse[learningGroup.statesToUse.length-1];// reflects the size of the largest FSM that will be generated.
        boolean aveOrMax = true;// average divide by the divisor
        boolean penaliseMissingPaths = true;
        int alphabetMultiplier = 2;
        boolean pathsOrSets = true;

        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : new int[]{0, 30}) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample)
                {
                    int scalingFactor = states*learningGroup.stateScale/learningGroup.statesToUse[0];
                    for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{new Pair(8*scalingFactor, 32 ),new Pair(1*scalingFactor,256*scalingFactor)})
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
                                    double weightOfInconsistencies = 2.0;
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

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            final RBoxPlot<String> gr_BestStructuralForDifferentPreset = new RBoxPlot<>("Trace length number and learner", "Structural Score",
                    new File(learningGroup.outPathPrefix + statesMax + "_centre-learner_structural.pdf"));
            gr_BestStructuralForDifferentPreset.setOtherOptions("las=2");
            for (int traceQuantityToUse : new int[]{1, 8}) {
                    final RBoxPlot<String> gr_PresetPerformance = new RBoxPlot<>("", "Structural Score",
                            new File(learningGroup.outPathPrefix + statesMax + "_centre-learner_tracenum="+traceQuantityToUse+"_structural.pdf"));
                    gr_PresetPerformance.setOtherOptions("las=2");
                    gr_PresetPerformance.setOrderingOfLabels(Arrays.asList( new String[]{"Best","Markov","M_Both","M_Forward","R_Forward","R_Both"}));
                    final SquareBagPlot gr_StructuralDiffBest = new SquareBagPlot("Structural score, VH", "Structural Score",
                            new File(learningGroup.outPathPrefix + statesMax + "_centre-learner_tracenum=" + traceQuantityToUse + "_VH_structuraldiffBest.pdf"), 0, 1, true);

                    String [] presetDescription = new String[]{"Markov","M_Both","R_Forward","R_Both","M_Forward"};

                    // Now select the best result from all those available
                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {

                        String[] rowValues = rowEntry.getKey().split("[_=]");
                        assert rowValues[0].equals("tQ");
                        if (Double.parseDouble(rowValues[1]) == traceQuantityToUse) {
                            // we are looking at specific rows
                            final Map<Integer,MarkovExperiment.LearningReport> bestLearningResultForThisRowAndAllPresets = new TreeMap<>();

                            MarkovExperiment.LearningReport bestLearningResultForThisRow = new MarkovExperiment.LearningReport();
                            for (final int preset : learnerExperiment) {
                                bestLearningResultForThisRowAndAllPresets.computeIfAbsent(preset, integer -> new MarkovExperiment.LearningReport());
                                MarkovExperiment.LearningReport bestLearningResultForThisRowAndPreset = bestLearningResultForThisRowAndAllPresets.get(preset);

                                getAllValuesFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_MARKOV.toString(), (columnText, Y) -> {
                                    // Here columnText is the description of the learner used, Y is the values reported by processSubResult above.
                                    boolean learntOK = obtainValueFromCell(Y, 0).equals("L_OK");
                                    boolean alwaysPositive = Boolean.parseBoolean(obtainValueFromCell(Y, 13));
                                    double bcr = Double.parseDouble(obtainValueFromCell(Y, 1));
                                    double structural = Double.parseDouble(obtainValueFromCell(Y, 2));
                                    long inconsistency = Long.parseLong(obtainValueFromCell(Y, 10));

                                    String[] columnValues = columnText.split("[_=]");
                                    if (learntOK && columnValues[0].equals(LearningAlgorithms.ScoringToApply.SCORING_MARKOV +"-"+ preset)) {
                                        // Now at the columns of interest (specific preset but different parameter of Markov)
                                        MarkovExperiment.LearningReport report = new MarkovExperiment.LearningReport(bcr, structural, inconsistency, alwaysPositive, columnText);
                                        bestLearningResultForThisRowAndPreset.updateIfValueBetter(report);
                                        bestLearningResultForThisRow.updateIfValueBetter(report);
                                    }
                                });
                            }

                            String Y_VH = getValueFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_VH + "-0");
                            double vh_score = Double.parseDouble(obtainValueFromCell(Y_VH, 2));
                            if (Y_VH != null)
                                gr_StructuralDiffBest.add(vh_score, bestLearningResultForThisRow.structural, null, null);
                            else
                                System.out.println("WARNING: missing VH-value for " + rowEntry.getKey());


                            StringBuilder sb = new StringBuilder();
                            Formatter formatter = new Formatter(sb, Locale.US);formatter.format("%1d",traceQuantityToUse);
                            gr_BestStructuralForDifferentPreset.add(sb+"_M",bestLearningResultForThisRowAndAllPresets.get(0).structural);
                            gr_BestStructuralForDifferentPreset.add(sb+"_MC",bestLearningResultForThisRow.structural);
                            gr_BestStructuralForDifferentPreset.add(sb+"_S",vh_score);
                            for(Map.Entry<Integer, MarkovExperiment.LearningReport> entry:bestLearningResultForThisRowAndAllPresets.entrySet())
                                gr_PresetPerformance.add(presetDescription[entry.getKey()],entry.getValue().structural);
                            gr_PresetPerformance.add("Best",bestLearningResultForThisRow.structural);

                        }
                    }
                    gr_StructuralDiffBest.reportResults(learningGroup.gr);
                    gr_PresetPerformance.reportResults(learningGroup.gr);
            }
            List<String> labelValuesForComparativeAnalysis = new LinkedList<>();
            for (int traceQuantityToUse : new int[]{8, 1}) {
                StringBuilder sb = new StringBuilder();
                Formatter formatter = new Formatter(sb, Locale.US);formatter.format("%1d",traceQuantityToUse);
                labelValuesForComparativeAnalysis.add(sb+"_M");
                labelValuesForComparativeAnalysis.add(sb+"_MC");
                labelValuesForComparativeAnalysis.add(sb+"_S");
            }
            gr_BestStructuralForDifferentPreset.setOrderingOfLabels(labelValuesForComparativeAnalysis);
            gr_BestStructuralForDifferentPreset.reportResults(learningGroup.gr);
        }
    }
}
