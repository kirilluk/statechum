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
import java.util.concurrent.atomic.AtomicInteger;

import static statechum.analysis.learning.DrawGraphs.*;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovTraceMult {

    public static class MarkovAlphabetLearningParameters extends MarkovLearningParameters {

        public MarkovAlphabetLearningParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample, int argSeed) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample, argSeed);
        }

        @Override
        public String getSubExperimentName() {
            return "tracelenMult";
        }
    }

    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        int[] learnerExperiment = new int[]{0};//0,1,2,3
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + "results.csv"));
        final int statesMax = learningGroup.statesToUse[learningGroup.statesToUse.length-1];// reflects the size of the largest FSM that will be generated.
        boolean aveOrMax = true;// average divide by the divisor
        boolean pathsOrSets = true;
        int [] traceLenMultValues = new int[] { 4,16,32, 64, 128 };
        double alphabetMultiplier = 2;
        int seedForFSM = 0;
        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : new int[]{0, 30}) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample, ++seedForFSM)
                    for (int traceLenMult:traceLenMultValues) {
                        int scalingFactor = states*learningGroup.stateScale/learningGroup.statesToUse[0];
                        int traceQuantityToUse = 8*scalingFactor;
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
                                    for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? new int[]{3, 4} : new int[]{2})
                                        for (double weightOfInconsistencies : learnerKind.isMarkov() ? new double[]{0.5, 1.0, 2.0} : new double[]{1.0})
                                            for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 1)} : new Pair[]{new Pair(1, 1), new Pair(1, 2), new Pair(2, 4)}) {
                                                int wlen = wlen_divisor.firstElem, divisor = wlen_divisor.secondElem;
                                                ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                                ev.config = learningGroup.eval.config.copy();
                                                ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                                MarkovAlphabetLearningParameters parameters = new MarkovAlphabetLearningParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample, seedForFSM);
                                                parameters.setTraceLengthMultiplier(traceLenMult);
                                                parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, alphabetMultiplier);
                                                parameters.markovParameters.setMarkovParameters(preset, chunkSizeToEvaluate, pathsOrSets, weightOfInconsistencies, aveOrMax, divisor, 0, wlen);
                                                parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                                MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovExperiment.MarkovLearnerRunner(parameters, ev);
                                                learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                                learningGroup.experimentRunner.submitTask(learnerRunner);
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
                CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(data.differenceBCR.getValue());// 1
                CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(data.differenceStructural.getValue());// 2
                CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(data.invalidMergersNearRoot);// 3
                CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(data.missedMergersNearRoot); // 4
                CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(data.invalidMergersFarFromRoot);// 5
                CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(data.missedMergersFarFromRoot); // 6
                CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(data.validMergers); // 7
                CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(data.nrOfstates.getValue());// 8
                CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(sm.inconsistencyReference);// 9
                CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(data.inconsistency);// 10

                if (result.parameters.learnerToUse.isMarkov()) {
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(data.inconsistencyAverage);// 11
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(data.inconsistencySD);// 12
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(data.inconsistencyAlwaysPositive);// 13
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(sm.fractionOfStatesIdentifiedBySingletons);// 14
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(sm.markovTransitionPrecision);// 15
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(sm.markovTransitionRecall);// 16
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(sm.markovHolePrecision);// 17
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(sm.markovHoleRecall);// 18
                    CSVExperimentResult.addSeparator(csvLine);
                    csvLine.append(sm.comparisonsPerformed);// 19
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
            final RBoxPlot<String> gr_BestStructuralForLengthMultiplier = new RBoxPlot<>("Trace length multiplier", "Structural Score, EDSM-Markov learner", new File(learningGroup.outPathPrefix + statesMax + "_lengthmult_structural.pdf"));
            final Map<Integer,SquareBagPlot> gr_StructuralDiffBestMap = new TreeMap<>();
            Map<Integer,Map<String, AtomicInteger>> learnerToHowOftenBestForAllMultipliers = new TreeMap<>();

            for(final int traceLenMult:traceLenMultValues) {
                // Now select the best result from all those available
                for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                    String[] elems = rowEntry.getKey().split("[_=]");
                    assert elems[20].equals("tM");
                    if (Double.parseDouble(elems[21]) == traceLenMult) {
                        final MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport();
                        Map<String, AtomicInteger> learnerToHowOftenBest = learnerToHowOftenBestForAllMultipliers.computeIfAbsent(traceLenMult,aDouble -> new HashMap<>());
                        gr_StructuralDiffBestMap.computeIfAbsent(traceLenMult,aDouble ->
                            new SquareBagPlot("Structural score, Sicco", "Structural Score, EDSM-Markov learner",
                                new File(learningGroup.outPathPrefix + "tracemult_tracelen=" + traceLenMult + " " + statesMax + "_sicco_structuraldiffBest.pdf"), 0, 1, true));


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
                        if (Y_Sicco != null) {
                            double sicco_score = Double.parseDouble(obtainValueFromCell(Y_Sicco, 2));
                            gr_StructuralDiffBestMap.get(traceLenMult).add(sicco_score, bestLearningResult.structural, null, null);
                            StringBuilder sb = new StringBuilder();
                            Formatter formatter = new Formatter(sb, Locale.US);formatter.format("%3d",traceLenMult);
                            gr_BestStructuralForLengthMultiplier.add(sb+"_M",bestLearningResult.structural);
                            gr_BestStructuralForLengthMultiplier.add(sb+"_S",sicco_score);
                        }
                        else
                            System.out.println("WARNING: missing Sicco-value for " + rowEntry.getKey());
                    }

                }
            }

            for(final int traceLenMult:traceLenMultValues) {
                System.out.println("traceLenMult Multiplier: " + traceLenMult);

                gr_StructuralDiffBestMap.get(traceLenMult).reportResults(learningGroup.gr);
                Map<String, AtomicInteger> learnerToHowOftenBest = learnerToHowOftenBestForAllMultipliers.get(traceLenMult);
                List<String> learners = new ArrayList<>(learnerToHowOftenBest.keySet());
                learners.sort((o1, o2) ->
                        learnerToHowOftenBest.get(o2).get() - learnerToHowOftenBest.get(o1).get());
                for (String l : learners)
                    System.out.println(l + " -> " + learnerToHowOftenBest.get(l).get());
            }
            gr_BestStructuralForLengthMultiplier.reportResults(learningGroup.gr);
        }
    }
}
