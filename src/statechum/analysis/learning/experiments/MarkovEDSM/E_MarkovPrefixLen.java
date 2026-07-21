package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Pair;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.constructResultsCollector;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovPrefixLen {
    public static final String description = "prefixlen";

    public static class MarkovLearningPrefixLenParameters extends MarkovLearningParameters {

        public MarkovLearningPrefixLenParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample);
        }

        @Override
        public String getSubExperimentName() {
            return description;
        }
    }


    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        int[] learnerExperiment = new int[]{0};//0,1,2,3
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + description+"-results.csv"),"results.csv");
        boolean aveOrMax = true;// average divide by the divisor
        boolean penaliseMissingPaths = true;
        int alphabetMultiplier = 2;
        boolean pathsOrSets = true;

        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample)
                {
                    for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{learningGroup.getTracesLengthmultBaseline(states)})
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
                                    for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? new int[]{2,3} : new int[]{2})
                                        for (double weightOfInconsistencies : learnerKind.isMarkov() ?
                                                ((chunkSizeToEvaluate <= 3)? new double[]{0.25, 0.5, 1.0, 2.0, 4.0}:new double[]{0.125, 0.25, 0.5})
//                                                new double[]{1.0}
                                                : new double[]{1.0})
                                            for (double inconsistencyOffset : learnerKind.isMarkov() ?
//                                                    new double[]{0, 0.5, 1.0}
                                                    new double[]{0.0}
                                                    : new double[]{0.0})
                                            for (int shuffleSeed : learnerKind.isMarkov() ?
                                                    new int[]{0,1,2,3}
                                                    : new int[]{0})
                                            for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 1)} : new Pair[]{new Pair(1, 1), new Pair(1, 2), new Pair(2, 4)}) {
                                                int wlen = wlen_divisor.firstElem, divisor = wlen_divisor.secondElem;
                                                ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                                ev.config = learningGroup.eval.config.copy();
                                                ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                                MarkovLearningParameters parameters = new MarkovLearningPrefixLenParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
                                                parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);

                                                parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, alphabetMultiplier);
                                                parameters.markovParameters.setMarkovParameters(preset, chunkSizeToEvaluate, pathsOrSets,
                                                        new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies, inconsistencyOffset), penaliseMissingPaths, aveOrMax, divisor, 0, wlen);
                                                parameters.markovParameters.setShuffleSeed(shuffleSeed);
                                                parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                                MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovExperiment.MarkovLearnerRunner(parameters, ev);
                                                learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                                learningGroup.experimentRunner.submitTask(learnerRunner);
                                            }
                    }
                }
            }

        learningGroup.experimentRunner.collectOutcomeOfExperiments(constructResultsCollector(resultCSV));

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
            for (final int preset : learnerExperiment) {
                String presetStr = "-" + preset;
                String experimentName = learningGroup.outPathPrefix + description+"_";
                for (int states : learningGroup.statesToUse) {
                    final RBoxPlot<String> gr_StructuralVsChunkLenWeight = new RBoxPlot<>("Prefix length and inconsistency multiplier", "Structural Score",
                            new File(experimentName + states + "_prefixLenInconsistencyWeight_structural.pdf"));
                    final Map<Integer,RBoxPlot<String>> gr_StructuralVsChunkLenWeightForDensity = new TreeMap();
                    final Map<Integer,RBoxPlot<String>> gr_StructuralWhereDidNotFailVsChunkLenWeightForDensity = new TreeMap();
                    for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                        {// structural score for different values of prefix length and inconsistency multiplier, considering offset
                            RBoxPlot<String> graph = new RBoxPlot<>("Prefix length and inconsistency multiplier", "Structural Score",
                                    new File(experimentName + states + "_" + perStateSquaredDensity100 + "_prefixLenInconsistencyWeight_structural.pdf"));
                            gr_StructuralVsChunkLenWeightForDensity.put(perStateSquaredDensity100, graph);
                            graph.setOtherOptions("las=2");
                        }
                        {// Results above for runs where learning did not fail on L_REDS
                            RBoxPlot<String> graph = new RBoxPlot<>("Prefix length and inconsistency multiplier", "Structural Score",
                                    new File(experimentName + states + "_" + perStateSquaredDensity100 + "_prefixLenInconsistencyWeight_NonFailStructural.pdf"));
                            gr_StructuralWhereDidNotFailVsChunkLenWeightForDensity.put(perStateSquaredDensity100, graph);
                            graph.setOtherOptions("las=2");
                        }

                        gr_StructuralVsChunkLenWeight.setOtherOptions("las=2");
                        for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                            String[] rowValues = rowEntry.getKey().split("[_=]");
                            assert rowValues[10].equals("d");
                            assert rowValues[6].equals("S");
                            if (Double.parseDouble(rowValues[11]) == perStateSquaredDensity100 && Integer.parseInt(rowValues[7]) == states)
                                getAllValuesFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_MARKOV + presetStr, (columnText, Y) -> {
                                    double value = Double.parseDouble(obtainValueFromCell(Y, 2));
                                    String[] elems = columnText.split("[_=]");
                                    assert elems[1].equals("cl");
                                    assert elems[3].equals("wW");
                                    assert elems[5].equals("wO");

                                    boolean learntOK = obtainValueFromCell(Y, 0).equals("L_OK");

                                    String prefixLenAndWeight = Integer.parseInt(elems[2]) - 1 + "_" + elems[4];// + "_" + elems[6];
                                    gr_StructuralVsChunkLenWeight.add(prefixLenAndWeight, value);
                                    gr_StructuralVsChunkLenWeightForDensity.get(Integer.parseInt(rowValues[11])).add(prefixLenAndWeight, value);
                                    if (learntOK)
                                        gr_StructuralWhereDidNotFailVsChunkLenWeightForDensity.get(Integer.parseInt(rowValues[11])).add(prefixLenAndWeight, value);
                                });
                        }

                        gr_StructuralVsChunkLenWeight.reportResults(learningGroup.gr);
                        for (RBoxPlot<String> graph : gr_StructuralVsChunkLenWeightForDensity.values())
                            graph.reportResults(learningGroup.gr);
                        for (RBoxPlot<String> graph : gr_StructuralWhereDidNotFailVsChunkLenWeightForDensity.values())
                            graph.reportResults(learningGroup.gr);
                    }
                }
            }
        }


        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            for (int states : learningGroup.statesToUse)
                for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                    final SquareBagPlot gr_StructuralDiffBest = new SquareBagPlot("Structural Score, VH", "Structural Score, EDSM-Markov",
                            new File(learningGroup.outPathPrefix + description+"_"+states+"_bestprefixlen_and_mult_" + states + "_"+perStateSquaredDensity100+"_VH_structuraldiffBest.pdf"), 0, 1, true);
                    final SquareBagPlot gr_StructuralDiffDefaultOrdering = new SquareBagPlot("Structural score, default order", "Structural Score, best order",
                            new File(learningGroup.outPathPrefix + description+"_"+states+"_bestprefixlen_and_mult_" + states + "_"+perStateSquaredDensity100+"_defaultorder_bestorder.pdf"), 0, 1, true);
                    // Now select the best result from all those available
                    FilterCollectionOfResultsForBestPerformingLearner report = new FilterCollectionOfResultsForBestPerformingLearner(states, perStateSquaredDensity100, resultCSV);
                    report.getResultForBestPerformingMarkovLearner(gr_StructuralDiffBest, gr_StructuralDiffDefaultOrdering);
                    gr_StructuralDiffBest.reportResults(learningGroup.gr);
                    gr_StructuralDiffDefaultOrdering.reportResults(learningGroup.gr);
                    report.reportResults();
                }
        }
    }
}