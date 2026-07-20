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
                                    for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? new int[]{2,3,4} : new int[]{2})
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

                                    gr_StructuralVsChunkLenWeight.add(Integer.parseInt(elems[2]) - 1 + "_" + elems[4] + "_" + elems[6], value);
                                    if (null == gr_StructuralVsChunkLenWeightForDensity.get(Integer.parseInt(rowValues[11])))
                                        System.out.println(rowValues[11]);
                                    gr_StructuralVsChunkLenWeightForDensity.get(Integer.parseInt(rowValues[11])).add(Integer.parseInt(elems[2]) - 1 + "_" + elems[4] + "_" + elems[6], value);
                                    if (learntOK)
                                        gr_StructuralWhereDidNotFailVsChunkLenWeightForDensity.get(Integer.parseInt(rowValues[11])).add(Integer.parseInt(elems[2]) - 1 + "_" + elems[4] + "_" + elems[6], value);
//                            System.out.println(Integer.parseInt(elems[2]) - 1 + "_" + elems[4]+"_"+elems[6]);
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