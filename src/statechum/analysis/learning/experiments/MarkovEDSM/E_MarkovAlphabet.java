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
import java.util.*;

import static statechum.analysis.learning.DrawGraphs.*;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovAlphabet {
    public static final String description = "alphabet";

    public static class MarkovAlphabetLearningParameters extends MarkovLearningParameters {

        public MarkovAlphabetLearningParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample);
        }

        @Override
        public String getSubExperimentName() {
            return description;
        }
    }

    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        int[] learnerExperiment = new int[]{0};//0,1,2,3
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + description+"-results.csv"), "results.csv");
        boolean aveOrMax = true;// average divide by the divisor
        boolean penaliseMissingPaths = true;
        boolean pathsOrSets = true;
        double [] alphabetMultValues = new double [] {0.5,1, 2, 4};
        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample)
                    for(final double alphabetMultiplier:alphabetMultValues) {
                        for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{learningGroup.getTracesLengthmultBaseline(states)}) {
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
                                        for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? new int[]{3, 4} : new int[]{2})
                                            for (double weightOfInconsistencies : learnerKind.isMarkov() ? new double[]{0.5, 1.0, 2.0} : new double[]{1.0})
                                                for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 1)} : new Pair[]{new Pair(1, 1), new Pair(1, 2), new Pair(2, 4)}) {
                                                    int wlen = wlen_divisor.firstElem, divisor = wlen_divisor.secondElem;
                                                    ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                                    ev.config = learningGroup.eval.config.copy();
                                                    ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                                    MarkovAlphabetLearningParameters parameters = new MarkovAlphabetLearningParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
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
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.relativeInconsistencyForReferenceGraph);// 19
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.relativeInconsistency);// 20
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.comparisonsPerformed);// 21
                    }

                    if (result.parameters.markovParameters.useCentreVertex) {
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.centreCorrect);
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.centrePathNumber);
                    }
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.referenceGraph.pathroutines.computeAlphabet().size());
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(100. * ConfusionMatrix.divide(sm.referenceGraph.pathroutines.countEdges(),sm.referenceGraph.getStateNumber()*sm.referenceGraph.getStateNumber())));
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.transitionsSampled);
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(data.executionTime / 1000000000.));// execution time is in nanoseconds, we only need seconds.
                    experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
                }

                @Override
                public SGEExperimentResult[] getGraphs() {

                    return new SGEExperimentResult[]{resultCSV};
                }

            });
        }

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            for (int states : learningGroup.statesToUse) {
                final RBoxPlot<String> gr_BestStructuralForAlphabet = new RBoxPlot<>("Alphabet multiplier", "Structural Score, EDSM-Markov learner",
                        new File(learningGroup.outPathPrefix + description + "_" + states + "_alphabetmult_structural.pdf"));
                final Map<Double, SquareBagPlot> gr_StructuralDiffBestMap = new TreeMap<>();
                Map<Double, FilterCollectionOfResultsForBestPerformingLearner> learnerToHowOftenBestForAllMultipliers = new TreeMap<>();

                for (final double alphabetMultiplier : alphabetMultValues) {
                    // Now select the best result from all those available
                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                        String[] elems = rowEntry.getKey().split("[_=]");
                        assert elems[6].equals("aMM");
                        if (Double.parseDouble(elems[7]) == alphabetMultiplier) {
                            final MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport();
                            gr_StructuralDiffBestMap.computeIfAbsent(alphabetMultiplier, aDouble ->
                                    new SquareBagPlot("Structural score, VH", "Structural Score, EDSM-Markov learner",
                                            new File(learningGroup.outPathPrefix + description+"_"+states+"alphabet_alphabetmult=" + alphabetMultiplier + "_VH_structuraldiffBest.pdf"), 0, 1, true));

                            FilterCollectionOfResultsForBestPerformingLearner report = new FilterCollectionOfResultsForBestPerformingLearner(states, -1, resultCSV);
                            report.getResultForBestPerformingMarkovLearner(gr_StructuralDiffBestMap.get(alphabetMultiplier), null);
                            learnerToHowOftenBestForAllMultipliers.computeIfAbsent(alphabetMultiplier, aDouble -> report);

                            String Y_VH = getValueFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_VH + "-0");
                            if (Y_VH != null) {
                                double vh_score = Double.parseDouble(obtainValueFromCell(Y_VH, 2));
                                gr_StructuralDiffBestMap.get(alphabetMultiplier).add(vh_score, bestLearningResult.structural, null, null);
                                gr_BestStructuralForAlphabet.add(alphabetMultiplier + "_M", bestLearningResult.structural);
                                gr_BestStructuralForAlphabet.add(alphabetMultiplier + "_S", vh_score);
                            } else
                                System.out.println("WARNING: missing VH-value for " + rowEntry.getKey());
                        }

                    }
                }

                for (final double alphabetMultiplier : alphabetMultValues) {
                    System.out.println("Alphabet Multiplier: " + alphabetMultiplier);

                    gr_StructuralDiffBestMap.get(alphabetMultiplier).reportResults(learningGroup.gr);
                    learnerToHowOftenBestForAllMultipliers.get(alphabetMultiplier).reportResults();
                }
                gr_BestStructuralForAlphabet.reportResults(learningGroup.gr);
            }
        }
    }
}
