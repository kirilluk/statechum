package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Pair;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.util.*;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovTraceNum {
    public static final String description = "traceNum";

    public static class MarkovTraceNumParameters extends MarkovLearningParameters {

        public MarkovTraceNumParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample) {
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
        int [] traceQuantityValues = new int[] { 1,2,4,8,32,64 };
        double alphabetMultiplier = 2;
        final int chunkSizeToEvaluate = 3;
        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states))
                for(int traceQuantityToUseV:traceQuantityValues)  {
                    int traceLenMult= learningGroup.getTracesLengthmultBaseline(states).secondElem;
                    int traceQuantityToUse = traceQuantityToUseV*learningGroup.getScalingFactor(states);
		            for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample)
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
                                        for (double weightOfInconsistencies : learnerKind.isMarkov() ? new double[]{0.5, 1.0, 2.0} : new double[]{1.0})
                                            for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 1)} : new Pair[]{new Pair(1, 1), new Pair(1, 2), new Pair(2, 4)}) {
                                                int wlen = wlen_divisor.firstElem, divisor = wlen_divisor.secondElem;
                                                ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                                ev.config = learningGroup.eval.config.copy();
                                                ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                                MarkovTraceNumParameters parameters = new MarkovTraceNumParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
                                                parameters.setTraceLengthMultiplier(traceLenMult);
                                                parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, alphabetMultiplier);
                                                parameters.markovParameters.setMarkovParameters(preset, chunkSizeToEvaluate, pathsOrSets,
                                                        new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies, 0), penaliseMissingPaths, aveOrMax, divisor, 0, wlen);
                                                parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                                MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovExperiment.MarkovLearnerRunner(parameters, ev);
                                                learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                                learningGroup.experimentRunner.submitTask(learnerRunner);
                                            }
		}


        learningGroup.experimentRunner.collectOutcomeOfExperiments(constructResultsCollector(resultCSV));


        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            for (int states : learningGroup.statesToUse) {
                final RBoxPlot<String> gr_BestStructuralForTraceNumber = new RBoxPlot<>("Trace number", "Structural Score, EDSM-Markov learner",
                        new File(learningGroup.outPathPrefix + description+"_"+states + "_tracenum_structural.pdf"));
                final Map<Integer, SquareBagPlot> gr_StructuralDiffBestMap = new TreeMap<>();
                Map<Integer, FilterCollectionOfResultsForBestPerformingLearner> learnerToHowOftenBestForAllTraceLength = new TreeMap<>();

                for (final int traceQuantityToUseV : traceQuantityValues) {
                    int traceQuantityToUse = traceQuantityToUseV*learningGroup.getScalingFactor(states);
                    // Now select the best result from all those available
                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                        MarkovLearningParameters rowValues = parseMarkovParametersRowFromCSV(rowEntry.getKey());
                        if (rowValues.traceQuantity == traceQuantityToUse) {
                            final MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport();
                            gr_StructuralDiffBestMap.computeIfAbsent(traceQuantityToUse, aDouble ->
                                    new SquareBagPlot("Structural score, VH", "Structural Score, EDSM-Markov learner",
                                            new File(learningGroup.outPathPrefix + description+"_"+states+"_tracenum_num=" + traceQuantityToUse + "_VH_structuraldiffBest.pdf"), 0, 1, true));

                            FilterCollectionOfResultsForBestPerformingLearner report = new FilterCollectionOfResultsForBestPerformingLearner(states, -1,
                                    rowHeader -> rowHeader.traceQuantity == traceQuantityToUse,
                                    columnParse -> columnParse.parameters.chunkLen == chunkSizeToEvaluate,
                                    resultCSV);
                            report.getResultForBestPerformingMarkovLearner(gr_StructuralDiffBestMap.get(traceQuantityToUse), null,
                                    (pair) -> {
                                        double markov = pair.firstElem, vh_score = pair.secondElem;
                                        StringBuilder sb = new StringBuilder();
                                        Formatter formatter = new Formatter(sb, Locale.US);
                                        formatter.format("%3d", traceQuantityToUse);
                                        gr_BestStructuralForTraceNumber.add(sb + "_M", markov);
                                        gr_BestStructuralForTraceNumber.add(sb + "_VH", vh_score);
                                    });
                            learnerToHowOftenBestForAllTraceLength.computeIfAbsent(traceQuantityToUse,aInteger -> report);
                        }

                    }
                }

                for (final int traceQuantityToUseV : traceQuantityValues) {
                    int traceQuantityToUse = traceQuantityToUseV*learningGroup.getScalingFactor(states);
                    System.out.println("trace quantity: " + traceQuantityToUse);

                    gr_StructuralDiffBestMap.get(traceQuantityToUse).reportResults(learningGroup.gr);
                    learnerToHowOftenBestForAllTraceLength.get(traceQuantityToUse).reportResults();
                }
                gr_BestStructuralForTraceNumber.reportResults(learningGroup.gr);
            }
        }
    }
}
