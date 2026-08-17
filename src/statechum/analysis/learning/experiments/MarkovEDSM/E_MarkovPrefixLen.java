package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Pair;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.RESULT_VALUES.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.obtainDoubleValueFromCell;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK;

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
            for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumberPrefixLen(states)) {
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

        learningGroup.experimentRunner.collectOutcomeOfExperiments(constructResultsCollector(resultCSV));

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
            Set<RESULT_VALUES>  validityOfCells = obtainValidityOfCellValues(resultCSV);
            for (final int preset : learnerExperiment) {
                String presetStr = "-" + preset;
                String experimentName = learningGroup.outPathPrefix + description+"_";
                for (int states : learningGroup.statesToUse) {
                    final RBoxPlot<String> gr_StructuralVsChunkLenWeight = new RBoxPlot<>("Prefix length and inconsistency multiplier", "Structural Score",
                            new File(experimentName + states + "_prefixLenInconsistencyWeight_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_StructuralVsReferenceDensity = new DrawGraphs.RBagPlot("Density of Reference", "Structural Score", new File(learningGroup.outPathPrefix + description+"_" + states + "_density_reference_structural.pdf"));
                    final DrawGraphs.RBagPlot gr_StructuralVsLearntDensity = new DrawGraphs.RBagPlot("Density of Learnt", "Structural Score", new File(learningGroup.outPathPrefix + description+"_" + states + "_density_learnt_structural.pdf"));
                    gr_StructuralVsChunkLenWeight.setupForTwoLineXLabels();
//                    gr_StructuralVsChunkLenWeight.setXLine(4);
//                    gr_StructuralVsChunkLenWeight.setMargins(5,4,0,0);
                    final Map<Integer,RBoxPlot<String>> gr_StructuralVsChunkLenWeightForDensity = new TreeMap();
                    final Map<Integer,RBoxPlot<String>> gr_StructuralWhereDidNotFailVsChunkLenWeightForDensity = new TreeMap();
                    Map<Integer,DrawGraphs.RBagPlot>
                            map_StructuralVsReferenceAccuracyAllDensities = new TreeMap(),
                            map_StructuralVsLearntRelativeInconsistencyAllDensities = new TreeMap(),
                            map_StructuralVsLearntInconsistencyAccuracyAllDensities = new TreeMap();

                    for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumberPrefixLen(states)) {
                        DataSelection source = new DataSelection(resultCSV,states,perStateSquaredDensity100,validityOfCells);
                        final DrawGraphs.RBagPlot gr_StructuralVsInconsistency = new DrawGraphs.RBagPlot("Inconsistency Learnt", "Structural Score", new File(learningGroup.outPathPrefix + description+"_" + states + "_" + perStateSquaredDensity100 + "_inconsistency_structural.pdf"));
                        spreadsheetToBagPlotNoZeroYValues(gr_StructuralVsInconsistency, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_INCONSISTENCY_LEARNT,
                                new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, null, null);

                        {// structural score for different values of prefix length and inconsistency multiplier, considering offset
                            RBoxPlot<String> graph = new RBoxPlot<>("Prefix length and inconsistency multiplier", "Structural Score",
                                    new File(experimentName + states + "_" + perStateSquaredDensity100 + "_prefixLenInconsistencyWeight_structural.pdf"));
                            gr_StructuralVsChunkLenWeightForDensity.put(perStateSquaredDensity100, graph);
                            graph.setupForOneLineXLabels();
//                            graph.setOtherOptions("las=2");
//                            graph.setXLine(4);
//                            graph.setMargins(5,4,0,0);
                        }
                        {// Results above for runs where learning did not fail on L_REDS
                            RBoxPlot<String> graph = new RBoxPlot<>("Prefix length and inconsistency multiplier", "Structural Score",
                                    new File(experimentName + states + "_" + perStateSquaredDensity100 + "_prefixLenInconsistencyWeight_NonFailStructural.pdf"));
                            gr_StructuralWhereDidNotFailVsChunkLenWeightForDensity.put(perStateSquaredDensity100, graph);
                            graph.setupForOneLineXLabels();
//                            graph.setOtherOptions("las=2");
//                            graph.setXLine(4);
//                            graph.setMargins(5,4,0,0);
                        }

                        Map<Integer,DrawGraphs.RBagPlot>
                                map_StructuralVsLearntRelativeInconsistency = new TreeMap(),
                                map_StructuralVsReferenceInconsistencyAccuracy=new TreeMap(),
                                map_StructuralVsLearntInconsistencyAccuracy = new TreeMap(),
                                map_StructuralVsInconsistencyForChunkLen = new TreeMap<>();

                        for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                            MarkovLearningParameters rowValues = parseMarkovParametersRowFromCSV(rowEntry.getKey());
                            if (rowValues.perStateSquaredDensityMultipliedBy100 == perStateSquaredDensity100 && rowValues.states == states)
                                getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), validityOfCells,(column, columnText, Y) -> {
                                    double value = obtainDoubleValueFromCell(Y, E_DIFF,column);
                                    boolean learntOK = obtainStringValueFromCell(Y, RESULT_VALUES.E_SUCCESS, column).equals(LEARNING_OK.name);

                                    String prefixLenAndWeight = column.parameters.chunkLen - 1 + "\n" + column.parameters.weightOfInconsistencies.weight;// + "_" + column.parameters.weightOfInconsistencies.offset;
                                    gr_StructuralVsChunkLenWeight.add(prefixLenAndWeight, value);
                                    gr_StructuralVsChunkLenWeightForDensity.get(rowValues.perStateSquaredDensityMultipliedBy100).add(prefixLenAndWeight, value);
                                    if (learntOK)
                                        gr_StructuralWhereDidNotFailVsChunkLenWeightForDensity.get(rowValues.perStateSquaredDensityMultipliedBy100).add(prefixLenAndWeight, value);

                                });
                        }

                        FilterCollectionOfResultsForBestPerformingLearner report = new FilterCollectionOfResultsForBestPerformingLearner(states,perStateSquaredDensity100,resultCSV,validityOfCells);
                        report.getResultForBestPerformingMarkovLearner(null, null, null, null);
                        for(Map.Entry<Integer, List<MarkovExperiment.LearningReport>> resultEntry:report.getExperimentsResultsPerChunkLen().entrySet()) {
                            int chunkLen = resultEntry.getKey();
                            DrawGraphs.RBagPlot gr_StructuralVsReferenceAccuracyAllDensities = map_StructuralVsReferenceAccuracyAllDensities.
                                    computeIfAbsent(chunkLen, k->
                                            new DrawGraphs.RBagPlot("inconsistency inaccuracy, reference", "Structural Score",
                                                    new File(learningGroup.outPathPrefix + description+"_" + states + "_" + k + "_difference_vs_reference_relativeinconsistency.pdf")));

                            DrawGraphs.RBagPlot gr_StructuralVsReferenceInconsistencyAccuracy = map_StructuralVsReferenceInconsistencyAccuracy.
                                    computeIfAbsent(chunkLen, k->{
                                        DrawGraphs.RBagPlot plot = new DrawGraphs.RBagPlot("Inconsistency inaccuracy, reference", "Structural Score",
                                            new File(learningGroup.outPathPrefix + description+"_" + states + "_" + perStateSquaredDensity100 + "_" + k +"_difference_vs_reference_inconsistencyaccuracy.pdf"));
                                        if (states >= 40) {
                                            plot.setMargins(3, 3.5, 0.2, 0.2);
                                            plot.setYLine(2.5);
                                        }
                                        return plot;
                                    });

                            DrawGraphs.RBagPlot gr_StructuralVsLearntRelativeInconsistency = map_StructuralVsLearntRelativeInconsistency.
                                    computeIfAbsent(chunkLen, k->
                                        new DrawGraphs.RBagPlot("Relative inconsistency", "Structural Score",
                                    new File(learningGroup.outPathPrefix + description+"_" + states + "_" + perStateSquaredDensity100 + "_" + k + "_difference_vs_learnt_relativeinconsistency.pdf")));
                            DrawGraphs.RBagPlot gr_StructuralVsLearntRelativeInconsistencyAllDensities = map_StructuralVsLearntRelativeInconsistencyAllDensities.
                                    computeIfAbsent(chunkLen, k-> {
                                        DrawGraphs.RBagPlot plot =
                                             new DrawGraphs.RBagPlot("Relative inconsistency", "Structural Score",
                                                new File(learningGroup.outPathPrefix + description+"_" + states + "_" + k + "_difference_vs_learnt_relativeinconsistency.pdf"));
                                        if (states >= 40) {
                                            plot.setMargins(3, 3.5, 0.2, 0.2);
                                            plot.setYLine(2.5);
                                        }
                                        return plot;
                                    });

                            DrawGraphs.RBagPlot gr_StructuralVsLearntInconsistencyAccuracy =  map_StructuralVsLearntInconsistencyAccuracy.
                                    computeIfAbsent(chunkLen, k->{
                                        DrawGraphs.RBagPlot plot =
                                                        new DrawGraphs.RBagPlot("Inconsistency inaccuracy, learnt", "Structural Score",
                                            new File(learningGroup.outPathPrefix + description+"_" + states + "_" + perStateSquaredDensity100 + "_" + k +"_difference_vs_learnt_inconsistencyaccuracy.pdf"));
                                        if (states >= 40) {
                                            plot.setMargins(3, 3.5, 0.2, 0.2);
                                            plot.setYLine(2.5);
                                        }
                                        return plot;
                                    });
                            DrawGraphs.RBagPlot gr_StructuralVsLearntInconsistencyAccuracyAllDensities =  map_StructuralVsLearntInconsistencyAccuracyAllDensities.
                                    computeIfAbsent(chunkLen, k->
                                            new DrawGraphs.RBagPlot("Inconsistency inaccuracy, learnt", "Structural Score",
                                    new File(learningGroup.outPathPrefix + description+"_" + states + "_" + k +"_difference_vs_learnt_inconsistencyaccuracy.pdf")));

                            DrawGraphs.RBagPlot gr_StructuralVsInconsistencyPerChunkLen = map_StructuralVsInconsistencyForChunkLen.
                                    computeIfAbsent(chunkLen, k-> new DrawGraphs.RBagPlot("Inconsistency Learnt", "Structural Score",
                                            new File(learningGroup.outPathPrefix + description+"_" + states + "_" + perStateSquaredDensity100 + "_"+chunkLen+"_inconsistency_structural.pdf")));

                            for(MarkovExperiment.LearningReport learningReport:resultEntry.getValue()) {
                                double markovReferenceInconsistencyAccuracy = obtainDoubleValueFromCell(learningReport.Yvalues, E_MARKOV_PREDICTIONACCURACY_REFERENCE,learningReport.column);
                                double markovPredictionAccuracyLearnt = obtainDoubleValueFromCell(learningReport.Yvalues, E_MARKOV_PREDICTIONACCURACY_LEARNT,learningReport.column);
                                double markovLearntRelativeInconsistency = obtainDoubleValueFromCell(learningReport.Yvalues, E_RELATIVEINCONSISTENCY_LEARNT,learningReport.column);
                                if (markovLearntRelativeInconsistency >= 5)
                                    markovLearntRelativeInconsistency = 5;
                                double value = obtainDoubleValueFromCell(learningReport.Yvalues, E_DIFF, learningReport.column);

                                boolean learntOK = obtainStringValueFromCell(learningReport.Yvalues, E_SUCCESS,learningReport.column).equals(LEARNING_OK.name);
                                if (learntOK) {
                                    gr_StructuralVsLearntInconsistencyAccuracy.add(markovPredictionAccuracyLearnt,
                                            value, null, null);
                                    gr_StructuralVsLearntInconsistencyAccuracyAllDensities.add(markovPredictionAccuracyLearnt,
                                            value, null, null);

                                    gr_StructuralVsLearntRelativeInconsistency.add(markovLearntRelativeInconsistency,
                                            value, null, null);
                                    gr_StructuralVsLearntRelativeInconsistencyAllDensities.add(markovLearntRelativeInconsistency,
                                            value, null, null);
                                }
                                gr_StructuralVsReferenceInconsistencyAccuracy.add(markovReferenceInconsistencyAccuracy,
                                        value, null, null);
                                gr_StructuralVsReferenceAccuracyAllDensities.add(markovReferenceInconsistencyAccuracy,
                                        value, null, null);
                                gr_StructuralVsInconsistencyPerChunkLen.add(Double.parseDouble(obtainValueFromCell(learningReport.Yvalues, 10)),learningReport.structural);

                                gr_StructuralVsReferenceDensity.add(Double.parseDouble(obtainValueFromCell(learningReport.Yvalues, 24)),value);
                                double cappedObtainedDensity = Double.parseDouble(obtainValueFromCell(learningReport.Yvalues, 25));
                                if (cappedObtainedDensity >= 1)
                                    cappedObtainedDensity = 1;
                                gr_StructuralVsLearntDensity.add(cappedObtainedDensity,value);
                            }
                        }

                        gr_StructuralVsChunkLenWeight.reportResults(learningGroup.gr);
                        for(DrawGraphs.RBagPlot gr_StructuralVsLearntRelativeInconsistency:map_StructuralVsLearntRelativeInconsistency.values())
                            gr_StructuralVsLearntRelativeInconsistency.reportResults(learningGroup.gr);
                        for(DrawGraphs.RBagPlot gr_StructuralVsReferenceInconsistencyAccuracy:map_StructuralVsReferenceInconsistencyAccuracy.values())
                            gr_StructuralVsReferenceInconsistencyAccuracy.reportResults(learningGroup.gr);
                        for(DrawGraphs.RBagPlot gr_StructuralVsLearntInconsistencyAccuracy:map_StructuralVsLearntInconsistencyAccuracy.values())
                            gr_StructuralVsLearntInconsistencyAccuracy.reportResults(learningGroup.gr);
                        for(DrawGraphs.RBagPlot gr_StructuralVsInconsistencyPerChunkLen:map_StructuralVsInconsistencyForChunkLen.values())
                            gr_StructuralVsInconsistencyPerChunkLen.reportResults(learningGroup.gr);
                        gr_StructuralVsInconsistency.reportResults(learningGroup.gr);

                        for(RBoxPlot<String> plot: gr_StructuralWhereDidNotFailVsChunkLenWeightForDensity.values())
                            plot.reportResults(learningGroup.gr);
                        for(RBoxPlot<String> plot: gr_StructuralVsChunkLenWeightForDensity.values())
                            plot.reportResults(learningGroup.gr);

//                        for (RBoxPlot<String> graph : gr_StructuralVsChunkLenWeightForDensity.values())
//                            graph.reportResults(learningGroup.gr);
//                        for (RBoxPlot<String> graph : gr_StructuralWhereDidNotFailVsChunkLenWeightForDensity.values())
//                            graph.reportResults(learningGroup.gr);
                    }

                    for(DrawGraphs.RBagPlot gr_StructuralVsReferenceAccuracyAllDensities:map_StructuralVsReferenceAccuracyAllDensities.values())
                        gr_StructuralVsReferenceAccuracyAllDensities.reportResults(learningGroup.gr);
                    for(DrawGraphs.RBagPlot gr_StructuralVsLearntRelativeInconsistencyAllDensities:map_StructuralVsLearntRelativeInconsistencyAllDensities.values())
                        gr_StructuralVsLearntRelativeInconsistencyAllDensities.reportResults(learningGroup.gr);
                    for(DrawGraphs.RBagPlot gr_StructuralVsLearntInconsistencyAccuracyAllDensities:map_StructuralVsLearntInconsistencyAccuracyAllDensities.values())
                        gr_StructuralVsLearntInconsistencyAccuracyAllDensities.reportResults(learningGroup.gr);
                    gr_StructuralVsReferenceDensity.reportResults(learningGroup.gr);
                    gr_StructuralVsLearntDensity.reportResults(learningGroup.gr);
                }
            }
        }

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            Set<RESULT_VALUES> validityOfCells = obtainValidityOfCellValues(resultCSV);
            for (int states : learningGroup.statesToUse)
                for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumberPrefixLen(states)) {
                    final SquareBagPlot gr_StructuralDiffBest = new SquareBagPlot("Structural Score, VH", "Structural Score, EDSM-Markov",
                            new File(learningGroup.outPathPrefix + description+"_"+states+"_bestprefixlen_and_mult_" + states + "_"+perStateSquaredDensity100+"_VH_structuraldiffBest.pdf"), 0, 1, true);
                    final SquareBagPlot gr_StructuralDiffDefaultOrdering = new SquareBagPlot("Structural score, default order", "Structural Score, best order",
                            new File(learningGroup.outPathPrefix + description+"_"+states+"_bestprefixlen_and_mult_" + states + "_"+perStateSquaredDensity100+"_defaultorder_bestorder.pdf"), 0, 1, true);
                    // Now select the best result from all those available
                    FilterCollectionOfResultsForBestPerformingLearner report = new FilterCollectionOfResultsForBestPerformingLearner(states, perStateSquaredDensity100, resultCSV,validityOfCells);
                    report.getResultForBestPerformingMarkovLearner(gr_StructuralDiffBest, gr_StructuralDiffDefaultOrdering, null, null);
//                    System.out.println("Values for "+states+" and "+perStateSquaredDensity100+" : "+report.getExperimentResults().size());
                    gr_StructuralDiffBest.reportResults(learningGroup.gr);
                    gr_StructuralDiffDefaultOrdering.reportResults(learningGroup.gr);
//                    report.reportResults();
                }
        }
    }
}