package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Pair;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.RESULT_VALUES.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK;

class FilterCollectionOfResultsForBestPerformingLearner {
    protected int states;
    final protected int perStateSquaredDensity100;
    final DrawGraphs.CSVExperimentResult resultCSV;
    final AtomicBoolean multipleOrderingsOfStates = new AtomicBoolean(false);
    final Function<MarkovLearningParameters,Boolean> selectorRow;
    final Function<MarkovLearningParameters.ColumnParseOutcome, Boolean> selectorCol;
    protected Map<String, AtomicInteger> learnerToHowOftenBest = new HashMap<>(), learnerToHowOftenDefaultOrdering = new HashMap<>();
    final Set<MarkovExperiment.RESULT_VALUES> invalidCellValues;

    /**
     * Given a results obtained by Markov learners using different parameters, uses inconsistency values to identify the best performing learner and report its results.
     *
     * @param states                    number of states to consider
     * @param perStateSquaredDensity100 density to consider. Use a negative value to consider all densities for the provided number of states
     * @param resultCSV                 CSV with results to process
     */
    public FilterCollectionOfResultsForBestPerformingLearner(int states, int perStateSquaredDensity100, DrawGraphs.CSVExperimentResult resultCSV,Set<MarkovExperiment.RESULT_VALUES> invalidCellValues) {
        this(states,perStateSquaredDensity100,(array) -> true, null, resultCSV, invalidCellValues);
    }

    /**
     * Given a results obtained by Markov learners using different parameters, uses inconsistency values to identify the best performing learner and report its results.
     *
     * @param states                    number of states to consider
     * @param perStateSquaredDensity100 density to consider. Use a negative value to consider all densities for the provided number of states
     * @param selRow                    selector to use to pick relevant rows
     * @param selCol                    selector to use to pick relevant columns
     * @param resultCSV                 CSV with results to process
     */
    public FilterCollectionOfResultsForBestPerformingLearner(int states, int perStateSquaredDensity100,
                                                             Function<MarkovLearningParameters,Boolean> selRow,
                                                             Function<MarkovLearningParameters.ColumnParseOutcome,
                                                                     Boolean> selCol, CSVExperimentResult resultCSV,
                                                             Set<MarkovExperiment.RESULT_VALUES> invalidCellValues) {
        this.states = states;
        this.perStateSquaredDensity100 = perStateSquaredDensity100;
        this.resultCSV = resultCSV;
        this.selectorRow = selRow == null? elems-> true : selRow;
        this.selectorCol = selCol == null? elems-> true : selCol;
        this.invalidCellValues = invalidCellValues;
    }
    protected List<MarkovExperiment.LearningReport> experimentResults = new ArrayList<>();
    public List<MarkovExperiment.LearningReport> getExperimentResults() {
        return experimentResults;
    }

    protected Map<Integer,List<MarkovExperiment.LearningReport>> learntokResultPerChunkLen = new TreeMap<>();
    public Map<Integer,List<MarkovExperiment.LearningReport>> getLearntOkExperimentsResultsPerChunkLen() {
        return learntokResultPerChunkLen;
    }

    public static class BestVsFixed {
        double scoreFixed = 0;
        int timeUsedFixed = 0;
        int timeUsedDefaultOrderingBest = 0, timeUsedBest = 0;
        int timeUsedConstChlenDefaultOrderingBest = 0, timeUsedConstChlenBest = 0;
        int inconsistencyFixed = 0;

        MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport(),bestLearningResultForDefaultOrdering = new MarkovExperiment.LearningReport();
        MarkovExperiment.LearningReport bestLearningConstChlenResult = new MarkovExperiment.LearningReport(),bestLearningConstChlenResultForDefaultOrdering = new MarkovExperiment.LearningReport();
    }

    public static class FixedPrefixLengthAndWeight {
        final public int chunkLen;
        final public double weight;

        public final Map<String,BestVsFixed> experimentResults = new TreeMap<>();

        public FixedPrefixLengthAndWeight(int chunkLen, double weight) {
            this.chunkLen = chunkLen;
            this.weight = weight;
        }
    }

    protected FixedPrefixLengthAndWeight fixedPrefixLengthAndWeight = null;

    public void setFixedPrefixLengthAndWeight(FixedPrefixLengthAndWeight fixedPrefixLengthAndWeight) {
        this.fixedPrefixLengthAndWeight = fixedPrefixLengthAndWeight;
    }
    /**
     * Given a results obtained by Markov learners using different parameters, uses inconsistency values to identify the best performing learner and report its results.
     *
     * @param gr_StructuralDiffBest            where to plot best v.s. HV.
     * @param gr_StructuralDiffDefaultOrdering where to plot best across multiple orderings v.s. default ordering.
     * @param markov_hv_diff_score_handler called with a pair of Diff scores for Markov v.s. HV
     * @param markov_hv_bcr_score_handler called with a pair of BCR scores for Markov v.s. HV
     */
    public Map<String, AtomicInteger> getResultForBestPerformingMarkovLearner(SquareBagPlot gr_StructuralDiffBest, SquareBagPlot gr_StructuralDiffDefaultOrdering,
                                                                              Consumer<Pair<Double, Double>> markov_hv_diff_score_handler,
                                                                              Consumer<Pair<Double, Double>> markov_hv_bcr_score_handler) {
        // Now select the best result from all those available
        for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
            MarkovLearningParameters rowValues = parseMarkovParametersRowFromCSV(rowEntry.getKey());

            if (
                (perStateSquaredDensity100 < 0 || rowValues.perStateSquaredDensityMultipliedBy100 == perStateSquaredDensity100) &&
                (states < 0 || rowValues.states == states) &&
                selectorRow.apply(rowValues))
            {
                final MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport(),bestLearningResultForDefaultOrdering = new MarkovExperiment.LearningReport();
                final Map<Integer,MarkovExperiment.LearningReport> learntOkResultForChunkLen = new TreeMap<>();
                final BestVsFixed bestVsFixed = (fixedPrefixLengthAndWeight != null)?
                        (fixedPrefixLengthAndWeight.experimentResults.computeIfAbsent(rowEntry.getKey(), k -> new BestVsFixed())):null;

                getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV),invalidCellValues,
                (column, columnText, Y) -> {
                    boolean learntOK = obtainStringValueFromCell(Y, MarkovExperiment.RESULT_VALUES.E_SUCCESS, column).equals(LEARNING_OK.name);
                    boolean alwaysPositive = obtainBooleanValueFromCell(Y, E_INCONSISTENCY_ALWAYSPOSITIVE,column);
                    double bcr = obtainDoubleValueFromCell(Y, E_BCR,column);
                    double structural = obtainDoubleValueFromCell(Y, E_DIFF,column);
                    long inconsistency = obtainLongValueFromCell(Y, E_INCONSISTENCY_LEARNT,column);
                    if (fixedPrefixLengthAndWeight != null) {
                        // Here we compare a fixed chunkLen/weight against the best value, making sure to also report the time spent
                        if (column.parameters.chunkLen == fixedPrefixLengthAndWeight.chunkLen &&
                                column.parameters.weightOfInconsistencies.weight == fixedPrefixLengthAndWeight.weight &&
                                column.parameters.seedToShuffleSurroundingStates == 0) {// default ordering and specified chunkLen and weight.
                            bestVsFixed.scoreFixed = structural;
                            bestVsFixed.timeUsedFixed = obtainIntValueFromCell(Y, E_RUNTIME, column);
                            bestVsFixed.inconsistencyFixed = obtainIntValueFromCell(Y, E_INCONSISTENCY_LEARNT, column);
                        }
                    }

                    // Update best value, fixed and lower chunkLen
                    if (bestVsFixed != null && selectorCol.apply(column) && (column.parameters.chunkLen < fixedPrefixLengthAndWeight.chunkLen ||
                            (column.parameters.chunkLen == fixedPrefixLengthAndWeight.chunkLen && column.parameters.weightOfInconsistencies.weight <= fixedPrefixLengthAndWeight.weight))) {// here we deliberately try to avoid depending on whether learning was successful

                        // Update runtime
                        bestVsFixed.timeUsedBest += obtainIntValueFromCell(Y, E_RUNTIME, column);
                        if (column.parameters.seedToShuffleSurroundingStates == 0)
                            bestVsFixed.timeUsedDefaultOrderingBest += obtainIntValueFromCell(Y, E_RUNTIME, column);

                        if (learntOK) {
                            // Successfully learnt, update values

                            // Here we use inconsistency computed with a reference to a specific value of chunklen, permitting comparison
                            // of results learnt across different values of chunkLen
                            long evaluationValueOfInconsistency = inconsistency;
                            if (column.parameters.chunkLen < fixedPrefixLengthAndWeight.chunkLen)
                                evaluationValueOfInconsistency = (long)obtainDoubleValueFromCell(Y, E_INCONSISTENCY_CONSTCHUNKLEN, column);
                            MarkovExperiment.LearningReport learningOutcome = new MarkovExperiment.LearningReport(bcr, structural,
                                    evaluationValueOfInconsistency, alwaysPositive, columnText, Y, column);
                            if (column.parameters.seedToShuffleSurroundingStates == 0)
                                bestVsFixed.bestLearningResultForDefaultOrdering.updateIfValueBetter(learningOutcome);
                            bestVsFixed.bestLearningResult.updateIfValueBetter(learningOutcome);
                        }

                    }

                    // Update best value, fixed chunkLen
                    if (bestVsFixed != null && selectorCol.apply(column) && column.parameters.chunkLen == fixedPrefixLengthAndWeight.chunkLen
                            && column.parameters.weightOfInconsistencies.weight <= fixedPrefixLengthAndWeight.weight) {// here we deliberately try to avoid depending on whether learning was successful

                        // Update runtime
                        bestVsFixed.timeUsedConstChlenBest += obtainIntValueFromCell(Y, E_RUNTIME, column);
                        if (column.parameters.seedToShuffleSurroundingStates == 0)
                            bestVsFixed.timeUsedConstChlenDefaultOrderingBest += obtainIntValueFromCell(Y, E_RUNTIME, column);

                        if (learntOK) {
                            // Successfully learnt, update values
                            MarkovExperiment.LearningReport learningOutcome = new MarkovExperiment.LearningReport(bcr, structural,
                                    inconsistency, alwaysPositive, columnText, Y, column);
                            if (column.parameters.seedToShuffleSurroundingStates == 0)
                                bestVsFixed.bestLearningConstChlenResultForDefaultOrdering.updateIfValueBetter(learningOutcome);
                            bestVsFixed.bestLearningConstChlenResult.updateIfValueBetter(learningOutcome);
                        }

                    }

                    if (selectorCol.apply(column)) {
                        MarkovExperiment.LearningReport currentOutcome = new MarkovExperiment.LearningReport(bcr, structural, inconsistency, alwaysPositive, columnText, Y, column);
                        if (column.parameters.seedToShuffleSurroundingStates == 0)
                            bestLearningResultForDefaultOrdering.updateIfValueBetterIfSuccessfulAndRecordSeen(learntOK,currentOutcome);
                        else
                            multipleOrderingsOfStates.set(true);
                        bestLearningResult.updateIfValueBetterIfSuccessfulAndRecordSeen(learntOK,currentOutcome);

                        if (learntOK) // results obtained from learntOkResultForChunkLen rely on values only computed on successful runs hence only report those for successful runs.
                            learntOkResultForChunkLen.computeIfAbsent(column.parameters.chunkLen, k -> new MarkovExperiment.LearningReport()).updateIfValueBetterIfSuccessfulAndRecordSeen(learntOK,currentOutcome);
                    }
                });
                if (bestLearningResult.valueSeen()) {// if any result was obtained as opposed to everything either missing or eliminated by filters
//                    if (bestVsFixed != null) {
//                        if (bestVsFixed.bestLearningResultForDefaultOrdering.structural < bestVsFixed.scoreFixed &&
//                                bestVsFixed.bestLearningResultForDefaultOrdering.inconsistency < bestVsFixed.inconsistencyFixed)
//                            System.out.println("[A]Row: "+rowEntry.getKey()+", inconsistency "+
//                                            bestVsFixed.bestLearningResultForDefaultOrdering.inconsistency+" < "+ bestVsFixed.inconsistencyFixed +
//                                            " , diff score: " + bestVsFixed.bestLearningResultForDefaultOrdering.structural+" < "+ bestVsFixed.scoreFixed +
//                                    " chunk len : " + bestVsFixed.bestLearningResultForDefaultOrdering.column.parameters.chunkLen + " chunkLen fixed: "+fixedPrefixLengthAndWeight.chunkLen +
//                                    " weight chosen: "+bestVsFixed.bestLearningResultForDefaultOrdering.column.parameters.weightOfInconsistencies.weight + " weight fixed: "+fixedPrefixLengthAndWeight.weight
//                                    );
//                        if (bestVsFixed.bestLearningConstChlenResultForDefaultOrdering.structural < bestVsFixed.scoreFixed &&
//                                bestVsFixed.bestLearningConstChlenResultForDefaultOrdering.inconsistency < bestVsFixed.inconsistencyFixed)
//                            System.out.println("[B]Row: "+rowEntry.getKey()+", inconsistency "+
//                                            bestVsFixed.bestLearningConstChlenResultForDefaultOrdering.inconsistency+" < "+ bestVsFixed.inconsistencyFixed +
//                                            " , diff score: " + bestVsFixed.bestLearningConstChlenResultForDefaultOrdering.structural+" < "+ bestVsFixed.scoreFixed +
//                                    " chunk len : " + bestVsFixed.bestLearningConstChlenResultForDefaultOrdering.column.parameters.chunkLen + " chunkLen fixed: "+fixedPrefixLengthAndWeight.chunkLen +
//                                    " weight chosen: "+bestVsFixed.bestLearningConstChlenResultForDefaultOrdering.column.parameters.weightOfInconsistencies.weight + " weight fixed: "+fixedPrefixLengthAndWeight.weight
//                                    );
//                    }

                    experimentResults.add(bestLearningResult);
                    learnerToHowOftenBest.computeIfAbsent(bestLearningResult.columnText, s -> new AtomicInteger(0));
                    learnerToHowOftenBest.get(bestLearningResult.columnText).addAndGet(1);
                    learnerToHowOftenDefaultOrdering.computeIfAbsent(bestLearningResultForDefaultOrdering.columnText, s -> new AtomicInteger(0));
                    learnerToHowOftenDefaultOrdering.get(bestLearningResultForDefaultOrdering.columnText).addAndGet(1);

                    for (Map.Entry<Integer, MarkovExperiment.LearningReport> result : learntOkResultForChunkLen.entrySet())
                        learntokResultPerChunkLen.computeIfAbsent(result.getKey(), k -> new ArrayList<>()).add(result.getValue());

                    ColumnAndValue Y_HV = getValueFromMapGivenSelector(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_HV), invalidCellValues);
                    if (Y_HV != null) {
                        double hv_score = obtainDoubleValueFromCell(Y_HV.value, E_DIFF, Y_HV.column);
                        if (gr_StructuralDiffBest != null)
                            gr_StructuralDiffBest.add(hv_score, bestLearningResult.structural, null, null);
                        if (gr_StructuralDiffDefaultOrdering != null)
                            gr_StructuralDiffDefaultOrdering.add(bestLearningResultForDefaultOrdering.structural, bestLearningResult.structural, null, null);

                        if (markov_hv_diff_score_handler != null)
                            markov_hv_diff_score_handler.accept(new Pair<>(bestLearningResult.structural, hv_score));
                        if (markov_hv_bcr_score_handler != null)
                            markov_hv_bcr_score_handler.accept(new Pair<>(bestLearningResult.bcr, obtainDoubleValueFromCell(Y_HV.value, E_BCR, Y_HV.column)));
                    } else
                        System.out.println("WARNING: missing HV-value for " + rowEntry.getKey());
                }

            }
        }
        return learnerToHowOftenBest;
    }

    public void reportResults() {
        List<String> learnersBest = new ArrayList<>(learnerToHowOftenBest.keySet()),learnersDefaultOrdering = new ArrayList<>(learnerToHowOftenDefaultOrdering.keySet());
        learnersBest.sort((o1, o2) ->
                learnerToHowOftenBest.get(o2).get() - learnerToHowOftenBest.get(o1).get());
        learnersDefaultOrdering.sort((o1, o2) ->
                learnerToHowOftenDefaultOrdering.get(o2).get() - learnerToHowOftenDefaultOrdering.get(o1).get());
        System.out.println("States: " + states + (perStateSquaredDensity100 >= 0 ? " density: " + perStateSquaredDensity100 : ""));
        if (!learnerToHowOftenDefaultOrdering.isEmpty() && multipleOrderingsOfStates.get())
            System.out.println("Best results across all orders:");
        for (String l : learnersBest)
            System.out.println(l + " -> " + learnerToHowOftenBest.get(l).get());
        if (!learnerToHowOftenDefaultOrdering.isEmpty() && multipleOrderingsOfStates.get()) {
            System.out.println("Default ordering best results :");
            for (String l : learnersDefaultOrdering)
                System.out.println(l + " -> " + learnerToHowOftenDefaultOrdering.get(l).get());
        }
    }
}
