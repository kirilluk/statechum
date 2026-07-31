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
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersColumnFromCSV;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK;

class FilterCollectionOfResultsForBestPerformingLearner {
    protected int states;
    protected int perStateSquaredDensity100 = -1;
    DrawGraphs.CSVExperimentResult resultCSV = null;
    AtomicBoolean multipleOrderingsOfStates = new AtomicBoolean(false);
    Function<MarkovLearningParameters,Boolean> selectorRow = elems -> true;
    Function<MarkovLearningParameters.ColumnParseOutcome, Boolean> selectorCol = elems -> true;
    protected Map<String, AtomicInteger> learnerToHowOftenBest = new HashMap<>(), learnerToHowOftenDefaultOrdering = new HashMap<>();

    /**
     * Given a results obtained by Markov learners using different parameters, uses inconsistency values to identify the best performing learner and report its results.
     *
     * @param states                    number of states to consider
     * @param perStateSquaredDensity100 density to consider. Use a negative value to consider all densities for the provided number of states
     * @param resultCSV                 CSV with results to process
     */
    public FilterCollectionOfResultsForBestPerformingLearner(int states, int perStateSquaredDensity100, DrawGraphs.CSVExperimentResult resultCSV) {
        this(states,perStateSquaredDensity100,(array) -> true, null, resultCSV);
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
                                                             Function<MarkovLearningParameters.ColumnParseOutcome, Boolean> selCol, CSVExperimentResult resultCSV) {
        this.states = states;
        this.perStateSquaredDensity100 = perStateSquaredDensity100;
        this.resultCSV = resultCSV;
        this.selectorRow = selRow;this.selectorCol = selCol;
    }
    protected List<MarkovExperiment.LearningReport> experimentResults = new ArrayList<>();
    public List<MarkovExperiment.LearningReport> getExperimentResults() {
        return experimentResults;
    }

    protected Map<Integer,List<MarkovExperiment.LearningReport>> resultPerChunkLen = new TreeMap<>();
    public Map<Integer,List<MarkovExperiment.LearningReport>> getExperimentsResultsPerChunkLen() {
        return resultPerChunkLen;
    }
    /**
     * Given a results obtained by Markov learners using different parameters, uses inconsistency values to identify the best performing learner and report its results.
     *
     * @param gr_StructuralDiffBest            where to plot best v.s. VH.
     * @param gr_StructuralDiffDefaultOrdering where to plot best across multiple orderings v.s. default ordering.
     * @param markov_vh_score_handler
     */
    public Map<String, AtomicInteger> getResultForBestPerformingMarkovLearner(SquareBagPlot gr_StructuralDiffBest, SquareBagPlot gr_StructuralDiffDefaultOrdering,
                                                                              Consumer<Pair<Double, Double>> markov_vh_score_handler) {
        // Now select the best result from all those available
        for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
            MarkovLearningParameters rowValues = parseMarkovParametersRowFromCSV(rowEntry.getKey());

            if ((perStateSquaredDensity100 < 0 || rowValues.perStateSquaredDensityMultipliedBy100 == perStateSquaredDensity100) && rowValues.states == states &&
                selectorRow.apply(rowValues)) {
                final MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport(),bestLearningResultForDefaultOrdering = new MarkovExperiment.LearningReport();
                final Map<Integer,MarkovExperiment.LearningReport> resultForChunkLen = new TreeMap<>();
                getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV),
                        (column, columnText, Y) -> {
                            boolean learntOK = obtainStringValueFromCell(Y, MarkovExperiment.RESULT_VALUES.E_SUCCESS, column).equals(LEARNING_OK.name);
                            boolean alwaysPositive = obtainBooleanValueFromCell(Y, E_INCONSISTENCY_ALWAYSPOSITIVE,column);
                            double bcr = obtainDoubleValueFromCell(Y, E_BCR,column);
                            double structural = obtainDoubleValueFromCell(Y, E_DIFF,column);
                            long inconsistency = obtainLongValueFromCell(Y, E_INCONSISTENCY_LEARNT,column);

                            MarkovLearningParameters.ColumnParseOutcome columnValues=parseMarkovParametersColumnFromCSV(columnText);

                            if (learntOK && selectorCol.apply(column)) {
                                MarkovExperiment.LearningReport currentOutcome = new MarkovExperiment.LearningReport(bcr, structural, inconsistency, alwaysPositive, columnText, Y, column);
                                if (columnValues.parameters.seedToShuffleSurroundingStates == 0)
                                    bestLearningResultForDefaultOrdering.updateIfValueBetter(currentOutcome);
                                else
                                    multipleOrderingsOfStates.set(true);
                                bestLearningResult.updateIfValueBetter(currentOutcome);

                                resultForChunkLen.computeIfAbsent(columnValues.parameters.chunkLen, k->new MarkovExperiment.LearningReport()).updateIfValueBetter(currentOutcome);
                            }
                        });
                experimentResults.add(bestLearningResult);
                learnerToHowOftenBest.computeIfAbsent(bestLearningResult.columnText, s -> new AtomicInteger(0));
                learnerToHowOftenBest.get(bestLearningResult.columnText).addAndGet(1);
                learnerToHowOftenDefaultOrdering.computeIfAbsent(bestLearningResultForDefaultOrdering.columnText, s -> new AtomicInteger(0));
                learnerToHowOftenDefaultOrdering.get(bestLearningResultForDefaultOrdering.columnText).addAndGet(1);

                for(Map.Entry<Integer,MarkovExperiment.LearningReport> result:resultForChunkLen.entrySet())
                    resultPerChunkLen.computeIfAbsent(result.getKey(), k->new ArrayList<>()).add(result.getValue());

                ColumnAndValue Y_VH = getValueFromMapGivenSelector(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_VH));
                if (Y_VH != null) {
                    double vh_score = obtainDoubleValueFromCell(Y_VH.value, E_DIFF,Y_VH.column);
                    if (gr_StructuralDiffBest != null)
                        gr_StructuralDiffBest.add(vh_score, bestLearningResult.structural, null, null);
                    if (gr_StructuralDiffDefaultOrdering != null)
                        gr_StructuralDiffDefaultOrdering.add(bestLearningResultForDefaultOrdering.structural, bestLearningResult.structural, null, null);

                    if (markov_vh_score_handler != null)
                        markov_vh_score_handler.accept(new Pair<>(bestLearningResult.structural,vh_score));
                }
                else
                    System.out.println("WARNING: missing VH-value for " + rowEntry.getKey());

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
