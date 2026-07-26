package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK;

class FilterCollectionOfResultsForBestPerformingLearner {
    protected int states;
    protected int perStateSquaredDensity100 = -1;
    DrawGraphs.CSVExperimentResult resultCSV = null;
    AtomicBoolean multipleOrderingsOfStates = new AtomicBoolean(false);
    Function<String[],Boolean> selector = elems -> true;
    protected Map<String, AtomicInteger> learnerToHowOftenBest = new HashMap<>(), learnerToHowOftenDefaultOrdering = new HashMap<>();

    /**
     * Given a results obtained by Markov learners using different parameters, uses inconsistency values to identify the best performing learner and report its results.
     *
     * @param states                    number of states to consider
     * @param perStateSquaredDensity100 density to consider. Use a negative value to consider all densities for the provided number of states
     * @param resultCSV                 CSV with results to process
     * @return
     */
    public FilterCollectionOfResultsForBestPerformingLearner(int states, int perStateSquaredDensity100, DrawGraphs.CSVExperimentResult resultCSV) {
    }
    public FilterCollectionOfResultsForBestPerformingLearner(int states, int perStateSquaredDensity100, Function<String[],Boolean> sel, DrawGraphs.CSVExperimentResult resultCSV) {
        this.states = states;
        this.perStateSquaredDensity100 = perStateSquaredDensity100;
        this.resultCSV = resultCSV;
        this.selector = sel;
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
     * @param gr_StructuralDiffBest  where to plot best v.s. VH.
     * @param gr_StructuralDiffDefaultOrdering where to plot best across multiple orderings v.s. default ordering.
     * @return
     */
    public Map<String, AtomicInteger> getResultForBestPerformingMarkovLearner(SquareBagPlot gr_StructuralDiffBest, SquareBagPlot gr_StructuralDiffDefaultOrdering) {
        // Now select the best result from all those available
        for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
            String[] rowValues = rowEntry.getKey().split("[_=]");
            assert rowValues[10].equals("d");
            assert rowValues[6].equals("S");

            if ((perStateSquaredDensity100 < 0 || Double.parseDouble(rowValues[11]) == perStateSquaredDensity100) && Integer.parseInt(rowValues[7]) == states &&
                selector.apply(rowValues)) {
                final MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport(),bestLearningResultForDefaultOrdering = new MarkovExperiment.LearningReport();
                final Map<Integer,MarkovExperiment.LearningReport> resultForChunkLen = new TreeMap<>();
                getAllValuesFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_MARKOV.toString(), (columnText, Y) -> {
                    boolean learntOK = obtainValueFromCell(Y, 0).equals(LEARNING_OK.name);
                    boolean alwaysPositive = Boolean.parseBoolean(obtainValueFromCell(Y, 13));
                    double bcr = Double.parseDouble(obtainValueFromCell(Y, 1));
                    double structural = Double.parseDouble(obtainValueFromCell(Y, 2));
                    long inconsistency = Long.parseLong(obtainValueFromCell(Y, 10));

                    String[] columnValues = columnText.split("[_=]");
                    assert columnValues[9].equals("sh");
                    assert columnValues[1].equals("cl");
                    int chunkLen = Integer.parseInt(columnValues[2]);

                    if (learntOK) {
                        MarkovExperiment.LearningReport currentOutcome = new MarkovExperiment.LearningReport(bcr, structural, inconsistency, alwaysPositive, columnText, Y);
                        if (Integer.parseInt(columnValues[10]) == 0)
                            bestLearningResultForDefaultOrdering.updateIfValueBetter(currentOutcome);
                        else
                            multipleOrderingsOfStates.set(true);
                        bestLearningResult.updateIfValueBetter(currentOutcome);

                        resultForChunkLen.computeIfAbsent(chunkLen, k->new MarkovExperiment.LearningReport()).updateIfValueBetter(currentOutcome);
                    }
                });
                experimentResults.add(bestLearningResult);
                learnerToHowOftenBest.computeIfAbsent(bestLearningResult.columnText, s -> new AtomicInteger(0));
                learnerToHowOftenBest.get(bestLearningResult.columnText).addAndGet(1);
                learnerToHowOftenDefaultOrdering.computeIfAbsent(bestLearningResultForDefaultOrdering.columnText, s -> new AtomicInteger(0));
                learnerToHowOftenDefaultOrdering.get(bestLearningResultForDefaultOrdering.columnText).addAndGet(1);

                for(Map.Entry<Integer,MarkovExperiment.LearningReport> result:resultForChunkLen.entrySet())
                    resultPerChunkLen.computeIfAbsent(result.getKey(), k->new ArrayList<>()).add(result.getValue());

                String Y_VH = getValueFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_VH + "-0");
                if (Y_VH != null) {
                    if (gr_StructuralDiffBest != null)
                        gr_StructuralDiffBest.add(Double.parseDouble(obtainValueFromCell(Y_VH, 2)), bestLearningResult.structural, null, null);
                    if (gr_StructuralDiffDefaultOrdering != null)
                        gr_StructuralDiffDefaultOrdering.add(bestLearningResultForDefaultOrdering.structural, bestLearningResult.structural, null, null);
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
