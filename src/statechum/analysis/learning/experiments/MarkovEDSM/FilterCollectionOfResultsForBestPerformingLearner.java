package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import static statechum.analysis.learning.DrawGraphs.*;

class FilterCollectionOfResultsForBestPerformingLearner {
    protected int states;
    protected int perStateSquaredDensity100 = -1;
    DrawGraphs.CSVExperimentResult resultCSV = null;

    protected Map<String, AtomicInteger> learnerToHowOftenBest = new HashMap<>();

    /**
     * Given a results obtained by Markov learners using different parameters, uses inconsistency values to identify the best performing learner and report its results.
     *
     * @param states                    number of states to consider
     * @param perStateSquaredDensity100 density to consider. Use a negative value to consider all densities for the provided number of states
     * @param resultCSV                 CSV with results to process
     * @return
     */
    public FilterCollectionOfResultsForBestPerformingLearner(int states, int perStateSquaredDensity100, DrawGraphs.CSVExperimentResult resultCSV) {
        this.states = states;
        this.perStateSquaredDensity100 = perStateSquaredDensity100;
        this.resultCSV = resultCSV;
    }

    /**
     * Given a results obtained by Markov learners using different parameters, uses inconsistency values to identify the best performing learner and report its results.
     *
     * @param gr_StructuralDiffBest where to plot results.
     * @return
     */
    public Map<String, AtomicInteger> getResultForBestPerformingMarkovLearner(DrawGraphs.SquareBagPlot gr_StructuralDiffBest) {
        // Now select the best result from all those available
        for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
            final MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport();
            String[] rowValues = rowEntry.getKey().split("[_=]");
            assert rowValues[10].equals("d");
            assert rowValues[6].equals("S");

            if ((perStateSquaredDensity100 < 0 || Double.parseDouble(rowValues[11]) == perStateSquaredDensity100) && Integer.parseInt(rowValues[7]) == states) {
                getAllValuesFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_MARKOV.toString(), (columnText, Y) -> {
                    boolean learntOK = obtainValueFromCell(Y, 0).equals("L_OK");
                    boolean alwaysPositive = Boolean.parseBoolean(obtainValueFromCell(Y, 13));
                    double bcr = Double.parseDouble(obtainValueFromCell(Y, 1));
                    double structural = Double.parseDouble(obtainValueFromCell(Y, 2));
                    long inconsistency = Long.parseLong(obtainValueFromCell(Y, 10));

                    if (learntOK)
                        bestLearningResult.updateIfValueBetter(new MarkovExperiment.LearningReport(bcr, structural, inconsistency, alwaysPositive, columnText));
                });
                learnerToHowOftenBest.computeIfAbsent(bestLearningResult.descr, s -> new AtomicInteger(0));
                learnerToHowOftenBest.get(bestLearningResult.descr).addAndGet(1);
                String Y_VH = getValueFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_VH + "-0");
                if (Y_VH != null)
                    gr_StructuralDiffBest.add(Double.parseDouble(obtainValueFromCell(Y_VH, 2)), bestLearningResult.structural, null, null);
                else
                    System.out.println("WARNING: missing VH-value for " + rowEntry.getKey());

            }
        }
        return learnerToHowOftenBest;
    }

    public void reportResults() {
        List<String> learners = new ArrayList<>(learnerToHowOftenBest.keySet());
        learners.sort((o1, o2) ->
                learnerToHowOftenBest.get(o2).get() - learnerToHowOftenBest.get(o1).get());
        System.out.println("States: " + states + (perStateSquaredDensity100 >= 0 ? " density: " + perStateSquaredDensity100 : ""));
        for (String l : learners)
            System.out.println(l + " -> " + learnerToHowOftenBest.get(l).get());
    }
}
