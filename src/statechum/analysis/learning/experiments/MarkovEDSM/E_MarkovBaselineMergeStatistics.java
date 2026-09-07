package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Pair;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Map;
import java.util.Set;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.RESULT_VALUES.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.spreadsheetAsDouble;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.spreadsheetToBagPlot;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersColumnFromCSV;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovBaselineMergeStatistics {
    public static final String description = "baselinestatistics";

    public static class MarkovLearningBaselineParameters extends MarkovLearningParameters {

        public MarkovLearningBaselineParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample);
        }

        @Override
        public String getSubExperimentName() {
            return description;
        }
    }

    public static CSVExperimentResult runExperiment(LearningExperimentGroupParameters learningGroup) {
        final DatapointsCollection resultCSV = new DatapointsCollection(learningGroup.outPathPrefix, learningGroup.copyToPrefix, learningGroup.moveToPrefix, description, true);
        PreGeneratePTA tasks = new PreGeneratePTA(learningGroup.phase, learningGroup.experimentRunner);
        boolean aveOrMax = true;// average divide by the divisor
        boolean penaliseMissingPaths = true;

        int alphabetMultiplier = 2;
        boolean pathsOrSets = true;

        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample) {
                    for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{learningGroup.getTracesLengthmultBaseline(states)})
                    {
                        int traceQuantityToUse = traces_lengthmult.firstElem;
                        for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample)
                            for (LearningAlgorithms.ScoringToApply learnerKind :
                                    new LearningAlgorithms.ScoringToApply[]{
                                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV,
                                            LearningAlgorithms.ScoringToApply.SCORING_EDSM_1, LearningAlgorithms.ScoringToApply.SCORING_EDSM_2, LearningAlgorithms.ScoringToApply.SCORING_EDSM_4,
                                            LearningAlgorithms.ScoringToApply.SCORING_PTAK_1, LearningAlgorithms.ScoringToApply.SCORING_PTAK_2,
                                            LearningAlgorithms.ScoringToApply.SCORING_HV
                                    })
                            {
                                int chunkSizeToEvaluate = 3;
//                                double weightOfInconsistencies = 1.0;// good for 10 states
                                double weightOfInconsistencies = 0.5;// good for 20 states
                                ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                ev.config = learningGroup.eval.config.copy();
                                ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                MarkovLearningBaselineParameters parameters = new MarkovLearningBaselineParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
                                parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
                                parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, alphabetMultiplier);
                                parameters.markovParameters.setMarkovParameters(0, chunkSizeToEvaluate, pathsOrSets,
                                        new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies, 0), penaliseMissingPaths, aveOrMax, 0, 0, 0);
                                parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(learningGroup.outPathPrefix, parameters, ev);
                                learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                tasks.submitTask(learnerRunner);
                            }
                    }
                }
        }

        tasks.generatePTAAndSubmitTasks();// this will generate PTAs and submit tasks to the runner as needed.
        learningGroup.experimentRunner.collectOutcomeOfExperiments(constructResultsCollector(resultCSV));

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
            Set<RESULT_VALUES> validityOfCells = obtainValidityOfCellValues(resultCSV);checkFullTransitionCoverageAttained(description, resultCSV, validityOfCells);
            for (int states : learningGroup.statesToUse) {
                String experimentNameForAllDensities = learningGroup.outPathPrefix + File.separator + description + "_" + states+"_";
                final RBagPlot gr_TotalMergers_Structural = new RBagPlot("Mergers done", "Structural difference", new File(experimentNameForAllDensities + "totalmergers_structuraldiff.pdf"));
                final DrawGraphs.Correlation correlation_mergers_diff = new DrawGraphs.Correlation(new File(experimentNameForAllDensities + "correlation_mergers_diff.csv"));

                for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states)) {
                    DataSelection source = new DataSelection(resultCSV, states, perStateSquaredDensity100, validityOfCells);

                    spreadsheetToBagPlot(gr_TotalMergers_Structural, source, new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_VALIDMERGERS,
                            new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), E_DIFF, null, null);

                    for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                        MarkovLearningParameters rowValues = parseMarkovParametersRowFromCSV(rowEntry.getKey());

                        if (rowValues.perStateSquaredDensityMultipliedBy100 == perStateSquaredDensity100 && rowValues.states == states)
                            getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), validityOfCells,
                                    (column, columnText, Y) -> {
                                        boolean alwaysPositive = obtainBooleanValueFromCell(Y, E_INCONSISTENCY_ALWAYSPOSITIVE, column);
                                        double value = obtainDoubleValueFromCell(Y, E_DIFF, column);

                                        correlation_mergers_diff.add(
                                                (double)obtainIntValueFromCell(Y,E_VALIDMERGERS,column)+
                                                (double)obtainIntValueFromCell(Y,E_ERR_INVALID_NEARROOT,column)+
                                                (double)obtainIntValueFromCell(Y,E_ERR_INVALID_FARFROMROOT,column)

                                                , value, null, null);
                                    });
                    }

                    for (@SuppressWarnings("rawtypes") RExperimentResult result : new RExperimentResult[]{gr_TotalMergers_Structural
                    })
                        result.reportResults(learningGroup.gr);

                }
                StatisticalTestResult correlation_mergersdiff = correlation_mergers_diff.obtainResultFromR(false);
                if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS){
                    if (!correlation_mergersdiff.valueValid)
                        throw new IllegalArgumentException("Invalid statistic correlation_mergers_diff");
                }
                NumberFormat f_signtest = new DecimalFormat("0.00E00");
                NumberFormat f_corr = new DecimalFormat("0.00");
                System.out.println("States: "+states+
                        " correlation between number of mergers and structural difference: "+f_corr.format(correlation_mergersdiff.statistic)
                );
            }
        }
        return resultCSV;
    }

}

