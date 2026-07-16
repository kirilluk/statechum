package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Pair;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.observers.ProgressDecorator;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static java.lang.Math.max;
import static statechum.analysis.learning.DrawGraphs.getValueFromMapGivenRegexp;
import static statechum.analysis.learning.DrawGraphs.obtainValueFromCell;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.directoryExperimentResult;

public class MarkovComparison {

    public static void main(String []args) {
        SGE_ExperimentRunner.PhaseEnum curPhase = null;
        List<DrawGraphs.CSVExperimentResult> twoExperiments = new ArrayList<>();
        String [] experimentsToCompare = new String[]{"without_all_paths-markov","with_all_paths-markov"};
        for(String namePrefix: experimentsToCompare) {
            String outDir = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_EXPERIMENTRESULTS) + File.separator + namePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
            UASExperiment.mkDir(outDir);

            MarkovExperiment.LearningExperimentGroupParameters learningGroup = new MarkovExperiment.LearningExperimentGroupParameters();

            learningGroup.outPathPrefix = outDir + File.separator;
            learningGroup.eval = UASExperiment.constructLearnerInitConfiguration();
            learningGroup.eval.config.setTransitionMatrixImplType(Configuration.STATETREE.STATETREE_LINKEDHASH);// small automata hence no need for array STATETREE.STATETREE_ARRAY);
            //STATETREE_ARRAY);
            learningGroup.eval.config.setLearnerScoreMode(Configuration.ScoreMode.GENERAL_NOFULLMERGE);
            learningGroup.eval.config.setTimeOut(3600000L * 16L);// timeout for tasks, in milliseconds, equivalent to 16hrs runtime for an old Xeon 5670 @ 2.93Ghz, modern E5/i7 are 3x faster.
            learningGroup.eval.config.setOverride_usePTAMerging(false);

            SGE_ExperimentRunner.configureCPUFreqNormalisation();

            learningGroup.experimentRunner = new SGE_ExperimentRunner.RunSubExperiment<>(ExperimentRunner.getCpuNumber(), learningGroup.outPathPrefix + directoryExperimentResult, args);
            learningGroup.phase = learningGroup.experimentRunner.getPhase();
            if (null == curPhase)
                curPhase = learningGroup.phase;
            twoExperiments.add(runExperiment(learningGroup));
        }

        if (curPhase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || curPhase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
            DrawGraphs gr = new DrawGraphs();
            String pathToResult = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_EXPERIMENTRESULTS) + File.separator;
            final DrawGraphs.SquareBagPlot gr_Comparison = new DrawGraphs.SquareBagPlot(experimentsToCompare[0], experimentsToCompare[1],
                    new File(pathToResult+"comparison.pdf"),0, 1, true);
            final DrawGraphs.SquareBagPlot gr_BestVsWithAllPaths = new DrawGraphs.SquareBagPlot(experimentsToCompare[1], "Best between the two",
                    new File(pathToResult+"best_vs_all_paths.pdf"),0, 1, true);
            final DrawGraphs.SquareBagPlot gr_BestAgainstSicco = new DrawGraphs.SquareBagPlot("Sicco", "Best between the two",
                    new File(pathToResult+"best_vs_sicco.pdf"),0, 1, true);
            final DrawGraphs.WilcoxonPairedTest Wilcoxon_test_best = new DrawGraphs.WilcoxonPairedTest(new File(pathToResult + "Wilcoxon_t_best.csv"));
            final DrawGraphs.WilcoxonPairedTest Wilcoxon_test_all_paths = new DrawGraphs.WilcoxonPairedTest(new File(pathToResult + "Wilcoxon_t_all_paths.csv"));
            for (Map.Entry<String, Map<String, String>> rowEntryA : twoExperiments.get(0).rowColumnText.entrySet()) {
                Map<String, String> entryB = twoExperiments.get(1).rowColumnText.get(rowEntryA.getKey());
                String cellsA = getValueFromMapGivenRegexp(rowEntryA.getValue(), LearningAlgorithms.ScoringToApply.SCORING_MARKOV.toString());
                double valueA = Double.parseDouble(obtainValueFromCell(cellsA, 2));
                String cellsB = getValueFromMapGivenRegexp(entryB, LearningAlgorithms.ScoringToApply.SCORING_MARKOV.toString());
                double valueB = Double.parseDouble(obtainValueFromCell(cellsB, 2));

                String Y_Sicco = getValueFromMapGivenRegexp(rowEntryA.getValue(), LearningAlgorithms.ScoringToApply.SCORING_VH + "-0");

                gr_Comparison.add(valueA, valueB);
                Wilcoxon_test_all_paths.add(valueA, valueB);


                final MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport();
                for(String cellY:new String[]{cellsA,cellsB}) {
                    boolean learntOK = obtainValueFromCell(cellY, 0).equals("L_OK");
                    double bcr = Double.parseDouble(obtainValueFromCell(cellY, 1));
                    double structural = Double.parseDouble(obtainValueFromCell(cellY, 2));
                    long inconsistency = Long.parseLong(obtainValueFromCell(cellY, 10));

                    if (learntOK)
                        bestLearningResult.updateIfValueBetter(new MarkovExperiment.LearningReport(bcr, structural, inconsistency, true, null));
                }
                gr_BestVsWithAllPaths.add(valueB, bestLearningResult.structural);
                Wilcoxon_test_best.add(valueB, bestLearningResult.structural);

                gr_BestAgainstSicco.add(Double.parseDouble(obtainValueFromCell(Y_Sicco, 2)),bestLearningResult.structural);
            }

            gr_Comparison.reportResults(gr);gr_BestVsWithAllPaths.reportResults(gr);
            Wilcoxon_test_best.reportResults(gr);Wilcoxon_test_all_paths.reportResults(gr);
            gr_BestAgainstSicco.reportResults(gr);
        }

        DrawGraphs.end();
    }

    public static DrawGraphs.CSVExperimentResult runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        final DrawGraphs.CSVExperimentResult resultCSV = new DrawGraphs.CSVExperimentResult(new File(learningGroup.outPathPrefix + "results.csv"));
        boolean aveOrMax = true;// average divide by the divisor
        boolean penaliseMissingPaths = true;
        int alphabetMultiplier = 2;
        boolean pathsOrSets = true;
        int [] densities = new int[]{0, 20, 30};
        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : densities) {
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample) {
                    for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{new Pair(states, 2*states )})
                    {
                        int traceQuantityToUse = traces_lengthmult.firstElem;
                        for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample)
                            for (LearningAlgorithms.ScoringToApply learnerKind :
                                    new LearningAlgorithms.ScoringToApply[]{
                                            LearningAlgorithms.ScoringToApply.SCORING_MARKOV,
                                            LearningAlgorithms.ScoringToApply.SCORING_EDSM_1, LearningAlgorithms.ScoringToApply.SCORING_EDSM_2, LearningAlgorithms.ScoringToApply.SCORING_EDSM_4,
                                            LearningAlgorithms.ScoringToApply.SCORING_PTAK_1, LearningAlgorithms.ScoringToApply.SCORING_PTAK_2,
                                            LearningAlgorithms.ScoringToApply.SCORING_VH
                                    })
                            {
                                int chunkSizeToEvaluate = 3;
                                double weightOfInconsistencies = 1.0;
                                ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                ev.config = learningGroup.eval.config.copy();
                                ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                E_MarkovBaselineLearn.MarkovLearningBaselineParameters parameters = new E_MarkovBaselineLearn.MarkovLearningBaselineParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
                                parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
                                parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, alphabetMultiplier);
                                parameters.markovParameters.setMarkovParameters(0, chunkSizeToEvaluate, pathsOrSets,
                                        new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies, 0), penaliseMissingPaths, aveOrMax, 0, 0, 0);
                                parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovExperiment.MarkovLearnerRunner(parameters, ev) {
                                    @Override
                                    public ExperimentResult<MarkovLearningParameters> runexperiment() {
                                        throw new  UnsupportedOperationException("Here we intend to collate results only, not to run an experiment");
                                    }
                                };
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
                throw new  UnsupportedOperationException("Here we intend to collate results only, not to run an experiment");
            }

            @Override
            public DrawGraphs.SGEExperimentResult[] getGraphs() {
                return new DrawGraphs.SGEExperimentResult[]{resultCSV};
            }

        });

        return resultCSV;
    }
}
