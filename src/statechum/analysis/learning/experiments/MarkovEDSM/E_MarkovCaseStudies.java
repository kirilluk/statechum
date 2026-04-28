package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.Pair;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.mutation.DiffExperiments;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.FsmParserDot;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.rpnicore.FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND;
import static statechum.analysis.learning.rpnicore.FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovCaseStudies {
    public static String [] caseStudies = new String[] {"coffeemachine", "coffeemachine - with_reset", "coffeemachine - noresetonerror"};
//            "coffeemachine - with_reset","coffeemachine","OpenSSH-8.8p1","TCP_Linux_Client","tls-1.2-openssl-1.1.1","xraypowercontrol - learnresult6"};


    public static class MarkovLearningBaselineParameters extends MarkovLearningParameters {

        public MarkovLearningBaselineParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample, int argSeed) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample, argSeed);
        }

        @Override
        public String getSubExperimentName() {
            return "casestudies";
        }
    }

    public static class MarkovLearnerRunnerForCaseStudies extends MarkovExperiment.MarkovLearnerRunner {

        public MarkovLearnerRunnerForCaseStudies(MarkovLearningBaselineParameters parameters, ProgressDecorator.LearnerEvaluationConfiguration ev) {
            super(parameters, ev);
        }

        /** Constructs a reference graph and assigns it to member variable <pre>referenceGraph</pre>. This is a separate method to permit overriding by subclasses.
         */
        public void generateReferenceFSM()
        {
            Configuration dotConfig = learnerInitConfiguration.config.copy();dotConfig.setLabelKind(Configuration.LABELKIND.LABEL_STRING);
            String referenceDot;
            try {
                referenceDot = Helper.loadFile(new File(
                        GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_CASESTUDIES)+
                                File.separator+caseStudies[par.sample]+".dot"));
            } catch (IOException e) {
                throw new RuntimeException("Failed to load graph "+e);
            }
            referenceGraph = FsmParserDot.buildLearnerGraph(referenceDot,dotConfig,
                    learnerInitConfiguration.getLabelConverter(), true,true,USE_START0);
            referenceGraph.setName(caseStudies[par.sample]);
        }
    }

    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        int[] learnerExperiment = new int[]{0,1,2,3,4};
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + "results_casestudies.csv"));
        boolean aveOrMax = true;// average divide by the divisor
        int trainingSamplesPerFSM = 40;// these are fixed automata hence we can try many different values to see how inference performs.
        boolean pathsOrSets = true;
        String pathToCaseStudyFiles = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_CASESTUDIES);
        if (null == pathToCaseStudyFiles ||  pathToCaseStudyFiles.isEmpty())
            throw new RuntimeException("Cannot load any case studies: path to case studies is not defined");
        if (!Files.exists(Paths.get(pathToCaseStudyFiles)))
            throw new RuntimeException("Cannot load any case studies: path to case studies does not exist "+pathToCaseStudyFiles);

        List<Pair<Integer,Integer> []> tracesAndLengthsForCaseStudy = new ArrayList<>();

        for (int casestudy=0; casestudy<caseStudies.length; casestudy++) {
            System.out.println("Loading " + caseStudies[casestudy]);

            Configuration dotConfig = learningGroup.eval.config.copy();
            dotConfig.setLabelKind(Configuration.LABELKIND.LABEL_STRING);
            String referenceDot;
            try {
                referenceDot = Helper.loadFile(new File(pathToCaseStudyFiles + File.separator + caseStudies[casestudy] + ".dot"));
            } catch (IOException e) {
                throw new RuntimeException("Failed to load graph " + e);
            }
            LearnerGraph reference = FsmParserDot.buildLearnerGraph(referenceDot, dotConfig,
                    new Transform.InternStringLabel(), true, true, USE_START0);
            int states = reference.getStateNumber();

            Pair<Integer, Integer> [] traces_and_lengths = new Pair[]{new Pair(1, reference.getCache().getAlphabet().size()  * states * states),
                    new Pair(states, reference.getCache().getAlphabet().size() * states), new Pair(states* states, reference.getCache().getAlphabet().size() )};
            tracesAndLengthsForCaseStudy.add(traces_and_lengths);

            for (final int preset : learnerExperiment)
                for (final Pair<Integer, Integer> traces_lengthmult : traces_and_lengths)
                {
                    int traceQuantityToUse = traces_lengthmult.firstElem;
                    for (int trainingSample = 0; trainingSample < trainingSamplesPerFSM; ++trainingSample)
                        for (LearningAlgorithms.ScoringToApply learnerKind :
                                preset == 0 ?// this is the only case where we can apply PTA-based merging algorithms, two other presets handle merging vertices in a connected graph
                                        new LearningAlgorithms.ScoringToApply[]{
                                                LearningAlgorithms.ScoringToApply.SCORING_MARKOV,
                                                LearningAlgorithms.ScoringToApply.SCORING_EDSM_1, LearningAlgorithms.ScoringToApply.SCORING_EDSM_2, LearningAlgorithms.ScoringToApply.SCORING_EDSM_4,
                                                LearningAlgorithms.ScoringToApply.SCORING_PTAK_1, LearningAlgorithms.ScoringToApply.SCORING_PTAK_2,
                                                LearningAlgorithms.ScoringToApply.SCORING_SICCO
                                        } :
                                        new LearningAlgorithms.ScoringToApply[]{
                                                LearningAlgorithms.ScoringToApply.SCORING_MARKOV
                                        })
                            for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? new int[]{2, 3, 4} : new int[]{2})
                                for (double weightOfInconsistencies : learnerKind.isMarkov() ?
                                        new double[]{0.5, 1.0, 2.0, 4.0, 8.0, 16.0}
                                        : new double[]{1.0})
                                    for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 1)} : new Pair[]{new Pair(1, 1), new Pair(1, 2), new Pair(2, 4)}) {
                                        ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                        ev.config = learningGroup.eval.config.copy();
                                        ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                        MarkovLearningBaselineParameters parameters = new MarkovLearningBaselineParameters(learnerKind, states, 0, 0, casestudy, trainingSample, 0);
                                        parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
                                        parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, 0);
                                        parameters.markovParameters.setMarkovParameters(preset, chunkSizeToEvaluate, pathsOrSets, weightOfInconsistencies, aveOrMax, wlen_divisor.secondElem, 0, wlen_divisor.firstElem);
                                        parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                        MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovLearnerRunnerForCaseStudies(parameters, ev);
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
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistencyAverage);// 11
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistencySD);// 12
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistencyAlwaysPositive);// 13
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.fractionOfStatesIdentifiedBySingletons);// 14
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovTransitionPrecision);// 15
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovTransitionRecall);// 16
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovHolePrecision);// 17
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovHoleRecall);// 18
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.comparisonsPerformed);// 19
                    }

                    if (result.parameters.markovParameters.useCentreVertex) {
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.centreCorrect);
                        CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.centrePathNumber);
                    }
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(100. * ConfusionMatrix.divide(sm.referenceGraph.getStateNumber(),sm.referenceGraph.pathroutines.countEdges())));
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.transitionsSampled);
                    CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(data.executionTime / 1000000000.));// execution time is in nanoseconds, we only need seconds.
                    experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
                }

                @Override
                public SGEExperimentResult[] getGraphs()
                {
                    return new SGEExperimentResult[]{resultCSV};
                }

            });

            if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
                for (int casestudy=0; casestudy<caseStudies.length; casestudy++)
                {
                    Pair<Integer, Integer> [] traces_and_lengths = tracesAndLengthsForCaseStudy.get(casestudy);

                    for (final boolean useCentre : new boolean[]{false,true})
                        for (final Pair<Integer, Integer> traces_lengthmult : traces_and_lengths) {
                            String plot_filename_prefix = learningGroup.outPathPrefix + "casestudies_" + caseStudies[casestudy] + "_" + traces_lengthmult.firstElem + "_" +
                                    (useCentre ? "centre" : "no_cnt");

                            Map<String, AtomicInteger> learnerToHowOftenBest = new HashMap<>();
                            final SquareBagPlot gr_StructuralDiffBest = new SquareBagPlot("Structural score, Sicco", "Structural Score, EDSM-Markov learner",
                                    new File(plot_filename_prefix + "_sicco_structuraldiffBest.pdf"), 0, 1, true);
                            final SquareBagPlot gr_BcrDiffBest = new SquareBagPlot("BCR, Sicco", "BCR, EDSM-Markov learner",
                                    new File(plot_filename_prefix + "_sicco_BCRBest.pdf"), 0.5, 1, true);

                            // Now select the best result from all those available
                            for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                                final MarkovExperiment.LearningReport bestLearningResult = new MarkovExperiment.LearningReport();
                                String[] rowValues = rowEntry.getKey().split("[_=]");
                                assert rowValues[0].equals("tQ");
                                assert rowValues[12].equals("sa");
                                if (rowValues[1].equals(Integer.toString(traces_lengthmult.firstElem)) && rowValues[13].equals(Integer.toString(casestudy))) {
                                    getAllValuesFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_MARKOV.toString(), (columnText, Y) -> {
                                        boolean learntOK = obtainValueFromCell(Y, 0).equals("L_OK");
                                        boolean alwaysPositive = Boolean.parseBoolean(obtainValueFromCell(Y, 13));
                                        double bcr = Double.parseDouble(obtainValueFromCell(Y, 1));
                                        double structural = Double.parseDouble(obtainValueFromCell(Y, 2));
                                        long inconsistency = Long.parseLong(obtainValueFromCell(Y, 10));
                                        String[] columnValues = columnText.split("[_=]");
                                        int presetCurrent = Integer.parseInt(columnValues[0].substring(LearningAlgorithms.ScoringToApply.SCORING_MARKOV.toString().length() + 1));
                                        boolean centreCurrent = presetCurrent > 0;

                                        if (learntOK && alwaysPositive && centreCurrent == useCentre)
                                            bestLearningResult.updateIfValueBetter(new MarkovExperiment.LearningReport(bcr, structural, inconsistency, columnText));
                                    });
                                    learnerToHowOftenBest.computeIfAbsent(bestLearningResult.descr, s -> new AtomicInteger(0));
                                    learnerToHowOftenBest.get(bestLearningResult.descr).addAndGet(1);
                                    String Y_Sicco = getValueFromMapGivenRegexp(rowEntry.getValue(), LearningAlgorithms.ScoringToApply.SCORING_SICCO + "-0");
                                    if (Y_Sicco != null) {
                                        gr_StructuralDiffBest.add(Double.parseDouble(obtainValueFromCell(Y_Sicco, 2)), bestLearningResult.structural, null, null);
                                        gr_BcrDiffBest.add(Double.parseDouble(obtainValueFromCell(Y_Sicco, 1)), bestLearningResult.bcr, null, null);
                                    } else
                                        System.out.println("WARNING: missing Sicco-value for " + rowEntry.getKey());
                                }
                            }

                            gr_StructuralDiffBest.reportResults(learningGroup.gr);gr_BcrDiffBest.reportResults(learningGroup.gr);
                            List<String> learners = new ArrayList<>(learnerToHowOftenBest.keySet());
                            learners.sort((o1, o2) ->
                                    learnerToHowOftenBest.get(o2).get() - learnerToHowOftenBest.get(o1).get());
                            for (String l : learners)
                                System.out.println(l + " -> " + learnerToHowOftenBest.get(l).get());
                        }
                }
            }
        }
}
