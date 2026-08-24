package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Configuration;
import statechum.Pair;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovClassifierLG;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import static statechum.analysis.learning.DrawGraphs.obtainValueFromCell;
import static statechum.analysis.learning.experiments.MarkovEDSM.E_MarkovCentre.MarkovCentreLearningParameters.description;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.getValueFromMapGivenSelector;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;

public class E_MarkovCentre {

    public static class MarkovCentreLearningParameters extends MarkovLearningParameters {

        public static final String description = "centre";

        public MarkovCentreLearningParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample);
        }

        @Override
        public String[] headerValuesForEachCell() {
            return new String[]{"centreCorrect", "centerpaths", "inconsistency", "inconsistency_with_practice", "Time"};
        }

        @Override
        public String getSubExperimentName() {
            return description;
        }
    }

    public static final int inconsistencyClamp = 50;

    public static class MarkovCentreIdentification extends MarkovExperiment.MarkovLearnerRunner {

        public MarkovCentreIdentification(String outDir, MarkovLearningParameters parameters, ProgressDecorator.LearnerEvaluationConfiguration cnf) {
            super(outDir, parameters, cnf);
        }

        @Override
        public ExperimentResult<MarkovLearningParameters> runexperiment() throws Exception {
            generateReferenceFSM();
            saveGraph(nameReference, referenceGraph);

            ExperimentResult<MarkovLearningParameters> outcome = new ExperimentResult<>(par);
            learnerInitConfiguration.testSet = LearningAlgorithms.buildEvaluationSet(referenceGraph);

            LearnerGraph pta = constructPTA();
            final MarkovModel m = new MarkovModel(par.markovParameters.chunkLen, true, true, true, false);
            new MarkovClassifierLG(m, pta, null).updateMarkov(false);// construct Markov chain if asked for.
            pta.clearColours();
            assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";

            final Configuration deepCopy = pta.config.copy();
            deepCopy.setLearnerCloneGraph(true);
            LearnerGraph ptaCopy = new LearnerGraph(deepCopy);
            LearnerGraph.copyGraphs(pta, ptaCopy);

            final MarkovClassifier.ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();

            PerformFirstMerge firstMerge = new PerformFirstMerge();
            firstMerge.ptaToUseForInference = pta;
            if (par.markovParameters.useCentreVertex) {
                saveGraph(namePTABEFORECENTRE, pta);
                // This replaces firstMerge.ptaToUseForInference with a graph built by merging around the most-connected vertex
                firstMerge.buildFirstGraph(pta, referenceGraph, par.markovParameters, m, checker);
                if (par.usePrintf) {
                    LearnerGraphND inverseOfPtaAfterInitialMerge = MarkovClassifier.computeInverseGraph(firstMerge.ptaToUseForInference);
                    System.out.println("Centre vertex: " + firstMerge.vertexWithMostTransitions + " number of transitions: " +
                            WaveBlueFringe.countTransitions(firstMerge.ptaToUseForInference,
                                    inverseOfPtaAfterInitialMerge, firstMerge.vertexWithMostTransitions));
                }
            }
            PairQualityLearner.SampleData dataSample = new PairQualityLearner.SampleData(null, null);
            dataSample.centreCorrect = firstMerge.correctCentre;
            dataSample.centrePathNumber = firstMerge.centrePathNumber;
            dataSample.actualLearner = new PairQualityLearner.ScoresForGraph();
            dataSample.actualLearner.inconsistency = MarkovClassifier.computeInconsistency(firstMerge.ptaToUseForInference, null, m, checker, false);
            outcome.samples.add(dataSample);

            MarkovExperiment.EDSM_MarkovLearner markovLearner;
            switch (par.learnerToUse) {
                case SCORING_MARKOV:
                    markovLearner = new MarkovExperiment.EDSM_MarkovLearner(learnerInitConfiguration, pta, 0,
                            par.markovParameters, Configuration.ScoreMode.GENERAL_NOFULLMERGE, null);
                    markovLearner.setMarkov(m);
                    markovLearner.setChecker(checker);
                    break;
                case SCORING_MARKOV_1:
                    markovLearner = new MarkovExperiment.EDSM_MarkovLearner(learnerInitConfiguration, pta, 1,
                            par.markovParameters, Configuration.ScoreMode.GENERAL_NOFULLMERGE, null);
                    markovLearner.setMarkov(m);
                    markovLearner.setChecker(checker);
                    break;
                case SCORING_MARKOV_2:
                    markovLearner = new MarkovExperiment.EDSM_MarkovLearner(learnerInitConfiguration, pta, 2,
                            par.markovParameters, Configuration.ScoreMode.GENERAL_NOFULLMERGE, null);
                    markovLearner.setMarkov(m);
                    markovLearner.setChecker(checker);
                    break;
                default:
                    throw new IllegalArgumentException("Invalid learner selected: " + par.learnerToUse);
            }

            PairQualityLearner.SampleData dataSampleAccountingForTestLearn = new PairQualityLearner.SampleData(null, null);
            dataSampleAccountingForTestLearn.centreCorrect = firstMerge.correctCentre;
            dataSampleAccountingForTestLearn.centrePathNumber = firstMerge.centrePathNumber;
            dataSampleAccountingForTestLearn.actualLearner = new PairQualityLearner.ScoresForGraph();
            dataSampleAccountingForTestLearn.actualLearner.inconsistency = dataSample.actualLearner.inconsistency;
            outcome.samples.add(dataSampleAccountingForTestLearn);

            LearnerGraph learntGraph = markovLearner.learnMachine(new LinkedList<>(), new LinkedList<>());
            if (AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK == learntGraph.getLearningAbortedReason()) {
                if (MarkovClassifier.computeInconsistency(learntGraph, null, m, checker, false) < dataSampleAccountingForTestLearn.actualLearner.inconsistency) {
                    dataSampleAccountingForTestLearn.actualLearner.inconsistency = inconsistencyClamp;// if a direct learn produced a lower number, set inconsistency to max
                    dataSampleAccountingForTestLearn.centreCorrect = false;// force centre detection to 'fail'
                }
            }
            return outcome;
        }
    }


    static class CentreSelectionResults {
        final DrawGraphs.RBoxPlot<String> gr_NumberOfCentreCorrect;
        final DrawGraphs.RBoxPlot<String> gr_PercentageOfCentreCorrect;
        final DrawGraphs.RBoxPlot<String> gr_InconsistenciesForCentres;
        final DrawGraphs.RBoxPlot<String> gr_CorrectVsInconsistency;
        final DrawGraphs.RBoxPlot<String> gr_CorrectVsInconsistencyWithPracticeLearn;

        final MarkovExperiment.LearningExperimentGroupParameters group;
        final Map<String, AtomicInteger> count = new TreeMap<>();
        /* total number of values, both right and wrong */
        final Map<String, AtomicInteger> total = new TreeMap<>();

        public CentreSelectionResults(MarkovExperiment.LearningExperimentGroupParameters learningGroup, int states, int traceNum) {
            group = learningGroup;
            String prefix = learningGroup.outPathPrefix + File.separator + description + "_" + states + "_" + traceNum + "_";
            gr_NumberOfCentreCorrect = new DrawGraphs.RBoxPlot<>("Centre Selection", "Number of correct selection",
                    new File(prefix + "centreselection_numbercorrect.pdf"));
            gr_NumberOfCentreCorrect.setupForOneLineXLabels();
            gr_NumberOfCentreCorrect.configureTextLabels(-3,1,0.5);
            gr_PercentageOfCentreCorrect = new DrawGraphs.RBoxPlot<>("Centre Selection", "% of correct selection",
                    new File(prefix + "centreselection_percentagecorrect.pdf"));
            gr_PercentageOfCentreCorrect.setupForOneLineXLabels();
            gr_PercentageOfCentreCorrect.configureTextLabels(-3,1,0.5);
            gr_InconsistenciesForCentres = new DrawGraphs.RBoxPlot<>("Centre Selection", "Inconsistency (clamped to " + inconsistencyClamp + ")",
                    new File(prefix + "centreselection_inconsistency.pdf"));
            gr_InconsistenciesForCentres.setupForOneLineXLabels();
            gr_InconsistenciesForCentres.configureTextLabels(-5,1,0.5);
            gr_CorrectVsInconsistency = new DrawGraphs.RBoxPlot<>("Centre correctly predicted", "Inconsistency (clamped to " + inconsistencyClamp + ")",
                    new File(prefix + "centrecorrect_inconsistency.pdf"));
            gr_CorrectVsInconsistency.setupForOneLineXLabels();
            gr_CorrectVsInconsistency.configureTextLabels(-5,1,0.5);
            gr_CorrectVsInconsistencyWithPracticeLearn = new DrawGraphs.RBoxPlot<>("Centre correctly predicted", "Inconsistency with practice (clamped to " + inconsistencyClamp + ")",
                    new File(prefix + "centrecorrect_inconsistency_P.pdf"));
            gr_CorrectVsInconsistencyWithPracticeLearn.setupForOneLineXLabels();
            gr_CorrectVsInconsistencyWithPracticeLearn.configureTextLabels(-6,1,0.5);
        }

        public void report() {
            gr_NumberOfCentreCorrect.reportResults(group.gr);
            gr_PercentageOfCentreCorrect.reportResults(group.gr);
            gr_InconsistenciesForCentres.reportResults(group.gr);
            gr_CorrectVsInconsistency.reportResults(group.gr);
            gr_CorrectVsInconsistencyWithPracticeLearn.reportResults(group.gr);
        }
    }

    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        // IDENTIFICATION OF CENTRE VERTEX
        final DrawGraphs.CSVExperimentResult centreCSV = new DrawGraphs.CSVExperimentResult(new File(learningGroup.outPathPrefix + File.separator + "centre.csv"), "results.csv");
        MarkovExperiment.PreGeneratePTA tasks = new MarkovExperiment.PreGeneratePTA(learningGroup.phase, learningGroup.experimentRunner);
        boolean aveOrMax = true;// average divide by the divisor
        final int chunkSizeForCentreExperiments = 3;
        int alphabetMultiplier = 2;
        boolean penaliseMissingPaths = true;
        boolean pathsOrSets = true;

        int[] wlen_values = new int[]{1, 2, 3};
        int[] divisor_values = new int[]{1, 2, 4, 8};

        for (int states : learningGroup.statesToUse)
            for (int perStateSquaredDensity100 : MarkovExperiment.densityFromStateNumber(states))
                for (int sample = 0; sample < learningGroup.fsmSamplesPerStateNumber; ++sample)
                    for (int trainingSample = 0; trainingSample < learningGroup.trainingSamplesPerFSM; ++trainingSample) {
                        for (final Pair<Integer, Integer> traces_lengthmult : new Pair[]{learningGroup.getTracesLengthmultBaseline(states),
                                new Pair(1, MarkovExperiment.LearningExperimentGroupParameters.datasetSize * learningGroup.getScalingFactor(states))})
//                            for (double weightOfInconsistencies : new double[]{2.0})// this is a good value for 10 states
                            for (double weightOfInconsistencies : new double[]{0.5})// good value for 20 states
                                for (int wlen : wlen_values)
                                    for (int divisor : divisor_values) {
                                        ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                        ev.config = learningGroup.eval.config.copy();
                                        ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

                                        int traceLengthMultiplier = traces_lengthmult.secondElem;

                                        MarkovCentreLearningParameters parameters = new MarkovCentreLearningParameters(LearningAlgorithms.ScoringToApply.SCORING_MARKOV, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
                                        parameters.setTraceLengthMultiplier(traceLengthMultiplier);
                                        parameters.setExperimentID(traces_lengthmult.firstElem, learningGroup.traceLengthMultiplierMax, alphabetMultiplier);
                                        parameters.markovParameters.setMarkovParameters(1, chunkSizeForCentreExperiments, pathsOrSets,
                                                new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies, 0), penaliseMissingPaths, aveOrMax, divisor, 0, wlen);
                                        parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                        MarkovCentreIdentification centreIdentificationExperiment = new MarkovCentreIdentification(learningGroup.outPathPrefix, parameters, ev);
                                        centreIdentificationExperiment.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                        tasks.submitTask(centreIdentificationExperiment);
                                    }
                    }

        tasks.generatePTAAndSubmitTasks();// this will generate PTAs and submit tasks to the runner as needed.
        learningGroup.experimentRunner.collectOutcomeOfExperiments(new SGE_ExperimentRunner.processSubExperimentResult<MarkovLearningParameters, ExperimentResult<MarkovLearningParameters>>() {

            @Override
            public void processSubResult(ExperimentResult<MarkovLearningParameters> result, SGE_ExperimentRunner.RunSubExperiment<MarkovLearningParameters, ExperimentResult<MarkovLearningParameters>> experimentrunner) throws IOException {// in these experiments, samples are singleton sequences because we run each of them in a separate process, in order to increase the efficiency with which all tasks are split between CPUs in an iceberg grid.
                PairQualityLearner.SampleData sm = result.samples.get(0), smClamp = result.samples.get(1);
                PairQualityLearner.ScoresForGraph data = sm.actualLearner;

                StringBuffer csvLine = new StringBuffer();
                csvLine.append(sm.centreCorrect);// 0
                DrawGraphs.CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(sm.centrePathNumber);// 1
                DrawGraphs.CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(data.inconsistency);// 2
                DrawGraphs.CSVExperimentResult.addSeparator(csvLine);
                csvLine.append(smClamp.actualLearner.inconsistency);// 3
                DrawGraphs.CSVExperimentResult.addSeparator(csvLine);
                csvLine.append("0");// add fake time value, otherwise last added value gets overwritten.
                experimentrunner.RecordCSV(centreCSV, result.parameters, csvLine.toString());
            }

            @Override
            public DrawGraphs.SGEExperimentResult[] getGraphs() {
                return new DrawGraphs.SGEExperimentResult[]{centreCSV};
            }

        });

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            Set<MarkovExperiment.RESULT_VALUES> invalidCellValues = null;// all values are valid here
            for (int states : learningGroup.statesToUse) {

                Map<Integer, CentreSelectionResults> results = new TreeMap<>();

                for (Map.Entry<String, Map<String, String>> rowEntry : centreCSV.rowColumnText.entrySet()) {
                    MarkovLearningParameters rowValues = parseMarkovParametersRowFromCSV(rowEntry.getKey());
                    if (rowValues.states == states) {
                        for (int traceQuantityToUse : new int[]{1, learningGroup.getTracesLengthmultBaseline(states).firstElem}) {
                            results.computeIfAbsent(traceQuantityToUse, integer -> new CentreSelectionResults(learningGroup, states, traceQuantityToUse));
                            CentreSelectionResults resultsToUpdate = results.get(traceQuantityToUse);
//                        String[] rowValues = rowEntry.getKey().split("[_=]");
                            if (rowValues.traceQuantity == traceQuantityToUse) {

                                for (int wlen : wlen_values)
                                    for (int d : divisor_values) {
                                        MarkovExperiment.ColLearnerPresetAvemaxDivisorWlen centreStrategy =
                                                new MarkovExperiment.ColLearnerPresetAvemaxDivisorWlen(LearningAlgorithms.ScoringToApply.SCORING_MARKOV, 1, true, d, wlen);
                                        MarkovExperiment.ColumnAndValue Y = getValueFromMapGivenSelector(rowEntry.getValue(), centreStrategy,invalidCellValues);
                                        if (Y != null) {
                                            boolean centreCorrect = Boolean.parseBoolean(obtainValueFromCell(Y.value, 0));
                                            int pathsCount = Integer.parseInt(obtainValueFromCell(Y.value, 1));

                                            if (pathsCount > 0) {
                                                int inconsistency = Integer.parseInt(obtainValueFromCell(Y.value, 2));
                                                if (inconsistency > inconsistencyClamp)
                                                    inconsistency = inconsistencyClamp;

                                                String parametersAsString = wlen + "_" + d;
                                                resultsToUpdate.total.computeIfAbsent(parametersAsString, k -> new AtomicInteger(0));
                                                resultsToUpdate.total.get(parametersAsString).incrementAndGet();
                                                resultsToUpdate.count.computeIfAbsent(parametersAsString, k -> new AtomicInteger(0));
                                                if (centreCorrect)
                                                    resultsToUpdate.count.get(parametersAsString).incrementAndGet();

                                                resultsToUpdate.gr_InconsistenciesForCentres.add(parametersAsString + "_" + (centreCorrect ? "T" : "F"), (double) inconsistency, centreCorrect ? null : "red", null);
                                                resultsToUpdate.gr_CorrectVsInconsistency.add(Boolean.toString(centreCorrect), (double) inconsistency, null, null);
                                                long inconsistencyWithPractice = Integer.parseInt(obtainValueFromCell(Y.value, 3));
                                                if (inconsistencyWithPractice > inconsistencyClamp)
                                                    inconsistencyWithPractice = inconsistencyClamp;
                                                resultsToUpdate.gr_CorrectVsInconsistencyWithPracticeLearn.add(Boolean.toString(centreCorrect), (double) inconsistencyWithPractice, null, null);
                                            }
                                        }
                                    }
                            }
                        }
                    }
                }
                for (Map.Entry<Integer, CentreSelectionResults> resultsEntry : results.entrySet()) {
                    CentreSelectionResults centreResults = resultsEntry.getValue();
                    for (Map.Entry<String, AtomicInteger> entry : centreResults.count.entrySet()) {
                        centreResults.gr_NumberOfCentreCorrect.add(entry.getKey(), (double) entry.getValue().get(), null, null);
                        centreResults.gr_PercentageOfCentreCorrect.add(entry.getKey(), 100. * entry.getValue().get() / centreResults.total.get(entry.getKey()).get(), null, null);
                    }
                    centreResults.report();
                }
            }
        }
    }
}
