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
import java.util.LinkedList;
import java.util.Map;

import static statechum.analysis.learning.DrawGraphs.getValueFromMapGivenRegexp;
import static statechum.analysis.learning.DrawGraphs.obtainValueFromCell;

public class E_MarkovCentre {

    public static class MarkovCentreLearningParameters extends MarkovLearningParameters {

        public MarkovCentreLearningParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample, int argSeed) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample, argSeed);
        }

        @Override
        public String[] headerValuesForEachCell() {
            return new String[] {"centreCorrect","centerpaths","inconsistency","inconsistency_with_practice","Time"};
        }

        @Override
        public String getSubExperimentName() {
            return "centre";
        }
    }

    public static final int inconsistencyClamp = 50;

    public static class MarkovCentreIdentification extends MarkovExperiment.MarkovLearnerRunner {

        public MarkovCentreIdentification(MarkovLearningParameters parameters, ProgressDecorator.LearnerEvaluationConfiguration cnf) {
            super(parameters, cnf);
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
            switch(par.learnerToUse)
            {
                case SCORING_MARKOV:
                    markovLearner = new MarkovExperiment.EDSM_MarkovLearner(learnerInitConfiguration,pta,0,
                            par.markovParameters, Configuration.ScoreMode.GENERAL_NOFULLMERGE, null);
                    markovLearner.setMarkov(m);markovLearner.setChecker(checker);
                    break;
                case SCORING_MARKOV_1:
                    markovLearner = new MarkovExperiment.EDSM_MarkovLearner(learnerInitConfiguration,pta,1,
                            par.markovParameters, Configuration.ScoreMode.GENERAL_NOFULLMERGE, null);
                    markovLearner.setMarkov(m);markovLearner.setChecker(checker);
                    break;
                case SCORING_MARKOV_2:
                    markovLearner = new MarkovExperiment.EDSM_MarkovLearner(learnerInitConfiguration,pta,2,
                            par.markovParameters, Configuration.ScoreMode.GENERAL_NOFULLMERGE, null);
                    markovLearner.setMarkov(m);markovLearner.setChecker(checker);
                    break;
                default:
                    throw new IllegalArgumentException("Invalid learner selected: "+par.learnerToUse);
            }

            PairQualityLearner.SampleData dataSampleAccountingForTestLearn = new PairQualityLearner.SampleData(null,null);
            dataSampleAccountingForTestLearn.centreCorrect = firstMerge.correctCentre;
            dataSampleAccountingForTestLearn.centrePathNumber = firstMerge.centrePathNumber;
            dataSampleAccountingForTestLearn.actualLearner = new PairQualityLearner.ScoresForGraph();
            dataSampleAccountingForTestLearn.actualLearner.inconsistency = dataSample.actualLearner.inconsistency;
            outcome.samples.add(dataSampleAccountingForTestLearn);

            LearnerGraph learntGraph = markovLearner.learnMachine(new LinkedList<>(), new LinkedList<>());
            if ( AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK == learntGraph.getLearningAbortedReason()) {
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
        final DrawGraphs.RBoxPlot<String> gr_InconsistenciesForCentres;
        final DrawGraphs.RBoxPlot<String> gr_CorrectVsInconsistency;
        final DrawGraphs.RBoxPlot<String> gr_CorrectVsInconsistencyWithPracticeLearn;

        final MarkovExperiment.LearningExperimentGroupParameters group;
        int count = 0;

        public CentreSelectionResults(MarkovExperiment.LearningExperimentGroupParameters learningGroup, int traceNum) {
            group = learningGroup;
            final int statesMax = learningGroup.statesToUse[learningGroup.statesToUse.length-1];
            String prefix = learningGroup.outPathPrefix + statesMax+"_"+traceNum+"_";
            gr_NumberOfCentreCorrect = new DrawGraphs.RBoxPlot<>("Centre Selection", "Number of correct selection",
                    new File(prefix + "centreselection_numbercorrect.pdf"));
            gr_InconsistenciesForCentres = new DrawGraphs.RBoxPlot<>("Centre Selection", "Inconsistency (clamped to "+inconsistencyClamp+" )",
                    new File(prefix + "centreselection_inconsistency.pdf"));
            gr_CorrectVsInconsistency = new DrawGraphs.RBoxPlot<>("Centre correctly predicted", "Inconsistency (clamped to "+inconsistencyClamp+" )",
                    new File(prefix + "centrecorrect_inconsistency.pdf"));
            gr_CorrectVsInconsistencyWithPracticeLearn = new DrawGraphs.RBoxPlot<>("Centre correctly predicted", "Inconsistency P (clamped to "+inconsistencyClamp+" )",
                    new File(prefix + "centrecorrect_inconsistency_P.pdf"));
        }

        public void report() {
            gr_NumberOfCentreCorrect.reportResults(group.gr);
            gr_InconsistenciesForCentres.reportResults(group.gr);
            gr_CorrectVsInconsistency.reportResults(group.gr);
            gr_CorrectVsInconsistencyWithPracticeLearn.reportResults(group.gr);
        }
    }

    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        // IDENTIFICATION OF CENTRE VERTEX
        final DrawGraphs.CSVExperimentResult centreCSV = new DrawGraphs.CSVExperimentResult(new File(learningGroup.outPathPrefix+"centre.csv"));
        final int statesMax = learningGroup.statesToUse[learningGroup.statesToUse.length-1];// reflects the size of the largest FSM that will be generated.
        boolean aveOrMax = true;// average divide by the divisor
        final int chunkSizeForCentreExperiments = 3;
        int alphabetMultiplier = 2;
        boolean pathsOrSets = true;

        int [] wlen_values = new int[]{1,2,3};
        int [] divisor_values = new int[]{1,2,4,8};

        for(final int traceQuantityToUse:new int[]{1})
        {
            int seedForFSM = 0;

            for(int states:learningGroup.statesToUse)
                for(int perStateSquaredDensity100:new int[] {0,30})
                {
                    for(int sample=0;sample<learningGroup.fsmSamplesPerStateNumber;++sample,++seedForFSM)
                        for(int trainingSample=0;trainingSample<learningGroup.trainingSamplesPerFSM;++trainingSample)
                            for(final Pair<Integer,Integer> traces_lengthmult:new Pair[]{new Pair(8,32), new Pair(1,256)})
                                    for(double weightOfInconsistencies:new double[]{2.0})//1.0,2.0,4.0}
                                        for(int wlen:wlen_values)
                                            for(int divisor:divisor_values)
                                            {
                                                ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                                ev.config = learningGroup.eval.config.copy();ev.config.setOverride_maximalNumberOfStates(states* LearningAlgorithms.maxStateNumberMultiplier);

                                                int traceLengthMultiplier = traces_lengthmult.secondElem;

                                                MarkovCentreLearningParameters parameters = new MarkovCentreLearningParameters(LearningAlgorithms.ScoringToApply.SCORING_MARKOV,states, alphabetMultiplier, perStateSquaredDensity100, sample,trainingSample, seedForFSM);
                                                parameters.setTraceLengthMultiplier(traceLengthMultiplier);
                                                parameters.setExperimentID(traceQuantityToUse,learningGroup.traceLengthMultiplierMax,statesMax,alphabetMultiplier);
                                                parameters.markovParameters.setMarkovParameters(1,chunkSizeForCentreExperiments,pathsOrSets,weightOfInconsistencies, aveOrMax,divisor,0,wlen);
                                                parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                                MarkovCentreIdentification centreIdentificationExperiment = new MarkovCentreIdentification(parameters, ev);
                                                centreIdentificationExperiment.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                                learningGroup.experimentRunner.submitTask(centreIdentificationExperiment);
                                            }
                }
        }

        learningGroup.experimentRunner.collectOutcomeOfExperiments(new SGE_ExperimentRunner.processSubExperimentResult<MarkovLearningParameters, ExperimentResult<MarkovLearningParameters>>() {

            @Override
            public void processSubResult(ExperimentResult<MarkovLearningParameters> result, SGE_ExperimentRunner.RunSubExperiment<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>> experimentrunner) throws IOException
            {// in these experiments, samples are singleton sequences because we run each of them in a separate process, in order to increase the efficiency with which all tasks are split between CPUs in an iceberg grid.
                PairQualityLearner.SampleData sm = result.samples.get(0), smClamp = result.samples.get(1);
                PairQualityLearner.ScoresForGraph data=sm.actualLearner;

                StringBuffer csvLine = new StringBuffer();
                csvLine.append(sm.centreCorrect);// 0
                DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.centrePathNumber);// 1
                DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistency);// 2
                DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(smClamp.actualLearner.inconsistency);// 3
                DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append("0");// add fake time value, otherwise last added value gets overwritten.
                experimentrunner.RecordCSV(centreCSV, result.parameters, csvLine.toString());
            }

            @Override
            public DrawGraphs.SGEExperimentResult[] getGraphs() {

                return new DrawGraphs.SGEExperimentResult[]{centreCSV};
            }

        });

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            CentreSelectionResults [] results = new CentreSelectionResults[] {new CentreSelectionResults(learningGroup,1),new CentreSelectionResults(learningGroup,8)};

            for (int wlen : wlen_values)
                for (int d : divisor_values) {
                    String centreStrategy = LearningAlgorithms.ScoringToApply.SCORING_MARKOV + "-1_dv=A_d=" + d + "_wl=" + wlen;

                    for (Map.Entry<String, Map<String, String>> rowEntry : centreCSV.rowColumnText.entrySet()) {
                        String Y = getValueFromMapGivenRegexp(rowEntry.getValue(), centreStrategy);
                        if (Y != null) {
                            boolean valid = Boolean.parseBoolean(obtainValueFromCell(Y, 0));
                            int pathsCount = Integer.parseInt(obtainValueFromCell(Y, 1));


                            if (pathsCount > 0) {
                                int inconsistency =  Integer.parseInt(obtainValueFromCell(Y, 2));
                                if (inconsistency > inconsistencyClamp)
                                    inconsistency = inconsistencyClamp;

                                CentreSelectionResults resultsToUpdate = results[0];
                                if (valid)
                                    resultsToUpdate.count++;
                                resultsToUpdate.gr_InconsistenciesForCentres.add(wlen + "_" + d + "_" + (valid ? "T" : "F"), (double) inconsistency, valid?null:"red", null);
                                resultsToUpdate.gr_CorrectVsInconsistency.add(Boolean.toString(valid), (double) inconsistency, null, null);
                                long inconsistencyWithPractice = Integer.parseInt(obtainValueFromCell(Y, 3));
                                if (inconsistencyWithPractice > inconsistencyClamp)
                                    inconsistencyWithPractice = inconsistencyClamp;
                                resultsToUpdate.gr_CorrectVsInconsistencyWithPracticeLearn.add(Boolean.toString(valid), (double) inconsistencyWithPractice, null, null);
                            }
                        }
                    }
                    for(CentreSelectionResults resultsToUpdate: results)
                        resultsToUpdate.gr_NumberOfCentreCorrect.add(wlen + "_" + d, (double) resultsToUpdate.count, null, null);
                }
            for(CentreSelectionResults r : results)
                r.report();
        }
    }
}
