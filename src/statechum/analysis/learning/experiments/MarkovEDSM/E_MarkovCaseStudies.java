package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.*;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.*;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.I2cexperiment.loadTrace;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.RESULT_VALUES.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_TIMEOUT;
import static statechum.analysis.learning.rpnicore.FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovCaseStudies {
    public static final String caseStudyFanTempMonitorSingleTrace = "FanTempMonitor_T";
    public static final String caseStudyFanTempMonitor = "FanTempMonitor_A";

    public static final String description = "casestudies";
    /**
     * Orders case studies - outcome directories use numbers that refer to specific positions in this list hence there should be no renumbering.
     */
    public static String[] caseStudies = new String[]{"CVS", "SSH", "MinePump", "ATM", "SmallTrain", caseStudyFanTempMonitor, caseStudyFanTempMonitorSingleTrace};
    public static Map<Integer, CaseStudyInformation> caseStudyInformationMap = new HashMap<>();

    public static LearnerGraph constructAutomatonForCaseStudy(String caseStudyName, Configuration config, final Transform.ConvertALabel conv) {
        switch (caseStudyName) {
            case "CVS":
                // Derived from a similar model described in:
                // D. Lo and S. Khoo, “QUARK: Empirical assessment of automaton-based specification miners,” in Proceedings of the Working Conference on Reverse Engineering (WCRE’06). IEEE Computer Society, 2006, pp. 51–60.
                return FsmParserStatechum.buildLearnerGraph(
                        "q1-connect->q2-login->q3-setfiletype->q4-rename->q6-storefile->q5-setfiletype->q4-storefile->q7-appendfile->q5\nq3-makedir->q8-makedir->q8-logout->q16-disconnect->q1\nq3-changedirectory->q9-listnames->q10-delete->q10-changedirectory->q9\nq10-appendfile->q11-logout->q16\nq3-storefile->q11\nq3-listfiles->q13-retrievefile->q13-logout->q16\nq13-changedirectory->q14-listfiles->q13\nq7-logout->q16\nq6-logout->q16", "cvs", config, conv);
            case "SSH":
                // Almost verbatim from Verifying an implementation of SSH by Poll, E.; Schubert, A. 2007.
                // The changes are : merged equivalent states WAIT_KEXDH_REPLY and KEXINIT_KEXDH_INIT_SENT into WAIT_KEXDH_REPLY
                // Added a 'comm' transition looping in the COMMUNICATION state and the 'reset' transition.
                return FsmParserStatechum.buildLearnerGraph(
                        "DISCONNECTED -connect! -> WAIT_VERSION - version? -> VERSION_RECEIVED - version! -> WAIT_KEXINIT / WAIT_VERSION-version! ->VERSION_SENT-version? ->WAIT_KEXINIT /" +
                                "WAIT_KEXINIT -kexinit? -> KEXINIT_RECEIVED -kexinit!->KEXINIT_SENT - kexdh_init!-> WAIT_KEXDH_REPLY -kexdh_reply? ->WAIT_NEWKEYS /" +
                                "WAIT_KEXINIT -kexinit! ->KEXINIT_SENT_NOTR -kexinit? ->KEXINIT_SENT / KEXINIT_SENT_NOTR -kexdh_init! ->KEXDH_KEXINIT_SENT_NOTR - kexinit? -> WAIT_KEXDH_REPLY /" +
                                "WAIT_NEWKEYS -newkeys? -> NEWKEYS_RECEIVED -newkeys! -> COMMUNICATION / WAIT_NEWKEYS -newkeys! ->NEWKEYS_SENT-newkeys? ->COMMUNICATION -comm->COMMUNICATION /" +
                                "COMMUNICATION - kexinit? ->KEXINIT_RECEIVED / COMMUNICATION -kexinit! ->KEXINIT_SENT_NOTR /" +
                                "COMMUNICATION -reset->DISCONNECTED", "ssh", config, conv);
            case "ATM":
                // The ATM is from Figure 5: An implementation of MTS in Fig. 4 of Existential Live Sequence Charts Revisited by German Sibay, Sebastian Uchitel and Victor Braberman,
                // with a ejectCard transition added from state 4 to 0.
                return FsmParserStatechum.buildLearnerGraph("0-pwd->1-verify->2-wait->3-verifying->4-ok->5-reqCash->6-getBalance->7-cash->8-updateBalance->5 / 7-notEnoughCash->5 / 4-wrongPwd->0 / 2-verifying->9-wait->4 / 5-ejectCard->0"
                        , "ATM", config, conv);
            case "MinePump":
                // The Mine pump is from Figure 10: Final MTS of Existential Live Sequence Charts Revisited by German Sibay, Sebastian Uchitel and Victor Braberman
                return FsmParserStatechum.buildLearnerGraph(
                        "0-tick->0 -medWater->1-lowWater->0 / 1-tick->1 -switchOn->2 -tick->2 - highWater->3-tick->3-medWater->4-tick->4-highWater->3 / 4 -lowWater->5-switchOff->0 /" +
                                "2-methAppears->6 / 4 -methAppears->6 - switchOff->7-tick->7-highWater->8-tick->8-medWater->7 / 8-methLeaves->9 -switchOn->3 / 7-methLeaves->10 -tick->10 -switchOn->2 /" +
                                "10-methAppears->11-tick->11-highWater->12-tick->12-medWater->11 / 11-lowWater->13-tick->13-medWater->11 / 11-methLeaves->1 / 13-methLeaves->0 / " +
                                "0 - methAppears->13 / 2-lowWater->15 / 1-highWater->9 / 1-methAppears->11 / 3-methAppears->14 - switchOff->8 /" +
                                "15 -switchOff ->16 -tick->16-medWater->17 -tick->17-highWater->9 / 17-methAppears->18-tick->18-methLeaves->17 / 18-highWater->19-tick->19 -methLeaves->9 /" +
                                "16-methAppears->20-tick->20 - medWater->18 / 20-methLeaves->16"
                        , "MinePump", config, conv);
            case "SmallTrain":
                // This is the small train controller from Figure 3 of Scenarios, Goals, and State Machines: a Win-Win Partnership for Model Synthesis Christophe Damas, Bernard Lambeau, and Axel van Lamsweerde,
                // where the dead-end state 5 was removed.
                return FsmParserStatechum.buildLearnerGraph("0-start->2-stop->0 -a.pres->9-a.prop->11-e.open->1-close->0 / 0-open->1/2-a.pres->4-a.prop->8-e.stop->11", "SmallTrain", config, conv);

            case caseStudyFanTempMonitor:
            case caseStudyFanTempMonitorSingleTrace:
                LearnerGraph fanTempMonitorWithNegatives = new LearnerGraph(config);
                try {
                    String pathToFanTempMonitor = "resources/i2c_study/i2c_outcome_correct";
                    if (!Files.exists(Paths.get(pathToFanTempMonitor + ".xml"))) // If running on HPC, the working directory is 'stanage'
                        pathToFanTempMonitor = "../" + pathToFanTempMonitor;
                    AbstractPersistence.loadGraph(pathToFanTempMonitor, fanTempMonitorWithNegatives, conv);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
                fanTempMonitorWithNegatives.setName("FanTempMonitor");
                return LearningSupportRoutines.removeAllNegatives(fanTempMonitorWithNegatives);
            default:
                Configuration dotConfig = config.copy();
                dotConfig.setLabelKind(Configuration.LABELKIND.LABEL_STRING);
                String referenceDot;
                try {
                    referenceDot = Helper.loadFile(new File(
                            GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_CASESTUDIES) +
                                    File.separator + caseStudyName + ".dot"));
                } catch (IOException e) {
                    throw new RuntimeException("Failed to load graph " + e);
                }
                LearnerGraph referenceGraph = FsmParserDot.buildLearnerGraph(referenceDot, dotConfig,
                        conv, true, true, USE_START0);
                referenceGraph.setName(caseStudyName);
                return referenceGraph;
        }
    }


    // When tuning results, I only need to run one, however I do wish to maintain the ordering of case studies, so that
    // experiments with a specific one do not replace experiments with others.
    public static Set<String> whichCaseStudyToRun = new TreeSet<>();

    static {
//        whichCaseStudyToRun.add("SmallTrain");
//        whichCaseStudyToRun.add("CVS");
//        whichCaseStudyToRun.add("ATM");
//        whichCaseStudyToRun.add("SSH");
//        whichCaseStudyToRun.add("MinePump");
//        whichCaseStudyToRun.add(caseStudyFanTempMonitor);
//        whichCaseStudyToRun.add(caseStudyFanTempMonitorSingleTrace);
    }

    public static class MarkovLearningBaselineParameters extends MarkovLearningParameters {

        public MarkovLearningBaselineParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample);
        }

        @Override
        public String getSubExperimentName() {
            return description;
        }
    }

    public static class MarkovLearnerRunnerForCaseStudies extends MarkovLearnerRunner {

        public MarkovLearnerRunnerForCaseStudies(String outDir, MarkovLearningBaselineParameters parameters, ProgressDecorator.LearnerEvaluationConfiguration ev) {
            super(outDir, parameters, ev);
        }

        /**
         * Constructs a reference graph and assigns it to member variable <pre>referenceGraph</pre>. This is a separate method to permit overriding by subclasses.
         */
        public void generateReferenceFSM() {
            referenceGraph = constructAutomatonForCaseStudy(caseStudies[par.sample], learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter());
//            Visualiser.updateFrame(referenceGraph, null);
//            Visualiser.waitForKey();
        }

        @Override
        public LearnerGraph constructPTA() {
            LearnerGraph constructedPTA = caseStudyInformationMap.get(par.sample).constructPTA(learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter());
            if (constructedPTA == null)
                constructedPTA = super.constructPTA();

            return constructedPTA;
        }
    }


    public static class CaseStudyInformation {
        int trainingSamplesPerFSM = 40;// these are fixed automata hence we can try many different values to see how inference performs.
        public final String name;
        public final int sample;
        public final LearnerGraph referenceGraph;
        public final int alphabetSize;
        Pair<Integer, Integer>[] traces_and_lengths;
        /**
         * For one of the case studies, we have an original trace it was learnt from. The variable below stores this length
         * (precise value rather than the one divided by the number of states in the second component of traces_and_lengths.
         * Negative if the value is not set.
         */
        int actualLength = -1;
        int[] chunkSizesToEvaluate = new int[]{3};
        final int states;

        /**
         * Maps chunk length to the associated values of weights: larger chunks are usually associated with smaller values of weights.
         * null means uses  weightOfInconsistencies instead.
         */
        Map<Integer, double[]> chunkLenToWeights = null;

        protected static final double[] defaultWeightsOfInconsistencies = new double[]{0.25, 0.5, 1.0, 2.0};

        public CaseStudyInformation(String name, int sample, LearnerGraph referenceGraph, int alphabetSize, Pair<Integer, Integer>[] traces_and_lengths) {
            this.name = name;
            this.sample = sample;
            this.referenceGraph = referenceGraph;
            this.alphabetSize = alphabetSize;
            this.traces_and_lengths = traces_and_lengths;
            this.states = this.referenceGraph.getStateNumber();
            setWeightOfInconsistencies(defaultWeightsOfInconsistencies);
        }

        public Configuration.STATETREE transitionMatrixImplType = Configuration.STATETREE.STATETREE_LINKEDHASH;

        public void setChunkSizesAndWeightsToEvaluate(int[] chunkSizesToEvaluate) {
            this.chunkSizesToEvaluate = chunkSizesToEvaluate;
            setWeightOfInconsistencies(defaultWeightsOfInconsistencies);
        }

        /**
         * Used to generate a PTA for the case study in case it is not built by randomly exploring a transition graph.
         * Should return either null in order to build PTA randomly or a PTA.
         */
        public LearnerGraph constructPTA(Configuration config, Transform.ConvertALabel labelConverter) {
            if (Objects.equals(name, caseStudyFanTempMonitorSingleTrace)) {
                LearnerGraph initialPTA = new LearnerGraph(config);
                String pathToLogForFanTempMonitor_T = "resources/i2c_study/log10.txt";
                if (!Files.exists(Paths.get(pathToLogForFanTempMonitor_T))) // If running on HPC, the working directory is 'stanage'
                    pathToLogForFanTempMonitor_T = "../" + pathToLogForFanTempMonitor_T;
                initialPTA.paths.augmentPTA(loadTrace(pathToLogForFanTempMonitor_T, labelConverter, "Err"), true, false, null);
                return initialPTA;
            }

            return null;
        }

        void setWeightOfInconsistencies(double[] weightOfInconsistencies) {
            chunkLenToWeights = new TreeMap<>();
            for (int chLen : chunkSizesToEvaluate)
                chunkLenToWeights.put(chLen, weightOfInconsistencies);
        }

        void setWeightOfInconsistenciesDependingOnChunkLen(Map<Integer, double[]> weights) {
            this.chunkLenToWeights = weights;
            for (int chLen : chunkSizesToEvaluate)
                if (!chunkLenToWeights.containsKey(chLen))
                    throw new IllegalStateException("Chunk length " + chLen + " is not present in the weights map");
        }

        void setTransitionMatrixImplType(Configuration.STATETREE transitionMatrixImplType) {
            this.transitionMatrixImplType = transitionMatrixImplType;
        }
    }

    public static double capToTimeout(double value, AtomicInteger timeoutCap) {
        if (timeoutCap == null || timeoutCap.get() <= 0)
            return value;

        if (value > timeoutCap.get())
            return timeoutCap.get();

        return value;
    }

    public static class ResultsXAxis implements Comparable<ResultsXAxis> {
        public final LearningAlgorithms.ScoringToApply learner;
        public final int traceNum;
        public final int chunkSize;
        public final boolean useCentre;

        public ResultsXAxis(LearningAlgorithms.ScoringToApply learner, int traceNum, int chunkSize, boolean useCentre) {
            this.learner = learner;
            this.traceNum = traceNum;
            this.chunkSize = chunkSize;
            this.useCentre = useCentre;
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof ResultsXAxis)) return false;
            ResultsXAxis that = (ResultsXAxis) o;
            return traceNum == that.traceNum && chunkSize == that.chunkSize && useCentre == that.useCentre && Objects.equals(learner, that.learner);
        }

        @Override
        public int hashCode() {
            return Objects.hash(learner, traceNum, chunkSize, useCentre);
        }

        @Override
        public String toString() {
            if (learner == LearningAlgorithms.ScoringToApply.SCORING_MARKOV)
                return traceNum + "\n" + (useCentre ? "C" : "N") + "M_" + (chunkSize - 1);
            return traceNum + "\n" + learner.name;
        }

        @Override
        public int compareTo(ResultsXAxis other) {
            if (traceNum != other.traceNum)
                return traceNum - other.traceNum;

            int value = learner.compareTo(other.learner);
            if (value != 0)
                return value;

            if (useCentre != other.useCentre)
                return useCentre ? -1 : 1;

//            if (prefixLength != o2.prefixLength)
            return chunkSize - other.chunkSize;
        }

        /**
         * The purpose of this method is to determine whether a particular combination of values needs reporting for a specific case study.
         * This is usually true since the values used in experiments are the ones to be reported, but not always. Sometimes, we might
         * report a smaller subset but keep the data points for everything computed.
         *
         * @param name case study name
         * @return whether a particular combinatinon of values of this data point is to be reported for the specific case study
         */
        public boolean filter(String name) {
            switch (name) {
                case "SmallTrain":
                    if (learner == LearningAlgorithms.ScoringToApply.SCORING_EDSM_4 || learner == LearningAlgorithms.ScoringToApply.SCORING_VH)
                        return true;
                    if (learner == LearningAlgorithms.ScoringToApply.SCORING_MARKOV)
                        return chunkSize >= 3 && chunkSize <= 4 && useCentre == false;
                    return false;
                case "CVS":
                    if (learner == LearningAlgorithms.ScoringToApply.SCORING_VH)
                        return true;
                    if (learner == LearningAlgorithms.ScoringToApply.SCORING_MARKOV)
                        return chunkSize >= 3 && chunkSize <= 4 && useCentre;
                    return false;
                case "ATM":
                    if (learner == LearningAlgorithms.ScoringToApply.SCORING_VH || learner == LearningAlgorithms.ScoringToApply.SCORING_EDSM_4 || learner == LearningAlgorithms.ScoringToApply.SCORING_PTAK_2)
                        return true;
                    if (learner == LearningAlgorithms.ScoringToApply.SCORING_MARKOV)
                        return chunkSize == 3;
                    return false;
                case "SSH":
                    if (learner == LearningAlgorithms.ScoringToApply.SCORING_VH || learner == LearningAlgorithms.ScoringToApply.SCORING_EDSM_4)
                        return true;
                    if (learner == LearningAlgorithms.ScoringToApply.SCORING_MARKOV)
                        return chunkSize >= 3 && chunkSize <= 3 && useCentre == true;
                    return false;
                case "MinePump":
                    if (learner == LearningAlgorithms.ScoringToApply.SCORING_VH)
                        return true;
                    if (learner == LearningAlgorithms.ScoringToApply.SCORING_MARKOV)
                        return chunkSize >= 3 && chunkSize <= 4;
                    return false;
                case caseStudyFanTempMonitor:
                    if (learner == LearningAlgorithms.ScoringToApply.SCORING_VH)
                        return true;
                    if (learner == LearningAlgorithms.ScoringToApply.SCORING_MARKOV)
                        return chunkSize >= 4;

                    return false;
                default:
                    return true;
            }
        }

        /**
         * Expected to return true if a particular result is to be included in the final spreadsheet. Used in conjunction
         * with filter, mostly to select the best prefix length.
         *
         * @param name case study to consider
         * @return whether the value should be included in the reported table
         */
        public boolean addToSpreadsheet(String name) {
            if (!filter(name))
                return false;
            switch (name) {
                case "SmallTrain":
                    return chunkSize == 4;
            }
            return true;
        }
    }

    public static void runExperiment(LearningExperimentGroupParameters learningGroup) {
        int[] learnerExperiment = new int[]{0, 1};
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + "casestudies-results.csv"), "results.csv");
        boolean aveOrMax = true;// average divide by the divisor
        boolean pathsOrSets = true, penaliseMissingPaths = true;
//        String pathToCaseStudyFiles = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_CASESTUDIES);
//        if (null == pathToCaseStudyFiles ||  pathToCaseStudyFiles.isEmpty())
//            throw new RuntimeException("Cannot load any case studies: path to case studies is not defined");
//        if (!Files.exists(Paths.get(pathToCaseStudyFiles)))
//            throw new RuntimeException("Cannot load any case studies: path to case studies does not exist "+pathToCaseStudyFiles);

        long timeout = 1800000L * 9L;// // for case studies, set timeout to 4.5 hours - the one that runs that long is centre-based computations for FanTempMonitor with 676 traces that do not produce brilliant results anyway (comparable to learning without centre since the PTA is dense enough for normal learning).

        fillInCaseStudyExperimentParameters(learningGroup);

        for (int casestudy = 0; casestudy < caseStudies.length; casestudy++)
            if (whichCaseStudyToRun == null || whichCaseStudyToRun.isEmpty() || whichCaseStudyToRun.contains(caseStudies[casestudy])) {

                for (final int preset : learnerExperiment)
                    for (final Pair<Integer, Integer> traces_lengthmult : caseStudyInformationMap.get(casestudy).traces_and_lengths) {
                        int states = caseStudyInformationMap.get(casestudy).referenceGraph.getStateNumber();
                        int traceQuantityToUse = traces_lengthmult.firstElem;
                        for (int trainingSample = 0; trainingSample < caseStudyInformationMap.get(casestudy).trainingSamplesPerFSM; ++trainingSample)
                            for (LearningAlgorithms.ScoringToApply learnerKind :
                                    preset == 0 ?// this is the only case where we can apply PTA-based merging algorithms, two other presets handle merging vertices in a connected graph
                                            new LearningAlgorithms.ScoringToApply[]{
                                                    LearningAlgorithms.ScoringToApply.SCORING_MARKOV,
                                                    LearningAlgorithms.ScoringToApply.SCORING_EDSM_1, LearningAlgorithms.ScoringToApply.SCORING_EDSM_2, LearningAlgorithms.ScoringToApply.SCORING_EDSM_4,
                                                    LearningAlgorithms.ScoringToApply.SCORING_PTAK_1, LearningAlgorithms.ScoringToApply.SCORING_PTAK_2,
                                                    LearningAlgorithms.ScoringToApply.SCORING_VH
                                            } :
                                            new LearningAlgorithms.ScoringToApply[]{
                                                    LearningAlgorithms.ScoringToApply.SCORING_MARKOV
                                            })
                                for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? caseStudyInformationMap.get(casestudy).chunkSizesToEvaluate : new int[]{2})
                                    for (double weightOfInconsistencies : learnerKind.isMarkov() ?
                                            caseStudyInformationMap.get(casestudy).chunkLenToWeights.get(chunkSizeToEvaluate)
                                            //new double[]{0.25,0.5,1.0, 2.0, 3.0, 4.0, 6.0, 8.0}
                                            : new double[]{1.0})
                                        for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 1)} :
                                                new Pair[]{new Pair(1, 2), new Pair(1, 4), new Pair(2, 4), new Pair(2, 8)}) {
                                            ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                            ev.config = learningGroup.eval.config.copy();
                                            ev.config.setOverride_maximalNumberOfStates(states * 2);//LearningAlgorithms.maxStateNumberMultiplier);
                                            if (learnerKind.isMarkov())
                                                ev.config.setLearnerScoreMode(Configuration.ScoreMode.ONLYOVERRIDE);
                                            // For some case studies (FanTempController_T) there is a large amount of data - need Array-based data structures
                                            ev.config.setTransitionMatrixImplType(caseStudyInformationMap.get(casestudy).transitionMatrixImplType);
                                            ev.config.setTimeOut(timeout);
                                            MarkovLearningBaselineParameters parameters = new MarkovLearningBaselineParameters(learnerKind, states, 0, 0, casestudy, trainingSample);
                                            parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
                                            parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, 0);
                                            parameters.markovParameters.setMarkovParameters(preset, chunkSizeToEvaluate, pathsOrSets,
                                                    new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies, 0), penaliseMissingPaths, aveOrMax, wlen_divisor.secondElem, 0, wlen_divisor.firstElem);
                                            parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
                                            parameters.disableReportMergeStatisticsWhenSolutionIsKnown();
//                                            parameters.setWalkType(RandomPathGenerator.WALKTYPE.WALKTYPE_AIMFORTRANSITIONCOVER_PREFERNONLOOP,0.6, 10);
                                            MarkovLearnerRunner learnerRunner = new MarkovLearnerRunnerForCaseStudies(learningGroup.outPathPrefix, parameters, ev);
                                            learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).

                                            // Important: this is the special case intended to avoid running experiments that do not deliver particularly good results
                                            // but take forever (many of them running longer than the 4.5 hours timeout). This happens because we attempt to use centre on a large graph
                                            // that causes a large number of red-blue comparions to be made (in the range of 100k - 1M), each of which is not very fast because we
                                            // have to compute inconsistency. Experiments taking points excluded below have been migrated to E_MarkovFanTempMonitor600.java
                                            if (!caseStudyInformationMap.get(casestudy).name.equals(caseStudyFanTempMonitor) || preset == 0 || traceQuantityToUse < 600)
                                                learningGroup.experimentRunner.submitTask(learnerRunner);
                                        }
                    }
            }

        learningGroup.experimentRunner.collectOutcomeOfExperiments(constructResultsCollector(resultCSV));

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            Set<RESULT_VALUES> validityOfCells = obtainValidityOfCellValues(resultCSV);
            List<List<String>> outputStatistics = new ArrayList<>();
            outputStatistics.add(new ArrayList<>(Arrays.asList("Case study", "States", "Alphabet", "Traces", "T. Length", "Centre", "P.Len", "Diff, M", "BCR, M", "Diff, VH", "BCR, VH", "A12", "A12 lo", "A12 hi", "Wilcoxon")));
            for (Map.Entry<Integer, CaseStudyInformation> entryForCaseStudy : caseStudyInformationMap.entrySet()) {

                // We need to compute the smallest runtime that was deemed to be a timeout. It is subsequently used as a cap
                // on the timeout values because in different experiments different timeouts were used (and time was also measured less precisely
                // in that some timeouts were only detected long past their timeout values).
                AtomicInteger timeoutValueObtained = new AtomicInteger(Integer.MAX_VALUE);
                for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                    MarkovLearningParameters rowHeader = parseMarkovParametersRowFromCSV(rowEntry.getKey());
                    if (rowHeader.sample == entryForCaseStudy.getKey()) {
                        getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), validityOfCells,
                                (column, columnText, Y) -> {
                                    boolean learntTimeout = obtainStringValueFromCell(Y, RESULT_VALUES.E_SUCCESS, column).equals(LEARNING_TIMEOUT.name);
                                    if (learntTimeout) {
                                        int runtime = (int) Math.round(obtainDoubleValueFromCell(Y, E_RUNTIME, column));
                                        timeoutValueObtained.accumulateAndGet(runtime, (a, b) -> Math.min(a, b));
                                    }
                                    if (obtainIntValueFromCell(Y, E_TRANSITIONS_SAMPLED,column) != 100)
                                        throw new IllegalArgumentException("Case study "+entryForCaseStudy.getValue().name+", experiment "+rowEntry.getKey()+" transition coverage is "+obtainIntValueFromCell(Y, E_TRANSITIONS_SAMPLED,column)+", it preferrably should be 100");
                                });
                    }
                }

                final RBoxPlot<String> gr_PerformanceOfLearners = new RBoxPlot<>("", "Structural Score",
                        new File(learningGroup.outPathPrefix + description + "_" + entryForCaseStudy.getValue().name + "_learner_structural.pdf"));
                gr_PerformanceOfLearners.setupForTwoLineXLabels();
                gr_PerformanceOfLearners.setMargins(3, 3, 0.2, 0.2);

                final RBoxPlot<String> gr_RuntimeOfLearners = new RBoxPlot<>("", "Runtime",
                        new File(learningGroup.outPathPrefix + description + "_" + entryForCaseStudy.getValue().name + "_learner_runtime.pdf"));
                gr_RuntimeOfLearners.setupForTwoLineXLabels();
                gr_RuntimeOfLearners.setMargins(3, 3, 0.2, 0.2);

                final RBoxPlot<String> gr_SuccessPercentage = new RBoxPlot<>("", "%% success",
                        new File(learningGroup.outPathPrefix + description + "_" + entryForCaseStudy.getValue().name + "_learner_successpercentage.pdf"));
                gr_SuccessPercentage.setupForTwoLineXLabels();
                gr_SuccessPercentage.setMargins(3, 3, 0.2, 0.2);


                ProgressIndicator progress = new ProgressIndicator(entryForCaseStudy.getValue().name,
                        entryForCaseStudy.getValue().chunkSizesToEvaluate.length * entryForCaseStudy.getValue().traces_and_lengths.length * 2);// 2 is for the use of centre or not
                Map<ResultsXAxis, AtomicInteger> countsSuccess = new HashMap<>();
                Map<ResultsXAxis, AtomicInteger> countsTotal = new HashMap<>();
                for (final int chunkSizeToEvaluate : entryForCaseStudy.getValue().chunkSizesToEvaluate) {
                    Pair<Integer, Integer>[] traces_and_lengths = entryForCaseStudy.getValue().traces_and_lengths;

                    for (final boolean useCentre : new boolean[]{false, true})
                        for (final Pair<Integer, Integer> traces_lengthmult : traces_and_lengths)
                            // Do not process values from the extremely slow case of caseStudyFanTempMonitor
                            if (!entryForCaseStudy.getValue().name.equals(caseStudyFanTempMonitor) || !useCentre || traces_lengthmult.firstElem < 600) {
                                // Now select the non-Markov result from all those available
                                for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
                                    MarkovLearningParameters rowHeader = parseMarkovParametersRowFromCSV(rowEntry.getKey());
                                    if (rowHeader.traceQuantity == traces_lengthmult.firstElem && rowHeader.sample == entryForCaseStudy.getKey()) {
                                        // First, evaluate non-Markov learners
                                        getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColOtherLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), validityOfCells,
                                                (column, columnText, Y) -> {
                                                    boolean learntOK = obtainStringValueFromCell(Y, RESULT_VALUES.E_SUCCESS, column).equals(LEARNING_OK.name);
                                                    double structural = obtainDoubleValueFromCell(Y, E_DIFF, column);
                                                    ResultsXAxis xValue = new ResultsXAxis(column.learner, rowHeader.traceQuantity, 0, false);
                                                    if (xValue.filter(entryForCaseStudy.getValue().name)) {
                                                        if (learntOK) {
                                                            gr_PerformanceOfLearners.add(xValue.toString(), structural);
                                                            double runtime = capToTimeout(obtainDoubleValueFromCell(Y, E_RUNTIME, column), timeoutValueObtained);// cap runtime to timeout, esp since earlier experimental runs could run longer than 4.5 hours (esp because they were not as frequently checking for a timeout).

                                                            if (runtime >= 1.0)
                                                                runtime = Math.log10(runtime);
                                                            gr_RuntimeOfLearners.add(xValue.toString(), runtime);
                                                            countsSuccess.computeIfAbsent(xValue, k -> new AtomicInteger(0)).incrementAndGet();
                                                        }
                                                        countsTotal.computeIfAbsent(xValue, k -> new AtomicInteger(0)).incrementAndGet();
                                                    }
                                                });

                                        // Second, evaluate Markov learning
                                        getAllValuesFromMapGivenRegexp(rowEntry.getValue(),
                                                column ->
                                                        (column.parameters.preset > 0) == useCentre &&
                                                                column.parameters.chunkLen == chunkSizeToEvaluate &&
                                                                column.learner == LearningAlgorithms.ScoringToApply.SCORING_MARKOV,
                                                validityOfCells,
                                                (column, columnText, Y) -> {
                                                    double runtime = capToTimeout(obtainDoubleValueFromCell(Y, E_RUNTIME, column), timeoutValueObtained);// cap runtime to timeout, esp since earlier experimental runs could run longer than 4.5 hours (esp because they were not as frequently checking for a timeout).
                                                    boolean learntOK = obtainStringValueFromCell(Y, RESULT_VALUES.E_SUCCESS, column).equals(LEARNING_OK.name);
                                                    ResultsXAxis xValue = new ResultsXAxis(column.learner, rowHeader.traceQuantity, chunkSizeToEvaluate, useCentre);
                                                    if (xValue.filter(entryForCaseStudy.getValue().name)) {
                                                        if (runtime >= 1.0)
                                                            runtime = Math.log10(runtime);

                                                        if (learntOK) {
                                                            gr_RuntimeOfLearners.add(xValue.toString(), runtime);
                                                            countsSuccess.computeIfAbsent(xValue, k -> new AtomicInteger(0)).incrementAndGet();
                                                        }
                                                        countsTotal.computeIfAbsent(xValue, k -> new AtomicInteger(0)).incrementAndGet();
                                                    }
                                                });
                                    }

                                }

                                for (Map.Entry<ResultsXAxis, AtomicInteger> entry : countsSuccess.entrySet())
                                    gr_SuccessPercentage.add(entry.getKey().toString(), (double) Math.round(100. * (double) entry.getValue().get() / countsTotal.get(entry.getKey()).intValue()));

                                String plot_filename_prefix = learningGroup.outPathPrefix + description + "_" + entryForCaseStudy.getValue().name + "_" + traces_lengthmult.firstElem + "_" +
                                        (useCentre ? "centre" : "no_cnt") + "_cl=" + chunkSizeToEvaluate;

                                final SquareBagPlot gr_StructuralDiffBest = new SquareBagPlot("Structural score, VH", "Structural Score, EDSM-Markov learner",
                                        new File(plot_filename_prefix + "_VH_structuraldiffBest.pdf"), 0, 1, true);
                                final SquareBagPlot gr_BcrDiffBest = new SquareBagPlot("BCR, VH", "BCR, EDSM-Markov learner",
                                        new File(plot_filename_prefix + "_VH_BCRBest.pdf"), 0.5, 1, true);
                                final WilcoxonPairedTest Wilcoxon_test_Structural = new WilcoxonPairedTest(new File(plot_filename_prefix + "_Wilcoxon_t_str.csv"));
                                final WilcoxonPairedTest Wilcoxon_Test_BCR = new WilcoxonPairedTest(new File(plot_filename_prefix + "_Wilcoxon_t_bcr.csv"));
                                final A_VarghaDelaney A12_test_Structural = new A_VarghaDelaney(new File(plot_filename_prefix + "_A12_str.csv"), 100);
                                final A_VarghaDelaney A12_test_BCR = new A_VarghaDelaney(new File(plot_filename_prefix + "_A12_bcr.csv"), 100);
                                // Now select the best result from all those available
                                final AtomicInteger diffReported = new AtomicInteger(0), bcrReported = new AtomicInteger(0);
                                final AtomicInteger diffAverageMarkov100 = new AtomicInteger(0), bcrAverageMarkov100 = new AtomicInteger(0);
                                final AtomicInteger diffAverageVH100 = new AtomicInteger(0), bcrAverageVH100 = new AtomicInteger(0);

                                FilterCollectionOfResultsForBestPerformingLearner report = new FilterCollectionOfResultsForBestPerformingLearner(-1, -1,
                                        rowHeader -> rowHeader.traceQuantity == traces_lengthmult.firstElem && rowHeader.sample == entryForCaseStudy.getKey(),
                                        columnParse -> (columnParse.parameters.preset > 0) == useCentre && columnParse.parameters.chunkLen == chunkSizeToEvaluate &&
                                                new ResultsXAxis(LearningAlgorithms.ScoringToApply.SCORING_MARKOV, traces_lengthmult.firstElem, chunkSizeToEvaluate, useCentre).filter(entryForCaseStudy.getValue().name),
                                        resultCSV, validityOfCells);

                                AtomicInteger bestDiffSum = new AtomicInteger(0);
                                AtomicInteger bestDiffCounter = new AtomicInteger(0);
                                Map<String, AtomicInteger> learnerToHowOftenBest = report.getResultForBestPerformingMarkovLearner(null, null,
                                        (pair) -> {
                                            double markov = pair.firstElem, vh_score = pair.secondElem;
                                            gr_StructuralDiffBest.add(vh_score, markov, null, null);
                                            A12_test_Structural.add(vh_score, markov);
                                            Wilcoxon_test_Structural.add(vh_score, markov);
                                            ResultsXAxis xValue = new ResultsXAxis(LearningAlgorithms.ScoringToApply.SCORING_MARKOV, traces_lengthmult.firstElem, chunkSizeToEvaluate, useCentre);
                                            gr_PerformanceOfLearners.add(xValue.toString(), markov);
                                            diffReported.addAndGet(1);
                                            diffAverageMarkov100.addAndGet((int) Math.round(markov * 100));
                                            diffAverageVH100.addAndGet((int) Math.round(vh_score * 100));

                                            bestDiffSum.addAndGet((int) Math.round(markov * 100));
                                            bestDiffCounter.incrementAndGet();
                                        },
                                        (pair) -> {
                                            double bcr = pair.firstElem, vh_bcr = pair.secondElem;
                                            gr_BcrDiffBest.add(vh_bcr, bcr, null, null);
                                            A12_test_BCR.add(vh_bcr, bcr);
                                            Wilcoxon_Test_BCR.add(vh_bcr, bcr);

                                            bcrReported.addAndGet(1);
                                            bcrAverageMarkov100.addAndGet((int) Math.round(bcr * 100));
                                            bcrAverageVH100.addAndGet((int) Math.round(vh_bcr * 100));
                                        }
                                );
                                if (diffReported.get() > 0) {// if filtering did not remove everything.
                                    if (diffReported.get() != entryForCaseStudy.getValue().trainingSamplesPerFSM) {
                                        String errorMessage = "Diff value not reported: got " + diffReported.get() + " values, expected " + entryForCaseStudy.getValue().trainingSamplesPerFSM +
                                                " for: " + traces_lengthmult.firstElem + " traces, chunklen: " + chunkSizeToEvaluate + " , useCentre=" + useCentre;
                                        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS)
                                            throw new IllegalStateException(errorMessage);
                                        System.out.println("WARNING: " + errorMessage);
                                    }
                                    if (bcrReported.get() != entryForCaseStudy.getValue().trainingSamplesPerFSM) {
                                        String errorMessage = "BCR value not reported: got " + bcrReported.get() + " values, expected " + entryForCaseStudy.getValue().trainingSamplesPerFSM +
                                                " for: " + traces_lengthmult.firstElem + " traces, chunklen: " + chunkSizeToEvaluate + " , useCentre=" + useCentre;
                                        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS)
                                            throw new IllegalStateException(errorMessage);
                                        System.out.println("WARNING: " + errorMessage);
                                    }
                                    StatisticalTestResult a12_diff = A12_test_Structural.obtainResultFromR(learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE);
                                    StatisticalTestResult wilcoxon_diff = Wilcoxon_test_Structural.obtainResultFromR(false);

                                    List<String> row = new ArrayList<>();
                                    row.add(entryForCaseStudy.getValue().name);
                                    row.add(Integer.toString(entryForCaseStudy.getValue().referenceGraph.getStateNumber()));
                                    row.add(Integer.toString(entryForCaseStudy.getValue().alphabetSize));
                                    row.add(Integer.toString(traces_lengthmult.firstElem));
                                    int traceLength = entryForCaseStudy.getValue().name.equals(caseStudyFanTempMonitorSingleTrace) ?
                                            entryForCaseStudy.getValue().actualLength :
                                            (traces_lengthmult.secondElem * entryForCaseStudy.getValue().referenceGraph.getStateNumber());
                                    row.add(Integer.toString(traceLength));
                                    row.add(useCentre ? "Y" : "");
                                    row.add(Integer.toString(chunkSizeToEvaluate - 1));
                                    row.add(Integer.toString(diffAverageMarkov100.get() / diffReported.get()));
                                    row.add(Integer.toString(bcrAverageMarkov100.get() / bcrReported.get()));

                                    row.add(Integer.toString(diffAverageVH100.get() / diffReported.get()));
                                    row.add(Integer.toString(bcrAverageVH100.get() / bcrReported.get()));

                                    NumberFormat f_A12 = new DecimalFormat("0.00");
                                    NumberFormat f_Wilcoxon = new DecimalFormat("0.00E00");

                                    if (a12_diff.valueValid) {
                                        row.add(f_A12.format(a12_diff.statistic));
                                        row.add(f_A12.format(a12_diff.confidence_lo));
                                        row.add(f_A12.format(a12_diff.confidence_hi));
                                        row.add(f_Wilcoxon.format(wilcoxon_diff.pvalue));
                                    } else
                                        for (int i = 0; i < 4; ++i)
                                            row.add("UNK");

                                    ResultsXAxis xValue = new ResultsXAxis(LearningAlgorithms.ScoringToApply.SCORING_MARKOV, traces_lengthmult.firstElem, chunkSizeToEvaluate, useCentre);

                                    // We are here for different values of chunklen
                                    if (xValue.addToSpreadsheet(entryForCaseStudy.getValue().name))
                                        outputStatistics.add(row);
                                    gr_StructuralDiffBest.reportResults(learningGroup.gr);
                                    gr_BcrDiffBest.reportResults(learningGroup.gr);
                                    A12_test_Structural.reportResults(learningGroup.gr, learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE);
                                    A12_test_BCR.reportResults(learningGroup.gr, learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE);
                                    Wilcoxon_test_Structural.reportResults(learningGroup.gr);
                                    Wilcoxon_Test_BCR.reportResults(learningGroup.gr);


//                            List<String> learners = new ArrayList<>(learnerToHowOftenBest.keySet());
//                            learners.sort((o1, o2) ->
//                                    learnerToHowOftenBest.get(o2).get() - learnerToHowOftenBest.get(o1).get());
//                            int average = bestDiffCounter.get() > 0 ? bestDiffSum.get() / bestDiffCounter.get() : 0;
//                            System.out.println("CASE STUDY: " + entryForCaseStudy.getValue().name + " centre: " + useCentre + " chunkLen: " + chunkSizeToEvaluate +
//                                    " with: " + traces_lengthmult.firstElem + ", " + traceLength + " , Best diff: " + average);
//
//                            for (String l : learners)
//                                System.out.println(l + " -> " + learnerToHowOftenBest.get(l).get());
                                }
                                progress.next();
                            }
                }
                ResultsXAxis[] xValues = countsTotal.keySet().toArray(new ResultsXAxis[0]);
                Arrays.sort(xValues);
                List<String> orderingXaxis = Arrays.stream(xValues).map(k -> k.toString()).collect(Collectors.toList());
                gr_PerformanceOfLearners.setOrderingOfLabels(orderingXaxis);
                gr_PerformanceOfLearners.reportResults(learningGroup.gr);
                gr_RuntimeOfLearners.setOrderingOfLabels(orderingXaxis);
                gr_RuntimeOfLearners.reportResults(learningGroup.gr);
                gr_SuccessPercentage.setOrderingOfLabels(orderingXaxis);
                gr_SuccessPercentage.reportResults(learningGroup.gr);
            }
            writeTEX(new File(learningGroup.outPathPrefix + description+"_statistics.tex"), outputStatistics, true);
        }
    }

    public static void fillInCaseStudyExperimentParameters(LearningExperimentGroupParameters learningGroup) {
        if (caseStudyInformationMap.isEmpty())
            for (int casestudy = 0; casestudy < caseStudies.length; casestudy++)
                if (whichCaseStudyToRun == null || whichCaseStudyToRun.isEmpty() || whichCaseStudyToRun.contains(caseStudies[casestudy])) {
                    if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS)
                        System.out.print("Loading " + caseStudies[casestudy] + " ...");
                    Configuration dotConfig = learningGroup.eval.config.copy();
                    dotConfig.setLabelKind(Configuration.LABELKIND.LABEL_STRING);
                    LearnerGraph reference = constructAutomatonForCaseStudy(caseStudies[casestudy], dotConfig, new Transform.InternStringLabel());
                    try {
                        WMethod.computeWSet_reducedmemory(reference);
                    } catch (WMethod.EquivalentStatesException ex) {
                        System.out.println("Equivalent states:");
                        for (EquivalenceClass<DeterministicDirectedSparseGraph.CmpVertex, LearnerGraphCachedData> eqClass : ex.getEquivalentStates())
                            System.out.println(eqClass.toString());
                        throw new IllegalArgumentException(ex);
                    }

                    double density = (double) reference.pathroutines.countEdges() / (reference.getStateNumber() * reference.getStateNumber());
                    int states = reference.getStateNumber();
                    if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS)
                        System.out.println("States: " + states + " , Alphabet: " + reference.getCache().getAlphabet().size() + " , Density: " + density + " done.");
                    Pair<Integer, Integer>[] traces_and_lengths = new Pair[]{
                            new Pair(1, reference.getCache().getAlphabet().size() * states),
                            new Pair(states, reference.getCache().getAlphabet().size()),
                            new Pair(states * states, reference.getCache().getAlphabet().size())
                    };
                    caseStudyInformationMap.put(casestudy, new CaseStudyInformation(caseStudies[casestudy], casestudy, reference, reference.pathroutines.computeAlphabet().size(), traces_and_lengths));
                    Map<Integer, double[]> chunkSizesToWeightsMinePump = new TreeMap<>();
                    chunkSizesToWeightsMinePump.put(3, new double[]{1.0, 2.0, 3.0, 4.0, 8.0, 16.0});
                    chunkSizesToWeightsMinePump.put(4, new double[]{0.5, 1.0, 2.0, 3.0, 4.0, 8.0, 12.0, 16.0});
                    chunkSizesToWeightsMinePump.put(5, new double[]{0.25, 0.5, 1.0});
                    chunkSizesToWeightsMinePump.put(6, new double[]{0.05, 0.1, 0.25});

                    Map<Integer, double[]> chunkSizesToWeightsFanTempMonitor = new TreeMap<>();
                    chunkSizesToWeightsFanTempMonitor.put(3, new double[]{1.0, 2.0, 3.0, 4.0, 8.0, 16.0});
                    chunkSizesToWeightsFanTempMonitor.put(4, new double[]{0.5, 1.0, 2.0, 3.0, 4.0, 8.0, 16.0});
                    chunkSizesToWeightsFanTempMonitor.put(5, new double[]{0.5, 1.0, 2.0, 4.0, 8.0});
                    chunkSizesToWeightsFanTempMonitor.put(6, new double[]{0.5, 1.0, 2.0, 4.0, 8.0});
                    chunkSizesToWeightsFanTempMonitor.put(7, new double[]{0.5, 1.0, 2.0, 4.0, 8.0});
                    switch (caseStudies[casestudy]) {
                        case "SmallTrain":
                            caseStudyInformationMap.get(casestudy).setChunkSizesAndWeightsToEvaluate(new int[]{3, 4});
                            break;
                        case "SSH":
                            caseStudyInformationMap.get(casestudy).setChunkSizesAndWeightsToEvaluate(new int[]{3, 4});
                            caseStudyInformationMap.get(casestudy).setWeightOfInconsistencies(new double[]{0.5, 1.0, 2.0, 3.0, 4.0, 8.0, 12.0, 16.0});
                            break;
                        case "CVS":
                            caseStudyInformationMap.get(casestudy).setChunkSizesAndWeightsToEvaluate(new int[]{3, 4});
                            caseStudyInformationMap.get(casestudy).setWeightOfInconsistencies(new double[]{0.5, 1.0, 2.0, 3.0, 4.0, 8.0, 12.0, 16.0});
                            break;
                        case "MinePump":
                            caseStudyInformationMap.get(casestudy).setChunkSizesAndWeightsToEvaluate(new int[]{3, 4});
                            caseStudyInformationMap.get(casestudy).setWeightOfInconsistenciesDependingOnChunkLen(chunkSizesToWeightsMinePump);
                            break;
                        case caseStudyFanTempMonitor:
                            caseStudyInformationMap.get(casestudy).setChunkSizesAndWeightsToEvaluate(new int[]{4, 5, 6, 7});
                            caseStudyInformationMap.get(casestudy).setWeightOfInconsistenciesDependingOnChunkLen(chunkSizesToWeightsFanTempMonitor);
                            break;
                        case caseStudyFanTempMonitorSingleTrace:
                            caseStudyInformationMap.get(casestudy).setChunkSizesAndWeightsToEvaluate(new int[]{3, 4, 5, 6, 7});
                            caseStudyInformationMap.get(casestudy).setWeightOfInconsistenciesDependingOnChunkLen(chunkSizesToWeightsFanTempMonitor);
                            caseStudyInformationMap.get(casestudy).setTransitionMatrixImplType(Configuration.STATETREE.STATETREE_ARRAY);// large PTA, use array. PTA is loaded by constructPTA of caseStudyInformation on request when needed.
                            caseStudyInformationMap.get(casestudy).traces_and_lengths = new Pair[]{
                                    new Pair(1, 797676 / states)};// bit of a cludge but 797676 is the actual length of the log however it is expressed here in proportion to the number of states.
                            caseStudyInformationMap.get(casestudy).trainingSamplesPerFSM = 1;// we only have one PTA here
                            caseStudyInformationMap.get(casestudy).actualLength = 797676;
                            break;
                        default:
                            break;// use default values
                    }
                }
    }
}
