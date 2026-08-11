package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.*;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.*;
import statechum.collections.MapWithSearch;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import static statechum.analysis.learning.DrawGraphs.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.*;
import static statechum.analysis.learning.rpnicore.FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0;

// EXPERIMENT WITH ACTUAL LEARNERS
public class E_MarkovCaseStudies {
    public static String [] caseStudies = new String[] {"CVS","SSH","MinePump","ATM","SmallTrain","FanTempMonitor"};

    public static LearnerGraph constructAutomatonForCaseStudy(String caseStudyName, Configuration config, final Transform.ConvertALabel conv) {
        switch(caseStudyName){
            case "CVS":
                // Derived from a similar model described in:
                // D. Lo and S. Khoo, “QUARK: Empirical assessment of automaton-based specification miners,” in Proceedings of the Working Conference on Reverse Engineering (WCRE’06). IEEE Computer Society, 2006, pp. 51–60.
                return FsmParserStatechum.buildLearnerGraph(
                        "q1-connect->q2-login->q3-setfiletype->q4-rename->q6-storefile->q5-setfiletype->q4-storefile->q7-appendfile->q5\nq3-makedir->q8-makedir->q8-logout->q16-disconnect->q1\nq3-changedirectory->q9-listnames->q10-delete->q10-changedirectory->q9\nq10-appendfile->q11-logout->q16\nq3-storefile->q11\nq3-listfiles->q13-retrievefile->q13-logout->q16\nq13-changedirectory->q14-listfiles->q13\nq7-logout->q16\nq6-logout->q16", "cvs",config,conv);
            case "SSH":
                // Almost verbatim from Verifying an implementation of SSH Poll, E.; Schubert, A. 2007. The two new parts are a 'comm' transition looping in the COMMUNICATION state and the 'reset' transition.
                return FsmParserStatechum.buildLearnerGraph(
                "DISCONNECTED -connect! -> WAIT_VERSION - version? -> VERSION_RECEIVED - version! -> WAIT_KEXINIT / WAIT_VERSION-version! ->VERSION_SENT-version? ->WAIT_KEXINIT /"+
                        "WAIT_KEXINIT -kexinit? -> KEXINIT_RECEIVED -kexinit!->KEXINIT_SENT - kexdh_init!-> WAIT_KEXDH_REPLY -kexdh_reply? ->WAIT_NEWKEYS /"+
                                "WAIT_KEXINIT -kexinit! ->KEXINIT_SENT_NOTR -kexinit? ->KEXINIT_SENT / KEXINIT_SENT_NOTR -kexdh_init! ->KEXDH_KEXINIT_SENT_NOTR - kexinit? -> KEXINIT_KEXDH_INIT_SENT - kexdh_reply? ->WAIT_NEWKEYS /"+
                                        "WAIT_NEWKEYS -newkeys? -> NEWKEYS_RECEIVED -newkeys! -> COMMUNICATION / WAIT_NEWKEYS -newkeys! ->NEWKEYS_SENT-newkeys? ->COMMUNICATION -comm->COMMUNICATION /"+
                                                "COMMUNICATION - kexinit? ->KEXINIT_RECEIVED / COMMUNICATION -kexinit! ->KEXINIT_SENT_NOTR /" +
                        "COMMUNICATION -reset->DISCONNECTED","ssh",config,conv);
            case "ATM":
                // The ATM is from Figure 5: An implementation of MTS in Fig. 4 of Existential Live Sequence Charts Revisited by German Sibay, Sebastian Uchitel and Victor Braberman,
                // with a ejectCard transition added from state 4 to 0.
                return FsmParserStatechum.buildLearnerGraph("0-pwd->1-verify->2-wait->3-verifying->4-ok->5-reqCash->6-getBalance->7-cash->8-updateBalance->5 / 7-notEnoughCash->5 / 4-wrongPwd->0 / 2-verifying->9-wait->4 / 5-ejectCard->0"
                        ,"ATM",config,conv);
            case "MinePump":
                // The Mine pump is from Figure 10: Final MTS of Existential Live Sequence Charts Revisited by German Sibay, Sebastian Uchitel and Victor Braberman
                return FsmParserStatechum.buildLearnerGraph(
                "0-tick->0 -medWater->1-lowWater->0 / 1-tick->1 -switchOn->2 -tick->2 - highWater->3-tick->3-medWater->4-tick->4-highWater->3 / 4 -lowWater->5-switchOff->0 /"+
                        "2-methAppears->6 / 4 -methAppears->6 - switchOff->7-tick->7-highWater->8-tick->8-medWater->7 / 8-methLeaves->9 -switchOn->3 / 7-methLeaves->10 -tick->10 -switchOn->2 /"+
                        "10-methAppears->11-tick->11-highWater->12-tick->12-medWater->11 / 11-lowWater->13-tick->13-medWater->11 / 11-methLeaves->1 / 13-methLeaves->0 / "+
                        "0 - methAppears->13 / 2-lowWater->15 / 1-highWater->9 / 1-methAppears->11 / 3-methAppears->14 - switchOff->8 /"+
                        "15 -switchOff ->16 -tick->16-medWater->17 -tick->17-highWater->9 / 17-methAppears->18-tick->18-methLeaves->17 / 18-highWater->19-tick->19 -methLeaves->9 /"+
                        "16-methAppears->20-tick->20 - medWater->18 / 20-methLeaves->16"
                        ,"MinePump",config,conv);
            case "SmallTrain":
                // This is the small train controller from Figure 3 of Scenarios, Goals, and State Machines: a Win-Win Partnership for Model Synthesis Christophe Damas, Bernard Lambeau, and Axel van Lamsweerde,
                // where the dead-end state 5 was removed.
                return FsmParserStatechum.buildLearnerGraph("0-start->2-stop->0 -a.pres->9-a.prop->11-e.open->1-close->0 / 0-open->1/2-a.pres->4-a.prop->8-e.stop->11","SmallTrain",config,conv);

            case "FanTempMonitor":
                LearnerGraph fanTempMonitorWithNegatives = new LearnerGraph(config);
                try {
                    AbstractPersistence.loadGraph("resources/i2c_study/i2c_outcome_correct", fanTempMonitorWithNegatives,conv);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
                fanTempMonitorWithNegatives.setName("FanTempMonitor");
                return LearningSupportRoutines.removeAllNegatives(fanTempMonitorWithNegatives);
            default:
                Configuration dotConfig = config.copy();dotConfig.setLabelKind(Configuration.LABELKIND.LABEL_STRING);
                String referenceDot;
                try {
                    referenceDot = Helper.loadFile(new File(
                            GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_CASESTUDIES)+
                                    File.separator+caseStudyName+".dot"));
                } catch (IOException e) {
                    throw new RuntimeException("Failed to load graph "+e);
                }
                LearnerGraph referenceGraph = FsmParserDot.buildLearnerGraph(referenceDot,dotConfig,
                        conv, true,true,USE_START0);
                referenceGraph.setName(caseStudyName);
                return referenceGraph;
        }
    }


    // When tuning results, I only need to run one, however I do wish to maintain the ordering of case studies, so that
    // experiments with a specific one do not replace experiments with others.
    public static Set<String> whichCaseStudyToRun = new TreeSet<>();
    static {
//        whichCaseStudyToRun.add("FanTempMonitor");
    }

    public static class MarkovLearningBaselineParameters extends MarkovLearningParameters {

        public MarkovLearningBaselineParameters(LearningAlgorithms.ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample) {
            super(l, argStates, argAlphabetMultiplier, perStateSquaredDensity10, argSample, argTrainingSample);
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
            referenceGraph = constructAutomatonForCaseStudy(caseStudies[par.sample],learnerInitConfiguration.config,learnerInitConfiguration.getLabelConverter());
//            Visualiser.updateFrame(referenceGraph, null);
//            Visualiser.waitForKey();
        }
    }


    static class CaseStudyInformation {
        public final String name;
        public final int sample;
        public final LearnerGraph referenceGraph;
        public final int alphabetSize;
        Pair<Integer, Integer> [] traces_and_lengths;

        public CaseStudyInformation(String name, int sample, LearnerGraph referenceGraph, int alphabetSize, Pair<Integer, Integer>[] traces_and_lengths) {
            this.name = name;this.sample = sample;
            this.referenceGraph = referenceGraph;
            this.alphabetSize = alphabetSize;
            this.traces_and_lengths = traces_and_lengths;
        }
    }

    public static void runExperiment(MarkovExperiment.LearningExperimentGroupParameters learningGroup) {
        int[] learnerExperiment = new int[]{0,1};
        final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(learningGroup.outPathPrefix + "results_casestudies.csv"), "results.csv");
        boolean aveOrMax = true;// average divide by the divisor
        final int trainingSamplesPerFSM = 40;// these are fixed automata hence we can try many different values to see how inference performs.
        boolean pathsOrSets = true, penaliseMissingPaths = true;
        String pathToCaseStudyFiles = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_CASESTUDIES);
        if (null == pathToCaseStudyFiles ||  pathToCaseStudyFiles.isEmpty())
            throw new RuntimeException("Cannot load any case studies: path to case studies is not defined");
        if (!Files.exists(Paths.get(pathToCaseStudyFiles)))
            throw new RuntimeException("Cannot load any case studies: path to case studies does not exist "+pathToCaseStudyFiles);

        Map<Integer,CaseStudyInformation> caseStudyInformationMap = new HashMap<>();
        for (int casestudy=0; casestudy<caseStudies.length; casestudy++)
            if (whichCaseStudyToRun == null || whichCaseStudyToRun.isEmpty() || whichCaseStudyToRun.contains(caseStudies[casestudy])) {
                System.out.print("Loading " + caseStudies[casestudy] + " ...");
                Configuration dotConfig = learningGroup.eval.config.copy();
                // Large amount of data - possibly need Array-based data structures
//                dotConfig.setTransitionMatrixImplType(Configuration.STATETREE.STATETREE_ARRAY);
                dotConfig.setLabelKind(Configuration.LABELKIND.LABEL_STRING);
                LearnerGraph reference = constructAutomatonForCaseStudy(caseStudies[casestudy], dotConfig, new Transform.InternStringLabel());
                double density = (double)reference.pathroutines.countEdges()/(reference.getStateNumber() * reference.getStateNumber());
                int states = reference.getStateNumber();
                System.out.println("States: "+states+" , Alphabet: "+reference.getCache().getAlphabet().size()+" , Density: "+density+" done.");
                Pair<Integer, Integer>[] traces_and_lengths = new Pair[]{
                        new Pair(1, reference.getCache().getAlphabet().size() * states),
                        new Pair(states, reference.getCache().getAlphabet().size()),
                        new Pair( states * states, reference.getCache().getAlphabet().size())
                };
                caseStudyInformationMap.put(casestudy,new CaseStudyInformation(caseStudies[casestudy], casestudy, reference, reference.pathroutines.computeAlphabet().size(), traces_and_lengths));
            }

        for (int casestudy=0; casestudy<caseStudies.length; casestudy++)
            if (whichCaseStudyToRun == null || whichCaseStudyToRun.isEmpty() || whichCaseStudyToRun.contains(caseStudies[casestudy]))
            {

                for (final int preset : learnerExperiment)
                    for (final Pair<Integer, Integer> traces_lengthmult : caseStudyInformationMap.get(casestudy).traces_and_lengths)
                    {
                        int states = caseStudyInformationMap.get(casestudy).referenceGraph.getStateNumber();
                        int traceQuantityToUse = traces_lengthmult.firstElem;
                        for (int trainingSample = 0; trainingSample < trainingSamplesPerFSM; ++trainingSample)
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
                                for (final int chunkSizeToEvaluate : learnerKind.isMarkov() ? new int[]{2,3,4} : new int[]{2})
                                    for (double weightOfInconsistencies : learnerKind.isMarkov() ?
                                            new double[]{0.25,0.5,1.0, 2.0, 3.0, 4.0, 6.0, 8.0}
                                            : new double[]{1.0})
                                        for (Pair<Integer, Integer> wlen_divisor : preset == 0 ? new Pair[]{new Pair(1, 1)} :
                                                new Pair[]{new Pair(1, 2), new Pair(1, 4), new Pair(2, 4), new Pair(2, 8)}) {
                                            ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
                                            ev.config = learningGroup.eval.config.copy();
                                            ev.config.setOverride_maximalNumberOfStates(states * 2);//LearningAlgorithms.maxStateNumberMultiplier);
                                            if (learnerKind.isMarkov())
                                                ev.config.setLearnerScoreMode(Configuration.ScoreMode.ONLYOVERRIDE);
                                            // Large amount of data - possibly need Array-based data structures
//                                            ev.config.setTransitionMatrixImplType(Configuration.STATETREE.STATETREE_ARRAY);
                                            MarkovLearningBaselineParameters parameters = new MarkovLearningBaselineParameters(learnerKind, states, 0, 0, casestudy, trainingSample);
                                            parameters.setTraceLengthMultiplier(traces_lengthmult.secondElem);
                                            parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, 0);
                                            parameters.markovParameters.setMarkovParameters(preset, chunkSizeToEvaluate, pathsOrSets,
                                                    new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies, 0), penaliseMissingPaths, aveOrMax, wlen_divisor.secondElem, 0, wlen_divisor.firstElem);
                                            parameters.setUsePrintf(learningGroup.experimentRunner.isInteractive());
//                                            parameters.disableReportMergeStatisticsWhenSolutionIsKnown();
//                                            parameters.setWalkType(RandomPathGenerator.WALKTYPE.WALKTYPE_AIMFORTRANSITIONCOVER_PREFERNONLOOP,0.6, 10);
                                            MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovLearnerRunnerForCaseStudies(parameters, ev);
                                            learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
                                            learningGroup.experimentRunner.submitTask(learnerRunner);
                                        }
                    }
            }

        learningGroup.experimentRunner.collectOutcomeOfExperiments(constructResultsCollector(resultCSV));

        if (learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || learningGroup.phase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {
            List<List<String>> outputStatistics = new ArrayList<>();
            outputStatistics.add(new ArrayList<>(Arrays.asList("Case study","States", "Alphabet", "Traces", "T. Length", "Centre", "Diff, M", "BCR, M", "Diff, VH", "BCR, VH","A12","A12 lo","A12 hi","Wilcoxon")));

            for (Map.Entry<Integer,CaseStudyInformation> entryForCaseStudy:caseStudyInformationMap.entrySet())
            {
                Pair<Integer, Integer> [] traces_and_lengths = entryForCaseStudy.getValue().traces_and_lengths;

                for (final boolean useCentre : new boolean[]{false,true})
                    for (final Pair<Integer, Integer> traces_lengthmult : traces_and_lengths) {
                        String plot_filename_prefix = learningGroup.outPathPrefix + "casestudies_" + entryForCaseStudy.getValue().name + "_" + traces_lengthmult.firstElem + "_" +
                                (useCentre ? "centre" : "no_cnt");

                        Map<String, AtomicInteger> learnerToHowOftenBest = new HashMap<>();
                        final SquareBagPlot gr_StructuralDiffBest = new SquareBagPlot("Structural score, VH", "Structural Score, EDSM-Markov learner",
                                new File(plot_filename_prefix + "_VH_structuraldiffBest.pdf"), 0, 1, true);
                        final SquareBagPlot gr_BcrDiffBest = new SquareBagPlot("BCR, VH", "BCR, EDSM-Markov learner",
                                new File(plot_filename_prefix + "_VH_BCRBest.pdf"), 0.5, 1, true);
                        final DrawGraphs.WilcoxonPairedTest Wilcoxon_test_Structural = new DrawGraphs.WilcoxonPairedTest(new File(plot_filename_prefix + "_Wilcoxon_t_str.csv"));
                        final DrawGraphs.WilcoxonPairedTest Wilcoxon_Test_BCR = new DrawGraphs.WilcoxonPairedTest(new File(plot_filename_prefix + "_Wilcoxon_t_bcr.csv"));
                        final DrawGraphs.A_VarghaDelaney A12_test_Structural = new DrawGraphs.A_VarghaDelaney(new File(plot_filename_prefix + "_A12_str.csv"), 100);
                        final DrawGraphs.A_VarghaDelaney A12_test_BCR = new DrawGraphs.A_VarghaDelaney(new File(plot_filename_prefix + "_A12_bcr.csv"), 100);
                        // Now select the best result from all those available
                        final AtomicInteger diffReported = new AtomicInteger(0), bcrReported = new AtomicInteger(0);
                        final AtomicInteger diffAverageMarkov100 = new AtomicInteger(0), bcrAverageMarkov100 = new AtomicInteger(0);
                        final AtomicInteger diffAverageVH100 = new AtomicInteger(0), bcrAverageVH100 = new AtomicInteger(0);

                        FilterCollectionOfResultsForBestPerformingLearner report = new FilterCollectionOfResultsForBestPerformingLearner(-1, -1,
                                rowHeader -> rowHeader.traceQuantity == traces_lengthmult.firstElem  && rowHeader.sample == entryForCaseStudy.getKey(),
                                columnParse -> (columnParse.parameters.preset > 0) == useCentre,
                                resultCSV);
                        report.getResultForBestPerformingMarkovLearner(null, null,
                                (pair) -> {
                                    double markov = pair.firstElem, vh_score = pair.secondElem;
                                    gr_StructuralDiffBest.add(vh_score, markov, null, null);
                                    A12_test_Structural.add(vh_score, markov);
                                    Wilcoxon_test_Structural.add(vh_score, markov);

                                    diffReported.addAndGet(1);
                                    diffAverageMarkov100.addAndGet((int)Math.round(markov*100));
                                    diffAverageVH100.addAndGet((int)Math.round(vh_score*100));
                                },
                                (pair) -> {
                                    double bcr = pair.firstElem, vh_bcr = pair.secondElem;
                                    gr_BcrDiffBest.add(vh_bcr, bcr, null, null);
                                    A12_test_BCR.add(vh_bcr, bcr);
                                    Wilcoxon_Test_BCR.add(vh_bcr, bcr);

                                    bcrReported.addAndGet(1);
                                    bcrAverageMarkov100.addAndGet((int)Math.round(bcr*100));
                                    bcrAverageVH100.addAndGet((int)Math.round(vh_bcr*100));
                                }
                            );

                        if (diffReported.get() != trainingSamplesPerFSM)
                            throw new IllegalArgumentException("Diff value not reported");
                        if (bcrReported.get() != trainingSamplesPerFSM)
                            throw new IllegalArgumentException("BCR value not reported");

                        StatisticalTestResult a12_diff = A12_test_Structural.obtainResultFromR();
                        StatisticalTestResult wilcoxon_diff = Wilcoxon_test_Structural.obtainResultFromR();

                        List<String> row = new ArrayList<>();
                        row.add(entryForCaseStudy.getValue().name);
                        row.add(Integer.toString(entryForCaseStudy.getValue().referenceGraph.getStateNumber()));
                        row.add(Integer.toString(entryForCaseStudy.getValue().alphabetSize));
                        row.add(Integer.toString(traces_lengthmult.firstElem));
                        row.add(Integer.toString(traces_lengthmult.secondElem* entryForCaseStudy.getValue().referenceGraph.getStateNumber()));
                        row.add(useCentre?"Y":"");

                        row.add(Integer.toString(diffAverageMarkov100.get()/diffReported.get()));
                        row.add(Integer.toString(bcrAverageMarkov100.get()/bcrReported.get()));

                        row.add(Integer.toString(diffAverageVH100.get()/diffReported.get()));
                        row.add(Integer.toString(bcrAverageVH100.get()/bcrReported.get()));

                        NumberFormat f_A12 = new DecimalFormat("0.00");
                        NumberFormat f_Wilcoxon = new DecimalFormat("0.00E00");

                        row.add(f_A12.format(a12_diff.statistic));row.add(f_A12.format(a12_diff.confidence_lo));row.add(f_A12.format(a12_diff.confidence_hi));
                        row.add(f_Wilcoxon.format(wilcoxon_diff.pvalue));
                        outputStatistics.add(row);

                        gr_StructuralDiffBest.reportResults(learningGroup.gr);gr_BcrDiffBest.reportResults(learningGroup.gr);
                        A12_test_Structural.reportResults(learningGroup.gr);A12_test_BCR.reportResults(learningGroup.gr);
                        Wilcoxon_test_Structural.reportResults(learningGroup.gr);Wilcoxon_Test_BCR.reportResults(learningGroup.gr);

                        List<String> learners = new ArrayList<>(learnerToHowOftenBest.keySet());
                        learners.sort((o1, o2) ->
                                learnerToHowOftenBest.get(o2).get() - learnerToHowOftenBest.get(o1).get());
                        for (String l : learners)
                            System.out.println(l + " -> " + learnerToHowOftenBest.get(l).get());
                    }


            }
            DrawGraphs.writeTEX(new File(learningGroup.outPathPrefix + "casestudies_statistics.tex"),outputStatistics,true);
        }
    }
}
