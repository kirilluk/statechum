package statechum.analysis.learning.experiments.com6911;

import statechum.Configuration;
import statechum.Label;
import statechum.StringLabel;
import statechum.Trace;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.util.OutputUtil;

import java.io.*;
import java.util.*;

import static statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;
import static statechum.analysis.learning.experiments.com6911.GenerateAutomata.graphmlExt;
import static statechum.analysis.learning.experiments.com6911.GenerateRandomWalks.walksExt;
import static statechum.analysis.learning.util.OutputUtil.dotGraph;

public class RunLearner {
    public static ProgressDecorator.LearnerEvaluationConfiguration constructLearnerInitConfiguration() {
        Configuration config = Configuration.getDefaultConfiguration().copy();
        ProgressDecorator.LearnerEvaluationConfiguration learnerInitConfiguration = new ProgressDecorator.LearnerEvaluationConfiguration(config);
        final Configuration learnerConfig = learnerInitConfiguration.config;
        learnerConfig.setGeneralisationThreshold(0);
        learnerConfig.setTransitionMatrixImplType(Configuration.STATETREE.STATETREE_ARRAY);
        learnerConfig.setAlwaysUseTheSameMatrixType(false);// permits computations to switch transition matrix type depending on matrix size.
        learnerConfig.setAskQuestions(false);
        learnerConfig.setDebugMode(false);
        learnerConfig.setLearnerScoreMode(Configuration.ScoreMode.GENERAL_NOFULLMERGE);
        learnerInitConfiguration.config.setUseConstraints(false);
        return learnerInitConfiguration;
    }

    public static Collection<Trace> loadTraces(File file) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(file));
        String line = null;
        Transform.ConvertALabel converter = new Transform.InternStringLabel();
        Collection<Trace> outcome = new ArrayList<>();
        while ((line = reader.readLine()) != null) {
            boolean negative = false;
            switch (line.charAt(0)) {
                case '+':
                    break;
                case '-':
                    negative = true;
                    break;
                default:
                    throw new IllegalArgumentException("Invalid start of string, should be either '+' or '-'");
            }
            List<Label> trace = new ArrayList<>();
            for (String s : line.substring(2).split(","))
                trace.add(converter.convertLabelToLabel(new StringLabel(s)));
            outcome.add(new Trace(trace, negative));
        }

        return outcome;
    }
    public final static String learntPrefix="learnt-";

    public static void main(String[] args) throws IOException {
        if (args.length < 3)
            throw new IllegalArgumentException("Usage: <WALKS DIRECTORY> <TARGET DIRECTORY> <learner>");
        if (!new File(args[0]).isDirectory())
            throw new IllegalArgumentException("Path " + args[0] + " is not a directory");
        if (!new File(args[1]).isDirectory())
            throw new IllegalArgumentException("Path " + args[1] + " is not a directory");
        LearningAlgorithms.ScoringToApply scoring;

        switch(args[2]) {
            case "KTPTL2":
                scoring = ScoringToApply.SCORING_PTAK_ALL_2;break;
            case "K0":
                scoring = ScoringToApply.SCORING_PTAK_0;break;
            case "K1":
                scoring = ScoringToApply.SCORING_PTAK_1;break;
            case "K2":
                scoring = ScoringToApply.SCORING_PTAK_2;break;
            case "K3":
                scoring = ScoringToApply.SCORING_PTAK_3;break;
            case "E0":
                scoring = ScoringToApply.SCORING_EDSM;break;
            default:
                throw new IllegalArgumentException("Unknown scoring method "+args[2]);
        }

        for (File file : new File(args[0]).listFiles((dir, name) -> name.endsWith(walksExt))) {
            ProgressDecorator.LearnerEvaluationConfiguration learnerInitConfiguration = constructLearnerInitConfiguration();
            UASExperiment.ScoringModeScore scoringMethod = new UASExperiment.ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE, scoring);

            final LearnerGraph pta = new LearnerGraph(learnerInitConfiguration.config);
            for(Trace trace:loadTraces(file))
                pta.paths.augmentPTA(trace.getList(),pta.getInit(),!trace.negative,false,null);

            Learner learner = LearningAlgorithms.constructLearner(learnerInitConfiguration, pta, scoringMethod.scoringMethod, scoringMethod.scoringForEDSM, null);
            LearnerGraph learntGraph = learner.learnMachine(new LinkedList<>(), new LinkedList<>());
            LearnerGraph actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
            String outputFileName = args[1] + File.separator + learntPrefix + file.getName().substring(0,file.getName().length()-walksExt.length());
            actualAutomaton.storage.writeGraphML(outputFileName+graphmlExt);
            OutputUtil.write(dotGraph(actualAutomaton.pathroutines.getGraph()).toString(),new File(outputFileName+".dot"));
        }
    }
}