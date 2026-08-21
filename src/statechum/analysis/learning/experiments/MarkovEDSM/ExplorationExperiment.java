package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;

import java.io.File;
import java.io.IOException;
import java.util.Random;

import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.directoryNamePrefix;
import static statechum.analysis.learning.rpnicore.RandomPathGenerator.WALKTYPE.WALKTYPE_AIMFORTRANSITIONCOVER;
import static statechum.analysis.learning.rpnicore.RandomPathGenerator.WALKTYPE.WALKTYPE_AIMFORTRANSITIONCOVER_PREFERNONLOOP;

public class ExplorationExperiment {
    public static void main(String[] args) throws IOException {

        String outDir = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_EXPERIMENTRESULTS)+ File.separator+directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
        UASExperiment.mkDir(outDir);

        MarkovExperiment.LearningExperimentGroupParameters learningGroup = new MarkovExperiment.LearningExperimentGroupParameters();

        learningGroup.outPathPrefix = outDir;
        learningGroup.copyToPrefix = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_EXPERIMENTRESULTS)+File.separator+"copy_of_markov";
        learningGroup.moveToPrefix = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_EXPERIMENTRESULTS)+File.separator+"possibly_recompute_markov";

        learningGroup.eval = UASExperiment.constructLearnerInitConfiguration();
        learningGroup.eval.config.setTransitionMatrixImplType(Configuration.STATETREE.STATETREE_LINKEDHASH);// small automata hence no need for array STATETREE.STATETREE_ARRAY);
        //STATETREE_ARRAY);
        learningGroup.eval.config.setLearnerScoreMode(Configuration.ScoreMode.GENERAL_NOFULLMERGE);
        learningGroup.eval.config.setTimeOut(3600000L*6L);// timeout for tasks, in milliseconds, equivalent to 6hrs runtime.
        learningGroup.eval.config.setOverride_usePTAMerging(false);

        SGE_ExperimentRunner.configureCPUFreqNormalisation();

        boolean aveOrMax = true;// average divide by the divisor
        boolean penaliseMissingPaths = true;
        boolean pathsOrSets = true;
        double alphabetMultiplier = 2;
        final int chunkSizeToEvaluate = 3;
        final int states = 20;
        final int perStateSquaredDensity100 = MarkovExperiment.densityFromStateNumber(states)[0];
        final int sample = 20;
        final int trainingSample = 1;

        final int preset = 0;

        final int traceQuantityToUseV = 1;
        int traceLenMult= learningGroup.getTracesLengthmultBaseline(states).secondElem;
        int traceQuantityToUse = traceQuantityToUseV*learningGroup.getScalingFactor(states);

        final int wlen = 2,divisor = 4;
        final double weightOfInconsistencies = 0.5;
        final LearningAlgorithms.ScoringToApply learnerKind = LearningAlgorithms.ScoringToApply.SCORING_MARKOV;
        ProgressDecorator.LearnerEvaluationConfiguration ev = new ProgressDecorator.LearnerEvaluationConfiguration(learningGroup.eval);
        ev.config = learningGroup.eval.config.copy();
        ev.config.setOverride_maximalNumberOfStates(states * LearningAlgorithms.maxStateNumberMultiplier);

        E_MarkovTraceNum.MarkovTraceNumParameters parameters = new E_MarkovTraceNum.MarkovTraceNumParameters(learnerKind, states, alphabetMultiplier, perStateSquaredDensity100, sample, trainingSample);
        parameters.setTraceLengthMultiplier(traceLenMult);
        parameters.setExperimentID(traceQuantityToUse, learningGroup.traceLengthMultiplierMax, alphabetMultiplier);
        parameters.markovParameters.setMarkovParameters(preset, chunkSizeToEvaluate, pathsOrSets,
                new MarkovParameters.WeightAndOffsetOfInconsistencies(weightOfInconsistencies, 0), penaliseMissingPaths, aveOrMax, divisor, 0, wlen);

        LearnerGraph referenceGraph = new LearnerGraph(ev.config);
        MarkovExperiment.MarkovLearnerRunner learnerRunner = new MarkovExperiment.MarkovLearnerRunner(learningGroup.outPathPrefix, parameters, ev);
        AbstractPersistence.loadGraph(learnerRunner.filenameForAutomaton.toFileName(), referenceGraph, ev.getLabelConverter());

        E_MarkovTraceNum.MarkovTraceNumParameters par = parameters;
        int attemptCounter = 0;
        LearnerGraph pta = null;
        boolean generationReported = false;
        int bestCoverage = 0;

        par.walkType = WALKTYPE_AIMFORTRANSITIONCOVER_PREFERNONLOOP;
        par.explorationPreference = 0.6;par.selectionPenalty = 10;
        learnerRunner.constructWalkParameters();
        System.out.println("Generating " +learnerRunner.filenameForPTA.fileName);
        do {
            pta = new LearnerGraph(ev.config);
            RandomPathGenerator generator = new RandomPathGenerator(referenceGraph, new Random(par.trainingSample + attemptCounter), 5, null);
            if (par.walkType != null) {
                generator.setWalkType(par.walkType);
                generator.setExplorationPreferenceAndPenalty(par.explorationPreference, par.selectionPenalty);
            }
            // Using 2*par.traceQuantity reflects the original goal to generate an equal number of positive and
            // negative traces hence an input to generateRandomPosNeg was expected to be even.
            // We are not doing this now, instead only generating positive traces in quantity par.traceQuantity.
            generator.generateRandomPosNeg(2 * par.traceQuantity, 1, false, new RandomPathGenerator.RandomLengthGenerator() {

                @Override
                public int getLength() {
                    return (int) (par.traceLengthMultiplier * par.states);
                }

                @Override
                public int getPrefixLength(int len) {
                    return len;
                }
            }, true, false, null, null);

            pta.paths.augmentPTA(generator.getAllSequences(0));

            LearnerGraph trimmedGraph = LearningSupportRoutines.trimUncoveredTransitions(pta, referenceGraph);
            if (trimmedGraph.pathroutines.countEdges() == referenceGraph.pathroutines.countEdges())
                break;// achieved coverage

            int currentCoverage =  100 * trimmedGraph.pathroutines.countEdges()/referenceGraph.pathroutines.countEdges();
            if (currentCoverage > bestCoverage) {
                bestCoverage = currentCoverage;
                System.out.println("Best coverage: "+bestCoverage);
            }

            attemptCounter+=1000;
        }
        while (true);

        System.out.println("Generated");
    }
}
