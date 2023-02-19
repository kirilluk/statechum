package statechum.analysis.learning.experiments.PairSelection;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import org.junit.Test;

import statechum.Label;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.MarkovClassifierLG;
import statechum.analysis.learning.experiments.I2cexperiment;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.EDSM_MarkovLearner;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovParameters;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;

public class TestMarkov_i2c 
{
	@Test
	public void testLearneri2c() throws IOException
	{
		LearnerEvaluationConfiguration eval = UASExperiment.constructLearnerInitConfiguration();
		eval.config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);eval.config.setLearnerScoreMode(ScoreMode.ONLYOVERRIDE);
		eval.config.setOverride_maximalNumberOfStates(50*LearningAlgorithms.maxStateNumberMultiplier);
		eval.config.setOverride_usePTAMerging(false);
	
		final int chunkSize = 7;
	
		LearnerGraph initialPTA = new LearnerGraph(eval.config);
		initialPTA.paths.augmentPTA(I2cexperiment.loadTrace("resources/i2c_study/log10.txt",eval.getLabelConverter(),I2cexperiment.alphabetElementToConsiderErr), true, false,null);
		// The purpose of if-then below is to make it clear that an error transition will not be repeated - this was the only problem in the inferred model. 
		//LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(Arrays.asList(new String[]{"ifthenFSM graph1 A-!"+errElement+"->A-"+errElement+"->B-"+errElement+"->B-!"+errElement+"->A / P-"+errElement+"-#Q / P == THEN == B"}), initialPTA.pathroutines.computeAlphabet(), eval.config, eval.getLabelConverter()).toArray(new LearnerGraph[0]);
		//Transform.augmentFromIfThenAutomaton(initialPTA, null, ifthenAutomata, 1);// we only need  to augment our PTA once.
		MarkovParameters markovParameters = new MarkovParameters(0, chunkSize,true, 1, true,1,0,1);
		final MarkovModel m= new MarkovModel(chunkSize,true, true,true,false);
		new MarkovClassifierLG(m, initialPTA, null).updateMarkov(false);// construct Markov chain if asked for.
		initialPTA.clearColours();
		final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();
		EDSM_MarkovLearner markovLearner = new EDSM_MarkovLearner(eval,initialPTA,0,markovParameters,null);markovLearner.setMarkov(m);markovLearner.setChecker(checker);
	
		LearnerGraph graph = markovLearner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		LearnerGraph expected = new LearnerGraph(eval.config);AbstractPersistence.loadGraph("resources/i2c_study/outcome_i2c_chunk7.xml", expected,eval.getLabelConverter());expected.setName("expected");
		LearnerGraph expectedWithoutNegatives=LearningSupportRoutines.removeAllNegatives(expected);
		DifferentFSMException ex = WMethod.checkM(expectedWithoutNegatives, graph);
		if (ex != null)
			throw ex;
	}
}
