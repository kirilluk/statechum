package statechum.analysis.learning.experiments;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.Callable;

import statechum.Configuration;
import statechum.Label;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ReferenceLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceDiff;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceLanguageBCR;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResult;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;
import statechum.model.testset.PTASequenceEngine;

public abstract class UASExperiment implements Callable<ThreadResult>
{
	protected final LearnerEvaluationConfiguration learnerInitConfiguration;
	protected LearnerGraph referenceGraph;
	protected String inputGraphFileName;
	
	public UASExperiment(LearnerEvaluationConfiguration eval)
	{
		learnerInitConfiguration = eval;
	}
	
	protected String constructFileName(String experimentSuffix)
	{
		return inputGraphFileName+"_"+experimentSuffix+".xml";
	}
	
	/** The outcome of learning might have been stored in a file from the previous run. For this reason, it makes sense to try to load it. 
	 * @throws IOException if the outcome of learning exists but cannot be loaded
	 */
	protected LearnerGraph loadOutcomeOfLearning(String experimentSuffix)
	{
		LearnerGraph outcome = null;
		String graphFileName = constructFileName(experimentSuffix);
		
    	if (new File(graphFileName).canRead())
    	{
	    	outcome = new LearnerGraph(learnerInitConfiguration.config);
    		try
			{
    			AbstractPersistence.loadGraph(graphFileName, outcome, learnerInitConfiguration.getLabelConverter());
			} 
    		catch (IOException e)
			{
				System.out.println("ERROR LOADING OUTCOME OF LEARNING \""+experimentSuffix+"\", exception text: "+e.getMessage());return null;
			}
    		catch (IllegalArgumentException e)
			{
				System.out.println("ERROR LOADING OUTCOME OF LEARNING \""+experimentSuffix+"\", exception text: "+e.getMessage());return null;
			}
    	}
    	
    	return outcome;
	}
	
	protected void saveOutcomeOfLearning(String experimentSuffix, LearnerGraph outcome) throws IOException
	{
		outcome.storage.writeGraphML(constructFileName(experimentSuffix));	
	}
	
	public static List<ReferenceLearner.ScoringToApply> listOfScoringMethodsToApply()
	{
		return Arrays.asList(ReferenceLearner.ScoringToApply.values());
	}

   public static LearnerGraph mergePTA(LearnerGraph initialPTA,Label labelToMerge, boolean buildAuxInfo)
   {
	   LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
	   List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMerge(initialPTA,Collections.<Label>emptyList(),
				Arrays.asList(new Label[]{labelToMerge}));
		if (initialPTA.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge,buildAuxInfo) < 0)
			throw new IllegalArgumentException("inconsistent initial PTA: vertices that are associated with the unique state cannot be merged in the PTA");
		return MergeStates.mergeCollectionOfVertices(initialPTA, null, verticesToMerge, buildAuxInfo);
   }
   

	/** Used by the learners to request a PTA to learn from. */
	public interface BuildPTAInterface
	{
		String kindOfPTA();
		LearnerGraph buildPTA() throws AugmentFromIfThenAutomatonException, IOException;
	}
	
	public static LearnerEvaluationConfiguration constructLearnerInitConfiguration()
	{
		LearnerEvaluationConfiguration learnerInitConfiguration = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		learnerInitConfiguration.setLabelConverter(new Transform.InternStringLabel());
        final Configuration learnerConfig = learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
        learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);
        learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);
        //learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
        learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);
        learnerConfig.setLearnerScoreMode(Configuration.ScoreMode.ONLYOVERRIDE);
       	learnerInitConfiguration.config.setUseConstraints(false);// do not use if-then during learning (enough to augment once)
        return learnerInitConfiguration;
	}

   public static Collection<List<Label>> computeEvaluationSet(LearnerGraph referenceGraph, int seqLength, int numberOfSeq)
   {
	   for(CmpVertex vert:referenceGraph.transitionMatrix.keySet()) 
		   if (!vert.isAccept())
			   throw new IllegalArgumentException("test set generation should not be attempted on an automaton with reject-states");
	   assert numberOfSeq > 0 && seqLength > 0;
		RandomPathGenerator pathGen = new RandomPathGenerator(referenceGraph,new Random(0),seqLength,referenceGraph.getInit());
		pathGen.generateRandomPosNeg(numberOfSeq, 1, false, null, true,true,null,null);
		return  pathGen.getAllSequences(0).getData(PTASequenceEngine.truePred);
	   /*
		Collection<List<Label>> evaluationTestSet = referenceGraph.wmethod.getFullTestSet(1);
		
		RandomPathGenerator pathGen = new RandomPathGenerator(referenceGraph,new Random(0),5,referenceGraph.getInit());
		int wPos=0;
		for(List<Label> seq:evaluationTestSet)
			if (referenceGraph.paths.tracePathPrefixClosed(seq) == AbstractOracle.USER_ACCEPTED) wPos++;
		pathGen.generateRandomPosNeg(2*(evaluationTestSet.size()-2*wPos), 1, false, null, true,false,evaluationTestSet,null);
		evaluationTestSet = pathGen.getAllSequences(0).getData(PTASequenceEngine.truePred);// we replacing the test set with new sequences rather than adding to it because existing sequences could be prefixes of the new ones.
		wPos = 0;
		for(List<Label> seq:evaluationTestSet) if (referenceGraph.paths.tracePathPrefixClosed(seq) == AbstractOracle.USER_ACCEPTED) wPos++;
		return evaluationTestSet;
	    */
   }

   /** Returns a test set to use for evaluation of the supplied reference graph using BCR. */
	public static Collection<List<Label>> buildEvaluationSet(LearnerGraph referenceGraph)
	{
		return computeEvaluationSet(referenceGraph,referenceGraph.getAcceptStateNumber()*3,LearningSupportRoutines.makeEven(referenceGraph.getAcceptStateNumber()*referenceGraph.pathroutines.computeAlphabet().size()));
	}
	
	public ScoresForGraph runExperimentUsingConventional(UASExperiment.BuildPTAInterface ptaSource, ReferenceLearner.ScoringToApply scoringMethod) throws AugmentFromIfThenAutomatonException, IOException
	{
		String experimentName = "conventional_"+ptaSource.kindOfPTA()+"_"+scoringMethod.toString();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(experimentName);
		if(actualAutomaton == null)
		{
			LearnerGraph pta = ptaSource.buildPTA();
 			ReferenceLearner learner = new ReferenceLearner(learnerInitConfiguration, pta,scoringMethod);
 			
 			actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 			saveOutcomeOfLearning(experimentName,actualAutomaton);
		}		
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		//Visualiser.updateFrame(actualAutomaton,referenceGraph);
		ScoresForGraph outcome = new ScoresForGraph(); 
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		return outcome;
	}
	
	/** Learns an automaton by requesting it from a supplier ptaSource.
	 * 
	 * @param ptaSource where to get automaton from
	 * @param scoringMethod how to compute scores
	 * @param uniqueLabel label identifying a transition from an initial state
	 * @return scores comparing the graph against the reference one.
	 * @throws AugmentFromIfThenAutomatonException
	 * @throws IOException
	 */
	public ScoresForGraph runExperimentUsingPremerge(UASExperiment.BuildPTAInterface ptaSource, ReferenceLearner.ScoringToApply scoringMethod, Label uniqueLabel) throws AugmentFromIfThenAutomatonException, IOException
	{// pre-merge and then learn. Generalised SICCO does not need a PTA and delivers the same results.
		String experimentName = "premerge_"+ptaSource.kindOfPTA()+"_"+scoringMethod.toString();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(experimentName);
		if(actualAutomaton == null)
		{
			LearnerGraph smallPta = UASExperiment.mergePTA(ptaSource.buildPTA(),uniqueLabel,false);
			ReferenceLearner learner = new ReferenceLearner(learnerInitConfiguration, smallPta,scoringMethod);

			actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 			saveOutcomeOfLearning(experimentName,actualAutomaton);
		}		
		
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		//Visualiser.updateFrame(actualAutomaton,referenceGraph);
		ScoresForGraph outcome =  new ScoresForGraph();
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		return outcome;
	}

	public ScoresForGraph runExperimentUsingConstraints(UASExperiment.BuildPTAInterface ptaSource, ReferenceLearner.ScoringToApply scoringMethod, Label uniqueLabel) throws AugmentFromIfThenAutomatonException, IOException
	{// conventional learning, but check each merger against the unique-label merge
		String experimentName = "constraints_"+ptaSource.kindOfPTA()+"_"+scoringMethod.toString();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(experimentName);
		if(actualAutomaton == null)
		{
			LearnerGraph pta = ptaSource.buildPTA();
 			ReferenceLearner learner = new ReferenceLearner(learnerInitConfiguration, pta,scoringMethod);
 			learner.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueLabel}));

 			actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 			saveOutcomeOfLearning(experimentName,actualAutomaton);
		}		
		
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		//Visualiser.updateFrame(actualAutomaton,referenceGraph);
		ScoresForGraph outcome =  new ScoresForGraph(); 
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		return outcome;
	}

}