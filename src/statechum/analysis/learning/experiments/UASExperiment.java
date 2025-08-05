package statechum.analysis.learning.experiments;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.Callable;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Label;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.StateMergingStatistics;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ReferenceLearner;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceDiff;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceLanguageBCR;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResult;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;
import statechum.collections.MapWithSearch;

public abstract class UASExperiment<PARS extends ThreadResultID,TR extends ThreadResult> implements Callable<TR>
{
	protected final LearnerEvaluationConfiguration learnerInitConfiguration;
	protected LearnerGraph referenceGraph;
	protected String graphFileNameDir;
	public final PARS par;
	
	protected boolean alwaysRunExperiment = false;
	
	public void setAlwaysRunExperiment(boolean b) 
	{
		alwaysRunExperiment = b;
	}

	public UASExperiment(PARS parameters, LearnerEvaluationConfiguration eval, String directoryNamePrefix)
	{
		par = parameters;
		learnerInitConfiguration = eval;
		String outDir = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP)+File.separator+directoryNamePrefix+File.separator+"experimentdata"+File.separator;
		mkDir(outDir);
		graphFileNameDir = outDir + File.separator;
	}
	
	public static void mkDir(String path)
	{
		if (path == null || path.isEmpty())
			return;
        mkDir(new java.io.File(path).getParent());// create a parent directory
		if (!new java.io.File(path).isDirectory())
		{
			new java.io.File(path).mkdir();// we do not check whether creation of a directory was successful because it seems that where multiple threads are creating the same directory, at least one attempt fails.
		}
	}
	
	@Override
	public TR call() throws Exception 
	{
		TR outcome = null;
		outcome = runexperiment();
		/*
		try
		{
		}
		catch(Exception ex)
		{
			System.err.println("Exception in task: "+par.getRowID()+"/"+par.getColumnID());
			ex.printStackTrace();
			outcome = null;
		}
		*/
		return outcome;
	}
	
	/** Responsible for running an experiment. */
	public abstract TR runexperiment() throws Exception;
	
	/** The outcome of learning might have been stored in a file from the previous run. For this reason, it makes sense to try to load it. 
	 */
	protected LearnerGraph loadOutcomeOfLearning(String graphPrefix)
	{
		LearnerGraph outcome = null;
		if (!alwaysRunExperiment)
		{
			String graphFileName = SGE_ExperimentRunner.RunSubExperiment.constructFileName(graphFileNameDir,graphPrefix,par);
			
	    	if (new File(graphFileName).canRead())
	    	{
		    	outcome = new LearnerGraph(learnerInitConfiguration.config);
	    		try
				{
	    			AbstractPersistence.loadGraph(graphFileName, outcome, learnerInitConfiguration.getLabelConverter());
				} 
	    		catch (IOException | IllegalArgumentException e)
				{
					System.out.println("ERROR LOADING OUTCOME OF LEARNING \""+graphFileName+"\", exception text: "+e.getMessage());return null;
				}
            }
		}    	
    	return outcome;
	}
	
	public void saveGraph(String graphPrefix, LearnerGraph outcome) throws IOException
	{
		if (!Boolean.parseBoolean(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.SGE_DISABLEGRAPHSAVE)))
			outcome.storage.writeGraphML(SGE_ExperimentRunner.RunSubExperiment.constructFileName(graphFileNameDir,graphPrefix,par));	
	}
	
	public static List<ScoringToApply> listOfScoringMethodsToApplyThatDependOnEDSMScoring()
	{
		return Arrays.asList(ScoringToApply.SCORING_EDSM,
                //ScoringToApply.SCORING_EDSM_1, // since we are talking of rejecting pairs with a score _less_ than the specified value, value of 0 and 1 are equivalent.
                //ScoringToApply.SCORING_EDSM_2,
                //ScoringToApply.SCORING_EDSM_3,
                ScoringToApply.SCORING_EDSM_4,//ScoringToApply.SCORING_EDSM_5,
                //ScoringToApply.SCORING_EDSM_6,
                //ScoringToApply.SCORING_EDSM_7,ScoringToApply.SCORING_EDSM_8
                ScoringToApply.SCORING_SICCO

                // SICCO_PTA performs the same as SICCO, RECURSIVE and RED perform very badly.
                // EDSM does not perform as well as SICCO and in particular, its performance is dependent on the threshold whereas for SICCO it does not.
                //,ScoringToApply.SCORING_SICCO_PTA,ScoringToApply.SCORING_SICCO_PTARECURSIVE, ScoringToApply.SCORING_SICCO_RED
        );
	}

	public static List<ScoringToApply> listOfScoringMethodsToApplyThatDoNotDependOnEDSMScoring()
	{
		return Arrays.asList(ScoringToApply.SCORING_PTAK_1,ScoringToApply.SCORING_PTAK_2,ScoringToApply.SCORING_PTAK_3,ScoringToApply.SCORING_PTAK_4,
                ScoringToApply.SCORING_PTAK_ALL_1,ScoringToApply.SCORING_PTAK_ALL_2,ScoringToApply.SCORING_PTAK_ALL_3,ScoringToApply.SCORING_PTAK_ALL_4);
	}

	public static List<ScoringToApply> listOfTraditionalKTailsMethods()
	{
		return Arrays.asList(ScoringToApply.SCORING_KT_1,ScoringToApply.SCORING_KT_2,ScoringToApply.SCORING_KT_3,ScoringToApply.SCORING_KT_4);
	}
	
	public static LearnerGraph mergePTA(LearnerGraph initialPTA,Label labelToMerge, boolean buildAuxInfo)
	{
	   LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<>();
	   List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMergeForPathsFrom(initialPTA,labelToMerge); 
			   //LearningSupportRoutines.buildVerticesToMerge(initialPTA,Collections.<Label>emptyList(),Arrays.asList(new Label[]{labelToMerge}));
		if (initialPTA.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge,buildAuxInfo) < 0)
			throw new IllegalArgumentException("inconsistent initial PTA: vertices that are associated with the unique state cannot be merged in the PTA");
		return MergeStates.mergeCollectionOfVertices(initialPTA, null, verticesToMerge, null, buildAuxInfo);
	}

	/** Used by the learners to request a PTA to learn from. */
	public interface BuildPTAInterface
	{
		String kindOfPTA();
		LearnerGraph buildPTA() throws AugmentFromIfThenAutomatonException, IOException;
	}

	public static String namePTA="pta", namePTABEFORECENTRE="pbc",nameOUTCOME="outcome";
	
	public static class ScoringModeScore
	{
		public final Configuration.ScoreMode scoringForEDSM;
		public final ScoringToApply scoringMethod;
		
		public ScoringModeScore(Configuration.ScoreMode a,ScoringToApply b)
		{
			scoringForEDSM = a;scoringMethod = b;
		}
	}
	
	public static LearnerEvaluationConfiguration constructLearnerInitConfiguration()
	{
		LearnerEvaluationConfiguration learnerInitConfiguration = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		learnerInitConfiguration.setLabelConverter(new Transform.InternStringLabel());
        final Configuration learnerConfig = learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
        learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);
        learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
        learnerConfig.setAlwaysUseTheSameMatrixType(false);// permits computations to switch transition matrix type depending on matrix size.
        learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);
        learnerConfig.setLearnerScoreMode(ScoreMode.GENERAL_NOFULLMERGE);
       	learnerInitConfiguration.config.setUseConstraints(false);// do not use if-then during learning (enough to augment once)
        return learnerInitConfiguration;
	}

	/** The reason behind using ptaSource is that where it is expensive to build a PTA, it will only be requested if the learner has not stored it or store the outcome of inference somewhere. 
	 * The argument <em>experimentID</em> is to ensure each experiment gets a unique name. Different experiments have different parameters so it is not realistic to expect them all to include scoring methods.
	 * For this reason, both <em>scoringMethod</em> and <em>scoringForEDSM</em> are arguments.
	 */
	public ScoresForGraph runExperimentUsingConventional(UASExperiment.BuildPTAInterface ptaSource, StateMergingStatistics redReducer, ThreadResultID experimentID, ScoringToApply scoringMethod,Configuration.ScoreMode scoringForEDSM) throws AugmentFromIfThenAutomatonException, IOException
	{
		String experimentName = experimentID.getRowID()+","+experimentID.getColumnID();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(nameOUTCOME);
		int ptaTotalNodes = 0, ptaTailNodes = 0;
		long runTime = 0;
		if(actualAutomaton == null)
		{
			LearnerGraph pta = ptaSource.buildPTA();
			ptaTotalNodes = pta.getStateNumber();ptaTailNodes = pta.getLeafStateNumber();
 			Learner learner = LearningAlgorithms.constructLearner(learnerInitConfiguration, pta,scoringMethod, scoringForEDSM, redReducer);
 			long startTime = LearningSupportRoutines.getThreadTime();
 			LearnerGraph learntGraph = learner.learnMachine(new LinkedList<>(), new LinkedList<>());
 			actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
 			runTime = LearningSupportRoutines.getThreadTime()-startTime;
 			
 			saveGraph(nameOUTCOME,actualAutomaton);
		}
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		ScoresForGraph outcome = new ScoresForGraph(); 
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		if (redReducer != null)
		{
			outcome.invalidMergers = redReducer.reportInvalidMergers();outcome.missedMergers = redReducer.reportMissedMergers();
		}
		outcome.whetherLearningSuccessfulOrAborted = actualAutomaton.getLearningAbortedReason();
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		outcome.ptaTotalNodes=ptaTotalNodes;outcome.ptaTailNodes = ptaTailNodes;
		outcome.executionTime = runTime;
		return outcome;
	}

	public ScoresForGraph runExperimentUsingConventionalInTwoSteps(UASExperiment.BuildPTAInterface ptaSource, StateMergingStatistics redReducer, ThreadResultID experimentID, ScoringToApply scoringMethodA,ScoringToApply scoringMethodB,Configuration.ScoreMode scoringForEDSM) throws AugmentFromIfThenAutomatonException, IOException
	{
		String experimentName = experimentID.getRowID()+","+experimentID.getColumnID();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(nameOUTCOME);
		int ptaTotalNodes = 0, ptaTailNodes = 0;
		long runTime = 0;
		int limitedSelfLoopsUsed = 0,totalMergers = 0;
		if(actualAutomaton == null)
		{
			LearnerGraph pta = ptaSource.buildPTA();
			ptaTotalNodes = pta.getStateNumber();ptaTailNodes = pta.getLeafStateNumber();
			Learner learnerStepA = LearningAlgorithms.constructLearner(learnerInitConfiguration, pta,scoringMethodA, scoringForEDSM, redReducer);
			long startTime = LearningSupportRoutines.getThreadTime();
			LearnerGraph learntGraph = learnerStepA.learnMachine(new LinkedList<>(), new LinkedList<>());
			learntGraph.clearColours();
			if (learnerStepA instanceof ReferenceLearner) {
				limitedSelfLoopsUsed += ((ReferenceLearner) learnerStepA).getLimitedSelfLoopCount();
				totalMergers+=((ReferenceLearner) learnerStepA).getTotalMergers();
			}

			if (scoringMethodB != null) {
				Learner learnerStepB = LearningAlgorithms.constructLearner(learnerInitConfiguration, learntGraph, scoringMethodB, scoringForEDSM, redReducer);
				if (learnerStepB instanceof ReferenceLearner)
					((ReferenceLearner) learnerStepB).setPtaWithProgressProperty(pta);

				learntGraph = learnerStepB.learnMachine(new LinkedList<>(), new LinkedList<>());
				if (learnerStepB instanceof ReferenceLearner) {
					limitedSelfLoopsUsed += ((ReferenceLearner) learnerStepB).getLimitedSelfLoopCount();
					totalMergers += ((ReferenceLearner) learnerStepB).getTotalMergers();
				}
			}
			actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
			runTime = LearningSupportRoutines.getThreadTime()-startTime;

			saveGraph(nameOUTCOME,actualAutomaton);
		}
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		ScoresForGraph outcome = new ScoresForGraph();
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		outcome.invalidMergers = (double)limitedSelfLoopsUsed/totalMergers;
//		if (redReducer != null)
//		{
//			outcome.invalidMergers = redReducer.reportInvalidMergers();outcome.missedMergers = redReducer.reportMissedMergers();
//		}

		outcome.whetherLearningSuccessfulOrAborted = actualAutomaton.getLearningAbortedReason();
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		outcome.ptaTotalNodes=ptaTotalNodes;outcome.ptaTailNodes = ptaTailNodes;
		outcome.executionTime = runTime;
		return outcome;
	}

	public ScoresForGraph runExperimentUsingConventionalWithUniqueLabel(UASExperiment.BuildPTAInterface ptaSource, StateMergingStatistics redReducer, ThreadResultID experimentID, ScoringToApply scoringMethod, Configuration.ScoreMode scoringForEDSM, Label uniqueLabel) throws AugmentFromIfThenAutomatonException, IOException
	{
		String experimentName = experimentID.getRowID()+","+experimentID.getColumnID();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(nameOUTCOME);
		int ptaTotalNodes = 0, ptaTailNodes = 0;
		long runTime = 0;
		if(actualAutomaton == null)
		{
			LearnerGraph ptaToLearnFrom = ptaSource.buildPTA();
			long startTime = LearningSupportRoutines.getThreadTime();
			LearnerGraph smallPta = UASExperiment.mergePTA(ptaToLearnFrom,uniqueLabel,false);
			Learner learner = new LearningAlgorithms.LearnerWithUniqueFromInitial(LearningAlgorithms.constructLearner(learnerInitConfiguration, smallPta,scoringMethod, scoringForEDSM, redReducer), smallPta, uniqueLabel);
			ptaTotalNodes = smallPta.getStateNumber();ptaTailNodes = smallPta.getLeafStateNumber();
 			LearnerGraph learntGraph = learner.learnMachine(new LinkedList<>(), new LinkedList<>());
 			actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
 			runTime = LearningSupportRoutines.getThreadTime()-startTime;

 			saveGraph(nameOUTCOME,actualAutomaton);
		}
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		ScoresForGraph outcome = new ScoresForGraph(); 
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		if (redReducer != null)
		{
			outcome.invalidMergers = redReducer.reportInvalidMergers();outcome.missedMergers = redReducer.reportMissedMergers();
		}
		outcome.whetherLearningSuccessfulOrAborted = actualAutomaton.getLearningAbortedReason();
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		outcome.ptaTotalNodes=ptaTotalNodes;outcome.ptaTailNodes = ptaTailNodes;
		outcome.executionTime = runTime;
		return outcome;
	}

	/** Learns an automaton after requesting it from a supplier ptaSource. Using the unique label, 
	 * turns a PTA into a graph with loops and the proceeds to learn using generalised merger routines.
	 * 
	 * @param ptaSource where to get automaton from
	 * @param useLearnerUnique whether to use a learner taking a unique transition from an initial state into account or not. The difference is that with unique from initial, we do not actually start merging states starting from the initial state, we start merging from the target state of that unique transition and then find a suitable match (in the learnt graph) for the original initial state.
	 * @param scoringMethod how to compute scores
	 * @param uniqueLabel label identifying a transition from an initial state
	 * @return scores comparing the graph against the reference one.
	 * @throws AugmentFromIfThenAutomatonException
	 * @throws IOException
	 */
	public ScoresForGraph runExperimentUsingPremerge(UASExperiment.BuildPTAInterface ptaSource, StateMergingStatistics redReducer, ThreadResultID experimentID, boolean useLearnerUnique, ScoringToApply scoringMethod, Configuration.ScoreMode scoringForEDSM, Label uniqueLabel) throws AugmentFromIfThenAutomatonException, IOException
	{// pre-merge and then learn. Generalised SICCO does not need a PTA and delivers the same results.
		String experimentName = experimentID.getRowID()+","+experimentID.getColumnID();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(nameOUTCOME);
		double fanoutPos=0, fanoutNeg = 0;
		int ptaStateNumber=0;
		int ptaTotalNodes = 0, ptaTailNodes = 0;
		long runTime = 0;
		if(actualAutomaton == null)
		{
			LearnerGraph ptaToLearnFrom = ptaSource.buildPTA();
			long startTime = LearningSupportRoutines.getThreadTime();
			LearnerGraph smallPta = UASExperiment.mergePTA(ptaToLearnFrom,uniqueLabel,false);
			ptaStateNumber=smallPta.getAcceptStateNumber();
			for(Entry<CmpVertex, MapWithSearch<Label,Label, CmpVertex>> entry:smallPta.transitionMatrix.entrySet())
			{
				for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
					if (transition.getValue().isAccept())
						fanoutPos++;
					else
						fanoutNeg++;
			}
			
			if (ptaStateNumber > 0)
			{
				fanoutPos/=ptaStateNumber;fanoutNeg/=ptaStateNumber;
			}
			//LearnerGraph trimmedGraph = smallPta.transform.trimGraph(5, smallPta.getInit());
			//trimmedGraph.setName(experimentName+"-part_of_premerge");
			//Visualiser.updateFrame(trimmedGraph,referenceGraph);
			
			ptaTotalNodes = smallPta.getStateNumber();ptaTailNodes = smallPta.getLeafStateNumber();
			Learner innerlearner = LearningAlgorithms.constructLearner(learnerInitConfiguration, smallPta,scoringMethod,scoringForEDSM, redReducer);
			Learner learner = null;
			if (useLearnerUnique)
				learner = new LearningAlgorithms.LearnerWithUniqueFromInitial(innerlearner,smallPta,uniqueLabel);
			else
				learner = innerlearner;

 			LearnerGraph learntGraph = learner.learnMachine(new LinkedList<>(), new LinkedList<>());
 			//Visualiser.updateFrame(learntGraph,referenceGraph);
 			actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
 			runTime = LearningSupportRoutines.getThreadTime()-startTime;

 			actualAutomaton.setName(experimentName+"-actual");
 			saveGraph(nameOUTCOME,actualAutomaton);
		}		
		//Visualiser.updateFrame(referenceGraph, actualAutomaton);
		//Visualiser.waitForKey();
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		ScoresForGraph outcome =  new ScoresForGraph();
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		if (redReducer != null)
		{
			outcome.invalidMergers = redReducer.reportInvalidMergers();outcome.missedMergers = redReducer.reportMissedMergers();
		}
		outcome.whetherLearningSuccessfulOrAborted = actualAutomaton.getLearningAbortedReason();
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		outcome.fanoutPos = fanoutPos;outcome.fanoutNeg = fanoutNeg;outcome.ptaStateNumber=ptaStateNumber;
		outcome.ptaTotalNodes=ptaTotalNodes;outcome.ptaTailNodes = ptaTailNodes;
		outcome.executionTime = runTime;
		return outcome;
	}
	
	/** Starts by constructing a PTA such that transitions with the unique label of interest will be merged during the learning process. See 
	 * {@link LearningSupportRoutines#constructPairsToMergeWithOutgoing(LearnerGraph, Label)} for details.
	 * 
	 * @param ptaSource generator for PTA
	 * @param experimentID
	 * @param scoringMethod
	 * @param scoringForEDSM 
	 * @param uniqueLabel labels transitions from the initial state
	 */
	public ScoresForGraph runExperimentUsingPTAPremerge(UASExperiment.BuildPTAInterface ptaSource, StateMergingStatistics redReducer, ThreadResultID experimentID, ScoringToApply scoringMethod, Configuration.ScoreMode scoringForEDSM, Label uniqueLabel) throws AugmentFromIfThenAutomatonException, IOException
	{
		// pre-merge and then learn. Generalised SICCO does not need a PTA and delivers the same results.
	 	// The problem with PTA premerge is that we need EDSM_0 otherwise a lot of edges need to be added to the new
	 	// state in order to persuade the learner to merge the right states
		// (unless EDSM is made to be _0 for pairs of states containing the label of interest).
		String experimentName = experimentID.getRowID()+","+experimentID.getColumnID();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(nameOUTCOME);
		int ptaStateNumber = 0;
		int ptaTotalNodes = 0, ptaTailNodes = 0;
		long runTime = 0;
		if(actualAutomaton == null)
		{
			// Perform semi-pre-merge by building a PTA rather than a graph with loops and learn from there without using constraints
			LearnerGraph pta = ptaSource.buildPTA();
			long startTime = LearningSupportRoutines.getThreadTime();
			LearnerGraph reducedPTA = LearningSupportRoutines.mergeStatesForUnique(pta,uniqueLabel);
			ptaStateNumber = reducedPTA.getAcceptStateNumber();
			ptaTotalNodes = reducedPTA.getStateNumber();ptaTailNodes = reducedPTA.getLeafStateNumber();
			ReferenceLearner refLearner = (ReferenceLearner)LearningAlgorithms.constructLearner(learnerInitConfiguration, reducedPTA,scoringMethod, scoringForEDSM, redReducer);
			refLearner.setLabelsLeadingFromStatesToBeMerged(Collections.singletonList(uniqueLabel));
			LearningAlgorithms.LearnerWithUniqueFromInitial learner = //LearningAlgorithms.constructReferenceLearner(learnerInitConfiguration, reducedPTA,scoringMethod);
					new LearningAlgorithms.LearnerWithUniqueFromInitial(refLearner,reducedPTA,uniqueLabel);
 			learner.setRedReducer(redReducer);

 			LearnerGraph learntGraph = learner.learnMachine(new LinkedList<>(), new LinkedList<>());
 			actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
			runTime = LearningSupportRoutines.getThreadTime()-startTime;

			actualAutomaton.setName(experimentName+"-actual");
			saveGraph(nameOUTCOME,actualAutomaton);
		}		
		
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		ScoresForGraph outcome =  new ScoresForGraph();
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		if (redReducer != null)
		{
			outcome.invalidMergers = redReducer.reportInvalidMergers();outcome.missedMergers = redReducer.reportMissedMergers();
		}
		outcome.whetherLearningSuccessfulOrAborted = actualAutomaton.getLearningAbortedReason();
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		outcome.ptaStateNumber=ptaStateNumber;outcome.ptaTotalNodes=ptaTotalNodes;outcome.ptaTailNodes = ptaTailNodes;
		outcome.executionTime = runTime;
		return outcome;
	}
	
	public ScoresForGraph runExperimentUsingConstraints(UASExperiment.BuildPTAInterface ptaSource, StateMergingStatistics redReducer, ThreadResultID experimentID, ScoringToApply scoringMethod, Configuration.ScoreMode scoringForEDSM, Label uniqueLabel) throws AugmentFromIfThenAutomatonException, IOException
	{// conventional learning, but check each merger against the unique-label merge
		String experimentName = experimentID.getRowID()+","+experimentID.getColumnID();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(nameOUTCOME);
		int ptaTotalNodes = 0, ptaTailNodes = 0;
		long runTime = 0;
		if(actualAutomaton == null)
		{
			LearnerGraph ptaToLearnFrom = ptaSource.buildPTA();
			long startTime = LearningSupportRoutines.getThreadTime();
 			ReferenceLearner learner = (ReferenceLearner)LearningAlgorithms.constructLearner(learnerInitConfiguration, ptaToLearnFrom,scoringMethod, scoringForEDSM, redReducer);
 			learner.setLabelsLeadingFromStatesToBeMerged(Collections.singletonList(uniqueLabel));
 			learner.setRedReducer(redReducer);
 			ptaTotalNodes = ptaToLearnFrom.getStateNumber();ptaTailNodes = ptaToLearnFrom.getLeafStateNumber();
			LearnerGraph learntGraph = learner.learnMachine(new LinkedList<>(), new LinkedList<>());
 			actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
			runTime = LearningSupportRoutines.getThreadTime()-startTime;

			saveGraph(nameOUTCOME,actualAutomaton);
		}		
		
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		ScoresForGraph outcome =  new ScoresForGraph(); 
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		if (redReducer != null)
		{
			outcome.invalidMergers = redReducer.reportInvalidMergers();outcome.missedMergers = redReducer.reportMissedMergers();
		}
		outcome.whetherLearningSuccessfulOrAborted = actualAutomaton.getLearningAbortedReason();
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		outcome.ptaTotalNodes=ptaTotalNodes;outcome.ptaTailNodes = ptaTailNodes;
		outcome.executionTime = runTime;
		return outcome;
	}

}