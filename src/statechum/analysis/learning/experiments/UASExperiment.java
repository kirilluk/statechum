package statechum.analysis.learning.experiments;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.Callable;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Label;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
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

public  abstract  class UASExperiment<PARS extends ThreadResultID,TR extends ThreadResult> implements Callable<TR>
{
	protected final LearnerEvaluationConfiguration learnerInitConfiguration;
	protected LearnerGraph referenceGraph;
	protected String inputGraphFileName;
	public final PARS par;
	
	protected boolean alwaysRunExperiment = false;
	
	public UASExperiment(PARS parameters, LearnerEvaluationConfiguration eval, String directoryNamePrefix)
	{
		par = parameters;
		learnerInitConfiguration = eval;
		String outDir = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP)+File.separator+directoryNamePrefix+File.separator+"experimentdata"+File.separator+par.getRowID();
		mkDir(outDir);
		inputGraphFileName = outDir + File.separator+"rnd";
	}
	
	public static void mkDir(String path)
	{
		if (path == null || path.isEmpty())
			return;
		String outDir = path;
		mkDir(new java.io.File(outDir).getParent());// create a parent directory
		if (!new java.io.File(outDir).isDirectory())
		{
			if (!new java.io.File(outDir).mkdir())
				throw new RuntimeException("failed to create a work directory");
		}
	}
	
	public String constructFileName(String experimentSuffix)
	{
		return inputGraphFileName+"_"+experimentSuffix+".xml";
	}
	
	/** The outcome of learning might have been stored in a file from the previous run. For this reason, it makes sense to try to load it. 
	 * @throws IOException if the outcome of learning exists but cannot be loaded
	 */
	protected LearnerGraph loadOutcomeOfLearning(String experimentSuffix)
	{
		LearnerGraph outcome = null;
		if (!alwaysRunExperiment)
		{
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
		}    	
    	return outcome;
	}
	
	public void saveGraph(String experimentSuffix, LearnerGraph outcome) throws IOException
	{
		outcome.storage.writeGraphML(constructFileName(experimentSuffix));	
	}
	
	public static List<ScoringToApply> listOfScoringMethodsToApplyThatDependOnEDSMScoring()
	{
		return Arrays.asList(new ScoringToApply[]{ScoringToApply.SCORING_EDSM,//ScoringToApply.SCORING_EDSM_1,
				ScoringToApply.SCORING_EDSM_2,
				//ScoringToApply.SCORING_EDSM_3,
				ScoringToApply.SCORING_EDSM_4,//ScoringToApply.SCORING_EDSM_5,
				//ScoringToApply.SCORING_EDSM_6,
				//ScoringToApply.SCORING_EDSM_7,ScoringToApply.SCORING_EDSM_8
				ScoringToApply.SCORING_SICCO
				
				// SICCO_PTA performs the same as SICCO, RECURSIVE and RED perform very badly.
				// EDSM does not perform as well as SICCO and in particular, its performance is dependent on the threshold whereas for SICCO it does not.
				//,ScoringToApply.SCORING_SICCO_PTA,ScoringToApply.SCORING_SICCO_PTARECURSIVE, ScoringToApply.SCORING_SICCO_RED
				});
	}

	public static List<ScoringToApply> listOfScoringMethodsToApplyThatDoNotDependOnEDSMScoring()
	{
		return Arrays.asList(new ScoringToApply[]{
				ScoringToApply.SCORING_PTAK_1,ScoringToApply.SCORING_PTAK_2,ScoringToApply.SCORING_PTAK_3,ScoringToApply.SCORING_PTAK_4
				});
	}

	public static List<ScoringToApply> listOfTraditionalKTailsMethods()
	{
		return Arrays.asList(new ScoringToApply[]{ ScoringToApply.SCORING_KT_1,ScoringToApply.SCORING_KT_2,ScoringToApply.SCORING_KT_3,ScoringToApply.SCORING_KT_4 });
	}
	
	public static LearnerGraph mergePTA(LearnerGraph initialPTA,Label labelToMerge, boolean buildAuxInfo)
	{
	   LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
	   List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMergeForPathsFrom(initialPTA,labelToMerge); 
			   //LearningSupportRoutines.buildVerticesToMerge(initialPTA,Collections.<Label>emptyList(),Arrays.asList(new Label[]{labelToMerge}));
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

	public static class ScoringModeScore
	{
		public final Configuration.ScoreMode scoringForEDSM;
		public final ScoringToApply scoringMethod;
		
		public ScoringModeScore(Configuration.ScoreMode a,ScoringToApply b)
		{
			scoringForEDSM = a;scoringMethod = b;
		}
	}
	
	public static long getThreadTime()
	{
		// thanks to http://stackoverflow.com/questions/14664897/measure-java-short-time-running-thread-execution-time
		return java.lang.management.ManagementFactory.getThreadMXBean().getThreadCpuTime(Thread.currentThread().getId());
	}
	public static LearnerEvaluationConfiguration constructLearnerInitConfiguration()
	{
		LearnerEvaluationConfiguration learnerInitConfiguration = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		learnerInitConfiguration.setLabelConverter(new Transform.InternStringLabel());
        final Configuration learnerConfig = learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
        learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);
        learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);
        learnerConfig.setAlwaysUseTheSameMatrixType(false);// permits computations to switch transition matrix type depending on matrix size.
        //learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
        learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);
        learnerConfig.setLearnerScoreMode(Configuration.ScoreMode.ONLYOVERRIDE);
       	learnerInitConfiguration.config.setUseConstraints(false);// do not use if-then during learning (enough to augment once)
        return learnerInitConfiguration;
	}

	/** The reason behind using ptaSource is that where it is expensive to build a PTA, it will only be requested if the learner has not stored it or store the outcome of inference somewhere. 
	 * The argument <em>experimentID</em> is to ensure each experiment gets a unique name. Different experiments have different parameters so it is not realistic to expect them all to include scoring methods.
	 * For this reason, both <em>scoringMethod</em> and <em>scoringForEDSM</em> are arguments.
	 */
	public ScoresForGraph runExperimentUsingConventional(UASExperiment.BuildPTAInterface ptaSource, ThreadResultID experimentID, ScoringToApply scoringMethod,Configuration.ScoreMode scoringForEDSM) throws AugmentFromIfThenAutomatonException, IOException
	{
		String experimentName = experimentID.getRowID()+","+experimentID.getColumnID();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(experimentName);
		long runTime = 0;
		if(actualAutomaton == null)
		{
			LearnerGraph pta = ptaSource.buildPTA();
 			Learner learner = LearningAlgorithms.constructLearner(learnerInitConfiguration, pta,scoringMethod, scoringForEDSM);
 			long startTime = getThreadTime();
 			LearnerGraph learntGraph = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 			actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
 			runTime = getThreadTime()-startTime;
 			
 			saveGraph(experimentName,actualAutomaton);
		}
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		ScoresForGraph outcome = new ScoresForGraph(); 
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		outcome.executionTime = runTime;
		return outcome;
	}
	
	public ScoresForGraph runExperimentUsingConventionalWithUniqueLabel(UASExperiment.BuildPTAInterface ptaSource, ThreadResultID experimentID, ScoringToApply scoringMethod, Configuration.ScoreMode scoringForEDSM, Label uniqueLabel) throws AugmentFromIfThenAutomatonException, IOException
	{
		String experimentName = experimentID.getRowID()+","+experimentID.getColumnID();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(experimentName);
		long runTime = 0;
		if(actualAutomaton == null)
		{
			LearnerGraph pta = ptaSource.buildPTA();
			long startTime = getThreadTime();
			Learner learner = new LearningAlgorithms.LearnerWithUniqueFromInitial(LearningAlgorithms.constructLearner(learnerInitConfiguration, pta,scoringMethod, scoringForEDSM), pta, uniqueLabel);
		
 			LearnerGraph learntGraph = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 			actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
 			runTime = getThreadTime()-startTime;

 			saveGraph(experimentName,actualAutomaton);
		}
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		ScoresForGraph outcome = new ScoresForGraph(); 
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		outcome.executionTime = runTime;
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
	public ScoresForGraph runExperimentUsingPremerge(UASExperiment.BuildPTAInterface ptaSource, ThreadResultID experimentID, ScoringToApply scoringMethod, Configuration.ScoreMode scoringForEDSM, Label uniqueLabel) throws AugmentFromIfThenAutomatonException, IOException
	{// pre-merge and then learn. Generalised SICCO does not need a PTA and delivers the same results.
		String experimentName = experimentID.getRowID()+","+experimentID.getColumnID();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(experimentName);
		double fanoutPos=0, fanoutNeg = 0;
		int ptaStateNumber=0;
		long runTime = 0;
		if(actualAutomaton == null)
		{
			LearnerGraph ptaToLearnFrom = ptaSource.buildPTA();
			long startTime = getThreadTime();
			LearnerGraph smallPta = UASExperiment.mergePTA(ptaToLearnFrom,uniqueLabel,false);
			ptaStateNumber=smallPta.getAcceptStateNumber();
			for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:smallPta.transitionMatrix.entrySet())
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
			
			Learner learner = LearningAlgorithms.constructLearner(learnerInitConfiguration, smallPta,scoringMethod,scoringForEDSM);
					//new LearningAlgorithms.LearnerWithUniqueFromInitial(LearningAlgorithms.constructReferenceLearner(learnerInitConfiguration, smallPta,scoringMethod),smallPta,uniqueLabel);

 			LearnerGraph learntGraph = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 			actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
 			runTime = getThreadTime()-startTime;

 			actualAutomaton.setName(experimentName+"-actual");
 			saveGraph(experimentName,actualAutomaton);
		}		
		
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		//Visualiser.updateFrame(actualAutomaton,referenceGraph);
		ScoresForGraph outcome =  new ScoresForGraph();
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		outcome.fanoutPos = fanoutPos;outcome.fanoutNeg = fanoutNeg;outcome.ptaStateNumber=ptaStateNumber;
		outcome.executionTime = runTime;
		return outcome;
	}
	
	public ScoresForGraph runExperimentUsingPTAPremerge(UASExperiment.BuildPTAInterface ptaSource, ThreadResultID experimentID, ScoringToApply scoringMethod, Configuration.ScoreMode scoringForEDSM, Label uniqueLabel) throws AugmentFromIfThenAutomatonException, IOException
	{// pre-merge and then learn. Generalised SICCO does not need a PTA and delivers the same results. The problem with PTA premerge is that we need EDSM_0 otherwise a lot of edges need to be added to the new state in order to persuade the learner to merge the right states.
		String experimentName = experimentID.getRowID()+","+experimentID.getColumnID();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(experimentName);
		int ptaStateNumber = 0;
		long runTime = 0;
		if(actualAutomaton == null)
		{
			// Perform semi-pre-merge by building a PTA rather than a graph with loops and learn from there without using constraints
			LearnerGraph pta = ptaSource.buildPTA();
			long startTime = getThreadTime();
			LearnerGraph reducedPTA = LearningSupportRoutines.mergeStatesForUnique(pta,uniqueLabel);
			ptaStateNumber = reducedPTA.getAcceptStateNumber();
			ReferenceLearner refLearner = (ReferenceLearner)LearningAlgorithms.constructLearner(learnerInitConfiguration, reducedPTA,scoringMethod, scoringForEDSM);
			refLearner.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueLabel}));
			Learner learner = //LearningAlgorithms.constructReferenceLearner(learnerInitConfiguration, reducedPTA,scoringMethod);
					new LearningAlgorithms.LearnerWithUniqueFromInitial(refLearner,reducedPTA,uniqueLabel);

 			LearnerGraph learntGraph = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 			actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
			runTime = getThreadTime()-startTime;

			actualAutomaton.setName(experimentName+"-actual");
			saveGraph(experimentName,actualAutomaton);
		}		
		
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		/*
		if (diffMeasure.getValue()>0.9)
		{
			GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
			DirectedSparseGraph gr = gd.showGD(
				actualAutomaton,referenceGraph,
				ExperimentRunner.getCpuNumber());
			Visualiser.updateFrameWithPos(gr,3);
			Visualiser.updateFrame(actualAutomaton, referenceGraph);
			Visualiser.waitForKey();
		}*/
		ScoresForGraph outcome =  new ScoresForGraph();
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		outcome.ptaStateNumber=ptaStateNumber;
		outcome.executionTime = runTime;
		return outcome;
	}
	
	public ScoresForGraph runExperimentUsingConstraints(UASExperiment.BuildPTAInterface ptaSource, ThreadResultID experimentID, ScoringToApply scoringMethod, Configuration.ScoreMode scoringForEDSM, Label uniqueLabel) throws AugmentFromIfThenAutomatonException, IOException
	{// conventional learning, but check each merger against the unique-label merge
		String experimentName = experimentID.getRowID()+","+experimentID.getColumnID();
		LearnerGraph actualAutomaton = loadOutcomeOfLearning(experimentName);
		long runTime = 0;
		if(actualAutomaton == null)
		{
			LearnerGraph ptaToLearnFrom = ptaSource.buildPTA();
			long startTime = getThreadTime();
 			ReferenceLearner learner = (ReferenceLearner)LearningAlgorithms.constructLearner(learnerInitConfiguration, ptaToLearnFrom,scoringMethod, scoringForEDSM);
 			learner.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueLabel}));

			LearnerGraph learntGraph = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 			actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
			runTime = getThreadTime()-startTime;

			saveGraph(experimentName,actualAutomaton);
		}		
		
		DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		actualAutomaton.setName(experimentName);
		//Visualiser.updateFrame(actualAutomaton,referenceGraph);
		ScoresForGraph outcome =  new ScoresForGraph(); 
		outcome.differenceStructural = diffMeasure;outcome.differenceBCR = bcrMeasure;
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
		outcome.executionTime = runTime;
		return outcome;
	}

}