/* Copyright (c) 2013 The University of Sheffield.
 * 
 * This file is part of StateChum.
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */

package statechum.analysis.learning.experiments.MarkovEDSM;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicLong;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Label;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.DrawGraphs.WilcoxonPairedTest;
import statechum.analysis.learning.DrawGraphs.Mann_Whitney_U_Test;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.Kruskal_Wallis;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.MarkovClassifierLG;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.StateMergingStatistics;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ReferenceLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceDiff;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceLanguageBCR;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.SampleData;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.analysis.learning.DrawGraphs.RBagPlot;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;

import static statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.constructLearner;


public class MarkovExperiment
{
	
	public static final String directoryNamePrefix = "markov";
	public static final String directoryExperimentResult = "experimentresult"+File.separator;
	
	public static class MarkovLearnerRunner extends UASExperiment<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>>
	{
		public MarkovLearnerRunner(MarkovLearningParameters parameters, LearnerEvaluationConfiguration cnf)
		{
			super(parameters,cnf,directoryNamePrefix);
		}

		/** Constructs a reference graph and assigns it to member variable <pre>referenceGraph</pre>. This is a separate method to permit overriding by subclasses.
		 */
		public void generateReferenceFSM()
		{
			final int alphabet = (int)(par.alphabetMultiplier*par.states);
			// density refers to the number of transitions per state. It is defined as a multiplier of a number
			// of states because we would like 100% to refer to a fully-connected graph (which is hence useless
			// as it accepts everything).
			final double density = (double) (par.states * par.perStateSquaredDensityMultipliedBy100) / 100;
			MachineGenerator mg = new MachineGenerator(par.states, 400 , (int)Math.round((double)par.states/5));mg.setGenerateConnected(true);
			
			try {
				// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
				referenceGraph = mg.nextMachine(alphabet, density,par.seed, learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()).pathroutines.buildDeterministicGraph();
			} catch (IncompatibleStatesException e) {
				Helper.throwUnchecked("failed to generate graph", e);
			}
		}
		
		/** Constructs a PTA to learn an FSM from. This could be based on a reference graph or obtained externally. */
		public LearnerGraph constructPTA()
		{
			// Use a random generator selector passed as a parameter.
			LearnerGraph pta = new LearnerGraph(learnerInitConfiguration.config);
			RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(par.trainingSample),5,null);
			// Using 2*par.traceQuantity reflects the original goal to generate an equal number of positive and
			// negative traces hence an input to generateRandomPosNeg was expected to be even.
			// We are not doing this now, instead only generating positive traces in quantity par.traceQuantity.
			generator.generateRandomPosNeg(2*par.traceQuantity, 1, false, new RandomLengthGenerator() {

					@Override
					public int getLength() {
						return (int)(par.traceLengthMultiplier*par.states);
					}
	
					@Override
					public int getPrefixLength(int len) {
						return len;
					}
				},true, false, null,null);

			pta.paths.augmentPTA(generator.getAllSequences(0));
			return pta;
		}
		
		@Override
		public ExperimentResult<MarkovLearningParameters> runexperiment() throws Exception 
		{
			generateReferenceFSM();
			saveGraph(nameReference,referenceGraph);

			ExperimentResult<MarkovLearningParameters> outcome = new ExperimentResult<>(par);
			learnerInitConfiguration.testSet = LearningAlgorithms.buildEvaluationSet(referenceGraph);
			
			LearnerGraph pta = constructPTA();
	
			final MarkovModel m= new MarkovModel(par.markovParameters.chunkLen,true,true,true,false);

			new MarkovClassifierLG(m, pta,null).updateMarkov(false);// construct Markov chain if asked for.
			
			pta.clearColours();

			assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
			
			final Configuration deepCopy = pta.config.copy();deepCopy.setLearnerCloneGraph(true);
			LearnerGraph ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);

//			LearnerGraph trimmedReference = LearningSupportRoutines.trimUncoveredTransitions(pta,referenceGraph);
			final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();
			long inconsistencyForTheReferenceGraph = MarkovClassifier.computeInconsistency(referenceGraph, null, m, checker,false);

			PerformFirstMerge firstMerge = new PerformFirstMerge();firstMerge.ptaToUseForInference=pta;
			if (par.markovParameters.useCentreVertex)
			{
				saveGraph(namePTABEFORECENTRE,pta);
				// This replaces firstMerge.ptaToUseForInference with a graph built by merging around the most-connected vertex
				firstMerge.buildFirstGraph(pta, referenceGraph, par.markovParameters, m, checker);
				if (par.usePrintf) {
					LearnerGraphND inverseOfPtaAfterInitialMerge = MarkovClassifier.computeInverseGraph(firstMerge.ptaToUseForInference);
					System.out.println("Centre vertex: " + firstMerge.vertexWithMostTransitions + " number of transitions: " +
							WaveBlueFringe.countTransitions(firstMerge.ptaToUseForInference,
									inverseOfPtaAfterInitialMerge, firstMerge.vertexWithMostTransitions));
				}
			}
	
			SampleData dataSample = new SampleData(null,null);

			EDSM_MarkovLearner markovLearner = null;
			ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown redReducer;
			saveGraph(namePTA, firstMerge.ptaToUseForInference);// although it may seem that pars.getExperimentID()
			// would be a better name than a full name, in cases where we use a middle vertex PTA to start from is
			// different to the one generated from a reference graph. Hence using full name and recording lots of graphs.

			// Ideally, we would like to record learnt graph and only rebuilt comparison results when asked. This is
			// not possible because without a learning process there is no record which mergers
			// were right or not and we will not have information how long it took for a learner to complete the learn.
			LearnerGraph ptaBuilt = firstMerge.ptaToUseForInference;
			Learner learnerOfPairs;
			redReducer = new ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown(referenceGraph,false);
			switch(par.learnerToUse)
			{
				case SCORING_MARKOV:
					markovLearner = new EDSM_MarkovLearner(learnerInitConfiguration,ptaBuilt,0,
							par.markovParameters,ScoreMode.GENERAL_NOFULLMERGE, redReducer);
					markovLearner.setMarkov(m);markovLearner.setChecker(checker);
					learnerOfPairs = markovLearner;
					break;
				case SCORING_MARKOV_1:
					markovLearner = new EDSM_MarkovLearner(learnerInitConfiguration,ptaBuilt,1,
							par.markovParameters,ScoreMode.GENERAL_NOFULLMERGE, redReducer);
					markovLearner.setMarkov(m);markovLearner.setChecker(checker);
					learnerOfPairs = markovLearner;
					break;
				case SCORING_MARKOV_2:
					markovLearner = new EDSM_MarkovLearner(learnerInitConfiguration,ptaBuilt,2,
							par.markovParameters,ScoreMode.GENERAL_NOFULLMERGE, redReducer);
					markovLearner.setMarkov(m);markovLearner.setChecker(checker);
					learnerOfPairs = markovLearner;
					break;
				default:
					// ScoreMode.GENERAL_NOFULLMERGE is ok here because all states are accept-states,
					// otherwise GENERAL_PLUS_NOFULLMERGE might have been a better choice.
					learnerOfPairs = constructLearner(learnerInitConfiguration,ptaBuilt, par.learnerToUse,ScoreMode.GENERAL_NOFULLMERGE,redReducer);
					break;
			}

			long startTime = LearningSupportRoutines.getThreadTime();
			LearnerGraph learntGraph = learnerOfPairs.learnMachine(new LinkedList<>(), new LinkedList<>());
			if (firstMerge.verticesToMergeBasedOnInitialPTA != null && par.markovParameters.mergeIdentifiedPathsAfterInference)
			{	// This accounts for learning from PTA where certain states are going to be eventually
			 	// merged (such as those with 'reset' transition leading from them) and accounting for such eventual
				// mergers them in the score computation, but keeping the inference process close to classical EDSM
				// where blue states are roots of trees. After learning is complete, we merge the remaining states
				// (an operation that by construction is expected to be possible).
				LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<>();
				int genScore = learntGraph.pairscores.computePairCompatibilityScore_general(null,
						constructPairsToMergeBasedOnSetsToMerge(learntGraph.transitionMatrix.keySet(),firstMerge.verticesToMergeBasedOnInitialPTA), verticesToMerge, false);
				assert genScore >= 0;
				learntGraph = MergeStates.mergeCollectionOfVertices(learntGraph, null, verticesToMerge, null, false);
			}

			if (par.markovParameters.useCentreVertex)
			{// select the initial state
				CmpVertex newInit = LearningSupportRoutines.findBestMatchForInitialVertexInGraph(learntGraph,pta);// will only return null if the learner failed (and returned an single-state reject graph)
				if (newInit != null)
					learntGraph.setInit(newInit);
			}

			long runTime = LearningSupportRoutines.getThreadTime()-startTime;
			LearnerGraph actualAutomaton = LearningSupportRoutines.removeRejects(learntGraph);
			saveGraph(nameOUTCOME,actualAutomaton);

			dataSample.actualLearner = WaveBlueFringe.estimateDifference(actualAutomaton,m,checker,referenceGraph,learnerInitConfiguration.testSet);
			if (redReducer != null)
			{
				dataSample.actualLearner.invalidMergers = redReducer.reportInvalidMergers();dataSample.actualLearner.missedMergers = redReducer.reportMissedMergers();
			}
			dataSample.actualLearner.whetherLearningSuccessfulOrAborted = actualAutomaton.getLearningAbortedReason();
			dataSample.actualLearner.executionTime = runTime;
			dataSample.inconsistencyReference = MarkovClassifier.computeInconsistency(referenceGraph, null, m, checker,false);
			dataSample.referenceLearner = zeroScore;
			dataSample.centreCorrect = firstMerge.correctCentre;
			dataSample.centrePathNumber = firstMerge.centrePathNumber;
			dataSample.fractionOfStatesIdentifiedBySingletons=Math.round(100*MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(referenceGraph));
			dataSample.stateNumber = referenceGraph.getStateNumber();
			dataSample.transitionsSampled = Math.round(100*(double)referenceGraph.pathroutines.countEdges()/referenceGraph.pathroutines.countEdges());
			statechum.Pair<Double,Double> correctnessOfMarkov = new MarkovClassifierLG(m, referenceGraph,null).evaluateCorrectnessOfMarkov(true, false);
			dataSample.markovPrecision = Math.round(100*correctnessOfMarkov.firstElem);dataSample.markovRecall = Math.round(100*correctnessOfMarkov.secondElem);
 			if (markovLearner != null)
 				dataSample.comparisonsPerformed = markovLearner.markovHelper.comparisonsPerformed;
 			
			if (par.usePrintf) {
				if (dataSample.actualLearner.differenceBCR.getValue() < 1.0 && dataSample.actualLearner.differenceStructural.getValue() == 1.0)
				{
					System.out.println("Graph with perfect DIFF but wrong initial state: "+SGE_ExperimentRunner.RunSubExperiment.constructFileName(graphFileNameDir+"outcome-",par));
					/*
					CmpVertex newInit = LearningSupportRoutines.findBestMatchForInitialVertexInGraph(actualAutomaton,pta);// this cannot return null since the outcome of learning will have at least one state
					DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
					Visualiser.updateFrame(actualAutomaton, referenceGraph);
					System.out.println();
					*/
				}
				Collection<List<Label>> wset = WMethod.computeWSet_reducedw(referenceGraph);
				int wSeqLen = 0;
				for (List<Label> seq : wset) {
					int len = seq.size();
					if (len > wSeqLen) wSeqLen = len;
				}
				System.out.println("actual: " + actualAutomaton.getStateNumber() +
						" difference actual is " + dataSample.actualLearner.differenceStructural
						+ " inconsistency learnt " + dataSample.actualLearner.inconsistency + " inconsistency reference: " + inconsistencyForTheReferenceGraph
						+ " transitions per state: " + (double) referenceGraph.pathroutines.countEdges() / referenceGraph.getStateNumber() +
						" W seq max len " + wSeqLen +
						" Uniquely identifiable by W " + Math.round(100 * MarkovClassifier.calculateFractionOfIdentifiedStates(referenceGraph, wset)) + " %"
						+ " and by singletons " + Math.round(100 * MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(referenceGraph)) + " %"
				);
			}
			outcome.samples.add(dataSample);
			return outcome;
		}

	}
	
		
	public static Collection<StatePair> constructPairsToMergeBasedOnSetsToMerge(Set<CmpVertex> validStates, Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA)
	{
		List<StatePair> pairsList = new LinkedList<>();
		for(Set<CmpVertex> groupOfStates:verticesToMergeBasedOnInitialPTA)
		{
            Set<CmpVertex> validStatesInGroup = new TreeSet<>(groupOfStates);
            validStatesInGroup.retainAll(validStates);
			if (validStatesInGroup.size() > 1)
			{
				CmpVertex v0=validStatesInGroup.iterator().next();
				for(CmpVertex v:validStatesInGroup)
				{
					if (v != v0)
						pairsList.add(new StatePair(v0,v));
					v0=v;
				}
			}
		}
		return pairsList;
	}
			
	public static final ScoresForGraph zeroScore;
	static
	{
		zeroScore = new ScoresForGraph();zeroScore.differenceBCR=new DifferenceToReferenceLanguageBCR(0, 0, 0, 0);zeroScore.differenceStructural=new DifferenceToReferenceDiff(0, 0);
	}

	
	/** Uses the supplied classifier to rank pairs. */
	public static class EDSM_MarkovLearner extends ReferenceLearner implements statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure
	{
		@Override
		public CmpVertex selectRedNode(LearnerGraph gr,Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
		{
			CmpVertex redVertex = tentativeRedNodes.iterator().next();
			if (redReducer != null)
				redReducer.stateSelectedAsRed(gr,redVertex,reds);

			LearningAlgorithms.LearnerAbortedException.throwExceptionIfTooManyReds(coregraph, config.getOverride_maximalNumberOfStates(),redReducer);
			return redVertex;
		}
		
		@SuppressWarnings("unused")
		@Override
		public CmpVertex resolvePotentialDeadEnd(LearnerGraph gr, Collection<CmpVertex> reds, List<PairScore> pairs) 
		{
			return null;												
		}
		
		protected final MarkovHelper markovHelper;
		
		public MarkovHelper getHelper()
		{
			return markovHelper;
		}
		
		public void setMarkov(MarkovModel m) {
			markovHelper.setMarkov(m);
		}

		public void setChecker(ConsistencyChecker c) {
			markovHelper.setChecker(c);
		}

		protected LearnerGraph coregraph;
		
		@Override
		public void initComputation(LearnerGraph graph) 
		{
			coregraph = graph;
			markovHelper.initComputation(graph, MarkovClassifier.computeInverseGraph(coregraph));
		}

		long lastComputedCompatibilityScore;

		@Override // we only need this in order to supply a routine to find surrounding transitions and initComputation
		public long overrideScoreComputation(PairScore p) 
		{
			long score = markovHelper.computeScoreBasedOnInconsistencies(p);
			lastComputedCompatibilityScore = markovHelper.getLastComputedInconsistency();
			return score;
		}

		@Override
		public long getLastComputedCompatibilityScore() {
			return lastComputedCompatibilityScore;
		}

		/** This one returns a set of transitions in all directions. */
		@Override
		public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(CmpVertex currentRed) 
		{
			return	markovHelper.getSurroundingTransitions(currentRed);
		}

		@Override
		public boolean useFirstFoundRed() {
			return true;
		}

		private static LearnerEvaluationConfiguration constructConfiguration(LearnerEvaluationConfiguration evalCnf,Configuration.ScoreMode scoreMode, int threshold)
		{
			Configuration config = evalCnf.config.copy();config.setRejectPositivePairsWithScoresLessThan(threshold);
			if (scoreMode != null)
				evalCnf.config.setLearnerScoreMode(scoreMode);
			LearnerEvaluationConfiguration copy = new LearnerEvaluationConfiguration(config);
			copy.graph = evalCnf.graph;copy.testSet = evalCnf.testSet;
			copy.setLabelConverter(evalCnf.getLabelConverter());
			copy.ifthenSequences = evalCnf.ifthenSequences;copy.labelDetails=evalCnf.labelDetails;
			return copy;
		}

		public EDSM_MarkovLearner(LearnerEvaluationConfiguration evalCnf, final LearnerGraph argInitialPTA, int threshold, MarkovParameters markovPars,Configuration.ScoreMode scoreMode, StateMergingStatistics redReducer)
		{
			super(constructConfiguration(evalCnf,scoreMode,threshold), argInitialPTA,null, redReducer);// null means that we expect our ChooseStatePairs to completely replace the one in the parent class.
			markovHelper = new MarkovHelper(markovPars);
		}
		
		@Override 
		public Stack<PairScore> ChooseStatePairs(LearnerGraph graph)
		{
			LearningAlgorithms.LearnerAbortedException.throwExceptionIfTooManyReds(graph, config.getOverride_maximalNumberOfStates(),redReducer);
			checkTimeout();
			return graph.pairscores.chooseStatePairs(this);
		}

		@Override
		public String toString()
		{
			return "EDSM_Markov";
		}
	}	
	
	public static void main(String []args)
	{
		String outDir = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_EXPERIMENTRESULTS)+File.separator+directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		UASExperiment.mkDir(outDir);
		String outPathPrefix = outDir + File.separator;
		LearnerEvaluationConfiguration eval = UASExperiment.constructLearnerInitConfiguration();
		eval.config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);eval.config.setLearnerScoreMode(ScoreMode.GENERAL_NOFULLMERGE);
		eval.config.setTimeOut(3600000L*4L);// timeout for tasks, in milliseconds, equivalent to 4hrs runtime for an old Xeon 5670 @ 2.93Ghz, modern E5/i7 are 3x faster.
		
		DrawGraphs gr = new DrawGraphs();
		
		final int fsmSamplesPerStateNumber = 20;
		final int trainingSamplesPerFSM = 4;
		final double traceLengthMultiplierMax = 16;

		final boolean pathsOrSets = true;
		final int[] statesToUse = new int[]{10};
		SGE_ExperimentRunner.configureCPUFreqNormalisation();
		
		RunSubExperiment<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>> experimentRunner = new RunSubExperiment<>(ExperimentRunner.getCpuNumber(), outPathPrefix + directoryExperimentResult, args);
		statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum phase = experimentRunner.getPhase();

		final double alphabetMultiplier=2;

		try
		{
		
		/*
		final RBoxPlotP<String> gr_BCRForDifferentLearners = new RBoxPlotP<String>("","BCR",new File(branch+"BCR_learner.pdf"));
		final RBoxPlotP<String> gr_StructuralForDifferentLearners = new RBoxPlotP<String>("","structural",new File(branch+"structural_learner.pdf"));
		{
			try
			{
				int numberOfTasks = 0;
				for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
					for(int sample=0;sample<samplesPerFSM;++sample)
					{
						Cav2014.EvaluationOfExisingLearnerRunner learnerRunner = new Cav2014.EvaluationOfExisingLearnerRunner(states,sample,numberOfTasks,traceQuantity, config, converter);
						learnerRunner.setOnlyUsePositives(onlyPositives);
						learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
						learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);learnerRunner.setChunkLen(chunkSize);
						learnerRunner.setSelectionID(branch+"_states"+states+"_sample"+sample);
						runner.submit(learnerRunner);
						++numberOfTasks;
					}
				ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for the behaviour of different learners", numberOfTasks);
				for(int count=0;count < numberOfTasks;++count)
				{
					ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
					for(SampleData sample:result.samples)
						for(Entry<String,ScoresForGraph> score:sample.miscGraphs.entrySet())
							gr_StructuralForDifferentLearners.add(score.getKey(),score.getValue().differenceStructural.getValue());
				
					for(SampleData sample:result.samples)
						for(Entry<String,ScoresForGraph> score:sample.miscGraphs.entrySet())
							gr_BCRForDifferentLearners.add(score.getKey(),score.getValue().differenceBCR.getValue());

					progress.next();
					gr_BCRForDifferentLearners.drawInteractive(gr);gr_StructuralForDifferentLearners.drawInteractive(gr);
				}
				
			}
			catch(Exception ex)
			{
				IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
				if (executorService != null) { executorService.shutdownNow();executorService = null; }
				throw e;
			}
		}
		if (gr_BCRForDifferentLearners != null) gr_BCRForDifferentLearners.drawPdf(gr);if (gr_StructuralForDifferentLearners != null) gr_StructuralForDifferentLearners.drawPdf(gr);
		*/
/*
		for(final boolean useCentreVertex:new boolean[]{true,false})
		for(final boolean mergeIdentifiedPathsAfterInference:new boolean[]{true,false})
		for(final boolean useClassifyToOrderPairs:new boolean[]{true,false})
			
		for(final int traceQuantity:new int[]{10})
		for(final double traceLengthMultiplier:new double[]{1})
			{
				
				final int traceQuantityToUse = traceQuantity;
				
				String selection = "c="+useCentreVertex+
						";m="+mergeIdentifiedPathsAfterInference+
						";o="+useClassifyToOrderPairs+
						";traceQuantity="+traceQuantity+";traceLengthMultiplier="+traceLengthMultiplier+";"+";alphabetMultiplier="+alphabetMultiplier+";";
				SquareBagPlot gr_StructuralDiff = new SquareBagPlot("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File(branch+"_"+selection+"_trace_structuraldiff.pdf"),0,1,true);
				SquareBagPlot gr_BCR = new SquareBagPlot("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+"_trace_bcr.pdf"),0.5,1,true);		
						try
						{
							int numberOfTasks = 0;
							for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
								for(int sample=0;sample<samplesPerFSM;++sample)
								{
									MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,numberOfTasks,traceQuantityToUse, config, converter);
									learnerRunner.setOnlyUsePositives(onlyPositives);
									learnerRunner.setAlphabetMultiplier(alphabetMultiplier);
									learnerRunner.setTraceLengthMultiplier(traceLengthMultiplier);
									learnerRunner.setChunkLen(chunkSize);
									learnerRunner.setSelectionID(selection);
									learnerRunner.setlearningParameters(useCentreVertex, mergeIdentifiedPathsAfterInference, useClassifyToOrderPairs);
									runner.submit(learnerRunner);
									++numberOfTasks;
								}
							ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for learning whole graphs", numberOfTasks);
							for(int count=0;count < numberOfTasks;++count)
							{
								ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
								for(SampleData sample:result.samples)
									gr_StructuralDiff.add(sample.referenceLearner.differenceStructural.getValue(),sample.actualLearner.differenceStructural.getValue());
							
								for(SampleData sample:result.samples)
								{
									gr_BCR.add(sample.referenceLearner.differenceBCR.getValue(),sample.actualLearner.differenceBCR.getValue());
								}
								progress.next();
							}
							gr_StructuralDiff.drawInteractive(gr);
							gr_BCR.drawInteractive(gr);
						}
						catch(Exception ex)
						{
							IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
							if (executorService != null) { executorService.shutdownNow();executorService = null; }
							throw e;
						}
						if (gr_StructuralDiff != null) gr_StructuralDiff.drawPdf(gr);
						if (gr_BCR != null) gr_BCR.drawPdf(gr);
			}
*/
		final int chunkSize = 3;
		final int statesMax = statesToUse[statesToUse.length-1];// reflects the size of the largest FSM that will be generated. 
		final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(outPathPrefix+"results.csv"));
		for(final int preset: new int[]{1})//,1,2})
		{
			for(final int traceQuantityToUse:new int[]{8})
			{
				int seedForFSM = 0;

				for(int states:statesToUse)
					for(int perStateSquaredDensity100:new int[] {0,30})
					{
						for(int sample=0;sample<fsmSamplesPerStateNumber;++sample,++seedForFSM)
							for(int trainingSample=0;trainingSample<trainingSamplesPerFSM;++trainingSample)
								for(boolean aveOrMax:new boolean[]{false})
									for(double traceLengthMultiplier:new double[] {32})
										for(ScoringToApply learnerKind:
												preset == 0?// this is the only case where we can apply PTA-based merging algorithms, two other presets handle merging vertices in a connected graph
													new ScoringToApply[]{
														ScoringToApply.SCORING_MARKOV,
														ScoringToApply.SCORING_EDSM_1, ScoringToApply.SCORING_EDSM_2, ScoringToApply.SCORING_EDSM_4,
														ScoringToApply.SCORING_PTAK_1, ScoringToApply.SCORING_PTAK_2, ScoringToApply.SCORING_SICCO
													}:
													new ScoringToApply[]{
														ScoringToApply.SCORING_MARKOV,
														ScoringToApply.SCORING_EDSM_1, ScoringToApply.SCORING_EDSM_2
										})
										// LEARNER_EDSMMARKOV("edsm_markov"),LEARNER_EDSM2("edsm_2"),LEARNER_EDSM4("edsm_4"),LEARNER_KTAILS_PTA1("kpta=1"),LEARNER_KTAILS_PTA2("kpta=2"),LEARNER_KTAILS_1("k=1"), LEARNER_KTAILS_2("k=2"),LEARNER_SICCO("SV");
											for(double weightOfInconsistencies:learnerKind.isMarkov()?new double[]{2.0}//1.0,2.0,4.0}
													:new double[]{1.0})
												for(int wlen:preset == 0?new int []{1} : new int[]{1,2})
													for(int divisor:preset == 0?new int []{1} : new int[]{4})
													{
														LearnerEvaluationConfiguration ev = new LearnerEvaluationConfiguration(eval);
														ev.config = eval.config.copy();ev.config.setOverride_maximalNumberOfStates(states*LearningAlgorithms.maxStateNumberMultiplier);
														ev.config.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);// small automata hence no need for array STATETREE.STATETREE_ARRAY);
														ev.config.setOverride_usePTAMerging(false);

														MarkovLearningParameters parameters = new MarkovLearningParameters(learnerKind,states, alphabetMultiplier, perStateSquaredDensity100, sample,trainingSample, seedForFSM);
														parameters.setTraceLengthMultiplier(traceLengthMultiplier);
														parameters.setExperimentID(traceQuantityToUse,traceLengthMultiplierMax,statesMax,alphabetMultiplier);
														parameters.markovParameters.setMarkovParameters(preset,chunkSize,pathsOrSets,weightOfInconsistencies, aveOrMax,divisor,0,wlen);
														parameters.setUsePrintf(experimentRunner.isInteractive());
														MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(parameters, ev);
														learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
														experimentRunner.submitTask(learnerRunner);
													}
					}
			}
		}
		
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>>() {

			@Override
			public void processSubResult(ExperimentResult<MarkovLearningParameters> result, RunSubExperiment<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>> experimentrunner) throws IOException 
			{// in these experiments, samples are singleton sequences because we run each of them in a separate process, in order to increase the efficiency with which all tasks are split between CPUs in an iceberg grid.
				SampleData sm = result.samples.get(0);
				ScoresForGraph data=sm.actualLearner;
				
				StringBuffer csvLine = new StringBuffer();
				csvLine.append(data.whetherLearningSuccessfulOrAborted);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceBCR.getValue());// 1
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceStructural.getValue());// 2
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.invalidMergers);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.missedMergers);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.nrOfstates.getValue());// 5
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.inconsistencyReference);// 6
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistency);// 7

				if (result.parameters.learnerToUse.isMarkov())
				{
					CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.fractionOfStatesIdentifiedBySingletons);
					CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovPrecision);
					CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovRecall);
					CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.comparisonsPerformed);
				}

				if (result.parameters.markovParameters.useCentreVertex) {
					CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.centreCorrect);
					CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.centrePathNumber);
				}
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.transitionsSampled);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(data.executionTime/1000000000.));// execution time is in nanoseconds, we only need seconds.
				experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
			}
			
			@Override
			public SGEExperimentResult[] getGraphs() {
				
				return new SGEExperimentResult[]{resultCSV};
			}
			
		});
		int referencePreset=0;
		for(final int preset: new int[]{1})//,1,2})
		{
			if (phase == PhaseEnum.COLLECT_AVAILABLE || phase == PhaseEnum.COLLECT_RESULTS)
			{// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
				String presetStr = "-"+preset;
				String referencePresetStr="-"+referencePreset;
				String experimentName = outPathPrefix+"preset_"+preset+"_";
				final RBagPlot gr_StructuralVsInconsistency = new RBagPlot("Inconsistency Learnt","Structural Score, EDSM-Markov learner",new File(experimentName+statesMax+"_trace_structural_inconsistency.pdf"));
				final RBagPlot gr_BCRVsInconsistency = new RBagPlot("Inconsistency Learnt","BCR Score, EDSM-Markov learner",new File(experimentName+statesMax+"_trace_bcr_inconsistency.pdf"));
				final SquareBagPlot gr_StructuralDiff = new SquareBagPlot("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File(experimentName+statesMax+"_trace_structuraldiff.pdf"),0,1,true);
				final SquareBagPlot gr_BCR = new SquareBagPlot("BCR, Sicco","BCR, EDSM-Markov learner",new File(experimentName+statesMax+"_trace_bcr.pdf"),0.5,1,true);
				final SquareBagPlot BCRAgainstKtails = new SquareBagPlot("BCR, K-tails,1","BCR, EDSM-Markov learner",new File(experimentName+"_"+statesMax+"_trace_kt_bcr.pdf"),0.5,1,true);
				final SquareBagPlot BCRAgainstEDSM_2 = new SquareBagPlot("BCR, EDSM-2","BCR, EDSM-Markov learner",new File(experimentName+"_"+statesMax+"_trace_markov_bcr.pdf"),0.5,1,true);

				final WilcoxonPairedTest Wilcoxon_test_Structural=new WilcoxonPairedTest(new File(experimentName +"Wilcoxon_t_str.csv"));
				final WilcoxonPairedTest Wilcoxon_Test_BCR=new WilcoxonPairedTest(new File(experimentName +"Wilcoxon_t_bcr.csv"));
				final Mann_Whitney_U_Test Mann_Whitney_U_Test_BCR=new Mann_Whitney_U_Test(new File(experimentName +"Mann_Whitney_U_Test_BCR.csv"));		 
				final Mann_Whitney_U_Test Mann_Whitney_U_Test_Structural=new Mann_Whitney_U_Test(new File(experimentName +"Whitney_U_Test_str.csv"));		 
				final Kruskal_Wallis Kruskal_Wallis_Test_BCR=new Kruskal_Wallis(new File(experimentName +"Kruskal_Wallis_Test_BCR.csv"));		 
				final Kruskal_Wallis Kruskal_Wallis_Test_Structural=new Kruskal_Wallis(new File(experimentName +"Kruskal_Wallis_Test_str.csv"));		 	 
				// names of columns include parameters used with learners, here we ignore that and pick those that match learner names
				DrawGraphs.spreadsheetToBagPlotNoZeroYValues(gr_StructuralVsInconsistency,resultCSV,ScoringToApply.SCORING_MARKOV+referencePresetStr,7,ScoringToApply.SCORING_MARKOV+presetStr,2,null,null);
				DrawGraphs.spreadsheetToBagPlotNoZeroYValues(gr_BCRVsInconsistency,resultCSV,ScoringToApply.SCORING_MARKOV+referencePresetStr,7,ScoringToApply.SCORING_MARKOV+presetStr,1,null,null);
				DrawGraphs.spreadsheetToBagPlot(gr_StructuralDiff,resultCSV,ScoringToApply.SCORING_SICCO+referencePresetStr,2,ScoringToApply.SCORING_MARKOV+presetStr,2,null,null);
				DrawGraphs.spreadsheetToBagPlot(gr_BCR,resultCSV,ScoringToApply.SCORING_SICCO+referencePresetStr,1,ScoringToApply.SCORING_MARKOV+presetStr,1,null,null);
				DrawGraphs.spreadsheetToBagPlot(BCRAgainstKtails,resultCSV,ScoringToApply.SCORING_PTAK_1+referencePresetStr,1,ScoringToApply.SCORING_MARKOV+presetStr,1,null,null);
				DrawGraphs.spreadsheetToBagPlot(BCRAgainstEDSM_2,resultCSV,ScoringToApply.SCORING_EDSM_2+referencePresetStr,1,ScoringToApply.SCORING_MARKOV+presetStr,1,null,null);

				DrawGraphs.spreadsheetAsDouble(Wilcoxon_Test_BCR,resultCSV,ScoringToApply.SCORING_MARKOV+presetStr,1,ScoringToApply.SCORING_SICCO+referencePresetStr,1);
				DrawGraphs.spreadsheetAsDouble(Wilcoxon_test_Structural,resultCSV,ScoringToApply.SCORING_MARKOV+presetStr,2,ScoringToApply.SCORING_SICCO+referencePresetStr,2);
				DrawGraphs.spreadsheetAsDouble(Mann_Whitney_U_Test_BCR,resultCSV,ScoringToApply.SCORING_MARKOV+presetStr,1,ScoringToApply.SCORING_SICCO+referencePresetStr,1);
				DrawGraphs.spreadsheetAsDouble(Mann_Whitney_U_Test_Structural,resultCSV,ScoringToApply.SCORING_MARKOV+presetStr,2,ScoringToApply.SCORING_SICCO+referencePresetStr,2);
				DrawGraphs.spreadsheetAsDouble(Kruskal_Wallis_Test_BCR,resultCSV,ScoringToApply.SCORING_MARKOV+presetStr,1,ScoringToApply.SCORING_SICCO+referencePresetStr,1);
				DrawGraphs.spreadsheetAsDouble(Kruskal_Wallis_Test_Structural,resultCSV,ScoringToApply.SCORING_MARKOV+presetStr,2,ScoringToApply.SCORING_SICCO+referencePresetStr,2);
				final AtomicLong comparisonsPerformed = new AtomicLong(0);
				/*
				DrawGraphs.spreadsheetAsString((A, B) -> {
					try {
						comparisonsPerformed.addAndGet(Long.parseLong(A));
					}
					catch(NumberFormatException e) {
						System.out.println("Failed to convert "+e);
					}
				},resultCSV,ScoringToApply.SCORING_MARKOV+presetStr,3,ScoringToApply.SCORING_MARKOV+presetStr,3);
					*/
				for(@SuppressWarnings("rawtypes") DrawGraphs.RExperimentResult result:new DrawGraphs.RExperimentResult[]{gr_StructuralVsInconsistency,gr_BCRVsInconsistency,gr_StructuralDiff,gr_BCR,BCRAgainstKtails,BCRAgainstEDSM_2, Wilcoxon_Test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,Kruskal_Wallis_Test_Structural,Kruskal_Wallis_Test_BCR})
				{
					result.reportResults(gr);
				}
				if (experimentRunner.isInteractive())
					System.out.println("\nLOG of comparisons performed: "+Math.log10(comparisonsPerformed.doubleValue())+"\n");
			}
		}
		
		
/*		final int traceQuantityToUse = traceQuantity;
		final int presetForBestResults = 0;
		{
			final SquareBagPlot gr_StructuralDiffWithoutInconsistencies = new SquareBagPlot("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File(branch+"_noinconsistencies_trace_structuraldiff.pdf"),0,1,true);
			final SquareBagPlot gr_BCRWithoutInconsistencies = new SquareBagPlot("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_noinconsistencies_trace_bcr.pdf"),0.5,1,true);		
			String selection = "noinconsistencies;quantity="+traceQuantity+";tracelen="+traceLengthMultiplierMax+";alphabetMult="+alphabetMultiplierMax+";";
			final AtomicLong comparisonsPerformed = new AtomicLong(0);

			for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
				for(int sample=0;sample<samplesPerFSM;++sample)
				{
					MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,experimentRunner.getTaskID(),traceQuantityToUse, config, converter);
					learnerRunner.setOnlyUsePositives(onlyPositives);
					learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
					learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);
					learnerRunner.setChunkLen(chunkSize);
					learnerRunner.setSelectionID(selection);
					learnerRunner.setPresetLearningParameters(presetForBestResults);
					learnerRunner.setDisableInconsistenciesInMergers(true);
					experimentRunner.submitTask(learnerRunner);
				}
			experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

				@Override
				public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
				{
					for(SampleData sample:result.samples)
						experimentrunner.Record(gr_StructuralDiffWithoutInconsistencies,sample.referenceLearner.differenceStructural.getValue(),sample.actualLearner.differenceStructural.getValue(),null,null);
				
					for(SampleData sample:result.samples)
					{
						experimentrunner.Record(gr_BCRWithoutInconsistencies,sample.referenceLearner.differenceBCR.getValue(),sample.actualLearner.differenceBCR.getValue(),null,null);
						comparisonsPerformed.addAndGet(sample.comparisonsPerformed);
					}
					
				}

				@Override
				public String getSubExperimentName()
				{
					return "learning without inconsistencies";
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiffWithoutInconsistencies,gr_BCRWithoutInconsistencies};
				}
			});
			
			if (experimentRunner.isInteractive())
				System.out.println("\nLOG of comparisons performed: "+Math.log10(comparisonsPerformed.doubleValue())+"\n");
		}*/

		// Same experiment but with different number of sequences.
/*		final RBoxPlot<Integer> gr_BCRImprovementForDifferentNrOfTracesWithNegatives = new RBoxPlot<Integer>("nr of traces","improvement, BCR",new File(branch+"WithNegatives_BCR_vs_tracenumber.pdf"));
		final RBoxPlot<Integer> gr_BCRForDifferentNrOfTracesWithNegatives = new RBoxPlot<Integer>("nr of traces","BCR",new File(branch+"WithNegatives_BCR_absolute_vs_tracenumber.pdf"));
		final RBoxPlot<Integer> gr_StructuralImprovementForDifferentNrOfTracesWithNegatives = new RBoxPlot<Integer>("nr of traces","improvement, structural",new File(branch+"WithNegatives_structural_vs_tracenumber.pdf"));
		final RBoxPlot<Integer> gr_StructuralForDifferentNrOfTracesWithNegatives = new RBoxPlot<Integer>("nr of traces","structural",new File(branch+"WithNegatives_structural_absolute_vs_tracenumber.pdf"));
			
		for(final int traceNum:new int[]{2})
		{
			final String selection = "number_of_traces="+traceNum;
			for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
			{
				final Wilcoxon <String> Wilcoxon_test_BCR=new Wilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_traceNum= "+traceNum+"_"+states+"Wilcoxon_trace_bcr.pdf"));		 
				final Wilcoxon <String> Wilcoxon_test_Structural=new Wilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_traceNum= "+traceNum+"_"+states+"Wilcoxon_trace_str.pdf"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_BCR=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_traceNum= "+traceNum+"_"+states+"Mann_Whitney_U_Test_BCR.pdf"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_Structural=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_traceNum= "+traceNum+"_"+states+"Mann_Whitney_U_Test_str.pdf"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_BCR=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_traceNum= "+traceNum+"_"+states+"Kruskal_Wallis_Test_BCR.pdf"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_Structural=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_traceNum= "+traceNum+"_"+states+"Kruskal_Wallis_Test_str.pdf"));		 
				for(int sample=0;sample<samplesPerFSM;++sample)
				{
					MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,experimentRunner.getTaskID(),traceNum, config, converter);
					learnerRunner.setOnlyUsePositives(false);
					learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
					learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);
					learnerRunner.setChunkLen(chunkSize);
					learnerRunner.setSelectionID(selection);
					learnerRunner.setPresetLearningParameters(presetForBestResults);
					experimentRunner.submitTask(learnerRunner);
				}
			experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

				@Override
				public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
				{
					for(SampleData sample:result.samples)
					{
						if (sample.referenceLearner.differenceBCR.getValue() > 0)
						{
							// we'll generate both positives and negatives; in the considered experiments, only positives are used hence half the number of sequences are actually being learnt from.
							experimentrunner.Record(gr_BCRImprovementForDifferentNrOfTracesWithNegatives,traceNum,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue(),null,null);
							experimentrunner.Record(gr_BCRForDifferentNrOfTracesWithNegatives,traceNum,sample.actualLearner.differenceBCR.getValue(),null,null);
						}
						if (sample.referenceLearner.differenceStructural.getValue() > 0)
						{
							experimentrunner.Record(gr_StructuralImprovementForDifferentNrOfTracesWithNegatives,traceNum,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue(),null,null);
							experimentrunner.Record(gr_StructuralForDifferentNrOfTracesWithNegatives,traceNum,sample.actualLearner.differenceStructural.getValue(),null,null);
						}
					}
					
			
					AverageValue BCRCollectResult = new AverageValue(0.0,0.0);
					AverageValue StructuralCollectResult = new AverageValue(0.0,0.0);

					double n=0.0;

					for(SampleData sample:result.samples)
					{
						BCRCollectResult.add(sample.actualLearner.differenceBCR.getValue(), sample.referenceLearner.differenceBCR.getValue());
						StructuralCollectResult.add(sample.actualLearner.differenceStructural.getValue(), sample.referenceLearner.differenceStructural.getValue());
						n++;
					}

					experimentrunner.RecordPairValue(Wilcoxon_test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
					experimentrunner.RecordPairValue(Wilcoxon_test_Structural, StructuralCollectResult.RefercneElem, StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
					experimentrunner.RecordPairValue(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
					experimentrunner.RecordPairValue(Mann_Whitney_U_Test_Structural, StructuralCollectResult.RefercneElem, StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
					experimentrunner.RecordPairValue(Kruskal_Wallis_Test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
					experimentrunner.RecordPairValue(Kruskal_Wallis_Test_Structural, StructuralCollectResult.RefercneElem, StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
					
					
				}

				@Override
				public String getSubExperimentName()
				{
					return selection;
				}
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_BCRImprovementForDifferentNrOfTracesWithNegatives,gr_BCRForDifferentNrOfTracesWithNegatives,
							gr_StructuralImprovementForDifferentNrOfTracesWithNegatives,gr_StructuralForDifferentNrOfTracesWithNegatives,Wilcoxon_test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,Kruskal_Wallis_Test_BCR,Kruskal_Wallis_Test_Structural};
				}
			});
			}
		}			
*/
		/*
		// Same experiment but with different number of sequences.
		final RBoxPlot<Integer> gr_BCRImprovementForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","improvement, BCR",new File(branch+"BCR_vs_tracenumber.pdf"));
		final RBoxPlot<Integer> gr_BCRForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","BCR",new File(branch+"BCR_absolute_vs_tracenumber.pdf"));
		final RBoxPlot<Integer> gr_StructuralImprovementForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","improvement, structural",new File(branch+"structural_vs_tracenumber.pdf"));
		final RBoxPlot<Integer> gr_StructuralForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","structural",new File(branch+"structural_absolute_vs_tracenumber.pdf"));
		
		
		for(final int traceNum:new int[]{2,4,6,8,10})
		{
			final String selection = "number_of_traces="+traceNum;	

			for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
			{
				final RWilcoxon <String> Wilcoxon_test_Structural=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+"_states_"+ states +"_Wilcoxon_t_str.csv"));		 
				final RWilcoxon <String> Wilcoxon_Test_BCR=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Wilcoxon_t_bcr.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_BCR=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Mann_Whitney_U_Test_BCR.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_Structural=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_= "+selection+ "_states_"+ states +"_Whitney_U_Test_str.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_BCR=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_BCR.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_Structural=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_str.csv"));		 	 

				for(int sample=0;sample<samplesPerFSM;++sample)
				{
					MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,experimentRunner.getTaskID(),traceNum, config, converter);
					learnerRunner.setOnlyUsePositives(onlyPositives);
					learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
					learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);
					learnerRunner.setChunkLen(chunkSize);
					learnerRunner.setSelectionID(selection);
					learnerRunner.setPresetLearningParameters(presetForBestResults);
					experimentRunner.submitTask(learnerRunner);
				}
			experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

				@Override
				public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
				{
					for(SampleData sample:result.samples)
					{
						if (sample.referenceLearner.differenceBCR.getValue() > 0)
						{
							// we'll generate both positives and negatives; in the considered experiments, only positives are used hence half the number of sequences are actually being learnt from.
							experimentrunner.Record(gr_BCRImprovementForDifferentNrOfTraces,traceNum/2,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue(),null,null);
							experimentrunner.Record(gr_BCRForDifferentNrOfTraces,traceNum/2,sample.actualLearner.differenceBCR.getValue(),null,null);
						}
						if (sample.referenceLearner.differenceStructural.getValue() > 0)
						{
							experimentrunner.Record(gr_StructuralImprovementForDifferentNrOfTraces,traceNum/2,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue(),null,null);
							experimentrunner.Record(gr_StructuralForDifferentNrOfTraces,traceNum/2,sample.actualLearner.differenceStructural.getValue(),null,null);
						}
					}
						
						AverageValue BCRCollectResult = new AverageValue(0.0,0.0);
						AverageValue StructuralCollectResult = new AverageValue(0.0,0.0);

						double n=0.0;

						for(SampleData sample:result.samples)
						{
							
								BCRCollectResult.add(sample.actualLearner.differenceBCR.getValue(), sample.referenceLearner.differenceBCR.getValue());														
								StructuralCollectResult.add(sample.actualLearner.differenceStructural.getValue(), sample.referenceLearner.differenceStructural.getValue());
								n++;
								experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
								experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
								experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
								experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
								experimentrunner.Record(Kruskal_Wallis_Test_BCR,BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
								experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
						}

//						experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Wilcoxon_test_Structural, StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Kruskal_Wallis_Test_BCR, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Kruskal_Wallis_Test_Structural, StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
					
				}

				@Override
				public String getSubExperimentName()
				{
					return selection;
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_BCRImprovementForDifferentNrOfTraces,gr_BCRForDifferentNrOfTraces,gr_StructuralImprovementForDifferentNrOfTraces,gr_StructuralForDifferentNrOfTraces,Wilcoxon_Test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,Kruskal_Wallis_Test_BCR,Kruskal_Wallis_Test_Structural};

				}
			});

		}
		
		}

		// Same experiment but with different trace length but the same number of sequences
		final RBoxPlot<Double> gr_BCRImprovementForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","improvement, BCR",new File(branch+"BCR_vs_tracelength.pdf"));
		final RBoxPlot<Double> gr_BCRForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","BCR",new File(branch+"BCR_absolute_vs_tracelength.pdf"));
		final RBoxPlot<Double> gr_StructuralImprovementForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","improvement, structural",new File(branch+"structural_vs_tracelength.pdf"));
		final RBoxPlot<Double> gr_StructuralForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","structural",new File(branch+"structural_absolute_vs_tracelength.pdf"));
		final RBoxPlot<Double> gr_TransitionCoverageForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","transition coverage",new File(branch+"transitionCoverage_vs_tracelength.pdf"));

		for(final int traceNum:new int[]{10})
			for(double traceLengthMultiplierToUse=0.125;traceLengthMultiplierToUse<4;traceLengthMultiplierToUse*=2.) 
			{
				final String selection="traceLengthMultiplier="+traceLengthMultiplierToUse;
				final double traceLengthMultToUse = traceLengthMultiplierToUse;
				for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
				{
					final RWilcoxon <String> Wilcoxon_test_Structural=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+"_states="+ states +"_Wilcoxon_t_str.csv"));		 
					final RWilcoxon <String> Wilcoxon_Test_BCR=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Wilcoxon_t_bcr.csv"));		 
					final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_BCR=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Mann_Whitney_U_Test_BCR.csv"));		 
					final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_Structural=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_= "+selection+ "_states="+ states +"_Whitney_U_Test_str.csv"));		 
					final Kruskal_Wallis <String> Kruskal_Wallis_Test_BCR=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_BCR.csv"));		 
					final Kruskal_Wallis <String> Kruskal_Wallis_Test_Structural=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_str.csv"));		 	 

					for(int sample=0;sample<samplesPerFSM;++sample)
					{
						MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,experimentRunner.getTaskID(),traceNum, config, converter);
						learnerRunner.setOnlyUsePositives(onlyPositives);
						learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
						learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierToUse);
						learnerRunner.setChunkLen(chunkSize);
						learnerRunner.setSelectionID(selection);
						learnerRunner.setPresetLearningParameters(presetForBestResults);
						experimentRunner.submitTask(learnerRunner);
					}
				experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

					@Override
					public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
					{
						for(SampleData sample:result.samples)
						{
							if (sample.referenceLearner.differenceBCR.getValue() > 0)
							{
								experimentrunner.Record(gr_BCRImprovementForDifferentLengthOfTraces,traceLengthMultToUse,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue(),null,null);
								experimentrunner.Record(gr_BCRForDifferentLengthOfTraces,traceLengthMultToUse,sample.actualLearner.differenceBCR.getValue(),null,null);
							}
							if (sample.referenceLearner.differenceStructural.getValue() > 0)
							{
								experimentrunner.Record(gr_StructuralImprovementForDifferentLengthOfTraces,traceLengthMultToUse,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue(),null,null);
								experimentrunner.Record(gr_StructuralForDifferentLengthOfTraces,traceLengthMultToUse,sample.actualLearner.differenceStructural.getValue(),null,null);
							}
							experimentrunner.Record(gr_TransitionCoverageForDifferentLengthOfTraces,traceLengthMultToUse,(double)sample.transitionsSampled,null,null);
						}
						
						AverageValue BCRCollectResult = new AverageValue(0.0,0.0);
						AverageValue StructuralCollectResult = new AverageValue(0.0,0.0);

						double n=0.0;

						for(SampleData sample:result.samples)
						{
							BCRCollectResult.add(sample.actualLearner.differenceBCR.getValue(), sample.referenceLearner.differenceBCR.getValue());
							StructuralCollectResult.add(sample.actualLearner.differenceStructural.getValue(), sample.referenceLearner.differenceStructural.getValue());
							n++;
							experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
							experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
							experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
							experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
							experimentrunner.Record(Kruskal_Wallis_Test_BCR,BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
							experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
						}

//						experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Kruskal_Wallis_Test_BCR,BCRCollectResult.RefercneElem, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
						
					}

					@Override
					public String getSubExperimentName()
					{
						return selection;
					}
					
					@SuppressWarnings("rawtypes")
					@Override
					public RGraph[] getGraphs() {
						return new RGraph[]{Wilcoxon_Test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,gr_BCRImprovementForDifferentLengthOfTraces,gr_BCRForDifferentLengthOfTraces,gr_StructuralImprovementForDifferentLengthOfTraces,
								gr_StructuralForDifferentLengthOfTraces,gr_TransitionCoverageForDifferentLengthOfTraces,Kruskal_Wallis_Test_Structural,Kruskal_Wallis_Test_BCR};
					}
				});
				}
			}

		final RBoxPlot<Integer> gr_BCRImprovementForDifferentPrefixlen = new RBoxPlot<Integer>("length of prefix","improvement, BCR",new File(branch+"BCR_vs_prefixLength.pdf"));
		final RBoxPlot<Integer> gr_BCRForDifferentPrefixlen = new RBoxPlot<Integer>("length of prefix","BCR",new File(branch+"BCR_absolute_vs_prefixLength.pdf"));
		final RBoxPlot<Integer> gr_StructuralImprovementForDifferentPrefixlen = new RBoxPlot<Integer>("length of prefix","improvement, structural",new File(branch+"structural_vs_prefixLength.pdf"));
		final RBoxPlot<Integer> gr_StructuralForDifferentPrefixlen = new RBoxPlot<Integer>("length of prefix","structural",new File(branch+"structural_absolute_vs_prefixLength.pdf"));
		final RBoxPlot<String> gr_MarkovAccuracyForDifferentPrefixlen = new RBoxPlot<String>("length of prefix","Markov accuracy",new File(branch+"markov_accuracy_vs_prefixLength.pdf"));
		for(int prefixLength=1;prefixLength<3;++prefixLength) 
		{
			final String selection="prefix Length ="+prefixLength;
			final int prefixLen = prefixLength;
			for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
			{
				final RWilcoxon <String> Wilcoxon_test_Structural=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+"_states="+ states +"_Wilcoxon_t_str.csv"));		 
				final RWilcoxon <String> Wilcoxon_Test_BCR=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Wilcoxon_t_bcr.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_BCR=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Mann_Whitney_U_Test_BCR.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_Structural=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_= "+selection+ "_states="+ states +"_Whitney_U_Test_str.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_BCR=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_BCR.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_Structural=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_str.csv"));		 	 
			
				for(int sample=0;sample<samplesPerFSM;++sample)
				{
					MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,experimentRunner.getTaskID(),traceQuantity, config, converter);
					learnerRunner.setOnlyUsePositives(onlyPositives);
					learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
					learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);
					learnerRunner.setChunkLen(prefixLength+1);
					learnerRunner.setSelectionID(selection);
					learnerRunner.setPresetLearningParameters(presetForBestResults);
					experimentRunner.submitTask(learnerRunner);
				}
			experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

				@Override
				public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
				{
					for(SampleData sample:result.samples)
					{
						if (sample.referenceLearner.differenceBCR.getValue() > 0)
						{
							experimentrunner.Record(gr_BCRImprovementForDifferentPrefixlen,prefixLen,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue(),null,null);
							experimentrunner.Record(gr_BCRForDifferentPrefixlen,prefixLen,sample.actualLearner.differenceBCR.getValue(),null,null);
						}
						if (sample.referenceLearner.differenceStructural.getValue() > 0)
						{
							experimentrunner.Record(gr_StructuralImprovementForDifferentPrefixlen,prefixLen,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue(),null,null);
							experimentrunner.Record(gr_StructuralForDifferentPrefixlen,prefixLen,sample.actualLearner.differenceStructural.getValue(),null,null);
						}
						experimentrunner.Record(gr_MarkovAccuracyForDifferentPrefixlen,""+prefixLen+",P",(double)sample.markovPrecision,"green",null);
						experimentrunner.Record(gr_MarkovAccuracyForDifferentPrefixlen,""+prefixLen+",R",(double)sample.markovRecall,"blue",null);
					}
					
					AverageValue BCRCollectResult = new AverageValue(0.0,0.0);
					AverageValue StructuralCollectResult = new AverageValue(0.0,0.0);

					double n=0.0;

					for(SampleData sample:result.samples)
					{
						BCRCollectResult.add(sample.actualLearner.differenceBCR.getValue(), sample.referenceLearner.differenceBCR.getValue());
						StructuralCollectResult.add(sample.actualLearner.differenceStructural.getValue(), sample.referenceLearner.differenceStructural.getValue());
						n++;
						experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
						experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
						experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
						experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
						experimentrunner.Record(Kruskal_Wallis_Test_BCR,BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
						experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
					}

//					experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Kruskal_Wallis_Test_BCR,BCRCollectResult.RefercneElem, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
				}

				@Override
				public String getSubExperimentName()
				{
					return selection;
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{Wilcoxon_Test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,gr_BCRImprovementForDifferentPrefixlen,gr_BCRForDifferentPrefixlen,gr_StructuralImprovementForDifferentPrefixlen,gr_StructuralForDifferentPrefixlen,gr_MarkovAccuracyForDifferentPrefixlen,Kruskal_Wallis_Test_Structural,Kruskal_Wallis_Test_BCR};
				}
			});
			}
		}
		
	

		final RBoxPlot<String> gr_BCRImprovementForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","improvement, BCR",new File(branch+"BCR_vs_alphabet.pdf"));
		final RBoxPlot<String> gr_BCRForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","BCR",new File(branch+"BCR_absolute_vs_alphabet.pdf"));
		final RBoxPlot<String> gr_StructuralImprovementForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","improvement, structural",new File(branch+"structural_vs_alphabet.pdf"));
		final RBoxPlot<String> gr_StructuralForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","structural",new File(branch+"structural_absolute_vs_alphabet.pdf"));
		final RBoxPlot<String> gr_MarkovAccuracyForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","Markov accuracy",new File(branch+"markov_accuracy_vs_alphabet.pdf"));

		// Same experiment but with different alphabet size
		for(final double alphabetMultiplierActual:new double[]{alphabetMultiplierMax/4,alphabetMultiplierMax/2,alphabetMultiplierMax,alphabetMultiplierMax*2,alphabetMultiplierMax*4}) 
		{
			final String selection="alphabet_size="+alphabetMultiplierActual;

			for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
			{
				
				final RWilcoxon <String> Wilcoxon_test_Structural=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+"_states="+ states +"_Wilcoxon_t_str.csv"));		 
				final RWilcoxon <String> Wilcoxon_Test_BCR=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Wilcoxon_t_bcr.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_BCR=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Mann_Whitney_U_Test_BCR.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_Structural=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_= "+selection+ "_states="+ states +"_Whitney_U_Test_str.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_BCR=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_BCR.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_Structural=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_str.csv"));		 	 
			
				for(int sample=0;sample<samplesPerFSM;++sample)
				{
					MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,experimentRunner.getTaskID(),traceQuantity, config, converter);
					learnerRunner.setOnlyUsePositives(onlyPositives);
					learnerRunner.setTracesAlphabetMultiplier(alphabetMultiplierMax);
					learnerRunner.setAlphabetMultiplier(alphabetMultiplierActual);
					learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);learnerRunner.setChunkLen(chunkSize);
					learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
					learnerRunner.setPresetLearningParameters(presetForBestResults);
					experimentRunner.submitTask(learnerRunner);
				}
			experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

				@Override
				public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
				{
					for(SampleData sample:result.samples)
					{
						if (sample.referenceLearner.differenceBCR.getValue() > 0)
						{
							experimentrunner.Record(gr_BCRImprovementForDifferentAlphabetSize,String.format("%.2f",alphabetMultiplierActual),sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue(),null,null);
							experimentrunner.Record(gr_BCRForDifferentAlphabetSize,String.format("%.2f",alphabetMultiplierActual),sample.actualLearner.differenceBCR.getValue(),null,null);
						}
						if (sample.referenceLearner.differenceStructural.getValue() > 0)
						{
							experimentrunner.Record(gr_StructuralImprovementForDifferentAlphabetSize,String.format("%.2f",alphabetMultiplierActual),sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue(),null,null);
							experimentrunner.Record(gr_StructuralForDifferentAlphabetSize,String.format("%.2f",alphabetMultiplierActual),sample.actualLearner.differenceStructural.getValue(),null,null);
						}
						experimentrunner.Record(gr_MarkovAccuracyForDifferentAlphabetSize,String.format("%.2f,P",alphabetMultiplierActual),(double)sample.markovPrecision,"green",null);
						experimentrunner.Record(gr_MarkovAccuracyForDifferentAlphabetSize,String.format("%.2f,R",alphabetMultiplierActual),(double)sample.markovRecall,"blue",null);
					}
					
					AverageValue BCRCollectResult = new AverageValue(0.0,0.0);
					AverageValue StructuralCollectResult = new AverageValue(0.0,0.0);

					double n=0.0;

					for(SampleData sample:result.samples)
					{
						BCRCollectResult.add(sample.actualLearner.differenceBCR.getValue(), sample.referenceLearner.differenceBCR.getValue());
						StructuralCollectResult.add(sample.actualLearner.differenceStructural.getValue(), sample.referenceLearner.differenceStructural.getValue());
						n++;
						experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
						experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
						experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
						experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
						experimentrunner.Record(Kruskal_Wallis_Test_BCR,BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
						experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
					}

//					experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Kruskal_Wallis_Test_BCR, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
				}

				@Override
				public String getSubExperimentName()
				{
					return selection;
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{Wilcoxon_Test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,gr_BCRImprovementForDifferentAlphabetSize,gr_StructuralImprovementForDifferentAlphabetSize,gr_BCRForDifferentAlphabetSize,
							gr_StructuralForDifferentAlphabetSize,gr_MarkovAccuracyForDifferentAlphabetSize,Kruskal_Wallis_Test_Structural,Kruskal_Wallis_Test_BCR};
				}
			});
			}
		}
		*/

		}
		catch(Exception ex)
		{
			ex.printStackTrace();
		}
		finally
		{
			experimentRunner.successfulTermination();
			DrawGraphs.end();// this is necessary to ensure termination of the JVM runtime at the end of experiments.
		}
	}
	
	public static class AverageValue
	{
		public double actualElem, RefercneElem;
		public AverageValue(double a, double b) {
			actualElem=a;RefercneElem=b;
		}
		
		public AverageValue add(double a, double b)
		{
			actualElem+=a;RefercneElem+=b;return this;
		}
		
		public AverageValue add(AverageValue d)
		{
			add(d.actualElem,d.RefercneElem);return this;
		}
		
		@Override
		public String toString()
		{
			return "(Actual: "+actualElem+", Reference: "+RefercneElem+")";
		}
	}
}

