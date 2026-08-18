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
import java.util.*;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;
import statechum.*;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.Erlang.Synapse;
import statechum.analysis.learning.*;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
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
import statechum.analysis.learning.linear.DifferenceVisualiser;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.*;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;

import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.RESULT_SECTION.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.RESULT_VALUES.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersColumnFromCSV;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.parseMarkovParametersRowFromCSV;
import static statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.constructLearner;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK;
import static statechum.analysis.learning.rpnicore.AbstractLearnerGraph.LearningAbortedReason.LEARNING_TIMEOUT;


public class MarkovExperiment
{
	
	public static final String directoryNamePrefix = "markov";
	public static final String directoryExperimentResult = "experimentresult"+File.separator;
	public static final String directoryExperimentStatistics = "experimentstatistics"+File.separator;

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
			final double density = (double) (par.states * par.perStateSquaredDensityMultipliedBy100) / 100.;
			MachineGenerator mg = new MachineGenerator(par.states, 400 , (int)Math.round((double)par.states/5));mg.setGenerateConnected(true);
			
			try {
			// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
				referenceGraph = mg.nextMachine(alphabet, density,Objects.hash(par.sample,par.perStateSquaredDensityMultipliedBy100,par.states), learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()).pathroutines.buildDeterministicGraph();
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
			if (par.walkType != null) {
				generator.setWalkType(par.walkType);generator.setExplorationPreferenceAndPenalty(par.explorationPreference, par.selectionPenalty);
			}
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

		long startTime = 0;

		@Override
		public ExperimentResult<MarkovLearningParameters> runexperiment() throws Exception 
		{
			generateReferenceFSM();
			saveGraph(nameReference,referenceGraph);

			ExperimentResult<MarkovLearningParameters> outcome = new ExperimentResult<>(par);
			learnerInitConfiguration.testSet = LearningAlgorithms.buildEvaluationSet(referenceGraph);
			
			LearnerGraph pta = constructPTA();
	
			final MarkovModel markovModel = new MarkovModel(par.markovParameters.chunkLen,true,true,true,false);
			markovModel.createMarkovFromPositiveDataAndGenerateInversePredictions(new ArrayList<>(),true);// use the new 'positive' version of fanout inconsistency computation.
			new MarkovClassifierLG(markovModel, pta,null).updateMarkov(false);
			
			pta.clearColours();

			assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
			
			final Configuration deepCopy = pta.config.copy();deepCopy.setLearnerCloneGraph(true);
			LearnerGraph ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);

//			LearnerGraph trimmedReference = LearningSupportRoutines.trimUncoveredTransitions(pta,referenceGraph);
			final ConsistencyChecker checker = par.markovParameters.penaliseMissingPaths?
					new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes() :
					new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklisting();
			long inconsistencyForTheReferenceGraph = MarkovClassifier.computeInconsistency(referenceGraph, null, markovModel, checker,false);

			PerformFirstMerge firstMerge = new PerformFirstMerge();firstMerge.ptaToUseForInference=pta;
			if (par.markovParameters.useCentreVertex)
			{
				saveGraph(namePTABEFORECENTRE,pta);
				// This replaces firstMerge.ptaToUseForInference with a graph built by merging around the most-connected vertex
				firstMerge.buildFirstGraph(pta, referenceGraph, par.markovParameters, markovModel, checker);
				if (par.usePrintf) {
					LearnerGraphND inverseOfPtaAfterInitialMerge = MarkovClassifier.computeInverseGraph(firstMerge.ptaToUseForInference);
					System.out.println("Centre vertex: " + firstMerge.vertexWithMostTransitions + " number of transitions: " +
							WaveBlueFringe.countTransitions(firstMerge.ptaToUseForInference,
									inverseOfPtaAfterInitialMerge, firstMerge.vertexWithMostTransitions)+
							" , reduction: "+pta.getStateNumber()+" -> "+firstMerge.ptaToUseForInference.getStateNumber());
				}
			}
	
			SampleData dataSample = new SampleData(referenceGraph,null);

			EDSM_MarkovLearner markovLearner = null;
			ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown redReducer = null;
			saveGraph(namePTA, firstMerge.ptaToUseForInference);// although it may seem that pars.getExperimentID()
			// would be a better name than a full name, in cases where we use a middle vertex PTA to start from is
			// different to the one generated from a reference graph. Hence using full name and recording lots of graphs.

			// Ideally, we would like to record learnt graph and only rebuilt comparison results when asked. This is
			// not possible because without a learning process there is no record which mergers
			// were right or not and we will not have information how long it took for a learner to complete learning.
			LearnerGraph ptaBuilt = firstMerge.ptaToUseForInference;
			Learner learnerOfPairs;
			switch(par.learnerToUse)
			{
				case SCORING_MARKOV:
					redReducer = new ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown(referenceGraph,false,par.markovParameters.chunkLen,par.reportMergeStatisticsWhenTheCorrectSolutionIsKnown);
					markovLearner = new EDSM_MarkovLearner(learnerInitConfiguration,ptaBuilt,0,
							par.markovParameters,ScoreMode.ONLYOVERRIDE, redReducer);
					markovLearner.setMarkov(markovModel);markovLearner.setChecker(checker);
					learnerOfPairs = markovLearner;
					break;
                case SCORING_ORACLE_STATISTICS:
                    redReducer = new ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown(referenceGraph,false,par.markovParameters.chunkLen,par.reportMergeStatisticsWhenTheCorrectSolutionIsKnown);
                    markovLearner = new LearnerRelyingOnOracle(learnerInitConfiguration,ptaBuilt,0,
                            par.markovParameters,ScoreMode.GENERAL_NOFULLMERGE, redReducer,referenceGraph);
                    markovLearner.setMarkov(markovModel);markovLearner.setChecker(checker);
                    learnerOfPairs = markovLearner;
                    break;
				default:
					// ScoreMode.GENERAL_NOFULLMERGE is ok here because all states are accept-states,
					// otherwise GENERAL_PLUS_NOFULLMERGE might have been a better choice.
					redReducer = new ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown(referenceGraph,false);
					learnerOfPairs = constructLearner(learnerInitConfiguration,ptaBuilt, par.learnerToUse,ScoreMode.GENERAL_NOFULLMERGE,redReducer);
					break;
			}

			startTime = LearningSupportRoutines.getThreadTime();
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

//			Visualiser.updateFrame(referenceGraph,learntGraph);
//			Visualiser.waitForKey();

			dataSample.actualLearner = WaveBlueFringe.estimateDifference(actualAutomaton,markovModel,checker,referenceGraph,learnerInitConfiguration.testSet);
			if (redReducer != null)
			{
				dataSample.actualLearner.invalidMergersFarFromRoot = redReducer.getInvalidMergersFarFromRoot();dataSample.actualLearner.missedMergersFarFromRoot = redReducer.getMissedMergersFarFromRoot();
				dataSample.actualLearner.invalidMergersNearRoot = redReducer.getInvalidMergersNearRoot();dataSample.actualLearner.missedMergersNearRoot = redReducer.getMissedMergersNearRoot();
				dataSample.actualLearner.validMergers = redReducer.getValidMergers();
			}
			dataSample.actualLearner.whetherLearningSuccessfulOrAborted = actualAutomaton.getLearningAbortedReason();
			dataSample.actualLearner.executionTime = runTime;

			if (markovLearner instanceof LearnerRelyingOnOracle)
				dataSample.actualLearner.mergeStatistics = ((LearnerRelyingOnOracle)markovLearner).getStatistics();
			dataSample.inconsistencyReference = MarkovClassifier.computeInconsistency(referenceGraph, null, markovModel, checker,false);
			dataSample.referenceLearner = zeroScore;
			dataSample.centreCorrect = firstMerge.correctCentre;
			dataSample.centrePathNumber = firstMerge.centrePathNumber;
			dataSample.fractionOfStatesIdentifiedBySingletons=Math.round(100*MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(referenceGraph));
			dataSample.stateNumber = referenceGraph.getStateNumber();
			LearnerGraph trimmedGraph = LearningSupportRoutines.trimUncoveredTransitions(pta,referenceGraph);


//			LearnerGraphND grA=buildLearnerGraphND(graphA, "labellingDemo_A_"+counter,config,null),
//					grB=buildLearnerGraphND(graphB, "labellingDemo_B_"+counter,config,null);

//			if (dataSample.actualLearner.differenceStructural.getValue() > 0.965) {
//				DirectedSparseGraph gr = DifferenceVisualiser.ChangesToGraph.computeVisualisationParameters(
//						Synapse.StatechumProcess.constructFSM(referenceGraph), DifferenceVisualiser.ChangesToGraph.computeGD(referenceGraph, actualAutomaton, referenceGraph.config));
//				Visualiser.updateFrame(gr, null);gr.setUserDatum(JUConstants.TITLE,referenceGraph.getName(), UserData.SHARED);
//				Visualiser.updateFrame(actualAutomaton,referenceGraph);
//				Visualiser.waitForKey();
//			}

			dataSample.transitionsSampled = Math.round(100*(double)trimmedGraph.pathroutines.countEdges()/referenceGraph.pathroutines.countEdges());
			statechum.Pair<Double,Double> correctnessOfTransitionPredictionsByMarkov = new MarkovClassifierLG(markovModel, referenceGraph,null).evaluateCorrectnessOfMarkov(true, false);
			dataSample.markovTransitionPrecision = Math.round(100*correctnessOfTransitionPredictionsByMarkov.firstElem);dataSample.markovTransitionRecall = Math.round(100*correctnessOfTransitionPredictionsByMarkov.secondElem);
			statechum.Pair<Double,Double> correctnessOfHolePredictionsByMarkov = new MarkovClassifierLG(markovModel, referenceGraph,null).evaluateCorrectnessOfHolePredictionByMarkov();
			dataSample.markovHolePrecision = Math.round(100*correctnessOfHolePredictionsByMarkov.firstElem);dataSample.markovHoleRecall = Math.round(100*correctnessOfHolePredictionsByMarkov.secondElem);
 			if (markovLearner != null) {
				dataSample.comparisonsPerformed = markovLearner.markovHelper.comparisonsPerformed;
				boolean inconsistencyAlwaysPositive = true;
				double average = 0;
				for(long value:redReducer.getInconsistencyValues()) {
					average += value;
					if (value < 0)
						inconsistencyAlwaysPositive = false;
				}
				dataSample.actualLearner.inconsistencyAlwaysPositive = inconsistencyAlwaysPositive;
				dataSample.actualLearner.inconsistencyAverage = 0;
				dataSample.actualLearner.inconsistencySD = 0;
				if (!redReducer.getInconsistencyValues().isEmpty()) {
					average /= redReducer.getInconsistencyValues().size();
					dataSample.actualLearner.inconsistencyAverage = average;

					double square_diff = 0;
					for (long value : redReducer.getInconsistencyValues())
						square_diff += (value - average) * (value - average);
					dataSample.actualLearner.inconsistencySD = Math.sqrt(square_diff/redReducer.getInconsistencyValues().size());

                }
                {
                    final MarkovModel markovModelFromReference = new MarkovModel(par.markovParameters.chunkLen, true, true, true, false);
                    dataSample.predictionAccuracyForReferenceGraph = markovModelFromReference.computeSelfInconsistencyFromAutomaton(referenceGraph);
                }
                dataSample.actualLearner.relativeInconsistency = -1;//MarkovClassifier.evaluateSignificanceOfObtainedInconsistency(actualAutomaton,learnerInitConfiguration.getLabelConverter(),markovModel,checker,20);
                dataSample.actualLearner.predictionAccuracy = -1;
                if (dataSample.actualLearner.whetherLearningSuccessfulOrAborted == AbstractLearnerGraph.LearningAbortedReason.LEARNING_OK)
                {
                    final MarkovModel markovModelFromLearnt = new MarkovModel(par.markovParameters.chunkLen,true,true,true,false);
                    dataSample.actualLearner.predictionAccuracy = markovModelFromLearnt.computeSelfInconsistencyFromAutomaton(actualAutomaton);
                }
				dataSample.actualLearner.density = (double)actualAutomaton.pathroutines.countEdges()/(actualAutomaton.getStateNumber() * actualAutomaton.getStateNumber());
			}
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
		public Collection<CmpVertex> getSurroundingStates(CmpVertex currentRed)
		{
			return	markovHelper.getSurroundingStates(currentRed);
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

		public EDSM_MarkovLearner(LearnerEvaluationConfiguration evalCnf, final LearnerGraph argInitialPTA, int threshold,
                                  MarkovParameters markovPars,Configuration.ScoreMode scoreMode,
                                  StateMergingStatistics redReducer)
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

    public static class LearnerRelyingOnOracle extends EDSM_MarkovLearner {
        final LearnerGraph referenceGraph;
		final List<PairQualityLearner.PairScoreValue> statistics = new  ArrayList<>();

		public List<PairQualityLearner.PairScoreValue> getStatistics() {
			return statistics;
		}

        public LearnerRelyingOnOracle(LearnerEvaluationConfiguration evalCnf, LearnerGraph argInitialPTA, int threshold,
									  MarkovParameters markovPars, ScoreMode scoreMode, StateMergingStatistics redReducer,
									  LearnerGraph ref) {
            super(evalCnf, argInitialPTA, threshold, markovPars, scoreMode, redReducer);
            referenceGraph = ref;
        }

		@Override
		public void initComputation(LearnerGraph graph) {
			super.initComputation(graph);
			long runTime = LearningSupportRoutines.getThreadTime()-startTime;
			long runTimeSec = Math.round(runTime / 1000000000.);
//			System.out.println(runTimeSec+" - "+statistics.size());
		}

		/** The purpose of this method is to compute scores. For this very learner, scores are obtained by comparison of pairs against a
		 * reference graph. This is needed to go through a 'perfect' inference process, guided by knowing the correct answer, and
		 * recording scores and inconsistencies in order to evaluate the relation between the two. Using a 'real' learner for the purpose of
		 * collecting such statistics is hard without knowing how to 'offset' inconsistencies against scores
		 * (because it will either merge too much or too little, giving a heavily skewed distribution), but importantly it is vital to be able
		 * to tell whether a merge is correct or not: if we simply merge states that appear plausible, some mergers could be correct and some
		 * will not be. Therefore, we'll find ourselves dealing with cases where we merge two sets of states and some pairs between these sets
		 * are correct mergers and some are not and even figuring out which are which is hard since a short path from the root to any of these
		 * states might not match any path in a reference automaton.
		 *
		 * @param p pair which score is to be returned.
		 * @return the score for this pair.
		 */
        @Override
        public long overrideScoreComputation(PairScore p) {
            List<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<>();//coregraph.getStateNumber()+1);// to ensure arraylist does not reallocate when we fill in the last element
            int genScore = coregraph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge, false);
            markovHelper.computeScoreBasedOnInconsistencies(p);
            lastComputedCompatibilityScore = markovHelper.getLastComputedInconsistency();

            Set<CmpVertex> statesOfInterest = new HashSet<>();
            statesOfInterest.add(p.getQ());statesOfInterest.add(p.getR());
            Map<CmpVertex,LinkedList<Label>> stateToPath = PairOfPaths.convertSetOfStatesToPaths(coregraph, statesOfInterest);
            CmpVertex blue = referenceGraph.getVertex(stateToPath.get(p.getQ()));
            assert blue != null;
            CmpVertex red = referenceGraph.getVertex(stateToPath.get(p.getR()));
            assert red != null;

			boolean correctMerge = blue == red;

			statistics.add(new PairQualityLearner.PairScoreValue(correctMerge,genScore,lastComputedCompatibilityScore));
            return correctMerge?1000:-1;
        }

        @Override
        public String toString()
        {
            return "OracleBasedLearner_Statistics";
        }
    }

	static class LearningReport {
		double bcr = 0, structural = 0;
		String columnText = "NONE";
		long inconsistency = -1;
		boolean alwaysPositive = true;
		String Yvalues = null;
		MarkovLearningParameters.ColumnParseOutcome column;

		public LearningReport() {
		}

		public LearningReport(double bcr, double structural, long inconsistency, boolean alwaysPositive, String columnText, String Yvalues, MarkovLearningParameters.ColumnParseOutcome column) {
			this.bcr = bcr;
			this.structural = structural;
			this.inconsistency = inconsistency;
			this.columnText = columnText;
			this.alwaysPositive = alwaysPositive;
			this.Yvalues = Yvalues;
			this.column = column;
		}

		@Override
		public boolean equals(Object o) {
			if (!(o instanceof LearningReport)) return false;
			LearningReport that = (LearningReport) o;
			return Double.compare(bcr, that.bcr) == 0 && Double.compare(structural, that.structural) == 0 && inconsistency == that.inconsistency && alwaysPositive == that.alwaysPositive && Objects.equals(columnText, that.columnText) && Objects.equals(Yvalues, that.Yvalues) && Objects.equals(column, that.column);
		}

		@Override
		public int hashCode() {
			return Objects.hash(bcr, structural, columnText, inconsistency, alwaysPositive, Yvalues, column);
		}

		public void updateIfValueBetter(LearningReport report) {
			if ((inconsistency < 0 && report.inconsistency >= 0) || inconsistency > report.inconsistency || (!alwaysPositive && report.alwaysPositive)) {
				bcr = report.bcr;
				structural = report.structural;
				inconsistency = report.inconsistency;
				columnText = report.columnText;
				Yvalues = report.Yvalues;
				column = report.column;
			}
		}

        @Override
        public String toString() {
            return "LearningReport{" +
                    "bcr=" + bcr +
                    ", structural=" + structural +
                    ", inconsistency=" + inconsistency +
                    ", alwaysPositive=" + alwaysPositive +
                    ", descr='" + columnText + '\'' +
                    ", Y='" + Yvalues + '\'' +
                    '}';
        }
    }

	public enum RESULT_SECTION {
		SECTION_MAIN, SECTION_MARKOV, SECTION_CENTRE, SECTION_END;
	};

	public enum RESULT_VALUES {
		E_SUCCESS(SECTION_MAIN, 0),
		E_BCR(SECTION_MAIN, 1),
		E_DIFF(SECTION_MAIN, 2),
		E_ERR_INVALID_NEARROOT(SECTION_MAIN, 3),
		E_ERR_MISSED_NEARROOT(SECTION_MAIN, 4),
		E_ERR_INVALID_FARFROMROOT(SECTION_MAIN, 5),
		E_ERR_MISSED_FARFROMROOT(SECTION_MAIN, 6),
		E_VALIDMERGERS(SECTION_MAIN, 7),
		E_EXTRASTATES(SECTION_MAIN, 8),
		E_INCONSISTENCY_REFERENCE(SECTION_MAIN, 9),
		E_INCONSISTENCY_LEARNT(SECTION_MAIN, 10),
		E_SECTION_MAIN_END(SECTION_MAIN, 11),

		E_INCONSISTENCY_AVERAGE(SECTION_MARKOV, 0),
		E_INCONSISTENCY_SD(SECTION_MARKOV, 1),
		E_INCONSISTENCY_ALWAYSPOSITIVE(SECTION_MARKOV, 2),
		E_FRACTION_SINGLETONS(SECTION_MARKOV, 3),
		E_MARKOV_TRANSITION_PRECISION(SECTION_MARKOV, 4),
		E_MARKOV_TRANSITION_RECALL(SECTION_MARKOV, 5),
		E_MARKOV_HOLE_PRECISION(SECTION_MARKOV, 6),
		E_MARKOV_HOLE_RECALL(SECTION_MARKOV, 7),
		E_MARKOV_PREDICTIONACCURACY_REFERENCE(SECTION_MARKOV, 8),
		E_RELATIVEINCONSISTENCY_LEARNT(SECTION_MARKOV, 9),
		E_MARKOV_PREDICTIONACCURACY_LEARNT(SECTION_MARKOV, 10),
		E_MARKOV_COMPARISONSPERFORMED(SECTION_MARKOV, 11),
		E_SECTION_MARKOV_END(SECTION_MARKOV, 12),

		E_CENTRE_CORRECT(SECTION_CENTRE, 0),
		E_CENTRE_PATH_NUMBER(SECTION_CENTRE, 1),
		E_SECTION_CENTRE_END(SECTION_MARKOV, 2),

		E_ALPHABET_SIZE(SECTION_END, 0),
		E_DENSITY_REFERENCE(SECTION_END, 1),
		E_DENSITY_LEARNT(SECTION_END, 2),
		E_TRANSITIONS_SAMPLED(SECTION_END, 3),
		E_RUNTIME(SECTION_END, 4);

		public final int value;
		public final RESULT_SECTION section;

		RESULT_VALUES(RESULT_SECTION s, int v)
		{
			value = v;section = s;
		}

		public static int getOffset(RESULT_VALUES v, MarkovLearningParameters.ColumnParseOutcome column) {
			int offset = 0;

			if (v.section == RESULT_SECTION.SECTION_MAIN)
				return offset + v.value;

			offset += E_SECTION_MAIN_END.value;

			if (column.learner.isMarkov()) {
				if (v.section == RESULT_SECTION.SECTION_MARKOV)
					return offset + v.value;

				offset+= E_SECTION_MARKOV_END.value;
			}
			else
				if (v.section == RESULT_SECTION.SECTION_MARKOV)
					throw new IllegalArgumentException("Requested markov value "+v+" but column does not correspond to Markov learner");

			if (column.parameters.useCentreVertex) {
				if (v.section == SECTION_CENTRE)
					return offset + v.value;

				offset+= E_SECTION_CENTRE_END.value;
			}
			else
				if (v.section == SECTION_CENTRE)
					throw new IllegalArgumentException("Requested centre value "+v+" but column does not correspond to a centre-based learner");

			if (v.section == RESULT_SECTION.SECTION_END)
				return offset + v.value;
			else
				throw new IllegalArgumentException("Unknown section "+v.section);
		}
	}

	public interface ExperimentBlockForColumn {
		void processBlock(MarkovLearningParameters.ColumnParseOutcome column,String columnText, String block);
	}

	public interface ColumnSelector {
		boolean check(MarkovLearningParameters.ColumnParseOutcome column);
	}

	public static class ColLearner implements ColumnSelector {
		public final LearningAlgorithms.ScoringToApply scorer;
		public ColLearner(LearningAlgorithms.ScoringToApply s) {
			scorer = s;
		}

		@Override
		public boolean check(MarkovLearningParameters.ColumnParseOutcome column) {
			return column.learner == scorer;
		}

		@Override
		public boolean equals(Object o) {
			if (!(o instanceof ColLearner)) return false;
			ColLearner that = (ColLearner) o;
			return scorer == that.scorer;
		}

		@Override
		public int hashCode() {
			return Objects.hashCode(scorer);
		}
	}

	public static class ColOtherLearner implements ColumnSelector {
		public final LearningAlgorithms.ScoringToApply scorer;
		public ColOtherLearner(LearningAlgorithms.ScoringToApply s) {
			scorer = s;
		}

		@Override
		public boolean check(MarkovLearningParameters.ColumnParseOutcome column) {
			return column.learner != scorer;
		}

		@Override
		public boolean equals(Object o) {
			if (!(o instanceof ColLearner)) return false;
			ColLearner that = (ColLearner) o;
			return scorer == that.scorer;
		}

		@Override
		public int hashCode() {
			return Objects.hashCode(scorer);
		}
	}

	public class ColLearnerAndPreset implements ColumnSelector {
		public final LearningAlgorithms.ScoringToApply scorer;
		public final int preset;
		public ColLearnerAndPreset(LearningAlgorithms.ScoringToApply s, int p) {
			scorer = s;preset = p;
		}

		@Override
		public boolean check(MarkovLearningParameters.ColumnParseOutcome column) {
			return column.learner.equals(scorer) && column.parameters.preset == preset;
		}

		@Override
		public boolean equals(Object o) {
			if (!(o instanceof ColLearnerAndPreset)) return false;
			ColLearnerAndPreset that = (ColLearnerAndPreset) o;
			return preset == that.preset && scorer == that.scorer;
		}

		@Override
		public int hashCode() {
			return Objects.hash(scorer, preset);
		}
	}

	public static class ColLearnerPresetAvemaxDivisorWlen implements ColumnSelector {
		public final LearningAlgorithms.ScoringToApply scorer;
		public final int preset, divisor, wlen;
		public final boolean averageOrMax;

		public ColLearnerPresetAvemaxDivisorWlen(LearningAlgorithms.ScoringToApply scorer, int preset, boolean averageOrMax, int divisor, int wlen) {
			this.scorer = scorer;
			this.preset = preset;
			this.averageOrMax = averageOrMax;
			this.divisor = divisor;
			this.wlen = wlen;
		}

		@Override
		public boolean check(MarkovLearningParameters.ColumnParseOutcome column) {
			return column.learner.equals(scorer) && column.parameters.preset == preset &&
					column.parameters.useAverageOrMax == averageOrMax &&
					column.parameters.divisorForPathCount == divisor && column.parameters.expectedWLen == wlen;
		}

		@Override
		public boolean equals(Object o) {
			if (!(o instanceof ColLearnerPresetAvemaxDivisorWlen)) return false;
			ColLearnerPresetAvemaxDivisorWlen that = (ColLearnerPresetAvemaxDivisorWlen) o;
			return preset == that.preset && divisor == that.divisor && wlen == that.wlen &&
					averageOrMax == that.averageOrMax && scorer == that.scorer;
		}

		@Override
		public int hashCode() {
			return Objects.hash(scorer, preset, divisor, wlen, averageOrMax);
		}
	}

	public static void getAllValuesFromMapGivenRegexp(Map<String,String> map, ColumnSelector columnSelector, Set<MarkovExperiment.RESULT_VALUES> invalidCellValues, ExperimentBlockForColumn lambda)
	{
		for(Map.Entry<String,String> entry:map.entrySet()) {
			String columnText = entry.getKey();
			MarkovLearningParameters.ColumnParseOutcome column = parseMarkovParametersColumnFromCSV(columnText,invalidCellValues);
			if (columnSelector.check(column))
				lambda.processBlock(column, columnText, entry.getValue());
		}
	}

	public static String obtainStringValueFromCell(String columnValue, RESULT_VALUES cellID, MarkovLearningParameters.ColumnParseOutcome column) {
		int cellNumber = RESULT_VALUES.getOffset(cellID,column);
		String [] elements = columnValue.split(",");
		if (elements.length <= cellNumber || cellNumber < 0)
			throw new IllegalArgumentException("invalid cell number "+cellNumber+", should be 0.."+elements.length);
        if (column.invalidCellValues != null && column.invalidCellValues.contains(cellID))
            throw new IllegalArgumentException("Value in cell "+cellID+" was not computed by this experiment");
		return elements[cellNumber];
	}

	public static double obtainDoubleValueFromCell(String columnValue, RESULT_VALUES cellID, MarkovLearningParameters.ColumnParseOutcome column) {
		return Double.parseDouble(obtainStringValueFromCell(columnValue, cellID, column));
	}

	public static int obtainIntValueFromCell(String columnValue, RESULT_VALUES cellID, MarkovLearningParameters.ColumnParseOutcome column) {
		return Integer.parseInt(obtainStringValueFromCell(columnValue, cellID, column));
	}

	public static long obtainLongValueFromCell(String columnValue, RESULT_VALUES cellID, MarkovLearningParameters.ColumnParseOutcome column) {
		return Long.parseLong(obtainStringValueFromCell(columnValue, cellID, column));
	}

	public static boolean obtainBooleanValueFromCell(String columnValue, RESULT_VALUES cellID, MarkovLearningParameters.ColumnParseOutcome column) {
		return Boolean.parseBoolean(obtainStringValueFromCell(columnValue, cellID, column));
	}
//
//	public static boolean matchRegExpToColumn(String columnText, String regexp) {
//		return columnText.equals(regexp) || columnText.matches(regexp) || columnText.matches(regexp+".*");
//	}

	public static class ColumnAndValue {
		public final String columnText;
		public final MarkovLearningParameters.ColumnParseOutcome column;
		public final String value;

		public ColumnAndValue(String columnText, MarkovLearningParameters.ColumnParseOutcome column, String value) {
			this.columnText = columnText;
			this.column = column;
			this.value = value;
		}
	}

    public static Set<RESULT_VALUES> obtainValidityOfCellValues(DrawGraphs.CSVExperimentResult resultCSV) {
        Set<RESULT_VALUES> invalidCellValues = new TreeSet<>();
        for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
            getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), null, (column, columnText, Y) -> {
                boolean learntOK = obtainStringValueFromCell(Y, E_SUCCESS,column).equals(LEARNING_OK.name);

                if (obtainDoubleValueFromCell(Y, E_RELATIVEINCONSISTENCY_LEARNT,column) < 0.0)
                    invalidCellValues.add(E_RELATIVEINCONSISTENCY_LEARNT);// negative value of relative inconsistency means we chose not to compute it.
                if (learntOK && (obtainDoubleValueFromCell(Y, E_DIFF, column) < 1.0 || obtainDoubleValueFromCell(Y, E_BCR, column) < 1.0)) {
                    // This is the case where we did not learn an exact automaton.
                    // If in this case values of mistakes near or far from root are all zeroes, it means
                    // we chose not to compute those values.
                    if (obtainIntValueFromCell(Y, E_ERR_INVALID_NEARROOT, column) == 0 &&
                            obtainIntValueFromCell(Y, E_ERR_INVALID_FARFROMROOT, column) == 0 &&
                            obtainIntValueFromCell(Y, E_ERR_MISSED_NEARROOT, column) == 0 &&
                            obtainIntValueFromCell(Y, E_ERR_MISSED_FARFROMROOT, column) == 0)
                    {
                        invalidCellValues.add(E_ERR_INVALID_NEARROOT);
                        invalidCellValues.add(E_ERR_INVALID_FARFROMROOT);
                        invalidCellValues.add(E_ERR_MISSED_NEARROOT);
                        invalidCellValues.add(E_ERR_MISSED_FARFROMROOT);
                    }
                }
            });
        }

        return invalidCellValues;
    }

	public static void checkFullTransitionCoverageAttained(DrawGraphs.CSVExperimentResult resultCSV, Set<RESULT_VALUES>  validityOfCells) {
		for (Map.Entry<String, Map<String, String>> rowEntry : resultCSV.rowColumnText.entrySet()) {
			MarkovLearningParameters rowHeader = parseMarkovParametersRowFromCSV(rowEntry.getKey());
				getAllValuesFromMapGivenRegexp(rowEntry.getValue(), new ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV), validityOfCells,
						(column, columnText, Y) -> {
							if (obtainIntValueFromCell(Y, E_TRANSITIONS_SAMPLED,column) != 100)
								throw new IllegalArgumentException("Experiment "+rowEntry.getKey()+" transition coverage is "+obtainIntValueFromCell(Y, E_TRANSITIONS_SAMPLED,column)+", it preferrably should be 100");
						});
		}
	}

	public static ColumnAndValue getValueFromMapGivenSelector(Map<String,String> map, ColumnSelector regexp, Set<RESULT_VALUES> invalidCellValues)
	{
		for(Map.Entry<String,String> entry:map.entrySet()) {
			MarkovLearningParameters.ColumnParseOutcome column = parseMarkovParametersColumnFromCSV(entry.getKey(),invalidCellValues);
			if (regexp.check(column))
				return new ColumnAndValue(entry.getKey(), column, entry.getValue());
		}
		return null;
	}


	public static void spreadsheetAsDouble(DrawGraphs.AggregateValues agg, DrawGraphs.CSVExperimentResult whereFrom,Set<RESULT_VALUES> invalidCellValues,
										   ColumnSelector columnX, RESULT_VALUES cellWithinX,
										   ColumnSelector columnY, RESULT_VALUES cellWithinY)
	{
		for(Map.Entry<String,Map<String,String>> rowEntry:whereFrom.rowColumnText.entrySet())
		{
			ColumnAndValue X = getValueFromMapGivenSelector(rowEntry.getValue(),columnX,invalidCellValues);
			ColumnAndValue Y = getValueFromMapGivenSelector(rowEntry.getValue(),columnY,invalidCellValues);
			if (X != null && Y != null)
				agg.merge(obtainDoubleValueFromCell(X.value,cellWithinX,X.column), obtainDoubleValueFromCell(Y.value,cellWithinY,Y.column));
		}
	}


	public static void spreadsheetAsDouble(DrawGraphs.RStatisticalAnalysis analysis, DrawGraphs.CSVExperimentResult whereFrom,Set<RESULT_VALUES> invalidCellValues,
										   ColumnSelector columnX, RESULT_VALUES cellWithinX,
										   ColumnSelector columnY, RESULT_VALUES cellWithinY)
	{
		for(Map.Entry<String,Map<String,String>> rowEntry:whereFrom.rowColumnText.entrySet())
		{
			ColumnAndValue X = getValueFromMapGivenSelector(rowEntry.getValue(),columnX,invalidCellValues);
			ColumnAndValue Y = getValueFromMapGivenSelector(rowEntry.getValue(),columnY,invalidCellValues);
			if (X != null && Y != null)
				analysis.add(obtainDoubleValueFromCell(X.value,cellWithinX,X.column), obtainDoubleValueFromCell(Y.value,cellWithinY,Y.column));
		}
	}

	public static void spreadsheetAsString(DrawGraphs.AggregateStringValues agg, DrawGraphs.CSVExperimentResult whereFrom,Set<RESULT_VALUES> invalidCellValues,
										   ColumnSelector columnX, RESULT_VALUES cellWithinX,
										   ColumnSelector columnY, RESULT_VALUES cellWithinY)
	{
		for(Map.Entry<String,Map<String,String>> rowEntry:whereFrom.rowColumnText.entrySet())
		{
			ColumnAndValue X = getValueFromMapGivenSelector(rowEntry.getValue(),columnX,invalidCellValues);
			ColumnAndValue Y = getValueFromMapGivenSelector(rowEntry.getValue(),columnY,invalidCellValues);
			agg.merge(X == null?null:obtainStringValueFromCell(X.value,cellWithinX,X.column),Y == null?null:obtainStringValueFromCell(Y.value,cellWithinY,Y.column));
		}
	}


	/** Constructs a graph from a spreadsheet, using the supplied columns as data for the graph.
	 *
	 * @param plot R graph to update
	 * @param whereFrom spreadsheet to get data from
	 * @param columnX column for the horizontal values.
	 * @param columnY column for the vertical values.
	 * @param colour the colour to use. Calling this method multiple times permits construction of coloured graphs.
	 * @param label label to add with each value.
	 */
	public static void spreadsheetToBagPlot(DrawGraphs.RBagPlot plot, DrawGraphs.CSVExperimentResult whereFrom,Set<RESULT_VALUES> invalidCellValues,
											ColumnSelector columnX, RESULT_VALUES cellWithinX,
											ColumnSelector columnY, RESULT_VALUES cellWithinY,
											String colour, String label)
	{
		for(Map.Entry<String,Map<String,String>> rowEntry:whereFrom.rowColumnText.entrySet())
		{
			ColumnAndValue X = getValueFromMapGivenSelector(rowEntry.getValue(),columnX, invalidCellValues);
			ColumnAndValue Y = getValueFromMapGivenSelector(rowEntry.getValue(),columnY, invalidCellValues);
			if (X != null && Y != null)
				plot.add(obtainDoubleValueFromCell(X.value,cellWithinX,X.column), obtainDoubleValueFromCell(Y.value,cellWithinY,Y.column), colour, label);
		}
	}

	/** Constructs a graph from a spreadsheet, using the supplied columns as data for the graph.
	 *
	 * @param plot R graph to update
	 * @param whereFrom spreadsheet to get data from
	 * @param columnX column for the horizontal values.
	 * @param columnY column for the vertical values.
	 * @param colour the colour to use. Calling this method multiple times permits construction of coloured graphs.
	 * @param label label to add with each value.
	 */
	public static void spreadsheetToBagPlotNoZeroYValues(DrawGraphs.RBagPlot plot,
														 DrawGraphs.CSVExperimentResult whereFrom,Set<RESULT_VALUES> invalidCellValues,
														 ColumnSelector columnX, RESULT_VALUES cellWithinX,
														 ColumnSelector columnY, RESULT_VALUES cellWithinY,
														 String colour, String label)
	{
		for(Map.Entry<String,Map<String,String>> rowEntry:whereFrom.rowColumnText.entrySet())
		{
			ColumnAndValue X = getValueFromMapGivenSelector(rowEntry.getValue(),columnX, invalidCellValues);
			ColumnAndValue Y = getValueFromMapGivenSelector(rowEntry.getValue(),columnY, invalidCellValues);
			if (X != null && Y != null) {
				double value = obtainDoubleValueFromCell(Y.value,cellWithinY,Y.column);
				if (value > 0.)
					plot.add(obtainDoubleValueFromCell(X.value,cellWithinX,X.column), value, colour, label);
			}
		}
	}


	public static class DataSelection {
		final DrawGraphs.CSVExperimentResult whereFrom;
		final int states;
		final int perStateSquaredDensity100;
        final Set<RESULT_VALUES> invalidCellValues;

        /** Creates an instance of data source containing a spreadsheet and selection criteria for automata.
		 * @param whereFrom spreadsheet to get data from
		 * @param states automata with that number of states to select
		 * @param perStateSquaredDensity100 density to select
		 */
		public DataSelection(DrawGraphs.CSVExperimentResult whereFrom, int states, int perStateSquaredDensity100, Set<RESULT_VALUES> invalidCellValues) {
			this.whereFrom = whereFrom;
			this.states = states;
			this.perStateSquaredDensity100 = perStateSquaredDensity100;
            if (invalidCellValues == null)
                throw new IllegalArgumentException("invalidCellValues cannot be null, its value needs to be obtained via obtainValidityOfCellValues(resultCSV)");
            this.invalidCellValues = invalidCellValues;
		}

		public interface ProcessExperimentEntry {
			void processPair(MarkovLearningParameters.ColumnParseOutcome columnX, String X, MarkovLearningParameters.ColumnParseOutcome columnY, String Y);
		}

		/** Goes through all pairs matching regexp for columns X and Y.
		 * The idea is that a spreadsheet with results has a lot of rows (for specific experiments). We are provided with selectors of those
		 * experiments as regexp and we go through experiments, matching selectors (such as Markov v.s. VH). For numerous Markov experiments
		 * using different parameter values we are therefore expected to pick a number of values.
		 *
		 * Everything is different where columnX and columnY are the same: it means we contrast values associated with each of the experiments.
		 *
		 * Where columnX and columnY are different but intended to refer to the same experiment is not supported and will produce a cross-
		 * product across all the matching experiments (aka a mess).
		 */
		public void iterateThroughData(ColumnSelector columnXselector,ColumnSelector columnYselector, ProcessExperimentEntry lambda) {
			for (Map.Entry<String, Map<String, String>> rowEntry : whereFrom.rowColumnText.entrySet()) {
				MarkovLearningParameters rowValues = parseMarkovParametersRowFromCSV(rowEntry.getKey());
				if (rowValues.perStateSquaredDensityMultipliedBy100 == perStateSquaredDensity100 && rowValues.states == states) {
					for(Map.Entry<String,String> entryX:rowEntry.getValue().entrySet()) {
						MarkovLearningParameters.ColumnParseOutcome columnX = parseMarkovParametersColumnFromCSV(entryX.getKey(),invalidCellValues);
						if (obtainStringValueFromCell(entryX.getValue(), RESULT_VALUES.E_SUCCESS,columnX).equals(LEARNING_OK.name) && columnXselector.check(columnX)) {
							String X = entryX.getValue();
							if (!columnXselector.equals(columnYselector)) {
								for (Map.Entry<String, String> entryY : rowEntry.getValue().entrySet()) {
									MarkovLearningParameters.ColumnParseOutcome columnY = parseMarkovParametersColumnFromCSV(entryY.getKey(),invalidCellValues);
									if (obtainStringValueFromCell(entryY.getValue(), RESULT_VALUES.E_SUCCESS,columnY).equals(LEARNING_OK.name) && columnYselector.check(columnY)) {
										String Y = entryY.getValue();
										if (X != null && Y != null)
											lambda.processPair(columnX,X,columnY,Y);
									}
								}
							} else
								lambda.processPair(columnX,X, columnX,X);
						}
					}
				}
			}
		}
	}


	/** Constructs a graph from a spreadsheet, using the supplied columns as data for the graph.
	 *
	 * @param plot R graph to update
	 * @param columnXselector column for the horizontal values.
	 * @param columnYselector column for the vertical values.
	 * @param colour the colour to use. Calling this method multiple times permits construction of coloured graphs.
	 * @param label label to add with each value.
	 */
	public static void spreadsheetToBagPlot(DrawGraphs.RBagPlot plot, DataSelection source,
											ColumnSelector columnXselector, RESULT_VALUES cellWithinX,
											ColumnSelector columnYselector, RESULT_VALUES cellWithinY,
											String colour, String label)
	{
		source.iterateThroughData(columnXselector,columnYselector,(columnX, X, columnY, Y) ->
				plot.add(obtainDoubleValueFromCell(X, cellWithinX, columnX), obtainDoubleValueFromCell(Y, cellWithinY, columnY), colour, label)
		);
	}

	/** Constructs a graph from a spreadsheet, using the supplied columns as data for the graph.
	 *
	 * @param plot R graph to update
	 * @param source data to process
	 * @param columnXselector column for the horizontal values.
	 * @param columnYselector column for the vertical values.
	 * @param colour the colour to use. Calling this method multiple times permits construction of coloured graphs.
	 * @param label label to add with each value.
	 */
	public static void spreadsheetToBagPlotNoZeroYValues(DrawGraphs.RBagPlot plot, DataSelection source,
														 ColumnSelector columnXselector, RESULT_VALUES cellWithinX,
														 ColumnSelector columnYselector, RESULT_VALUES cellWithinY,
														 String colour, String label)
	{
		source.iterateThroughData(columnXselector,columnYselector,(columnX, X, columnY, Y) -> {
			double value = obtainDoubleValueFromCell(Y, cellWithinY, columnY);
			if (value > 0.)
				plot.add(obtainDoubleValueFromCell(X, cellWithinX, columnX), value, colour, label);
		});
	}

	public static void spreadsheetAsDouble(DrawGraphs.RStatisticalAnalysis analysis, DataSelection source,
										   ColumnSelector columnXselector, RESULT_VALUES cellWithinX,
										   ColumnSelector columnYselector, RESULT_VALUES cellWithinY)
	{
		source.iterateThroughData(columnXselector,columnYselector,(columnX, X, columnY, Y) ->
				analysis.add(obtainDoubleValueFromCell(X,cellWithinX, columnX), obtainDoubleValueFromCell(Y,cellWithinY, columnY))
		);
	}

	public static void spreadsheetAsString(DrawGraphs.AggregateStringValues agg, DataSelection source,
										   ColumnSelector columnXselector, RESULT_VALUES cellWithinX,
										   ColumnSelector columnYselector, RESULT_VALUES cellWithinY)
	{
		source.iterateThroughData(columnXselector,columnYselector,(columnX, X, columnY, Y) ->
				agg.merge(X == null?null:obtainStringValueFromCell(X,cellWithinX,columnX),Y == null?null:obtainStringValueFromCell(Y,cellWithinY, columnY))
		);
	}


	public static SGE_ExperimentRunner.processSubExperimentResult<MarkovLearningParameters, ExperimentResult<MarkovLearningParameters>>
		constructResultsCollector(DrawGraphs.CSVExperimentResult resultCSV) {
		return new SGE_ExperimentRunner.processSubExperimentResult<MarkovLearningParameters, ExperimentResult<MarkovLearningParameters>>() {

			@Override
			public void processSubResult(ExperimentResult<MarkovLearningParameters> result, SGE_ExperimentRunner.RunSubExperiment<MarkovLearningParameters, ExperimentResult<MarkovLearningParameters>> experimentrunner) throws
					IOException {// in these experiments, samples are singleton sequences because we run each of them in a separate process, in order to increase the efficiency with which all tasks are split between CPUs in an iceberg grid.
				PairQualityLearner.SampleData sm = result.samples.get(0);
				PairQualityLearner.ScoresForGraph data = sm.actualLearner;

				StringBuffer csvLine = new StringBuffer();
				csvLine.append(data.whetherLearningSuccessfulOrAborted);
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceBCR.getValue());// 1
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceStructural.getValue());// 2
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.invalidMergersNearRoot);// 3
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.missedMergersNearRoot); // 4
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.invalidMergersFarFromRoot);// 5
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.missedMergersFarFromRoot); // 6
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.validMergers); // 7
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.nrOfstates.getValue());// 8
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.inconsistencyReference);// 9
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistency);// 10

				if (result.parameters.learnerToUse.isMarkov()) {
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistencyAverage);// 11
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistencySD);// 12
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistencyAlwaysPositive);// 13
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.fractionOfStatesIdentifiedBySingletons);// 14
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovTransitionPrecision);// 15
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovTransitionRecall);// 16
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovHolePrecision);// 17
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovHoleRecall);// 18
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.predictionAccuracyForReferenceGraph);// 19
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.relativeInconsistency);// 20
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.predictionAccuracy);// 21
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.comparisonsPerformed);// 22
				}

				if (result.parameters.markovParameters.useCentreVertex) {
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.centreCorrect);
					DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.centrePathNumber);
				}
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.referenceGraph.pathroutines.computeAlphabet().size());// 23
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append( (double)sm.referenceGraph.pathroutines.countEdges()/(sm.referenceGraph.getStateNumber() * sm.referenceGraph.getStateNumber()));// 24
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append( data.density );// 25
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.transitionsSampled);
				DrawGraphs.CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(data.executionTime / 1000000000.));// execution time is in nanoseconds, we only need seconds.

				experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
			}

			@Override
			public DrawGraphs.SGEExperimentResult[] getGraphs() {

				return new DrawGraphs.SGEExperimentResult[]{resultCSV};
			}

		};
	}

	public static class LearningExperimentGroupParameters {
		DrawGraphs gr = new DrawGraphs();

		final int fsmSamplesPerStateNumber = 40;
		final int trainingSamplesPerFSM = 2;
		final double traceLengthMultiplierMax = 16;

		final int[] statesToUse = new int[]{20,40};

		public static final int datasetSize = 256;

		public static List<Integer> getTraceLenMultValues() {
			List<Integer> traceLenMultValues = new LinkedList<>();
			for (int i = 4; i <= MarkovExperiment.LearningExperimentGroupParameters.datasetSize; i <<= 1)
				traceLenMultValues.add(i);

			return traceLenMultValues;
		}

		public static final int baseNumberOfTracesMult = 8;

		Pair<Integer,Integer> getTracesLengthmultBaseline(int states) {
			return new Pair(baseNumberOfTracesMult * getScalingFactor(states), 2*baseNumberOfTracesMult * getScalingFactor(states));//datasetSize/baseNumberOfTracesMult );
		}
//		Pair<Integer,Integer> getTracesLengthmultSingleTrace(int states) {
//			return new Pair(1, datasetSize * getScalingFactor(states) );
//		}

		/** Scales the number of traces to account for larger automata
		 *
		 * @param states number of states
		 * @return scaling factor to increase the number of traces or length of traces
		 */
		public int getScalingFactor(int states) {
			return  states / 10;
		}

		RunSubExperiment<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>> experimentRunner;

		LearnerEvaluationConfiguration eval;

		String outPathPrefix;

		statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum phase;
	}

    public static int [] densityFromStateNumberPrefixLen(int stateNumber) {
        if (stateNumber < 40)
            return new int[]{0,20, 30};

        return new int[]{0};
    }

	public static int [] densityFromStateNumber(int stateNumber) {
		if (stateNumber < 40)
			return new int[]{0,20};

		return new int[]{0};
	}

	public static void main(String []args)
	{
		String outDir = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_EXPERIMENTRESULTS)+File.separator+directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		UASExperiment.mkDir(outDir);

		LearningExperimentGroupParameters learningGroup = new LearningExperimentGroupParameters();

		learningGroup.outPathPrefix = outDir + File.separator;
		learningGroup.eval = UASExperiment.constructLearnerInitConfiguration();
		learningGroup.eval.config.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);// small automata hence no need for array STATETREE.STATETREE_ARRAY);
		//STATETREE_ARRAY);
		learningGroup.eval.config.setLearnerScoreMode(ScoreMode.GENERAL_NOFULLMERGE);
		learningGroup.eval.config.setTimeOut(3600000L*6L);// timeout for tasks, in milliseconds, equivalent to 6hrs runtime.
		learningGroup.eval.config.setOverride_usePTAMerging(false);

		SGE_ExperimentRunner.configureCPUFreqNormalisation();

		learningGroup.experimentRunner = new RunSubExperiment<>(ExperimentRunner.getCpuNumber(), learningGroup.outPathPrefix + directoryExperimentResult, args);
		learningGroup.phase = learningGroup.experimentRunner.getPhase();

		//final double alphabetMultiplier=2;

		try
		{
			E_MarkovCaseStudies.runExperiment(learningGroup);
//			E_MarkovTempFanMonitor600.runExperiment(learningGroup);
//			E_MarkovBaselineLearn.runExperiment(learningGroup);
//			E_MarkovScoreVsInconsistency.runExperiment(learningGroup);
//			E_MarkovCentre.runExperiment(learningGroup);
//			E_MarkovAlphabet.runExperiment(learningGroup);
//			E_MarkovTraceLenMult.runExperiment(learningGroup);
//			E_MarkovTraceConstSize.runExperiment(learningGroup);
//			E_MarkovPrefixLen.runExperiment(learningGroup);
//			E_MarkovTraceNum.runExperiment(learningGroup);
//			E_MarkovLearnWithCentre.runExperiment(learningGroup);
		}
		catch(Exception ex)
		{
			ex.printStackTrace();
		}
		finally
		{
			learningGroup.experimentRunner.successfulTermination();
			DrawGraphs.end();// this is necessary to ensure termination of the JVM runtime at the end of experiments.
		}
	}
}

