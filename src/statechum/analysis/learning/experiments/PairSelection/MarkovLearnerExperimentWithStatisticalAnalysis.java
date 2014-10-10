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

package statechum.analysis.learning.experiments.PairSelection;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Stack;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicLong;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.JUConstants;
import statechum.Label;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.RGraph;
import statechum.analysis.learning.DrawGraphs.Wilcoxon;
import statechum.analysis.learning.DrawGraphs.Mann_Whitney_U_Test;
import statechum.analysis.learning.DrawGraphs.Kruskal_Wallis;
import statechum.analysis.learning.DrawGraphs.RWilcoxon;

import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.PaperUAS;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatCanClassifyPairs;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.collections.ArrayMapWithSearchPos;


public class MarkovLearnerExperimentWithStatisticalAnalysis extends PairQualityLearner
{
	public static class LearnerRunner implements Callable<ThreadResult>
	{
		protected final Configuration config;
		protected final ConvertALabel converter;
		protected final int states,sample;
		protected boolean onlyUsePositives;
		protected final int seed;
		protected int chunkLen=3;
		protected int traceQuantity;
		protected String selectionID;
		protected double alphabetMultiplier = 1;
		protected double traceLengthMultiplier = 1;

		protected double tracesAlphabetMultiplier = 0;
		
		/** Whether we should try learning with zero inconsistencies, to see how heuristics fare. */
		protected boolean disableInconsistenciesInMergers = false;
		
		public void setDisableInconsistenciesInMergers(boolean v)
		{
			disableInconsistenciesInMergers = v;
		}
		
		public void setTracesAlphabetMultiplier(double evalAlphabetMult)
		{
			tracesAlphabetMultiplier = evalAlphabetMult;
		}
		
		public void setSelectionID(String value)
		{
			selectionID = value;
		}
		
		/** Whether to filter the collection of traces such that only positive traces are used. */
		public void setOnlyUsePositives(boolean value)
		{
			onlyUsePositives = value;
		}
		
		public void setAlphabetMultiplier(double mult)
		{
			alphabetMultiplier = mult;
		}
		
		public void setTraceLengthMultiplier(double traceMulti) {
			traceLengthMultiplier=traceMulti;
		}
		
		public void setTraceQuantity(int traceQuantity2) {
			traceQuantity=	traceQuantity2;		
		}
		
		public void setChunkLen(int len)
		{
			chunkLen = len;
		}
		
		public LearnerRunner(int argStates, int argSample, int argSeed, int nrOfTraces, Configuration conf, ConvertALabel conv)
		{
			states = argStates;sample = argSample;config = conf;seed = argSeed;traceQuantity=nrOfTraces;converter=conv;
		}
		
		boolean useCentreVertex = true, useDifferentScoringNearRoot = false, mergeIdentifiedPathsAfterInference = true, useClassifyToOrderPairs = true,useMostConnectedVertexToStartLearning = false;

		public void setlearningParameters(boolean useCentreVertexArg, boolean useDifferentScoringNearRootArg, boolean mergeIdentifiedPathsAfterInferenceArg, boolean useClassifyToOrderPairsArg, boolean useMostConnectedVertexToStartLearningArg)
		{
			useCentreVertex = useCentreVertexArg;useDifferentScoringNearRoot = useDifferentScoringNearRootArg;mergeIdentifiedPathsAfterInference = mergeIdentifiedPathsAfterInferenceArg;useClassifyToOrderPairs = useClassifyToOrderPairsArg;useMostConnectedVertexToStartLearning = useMostConnectedVertexToStartLearningArg; 
		}
		
		public void setPresetLearningParameters(int value)
		{
			switch(value)
			{
			case 0:// learning by not doing pre-merging, starting from root 
				setlearningParameters(false, false, false, false, false);break;
			case 1:// learning by doing pre-merging, starting from most connected vertex. This evaluates numerous pairs and hence is very slow.
				setlearningParameters(true, false, false, true, true);break;
			case 2:// learning by doing pre-merging but starting from root. This seems similar to preset 1 on 20 states.
				setlearningParameters(true, true, false, true, false);break;
			case 3:// learning by not doing pre-merging, starting from root and using a heuristic around root 
				setlearningParameters(false, true, false, true, false);break;
			case 4:// learning by not doing pre-merging, starting from root and not ranking the top IScore candidates with the fanout metric.
				setlearningParameters(false, false, false, false, false);break;
			default:
				throw new IllegalArgumentException("invalid preset number");
			}
		}
		@Override
		public ThreadResult call() throws Exception 
		{
			if (tracesAlphabetMultiplier <= 0)
				tracesAlphabetMultiplier = alphabetMultiplier;
			final int alphabet = (int)(alphabetMultiplier*states);
			final int tracesAlphabet = (int)(tracesAlphabetMultiplier*states);
			
			LearnerGraph referenceGraph = null;
			ThreadResult outcome = new ThreadResult();
			MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
			referenceGraph = mg.nextMachine(alphabet,seed, config, converter).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
			
			LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);learnerEval.setLabelConverter(converter);
			final Collection<List<Label>> testSet = PaperUAS.computeEvaluationSet(referenceGraph,states*3,makeEven(states*tracesAlphabet));
			
			for(int attempt=0;attempt<10;++attempt)
			{// try learning the same machine a few times
				LearnerGraph pta = new LearnerGraph(config);
				RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(attempt),5,null);
				final int tracesToGenerate = makeEven(traceQuantity);
				generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
										
						@Override
						public int getLength() {
							return (int)(traceLengthMultiplier*states*tracesAlphabet);
						}
		
						@Override
						public int getPrefixLength(int len) {
							return len;
						}
					});


				if (onlyUsePositives)
				{
					pta.paths.augmentPTA(generator.getAllSequences(0).filter(new FilterPredicate() {
						@Override
						public boolean shouldBeReturned(Object name) {
							return ((statechum.analysis.learning.rpnicore.RandomPathGenerator.StateName)name).accept;
						}
					}));
				}
				else
					pta.paths.augmentPTA(generator.getAllSequences(0));
		
				final MarkovModel m= new MarkovModel(chunkLen,true,true,false);

				new MarkovClassifier(m, pta).updateMarkov(false);// construct Markov chain if asked for.
				
				pta.clearColours();

				if (!onlyUsePositives)
					assert pta.getStateNumber() > pta.getAcceptStateNumber() : "graph with only accept states but onlyUsePositives is not set";
				else 
					assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
				
				EDSM_MarkovLearner learnerOfPairs = null;
				LearnerGraph actualAutomaton = null;
				
				final Configuration deepCopy = pta.config.copy();deepCopy.setLearnerCloneGraph(true);
				LearnerGraph ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);

				LearnerGraph trimmedReference = MarkovPassivePairSelection.trimUncoveredTransitions(pta,referenceGraph);
				final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();
				long inconsistencyForTheReferenceGraph = MarkovClassifier.computeInconsistency(trimmedReference, m, checker,false);

				LearnerGraph ptaToUseForInference = pta;
				Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA=null;
								
				if (useCentreVertex)
				{
					final MarkovClassifier ptaClassifier = new MarkovClassifier(m,pta);
					final List<List<Label>> pathsToMerge=ptaClassifier.identifyPathsToMerge(checker);
					// These vertices are merged first and then the learning start from the root as normal.
					// The reason to learn from the root is a memory cost. if we learn from the middle, we can get a better results
					verticesToMergeBasedOnInitialPTA=ptaClassifier.buildVerticesToMergeForPaths(pathsToMerge);
					
					List<StatePair> pairsListInitialMerge = ptaClassifier.buildVerticesToMergeForPath(pathsToMerge);
					LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMergeInitialMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					int scoreInitialMerge = pta.pairscores.computePairCompatibilityScore_general(null, pairsListInitialMerge, verticesToMergeInitialMerge);
					assert scoreInitialMerge >= 0;
					ptaToUseForInference = MergeStates.mergeCollectionOfVertices(pta, null, verticesToMergeInitialMerge);
					final CmpVertex vertexWithMostTransitions = MarkovPassivePairSelection.findVertexWithMostTransitions(ptaToUseForInference,MarkovClassifier.computeInverseGraph(pta));
					if (useMostConnectedVertexToStartLearning)
					{
						ptaToUseForInference.clearColours();ptaToUseForInference.getInit().setColour(null);vertexWithMostTransitions.setColour(JUConstants.RED);
					}
					LearnerGraphND inverseOfPtaAfterInitialMerge = MarkovClassifier.computeInverseGraph(ptaToUseForInference);
					System.out.println("Centre vertex: "+vertexWithMostTransitions+" number of transitions: "+MarkovPassivePairSelection.countTransitions(ptaToUseForInference, inverseOfPtaAfterInitialMerge, vertexWithMostTransitions));
				}
				
				learnerOfPairs = new EDSM_MarkovLearner(learnerEval,ptaToUseForInference,0);learnerOfPairs.setMarkov(m);learnerOfPairs.setChecker(checker);
				learnerOfPairs.setUseNewScoreNearRoot(useDifferentScoringNearRoot);learnerOfPairs.setUseClassifyPairs(useClassifyToOrderPairs);
				learnerOfPairs.setDisableInconsistenciesInMergers(disableInconsistenciesInMergers);
				
				actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
				
				if (verticesToMergeBasedOnInitialPTA != null && mergeIdentifiedPathsAfterInference)
				{
					LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					int genScore = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, constructPairsToMergeBasedOnSetsToMerge(actualAutomaton.transitionMatrix.keySet(),verticesToMergeBasedOnInitialPTA), verticesToMerge);
					assert genScore >= 0;
					actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge);
				}			
				
				SampleData dataSample = new SampleData(null,null);
				//dataSample.difference = new DifferenceToReferenceDiff(0, 0);
				//dataSample.differenceForReferenceLearner = new DifferenceToReferenceDiff(0, 0);
				long inconsistencyActual = MarkovClassifier.computeInconsistency(actualAutomaton, m, checker,false);
				
				VertID rejectVertexID = null;
				for(CmpVertex v:actualAutomaton.transitionMatrix.keySet())
					if (!v.isAccept())
					{
						assert rejectVertexID == null : "multiple reject vertices in learnt automaton, such as "+rejectVertexID+" and "+v;
						rejectVertexID = v;break;
					}
				if (rejectVertexID == null)
					rejectVertexID = actualAutomaton.nextID(false);
				actualAutomaton.pathroutines.completeGraphPossiblyUsingExistingVertex(rejectVertexID);// we need to complete the graph, otherwise we are not matching it with the original one that has been completed.
				dataSample.actualLearner = estimateDifference(referenceGraph,actualAutomaton,testSet);
				dataSample.actualLearner.inconsistency = inconsistencyActual;
				dataSample.referenceLearner = zeroScore;
				
				
				// This is to ensure that scoring is computed in the usual way rather than with override.
				Configuration evaluationConfig = config.copy();evaluationConfig.setLearnerScoreMode(ScoreMode.COMPATIBILITY);
				
				LearnerGraph outcomeOfReferenceLearner = new LearnerGraph(evaluationConfig);
				try
				{
					LearnerEvaluationConfiguration referenceLearnerEval = new LearnerEvaluationConfiguration(learnerEval.graph, learnerEval.testSet, evaluationConfig, learnerEval.ifthenSequences, learnerEval.labelDetails);
					//outcomeOfReferenceLearner = new Cav2014.EDSMReferenceLearner(referenceLearnerEval,ptaCopy,2).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
					outcomeOfReferenceLearner = new ReferenceLearner(referenceLearnerEval,referenceGraph,ptaCopy,false).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
					dataSample.referenceLearner = estimateDifference(referenceGraph, outcomeOfReferenceLearner,testSet);
					dataSample.referenceLearner.inconsistency = MarkovClassifier.computeInconsistency(outcomeOfReferenceLearner, m, checker,false);
				}
				catch(Cav2014.LearnerAbortedException ex)
				{// the exception is thrown because the learner failed to learn anything completely. Ignore it because the default score is zero assigned via zeroScore. 
				}				
				dataSample.fractionOfStatesIdentifiedBySingletons=Math.round(100*MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(referenceGraph));
				dataSample.stateNumber = referenceGraph.getStateNumber();
				dataSample.transitionsSampled = Math.round(100*trimmedReference.pathroutines.countEdges()/referenceGraph.pathroutines.countEdges());
				statechum.Pair<Double,Double> correctnessOfMarkov = new MarkovClassifier(m, referenceGraph).evaluateCorrectnessOfMarkov();
				dataSample.markovPrecision = Math.round(100*correctnessOfMarkov.firstElem);dataSample.markovRecall = Math.round(100*correctnessOfMarkov.secondElem);
				dataSample.comparisonsPerformed = learnerOfPairs.comparisonsPerformed;
				Collection<List<Label>> wset=WMethod.computeWSet_reducedw(referenceGraph);
				int wSeqLen=0;
				for(List<Label> seq:wset)
				{
					int len = seq.size();if (len > wSeqLen) wSeqLen=len;
				}
				System.out.println("actual: "+actualAutomaton.getStateNumber()+" from reference learner: "+outcomeOfReferenceLearner.getStateNumber()+ 
						" difference actual is "+dataSample.actualLearner.differenceStructural+ " difference ref is "+dataSample.referenceLearner.differenceStructural
						+ " inconsistency learnt "+dataSample.actualLearner.inconsistency+" inconsistency reference: "+inconsistencyForTheReferenceGraph
						+" transitions per state: "+(double)referenceGraph.pathroutines.countEdges()/referenceGraph.getStateNumber()+
							" W seq max len "+wSeqLen+
							" Uniquely identifiable by W "+Math.round(100*MarkovClassifier.calculateFractionOfIdentifiedStates(referenceGraph, wset))+" %"
						+ " and by singletons "+Math.round(100*MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(referenceGraph))+" %"
						);
				outcome.samples.add(dataSample);
			}
			
			return outcome;
		}

		// Delegates to a specific estimator
		ScoresForGraph estimateDifference(LearnerGraph reference, LearnerGraph actual,Collection<List<Label>> testSet)
		{
			ScoresForGraph outcome = new ScoresForGraph();
			outcome.differenceStructural=DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(reference, actual, config, 1);
			outcome.differenceBCR=DifferenceToReferenceLanguageBCR.estimationOfDifference(reference, actual,testSet);
			return outcome;
		}		
	}
	
		
	public static Collection<StatePair> constructPairsToMergeBasedOnSetsToMerge(Set<CmpVertex> validStates, Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA)
	{
		List<StatePair> pairsList = new LinkedList<StatePair>();
		for(Set<CmpVertex> groupOfStates:verticesToMergeBasedOnInitialPTA)
		{
			Set<CmpVertex> validStatesInGroup = new TreeSet<CmpVertex>();validStatesInGroup.addAll(groupOfStates);validStatesInGroup.retainAll(validStates);
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
	public static class EDSM_MarkovLearner extends LearnerThatCanClassifyPairs implements statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure
	{
		@SuppressWarnings("unused")
		@Override
		public CmpVertex selectRedNode(LearnerGraph gr,Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
		{
			return tentativeRedNodes.iterator().next();
		}
		
		@SuppressWarnings("unused")
		@Override
		public CmpVertex resolvePotentialDeadEnd(LearnerGraph gr, Collection<CmpVertex> reds, List<PairScore> pairs) 
		{
			return null;												
		}
		
		long inconsistencyFromAnEarlierIteration = 0;
		LearnerGraph coregraph = null;
		LearnerGraph extendedGraph = null;
		MarkovClassifier cl=null;
		LearnerGraphND inverseGraph = null;
		long comparisonsPerformed = 0;
		
		boolean useNewScoreNearRoot = false, useClassifyPairs = false;

		public void setUseNewScoreNearRoot(boolean v)
		{
			useNewScoreNearRoot = v;
		}
		
		public void setUseClassifyPairs(boolean v)
		{
			useClassifyPairs = v;
		}
		
		Map<CmpVertex,Long> inconsistenciesPerVertex = null;
		
		/** Whether we should try learning with zero inconsistencies, to see how heuristics fare. */
		protected boolean disableInconsistenciesInMergers = false;
		
		public void setDisableInconsistenciesInMergers(boolean v)
		{
			disableInconsistenciesInMergers = v;
		}

		@Override
		public void initComputation(LearnerGraph graph) 
		{
			coregraph = graph;
					 				
			long value = MarkovClassifier.computeInconsistency(coregraph, Markov, checker,false);
			inconsistencyFromAnEarlierIteration=value;
			cl = new MarkovClassifier(Markov, coregraph);
		    extendedGraph = cl.constructMarkovTentative();
			inverseGraph = (LearnerGraphND)MarkovClassifier.computeInverseGraph(coregraph,true);
			inconsistenciesPerVertex = new ArrayMapWithSearchPos<CmpVertex,Long>(coregraph.getStateNumber());
		}
		
		@Override // we only need this in order to supply a routine to find surrounding transitions and initComputation
		public long overrideScoreComputation(PairScore p) 
		{
			return computeScoreBasedOnInconsistencies(p);
		}		

		public long computeScoreBasedOnInconsistencies(PairScore p) 
		{
			if(p.getQ().isAccept()==false && p.getR().isAccept()==false)
				return 0;
			++comparisonsPerformed;
			long currentInconsistency = 0;
			List<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();//coregraph.getStateNumber()+1);// to ensure arraylist does not reallocate when we fill in the last element
			int genScore = coregraph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge);
			long score= genScore;
			if (genScore >= 0)
			{			
				LearnerGraph merged = MergeStates.mergeCollectionOfVerticesNoUpdateOfAuxiliaryInformation(coregraph, verticesToMerge);
				if (!disableInconsistenciesInMergers)
					currentInconsistency = MarkovClassifier.computeInconsistencyOfAMerger(coregraph, verticesToMerge, inconsistenciesPerVertex, merged, Markov, cl, checker);
				
				score=genScore-currentInconsistency;
				if (useNewScoreNearRoot && genScore <= 1) // could do with 2 but it does not make a difference.
				{
					if (!MarkovClassifier.checkIfThereIsPathOfSpecificLength(inverseGraph,p.getR(),Markov.getPredictionLen()) ||
							!MarkovClassifier.checkIfThereIsPathOfSpecificLength(inverseGraph,p.getQ(),Markov.getPredictionLen()))
						score = //(long)MarkovScoreComputation.computeMMScoreImproved(p,coregraph, extendedGraph);
							MarkovScoreComputation.computenewscore(p, extendedGraph);// use a different score computation in this case
				}
			}
			return score;
		}

		/** This one returns a set of transitions in all directions. */
		@Override
		public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(CmpVertex currentRed) 
		{
			return	MarkovPassivePairSelection.obtainSurroundingTransitions(coregraph,inverseGraph,currentRed);
		}

		protected MarkovModel Markov;
		protected ConsistencyChecker checker;
		
		private static LearnerEvaluationConfiguration constructConfiguration(LearnerEvaluationConfiguration evalCnf, int threshold)
		{
			Configuration config = evalCnf.config.copy();config.setRejectPositivePairsWithScoresLessThan(threshold);
			LearnerEvaluationConfiguration copy = new LearnerEvaluationConfiguration(config);
			copy.graph = evalCnf.graph;copy.testSet = evalCnf.testSet;
			copy.setLabelConverter(evalCnf.getLabelConverter());
			copy.ifthenSequences = evalCnf.ifthenSequences;copy.labelDetails=evalCnf.labelDetails;
			return copy;
		}
		
		public void setMarkov(MarkovModel m) {
			Markov=m;
		}

		public void setChecker(ConsistencyChecker c) {
			checker=c;
		}

		public EDSM_MarkovLearner(LearnerEvaluationConfiguration evalCnf, final LearnerGraph argInitialPTA, int threshold) 
		{
			super(constructConfiguration(evalCnf,threshold),null, argInitialPTA);
		}

		@Override 
		public Stack<PairScore> ChooseStatePairs(LearnerGraph graph)
		{
			Stack<PairScore> outcome = graph.pairscores.chooseStatePairs(this);
			if (!outcome.isEmpty())
			{
				Stack<PairScore> pairsWithScoresComputedUsingGeneralMerger = outcome;
				/*
				new Stack<PairScore>();
				int count=0;
				for(PairScore p:outcome)
				{
					long inconsistencyScore = computeScoreBasedOnInconsistencies(p);
					if (inconsistencyScore >= 0)
					{
						pairsWithScoresComputedUsingGeneralMerger.push(new PairScore(p.getQ(),p.getR(),inconsistencyScore,p.getAnotherScore()));
						if (++count > 10)
							break;
					}
				}

				Collections.sort(pairsWithScoresComputedUsingGeneralMerger);
				*/
				PairScore chosenPair = null;
				if (useClassifyPairs)
				{// This part is to prioritise pairs based on the classify Pairs method.
					Stack<PairScore> NEwresult = MarkovScoreComputation.possibleAtTop(pairsWithScoresComputedUsingGeneralMerger);
					List<PairScore> filter = this.classifyPairs(NEwresult, graph, extendedGraph);

					if(filter.size() >= 1)
						chosenPair = pickPairQSMLike(filter);
					else
						chosenPair = pickPairQSMLike(pairsWithScoresComputedUsingGeneralMerger);
				}
				else
					chosenPair = pickPairQSMLike(pairsWithScoresComputedUsingGeneralMerger);

				outcome.clear();outcome.push(chosenPair);
			}
			
			return outcome;
		}		
		
		/** This method orders the supplied pairs in the order of best to merge to worst to merge. 
		 * We do not simply return the best pair because the next step is to check whether pairs we think are right are classified correctly.
		 * <p/> 
		 * Pairs are supposed to be the ones from {@link LearnerThatCanClassifyPairs#filterPairsBasedOnMandatoryMerge(Stack, LearnerGraph)} where all those not matching mandatory merge conditions are not included.
		 * Inclusion of such pairs will not affect the result but it would be pointless to consider such pairs.
		 * @param extension_graph 
		 * @param learnerGraph 
		 * @param pairs 
		 */
		public List<PairScore> classifyPairs(Collection<PairScore> pairs, LearnerGraph graph, LearnerGraph extension_graph)
		{
			boolean allPairsNegative = true;
			for(PairScore p:pairs)
			{
				assert p.getScore() >= 0;
				
				if (p.getQ().isAccept() || p.getR().isAccept()) // if any are rejects, add with a score of zero, these will always work because accept-reject pairs will not get here and all rejects can be merged.
				{
					allPairsNegative = false;break;
				}
			}
			ArrayList<PairScore> possibleResults = new ArrayList<PairScore>(pairs.size()),nonNegPairs = new ArrayList<PairScore>(pairs.size());
			if (allPairsNegative)
				possibleResults.addAll(pairs);
			else
			{
				for(PairScore p:pairs)
				{
					assert p.getScore() >= 0;
					if (!p.getQ().isAccept() || !p.getR().isAccept()) // if any are rejects, add with a score of zero, these will always work because accept-reject pairs will not get here and all rejects can be merged.
						possibleResults.add(new MarkovPassivePairSelection.PairScoreWithDistance(p,0));
					else
						nonNegPairs.add(p);// meaningful pairs, will check with the classifier
				}
				
				for(PairScore p:nonNegPairs)
				{
					double d = MarkovScoreComputation.computeMMScoreImproved(p,graph, extension_graph);
					if(d >= 0.0)
						possibleResults.add(new MarkovPassivePairSelection.PairScoreWithDistance(p, d));
				}
			
					
				Collections.sort(possibleResults, new Comparator<PairScore>(){
	
					@Override
					public int compare(PairScore o1, PairScore o2) {
						int outcome = sgn( ((MarkovPassivePairSelection.PairScoreWithDistance)o2).getDistanceScore() - ((MarkovPassivePairSelection.PairScoreWithDistance)o1).getDistanceScore());  
						if (outcome != 0)
							return outcome;
						return o2.compareTo(o1);
					}}); 
			}				
			return possibleResults;
		}

		@Override
		public String toString()
		{
			return "EDSM_Markov";
		}
	}

	public static void main(String args[]) throws Exception
	{
		try
		{
			runExperiment(args);
		}
		catch(Exception ex)
		{
			ex.printStackTrace();
		}
		finally
		{
			DrawGraphs.end();
		}
	}
	
	
	public static int runExperiment(String args[]) throws Exception
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setAskQuestions(false);config.setDebugMode(false);config.setGdLowToHighRatio(0.7);config.setRandomPathAttemptFudgeThreshold(1000);
		config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);config.setLearnerScoreMode(ScoreMode.GENERAL);
		ConvertALabel converter = new Transform.InternStringLabel();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int minStateNumber = 10;
		final int samplesPerFSM = 15;
		final int stateNumberIncrement = 5;
		final int rangeOfStateNumbers = 30+stateNumberIncrement;
		final int traceQuantity = 10;
		final double traceLengthMultiplierMax = 1;
		final int chunkSize = 3;
		
		final String branch = "JUNE2014;";
		RunSubExperiment<ThreadResult> experimentRunner = new RunSubExperiment<PairQualityLearner.ThreadResult>(ExperimentRunner.getCpuNumber(),"data",args);
		// Inference from a few traces
		final boolean onlyPositives=true;
		final double alphabetMultiplierMax=2;

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
		
/*
		for(final boolean useCentreVertex:new boolean[]{true,false})
		for(final boolean useDifferentScoringNearRoot:new boolean[]{true,false}) 
		for(final boolean mergeIdentifiedPathsAfterInference:new boolean[]{true,false}) 
		for(final boolean useClassifyToOrderPairs:new boolean[]{true,false})
			
		for(final int traceQuantity:new int[]{10})
		for(final double traceLengthMultiplier:new double[]{1})
			{
				
				final int traceQuantityToUse = traceQuantity;
				
				String selection = "c="+useCentreVertex+
						";r="+useDifferentScoringNearRoot+
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
									LearnerRunner learnerRunner = new LearnerRunner(states,sample,numberOfTasks,traceQuantityToUse, config, converter);
									learnerRunner.setOnlyUsePositives(onlyPositives);
									learnerRunner.setAlphabetMultiplier(alphabetMultiplier);
									learnerRunner.setTraceLengthMultiplier(traceLengthMultiplier);
									learnerRunner.setChunkLen(chunkSize);
									learnerRunner.setSelectionID(selection);
									learnerRunner.setlearningParameters(useCentreVertex, useDifferentScoringNearRoot, mergeIdentifiedPathsAfterInference, useClassifyToOrderPairs);
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
		
		
		for(final int preset: new int[]{0})//0,1,2})
		{
				
			final int traceQuantityToUse = traceQuantity;
			final AtomicLong comparisonsPerformed = new AtomicLong(0);
			String selection = "preset="+preset+";quantity="+traceQuantity+";tracelen="+traceLengthMultiplierMax+";statesMax="+(minStateNumber+rangeOfStateNumbers-stateNumberIncrement)+";alphabetMult="+alphabetMultiplierMax+";";
			final SquareBagPlot gr_StructuralDiff = new SquareBagPlot("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File(branch+"_"+preset+"_"+(minStateNumber+rangeOfStateNumbers-stateNumberIncrement)+"_trace_structuraldiff.pdf"),0,1,true);
			final SquareBagPlot gr_BCR = new SquareBagPlot("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+preset+"_"+(minStateNumber+rangeOfStateNumbers-stateNumberIncrement)+"_trace_bcr.pdf"),0.5,1,true);		
			
			for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
			{
				// in order to compute the statistical test for each group of states, we generate a lot of object to add the mean of BCR and structural difference
				final Wilcoxon <String> Wilcoxon_test_Structural=new Wilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+"_states="+ states +"_Wilcoxon_t_str.csv"));		 
				final Wilcoxon <String> Wilcoxon_Test_BCR=new Wilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Wilcoxon_t_bcr.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_BCR=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Mann_Whitney_U_Test_BCR.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_Structural=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_= "+selection+ "_states="+ states +"_Whitney_U_Test_str.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_BCR=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_BCR.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_Structural=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_str.csv"));		 	 


					for(int sample=0;sample<samplesPerFSM;++sample)
					{
						LearnerRunner learnerRunner = new LearnerRunner(states,sample,experimentRunner.getTaskID(),traceQuantityToUse, config, converter);
						learnerRunner.setOnlyUsePositives(onlyPositives);
						learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
						learnerRunner.setTracesAlphabetMultiplier(alphabetMultiplierMax);
						learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);
						learnerRunner.setChunkLen(chunkSize);
						learnerRunner.setSelectionID(selection);
						learnerRunner.setPresetLearningParameters(preset);
						learnerRunner.setDisableInconsistenciesInMergers(false);
						experimentRunner.submitTask(learnerRunner);
					}
				experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

					@Override
					public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
					{
						for(SampleData sample:result.samples)
							experimentrunner.Record(gr_StructuralDiff,sample.referenceLearner.differenceStructural.getValue(),sample.actualLearner.differenceStructural.getValue(),null,null);
					
						for(SampleData sample:result.samples)
						{
							experimentrunner.Record(gr_BCR,sample.referenceLearner.differenceBCR.getValue(),sample.actualLearner.differenceBCR.getValue(),null,null);
							comparisonsPerformed.addAndGet(sample.comparisonsPerformed);
						}
						AverageValue BCRCollectResult = new AverageValue(0.0,0.0);
						AverageValue StructuralCollectResult = new AverageValue(0.0,0.0);

						for(SampleData sample:result.samples)
						{
							BCRCollectResult.add(sample.actualLearner.differenceBCR.getValue(), sample.referenceLearner.differenceBCR.getValue());
							StructuralCollectResult.add(sample.actualLearner.differenceStructural.getValue(), sample.referenceLearner.differenceStructural.getValue());
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
						return "running tasks for learning whole graphs, preset "+preset;
					}
					
					@SuppressWarnings("rawtypes")
					@Override
					public RGraph[] getGraphs() {
						
						return new RGraph[]{gr_StructuralDiff,gr_BCR,Wilcoxon_Test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,Kruskal_Wallis_Test_Structural,Kruskal_Wallis_Test_BCR};
					}
					
				});
				

				if (experimentRunner.isInteractive())
					System.out.println("\nLOG of comparisons performed: "+Math.log10(comparisonsPerformed.doubleValue())+"\n");
			}
		}

		final int presetForBestResults = 0;
		
/*		final int traceQuantityToUse = traceQuantity;
		{
			final SquareBagPlot gr_StructuralDiffWithoutInconsistencies = new SquareBagPlot("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File(branch+"_noinconsistencies_trace_structuraldiff.pdf"),0,1,true);
			final SquareBagPlot gr_BCRWithoutInconsistencies = new SquareBagPlot("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_noinconsistencies_trace_bcr.pdf"),0.5,1,true);		
			String selection = "noinconsistencies;quantity="+traceQuantity+";tracelen="+traceLengthMultiplierMax+";alphabetMult="+alphabetMultiplierMax+";";
			final AtomicLong comparisonsPerformed = new AtomicLong(0);

			for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
				for(int sample=0;sample<samplesPerFSM;++sample)
				{
					LearnerRunner learnerRunner = new LearnerRunner(states,sample,experimentRunner.getTaskID(),traceQuantityToUse, config, converter);
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
					LearnerRunner learnerRunner = new LearnerRunner(states,sample,experimentRunner.getTaskID(),traceNum, config, converter);
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
					LearnerRunner learnerRunner = new LearnerRunner(states,sample,experimentRunner.getTaskID(),traceNum, config, converter);
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
						LearnerRunner learnerRunner = new LearnerRunner(states,sample,experimentRunner.getTaskID(),traceNum, config, converter);
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
					LearnerRunner learnerRunner = new LearnerRunner(states,sample,experimentRunner.getTaskID(),traceQuantity, config, converter);
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
					LearnerRunner learnerRunner = new LearnerRunner(states,sample,experimentRunner.getTaskID(),traceQuantity, config, converter);
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
		
		return experimentRunner.successfulTermination();
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

