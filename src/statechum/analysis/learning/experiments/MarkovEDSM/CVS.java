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
import java.util.Stack;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import statechum.analysis.learning.linear.GD;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
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
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.mutation.DiffExperiments;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.apps.QSMTool;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.collections.ArrayMapWithSearchPos;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.MarkovPassivePairSelection;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.*;
import statechum.analysis.learning.experiments.PairSelection.MarkovPassivePairSelection.PairScoreWithDistance;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceDiff;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceLanguageBCR;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.SampleData;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResult;

public class CVS extends PairQualityLearner
{
	public static class LearnerRunner implements Callable<ThreadResult>
	{
		protected final Configuration config;
		protected final ConvertALabel converter;
		protected final int states=0,sample=0;
		protected boolean onlyUsePositives;
		protected final int seed;
		protected int chunkLen=3;
		protected int traceQuantity;
		protected String selectionID;
		protected double alphabetMultiplier = 1;
		protected double traceLengthMultiplier = 1;
		LearnerGraph pta = null;
		Collection<List<Label>> testSet=null;
		protected double tracesAlphabetMultiplier = 0;
		
		/** Whether we should try learning with zero inconsistencies, to see how heuristics fare. */
		protected boolean disableInconsistenciesInMergers = false;
		
		public void setDisableInconsistenciesInMergers(boolean v)
		{
			disableInconsistenciesInMergers = v;
		}
		
		public void setpta(LearnerGraph ptagraph)
		{
			pta = ptagraph;
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
		
		public LearnerRunner(int argSeed, Configuration conf, ConvertALabel conv)
		{
			config = conf;seed = argSeed;converter=conv;
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

			ThreadResult outcome = new ThreadResult();

			LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);learnerEval.setLabelConverter(converter);
			
			for(int attempt=0;attempt<1;++attempt)
			{// try learning the same machine a few times
				LearnerGraph referenceGraph = FsmParser.buildLearnerGraph("q0-initialise->q1-connect->q2-login->q3-setfiletype->q4-rename->q6-storefile->q5-setfiletype->q4-storefile->q7-appendfile->q5\nq3-makedir->q8-makedir->q8-logout->q16-disconnect->q17\nq3-changedirectory->q9-listnames->q10-delete->q10-changedirectory->q9\nq10-appendfile->q11-logout->q16\nq3-storefile->q11\nq3-listfiles->q13-retrievefile->q13-logout->q16\nq13-changedirectory->q14-listfiles->q13\nq7-logout->q16\nq6-logout->q16", "specgraph",config,converter);

				final MarkovModel m= new MarkovModel(chunkLen,true,true, disableInconsistenciesInMergers);

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

//				LearnerGraph trimmedReference = MarkovPassivePairSelection.trimUncoveredTransitions(pta,referenceGraph);
				final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();
//				long inconsistencyForTheReferenceGraph = MarkovClassifier.computeInconsistency(trimmedReference, m, checker,false);

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
					LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMergeInitialMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					int scoreInitialMerge = pta.pairscores.computePairCompatibilityScore_general(null, pairsListInitialMerge, verticesToMergeInitialMerge, false);
					assert scoreInitialMerge >= 0;
					ptaToUseForInference = MergeStates.mergeCollectionOfVertices(pta, null, verticesToMergeInitialMerge, false);
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
				actualAutomaton.setName("CVS");
//				Visualiser.updateFrame(actualAutomaton, referenceGraph);	
//				Visualiser.waitForKey();
				GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = 
	                    new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
                final AbstractLearnerGraph graph_Learnt = actualAutomaton;
                final AbstractLearnerGraph graph1=referenceGraph;
                DirectedSparseGraph gr = gd.showGD(graph_Learnt,graph1,ExperimentRunner.getCpuNumber());
                 Visualiser.updateFrame(gr, null);
	                    
				
				if (verticesToMergeBasedOnInitialPTA != null && mergeIdentifiedPathsAfterInference)
				{
					LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					int genScore = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, constructPairsToMergeBasedOnSetsToMerge(actualAutomaton.transitionMatrix.keySet(),verticesToMergeBasedOnInitialPTA), verticesToMerge, false);
					assert genScore >= 0;
					actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge, false);
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
				System.out.println("EDSM-Markov Actual Learnt");

				dataSample.actualLearner = estimateDifference(referenceGraph,actualAutomaton,testSet);
				dataSample.actualLearner.inconsistency = inconsistencyActual;
				dataSample.referenceLearner = zeroScore;
				
				
				// This is to ensure that scoring is computed in the usual way rather than with override.
				ScoreMode scoringModeToUse = ScoreMode.COMPATIBILITY;
				Configuration evaluationConfig = config.copy();evaluationConfig.setLearnerScoreMode(scoringModeToUse);
				
				LearnerGraph outcomeOfReferenceLearner = new LearnerGraph(evaluationConfig);
				{
					try
					{
						LearnerEvaluationConfiguration referenceLearnerEval = new LearnerEvaluationConfiguration(learnerEval.graph, learnerEval.testSet, evaluationConfig, learnerEval.ifthenSequences, learnerEval.labelDetails);
						outcomeOfReferenceLearner = new ReferenceLearnerUsingSiccoScoring(referenceLearnerEval,ptaCopy,false).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
						System.out.println("Sicco's Reference");
						dataSample.referenceLearner = estimateDifference(referenceGraph, outcomeOfReferenceLearner,testSet);
						dataSample.referenceLearner.inconsistency = MarkovClassifier.computeInconsistency(outcomeOfReferenceLearner, m, checker,false);
					}
					catch(LearnerAbortedException ex)
					{// the exception is thrown because the learner failed to learn anything completely. Ignore it because the default score is zero assigned via zeroScore. 
					}
				}
				
				{
					LearnerGraph outcomeOfKTailsLearner = new LearnerGraph(evaluationConfig);
					try
					{
						LearnerEvaluationConfiguration referenceLearnerEval = new LearnerEvaluationConfiguration(learnerEval.graph, learnerEval.testSet, evaluationConfig, learnerEval.ifthenSequences, learnerEval.labelDetails);
						outcomeOfKTailsLearner = new KTailsReferenceLearner(referenceLearnerEval,ptaCopy,true,1).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
						System.out.println("K-tails Reference");

						dataSample.ktailsLearner = estimateDifference(referenceGraph, outcomeOfKTailsLearner,testSet);
						dataSample.ktailsLearner.inconsistency = MarkovClassifier.computeInconsistency(outcomeOfKTailsLearner, m, checker,false);
					}
					catch(LearnerAbortedException ex)
					{// the exception is thrown because the learner failed to learn anything completely. Ignore it because the default score is zero assigned via zeroScore. 
					}
				}
				
				{
					LearnerGraph EDSMReferenceLearnerzero = new LearnerGraph(evaluationConfig);
					try
					{
						LearnerEvaluationConfiguration referenceLearnerEval = new LearnerEvaluationConfiguration(learnerEval.graph, learnerEval.testSet, evaluationConfig, learnerEval.ifthenSequences, learnerEval.labelDetails);
						EDSMReferenceLearnerzero = new EDSMReferenceLearner(referenceLearnerEval,ptaCopy,scoringModeToUse,0).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
						System.out.println("EDSM >= 0 Reference");

						dataSample.EDSMzero = estimateDifference(referenceGraph, EDSMReferenceLearnerzero,testSet);
						dataSample.EDSMzero.inconsistency = MarkovClassifier.computeInconsistency(EDSMReferenceLearnerzero, m, checker,false);
					}
					catch(LearnerAbortedException ex)
					{// the exception is thrown because the learner failed to learn anything completely. Ignore it because the default score is zero assigned via zeroScore. 
					}
				}
				
				
				{
					LearnerGraph EDSMReferenceLearnerone = new LearnerGraph(evaluationConfig);
					try
					{
						LearnerEvaluationConfiguration referenceLearnerEval = new LearnerEvaluationConfiguration(learnerEval.graph, learnerEval.testSet, evaluationConfig, learnerEval.ifthenSequences, learnerEval.labelDetails);
						EDSMReferenceLearnerone = new EDSMReferenceLearner(referenceLearnerEval,ptaCopy,scoringModeToUse,1).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
						System.out.println("EDSM >= 1 Reference");

						dataSample.EDSMone = estimateDifference(referenceGraph, EDSMReferenceLearnerone,testSet);
						dataSample.EDSMone.inconsistency = MarkovClassifier.computeInconsistency(EDSMReferenceLearnerone, m, checker,false);
					}
					catch(LearnerAbortedException ex)
					{// the exception is thrown because the learner failed to learn anything completely. Ignore it because the default score is zero assigned via zeroScore. 
					}
				}
				
				
				{
					LearnerGraph EDSMReferenceLearnertwo = new LearnerGraph(evaluationConfig);
					try
					{
						LearnerEvaluationConfiguration referenceLearnerEval = new LearnerEvaluationConfiguration(learnerEval.graph, learnerEval.testSet, evaluationConfig, learnerEval.ifthenSequences, learnerEval.labelDetails);
						EDSMReferenceLearnertwo = new EDSMReferenceLearner(referenceLearnerEval,ptaCopy,scoringModeToUse,2).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
						System.out.println("EDSM >= 2 Reference");

						dataSample.EDSMtwo = estimateDifference(referenceGraph, EDSMReferenceLearnertwo,testSet);
						dataSample.EDSMtwo.inconsistency = MarkovClassifier.computeInconsistency(EDSMReferenceLearnertwo, m, checker,false);
					}
					catch(LearnerAbortedException ex)
					{// the exception is thrown because the learner failed to learn anything completely. Ignore it because the default score is zero assigned via zeroScore. 
					}
				}
				

				{// For Markov, we do not need to learn anything at all - our Markov matrix contains enough information to classify paths and hence compare it to the reference graph.
					ConfusionMatrix mat = DiffExperiments.classifyAgainstMarkov(testSet, referenceGraph, m);
					dataSample.markovLearner = new ScoresForGraph();			
					System.out.println("Markov");
					dataSample.markovLearner.differenceBCR = new DifferenceToReferenceLanguageBCR(mat);
					System.out.println(dataSample.markovLearner.differenceBCR.getValue());
				}
				
				dataSample.fractionOfStatesIdentifiedBySingletons=Math.round(100*MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(referenceGraph));
				dataSample.stateNumber = referenceGraph.getStateNumber();
//				dataSample.transitionsSampled = Math.round(100*trimmedReference.pathroutines.countEdges()/referenceGraph.pathroutines.countEdges());
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
//						+ " inconsistency learnt "+dataSample.actualLearner.inconsistency+" inconsistency reference: "+inconsistencyForTheReferenceGraph
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
		ScoresForGraph estimateDifference(LearnerGraph reference, LearnerGraph actual,Collection<List<Label>> evaluationTestSet)
		{
			ScoresForGraph outcome = new ScoresForGraph();
			outcome.differenceStructural=DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(reference, actual, config, 1);
			outcome.differenceBCR=DifferenceToReferenceLanguageBCR.estimationOfDifference(reference, actual,evaluationTestSet);
			System.out.println("Structure= "+outcome.differenceStructural.getValue());
			System.out.println("BCR= "+outcome.differenceBCR.getValue());
			System.out.println("---------------------------------------------------");

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
	public static class EDSM_MarkovLearner extends ReferenceLearnerUsingSiccoScoring implements statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure
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
		
		boolean useNewScoreNearRoot = true, useClassifyPairs = true;

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
			List<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();//coregraph.getStateNumber()+1);// to ensure arraylist does not reallocate when we fill in the last element
			int genScore = coregraph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge, false);
			long score= genScore;
			if (genScore >= 0)
			{			
				LearnerGraph merged = MergeStates.mergeCollectionOfVertices(coregraph, null, verticesToMerge,false);
				if (!disableInconsistenciesInMergers)
					currentInconsistency = MarkovClassifier.computeInconsistencyOfAMerger(coregraph, verticesToMerge, inconsistenciesPerVertex, merged, Markov, cl, checker);
				System.out.println("genScore= "+ genScore +" p.getScore= "+p.getScore());
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
			super(constructConfiguration(evalCnf,threshold), argInitialPTA,false);
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
						chosenPair = LearningSupportRoutines.pickPairQSMLike(filter);
					else
						chosenPair = LearningSupportRoutines.pickPairQSMLike(pairsWithScoresComputedUsingGeneralMerger);
				}
				else
					chosenPair = LearningSupportRoutines.pickPairQSMLike(pairsWithScoresComputedUsingGeneralMerger);

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
						int outcome = (int) Math.signum( ((MarkovPassivePairSelection.PairScoreWithDistance)o2).getDistanceScore() - ((MarkovPassivePairSelection.PairScoreWithDistance)o1).getDistanceScore());  
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
	
	public static class TraceLoader extends QSMTool
	{
		public TraceLoader(Configuration c,ConvertALabel converter)
		{
			learnerInitConfiguration.config = c;learnerInitConfiguration.setLabelConverter(converter);
		}
		
		public LearnerGraph getPTA()
		{
			LearnerGraph outcome = new LearnerGraph(learnerInitConfiguration.config);outcome.paths.augmentPTA(sPlus, true, false);outcome.paths.augmentPTA(sMinus, false, false);return outcome;
		}
	}
	
	public static void runExperiment(@SuppressWarnings("unused") String args[]) throws Exception
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setAskQuestions(false);config.setDebugMode(false);config.setGdLowToHighRatio(0.7);config.setRandomPathAttemptFudgeThreshold(1000);
		config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);config.setLearnerScoreMode(ScoreMode.GENERAL);
		ConvertALabel converter = new Transform.InternStringLabel();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int chunkSize = 3;
	
		final int ThreadNumber = ExperimentRunner.getCpuNumber();	
		ExecutorService executorService = Executors.newFixedThreadPool(ThreadNumber);
		CompletionService<ThreadResult> runner = new ExecutorCompletionService<ThreadResult>(executorService);
		// Inference from a few traces
		final boolean onlyPositives=true;

		for(final int preset: new int[]{0})//0,1,2})
		{
				
			TraceLoader tool = new TraceLoader(config,converter);
			tool.loadConfig("resources/CVS.txt");

			LearnerGraph pta = tool.getPTA();

		    System.out.println(pta.learnerCache.getAlphabet());
			int numberOfTasks = 0;
			numberOfTasks++;
		    LearnerRunner learnerRunner = new LearnerRunner(numberOfTasks, config, converter);
		    learnerRunner.setOnlyUsePositives(onlyPositives);
			learnerRunner.setChunkLen(chunkSize);
			learnerRunner.setpta(pta);

			learnerRunner.setPresetLearningParameters(preset);
			learnerRunner.setDisableInconsistenciesInMergers(false);
			runner.submit(learnerRunner);
			
		}
	}
}