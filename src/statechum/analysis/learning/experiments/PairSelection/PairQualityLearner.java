/* Copyright (c) 2013 The University of Sheffield.
 * 
 * This file is part of StateChum
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
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.Map.Entry;


import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Label;
import statechum.StatechumXML;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.EvaluationOfLearners.ConstructRandomFSM;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovHelper;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovParameters;
import statechum.analysis.learning.experiments.MarkovEDSM.PerformFirstMerge;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.LearnerThatCanClassifyPairs;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.LearnerWithMandatoryMergeConstraints;
import statechum.analysis.learning.experiments.PairSelection.WekaDataCollector.PairRank;
import statechum.analysis.learning.experiments.mutation.DiffExperiments;
import statechum.analysis.learning.observers.LearnerSimulator;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;
import weka.classifiers.Classifier;
import weka.core.Instance;

/** This one aims to learn how to choose pairs and red states in the way that leads to most accurate learning
 * outcomes.
 * 
 * @author kirill
 */
public class PairQualityLearner 
{
   	public static final String largePTALogsDir = "resources"+File.separator+"largePTA"+File.separator;
  	public static final String largePTAFileName = largePTALogsDir+"largePTA.zip";
  	public static final String veryLargePTAFileName = largePTALogsDir+"VeryLargePTA.zip";
	
	public static String directoryNamePrefix= "LearningWithClassifiers";
	public static final String directoryExperimentResult = "experimentresult"+File.separator;
	
	public static MarkovParameters defaultMarkovParameters()
	{
		return new MarkovParameters();// or new MarkovParameters(0, 3,1, true,1,0,1);
	}

	
  	/** Given a graph and a vertex, this method computes the number of states in the tree rooted at the supplied state.
	 * 
	 * @param graph graph to go through
	 * @param stateToStartFrom the state to start exploration from 
	 * @return the number of vertices in the tree, {@link JUConstants#intUNKNOWN} if the tree contains loops. 
	 */
	public static int computeTreeSize(LearnerGraph graph,CmpVertex stateToStartFrom)
	{
		int counter = 0;
		Queue<CmpVertex> fringe = new LinkedList<CmpVertex>();
		Set<CmpVertex> statesInFringe = new HashSet<CmpVertex>();
		fringe.add(stateToStartFrom);statesInFringe.add(stateToStartFrom);
		while(!fringe.isEmpty())
		{
			CmpVertex currentState = fringe.remove();
			Map<Label,CmpVertex> targets = graph.transitionMatrix.get(currentState);
			if(targets != null && !targets.isEmpty())
				for(Entry<Label,CmpVertex> labelstate:targets.entrySet())
					for(CmpVertex target:graph.getTargets(labelstate.getValue()))
					{
						if (!statesInFringe.contains(target))
						{
							fringe.offer(target);
							statesInFringe.add(target);
							++counter;
						}
						else
							return JUConstants.intUNKNOWN;// encountered a loop
					}
		}
		return counter;
	}
	
	/** Describes the data that is gathered for every filtered pair in order to make meaningful comparisons between this pair and others.
	 * Filtered implies that results depend on comparison between this pair and other pairs in a set. 
	 */   
	public static class FilteredPairMeasurements
	{
		public int nrOfAlternatives;
		public boolean adjacent;
	}
	
	/** Describes the data that is gathered for every pair in order to make meaningful comparisons between this pair and others. */   
	public static class PairMeasurements
	{
 		public long compatibilityScore, inconsistencyScore;
	}
	
	/** Configures the attributes used in the data collector. */
	public static class DataCollectorParameters
	{
		public final int ifDepth;
		public final MarkovParameters markovParameters;
		public final boolean graphIsPTA;
		public final boolean comparePairOnlyWithBadPairs;
		
		public DataCollectorParameters(final int depth, MarkovParameters markov, boolean pta, boolean compareOnlyWithBadPairs)
		{
			ifDepth = depth;markovParameters = markov;graphIsPTA = pta;comparePairOnlyWithBadPairs = compareOnlyWithBadPairs;
		}
		
		public List<String> getColumnList()
		{
			List<String> result = new ArrayList<String>();result.addAll(Arrays.asList(new String[]{Integer.toString(ifDepth),graphIsPTA?"PTA":"nonPTA"}));result.addAll(markovParameters.getColumnListForMarkovLearner());
			return result;
		}
		
		@Override
		public String toString()
		{
			return "["+ifDepth+(graphIsPTA?"_PTA":"")+(comparePairOnlyWithBadPairs?"_CB":"")+"_"+markovParameters.getColumnID(true)+"]";
		}
		
		public DataCollectorParameters()
		{
			ifDepth = 1;markovParameters = null;graphIsPTA = false;comparePairOnlyWithBadPairs = false;
		}
		
		public DataCollectorParameters(DataCollectorParameters from)
		{
			ifDepth = from.ifDepth;markovParameters = new MarkovParameters(from.markovParameters);graphIsPTA = from.graphIsPTA;comparePairOnlyWithBadPairs = from.comparePairOnlyWithBadPairs; 
		}
	}
	
	/** Constructs instance generator by making it possible to collect results of measurements. 
	 * 
	 * @param ifDepth how deep max-then-... nesting is to be considered. 
	 * @param markovHelper markov helper to use for computation of scores. Needed to create the data collector which uses scores as part of data stored in instances.
	 * @param graphIsPTA whether to utilise measures relying on a graph rooted at the blue state being a PTA as opposed a directed graph with loop (as is the case with premerge). Needed to create the data collector which uses scores as part of data stored in instances.
	 * @return instance-constructing instance.
	 */
	public static WekaDataCollector createDataCollector(DataCollectorParameters parameters, MarkovHelper m)
	{
		final WekaDataCollector classifier = new WekaDataCollector(m,parameters.graphIsPTA);
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		
		if (parameters.graphIsPTA)
			assessors.add(classifier.new PairRank("conventional score")
			{// 1
				@Override
				public long getValue(PairScore p) {
					return classifier.measurementsObtainedFromPairs.get(p).compatibilityScore;
				}
	
				@Override
				public boolean isAbsolute() {
					return false;
				}
			});

		assessors.add(classifier.new PairRank("score (which could be Statechum or any other)")
		{// 2
			@Override
			public long getValue(PairScore p) {
				return p.getScore();
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("size of tree rooted at Blue")
		{// 3
			@Override
			public long getValue(PairScore p) {
				return treeRootedAt(p.getQ());
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("Number of alternatives with same red")
		{// 4
			@Override
			public long getValue(PairScore p) {
				return  classifier.measurementsForFilteredCollectionOfPairs.measurementsForComparators.get(p).nrOfAlternatives;
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("Depth of Blue")
		{// 5
			@Override
			public long getValue(PairScore p) {
				return  p.getQ().getDepth();
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("Depth of Red")
		{// 6
			@Override
			public long getValue(PairScore p) {
				return  p.getR().getDepth();
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("Statechum score is above zero")
		{// 7
			@Override
			public long getValue(PairScore p) {
				return  p.getScore() > 0?1:0;
			}

			@Override
			public boolean isAbsolute() {
				return true;
			}
		});
		
		assessors.add(classifier.new PairRank("state identifiers Red")
		{// 8
			@Override
			public long getValue(PairScore p) {
				return p.getR().getIntegerID();
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("state identifiers Blue")
		{// 9
			@Override
			public long getValue(PairScore p) {
				return p.getQ().getIntegerID();
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("proximity of the red and blue by depth")
		{// 10
			@Override
			public long getValue(PairScore p) {
				return p.getQ().getDepth()-p.getR().getDepth();
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("whether red and blue are adjacent")
		{// 11
			@Override
			public long getValue(PairScore p) {
				return classifier.measurementsForFilteredCollectionOfPairs.measurementsForComparators.get(p).adjacent? 1:0;
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("new outgoing transitions from blue")
		{// 12
			@Override
			public long getValue(PairScore p) {
				return newTransitionsFromStateB(tentativeGraph(), p.getR(), p.getQ());
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("new outgoing transitions from red")
		{// 13
			@Override
			public long getValue(PairScore p) {
				return newTransitionsFromStateB(tentativeGraph(), p.getQ(), p.getR());
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		
		assessors.add(classifier.new PairRank("new outgoing transitions from blue v.s. all outgoing from red")
		{// 14
			@Override
			public long getValue(PairScore p) {
				long newFromBlue = newTransitionsFromStateB(tentativeGraph(), p.getR(), p.getQ());
				return kFrom_ab(tentativeGraph().transitionMatrix.get(p.getR()).size(), newFromBlue);						
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
	
		if (parameters.markovParameters != null)
		{
			assessors.add(classifier.new PairRank("score v.s. inconsistency")
			{// 15
				@Override
				public long getValue(PairScore p) {
					return kFrom_ab(p.getScore(),classifier.measurementsObtainedFromPairs.get(p).inconsistencyScore);
				}
	
				@Override
				public boolean isAbsolute() {
					return false;
				}
			});
		}

		classifier.initialise("HindsightExperiment",400000,assessors,parameters.ifDepth, parameters.comparePairOnlyWithBadPairs);// capacity is an estimate, the actual capacity depends on the size of the experiment, more than 0.25mil take a while to build a classifier for.
		return classifier;
	}
	
	/** Computes the  balance of two scores, the former indicating that a pair should be merged and another one that suggests it should not be. */
	public static long kFrom_ab(long score, long b)
	{
		assert b >= 0;
		if (score < 0)
			return 0;
		if (b == 0)
			return score*2;// in score/b computation where b is an integer, we inflate score to where b reflects perfection (== 0)
		return score/b;
	}
	
	/** Computes the number of transitions that are new to state b compared to state a. Assumes states a and b can be merged. */
	public static long newTransitionsFromStateB(LearnerGraph graph, CmpVertex a, CmpVertex b)
	{
		Map<Label,CmpVertex> redTransitions = graph.transitionMatrix.get(a);
		long counter = 0;
		for(Label lbl:graph.transitionMatrix.get(b).keySet())
			if (!redTransitions.containsKey(lbl))
				counter++;// this is the same as computing an intersection of the two sets - default Java implementation iterates through the vertices. 
		
		return counter;
	}
	
	public static class InitialConfigurationAndData
	{
		public ProgressDecorator.InitialData initial;
		public LearnerEvaluationConfiguration learnerInitConfiguration;
	}
	
	/** Loads a (potentially large) graph and a small version of it from a supplied file. Much faster than constructing it out of traces. 
	 * Loaded graphs will be using the configuration loaded from a file rather the one that was passed in, hence
	 * they need to be copied if you need them with a different type of transition matrix. 
	 * <br>
	 * Another reason this is placed in a separate method in order to ensure that an instance of LearnerSimulator
	 * goes out of scope and is garbage collected as soon as possible. It holds a great deal
	 * of Xerces objects used for recording execution traces that is not used in this test but takes
	 * a lot of memory.	
	 *  
	 * @param argPTAFileName where to load from.
	 * @param configToUse configuration that will be used for loading
	 * @param converter label interning converter, cannot be null but can be thrown away after loading.
	 * @return Initial configuration and evaluation configuration.
	 * @throws IOException
	 */
	public static InitialConfigurationAndData loadInitialAndPopulateInitialConfiguration(String argPTAFileName, Configuration configToUse, ConvertALabel converter) throws IOException
	{
		InitialConfigurationAndData outcome = new InitialConfigurationAndData();
		
		final java.io.FileInputStream inputStream = new java.io.FileInputStream(argPTAFileName);
		final LearnerSimulator simulator = new LearnerSimulator(inputStream,true,converter);
		Configuration defaultConfig = configToUse.copy();
		//defaultConfig.setRejectPositivePairsWithScoresLessThan(1);
		assert converter != null : "we expect this method to be used with large graphs and Array matrix types hence converter must be set";
		outcome.learnerInitConfiguration = simulator.readLearnerConstructionData(defaultConfig);
		outcome.learnerInitConfiguration.setLabelConverter(converter);
		
		final org.w3c.dom.Element nextElement = simulator.expectNextElement(StatechumXML.ELEM_INIT.name());
		outcome.initial = simulator.readInitialData(nextElement);
		inputStream.close();
		return outcome;
	}		
	
	public static class LearnerThatUsesClassifiers extends LearningAlgorithms.LearnerThatCanClassifyPairs
	{
		public LearnerThatUsesClassifiers(LearnerEvaluationConfiguration evalCnf, LearnerGraph reference, LearnerGraph argInitialPTA, OverrideScoringToApply scoring,MarkovHelper helper, boolean noLimitOnStateNumber) 
		{
			super(evalCnf, reference, argInitialPTA, scoring, noLimitOnStateNumber);markovHelper = helper;
		}

		/** Permits us to compute Markov scores. */
		final MarkovHelper markovHelper;
		protected MarkovModel Markov;
		protected ConsistencyChecker checker;
		
		public void setMarkov(MarkovModel m) {
			Markov=m;
		}

		public void setChecker(ConsistencyChecker c) {
			checker=c;
		}

		public MarkovHelper getHelper()
		{
			return markovHelper;
		}
	}

	/** This one uses Weka to learn how to make correct classification of pairs into right and wrong. As such, it is an inherently cheating learner: 
	 * resolvePotentialDeadEnd uses a reference graph to make sure that were no valid mergers have been selected as a set of pairs, report one of them to be marked as red. 
	 */
	public static class LearnerThatUpdatesWekaResults extends LearnerThatUsesClassifiers
	{
		final WekaDataCollector dataCollector;
		final boolean scoringUsesInconsistency;
		
		public LearnerThatUpdatesWekaResults(LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, WekaDataCollector argDataCollector, final LearnerGraph argInitialPTA,MarkovHelper helper, boolean useInconsistencyScoring) 
		{
			super(evalCnf,argReferenceGraph, argInitialPTA,null,helper, false);// the scoring argument is not set for the parent learner since the part that makes use of it is completely overridden below. 
			dataCollector = argDataCollector;scoringUsesInconsistency = useInconsistencyScoring;
		}
		
		@Override 
		public Stack<PairScore> ChooseStatePairs(LearnerGraph graph)
		{
			Stack<PairScore> outcome = graph.pairscores.chooseStatePairs(new PairScoreComputation.RedNodeSelectionProcedure(){

				// Here I could use a learner based on metrics of both tentative reds and the perceived quality of the red-blue pairs obtained if I choose any given value.
				// This can be accomplished by doing a clone of the graph and running chooseStatePairs on it with decision procedure that 
				// (a) applies the same rule (of so many) to choose pairs and
				// (b) checks that deadends are flagged. I could iterate this process for a number of decision rules, looking locally for the one that gives best quality of pairs
				// for a particular pairscore decision procedure.
				@Override
				public CmpVertex selectRedNode(@SuppressWarnings("unused") LearnerGraph coregraph, @SuppressWarnings("unused") Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
				{
					CmpVertex redVertex = tentativeRedNodes.iterator().next();
					return redVertex;
				}

				@Override
				public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, Collection<CmpVertex> reds, List<PairScore> pairs) 
				{
					dataCollector.updateDatasetWithPairs(pairs, coregraph, referenceGraph);
					CmpVertex red = LearnerThatUpdatesWekaResults.this.resolvePotentialDeadEnd(coregraph, reds, pairs);
					return red;
				}
				
				LearnerGraph g=null;
				
				@Override
				public void initComputation(LearnerGraph gr) 
				{
					g=gr;
					markovHelper.initComputation(gr);
				}

				@Override
				public long overrideScoreComputation(PairScore p) 
				{
					if (!labelsLeadingToStatesToBeMerged.isEmpty() || !labelsLeadingFromStatesToBeMerged.isEmpty())
						if (LearningSupportRoutines.computeScoreBasedOnMandatoryMerge(p, g, labelsLeadingToStatesToBeMerged, labelsLeadingFromStatesToBeMerged) < 0)
							return -1;
					return p.getScore();// here we always have to return a conventional score because if any other one is used, a negative will mean that a pair will not even be considered for mergers and passed to the outcome.
				}

				@Override
				public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(CmpVertex currentRed) 
				{
					return markovHelper.getSurroundingTransitions(currentRed);
				}
			});
			if (!outcome.isEmpty())
			{/*
				if (scoringUsesInconsistency)
				{
					ArrayList<PairScore> pairsWithScoresUsingInconsistency = new ArrayList<PairScore>(outcome.size());
					for(PairScore p:outcome)
						pairsWithScoresUsingInconsistency.add(new PairScore(p.getQ(),p.getR(),markovHelper.computeScoreBasedOnInconsistencies(p),0));
					dataCollector.updateDatasetWithPairs(pairsWithScoresUsingInconsistency, graph, referenceGraph);
				}
				else*/
					dataCollector.updateDatasetWithPairs(outcome, graph, referenceGraph);// we learn from the whole range of pairs, not just the filtered ones
				PairScore chosenPair = pickCorrectPair(outcome, graph);// selects any of the correct pairs, using a reference graph. This is why this learner is called 
				outcome.clear();outcome.push(chosenPair);
			}
			if (!checkAllMergersCorrect())
				throw new IllegalArgumentException("learner could not make the right choice even where correct graph was known in advance");
			return outcome;
		}		
	} // class that builds a classifier tree.

	/** Uses the supplied classifier to rank pairs. */
	public static class LearnerThatUsesWekaResults extends LearnerThatUsesClassifiers
	{
		final Classifier classifier;
		
		/** The data collector is only used in order to evaluate pairs, no data is actually added to it hence no need to use the same instance across many machines. */
		final WekaDataCollector dataCollector;
		final int classTrue,classFalse;
		final UseWekaResultsParameters par;
	
		public void setPairQualityCounter(Map<Long,TrueFalseCounter> argCounter)
		{
			pairQuality = argCounter;
		}
		
		public void setPairPredictionCounter(PredictionEvaluation qualityCounter)
		{
			predictionQuality = qualityCounter;
		}

		public LearnerThatUsesWekaResults(UseWekaResultsParameters parameters,LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, Classifier wekaClassifier, final LearnerGraph argInitialPTA,MarkovHelper helper, boolean noLimitOnStateNumber) 
		{
			super(evalCnf,argReferenceGraph,argInitialPTA,null,helper, noLimitOnStateNumber);// the scoring argument is not set for the parent learner since the part that makes use of it is completely overridden below.
			par=parameters;
			dataCollector = createDataCollector(parameters.dataCollectorParameters, helper);//if we do a second pass, the graph will not be a PTA. !parameters.markovParameters.useCentreVertex);
			classifier=wekaClassifier;
			classTrue=dataCollector.classAttribute.indexOfValue(Boolean.TRUE.toString());classFalse=dataCollector.classAttribute.indexOfValue(Boolean.FALSE.toString());
		}
		
		protected double obtainEstimateOfTheQualityOfTheCollectionOfPairs(Collection<PairScore> pairs)
		{
			double sum = 0;
			for(PairScore p:pairs)
			{
				assert p.getScore() >= 0;
				
				if (!p.getQ().isAccept() || !p.getR().isAccept()) // if any are reject-states, we can always merge them
					sum+=1;
				else
				{// meaningful pairs, check with the classifier
					try
					{
						int []comparisonResults = new int[dataCollector.getInstanceLength()];
						dataCollector.fillInPairDetails(comparisonResults,p, pairs);
						Instance instance = dataCollector.constructInstance(comparisonResults, true);
						double distribution[]=classifier.distributionForInstance(instance);
						long quality = obtainMeasureOfQualityFromDistribution(distribution,classTrue);
						if ( quality >= 0 )
						{
							sum+=quality;
						}
					}
					catch(Exception ex)
					{
						ex.printStackTrace();
						throw new IllegalArgumentException("failed to classify pair "+p, ex);
					}
				}
			}

			return sum/pairs.size();
		}
		
		static final long maxQuality = 100, unknown=-1000; 
		long obtainMeasureOfQualityFromDistribution(double distribution[], int classOfInterest)
		{
			assert distribution.length == 2;
			double goodClass = distribution[classOfInterest], badClass = distribution[1-classOfInterest];
			if (Math.abs(goodClass - badClass) < Configuration.fpAccuracy)
				return unknown;
			if (badClass < Configuration.fpAccuracy)
				return maxQuality;
			double ratio = goodClass/badClass;
			if (ratio < par.threshold)
				return -1;
			return (long)(maxQuality*goodClass);
			//return Math.min(maxQuality, (long)(maxQuality*ratio/2));
		}
		
		
		// values for pairs cannot be cached because they depend on other pairs in consideration. Hence the same pair as part of a larger collection of pairs may be deemed either good or not so good, it all depends on other pairs.
		
		protected long classifyPair(PairScore p,Collection<PairScore> allPairs, int classToUse)
		{
			int []comparisonResults = new int[dataCollector.getInstanceLength()];
			dataCollector.fillInPairDetails(comparisonResults,p, allPairs);
			Instance instance = dataCollector.constructInstance(comparisonResults, true);
			double distribution[]=null;
			try
			{
				distribution=classifier.distributionForInstance(instance);
			}
			catch(Exception ex)
			{
				ex.printStackTrace();
				throw new IllegalArgumentException("failed to classify pair "+p, ex);
			}
			return obtainMeasureOfQualityFromDistribution(distribution,classToUse);
		}
		
		/** This method orders the supplied pairs in the order of best to merge to worst to merge. 
		 * We do not simply return the best pair because the next step is to check whether pairs we think are right are classified correctly.
		 * <p/> 
		 * Pairs are supposed to be the ones from {@link LearnerThatCanClassifyPairs#filterPairsBasedOnMandatoryMerge(Stack, LearnerGraph)} where all those contradicting mandatory merge conditions are not included.
		 * Inclusion of such pairs will not affect the result but it would be pointless to consider such pairs.
		 */
		protected Stack<PairScore> classifyPairs(Collection<PairScore> pairs, LearnerGraph tentativeGraph, PairScore pairToDebug)
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
			Stack<PairScore> possibleResults = new Stack<PairScore>();
			ArrayList<PairScore> nonNegPairs = new ArrayList<PairScore>(pairs.size());
			if (allPairsNegative)
				possibleResults.addAll(pairs);
			else
			{// not all pairs contain negative nodes, hence we have to build the sets necessary to evaluate those pairs. Strictly speaking, only those that are used in a tree need to be computed but this 
			 // would make the whole process significantly more complex and hence not done.

				// By the time we get here, it is assumed that all mandatory merge conditions are satisfied and hence every pair can be merged.
				dataCollector.buildSetsForComparators(pairs,tentativeGraph);
				for(PairScore p:pairs)
				{
					assert p.getScore() >= 0;
					if (!p.getQ().isAccept() || !p.getR().isAccept()) // if any are rejects, add with a score of zero, these will always work because accept-reject pairs will not get here and all rejects can be merged.
						possibleResults.add(new PairScore(p.getQ(), p.getR(), p.getScore(), 0));
					else
						nonNegPairs.add(p);// meaningful pairs, will check with the classifier
				}
				
				for(PairScore p:nonNegPairs)
				{
					if (pairToDebug != null && (p.getQ() == pairToDebug.getQ() && p.getR() == pairToDebug.getR()))
						debugBreak();
					long inconsistencyScore = p.getScore();
					/*
					if (par.scoresIncludeInconsistencies)
						inconsistencyScore = markovHelper.computeScoreBasedOnInconsistencies(p);
					*/
					long quality = 0;
					
					if (inconsistencyScore >= 0)
					{
						quality = classifyPair(p,nonNegPairs,classTrue);
						if (quality == unknown)
							quality = inconsistencyScore;// if the classifier does not know, use the inconsistency score.
						
						// if we do not use classifyPair above, we are trying to avoid ruling out plausible mergers here since classifyPair is much better at telling us what is wrong than confirming what is right (it sometimes predicts them as negatives).
					}
					else
						quality = -1;// no merge if inconsistency prevents us from merging.
					
					if ( quality >= 0 )// && p.getScore() > 0)
					{
						possibleResults.add(new PairScore(p.getQ(), p.getR(), inconsistencyScore, p.getScore() )); // we need to re-create pair if sorting uses anything other than the original score
					}
				}
				Collections.sort(possibleResults);
				/*
				Collections.sort(possibleResults, new Comparator<PairScore>(){
	
					@Override
					public int compare(PairScore o1, PairScore o2) {
						int outcome = LearningSupportRoutines.signum( o1.getScore() - o2.getScore() );// scores are between 100 and 0, hence it is appropriate to cast to int without a risk of overflow.
						if (outcome != 0)
							return outcome;
						return o1.compareTo(o2);
					}}); 
				*/
			}
			return possibleResults;
		}
		
		protected static class ValueAndCount
		{
			public int value=0,count=0;
		}
		
		public static final void debugBreak()
		{
			System.out.println();
		}
		
		/** If there is a state to be labelled red, returns it. This permits one to enlarge a set of possible pairs, merging only best ones compared to choosing from a list of mediocre pairs. */
		protected CmpVertex selectRedStateIfAnySeemsRedEnough(Collection<PairScore> pairs, LearnerGraph tentativeGraph,CmpVertex stateToDebug)
		{
			Map<CmpVertex,ValueAndCount> possiblyRedVerticesToTheirQuality = new TreeMap<CmpVertex,ValueAndCount>();
			for(PairScore p:pairs)
				possiblyRedVerticesToTheirQuality.put(p.getQ(),new ValueAndCount());

			dataCollector.buildSetsForComparators(pairs,tentativeGraph);
			LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMerge(tentativeGraph,labelsLeadingToStatesToBeMerged,labelsLeadingFromStatesToBeMerged);

			for(PairScore p:pairs)
			{
				if (p.getQ() == stateToDebug)	
					debugBreak();
				
				if (possiblyRedVerticesToTheirQuality.containsKey(p.getQ())) // once a vertex is no longer a plausible red, no point considering pairs it is part of.
				{
					if (!p.getQ().isAccept() || !p.getR().isAccept())
						possiblyRedVerticesToTheirQuality.remove(p.getQ());// if both are rejects, a merge is always possible, hence disqualify the vertex from being possibly red.
					else
						if (!pairsList.isEmpty() && tentativeGraph.pairscores.computePairCompatibilityScore_general(p, pairsList, verticesToMerge, false) < 0)
						{
							// This pair cannot be merged, keep the corresponding blue state marked as a potentially red state. Note that this computation is deferred to this stage from computePairScores in order to consider 
							// only a small subset of pairs that are apparently compatible but will be incompatible once the mandatory merge conditions are taken into account.
						}
						else
							try
							{
								long inconsistencyScore = 0;
								/*if (par.scoresIncludeInconsistencies)
									inconsistencyScore = markovHelper.computeScoreBasedOnInconsistencies(p);*/
								long quality = 0;
								
								if (inconsistencyScore >= 0)
								{
									quality = classifyPair(p,pairs,classFalse);
									if (quality == unknown)
										quality = -inconsistencyScore;// inconsistency was positive because it looks like a reasonable merge. Classifier does not know, hence we have to conclude that it really is a possible merge and return a value reflecting it.
								}
								else
									quality = 200;
									
								if ( quality > 0 )
								{// seems like a bad pair to merge, hence keep the corresponding blue state marked as a possible red.
									if (stateToDebug != null)
										System.out.println("vertex "+p.getQ()+" quality: "+quality+" classification score: "+classifyPair(p,pairs,classFalse));
									ValueAndCount vc = possiblyRedVerticesToTheirQuality.get(p.getQ());
									vc.count++;vc.value+=quality;
								}
								else // we are here because Weka was not confident that this pair should not be merged, hence check the score and if zero, consider blocking the merge. 
								if (par.blacklistZeroScoringPairs && p.getScore() == 0)
								{// blacklisting pairs with zero score - keep the corresponding blue state marked as a possible red.
								}
								else // quality is <= 0 (which means that pair is probably good) and we do not blacklist zeroes
									possiblyRedVerticesToTheirQuality.remove(p.getQ());// this pair seems a plausible merge candidate.
							}
							catch(Exception ex)
							{
								ex.printStackTrace();
								throw new IllegalArgumentException("failed to classify pair "+p, ex);
							}
				}
			}
			if (possiblyRedVerticesToTheirQuality.isEmpty())
				return null;
			CmpVertex bestCandidateForRed = null;
			double bestAverage = -1;
			for(Entry<CmpVertex,ValueAndCount> entry:possiblyRedVerticesToTheirQuality.entrySet())
			{
				double currentValue = ConfusionMatrix.divide(entry.getValue().value, entry.getValue().count);
				if (currentValue > bestAverage)
				{
					bestAverage = currentValue;bestCandidateForRed = entry.getKey();
				}
			}
			assert bestCandidateForRed != null;
			return bestCandidateForRed;
		}
		
		
		/** This function aims to identify a pair that clearly should be merged. In this case, this method will return null. Where no pairs are mergeable, the method will return the pair it is most certain to be unmergeable. */
		protected PairScore getPairToBeLabelledRed(Collection<PairScore> pairs, LearnerGraph tentativeGraph)
		{
			for(PairScore p:pairs)
			{
				assert p.getScore() >= 0;
				
				if (!p.getQ().isAccept() || !p.getR().isAccept()) // if any are rejects, add with a score of zero, these will always work because accept-reject pairs will not get here and all rejects can be merged.
					return null;// negatives can always be merged.
			}
			
			// if we are here, none of the pairs are clear candidates for mergers.
			
			dataCollector.buildSetsForComparators(pairs,tentativeGraph);

			PairScore pairBestToReturnAsRed = null;
			LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMerge(tentativeGraph,labelsLeadingToStatesToBeMerged,labelsLeadingFromStatesToBeMerged);
			
			for(PairScore p:pairs)
			{
				if (!pairsList.isEmpty() && tentativeGraph.pairscores.computePairCompatibilityScore_general(p, pairsList, verticesToMerge, false) < 0)
				// This pair cannot be merged, return as red. Note that this computation is deferred to this stage from computePairScores in order to consider 
				// only a small subset of pairs that are apparently compatible but will be incompatible once the mandatory merge conditions are taken into account.
					return p;
				
				// potentially meaningful pair, check with the classifier
				try
				{
					long inconsistencyScore = 0;
					/*
					if (par.scoresIncludeInconsistencies)
						inconsistencyScore = markovHelper.computeScoreBasedOnInconsistencies(p);
						*/
					long quality = 0;
					
					if (inconsistencyScore >= 0)
						quality = classifyPair(p,pairs,classFalse);
					else
						quality = 200;
						
					if ( quality > 0 )
					{// seems like a bad pair
						if (pairBestToReturnAsRed == null || quality >pairBestToReturnAsRed.getAnotherScore())
							pairBestToReturnAsRed = new PairScore(p.getQ(), p.getR(), p.getScore(), quality);// this is the pair to return.
					}
					else // we are here because Weka was not confident that this pair should not be merged, hence check the score and if zero, consider blocking the merge. 
					if (par.blacklistZeroScoringPairs && p.getScore() == 0)
					{// blacklisting pairs with zero score
						if (pairBestToReturnAsRed == null || pairBestToReturnAsRed.getAnotherScore() == 0)
							pairBestToReturnAsRed = new PairScore(p.getQ(), p.getR(), p.getScore(), 0);// this is the pair that may be mergeable but the score is zero and we hence blacklist it.
					}
					else // quality is <= 0 (which means that pair is probably good) and we do not blacklist zeroes
						return null;// this pair seems a plausible merge candidate.
				}
				catch(Exception ex)
				{
					ex.printStackTrace();
					throw new IllegalArgumentException("failed to classify pair "+p, ex);
				}
			}
			
			return pairBestToReturnAsRed;
		}
		
		public static class TrueFalseCounter implements Serializable
		{
			/**
			 * ID for serialization.
			 */
			private static final long serialVersionUID = 2593806559873226339L;
			
			public int trueCounter = 0, falseCounter = 0;
		}

		public static class PredictionCounter
		{
			int unknownCounter=0,predictedCounter=0,notpredictedCounter=0;

			@Override
			public String toString()
			{
				return "[unknown: "+unknownCounter+", predicted: "+predictedCounter+", not predicted: "+notpredictedCounter+"]";						
			}
			
			public void updateUsingWeka(LearnerThatUsesWekaResults learner, List<PairScore> pairs,List<PairScore> selectedPairs, int classToUse)
			{
				for(PairScore p:selectedPairs)
				{
					long quality = learner.classifyPair(p,pairs,classToUse);
					if (quality == unknown)
						unknownCounter++;
					else
						if (quality>=0)
							predictedCounter++;
						else
							notpredictedCounter++;
				}

			}
			
			public void updateUsingMarkov(MarkovHelper helper, List<PairScore> selectedPairs, boolean positiveForPrediction)
			{
				for(PairScore p:selectedPairs)
				{
					long score = helper.computeScoreBasedOnInconsistencies(p);
					boolean positive = score >= 0;
					if (positive == positiveForPrediction)
						predictedCounter++;
					else
						notpredictedCounter++;
				}

			}
			
			/** Counts the number of times a pair is predicted as positive by Markov and as a not-a-negative by Weka. */
			public void updatePositiveBoth(LearnerThatUsesWekaResults learner, MarkovHelper helper, List<PairScore> pairs,List<PairScore> selectedPairs, int classForNeg)
			{
				for(PairScore p:selectedPairs)
				{
					long quality = learner.classifyPair(p,pairs,classForNeg);
					if (quality < 0 && helper.computeScoreBasedOnInconsistencies(p) >= 0)
						predictedCounter++;
					else
						notpredictedCounter++;
				}
			}
			/** Counts the number of times a pair is predicted as positive by Markov and as a not-a-negative by Weka. */
			public void updateNegativeBoth(LearnerThatUsesWekaResults learner, MarkovHelper helper, List<PairScore> pairs,List<PairScore> selectedPairs, int classForNeg)
			{
				for(PairScore p:selectedPairs)
				{
					long quality = learner.classifyPair(p,pairs,classForNeg);
					if (quality >= 0 && helper.computeScoreBasedOnInconsistencies(p) < 0)
						predictedCounter++;
					else
						notpredictedCounter++;
				}
			}
			/** Counts the number of times a pair is predicted as positive by Markov and as a not-a-negative by Weka. */
			public void updatePositiveEither(LearnerThatUsesWekaResults learner, MarkovHelper helper, List<PairScore> pairs,List<PairScore> selectedPairs, int classForNeg)
			{
				for(PairScore p:selectedPairs)
				{
					long quality = learner.classifyPair(p,pairs,classForNeg);
					if (quality < 0 || helper.computeScoreBasedOnInconsistencies(p) >= 0)
						predictedCounter++;
					else
						notpredictedCounter++;
				}
			}
		}
		
		public static class PredictionEvaluation
		{
			public final PredictionCounter wekaCorrect = new PredictionCounter(),wekaWrong = new PredictionCounter();
			public final PredictionCounter markovCorrect = new PredictionCounter(),markovWrong = new PredictionCounter();
			public final PredictionCounter wmPlusCorrect = new PredictionCounter(),wmPlusWrong = new PredictionCounter();
			public final PredictionCounter wmMinusCorrect = new PredictionCounter(),wmMinusWrong = new PredictionCounter();
			public final PredictionCounter wmPosEitherCorrect = new PredictionCounter(),wmPosEitherWrong = new PredictionCounter();
			protected int updateCounter = 0;
			
			@Override
			public String toString()
			{
				return "WC: "+wekaCorrect+", WW: "+wekaWrong+", MC: "+markovCorrect+", MW: "+markovWrong+", WMPC: "+wmPlusCorrect+", WMPW: "+wmMinusCorrect+", WMnC: "+wmPlusCorrect+", WMnW: "+wmMinusWrong+
						"posEP: "+wmPosEitherCorrect+", posEW: "+wmPosEitherWrong;
			}

			public synchronized void update(LearnerThatUsesWekaResults learner, MarkovHelper markovHelper,List<PairScore> pairs,List<PairScore> correctPairs,List<PairScore> wrongPairs)
			{
				wekaCorrect.updateUsingWeka(learner,pairs,correctPairs, learner.classTrue);wekaWrong.updateUsingWeka(learner,pairs,wrongPairs,learner.classFalse);
				markovCorrect.updateUsingMarkov(markovHelper, correctPairs, true);markovWrong.updateUsingMarkov(markovHelper, wrongPairs, false);
				wmPlusCorrect.updatePositiveBoth(learner,markovHelper,pairs,correctPairs, learner.classFalse);wmPlusWrong.updatePositiveBoth(learner,markovHelper,pairs,wrongPairs, learner.classFalse);
				wmMinusCorrect.updateNegativeBoth(learner,markovHelper,pairs,correctPairs, learner.classFalse);wmMinusWrong.updateNegativeBoth(learner,markovHelper,pairs,wrongPairs, learner.classFalse);
				wmPosEitherCorrect.updatePositiveEither(learner,markovHelper,pairs,correctPairs, learner.classFalse);wmPosEitherWrong.updatePositiveEither(learner,markovHelper,pairs,wrongPairs, learner.classFalse);
				if (updateCounter++ == 100)
				{
					updateCounter = 0;
					System.out.println("QUALITY COUNTER: "+this);
				}
			}
		}

		
		public static interface CollectionOfPairsEstimator
		{
			double obtainEstimateOfTheQualityOfTheCollectionOfPairs(LearnerGraph coregraph, Collection<PairScore> pairs);
		}

		/** Used to select the best red node based on what the subsequent collection of pairs will be. */
		public static CmpVertex selectRedNodeUsingQualityEstimator(LearnerGraph coregraph, Collection<CmpVertex> tentativeRedNodes, CollectionOfPairsEstimator pairQualityEstimator) 
		{
			CmpVertex redVertex = null;double bestScore=-1;
			
			// It is not hard to calculate what blue states will directly surround a specific state chosen to become red, 
			// however those blue states may in turn immediately become red after evaluation and the same would apply
			// to the newly-discovered red states, so we effectively have to re-implement blue state calculation here. 
			// For this reason, it was decided not to do this but instead clone the state machine (possibly in a trimmed form)
			// and ask it for a list of pairs. 
			// In practice, this algorithm turned out to be rather slow because there could be many red states to choose 
			// from and among those, many would lead to many pairs, all of which have to be scored.
			Configuration configCloneAll = coregraph.config.copy();configCloneAll.setLearnerCloneGraph(true);// we need to clone vertices because chooseStatePairs would colour some of the vertices red or blue.
			for(CmpVertex tentativeRed:tentativeRedNodes)
			{
				LearnerGraph graph = new LearnerGraph(configCloneAll);LearnerGraph.copyGraphs(coregraph,graph);
				graph.findVertex(tentativeRed).setColour(JUConstants.RED);// mark the tentative blue as red
				Stack<PairScore> pairs = graph.pairscores.chooseStatePairs(null);// calculate the subsequent red-blue pairs
				double estimate = pairs.isEmpty()? 0:pairQualityEstimator.obtainEstimateOfTheQualityOfTheCollectionOfPairs(coregraph, pairs);
				if (estimate > bestScore)
				{
					bestScore = estimate;redVertex = tentativeRed;
				}
			}			
			
			return redVertex;
		}

		/** Used to select the best red node based on what the subsequent collection of pairs will be. */
		public static CmpVertex selectRedNodeUsingNumberOfNewRedStates(LearnerGraph coregraph, Collection<CmpVertex> tentativeRedNodes) 
		{
			CmpVertex redVertex = null;double bestScore=-1;
			
			// It is not hard to calculate what blue states will directly surround a specific state chosen to become red, 
			// however those blue states may in turn immediately become red after evaluation and the same would apply
			// to the newly-discovered red states, so we effectively have to re-implement blue state calculation here. 
			// For this reason, it was decided not to do this but instead clone the state machine (possibly in a trimmed form)
			// and ask it for a list of pairs. 
			// Assuming that we received red states in the same order as they are encountered by Statechum, 
			// it is appropriate to return the first state that has the highest number of reds after mergers,
			// because where it is actually 
			Configuration configCloneAll = coregraph.config.copy();configCloneAll.setLearnerCloneGraph(true);// we need to clone vertices because chooseStatePairs would colour some of the vertices red or blue.
			for(CmpVertex tentativeRed:tentativeRedNodes)
			{
				LearnerGraph graph = new LearnerGraph(configCloneAll);LearnerGraph.copyGraphs(coregraph,graph);
				graph.findVertex(tentativeRed).setColour(JUConstants.RED);// mark the tentative blue as red
				graph.pairscores.chooseStatePairs(null);// calculate the subsequent red-blue pairs
				int nrReds = graph.getRedStateNumber();
				if (nrReds > bestScore)
				{
					bestScore = nrReds;redVertex = tentativeRed;
				}
			}
			return redVertex;
		}
		
		protected class QualityEstimatorUsingWeka implements CollectionOfPairsEstimator
		{

			@Override
			public double obtainEstimateOfTheQualityOfTheCollectionOfPairs(LearnerGraph coregraph, Collection<PairScore> pairs) 
			{
				dataCollector.buildSetsForComparators(pairs, coregraph);
				return LearnerThatUsesWekaResults.this.obtainEstimateOfTheQualityOfTheCollectionOfPairs(pairs);
			}
			
		}

		CollectionOfPairsEstimator redStateEstimator = new QualityEstimatorUsingWeka();
		
		protected Set<CmpVertex> obtainBlueStatesThatChouldBecomeRedUsingReferenceGraph(LearnerGraph coregraph, List<PairScore> pairs)
		{
			List<PairScore> correctPairs = new ArrayList<PairScore>(pairs.size()), wrongPairs = new ArrayList<PairScore>(pairs.size());
			LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(coregraph, referenceGraph, pairs, correctPairs, wrongPairs);
			Set<CmpVertex> possiblyRedVertices = new TreeSet<CmpVertex>();
			for(PairScore p:pairs)
				possiblyRedVertices.add(p.getQ());

			for(PairScore p:correctPairs)
			{
				possiblyRedVertices.remove(p.getQ());// if we know that a pair is supposed to be accepted, the corresponding blue state should not become red.
			}
			return possiblyRedVertices;
		}
		
		@Override 
		public Stack<PairScore> ChooseStatePairs(final LearnerGraph graph)
		{
			Stack<PairScore> outcome = graph.pairscores.chooseStatePairs(new PairScoreComputation.RedNodeSelectionProcedure() {

				// Here I could use a learner based on metrics of both tentative reds and the perceived quality of the red-blue pairs obtained if I choose any given value.
				// This can be accomplished by doing a clone of the graph and running chooseStatePairs on it with decision procedure that 
				// (a) applies the same rule (of so many) to choose pairs and
				// (b) checks that deadends are flagged. I could iterate this process for a number of decision rules, looking locally for the one that gives best quality of pairs
				// for a particular pairscore decision procedure.
				@Override
				public CmpVertex selectRedNode(LearnerGraph coregraph, @SuppressWarnings("unused") Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
				{
					CmpVertex redVertex = null;
					if (par.useClassifierToChooseNextRed) 
						redVertex = LearnerThatUsesWekaResults.selectRedNodeUsingNumberOfNewRedStates(coregraph, tentativeRedNodes);
					else 
						redVertex = tentativeRedNodes.iterator().next();
					
					return redVertex;
				}

				@Override
				public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, @SuppressWarnings("unused") Collection<CmpVertex> reds, List<PairScore> pairs) 
				{
					CmpVertex stateToLabelRed = null;

					if (par.classifierToChooseWhereNoMergeIsAppropriate)
					{
						PairScore worstPair = getPairToBeLabelledRed(pairs,coregraph);
						if (worstPair != null)
						{
							stateToLabelRed = worstPair.getQ();

							long highestScore=-1;
							for(PairScore p:pairs)
								if (p.getScore() > highestScore) highestScore = p.getScore();
							{
								List<PairScore> pairOfInterest = Arrays.asList(new PairScore[]{worstPair});
								List<PairScore> correctPairs = new ArrayList<PairScore>(pairOfInterest.size()), wrongPairs = new ArrayList<PairScore>(pairOfInterest.size());
								LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(coregraph, referenceGraph, pairOfInterest, correctPairs, wrongPairs);
								if (!correctPairs.isEmpty())
								{// this one is checking the list of wrong pairs because we aim to check that the pair chosen is not the right one to merge
									System.out.println("resolvePotentialDeadEnd: pair forced red: "+stateToLabelRed+" pair: "+worstPair+" max score: "+highestScore+(wrongPairs.isEmpty()?" THAT IS INCORRECT":""));
									getPairToBeLabelledRed(pairs,coregraph);
								}
							}

						}
						else
						{// worstpair == null, implying that there is at least one ok pair to merge.
							List<PairScore> correctPairs = new ArrayList<PairScore>(pairs.size()), wrongPairs = new ArrayList<PairScore>(pairs.size());
							LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(coregraph, referenceGraph, pairs, correctPairs, wrongPairs);
							if (correctPairs.isEmpty())
							{
								System.out.println("neither pair is correct in "+pairs);
								getPairToBeLabelledRed(pairs,coregraph);
							}
						}

						/*
						List<PairScore> correctPairs = new ArrayList<PairScore>(pairs.size()), wrongPairs = new ArrayList<PairScore>(pairs.size());
						LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(coregraph, referenceGraph, pairs, correctPairs, wrongPairs);
						if (predictionQuality != null)
						{
							dataCollector.buildSetsForComparators(pairs,coregraph);
							predictionQuality.update(LearnerThatUsesWekaResults.this,markovHelper,pairs,correctPairs,wrongPairs);
						}
						stateToLabelRed = selectRedStateIfAnySeemsRedEnough(pairs,coregraph,null);
						long highestScore=-1;
						for(PairScore p:pairs)
							if (p.getScore() > highestScore) highestScore = p.getScore();
						Set<CmpVertex> possiblyReds = obtainBlueStatesThatChouldBecomeRedUsingReferenceGraph(coregraph, pairs);
						if (stateToLabelRed != null)
						{
							
							if (!possiblyReds.contains(stateToLabelRed))
							{// this one is checking the list of wrong pairs because we aim to check that the pair chosen is not the right one to merge
								System.out.println("resolvePotentialDeadEnd: state "+stateToLabelRed+" is incorrectly made red, correct reds are "+possiblyReds+"  max score: "+highestScore);
								//selectRedStateIfAnySeemsRedEnough(pairs,coregraph,stateToLabelRed);
							}
								
						}
						else
						{
							if (correctPairs.isEmpty())
							{
								System.out.println("no red vertex returned, however neither pair is correct in "+pairs);
								//selectRedStateIfAnySeemsRedEnough(pairs,coregraph,wrongPairs.get(0).getQ());
							}
						}		
						 */
						//System.out.println("resolvePotentialDeadEnd: number of states considered = "+pairs.size()+" number of reds: "+reds.size()+(worstPair != null?(" pair chosen as the worst: "+worstPair):""));
					}
					return stateToLabelRed;// resolution depends on whether Weka has successfully guessed that all pairs are wrong.
				}
				
				@Override
				public void initComputation(LearnerGraph gr) {
					markovHelper.initComputation(gr);
				}

				@Override
				public long overrideScoreComputation(PairScore p) {
					long score = -1;
					/*
					if (par.scoresIncludeInconsistencies)
						score = markovHelper.computeScoreBasedOnInconsistencies(p);
					else
					*/
						score = p.getScore();// dummy
					return score;
				}

				@Override
				public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(CmpVertex currentRed) 
				{
					return markovHelper.getSurroundingTransitions(currentRed);
				}
			});
			if (!outcome.isEmpty())
			{
				//System.out.println("classifyPairs: number of states considered = "+filteredPairs.size()+" number of reds: "+graph.getRedStateNumber()+" ( before filtering "+outcome.size()+")");
				
				Stack<PairScore> possibleResults = classifyPairs(outcome,graph,null), origPairs = outcome;
				LearningSupportRoutines.updateStatistics(pairQuality, graph,referenceGraph, outcome);

				if (!possibleResults.isEmpty())
				{
					outcome = possibleResults;// no pairs have been provided by the modified algorithm, hence using the default one.
				}

				{
					List<PairScore> correctPairs = new ArrayList<PairScore>(1), wrongPairs = new ArrayList<PairScore>(1);
					List<PairScore> pairs = new ArrayList<PairScore>(1);pairs.add(outcome.peek());
					LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(graph, referenceGraph, pairs, correctPairs, wrongPairs);
					
					{
						List<PairScore> correctPairsFromAll = new ArrayList<PairScore>(origPairs.size()), wrongPairsFromAll = new ArrayList<PairScore>(origPairs.size());
						LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(graph, referenceGraph, origPairs, correctPairsFromAll, wrongPairsFromAll);
						if (predictionQuality != null)
						{
							dataCollector.buildSetsForComparators(origPairs,graph);
							predictionQuality.update(LearnerThatUsesWekaResults.this,markovHelper,origPairs,correctPairsFromAll,wrongPairsFromAll);
						}					
					}
					
					if (correctPairs.isEmpty())
					{
						System.out.println("wrong merge at "+outcome.peek()+" entire list of pairs is "+origPairs);
						List<PairScore> correctPairsFromAllPairs = new ArrayList<PairScore>(origPairs.size()), wrongPairsFromAllPairs = new ArrayList<PairScore>(origPairs.size());
						LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(graph, referenceGraph, origPairs, correctPairsFromAllPairs, wrongPairsFromAllPairs);
						System.out.println("correct pairs are: "+correctPairsFromAllPairs);
						classifyPairs(origPairs,graph,outcome.peek());
						if (!correctPairsFromAllPairs.isEmpty())
							classifyPairs(origPairs,graph,correctPairsFromAllPairs.get(0));
						selectRedStateIfAnySeemsRedEnough(outcome,graph,null);
					}
				}
			}
			return outcome;
		}

	} // uses a classifier in order to rank pairs.

	/** Used to encapsulate computation of a typical difference using different methods. */
	public static interface DifferenceToReference
	{
		public double getValue();
	}
	
	public static class DifferenceOfTheNumberOfStates implements DifferenceToReference
	{
		private final int difference;
		
		public DifferenceOfTheNumberOfStates(int val)
		{
			difference = val;
		}
		
		@Override
		public double getValue()
		{
			return difference;
		}
		
	}
	
	public static class DifferenceToReferenceFMeasure extends ConfusionMatrix implements DifferenceToReference
	{		
		protected DifferenceToReferenceFMeasure(int tpArg, int tnArg, int fpArg, int fnArg) 
		{
			super(tpArg, tnArg, fpArg, fnArg);
		}
		
		protected DifferenceToReferenceFMeasure(ConfusionMatrix mat) 
		{
			super(mat);
		}

		@Override
		public double getValue()
		{
			return fMeasure();
		}
		
		/** Given two graphs, estimates the difference between the two. 
		 *
		 * @param refenceGraph reference automaton
		 * @param actualAutomaton the automaton to compare the reference with
		 * @param testSet a test set to use for comparison, useful when language-based measures such as an f-measure are utulised.
		 * @param config configuration to use for doing the comparison. This is useful to configure Linear (if the comparison is done using Linear).
		 * @param cpuNumber the number of processors to use. Usually set to 1 because we run as many experiments as there are CPUs and so individual experiments should not consume more computational power than we have available for them. 
		 */
		public static DifferenceToReferenceFMeasure estimationOfDifference(LearnerGraph referenceGraph, LearnerGraph actualAutomaton, Collection<List<Label>> testSet)
		{
			if (actualAutomaton.getAcceptStateNumber() == 0)
				return new DifferenceToReferenceFMeasure(new DifferenceToReferenceFMeasure(0,0,0,0));// a graph with all reject states is used to indicate that the learnt graph contains too many states.

			LearnerGraph learntGraph = new LearnerGraph(actualAutomaton.config);AbstractPathRoutines.removeRejectStates(actualAutomaton,learntGraph);
	       	ConfusionMatrix mat = DiffExperiments.classify(testSet, referenceGraph, learntGraph);
			return new DifferenceToReferenceFMeasure(mat);
		}
	}
	
	public static class DifferenceToReferenceLanguageBCR extends ConfusionMatrix implements DifferenceToReference
	{		
		public DifferenceToReferenceLanguageBCR(int tpArg, int tnArg, int fpArg, int fnArg) 
		{
			super(tpArg, tnArg, fpArg, fnArg);
		}
		
		public DifferenceToReferenceLanguageBCR(ConfusionMatrix mat) 
		{
			super(mat);
		}

		@Override
		public double getValue()
		{
			return BCR();
		}
		
		/** Given two graphs, estimates the difference between the two. 
		 *
		 * @param refenceGraph reference automaton
		 * @param actualAutomaton the automaton to compare the reference with
		 * @param testSet a test set to use for comparison, useful when language-based measures such as an f-measure are utulised.
		 * @param config configuration to use for doing the comparison. This is useful to configure Linear (if the comparison is done using Linear).
		 * @param cpuNumber the number of processors to use. Usually set to 1 because we run as many experiments as there are CPUs and so individual experiments should not consume more computational power than we have available for them. 
		 */
		public static DifferenceToReferenceLanguageBCR estimationOfDifference(LearnerGraph referenceGraph, LearnerGraph actualAutomaton, Collection<List<Label>> testSet)
		{
			if (actualAutomaton.getAcceptStateNumber() == 0)
				return new DifferenceToReferenceLanguageBCR(0,0,0,0);// a graph with all reject states is used to indicate that the learnt graph contains too many states.

			LearnerGraph learntGraph = new LearnerGraph(actualAutomaton.config);AbstractPathRoutines.removeRejectStates(actualAutomaton,learntGraph);
	       	ConfusionMatrix mat = DiffExperiments.classify(testSet, referenceGraph, learntGraph);
			return new DifferenceToReferenceLanguageBCR(mat);
		}
		
		
	}
	
	public static class DifferenceToReferenceDiff implements DifferenceToReference
	{
		protected double valueA, valueB;
		
		public DifferenceToReferenceDiff(double A, double B)
		{
			valueA=A;valueB=B;
		}
		
		@Override
		public double getValue()
		{
			return (valueA+valueB)/2;
		}
		
		@Override
		public String toString()
		{
			return String.format("< %g, %g >",valueA,valueB);
		}

		public static DifferenceToReferenceDiff estimationOfDifferenceDiffMeasure(LearnerGraph referenceGraph, LearnerGraph actualAutomaton, Configuration config, int cpuNumber)
		{
			if (actualAutomaton.getAcceptStateNumber() == 0)
				return new DifferenceToReferenceDiff(0,0);// a graph with all reject states is used to indicate that the learnt graph contains too many states.
			
	       	LearnerGraph learntGraph = new LearnerGraph(actualAutomaton.config);AbstractPathRoutines.removeRejectStates(actualAutomaton,learntGraph);
			statechum.analysis.learning.linear.GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new statechum.analysis.learning.linear.GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
			statechum.analysis.learning.linear.GD.ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> changesCounter = new statechum.analysis.learning.linear.GD.ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(referenceGraph, learntGraph, null);
			gd.computeGD(referenceGraph, learntGraph, cpuNumber,changesCounter,config);
			
			int referenceEdges = referenceGraph.pathroutines.countEdges(), actualEdges = learntGraph.pathroutines.countEdges();
			//return (((double)referenceEdges-changesCounter.getRemoved())/referenceEdges+((double)actualEdges-changesCounter.getAdded())/actualEdges)/2;
			return new DifferenceToReferenceDiff(
					referenceEdges == 0?0:((double)referenceEdges-changesCounter.getRemoved())/referenceEdges,
					actualEdges == 0?0:(((double)actualEdges-changesCounter.getAdded())/actualEdges));
		}
	}
	
	
	/** This one holds two values and picks one that is not null. Where both are present, BCR is given preference.
	 */
	public static class ScoresForGraph implements DifferenceToReference
	{
		public DifferenceToReference differenceStructural, differenceBCR , differenceFMeasure;
		public DifferenceToReference nrOfstates;
		public long inconsistency;
		public double fanoutPos,fanoutNeg;
		public int ptaStateNumber;
		public long executionTime;
		
		@Override
		public double getValue()
		{
			return differenceBCR != null? differenceBCR.getValue():differenceStructural.getValue();
		}
		
		@Override
		public String toString()
		{
			if (differenceBCR == null && differenceStructural == null)
				return "UNKNOWN SCORE";
			String outcome = "";
			if (differenceBCR != null)
				outcome+=String.format("BCR: %s",differenceBCR.toString());
			if (differenceStructural != null)
			{
				if (!outcome.isEmpty()) outcome+=",";
				outcome+=String.format("structural: %s",differenceStructural.toString());
			}
			if (differenceFMeasure != null)
			{
				if (!outcome.isEmpty()) outcome+=",";
				outcome+=String.format("FMeasure: %s",differenceFMeasure.toString());
			}
			return outcome;
		}
	}
	
	/** The outcome of an experiment using a single FSM and a collection of walks represented as a PTA. */
	public static class SampleData
	{
		public final LearnerGraph referenceGraph, initialPTA;
		public ScoresForGraph premergeLearner, actualLearner,actualConstrainedLearner,referenceLearner,ktailsLearner,markovLearner,EDSMzero, EDSMone, EDSMtwo;
		public Map<String,ScoresForGraph> miscGraphs;
		
		/** %% of states in a reference graph can be identified by singleton sequences. */
		public long fractionOfStatesIdentifiedBySingletons = 0;
		
		/** Whether our selection of a centre vertex was correct. */
		public boolean centreCorrect;
		
		/** The number of paths that were merged before identification of a centre vertex. */
		public int centrePathNumber = 0;
		
		public long traceNumber = 0;
		
		/** Number of states in the reference graph. */
		public long stateNumber = 0;
		
		/** Inconsistency of the reference graph. Important to ensure that the input sample is not excessively biased. */
		public long inconsistencyReference;
		
		/** %% of transitions in the reference graph that were covered by the training sample. */
		public long transitionsSampled;
		
		/** %% of correct predictions by the Markov model. */
		public long markovPrecision, markovRecall;
		
		/** How many comparisons have been performed as part of learning. */
		public long comparisonsPerformed;
		
		public SampleData()
		{
			this(null,null);
		}
		
		public SampleData(LearnerGraph argReferenceGraph, LearnerGraph argInitialPTA)
		{
			referenceGraph = argReferenceGraph;initialPTA = argInitialPTA;
		}
		
		@Override
		public String toString()
		{
			if (actualLearner !=null) return actualLearner.toString();
			if (referenceLearner !=null) return referenceLearner.toString();
			if (miscGraphs != null) return miscGraphs.toString();
			
			return "UNKNOWN";
		}
	}
	
	public static class ThreadResult 
	{
		public List<SampleData> samples = new LinkedList<SampleData>();
	}
	
	public static interface ThreadResultID
	{
		/** Returns an ID of a row of results in a spreadsheet. Different columns correspond to different values of parameters. */
		public String getRowID();
		/** Header for each column. It is frequently multi-line hence an array of strings is returned. */
		public String []getColumnText();
		/** Returns an ID of a column of results in a spreadsheet. Where multiple experiments populate the same row in a spreadsheet, we need to tell which entries are to be placed in a single row. This ID makes it possible to do it. */
		public String getColumnID();
		/** Each cell may contain results of multiple experiments, this one reports the respective headers. */
		public String[] headerValuesForEachCell();
		/** Returns the name of the current experiment. */
		public String getSubExperimentName();
		/** Returns the position of the "execution time element", starting from zero. Negatives mean no execution time. This element will be scaled based on the factor in the global configuration. */
		public int executionTimeInCell();
	}

	public abstract static class PairQualityLearnerRunner extends UASExperiment<PairQualityParameters,ExperimentResult<PairQualityParameters>>
	{
		protected final WekaDataCollector sampleCollector;
		
		public PairQualityLearnerRunner(WekaDataCollector collector,PairQualityParameters parameters, LearnerEvaluationConfiguration evalCnf)
		{
			super(parameters,evalCnf,directoryNamePrefix);sampleCollector = collector;
		}
		
		public abstract LearnerWithMandatoryMergeConstraints createLearner(LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, WekaDataCollector argDataCollector, final LearnerGraph argInitialPTA);
		
		@Override
		public ExperimentResult<PairQualityParameters> call() throws Exception 
		{
			final int tracesAlphabet = par.tracesAlphabetMultiplier*par.states;
			ExperimentResult<PairQualityParameters> outcome = new ExperimentResult<PairQualityParameters>(par);
			
			final Random rnd = new Random(par.seed*31+par.attempt*par.states);
			ConstructRandomFSM fsmConstruction = new ConstructRandomFSM();
			fsmConstruction.generateFSM(rnd, tracesAlphabet, par.states, par.seed, par.pickUniqueFromInitial, learnerInitConfiguration);
			referenceGraph = fsmConstruction.referenceGraph;
			
			final Collection<List<Label>> testSet = LearningAlgorithms.buildEvaluationSet(referenceGraph);
			LearnerGraph pta = new LearnerGraph(learnerInitConfiguration.config);
			final int tracesToGenerate = LearningSupportRoutines.makeEven(par.traceQuantity);
			
			if (par.pickUniqueFromInitial)
			{
				final RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(par.seed*31+par.attempt*par.states),5,referenceGraph.getVertex(Arrays.asList(new Label[]{fsmConstruction.uniqueFromInitial})));
				generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
										
						@Override
						public int getLength() {
							return  par.traceLengthMultiplier*par.states*tracesAlphabet;// same as for Markov learner
						}
	
						@Override
						public int getPrefixLength(int len) {
							return len;
						}
					},true,true,null,Arrays.asList(new Label[]{fsmConstruction.uniqueFromInitial}));
	
				if (par.onlyUsePositives)
					pta.paths.augmentPTA(generator.getAllSequences(0).filter(new FilterPredicate() {
						@Override
						public boolean shouldBeReturned(Object name) {
							return ((statechum.analysis.learning.rpnicore.RandomPathGenerator.StateName)name).accept;
						}
					}));
				else
					pta.paths.augmentPTA(generator.getAllSequences(0));// the PTA will have very few reject-states because we are generating few sequences and hence there will be few negative sequences.
					// In order to approximate the behaviour of our case study, we need to compute which pairs are not allowed from a reference graph and use those as if-then automata to start the inference.
					// This is done below if onlyUsePositives is not set. 
			}
			else
			{// not using unique from initial
				final RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(par.seed*31+par.attempt*par.states),5,null);
				generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
						
						@Override
						public int getLength() {
							return par.traceLengthMultiplier*par.states*tracesAlphabet;// not the same as for SmallVsHuge or LearnerEvaluation
						}
		
						@Override
						public int getPrefixLength(int len) {
							return len;
						}
					});
	
	
				if (par.onlyUsePositives)
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
			}
			pta.clearColours();
			if (!par.onlyUsePositives)
				assert pta.getStateNumber() > pta.getAcceptStateNumber() : "graph with only accept states but onlyUsePositives is not set";
			else 
				assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
			
			LearnerWithMandatoryMergeConstraints learnerOfPairs = null;
			LearnerGraph actualAutomaton = null;
			LearnerGraph trimmedReference = LearningSupportRoutines.trimUncoveredTransitions(pta,referenceGraph);

			final MarkovModel m= new MarkovModel(par.dataCollectorParameters.markovParameters.chunkLen,true,true,false);
			new MarkovClassifier(m, pta).updateMarkov(false);// construct Markov chain if asked for.
			final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();
			
			PerformFirstMerge fmg = new PerformFirstMerge();fmg.ptaToUseForInference=pta;
			if (par.dataCollectorParameters.markovParameters.useCentreVertex)
			{
				saveGraph(namePTABEFORECENTRE,pta);
				fmg.buildFirstGraph(pta, null, par.dataCollectorParameters.markovParameters, m, checker);
			}
						
			// not merging based on a unique transition from an initial state
			learnerOfPairs = createLearner(learnerInitConfiguration,referenceGraph,sampleCollector,fmg.ptaToUseForInference);
			sampleCollector.markovHelper.setMarkov(m);sampleCollector.markovHelper.setChecker(checker);
 			long startTime = LearningSupportRoutines.getThreadTime();
 			/*
 			System.out.println("learning started");
 			tmpAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
			System.out.println("second go on the graph");
			tmpAutomaton.clearColours();
			LearnerWithMandatoryMergeConstraints learnerSecondAttempt = createLearner(learnerInitConfiguration,referenceGraph,dataCollector,tmpAutomaton);
			actualAutomaton = learnerSecondAttempt.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 			System.out.println("learning finished");
 			*/
 			actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 			
 			long runTime = LearningSupportRoutines.getThreadTime()-startTime;
			
			SampleData dataSample = new SampleData(null,null);
			dataSample.actualLearner = estimateDifference(referenceGraph, actualAutomaton, testSet);
			dataSample.actualLearner.executionTime = runTime;
			dataSample.referenceLearner = MarkovExperiment.zeroScore;
			dataSample.centreCorrect = fmg.correctCentre;
			dataSample.centrePathNumber = fmg.centrePathNumber;
			dataSample.fractionOfStatesIdentifiedBySingletons=Math.round(100*MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(referenceGraph));
			dataSample.stateNumber = referenceGraph.getStateNumber();
			dataSample.transitionsSampled = Math.round(100*trimmedReference.pathroutines.countEdges()/referenceGraph.pathroutines.countEdges());
			statechum.Pair<Double,Double> correctnessOfMarkov = new MarkovClassifier(m, referenceGraph).evaluateCorrectnessOfMarkov();
			dataSample.markovPrecision = Math.round(100*correctnessOfMarkov.firstElem);dataSample.markovRecall = Math.round(100*correctnessOfMarkov.secondElem);
			/*
			GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = 
					new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();

				LearnerGraphND grA=new LearnerGraphND(referenceGraph,referenceGraph.config),
						grB=new LearnerGraphND(actualAutomaton,actualAutomaton.config);
				DirectedSparseGraph gr = gd.showGD(
						grA,grB,
						ExperimentRunner.getCpuNumber());
				Visualiser.updateFrame(gr, null);
				*/
 			outcome.samples.add(dataSample);
			if (sampleCollector != null)
				synchronized(sampleCollector.trainingData)
				{
					for(int i=0;i< sampleCollector.trainingData.numInstances();++i)
						sampleCollector.trainingData.add(sampleCollector.trainingData.instance(i));
				}
			sampleCollector.trainingData.delete();
			
			return outcome;
		}

		// Delegates to a specific estimator
		ScoresForGraph estimateDifference(LearnerGraph reference, LearnerGraph actual,Collection<List<Label>> testSet)
		{
			ScoresForGraph outcome = new ScoresForGraph();
			outcome.differenceStructural=DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(reference, actual, learnerInitConfiguration.config, 1);
			outcome.differenceBCR=DifferenceToReferenceLanguageBCR.estimationOfDifference(reference, actual,testSet);
			outcome.differenceFMeasure=DifferenceToReferenceFMeasure.estimationOfDifference(reference, actual,testSet);
			outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actual.getStateNumber() - referenceGraph.getStateNumber());
			return outcome;
		}
	}
}
