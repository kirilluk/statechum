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

import java.awt.Frame;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.TreeSet;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.Stack;

import junit.framework.Assert;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.Label;
import statechum.ProgressIndicator;
import statechum.StatechumXML;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.analysis.learning.PairOfPaths;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.PairSelection.WekaDataCollector.PairRank;
import statechum.analysis.learning.experiments.mutation.DiffExperiments;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.LearnerSimulator;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.collections.ArrayMapWithSearch;
import statechum.model.testset.PTASequenceEngine;
import weka.classifiers.Classifier;
import weka.core.Instance;
import weka.core.Instances;

/** This one aims to learn how to choose pairs and red states in the way that leads to most accurate learning
 * outcomes.
 * 
 * @author kirill
 */
public class PairQualityLearner 
{
   	public static final String largePTALogsDir = "resources"+File.separator+"largePTA"+File.separator;
  	public static final String largePTAFileName = largePTALogsDir+"largePTA.zip";
	
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
	
	/** Describes the data that is gathered for every pair in order to make meaningful comparisons between this pair and others. */   
	public static class PairMeasurements
	{
		public int nrOfAlternatives;
		public long compatibilityScore;
		public boolean adjacent;
	}

	public static int sgn(long value)
	{
		if (value>0)
			return 1;
		else
			if (value < 0)
				return -1;
		return 0;
	}

	/** Given a graph and a collection of pairs, this one uses the correct graph to split the collection into "correct" pairs that correspond to the same state in the correct graph and "wrong" pairs which states
	 * are not merged in the correct graph.
	 *  
	 * @param graph the graph to consider
	 * @param correctGraph states that should be merged
	 * @param pairs pairs to consider
	 * @param correct collection into which correct ones will be added. 
	 * @param wrong collection where the wrong ones will be added.
	 * @return the index of the first pair in the supplied list of pairs that is deemed correct.
	 */
	public static int SplitSetOfPairsIntoRightAndWrong(LearnerGraph graph, LearnerGraph correctGraph, Collection<PairScore> pairs, Collection<PairScore> correctPairs, Collection<PairScore> wrongPairs)
	{
		Set<CmpVertex> statesOfInterest = new HashSet<CmpVertex>();
		for(PairScore pair:pairs)
		{
			statesOfInterest.add(pair.getQ());statesOfInterest.add(pair.getR());
		}
		Map<CmpVertex,LinkedList<Label>> stateToPath = PairOfPaths.convertSetOfStatesToPaths(graph, statesOfInterest);

		
		int firstCorrectPair = JUConstants.intUNKNOWN, cnt=0;
		for(PairScore p:pairs)
		{
			CmpVertex blue = correctGraph.getVertex(stateToPath.get(p.getQ()));if (blue != null && !blue.isAccept()) blue = null;
			CmpVertex red = correctGraph.getVertex(stateToPath.get(p.getR()));if (red != null && !red.isAccept()) red = null;
			if (blue == red)
			{
				// it would be right to merge this pair.
				correctPairs.add(p);
				if (firstCorrectPair == JUConstants.intUNKNOWN)
					firstCorrectPair = cnt;
			}
			else
				// not appropriate to merge this pair.
				wrongPairs.add(p);
			
			++cnt;
		}
		return firstCorrectPair;
	}
	
	
	public static WekaDataCollector createDataCollector()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		
		assessors.add(classifier.new PairRank("conventional score")
		{// 1
			@Override
			public long getValue(PairScore p) {
				return measurementsForCurrentStack(p).compatibilityScore;
			}
		});
		
		assessors.add(classifier.new PairRank("statechum score")
		{// 2
			@Override
			public long getValue(PairScore p) {
				return p.getScore();
			}
		});
		
		assessors.add(classifier.new PairRank("size of tree rooted at Blue")
		{// 3
			@Override
			public long getValue(PairScore p) {
				return treeRootedAt(p.getQ());
			}
		});
		
		assessors.add(classifier.new PairRank("Number of alternatives with same red")
		{// 4
			@Override
			public long getValue(PairScore p) {
				return  measurementsForCurrentStack(p).nrOfAlternatives;
			}
		});
		
		assessors.add(classifier.new PairRank("Depth of Blue")
		{// 5
			@Override
			public long getValue(PairScore p) {
				return  p.getQ().getDepth();
			}
		});
		
		assessors.add(classifier.new PairRank("Depth of Red")
		{// 6
			@Override
			public long getValue(PairScore p) {
				return  p.getR().getDepth();
			}
		});
		
		assessors.add(classifier.new PairRank("Statechum score is above zero")
		{// 7
			@Override
			public long getValue(PairScore p) {
				return  p.getScore() > 0?1:0;
			}
		});
		
		assessors.add(classifier.new PairRank("state identifiers Red")
		{// 8
			@Override
			public long getValue(PairScore p) {
				return p.getR().getIntegerID();
			}
		});
		
		assessors.add(classifier.new PairRank("state identifiers Blue")
		{// 9
			@Override
			public long getValue(PairScore p) {
				return p.getQ().getIntegerID();
			}
		});
		
		assessors.add(classifier.new PairRank("proximity of the red and blue by depth")
		{// 10
			@Override
			public long getValue(PairScore p) {
				return p.getQ().getDepth()-p.getR().getDepth();
			}
		});
		
		assessors.add(classifier.new PairRank("difference between conventional scores divided by 3")
		{// 11
			@Override
			public long getValue(PairScore p) {
				return  measurementsForCurrentStack(p).compatibilityScore/3;
			}
		});
		
		assessors.add(classifier.new PairRank("whether red and blue are adjacent")
		{// 12
			@Override
			public long getValue(PairScore p) {
				return measurementsForCurrentStack(p).adjacent? 1:0;
			}
		});
		
		assessors.add(classifier.new PairRank("number of new outgoing transitions from blue")
		{// 13
			@Override
			public long getValue(PairScore p) {
				Set<Label> outgoingRed = new TreeSet<Label>();outgoingRed.addAll(tentativeGraph().transitionMatrix.get(p.getR()).keySet());
				Set<Label> outgoingBlue = new TreeSet<Label>();outgoingBlue.addAll(tentativeGraph().transitionMatrix.get(p.getQ()).keySet());
				outgoingBlue.removeAll(outgoingRed);return outgoingRed.size();
			}
		});
		
		assessors.add(classifier.new PairRank("number of new outgoing transitions from red")
		{// 14
			@Override
			public long getValue(PairScore p) {
				Set<Label> outgoingRed = new TreeSet<Label>();outgoingRed.addAll(tentativeGraph().transitionMatrix.get(p.getR()).keySet());
				Set<Label> outgoingBlue = new TreeSet<Label>();outgoingBlue.addAll(tentativeGraph().transitionMatrix.get(p.getQ()).keySet());
				outgoingRed.removeAll(outgoingBlue);return outgoingRed.size();
			}
		});
		
		classifier.initialise("HindsightExperiment",100000,assessors);
		return classifier;
	}
	
	public static class InitialConfigurationAndData
	{
		public ProgressDecorator.InitialData initial;
		public LearnerEvaluationConfiguration learnerInitConfiguration;
	}
	
	public static InitialConfigurationAndData loadInitialAndPopulateInitialConfiguration(String argPTAFileName, STATETREE matrixType, ConvertALabel converter) throws IOException
	{// this part is nested in order to ensure that an instance of LearnerSimulator
	 // goes out of scope and is garbage collected as soon as possible. It holds a great deal
	 // of Xerces objects used for recording execution traces that is not used in this test but takes
	 // a lot of memory.
		InitialConfigurationAndData outcome = new InitialConfigurationAndData();
		
		final java.io.FileInputStream inputStream = new java.io.FileInputStream(argPTAFileName);
		final LearnerSimulator simulator = new LearnerSimulator(inputStream,true,converter);
		Configuration defaultConfig = Configuration.getDefaultConfiguration().copy();
		//defaultConfig.setRejectPositivePairsWithScoresLessThan(1);
		outcome.learnerInitConfiguration = simulator.readLearnerConstructionData(defaultConfig);
		outcome.learnerInitConfiguration.setLabelConverter(converter);
		if (matrixType != null) outcome.learnerInitConfiguration.config.setTransitionMatrixImplType(matrixType);
		
		final org.w3c.dom.Element nextElement = simulator.expectNextElement(StatechumXML.ELEM_INIT.name());
		outcome.initial = simulator.readInitialData(nextElement);
		inputStream.close();
		return outcome;
	}
	

	/** This learner runs an experiment that attempts to determine the best strategy for selection of pairs based on scores and other parameters and subsequently evaluates it. */
	public static void main(String args[]) throws Exception
	{
		try
		{
			runExperiment();
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
		
	/** This class knows what the reference automaton is and is able to pick correct pairs out of a set to merge. */
	public static abstract class LearnerThatCanClassifyPairs extends RPNIUniversalLearner
	{
		final LearnerGraph initialPTA;

		public LearnerThatCanClassifyPairs(Frame parent, LearnerEvaluationConfiguration evalCnf, LearnerGraph reference, LearnerGraph argInitialPTA) 
		{
			super(parent, evalCnf);
			referenceGraph = reference;initialPTA = argInitialPTA;
		}

		protected final LearnerGraph referenceGraph;
		protected boolean allMergersCorrect = true;
		
		public boolean checkAllMergersCorrect()
		{
			return allMergersCorrect;
		}

		/** Returns one of the correct pairs.
		 */
		public PairScore pickCorrectPair(Stack<PairScore> pairs, LearnerGraph tentativeGraph)
		{
			List<PairScore> correctPairs = new ArrayList<PairScore>(pairs.size()), wrongPairs = new ArrayList<PairScore>(pairs.size());
					
			SplitSetOfPairsIntoRightAndWrong(tentativeGraph, referenceGraph, pairs, correctPairs, wrongPairs);
			
			// without sorting the pairs, the learner finds itself in a situation with no valid pairs to choose from.
			Comparator<PairScore> PairComparator = new Comparator<PairScore>(){

				@Override
				// The first element is the one where o2 is greater than o1, i.e. comparison below returns negative.
				public int compare(PairScore o1, PairScore o2) {
					// if o1 is negative and o2 is positive, the outcome is negative.
					int outcome = sgn( o2.getAnotherScore() - o1.getAnotherScore() );
					return outcome;
					
				}};
				
			Collections.sort(correctPairs,PairComparator);
			if (correctPairs.isEmpty())
			{
				Collections.sort(wrongPairs, PairComparator);
				allMergersCorrect = false;
				return wrongPairs.iterator().next();
			}
			return correctPairs.iterator().next();
		}

		/** Returns the best pair according to the score.
		 */
		public PairScore pickPairQSMLike(Stack<PairScore> pairs)
		{
			PairScore bestPair = pairs.iterator().next();
			for(PairScore p:pairs)
				if (p.getScore() > bestPair.getScore())
					bestPair=p;
			
			return bestPair;
		}
		
		/** There are cases when no selected pair is actually valid. The method below chooses a state to be marked as red because it is the only choice that we can make. */
		public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, @SuppressWarnings("unused") Collection<CmpVertex> reds, Collection<PairScore> pairs) 
		{
			
			CmpVertex stateToMarkRed = null;
			List<PairScore> correctPairs = new ArrayList<PairScore>(pairs.size()), wrongPairs = new ArrayList<PairScore>(pairs.size());
			SplitSetOfPairsIntoRightAndWrong(coregraph, referenceGraph, pairs, correctPairs, wrongPairs);
			if (correctPairs.isEmpty())
				stateToMarkRed = wrongPairs.get(0).getQ();// no correct pairs found to merge, return the first wrong pair so the corresponding state is marked as red.
			
			return stateToMarkRed;
		}

		@Override
		public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
		{// fast merger
			return MergeStates.mergeAndDeterminize(original, pair);
		}
		

		@Override 
		public LearnerGraph init(Collection<List<Label>> plus,	Collection<List<Label>> minus) 
		{
			LearnerGraph graph = super.init(plus,minus);
			LearnerGraph.copyGraphs(initialPTA, graph);
			return initialPTA;
		}
		
		@SuppressWarnings("unused")
		@Override 
		public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
		{
			throw new UnsupportedOperationException();
		}			
	}
	

	/** This one is a reference learner. */
	public static class ReferenceLearner extends LearnerThatCanClassifyPairs
	{
		public ReferenceLearner(Frame parent, LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, final LearnerGraph argInitialPTA) 
		{
			super(parent, evalCnf,argReferenceGraph, argInitialPTA);
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

				@SuppressWarnings("unused")
				@Override
				public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<PairScore> pairs) 
				{
					return null;
				}});
			if (!outcome.isEmpty())
			{
				PairScore chosenPair = pickPairQSMLike(outcome);
				outcome.clear();outcome.push(chosenPair);
			}
			
			return outcome;
		}		
	}
	
	/** This one uses Weka to learn how to make correct classification of pairs into right and wrong. */
	public static class LearnerThatUpdatesWekaResults extends LearnerThatCanClassifyPairs
	{
		final WekaDataCollector dataCollector;
		final RBoxPlot<String> gr_ErrorsAndDeadends;
		
		/** Builds a decision tree and returns it. Throws an exception if something goes wrong. */
		public Classifier getClassifier() throws Exception
		{
			weka.classifiers.trees.J48 cl = new weka.classifiers.trees.J48();
			cl.buildClassifier(dataCollector.trainingData);return cl;
		}
				
		public LearnerThatUpdatesWekaResults(LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, WekaDataCollector argDataCollector, final LearnerGraph argInitialPTA, RBoxPlot<String> argErrorsAndDeadends) 
		{
			super(null, evalCnf,argReferenceGraph, argInitialPTA);dataCollector = argDataCollector;gr_ErrorsAndDeadends = argErrorsAndDeadends;
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
				public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<PairScore> pairs) 
				{
					dataCollector.updateDatasetWithPairs(pairs, coregraph, referenceGraph);
					CmpVertex red = LearnerThatUpdatesWekaResults.this.resolvePotentialDeadEnd(coregraph, reds, pairs);
					if (red != null && gr_ErrorsAndDeadends != null)
						synchronized(gr_ErrorsAndDeadends)
						{
							gr_ErrorsAndDeadends.add("D", (double)coregraph.getRedStateNumber());
						}
					return red;
					//return null;// for no resolution
				}});
			if (!outcome.isEmpty())
			{
				dataCollector.updateDatasetWithPairs(outcome, graph, referenceGraph);
				PairScore chosenPair = pickCorrectPair(outcome, graph);
				PairScore qsmPair = pickPairQSMLike(outcome);
				if (!qsmPair.equals(chosenPair) && gr_ErrorsAndDeadends != null)
					synchronized(gr_ErrorsAndDeadends)
					{
						gr_ErrorsAndDeadends.add("E", (double)graph.getRedStateNumber());
					}
					
				outcome.clear();outcome.push(chosenPair);
			}
			
			return outcome;
		}		
	} // class that builds a classifier tree.

	/** Uses the supplied classifier to rank pairs. */
	public static class LearnerThatUsesWekaResults extends LearnerThatCanClassifyPairs
	{
		final Classifier classifier;
		
		/** The data collector is only used in order to evaluate pairs, no data is actually added to it hence no need to use the same instance across many machines. */
		final WekaDataCollector dataCollector = createDataCollector();
		protected RBoxPlot<String> gr_PairQuality = null;
		final int classTrue,classFalse;
		protected String xPrefix = null;
		
		public void setBoxPlotPairQuality(RBoxPlot<String> plot, String xprefix)
		{
			gr_PairQuality = plot;xPrefix = xprefix;
		}

		/** During the evaluation of the blue pairs, those with zero Statechum scores will be considered for being marked red. */
		protected boolean considerZeroScoringPairsForRed = false;
		
		/** Used to select next red state based on the subjective quality of the subsequent set of red-blue pairs, as determined by the classifier. */
		protected boolean useClassifierToChooseNextRed = false;
		
		/** Used for benchmarking. */
		long timeOfLastTick = 0;
		
		public LearnerThatUsesWekaResults(LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, Classifier wekaClassifier, final LearnerGraph argInitialPTA) 
		{
			super(null, evalCnf,argReferenceGraph,argInitialPTA);classifier=wekaClassifier;
			classTrue=dataCollector.classAttribute.indexOfValue(Boolean.TRUE.toString());classFalse=dataCollector.classAttribute.indexOfValue(Boolean.FALSE.toString());
			timeOfLastTick = System.currentTimeMillis();
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
						int []comparisonResults = dataCollector.comparePairWithOthers(p, pairs), assessmentResults = dataCollector.assessPair(p);
						Instance instance = dataCollector.constructInstance(comparisonResults, assessmentResults, true);
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
		
		static final long maxQuality = 100; 
		long obtainMeasureOfQualityFromDistribution(double distribution[], int classOfInterest)
		{
			assert distribution.length == 2;
			double goodClass = distribution[classOfInterest], badClass = distribution[1-classOfInterest];
			if (badClass < Configuration.fpAccuracy)
				return maxQuality;
			double ratio = goodClass/badClass;
			if (ratio < 1.5)
				return -1;
			return (long)(maxQuality*ratio);
			//return Math.min(maxQuality, (long)(maxQuality*ratio/2));
		}
		
		/** Checks if there are transitions from the supplied pair that are worth merging. */
		public static boolean checkForMerge(PairScore pair, LearnerGraph tentativeGraph)
		{
			Set<Label> labelsOfInterest = null;
			for(Entry<Label,CmpVertex> entry:tentativeGraph.transitionMatrix.get(pair.getQ()).entrySet()) // iterate over the smaller set
				if (entry.getKey().toString().startsWith(prefixOfMandatoryMergeTransition))
				{
					if (labelsOfInterest == null) labelsOfInterest = new TreeSet<Label>();
					labelsOfInterest.add(entry.getKey());
				}
			if (labelsOfInterest == null)
				return false;// nothing of interest found.
			
			for(Entry<Label,CmpVertex> entry:tentativeGraph.transitionMatrix.get(pair.getR()).entrySet())
				if (entry.getKey().toString().startsWith(prefixOfMandatoryMergeTransition) && labelsOfInterest.contains(entry.getKey()))
					return true;
			
			return false;
		}
		
		
		/** Given a collection of labels, identifies states that transitions with those labels lead to. For each label, there will be a set of states that is supposed to be merged. 
		 * It is important to point out that only positive states are taken into account, there are frequent cases where a transition cannot be repeated, hence all transitions with this label will lead to the same state in the dataset,
		 * except for a transition from that very state that is often to be rejected.
		 *  
		 * @param tentativeGraph graph to process
		 * @param transitionsToTheSameState labels that are supposed to lead to the same state
		 * @param transitionsFromTheSameState labels that are supposed to uniquely identify a state
		 * @return a collection of pairs of state that are supposed to be merged.
		 */
		public static List<StatePair> buildVerticesToMerge(LearnerGraph tentativeGraph, Collection<Label> transitionsToTheSameState,Collection<Label> transitionsFromTheSameState)
		{
			List<StatePair> pairsList = new ArrayList<StatePair>();
			if (transitionsToTheSameState.isEmpty() && transitionsFromTheSameState.isEmpty() )
				return pairsList;
			
			Map<Label,Collection<CmpVertex>> labelToStates = 
					tentativeGraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY? new ArrayMapWithSearch<Label,Collection<CmpVertex>>() : new TreeMap<Label,Collection<CmpVertex>>();
			Map<Label,Collection<CmpVertex>> labelFromStates = 
					tentativeGraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY? new ArrayMapWithSearch<Label,Collection<CmpVertex>>() : new TreeMap<Label,Collection<CmpVertex>>();
						
			for(Label lbl:transitionsToTheSameState) labelToStates.put(lbl,new ArrayList<CmpVertex>());
			for(Label lbl:transitionsFromTheSameState) labelFromStates.put(lbl,new ArrayList<CmpVertex>());
			for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:tentativeGraph.transitionMatrix.entrySet())
				if (entry.getKey().isAccept())
					for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
					{
						Collection<CmpVertex> statesToMerge = labelToStates.get(transition.getKey());
						if (statesToMerge != null && transition.getValue().isAccept()) statesToMerge.add(transition.getValue());
	
						Collection<CmpVertex> sourceStatesToMerge = labelFromStates.get(transition.getKey());
						if (sourceStatesToMerge != null && transition.getValue().isAccept()) sourceStatesToMerge.add(entry.getKey());
					}
			
			for(Collection<CmpVertex> vertices:labelToStates.values())
			{
				CmpVertex prevVertex = null;
				for(CmpVertex v:vertices)
				{
					if (prevVertex != null)
						pairsList.add(new StatePair(prevVertex,v));
					prevVertex = v;
				}
			}
			for(Collection<CmpVertex> vertices:labelFromStates.values())
			{
				CmpVertex prevVertex = null;
				for(CmpVertex v:vertices)
				{
					if (prevVertex != null)
						pairsList.add(new StatePair(prevVertex,v));
					prevVertex = v;
				}
			}
			/*
			LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			long score = tentativeGraph.pairscores.computePairCompatibilityScore_general(pairsList, verticesToMerge);
			return score >= 0;// negative if the merge fails*/
			
			return pairsList;
		}
		
		Collection<Label> labelsLeadingToStatesToBeMerged = new LinkedList<Label>(),labelsLeadingFromStatesToBeMerged = new LinkedList<Label>();
		
		public void setLabelsLeadingToStatesToBeMerged(Collection<Label> labels)
		{
			labelsLeadingToStatesToBeMerged = labels;
		}
		
		public void setLabelsLeadingFromStatesToBeMerged(Collection<Label> labels)
		{
			labelsLeadingFromStatesToBeMerged = labels;
		}
		
		/** This method orders the supplied pairs in the order of best to merge to worst to merge. 
		 * We do not simply return the best pair because the next step is to check whether pairs we think are right are classified correctly. 
		 */
		protected ArrayList<PairScore> classifyPairs(Collection<PairScore> pairs, LearnerGraph tentativeGraph)
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
			ArrayList<PairScore> possibleResults = new ArrayList<PairScore>(pairs.size());
			if (allPairsNegative)
				possibleResults.addAll(pairs);
			else
			{// not all pairs contain negative nodes, hence we have to build the sets necessary to evaluate those pairs. Strictly speaking, only those that are used in a tree need to be computed but this 
			 // would make the whole process significantly more complex and hence not done.
				dataCollector.buildSetsForComparators(pairs,tentativeGraph);
	
				for(PairScore p:pairs)
				{
					assert p.getScore() >= 0;
					// By the time we get here, it is assumed that all mandatory merge conditions are satisfied and hence every pair can be merged.
						
					if (!p.getQ().isAccept() || !p.getR().isAccept()) // if any are rejects, add with a score of zero, these will always work because accept-reject pairs will not get here and all rejects can be merged.
						possibleResults.add(new PairScore(p.getQ(), p.getR(), p.getScore(), 0));
/*					else
					if (checkForMerge(p,tentativeGraph))
						possibleResults.add(new PairScore(p.getQ(), p.getR(), p.getScore(), Long.MAX_VALUE));
						*/
					else
					{// meaningful pairs, check with the classifier
						try
						{
							int []comparisonResults = dataCollector.comparePairWithOthers(p, pairs), assessmentResults = dataCollector.assessPair(p);
							Instance instance = dataCollector.constructInstance(comparisonResults, assessmentResults, true);
							double distribution[]=classifier.distributionForInstance(instance);
							long quality = obtainMeasureOfQualityFromDistribution(distribution,classTrue);
							if ( quality >= 0 )// && p.getScore() > 0)
							{
								possibleResults.add(new PairScore(p.getQ(), p.getR(), p.getScore(), quality));
							}
						}
						catch(Exception ex)
						{
							ex.printStackTrace();
							throw new IllegalArgumentException("failed to classify pair "+p, ex);
						}
					}
				}
				
				Collections.sort(possibleResults, new Comparator<PairScore>(){
	
					@Override
					public int compare(PairScore o1, PairScore o2) {
						return sgn( o2.getAnotherScore() - o1.getAnotherScore() );// scores are between 100 and 0, hence it is appropriate to cast to int without a risk of overflow.
					}}); 
				
			}				
			return possibleResults;
		}
		
		/** Where there does not seem to be anything useful to merge, return the pair clearly incorrectly labelled. */
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
			LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			List<StatePair> pairsList = buildVerticesToMerge(tentativeGraph,labelsLeadingToStatesToBeMerged,labelsLeadingFromStatesToBeMerged);
			
			for(PairScore p:pairs)
			{
				if (!pairsList.isEmpty() && tentativeGraph.pairscores.computePairCompatibilityScore_general(p, pairsList, verticesToMerge) < 0)
				// This pair cannot be merged, return as red. Note that this computation is deferred to this stage from computePairScores in order to consider 
				// only a small subset of pairs that are apparently compatible but will be incompatible once the mandatory merge conditions are taken into account.
					return p;
				
				// potentially meaningful pair, check with the classifier
				try
				{
					int []comparisonResults = dataCollector.comparePairWithOthers(p, pairs), assessmentResults = dataCollector.assessPair(p);
					Instance instance = dataCollector.constructInstance(comparisonResults, assessmentResults, true);
					double distribution[]=classifier.distributionForInstance(instance);
					long quality = obtainMeasureOfQualityFromDistribution(distribution,classFalse);
					if ( quality >= 0 )
					{
						if (pairBestToReturnAsRed == null) // || quality >pairBestToReturnAsRed.getAnotherScore())
							pairBestToReturnAsRed = new PairScore(p.getQ(), p.getR(), p.getScore(), quality);// this is the pair to return.
					}
					else
					if (considerZeroScoringPairsForRed && p.getScore() == 0)
					{// blacklisting pairs with zero score
						if (pairBestToReturnAsRed == null || pairBestToReturnAsRed.getAnotherScore() == 0)
							pairBestToReturnAsRed = new PairScore(p.getQ(), p.getR(), p.getScore(), 0);// this is the pair that may be mergeable but the score is zero and we hence blacklist it.
					}
					else
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
		
		/** Reports whether pairs are correctly or wrongly classified. */
		protected void reportPairRight(LearnerGraph tentativeGraph, PairScore firstPair)
		{
			if (gr_PairQuality != null)
			{
				Collection<PairScore> pairs=Arrays.asList(new PairScore[]{firstPair});
				List<PairScore> correctPairs = new ArrayList<PairScore>(1), wrongPairs = new ArrayList<PairScore>(1);
				SplitSetOfPairsIntoRightAndWrong(tentativeGraph, referenceGraph, pairs, correctPairs, wrongPairs);
				
				//boolean firstCorrect = !firstPair.getQ().isAccept() || !firstPair.getR().isAccept() || correctPairs.contains(firstPair);
				long currentMillis = System.currentTimeMillis();
				boolean rejectPair=!firstPair.getQ().isAccept() || !firstPair.getR().isAccept();
				System.out.println( (currentMillis-timeOfLastTick)+" "+gr_PairQuality.size()+" chosen pair"+firstPair+" "+
							(rejectPair? "":
								(correctPairs.contains(firstPair)? "T":"WRONG")));
				timeOfLastTick = currentMillis;
			}
		}

		/** Records scores of pairs classified as "to be merged" by the classifier that are correctly classified and misclassified. */
		protected void updateStatistics(LearnerGraph tentativeGraph, Collection<PairScore> rankedPairs)
		{
			if (!rankedPairs.isEmpty() && gr_PairQuality != null && xPrefix != null)
			{
				List<PairScore> correctPairs = new ArrayList<PairScore>(rankedPairs.size()), wrongPairs = new ArrayList<PairScore>(rankedPairs.size());
				SplitSetOfPairsIntoRightAndWrong(tentativeGraph, referenceGraph, rankedPairs, correctPairs, wrongPairs);

				
				for(PairScore pair:rankedPairs)
				{
					if (pair.getQ().isAccept() && pair.getR().isAccept())
						synchronized(gr_PairQuality)
						{
							if (correctPairs.contains(pair))
								gr_PairQuality.add(xPrefix+",T", (double)pair.getAnotherScore());
							else
								gr_PairQuality.add(xPrefix+",F", (double)pair.getAnotherScore());
						}
				}
			}
		}
		
		/** Obtains states that are currently blue and not marked as tentatively red. 
		 * In other words, given a collection of red states and those earmarked to be possibly red, identifies all the currently blue nodes not included among the red-to-be.
		 * 
		 * @param coregraph graph to consider
		 * @param reds currently red states
		 * @param tentativeRedNodes
		 * @return
		 */
		public static List<CmpVertex> computeBlueStates(final LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes)
		{
			List<CmpVertex> outcome = new LinkedList<CmpVertex>();
			
			for(CmpVertex r:reds)
				for(Entry<Label,CmpVertex> entry:coregraph.transitionMatrix.get(r).entrySet())
					if (entry.getValue().getColour() == JUConstants.BLUE && !tentativeRedNodes.contains(entry.getValue()))
						outcome.add(entry.getValue());
			
			return outcome;
		}
		
		public static interface CollectionOfPairsEstimator
		{
			double obtainEstimateOfTheQualityOfTheCollectionOfPairs(LearnerGraph coregraph, Collection<PairScore> pairs);
		}
		
		/** Used to select the best red node based on what the subsequent collection of pairs will be. */
		public static CmpVertex selectRedNode(LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes, CollectionOfPairsEstimator pairQualityEstimator) 
		{
			CmpVertex redVertex = null;double bestScore=-1;
			Collection<CmpVertex> blueStates = computeBlueStates(coregraph,reds,tentativeRedNodes);
			
			// At this point, pairs is a collection of pairs obtained by evaluating the collection of current blue vertices against all current reds
			
			ArrayList<PairScore> pairs = new ArrayList<PairScore>();
			for(CmpVertex b:blueStates)
				for(CmpVertex r:reds)
				{
					PairScore p=coregraph.pairscores.obtainPair(b,r);
					if (p.getScore() >= 0)
						pairs.add(p);
				}
				
			ArrayList<PairScore> additionalPairs = new ArrayList<PairScore>( pairs.size()+tentativeRedNodes.size() );
			for(CmpVertex v:tentativeRedNodes)
			{
				additionalPairs.clear();additionalPairs.addAll(pairs);
				
				// now compare with the node v considered to become red with all blue nodes 
				for(CmpVertex b:blueStates)
				{
					PairScore p=coregraph.pairscores.obtainPair(b,v);
					if (p.getScore() >= 0)
						additionalPairs.add(p);
				}
				
				for(CmpVertex b:tentativeRedNodes)
					if (b != v)
					{
						for(CmpVertex r:reds)	
						{// if I choose v to be red, all the other tentative reds may remain blue
							PairScore p=coregraph.pairscores.obtainPair(b,r);
							if (p.getScore() >= 0)
								additionalPairs.add(p);
						}
						
						// comparing the considered node v with all the other tentative reds since they may remain blue
						PairScore p=coregraph.pairscores.obtainPair(b,v);
						if (p.getScore() >= 0)
							additionalPairs.add(p);
					}
				// additionalPairs are now a collection of all pairs where existing blue states are compared with all existing and the considered red one
				double estimate = pairQualityEstimator.obtainEstimateOfTheQualityOfTheCollectionOfPairs(coregraph, additionalPairs);
				if (estimate > bestScore)
				{
					bestScore = estimate;redVertex = v;
				}
			}
			
			return redVertex;
		}

		protected class QualityEstimator implements CollectionOfPairsEstimator
		{

			@Override
			public double obtainEstimateOfTheQualityOfTheCollectionOfPairs(LearnerGraph coregraph, Collection<PairScore> pairs) 
			{
				dataCollector.buildSetsForComparators(pairs, coregraph);
				return LearnerThatUsesWekaResults.this.obtainEstimateOfTheQualityOfTheCollectionOfPairs(pairs);
			}
			
		}
		
		CollectionOfPairsEstimator redStateEstimator = new QualityEstimator();
		
		@Override 
		public Stack<PairScore> ChooseStatePairs(final LearnerGraph graph)
		{
			Stack<PairScore> outcome = graph.pairscores.chooseStatePairs(new PairScoreComputation.RedNodeSelectionProcedure(){

				// Here I could use a learner based on metrics of both tentative reds and the perceived quality of the red-blue pairs obtained if I choose any given value.
				// This can be accomplished by doing a clone of the graph and running chooseStatePairs on it with decision procedure that 
				// (a) applies the same rule (of so many) to choose pairs and
				// (b) checks that deadends are flagged. I could iterate this process for a number of decision rules, looking locally for the one that gives best quality of pairs
				// for a particular pairscore decision procedure.
				@Override
				public CmpVertex selectRedNode(LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
				{
					CmpVertex redVertex = null;
					if (useClassifierToChooseNextRed) 
						redVertex = LearnerThatUsesWekaResults.selectRedNode(coregraph, reds, tentativeRedNodes, redStateEstimator);
					else 
						redVertex = tentativeRedNodes.iterator().next();
					
					return redVertex;
				}

				@Override
				public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, @SuppressWarnings("unused") Collection<CmpVertex> reds, Collection<PairScore> pairs) 
				{
					CmpVertex stateToLabelRed = null;
					 PairScore worstPair = getPairToBeLabelledRed(pairs,coregraph);
					
					if (worstPair != null)
						stateToLabelRed = worstPair.getQ();
					
					return stateToLabelRed;// resolution depends on whether Weka has successfully guessed that all pairs are wrong.
					//return LearnerThatUsesWekaResults.this.resolvePotentialDeadEnd(coregraph, reds, pairs);
					
				}});
			if (!outcome.isEmpty())
			{
				ArrayList<PairScore> possibleResults = classifyPairs(outcome,graph);
				updateStatistics(graph,possibleResults);
				if (possibleResults.isEmpty())
					possibleResults.add(pickPairQSMLike(outcome));// no pairs have been provided by the modified algorithm, hence using the default one.
				
				PairScore result = possibleResults.iterator().next();

				//PairScore result= pickPairQSMLike(outcome);
				reportPairRight(graph,result);
				outcome.clear();outcome.push(result);
			}
			return outcome;
		}

	} // uses a classifier in order to rank pairs.
	
	static final int minStateNumber = 5;

	/** The outcome of an experiment using a single FSM and a collection of walks represented as a PTA. */
	public static class SampleData
	{
		public final LearnerGraph referenceGraph, initialPTA;
		public double difference, differenceForReferenceLearner;
		
		public SampleData(LearnerGraph argReferenceGraph, LearnerGraph argInitialPTA)
		{
			referenceGraph = argReferenceGraph;initialPTA = argInitialPTA;
		}
		
		@Override
		public String toString()
		{
			return difference+"";
		}
	}
	
	public static class ThreadResult 
	{
		public List<SampleData> samples = new LinkedList<SampleData>();;
		public Instances instances;
		
		public void addAllInstancesTo(Instances whereToAdd)
		{
			@SuppressWarnings("unchecked")
			Enumeration<Instance> instancesEnum = instances.enumerateInstances();
			while(instancesEnum.hasMoreElements())
				whereToAdd.add(instancesEnum.nextElement());
		}
	}
	
	/** Looks for a labels each of which is only used on transitions entering a specific state.
	 * 
	 * @param graph the graph where to look for such labels.
	 * @return a map from labels to states.
	 */
	public static Map<Label,CmpVertex> uniqueIntoState(LearnerGraph graph)
	{
		Set<Label> deadLabels = new HashSet<Label>();
		
		Map<Label,CmpVertex> labelToState = new TreeMap<Label,CmpVertex>();
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
			for(Entry<Label,CmpVertex> target:entry.getValue().entrySet())
				if (!deadLabels.contains(target.getKey()))
				{
					CmpVertex recordedState = labelToState.get(target.getKey());
					{// the label is not already recorded as leading to multiple different states.
						if (recordedState == null)
							// first time we've seen this label in use
							labelToState.put(target.getKey(),target.getValue());
						else
							if (recordedState != target.getValue())
							{
								// record the label as leading to multiple states
								deadLabels.add(target.getKey());
								labelToState.remove(target.getKey());
							}
					}
				}
		
		return labelToState;
	}
	
	public static Map<Label,CmpVertex> uniqueFromState(LearnerGraph graph)
	{
		Set<Label> deadLabels = new HashSet<Label>();
		
		Map<Label,CmpVertex> labelToState = new TreeMap<Label,CmpVertex>();
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
		{
			CmpVertex state = entry.getKey(); 
			for(Entry<Label,CmpVertex> target:entry.getValue().entrySet())
				if (!deadLabels.contains(target.getKey()))
				{// the label is not already recorded as leading to multiple different states.
					CmpVertex recordedState = labelToState.get(target.getKey());
						if (recordedState == null)
							// first time we've seen this label in use
							labelToState.put(target.getKey(),state);
						else
							if (recordedState != state)
							{
								// record the label as leading to multiple states
								deadLabels.add(target.getKey());
								labelToState.remove(target.getKey());
							}
				}
		}
		
		return labelToState;
	}
	
	/** All label starting from this prefix are going to be merged. */
	public static final String prefixOfMandatoryMergeTransition = "toMerge";
	
	public static void addIfThenForMandatoryMerge(LearnerEvaluationConfiguration initialData, Collection<Label> dataOnUniqueTransitions)
	{
		if (initialData.ifthenSequences == null)
			initialData.ifthenSequences = new LinkedList<String>();
		
		int transitionNumber = 1;
		for(Label l:dataOnUniqueTransitions)
		{
			String lbl = l.toString(), mandatory = prefixOfMandatoryMergeTransition+"_"+transitionNumber+"_"+lbl;
			initialData.ifthenSequences.add("Mandatory_"+transitionNumber+"_via_"+lbl+" A- !"+lbl+" || "+mandatory+" ->A-"+lbl+"->B - "+lbl+" ->B / B- !"+lbl+" || "+mandatory+" ->A / B == THEN == C / C-"+mandatory+"->D");
			++transitionNumber;
		}
	}
	
	public abstract static class LearnerRunner implements Callable<ThreadResult>
	{
		protected final Configuration config;
		protected final int states,sample;
		protected final boolean learnUsingReferenceLearner;
		protected final int seed;
		protected final int numberOfTraces;
		
		public LearnerRunner(int argStates, int argSample, int argSeed, int nrOfTraces, boolean argLearnUsingReferenceLearner, Configuration conf)
		{
			states = argStates;sample = argSample;config = conf;learnUsingReferenceLearner = argLearnUsingReferenceLearner;seed = argSeed;numberOfTraces=nrOfTraces;
		}
		
		public abstract LearnerThatCanClassifyPairs createLearner(LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, WekaDataCollector argDataCollector, final LearnerGraph argInitialPTA);
		
		public static int makeEven(int number)
		{
			if (number % 2 == 0)
				return number;
			return number + 1;
		}
		
		@Override
		public ThreadResult call() throws Exception 
		{
			final int alphabet = states*2;
			Frame frame = null;
			LearnerGraph referenceGraph = null;
			ThreadResult outcome = new ThreadResult();
			WekaDataCollector dataCollector = createDataCollector();
			synchronized(AbstractLearnerGraph.syncObj)
			{
				MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
				referenceGraph = mg.nextMachine(alphabet,seed, config).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
				Assert.assertFalse(WMethod.checkEquivalentStates(referenceGraph));
			}
			
			Map<Label,CmpVertex> uniqueFrom = PairQualityLearner.uniqueFromState(referenceGraph), uniqueInto = PairQualityLearner.uniqueIntoState(referenceGraph);
			if (!uniqueFrom.isEmpty() || uniqueInto.isEmpty())
			{
				synchronized(AbstractLearnerGraph.syncObj)
				{
					System.out.println("FSM with "+referenceGraph.getStateNumber()+"states : unique from: "+uniqueFrom+" and unique to: "+uniqueInto);System.out.flush();
				}
			}
			
			LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);
			final Collection<List<Label>> testSet = referenceGraph.wmethod.computeNewTestSet(1);
			for(int attempt=0;attempt<10;++attempt)
			{// try learning the same machine a few times
				LearnerGraph pta = new LearnerGraph(config);
				RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(attempt),5,null);
				// test sequences will be distributed around 
				final int pathLength = generator.getPathLength();
				final int sequencesPerChunkTypical = makeEven(alphabet*states*numberOfTraces);
				// The total number of elements in test sequences will be distributed around (random(pathLength)+1)*sequencesPerChunkTypical
				// For the purpose of generating long traces, we construct as many traces as there are states but these traces have to be rather long,
				// that is, length of traces will be (random(pathLength)+1)*sequencesPerChunkTypical/states and there will be a total of states traces generated.
				final int tracesToGenerate = makeEven(states);
				final Random rnd = new Random(seed+attempt);
				generator.generateRandomPosNeg(tracesToGenerate, 1, false
					, new RandomLengthGenerator() {
										
						@Override
						public int getLength() {
							return (rnd.nextInt(pathLength)+1)*sequencesPerChunkTypical/tracesToGenerate;
						}
		
						@Override
						public int getPrefixLength(int len) {
							return len;
						}
					});
				/*
				for(List<Label> seq:referenceGraph.wmethod.computeNewTestSet(1))
				{
					pta.paths.augmentPTA(seq, referenceGraph.getVertex(seq) != null, false, null);
				}*/
				//pta.paths.augmentPTA(referenceGraph.wmethod.computeNewTestSet(referenceGraph.getInit(),1));// this one will not set any states as rejects because it uses shouldbereturned
				//referenceGraph.pathroutines.completeGraph(referenceGraph.nextID(false));
				
				pta.paths.augmentPTA(generator.getAllSequences(0));pta.clearColours();
				
				LearnerThatCanClassifyPairs learnerOfPairs = createLearner(learnerEval,referenceGraph,dataCollector,pta);
				LearnerGraph actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
				VertID rejectVertexID = null;
				for(CmpVertex v:actualAutomaton.transitionMatrix.keySet())
					if (!v.isAccept())
					{
						assert rejectVertexID == null : "multiple reject vertices in learnt automaton, such as "+rejectVertexID+" and "+v;
						rejectVertexID = v;break;
					}
				
				actualAutomaton.pathroutines.completeGraphPossiblyUsingExistingVertex(rejectVertexID);// we need to complete the graph, otherwise we are not matching it with the original one that has been completed.
				SampleData dataSample = new SampleData(null,null);
				dataSample.difference = estimationOfDifference(referenceGraph, actualAutomaton, testSet, config, 1);
				if (learnUsingReferenceLearner)
					dataSample.differenceForReferenceLearner = estimationOfDifference(referenceGraph, new ReferenceLearner(frame,learnerEval,referenceGraph,pta).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>()), testSet, config, 1);
				outcome.samples.add(dataSample);
			}
			
			outcome.instances = dataCollector.trainingData;
			return outcome;
		}
	}
	
	/** Given two graphs, estimates the difference between the two. 
	 *
	 * @param refenceGraph reference automaton
	 * @param actualAutomaton the automaton to compare the reference with
	 * @param testSet a test set to use for comparison, useful when language-based measures such as an f-measure are utulised.
	 * @param config configuration to use for doing the comparison. This is useful to configure Linear (if the comparison is done using Linear).
	 * @param cpuNumber the number of processors to use. Usually set to 1 because we run as many experiments as there are CPUs and so individual experiments should not consume more computational power than we have available for them. 
	 */
	@SuppressWarnings("unused")
	public static double estimationOfDifference(LearnerGraph referenceGraph, LearnerGraph actualAutomaton, Collection<List<Label>> testSet, Configuration config, int cpuNumber)
	{
		/*
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> changesCounter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(referenceGraph, actualAutomaton, null);
		gd.computeGD(referenceGraph, actualAutomaton, cpuNumber,changesCounter,config);
		
		int referenceEdges = referenceGraph.pathroutines.countEdges(), actualEdges = actualAutomaton.pathroutines.countEdges();
		//System.out.println(changesCounter.getRemoved()+","+changesCounter.getAdded()+", difference is "+(((double)referenceEdges-changesCounter.getRemoved())/referenceEdges+((double)actualEdges-changesCounter.getAdded())/actualEdges)/2);
		double difference = ( ((double)changesCounter.getRemoved()/referenceEdges)+((double)changesCounter.getAdded()/actualEdges) )/2;
				//(((double)referenceEdges-changesCounter.getRemoved())/referenceEdges+((double)actualEdges-changesCounter.getAdded())/actualEdges)/2;
		boolean eq=WMethod.checkEquivalentStates(actualAutomaton);
		System.out.println(eq);
		Visualiser.updateFrame(referenceGraph, actualAutomaton);
		return difference;*/
		return DiffExperiments.classify(testSet, referenceGraph, actualAutomaton).fMeasure();
	}

	public static void runExperiment() throws Exception
	{
		DrawGraphs gr = new DrawGraphs();
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setAskQuestions(false);config.setDebugMode(false);config.setGdLowToHighRatio(0.7);config.setRandomPathAttemptFudgeThreshold(1000);
		//config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
		final RBoxPlot<String> gr_PairQuality = null;//new RBoxPlot<String>("Correct v.s. wrong","%%",new File("percentage_correctwrong.pdf"));
		final RBoxPlot<String> gr_QualityForNumberOfTraces = new RBoxPlot<String>("traces","%%",new File("quality_traces.pdf"));
		final RBoxPlot<String> gr_ErrorsAndDeadends = null;//new RBoxPlot<String>("Errors and deadends","Red states",new File("errors_deadends.pdf"));
		SquareBagPlot gr_NewToOrig = new SquareBagPlot("orig score","score with learnt selection",new File("new_to_orig.pdf"),0,1,true);
		//gr_NewToOrig.setLimit(7000);
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int ThreadNumber = ExperimentRunner.getCpuNumber();
		
		ExecutorService executorService = Executors.newFixedThreadPool(ThreadNumber);
		final int samplesPerFSM = 2;

		// Stores tasks to complete.
		CompletionService<ThreadResult> runner = new ExecutorCompletionService<ThreadResult>(executorService);

		for(int numberOfTraces=1;numberOfTraces<=4;++numberOfTraces)
		{
			WekaDataCollector dataCollector = createDataCollector();
			List<SampleData> samples = new LinkedList<SampleData>();
			try
			{	
				int numberOfTasks = 0;
				for(int states=minStateNumber;states < minStateNumber+15;states+=5)
					for(int sample=0;sample<samplesPerFSM;++sample)
					{
						runner.submit(new LearnerRunner(states,sample,1+numberOfTasks,numberOfTraces, false,config)
						{
							@Override
							public LearnerThatCanClassifyPairs createLearner(LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph,WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
							{
								return new LearnerThatUpdatesWekaResults(evalCnf,argReferenceGraph,argDataCollector,argInitialPTA,gr_ErrorsAndDeadends);
							}
						});
						++numberOfTasks;
					}
				ProgressIndicator progress = new ProgressIndicator("running "+numberOfTasks+" tasks", numberOfTasks);
				for(int count=0;count < numberOfTasks;++count)
				{
					ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
					samples.addAll(result.samples);
					result.addAllInstancesTo(dataCollector.trainingData);
					if (gr_ErrorsAndDeadends != null)
						synchronized(gr_ErrorsAndDeadends)
						{
							gr_ErrorsAndDeadends.drawInteractive(gr);
						}
					progress.next();
				}
			}
			catch(Exception ex)
			{
				IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
				if (executorService != null) { executorService.shutdown();executorService = null; }
				throw e;
			}
					
			// Run the evaluation
			
			final weka.classifiers.trees.REPTree classifier = new weka.classifiers.trees.REPTree();classifier.setMaxDepth(4);
			classifier.setNoPruning(true);// since we only use the tree as a classifier (as a conservative extension of what is currently done) and do not actually look at it, elimination of pruning is not a problem. 
			// As part of learning, we also prune some of the nodes where the ratio of correctly-classified pairs to those incorrectly classified is comparable.
			// The significant advantage of not pruning is that the result is no longer sensitive to the order of elements in the tree and hence does not depend on the order in which elements have been obtained by concurrent threads.
			
			// final weka.classifiers.trees.J48 classifier = new weka.classifiers.trees.J48();
			classifier.buildClassifier(dataCollector.trainingData);
			System.out.println("Entries in the classifier: "+dataCollector.trainingData.numInstances());
			System.out.println(classifier);
			dataCollector=null;// throw all the training data away.
			final int totalTaskNumber = numberOfTraces;
			try
			{
				int numberOfTasks = 0;
				for(int states=minStateNumber;states < minStateNumber+15;states+=5)
					for(int sample=0;sample<samplesPerFSM;++sample)
					{
						runner.submit(new LearnerRunner(states,sample,totalTaskNumber+numberOfTasks,numberOfTraces, true,config)
						{
							@Override
							public LearnerThatCanClassifyPairs createLearner(LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph,@SuppressWarnings("unused") WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
							{
								LearnerThatUsesWekaResults outcome = new LearnerThatUsesWekaResults(evalCnf,argReferenceGraph,classifier,argInitialPTA);
								if (gr_PairQuality != null)
									outcome.setBoxPlotPairQuality(gr_PairQuality, argReferenceGraph.getStateNumber()+"");
								
								return outcome;
							}
							
						});
						++numberOfTasks;
					}
				ProgressIndicator progress = new ProgressIndicator("evaluating "+numberOfTasks+" tasks", numberOfTasks);
				for(int count=0;count < numberOfTasks;++count)
				{
					ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
					if (gr_PairQuality != null)
						synchronized(gr_PairQuality)
						{
							gr_PairQuality.drawInteractive(gr);
						}
					
					if (gr_NewToOrig != null)
					{
						for(SampleData sample:result.samples)
							gr_NewToOrig.add(sample.differenceForReferenceLearner,sample.difference);
						gr_NewToOrig.drawInteractive(gr);
					}
					
					for(SampleData sample:result.samples)
						if (sample.differenceForReferenceLearner > 0)
							gr_QualityForNumberOfTraces.add(numberOfTraces+"",sample.difference/sample.differenceForReferenceLearner);
					if (gr_QualityForNumberOfTraces.size() > 0)
						gr_QualityForNumberOfTraces.drawInteractive(gr);
					progress.next();
				}
			}
			catch(Exception ex)
			{
				IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
				if (executorService != null) { executorService.shutdown();executorService = null; }
				throw e;
			}
		}
		
		/*
		for(SampleData sample:samples)
		{// try learning the same machine a few times
			
			LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);
			LearnerThatUsesWekaResults evaluator = new LearnerThatUsesWekaResults(null,learnerEval,sample.referenceGraph,classifier,sample.initialPTA,gr_PairQuality,sample.toString());
			LearnerGraph actualAutomaton = evaluator.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
			gr_NewToOrig.add(sample.difference,estimationOfDifference(sample.referenceGraph, actualAutomaton, config, ExperimentRunner.getCpuNumber()));
		}
		 */
		
		if (executorService != null) { executorService.shutdown();executorService = null; }
		
		if (gr_PairQuality != null) gr_PairQuality.drawPdf(gr);if (gr_NewToOrig != null) gr_NewToOrig.drawPdf(gr);if (gr_ErrorsAndDeadends != null) gr_ErrorsAndDeadends.drawPdf(gr);
		if (gr_QualityForNumberOfTraces != null) gr_QualityForNumberOfTraces.drawPdf(gr);
	}
}