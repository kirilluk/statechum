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
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
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

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
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
import statechum.analysis.learning.experiments.PaperUAS;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.TrueFalseCounter;
import statechum.analysis.learning.experiments.PairSelection.WekaDataCollector.PairRank;
import statechum.analysis.learning.experiments.mutation.DiffExperiments;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.LearnerSimulator;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph.StatesToConsider;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.apps.QSMTool;
import statechum.collections.ArrayMapWithSearch;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;
import weka.classifiers.Classifier;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.Utils;

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
	
	
	public static WekaDataCollector createDataCollector(final int ifDepth)
	{
		WekaDataCollector classifier = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		
		assessors.add(classifier.new PairRank("conventional score")
		{// 1
			@Override
			public long getValue(PairScore p) {
				return measurementsForCurrentStack(p).compatibilityScore;
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("statechum score")
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
				return  measurementsForCurrentStack(p).nrOfAlternatives;
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
		
		assessors.add(classifier.new PairRank("difference between conventional scores divided by 3")
		{// 11
			@Override
			public long getValue(PairScore p) {
				return  measurementsForCurrentStack(p).compatibilityScore/3;
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("whether red and blue are adjacent")
		{// 12
			@Override
			public long getValue(PairScore p) {
				return measurementsForCurrentStack(p).adjacent? 1:0;
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("number of new outgoing transitions from blue")
		{// 13
			@Override
			public long getValue(PairScore p) {
				Map<Label,CmpVertex> redTransitions = tentativeGraph().transitionMatrix.get(p.getR());
				int counter = 0;
				for(Label lbl:tentativeGraph().transitionMatrix.get(p.getQ()).keySet())
					if (!redTransitions.containsKey(lbl))
						counter++;
				
				return counter;
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		assessors.add(classifier.new PairRank("number of new outgoing transitions from red")
		{// 14
			@Override
			public long getValue(PairScore p) {
				Map<Label,CmpVertex> redTransitions = tentativeGraph().transitionMatrix.get(p.getQ());
				int counter = 0;
				for(Label lbl:tentativeGraph().transitionMatrix.get(p.getR()).keySet())
					if (!redTransitions.containsKey(lbl))
						counter++;
				
				return counter;
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		
		classifier.initialise("HindsightExperiment",100000,assessors,ifDepth);
		return classifier;
	}
	
	public static class InitialConfigurationAndData
	{
		public ProgressDecorator.InitialData initial;
		public LearnerEvaluationConfiguration learnerInitConfiguration;
	}
	
	/** Loads a (potentially large) graph and a small version of it from a supplied file. Much faster than constructing it out of traces. 
	 * Loaded graphs will be using the configuration loaded from a file rather the one that was passed in, hence
	 * they need to be copied if you need then with a different type of transition matrix. 
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
		assert converter != null : "we expect this methods to be used with large graphs and Array matrix types hence converter must be set";
		outcome.learnerInitConfiguration = simulator.readLearnerConstructionData(defaultConfig);
		outcome.learnerInitConfiguration.setLabelConverter(converter);
		
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
		
	
	/** Records scores of pairs that are correctly classified and misclassified. */
	protected static void updateStatistics( Map<Long,TrueFalseCounter> pairQuality, LearnerGraph tentativeGraph, LearnerGraph referenceGraph, Collection<PairScore> pairsToConsider)
	{
		if (!pairsToConsider.isEmpty() && pairQuality != null)
		{
			List<PairScore> correctPairs = new ArrayList<PairScore>(pairsToConsider.size()), wrongPairs = new ArrayList<PairScore>(pairsToConsider.size());
			SplitSetOfPairsIntoRightAndWrong(tentativeGraph, referenceGraph, pairsToConsider, correctPairs, wrongPairs);

			
			for(PairScore pair:pairsToConsider)
			{
				if (pair.getQ().isAccept() && pair.getR().isAccept() && pair.getScore() < 150)
					synchronized(pairQuality)
					{
						TrueFalseCounter counter = pairQuality.get(pair.getScore());
						if (counter == null)
						{
							counter = new TrueFalseCounter();pairQuality.put(pair.getScore(),counter);
						}
						if (correctPairs.contains(pair))
							counter.trueCounter++;
						else
							counter.falseCounter++;
					}
			}
		}
	}
	
	public static void updateGraph(final RBoxPlot<Long> gr_PairQuality, Map<Long,TrueFalseCounter> pairQuality)
	{
		if (gr_PairQuality != null)
		{
			for(Entry<Long,TrueFalseCounter> entry:pairQuality.entrySet())
				gr_PairQuality.add(entry.getKey(), 100*entry.getValue().trueCounter/((double)entry.getValue().trueCounter+entry.getValue().falseCounter));
		}		
	}
	
	
	/** Given a reference graph, identifies pairs of labels that cannot be taken in a sequence. This is subsequently used to construct if-then automata.
	 * 
	 */
	public static Map<Label,Set<Label>> computeInfeasiblePairs(LearnerGraph tentativeGraph)
	{
		Map<Label,Set<Label>> labelToSet = new TreeMap<Label,Set<Label>>();
		Set<Label> alphabet = tentativeGraph.pathroutines.computeAlphabet();
		for(Label lbl:alphabet)
		{
			Set<Label> labels = new TreeSet<Label>();labels.addAll(alphabet);labelToSet.put(lbl,labels);
		}
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:tentativeGraph.transitionMatrix.entrySet())
			for(Entry<Label,CmpVertex> firstTransition:entry.getValue().entrySet())
				labelToSet.get(firstTransition.getKey()).removeAll(tentativeGraph.transitionMatrix.get(firstTransition.getValue()).keySet());
			
		return labelToSet;
	}

	/** This class knows what the reference automaton is and is able to pick correct pairs out of a set to merge. */
	public static abstract class LearnerThatCanClassifyPairs extends RPNIUniversalLearner
	{
		final LearnerGraph initialPTA;
		
		public LearnerThatCanClassifyPairs(LearnerEvaluationConfiguration evalCnf, LearnerGraph reference, LearnerGraph argInitialPTA) 
		{
			super(null, evalCnf);
			referenceGraph = reference;initialPTA = argInitialPTA;
		}

		protected final LearnerGraph referenceGraph;
		protected boolean allMergersCorrect = true;
		
		public boolean checkAllMergersCorrect()
		{
			return allMergersCorrect;
		}
		
		Collection<Label> labelsLeadingToStatesToBeMerged = new LinkedList<Label>(),labelsLeadingFromStatesToBeMerged = new LinkedList<Label>();
		
		public Collection<Label> getLabelsLeadingToStatesToBeMerged()
		{
			return labelsLeadingToStatesToBeMerged;
		}
		
		public void setLabelsLeadingToStatesToBeMerged(Collection<Label> labels)
		{
			labelsLeadingToStatesToBeMerged = labels;
		}
		
		public Collection<Label> getLabelsLeadingFromStatesToBeMerged()
		{
			return labelsLeadingFromStatesToBeMerged;
		}
		
		public void setLabelsLeadingFromStatesToBeMerged(Collection<Label> labels)
		{
			labelsLeadingFromStatesToBeMerged = labels;
		}

		/** Returns one of the correct pairs.
		 */
		public PairScore pickCorrectPair(Collection<PairScore> pairs, LearnerGraph tentativeGraph)
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
				
		
		/** Returns a subset of pairs that are not in contradiction with mandatory merge constraints.
		 *  
		 * @param pairs pairs to merge
		 * @return the outcome of merging.
		 */
		public List<PairScore> filterPairsBasedOnMandatoryMerge(Stack<PairScore> pairs, LearnerGraph tentativeGraph)
		{
			LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			List<StatePair> pairsList = buildVerticesToMerge(tentativeGraph,labelsLeadingToStatesToBeMerged,labelsLeadingFromStatesToBeMerged);
			if (pairsList.isEmpty())
				return pairs;
			
			List<PairScore> outcome = new ArrayList<PairScore>();
			for(PairScore p:pairs)
				if (
						!p.getQ().isAccept() || !p.getR().isAccept() || // if any is a negative, it can always be merged.
						tentativeGraph.pairscores.computePairCompatibilityScore_general(p, pairsList, verticesToMerge) >= 0 // the pair does not contradict mandatory merge.
				)
				outcome.add(p);
			assert !outcome.isEmpty() : "no feasible pairs left for a choice by QSM, this case should have been handled by resolvePotentialDeadEnd";
			return outcome;
		}
		
		/** Returns the best pair according to the ordering associated with individual pairs.
		 */
		public static PairScore pickPairQSMLike(Collection<PairScore> pairs)
		{
			ArrayList<PairScore> pairsSorted = new ArrayList<PairScore>(pairs);
			Collections.sort(pairsSorted, new Comparator<PairScore>(){

				@Override
				public int compare(PairScore o1, PairScore o2) {
					long scoreDiff = o1.getAnotherScore() - o2.getAnotherScore();// using QSM rather than Statechum scoring system
					if (scoreDiff != 0)
						return sgn(scoreDiff);
					
					return o1.compareTo(o2);// other than by score, we sort using vertex IDs
				}});
			return pairsSorted.get(pairsSorted.size()-1);
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
			LearnerGraph outcome = MergeStates.mergeAndDeterminize(original, pair);outcome.pathroutines.updateDepthLabelling();
			return outcome;
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
		public ReferenceLearner(LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, final LearnerGraph argInitialPTA) 
		{
			super(evalCnf,argReferenceGraph, argInitialPTA);
		}
		
		protected Map<Long,TrueFalseCounter> pairQuality;
		
		public void setPairQualityCounter(Map<Long,TrueFalseCounter> argCounter)
		{
			pairQuality = argCounter;
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
				updateStatistics(pairQuality, graph,referenceGraph, outcome);
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
			super(evalCnf,argReferenceGraph, argInitialPTA);dataCollector = argDataCollector;gr_ErrorsAndDeadends = argErrorsAndDeadends;
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
				dataCollector.updateDatasetWithPairs(outcome, graph, referenceGraph);// we learn from the whole range of pairs, not just the filtered ones
				List<PairScore> filteredPairs = filterPairsBasedOnMandatoryMerge(outcome,graph);
				PairScore chosenPair = pickCorrectPair(filteredPairs, graph);
				
				PairScore qsmPair = pickPairQSMLike(outcome);
				if (!qsmPair.equals(chosenPair))
				{
					if(gr_ErrorsAndDeadends != null)
						synchronized(gr_ErrorsAndDeadends)
						{
							gr_ErrorsAndDeadends.add("E", (double)graph.getRedStateNumber());
						}
					
					//int []comparisonResults = dataCollector.comparePairWithOthers(qsmPair, filteredPairs);
					//System.out.println(p+" "+Arrays.toString(comparisonResults));
					//System.out.println("QSM pair: "+qsmPair+" is not the best choice out of \n"+outcome+"\n vector is: "+dataCollector.constructInstance(comparisonResults, dataCollector.assessPair(qsmPair), true));
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
		final WekaDataCollector dataCollector;
		protected Map<Long,TrueFalseCounter> pairQuality = null;
		final int classTrue,classFalse;
		
		public void setPairQualityCounter(Map<Long,TrueFalseCounter> argCounter)
		{
			pairQuality = argCounter;
		}

		double threshold = 1.5;
		
		public void setThreshold(double value)
		{
			threshold = value;
		}
		
		/** During the evaluation of the blue pairs, those with zero Statechum scores will be considered for being marked red. */
		protected boolean considerZeroScoringPairsForRed = false;
		
		/** Used to select next red state based on the subjective quality of the subsequent set of red-blue pairs, as determined by the classifier. */
		protected boolean useClassifierToChooseNextRed = false;
		
		public void configureLearner(boolean classifierForRed, boolean zeroScoringAsRed)
		{
			considerZeroScoringPairsForRed = zeroScoringAsRed;useClassifierToChooseNextRed = classifierForRed;
		}
		
		public LearnerThatUsesWekaResults(int ifDepth,LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, Classifier wekaClassifier, final LearnerGraph argInitialPTA) 
		{
			super(evalCnf,argReferenceGraph,argInitialPTA);
			dataCollector = createDataCollector(ifDepth);
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
		
		static final long maxQuality = 100; 
		long obtainMeasureOfQualityFromDistribution(double distribution[], int classOfInterest)
		{
			assert distribution.length == 2;
			double goodClass = distribution[classOfInterest], badClass = distribution[1-classOfInterest];
			if (badClass < Configuration.fpAccuracy)
				return maxQuality;
			double ratio = goodClass/badClass;
			if (ratio < threshold)
				return -1;
			return (long)(maxQuality*goodClass);
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
		
		
		/** This method orders the supplied pairs in the order of best to merge to worst to merge. 
		 * We do not simply return the best pair because the next step is to check whether pairs we think are right are classified correctly.
		 * <p/> 
		 * Pairs are supposed to be the ones from {@link LearnerThatCanClassifyPairs#filterPairsBasedOnMandatoryMerge(Stack, LearnerGraph)} where all those not matching mandatory merge conditions are not included.
		 * Inclusion of such pairs will not affect the result but it would be pointless to consider such pairs.
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
			ArrayList<PairScore> possibleResults = new ArrayList<PairScore>(pairs.size()),nonNegPairs = new ArrayList<PairScore>(pairs.size());
			if (allPairsNegative)
				possibleResults.addAll(pairs);
			else
			{// not all pairs contain negative nodes, hence we have to build the sets necessary to evaluate those pairs. Strictly speaking, only those that are used in a tree need to be computed but this 
			 // would make the whole process significantly more complex and hence not done.

				// By the time we get here, it is assumed that all mandatory merge conditions are satisfied and hence every pair can be merged.
				dataCollector.buildSetsForComparatorsThatDoNotDependOnFiltering(pairs,tentativeGraph);
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
					try
					{
						int []comparisonResults = new int[dataCollector.getInstanceLength()];
						dataCollector.fillInPairDetails(comparisonResults,p, nonNegPairs);
						Instance instance = dataCollector.constructInstance(comparisonResults, true);
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
					
				Collections.sort(possibleResults, new Comparator<PairScore>(){
	
					@Override
					public int compare(PairScore o1, PairScore o2) {
						int outcome = sgn( o2.getAnotherScore() - o1.getAnotherScore() );// scores are between 100 and 0, hence it is appropriate to cast to int without a risk of overflow.
						if (outcome != 0)
							return outcome;
						return o2.compareTo(o1);
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
			
			dataCollector.buildSetsForComparatorsThatDoNotDependOnFiltering(pairs,tentativeGraph);

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
					int []comparisonResults = new int[dataCollector.getInstanceLength()];
					dataCollector.fillInPairDetails(comparisonResults,p, pairs);
					Instance instance = dataCollector.constructInstance(comparisonResults, true);
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
		
		public static class TrueFalseCounter
		{
			public int trueCounter = 0, falseCounter = 0;
		}

		public static interface CollectionOfPairsEstimator
		{
			double obtainEstimateOfTheQualityOfTheCollectionOfPairs(LearnerGraph coregraph, Collection<PairScore> pairs);
		}

		/** Used to select the best red node based on what the subsequent collection of pairs will be. */
		public static CmpVertex selectRedNode(LearnerGraph coregraph, @SuppressWarnings("unused") Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes, CollectionOfPairsEstimator pairQualityEstimator) 
		{
			CmpVertex redVertex = null;double bestScore=-1;
			
			// It is not hard to calculate what blue states will directly surround a specific state chosen to become red, however those blue states may in turn immediately become red after evaluation and the same would apply
			// to the newly-discovered red states, so we effectively have to re-implement blue state calculation here. For this reason, it is chosen no to do this but instead clone the state machine (possibly in a trimmed form)
			// and ask it for a list of pairs.
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

		protected class QualityEstimator implements CollectionOfPairsEstimator
		{

			@Override
			public double obtainEstimateOfTheQualityOfTheCollectionOfPairs(LearnerGraph coregraph, Collection<PairScore> pairs) 
			{
				dataCollector.buildSetsForComparatorsThatDoNotDependOnFiltering(pairs, coregraph);
				return LearnerThatUsesWekaResults.this.obtainEstimateOfTheQualityOfTheCollectionOfPairs(pairs);
			}
			
		}
		
		CollectionOfPairsEstimator redStateEstimator = new QualityEstimator();
		
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

					//return null;
					return stateToLabelRed;// resolution depends on whether Weka has successfully guessed that all pairs are wrong.
					//return LearnerThatUsesWekaResults.this.resolvePotentialDeadEnd(coregraph, reds, pairs);
					
				}});
			if (!outcome.isEmpty())
			{
				List<PairScore> filteredPairs = filterPairsBasedOnMandatoryMerge(outcome,graph);
				ArrayList<PairScore> possibleResults = classifyPairs(filteredPairs,graph);
				updateStatistics(pairQuality, graph,referenceGraph, filteredPairs);
				if (possibleResults.isEmpty())
				{
					possibleResults.add(pickPairQSMLike(filteredPairs));// no pairs have been provided by the modified algorithm, hence using the default one.
					//System.out.println("no suitable pair was found");
				}
				else
				{/*
					List<PairScore> correctPairs = new ArrayList<PairScore>(1), wrongPairs = new ArrayList<PairScore>(1);
					SplitSetOfPairsIntoRightAndWrong(graph, referenceGraph, possibleResults, correctPairs, wrongPairs);
					System.out.print("Pairs: ");boolean first=true;
					for(PairScore p:possibleResults)
					{
						if (first) first = false;else System.out.print(", ");
						if (!p.getQ().isAccept() || !p.getR().isAccept())
							System.out.print(p.toString());
						else
							if (correctPairs.contains(p))
								System.out.print(p.toString()+"[T]");
							else
								System.out.print(p.toString()+"[F]");
					}
					System.out.println();
					System.out.println("QSM pair would have been : "+pickPairQSMLike(outcome));*/
				}
				PairScore result = possibleResults.iterator().next();

				//PairScore result= pickPairQSMLike(outcome);
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
		//public Instances instances;
		/*
		public void addAllInstancesTo(Instances whereToAdd)
		{
			@SuppressWarnings("unchecked")
			Enumeration<Instance> instancesEnum = instances.enumerateInstances();
			while(instancesEnum.hasMoreElements())
				whereToAdd.add(instancesEnum.nextElement());
		}*/
	}
	
	/** Looks for a labels each of which is only used on transitions entering a specific state.
	 * <p/>
	 * It also looks for labels that are only present on a single transition in a graph, hence uniquely identifying both their source and their target states.
	 * <p/>
	 * Assumes that all states are reachable from an initial state, otherwise it may decide that some labels are not unique to 
	 * the initial state while in reality they are unique to all states that are reachable from it.
	 * 
	 * @param graph the graph where to look for such labels.
	 * @return a map from labels to pairs of states, where the first element is the state the label of interest is uniqueFrom and the second is the state it is uniqueTo. The first of the two can be null, the second cannot.
	 * <p>Justification: if a label is uniqueTo, it can be used on a number of transitions to the state of interest. Each of such transitions may have a different source state. Where it is uniqueFrom, this is a 
	 * label that is only used on a single transition in a graph, hence it is also uniqueTo. UniqueTo can only be uniqueFrom if it is only used on a single transition.
	 */
	public static Map<Label,Pair<CmpVertex,CmpVertex> > uniqueIntoState(LearnerGraph graph)
	{
		Set<Label> deadLabels = new HashSet<Label>();
		
		Map<Label, Pair<CmpVertex,CmpVertex> > labelToStatePair = new TreeMap<Label,Pair<CmpVertex,CmpVertex>>();
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
			for(Entry<Label,CmpVertex> target:entry.getValue().entrySet())
				if (!deadLabels.contains(target.getKey()))
				{// the label is not already recorded as leading to multiple different states.
					Pair<CmpVertex,CmpVertex> recordedSourceTarget = labelToStatePair.get(target.getKey());
					if (recordedSourceTarget == null)
						// first time we've seen this label in use
						labelToStatePair.put(target.getKey(),new Pair<CmpVertex,CmpVertex>(entry.getKey(),target.getValue()));
					else
						if (recordedSourceTarget.secondElem != target.getValue())
						{
							// record the label as leading to multiple states
							deadLabels.add(target.getKey());
							labelToStatePair.remove(target.getKey());
						}
						else
							// a different state leading to the same target, clear the first element of the pair since the label of interest is not longer uniqueFrom
							labelToStatePair.put(target.getKey(),new Pair<CmpVertex,CmpVertex>(null,recordedSourceTarget.secondElem));
				}
		
		return labelToStatePair;
	}
	
	/** 
	 *  
	 * @param graph graph in which to calculate those labels
	 * @return map from a label to a vertex that can be identified by a transition with a label of interest. 
	 */
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

	/** Finds a label that uniquely identifies the initial state. 
	 */ 
	public static Label uniqueFromInitial(LearnerGraph graph)
	{
		if (graph.getInit() == null)
			return null;
		Set<Label> liveLabels = new HashSet<Label>();liveLabels.addAll(graph.transitionMatrix.get(graph.getInit()).keySet());
		
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
			if (entry.getKey() != graph.getInit()) liveLabels.removeAll(entry.getValue().keySet());
		
		if (liveLabels.isEmpty())
			return null;
		
		return liveLabels.iterator().next();
	}
	
	/** All label starting from this prefix are going to be merged. */
	public static final String prefixOfMandatoryMergeTransition = "toMerge", pairwiseAutomata = "pairwiseConstraints";
	
	public static void addIfThenForMandatoryMerge(LearnerEvaluationConfiguration initialData, Collection<Label> dataOnUniqueTransitions)
	{
		if (initialData.ifthenSequences == null)
			initialData.ifthenSequences = new LinkedList<String>();
		
		int transitionNumber = 1;
		for(Label l:dataOnUniqueTransitions)
		{
			String lbl = l.toString(), mandatory = prefixOfMandatoryMergeTransition+"_"+transitionNumber+"_"+lbl;
			initialData.ifthenSequences.add(QSMTool.cmdIFTHENAUTOMATON + " Mandatory_"+transitionNumber+"_via_"+lbl+" A- !"+lbl+" || "+mandatory+" ->A-"+lbl+"->B - "+lbl+" ->B / B- !"+lbl+" || "+mandatory+" ->A / B == THEN == C / C-"+mandatory+"->D");
			++transitionNumber;
		}
	}
	
	public static void addIfThenForPairwiseConstraints(LearnerEvaluationConfiguration initialData, Map<Label,Set<Label>> pairwiseConstraints)
	{
		if (initialData.ifthenSequences == null)
			initialData.ifthenSequences = new LinkedList<String>();
		
		for(Entry<Label,Set<Label>> entry:pairwiseConstraints.entrySet())
			if (!entry.getValue().isEmpty())
			{
				String lbl = entry.getKey().toString();
				StringBuffer rejects = new StringBuffer();
				for(Label l:entry.getValue())
				{
					rejects.append("/ C -");rejects.append(l);rejects.append("-#D");
				}
				initialData.ifthenSequences.add(QSMTool.cmdIFTHENAUTOMATON + " "+ pairwiseAutomata+"_"+lbl+" A- !"+lbl+" ->A-"+lbl+"->B - "+lbl+" ->B / B- !"+lbl+" ->A "+rejects.toString()+"/ B == THEN == C");
			}
	}
	
	public static void printTraceLeadingTo(LearnerGraph graph, String vert)
	{
		LearnerGraphND ptaInverse = new LearnerGraphND(graph.config);
		AbstractPathRoutines.buildInverse(graph, new StatesToConsider() {
			@Override public boolean stateToConsider(@SuppressWarnings("unused") CmpVertex v) { return true; }}
			, ptaInverse);
		CmpVertex v=ptaInverse.findVertex(vert);
		for(int i=0;i<10;++i)
		{
			Map<Label,List<CmpVertex>> transitions = ptaInverse.transitionMatrix.get(v);
			if (transitions.size() > 1)
			{
				System.out.println(transitions);break;
			}
			List<CmpVertex> next = transitions.entrySet().iterator().next().getValue();
			if (next.size() > 1)
			{
				System.out.println(next);break;
			}
			CmpVertex nextState = next.iterator().next();
			System.out.println(v+"-"+transitions.entrySet().iterator().next().getKey()+"->"+nextState);
			v  = nextState;
		}
		
	}

	/** Whenever a transition is encountered with the supplied label, we replace it with a transition to a new state and record what the original state was. The collection of pairs initial-original is then returned. 
	 * Important: this method modifies the supplied graph because it needs to add vertices. 
	 */
	public static List<CmpVertex> constructPairsToMergeWithOutgoing(LearnerGraph pta, Label uniqueFromInitial)
	{
		List<CmpVertex> sourceStates = new LinkedList<CmpVertex>(), statesOfInterest = new LinkedList<CmpVertex>();
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:pta.transitionMatrix.entrySet())
			if (entry.getKey().isAccept())
				for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
					if (transition.getValue().isAccept() && transition.getKey().equals(uniqueFromInitial))
					{
						sourceStates.add(entry.getKey());
					}

		for(CmpVertex vert:sourceStates)
			if (vert != pta.getInit())
			{
				CmpVertex newSource = AbstractLearnerGraph.generateNewCmpVertex(pta.nextID(true), pta.config);
				Map<Label,CmpVertex> row = pta.createNewRow();
				pta.transitionMatrix.put(newSource,row);row.put(uniqueFromInitial, pta.transitionMatrix.get(vert).get(uniqueFromInitial));
				statesOfInterest.add(newSource);
				
				CmpVertex tailState = AbstractLearnerGraph.generateNewCmpVertex(pta.nextID(true), pta.config);
				Map<Label,CmpVertex> tailRow = pta.createNewRow();
				pta.transitionMatrix.put(tailState,tailRow);pta.transitionMatrix.get(vert).put(uniqueFromInitial,tailState);
			}
			else
				statesOfInterest.add(vert);
		
		return statesOfInterest;
	}
	
	public static LearnerGraph mergeStatesForUnique(LearnerGraph pta, Label unique)
	{
		List<StatePair> pairs = new LinkedList<StatePair>();
		LearnerGraph sourcePta = new LearnerGraph(pta,pta.config);
		List<CmpVertex> whatToMerge = constructPairsToMergeWithOutgoing(sourcePta,unique);
		for(CmpVertex vert:whatToMerge)
			pairs.add(new StatePair(sourcePta.getInit(),vert));
		List<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new ArrayList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		
		if (sourcePta.pairscores.computePairCompatibilityScore_general(null, pairs, verticesToMerge) < 0)
			throw new IllegalArgumentException("failed to merge states corresponding to a unique outgoing transition "+unique);
		LearnerGraph outcome = MergeStates.mergeCollectionOfVertices(sourcePta, null, verticesToMerge);
		outcome.pathroutines.updateDepthLabelling();
		return outcome;
	}
	
	public static int makeEven(int number)
	{
		if (number % 2 == 0)
			return number;
		return number + 1;
	}

	public abstract static class LearnerRunner implements Callable<ThreadResult>
	{
		protected final Configuration config;
		protected final ConvertALabel converter;
		protected final int states,sample;
		protected boolean learnUsingReferenceLearner, onlyUsePositives, pickUniqueFromInitial;
		protected final int seed;
		protected final int traceQuantity;
		protected final WekaDataCollector sampleCollector;
		protected int ifDepth = 0;
		protected String selectionID;

		public void setSelectionID(String value)
		{
			selectionID = value;
		}
		
		/** The length of compound if-then conditions over REL metrics to evaluate. */
		public void setIfdepth(int value)
		{
			ifDepth = value;
		}
		
		/** Whether to compute the value from QSM as a reference learner. */
		public void setEvaluateAlsoUsingReferenceLearner(boolean value)
		{
			learnUsingReferenceLearner = value;
		}
		
		/** Whether to filter the collection of traces such that only positive traces are used. */
		public void setOnlyUsePositives(boolean value)
		{
			onlyUsePositives = value;
		}
		
		/** Where a transition that can be uniquely identifying an initial state be used both for mergers and for building a partly-merged PTA. */
		public void setPickUniqueFromInitial(boolean value)
		{
			pickUniqueFromInitial = value;
		}
		
		public LearnerRunner(WekaDataCollector collector,int argStates, int argSample, int argSeed, int nrOfTraces, Configuration conf, ConvertALabel conv)
		{
			states = argStates;sample = argSample;config = conf;seed = argSeed;traceQuantity=nrOfTraces;converter=conv;sampleCollector = collector;
		}
		
		public abstract LearnerThatCanClassifyPairs createLearner(LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, WekaDataCollector argDataCollector, final LearnerGraph argInitialPTA);
		
		
		@Override
		public ThreadResult call() throws Exception 
		{
			final int alphabet = 2*states;
			LearnerGraph referenceGraph = null;
			ThreadResult outcome = new ThreadResult();
			WekaDataCollector dataCollector = createDataCollector(ifDepth);
			Label uniqueFromInitial = null;
			MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
			do
			{
				referenceGraph = mg.nextMachine(alphabet,seed, config, converter).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
				if (pickUniqueFromInitial)
				{
					Map<Label,CmpVertex> uniques = uniqueFromState(referenceGraph);
					if(!uniques.isEmpty())
					{
						Entry<Label,CmpVertex> entry = uniques.entrySet().iterator().next();
						referenceGraph.setInit(entry.getValue());uniqueFromInitial = entry.getKey();
					}
				}
			}
			while(pickUniqueFromInitial && uniqueFromInitial == null);
			
			LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);learnerEval.setLabelConverter(converter);
			final Collection<List<Label>> testSet = PaperUAS.computeEvaluationSet(referenceGraph);
			
			for(int attempt=0;attempt<3;++attempt)
			{// try learning the same machine a few times
				LearnerGraph pta = new LearnerGraph(config);
				RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(attempt),5,null);
				// test sequences will be distributed around 
				final int pathLength = generator.getPathLength();
				final int sequencesPerChunk = makeEven(alphabet*states*traceQuantity*traceQuantity);// we are only using one chunk here but the name is unchanged.
				// Usually, the total number of elements in test sequences (alphabet*states*traceQuantity) will be distributed around (random(pathLength)+1). The total size of PTA is a product of these two.
				// For the purpose of generating long traces, we construct as many traces as there are states but these traces have to be rather long,
				// that is, length of traces will be (random(pathLength)+1)*sequencesPerChunk/states and the number of traces generated will be the same as the number of states.
				final int tracesToGenerate = makeEven(states*traceQuantity*4);
				final Random rnd = new Random(seed*31+attempt);
				generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
										
						@Override
						public int getLength() {
							return (rnd.nextInt(pathLength)+1)*sequencesPerChunk/tracesToGenerate;
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
				if (onlyUsePositives)
					pta.paths.augmentPTA(generator.getAllSequences(0).filter(new FilterPredicate() {
						@Override
						public boolean shouldBeReturned(Object name) {
							return ((statechum.analysis.learning.rpnicore.RandomPathGenerator.StateName)name).accept;
						}
					}));
				else
					pta.paths.augmentPTA(generator.getAllSequences(0));// the PTA will have very few reject-states because we are generating few sequences and hence there will be few negative sequences.
					// In order to approximate the behaviour of our case study, we need to compute which pairs are not allowed from a reference graph and use those as if-then automata to start the inference.
					
				pta.clearColours();
				
				if (!onlyUsePositives)
				{
					assert pta.getStateNumber() > pta.getAcceptStateNumber() : "graph with only accept states but onlyUsePositives is not set";
					Map<Label,Set<Label>> infeasiblePairs = computeInfeasiblePairs(referenceGraph);
					Map<Label,Set<Label>> subsetOfPairs = new TreeMap<Label,Set<Label>>();
					for(Entry<Label,Set<Label>> entry:infeasiblePairs.entrySet())
					{
						Set<Label> value = new TreeSet<Label>();
						if (!entry.getValue().isEmpty()) 
						{
							Label possibleLabels[]=entry.getValue().toArray(new Label[]{});
							if (possibleLabels.length == 1)
								value.add(possibleLabels[0]);
							else
								value.add(possibleLabels[rnd.nextInt(possibleLabels.length)]);
						}
						subsetOfPairs.put(entry.getKey(),value);
					}
					addIfThenForPairwiseConstraints(learnerEval,subsetOfPairs);
					LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(learnerEval.ifthenSequences, null, referenceGraph, learnerEval.config, learnerEval.getLabelConverter()).toArray(new LearnerGraph[0]);
					learnerEval.config.setUseConstraints(false);// do not use if-then during learning (refer to the explanation above)
					int statesToAdd = 1;// we are adding pairwise constraints hence only one has to be added.
					//System.out.println(new Date().toString()+" Graph loaded: "+pta.getStateNumber()+" states ("+pta.getAcceptStateNumber()+" accept states), adding at most "+ statesToAdd+" if-then states");
					Transform.augmentFromIfThenAutomaton(pta, null, ifthenAutomata, statesToAdd);// we only need  to augment our PTA once (refer to the explanation above).
					//System.out.println(new Date().toString()+" Graph augmented: "+pta.getStateNumber()+" states ("+pta.getAcceptStateNumber()+" accept states)");
				}
				else assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
				
				LearnerThatCanClassifyPairs learnerOfPairs = null;
				LearnerGraph actualAutomaton = null;
				
				if (pickUniqueFromInitial)
				{
					pta = mergeStatesForUnique(pta,uniqueFromInitial);
					learnerOfPairs = createLearner(learnerEval,referenceGraph,dataCollector,pta);
					learnerOfPairs.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueFromInitial}));
					
					synchronized (AbstractLearnerGraph.syncObj) {
						//PaperUAS.computePTASize(selectionID+" attempt: "+attempt+" with unique: ", pta, referenceGraph);
					}

					actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());

					LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					List<StatePair> pairsList = LearnerThatCanClassifyPairs.buildVerticesToMerge(actualAutomaton,learnerOfPairs.getLabelsLeadingToStatesToBeMerged(),learnerOfPairs.getLabelsLeadingFromStatesToBeMerged());
					if (!pairsList.isEmpty())
					{
						int score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
						if (score < 0)
						{
							learnerOfPairs = createLearner(learnerEval,referenceGraph,dataCollector,pta);
							learnerOfPairs.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueFromInitial}));
							actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
							score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
							throw new RuntimeException("last merge in the learning process was not possible");
						}
						actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge);
					}
				}
				else
				{// not merging based on a unique transition from an initial state
					learnerOfPairs = createLearner(learnerEval,referenceGraph,dataCollector,pta);
					synchronized (AbstractLearnerGraph.syncObj) {
						//PaperUAS.computePTASize(selectionID+" attempt: "+attempt+" no unique: ", pta, referenceGraph);
					}
					actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
				}
				
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
				SampleData dataSample = new SampleData(null,null);
				dataSample.difference = estimationOfDifferenceFmeasure(referenceGraph, actualAutomaton, testSet);
				if (learnUsingReferenceLearner)
					dataSample.differenceForReferenceLearner = estimationOfDifferenceFmeasure(referenceGraph, new ReferenceLearner(learnerEval,referenceGraph,pta).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>()), testSet);
				outcome.samples.add(dataSample);
				for(int i=0;i< dataCollector.trainingData.numInstances();++i)
					sampleCollector.trainingData.add(dataCollector.trainingData.instance(i));
				dataCollector.trainingData.delete();
			}
			
			//outcome.instances = dataCollector.trainingData;
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
	public static double estimationOfDifferenceFmeasure(LearnerGraph referenceGraph, LearnerGraph actualAutomaton, Collection<List<Label>> testSet)
	{
       	LearnerGraph learntGraph = new LearnerGraph(actualAutomaton.config);AbstractPathRoutines.removeRejectStates(actualAutomaton,learntGraph);
		return DiffExperiments.classify(testSet, referenceGraph, learntGraph).fMeasure();
	}
	
	public static double estimationOfDifferenceDiffMeasure(LearnerGraph referenceGraph, LearnerGraph actualAutomaton, Configuration config, int cpuNumber)
	{
       	LearnerGraph learntGraph = new LearnerGraph(actualAutomaton.config);AbstractPathRoutines.removeRejectStates(actualAutomaton,learntGraph);
		statechum.analysis.learning.linear.GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new statechum.analysis.learning.linear.GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		statechum.analysis.learning.linear.GD.ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> changesCounter = new statechum.analysis.learning.linear.GD.ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(referenceGraph, learntGraph, null);
		gd.computeGD(referenceGraph, learntGraph, cpuNumber,changesCounter,config);
		
		int referenceEdges = referenceGraph.pathroutines.countEdges(), actualEdges = learntGraph.pathroutines.countEdges();
		return
				(((double)referenceEdges-changesCounter.getRemoved())/referenceEdges+((double)actualEdges-changesCounter.getAdded())/actualEdges)/2;
		
	}

	@SuppressWarnings("null")
	public static void runExperiment() throws Exception
	{
		DrawGraphs gr = new DrawGraphs();
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setAskQuestions(false);config.setDebugMode(false);config.setGdLowToHighRatio(0.7);config.setRandomPathAttemptFudgeThreshold(1000);
		config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
		ConvertALabel converter = new Transform.InternStringLabel();
		final RBoxPlot<String> gr_ErrorsAndDeadends = null;//new RBoxPlot<String>("Errors and deadends","Red states",new File("errors_deadends.pdf"));
		//gr_NewToOrig.setLimit(7000);
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int ThreadNumber = ExperimentRunner.getCpuNumber();
		
		ExecutorService executorService = Executors.newFixedThreadPool(ThreadNumber);
		final int samplesPerFSM = 4;

		// Stores tasks to complete.
		CompletionService<ThreadResult> runner = new ExecutorCompletionService<ThreadResult>(executorService);
		for(final double threshold:new double[]{1,1.2,1.5,3,10})
		for(final int ifDepth:new int []{0,1})
		for(final boolean onlyPositives:new boolean[]{true,false})
			for(final boolean zeroScoringAsRed:new boolean[]{true,false})
			for(final boolean selectingRed:new boolean[]{false})
			for(final boolean useUnique:new boolean[]{true,false})
			{
				String selection = "TRUNK"+"I"+ifDepth+"_"+"T"+threshold+"_"+
						(onlyPositives?"P_":"-")+(selectingRed?"R":"-")+(useUnique?"U":"-")+(zeroScoringAsRed?"Z":"-");
				SquareBagPlot gr_NewToOrig = new SquareBagPlot("orig score","score with learnt selection",new File("new_to_orig"+selection+".pdf"),0,1,true);
				final RBoxPlot<String> gr_QualityForNumberOfTraces = new RBoxPlot<String>("traces","%%",new File("quality_traces"+selection+".pdf"));
		
				for(int traceQuantity=2;traceQuantity<=2;++traceQuantity)
				{
					WekaDataCollector dataCollector = createDataCollector(ifDepth);
					List<SampleData> samples = new LinkedList<SampleData>();
					try
					{
						int numberOfTasks = 0;
						for(int states=minStateNumber;states < minStateNumber+10;states+=2)
							for(int sample=0;sample<samplesPerFSM*2;++sample)
							{
								LearnerRunner learnerRunner = new LearnerRunner(dataCollector,states,sample,1+numberOfTasks,traceQuantity, config, converter)
								{
									@Override
									public LearnerThatCanClassifyPairs createLearner(LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph,WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
									{
										return new LearnerThatUpdatesWekaResults(evalCnf,argReferenceGraph,argDataCollector,argInitialPTA,gr_ErrorsAndDeadends);
									}
								};
								learnerRunner.setPickUniqueFromInitial(useUnique);learnerRunner.setOnlyUsePositives(onlyPositives);learnerRunner.setIfdepth(ifDepth);
								learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
								runner.submit(learnerRunner);
								++numberOfTasks;
							}
						ProgressIndicator progress = new ProgressIndicator("running "+numberOfTasks+" tasks", numberOfTasks);
						for(int count=0;count < numberOfTasks;++count)
						{
							ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
							samples.addAll(result.samples);
							//result.addAllInstancesTo(dataCollector.trainingData);
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

					int nonZeroes = 0;
					for(int attrNum=0;attrNum<dataCollector.attributesOfAnInstance.length;++attrNum)
					{
						assert dataCollector.attributesOfAnInstance[attrNum].index() == attrNum;
						boolean nonZero = false;
						for(int i=0;i<dataCollector.trainingData.numInstances() && !nonZero;++i)
							if (dataCollector.trainingData.instance(i).stringValue(attrNum) != WekaDataCollector.ZERO)
								nonZero = true;
						
						if (nonZero) ++nonZeroes;
					}
					
					System.out.println("Total instances: "+dataCollector.trainingData.numInstances()+" with "+dataCollector.attributesOfAnInstance.length+" attributes, non-zeroes are "+nonZeroes);
					
					// write arff
					FileWriter wekaInstances = null;
					String whereToWrite = "qualityLearner_"+selection+".arff";
					try
					{
						wekaInstances = new FileWriter(whereToWrite);
						// This chunk is almost verbatim from Weka's Instances.toString()
						wekaInstances.append(Instances.ARFF_RELATION).append(" ").append(Utils.quote(dataCollector.trainingData.relationName())).append("\n\n");
					    for (int i = 0; i < dataCollector.trainingData.numAttributes(); i++) {
					    	wekaInstances.append(dataCollector.trainingData.attribute(i).toString()).append("\n");
					    }
					    wekaInstances.append("\n").append(Instances.ARFF_DATA).append("\n");
					    for (int i = 0; i < dataCollector.trainingData.numInstances(); i++) {
					    	wekaInstances.append(dataCollector.trainingData.instance(i).toString());
					        if (i < dataCollector.trainingData.numInstances() - 1) {
					        	wekaInstances.append('\n');
					        }
					      }
					}
					catch(Exception ex)
					{
						Helper.throwUnchecked("failed to create a file with training data for "+whereToWrite, ex);
					}
					finally
					{
						if (wekaInstances != null)
							try {
								wekaInstances.close();
							} catch (IOException e) {
								// ignore this, we are not proceeding anyway due to an earlier exception so whether the file was actually written does not matter
							}
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
					final int totalTaskNumber = traceQuantity;
					final RBoxPlot<Long> gr_PairQuality = new RBoxPlot<Long>("Correct v.s. wrong","%%",new File("percentage_score"+selection+".pdf"));
					final Map<Long,TrueFalseCounter> pairQualityCounter = new TreeMap<Long,TrueFalseCounter>();
					
					try
					{
						int numberOfTasks = 0;
						for(int states=minStateNumber;states < minStateNumber+10;states+=2)
							for(int sample=0;sample<samplesPerFSM;++sample)
							{
								LearnerRunner learnerRunner = new LearnerRunner(dataCollector,states,sample,totalTaskNumber+numberOfTasks,traceQuantity, config, converter)
								{
									@Override
									public LearnerThatCanClassifyPairs createLearner(LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph,@SuppressWarnings("unused") WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
									{
										LearnerThatUsesWekaResults l = new LearnerThatUsesWekaResults(ifDepth,evalCnf,argReferenceGraph,classifier,argInitialPTA);
										if (gr_PairQuality != null)
											l.setPairQualityCounter(pairQualityCounter);
										
										l.configureLearner(selectingRed, zeroScoringAsRed);l.setThreshold(threshold);
										return l;
									}
									
								};
								learnerRunner.setPickUniqueFromInitial(useUnique);learnerRunner.setEvaluateAlsoUsingReferenceLearner(true);learnerRunner.setOnlyUsePositives(onlyPositives);learnerRunner.setIfdepth(ifDepth);
								learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
								runner.submit(learnerRunner);
								++numberOfTasks;
							}
						ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks", numberOfTasks);
						for(int count=0;count < numberOfTasks;++count)
						{
							ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
							if (gr_PairQuality != null)
							{
								updateGraph(gr_PairQuality,pairQualityCounter);
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
									gr_QualityForNumberOfTraces.add(traceQuantity+"",sample.difference/sample.differenceForReferenceLearner);
							if (gr_QualityForNumberOfTraces.size() > 0)
								gr_QualityForNumberOfTraces.drawInteractive(gr);
							progress.next();
						}
						if (gr_PairQuality != null) gr_PairQuality.drawPdf(gr);
					}
					catch(Exception ex)
					{
						IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
						if (executorService != null) { executorService.shutdown();executorService = null; }
						throw e;
					}
				}
				if (gr_NewToOrig != null) gr_NewToOrig.drawPdf(gr);if (gr_ErrorsAndDeadends != null) gr_ErrorsAndDeadends.drawPdf(gr);
				if (gr_QualityForNumberOfTraces != null) gr_QualityForNumberOfTraces.drawPdf(gr);
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
		
	}
}
