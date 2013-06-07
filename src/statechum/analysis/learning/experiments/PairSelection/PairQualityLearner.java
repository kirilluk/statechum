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
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.Stack;

import junit.framework.Assert;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.Label;
import statechum.ProgressIndicator;
import statechum.StatechumXML;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.PairOfPaths;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.PairSelection.WekaDataCollector.PairRank;
import statechum.analysis.learning.experiments.PaperUAS.TracesForSeed.Automaton;
import statechum.analysis.learning.experiments.mutation.DiffExperiments;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.DummyLearner;
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
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceEngine.SequenceSet;
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
		
		assessors.add(classifier.new PairRank("PosNeg pos preferred to Neg")
		{// 7
			@Override
			public long getValue(PairScore p) {
				return  p.getQ().isAccept()?1:-1;
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
		
		classifier.initialise("HindsightExperiment",10000,assessors);
		return classifier;
	}
	
	//int counterPos[]=new int[comparators.length],counterNeg[]=new int[comparators.length];
	
	public static class InitialConfigurationAndData
	{
		public ProgressDecorator.InitialData initial;
		public LearnerEvaluationConfiguration learnerInitConfiguration;
	}
	
	public static InitialConfigurationAndData loadInitialAndPopulateInitialConfiguration(String argPTAFileName, Transform.InternStringLabel converter) throws IOException
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
		final org.w3c.dom.Element nextElement = simulator.expectNextElement(StatechumXML.ELEM_INIT.name());
		outcome.initial = simulator.readInitialData(nextElement);
		inputStream.close();
		return outcome;
	}
	
	/** Makes it possible to run the same learner with different choices of pairs to see how it affects the outcome. It is possible not only to record pair selection but also to replay the one recorded before. */
	public static class RPNIBlueFringeTestVariability
    {
    	private Learner learner;
    	final List<PairOfPaths> listOfPairsToWrite;
    	final Iterator<PairOfPaths> listOfPairsToCheckAgainstIterator;
    	final boolean useOptimizedMerge;
    	LearnerGraph initPta = null;
    	final Configuration config;
    	
    	VertexID phantomVertex = null;
    	
    	public Learner getLearner()
    	{
    		return learner;
    	}
    	
    	/** Constructs an instance of this learner. 
    	 * 
    	 * @param evaluationConfiguration configuration to initialise with. Uses ifthensequences, configuration and converter.
    	 * @param optimisedMerge whether to use a slow ({@link PairScoreComputation#computePairCompatibilityScore_general} or a fast {@link MergeStates#mergeAndDeterminize}. 
    	 * Fast merger is much faster but expects to merge a PTA into a graph; the slow one can merge arbitrary states in a graph.
    	 * @param lw if non-<i>null</i>, stores the list of pairs encountered while learning.
    	 * @param lc if non-<i>null</i>, uses this as a source of pairs to merge. This is used to check that a learner will learn the same automaton when it goes through the same sequences of mergers.
    	 */
		public RPNIBlueFringeTestVariability(final LearnerEvaluationConfiguration evaluationConfiguration, boolean optimisedMerge, final List<PairOfPaths> lw, final List<PairOfPaths> lc) 
		{
			listOfPairsToWrite = lw;useOptimizedMerge = optimisedMerge;
			if (lc != null) listOfPairsToCheckAgainstIterator = lc.iterator();else listOfPairsToCheckAgainstIterator = null;
			final LearnerEvaluationConfiguration bfLearnerInitConfiguration = new LearnerEvaluationConfiguration(evaluationConfiguration.config);
			bfLearnerInitConfiguration.ifthenSequences = evaluationConfiguration.ifthenSequences;
			bfLearnerInitConfiguration.setLabelConverter(evaluationConfiguration.getLabelConverter());
			config = bfLearnerInitConfiguration.config;// our config is a copy of the one supplied as an argument.
			config.setAskQuestions(false); 
			learner = new DummyLearner(new RPNIUniversalLearner(null, bfLearnerInitConfiguration)) 
			{
				
				@Override
				public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
				{
					LearnerGraph outcome = null;
					int extraPhantomVertices = 0;
					if (useOptimizedMerge)
						// Use the old and limited version to compute the merge because the general one is too slow on large graphs and we do not need either to merge arbitrary states or to handle "incompatibles".
						outcome = MergeStates.mergeAndDeterminize(original, pair);
					else
					{
						Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
						long score = original.pairscores.computePairCompatibilityScore_general(pair,mergedVertices);
						outcome = MergeStates.mergeCollectionOfVertices(original,pair.getR(),mergedVertices);
						
						if (score != original.getStateNumber()-outcome.getStateNumber())
						{// This is either a bug somewhere in the merger or (most likely) that the phantomVertex has been removed by the generalised learner. 
						 // We are checking which of these two has happened in the code below.
						 // The computation below is expensive on large graphs but only needs to be done once.
							LinkedHashSet<CmpVertex> removedStates = new LinkedHashSet<CmpVertex>();removedStates.addAll(original.transitionMatrix.keySet());
							removedStates.removeAll(outcome.transitionMatrix.keySet());removedStates.remove(pair.getQ());removedStates.remove(pair.getR());
							Assert.assertEquals(1,removedStates.size());// if it were a phantom vertex, there would only be one of them.
							CmpVertex tentativePhantom = removedStates.iterator().next();
							Set<Label> alphabetUsedOnPhantom = new TreeSet<Label>();alphabetUsedOnPhantom.addAll(original.pathroutines.computeAlphabet());
							for(Entry<Label,CmpVertex> transition:original.transitionMatrix.get(tentativePhantom).entrySet())
							{
								Assert.assertSame(tentativePhantom,transition.getValue());alphabetUsedOnPhantom.remove(transition.getKey());
							}
							Assert.assertEquals(0, alphabetUsedOnPhantom.size());
							extraPhantomVertices = 1;// now certain it was indeed a phantom vertex added when the PTA was initially built.
						}
						
						Assert.assertEquals(score+extraPhantomVertices,original.getStateNumber()-outcome.getStateNumber());
					}
					ScoreMode origScore = original.config.getLearnerScoreMode();original.config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);
					long compatibilityScore = original.pairscores.computePairCompatibilityScore(pair);
					original.config.setLearnerScoreMode(origScore);
					
					Assert.assertEquals(compatibilityScore+1+extraPhantomVertices,original.getStateNumber()-outcome.getStateNumber());
					return outcome;
				}

				@Override 
				public Stack<PairScore> ChooseStatePairs(LearnerGraph graph)
				{
					Stack<PairScore> outcome = graph.pairscores.chooseStatePairs(new PairScoreComputation.RedNodeSelectionProcedure(){

						@Override
						public CmpVertex selectRedNode(LearnerGraph coregraph,
								@SuppressWarnings("unused") Collection<CmpVertex> reds,
								Collection<CmpVertex> tentativeRedNodes) 
						{
							CmpVertex redVertex = null;
							if (listOfPairsToWrite != null)
							{
								redVertex = tentativeRedNodes.iterator().next();
								listOfPairsToWrite.add(new PairOfPaths(coregraph, new PairScore(null, redVertex, 0, 0)));
							}
							
							if(listOfPairsToCheckAgainstIterator != null)
							{
								PairOfPaths pair = listOfPairsToCheckAgainstIterator.next();
								Assert.assertNull(pair.getQ());
								redVertex = coregraph.getVertex(pair.getR());
							}
							return redVertex;
						}

						@SuppressWarnings("unused")
						@Override
						public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<PairScore> pairs) {
							return null;// do not resolve in any way
						}});
					
					if (!outcome.isEmpty())
					{
						if (listOfPairsToWrite != null)
						{
							//System.out.println("Optimized: "+useOptimizedMerge+", matrix: "+graph.config.getTransitionMatrixImplType()+", pair : "+outcome.peek());
							listOfPairsToWrite.add(new PairOfPaths(graph, outcome.peek()));
						}
						
						if(listOfPairsToCheckAgainstIterator != null)
						{
							PairOfPaths pair = listOfPairsToCheckAgainstIterator.next();
							//System.out.println("chosen "+outcome.peek()+", expected "+new PairScore(graph.getVertex(pair.getQ()),graph.getVertex(pair.getR()),0,0));
							pair.rebuildStack(graph, outcome);
						}
					}
					
					return outcome;
				}
			
				@Override 
				public LearnerGraph init(Collection<List<Label>> plus,	Collection<List<Label>> minus) 
				{
					if (initPta != null)
					{
						LearnerGraph graph = decoratedLearner.init(plus,minus);
						LearnerGraph.copyGraphs(initPta, graph);
						return initPta;
					}
					throw new IllegalArgumentException("should not be called");
				}
				
				@Override 
				public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
				{
					LearnerGraph graph = decoratedLearner.init(engine,plusSize,minusSize);

					if (initPta != null)
					{
						LearnerGraph.copyGraphs(initPta, graph);
					}
					else
					{
						Set<Label> alphabet = graph.pathroutines.computeAlphabet();
						// Create a state to ensure that the entire alphabet is visible when if-then automata are loaded.
						phantomVertex = graph.nextID(true);
						CmpVertex dummyState = AbstractLearnerGraph.generateNewCmpVertex(phantomVertex, bfLearnerInitConfiguration.config);
						Map<Label,CmpVertex> row = graph.createNewRow();
						for(Label lbl:alphabet) graph.addTransition(row, lbl, dummyState);
						graph.transitionMatrix.put(dummyState, row);
					}
					return graph;
				}
			};
	
		}
		
		/** After this method is called, the learner used above no longer looks at the PTA is it given but assumes that the PTA supplied to this call should be returned as a result of initialisation.
		 * This is used to get around the problem of  
		 * @param initPTAArg
		 */
		public void setInitPta(LearnerGraph initPTAArg)
		{
			initPta = initPTAArg;
		}
		
		/** Learns starting with the supplied PTA. */
		public LearnerGraph learn(LearnerGraph initPTAArg)
		{
			setInitPta(initPTAArg);
			LearnerGraph outcome = learner.learnMachine(new LinkedList<List<Label>>(), new LinkedList<List<Label>>());
			if (phantomVertex != null) 
				outcome.transitionMatrix.remove(outcome.findVertex(phantomVertex));
			return outcome;
		}
		
		public LearnerGraph learn(PTASequenceEngine engineArg, boolean useNegatives)
		{
			PTASequenceEngine engine = null;
			if (!useNegatives)
			{
				PTASequenceEngine positives = new PTASequenceEngine();positives.init(new Automaton());
    			SequenceSet initSeq = positives.new SequenceSet();initSeq.setIdentity();
    			initSeq.cross(engineArg.getData());
    			engine = positives;
			}
			else
				engine = engineArg;
			
			LearnerGraph outcome = learner.learnMachine(engine,0,0);
			if (phantomVertex != null) 
				outcome.transitionMatrix.remove(outcome.findVertex(phantomVertex));
			return outcome;
		}
		
    }

	/** This learner runs an experiment that attempts to determine the best strategy for selection of pairs based on scores and other parameters and subsequently evaluates it. */
	public static void main(String args[]) throws Exception
	{
		/*
		Transform.InternStringLabel converter = new Transform.InternStringLabel();
		final InitialConfigurationAndData initialConfigAndData = loadInitialAndPopulateInitialConfiguration(PairQualityLearner.largePTAFileName, converter);
		String outcomeName = PairQualityLearner.largePTALogsDir+"outcome_correct.xml";
		final LearnerGraph referenceA = new LearnerGraph(initialConfigAndData.learnerInitConfiguration.config);AbstractPersistence.loadGraph(outcomeName, referenceA, converter);
		//runWekaOnPairSelection(referenceA,initialConfigAndData.initial.graph,initialConfigAndData.learnerInitConfiguration);
		//Assert.assertNull(WMethod.checkM(learntMachine, referenceGraph));
		*/
		try
		{
			runExperiment();
		}
		catch(Exception ex)
		{
			ex.printStackTrace();
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
			List<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
					
			SplitSetOfPairsIntoRightAndWrong(tentativeGraph, referenceGraph, pairs, correctPairs, wrongPairs);
			
			// without sorting the pairs, the learner finds itself in a situation with no valid pairs to choose from.
			LinkedList<PairScore> sortedPairs = new LinkedList<PairScore>(pairs);
			Comparator<PairScore> PairComparator = new Comparator<PairScore>(){

				@Override
				// The first element is the one where o2 is greater than o1, i.e. comparison below returns negative.
				public int compare(PairScore o1, PairScore o2) {
					// if o1 is negative and o2 is positive, the outcome is negative.
					//int outcome = sgn( (o2.getQ().isAccept()?1:-1) -  (o1.getQ().isAccept()?1:-1));
					//if (outcome == 0)
					int outcome = sgn( o2.getScore() - o1.getScore() );
					return outcome;
					
				}};
				
			Collections.sort(sortedPairs, PairComparator);Collections.sort(correctPairs,PairComparator);
			/*
			PairScore firstPair = sortedPairs.iterator().next();
			if (!correctPairs.contains(firstPair))
			{
				System.out.println(sortedPairs);
				System.out.println("the first pair ("+firstPair+") is not the right one ");
				allMergersCorrect = false;
			}
			 */
			/*
			System.out.println("first correct pair is nr "+firstCorrectPair+", wrong pairs: "+wrongPairs.size());
			Iterator<PairScore> pairIter = sortedPairs.iterator();
			System.out.print("# ");
			for(int i=0;i<firstCorrectPair;++i)
				System.out.print(pairIter.next());
			System.out.println();*/
			if (correctPairs.isEmpty())
			{
				allMergersCorrect = false;
				return wrongPairs.iterator().next();
				//throw new IllegalArgumentException("no correct pairs found");
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
			LinkedList<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
			SplitSetOfPairsIntoRightAndWrong(coregraph, referenceGraph, pairs, correctPairs, wrongPairs);
			if (correctPairs.isEmpty())
				stateToMarkRed = wrongPairs.peek().getQ();// no correct pairs found to merge, return the first wrong pair so the corresponding state is marked as red.
			
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
		
		/** Builds a decision tree and returns it. Throws an exception if something goes wrong. */
		public Classifier getClassifier() throws Exception
		{
			weka.classifiers.trees.J48 cl = new weka.classifiers.trees.J48();
			cl.buildClassifier(dataCollector.trainingData);return cl;
		}
				
		public LearnerThatUpdatesWekaResults(Frame parent, LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, WekaDataCollector argDataCollector, final LearnerGraph argInitialPTA) 
		{
			super(parent, evalCnf,argReferenceGraph, argInitialPTA);dataCollector = argDataCollector;
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
					return LearnerThatUpdatesWekaResults.this.resolvePotentialDeadEnd(coregraph, reds, pairs);
					//return null;// for no resolution
				}});
			if (!outcome.isEmpty())
			{
				dataCollector.updateDatasetWithPairs(outcome, graph, referenceGraph);
				PairScore chosenPair = pickCorrectPair(outcome, graph);
				//PairScore chosenPair = pickPairQSMLike(outcome);
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
		final RBoxPlot<String> gr_PairQuality;
		final int classTrue,classFalse;
		final String xPrefix;
		
		public LearnerThatUsesWekaResults(Frame parent, LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, Classifier wekaClassifier, final LearnerGraph argInitialPTA, RBoxPlot<String> argPairQuality, String argXprefix) 
		{
			super(parent, evalCnf,argReferenceGraph,argInitialPTA);classifier=wekaClassifier;gr_PairQuality=argPairQuality;xPrefix=argXprefix;
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
						int []comparisonResults = dataCollector.comparePairWithOthers(p, pairs), assessmentResults = dataCollector.assessPair(p);
						Instance instance = dataCollector.constructInstance(comparisonResults, assessmentResults, true);
						if ( classTrue == (int) classifier.classifyInstance(instance))
						{
							double distribution[]=classifier.distributionForInstance(instance);
							sum+=distribution[classTrue];
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
		
		/** This method orders the supplied pairs in the order of best to merge to worst to merge. */
		protected ArrayList<PairScore> classifyPairs(Collection<PairScore> pairs, LearnerGraph tentativeGraph)
		{
			dataCollector.buildSetsForComparators(pairs,tentativeGraph);

			ArrayList<PairScore> possibleResults = new ArrayList<PairScore>(pairs.size());
			for(PairScore p:pairs)
			{
				assert p.getScore() >= 0;
				
				if (!p.getQ().isAccept() || !p.getR().isAccept()) // if any are rejects, add with a score of zero, these will always work because accept-reject pairs will not get here and all rejects can be merged.
					possibleResults.add(new PairScore(p.getQ(), p.getR(), p.getScore(), 0));
				else
				{// meaningful pairs, check with the classifier
					try
					{
						int []comparisonResults = dataCollector.comparePairWithOthers(p, pairs), assessmentResults = dataCollector.assessPair(p);
						Instance instance = dataCollector.constructInstance(comparisonResults, assessmentResults, true);
						if ( classTrue == (int) classifier.classifyInstance(instance))
						{
							double distribution[]=classifier.distributionForInstance(instance);
							assert distribution.length == 2;
							possibleResults.add(new PairScore(p.getQ(), p.getR(), p.getScore(), (long)(100*distribution[classTrue])));
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
			
			
			return possibleResults;
		}
		
		/** Where there does not seem to be anything useful to merge, return the pair clearly incorrectly labelled. */
		protected PairScore getPairToBeLabelledRed(Collection<PairScore> pairs, LearnerGraph tentativeGraph)
		{
			dataCollector.buildSetsForComparators(pairs,tentativeGraph);
			ArrayList<PairScore> possibleResults = new ArrayList<PairScore>(pairs.size());

			for(PairScore p:pairs)
			{
				assert p.getScore() >= 0;
				
				if (!p.getQ().isAccept() || !p.getR().isAccept()) // if any are rejects, add with a score of zero, these will always work because accept-reject pairs will not get here and all rejects can be merged.
					return null;// negatives can always be merged.
				
				// meaningful pairs, check with the classifier
				try
				{
					int []comparisonResults = dataCollector.comparePairWithOthers(p, pairs), assessmentResults = dataCollector.assessPair(p);
					Instance instance = dataCollector.constructInstance(comparisonResults, assessmentResults, true);
					if ( classFalse == (int) classifier.classifyInstance(instance))
					{
						double distribution[]=classifier.distributionForInstance(instance);
						assert distribution.length == 2;
						possibleResults.add(new PairScore(p.getQ(), p.getR(), p.getScore(), (long)(100*distribution[classTrue])));
					}
				}
				catch(Exception ex)
				{
					ex.printStackTrace();
					throw new IllegalArgumentException("failed to classify pair "+p, ex);
				}
			}
			
			if (possibleResults.size() < pairs.size())
				return null;// at least one pair is plausible.
			
			Collections.sort(possibleResults, new Comparator<PairScore>(){

				@Override
				public int compare(PairScore o1, PairScore o2) {
					return sgn( o2.getAnotherScore() - o1.getAnotherScore() );// scores are between 100 and 0, hence it is appropriate to cast to int without a risk of overflow.
				}}); 
			
			return possibleResults.iterator().next();
		}
		
		protected void updateStatistics(LearnerGraph tentativeGraph, Collection<PairScore> rankedPairs)
		{
			LinkedList<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
			SplitSetOfPairsIntoRightAndWrong(tentativeGraph, referenceGraph, rankedPairs, correctPairs, wrongPairs);
			if (!rankedPairs.isEmpty())
			{
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
		
		/** Obtains states that are currently blue and not marked as tentatively red. */
		public static List<CmpVertex> computeBlueStates(final LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes)
		{
			List<CmpVertex> outcome = new LinkedList<CmpVertex>();
			
			for(CmpVertex r:reds)
				for(Entry<Label,CmpVertex> entry:coregraph.transitionMatrix.get(r).entrySet())
					if (entry.getValue().getColour() == JUConstants.BLUE && !tentativeRedNodes.contains(entry.getValue()))
						outcome.add(entry.getValue());
			
			return outcome;
		}
		
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
					CmpVertex redVertex = null;double bestScore=-1;
					/*
					Collection<CmpVertex> blueStates = computeBlueStates(coregraph,reds,tentativeRedNodes);
					
					// At this point, pairs is a collection of pairs obtained by evaluating the collection of current blue vertices against all current reds
					
					if (blueStates.size() > 0)
					{
						ArrayList<PairScore> pairs = new ArrayList<PairScore>();
						for(CmpVertex b:blueStates)
							for(CmpVertex r:reds)
							{
								PairScore p=graph.pairscores.obtainPair(b,r);
								if (p.getScore() >= 0)
									pairs.add(p);
							}
							
						ArrayList<PairScore> additionalPairs = new ArrayList<PairScore>( pairs.size()+blueStates.size() );
						for(CmpVertex v:tentativeRedNodes)
						{
							additionalPairs.clear();additionalPairs.addAll(pairs);
							for(CmpVertex b:blueStates)
							{
								// now compare with the node considered to become red
								PairScore p=graph.pairscores.obtainPair(b,v);
								if (p.getScore() >= 0)
									additionalPairs.add(p);
							}
						
							// additionalPairs are now a collection of all pairs where existing blue states are compared with all existing and the considered red one
							dataCollector.buildSetsForComparators(additionalPairs, coregraph);
							double estimate = obtainEstimateOfTheQualityOfTheCollectionOfPairs(additionalPairs);
							if (estimate > bestScore)
							{
								bestScore = estimate;redVertex = v;
							}
						}
					}
					else
					 */
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
					return stateToLabelRed;// resolution depends on whether Weka successfull guessed that all pairs are wrong.
					//return LearnerThatUsesWekaResults.this.resolvePotentialDeadEnd(coregraph, reds, pairs);
					
				}});
			if (!outcome.isEmpty())
			{
				ArrayList<PairScore> possibleResults = classifyPairs(outcome,graph);
				updateStatistics(graph,possibleResults);
				if (possibleResults.isEmpty())
					possibleResults.add(pickPairQSMLike(outcome));// no pairs have been provided by the modified algorithm, hence using the default one.
				
				//PairScore result= pickPairQSMLike(outcome);
				PairScore result = possibleResults.iterator().next();
				outcome.clear();outcome.push(result);
			}
			return outcome;
		}

	} // uses a classifier in order to rank pairs.
	
	static final int minStateNumber = 16;

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
			return referenceGraph.getStateNumber()+"";
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
	
	/** Looks for a label that is only used on transitions entering the initial state.
	 * 
	 * @param graph the graph where to look for such labels.
	 * @return one of such labels or null if no such label exists.
	 */
	public static Label uniqueIntoInitial(LearnerGraph graph)
	{
		Map<Label,Boolean> counterOfIncoming = new TreeMap<Label,Boolean>();
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
			for(Entry<Label,CmpVertex> target:entry.getValue().entrySet())
			{
				Boolean value = counterOfIncoming.get(target.getKey());
				if (target.getValue() == graph.getInit())
				{// the target state is our initial one
					if (value == null)
						value = new Boolean(true);// if value is not null, it is either true (everything is fine) or false (found a transition with this label to a non-Init state). In either case, value has to remain unchanged.
				}
				else
					value = new Boolean(false);

				counterOfIncoming.put(target.getKey(),value);
			}
		
		Label lbl = null;
		for(Entry<Label,Boolean> entry:counterOfIncoming.entrySet())
			if (entry.getValue().booleanValue())
			{
				lbl = entry.getKey();break;
			}
		return lbl;
	}
	
	public abstract static class LearnerRunner implements Callable<ThreadResult>
	{
		protected final Configuration config;
		protected final int states,sample;
		protected final boolean learnUsingReferenceLearner;
		protected final int seed;
		
		public LearnerRunner(int argStates, int argSample, int argSeed,boolean argLearnUsingReferenceLearner, Configuration conf)
		{
			states = argStates;sample = argSample;config = conf;learnUsingReferenceLearner = argLearnUsingReferenceLearner;seed = argSeed;
		}
		
		public abstract LearnerThatCanClassifyPairs createLearner(Frame parent, LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, WekaDataCollector argDataCollector, final LearnerGraph argInitialPTA);
		
		@Override
		public ThreadResult call() throws Exception 
		{
			int alphabet = states/2;
			Frame frame = null;
			MachineGenerator mg = new MachineGenerator(states, 40 , (int)Math.round((double)states/5));
			LearnerGraph referenceGraph = mg.nextMachine(alphabet,seed, config).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, because we assume that undefined transitions lead to reject states. 
			Assert.assertFalse(WMethod.checkEquivalentStates(referenceGraph));
			
			LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);
			WekaDataCollector dataCollector = createDataCollector();
			ThreadResult outcome = new ThreadResult();
			for(int attempt=0;attempt<10;++attempt)
			{// try learning the same machine a few times
				LearnerGraph pta = new LearnerGraph(config);
				RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(attempt),5,null);
				int sequencesPerChunk= alphabet*states*states*4/minStateNumber;
				if (sequencesPerChunk % 2 > 0) ++sequencesPerChunk;
				
				generator.generateRandomPosNeg(sequencesPerChunk, 1, false);
					/*, new RandomLengthGenerator() {
										
						@Override
						public int getLength() {
							return 20;
						}
		
						@Override
						public int getPrefixLength(int len) {
							return len;
						}
					});*/
				/*
				for(List<Label> seq:referenceGraph.wmethod.computeNewTestSet(1))
				{
					pta.paths.augmentPTA(seq, referenceGraph.getVertex(seq) != null, false, null);
				}*/
				//pta.paths.augmentPTA(referenceGraph.wmethod.computeNewTestSet(referenceGraph.getInit(),1));// this one will not set any states as rejects because it uses shouldbereturned
				//referenceGraph.pathroutines.completeGraph(referenceGraph.nextID(false));
				pta.paths.augmentPTA(generator.getAllSequences(0));pta.clearColours();
				LearnerThatCanClassifyPairs learnerOfPairs = createLearner(frame,learnerEval,referenceGraph,dataCollector,pta);
				//LearnerThatUpdatesWekaResults learnerOfPairs = new LearnerThatUpdatesWekaResults(frame,learnerEval,referenceGraph,dataCollector,pta);
				LearnerGraph actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
				VertID rejectVertexID = null;
				for(CmpVertex v:actualAutomaton.transitionMatrix.keySet())
					if (!v.isAccept())
					{
						assert rejectVertexID == null : "multiple reject vertices in learnt automaton, such as "+rejectVertexID+" and "+v;
						rejectVertexID = v;break;
					}
				
				actualAutomaton.pathroutines.completeGraphPossiblyUsingExistingVertex(rejectVertexID);// we need to complete the graph, otherwise we are not matching it with the original one that has been completed.
				//while(!learnerOfPairs.checkAllMergersCorrect()); 
				SampleData dataSample = new SampleData(null,null);//referenceGraph,pta);
				dataSample.difference = estimationOfDifference(referenceGraph, actualAutomaton, config, 1);
				if (learnUsingReferenceLearner)
					dataSample.differenceForReferenceLearner = estimationOfDifference(referenceGraph, new ReferenceLearner(frame,learnerEval,referenceGraph,pta).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>()), config, 1);
				outcome.samples.add(dataSample);
			}
			
			outcome.instances = dataCollector.trainingData;
			return outcome;
		}
	}
	
	/** Given two graphs, estimates the difference between the two, using linear. */
	@SuppressWarnings("unused")
	public static double estimationOfDifference(LearnerGraph referenceGraph, LearnerGraph actualAutomaton, Configuration config, int cpuNumber)
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
		return DiffExperiments.classify(referenceGraph.wmethod.computeNewTestSet(1), referenceGraph, actualAutomaton).fMeasure();
	}

	public static void runExperiment() throws Exception
	{
		DrawGraphs gr = new DrawGraphs();
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setAskQuestions(false);config.setGdLowToHighRatio(0.7);
		final RBoxPlot<String> gr_PairQuality = new RBoxPlot<String>("Correct v.s. wrong","%%",new File("percentage_correctwrong.pdf"));
		SquareBagPlot gr_NewToOrig = new SquareBagPlot("orig score","score with learnt selection",new File("new_to_orig.pdf"),0,1,true);
		gr_NewToOrig.setLimit(7000);
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		List<SampleData> samples = new LinkedList<SampleData>();
		final int ThreadNumber = ExperimentRunner.getCpuNumber();
		WekaDataCollector dataCollector = createDataCollector();

		ExecutorService executorService = null;
		try
		{
			executorService = Executors.newFixedThreadPool(ThreadNumber);

			/** Stores tasks to complete. */
			CompletionService<ThreadResult> runner = new ExecutorCompletionService<ThreadResult>(executorService);
			int numberOfTasks = 0;
			for(int states=minStateNumber;states < minStateNumber+15;states+=5)
				for(int sample=0;sample<10;++sample)
				{
					runner.submit(new LearnerRunner(states,sample,1+numberOfTasks,false,config)
					{

						@Override
						public LearnerThatCanClassifyPairs createLearner(Frame parent,LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph,WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
						{
							return new LearnerThatUpdatesWekaResults(parent,evalCnf,argReferenceGraph,argDataCollector,argInitialPTA);
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
				progress.next();
			}
		}
		catch(Exception ex)
		{
			IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
			if (executorService != null) { executorService.shutdown();executorService = null; }
			throw e;
		}
				
		final weka.classifiers.trees.REPTree classifier = new weka.classifiers.trees.REPTree();classifier.setMaxDepth(4);
		// final weka.classifiers.trees.J48 classifier = new weka.classifiers.trees.J48();
		classifier.buildClassifier(dataCollector.trainingData);
		System.out.println("Entries in the classifier: "+dataCollector.trainingData.numInstances());
		System.out.println(classifier);
		dataCollector=null;// throw all the training data away.
		
		try
		{
			// Stores tasks to complete. 
			CompletionService<ThreadResult> runner = new ExecutorCompletionService<ThreadResult>(executorService);
			int numberOfTasks = 0;
			for(int states=minStateNumber;states < minStateNumber+15;states+=5)
				for(int sample=0;sample<10;++sample)
				{
					runner.submit(new LearnerRunner(states,sample,~numberOfTasks,true,config)
					{
						@Override
						public LearnerThatCanClassifyPairs createLearner(Frame parent,LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph,@SuppressWarnings("unused") WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
						{
							return new LearnerThatUsesWekaResults(parent,evalCnf,argReferenceGraph,classifier,argInitialPTA,gr_PairQuality,argReferenceGraph.getStateNumber()+"");
						}
						
					});
					++numberOfTasks;
				}
			ProgressIndicator progress = new ProgressIndicator("evaluating "+numberOfTasks+" tasks", numberOfTasks);
			for(int count=0;count < numberOfTasks;++count)
			{
				ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
				synchronized(gr_PairQuality)
				{
					gr_PairQuality.drawInteractive(gr);
				}
				
				for(SampleData sample:result.samples)
					gr_NewToOrig.add(sample.differenceForReferenceLearner,sample.difference);
				gr_NewToOrig.drawInteractive(gr);

				progress.next();
			}
		}
		catch(Exception ex)
		{
			IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
			if (executorService != null) { executorService.shutdown();executorService = null; }
			throw e;
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
		
		gr_PairQuality.drawPdf(gr);gr_NewToOrig.drawPdf(gr);
	}
	
}
