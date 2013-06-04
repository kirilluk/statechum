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
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.Stack;

import junit.framework.Assert;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Label;
import statechum.StatechumXML;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.PairOfPaths;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PairSelection.WekaPairClassifier.PairComparator;
import statechum.analysis.learning.experiments.PaperUAS.TracesForSeed.Automaton;
import statechum.analysis.learning.observers.DummyLearner;
import statechum.analysis.learning.observers.LearnerSimulator;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceEngine.SequenceSet;
import weka.classifiers.Classifier;
import weka.core.Instance;

/** This one aims to learn how to choose pairs and red states in the way that leads to most accurate learning
 * outcomes.
 * 
 * @author kirill
 */
public class PairQualityLearner {
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
	
	
	public static WekaPairClassifier createPairClassifier()
	{
		WekaPairClassifier classifier = new WekaPairClassifier();
		List<PairComparator> comps = new ArrayList<PairComparator>(20);
		comps.add(classifier.new PairComparator("conventional score")
		{// 1

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  sgn(measurementsForCurrentStack(o1).compatibilityScore - measurementsForCurrentStack(o2).compatibilityScore);
			}
		});
				
		comps.add(classifier.new PairComparator("statechum score")
		{// 2

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return sgn(o1.getScore() - o2.getScore());
			}
		});
		
		comps.add(classifier.new PairComparator("size of tree rooted at Blue")
		{// 3

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return sgn(treeRootedAt(o1.getQ()) - treeRootedAt(o2.getQ()));
			}
		});
		
		comps.add(classifier.new PairComparator("Number of alternatives with same red the more alt the worse")
		{// 4

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  sgn(measurementsForCurrentStack(o2).nrOfAlternatives - measurementsForCurrentStack(o1).nrOfAlternatives);
			}
		});
		
		comps.add(classifier.new PairComparator("Depth of Blue the deeper the worse")
		{// 5

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  sgn(o2.getQ().getDepth() - o1.getQ().getDepth());
			}
		});
		
		comps.add(classifier.new PairComparator("Depth of Red the deeper the worse")
		{// 6

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  sgn(o2.getR().getDepth() - o1.getR().getDepth());
			}
		});
		
		comps.add(classifier.new PairComparator("PosNeg pos preferred to Neg")
		{// 7

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  sgn( (o1.getQ().isAccept()?1:-1) -  (o2.getQ().isAccept()?1:-1));
			}
		});
		
		comps.add(classifier.new PairComparator("state identifiers Red")
		{// 8

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return sgn( o1.getR().getIntegerID() - o2.getR().getIntegerID());
			}
		});
		
		comps.add(classifier.new PairComparator("state identifiers Blue")
		{// 9

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return sgn( o1.getQ().getIntegerID() - o2.getQ().getIntegerID());
			}
		});
		
		comps.add(classifier.new PairComparator("proximity of the red and blue by depth")
		{// 10

			@Override
			public int compare(PairScore o1, PairScore o2) {
				int prox1 = o1.getQ().getDepth()-o1.getR().getDepth(),
						prox2 = o2.getQ().getDepth()-o2.getR().getDepth();
				
				return sgn(prox1-prox2);
			}
		});
		
		comps.add(classifier.new PairComparator("difference between conventional scores divided by 3")
		{// 11

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  sgn( (measurementsForCurrentStack(o1).compatibilityScore - measurementsForCurrentStack(o2).compatibilityScore)/3);
			}
		});
		
		comps.add(classifier.new PairComparator("whether red and blue are adjacent")
		{// 12

			@Override
			public int compare(PairScore o1, PairScore o2) {
				int o1Adjacent = measurementsForCurrentStack(o1).adjacent? 1:0,  o2Adjacent = measurementsForCurrentStack(o2).adjacent? 1:0;
				return  sgn( o1Adjacent - o2Adjacent );
			}
		});
		
		classifier.initialise("HindsightExperiment",10000,comps);
		return classifier;
	}
	
	//int counterPos[]=new int[comparators.length],counterNeg[]=new int[comparators.length];

	
	/** Updates the statistics on the pairs, using the correct automaton. Returns one of the correct pairs (throws an exception if there is none). 
	 */
	public static PairScore pickCorrectPair(Stack<PairScore> pairs,LearnerGraph tentativeGraph, LearnerGraph correctGraph)
	{
		List<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
				
		SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, pairs, correctPairs, wrongPairs);
		
		if (correctPairs.isEmpty())
			throw new IllegalArgumentException("no correct pairs found");

		// without sorting the pairs, the learner finds itself in a situation with no valid pairs to choose from.
		LinkedList<PairScore> sortedPairs = new LinkedList<PairScore>(pairs);
		Comparator<PairScore> PairComparator = new Comparator<PairScore>(){

			@Override
			// The first element is the one where o2 is greater than o1, i.e. comparison below returns negative.
			public int compare(PairScore o1, PairScore o2) {
				// if o1 is negative and o2 is positive, the outcome is negative.
				int outcome = sgn( (o2.getQ().isAccept()?1:-1) -  (o1.getQ().isAccept()?1:-1));
				if (outcome == 0)
					outcome = sgn( o2.getScore() - o1.getScore() );
				return outcome;
				
			}};
			
		Collections.sort(sortedPairs, PairComparator);Collections.sort(correctPairs,PairComparator);
		if (!correctPairs.contains(sortedPairs.iterator().next()))
		{
			System.out.println(sortedPairs);
			System.out.println("the first pair is not the right one "+sortedPairs.iterator().next());
		}

		/*
		System.out.println("first correct pair is nr "+firstCorrectPair+", wrong pairs: "+wrongPairs.size());
		Iterator<PairScore> pairIter = sortedPairs.iterator();
		System.out.print("# ");
		for(int i=0;i<firstCorrectPair;++i)
			System.out.print(pairIter.next());
		System.out.println();*/
		return correctPairs.iterator().next();
	}
	
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
						public CmpVertex resolveDeadEnd(LearnerGraph coregraph,
								Collection<CmpVertex> reds,
								Collection<PairScore> pairs) {
							return null;
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
	public static void main(String args[]) throws IOException
	{
		Transform.InternStringLabel converter = new Transform.InternStringLabel();
		final InitialConfigurationAndData initialConfigAndData = loadInitialAndPopulateInitialConfiguration(PairQualityLearner.largePTAFileName, converter);
		String outcomeName = PairQualityLearner.largePTALogsDir+"outcome_correct.xml";
		final LearnerGraph referenceA = new LearnerGraph(initialConfigAndData.learnerInitConfiguration.config);AbstractPersistence.loadGraph(outcomeName, referenceA, converter);
		//runWekaOnPairSelection(referenceA,initialConfigAndData.initial.graph,initialConfigAndData.learnerInitConfiguration);
		//Assert.assertNull(WMethod.checkM(learntMachine, referenceGraph));
		
	}
	
	public static class LearnerThatUpdatesWekaResults extends RPNIUniversalLearner
	{
		final WekaPairClassifier classifier = createPairClassifier();
		
		/** Builds a decision tree and returns it. Throws an exception if something goes wrong. */
		public Classifier getClassifier() throws Exception
		{
			weka.classifiers.trees.J48 cl = new weka.classifiers.trees.J48();
			cl.buildClassifier(classifier.trainingData);return cl;
		}
		
		final LearnerGraph referenceGraph, initialPTA;
		
		public LearnerThatUpdatesWekaResults(Frame parent, LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, final LearnerGraph argInitialPTA) 
		{
			super(parent, evalCnf);referenceGraph = argReferenceGraph;initialPTA = argInitialPTA;
		}
		
		@Override
		public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
		{// fast merger
			return MergeStates.mergeAndDeterminize(original, pair);
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
				public CmpVertex resolveDeadEnd(LearnerGraph coregraph,	@SuppressWarnings("unused") Collection<CmpVertex> reds,	Collection<PairScore> pairs) 
				{//XXX I think we should return a wrong pair here if any are available because it is a vertex that should not be merged anywhere
					CmpVertex stateToMarkRed = null;
					LinkedList<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
					SplitSetOfPairsIntoRightAndWrong(coregraph, referenceGraph, pairs, correctPairs, wrongPairs);
					if (correctPairs.isEmpty())
						stateToMarkRed = wrongPairs.peek().getQ();
					
					return stateToMarkRed;
						
				}});
			if (!outcome.isEmpty())
			{
				classifier.updateDatasetWithPairs(outcome, graph, referenceGraph);
				PairScore correctPair = pickCorrectPair(outcome, graph, referenceGraph);
				//System.out.println("pairs : "+outcome+", chosen: "+correctPair);
				outcome.clear();outcome.push(correctPair);
			}
			
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
		
	} // class that builds a classifier tree.
	
	public static class LearnerThatUsesWekaResults extends RPNIUniversalLearner
	{
		final Classifier classifier;
		final WekaPairClassifier weka = createPairClassifier();
		final LearnerGraph initialPTA;
		
		final int classTrue,classFalse;
		
		public LearnerThatUsesWekaResults(Frame parent, LearnerEvaluationConfiguration evalCnf, Classifier wekaClassifier, final LearnerGraph argInitialPTA) 
		{
			super(parent, evalCnf);initialPTA = argInitialPTA;classifier=wekaClassifier;
			classTrue=weka.classAttribute.indexOfValue(Boolean.TRUE.toString());classFalse=weka.classAttribute.indexOfValue(Boolean.FALSE.toString());
		}
		
		@Override
		public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
		{// fast merger
			return MergeStates.mergeAndDeterminize(original, pair);
		}
		
		/** This method orders the supplied pairs in the order of best to merge to worst to merge. */
		protected PairScore [] classifyPairs(Collection<PairScore> pairs)
		{
			PairScore possibleResults[] = new PairScore[pairs.size()];
			int i=0;
			for(PairScore p:pairs)
			{
				assert p.getScore() >= 0;
				
				if (!p.getQ().isAccept() || !p.getR().isAccept()) // if any are rejects, add with a score of zero, these will always work because accept-reject pairs will not get here and all rejects can be merged.
					possibleResults[i++]=new PairScore(p.getQ(), p.getR(), p.getScore(), 0);
				else
				{// meaningful pairs, check with the classifier
					try
					{
						int []comparisonResults = weka.comparePairWithOthers(p, pairs);
						Instance instance = weka.constructInstance(comparisonResults, true);
						if ( classTrue == (int) classifier.classifyInstance(instance))
						{
							double distribution[]=classifier.distributionForInstance(instance);
							assert distribution.length == 2;
							possibleResults[i++]=new PairScore(p.getQ(), p.getR(), p.getScore(), (long)(1000*distribution[classTrue]));
						}
					}
					catch(Exception ex)
					{
						throw new IllegalArgumentException("failed to classify pair "+p, ex);
					}
				}
			}
			Arrays.sort(possibleResults, new Comparator<PairScore>(){

				@Override
				public int compare(PairScore o1, PairScore o2) {
					return (int)(o1.getAnotherScore() - o2.getAnotherScore());// scores are between 1000 and 0, hence it is appropriate to cast to int without a risk of overflow.
				}}); 
			
			return possibleResults;
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
				public CmpVertex resolveDeadEnd(@SuppressWarnings("unused") LearnerGraph coregraph,	@SuppressWarnings("unused") Collection<CmpVertex> reds,	Collection<PairScore> pairs) 
				{
					PairScore possibleResults[] = classifyPairs(pairs);
					return possibleResults[0].getQ();
						
				}});
			if (!outcome.isEmpty())
			{
					PairScore possibleResults[] = classifyPairs(outcome);
					outcome.clear();outcome.push(possibleResults[0]);
			}
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
}
