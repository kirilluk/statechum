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
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Map.Entry;
import java.util.Stack;

import junit.framework.Assert;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Label;
import statechum.StatechumXML;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.PairOfPaths;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PaperUAS;
import statechum.analysis.learning.experiments.PairSelection.WekaPairClassifier.PairComparator;
import statechum.analysis.learning.observers.LearnerSimulator;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.collections.HashMapWithSearch;
import statechum.model.testset.PTASequenceEngine;

/** This one aims to learn how to choose pairs and red states in the way that leads to most accurate learning
 * outcomes.
 * 
 * @author kirill
 */
public class PairQualityLearner {
   	public static final String largePTALogsDir = "resources"+File.separator+"largePTA"+File.separator;
  	public static final String largePTAFileName = largePTALogsDir+"largePTA.zip";
	
   	protected Writer wekaOutput;
   	
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
	
	
	public WekaPairClassifier createPairClassifier()
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
	public PairScore updateMaps(Stack<PairScore> pairs,LearnerGraph graph, LearnerGraph correctResult)
	{
		List<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
				
		SplitSetOfPairsIntoRightAndWrong(graph, correctResult, pairs, correctPairs, wrongPairs);
		
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
	
	public static ProgressDecorator.InitialData loadInitialAndPopulateInitialConfiguration(PaperUAS paper,String argPTAFileName, Transform.InternStringLabel converter) throws IOException
	{// this part is nested in order to ensure that an instance of LearnerSimulator
	 // goes out of scope and is garbage collected as soon as possible. It holds a great deal
	 // of Xerces objects used for recording execution traces that is not used in this test but takes
	 // a lot of memory.
		final java.io.FileInputStream inputStream = new java.io.FileInputStream(argPTAFileName);
		final LearnerSimulator simulator = new LearnerSimulator(inputStream,true,converter);
		Configuration defaultConfig = Configuration.getDefaultConfiguration().copy();
		//defaultConfig.setRejectPositivePairsWithScoresLessThan(1);
		paper.learnerInitConfiguration = simulator.readLearnerConstructionData(defaultConfig);
		paper.learnerInitConfiguration.setLabelConverter(converter);
		final org.w3c.dom.Element nextElement = simulator.expectNextElement(StatechumXML.ELEM_INIT.name());
		ProgressDecorator.InitialData initial = simulator.readInitialData(nextElement);
		inputStream.close();
		return initial;
	}
	
	public static LearnerGraph trimGraph(LearnerGraph coregraph,int depth)
	{
		Map<CmpVertex,LinkedList<Label>> stateToPath = new HashMapWithSearch<CmpVertex,LinkedList<Label>>(coregraph.getStateNumber());
		stateToPath.put(coregraph.getInit(), new LinkedList<Label>());
		LearnerGraph trimmedOne = new LearnerGraph(coregraph.config);
		Queue<CmpVertex> fringe = new LinkedList<CmpVertex>();
		Set<CmpVertex> statesInFringe = new HashSet<CmpVertex>();// in order not to iterate through the list all the time.
		fringe.add(coregraph.getInit());statesInFringe.add(coregraph.getInit());
		while(!fringe.isEmpty())
		{
			CmpVertex currentState = fringe.remove();
			Map<Label,CmpVertex> currentRow = trimmedOne.transitionMatrix.get(currentState);
			LinkedList<Label> currentPath = stateToPath.get(currentState);
			Map<Label,CmpVertex> targets = coregraph.transitionMatrix.get(currentState);
			if(targets != null && !targets.isEmpty())
				for(Entry<Label,CmpVertex> labelstate:targets.entrySet())
					
				for(CmpVertex target:coregraph.getTargets(labelstate.getValue()))
				{
					Map<Label,CmpVertex> row = trimmedOne.transitionMatrix.get(target);
					if (row == null) 
					{
						row = trimmedOne.createNewRow();trimmedOne.transitionMatrix.put(target, row);
					}
					trimmedOne.addTransition(currentRow, labelstate.getKey(), target);
					
					if (!statesInFringe.contains(target))
					{
						@SuppressWarnings("unchecked")
						LinkedList<Label> newPath = (LinkedList<Label>)currentPath.clone();newPath.add(labelstate.getKey());
						stateToPath.put(target,newPath);
						
						if (stateToPath.size() <= depth)
						{
							fringe.offer(target);
							statesInFringe.add(target);
						}
					}
				}
		}
		return trimmedOne;
	}
	
	/** This learner runs an experiment that attempts to determine the best strategy for selection of pairs based on scores and other parameters and subsequently evaluates it. */
	public static void main(String args[]) throws IOException
	{
		Transform.InternStringLabel converter = new Transform.InternStringLabel();
		PaperUAS paper = new PaperUAS();
		final ProgressDecorator.InitialData initial = loadInitialAndPopulateInitialConfiguration(paper, PairQualityLearner.largePTAFileName, converter);
		String outcomeName = PairQualityLearner.largePTALogsDir+"outcome_correct.xml";
		final LearnerGraph referenceA = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph(outcomeName, referenceA, converter);
		final PairQualityLearner qualityLearner = new PairQualityLearner();
		
		LearnerGraph learntMachine = new RPNIUniversalLearner(null, paper.learnerInitConfiguration) 
		{
			
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
					// This can be accomplished by doing a clone of the graph and running chooseStatePairs on it with decision procedure that (a) applies the same rule (of so many) to choose pairs and
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
					{
						CmpVertex stateToMarkRed = null;
						LinkedList<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
						SplitSetOfPairsIntoRightAndWrong(coregraph, referenceA, pairs, correctPairs, wrongPairs);
						if (correctPairs.isEmpty())
						{
							stateToMarkRed = wrongPairs.peek().getQ();
							System.out.println("DEADEND FUDGED: "+wrongPairs);
							
							//Visualiser.updateFrame(trimGraph(coregraph,4),null);Visualiser.waitForKey();
						}
						return stateToMarkRed;
							
					}});
				if (!outcome.isEmpty())
				{
					PairScore correctPair = qualityLearner.updateMaps(outcome, graph, referenceA);
					//System.out.println("pairs : "+outcome+", chosen: "+correctPair);
					outcome.clear();outcome.push(correctPair);
				}
				
				return outcome;
			}
		
			@Override 
			public LearnerGraph init(Collection<List<Label>> plus,	Collection<List<Label>> minus) 
			{
				LearnerGraph graph = super.init(plus,minus);
				LearnerGraph.copyGraphs(initial.graph, graph);
				return initial.graph;
			}
			
			@SuppressWarnings("unused")
			@Override 
			public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
			{
				throw new UnsupportedOperationException();
			}			
		}.learnMachine(new LinkedList<List<Label>>(), new LinkedList<List<Label>>());
		if (qualityLearner.wekaOutput != null)
		{
			qualityLearner.wekaOutput.close();qualityLearner.wekaOutput = null;
		}
		//Visualiser.updateFrame(learntMachine, referenceA);Visualiser.waitForKey();
		Assert.assertNull(WMethod.checkM(learntMachine, referenceA));
		
	}
}
