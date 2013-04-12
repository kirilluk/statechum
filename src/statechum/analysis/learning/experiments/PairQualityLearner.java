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
package statechum.analysis.learning.experiments;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Arrays;
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
import java.util.TreeMap;

import statechum.Configuration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.Label;
import statechum.StatechumXML;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.PairOfPaths;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.observers.DummyLearner;
import statechum.analysis.learning.observers.LearnerSimulator;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.analysis.learning.rpnicore.Transform;
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
							return JUConstants.intUNKNOWN;// encoutered a loop
					}
		}
		return counter;
	}
	
	public static class PairMeasurements
	{
		public int nrOfAlternatives;
		public long compatibilityScore;
	}
	Map<StatePair,PairMeasurements> measurementsForCurrentStack=new TreeMap<StatePair,PairMeasurements>();
	
	Map<CmpVertex,Integer> treeRootedAt = new TreeMap<CmpVertex,Integer>();

	public int sgn(long value)
	{
		if (value>0)
			return 1;
		else
			if (value < 0)
				return -1;
		return 0;
	}
	
	@SuppressWarnings("unchecked")
	public final Comparator<PairScore>[] comparators = new Comparator[]{
		new Comparator<PairScore>()
		{// 1

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  sgn(measurementsForCurrentStack.get(o1).compatibilityScore - measurementsForCurrentStack.get(o2).compatibilityScore);
			}

			@Override
			public String toString()
			{
				return "conventional score";
			}
		},
		
		new Comparator<PairScore>()
		{// 2

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return sgn(o1.getScore() - o2.getScore());
			}

			@Override
			public String toString()
			{
				return "statechum score";
			}
		},
		
		new Comparator<PairScore>()
		{// 3

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return sgn(treeRootedAt.get(o1.getQ()) - treeRootedAt.get(o2.getQ()));
			}

			@Override
			public String toString()
			{
				return "size of tree rooted at Blue";
			}
		},
		
		new Comparator<PairScore>()
		{// 4

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  sgn(measurementsForCurrentStack.get(o2).nrOfAlternatives - measurementsForCurrentStack.get(o1).nrOfAlternatives);
			}

			@Override
			public String toString()
			{
				return "Number of alternatives with same red (the more alt, the worse)";
			}
		},
		
		new Comparator<PairScore>()
		{// 5

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  sgn(o2.getQ().getDepth() - o1.getQ().getDepth());
			}

			@Override
			public String toString()
			{
				return "Depth of Blue, the deeper the worse";
			}
		},
		
		new Comparator<PairScore>()
		{// 6

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  sgn(o2.getR().getDepth() - o1.getR().getDepth());
			}

			@Override
			public String toString()
			{
				return "Depth of Red, the deeper the worse";
			}
		},
		
		new Comparator<PairScore>()
		{// 7

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  sgn( (o1.getQ().isAccept()?1:-1) -  (o2.getQ().isAccept()?1:-1));
			}

			@Override
			public String toString()
			{
				return "Pos/Neg, pos preferred to Neg";
			}
		},
		
		new Comparator<PairScore>()
		{// 8

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return sgn( o1.getR().getIntegerID() - o2.getR().getIntegerID());
			}

			@Override
			public String toString()
			{
				return "state identifiers, Red";
			}
		},
		
		new Comparator<PairScore>()
		{// 9

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return sgn( o1.getQ().getIntegerID() - o2.getQ().getIntegerID());
			}

			@Override
			public String toString()
			{
				return "state identifiers, Blue";
			}
		},
		
		new Comparator<PairScore>()
		{// 10

			@Override
			public int compare(PairScore o1, PairScore o2) {
				int prox1 = o1.getQ().getDepth()-o1.getR().getDepth(),
						prox2 = o2.getQ().getDepth()-o2.getR().getDepth();
				
				return sgn(prox1-prox2);
			}

			@Override
			public String toString()
			{
				return "proximity of the red and blue by depth";
			}
		},
		new Comparator<PairScore>()
		{// 11

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  sgn( (measurementsForCurrentStack.get(o1).compatibilityScore - measurementsForCurrentStack.get(o2).compatibilityScore)/3);
			}

			@Override
			public String toString()
			{
				return "difference between conventional scores, divided by 3";
			}
		},
	};
	
	int counterPos[]=new int[comparators.length],counterNeg[]=new int[comparators.length];

	/** Used to denote a value corresponding to an "inconclusive" verdict where a comparator returns values of greater for some points and less for others. */
	public static final int comparison_inconclusive=-10;
	
	/** Updates the statistics on the pairs, using the correct automaton. Returns one of the correct pairs (throws an exception if there is none). 
	 */
	public PairScore updateMaps(Stack<PairScore> pairs,LearnerGraph graph, LearnerGraph correctResult)
	{
		List<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
		
		treeRootedAt.clear();
		Set<CmpVertex> statesOfInterest = new HashSet<CmpVertex>();measurementsForCurrentStack.clear();
		for(PairScore pair:pairs)
		{
			statesOfInterest.add(pair.getQ());statesOfInterest.add(pair.getR());
			if (!treeRootedAt.containsKey(pair.getQ()))
				treeRootedAt.put(pair.getQ(),computeTreeSize(graph, pair.getQ()));
			
			PairMeasurements m = new PairMeasurements();m.nrOfAlternatives=-1;
			for(PairScore p:pairs)
			{
				if (p.getR() == pair.getR())
					++m.nrOfAlternatives;
			}
			ScoreMode origScore = graph.config.getLearnerScoreMode();graph.config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);
			m.compatibilityScore = graph.pairscores.computePairCompatibilityScore(pair);
			graph.config.setLearnerScoreMode(origScore);

			measurementsForCurrentStack.put(pair,m);
		}
		
		Map<CmpVertex,LinkedList<Label>> stateToPath = PairOfPaths.convertSetOfStatesToPaths(graph, statesOfInterest);

	
		int firstCorrectPair = JUConstants.intUNKNOWN, cnt=1;
		for(PairScore p:pairs)
		{
			CmpVertex blue = correctResult.getVertex(stateToPath.get(p.getQ()));
			CmpVertex red = correctResult.getVertex(stateToPath.get(p.getR()));
			if (blue != null && blue == red)
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
		
		if (correctPairs.isEmpty())
			throw new IllegalArgumentException("no correct pairs found");

		// without sorting the pairs, the learner finds itself in a situation with no valid pairs to choose from.
		LinkedList<PairScore> sortedCorrectPairs = new LinkedList<PairScore>(correctPairs);
		Collections.sort(sortedCorrectPairs, new Comparator<PairScore>(){

			@Override
			// The first element is the one where o2 is greater than o1.
			public int compare(PairScore o1, PairScore o2) {
				int outcome = sgn( (o2.getQ().isAccept()?1:-1) -  (o1.getQ().isAccept()?1:-1));
				if (outcome == 0)
					outcome = sgn(measurementsForCurrentStack.get(o2).compatibilityScore - measurementsForCurrentStack.get(o1).compatibilityScore);
				if (outcome == 0)
					outcome = sgn(o2.getR().getIntegerID() - o1.getR().getIntegerID());
				if (outcome == 0)
					outcome = sgn(o2.getR().getDepth() - o1.getR().getDepth());
				if (outcome == 0)
					outcome = sgn(measurementsForCurrentStack.get(o2).nrOfAlternatives - measurementsForCurrentStack.get(o1).nrOfAlternatives);
				return outcome;
			}});

		int pairsThatCannotBeSeparatedFromBadOnes = 0;
		
		for(PairScore p:correctPairs)
		{
			int comparisonResults[] = new int[comparators.length];
			Arrays.fill(comparisonResults, 0);
			
			for(PairScore w:wrongPairs)
			{
				for(int i=0;i<comparators.length;++i)
					if (comparisonResults[i] != comparison_inconclusive)
					{
						int newValue = comparators[i].compare(p, w);
						assert newValue != comparison_inconclusive;
						// comparisonResults[i] can be 1,0,-1, same for newValue
						if (newValue > 0)
						{
							if (comparisonResults[i] < 0)
								comparisonResults[i] = comparison_inconclusive;
							else
								comparisonResults[i]=newValue;
						}
						else
							if (newValue < 0)
							{
								if (comparisonResults[i] > 0)
									comparisonResults[i] = comparison_inconclusive;
								else
									comparisonResults[i]=newValue;
							}
					}
			}

			if (p.getQ().isAccept() && !wrongPairs.isEmpty())
			{
				boolean pairCanBeDistinguished = false;
				for(int i=0;i<comparators.length && !pairCanBeDistinguished;++i)
					if (comparisonResults[i] != comparison_inconclusive)
					{
						if (comparisonResults[i] != 0)
							pairCanBeDistinguished = true;
					}
				if (!pairCanBeDistinguished)
					++pairsThatCannotBeSeparatedFromBadOnes;
			}

			for(int i=0;i<comparators.length;++i)
				if (comparisonResults[i] != comparison_inconclusive)
				{
					if (comparisonResults[i] > 0)
						++counterPos[i];
					else
						if (comparisonResults[i] < 0)
							++counterNeg[i];
				}
		}
		
		if (wekaOutput != null)
			try
			{
				// Compute the Weka statistics, where we compare each pair to all others.
				for(PairScore p:pairs)
				{
					int comparisonResults[] = new int[comparators.length];
					Arrays.fill(comparisonResults, 0);
					
					for(PairScore w:pairs)
					{// it does not matter if w==p, the comparison result will be zero so it will not affect anything
						for(int i=0;i<comparators.length;++i)
							if (comparisonResults[i] != comparison_inconclusive)
							{
								int newValue = comparators[i].compare(p, w);
								assert newValue != comparison_inconclusive;
								// comparisonResults[i] can be 1,0,-1, same for newValue
								if (newValue > 0)
								{
									if (comparisonResults[i] < 0)
										comparisonResults[i] = comparison_inconclusive;
									else
										comparisonResults[i]=newValue;
								}
								else
									if (newValue < 0)
									{
										if (comparisonResults[i] > 0)
											comparisonResults[i] = comparison_inconclusive;
										else
											comparisonResults[i]=newValue;
									}
							}
					}
		
					StringBuffer outputToWeka = new StringBuffer();
					boolean nonZero = false;
					for(int i=0;i<comparators.length;++i)
					{
						int result = comparisonResults[i];if (result == comparison_inconclusive) result = 0;
						if (result != 0) nonZero = true;
						outputToWeka.append(Integer.toString(result));outputToWeka.append(',');
					}
					if (nonZero)
					{// non-zeroes
						wekaOutput.append(outputToWeka.toString());wekaOutput.append(Boolean.toString(correctPairs.contains(p)));wekaOutput.append(endl);
					}
				}
			} // end of weka output		
			catch(IOException e)
			{
				Helper.throwUnchecked("failed to write Weka output", e);
			}
		
		for(int i=0;i<comparators.length;++i)
			System.out.print((i+1)+": ["+counterPos[i]+"/"+counterNeg[i]+"] ");
		if (pairsThatCannotBeSeparatedFromBadOnes > 0)
			System.out.print(" POSITIVE PAIRS NOT CLEARLY BETTER :"+pairsThatCannotBeSeparatedFromBadOnes+" out of "+correctPairs.size());
		System.out.println();
		
		/*
		System.out.println("first correct pair is nr "+firstCorrectPair+", wrong pairs: "+wrongPairs.size());
		Iterator<PairScore> pairIter = sortedPairs.iterator();
		System.out.print("# ");
		for(int i=0;i<firstCorrectPair;++i)
			System.out.print(pairIter.next());
		System.out.println();*/
		return sortedCorrectPairs.iterator().next();
	}
	
	public static ProgressDecorator.InitialData loadInitialAndPopulateInitialConfiguration(PaperUAS paper,String argPTAFileName, Transform.InternStringLabel converter) throws IOException
	{// this part is nested in order to ensure that an instance of LearnerSimulator
	 // goes out of scope and is garbage collected as soon as possible. It holds a great deal
	 // of Xerces objects used for recording execution traces that is not used in this test but takes
	 // a lot of memory.
		final java.io.FileInputStream inputStream = new java.io.FileInputStream(argPTAFileName);
		final LearnerSimulator simulator = new LearnerSimulator(inputStream,true,converter);
		Configuration defaultConfig = Configuration.getDefaultConfiguration().copy();
		paper.learnerInitConfiguration = simulator.readLearnerConstructionData(defaultConfig);
		paper.learnerInitConfiguration.setLabelConverter(converter);
		final org.w3c.dom.Element nextElement = simulator.expectNextElement(StatechumXML.ELEM_INIT.name());
		ProgressDecorator.InitialData initial = simulator.readInitialData(nextElement);
		inputStream.close();
		return initial;
	}
	
	private static final String endl = "\n",pairOutcome="outcome";
	
	public void initWeka(String outputFileName,String datasetName) throws IOException
	{
		if (datasetName.contains(endl) || datasetName.isEmpty())
			throw new IllegalArgumentException("invalid dataset name");
		wekaOutput = new FileWriter(outputFileName);
		wekaOutput.append("@relation ");wekaOutput.append(datasetName);wekaOutput.append(endl);
		for(int i=0;i<comparators.length;++i)
		{
			wekaOutput.append("@attribute ");
			wekaOutput.append(comparators[i].toString().replace(' ', '_'));
			wekaOutput.append(" { 0,-1,1 } ");wekaOutput.append(endl);
		}
		wekaOutput.append("@attribute ");wekaOutput.append(pairOutcome);wekaOutput.append(" { true,false } ");wekaOutput.append(endl);
		wekaOutput.append("@data");wekaOutput.append(endl);
	}
	
	public static void main(String args[]) throws IOException
	{
		Transform.InternStringLabel converter = new Transform.InternStringLabel();
		PaperUAS paper = new PaperUAS();
		final ProgressDecorator.InitialData initial = loadInitialAndPopulateInitialConfiguration(paper, PairQualityLearner.largePTAFileName, converter);
		String outcomeName = PairQualityLearner.largePTALogsDir+"outcome_listOpt.xml";
		final LearnerGraph referenceA = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph(outcomeName, referenceA, converter);
		final PairQualityLearner qualityLearner = new PairQualityLearner();
		qualityLearner.initWeka("wekaoutput2.arff",PairQualityLearner.largePTAFileName);
		
		
		new DummyLearner(new RPNIUniversalLearner(null, paper.learnerInitConfiguration)) 
		{
			
			@Override
			public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
			{// fast merger
				return MergeStates.mergeAndDeterminize(original, pair);
			}
			
			@Override 
			public Stack<PairScore> ChooseStatePairs(LearnerGraph graph)
			{
				Stack<PairScore> outcome = graph.pairscores.chooseStatePairs(new PairScoreComputation.RedNodeDecisionProcedure(){

					@Override
					public CmpVertex selectRedNode(
							@SuppressWarnings("unused") LearnerGraph coregraph,
							@SuppressWarnings("unused") Collection<CmpVertex> reds,
							Collection<CmpVertex> tentativeRedNodes) 
					{
						CmpVertex redVertex = tentativeRedNodes.iterator().next();
						return redVertex;
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
				LearnerGraph graph = decoratedLearner.init(plus,minus);
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
	}
}
