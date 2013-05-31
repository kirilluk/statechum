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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.PairMeasurements;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import weka.classifiers.Classifier;
import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;

public class WekaPairClassifier
{
	Classifier classifier;
	
	/**
	 * Begins construction of an instance of pair classifier.
	 */
	public WekaPairClassifier()
	{
		FastVector vecBool = new FastVector(2);vecBool.addElement(Boolean.TRUE.toString());vecBool.addElement(Boolean.FALSE.toString());
		classAttribute = new Attribute("class",vecBool);
	}

	/**
	 * Completes construction of an instance of pair classifier. Comparators contain attributes that are tied into the training set when it is constructed in this method.
	 * 
	 * @param trainingSetName the name for a training set.
	 * @param capacity the maximal number of elements in the training set
	 * @param comp a collection of comparators to use.
	 */
	public void initialise(String trainingSetName, int capacity, List<PairComparator> comp)
	{
		comparators = comp;
		FastVector attributes = new FastVector(comparators.size()+1);
		for(PairComparator cmp:comparators)
			attributes.addElement(cmp.getAttribute());
		attributes.addElement(classAttribute);
		trainingData = new Instances(trainingSetName,attributes,capacity);
		trainingData.setClass(classAttribute);
	}

	/** Given a pair and a collection of possible pairs to merge, compares the specified pairs to others to determine its attributes that may make it more likely to be a valid merge.
	 *  Constructs a Weka {@link Instance}.
	 * 
	 * @param comparisonResults results of comparison of this pair to other pairs.
	 * @param classification whether this is a correct pair
	 * @return an instance of a test or a training sample. 
	 */
	Instance constructInstance(int []comparisonResults, boolean classification)
	{
		Instance outcome = new Instance(comparators.size()+1);outcome.setDataset(trainingData);
		if (comparisonResults.length != comparators.size())
			throw new IllegalArgumentException("results' length does not match the number of comparators");
		for(int i=0;i<comparators.size();++i)
		{
			String value = null;
			switch(comparisonResults[i])
			{
			case 1:
				value = ONE;break;
			case 0:
				value = ZERO;break;
			case -1:
				value = MINUSONE;break;
			default:
				throw new IllegalArgumentException("invalid comparison value "+comparisonResults[i]+" for comparator "+comparators.get(i));
			}
			outcome.setValue(comparators.get(i).getAttribute(), value);
		}
		outcome.setValue(classAttribute, Boolean.toString(classification));
		return outcome;
	}
	
	/** Given an outcome of a comparison of a pair to other pairs, attempts to estimate how significant the result is.
	 * This is useful for prioritising states to be selected as red.
	 * 
	 * For instance, this can based on counting the attributes that contributed to a decision
	 * by a learner to consider the pair as either good or bad. At present, we evaluate the probability that the given result belongs to the specific class. 
	 *  
	 * @param comparisonResults the outcome of {@link #comparePairWithOthers(PairScore, Collection)}.
	 * @return a non-negative "quality" of a pair. 
	 * @throws Exception 
	 */
	double getPairQuality(int[]comparisonResults) throws Exception
	{
		return classifier.distributionForInstance(constructInstance(comparisonResults,false))[0];
	}
	
	protected final Attribute classAttribute;
	protected Instances trainingData;
	
	static final String MINUSONE="-1";
	static final String ZERO="0";
	static final String ONE="1";

	protected List<PairComparator> comparators;
	
	Map<StatePair,PairMeasurements> measurementsForComparators=new TreeMap<StatePair,PairMeasurements>();
	double scoreAverage=0, compatibilityScoreAverage=0,scoreSD=0,compatibilityScoreSD=0;
	Map<CmpVertex,Integer> treeForComparators = new TreeMap<CmpVertex,Integer>();

	/** Given a collection of pairs and a tentative automaton, constructs auxiliary structures used by comparators and stores it as an instance variable.
	 * 
	 * @param pairs pairs to build sets for
	 * @param graph graph to use for construction
	 * @returns constructed tree. The return value is only used for testing.
	 */
	Map<StatePair,PairMeasurements> buildSetsForComparators(Collection<PairScore> pairs, LearnerGraph graph)
	{
		treeForComparators.clear();
		measurementsForComparators.clear();
		scoreAverage=0;compatibilityScoreAverage=0;scoreSD=0;compatibilityScoreSD=0;
		
		for(PairScore pair:pairs)
		{
			if (!treeForComparators.containsKey(pair.getQ()))
				treeForComparators.put(pair.getQ(),PairQualityLearner.computeTreeSize(graph, pair.getQ()));
			
			PairMeasurements m = new PairMeasurements();m.nrOfAlternatives=-1;
			for(PairScore p:pairs)
			{
				if (p.getR() == pair.getR())
					++m.nrOfAlternatives;
			}
			
			Collection<CmpVertex> adjacentOutgoingBlue = graph.transitionMatrix.get(pair.getQ()).values(), adjacentOutgoingRed = graph.transitionMatrix.get(pair.getR()).values(); 
			m.adjacent = adjacentOutgoingBlue.contains(pair.getR()) || adjacentOutgoingRed.contains(pair.getQ());
			ScoreMode origScore = graph.config.getLearnerScoreMode();graph.config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);
			m.compatibilityScore = graph.pairscores.computePairCompatibilityScore(pair);
			graph.config.setLearnerScoreMode(origScore);
			
			compatibilityScoreAverage=+m.compatibilityScore;scoreAverage+=pair.getScore();

			measurementsForComparators.put(pair,m);
		}
		
		if (pairs.size()>0)
		{
			scoreAverage/=pairs.size();compatibilityScoreAverage/=pairs.size();
			for(PairScore pair:pairs)
			{
				double scoreDiff = pair.getScore()-scoreAverage, compatibilityScoreDiff = measurementsForComparators.get(pair).compatibilityScore-compatibilityScoreAverage;
				scoreSD+=scoreDiff*scoreDiff;compatibilityScoreSD+=compatibilityScoreDiff*compatibilityScoreDiff;
			}
			scoreSD/=pairs.size();compatibilityScoreSD/=pairs.size();
		}		
		return measurementsForComparators;
	}
	
	/** Used to denote a value corresponding to an "inconclusive" verdict where a comparator returns values of greater for some points and less for others. */
	public static final int comparison_inconclusive=-10;

	/** Given a pair and a collection of possible pairs to merge, compares the specified pairs to others to determine its attributes that may make it more likely to be a valid merge.
	 * Where the returned value is +1 or -1 in a specific cell, this means that the pair of interest is not dominated in the specific component by all other pairs.
	 * The outcome of 1 means that it is equal to some other pairs and above others but never below.
	 * In a similar way, -1 means that it does not dominate any other pairs.
	 *  
	 * @param pair pair to consider
	 * @param others other pairs (possibly, both valid and invalid mergers).
	 * @return the vector of comparison results.
	 */
	int [] comparePairWithOthers(PairScore pair, Collection<PairScore> others)
	{
		assert !comparators.isEmpty();
		int comparisonResults[] = new int[comparators.size()];
		Arrays.fill(comparisonResults, 0);
		
		for(PairScore w:others)
		{// it does not matter if w==pair, the comparison result will be zero so it will not affect anything
			int i=0;
			for(PairComparator cmp:comparators)
			{
				if (comparisonResults[i] != comparison_inconclusive)
				{
					int newValue = cmp.compare(pair, w);
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
				
				++i;
			}
		}
		
		for(int cnt=0;cnt<comparators.size();++cnt)
			if (comparisonResults[cnt] == comparison_inconclusive) comparisonResults[cnt]=0;
		return comparisonResults;
	}
	
	
	/** Given a collection of pairs from a tentative graph, this method generates Weka data instances and adds them to the Weka dataset.
	 * We do not compare correct pairs with each other, or wrong pairs with each other. Pairs that have negative scores are ignored.
	 * 
	 * @param pairs pairs to add
	 * @param tentativeGraph the current graph
	 * @param correctGraph the graph we are trying to learn by merging states in tentativeGraph.
	 */
	public void updateDatasetWithPairs(Collection<PairScore> pairs, LearnerGraph tentativeGraph, LearnerGraph correctGraph)
	{
		buildSetsForComparators(pairs,tentativeGraph);
		
		List<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
		List<PairScore> pairsToConsider = new LinkedList<PairScore>();
		if (!pairs.isEmpty())
		{
			for(PairScore p:pairs) if (p.getQ().isAccept()) pairsToConsider.add(p);// only consider non-negatives
		}
		PairQualityLearner.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, pairsToConsider, correctPairs, wrongPairs);
		
		
		// Compute Weka statistics, where we compare each pair to all others.
		for(PairScore p:correctPairs)
		{
			int []comparisonResults = comparePairWithOthers(p, wrongPairs);
			/*
			boolean nonZero = false;
			for(int i=0;i<comparators.size() && !nonZero;++i)
			{
				int result = comparisonResults[i];if (result == comparison_inconclusive) result = 0;
				if (result != 0) nonZero = true;
			}
			
			if (nonZero)
			*/
			
			trainingData.add(constructInstance(comparisonResults, true));
		}
		for(PairScore p:wrongPairs)
		{
			int []comparisonResults = comparePairWithOthers(p, correctPairs);trainingData.add(constructInstance(comparisonResults, false));
		}
	}
	
	/** This one is a nested class to permit access to instance variables. This seems natural because elements of this class need access to data obtained from the transition matrix. */
	public abstract class PairComparator implements Comparator<PairScore> 
	{
		/** Weka attribute associated with this comparator. */
		final Attribute att;
		
		public Attribute getAttribute()
		{
			return att;
		}
		
		protected PairComparator(String name)
		{
			FastVector vecA = new FastVector(3);vecA.addElement(MINUSONE);vecA.addElement(ZERO);vecA.addElement(ONE);
			att = new Attribute(name,vecA);
		}

		public PairMeasurements measurementsForCurrentStack(PairScore p)
		{
			return measurementsForComparators.get(p);
		}
	
		public int treeRootedAt(CmpVertex p)
		{
			return treeForComparators.get(p);
		}
		
		@Override
		public String toString()
		{
			return att.name();
		}

	}
}
