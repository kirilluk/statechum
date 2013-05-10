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
	 * Constructs an instance of pair classifier.
	 * 
	 * @param trainingSetName the name for a tranining set.
	 * @param capacity the maximal number of elements in the training set
	 */
	public WekaPairClassifier(String trainingSetName, int capacity)
	{
		FastVector vecBool = new FastVector(2);vecBool.addElement(Boolean.TRUE.toString());vecBool.addElement(Boolean.FALSE.toString());
		classAttribute = new Attribute("class",vecBool);
		FastVector vecAttribute = new FastVector(3);vecAttribute.addElement(MINUSONE);vecAttribute.addElement(ZERO);vecAttribute.addElement(ONE);
		FastVector attributes = new FastVector(comparators.size()+1);
		for(Comparator<PairScore> cmp:comparators)
			attributes.addElement(new Attribute(cmp.toString(),vecAttribute));
		trainingData = new Instances(trainingSetName,attributes,capacity);
		trainingData.setClass(classAttribute);
	}

	/** Given a pair and a collection of possible pairs to merge, compares the specified pairs to others to determine its attributes that may make it more likely to be a valid merge.
	 *  Constructs a Weka {@link Instance}. 
	 * @param comparisonResults results of comparison of this pair to other pairs.
	 * @param classification whether this is a correct pair
	 * @return an instance of a test or a training sample. 
	 */
	Instance constructInstance(int []comparisonResults, boolean classification)
	{
		Instance outcome = new Instance(comparators.size()+1);outcome.setDataset(trainingData);
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
			outcome.setValue(i, value);
		}
		outcome.setValue(classAttribute, Boolean.toString(classification));trainingData.add(outcome);
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
	protected final Instances trainingData;
	
	public void addComparator(PairComparator cmp)
	{
		comparators.add(cmp);
	}
	
	private static final String MINUSONE="-1",ZERO="0",ONE="1";

	protected final List<PairComparator> comparators = new ArrayList<PairComparator>();
	
	Map<StatePair,PairMeasurements> measurementsForComparators=new TreeMap<StatePair,PairMeasurements>();
	
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

			measurementsForComparators.put(pair,m);
		}
		return measurementsForComparators;
	}
	
	/** Used to denote a value corresponding to an "inconclusive" verdict where a comparator returns values of greater for some points and less for others. */
	public static final int comparison_inconclusive=-10;

	/** Given a pair and a collection of possible pairs to merge, compares the specified pairs to others to determine its attributes that may make it more likely to be a valid merge.
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
		{// it does not matter if w==p, the comparison result will be zero so it will not affect anything
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
		return comparisonResults;
	}
	
	
	/** Given a collection of pairs frmo a tentative graph, this method generates Weka data instances and adds them to the Weka dataset.
	 *  
	 * @param pairs pairs to add
	 * @param tentativeGraph the current graph
	 * @param correctGraph the graph we are trying to learn by merging states in tentativeGraph.
	 */
	public void updateDatasetWithPairs(Collection<PairScore> pairs, LearnerGraph tentativeGraph, LearnerGraph correctGraph)
	{
		buildSetsForComparators(pairs,tentativeGraph);
		
		List<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
		PairQualityLearner.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, pairs, correctPairs, wrongPairs);
		
		List<PairScore> pairsToConsider = new LinkedList<PairScore>();
		if (!pairs.isEmpty())
		{
			for(PairScore p:pairs) if (p.getQ().isAccept()) pairsToConsider.add(p);// only consider non-negatives
		}
		
		// Compute Weka statistics, where we compare each pair to all others.
		for(PairScore p:pairsToConsider)
		{
			int []comparisonResults = comparePairWithOthers(p, pairs);
			/*
			boolean nonZero = false;
			for(int i=0;i<comparators.size() && !nonZero;++i)
			{
				int result = comparisonResults[i];if (result == comparison_inconclusive) result = 0;
				if (result != 0) nonZero = true;
			}
			
			if (nonZero)
			*/
			
			trainingData.add(constructInstance(comparisonResults, correctPairs.contains(p)));
		}
	}
	
	public abstract class PairComparator implements Comparator<PairScore> 
	{
		public PairMeasurements measurementsForCurrentStack(PairScore p)
		{
			return measurementsForComparators.get(p);
		}
	
		public int treeRootedAt(CmpVertex p)
		{
			return treeForComparators.get(p);
		}
	}
}
