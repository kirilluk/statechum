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
import java.util.HashMap;
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

public class WekaDataCollector
{
	Classifier classifier;
	
	/** The maximal number of attributes to use as part of a conditional statement. Where 0, no conditionals are considered, for QSM (Score/Red/Blue) it has to be 2.
	 */
	int maxLevel;
	
	/**
	 * The length of an instance, taking {@link WekaDataCollector#maxLevel} into account.
	 */
	int instanceLength;
	
	/** Attributes associated with each instance. These are not the same as attributes of comparators because comparators are only considering individual comparisons and here we are working with if-then chains. */
	Attribute[] attributesOfAnInstance;
	
	public int getInstanceLength()
	{
		return instanceLength;
	}
	
	/**
	 * Begins construction of an instance of pair classifier.
	 */
	public WekaDataCollector()
	{
		FastVector vecBool = new FastVector(2);vecBool.addElement(Boolean.TRUE.toString());vecBool.addElement(Boolean.FALSE.toString());
		classAttribute = new Attribute("class",vecBool);
	}

	protected int n,L;
	
	/** Number of values for attributes to consider. */
	protected final static int V=2;
	/**
	 * Completes construction of an instance of pair classifier. Comparators contain attributes that are tied into the training set when it is constructed in this method.
	 * 
	 * @param trainingSetName the name for a training set.
	 * @param capacity the maximal number of elements in the training set
	 * @param argAssessor a collection of assessors to use.
	 * @param level the maximal number of attributes to use as part of a conditional statement.
	 */
	public void initialise(String trainingSetName, int capacity, List<PairRank> argAssessor, int level)
	{
		if (assessors != null) throw new IllegalArgumentException("WekaDataCollector should not be re-initialised");
		
		assessors = argAssessor;measurementsForUnfilteredCollectionOfPairs.valueAverage = new double[assessors.size()];measurementsForUnfilteredCollectionOfPairs.valueSD=new double[assessors.size()];
		comparators = new ArrayList<PairComparator>(assessors.size());
		for(PairRank pr:assessors)
			comparators.add(new PairComparator(pr));
		if (comparators.size() > Long.SIZE-1)
			throw new IllegalArgumentException("attributes will not fit into long");
		
		maxLevel = level;

		n = comparators.size();// the number of indices we go through
		L = comparators.size()*2;// the length of the attributes accumulated for each pair.
		long instanceLen = L, sectionLen=1;
		if (V*n<maxLevel)
			throw new IllegalArgumentException("too many levels for the considered number of attributes");
			
		for(int i=0;i<maxLevel;++i)
		{
			sectionLen*=V*(n-i);
			instanceLen+=L*sectionLen;
			if (sectionLen > Integer.MAX_VALUE)
				throw new IllegalArgumentException("too many attributes per instance");
		}
		instanceLength = (int)instanceLen;

		
		FastVector attributes = new FastVector(instanceLength+1);
		attributesOfAnInstance = new Attribute[instanceLength];
		fillInAttributeNames(attributesOfAnInstance,0,0,1,0,"",0);for(int i=0;i<instanceLength;++i) attributes.addElement(attributesOfAnInstance[i]);
		attributes.addElement(classAttribute);
		trainingData = new Instances(trainingSetName,attributes,capacity);
		trainingData.setClass(classAttribute);
	}

	protected void fillInAttributeNames(Attribute[] whatToFillIn, int section_start, int idx_in_section, int section_size, long xyz, String pathToThisLevel, int currentLevel)
	{
		final int sectionPlusOffset = section_start + L*idx_in_section;

		int i=0;

		for(PairComparator cmp:comparators)
			whatToFillIn[sectionPlusOffset+i++]=cmp.getAttribute().copy(pathToThisLevel+cmp.getAttribute().name());
		for(PairRank pr:assessors)
			whatToFillIn[sectionPlusOffset+i++]=pr.getAttribute().copy(pathToThisLevel+pr.getAttribute().name());

		if (currentLevel < maxLevel)
		{
			final int rowNumber = V*(n-currentLevel);
			final int nextSectionStart = section_start+L*section_size;
			int z=0;
			for(int attr=0;attr<n;++attr)
			{
				long positionalBit = 1 << attr;
				if ((xyz & positionalBit) == 0) // this attribute was not already used on a path to the current instance of fillInEntry
				{

					fillInAttributeNames(whatToFillIn,nextSectionStart,idx_in_section*rowNumber+z+0,section_size*rowNumber, xyz|positionalBit,pathToThisLevel+" if "+comparators.get(attr).getAttribute().name()+"==-1 then ",currentLevel+1);
					fillInAttributeNames(whatToFillIn,nextSectionStart,idx_in_section*rowNumber+z+1,section_size*rowNumber, xyz|positionalBit,pathToThisLevel+" if "+comparators.get(attr).getAttribute().name()+"==1 then ",currentLevel+1);
					z+=V;
				}
			}
		}
	}

	protected double convertAssessmentResultToString(int assessmentResult, Attribute attribute)
	{
		String value = null;
		switch(assessmentResult)
		{
		case 2:
			value = TWO;break;
		case 1:
			value = ONE;break;
		case 0:
			value = ZERO;break;
		case -1:
			value = MINUSONE;break;
		case -2:
			value = MINUSTWO;break;
			
		default:
			throw new IllegalArgumentException("invalid comparison value "+assessmentResult+" for attribute "+attribute);
			
		}
		double outcome = attribute.indexOfValue(value);
		if (outcome < 0)
			throw new IllegalArgumentException("value "+value+" was not defined for attribute "+attribute);

		return outcome;
	}
	
	/**  Constructs a Weka {@link Instance} for a pair of interest.
	 * 
	 * @param comparisonResults metrics related to the considered pair.
	 * @param classification whether this is a correct pair
	 * @return an instance of a test or a training sample. 
	 */
	Instance constructInstance(int []comparisonResults, boolean classification)
	{
		if (comparisonResults.length != instanceLength)
			throw new IllegalArgumentException("results' length does not match the number of comparators");

		double []instanceValues=new double[instanceLength+1];
		for(int i=0;i<instanceLength;++i)
			instanceValues[i]=convertAssessmentResultToString(comparisonResults[i],attributesOfAnInstance[i]);
		
		instanceValues[instanceLength]=trainingData.classAttribute().indexOfValue(Boolean.toString(classification));
		Instance outcome = new Instance(1,instanceValues);outcome.setDataset(trainingData);
		return outcome;
	}
	
	/** Given an outcome of a comparison of a pair to other pairs, attempts to estimate how significant the result is.
	 * This is useful for prioritising states to be selected as red.
	 * 
	 * For instance, this can based on counting the attributes that contributed to a decision
	 * by a learner to consider the pair as either good or bad. At present, we evaluate the probability that the given result belongs to the specific class. 
	 *  
	 * @param comparisonResults the outcome of {@link #fillInPairDetails(int[], PairScore, Collection)}.
	 * @return a non-negative "quality" of a pair. 
	 * @throws Exception 
	 */
	double getPairQuality(int[]comparisonResults) throws Exception
	{
		return classifier.distributionForInstance(constructInstance(comparisonResults, false))[0];
	}
	
	protected final Attribute classAttribute;
	public Instances trainingData;
	
	static final String MINUSTWO="-2";
	static final String MINUSONE="-1";
	static final String ZERO="0";
	static final String ONE="1";
	static final String TWO="2";

	protected List<PairComparator> comparators;
	protected List<PairRank> assessors;
	
	class MeasurementsForCollectionOfPairs
	{
		Map<StatePair,PairMeasurements> measurementsForComparators=new HashMap<StatePair,PairMeasurements>();
		double valueAverage[]=new double[0], valueSD[]=new double[0];
	}
	
	Map<CmpVertex,Integer> treeForComparators = new TreeMap<CmpVertex,Integer>();
	MeasurementsForCollectionOfPairs measurementsForUnfilteredCollectionOfPairs = new MeasurementsForCollectionOfPairs();
	
	LearnerGraph tentativeGraph = null;
	
	
	void buildSetsForComparatorsThatDoNotDependOnFiltering(Collection<PairScore> pairs, LearnerGraph graph)
	{
		treeForComparators.clear();
		tentativeGraph = graph;
		
		for(PairScore pair:pairs)
		{
			if (!treeForComparators.containsKey(pair.getQ()))
				treeForComparators.put(pair.getQ(),PairQualityLearner.computeTreeSize(graph, pair.getQ()));
		}
		buildSetsForComparatorsDependingOnFiltering(measurementsForUnfilteredCollectionOfPairs,pairs);
	}
	
	/** Given a collection of pairs and a tentative automaton, constructs auxiliary structures used by comparators and stores it as an instance variable.
	 * The graph used for construction is the one that was passed earlier to {@link WekaDataCollector#buildSetsForComparatorsThatDoNotDependOnFiltering(Collection, LearnerGraph)}.
	 * @param pairs pairs to build sets for
	 * @param measurements where to store the result of measurement.
	 */
	void buildSetsForComparatorsDependingOnFiltering(MeasurementsForCollectionOfPairs measurements, Collection<PairScore> pairs)
	{
		measurements.measurementsForComparators.clear();
		if (measurements.valueAverage.length < n)
			measurements.valueAverage = new double[n];
		if (measurements.valueSD.length < n)
			measurements.valueSD = new double[n];
		
		Arrays.fill(measurements.valueAverage, 0);Arrays.fill(measurements.valueSD, 0);
		
		for(PairScore pair:pairs)
		{
			PairMeasurements m = new PairMeasurements();m.nrOfAlternatives=-1;
			for(PairScore p:pairs)
			{
				if (p.getR() == pair.getR())
					++m.nrOfAlternatives;
			}
			
			Collection<CmpVertex> adjacentOutgoingBlue = tentativeGraph.transitionMatrix.get(pair.getQ()).values(), adjacentOutgoingRed = tentativeGraph.transitionMatrix.get(pair.getR()).values(); 
			m.adjacent = adjacentOutgoingBlue.contains(pair.getR()) || adjacentOutgoingRed.contains(pair.getQ());
			ScoreMode origScore = tentativeGraph.config.getLearnerScoreMode();tentativeGraph.config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);
			m.compatibilityScore = tentativeGraph.pairscores.computePairCompatibilityScore(pair);
			tentativeGraph.config.setLearnerScoreMode(origScore);
			
			measurements.measurementsForComparators.put(pair,m);
		}

		if (assessors != null)
			for(PairScore pair:pairs)
				for(int i=0;i<assessors.size();++i)
				{
					long value = assessors.get(i).getValue(pair);
					measurements.valueAverage[i]+=value;measurements.valueSD[i]+=value*value;
				}
		
		if (assessors != null)
			for(int i=0;i<assessors.size();++i)
			{
				measurements.valueAverage[i]/=pairs.size();measurements.valueSD[i]=Math.sqrt(measurements.valueSD[i]/pairs.size()-measurements.valueAverage[i]*measurements.valueAverage[i]);
			}
	}

	/** Used to denote a value corresponding to an "inconclusive" verdict where a comparator returns values of greater for some points and less for others. */
	public static final int comparison_inconclusive=-10;

	int comparePairWithOthers(PairComparator cmp, PairScore pair, Collection<PairScore> others)
	{
		int comparisonResult = 0;
		for(PairScore w:others)
		{// it does not matter if w==pair, the comparison result will be zero so it will not affect anything
				int newValue = cmp.compare(pair, w);
				assert newValue != comparison_inconclusive;
				// comparisonResults[i] can be 1,0,-1, same for newValue
				if (newValue > 0)
				{
					if (comparisonResult < 0)
					{
						comparisonResult= comparison_inconclusive;break;
					}
					comparisonResult=newValue;
				}
				else
					if (newValue < 0)
					{
						if (comparisonResult > 0)
						{
							comparisonResult = comparison_inconclusive;break;
						}
						comparisonResult=newValue;
					}
		}
		return comparisonResult;
	}
	
	/** Given a pair and a collection of possible pairs to merge, compares the specified pairs to others to determine its attributes that may make it more likely to be a valid merge.
	 * Where the returned value is +1 or -1 in a specific cell, this means that the pair of interest is not dominated in the specific component by all other pairs.
	 * The outcome of 1 means that it is equal to some other pairs and above others but never below.
	 * In a similar way, -1 means that it does not dominate any other pairs.
	 * 
	 * @param pair pair to consider
	 * @param others other pairs (possibly, both valid and invalid mergers).
	 * @param whatToFillIn array to populate with results
	 * @param offset the starting position to fill in.
	 */
	void comparePairWithOthers(PairScore pair, Collection<PairScore> others, int []whatToFillIn, int offset)
	{
		assert !comparators.isEmpty();
		
		int i=0;
		for(PairComparator cmp:comparators)
		{
			whatToFillIn[i+offset] = comparePairWithOthers(cmp, pair, others);
			++i;
		}
		
		for(int cnt=0;cnt<comparators.size();++cnt)
			if (whatToFillIn[cnt+offset] == comparison_inconclusive) whatToFillIn[cnt+offset]=0;
	}

	/** Assesses a supplied pair based on the values.
	 * 
	 * @param pair pair to consider
	 * @param measurements set of measurements to use for assessment
	 * @param whatToFillIn array to populate with results
	 * @param offset the starting position to fill in.
	 */
	void assessPair(PairScore pair, MeasurementsForCollectionOfPairs measurements, int []whatToFillIn, int offset)
	{
		assert !assessors.isEmpty();
		//Arrays.fill(whatToFillIn, offset, comparators.size(), 0);
		for(int i=0;i<assessors.size();++i)
			whatToFillIn[i+offset]=assessors.get(i).getRanking(pair, measurements.valueAverage[i], measurements.valueSD[i]);
	}

	protected void fillInEntry(int [] whatToFillIn,int section_start, int idx_in_section, int section_size, long xyz, PairScore pairOfInterest, Collection<PairScore> pairs, int currentLevel)
	{
		MeasurementsForCollectionOfPairs measurements = new MeasurementsForCollectionOfPairs();
		buildSetsForComparatorsDependingOnFiltering(measurements,pairs);
		final int sectionPlusOffset = section_start + L*idx_in_section;
		comparePairWithOthers(pairOfInterest,pairs,whatToFillIn,sectionPlusOffset);
		assessPair(pairOfInterest,measurements, whatToFillIn,sectionPlusOffset+n);
		if (currentLevel < maxLevel)
		{
			final int rowNumber = V*(n-currentLevel);
			final int nextSectionStart = section_start+L*section_size;
			int z=0;
			for(int attr=0;attr<n;++attr)
			{
				long positionalBit = 1 << attr;
				if ((xyz & positionalBit) == 0) // this attribute was not already used on a path to the current instance of fillInEntry
				{
					int attributeREL = whatToFillIn[sectionPlusOffset+attr];
					if (attributeREL != 0)
					{
						assert attributeREL == 1 || attributeREL == -1;
						Collection<PairScore> others = new ArrayList<PairScore>(pairs.size());
						for(PairScore other:pairs) 
						{
							int comparisonOnAttribute_i = comparePairWithOthers(comparators.get(attr),other,pairs);
							if (comparisonOnAttribute_i == attributeREL) // we only compare our vertex with those that are also distinguished by the specified attribute
								others.add(other);
						}
						if (others.size()>1)
							// the value of 2 below is a reflection that we only distinguish between two different relative values. If SD part were considered, there would be a lot more values.
							fillInEntry(whatToFillIn,nextSectionStart,idx_in_section*rowNumber+z+(attributeREL>0?1:0),section_size*rowNumber, xyz|positionalBit,pairOfInterest,others,currentLevel+1);
					}				
					z+=V;
				}
			}
		}
	}

	/** Fills in the array with comparison results. For correct operation, the supplied pair of interest has to be included in the collection of pairs. */
	public void fillInPairDetails(int [] whatToFillIn, PairScore pairOfInterest, Collection<PairScore> pairs)
	{
		if (whatToFillIn.length < getInstanceLength())
			throw new IllegalArgumentException("array is too short");
		fillInEntry(whatToFillIn,0,0,1,0,pairOfInterest,pairs,0);
	}

	/** Given a collection of pairs from a tentative graph, this method generates Weka data instances and adds them to the Weka dataset.
	 * We do not compare correct pairs with each other, or wrong pairs with each other. Pairs that have negative scores are ignored.
	 * 
	 * @param pairs pairs to add
	 * @param currentGraph the current graph
	 * @param correctGraph the graph we are trying to learn by merging states in tentativeGraph.
	 */
	public void updateDatasetWithPairs(Collection<PairScore> pairs, LearnerGraph currentGraph, LearnerGraph correctGraph)
	{
		buildSetsForComparatorsThatDoNotDependOnFiltering(pairs,currentGraph);
		
		List<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
		List<PairScore> pairsToConsider = new LinkedList<PairScore>();
		if (!pairs.isEmpty())
		{
			for(PairScore p:pairs) if (p.getQ().isAccept() && p.getR().isAccept()) pairsToConsider.add(p);// only consider non-negatives
		}
		PairQualityLearner.SplitSetOfPairsIntoRightAndWrong(currentGraph, correctGraph, pairsToConsider, correctPairs, wrongPairs);
		
		for(PairScore p:pairsToConsider)
		{
			int []comparisonResults = new int[instanceLength];
			fillInPairDetails(comparisonResults,p, pairsToConsider);// only compare with other non-negatives
			boolean correctPair = correctPairs.contains(p);
			//boolean correctPair = p.equals(PairQualityLearner.LearnerThatCanClassifyPairs.pickPairQSMLike(pairsToConsider));
			trainingData.add(constructInstance(comparisonResults, correctPair));
		}

		/*
		// Compute Weka statistics, where we compare each pair to all others.
		for(PairScore p:correctPairs)
		{
			int []comparisonResults = comparePairWithOthers(p, pairs);
			//System.out.println(p+" "+Arrays.toString(comparisonResults));
			trainingData.add(constructInstance(comparisonResults, assessPair(p), true));
		}
		
		for(PairScore p:wrongPairs)
		{
			int []comparisonResults = comparePairWithOthers(p, pairs);
			//System.out.println(p+" "+Arrays.toString(comparisonResults));
			trainingData.add(constructInstance(comparisonResults, assessPair(p), false));
		}*/
	}
	
	/**
	 * Provides helper methods in order to train a classifier to recognise good/bad pairs
	 * <hr/>
	 * It is a nested class to permit access to instance variables. This seems natural because elements of this class need access to data obtained from the transition matrix. 
	 *
	 */
	public abstract class PairRankingSupport
	{
		/** Weka attribute associated with this comparator. */
		final Attribute att;
		
		public Attribute getAttribute()
		{
			return att;
		}
		
		protected PairRankingSupport(String name, String [] range)
		{
			FastVector vecA = new FastVector(3);
			for(String v:range) vecA.addElement(v);
			att = new Attribute(name,vecA);
		}
		
		@Override
		public String toString()
		{
			return att.name();
		}
		public PairMeasurements measurementsForCurrentStack(PairScore p)
		{
			return measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.get(p);
		}
	
		public int treeRootedAt(CmpVertex p)
		{
			return treeForComparators.get(p);
		}
		
		public LearnerGraph tentativeGraph()
		{
			return tentativeGraph;
		}
	}
	
	/** Used to compute values permitting one to train a classifier to recognise good/bad pairs. 
	 * 
	 */
	public class PairComparator extends PairRankingSupport implements Comparator<PairScore> 
	{
		protected final PairRank assessor;
		
		protected PairComparator(PairRank argAssessor)
		{
			super("REL "+argAssessor.getAttribute().name(), new String[]{ZERO,ONE,MINUSONE});assessor = argAssessor;
		}

		@Override
		public int compare(PairScore o1, PairScore o2) {
			return PairQualityLearner.sgn(assessor.getValue(o1) - assessor.getValue(o2));
		}
	}
	
	/** {@link PairComparator} permits one to compare pairs with each other. This one aims to give a rank to each pair in a collection of pairs, by either retrieving specific attributes or 
	 * doing the average/standard deviation thresholding.
	 * @author kirill
	 *
	 */
	public abstract class PairRank extends PairRankingSupport
	{
		protected PairRank(String name)
		{
			super(name, new String[]{ZERO,ONE,MINUSONE,TWO,MINUSTWO});
		}
		
		
		/** Returns 0,1, or -1 depending on how the pair scores compared to an average across the collection of pairs, 
		 * standard deviation and average.
		 * 
		 * @param pair
		 * @param average
		 * @param sd
		 * @return
		 */
		public int getRanking(PairScore pair, double average, double sd)
		{
			long value = getValue(pair);
			if (isAbsolute())
				return (int)value;
			
			if (value > average+sd)
			{
				if (value > average+sd+sd)
					return 2;
				return 1;
			}
			if (value < average-sd)
			{
				if (value < average-sd-sd)
					return -2;
				return -1;
			}
			return 0;
		}
		
		/** Returns true if {@link PairRank#getRanking} should not use average/standard deviation in order to normalise results across different sets of pairs. This is important where we aim to distinguish between zero/above-zero scores. */
		abstract public boolean isAbsolute();
		
		/** Obtains a value from a supplied pair that can be used in order to calculate the ranking. */
		abstract public long getValue(PairScore pair);
	}
}
