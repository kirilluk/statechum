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
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovHelper;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DataCollectorParameters;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.FilteredPairMeasurements;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.PairMeasurements;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import weka.classifiers.Classifier;
import weka.core.Attribute;
import weka.core.BinarySparseInstance;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

public class WekaDataCollector
{
	Classifier classifier;
	
	/** The maximal number of attributes to use as part of a conditional statement. Where 0, no conditionals are considered, for QSM (Score/Red/Blue) it has to be 2.
	 */
	int maxLevel;
	
	public int getLevel()
	{
		return maxLevel;
	}
	
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
	
	protected final DataCollectorParameters dataCollectorParameters;
	
	/**
	 * Begins construction of an instance of pair classifier.
	 * @param markovHelper helper used to construct inconsistencies for the use with Markov experiments. The associated ranking routines are not used if this is null.
	 * @param graphPTA true if the graph rooted at the blue state is expected to be a PTA rather than a directed graph. 
	 */
	public WekaDataCollector(MarkovHelper helper, DataCollectorParameters p)
	{
		markovHelper = helper;dataCollectorParameters = p;
		classAttribute = new Attribute("class",Arrays.asList(new String[]{Boolean.TRUE.toString(),Boolean.FALSE.toString()}));
	}

	protected int n,attrMult=2;
	
	public void setEnableSD(boolean value)
	{
		if (value)
			attrMult = 2;
		else
			attrMult = 1;
	}
	
	/** Number of values for attributes to consider. This means where a pair scores at the top or at the bottom of a list of pairs according to each attribute from comparators.size(); pairs with values outside SD are recorded but not used for splitting to reduce the amount of data thrown at the machine learner. */
	protected final static int V=2;
	
	/** Attributes of this collection. Constructed by the {@link WekaDataCollector#initialise(String, int, List, int)} call. */
	ArrayList<Attribute> attributes = null;
	
	public ArrayList<Attribute> getAttributes()
	{
		return attributes;
	}
	
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
		
		assessors = argAssessor;measurementsForFilteredCollectionOfPairs.valueAverage = new double[assessors.size()];measurementsForFilteredCollectionOfPairs.valueSD=new double[assessors.size()];
		comparators = new ArrayList<PairComparator>(assessors.size());
		for(PairRank pr:assessors)
			comparators.add(new PairComparator(pr));
		if (comparators.size() > Long.SIZE-1)
			throw new IllegalArgumentException("attributes will not fit into long");
		
		maxLevel = level;

		n = comparators.size();// the number of indices we go through
		long instanceLen = 0;
		int prev_section_size=1;
		if (n<=maxLevel)
			throw new IllegalArgumentException("too many levels for the considered number of attributes");
			
		for(int currentLevel=0;currentLevel<=maxLevel;++currentLevel)
		{
			final int attrCountAtThisLevel = (n-currentLevel);// at the top level, we consider min/max of n attributes, at level 1 it is n-1 attribute. This shows that at each level, we have one less attribute to consider. Hence at level i, we split into (n-i) values.
			final int currentSectionSize = prev_section_size*attrCountAtThisLevel*(currentLevel == 0?1:V);// at each level, we have V*attrCountAtThisLevel parts for each element of the previous level, except for the first level.

			instanceLen+=currentSectionSize*attrMult;
			if (instanceLen > Integer.MAX_VALUE)
				throw new IllegalArgumentException("too many attributes per instance");
			prev_section_size = currentSectionSize;
		}
		instanceLength = (int)instanceLen;

		boolean [] uniqueArray = new boolean[instanceLength];
		attributes = new ArrayList<Attribute>(instanceLength+1);
		attributesOfAnInstance = new Attribute[instanceLength];
		fillInAttributeNames(attributesOfAnInstance,0,0,1,0,"",0,uniqueArray);
		for(int i=0;i<instanceLength;++i)
			if (!uniqueArray[i])
				throw new IllegalArgumentException("entry "+i+" was not filled in");
		for(int i=0;i<instanceLength;++i) attributes.add(attributesOfAnInstance[i]);
		attributes.add(classAttribute);
		trainingData = new Instances(trainingSetName,attributes,capacity);
		trainingData.setClass(classAttribute);
		
		fillinMask = new int[instanceLength];
		Arrays.fill(fillinMask, fillin_FILL);
	}

	public void setBlock(int attr)
	{
		fillinMask[attr]=fillin_MASKED;
	}
	
	public int [] getMask()
	{
		int outcome[] = new int[fillinMask.length];System.arraycopy(fillinMask, 0, outcome, 0, fillinMask.length);return outcome;
	}
	
	/** Fills in the names attributes for the current level and position in the level, recursing across all the positions in the next level.
	 * 
	 * @param whatToFillIn array where data is to be stored.
	 * @param section_start the buffer for the current level will start at this offset, in units of int (or whatever the type of buffer is).
	 * @param idx_in_section we iterate over attributes, then recurse, then again and so on. This reflects an offset from section_start we are at, in units of attrMult.
	 * The reason to have both section_start and idx_in_section rather than a combined offset is that we need to know where the next section should start and for this we need the 
	 * start of the current section (or we'll have to compute it based on currentLevel).
	 * @param prev_section_size the size of the previous level in terms of attrMult. Current level is all that multiplied by prev_section_size*V*(n-currentLevel) 
	 * because for each entry at the previous level we need to have +1 and -1 entries of (n-currentLevel) attributes at the current level. Hence multiplying by V*(n-currentLevel). 
	 * @param xyz bitmask of the attributes used in the previous levels.
	 * @param pathToThisLevel the string reflecting the condition for the position at this level to be active.
	 * @param currentLevel current level in construction of the attributes.
	 * @param checkUniqueness only used for testing, an array to verify that computation of indices is hitting unique cells.
	 */
	protected void fillInAttributeNames(Attribute[] whatToFillIn, int section_start, int idx_in_section, int prev_section_size, long xyz, String pathToThisLevel, int currentLevel, boolean checkUniqueness[])
	{
		final int attrCountAtThisLevel = (n-currentLevel);
		final int sectionPlusOffset = attrMult*(section_start + idx_in_section);
		final int currentSectionSize = prev_section_size*attrCountAtThisLevel*(currentLevel == 0?1:V);

		assert idx_in_section >= 0 && idx_in_section < currentSectionSize;

		int i=0;
		for(int attr=0;attr<n;++attr)
			if ( ((1 << attr) & xyz) == 0) // if the attribute has not been already used
			{
				//System.out.println("level "+currentLevel+" start="+section_start+" offset="+idx_in_section+" filling cmp attribute "+attr+" final offset is "+(sectionPlusOffset+i));
				PairComparator cmp = comparators.get(attr);
				final int offsetOfCmp = sectionPlusOffset+i;
				whatToFillIn[offsetOfCmp]=cmp.getAttribute().copy(pathToThisLevel+cmp.getAttribute().name());

				if (checkUniqueness != null)
				{
					if (checkUniqueness[offsetOfCmp])
						throw new IllegalArgumentException("duplicate assignment to cell "+(offsetOfCmp));
					checkUniqueness[offsetOfCmp]=true;
				}

				if (attrMult > 1)
				{
					PairRank pr = assessors.get(attr);
					//System.out.println("level "+currentLevel+" start="+section_start+" offset="+idx_in_section+" filling SD  attribute "+attr+" final offset is "+(sectionPlusOffset+attrCountAtThisLevel+i));
					final int offsetOfSD = sectionPlusOffset+attrCountAtThisLevel+i;
					whatToFillIn[sectionPlusOffset+attrCountAtThisLevel+i]=pr.getAttribute().copy(pathToThisLevel+pr.getAttribute().name());

					if (checkUniqueness != null)
					{
						if (checkUniqueness[offsetOfSD])
							throw new IllegalArgumentException("duplicate assignment to cell "+(offsetOfSD));
						checkUniqueness[offsetOfSD]=true;
					}
				}
				++i;
			}
		if (currentLevel < maxLevel)
		{
			i=0;
			for(int attr=0;attr<n;++attr)
			{
				long positionalBit = 1 << attr;
				if ((xyz & positionalBit) == 0) // this attribute was not already used on a path to the current instance of fillInEntry
				{
					int newOffset = (idx_in_section+i)*V*(attrCountAtThisLevel-1);
					fillInAttributeNames(whatToFillIn,section_start+currentSectionSize,newOffset,currentSectionSize, xyz|positionalBit,pathToThisLevel+" if "+comparators.get(attr).getAttribute().name()+"==-1 then ",currentLevel+1,checkUniqueness);
					fillInAttributeNames(whatToFillIn,section_start+currentSectionSize,newOffset+(attrCountAtThisLevel-1),currentSectionSize, xyz|positionalBit,pathToThisLevel+" if "+comparators.get(attr).getAttribute().name()+"==1 then ",currentLevel+1,checkUniqueness);
					++i;
				}
			}
			assert i == attrCountAtThisLevel;
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
	
	protected boolean useDenseInstance = true;
	
	public void setUseDenseInstance(boolean value)
	{
		useDenseInstance = value;
	}
	
	public double []constructInstanceValuesBasedOnComparisonResults(int []comparisonResults, boolean classification)
	{
		double []instanceValues=new double[instanceLength+1];
		for(int i=0;i<instanceLength;++i)
			instanceValues[i]=convertAssessmentResultToString(comparisonResults[i],attributesOfAnInstance[i]);
		instanceValues[instanceLength]=trainingData.classAttribute().indexOfValue(Boolean.toString(classification));
		return instanceValues;
	}
	
	/** Constructs a Weka {@link Instance} for a pair of interest.
	 * 
	 * @param comparisonResults metrics related to the considered pair.
	 * @param classification whether this is a correct pair
	 * @return an instance of a test or a training sample. 
	 */
	Instance constructInstance(int []comparisonResults, boolean classification)
	{
		if (comparisonResults.length != instanceLength)
			throw new IllegalArgumentException("results' length does not match the number of comparators");

		double []instanceValues=constructInstanceValuesBasedOnComparisonResults(comparisonResults, classification);		
		Instance outcome = useDenseInstance?new DenseInstance(1,instanceValues):new BinarySparseInstance(1,instanceValues);
		outcome.setDataset(trainingData);
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
	
	final Attribute classAttribute;
	public Instances trainingData;
	
	static final String MINUSTWO="-2";
	static final String MINUSONE="-1";
	static final String ZERO="0";
	static final String ONE="1";
	static final String TWO="2";

	protected List<PairComparator> comparators;
	protected List<PairRank> assessors;
	
	protected final MarkovHelper markovHelper;
	
	class MeasurementsForCollectionOfPairs
	{
		Map<StatePair,FilteredPairMeasurements> measurementsForComparators=new HashMap<StatePair,FilteredPairMeasurements>();
		double valueAverage[]=new double[0], valueSD[]=new double[0];
	}
	Map<StatePair,PairMeasurements> measurementsObtainedFromPairs = new HashMap<StatePair,PairMeasurements>();
	
	Map<CmpVertex,Integer> treeForComparators = new TreeMap<CmpVertex,Integer>();
	
	MeasurementsForCollectionOfPairs measurementsForFilteredCollectionOfPairs = new MeasurementsForCollectionOfPairs();
	
	LearnerGraph tentativeGraph = null;
	
	void buildSetsForComparators(Collection<PairScore> pairs, LearnerGraph graph)
	{
		treeForComparators.clear();
		tentativeGraph = graph;
		
		// First, we obtain measurements that only depend on a specific pair of states, regardless of other pairs in a collection of pairs. 
		for(PairScore pair:pairs)
		{
			if (!treeForComparators.containsKey(pair.getQ()))
				treeForComparators.put(pair.getQ(),PairQualityLearner.computeTreeSize(graph, pair.getQ()));

			PairMeasurements m = new PairMeasurements();
			ScoreMode origScore = tentativeGraph.config.getLearnerScoreMode();tentativeGraph.config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);
			if (dataCollectorParameters.graphIsPTA)
				m.compatibilityScore = tentativeGraph.pairscores.computePairCompatibilityScore(pair);
			if (markovHelper!= null)
				m.inconsistencyScore = markovHelper.computeScoreBasedOnInconsistencies(pair);
			tentativeGraph.config.setLearnerScoreMode(origScore);
			measurementsObtainedFromPairs.put(pair,m);
		}

		// Second, obtain measurements that depend on other pairs in a collection of pairs.  
		buildSetsForComparatorsDependingOnFiltering(measurementsForFilteredCollectionOfPairs,pairs);
	}
	
	/** Given a collection of pairs and a tentative automaton, constructs auxiliary structures used by comparators and stores it as an instance variable.
	 * The graph used for construction is the one that was passed earlier to {@link WekaDataCollector#buildSetsForComparators(Collection, LearnerGraph)}.
	 * @param measurements where to store the result of measurement.
	 * @param pairs pairs to build sets for.
	 */
	void buildSetsForComparatorsDependingOnFiltering(MeasurementsForCollectionOfPairs measurements, Collection<PairScore> pairs)
	{
		measurements.measurementsForComparators.clear();
		if (measurements.valueAverage.length < n)
			measurements.valueAverage = new double[n];
		if (measurements.valueSD.length < n)
			measurements.valueSD = new double[n];
		
		Arrays.fill(measurements.valueAverage, 0);Arrays.fill(measurements.valueSD, 0);
		
		// prepare auxiliary information that is supposed to be cached rather than recomputed by individual instances of comparators in measurementsForComparators 
		for(PairScore pair:pairs)
		{
			FilteredPairMeasurements m = new FilteredPairMeasurements();m.nrOfAlternatives=-1;
			for(PairScore p:pairs)
			{
				if (p.getR() == pair.getR())
					++m.nrOfAlternatives;
			}
			
			Collection<CmpVertex> adjacentOutgoingBlue = tentativeGraph.transitionMatrix.get(pair.getQ()).values(), adjacentOutgoingRed = tentativeGraph.transitionMatrix.get(pair.getR()).values(); 
			m.adjacent = adjacentOutgoingBlue.contains(pair.getR()) || adjacentOutgoingRed.contains(pair.getQ());
			measurementsForFilteredCollectionOfPairs.measurementsForComparators.put(pair, m);
		}			

		// now run comparators. These obtain values based both on filtered an unfiltered values.
		if (assessors != null)
		{
			for(PairScore pair:pairs)
				for(int i=0;i<assessors.size();++i)
				{
					long value = assessors.get(i).getValue(pair);
					measurements.valueAverage[i]+=value;
					measurements.valueSD[i]+=value*value;// abuse valueSD into storing squares of values.
				}

			for(int i=0;i<assessors.size();++i)
			{
				measurements.valueAverage[i]/=pairs.size();measurements.valueSD[i]=Math.sqrt(measurements.valueSD[i]/pairs.size()-measurements.valueAverage[i]*measurements.valueAverage[i]);
			}
		}
	}

	/** Used to denote a value corresponding to an "inconclusive" verdict where a comparator returns values greater for some points and less for others. */
	public static final int comparison_inconclusive=-10;
	
	/** Compares a pair with all others. 
	 * 
	 * @param badPairs we only compare the current pair with all the pairs that are 'bad'. By using all pairs here, we mimic the implementation that existed for some time; using only wrong pairs or correct pairs, one can build a classifier that intends to discriminate between good and bad pairs rather than picking top pair and hence being too selective.  
	 */ 
	int comparePairWithOthers(PairComparator cmp, PairScore pair, Collection<PairScore> badPairs)
	{
		int comparisonResult = cmp.compare(pair, pair);// first of all, compare the pair with itself. It hence does not matter whether badPairs includes this pair or not (it will for comparePairOnlyWithBadPairs == false and not otherwise).
		for(PairScore w:badPairs)
			if (w != null) // in order to permit sparse collections, we allow null-entries.
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
	 * @param xyz bitmask indicating attributes to be ignored.
	 */
	void comparePairWithOthers(PairScore pair, Collection<PairScore> badPairs, int []whatToFillIn, int offset, long xyz)
	{
		assert !comparators.isEmpty();
		
		int i=0;
		for(int attr=0;attr<n;++attr)
			if (  ((1 << attr) & xyz) == 0) // if the attribute has not been already used. Important: the number of attributes available at each level is taken into account during computation of offsets, therefore skipping this check will make the data no longer fit in the space provided.
			{
				if (fillinMask[i+offset] != fillin_MASKED)
				{
					PairComparator cmp = comparators.get(attr);
					int value = comparePairWithOthers(cmp, pair, badPairs);
					if (value == comparison_inconclusive)
						value = 0;
					whatToFillIn[i+offset] = value;
				}

				++i;
			}
	}

	/** Assesses a supplied pair based on the values.
	 * 
	 * @param pair pair to consider
	 * @param measurements set of measurements to use for assessment
	 * @param whatToFillIn array to populate with results
	 * @param offset the starting position to fill in.
	 * @param xyz bitmask indicating attributes to be ignored.
	 */
	void assessPair(PairScore pair, MeasurementsForCollectionOfPairs measurements, int []whatToFillIn, int offset, long xyz)
	{
		assert !assessors.isEmpty();
		int i=0;
		for(int attr=0;attr<n;++attr)
			if ( ((1 << attr) & xyz) == 0) // if the attribute has not been already used. Important: the number of attributes available at each level is taken into account during computation of offsets, therefore skipping this check will make the data no longer fit in the space provided.
			{
				if (fillinMask[i+offset] != fillin_MASKED)
				{
					final int value = assessors.get(attr).getRanking(pair, measurements.valueAverage[attr], measurements.valueSD[attr]);
					whatToFillIn[i+offset]=value;
				}
				++i;
			}
	}

	public static final int fillin_FILL=0, fillin_MASKED=10, fillin_NORECURSE = 11;
	/** Reflects values that are to be filled in; values marked with masked are ignored. and those marked norecurse are filled in but {@link WekaDataCollector#fillInEntry(int[], int, int, int, long, PairScore, Collection, MeasurementsForCollectionOfPairs, int)} is not called recursively on them. */
	protected int [] fillinMask;
	
	/** Fills in the values of attributes for the current level and position in the level, recursing across all the positions in the next level. 
	 * Only attributes set to zero in the mask are filled in, those that are 'masked' are ignored; 'norecurse' attributes 
	 * are recorded but fillInEntry is not called recursively on those attributes. 
	 * 
	 * @param whatToFillIn array where data is to be stored.
	 * @param section_start the buffer for the current level will start at this offset, in units of int (or whatever the type of buffer is).
	 * @param idx_in_section we iterate over attributes, then recurse, then again and so on. This reflects an offset from section_start we are at, in units of attrMult.
	 * The reason to have both section_start and idx_in_section rather than a combined offset is that we need to know where the next section should start and for this we need the 
	 * start of the current section (or we'll have to compute it based on currentLevel).
	 * @param prev_section_size the size of the previous level in terms of attrMult. Current level is all that multiplied by prev_section_size*V*(n-currentLevel) 
	 * because for each entry at the previous level we need to have +1 and -1 entries of (n-currentLevel) attributes at the current level. Hence multiplying by V*(n-currentLevel). 
	 * @param xyz bitmask of the attributes used in the previous levels.
	 * @param pairOfInterest pair being evaluated.
	 * @param pairs other pairs it is to be compared with. This can be a collection of all pairs where we intend to identify dominating pairs or only a collection of 'bad' pairs where we filter pairs dominating (or being dominated) by the wrong-merge pairs for learning of a 'positive' classifier and the other way around for a 'negative' one.
	 * @param measurements long-to-compute parameters of the pairs, used by assessors to evaluate the pairOfInterest
	 * @param currentLevel current level in construction of the attributes.
	 * @return true if any entries were filled in.
	 */
	protected void fillInEntry(int [] whatToFillIn,int section_start, int idx_in_section, int prev_section_size, long xyz, 
			PairScore pairOfInterest, Collection<PairScore> pairs,MeasurementsForCollectionOfPairs measurements, int currentLevel)
	{
		final int attrCountAtThisLevel = (n-currentLevel);
		final int sectionPlusOffset = attrMult*(section_start + idx_in_section);
		final int currentSectionSize = prev_section_size*attrCountAtThisLevel*(currentLevel == 0?1:V);
		
		assert idx_in_section >= 0 && idx_in_section < currentSectionSize;
		
		comparePairWithOthers(pairOfInterest,pairs,whatToFillIn,sectionPlusOffset,xyz);
		if (attrMult>1)
			assessPair(pairOfInterest,measurements, whatToFillIn,sectionPlusOffset+attrCountAtThisLevel,xyz);
		if (currentLevel < maxLevel)
		{
			int i=0;
			for(int attr=0;attr<n;++attr)
			{
				long positionalBit = 1 << attr;
				if ((xyz & positionalBit) == 0) // this attribute was not already used on a path to the current instance of fillInEntry
				{
					if (fillinMask[sectionPlusOffset+i] == fillin_FILL)
					{
						int attributeREL = whatToFillIn[sectionPlusOffset+i];
						if (attributeREL != 0)
						{
							assert attributeREL == 1 || attributeREL == -1;
							Collection<PairScore> others = new ArrayList<PairScore>(pairs.size());
							for(PairScore currentPair:pairs)
								if (currentPair != null)
								{// to permit the list of pairs to have holes
									int comparisonOnAttribute_attr = comparePairWithOthers(comparators.get(attr),currentPair,pairs);
									if (comparisonOnAttribute_attr == attributeREL) // we only compare our vertex with those that are also distinguished by the specified attribute
										others.add(currentPair);// here we select pairs that are either both correct and better/worse than wrong pairs or are both wrong and better/worse than correct pairs. 
								}
							
							if (others.size()>1)
							{
								MeasurementsForCollectionOfPairs measurementsForFilteredPairs = new MeasurementsForCollectionOfPairs();
								buildSetsForComparatorsDependingOnFiltering(measurementsForFilteredPairs,others);
								int newOffset = (idx_in_section+i)*V*(attrCountAtThisLevel-1);
								// the value of 2 below is a reflection that we only distinguish between two different relative values. If SD part were considered, there would be a lot more values.
								fillInEntry(whatToFillIn,section_start+currentSectionSize,newOffset+(attributeREL>0?1:0)*(attrCountAtThisLevel-1),currentSectionSize, xyz|positionalBit,pairOfInterest,others,measurementsForFilteredPairs,currentLevel+1);
							}
						}
					}
					++i;
				}
			}
			assert i == attrCountAtThisLevel;
		}
	}

	/** If all lower-level elements are not supposed to be filled in, it does not make sense to make a recursive call, hence we mark an element as 'no recurse'. Returns true if any elements have to be filled in. */
	protected boolean configureNoRecurse(int section_start, int idx_in_section, int prev_section_size, long xyz, int currentLevel)
	{
		final int attrCountAtThisLevel = (n-currentLevel);
		final int sectionPlusOffset = attrMult*(section_start + idx_in_section);
		final int currentSectionSize = prev_section_size*attrCountAtThisLevel*(currentLevel == 0?1:V);
		
		assert idx_in_section >= 0 && idx_in_section < currentSectionSize;
		boolean outcome = false;
		int attrOffset = 0;
		for(int attr=0;attr<n;++attr)
		{
			if (  ((1 << attr) & xyz) == 0 )
			{
				if (fillinMask[sectionPlusOffset+attrOffset] == fillin_FILL)
					outcome = true;
				
				++attrOffset;
			}
		}
		if (currentLevel < maxLevel)
		{
			int i=0;
			for(int attr=0;attr<n;++attr)
			{
				boolean nested = false;
				long positionalBit = 1 << attr;
				if ((xyz & positionalBit) == 0) // this attribute was not already used on a path to the current instance of fillInEntry
				{
					if (fillinMask[sectionPlusOffset+i] == fillin_FILL)
					{
						int newOffset = (idx_in_section+i)*V*(attrCountAtThisLevel-1);
						// the value of 2 below is a reflection that we only distinguish between two different relative values. If SD part were considered, there would be a lot more values.
						nested |= configureNoRecurse(section_start+currentSectionSize,newOffset+(1)*(attrCountAtThisLevel-1),currentSectionSize, xyz|positionalBit,currentLevel+1);
						nested |= configureNoRecurse(section_start+currentSectionSize,newOffset+(0)*(attrCountAtThisLevel-1),currentSectionSize, xyz|positionalBit,currentLevel+1);
						
						if (!nested)
							fillinMask[sectionPlusOffset+i] = fillin_NORECURSE;// if nothing to fill in below here, mark the entry as such.   
					}
					++i;
				}
			}
			assert i == attrCountAtThisLevel;
		}
		
		return outcome;
	}

	public void configureNoRecurse()
	{
		configureNoRecurse(0,0,1,0,0);
	}
	
	/** Fills in the array with comparison results. For correct operation, the supplied pair of interest has to be included in the collection of pairs. */
	public void fillInPairDetails(int [] whatToFillIn, PairScore pairOfInterest, Collection<PairScore> pairs)
	{
		if (whatToFillIn.length < getInstanceLength())
			throw new IllegalArgumentException("array is too short");
		fillInEntry(whatToFillIn,0,0,1,0,pairOfInterest,pairs,measurementsForFilteredCollectionOfPairs,0);
	}
	
	/** If true, only compares the pair with wrong pairs when building a 'positive' instance and with correct pairs for a 'negative' instance. If false, compares a pair with absolutely all pairs, hence utilising a rather narrow selection criteria (any correct pairs that did not happen to be 'top' are set as 'inconclusive' even though they could become 'top' in the absence of the current top-scoring pair). */
	protected boolean comparePairOnlyWithBadPairs;

	/** Given a collection of pairs from a tentative graph, this method generates Weka data instances and adds them to the Weka dataset.
	 * Pairs that have negative scores are ignored.
	 * 
	 * @param pairs pairs to add
	 * @param currentGraph the current graph
	 * @param correctGraph the graph we are trying to learn by merging states in tentativeGraph.
	 */
	public void updateDatasetWithPairsOrig(Collection<PairScore> pairs, LearnerGraph currentGraph, LearnerGraph correctGraph)
	{
		buildSetsForComparators(pairs,currentGraph);
		
		List<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
		List<PairScore> pairsToConsider = new LinkedList<PairScore>();
		if (!pairs.isEmpty())
		{
			for(PairScore p:pairs) if (p.getQ().isAccept() && p.getR().isAccept()) pairsToConsider.add(p);// only consider non-negatives
		}
		LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(currentGraph, correctGraph, pairsToConsider, correctPairs, wrongPairs);
		
		for(PairScore p:pairsToConsider)
		{
			int []comparisonResults = new int[instanceLength];
			boolean correctPair = correctPairs.contains(p);
			if (comparePairOnlyWithBadPairs)
			{// If we leave lists of wrong/correct pairs unchanged, not only comparison but also subsequent filtering (if p is best or worst in some attribute) will apply to 'wrong' pairs. 
			 // This implies that the pair will not be compared with itself and hence where the list of wrong pairs only has one element, it will be ignored unless (other.size() > 0 && comparePairOnlyWithBadPairs) 
		     // is added to (others.size()>1) condition in fillInEntry. 
				
				if (correctPair)
				{
					List<PairScore> badPairs = new ArrayList<PairScore>(wrongPairs.size()+1);badPairs.addAll(wrongPairs);badPairs.add(p);
					fillInPairDetails(comparisonResults,p, badPairs);// this is the correct pair, hence compare it to all the 'wrong' pairs.
				}
				else
				{
					List<PairScore> badPairs = new ArrayList<PairScore>(wrongPairs.size()+1);badPairs.addAll(correctPairs);badPairs.add(p);
					fillInPairDetails(comparisonResults,p, badPairs);// this is the wrong pair, hence compare it to all the 'correct' pairs.
				}
			}
			else
				fillInPairDetails(comparisonResults,p, pairsToConsider);
			trainingData.add(constructInstance(comparisonResults, correctPair));
		}
	}
	
	/** Returns next pair that has the most number of attributes set to non-zero. 
	 * 
	 * @param currentPairs the collection of current pairs. Upon return, the entry for the returned pair will be set to null.
	 * @param comparisonResults worker array, to avoid reallocation.
	 * @param bestComparisonResults contains details for the returned pair and can be directly used to construct an {@link Instance}.
	 * @return pair that has the most non-zero attributes of all the pairs.
	 */
	public PairScore pickNextMostNonZeroPair(List<PairScore> currentPairs, int []comparisonResults, int []bestComparisonResults)
	{
		int bestMatch = attributes.size();int bestPair = -1;
		for(int i=0;i<currentPairs.size();++i)
		{
			PairScore p = currentPairs.get(i);
			if (p != null)
			{
				fillInPairDetails(comparisonResults,p, currentPairs);
				int cnt=0;
				for(int a=0;a<comparisonResults.length;++a)
					if (comparisonResults[a] == 0)
						cnt++;
				if (cnt<bestMatch)
				{
					bestMatch = cnt;bestPair = i;System.arraycopy(comparisonResults, 0, bestComparisonResults, 0, comparisonResults.length);
				}
			}
		}
		PairScore currentBest = currentPairs.get(bestPair);
		currentPairs.set(bestPair,null);// 'remove' the current pair from the list.
		return currentBest;
	}
	
	/** Given a collection of pairs from a tentative graph, this method generates Weka data instances and adds them to the Weka dataset.
	 * Pairs that have negative scores are ignored.
	 * 
	 * @param pairs pairs to add
	 * @param currentGraph the current graph
	 * @param correctGraph the graph we are trying to learn by merging states in tentativeGraph.
	 */
	public void updateDatasetWithPairs(Collection<PairScore> pairs, LearnerGraph currentGraph, LearnerGraph correctGraph)
	{
		buildSetsForComparators(pairs,currentGraph);
		
		List<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
		List<PairScore> currentPairs = new ArrayList<PairScore>(pairs.size());
		for(PairScore p:pairs) 
			if (p.getQ().isAccept() && p.getR().isAccept()) currentPairs.add(p);// only consider non-negatives

		LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(currentGraph, correctGraph, currentPairs, correctPairs, wrongPairs);
		
		int pairsLeft = currentPairs.size();
		while(pairsLeft > currentPairs.size()*dataCollectorParameters.fraction_of_pairs_to_ignore)
		{
			int []comparisonResults = new int[instanceLength], bestComparisonResults = new int[instanceLength];
			PairScore nextBest = pickNextMostNonZeroPair(currentPairs, comparisonResults, bestComparisonResults);

			boolean correctPair = correctPairs.contains(nextBest);
			trainingData.add(constructInstance(bestComparisonResults, correctPair));

			--pairsLeft;
		}
	}

	/** Provides helper methods in order to train a classifier to recognise good/bad pairs
	 * <hr/>
	 * It is a nested class to permit access to instance variables. This seems natural because elements of this class need access to data obtained from the transition matrix. 
	 */
	public abstract class PairRankingSupport
	{
		/** Weka attribute associated with this comparator. */
		final Attribute att;
		
		public Attribute getAttribute()
		{
			return att;
		}
		
		/** Constructs a nominal attribute using the provided values.
		 * @param name attribute name
		 * @param range possible values for the attribute. 
		 */
		protected PairRankingSupport(String name, String [] range)
		{
			att = new Attribute(name,Arrays.asList(range));
		}

		/** Constructs a numeric attribute using the provided values.
		 * @param name attribute name
		 */
		protected PairRankingSupport(String name)
		{
			att = new Attribute(name);
		}

		@Override
		public String toString()
		{
			return att.name();
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
			return LearningSupportRoutines.signum(assessor.getValue(o1) - assessor.getValue(o2));
		}
	}
	
	/** {@link PairComparator} permits one to compare pairs with each other. This one aims to give a rank to each pair in a collection of pairs, by either retrieving specific attributes or 
	 * doing the average/standard deviation thresholding.
	 */
	public abstract class PairRank extends PairRankingSupport
	{
		protected PairRank(String name)
		{
			super(name, new String[]{ZERO,ONE,MINUSONE,TWO,MINUSTWO});
		}
		
		
		/** Returns 0,1,2 or -1,-2 depending on how the pair scores compared to an average across the collection of pairs, 
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
