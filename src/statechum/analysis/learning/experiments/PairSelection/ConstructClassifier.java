/* Copyright (c) 2016 The University of Sheffield
 * 
 * This file is part of StateChum.
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
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper;
import statechum.Pair;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
import statechum.analysis.learning.DrawGraphs.ScatterPlot;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovHelper;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovHelperClassifier;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovParameters;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.LearnerThatCanClassifyPairs;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DataCollectorParameters;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUpdatesWekaResults;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import weka.classifiers.AbstractClassifier;
import weka.classifiers.Classifier;
import weka.core.Attribute;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.Utils;

public class ConstructClassifier 
{
	
	public static class PosNeg implements Serializable
	{
		/**
		 * ID for serialisation
		 */
		private static final long serialVersionUID = -5672965229484999993L;
		double aboveZero=0,zero=0;

		public PosNeg(double aZ,double z)
		{
			aboveZero = aZ;zero = z;
		}
		
		@Override
		public String toString()
		{
			return "PosNeg("+aboveZero+","+zero+")";
		}
		
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			long temp;
			temp = Double.doubleToLongBits(aboveZero);
			result = prime * result + (int) (temp ^ (temp >>> 32));
			temp = Double.doubleToLongBits(zero);
			result = prime * result + (int) (temp ^ (temp >>> 32));
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			PosNeg other = (PosNeg) obj;
			//if (Double.doubleToLongBits(aboveZero) != Double.doubleToLongBits(other.aboveZero))
			if (Math.abs(aboveZero-other.aboveZero) > Configuration.fpAccuracy)
				return false;
			if (Math.abs(zero-other.zero) > Configuration.fpAccuracy)
				return false;
			return true;
		}
	}

	public static abstract class NominalAttributeClassifier extends AbstractClassifier
	{
		/**
		 *  ID for serialisation
		 */
		private static final long serialVersionUID = -8407270538306744387L;
		
		protected int attrValueForTrue,attrValueForFalse;
		
		/** Returns the position of the 'true' value in the distribution returned by {@link ConstructClassifier#distributionForInstance()}
		 */
		public int posForTrue()
		{
			return attrValueForTrue;
		}
		
		/** Returns the position of the 'false' value in the distribution returned by {@link ConstructClassifier#distributionForInstance()}
		 */
		public int posForFalse()
		{
			return attrValueForFalse;
		}
		
		protected static int getIndexFor(Attribute classAttr, String forWhat)
		{
			for(int i=0;i<classAttr.numValues();++i)
				if (classAttr.value(i).equalsIgnoreCase(forWhat))
					return i;
			
			throw new IllegalArgumentException("failed to fine value for \""+forWhat+"\"");
		}

		public abstract PosNeg classifyInstanceAsPosNeg(Instance instance) throws Exception;
		
		@Override
		public double classifyInstance(Instance instance) throws Exception 
		{
			PosNeg values = classifyInstanceAsPosNeg(instance);
			if (values.aboveZero> values.zero)
				return 1.0;
			else
				return 0.0;
		}
		
		@Override
		public double[] distributionForInstance(Instance instance) throws Exception 
		{
			PosNeg values = classifyInstanceAsPosNeg(instance);
			double[] outcome = new double[]{values.zero,values.aboveZero};
			return outcome;			
		}
	}
	
	public static class PickBestAttributeClassifier extends NominalAttributeClassifier
	{
		/**
		 * Value for serialisation.
		 */
		private static final long serialVersionUID = 469870791295294824L;

		public static boolean isInstanceAccept(Instance instance) 
		{
			return Boolean.parseBoolean(instance.stringValue(instance.classIndex()));
		}

		/** Attribute that is best for learning. */
		int bestAttribute = 0;
		/** Whether the positive value of the attribute should be seen as a predictor for a classifier returning a positive value of an attribute. */
		boolean positivePredictsAboveZero;
		
		private PosNeg attributeDataPos, attributeDataNeg;
		private PosNeg attributeClassificationUnknown = new PosNeg(0.5,0.5);
		
		/** We would like a classifier that returns true/false most of the time. 
		 * This is the maximal %% of instances where a classifier is permitted to say 'unknown'. 
		 * Attributes where WekaDataCollector.ZERO account for more than this percentage are ignored.
		 */
		double percentageInstancesIgnoredByAttribute=1.0;//0.3;
		
		
		private static boolean attributeValuePositive(String value)
		{
			return value == WekaDataCollector.ONE || value == WekaDataCollector.TWO;
		}
		
		private static boolean attributeValueNegative(String value)
		{
			return value == WekaDataCollector.MINUSONE || value == WekaDataCollector.MINUSTWO;
		}
		
		@Override
		public void buildClassifier(Instances data) throws Exception 
		{
			Attribute classAttr = data.attribute(data.classIndex());
			if (classAttr.numValues() != 2)
				throw new IllegalArgumentException("class should have exactly two values, {true,false}");
			attrValueForTrue = getIndexFor(classAttr,"true");attrValueForFalse = getIndexFor(classAttr,"false");

			PosNeg classificationPos[]=new PosNeg[data.numAttributes()];// counts the number of positive values of the attribute that match a specific classification.
			PosNeg classificationNeg[]=new PosNeg[data.numAttributes()];// counts the number of negative values of the attribute that match a specific classification.
			for(int attr=0;attr<data.numAttributes();++attr)
			{
				classificationPos[attr]=new PosNeg(0,0);classificationNeg[attr]=new PosNeg(0,0);
			}
			
			for(int instanceid=0;instanceid < data.numInstances();++instanceid)
			{
				Instance instance = data.get(instanceid);
				for(int attr=0;attr<data.numAttributes();++attr)
				{
					String value = instance.stringValue(attr);
					
					if (attributeValuePositive(value))
					{
						if (instance.classValue() > 0)
							++classificationPos[attr].aboveZero;
						else
							++classificationPos[attr].zero;
					}
					else
						if (attributeValueNegative(value))
						{
							if (instance.classValue() > 0)
								++classificationNeg[attr].aboveZero;
							else
								++classificationNeg[attr].zero;
						}
						
				}
			}
			double bestPerformance = -1;
			final double numNonZeroInstancesToConsiderAttribute = data.numInstances()*(1-percentageInstancesIgnoredByAttribute);
			// With information for all the instances collected, we pick an attribute that is likely to perform the best.
			for(int attr=0;attr<data.numAttributes();++attr)
			{
				PosNeg pos = classificationPos[attr], neg = classificationNeg[attr];
				double performance = 0;
				if (pos.aboveZero > pos.zero)
					performance = Math.min(ConfusionMatrix.divide(pos.aboveZero, pos.aboveZero+pos.zero),ConfusionMatrix.divide(neg.zero, neg.aboveZero+neg.zero));
				else
					performance = Math.min(ConfusionMatrix.divide(pos.zero, pos.aboveZero+pos.zero),ConfusionMatrix.divide(neg.aboveZero, neg.aboveZero+neg.zero));
				
				if (performance > bestPerformance && pos.aboveZero+pos.zero+neg.aboveZero+neg.zero > numNonZeroInstancesToConsiderAttribute)
				{
					bestPerformance = performance; 
					bestAttribute = attr;
					attributeDataPos = pos;attributeDataNeg = neg;
					if (pos.aboveZero > pos.zero)
						positivePredictsAboveZero = true;
				}
			}
			if (bestPerformance <= 0)
				throw new IllegalArgumentException("invalid training data - the classifier could not pick an attribute that is good enough");
			System.out.println("PickBestAttributeClassifier: best attr is "+data.attribute(bestAttribute).name()+" with performance at "+bestPerformance);
		}

		@Override
		public PosNeg classifyInstanceAsPosNeg(Instance instance) throws Exception 
		{
			String value = instance.stringValue(bestAttribute);
			if (attributeValuePositive(value))
				return attributeDataPos;
			else
				if (attributeValueNegative(value))
					return attributeDataNeg;
				else
					return attributeClassificationUnknown;
		}
		
	}
	
	public static class NearestClassifier extends NominalAttributeClassifier
	{
		/**
		 * Value for serialisation.
		 */
		private static final long serialVersionUID = -3248779287355047358L;
		
		/** Bit-string representation of data. In order to accelerate computations, we denote individual values by separate bits, so -1/1 is 01 v.s. 10 and -2/+2 are 1000 and 0001.
		 * This permits bitwise AND operations to compare values and hence countBits can be used to find the number of matches. 
		 * Given that sizeof(long)==64, one long represents 16 attributes. 21000 attributes would be represented with around 1300 values.
		 * 21000 attributes and 0.25 mil data points is 2.6GB of data. This can scale to 1 million entries with 21000 attributes (10.5GB allocated by JVM) 
		 * before turning array size negative and failing allocation.
		 */
		long trainingData[];
		
		PosNeg trainingDistribution[];
		int instanceSize;
		int instanceNumber;
		int classAttrInTrainingDataArray;
		public static long valueToBits(String value)
		{
			if (value == WekaDataCollector.ZERO)
				return 0x0;
			else // this ordering of values makes it possible to predict 
				if (value == WekaDataCollector.MINUSONE)
					return 0x2;
				else
					if (value == WekaDataCollector.ONE)
						return 0x4;
					else
						if (value == WekaDataCollector.MINUSTWO)
							return 0x8;
						else
							if (value == WekaDataCollector.TWO)
								return 0x10;
							else
								throw new IllegalArgumentException("invalid attribute value "+value);
		}

		final static int bitsPerAttribute = 5;
		final static int bitMask = (1 << bitsPerAttribute)-1;
		final static int attributesPerLong = 64/bitsPerAttribute;

		public static boolean isInstanceAccept(Instance instance) 
		{
			return Boolean.parseBoolean(instance.stringValue(instance.classIndex()));
		}

		/** Turns instance into a bitstring. */
		protected void fillInArray(long []array, int offset, Instance instance)
		{
			int classIndex = instance.classIndex();
			int attrPos = 0;
			for(int attr=0;attr<instance.numAttributes();++attr)
				if (attr != classIndex)
				{
					array[offset+(attrPos/attributesPerLong)] |= valueToBits(instance.stringValue(attr)) << (bitsPerAttribute* (attrPos % attributesPerLong) );
					++attrPos;
				}
			//if (fillInClassification)
			//	array[offset+(classAttrInTrainingDataArray/attributesPerLong)] |= (instance.classValue()>0?1:0) << (bitsPerAttribute* (classAttrInTrainingDataArray & bitMask) );
		}
		
		public int getTrainingSize()
		{
			return instanceNumber;
		}
				
		/** Contains the least upper bound on the number of matched attributes where the result of match has no ambiguity. */
		long thresholdForUnambiguousResults = 0;
		
		public long getThresholdForUnambiguousResults()
		{
			return thresholdForUnambiguousResults;
		}
		
		@Override
		public void buildClassifier(Instances data) throws Exception 
		{
			instanceSize = (data.numAttributes() + attributesPerLong-1)/attributesPerLong;
			classAttrInTrainingDataArray = data.numAttributes()-1;thresholdForUnambiguousResults = 0;
			
			
			Attribute classAttr = data.attribute(data.classIndex());
			if (classAttr.numValues() != 2)
				throw new IllegalArgumentException("class should have exactly two values, {true,false}");
			attrValueForTrue = getIndexFor(classAttr,"true");attrValueForFalse = getIndexFor(classAttr,"false");
			
			Map<List<Long>,PosNeg> instanceToInteger = new HashMap<List<Long>,PosNeg>();
			
			for(int i=0;i<data.numInstances();++i)
			{
				Instance instance = data.instance(i);
				long [] instanceData=new long[instanceSize];fillInArray(instanceData, 0, instance);
				List<Long> instanceDataAsList=new ArrayList<Long>(instanceSize);for(long iv:instanceData) instanceDataAsList.add(iv);
				PosNeg value = instanceToInteger.get(instanceDataAsList);
				if (value == null)
				{
					value = new PosNeg(0,0);instanceToInteger.put(instanceDataAsList,value);
				}
				if (instance.classValue() > 0)
					value.aboveZero++;
				else
					value.zero++;
			}
			
			if (!instanceToInteger.isEmpty())
			{
				instanceNumber = instanceToInteger.size();
				if (instanceNumber*(long)instanceSize > Integer.MAX_VALUE)
					throw new IllegalArgumentException("training data will not fit in the array");
				trainingData = new long[instanceSize*instanceNumber];trainingDistribution=new PosNeg[instanceNumber];
				int instanceCnt=0;
				for(Entry<List<Long>,PosNeg> v:instanceToInteger.entrySet())
				{
					for(int i=0;i<instanceSize;++i)
						trainingData[instanceCnt*instanceSize+i]=v.getKey().get(i);
					PosNeg value = v.getValue();
					
					int cnt=0;
					for(int a=0;a<instanceSize;++a)
						cnt+=Long.bitCount(trainingData[instanceCnt*instanceSize+a]);
					if (value.aboveZero > 0 && value.zero > 0 && cnt > thresholdForUnambiguousResults)
						thresholdForUnambiguousResults = cnt;
					
					double sum=value.aboveZero+value.zero;
					if (sum>0)
					{
						value.aboveZero/=sum;value.zero/=sum;
					}
					trainingDistribution[instanceCnt]=value;

					++instanceCnt;
				}
			}
		}
		
		public class PosNegAndInstanceId extends PosNeg
		{
			/**
			 * ID for serialisation
			 */
			private static final long serialVersionUID = -3991150475258972694L;

			public PosNegAndInstanceId(PosNeg p, int idx, int cnt, int cntInstance) {
				super(p.aboveZero, p.zero);
				bestInstanceIdx = idx;bestCount = cnt;cntForInstance = cntInstance;
			}
			
			public int bestInstanceIdx, bestCount, cntForInstance;
		}
		
		public PosNegAndInstanceId classifyInstanceAsPosNegAndInstance(Instance instance) throws Exception 
		{
			long instanceAsBitString[] = new long[instanceSize];
			fillInArray(instanceAsBitString, 0, instance);
			int currentBestInstanceIdx = -1, currentBestCount=-1;
			for(int i=0;i<instanceNumber;++i)
			{
				int cnt=0;
				for(int a=0;a<instanceSize;++a)
					cnt+=Long.bitCount(instanceAsBitString[a] & trainingData[i*instanceSize+a]);
				if (cnt > currentBestCount)
				{
					currentBestCount = cnt;currentBestInstanceIdx = i;
				}
			}
			int cntForInstance=0;
			for(int a=0;a<instanceSize;++a)
				cntForInstance+=Long.bitCount(instanceAsBitString[a]);
				
			if (currentBestInstanceIdx < 0)
				throw new IllegalArgumentException("missing data");
			return new PosNegAndInstanceId(trainingDistribution[currentBestInstanceIdx],currentBestInstanceIdx,currentBestCount,cntForInstance);
		}

		@Override
		public PosNeg classifyInstanceAsPosNeg(Instance instance) throws Exception 
		{
			return //classifyInstanceAsPosNegInexact(instance);
				//	classifyInstanceAsPosNegExact(instance);
					classifyInstanceAsPosInexactWithThreshold(instance);
		}
		
		/** Permits matches where an instance of training data could have had more or less bits set compared to the instance being classified. */
		public PosNeg classifyInstanceAsPosNegInexact(Instance instance) throws Exception 
		{
			return  classifyInstanceAsPosNegAndInstance(instance);			
		}

		/** Permits matches where an instance of training data could have had more bits set compared to the instance being classified. */
		public PosNeg classifyInstanceAsPosNegExact(Instance instance) throws Exception 
		{
			PosNegAndInstanceId pnid = classifyInstanceAsPosNegAndInstance(instance);
			if (pnid.cntForInstance == pnid.bestCount)
				return pnid;
			return new PosNeg(0.5,0.5);
		}

		/** Permits matches where an instance of training data could have had more bits set compared to the instance being classified. */
		public PosNeg classifyInstanceAsPosInexactWithThreshold(Instance instance) throws Exception 
		{
			PosNegAndInstanceId pnid = classifyInstanceAsPosNegAndInstance(instance);
			if (pnid.cntForInstance == pnid.bestCount)
				return pnid;
			
			double multiplier = Math.exp( -3.0*((double)pnid.cntForInstance - pnid.bestCount)/pnid.cntForInstance );
			PosNeg value = new PosNeg(pnid.aboveZero*multiplier,pnid.zero*multiplier);
			if (value.aboveZero < 0.45 && value.zero < 0.45)
				return new PosNeg(0.5,0.5);
			return value;
		}

		
		public String classifyInstanceString(Instance instance) throws Exception
		{
			return instance.classAttribute().value((int)classifyInstance(instance));
		}
		
		public boolean classifyInstanceBoolean(Instance instance) throws Exception
		{
			return Boolean.parseBoolean(classifyInstanceString(instance));
		}
		

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + attrValueForFalse;
			result = prime * result + attrValueForTrue;
			result = prime * result + classAttrInTrainingDataArray;
			result = prime * result + instanceNumber;
			result = prime * result + instanceSize;
			result = prime * result + Arrays.hashCode(trainingData);
			result = prime * result + Arrays.hashCode(trainingDistribution);
			return result;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (!(obj instanceof NearestClassifier)) {
				return false;
			}
			NearestClassifier other = (NearestClassifier) obj;
			if (attrValueForFalse != other.attrValueForFalse) {
				return false;
			}
			if (attrValueForTrue != other.attrValueForTrue) {
				return false;
			}
			if (classAttrInTrainingDataArray != other.classAttrInTrainingDataArray) {
				return false;
			}
			if (instanceNumber != other.instanceNumber) {
				return false;
			}
			if (instanceSize != other.instanceSize) {
				return false;
			}
			if (!Arrays.equals(trainingData, other.trainingData)) {
				return false;
			}
			if (!Arrays.equals(trainingDistribution, other.trainingDistribution)) {
				return false;
			}
			return true;
		}
	}
	
	public static class EvaluationOfClassifierOutcome
	{
		public final double positive, negative,unknown;
		
		public EvaluationOfClassifierOutcome()
		{
			positive = 0;negative=0;unknown=0;
		}
		public EvaluationOfClassifierOutcome(double pos,double neg, double unk)
		{
			positive = pos;negative=neg;unknown=unk;
		}
		
		@Override
		public String toString()
		{
			return "Positive: "+positive+" Negative: "+negative+" Unknown:"+unknown;
		}
	}
	
	public static EvaluationOfClassifierOutcome evaluateClassifier(Classifier classifier, WekaDataCollector data) throws Exception
	{
		Instances ninetyPercent = new Instances(data.trainingData), evaluation = new Instances("ninety percent",data.getAttributes(),data.trainingData.numInstances());
		evaluation.setClass(ninetyPercent.classAttribute());
		int nrToRemove = data.trainingData.numInstances()/10;
		Random rnd = new Random(1);
		for(int i=0;i<nrToRemove;++i)
		{
			int elemToRemove = rnd.nextInt(ninetyPercent.numInstances());
			Instance instanceToRemove = ninetyPercent.instance(elemToRemove);
			if (instanceToRemove != null)
			{
				evaluation.add(instanceToRemove);
				ninetyPercent.delete(elemToRemove);
			}
		}
		System.out.println("training data: "+ninetyPercent.numInstances()+", evaluation data: "+evaluation.numInstances());
		return evaluateClassifierUsingSpecifiedEvaluationData(classifier, ninetyPercent, evaluation);
	}
	
	public static void evaluateClassifierOnDataFromIndividualInstances(Classifier classifier, List<Instances> data) throws Exception
	{
		Instances firstInstance = data.get(0);
		ArrayList<Attribute> attributes = new ArrayList<Attribute>(firstInstance.numAttributes());
		for(int i=0;i<firstInstance.numAttributes();++i) attributes.add(firstInstance.attribute(i));
		int totalInstances = 0;for(Instances i:data) totalInstances+=i.numInstances();
		for(int i=0;i<data.size();++i)
		{
			Instances trainingData = new Instances("trainingData",attributes,totalInstances), evaluation = data.get(i);
			trainingData.setClass(firstInstance.classAttribute());
			for(int j=0;j<data.size();++j)
				if (i != j)
					trainingData.addAll(data.get(j));
			System.out.println("Evaluation "+i+"/"+data.size()+" "+evaluateClassifierUsingSpecifiedEvaluationData(classifier,trainingData,evaluation));
		}
	}
	
	public static EvaluationOfClassifierOutcome evaluateClassifierUsingSpecifiedEvaluationData(Classifier classifier, Instances trainingSet, Instances evaluationSet) throws Exception
	{
		classifier.buildClassifier(trainingSet);return evaluateSpecificClassifierUsingSpecifiedEvaluationData(classifier,evaluationSet);
	}
	
	public static EvaluationOfClassifierOutcome evaluateSpecificClassifierUsingSpecifiedEvaluationData(Classifier classifier, Instances evaluationSet) throws Exception
	{
		/*
		for(int i=0;i<ninetyPercent.numInstances();++i)
		{
			Instance instance = ninetyPercent.instance(i);
			assert Math.abs(instance.classValue()-classifier.classifyInstance(instance)) < Configuration.fpAccuracy : "invalid classification of instance "+i;
		}
		*/
		long correctPredictionPos = 0, correctPredictionNeg = 0, totalPos = 0, totalNeg = 0, unknownValues = 0;
		for(int i=0;i<evaluationSet.numInstances();++i)
		{
			Instance instance = evaluationSet.instance(i);
			
			double [] distribution = classifier.distributionForInstance(instance);
			assert distribution.length == 2;
			//int classification = (int)classifier.classifyInstance(instance);
			int value = (int)instance.classValue();
			int indexOfTrue = instance.classAttribute().indexOfValue(Boolean.TRUE.toString()), indexOfFalse=1-indexOfTrue;
			//classTrue=dataCollector.classAttribute.indexOfValue(Boolean.TRUE.toString());classFalse=dataCollector.classAttribute.indexOfValue(Boolean.FALSE.toString());
			if (value == indexOfTrue)
			{// positive
				
				if (Math.abs(distribution[indexOfTrue] - distribution[indexOfFalse]) < Configuration.fpAccuracy)
					++unknownValues;
				else
				{
					++totalPos;
					if (distribution[indexOfTrue] > distribution[indexOfFalse])
						++correctPredictionPos;
					/*
					else
					{
						System.out.println();
						classifier.distributionForInstance(instance);
					}*/
				}
			}
			else
			{// negative
				
				if (Math.abs(distribution[indexOfTrue] - distribution[indexOfFalse]) < Configuration.fpAccuracy)
					++unknownValues;
				else
				{
					++totalNeg;
					if (distribution[indexOfFalse] > distribution[indexOfTrue])
						++correctPredictionNeg;
					/*
					else
					{
						System.out.println();
						classifier.distributionForInstance(instance);
					}*/
				}
			}
		}
		if (evaluationSet.numInstances() == 0)
			return new EvaluationOfClassifierOutcome();
		else
			return new EvaluationOfClassifierOutcome(ConfusionMatrix.divide(correctPredictionPos, totalPos),ConfusionMatrix.divide(correctPredictionNeg, totalNeg),ConfusionMatrix.divide(unknownValues,evaluationSet.numInstances()));
					
					//correctPrediction/(double)evaluation.numInstances();
	}
	
	/** Constructs a helper that can be used to both build Markov models and to evaluate inconsistencies. 
	 * The intention is to have such a configuration in the same place where it can be picked up by both 
	 * classifier construction and classifier evaluation. 
	 * The method below constructs a helper that can evaluate four Markov scores in the time it takes to do two of them. 
	 */
	public static MarkovHelperClassifier constructMarkovModelsWithParametersForExperiment(MarkovParameters markovParameters)
	{
		final MarkovModel m_pathForward= new MarkovModel(markovParameters.chunkLen,true,true,true,false);
		final MarkovModel m_pathBackward= new MarkovModel(markovParameters.chunkLen,true,true,false,false);
		final MarkovModel m_setForward= new MarkovModel(markovParameters.chunkLen,false,true,true,false);
		final MarkovModel m_setBackward= new MarkovModel(markovParameters.chunkLen,false,true,false,false);
		
		MarkovModel [] models = new MarkovModel[]{m_pathForward,m_pathBackward, m_setForward, m_setBackward };
		final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();
		final ConsistencyChecker []checkers = new ConsistencyChecker[models.length];for(int i=0;i<models.length;++i) checkers[i] = checker; 
		MarkovHelperClassifier outcome = new MarkovHelperClassifier(markovParameters);
		outcome.setMarkovAndChecker(models, checkers);
		return outcome;
	}
	
	/** Constructs a helper that can be used to both build Markov models and to evaluate inconsistencies. 
	 * The intention is to have such a configuration in the same place where it can be picked up by both 
	 * classifier construction and classifier evaluation.
	 * 
	 *  
	 */
	public static MarkovHelper constructMarkovHelper(MarkovParameters markovParameters)
	{
		final MarkovModel m= new MarkovModel(markovParameters.chunkLen,markovParameters.pathsOrSets,true,true,false);
		final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();
		MarkovHelper result = new MarkovHelper(markovParameters);
		result.setMarkov(m);result.setChecker(checker);
		return result;
	}
	
	// Important: this routine ignores its arguments and always runs in a 'standalone' mode.
	public static void main(String args[]) throws Exception
	{
		DrawGraphs gr = new DrawGraphs();
	    LearnerEvaluationConfiguration learnerInitConfiguration = UASExperiment.constructLearnerInitConfiguration();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");

		String outDir = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_EXPERIMENTRESULTS)+File.separator+PairQualityLearner.directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		UASExperiment.mkDir(outDir);
		String outPathPrefix = outDir + File.separator;
		// This has to be run in a standalone mode in order to collect pair data across all experiments 
		// (which hence have to be all in the same process). This should not be a slow process because
		// there is not as much to learn as during evaluation and the amount of collected data is quite significant.
		RunSubExperiment<PairQualityParameters,ExperimentResult<PairQualityParameters>> experimentRunner = new RunSubExperiment<PairQualityParameters,ExperimentResult<PairQualityParameters>>(ExperimentRunner.getCpuNumber(),outPathPrefix + PairQualityLearner.directoryExperimentResult,new String[]{PhaseEnum.RUN_STANDALONE.toString()});

		final int samplesPerFSM = 8;//16;
		final int alphabetMultiplier = 1;
		final double trainingDataMultiplier = 2;
		MarkovParameters markovParameters = PairQualityLearner.defaultMarkovParameters();
		try
		{
			for(final int lengthMultiplier:new int[]{1})
			for(final int ifDepth:new int []{-2})
			for(final boolean onlyPositives:new boolean[]{true})
			{
					final int traceQuantity=10;
					for(final boolean useUnique:new boolean[]{false})
					{
						PairQualityParameters parExperiment = new PairQualityParameters(0, 0, 0, 0);
						DataCollectorParameters dataCollectorParameters = new DataCollectorParameters(ifDepth,markovParameters,false,(1 << 13) | (1 << 12) | (1 << 0) | (0xfffff << 14));
						dataCollectorParameters.depthToTrim=5;
						parExperiment.setExperimentParameters(false,dataCollectorParameters, onlyPositives, useUnique, alphabetMultiplier, traceQuantity, lengthMultiplier, trainingDataMultiplier);
						WekaDataCollector globalDataCollector = PairQualityLearner.createDataCollector(dataCollectorParameters, 
								constructMarkovHelper(markovParameters),// this cannot be shared between different experiments, hence we create a unique instance for each experiment.
								constructMarkovModelsWithParametersForExperiment(markovParameters));// this cannot be shared between different experiments, hence we create a unique instance for each experiment.
						List<WekaDataCollector> listOfCollectors = new ArrayList<WekaDataCollector>();
						int numberOfTasks = 0;
						for(int states:new int[]{20})
							for(int sample=0;sample<Math.round(samplesPerFSM*trainingDataMultiplier);++sample)
								for(int attempt=0;attempt<2;++attempt)
								{
									final PairQualityParameters parameters = new PairQualityParameters(states,sample,attempt,1+numberOfTasks);
									parameters.setExperimentParameters(false,dataCollectorParameters, onlyPositives, useUnique, alphabetMultiplier, traceQuantity, lengthMultiplier, trainingDataMultiplier);
									parameters.setColumn("LearnClassifier");
									// Important: there should be an instance of data collector per instance of learner runner because markov helpers are stateful and
									// running multiple tasks in parallel on different graphs will mess them up unless there is a unique instance of a data collector per learner. 
									WekaDataCollector dataCollector = PairQualityLearner.createDataCollector(dataCollectorParameters,
											constructMarkovHelper(markovParameters),// this cannot be shared between different experiments, hence we create a unique instance for each experiment.
											constructMarkovModelsWithParametersForExperiment(markovParameters));// this cannot be shared between different experiments, hence we create a unique instance for each experiment.

									listOfCollectors.add(dataCollector);
									PairQualityLearnerRunner learnerRunner = new PairQualityLearnerRunner(dataCollector,parameters, learnerInitConfiguration)
									{
										@Override
										public LearnerThatCanClassifyPairs createLearner(LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph,WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
										{
											return new LearnerThatUpdatesWekaResults(evalCnf,argReferenceGraph,argDataCollector,argInitialPTA, argDataCollector.markovHelper, argDataCollector.markovMultiHelper);
										}
									};
									experimentRunner.submitTask(learnerRunner);
									++numberOfTasks;
								}
				    	processSubExperimentResult<PairQualityParameters,ExperimentResult<PairQualityParameters>> resultHandler = new processSubExperimentResult<PairQualityParameters,ExperimentResult<PairQualityParameters>>() {
							@SuppressWarnings("unused")
							@Override
							public void processSubResult(ExperimentResult<PairQualityParameters> result, RunSubExperiment<PairQualityParameters,ExperimentResult<PairQualityParameters>> experimentrunner) throws IOException 
							{
							}
							
							@Override
							public SGEExperimentResult[] getGraphs() {
								return new SGEExperimentResult[]{};
							}
						};
						
						experimentRunner.collectOutcomeOfExperiments(resultHandler);
						for(WekaDataCollector w:listOfCollectors)
							globalDataCollector.trainingData.addAll(w.trainingData);// pool all the training data.
						
						// we are here because the outcome of all experiments submitted so far has been obtained, it is therefore time to construct classifiers from the logged pair information.
						
						int nonZeroes = 0;
						long numberOfValues = 0;
						System.out.println("number of instances: "+globalDataCollector.trainingData.numInstances());
						if (!globalDataCollector.getUseNumericalAttributes())
						{
							int freqData[] = new int[globalDataCollector.attributesOfAnInstance.length];
							for(int i=0;i<globalDataCollector.trainingData.numInstances();++i)
								for(int attrNum=0;attrNum<globalDataCollector.attributesOfAnInstance.length;++attrNum)
								{
									assert globalDataCollector.attributesOfAnInstance[attrNum].index() == attrNum;
									if (globalDataCollector.trainingData.instance(i).stringValue(attrNum) != WekaDataCollector.ZERO)
									{
										++freqData[attrNum];++numberOfValues;
									}
								}
							for(int attrNum=0;attrNum<globalDataCollector.attributesOfAnInstance.length;++attrNum)
								if (freqData[attrNum]>0) 
									++nonZeroes;
	
							List<Pair<Integer,Integer>> sortedFreq = new ArrayList<Pair<Integer,Integer>>(globalDataCollector.attributesOfAnInstance.length);
							for(int i=0;i<globalDataCollector.attributesOfAnInstance.length;++i) sortedFreq.add(new Pair<Integer,Integer>(freqData[i],i));
							sortedFreq.sort(new Comparator<Pair<Integer,Integer>>() {
	
								@Override
								public int compare(Pair<Integer, Integer> o1, Pair<Integer, Integer> o2) {
									return -o1.compareTo(o2);
								}});
						/*
						int attributesToKeep = 0;//nonZeroes/10;
						if (attributesToKeep > 0)
						{// do the filtering.
							for(int attrNum=attributesToKeep;attrNum<globalDataCollector.attributesOfAnInstance.length;++attrNum)
							{
								int attrIdx = sortedFreq.get(attrNum).secondElem;
								globalDataCollector.setBlock(attrIdx);
							}
							globalDataCollector.configureNoRecurse();
							System.out.println("Setting masked attributes to zero in datasets");
							for(int i=0;i<globalDataCollector.trainingData.numInstances();++i)
								for(int attrNum=attributesToKeep;attrNum<globalDataCollector.attributesOfAnInstance.length;++attrNum)
								{
									int attrIdx = sortedFreq.get(attrNum).secondElem;
									// set values of attributes that are not used as much to a zero
									globalDataCollector.trainingData.instance(i).setValue(attrIdx, WekaDataCollector.ZERO);								
								}
							for(WekaDataCollector w:listOfCollectors)
							{
								for(int i=0;i<w.trainingData.numInstances();++i)
									for(int attrNum=attributesToKeep;attrNum<w.attributesOfAnInstance.length;++attrNum)
									{
										int attrIdx = sortedFreq.get(attrNum).secondElem;
										// set values of attributes that are not used as much to a zero
										w.trainingData.instance(i).setValue(attrIdx, WekaDataCollector.ZERO);								
									}
							}
						}
						System.out.println("Total instances: "+globalDataCollector.trainingData.numInstances()+" with "+globalDataCollector.attributesOfAnInstance.length+" attributes, non-zeroes are "+
								nonZeroes+" with average of "+((double)numberOfValues)/nonZeroes+" of attributes"+(attributesToKeep>0? (", "+attributesToKeep+" were kept"):""));
						*/
							Arrays.sort(freqData);
							int numOfcolumns=20;
							int stepWidth = globalDataCollector.attributesOfAnInstance.length/numOfcolumns;
							
							final RBoxPlot<Long> gr_HistogramOfAttributeValues = new RBoxPlot<Long>("Attributes","Number of values",new File(outPathPrefix+parExperiment.getExperimentID()+"_attributes_use"+".pdf"));
							for(int i=0;i<numOfcolumns;++i)
							{
								int columnData=0;
								for(int j=i*stepWidth;j<(i+1)*stepWidth;++j)
									if (j < globalDataCollector.attributesOfAnInstance.length)
										columnData+=freqData[j];
								
								gr_HistogramOfAttributeValues.add(new Long(numOfcolumns-i),new Double(columnData>0?Math.log10(columnData):0));
							}
							//gr_HistogramOfAttributeValues.drawInteractive(gr);
							gr_HistogramOfAttributeValues.reportResults(gr);
						}
						
						final double valueCap = 250;
						
						// Now cap large values
					    for (int i = 0; i < globalDataCollector.trainingData.numInstances(); i++)
					    {
					    	Instance instance = globalDataCollector.trainingData.instance(i);
					    	for(int attr=0;attr<instance.numAttributes();++attr)
					    		if (attr != instance.classIndex() && instance.value(attr) > valueCap)
					    			instance.setValue(attr, valueCap);
					    }
						
						// write arff
						FileWriter wekaInstances = null;
						String whereToWrite = outDir+File.separator+parExperiment.getExperimentID()+".arff";
						try
						{
							wekaInstances = new FileWriter(whereToWrite);
							// This chunk is almost verbatim from Weka's Instances.toString()
							wekaInstances.append(Instances.ARFF_RELATION).append(" ").append(Utils.quote(globalDataCollector.trainingData.relationName())).append("\n\n");
						    for (int i = 0; i < globalDataCollector.trainingData.numAttributes(); i++) {
						    	wekaInstances.append(globalDataCollector.trainingData.attribute(i).toString()).append("\n");
						    }
						    wekaInstances.append("\n").append(Instances.ARFF_DATA).append("\n");
						    for (int i = 0; i < globalDataCollector.trainingData.numInstances(); i++) {
						    	wekaInstances.append(globalDataCollector.trainingData.instance(i).toString());
						        if (i < globalDataCollector.trainingData.numInstances() - 1) {
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
						File output = new File(outDir,parExperiment.getExperimentID()+"-out.pdf");
						final ScatterPlot plot = new ScatterPlot("Value", "Plot", output);
					    for (int i = 0; i < globalDataCollector.trainingData.numInstances(); i++) 
					    {
					    	Instance instance = globalDataCollector.trainingData.instance(i);

					    	int value = (int)instance.classValue();
							int indexOfTrue = instance.classAttribute().indexOfValue(Boolean.TRUE.toString());
							int y=0;
							for(int attr=0;attr<instance.numValues();++attr)
								if (attr != instance.classIndex())
									plot.add(instance.value(attr), y++, value == indexOfTrue?"blue":"red");
					    }
					    
						plot.reportResults(gr);
												
						FileWriter csvInstances = null;
						String csvToWrite = outDir+File.separator+parExperiment.getExperimentID()+".csv";
						try
						{
							csvInstances = new FileWriter(csvToWrite);
							csvInstances.append("c1,c2,class\n");
						    for (int i = 0; i < globalDataCollector.trainingData.numInstances(); i++) 
						    {
						    	Instance instance = globalDataCollector.trainingData.instance(i);

						    	int value = (int)instance.classValue();
								int indexOfTrue = instance.classAttribute().indexOfValue(Boolean.TRUE.toString());
								
						    	csvInstances.append(Double.toString(instance.value(0)));csvInstances.append(',');
						    	//csvInstances.append(Double.toString(instance.value(1)/PairQualityLearner.kfromab_multiplier));csvInstances.append(',');
						    	csvInstances.append(Double.toString(instance.value(2)/PairQualityLearner.kfromab_multiplier));csvInstances.append(',');
						    	csvInstances.append(Boolean.toString(value == indexOfTrue));csvInstances.append('\n');
						    }		
						    
						    /* Drawing the plots involves calling the following:
						     * mydata = read.csv("C:\\Users\\Kirill\\git\\statechum\\tmp\\LearningWithClassifiers\\POS_tQU=10_tM=1_tAMr=1_tDM=2.0_[Num_G=0.2_I=0.0_P_0_cl=3_w1.0].csv")
						     * f=subset(mydata,class=="false")
						     * t=subset(mydata,class=="true")
						     * t=data.frame(t$c1,t$c2)
						     * f=data.frame(f$c1,f$c2)
						     * plot(as.vector(t[[1]]),as.vector(t[[2]]) , type = "p",col="blue",xlim=range(0,20), ylim=range(0, 20))
						     * par(new=TRUE)
						     * plot(as.vector(f[[1]]),as.vector(f[[2]]) , type = "p",col="red",xlim=range(0,20), ylim=range(0, 20))
						     */
						    
						}
						catch(Exception ex)
						{
							Helper.throwUnchecked("failed to create a file with training data for "+csvToWrite, ex);
						}
						finally
						{
							if (csvInstances != null)
								try {
									csvInstances.close();
								} catch (IOException e) {
									// ignore this, we are not proceeding anyway due to an earlier exception so whether the file was actually written does not matter
								}
						}
						// Run the evaluation
						//final weka.classifiers.trees.REPTree repTree = new weka.classifiers.trees.REPTree();repTree.setMaxDepth(4);
						//repTree.setNoPruning(true);// since we only use the tree as a classifier (as a conservative extension of what is currently done) and do not actually look at it, elimination of pruning is not a problem. 
						// As part of learning, we also prune some of the nodes where the ratio of correctly-classified pairs to those incorrectly classified is comparable.
						// The significant advantage of not pruning is that the result is no longer sensitive to the order of elements in the tree and hence does not depend on the order in which elements have been obtained by concurrent threads.
						//final weka.classifiers.lazy.IB1 ib1 = new weka.classifiers.lazy.IB1();
						//final weka.classifiers.trees.J48 j48classifier = new weka.classifiers.trees.J48();
						//final weka.classifiers.lazy.IBk ibk = new weka.classifiers.lazy.IBk(1);
						final Classifier classifier =
								new PickBestAttributeClassifier();
								//new NearestClassifier();
								//new weka.classifiers.trees.J48();
								//new weka.classifiers.trees.REPTree();
						/*
						classifier.buildClassifier(globalDataCollector.trainingData);
						System.out.println("Entries in the classifier: "+globalDataCollector.trainingData.numInstances());
						if (classifier instanceof NearestClassifier)
						{
							System.out.println("Reduced entries in the classifier: "+((NearestClassifier)classifier).getTrainingSize());
							System.out.println("Threshold for unambiguous: "+((NearestClassifier)classifier).getThresholdForUnambiguousResults());
						}
						*/
						
						/*
						int itersCount = 0;
						long endTime = System.nanoTime()+1000000000*10L;// 10 sec
						while(System.nanoTime() < endTime)
						{
							classifier.classifyInstance(dataCollector.trainingData.instance(itersCount));
							itersCount = (1+itersCount) % dataCollector.trainingData.numInstances();
							
						}
						System.out.println("time per iteration: "+((double)10000/itersCount)+" ms");
						*/
						//System.out.println("evaluation of the classifier: "+evaluateClassifier(classifier,globalDataCollector));
						
						/*
						List<Instances> trainingDataComponents = new ArrayList<Instances>();for(WekaDataCollector c:listOfCollectors) trainingDataComponents.add(c.trainingData);
						evaluateClassifierOnDataFromIndividualInstances(classifier,trainingDataComponents);
						
						//System.out.println(classifier);
						globalDataCollector=null;// throw all the training data away.

						{// serialise the classifier, this is the only way to store it.
							OutputStream os = new FileOutputStream(outDir+File.separator+parExperiment.getExperimentID()+".ser");
							ObjectOutputStream oo = new ObjectOutputStream(os); 
		                    oo.writeObject(classifier);
		                    os.close();
						}
						*/
/*
						{
							InputStream inputStream = new FileInputStream(outPathPrefix+parExperiment.getExperimentID()+".ser");
							ObjectInputStream objectInputStream = new ObjectInputStream(inputStream); 
							final Classifier classifierLoaded = (Classifier)objectInputStream.readObject();
		                    inputStream.close();
		                    classifierLoaded.equals(classifier);
		                    System.out.println("Equality of classifiers: "+classifierLoaded.equals(classifier)+" and performance is : "+evaluateSpecificClassifierUsingSpecifiedEvaluationData(classifierLoaded,trainingDataComponents.get(0)));
						}
*/

					}
				}
		}
		finally
		{
			experimentRunner.successfulTermination();
			DrawGraphs.end();// this is necessary to ensure termination of the JVM runtime at the end of experiments.
		}
	}
}
