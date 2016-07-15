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
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovHelper;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovParameters;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.LearnerThatCanClassifyPairs;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUpdatesWekaResults;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.PairQualityLearnerRunner;
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
	public static class NearestClassifier extends AbstractClassifier
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
		public static int valueToBits(String value)
		{
			if (value == WekaDataCollector.ZERO)
				return 0x1;
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
					array[offset+(attrPos/attributesPerLong)] |= valueToBits(instance.stringValue(attr)) << (bitsPerAttribute* (attrPos & bitMask) );
					++attrPos;
				}
			//if (fillInClassification)
			//	array[offset+(classAttrInTrainingDataArray/attributesPerLong)] |= (instance.classValue()>0?1:0) << (bitsPerAttribute* (classAttrInTrainingDataArray & bitMask) );
		}
		
		public static class PosNeg implements Serializable
		{
			/**
			 * ID for serialisation
			 */
			private static final long serialVersionUID = -5672965229484999993L;
			double aboveZero=0,zero=0;

			public PosNeg(double a,double b)
			{
				aboveZero = a;zero = b;
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
				if (Math.abs(aboveZero-other.aboveZero) < Configuration.fpAccuracy)
					return false;
				if (Math.abs(zero-other.zero) < Configuration.fpAccuracy)
					return false;
				return true;
			}
		}
		
		public int getTrainingSize()
		{
			return instanceNumber;
		}
		
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
		
		private static int getIndexFor(Attribute classAttr, String forWhat)
		{
			for(int i=0;i<classAttr.numValues();++i)
				if (classAttr.value(i).equalsIgnoreCase(forWhat))
					return i;
			
			throw new IllegalArgumentException("failed to fine value for \""+forWhat+"\"");
		}
		
		@Override
		public void buildClassifier(Instances data) throws Exception 
		{
			instanceSize = (data.numAttributes() + attributesPerLong-1)/attributesPerLong;
			classAttrInTrainingDataArray = data.numAttributes()-1;
			
			
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
		public PosNeg classifyInstanceAsPosNeg(Instance instance) throws Exception 
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
			
			if (currentBestInstanceIdx < 0)
				throw new IllegalArgumentException("missing data");
			return trainingDistribution[currentBestInstanceIdx];
		}

		@Override
		public double classifyInstance(Instance instance) throws Exception 
		{
			PosNeg values = classifyInstanceAsPosNeg(instance);
			if (values.aboveZero> values.zero)
				return 1.0;
			else
				return 0.0;
		}
		
		public String classifyInstanceString(Instance instance) throws Exception
		{
			return instance.classAttribute().value((int)classifyInstance(instance));
		}
		
		public boolean classifyInstanceBoolean(Instance instance) throws Exception
		{
			return Boolean.parseBoolean(classifyInstanceString(instance));
		}
		
		@Override
		public double[] distributionForInstance(Instance instance) throws Exception 
		{
			PosNeg values = classifyInstanceAsPosNeg(instance);
			double[] outcome = new double[]{values.zero,values.aboveZero};
			return outcome;			
		}
	}
	
	public static double evaluateClassifier(Classifier classifier, WekaDataCollector data) throws Exception
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
		classifier.buildClassifier(ninetyPercent);
		/*
		for(int i=0;i<ninetyPercent.numInstances();++i)
		{
			Instance instance = ninetyPercent.instance(i);
			assert Math.abs(instance.classValue()-classifier.classifyInstance(instance)) < Configuration.fpAccuracy : "invalid classification of instance "+i;
		}
		*/
		long correctPrediction = 0;
		for(int i=0;i<evaluation.numInstances();++i)
		{
			Instance instance = evaluation.instance(i);
			double classification = classifier.classifyInstance(instance);
			double value = instance.classValue();
			if (Math.abs(classification-value) < Configuration.fpAccuracy)
				++correctPrediction;
		}
		if (evaluation.numInstances() == 0)
			return 0;
		else
			return correctPrediction/(double)evaluation.numInstances();
	}
	
	public static void main(String args[]) throws Exception
	{
		DrawGraphs gr = new DrawGraphs();
	    LearnerEvaluationConfiguration learnerInitConfiguration = UASExperiment.constructLearnerInitConfiguration();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");

		String outDir = "tmp"+File.separator+PairQualityLearner.directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		UASExperiment.mkDir(outDir);
		String outPathPrefix = outDir + File.separator;
		// This has to be run in a standalone mode in order to collect pair data across all experiments 
		// (which hence have to be all in the same process). This should not be a slow process because
		// there is not as much to learn as during evaluation and the amount of collected data is quite significant.
		RunSubExperiment<PairQualityParameters,ExperimentResult<PairQualityParameters>> experimentRunner = new RunSubExperiment<PairQualityParameters,ExperimentResult<PairQualityParameters>>(ExperimentRunner.getCpuNumber(),outPathPrefix + PairQualityLearner.directoryExperimentResult,new String[]{PhaseEnum.RUN_STANDALONE.toString()});

		final int samplesPerFSM = 16;
		final int alphabetMultiplier = 2;
		final double trainingDataMultiplier = 2;

		try
		{
			for(final int lengthMultiplier:new int[]{10})
			for(final int ifDepth:new int []{1})
			for(final boolean onlyPositives:new boolean[]{true})
			{
					final int traceQuantity=1;
					for(final boolean useUnique:new boolean[]{false})
					{
						PairQualityParameters parExperiment = new PairQualityParameters(0, 0, 0, 0);
						parExperiment.setExperimentParameters(false,ifDepth, onlyPositives, useUnique, alphabetMultiplier, traceQuantity, lengthMultiplier, trainingDataMultiplier);
						MarkovParameters markovParameters = new MarkovParameters(0, 3,1, true,1,0,1);
						WekaDataCollector dataCollector = PairQualityLearner.createDataCollector(ifDepth, new MarkovHelper(markovParameters),!markovParameters.useCentreVertex);
						int numberOfTasks = 0;
						for(int states:new int[]{20})
							for(int sample=0;sample<Math.round(samplesPerFSM*trainingDataMultiplier);++sample)
								for(int attempt=0;attempt<2;++attempt)
								{
									PairQualityParameters parameters = new PairQualityParameters(states,sample,attempt,1+numberOfTasks);
									parameters.setExperimentParameters(false,ifDepth, onlyPositives, useUnique, alphabetMultiplier, traceQuantity, lengthMultiplier, trainingDataMultiplier);
									parameters.setColumn("LearnClassifier");
									parameters.markovParameters = markovParameters;
									PairQualityLearnerRunner learnerRunner = new PairQualityLearnerRunner(dataCollector,parameters, learnerInitConfiguration)
									{
										@Override
										public LearnerThatCanClassifyPairs createLearner(LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph,WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
										{
											return new LearnerThatUpdatesWekaResults(evalCnf,argReferenceGraph,argDataCollector,argInitialPTA, argDataCollector.markovHelper);
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
	
						// we are here because the outcome of all experiments submitted so far has been obtained, it is therefore time to construct classifiers from the logged pair information.
						int nonZeroes = 0;
						long numberOfValues = 0;
						System.out.println("number of instances: "+dataCollector.trainingData.numInstances());
						int freqData[] = new int[dataCollector.attributesOfAnInstance.length];
						for(int i=0;i<dataCollector.trainingData.numInstances();++i)
							for(int attrNum=0;attrNum<dataCollector.attributesOfAnInstance.length;++attrNum)
							{
								assert dataCollector.attributesOfAnInstance[attrNum].index() == attrNum;
								if (dataCollector.trainingData.instance(i).stringValue(attrNum) != WekaDataCollector.ZERO)
								{
									++freqData[attrNum];++numberOfValues;
								}
							}
						for(int attrNum=0;attrNum<dataCollector.attributesOfAnInstance.length;++attrNum)
							if (freqData[attrNum]>0) 
								++nonZeroes;
						
						System.out.println("Total instances: "+dataCollector.trainingData.numInstances()+" with "+dataCollector.attributesOfAnInstance.length+" attributes, non-zeroes are "+nonZeroes+" with average of "+((double)numberOfValues)/nonZeroes);
						Arrays.sort(freqData);
						int numOfcolumns=20;
						int stepWidth = dataCollector.attributesOfAnInstance.length/numOfcolumns;
						
						final RBoxPlot<Long> gr_HistogramOfAttributeValues = new RBoxPlot<Long>("Attributes","Number of values",new File(outPathPrefix+parExperiment.getExperimentID()+"_attributes_use"+".pdf"));
						for(int i=0;i<numOfcolumns;++i)
						{
							int columnData=0;
							for(int j=i*stepWidth;j<(i+1)*stepWidth;++j)
								if (j < dataCollector.attributesOfAnInstance.length)
									columnData+=freqData[j];
							
							gr_HistogramOfAttributeValues.add(new Long(numOfcolumns-i),new Double(columnData>0?Math.log10(columnData):0));
						}
						//gr_HistogramOfAttributeValues.drawInteractive(gr);
						gr_HistogramOfAttributeValues.reportResults(gr);
						
						// write arff
						FileWriter wekaInstances = null;
						String whereToWrite = outDir+File.separator+parExperiment.getExperimentID()+".arff";
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
						final weka.classifiers.trees.REPTree repTree = new weka.classifiers.trees.REPTree();repTree.setMaxDepth(4);
						//repTree.setNoPruning(true);// since we only use the tree as a classifier (as a conservative extension of what is currently done) and do not actually look at it, elimination of pruning is not a problem. 
						// As part of learning, we also prune some of the nodes where the ratio of correctly-classified pairs to those incorrectly classified is comparable.
						// The significant advantage of not pruning is that the result is no longer sensitive to the order of elements in the tree and hence does not depend on the order in which elements have been obtained by concurrent threads.
						//final weka.classifiers.lazy.IB1 ib1 = new weka.classifiers.lazy.IB1();
						final weka.classifiers.trees.J48 j48classifier = new weka.classifiers.trees.J48();
						final weka.classifiers.lazy.IBk ibk = new weka.classifiers.lazy.IBk(1);
						final Classifier classifier = new NearestClassifier();
						classifier.buildClassifier(dataCollector.trainingData);
						System.out.println("Entries in the classifier: "+dataCollector.trainingData.numInstances());
						if (classifier instanceof NearestClassifier)
							System.out.println("Reduced entries in the classifier: "+((NearestClassifier)classifier).getTrainingSize());
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
						System.out.println("evaluation of the classifier: "+evaluateClassifier(classifier,dataCollector));
						//System.out.println(classifier);
						dataCollector=null;// throw all the training data away.

						{// serialise the classifier, this is the only way to store it.
							OutputStream os = new FileOutputStream(outDir+File.separator+parExperiment.getExperimentID()+".ser");
							ObjectOutputStream oo = new ObjectOutputStream(os); 
		                    oo.writeObject(classifier);
		                    os.close();
						}
/*						
						{
							InputStream inputStream = new FileInputStream(outPathPrefix+parExperiment.getExperimentID()+".ser");
							ObjectInputStream objectInputStream = new ObjectInputStream(inputStream); 
							final Classifier classifierLoaded = (Classifier)objectInputStream.readObject();
		                    inputStream.close();
		                    System.out.println(classifierLoaded.equals(classifier));
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
