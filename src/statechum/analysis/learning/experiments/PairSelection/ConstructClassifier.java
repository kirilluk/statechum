package statechum.analysis.learning.experiments.PairSelection;

import java.io.File;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.LearnerThatCanClassifyPairs;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerRunner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUpdatesWekaResults;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.SampleData;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResult;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import weka.classifiers.Classifier;

public class ConstructClassifier 
{
	public static void main(String args[]) throws Exception
	{
		DrawGraphs gr = new DrawGraphs();
		ConvertALabel converter = new Transform.InternStringLabel();
		Configuration config = PairQualityLearner.constructConfigurationForLearningUsingClassifiers();
		//gr_NewToOrig.setLimit(7000);
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int ThreadNumber = ExperimentRunner.getCpuNumber();

		String outDir = "tmp"+File.separator+PairQualityLearner.nameForExperimentRun;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		if (!new java.io.File(outDir).isDirectory())
		{
			if (!new java.io.File(outDir).mkdir())
			{
				System.out.println("failed to create a work directory");return ;
			}
		}
		ExecutorService executorService = Executors.newFixedThreadPool(ThreadNumber);
		// Stores tasks to complete.
		CompletionService<ThreadResult> runner = new ExecutorCompletionService<ThreadResult>(executorService);
		String outPathPrefix = outDir + File.separator;
		final int minStateNumber = 20;
		final int samplesPerFSM = 4;
		final int rangeOfStateNumbers = 4;
		final int stateNumberIncrement = 4;
		final double trainingDataMultiplier = 2;

		for(final int lengthMultiplier:new int[]{50})
		for(final int ifDepth:new int []{1})
		for(final boolean onlyPositives:new boolean[]{true})
		{
				final int traceQuantity=1;
				for(final boolean useUnique:new boolean[]{false})
				{
					PairQualityParameters parExperiment = new PairQualityParameters(0, 0, 0, 0);
					parExperiment.setExperimentParameters(ifDepth, onlyPositives, useUnique, traceQuantity, lengthMultiplier, trainingDataMultiplier);
					WekaDataCollector dataCollector = PairQualityLearner.createDataCollector(ifDepth);
					List<SampleData> samples = new LinkedList<SampleData>();
					try
					{
						int numberOfTasks = 0;
						for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
							for(int sample=0;sample<Math.round(samplesPerFSM*trainingDataMultiplier);++sample)
								for(int attempt=0;attempt<2;++attempt)
								{
									PairQualityParameters parameters = new PairQualityParameters(states,sample,attempt,1+numberOfTasks);
									parameters.traceQuantity = traceQuantity;
									LearnerRunner learnerRunner = new LearnerRunner(dataCollector,parameters, config, converter)
									{
										@Override
										public LearnerThatCanClassifyPairs createLearner(LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph,WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
										{
											return new LearnerThatUpdatesWekaResults(evalCnf,argReferenceGraph,argDataCollector,argInitialPTA);
										}
									};
									parameters.setPickUniqueFromInitial(useUnique);parameters.setOnlyUsePositives(onlyPositives);parameters.setIfdepth(ifDepth);parameters.setLengthMultiplier(lengthMultiplier);
									runner.submit(learnerRunner);
									++numberOfTasks;
								}
						for(int count=0;count < numberOfTasks;++count)
						{
							ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
							samples.addAll(result.samples);
						}
					}
					catch(Exception ex)
					{
						IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
						if (executorService != null) { executorService.shutdown();executorService = null; }
						throw e;
					}

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
					/*
					// write arff
					FileWriter wekaInstances = null;
					String whereToWrite = "qualityLearner_"+selection+".arff";
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
					*/
					// Run the evaluation
					final weka.classifiers.trees.REPTree repTree = new weka.classifiers.trees.REPTree();repTree.setMaxDepth(4);
					//repTree.setNoPruning(true);// since we only use the tree as a classifier (as a conservative extension of what is currently done) and do not actually look at it, elimination of pruning is not a problem. 
					// As part of learning, we also prune some of the nodes where the ratio of correctly-classified pairs to those incorrectly classified is comparable.
					// The significant advantage of not pruning is that the result is no longer sensitive to the order of elements in the tree and hence does not depend on the order in which elements have been obtained by concurrent threads.
					//final weka.classifiers.lazy.IB1 ib1 = new weka.classifiers.lazy.IB1();
					//final weka.classifiers.trees.J48 classifier = new weka.classifiers.trees.J48();
					final Classifier classifier = repTree;
					classifier.buildClassifier(dataCollector.trainingData);
					System.out.println("Entries in the classifier: "+dataCollector.trainingData.numInstances());
					System.out.println(classifier);
					dataCollector=null;// throw all the training data away.
					
					{// serialise the classifier, this is the only way to store it.
						OutputStream os = new FileOutputStream(outDir+File.separator+parExperiment.getExperimentID()+".ser");
						ObjectOutputStream oo = new ObjectOutputStream(os); 
	                    oo.writeObject(classifier);
	                    os.close();
					}
				}
			}
	}
}
