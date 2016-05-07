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
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.util.Map;
import java.util.TreeMap;

import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.AggregateValues;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.MergeObjects;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.LearnerWithMandatoryMergeConstraints;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.TrueFalseCounter;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.PairQualityLearnerRunner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import weka.classifiers.Classifier;

public class LearnUsingClassifier {
	public static void main(String args[]) throws Exception
	{
		DrawGraphs gr = new DrawGraphs();
	    LearnerEvaluationConfiguration learnerInitConfiguration = UASExperiment.constructLearnerInitConfiguration();
	    PairQualityLearner.configureConfigurationForLearningUsingClassifiers(learnerInitConfiguration.config);
	    
		//gr_NewToOrig.setLimit(7000);
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");

		String outDir = "tmp"+File.separator+PairQualityLearner.nameForExperimentRun;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		if (!new java.io.File(outDir).isDirectory())
		{
			if (!new java.io.File(outDir).mkdir())
			{
				System.out.println("failed to create a work directory");return ;
			}
		}
		String outPathPrefix = outDir + File.separator;
		RunSubExperiment<LearnWithClassifiersResult> experimentRunner = new RunSubExperiment<LearnWithClassifiersResult>(ExperimentRunner.getCpuNumber(),"data",args);
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
					// load the classified from serialised representation
					InputStream inputStream = new FileInputStream(outPathPrefix+parExperiment.getExperimentID()+".ser");
					ObjectInputStream objectInputStream = new ObjectInputStream(inputStream); 
					final Classifier classifier = (Classifier)objectInputStream.readObject();
                    inputStream.close();
                    
					for(final boolean selectingRed:new boolean[]{false})
					for(final boolean classifierToBlockAllMergers:new boolean[]{true})
					//for(final boolean zeroScoringAsRed:(classifierToBlockAllMergers?new boolean[]{true,false}:new boolean[]{false}))// where we are not using classifier to rule out all mergers proposed by pair selection, it does not make sense to use two values configuring this classifier.
					for(final double threshold:new double[]{1})
					{
						final boolean zeroScoringAsRed = false;

						final UseWekaResultsParameters parametersInnerLearner = new UseWekaResultsParameters(ifDepth);
						parametersInnerLearner.setUseClassifierForRed(selectingRed);parametersInnerLearner.setUseClassifierToChooseNextRed(classifierToBlockAllMergers);
						parametersInnerLearner.setBlacklistZeroScoringPairs(zeroScoringAsRed);
						parametersInnerLearner.setThreshold(threshold);

						String selection = parExperiment.getExperimentID()+"-"+parametersInnerLearner.getRowID();

						final int totalTaskNumber = traceQuantity;
						final RBoxPlot<Long> gr_PairQuality = new RBoxPlot<Long>("Correct v.s. wrong","%%",new File(outPathPrefix+"percentage_score"+selection+".pdf"));
						final RBoxPlot<String> gr_QualityForNumberOfTraces = new RBoxPlot<String>("traces","%%",new File(outPathPrefix+"quality_traces"+selection+".pdf"));
						SquareBagPlot gr_NewToOrig = new SquareBagPlot("orig score","score with learnt selection",new File(outPathPrefix+"new_to_orig"+selection+".pdf"),0,1,true);
						
						final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(outPathPrefix+"results"+selection+".csv"));
						
						processSubExperimentResult<LearnWithClassifiersResult> resultHandler = new processSubExperimentResult<LearnWithClassifiersResult>() {
							@Override
							public void processSubResult(LearnWithClassifiersResult result, RunSubExperiment<LearnWithClassifiersResult> experimentrunner) throws IOException 
							{
								ScoresForGraph difference = result.samples.get(0).actualLearner;
								StringBuffer csvLine = new StringBuffer();
								csvLine.append(difference.differenceBCR.getValue());
								CSVExperimentResult.addSeparator(csvLine);csvLine.append(difference.differenceStructural.getValue());
								CSVExperimentResult.addSeparator(csvLine);csvLine.append(difference.nrOfstates.getValue());
								CSVExperimentResult.addSeparator(csvLine);if (result.parameters.pairQualityCounter != null) csvLine.append(DrawGraphs.objectAsText(result.parameters.pairQualityCounter));
								experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
							}
							
							@Override
							public String getSubExperimentName()
							{
								return "Learning using classifier";
							}
							
							@Override
							public SGEExperimentResult[] getGraphs() {
								return new SGEExperimentResult[]{resultCSV};
							}
						};
						int numberOfTasks = 0;
						for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
							for(int sample=0;sample<samplesPerFSM;++sample)
								for(int attempt=0;attempt<2;++attempt)
								{
									{// first, use the learner with a classifier 
										final PairQualityParameters parameters = new PairQualityParameters(states, sample, attempt,totalTaskNumber+numberOfTasks);
										parameters.setExperimentParameters(ifDepth, onlyPositives, useUnique, traceQuantity, lengthMultiplier, trainingDataMultiplier);
										parameters.setInnerParameters(parametersInnerLearner);
										parameters.setColumn("WithClassifier");
										final Map<Long,TrueFalseCounter> pairQualityCounter = new TreeMap<Long,TrueFalseCounter>();
										parameters.setPairQualityCounter(pairQualityCounter);// pairQualityCounter is shared between parameters and the inner learner. This permits the inner learner to use it and for the processSubExperimentResult to extract the value of it, all with the outer learner being oblivious to it.
										PairQualityLearnerRunner learnerRunner = new PairQualityLearnerRunner(null,parameters, learnerInitConfiguration)
										{
											@Override
											public LearnerWithMandatoryMergeConstraints createLearner(LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph,@SuppressWarnings("unused") WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
											{
												LearnerThatUsesWekaResults l = new LearnerThatUsesWekaResults(parametersInnerLearner,evalCnf,argReferenceGraph,classifier,argInitialPTA);
												l.setPairQualityCounter(pairQualityCounter);
												return l;
											}											
										};
										experimentRunner.submitTask(learnerRunner);
									}
									{// second, use a traditional learner 
										final PairQualityParameters parameters = new PairQualityParameters(states, sample, attempt,totalTaskNumber+numberOfTasks);
										parameters.setExperimentParameters(ifDepth, onlyPositives, useUnique, traceQuantity, lengthMultiplier, trainingDataMultiplier);
										parameters.setInnerParameters(parametersInnerLearner);
										parameters.setColumn("Reference");
										PairQualityLearnerRunner learnerRunner = new PairQualityLearnerRunner(null,parameters, learnerInitConfiguration)
										{
											@SuppressWarnings("unused")
											@Override
											public LearnerWithMandatoryMergeConstraints createLearner(LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph, WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
											{
												return new LearningAlgorithms.ReferenceLearner(evalCnf,argInitialPTA,LearningAlgorithms.ReferenceLearner.OverrideScoringToApply.SCORING_SICCO);
											}
											
										};
										experimentRunner.submitTask(learnerRunner);
									}
									++numberOfTasks;
								}
				    		
				    	experimentRunner.collectOutcomeOfExperiments(resultHandler);
				    	// When we reach this point, the CSV is built so it is handy to process the data in it.
				    	DrawGraphs.spreadsheetToDoubleGraph(gr_NewToOrig, resultCSV, "Reference", 0, "WithClassifier", 0, null, null);
				    	DrawGraphs.spreadsheetAsDouble(new AggregateValues() {
							
							@Override
							public void merge(double reference, double actual) {
								if (reference > 0)
									gr_QualityForNumberOfTraces.add(traceQuantity+"",actual/reference);
							}
						}, resultCSV, "Reference", 0,"WithClassifier",0);
						
						if (gr_PairQuality != null)
						{
							final Map<Long,TrueFalseCounter> pairQualityCounter = new TreeMap<Long,TrueFalseCounter>();
							DrawGraphs.spreadsheetObjectsCollect(new MergeObjects() {
								@Override
								public void merge(@SuppressWarnings("unused") String key, Object obj) {
									@SuppressWarnings("unchecked")
									Map<Long,TrueFalseCounter> cnt = (Map<Long,TrueFalseCounter>)obj;
									for(Map.Entry<Long,TrueFalseCounter> entry:cnt.entrySet())
									{
										TrueFalseCounter c = pairQualityCounter.get(entry.getKey());
										if (c == null)
										{
											c= new TrueFalseCounter();pairQualityCounter.put(entry.getKey(), c);
										}
										c.trueCounter+=entry.getValue().trueCounter;c.falseCounter+=entry.getValue().falseCounter;
									}
								}},resultCSV,null,0,"PairQuality",3);
							LearningSupportRoutines.updateGraph(gr_PairQuality,pairQualityCounter);
								//gr_PairQuality.drawInteractive(gr);
								//gr_NewToOrig.drawInteractive(gr);
								//if (gr_QualityForNumberOfTraces.size() > 0)
								//	gr_QualityForNumberOfTraces.drawInteractive(gr);
						}
						if (gr_PairQuality != null) gr_PairQuality.reportResults(gr);
						if (gr_NewToOrig != null) gr_NewToOrig.reportResults(gr);
						if (gr_QualityForNumberOfTraces != null) gr_QualityForNumberOfTraces.reportResults(gr);
					}
				}
			}
	}
}
