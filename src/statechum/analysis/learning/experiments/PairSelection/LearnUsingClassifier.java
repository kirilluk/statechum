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
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.SampleData;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum;
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
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");

		String outDir = "tmp"+File.separator+PairQualityLearner.directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		UASExperiment.mkDir(outDir);
		String outPathPrefix = outDir + File.separator;
		RunSubExperiment<PairQualityParameters,ExperimentResult<PairQualityParameters>> experimentRunner = new RunSubExperiment<PairQualityParameters,ExperimentResult<PairQualityParameters>>(ExperimentRunner.getCpuNumber(),outPathPrefix + PairQualityLearner.directoryExperimentResult,args);
		final int samplesPerFSM = 4;
		final int alphabetMultiplier = 2;
		final double trainingDataMultiplier = 2;

		try
		{
			for(final int traceLengthMultiplier:new int[]{10})
			for(final int ifDepth:new int []{1})
			for(final boolean onlyPositives:new boolean[]{true})
				{
					final int traceQuantityToUse=1;
					for(final boolean useUnique:new boolean[]{false})
					{
						PairQualityParameters parExperiment = new PairQualityParameters(0, 0, 0, 0);
						parExperiment.setExperimentParameters(true,ifDepth, onlyPositives, useUnique, alphabetMultiplier, traceQuantityToUse, traceLengthMultiplier, trainingDataMultiplier);
						// load the classified from serialised representation
						InputStream inputStream = new FileInputStream(outPathPrefix+parExperiment.getIfDepthAsString()+"_"+parExperiment.getExperimentID()+".ser");
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
	
							String selection = parExperiment.getIfDepthAsString()+"_"+parExperiment.getExperimentID()+"-"+parametersInnerLearner.getRowID();
	
							final RBoxPlot<Long> gr_PairQuality = new RBoxPlot<Long>("Correct v.s. wrong","%%",new File(outPathPrefix+"percentage_score"+selection+".pdf"));
							final RBoxPlot<String> gr_QualityForNumberOfTraces = new RBoxPlot<String>("traces","%%",new File(outPathPrefix+"quality_traces"+selection+".pdf"));
							SquareBagPlot gr_NewToOrig = new SquareBagPlot("orig score","score with learnt selection",new File(outPathPrefix+"new_to_orig"+selection+".pdf"),0,1,true);
							
							final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(outPathPrefix+"results"+selection+".csv"));
							
							processSubExperimentResult<PairQualityParameters,ExperimentResult<PairQualityParameters>> resultHandler = new processSubExperimentResult<PairQualityParameters,ExperimentResult<PairQualityParameters>>() {
								@Override
								public void processSubResult(ExperimentResult<PairQualityParameters> result, RunSubExperiment<PairQualityParameters,ExperimentResult<PairQualityParameters>> experimentrunner) throws IOException 
								{
									SampleData sm = result.samples.get(0);
									ScoresForGraph data=sm.actualLearner;
									StringBuffer csvLine = new StringBuffer();
									csvLine.append(data.differenceBCR.getValue());
									CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceStructural.getValue());
									CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.nrOfstates.getValue());

									CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.fractionOfStatesIdentifiedBySingletons);
									CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovPrecision);
									CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovRecall);
																
									CSVExperimentResult.addSeparator(csvLine);csvLine.append(Boolean.toString(sm.centreCorrect));
									CSVExperimentResult.addSeparator(csvLine);csvLine.append(Integer.toString(sm.centrePathNumber));
									CSVExperimentResult.addSeparator(csvLine);csvLine.append(Long.toString(sm.transitionsSampled));

									if (result.parameters.pairQualityCounter != null)
									{
										CSVExperimentResult.addSeparator(csvLine); csvLine.append(DrawGraphs.objectAsText(result.parameters.pairQualityCounter));
									}
									CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(data.executionTime/1000000000.));// execution time is in nanoseconds, we only need seconds.
									experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
								}
								
								@Override
								public SGEExperimentResult[] getGraphs() {
									return new SGEExperimentResult[]{resultCSV};
								}
							};
							int chunkLen = 3;
							int seedForFSM = 0;
							double weightOfInconsistencies = 1.0;
							boolean aveOrMax=false;
							int divisor=2;
							for(int states:new int[]{20})
								for(int sample=0;sample<Math.round(samplesPerFSM*trainingDataMultiplier);++sample)
									for(int attempt=0;attempt<2;++attempt)
									{
										final PairQualityParameters pars = new PairQualityParameters(states, sample, attempt,seedForFSM);
										pars.setExperimentParameters(true,ifDepth, onlyPositives, useUnique, alphabetMultiplier, traceQuantityToUse, traceLengthMultiplier, trainingDataMultiplier);
										pars.setInnerParameters(parametersInnerLearner);
										pars.markovParameters.setMarkovParameters(0,chunkLen,weightOfInconsistencies, aveOrMax,divisor,0,1);
										
										{// first, use the learner with a classifier 
											PairQualityParameters parameters = new PairQualityParameters(pars);
											parameters.setColumn("WithClassifier");
											final Map<Long,TrueFalseCounter> pairQualityCounter = new TreeMap<Long,TrueFalseCounter>();
											parameters.setPairQualityCounter(pairQualityCounter);// pairQualityCounter is shared between parameters and the inner learner. This permits the inner learner to use it and for the processSubExperimentResult to extract the value of it, all with the outer learner being oblivious to it.
											PairQualityLearnerRunner learnerRunner = new PairQualityLearnerRunner(null,parameters, learnerInitConfiguration)
											{
												@Override
												public LearnerWithMandatoryMergeConstraints createLearner(LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph,WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
												{
													LearnerThatUsesWekaResults l = new LearnerThatUsesWekaResults(parametersInnerLearner,evalCnf,argReferenceGraph,classifier,argInitialPTA,argDataCollector.markovHelper);
													l.setPairQualityCounter(pairQualityCounter);
													return l;
												}											
											};
											experimentRunner.submitTask(learnerRunner);
										}
										{// second, use a traditional learner 
											PairQualityParameters parameters = new PairQualityParameters(pars);
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
											learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
											experimentRunner.submitTask(learnerRunner);
										}
										/*
										{// third, use EDSM-Markov learner, no premerge
											PairQualityParameters parameters = new PairQualityParameters(pars);
											parameters.setColumn("EDSM-Markov");
											parameters.markovParameters.setPresetLearningParameters(0);
											PairQualityLearnerRunner learnerRunner = new PairQualityLearnerRunner(null,parameters, learnerInitConfiguration)
											{
												@SuppressWarnings("unused")
												@Override
												public LearnerWithMandatoryMergeConstraints createLearner(LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph, WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
												{
													EDSM_MarkovLearner markovLearner = new EDSM_MarkovLearner(evalCnf,argInitialPTA,0,par.markovParameters);
													final MarkovModel m= new MarkovModel(par.markovParameters.chunkLen,true,true,false);

													new MarkovClassifier(m, argInitialPTA).updateMarkov(false);// construct Markov chain if asked for.
													
													argInitialPTA.clearColours();
													final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();
													markovLearner.setMarkov(m);markovLearner.setChecker(checker);
													return markovLearner;
												}
												
											};
											learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
											experimentRunner.submitTask(learnerRunner);
										}
										{// fourth, use EDSM-Markov learner, premerge
											PairQualityParameters parameters = new PairQualityParameters(pars);
											parameters.setColumn("EDSM-Markov");
											parameters.markovParameters.setPresetLearningParameters(0);
											PairQualityLearnerRunner learnerRunner = new PairQualityLearnerRunner(null,parameters, learnerInitConfiguration)
											{
												@SuppressWarnings("unused")
												@Override
												public LearnerWithMandatoryMergeConstraints createLearner(LearnerEvaluationConfiguration evalCnf,LearnerGraph argReferenceGraph, WekaDataCollector argDataCollector,	LearnerGraph argInitialPTA) 
												{
													EDSM_MarkovLearner markovLearner = new EDSM_MarkovLearner(evalCnf,argInitialPTA,0,par.markovParameters);
													final MarkovModel m= new MarkovModel(par.markovParameters.chunkLen,true,true,false);

													new MarkovClassifier(m, argInitialPTA).updateMarkov(false);// construct Markov chain if asked for.
													
													argInitialPTA.clearColours();
													final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();
													markovLearner.setMarkov(m);markovLearner.setChecker(checker);
													return markovLearner;
												}
												
											};
											learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
											experimentRunner.submitTask(learnerRunner);
										}*/
										++seedForFSM;
									}
					    	
					    	experimentRunner.collectOutcomeOfExperiments(resultHandler);
					    	
					    	if (experimentRunner.getPhase() == PhaseEnum.COLLECT_RESULTS || experimentRunner.getPhase() == PhaseEnum.COLLECT_AVAILABLE)
					    	{
						    	// When we reach this point, the CSV is built so it is handy to process the data in it.
						    	DrawGraphs.spreadsheetToBagPlot(gr_NewToOrig, resultCSV, "Reference", 0, "WithClassifier", 0, null, null);
						    	DrawGraphs.spreadsheetAsDouble(new AggregateValues() {
									
									@Override
									public void merge(double reference, double actual) {
										if (reference > 0)
											gr_QualityForNumberOfTraces.add(traceQuantityToUse+"",actual/reference);
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
		finally
		{
			experimentRunner.successfulTermination();
			DrawGraphs.end();// this is necessary to ensure termination of the JVM runtime at the end of experiments.
		}
	}
}
