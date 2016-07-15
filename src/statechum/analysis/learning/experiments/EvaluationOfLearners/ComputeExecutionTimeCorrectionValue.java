/* Copyright (c) 2016 The University of Sheffield.
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
package statechum.analysis.learning.experiments.EvaluationOfLearners;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
import statechum.analysis.learning.DrawGraphs.TimeAndCorrection;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.EvaluationOfLearners.EvaluationOfLearnersParameters.LearningType;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;
import statechum.analysis.learning.experiments.UASExperiment.ScoringModeScore;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;

public class ComputeExecutionTimeCorrectionValue
{
	public static final String unknownValue = "UNKNOWN";
	
	public static void main(String []ignored)
	{
		String[] args={"COLLECT_RESULTS"};
		String directoryToUse[] = new String[]{"evaluation_of_learners_Apr_2016","E5_2650-benchmark_learners_Apr_2016","m6e_benchmark_learners_Apr_2016","regi_benchmark_learners_Apr_2016"};
		List<CSVExperimentResult> csvOfExperiment = new ArrayList<CSVExperimentResult>();
		for(final String directoryNamePrefix:directoryToUse)
		{
			final String directoryExperimentResult = "experimentresult"+File.separator;
			
			String outDir = "tmp"+File.separator+directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
			String outPathPrefix = outDir + File.separator;
			final RunSubExperiment<EvaluationOfLearnersParameters,EvaluationOfLearnersResult> experimentRunner = new RunSubExperiment<EvaluationOfLearnersParameters,EvaluationOfLearnersResult>(ExperimentRunner.getCpuNumber(),outPathPrefix + directoryExperimentResult,args);
			SGE_ExperimentRunner.configureCPUFreqNormalisation();
			LearnerEvaluationConfiguration eval = UASExperiment.constructLearnerInitConfiguration();
			GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");

			final int samplesPerFSMSize = 5;
			final int attemptsPerFSM = 2;

			final RBoxPlot<String> BCR_vs_experiment = new RBoxPlot<String>("experiment","BCR",new File(outPathPrefix+"BCR_vs_experiment.pdf"));
			final RBoxPlot<String> diff_vs_experiment = new RBoxPlot<String>("experiment","Structural difference",new File(outPathPrefix+"diff_vs_experiment.pdf"));

			final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(outPathPrefix+"results.csv"));
			csvOfExperiment.add(resultCSV);
	    	processSubExperimentResult<EvaluationOfLearnersParameters,EvaluationOfLearnersResult> resultHandler = new processSubExperimentResult<EvaluationOfLearnersParameters,EvaluationOfLearnersResult>() {

				@Override
				public void processSubResult(EvaluationOfLearnersResult result, RunSubExperiment<EvaluationOfLearnersParameters,EvaluationOfLearnersResult> experimentrunner) throws IOException 
				{
					ScoresForGraph difference = result.samples.get(0).actualLearner;
					StringBuffer csvLine = new StringBuffer();
					csvLine.append(difference.differenceBCR.getValue());
					CSVExperimentResult.addSeparator(csvLine);csvLine.append(difference.differenceStructural.getValue());
					CSVExperimentResult.addSeparator(csvLine);csvLine.append(difference.nrOfstates.getValue());
					CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(difference.executionTime/1000000000.));// execution time is in nanoseconds, we only need seconds.
					experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
					String experimentName = result.parameters.states+"-"+result.parameters.traceQuantity+"-"+result.parameters.lengthmult+"_"+result.parameters.uniqueAsString()+"-"+result.parameters.getColumnID();
					experimentrunner.RecordR(BCR_vs_experiment,experimentName ,difference.differenceBCR.getValue(),null,null);
					experimentrunner.RecordR(diff_vs_experiment,experimentName,difference.differenceStructural.getValue(),null,null);
				}
				
				@Override
				public SGEExperimentResult[] getGraphs() {
					return new SGEExperimentResult[]{BCR_vs_experiment,diff_vs_experiment,resultCSV};
				}
			};
			
			List<BenchmarkCPU> listOfExperiments = new ArrayList<BenchmarkCPU>();
			
			try
			{
				for(int states:new int[]{35})
					for(boolean unique:new boolean[]{true,false})
					{
						int seedThatIdentifiesFSM=0;
						for(int sample=0;sample<samplesPerFSMSize;++sample,++seedThatIdentifiesFSM)
							for(int attempt=0;attempt<attemptsPerFSM;++attempt)
							{
								for(Configuration.STATETREE matrix:new Configuration.STATETREE[]{Configuration.STATETREE.STATETREE_ARRAY,// this one will switch to LINKEDHASH when the amount of data is small.
										//Configuration.STATETREE.STATETREE_LINKEDHASH
										})
									for(boolean pta:new boolean[]{false}) // the choice of using PTA or not does not make a significant impact.
									{
										for(int traceQuantity:new int[]{1,8})
											for(int traceLengthMultiplier:new int[]{1,8})
												if (traceQuantity*traceLengthMultiplier == 64) // <= 64)
												for(ScoringModeScore scoringPair:new ScoringModeScore[]{
														new ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_EDSM_4),
														new ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_EDSM_6),/*
														new ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_EDSM_8),
														new ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_EDSM_10),
														new ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_EDSM_12),
														new ScoringModeScore(Configuration.ScoreMode.GENERAL_PLUS_NOFULLMERGE,ScoringToApply.SCORING_EDSM_4),
														new ScoringModeScore(Configuration.ScoreMode.GENERAL_PLUS_NOFULLMERGE,ScoringToApply.SCORING_EDSM_6),
														new ScoringModeScore(Configuration.ScoreMode.GENERAL_PLUS_NOFULLMERGE,ScoringToApply.SCORING_EDSM_8),*/
														new ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_SICCO),
												})
												{
													LearnerEvaluationConfiguration ev = new LearnerEvaluationConfiguration(eval);
													ev.config.setOverride_maximalNumberOfStates(states*LearningAlgorithms.maxStateNumberMultiplier);
													ev.config.setOverride_usePTAMerging(pta);ev.config.setTransitionMatrixImplType(matrix);
													ev.config.setAlwaysUseTheSameMatrixType(false);
													EvaluationOfLearnersParameters par = new EvaluationOfLearnersParameters(scoringPair.scoringForEDSM,scoringPair.scoringMethod,null,pta,matrix);
													par.setParameters(states, sample, attempt, seedThatIdentifiesFSM, traceQuantity, traceLengthMultiplier);
													par.setPickUniqueFromInitial(unique);
													BenchmarkCPU learnerRunner = new BenchmarkCPU(par, ev);
													//learnerRunner.setAlwaysRunExperiment(true);
													listOfExperiments.add(learnerRunner);
												}
									}
							}
					}
			}
			catch(Exception ex)
			{
				Helper.throwUnchecked("failed to compute", ex);
			}

			try
			{
		    	for(BenchmarkCPU e:listOfExperiments)
		    		experimentRunner.submitTask(e);
		    	experimentRunner.collectOutcomeOfExperiments(resultHandler);
			}
			finally
			{
				experimentRunner.successfulTermination();
			}
		}
		System.out.println();
		// Now we have all the spreadsheets, use them to compute correction.
		EvaluationOfLearnersParameters par = new EvaluationOfLearnersParameters(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_SICCO,LearningType.PREMERGE,false,Configuration.STATETREE.STATETREE_ARRAY);
		// parameters above are dummy - they are needed to construct an instance but only values hardwired into EvaluationOfLearnersParameters are used.
		TimeAndCorrection tc = DrawGraphs.computeTimeAndCorrection(csvOfExperiment.get(0), csvOfExperiment.get(1), par, 3600L*4L,10,1);
		System.out.println("iceberg v.s. S5520sc, correction: "+tc.average+", stDev: "+tc.stdev+" number of pairs: "+tc.count);
		tc = DrawGraphs.computeTimeAndCorrection(csvOfExperiment.get(0), csvOfExperiment.get(2), par, 3600L*4L,50,1);
		System.out.println("m6e v.s. S5520sc, correction: "+tc.average+", stDev: "+tc.stdev+" number of pairs: "+tc.count);
		tc = DrawGraphs.computeTimeAndCorrection(csvOfExperiment.get(0), csvOfExperiment.get(3), par, 3600L*4L,20,1.0/0.4);
		System.out.println("regi v.s. S5520sc, correction: "+tc.average+", stDev: "+tc.stdev+" number of pairs: "+tc.count);
		DrawGraphs.end();// this is necessary to ensure termination of the JVM runtime at the end of experiments.
		
	}
}
