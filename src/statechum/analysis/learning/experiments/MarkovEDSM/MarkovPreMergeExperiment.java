/* Copyright (c) 2013 The University of Sheffield.
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

package statechum.analysis.learning.experiments.MarkovEDSM;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicLong;

import statechum.GlobalConfiguration;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.Wilcoxon;
import statechum.analysis.learning.DrawGraphs.Mann_Whitney_U_Test;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
import statechum.analysis.learning.DrawGraphs.AggregateStringValues;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.Kruskal_Wallis;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.MarkovLearnerRunner;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovLearningParameters.LearnerToUseEnum;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.SampleData;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;

public class MarkovPreMergeExperiment
{
	
	public static final String directoryNamePrefix = "premerge_june_2016";
	public static final String directoryExperimentResult = "experimentresult"+File.separator;
	
	public static void main(String []args)
	{
		String outDir = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_EXPERIMENTRESULTS)+File.separator+directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		UASExperiment.mkDir(outDir);
		String outPathPrefix = outDir + File.separator;
		LearnerEvaluationConfiguration eval = UASExperiment.constructLearnerInitConfiguration();
		eval.config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);eval.config.setLearnerScoreMode(ScoreMode.GENERAL_NOFULLMERGE);
		eval.config.setTimeOut(3600000L*4L);// timeout for tasks, in milliseconds, equivalent to 4hrs runtime for an old Xeon 5670 @ 2.93Ghz, modern E5/i7 are 3x faster.
		
		DrawGraphs gr = new DrawGraphs();
		
		final int samplesPerFSM = 100;
		final int trainingSamplesPerFSM = 2;
		final int traceQuantity = 1;
		final double traceLengthMultiplierMax = 10;
		final int chunkSize = 3;
		final int statesToUse[] = new int[]{10,20,40};
		SGE_ExperimentRunner.configureCPUFreqNormalisation();
		
		RunSubExperiment<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>> experimentRunner = new RunSubExperiment<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>>(ExperimentRunner.getCpuNumber(),outPathPrefix + directoryExperimentResult,args);
		SGE_ExperimentRunner.configureCPUFreqNormalisation();
		statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum phase = experimentRunner.getPhase();

		// Inference from a few traces
		final boolean onlyPositives=true;
		final double alphabetMultiplierMax=2;

		try
		{
		
		/*
		final RBoxPlotP<String> gr_BCRForDifferentLearners = new RBoxPlotP<String>("","BCR",new File(branch+"BCR_learner.pdf"));
		final RBoxPlotP<String> gr_StructuralForDifferentLearners = new RBoxPlotP<String>("","structural",new File(branch+"structural_learner.pdf"));
		{
			try
			{
				int numberOfTasks = 0;
				for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
					for(int sample=0;sample<samplesPerFSM;++sample)
					{
						Cav2014.EvaluationOfExisingLearnerRunner learnerRunner = new Cav2014.EvaluationOfExisingLearnerRunner(states,sample,numberOfTasks,traceQuantity, config, converter);
						learnerRunner.setOnlyUsePositives(onlyPositives);
						learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
						learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);learnerRunner.setChunkLen(chunkSize);
						learnerRunner.setSelectionID(branch+"_states"+states+"_sample"+sample);
						runner.submit(learnerRunner);
						++numberOfTasks;
					}
				ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for the behaviour of different learners", numberOfTasks);
				for(int count=0;count < numberOfTasks;++count)
				{
					ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
					for(SampleData sample:result.samples)
						for(Entry<String,ScoresForGraph> score:sample.miscGraphs.entrySet())
							gr_StructuralForDifferentLearners.add(score.getKey(),score.getValue().differenceStructural.getValue());
				
					for(SampleData sample:result.samples)
						for(Entry<String,ScoresForGraph> score:sample.miscGraphs.entrySet())
							gr_BCRForDifferentLearners.add(score.getKey(),score.getValue().differenceBCR.getValue());

					progress.next();
					gr_BCRForDifferentLearners.drawInteractive(gr);gr_StructuralForDifferentLearners.drawInteractive(gr);
				}
				
			}
			catch(Exception ex)
			{
				IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
				if (executorService != null) { executorService.shutdownNow();executorService = null; }
				throw e;
			}
		}
		if (gr_BCRForDifferentLearners != null) gr_BCRForDifferentLearners.drawPdf(gr);if (gr_StructuralForDifferentLearners != null) gr_StructuralForDifferentLearners.drawPdf(gr);
		*/
/*
		for(final boolean useCentreVertex:new boolean[]{true,false})
		for(final boolean useDifferentScoringNearRoot:new boolean[]{true,false}) 
		for(final boolean mergeIdentifiedPathsAfterInference:new boolean[]{true,false}) 
		for(final boolean useClassifyToOrderPairs:new boolean[]{true,false})
			
		for(final int traceQuantity:new int[]{10})
		for(final double traceLengthMultiplier:new double[]{1})
			{
				
				final int traceQuantityToUse = traceQuantity;
				
				String selection = "c="+useCentreVertex+
						";r="+useDifferentScoringNearRoot+
						";m="+mergeIdentifiedPathsAfterInference+
						";o="+useClassifyToOrderPairs+
						";traceQuantity="+traceQuantity+";traceLengthMultiplier="+traceLengthMultiplier+";"+";alphabetMultiplier="+alphabetMultiplier+";";
				SquareBagPlot gr_StructuralDiff = new SquareBagPlot("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File(branch+"_"+selection+"_trace_structuraldiff.pdf"),0,1,true);
				SquareBagPlot gr_BCR = new SquareBagPlot("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+"_trace_bcr.pdf"),0.5,1,true);		
						try
						{
							int numberOfTasks = 0;
							for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
								for(int sample=0;sample<samplesPerFSM;++sample)
								{
									MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,numberOfTasks,traceQuantityToUse, config, converter);
									learnerRunner.setOnlyUsePositives(onlyPositives);
									learnerRunner.setAlphabetMultiplier(alphabetMultiplier);
									learnerRunner.setTraceLengthMultiplier(traceLengthMultiplier);
									learnerRunner.setChunkLen(chunkSize);
									learnerRunner.setSelectionID(selection);
									// The call below wiggles values that are encoded in presets and assigned via learnerRunner.setPresetLearningParametes
									learnerRunner.setlearningParameters(useCentreVertex, useDifferentScoringNearRoot, mergeIdentifiedPathsAfterInference, useClassifyToOrderPairs);
									runner.submit(learnerRunner);
									++numberOfTasks;
								}
							ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for learning whole graphs", numberOfTasks);
							for(int count=0;count < numberOfTasks;++count)
							{
								ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
								for(SampleData sample:result.samples)
									gr_StructuralDiff.add(sample.referenceLearner.differenceStructural.getValue(),sample.actualLearner.differenceStructural.getValue());
							
								for(SampleData sample:result.samples)
								{
									gr_BCR.add(sample.referenceLearner.differenceBCR.getValue(),sample.actualLearner.differenceBCR.getValue());
								}
								progress.next();
							}
							gr_StructuralDiff.drawInteractive(gr);
							gr_BCR.drawInteractive(gr);
						}
						catch(Exception ex)
						{
							IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
							if (executorService != null) { executorService.shutdownNow();executorService = null; }
							throw e;
						}
						if (gr_StructuralDiff != null) gr_StructuralDiff.drawPdf(gr);
						if (gr_BCR != null) gr_BCR.drawPdf(gr);
			}
*/
		
		final int statesMax = statesToUse[statesToUse.length-1];// reflects the size of the largest FSM that will be generated. 
		final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(outPathPrefix+"results.csv"));
		for(final int preset: new int[]{1,0})
		{
			final int traceQuantityToUse = traceQuantity;
			int seedForFSM = 0;

			for(int states:statesToUse)
			for(int density:new int[] {0,3})
			{
				for(int sample=0;sample<samplesPerFSM;++sample,++seedForFSM)
					for(int trainingSample=0;trainingSample<trainingSamplesPerFSM;++trainingSample)
						for(boolean aveOrMax:new boolean[]{false})
							for(int wlen:new int[]{1,2})
								for(int divisor:new int[]{2,4})
									for(int positionOfMostConnectedVertex:(preset == 0?new int[]{0}:new int []{0,1,2}))
										for(LearnerToUseEnum learnerKind:new LearnerToUseEnum[]{LearnerToUseEnum.LEARNER_EDSMMARKOV,LearnerToUseEnum.LEARNER_SICCO,LearnerToUseEnum.LEARNER_KTAILS_1})
											for(double weightOfInconsistencies:learnerKind == LearnerToUseEnum.LEARNER_EDSMMARKOV?new double[]{1.0,2.0,4.0}:new double[]{1.0})
											{
												LearnerEvaluationConfiguration ev = new LearnerEvaluationConfiguration(eval);
												ev.config = eval.config.copy();ev.config.setOverride_maximalNumberOfStates(states*LearningAlgorithms.maxStateNumberMultiplier);
												ev.config.setOverride_usePTAMerging(false);
					
												MarkovLearningParameters parameters = new MarkovLearningParameters(learnerKind,states, alphabetMultiplierMax, density, sample,trainingSample, seedForFSM);
												parameters.setOnlyUsePositives(onlyPositives);
												parameters.setTracesAlphabetMultiplier(alphabetMultiplierMax);
												parameters.setTraceLengthMultiplier(traceLengthMultiplierMax);
												parameters.setExperimentID(traceQuantityToUse,traceLengthMultiplierMax,statesMax,alphabetMultiplierMax);
												parameters.markovParameters.setMarkovParameters(preset, chunkSize,true,weightOfInconsistencies, aveOrMax,divisor,positionOfMostConnectedVertex,wlen);
												parameters.setDisableInconsistenciesInMergers(false);
												parameters.setUsePrintf(experimentRunner.isInteractive());
												MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(parameters, ev);
												learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
												experimentRunner.submitTask(learnerRunner);
											}
			}
		}
		
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>>() {

			@Override
			public void processSubResult(ExperimentResult<MarkovLearningParameters> result, RunSubExperiment<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>> experimentrunner) throws IOException 
			{// in these experiments, samples are singleton sequences because we run each of them in a separate process, in order to increase the efficiency with which all tasks are split between CPUs in an iceberg grid.
				SampleData sm = result.samples.get(0);
				ScoresForGraph data=sm.actualLearner;
				
				StringBuffer csvLine = new StringBuffer();
				csvLine.append(data.whetherLearningSuccessfulOrAborted);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceBCR.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceStructural.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.invalidMergers);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.missedMergers);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.nrOfstates.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.inconsistencyReference);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistency);

				if (result.parameters.learnerToUse == LearnerToUseEnum.LEARNER_EDSMMARKOV)
				{
					CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.fractionOfStatesIdentifiedBySingletons);
					CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovPrecision);
					CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovRecall);
					CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.comparisonsPerformed);
				}
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(Boolean.toString(sm.centreCorrect));
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(Integer.toString(sm.centrePathNumber));
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(Long.toString(sm.transitionsSampled));
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(data.executionTime/1000000000.));// execution time is in nanoseconds, we only need seconds.
				experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
			}
			
			@Override
			public SGEExperimentResult[] getGraphs() {
				
				return new SGEExperimentResult[]{resultCSV};
			}
			
		});
		
		for(final int preset: new int[]{0,1})
		{
			if (phase == PhaseEnum.COLLECT_AVAILABLE || phase == PhaseEnum.COLLECT_RESULTS)
			{// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
				String experimentName = outPathPrefix+"preset"+preset+"_";
				final SquareBagPlot gr_StructuralDiff = new SquareBagPlot("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File(outPathPrefix+preset+"_"+statesMax+"_trace_structuraldiff.pdf"),0,1,true);
				final SquareBagPlot gr_BCR = new SquareBagPlot("BCR, Sicco","BCR, EDSM-Markov learner",new File(outPathPrefix+preset+"_"+statesMax+"_trace_bcr.pdf"),0.5,1,true);		
				final SquareBagPlot BCRAgainstKtails = new SquareBagPlot("BCR, K-tails,1","BCR, EDSM-Markov learner",new File(outPathPrefix+preset+"_"+statesMax+"_trace_kt_bcr.pdf"),0.5,1,true);		
				final SquareBagPlot BCRAgainstMarkov = new SquareBagPlot("BCR, Markov","BCR, EDSM-Markov learner",new File(outPathPrefix+preset+"_"+statesMax+"_trace_markov_bcr.pdf"),0.5,1,true);		

				final Wilcoxon Wilcoxon_test_Structural=new Wilcoxon(new File(experimentName +"Wilcoxon_t_str.csv"));		 
				final Wilcoxon Wilcoxon_Test_BCR=new Wilcoxon(new File(experimentName +"Wilcoxon_t_bcr.csv"));		 
				final Mann_Whitney_U_Test Mann_Whitney_U_Test_BCR=new Mann_Whitney_U_Test(new File(experimentName +"Mann_Whitney_U_Test_BCR.csv"));		 
				final Mann_Whitney_U_Test Mann_Whitney_U_Test_Structural=new Mann_Whitney_U_Test(new File(experimentName +"Whitney_U_Test_str.csv"));		 
				final Kruskal_Wallis Kruskal_Wallis_Test_BCR=new Kruskal_Wallis(new File(experimentName +"Kruskal_Wallis_Test_BCR.csv"));		 
				final Kruskal_Wallis Kruskal_Wallis_Test_Structural=new Kruskal_Wallis(new File(experimentName +"Kruskal_Wallis_Test_str.csv"));		 	 

				DrawGraphs.spreadsheetToBagPlot(gr_StructuralDiff,resultCSV,LearnerToUseEnum.LEARNER_SICCO.name(),1,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),1,null,null);
				DrawGraphs.spreadsheetToBagPlot(gr_BCR,resultCSV,LearnerToUseEnum.LEARNER_SICCO.name(),0,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),0,null,null);
				DrawGraphs.spreadsheetToBagPlot(BCRAgainstKtails,resultCSV,LearnerToUseEnum.LEARNER_KTAILS_1.name(),0,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),0,null,null);
				DrawGraphs.spreadsheetToBagPlot(BCRAgainstMarkov,resultCSV,LearnerToUseEnum.LEARNER_KTAILS_1.name(),0,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),0,null,null);
				
				DrawGraphs.spreadsheetAsDouble(Wilcoxon_Test_BCR,resultCSV,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),0,LearnerToUseEnum.LEARNER_SICCO.name(),0);
				DrawGraphs.spreadsheetAsDouble(Wilcoxon_test_Structural,resultCSV,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),1,LearnerToUseEnum.LEARNER_SICCO.name(),1);
				DrawGraphs.spreadsheetAsDouble(Mann_Whitney_U_Test_BCR,resultCSV,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),0,LearnerToUseEnum.LEARNER_SICCO.name(),0);
				DrawGraphs.spreadsheetAsDouble(Mann_Whitney_U_Test_Structural,resultCSV,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),1,LearnerToUseEnum.LEARNER_SICCO.name(),1);
				DrawGraphs.spreadsheetAsDouble(Kruskal_Wallis_Test_BCR,resultCSV,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),0,LearnerToUseEnum.LEARNER_SICCO.name(),0);
				DrawGraphs.spreadsheetAsDouble(Kruskal_Wallis_Test_Structural,resultCSV,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),1,LearnerToUseEnum.LEARNER_SICCO.name(),1);
				
				final AtomicLong comparisonsPerformed = new AtomicLong(0);
				DrawGraphs.spreadsheetAsString(new AggregateStringValues() {
					@Override
					public void merge(String A, @SuppressWarnings("unused") String B) {
						comparisonsPerformed.addAndGet(Long.parseLong(A));
					}},resultCSV,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),3,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),3);
					
				for(@SuppressWarnings("rawtypes") DrawGraphs.RExperimentResult result:new DrawGraphs.RExperimentResult[]{gr_StructuralDiff,gr_BCR,BCRAgainstKtails,BCRAgainstMarkov, Wilcoxon_Test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,Kruskal_Wallis_Test_Structural,Kruskal_Wallis_Test_BCR})
				{
					result.reportResults(gr);
				}
				if (experimentRunner.isInteractive())
					System.out.println("\nLOG of comparisons performed: "+Math.log10(comparisonsPerformed.doubleValue())+"\n");
			}
		}
		
		
/*		final int traceQuantityToUse = traceQuantity;
		final int presetForBestResults = 0;
		{
			final SquareBagPlot gr_StructuralDiffWithoutInconsistencies = new SquareBagPlot("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File(branch+"_noinconsistencies_trace_structuraldiff.pdf"),0,1,true);
			final SquareBagPlot gr_BCRWithoutInconsistencies = new SquareBagPlot("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_noinconsistencies_trace_bcr.pdf"),0.5,1,true);		
			String selection = "noinconsistencies;quantity="+traceQuantity+";tracelen="+traceLengthMultiplierMax+";alphabetMult="+alphabetMultiplierMax+";";
			final AtomicLong comparisonsPerformed = new AtomicLong(0);

			for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
				for(int sample=0;sample<samplesPerFSM;++sample)
				{
					MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,experimentRunner.getTaskID(),traceQuantityToUse, config, converter);
					learnerRunner.setOnlyUsePositives(onlyPositives);
					learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
					learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);
					learnerRunner.setChunkLen(chunkSize);
					learnerRunner.setSelectionID(selection);
					learnerRunner.setPresetLearningParameters(presetForBestResults);
					learnerRunner.setDisableInconsistenciesInMergers(true);
					experimentRunner.submitTask(learnerRunner);
				}
			experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

				@Override
				public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
				{
					for(SampleData sample:result.samples)
						experimentrunner.Record(gr_StructuralDiffWithoutInconsistencies,sample.referenceLearner.differenceStructural.getValue(),sample.actualLearner.differenceStructural.getValue(),null,null);
				
					for(SampleData sample:result.samples)
					{
						experimentrunner.Record(gr_BCRWithoutInconsistencies,sample.referenceLearner.differenceBCR.getValue(),sample.actualLearner.differenceBCR.getValue(),null,null);
						comparisonsPerformed.addAndGet(sample.comparisonsPerformed);
					}
					
				}

				@Override
				public String getSubExperimentName()
				{
					return "learning without inconsistencies";
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiffWithoutInconsistencies,gr_BCRWithoutInconsistencies};
				}
			});
			
			if (experimentRunner.isInteractive())
				System.out.println("\nLOG of comparisons performed: "+Math.log10(comparisonsPerformed.doubleValue())+"\n");
		}*/

		// Same experiment but with different number of sequences.
/*		final RBoxPlot<Integer> gr_BCRImprovementForDifferentNrOfTracesWithNegatives = new RBoxPlot<Integer>("nr of traces","improvement, BCR",new File(branch+"WithNegatives_BCR_vs_tracenumber.pdf"));
		final RBoxPlot<Integer> gr_BCRForDifferentNrOfTracesWithNegatives = new RBoxPlot<Integer>("nr of traces","BCR",new File(branch+"WithNegatives_BCR_absolute_vs_tracenumber.pdf"));
		final RBoxPlot<Integer> gr_StructuralImprovementForDifferentNrOfTracesWithNegatives = new RBoxPlot<Integer>("nr of traces","improvement, structural",new File(branch+"WithNegatives_structural_vs_tracenumber.pdf"));
		final RBoxPlot<Integer> gr_StructuralForDifferentNrOfTracesWithNegatives = new RBoxPlot<Integer>("nr of traces","structural",new File(branch+"WithNegatives_structural_absolute_vs_tracenumber.pdf"));
			
		for(final int traceNum:new int[]{2})
		{
			final String selection = "number_of_traces="+traceNum;
			for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
			{
				final Wilcoxon <String> Wilcoxon_test_BCR=new Wilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_traceNum= "+traceNum+"_"+states+"Wilcoxon_trace_bcr.pdf"));		 
				final Wilcoxon <String> Wilcoxon_test_Structural=new Wilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_traceNum= "+traceNum+"_"+states+"Wilcoxon_trace_str.pdf"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_BCR=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_traceNum= "+traceNum+"_"+states+"Mann_Whitney_U_Test_BCR.pdf"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_Structural=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_traceNum= "+traceNum+"_"+states+"Mann_Whitney_U_Test_str.pdf"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_BCR=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_traceNum= "+traceNum+"_"+states+"Kruskal_Wallis_Test_BCR.pdf"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_Structural=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_traceNum= "+traceNum+"_"+states+"Kruskal_Wallis_Test_str.pdf"));		 
				for(int sample=0;sample<samplesPerFSM;++sample)
				{
					MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,experimentRunner.getTaskID(),traceNum, config, converter);
					learnerRunner.setOnlyUsePositives(false);
					learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
					learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);
					learnerRunner.setChunkLen(chunkSize);
					learnerRunner.setSelectionID(selection);
					learnerRunner.setPresetLearningParameters(presetForBestResults);
					experimentRunner.submitTask(learnerRunner);
				}
			experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

				@Override
				public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
				{
					for(SampleData sample:result.samples)
					{
						if (sample.referenceLearner.differenceBCR.getValue() > 0)
						{
							// we'll generate both positives and negatives; in the considered experiments, only positives are used hence half the number of sequences are actually being learnt from.
							experimentrunner.Record(gr_BCRImprovementForDifferentNrOfTracesWithNegatives,traceNum,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue(),null,null);
							experimentrunner.Record(gr_BCRForDifferentNrOfTracesWithNegatives,traceNum,sample.actualLearner.differenceBCR.getValue(),null,null);
						}
						if (sample.referenceLearner.differenceStructural.getValue() > 0)
						{
							experimentrunner.Record(gr_StructuralImprovementForDifferentNrOfTracesWithNegatives,traceNum,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue(),null,null);
							experimentrunner.Record(gr_StructuralForDifferentNrOfTracesWithNegatives,traceNum,sample.actualLearner.differenceStructural.getValue(),null,null);
						}
					}
					
			
					AverageValue BCRCollectResult = new AverageValue(0.0,0.0);
					AverageValue StructuralCollectResult = new AverageValue(0.0,0.0);

					double n=0.0;

					for(SampleData sample:result.samples)
					{
						BCRCollectResult.add(sample.actualLearner.differenceBCR.getValue(), sample.referenceLearner.differenceBCR.getValue());
						StructuralCollectResult.add(sample.actualLearner.differenceStructural.getValue(), sample.referenceLearner.differenceStructural.getValue());
						n++;
					}

					experimentrunner.RecordPairValue(Wilcoxon_test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
					experimentrunner.RecordPairValue(Wilcoxon_test_Structural, StructuralCollectResult.RefercneElem, StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
					experimentrunner.RecordPairValue(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
					experimentrunner.RecordPairValue(Mann_Whitney_U_Test_Structural, StructuralCollectResult.RefercneElem, StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
					experimentrunner.RecordPairValue(Kruskal_Wallis_Test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
					experimentrunner.RecordPairValue(Kruskal_Wallis_Test_Structural, StructuralCollectResult.RefercneElem, StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
					
					
				}

				@Override
				public String getSubExperimentName()
				{
					return selection;
				}
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_BCRImprovementForDifferentNrOfTracesWithNegatives,gr_BCRForDifferentNrOfTracesWithNegatives,
							gr_StructuralImprovementForDifferentNrOfTracesWithNegatives,gr_StructuralForDifferentNrOfTracesWithNegatives,Wilcoxon_test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,Kruskal_Wallis_Test_BCR,Kruskal_Wallis_Test_Structural};
				}
			});
			}
		}			
*/
		/*
		// Same experiment but with different number of sequences.
		final RBoxPlot<Integer> gr_BCRImprovementForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","improvement, BCR",new File(branch+"BCR_vs_tracenumber.pdf"));
		final RBoxPlot<Integer> gr_BCRForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","BCR",new File(branch+"BCR_absolute_vs_tracenumber.pdf"));
		final RBoxPlot<Integer> gr_StructuralImprovementForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","improvement, structural",new File(branch+"structural_vs_tracenumber.pdf"));
		final RBoxPlot<Integer> gr_StructuralForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","structural",new File(branch+"structural_absolute_vs_tracenumber.pdf"));
		
		
		for(final int traceNum:new int[]{2,4,6,8,10})
		{
			final String selection = "number_of_traces="+traceNum;	

			for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
			{
				final RWilcoxon <String> Wilcoxon_test_Structural=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+"_states_"+ states +"_Wilcoxon_t_str.csv"));		 
				final RWilcoxon <String> Wilcoxon_Test_BCR=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Wilcoxon_t_bcr.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_BCR=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Mann_Whitney_U_Test_BCR.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_Structural=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_= "+selection+ "_states_"+ states +"_Whitney_U_Test_str.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_BCR=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_BCR.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_Structural=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_str.csv"));		 	 

				for(int sample=0;sample<samplesPerFSM;++sample)
				{
					MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,experimentRunner.getTaskID(),traceNum, config, converter);
					learnerRunner.setOnlyUsePositives(onlyPositives);
					learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
					learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);
					learnerRunner.setChunkLen(chunkSize);
					learnerRunner.setSelectionID(selection);
					learnerRunner.setPresetLearningParameters(presetForBestResults);
					experimentRunner.submitTask(learnerRunner);
				}
			experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

				@Override
				public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
				{
					for(SampleData sample:result.samples)
					{
						if (sample.referenceLearner.differenceBCR.getValue() > 0)
						{
							// we'll generate both positives and negatives; in the considered experiments, only positives are used hence half the number of sequences are actually being learnt from.
							experimentrunner.Record(gr_BCRImprovementForDifferentNrOfTraces,traceNum/2,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue(),null,null);
							experimentrunner.Record(gr_BCRForDifferentNrOfTraces,traceNum/2,sample.actualLearner.differenceBCR.getValue(),null,null);
						}
						if (sample.referenceLearner.differenceStructural.getValue() > 0)
						{
							experimentrunner.Record(gr_StructuralImprovementForDifferentNrOfTraces,traceNum/2,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue(),null,null);
							experimentrunner.Record(gr_StructuralForDifferentNrOfTraces,traceNum/2,sample.actualLearner.differenceStructural.getValue(),null,null);
						}
					}
						
						AverageValue BCRCollectResult = new AverageValue(0.0,0.0);
						AverageValue StructuralCollectResult = new AverageValue(0.0,0.0);

						double n=0.0;

						for(SampleData sample:result.samples)
						{
							
								BCRCollectResult.add(sample.actualLearner.differenceBCR.getValue(), sample.referenceLearner.differenceBCR.getValue());														
								StructuralCollectResult.add(sample.actualLearner.differenceStructural.getValue(), sample.referenceLearner.differenceStructural.getValue());
								n++;
								experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
								experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
								experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
								experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
								experimentrunner.Record(Kruskal_Wallis_Test_BCR,BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
								experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
						}

//						experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Wilcoxon_test_Structural, StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Kruskal_Wallis_Test_BCR, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Kruskal_Wallis_Test_Structural, StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
					
				}

				@Override
				public String getSubExperimentName()
				{
					return selection;
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_BCRImprovementForDifferentNrOfTraces,gr_BCRForDifferentNrOfTraces,gr_StructuralImprovementForDifferentNrOfTraces,gr_StructuralForDifferentNrOfTraces,Wilcoxon_Test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,Kruskal_Wallis_Test_BCR,Kruskal_Wallis_Test_Structural};

				}
			});

		}
		
		}

		// Same experiment but with different trace length but the same number of sequences
		final RBoxPlot<Double> gr_BCRImprovementForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","improvement, BCR",new File(branch+"BCR_vs_tracelength.pdf"));
		final RBoxPlot<Double> gr_BCRForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","BCR",new File(branch+"BCR_absolute_vs_tracelength.pdf"));
		final RBoxPlot<Double> gr_StructuralImprovementForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","improvement, structural",new File(branch+"structural_vs_tracelength.pdf"));
		final RBoxPlot<Double> gr_StructuralForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","structural",new File(branch+"structural_absolute_vs_tracelength.pdf"));
		final RBoxPlot<Double> gr_TransitionCoverageForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","transition coverage",new File(branch+"transitionCoverage_vs_tracelength.pdf"));

		for(final int traceNum:new int[]{10})
			for(double traceLengthMultiplierToUse=0.125;traceLengthMultiplierToUse<4;traceLengthMultiplierToUse*=2.) 
			{
				final String selection="traceLengthMultiplier="+traceLengthMultiplierToUse;
				final double traceLengthMultToUse = traceLengthMultiplierToUse;
				for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
				{
					final RWilcoxon <String> Wilcoxon_test_Structural=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+"_states="+ states +"_Wilcoxon_t_str.csv"));		 
					final RWilcoxon <String> Wilcoxon_Test_BCR=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Wilcoxon_t_bcr.csv"));		 
					final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_BCR=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Mann_Whitney_U_Test_BCR.csv"));		 
					final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_Structural=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_= "+selection+ "_states="+ states +"_Whitney_U_Test_str.csv"));		 
					final Kruskal_Wallis <String> Kruskal_Wallis_Test_BCR=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_BCR.csv"));		 
					final Kruskal_Wallis <String> Kruskal_Wallis_Test_Structural=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_str.csv"));		 	 

					for(int sample=0;sample<samplesPerFSM;++sample)
					{
						MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,experimentRunner.getTaskID(),traceNum, config, converter);
						learnerRunner.setOnlyUsePositives(onlyPositives);
						learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
						learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierToUse);
						learnerRunner.setChunkLen(chunkSize);
						learnerRunner.setSelectionID(selection);
						learnerRunner.setPresetLearningParameters(presetForBestResults);
						experimentRunner.submitTask(learnerRunner);
					}
				experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

					@Override
					public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
					{
						for(SampleData sample:result.samples)
						{
							if (sample.referenceLearner.differenceBCR.getValue() > 0)
							{
								experimentrunner.Record(gr_BCRImprovementForDifferentLengthOfTraces,traceLengthMultToUse,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue(),null,null);
								experimentrunner.Record(gr_BCRForDifferentLengthOfTraces,traceLengthMultToUse,sample.actualLearner.differenceBCR.getValue(),null,null);
							}
							if (sample.referenceLearner.differenceStructural.getValue() > 0)
							{
								experimentrunner.Record(gr_StructuralImprovementForDifferentLengthOfTraces,traceLengthMultToUse,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue(),null,null);
								experimentrunner.Record(gr_StructuralForDifferentLengthOfTraces,traceLengthMultToUse,sample.actualLearner.differenceStructural.getValue(),null,null);
							}
							experimentrunner.Record(gr_TransitionCoverageForDifferentLengthOfTraces,traceLengthMultToUse,(double)sample.transitionsSampled,null,null);
						}
						
						AverageValue BCRCollectResult = new AverageValue(0.0,0.0);
						AverageValue StructuralCollectResult = new AverageValue(0.0,0.0);

						double n=0.0;

						for(SampleData sample:result.samples)
						{
							BCRCollectResult.add(sample.actualLearner.differenceBCR.getValue(), sample.referenceLearner.differenceBCR.getValue());
							StructuralCollectResult.add(sample.actualLearner.differenceStructural.getValue(), sample.referenceLearner.differenceStructural.getValue());
							n++;
							experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
							experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
							experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
							experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
							experimentrunner.Record(Kruskal_Wallis_Test_BCR,BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
							experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
						}

//						experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Kruskal_Wallis_Test_BCR,BCRCollectResult.RefercneElem, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//						experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
						
					}

					@Override
					public String getSubExperimentName()
					{
						return selection;
					}
					
					@SuppressWarnings("rawtypes")
					@Override
					public RGraph[] getGraphs() {
						return new RGraph[]{Wilcoxon_Test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,gr_BCRImprovementForDifferentLengthOfTraces,gr_BCRForDifferentLengthOfTraces,gr_StructuralImprovementForDifferentLengthOfTraces,
								gr_StructuralForDifferentLengthOfTraces,gr_TransitionCoverageForDifferentLengthOfTraces,Kruskal_Wallis_Test_Structural,Kruskal_Wallis_Test_BCR};
					}
				});
				}
			}

		final RBoxPlot<Integer> gr_BCRImprovementForDifferentPrefixlen = new RBoxPlot<Integer>("length of prefix","improvement, BCR",new File(branch+"BCR_vs_prefixLength.pdf"));
		final RBoxPlot<Integer> gr_BCRForDifferentPrefixlen = new RBoxPlot<Integer>("length of prefix","BCR",new File(branch+"BCR_absolute_vs_prefixLength.pdf"));
		final RBoxPlot<Integer> gr_StructuralImprovementForDifferentPrefixlen = new RBoxPlot<Integer>("length of prefix","improvement, structural",new File(branch+"structural_vs_prefixLength.pdf"));
		final RBoxPlot<Integer> gr_StructuralForDifferentPrefixlen = new RBoxPlot<Integer>("length of prefix","structural",new File(branch+"structural_absolute_vs_prefixLength.pdf"));
		final RBoxPlot<String> gr_MarkovAccuracyForDifferentPrefixlen = new RBoxPlot<String>("length of prefix","Markov accuracy",new File(branch+"markov_accuracy_vs_prefixLength.pdf"));
		for(int prefixLength=1;prefixLength<3;++prefixLength) 
		{
			final String selection="prefix Length ="+prefixLength;
			final int prefixLen = prefixLength;
			for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
			{
				final RWilcoxon <String> Wilcoxon_test_Structural=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+"_states="+ states +"_Wilcoxon_t_str.csv"));		 
				final RWilcoxon <String> Wilcoxon_Test_BCR=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Wilcoxon_t_bcr.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_BCR=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Mann_Whitney_U_Test_BCR.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_Structural=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_= "+selection+ "_states="+ states +"_Whitney_U_Test_str.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_BCR=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_BCR.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_Structural=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_str.csv"));		 	 
			
				for(int sample=0;sample<samplesPerFSM;++sample)
				{
					MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,experimentRunner.getTaskID(),traceQuantity, config, converter);
					learnerRunner.setOnlyUsePositives(onlyPositives);
					learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
					learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);
					learnerRunner.setChunkLen(prefixLength+1);
					learnerRunner.setSelectionID(selection);
					learnerRunner.setPresetLearningParameters(presetForBestResults);
					experimentRunner.submitTask(learnerRunner);
				}
			experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

				@Override
				public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
				{
					for(SampleData sample:result.samples)
					{
						if (sample.referenceLearner.differenceBCR.getValue() > 0)
						{
							experimentrunner.Record(gr_BCRImprovementForDifferentPrefixlen,prefixLen,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue(),null,null);
							experimentrunner.Record(gr_BCRForDifferentPrefixlen,prefixLen,sample.actualLearner.differenceBCR.getValue(),null,null);
						}
						if (sample.referenceLearner.differenceStructural.getValue() > 0)
						{
							experimentrunner.Record(gr_StructuralImprovementForDifferentPrefixlen,prefixLen,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue(),null,null);
							experimentrunner.Record(gr_StructuralForDifferentPrefixlen,prefixLen,sample.actualLearner.differenceStructural.getValue(),null,null);
						}
						experimentrunner.Record(gr_MarkovAccuracyForDifferentPrefixlen,""+prefixLen+",P",(double)sample.markovPrecision,"green",null);
						experimentrunner.Record(gr_MarkovAccuracyForDifferentPrefixlen,""+prefixLen+",R",(double)sample.markovRecall,"blue",null);
					}
					
					AverageValue BCRCollectResult = new AverageValue(0.0,0.0);
					AverageValue StructuralCollectResult = new AverageValue(0.0,0.0);

					double n=0.0;

					for(SampleData sample:result.samples)
					{
						BCRCollectResult.add(sample.actualLearner.differenceBCR.getValue(), sample.referenceLearner.differenceBCR.getValue());
						StructuralCollectResult.add(sample.actualLearner.differenceStructural.getValue(), sample.referenceLearner.differenceStructural.getValue());
						n++;
						experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
						experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
						experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
						experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
						experimentrunner.Record(Kruskal_Wallis_Test_BCR,BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
						experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
					}

//					experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Kruskal_Wallis_Test_BCR,BCRCollectResult.RefercneElem, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
				}

				@Override
				public String getSubExperimentName()
				{
					return selection;
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{Wilcoxon_Test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,gr_BCRImprovementForDifferentPrefixlen,gr_BCRForDifferentPrefixlen,gr_StructuralImprovementForDifferentPrefixlen,gr_StructuralForDifferentPrefixlen,gr_MarkovAccuracyForDifferentPrefixlen,Kruskal_Wallis_Test_Structural,Kruskal_Wallis_Test_BCR};
				}
			});
			}
		}
		
	

		final RBoxPlot<String> gr_BCRImprovementForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","improvement, BCR",new File(branch+"BCR_vs_alphabet.pdf"));
		final RBoxPlot<String> gr_BCRForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","BCR",new File(branch+"BCR_absolute_vs_alphabet.pdf"));
		final RBoxPlot<String> gr_StructuralImprovementForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","improvement, structural",new File(branch+"structural_vs_alphabet.pdf"));
		final RBoxPlot<String> gr_StructuralForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","structural",new File(branch+"structural_absolute_vs_alphabet.pdf"));
		final RBoxPlot<String> gr_MarkovAccuracyForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","Markov accuracy",new File(branch+"markov_accuracy_vs_alphabet.pdf"));

		// Same experiment but with different alphabet size
		for(final double alphabetMultiplierActual:new double[]{alphabetMultiplierMax/4,alphabetMultiplierMax/2,alphabetMultiplierMax,alphabetMultiplierMax*2,alphabetMultiplierMax*4}) 
		{
			final String selection="alphabet_size="+alphabetMultiplierActual;

			for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
			{
				
				final RWilcoxon <String> Wilcoxon_test_Structural=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+"_states="+ states +"_Wilcoxon_t_str.csv"));		 
				final RWilcoxon <String> Wilcoxon_Test_BCR=new RWilcoxon <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Wilcoxon_t_bcr.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_BCR=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states="+ states +"_Mann_Whitney_U_Test_BCR.csv"));		 
				final Mann_Whitney_U_Test <String> Mann_Whitney_U_Test_Structural=new Mann_Whitney_U_Test <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_= "+selection+ "_states="+ states +"_Whitney_U_Test_str.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_BCR=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_BCR.csv"));		 
				final Kruskal_Wallis <String> Kruskal_Wallis_Test_Structural=new Kruskal_Wallis <String>("BCR, Sicco","BCR, EDSM-Markov learner",new File(branch+"_"+selection+ "_states_"+ states +"_Kruskal_Wallis_Test_str.csv"));		 	 
			
				for(int sample=0;sample<samplesPerFSM;++sample)
				{
					MarkovLearnerRunner learnerRunner = new MarkovLearnerRunner(states,sample,experimentRunner.getTaskID(),traceQuantity, config, converter);
					learnerRunner.setOnlyUsePositives(onlyPositives);
					learnerRunner.setTracesAlphabetMultiplier(alphabetMultiplierMax);
					learnerRunner.setAlphabetMultiplier(alphabetMultiplierActual);
					learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);learnerRunner.setChunkLen(chunkSize);
					learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
					learnerRunner.setPresetLearningParameters(presetForBestResults);
					experimentRunner.submitTask(learnerRunner);
				}
			experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

				@Override
				public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
				{
					for(SampleData sample:result.samples)
					{
						if (sample.referenceLearner.differenceBCR.getValue() > 0)
						{
							experimentrunner.Record(gr_BCRImprovementForDifferentAlphabetSize,String.format("%.2f",alphabetMultiplierActual),sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue(),null,null);
							experimentrunner.Record(gr_BCRForDifferentAlphabetSize,String.format("%.2f",alphabetMultiplierActual),sample.actualLearner.differenceBCR.getValue(),null,null);
						}
						if (sample.referenceLearner.differenceStructural.getValue() > 0)
						{
							experimentrunner.Record(gr_StructuralImprovementForDifferentAlphabetSize,String.format("%.2f",alphabetMultiplierActual),sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue(),null,null);
							experimentrunner.Record(gr_StructuralForDifferentAlphabetSize,String.format("%.2f",alphabetMultiplierActual),sample.actualLearner.differenceStructural.getValue(),null,null);
						}
						experimentrunner.Record(gr_MarkovAccuracyForDifferentAlphabetSize,String.format("%.2f,P",alphabetMultiplierActual),(double)sample.markovPrecision,"green",null);
						experimentrunner.Record(gr_MarkovAccuracyForDifferentAlphabetSize,String.format("%.2f,R",alphabetMultiplierActual),(double)sample.markovRecall,"blue",null);
					}
					
					AverageValue BCRCollectResult = new AverageValue(0.0,0.0);
					AverageValue StructuralCollectResult = new AverageValue(0.0,0.0);

					double n=0.0;

					for(SampleData sample:result.samples)
					{
						BCRCollectResult.add(sample.actualLearner.differenceBCR.getValue(), sample.referenceLearner.differenceBCR.getValue());
						StructuralCollectResult.add(sample.actualLearner.differenceStructural.getValue(), sample.referenceLearner.differenceStructural.getValue());
						n++;
						experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
						experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
						experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
						experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
						experimentrunner.Record(Kruskal_Wallis_Test_BCR,BCRCollectResult.RefercneElem, BCRCollectResult.actualElem, null, null);
						experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem, StructuralCollectResult.actualElem, null, null);
					}

//					experimentrunner.Record(Wilcoxon_Test_BCR,  BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Wilcoxon_test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Mann_Whitney_U_Test_BCR, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Mann_Whitney_U_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Kruskal_Wallis_Test_BCR, BCRCollectResult.RefercneElem/n, BCRCollectResult.actualElem/n, null, null);
//					experimentrunner.Record(Kruskal_Wallis_Test_Structural,  StructuralCollectResult.RefercneElem/n, StructuralCollectResult.actualElem/n, null, null);
				}

				@Override
				public String getSubExperimentName()
				{
					return selection;
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{Wilcoxon_Test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,gr_BCRImprovementForDifferentAlphabetSize,gr_StructuralImprovementForDifferentAlphabetSize,gr_BCRForDifferentAlphabetSize,
							gr_StructuralForDifferentAlphabetSize,gr_MarkovAccuracyForDifferentAlphabetSize,Kruskal_Wallis_Test_Structural,Kruskal_Wallis_Test_BCR};
				}
			});
			}
		}
		*/

		}
		finally
		{
			experimentRunner.successfulTermination();
			DrawGraphs.end();// this is necessary to ensure termination of the JVM runtime at the end of experiments.
		}
	}
	
}

