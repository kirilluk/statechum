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

import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.AggregateStringValues;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.Kruskal_Wallis;
import statechum.analysis.learning.DrawGraphs.Mann_Whitney_U_Test;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.analysis.learning.DrawGraphs.Wilcoxon;
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
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.SampleData;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;


public class CVS_With_Random_traces_Generation
{
	public static final String directoryNamePrefix = "cvs_june_2016";
	public static final String directoryExperimentData = directoryNamePrefix+File.separator+"experimentdata"+File.separator;
	public static final String directoryExperimentResult = "experimentresult"+File.separator;

	public static class MarkovLearnerUsingReference extends MarkovLearnerRunner
	{
		public MarkovLearnerUsingReference(MarkovLearningParameters parameters, LearnerEvaluationConfiguration cnf, LearnerGraph ref) 
		{
			super(parameters, cnf);referenceGraph = ref;
		}
		
		/** Constructs a reference graph and assigns it to member variable <pre>referenceGraph</pre>. This is a separate method to permit overriding by subclasses.
		 */
		@Override
		public void generateReferenceFSM()
		{// does nothing- reference already assigned in the constructor
		}
	}
	
	public static void main(String args[]) throws Exception
	{
		String outDir = "tmp"+File.separator+directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		UASExperiment.mkDir(outDir);
		String outPathPrefix = outDir + File.separator;
		LearnerEvaluationConfiguration eval = UASExperiment.constructLearnerInitConfiguration();
		eval.config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);eval.config.setLearnerScoreMode(ScoreMode.GENERAL_NOFULLMERGE);
		eval.config.setTimeOut(3600000L*4L);// timeout for tasks, in milliseconds, equivalent to 4hrs runtime for an old Xeon 5670 @ 2.93Ghz, modern E5/i7 are 3x faster.
		
		DrawGraphs gr = new DrawGraphs();
		
		final int trainingSamplesPerFSM = 5;
		final int traceQuantity = 1;
		final double traceLengthMultiplierMax = 10;
		final int chunkSize = 3;
		SGE_ExperimentRunner.configureCPUFreqNormalisation();
		
		RunSubExperiment<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>> experimentRunner = new RunSubExperiment<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>>(ExperimentRunner.getCpuNumber(),outPathPrefix + directoryExperimentResult,args);
		statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum phase = experimentRunner.getPhase();

		// Inference from a few traces
		final boolean onlyPositives=true;
		final int alphabetMultiplierMax = 1;// dummy value - it is needed by parameters
		final LearnerGraph cvsReference = CVS.getCVSReference(eval); 
		final int states = cvsReference.getAcceptStateNumber();
		try
		{
			for(final int preset: new int[]{0})//0,1,2})
			{
				final int traceQuantityToUse = traceQuantity;
				int seedForFSM = 0;
				final AtomicLong comparisonsPerformed = new AtomicLong(0);
				MarkovLearningParameters parExp = new MarkovLearningParameters(null,0,0,0,0,0);
				parExp.setExperimentID(traceQuantity,traceLengthMultiplierMax,states,alphabetMultiplierMax);
				
					final String experimentName = outPathPrefix+parExp.getExperimentID();
					final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(experimentName+"results.csv"));
					
						for(int trainingSample=0;trainingSample<trainingSamplesPerFSM;++trainingSample)
							for(LearnerToUseEnum learnerKind:LearnerToUseEnum.values())
							{
								LearnerEvaluationConfiguration ev = new LearnerEvaluationConfiguration(eval);
								ev.config = eval.config.copy();ev.config.setOverride_maximalNumberOfStates(states*LearningAlgorithms.maxStateNumberMultiplier);
								ev.config.setOverride_usePTAMerging(false);
	
								MarkovLearningParameters parameters = new MarkovLearningParameters(learnerKind,states, 0,trainingSample, seedForFSM,traceQuantityToUse);
								parameters.setOnlyUsePositives(onlyPositives);
								parameters.setAlphabetMultiplier(alphabetMultiplierMax);
								parameters.setTracesAlphabetMultiplier(alphabetMultiplierMax);
								parameters.setTraceLengthMultiplier(traceLengthMultiplierMax);
								parameters.setExperimentID(traceQuantity,traceLengthMultiplierMax,states,alphabetMultiplierMax);
								parameters.setMarkovParameters(preset, chunkSize);
								parameters.setDisableInconsistenciesInMergers(false);
								parameters.setUsePrintf(experimentRunner.isInteractive());
								MarkovLearnerUsingReference learnerRunner = new MarkovLearnerUsingReference(parameters, ev, cvsReference);
								learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
								
								experimentRunner.submitTask(learnerRunner);
							}
					experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>>() {
	
						@Override
						public void processSubResult(ExperimentResult<MarkovLearningParameters> result, RunSubExperiment<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>> experimentrunner) throws IOException 
						{// in these experiments, samples are singleton sequences because we run each of them in a separate process, in order to increase the efficiency with which all tasks are split between CPUs in an iceberg grid.
							SampleData sm = result.samples.get(0);
							ScoresForGraph data=sm.actualLearner;
							
							StringBuffer csvLine = new StringBuffer();
							csvLine.append(data.differenceBCR.getValue());
							CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceStructural.getValue());
							CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.nrOfstates.getValue());
	
							if (result.parameters.learnerToUse == LearnerToUseEnum.LEARNER_EDSMMARKOV)
							{
								CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.inconsistencyReference);
								CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.inconsistency);
								CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.fractionOfStatesIdentifiedBySingletons);
								CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovPrecision);
								CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.markovRecall);
								CSVExperimentResult.addSeparator(csvLine);csvLine.append(sm.comparisonsPerformed);
							}
							CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(data.executionTime/1000000000.));// execution time is in nanoseconds, we only need seconds.
							experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
						}
						
						@Override
						public SGEExperimentResult[] getGraphs() {
							
							return new SGEExperimentResult[]{resultCSV};
						}
						
					});
					
					if (phase == PhaseEnum.COLLECT_AVAILABLE || phase == PhaseEnum.COLLECT_RESULTS)
					{// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
						final SquareBagPlot gr_StructuralDiff = new SquareBagPlot("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File(outPathPrefix+preset+"_"+states+"_trace_structuraldiff.pdf"),0,1,true);
						final SquareBagPlot gr_BCR = new SquareBagPlot("BCR, Sicco","BCR, EDSM-Markov learner",new File(outPathPrefix+preset+"_"+states+"_trace_bcr.pdf"),0.5,1,true);		
						final SquareBagPlot BCRAgainstKtails = new SquareBagPlot("BCR, K-tails,1","BCR, EDSM-Markov learner",new File(outPathPrefix+preset+"_"+states+"_trace_kt_bcr.pdf"),0.5,1,true);		
						final SquareBagPlot BCRAgainstMarkov = new SquareBagPlot("BCR, Markov","BCR, EDSM-Markov learner",new File(outPathPrefix+preset+"_"+states+"_trace_markov_bcr.pdf"),0.5,1,true);		
	
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
						
						DrawGraphs.spreadsheetAsString(new AggregateStringValues() {
							@Override
							public void merge(String A, @SuppressWarnings("unused") String B) {
								comparisonsPerformed.addAndGet(Long.parseLong(A));
							}},resultCSV,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),3,LearnerToUseEnum.LEARNER_EDSMMARKOV.name(),3);
							
						for(@SuppressWarnings("rawtypes") DrawGraphs.RExperimentResult result:new DrawGraphs.RExperimentResult[]{gr_StructuralDiff,gr_BCR,BCRAgainstKtails,BCRAgainstMarkov, Wilcoxon_Test_BCR,Wilcoxon_test_Structural,Mann_Whitney_U_Test_BCR,Mann_Whitney_U_Test_Structural,Kruskal_Wallis_Test_Structural,Kruskal_Wallis_Test_BCR})
						{
							result.reportResults(gr);
						}
					}
					if (experimentRunner.isInteractive())
						System.out.println("\nLOG of comparisons performed: "+Math.log10(comparisonsPerformed.doubleValue())+"\n");
				}
				
				if (phase == PhaseEnum.COLLECT_AVAILABLE || phase == PhaseEnum.COLLECT_RESULTS)
					for(SquareBagPlot result:new SquareBagPlot[]{})
					{
						result.reportResults(gr);
					}
	
		}
		finally
		{
			experimentRunner.successfulTermination();
			DrawGraphs.end();// this is necessary to ensure termination of the JVM runtime at the end of experiments.
		}
	}
}