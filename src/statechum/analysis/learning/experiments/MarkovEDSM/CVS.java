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

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.GlobalConfiguration.G_PROPERTIES;
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
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.apps.QSMTool;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.SampleData;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;

public class CVS
{
	public static final String directoryNamePrefix = "cvs_from_existing_trace_june_2016";
	public static final String directoryExperimentResult = "experimentresult"+File.separator;

	public static class MarkovLearnerFromPTAUsingReference extends MarkovLearnerRunner
	{
		private final LearnerGraph ptaToStartWith;

		public MarkovLearnerFromPTAUsingReference(MarkovLearningParameters parameters, LearnerEvaluationConfiguration cnf, LearnerGraph initialPTA, LearnerGraph ref) 
		{
			super(parameters, cnf);ptaToStartWith = initialPTA;referenceGraph = ref;
		}
		
		/** Constructs a reference graph and assigns it to member variable <pre>referenceGraph</pre>. This is a separate method to permit overriding by subclasses.
		 */
		@Override
		public void generateReferenceFSM()
		{// does nothing- reference already assigned in the constructor
		}
		
		/** Constructs a PTA to learn an FSM from. This could be based on a reference graph or obtained externally. */
		@Override
		public LearnerGraph constructPTA()
		{
			return ptaToStartWith;
		}
	}

	/** Returns a reference graph for the CVS experiment. */
	public static final LearnerGraph getCVSReference(LearnerEvaluationConfiguration cnf)
	{
		return FsmParser.buildLearnerGraph("q1-connect->q2-login->q3-setfiletype->q4-rename->q6-storefile->q5-setfiletype->q4-storefile->q7-appendfile->q5\nq3-makedir->q8-makedir->q8-logout->q16-disconnect->q1\nq3-changedirectory->q9-listnames->q10-delete->q10-changedirectory->q9\nq10-appendfile->q11-logout->q16\nq3-storefile->q11\nq3-listfiles->q13-retrievefile->q13-logout->q16\nq13-changedirectory->q14-listfiles->q13\nq7-logout->q16\nq6-logout->q16", "specgraph",cnf.config,cnf.getLabelConverter());
	}
	
	public static void main(String args[]) throws Exception
	{
		String outDir = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_EXPERIMENTRESULTS)+File.separator+directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		UASExperiment.mkDir(outDir);
		String outPathPrefix = outDir + File.separator;
		LearnerEvaluationConfiguration eval = UASExperiment.constructLearnerInitConfiguration();
		eval.config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);eval.config.setLearnerScoreMode(ScoreMode.GENERAL_NOFULLMERGE);
		eval.config.setTimeOut(3600000L*4L);// timeout for tasks, in milliseconds, equivalent to 4hrs runtime for an old Xeon 5670 @ 2.93Ghz, modern E5/i7 are 3x faster.
		
		DrawGraphs gr = new DrawGraphs();
		
		final int samplesPerFSM = 30;
		final int trainingSamplesPerFSM = 5;
		final int traceQuantity = 1;
		final double traceLengthMultiplierMax = 10;
		final int chunkSize = 3;
		final int statesToUse[] = new int[]{5,10,20,40};
		SGE_ExperimentRunner.configureCPUFreqNormalisation();
		
		RunSubExperiment<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>> experimentRunner = new RunSubExperiment<MarkovLearningParameters,ExperimentResult<MarkovLearningParameters>>(ExperimentRunner.getCpuNumber(),outPathPrefix + directoryExperimentResult,args);
		SGE_ExperimentRunner.configureCPUFreqNormalisation();
		statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum phase = experimentRunner.getPhase();

		// Inference from a few traces
		final boolean onlyPositives=true;
		final double alphabetMultiplierMax=2;
		try
		{
			for(final int preset: new int[]{0})//0,1,2})
			{
				TraceLoader tool = new TraceLoader(eval.config,eval.getLabelConverter());
				tool.loadConfig("resources/CVS.txt");
	
				LearnerGraph pta = tool.getPTA();
				final int traceQuantityToUse = traceQuantity;
				int seedForFSM = 0;
				final AtomicLong comparisonsPerformed = new AtomicLong(0);
				final int statesMax = statesToUse[statesToUse.length-1];// reflects the size of the largest FSM that will be generated. 
				MarkovLearningParameters parExp = new MarkovLearningParameters(null,0,0,0,0,0);
				parExp.setExperimentID(traceQuantity,traceLengthMultiplierMax,statesMax,alphabetMultiplierMax);
	
				for(int states:statesToUse)
				{
					final String experimentName = outPathPrefix+parExp.getExperimentID()+"_states="+ states+"_";
					final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(experimentName+"results.csv"));
					
					for(int sample=0;sample<samplesPerFSM;++sample,++seedForFSM)
						for(int trainingSample=0;trainingSample<trainingSamplesPerFSM;++trainingSample)
							for(boolean aveOrMax:new boolean[]{true,false})
								for(int divisorForPathCount:new int[]{1,2,4})
									for(LearnerToUseEnum learnerKind:LearnerToUseEnum.values())
										for(double weightOfInconsistencies:learnerKind == LearnerToUseEnum.LEARNER_EDSMMARKOV?new double[]{0.5,1.0,2.0}:new double[]{1.0})
										{
											LearnerEvaluationConfiguration ev = new LearnerEvaluationConfiguration(eval);
											ev.config = eval.config.copy();ev.config.setOverride_maximalNumberOfStates(states*LearningAlgorithms.maxStateNumberMultiplier);
											ev.config.setOverride_usePTAMerging(false);
				
											MarkovLearningParameters parameters = new MarkovLearningParameters(learnerKind,states, sample,trainingSample, seedForFSM,traceQuantityToUse);
											parameters.setOnlyUsePositives(onlyPositives);
											parameters.setAlphabetMultiplier(alphabetMultiplierMax);
											parameters.setTracesAlphabetMultiplier(alphabetMultiplierMax);
											parameters.setTraceLengthMultiplier(traceLengthMultiplierMax);
											parameters.setExperimentID(traceQuantity,traceLengthMultiplierMax,statesMax,alphabetMultiplierMax);
											parameters.markovParameters.setMarkovParameters(preset, chunkSize,true,weightOfInconsistencies,aveOrMax,divisorForPathCount,0,1);
											parameters.setDisableInconsistenciesInMergers(false);
											parameters.setUsePrintf(experimentRunner.isInteractive());
											MarkovLearnerFromPTAUsingReference learnerRunner = new MarkovLearnerFromPTAUsingReference(parameters, ev,pta, getCVSReference(eval));
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
		}
		finally
		{
			experimentRunner.successfulTermination();
			DrawGraphs.end();// this is necessary to ensure termination of the JVM runtime at the end of experiments.
		}
	}
	
	public static class TraceLoader extends QSMTool
	{
		public TraceLoader(Configuration c,ConvertALabel converter)
		{
			learnerInitConfiguration.config = c;learnerInitConfiguration.setLabelConverter(converter);
		}
		
		public LearnerGraph getPTA()
		{
			LearnerGraph outcome = new LearnerGraph(learnerInitConfiguration.config);outcome.paths.augmentPTA(sPlus, true, false);outcome.paths.augmentPTA(sMinus, false, false);return outcome;
		}
	}
	
}