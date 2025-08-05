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

import statechum.*;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.collections.MapWithSearch;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.Map.Entry;

public class EvaluationOfLearnersForProgressProperty extends UASExperiment<EvaluationOfLearnersParameters,EvaluationOfLearnersResult>
{
	public static final String directoryNamePrefix = "evaluation_of_progress_property";
	public static final String directoryExperimentResult = "experimentresult"+File.separator;

	public EvaluationOfLearnersForProgressProperty(EvaluationOfLearnersParameters parameters, LearnerEvaluationConfiguration eval)
	{
		super(parameters,eval,directoryNamePrefix);
	}

	@Override
	public EvaluationOfLearnersResult runexperiment() throws Exception 
	{
		final int alphabet = (int)Math.round(par.states*par.alphabetMultiplier);
		EvaluationOfLearnersResult outcome = new EvaluationOfLearnersResult(par);
		ConstructRandomFSM fsmConstruction = new ConstructRandomFSM();

		double transitionsPerState = 3;
//		learnerInitConfiguration.selfLoopChance = 0.6;

		fsmConstruction.generateFSM(new Random(par.seed* 31L +par.states), alphabet, par.states, transitionsPerState,par.seed, par.pickUniqueFromInitial, learnerInitConfiguration);
		referenceGraph = fsmConstruction.referenceGraph;

		int loopCount = 0;
		for(Entry<CmpVertex, MapWithSearch<Label, Label, CmpVertex>> entry:referenceGraph.transitionMatrix.entrySet())
			for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
			if (entry.getKey().equals(transition.getValue()))
				loopCount++;
		System.out.println(loopCount/(double)referenceGraph.transitionMatrix.size());
		final LearnerGraph pta = new LearnerGraph(learnerInitConfiguration.config);
		//final RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(attempt*23+seed),5,referenceGraph.getInit());//referenceGraph.getVertex(Arrays.asList(new Label[]{uniqueFromInitial})));
		//generator.setWalksShouldLeadToInitialState();
		final int tracesToGenerate = LearningSupportRoutines.makeEven(par.states*par.traceQuantity);
		final RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(par.attempt* 23L +par.seed),5,referenceGraph.getInit());
//		generator.setWalkType(RandomPathGenerator.WALKTYPE.WALKTYPE_LIMITEDSELFLOOPS);
		generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
								
				@Override
				public int getLength() {
					return  par.lengthmult*par.states;
				}

				@Override
				public int getPrefixLength(int len) {
					return len;
				}
			},true,false,null,null);

		//generator.generateRandomPosNeg(tracesToGenerate, 1, false);
		if (par.onlyUsePositives)
			pta.paths.augmentPTA(generator.getAllSequences(0).filter(name -> ((RandomPathGenerator.StateName)name).accept));
		else
			pta.paths.augmentPTA(generator.getAllSequences(0));// the PTA will have very few reject-states because we are generating few sequences and hence there will be few negative sequences.

		pta.clearColours();

		if (par.onlyUsePositives)
			assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";

		for(Entry<CmpVertex,List<Label>> path: pta.pathroutines.computeShortPathsToAllStates().entrySet())
		{
			boolean accept = path.getKey().isAccept();
			CmpVertex vert = referenceGraph.getVertex(path.getValue());
			boolean shouldBe = vert != null && vert.isAccept();
			assert accept == shouldBe: "state "+vert+" is incorrectly annotated as "+accept+" in path "+path;
		}

		learnerInitConfiguration.testSet = LearningAlgorithms.buildEvaluationSet(referenceGraph);
		BuildPTAInterface ptaConstructor = new BuildPTAInterface() {
			@Override
			public String kindOfPTA()
			{
				return par.getRowID();
			}
			@Override
			public LearnerGraph buildPTA() throws IOException
			{
				saveGraph(namePTA,pta);
				return pta;
			}
		};
		LearningAlgorithms.StateMergingStatistics redReducer = new LearningAlgorithms.ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown(referenceGraph,
				par.scoringMethod == LearningAlgorithms.ScoringToApply.SCORING_SICCO);// use reducedReds for SICCO scoring that will deliberately significantly under-merge.
		PairQualityLearner.SampleData sample = new PairQualityLearner.SampleData();
		// if par.secondScoringMethod is null, runExperimentUsingConventionalInTwoSteps will be running a single-step experiment.
		sample.actualLearner = runExperimentUsingConventionalInTwoSteps(ptaConstructor,redReducer,par,par.scoringMethod,par.secondScoringMethod,par.scoringForEDSM);

		//sample.referenceLearner = runExperimentUsingConventionalWithUniqueLabel(ptaConstructor,scoringMethod, uniqueFromInitial);
		//sample.premergeLearner = runExperimentUsingPTAPremerge(ptaConstructor,scoringMethod,uniqueFromInitial);
				
		//sample.premergeLearner = runExperimentUsingPremerge(ptaConstructor,scoringMethod,uniqueFromInitial);
		//sample.actualConstrainedLearner = runExperimentUsingConstraints(ptaConstructor,scoringMethod,uniqueFromInitial);
		
		outcome.samples.add(sample);
		return outcome;
	}
	
	public static void main(String []args)
	{
		String outDir = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_EXPERIMENTRESULTS)+File.separator+directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		mkDir(outDir);
		String outPathPrefix = outDir + File.separator;
		mkDir(outPathPrefix+directoryExperimentResult);
		final RunSubExperiment<EvaluationOfLearnersParameters,EvaluationOfLearnersResult> experimentRunner = new RunSubExperiment<>(ExperimentRunner.getCpuNumber(), outPathPrefix + directoryExperimentResult, args);
		SGE_ExperimentRunner.configureCPUFreqNormalisation();
		LearnerEvaluationConfiguration eval = UASExperiment.constructLearnerInitConfiguration();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");

		final int samplesPerFSMSize = 10;
		final int attemptsPerFSM = 2;

		final RBoxPlot<String> BCR_vs_experiment = new RBoxPlot<>("experiment", "BCR", new File(outPathPrefix + "BCR_vs_experiment.pdf"));
		final RBoxPlot<String> diff_vs_experiment = new RBoxPlot<>("experiment", "Structural difference", new File(outPathPrefix + "diff_vs_experiment.pdf"));

		final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(outPathPrefix+"results.csv"));

    	processSubExperimentResult<EvaluationOfLearnersParameters,EvaluationOfLearnersResult> resultHandler = new processSubExperimentResult<EvaluationOfLearnersParameters,EvaluationOfLearnersResult>() {

			@Override
			public void processSubResult(EvaluationOfLearnersResult result, RunSubExperiment<EvaluationOfLearnersParameters,EvaluationOfLearnersResult> experimentrunner) throws IOException 
			{
				ScoresForGraph data = result.samples.get(0).actualLearner;
				StringBuffer csvLine = new StringBuffer();

				csvLine.append(data.whetherLearningSuccessfulOrAborted);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceBCR.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.differenceStructural.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.invalidMergers);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.missedMergers);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(data.nrOfstates.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(data.executionTime/1000000000.));// execution time is in nanoseconds, we only need seconds.

				experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
				String experimentName = result.parameters.states+"-"+result.parameters.alphabetMultiplier+"-"+result.parameters.traceQuantity+"-"+result.parameters.lengthmult+"_"+result.parameters.uniqueAsString()+"-"+result.parameters.getColumnID();
				experimentrunner.RecordR(BCR_vs_experiment,experimentName ,data.differenceBCR.getValue(),null,null);
				experimentrunner.RecordR(diff_vs_experiment,experimentName,data.differenceStructural.getValue(),null,null);
			}
			
			@Override
			public SGEExperimentResult[] getGraphs() {
				return new SGEExperimentResult[]{BCR_vs_experiment,diff_vs_experiment,resultCSV};
			}
		};
		
		List<EvaluationOfLearnersForProgressProperty> listOfExperiments = new ArrayList<>();
		
		try
		{
			for(int states:new int[]{10})
				for(double alphabetMult:new double[] {0.5,1,2})
				{
					int seedThatIdentifiesFSM=0;
					for(int sample=0;sample<samplesPerFSMSize;++sample,++seedThatIdentifiesFSM)
						for(int attempt=0;attempt<attemptsPerFSM;++attempt)
						{
								for(boolean pta:new boolean[]{false}) // the choice of using PTA or not does not make a significant impact.
								{
									for(int traceQuantity:new int[]{8})
										for(int traceLengthMultiplier:new int[]{16})// 16
												for(ScoringToApply firstScore:new ScoringToApply[]{
//														ScoringToApply.SCORING_EDSM,
//														ScoringToApply.SCORING_EDSM_3,
														ScoringToApply.SCORING_SICCO,
														ScoringToApply.SCORING_SICCO_3,
//														ScoringToApply.SCORING_LIMITEDSELFLOOPS_SICCO_0,
//														ScoringToApply.SCORING_LIMITEDSELFLOOPS_SICCO_3,
//														ScoringToApply.SCORING_LIMITEDSELFLOOPS_0,
//														ScoringToApply.SCORING_LIMITEDSELFLOOPS_3,
														ScoringToApply.SCORING_LIMITEDLOOPS_0,
														ScoringToApply.SCORING_LIMITEDLOOPS_3,
														ScoringToApply.SCORING_LIMITEDLOOPS_SICCO_0,
														ScoringToApply.SCORING_LIMITEDLOOPS_SICCO_3,

												})
												{
													LearnerEvaluationConfiguration ev = new LearnerEvaluationConfiguration(eval);
													//ev.config.setOverride_maximalNumberOfStates(states*LearningAlgorithms.maxStateNumberMultiplier);
													ev.config.setOverride_usePTAMerging(pta);ev.config.setTransitionMatrixImplType(Configuration.STATETREE.STATETREE_ARRAY);
													ev.config.setAlwaysUseTheSameMatrixType(false);
													EvaluationOfLearnersParameters par = new EvaluationOfLearnersParameters(Configuration.ScoreMode.GENERAL_NOFULLMERGE,firstScore,null,pta,Configuration.STATETREE.STATETREE_ARRAY);
													par.secondScoringMethod = null;//ScoringToApply.SCORING_LIMITEDSELFLOOPS_0;
													par.setParameters(states, alphabetMult, 0, sample, attempt, seedThatIdentifiesFSM, traceQuantity, traceLengthMultiplier);
													par.onlyUsePositives=true;
													par.setPickUniqueFromInitial(false);
													EvaluationOfLearnersForProgressProperty learnerRunner = new EvaluationOfLearnersForProgressProperty(par, ev);
													learnerRunner.setAlwaysRunExperiment(true);
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
	    	for(EvaluationOfLearnersForProgressProperty e:listOfExperiments)
	    		experimentRunner.submitTask(e);
	    	experimentRunner.collectOutcomeOfExperiments(resultHandler);
		}
		finally
		{
			experimentRunner.successfulTermination();
			DrawGraphs.end();// this is necessary to ensure termination of the JVM runtime at the end of experiments.
		}
	}
}
