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
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.EvaluationOfLearners.EvaluationOfLearnersParameters.LearningType;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

public class SmallVsHuge extends UASExperiment<SmallVsHugeParameters,ExperimentResult<SmallVsHugeParameters>>
{
	public static final String directoryNamePrefix = "small_vs_huge";
	public static final String directoryExperimentResult = "experimentresult"+File.separator;
	
	public SmallVsHuge(SmallVsHugeParameters parameters, LearnerEvaluationConfiguration eval)
	{
		super(parameters,eval,directoryNamePrefix);
	}
	
	public static final Configuration.ScoreMode conventionalScoringToUse[] = new Configuration.ScoreMode[]{Configuration.ScoreMode.GENERAL, Configuration.ScoreMode.GENERAL_PLUS_NOFULLMERGE};
	
	@Override
	public ExperimentResult<SmallVsHugeParameters> call() throws Exception 
	{
		final int alphabet = par.states*2;
		ExperimentResult<SmallVsHugeParameters> outcome = new ExperimentResult<SmallVsHugeParameters>(par);
		
		final Random rnd = new Random(par.seed*31+par.attempt*par.states);
		ConstructRandomFSM fsmConstruction = new ConstructRandomFSM();
		fsmConstruction.generateFSM(rnd, alphabet, par.states, par.seed, true, learnerInitConfiguration);
		referenceGraph = fsmConstruction.referenceGraph;
		assert fsmConstruction.uniqueFromInitial != null : "unique transition has to be available";
		//referenceGraph = mg.nextMachine(alphabet,seed, learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()).pathroutines.buildDeterministicGraph();
		final LearnerGraph pta = new LearnerGraph(learnerInitConfiguration.config);
		//generator.setWalksShouldLeadToInitialState();
		final int tracesToGenerate = LearningSupportRoutines.makeEven(par.states*par.traceQuantity);
		final RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(par.attempt*23+par.seed),5,referenceGraph.getVertex(Arrays.asList(new Label[]{fsmConstruction.uniqueFromInitial})));
		generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
								
				@Override
				public int getLength() {
					return  par.lengthmult*par.states;
				}

				@Override
				public int getPrefixLength(int len) {
					return len;
				}
			},true,true,null,Arrays.asList(new Label[]{fsmConstruction.uniqueFromInitial}));

		//generator.generateRandomPosNeg(tracesToGenerate, 1, false);
		if (par.onlyUsePositives)
			pta.paths.augmentPTA(generator.getAllSequences(0).filter(new FilterPredicate() {
				@Override
				public boolean shouldBeReturned(Object name) {
					return ((statechum.analysis.learning.rpnicore.RandomPathGenerator.StateName)name).accept;
				}
			}));
		else
			pta.paths.augmentPTA(generator.getAllSequences(0));// the PTA will have very few reject-states because we are generating few sequences and hence there will be few negative sequences.
			// In order to approximate the behaviour of our case study, we need to compute which pairs are not allowed from a reference graph and use those as if-then automata to start the inference.
			// This is done below if onlyUsePositives is not set. 

		pta.clearColours();
		
		if (!par.onlyUsePositives)
		{// now we have an even positive/negative split, add negatives by encoding them as if-then automata.
			assert pta.getStateNumber() > pta.getAcceptStateNumber() : "graph with only accept states but onlyUsePositives is not set";
			Map<Label,Set<Label>> infeasiblePairs = LearningSupportRoutines.computeInfeasiblePairs(referenceGraph);
			Map<Label,Set<Label>> subsetOfPairs = new TreeMap<Label,Set<Label>>();
			for(Entry<Label,Set<Label>> entry:infeasiblePairs.entrySet())
			{
				Set<Label> value = new TreeSet<Label>();
				if (!entry.getValue().isEmpty()) 
				{// we add a single entry per label, to mimic what was done with UAS study, where labels could not be repeated.
					Label possibleLabels[]=entry.getValue().toArray(new Label[]{});
					if (possibleLabels.length == 1)
						value.add(possibleLabels[0]);
					else
					{
						for(int cnt=0;cnt < 2;++cnt)
							value.add(possibleLabels[rnd.nextInt(possibleLabels.length)]);
					}
				}
				subsetOfPairs.put(entry.getKey(),value);
			}
			
			LearningSupportRoutines.addIfThenForPairwiseConstraints(learnerInitConfiguration,subsetOfPairs);
			LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(learnerInitConfiguration.ifthenSequences, referenceGraph.pathroutines.computeAlphabet(), learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()).toArray(new LearnerGraph[0]);
			learnerInitConfiguration.config.setUseConstraints(false);// do not use if-then during learning (refer to the explanation above)
			int statesToAdd = 1;// we are adding pairwise constraints hence only one has to be added.
			Transform.augmentFromIfThenAutomaton(pta, null, ifthenAutomata, statesToAdd);// we only need  to augment our PTA once (refer to the explanation above).
		}
		else 
			assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
		
		
		for(Entry<CmpVertex,List<Label>> path: pta.pathroutines.computeShortPathsToAllStates().entrySet())
		{
			boolean accept = path.getKey().isAccept();
			CmpVertex vert = referenceGraph.getVertex(path.getValue());
			boolean shouldBe = vert==null?false:vert.isAccept();
			assert accept == shouldBe: "state "+vert+" is incorrectly annotated as "+accept+" in path "+path;
		}

		learnerInitConfiguration.testSet = LearningAlgorithms.buildEvaluationSet(referenceGraph);
		UASExperiment.BuildPTAInterface ptaConstructor = new BuildPTAInterface() {
			@Override
			public String kindOfPTA()
			{
				return par.getRowID();
			}
			@Override
			public LearnerGraph buildPTA() throws AugmentFromIfThenAutomatonException, IOException 
			{
				saveGraph(namePTA,pta);
				return pta;
			}
		};

		PairQualityLearner.SampleData sample = new PairQualityLearner.SampleData();
		switch(par.learningType)
		{
		case CONVENTIONAL:
			sample.actualLearner = runExperimentUsingConventional(ptaConstructor,par,par.scoringMethod,par.scoringForEDSM);
			break;
		case CONVENTIONALUNIQUE:
			sample.actualLearner = runExperimentUsingConventionalWithUniqueLabel(ptaConstructor,par,par.scoringMethod,par.scoringForEDSM, fsmConstruction.uniqueFromInitial);
			break;
		case CONSTRAINTS:
			sample.actualLearner = runExperimentUsingConstraints(ptaConstructor,par,par.scoringMethod,par.scoringForEDSM,fsmConstruction.uniqueFromInitial);
			break;
		case PREMERGE:
			sample.actualLearner = runExperimentUsingPremerge(ptaConstructor,par,par.scoringMethod,par.scoringForEDSM,fsmConstruction.uniqueFromInitial);
			break;
		case PTAPREMERGE:
			sample.actualLearner = runExperimentUsingPTAPremerge(ptaConstructor,par,par.scoringMethod,par.scoringForEDSM,fsmConstruction.uniqueFromInitial);
			break;
		}
		
		outcome.samples.add(sample);
		/*
		{// Perform semi-pre-merge by building a PTA rather than a graph with loops and learn from there without using constraints
			LearnerGraph reducedPTA = LearningSupportRoutines.mergeStatesForUnique(pta,uniqueFromInitial);
			//Visualiser.updateFrame(reducedPTA.transform.trimGraph(4, reducedPTA.getInit()), pta);
			// in these experiments I cannot use SICCO merging because it will stop any mergers with an initial state.
			learnerOfPairs = new LearningAlgorithms.ReferenceLearner(learnerEval, reducedPTA,scoringToUse);
			
			System.out.println("PTApremerge size: "+reducedPTA.getStateNumber()+" states, "+reducedPTA.getAcceptStateNumber()+" accept-states and "+reducedPTA.pathroutines.countEdges()+" transitions");
			actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());

			DifferenceToReference similarityMeasure = getMeasure(actualAutomaton,referenceGraph,testSet);
			System.out.println(sample+" PTA permerge, similarity = "+similarityMeasure.getValue()+" ( "+similarityMeasure+" )");
		}
		 */
		/*
		{// Perform semi-pre-merge by building a PTA rather than a graph with loops and then use constraints
			LearnerGraph reducedPTA = LearningSupportRoutines.mergeStatesForUnique(pta,uniqueFromInitial);
			//Visualiser.updateFrame(reducedPTA.transform.trimGraph(4, reducedPTA.getInit()), pta);
			// in these experiments I cannot use SICCO merging because it will stop any mergers with an initial state.
			learnerOfPairs = new LearningAlgorithms.ReferenceLearner(learnerEval, reducedPTA,scoringToUse);
			
			System.out.println("PTApremerge size: "+reducedPTA.getStateNumber()+" states, "+reducedPTA.getAcceptStateNumber()+" accept-states and "+reducedPTA.pathroutines.countEdges()+" transitions");
			learnerOfPairs.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueFromInitial}));
			actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());

			DifferenceToReference similarityMeasure = getMeasure(actualAutomaton,referenceGraph,testSet);
			System.out.println(sample+" PTA permerge, similarity = "+similarityMeasure.getValue()+" ( "+similarityMeasure+" )");
		}
		 */
		
		return outcome;
	}
	
	public static final String unknownValue = "UNKNOWN";
	
	public static void main(String []args)
	{
		String outDir = "tmp"+File.separator+directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		mkDir(outDir);
		String outPathPrefix = outDir + File.separator;
		mkDir(outPathPrefix+directoryExperimentResult);
		final RunSubExperiment<SmallVsHugeParameters,ExperimentResult<SmallVsHugeParameters>> experimentRunner = new RunSubExperiment<SmallVsHugeParameters,ExperimentResult<SmallVsHugeParameters>>(ExperimentRunner.getCpuNumber(),outPathPrefix + directoryExperimentResult,args);
		SGE_ExperimentRunner.configureCPUFreqNormalisation();
		
		LearnerEvaluationConfiguration eval = UASExperiment.constructLearnerInitConfiguration();
		eval.config.setTimeOut(3600000L*8L);// timeout for tasks, in milliseconds, equivalent to 8hrs runtime for an old Xeon 5670 @ 2.93Ghz, modern E5/i7 are 3x faster.
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		
		final int samplesPerFSMSize = 100;
		final int attemptsPerFSM = 2;

		final RBoxPlot<String> BCR_vs_experiment = new RBoxPlot<String>("experiment","BCR",new File(outPathPrefix+"BCR_vs_experiment.pdf"));
		final RBoxPlot<String> diff_vs_experiment = new RBoxPlot<String>("experiment","Structural difference",new File(outPathPrefix+"diff_vs_experiment.pdf"));

		final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(outPathPrefix+"results.csv"));
		resultCSV.setMissingValue(unknownValue);
		
    	processSubExperimentResult<SmallVsHugeParameters,ExperimentResult<SmallVsHugeParameters>> resultHandler = new processSubExperimentResult<SmallVsHugeParameters,ExperimentResult<SmallVsHugeParameters>>() {

			@Override
			public void processSubResult(ExperimentResult<SmallVsHugeParameters> result, RunSubExperiment<SmallVsHugeParameters,ExperimentResult<SmallVsHugeParameters>> experimentrunner) throws IOException 
			{
				ScoresForGraph difference = result.samples.get(0).actualLearner;
				StringBuffer csvLine = new StringBuffer();
				csvLine.append(difference.differenceBCR.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(difference.differenceStructural.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(difference.nrOfstates.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(difference.executionTime/1000000000.));// execution time is in nanoseconds, we only need seconds.
				experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
				String experimentName = result.parameters.states+"-"+result.parameters.traceQuantity+"-"+result.parameters.lengthmult+"_"+EvaluationOfLearnersParameters.ptaMergersToString(result.parameters.ptaMergers)+"-"+result.parameters.matrixType.name;
				experimentrunner.RecordR(BCR_vs_experiment,experimentName ,difference.differenceBCR.getValue(),null,null);
				experimentrunner.RecordR(diff_vs_experiment,experimentName,difference.differenceStructural.getValue(),null,null);
			}

			@Override
			public SGEExperimentResult[] getGraphs() {
				return new SGEExperimentResult[]{BCR_vs_experiment,diff_vs_experiment,resultCSV};
			}
		};
		List<SmallVsHuge> listOfExperiments = new ArrayList<SmallVsHuge>();
		try
		{
			for(int states:new int[]{5,10,20,40})
			{
				int seedThatIdentifiesFSM=0;
				for(int sample=0;sample<samplesPerFSMSize;++sample,++seedThatIdentifiesFSM)
					for(int attempt=0;attempt<attemptsPerFSM;++attempt)
					{
						for(int traceQuantity:new int[]{1,8})
							for(int traceLengthMultiplier:new int[]{1,8})
								if (traceQuantity*traceLengthMultiplier <= 64)
									for(Configuration.STATETREE matrix:new Configuration.STATETREE[]{Configuration.STATETREE.STATETREE_ARRAY})
										for(boolean pta:new boolean[]{false})
										{
											for(ScoringModeScore scoringPair:new ScoringModeScore[]{
													//new ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_EDSM_4),
													//new ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_EDSM_6),
													//new ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_EDSM_8),
													//new ScoringModeScore(Configuration.ScoreMode.GENERAL_PLUS_NOFULLMERGE,ScoringToApply.SCORING_EDSM_4),
													new ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_SICCO),
											})
											{
													for(LearningType type:new LearningType[]{LearningType.CONVENTIONAL,LearningType.CONVENTIONALUNIQUE, LearningType.PREMERGE, LearningType.CONSTRAINTS})
													{
														LearnerEvaluationConfiguration ev = new LearnerEvaluationConfiguration(eval);
														ev.config.setOverride_maximalNumberOfStates(states*LearningAlgorithms.maxStateNumberMultiplier);
														ev.config.setOverride_usePTAMerging(pta);ev.config.setTransitionMatrixImplType(matrix);
														
														SmallVsHugeParameters par = new SmallVsHugeParameters(scoringPair.scoringForEDSM,scoringPair.scoringMethod,type,pta,matrix);
														par.setParameters(states, sample, attempt, seedThatIdentifiesFSM, traceQuantity, traceLengthMultiplier);
// here we pretend to generate normal state machines (without unique transitions), however in reality we request unique transitions. 
// There is therefore a discrepancy between names used for experiment results ('ANY') v.s. what they really mean ('UNIQ').
														SmallVsHuge learnerRunner = new SmallVsHuge(par, ev);
														learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
														listOfExperiments.add(learnerRunner);
													}
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
	    	for(SmallVsHuge e:listOfExperiments)
	    		experimentRunner.submitTask(e);
	    	experimentRunner.collectOutcomeOfExperiments(resultHandler);
	    	
	    	if (experimentRunner.getPhase() == PhaseEnum.COLLECT_RESULTS)
	    	{// process the results.
	    		/*
	    		final String 
	    		DrawGraphs.spreadsheetAsDouble(new AggregateValues() {
					
					@Override
					public void merge(double A, double B) {
						// TODO Auto-generated method stub
						
					}
				},resultCSV,)*/
	    	}
		}
		finally
		{
			experimentRunner.successfulTermination();
			DrawGraphs.end();// this is necessary to ensure termination of the JVM runtime at the end of experiments.
		}
	}
}
