/* Copyright (c) 2016 The University of Sheffield.
 * 
 * This file is part of StateChum
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

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import statechum.Label;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovClassifierLG;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.EvaluationOfLearners.ConstructRandomFSM;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment;
import statechum.analysis.learning.experiments.MarkovEDSM.PerformFirstMerge;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.LearnerWithMandatoryMergeConstraints;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceDiff;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceFMeasure;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceLanguageBCR;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.SampleData;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

public abstract class PairQualityLearnerRunner extends UASExperiment<PairQualityParameters,ExperimentResult<PairQualityParameters>>
{
	protected final WekaDataCollector sampleCollector;
	public PairQualityLearnerRunner(WekaDataCollector collector,PairQualityParameters parameters, LearnerEvaluationConfiguration evalCnf)
	{
		super(parameters,evalCnf,PairQualityLearner.directoryNamePrefix);sampleCollector = collector;
	}
	
	public abstract LearnerWithMandatoryMergeConstraints createLearner(LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, WekaDataCollector argDataCollector, final LearnerGraph argInitialPTA);
	
	@Override
	public ExperimentResult<PairQualityParameters> call() throws Exception 
	{
		final int tracesAlphabet = par.tracesAlphabetMultiplier*par.states;
		ExperimentResult<PairQualityParameters> outcome = new ExperimentResult<PairQualityParameters>(par);
		
		final Random rnd = new Random(par.seed*31+par.attempt*par.states);
		ConstructRandomFSM fsmConstruction = new ConstructRandomFSM();
		fsmConstruction.generateFSM(rnd, tracesAlphabet, par.states, par.seed, par.pickUniqueFromInitial, learnerInitConfiguration);
		referenceGraph = fsmConstruction.referenceGraph;
		
		final Collection<List<Label>> testSet = LearningAlgorithms.buildEvaluationSet(referenceGraph);
		LearnerGraph pta = new LearnerGraph(learnerInitConfiguration.config);
		final int tracesToGenerate = LearningSupportRoutines.makeEven(par.traceQuantity);
		
		if (par.pickUniqueFromInitial)
		{
			final RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(par.seed*31+par.attempt*par.states),5,referenceGraph.getVertex(Arrays.asList(new Label[]{fsmConstruction.uniqueFromInitial})));
			generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
									
					@Override
					public int getLength() {
						return  par.traceLengthMultiplier*par.states*tracesAlphabet;// same as for Markov learner
					}

					@Override
					public int getPrefixLength(int len) {
						return len;
					}
				},true,true,null,Arrays.asList(new Label[]{fsmConstruction.uniqueFromInitial}));

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
		}
		else
		{// not using unique from initial
			final RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(par.seed*31+par.attempt*par.states),5,null);
			generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
					
					@Override
					public int getLength() {
						return par.traceLengthMultiplier*par.states*tracesAlphabet;// not the same as for SmallVsHuge or LearnerEvaluation
					}
	
					@Override
					public int getPrefixLength(int len) {
						return len;
					}
				});


			if (par.onlyUsePositives)
			{
				pta.paths.augmentPTA(generator.getAllSequences(0).filter(new FilterPredicate() {
					@Override
					public boolean shouldBeReturned(Object name) {
						return ((statechum.analysis.learning.rpnicore.RandomPathGenerator.StateName)name).accept;
					}
				}));
			}
			else
				pta.paths.augmentPTA(generator.getAllSequences(0));
		}
		pta.clearColours();
		if (!par.onlyUsePositives)
			assert pta.getStateNumber() > pta.getAcceptStateNumber() : "graph with only accept states but onlyUsePositives is not set";
		else 
			assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
		
		LearnerWithMandatoryMergeConstraints learnerOfPairs = null;
		LearnerGraph actualAutomaton = null;
		LearnerGraph trimmedReference = LearningSupportRoutines.trimUncoveredTransitions(pta,referenceGraph);

		if (sampleCollector.markovHelper != null)
			sampleCollector.markovHelper.updateMarkovModel(pta);// construct Markov chain if asked for.
		if ( sampleCollector.markovMultiHelper != null)
			sampleCollector.markovMultiHelper.updateMarkovModel(pta);
		PerformFirstMerge fmg = new PerformFirstMerge();fmg.ptaToUseForInference=pta;
		if (par.dataCollectorParameters.markovParameters.useCentreVertex && sampleCollector.markovHelper != null)
		{
			saveGraph(namePTABEFORECENTRE,pta);
			fmg.buildFirstGraph(pta, null, par.dataCollectorParameters.markovParameters, sampleCollector.markovHelper.getModel(), sampleCollector.markovHelper.getChecker());
		}
					
		// not merging based on a unique transition from an initial state
		learnerOfPairs = createLearner(learnerInitConfiguration,referenceGraph,sampleCollector,fmg.ptaToUseForInference);
		long startTime = LearningSupportRoutines.getThreadTime();
		/*
		System.out.println("learning started");
		tmpAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		System.out.println("second go on the graph");
		tmpAutomaton.clearColours();
		LearnerWithMandatoryMergeConstraints learnerSecondAttempt = createLearner(learnerInitConfiguration,referenceGraph,dataCollector,tmpAutomaton);
		actualAutomaton = learnerSecondAttempt.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		System.out.println("learning finished");
		*/
		actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		
		long runTime = LearningSupportRoutines.getThreadTime()-startTime;
		
		SampleData dataSample = new SampleData(null,null);
		dataSample.actualLearner = estimateDifference(referenceGraph, actualAutomaton, testSet);
		dataSample.actualLearner.executionTime = runTime;
		dataSample.referenceLearner = MarkovExperiment.zeroScore;
		dataSample.centreCorrect = fmg.correctCentre;
		dataSample.centrePathNumber = fmg.centrePathNumber;
		dataSample.fractionOfStatesIdentifiedBySingletons=Math.round(100*MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(referenceGraph));
		dataSample.stateNumber = referenceGraph.getStateNumber();
		dataSample.transitionsSampled = Math.round(100*trimmedReference.pathroutines.countEdges()/referenceGraph.pathroutines.countEdges());
		statechum.Pair<Double,Double> correctnessOfMarkov = new MarkovClassifierLG(sampleCollector.markovHelper.getModel(), referenceGraph,null).evaluateCorrectnessOfMarkov();
		dataSample.markovPrecision = Math.round(100*correctnessOfMarkov.firstElem);dataSample.markovRecall = Math.round(100*correctnessOfMarkov.secondElem);
		/*
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = 
				new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();

			LearnerGraphND grA=new LearnerGraphND(referenceGraph,referenceGraph.config),
					grB=new LearnerGraphND(actualAutomaton,actualAutomaton.config);
			DirectedSparseGraph gr = gd.showGD(
					grA,grB,
					ExperimentRunner.getCpuNumber());
			Visualiser.updateFrame(gr, null);
			*/
		outcome.samples.add(dataSample);
		return outcome;
	}

	// Delegates to a specific estimator
	ScoresForGraph estimateDifference(LearnerGraph reference, LearnerGraph actual,Collection<List<Label>> testSet)
	{
		ScoresForGraph outcome = new ScoresForGraph();
		outcome.differenceStructural=DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(reference, actual, learnerInitConfiguration.config, 1);
		outcome.differenceBCR=DifferenceToReferenceLanguageBCR.estimationOfDifference(reference, actual,testSet);
		outcome.differenceFMeasure=DifferenceToReferenceFMeasure.estimationOfDifference(reference, actual,testSet);
		outcome.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actual.getStateNumber() - referenceGraph.getStateNumber());
		return outcome;
	}
}