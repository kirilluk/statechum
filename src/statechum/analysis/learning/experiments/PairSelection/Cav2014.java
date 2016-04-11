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

package statechum.analysis.learning.experiments.PairSelection;

import java.io.File;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Label;
import statechum.ProgressIndicator;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.ScatterPlot;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;
import statechum.analysis.learning.DrawGraphs.RBagPlot;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;


public class Cav2014 extends PairQualityLearner
{
	
	public static class LearnerRunner implements Callable<ThreadResult>
	{
		protected final Configuration config;
		protected final ConvertALabel converter;
		protected final int states,sample;
		protected boolean onlyUsePositives;
		protected final int seed;
		protected int chunkLen=4;
		protected final int traceQuantity;
		protected String selectionID;
		protected double alphabetMultiplier = 2.;
		protected double traceLengthMultiplier = 2;
		protected double tracesAlphabetMultiplier = 0;
		
		public void setTracesAlphabetMultiplier(double evalAlphabetMult)
		{
			tracesAlphabetMultiplier = evalAlphabetMult;
		}
		
		public void setSelectionID(String value)
		{
			selectionID = value;
		}
		
		public void setTraceLengthMultiplier(double value)
		{
			traceLengthMultiplier = value;
		}
		
		/** Whether to filter the collection of traces such that only positive traces are used. */
		public void setOnlyUsePositives(boolean value)
		{
			onlyUsePositives = value;
		}
		
		public void setAlphabetMultiplier(double mult)
		{
			alphabetMultiplier = mult;
		}
		
		public void setChunkLen(int len)
		{
			chunkLen = len;
		}
		
		public LearnerRunner(int argStates, int argSample, int argSeed, int nrOfTraces, Configuration conf, ConvertALabel conv)
		{
			states = argStates;sample = argSample;config = conf;seed = argSeed;traceQuantity=nrOfTraces;converter=conv;
		}
		
		@Override
		public ThreadResult call() throws Exception 
		{
			if (tracesAlphabetMultiplier <= 0)
				tracesAlphabetMultiplier = alphabetMultiplier;
			final int alphabet = (int)(alphabetMultiplier*states);
			final int tracesAlphabet = (int)(tracesAlphabetMultiplier*states);
			
			LearnerGraph referenceGraph = null;
			ThreadResult outcome = new ThreadResult();
			MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
			referenceGraph = mg.nextMachine(alphabet,seed, config, converter).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
			
			LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);learnerEval.setLabelConverter(converter);
			final Collection<List<Label>> testSet = LearningAlgorithms.computeEvaluationSet(referenceGraph,states*3,LearningSupportRoutines.makeEven(states*tracesAlphabet));

			for(int attempt=0;attempt<2;++attempt)
			{// try learning the same machine a few times
				LearnerGraph pta = new LearnerGraph(config);
				RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(attempt),5,null);
				final int tracesToGenerate = LearningSupportRoutines.makeEven(traceQuantity);
				generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
										
						@Override
						public int getLength() {
							return (int)(traceLengthMultiplier*states*tracesAlphabet);
						}
		
						@Override
						public int getPrefixLength(int len) {
							return len;
						}
					});

				if (onlyUsePositives)
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
		
				List<List<Label>> sPlus = generator.getAllSequences(0).getData(new FilterPredicate() {
					@Override
					public boolean shouldBeReturned(Object name) {
						return ((statechum.analysis.learning.rpnicore.RandomPathGenerator.StateName)name).accept;
					}
				});
				List<List<Label>> sMinus= generator.getAllSequences(0).getData(new FilterPredicate() {
					@Override
					public boolean shouldBeReturned(Object name) {
						return !((statechum.analysis.learning.rpnicore.RandomPathGenerator.StateName)name).accept;
					}
				});
				assert sPlus.size() > 0;
				assert sMinus.size() > 0;
				final MarkovModel m= new MarkovModel(chunkLen,true,true,false);
				new MarkovClassifier(m, pta).updateMarkov(false);
				pta.clearColours();

				if (!onlyUsePositives)
					assert pta.getStateNumber() > pta.getAcceptStateNumber() : "graph with only accept states but onlyUsePositives is not set";
				else 
					assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
				
				LearningAlgorithms.LearnerThatDelegatesToTheSuppliedClassifier learnerOfPairs = null;
				LearnerGraph actualAutomaton = null;
				
				final Configuration deepCopy = pta.config.copy();deepCopy.setLearnerCloneGraph(true);
				LearnerGraph ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);

				LearnerGraph trimmedReference = MarkovPassivePairSelection.trimUncoveredTransitions(pta,referenceGraph);
				
				final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklisting();
				long inconsistencyForTheReferenceGraph = MarkovClassifier.computeInconsistency(referenceGraph, m, checker, false);
				
				learnerOfPairs = new LearningAlgorithms.LearnerThatDelegatesToTheSuppliedClassifier(learnerEval,referenceGraph,pta);

				learnerOfPairs.setScoreComputationOverride(new LearningAlgorithms.RedPriorityOverBluePairSelectionRoutine(m));
				actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
				
				SampleData dataSample = new SampleData(null,null);
				//dataSample.difference = new DifferenceToReferenceDiff(0, 0);
				//dataSample.differenceForReferenceLearner = new DifferenceToReferenceDiff(0, 0);
				
				VertID rejectVertexID = null;
				for(CmpVertex v:actualAutomaton.transitionMatrix.keySet())
					if (!v.isAccept())
					{
						assert rejectVertexID == null : "multiple reject vertices in learnt automaton, such as "+rejectVertexID+" and "+v;
						rejectVertexID = v;break;
					}
				if (rejectVertexID == null)
					rejectVertexID = actualAutomaton.nextID(false);
				actualAutomaton.pathroutines.completeGraphPossiblyUsingExistingVertex(rejectVertexID);// we need to complete the graph, otherwise we are not matching it with the original one that has been completed.
				dataSample.actualLearner = estimateDifference(referenceGraph,actualAutomaton,testSet);
				dataSample.actualLearner.inconsistency = MarkovClassifier.computeInconsistency(actualAutomaton, m, checker, false);
				dataSample.referenceLearner = LearningAlgorithms.zeroScore;
				LearnerGraph outcomeOfReferenceLearner = new LearnerGraph(config);
				try
				{
					outcomeOfReferenceLearner = //new ReferenceLearnerUsingSiccoScoring(learnerEval,ptaCopy,false).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
							new LearningAlgorithms.EDSMReferenceLearner(learnerEval,ptaCopy,2).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
					dataSample.referenceLearner = estimateDifference(referenceGraph, outcomeOfReferenceLearner,testSet);
					dataSample.referenceLearner.inconsistency = MarkovClassifier.computeInconsistency(outcomeOfReferenceLearner, m, checker, false);
				}
				catch(LearningAlgorithms.LearnerAbortedException ex)
				{// the exception is thrown because the learner failed to learn anything completely. Ignore it because the default score is zero assigned via zeroScore. 
				}				
				dataSample.fractionOfStatesIdentifiedBySingletons=Math.round(100*MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(referenceGraph));
				dataSample.stateNumber = referenceGraph.getStateNumber();
				dataSample.transitionsSampled = Math.round(100*trimmedReference.pathroutines.countEdges()/referenceGraph.pathroutines.countEdges());
				statechum.Pair<Double,Double> correctnessOfMarkov = new MarkovClassifier(m, referenceGraph).evaluateCorrectnessOfMarkov();
				dataSample.markovPrecision = Math.round(100*correctnessOfMarkov.firstElem);dataSample.markovRecall = Math.round(100*correctnessOfMarkov.secondElem);
				Collection<List<Label>> wset=WMethod.computeWSet_reducedw(referenceGraph);
				int wSeqLen=0;
				for(List<Label> seq:wset)
				{
					int len = seq.size();if (len > wSeqLen) wSeqLen=len;
				}
				System.out.println("actual: "+actualAutomaton.getStateNumber()+" from reference learner: "+outcomeOfReferenceLearner.getStateNumber()+ 
						" difference actual is "+dataSample.actualLearner.inconsistency+ " difference ref is "+dataSample.referenceLearner.inconsistency
						+ " inconsistency learnt "+dataSample.actualLearner.inconsistency+" inconsistency reference: "+inconsistencyForTheReferenceGraph
						+" transitions per state: "+(double)referenceGraph.pathroutines.countEdges()/referenceGraph.getStateNumber()+
							" W seq max len "+wSeqLen+
							" Uniquely identifiable by W "+Math.round(100*MarkovClassifier.calculateFractionOfIdentifiedStates(referenceGraph, wset))+" %"
						+ " and by singletons "+Math.round(100*MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(referenceGraph))+" %"
						);
				outcome.samples.add(dataSample);
			}
			
			return outcome;
		}

		// Delegates to a specific estimator
		ScoresForGraph estimateDifference(LearnerGraph reference, LearnerGraph actual,Collection<List<Label>> testSet)
		{
			ScoresForGraph outcome = new ScoresForGraph();
			outcome.differenceStructural=DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(reference, actual, config, 1);
			outcome.differenceBCR=DifferenceToReferenceLanguageBCR.estimationOfDifference(reference, actual,testSet);
			return outcome;
		}
	}
	

	public static void main(String args[]) throws Exception
	{
		try
		{
			runExperiment();
		}
		catch(Exception ex)
		{
			ex.printStackTrace();
		}
		finally
		{
			DrawGraphs.end();
		}
	}
		
	@SuppressWarnings("null")
	public static void runExperiment() throws Exception
	{
		DrawGraphs gr = new DrawGraphs();
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setAskQuestions(false);config.setDebugMode(false);config.setGdLowToHighRatio(0.7);config.setRandomPathAttemptFudgeThreshold(1000);
		config.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);
		ConvertALabel converter = new Transform.InternStringLabel();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int ThreadNumber = ExperimentRunner.getCpuNumber();	
		ExecutorService executorService = Executors.newFixedThreadPool(ThreadNumber);
		final int minStateNumber = 20;
		final int samplesPerFSM = 5;
		final int stateNumberIncrement = 5;
		final int rangeOfStateNumbers = 30+stateNumberIncrement;
		
		final double traceLengthMultiplierMax = 2;
		final int chunkSize = 3;
		final double alphabetMultiplierMax = 2;
		
		/* A very unfavourable case.
		final double traceLengthMultiplierMax = 5;
		final int chunkSize = 4;
		final double alphabetMultiplierMax = 0.5;
		*/
		final int traceQuantity=6;// half of those generated will be negative that will be thrown away in most experiments.
		final String branch = "CAV2014;";
		// Stores tasks to complete.
		CompletionService<ThreadResult> runner = new ExecutorCompletionService<ThreadResult>(executorService);
/*
		final RBoxPlotP<String> gr_BCRForDifferentLearners = new RBoxPlotP<String>("","BCR",new File(branch+"BCR_learner.pdf"));
		final RBoxPlotP<String> gr_StructuralForDifferentLearners = new RBoxPlotP<String>("","structural",new File(branch+"structural_learner.pdf"));
		for(final boolean onlyPositives:new boolean[]{true})
			for(final double alphabetMultiplier:new double[]{alphabetMultiplierMax}) 
			{
						final int totalTaskNumber = traceQuantity;
						try
						{
							int numberOfTasks = 0;
							for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
								for(int sample=0;sample<samplesPerFSM;++sample)
								{
									EvaluationOfExisingLearnerRunner learnerRunner = new EvaluationOfExisingLearnerRunner(states,sample,totalTaskNumber+numberOfTasks,traceQuantity, config, converter);
									learnerRunner.setOnlyUsePositives(onlyPositives);
									learnerRunner.setAlphabetMultiplier(alphabetMultiplier);
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
		// Inference from a few traces
		final int traceQuantityToUse = traceQuantity;
		SquareBagPlot gr_StructuralDiff = new SquareBagPlot("Structural score, EDSM,>=2","Structural Score, EDSM-Markov learner",new File(branch+"_"+(traceQuantityToUse/2)+"_trace_structuraldiff.pdf"),0,1,true);
		SquareBagPlot gr_BCR = new SquareBagPlot("BCR, EDSM,>=2","BCR, EDSM-Markov learner",new File(branch+"_"+(traceQuantityToUse/2)+"_trace_bcr.pdf"),0.5,1,true);
		RBagPlot gr_BCR_singletons = new RBagPlot("%% states identified by singletons","BCR Score, EDSM-Markov learner",new File(branch+"_"+(traceQuantityToUse/2)+"_trace_bcr_singletons.pdf"));
		RBagPlot gr_BCR_states = new RBagPlot("number of states in reference","BCR Score, EDSM-Markov learner",new File(branch+"_"+(traceQuantityToUse/2)+"_trace_bcr_numberofstates.pdf"));
		ScatterPlot gr_ImprovementPerState = new ScatterPlot("State number", "BCR, improvement",new File(branch+"_"+(traceQuantityToUse/2)+"_bcr_statenumber.pdf"));
		for(final boolean onlyPositives:new boolean[]{true})
			for(final double alphabetMultiplier:new double[]{alphabetMultiplierMax}) 
			{
						final int totalTaskNumber = traceQuantityToUse;
						try
						{
							int numberOfTasks = 0;
							for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
								for(int sample=0;sample<samplesPerFSM;++sample)
								{
									LearnerRunner learnerRunner = new LearnerRunner(states,sample,totalTaskNumber+numberOfTasks,traceQuantityToUse, config, converter);
									learnerRunner.setOnlyUsePositives(onlyPositives);
									learnerRunner.setAlphabetMultiplier(alphabetMultiplier);
									learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);learnerRunner.setChunkLen(chunkSize);
									learnerRunner.setSelectionID(branch+"_states"+states+"_sample"+sample);
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
									gr_BCR_singletons.add((double)sample.fractionOfStatesIdentifiedBySingletons,sample.actualLearner.differenceBCR.getValue());
									gr_BCR_states.add((double)sample.stateNumber,sample.actualLearner.differenceBCR.getValue());
									if (sample.referenceLearner.differenceBCR.getValue() > 0)
									gr_ImprovementPerState.add((double)sample.stateNumber,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue());
								}
								progress.next();
							}
							gr_StructuralDiff.drawInteractive(gr);gr_BCR.drawInteractive(gr);gr_BCR_singletons.drawInteractive(gr);gr_BCR_states.drawInteractive(gr);gr_ImprovementPerState.drawInteractive(gr);
						}
						catch(Exception ex)
						{
							IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
							if (executorService != null) { executorService.shutdownNow();executorService = null; }
							throw e;
						}
			}
			if (gr_StructuralDiff != null) gr_StructuralDiff.drawPdf(gr);
			if (gr_BCR != null) gr_BCR.drawPdf(gr);
			if (gr_BCR_singletons != null) gr_BCR_singletons.drawPdf(gr);
			if (gr_BCR_states != null) gr_BCR_states.drawPdf(gr);
			if (gr_ImprovementPerState != null) gr_ImprovementPerState.drawPdf(gr);

		final RBoxPlot<String> gr_BCRImprovementForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","improvement, BCR",new File(branch+"BCR_vs_alphabet.pdf"));
		final RBoxPlot<String> gr_BCRForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","BCR",new File(branch+"BCR_absolute_vs_alphabet.pdf"));
		final RBoxPlot<String> gr_StructuralImprovementForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","improvement, structural",new File(branch+"structural_vs_alphabet.pdf"));
		final RBoxPlot<String> gr_StructuralForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","structural",new File(branch+"structural_absolute_vs_alphabet.pdf"));
		final RBoxPlot<String> gr_MarkovAccuracyForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","Markov accuracy",new File(branch+"markov_accuracy_vs_alphabet.pdf"));

		// Same experiment but with different alphabet size
		for(final boolean onlyPositives:new boolean[]{true})
			for(final double alphabetMultiplier:new double[]{alphabetMultiplierMax/16,alphabetMultiplierMax/8,alphabetMultiplierMax/4,alphabetMultiplierMax/2,alphabetMultiplierMax,alphabetMultiplierMax*2,alphabetMultiplierMax*4}) 
			{
				String selection;
					selection = branch+";EVALUATION;"+
							";onlyPositives="+onlyPositives+";";

				final int totalTaskNumber = traceQuantity;
				try
				{
					int numberOfTasks = 0;
					for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
						for(int sample=0;sample<samplesPerFSM;++sample)
						{
							LearnerRunner learnerRunner = new LearnerRunner(states,sample,totalTaskNumber+numberOfTasks,traceQuantity, config, converter);
							learnerRunner.setOnlyUsePositives(onlyPositives);
							learnerRunner.setTracesAlphabetMultiplier(alphabetMultiplierMax);
							learnerRunner.setAlphabetMultiplier(alphabetMultiplier);
							learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);learnerRunner.setChunkLen(chunkSize);
							learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
							runner.submit(learnerRunner);
							++numberOfTasks;
						}
					ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for "+selection+" alphabet multiplier: "+alphabetMultiplier, numberOfTasks);
					for(int count=0;count < numberOfTasks;++count)
					{
						ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.

						for(SampleData sample:result.samples)
						{
							if (sample.referenceLearner.differenceBCR.getValue() > 0)
							{
								gr_BCRImprovementForDifferentAlphabetSize.add(String.format("%.2f",alphabetMultiplier),sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue());
								gr_BCRForDifferentAlphabetSize.add(String.format("%.2f",alphabetMultiplier),sample.actualLearner.differenceBCR.getValue());
							}
							if (sample.referenceLearner.differenceStructural.getValue() > 0)
							{
								gr_StructuralImprovementForDifferentAlphabetSize.add(String.format("%.2f",alphabetMultiplier),sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue());
								gr_StructuralForDifferentAlphabetSize.add(String.format("%.2f",alphabetMultiplier),sample.actualLearner.differenceStructural.getValue());
							}
							gr_MarkovAccuracyForDifferentAlphabetSize.add(String.format("%.2f,P",alphabetMultiplier),(double)sample.markovPrecision,"green");
							gr_MarkovAccuracyForDifferentAlphabetSize.add(String.format("%.2f,R",alphabetMultiplier),(double)sample.markovRecall,"blue");
						}
						progress.next();
					}
					gr_BCRImprovementForDifferentAlphabetSize.drawInteractive(gr);gr_BCRForDifferentAlphabetSize.drawInteractive(gr);gr_MarkovAccuracyForDifferentAlphabetSize.drawInteractive(gr);
					gr_StructuralImprovementForDifferentAlphabetSize.drawInteractive(gr);gr_StructuralForDifferentAlphabetSize.drawInteractive(gr);
				}
				catch(Exception ex)
				{
					IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
					if (executorService != null) { executorService.shutdownNow();executorService = null; }
					throw e;
				}
			}
			if (gr_BCRImprovementForDifferentAlphabetSize != null) gr_BCRImprovementForDifferentAlphabetSize.drawPdf(gr);
			if (gr_StructuralImprovementForDifferentAlphabetSize != null) gr_StructuralImprovementForDifferentAlphabetSize.drawPdf(gr);
			if (gr_BCRForDifferentAlphabetSize != null) gr_BCRForDifferentAlphabetSize.drawPdf(gr);
			if (gr_StructuralForDifferentAlphabetSize != null) gr_StructuralForDifferentAlphabetSize.drawPdf(gr);
			if (gr_MarkovAccuracyForDifferentAlphabetSize != null) gr_MarkovAccuracyForDifferentAlphabetSize.drawPdf(gr);

			// Same experiment but with different number of sequences.
			final RBoxPlot<Integer> gr_BCRImprovementForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","improvement, BCR",new File(branch+"BCR_vs_tracenumber.pdf"));
			final RBoxPlot<Integer> gr_BCRForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","BCR",new File(branch+"BCR_absolute_vs_tracenumber.pdf"));
			final RBoxPlot<Integer> gr_StructuralImprovementForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","improvement, structural",new File(branch+"structural_vs_tracenumber.pdf"));
			final RBoxPlot<Integer> gr_StructuralForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","structural",new File(branch+"structural_absolute_vs_tracenumber.pdf"));
			for(final boolean onlyPositives:new boolean[]{true})
				for(final double alphabetMultiplier:new double[]{alphabetMultiplierMax}) 
					for(int traceNum=2;traceNum<traceQuantity*3;traceNum+=2)
					{
						String selection;
							selection = branch+";EVALUATION;"+
									";onlyPositives="+onlyPositives+";";

						final int totalTaskNumber = traceNum;
						try
						{
							int numberOfTasks = 0;
							for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
								for(int sample=0;sample<samplesPerFSM;++sample)
								{
									LearnerRunner learnerRunner = new LearnerRunner(states,sample,totalTaskNumber+numberOfTasks,traceNum, config, converter);
									learnerRunner.setOnlyUsePositives(onlyPositives);
									learnerRunner.setAlphabetMultiplier(alphabetMultiplier);
									learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);learnerRunner.setChunkLen(chunkSize);
									learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
									runner.submit(learnerRunner);
									++numberOfTasks;
								}
							ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for "+selection+" trace num: "+traceNum, numberOfTasks);
							for(int count=0;count < numberOfTasks;++count)
							{
								ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
								
								for(SampleData sample:result.samples)
								{
									if (sample.referenceLearner.differenceBCR.getValue() > 0)
									{
										// we'll generate both positives and negatives; in the considered experiments, only positives are used hence half the number of sequences are actually being learnt from.
										gr_BCRImprovementForDifferentNrOfTraces.add(traceNum/2,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue());
										gr_BCRForDifferentNrOfTraces.add(traceNum/2,sample.actualLearner.differenceBCR.getValue());
									}
									if (sample.referenceLearner.differenceStructural.getValue() > 0)
									{
										gr_StructuralImprovementForDifferentNrOfTraces.add(traceNum/2,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue());
										gr_StructuralForDifferentNrOfTraces.add(traceNum/2,sample.actualLearner.differenceStructural.getValue());
									}
								}
								progress.next();
							}
							gr_BCRForDifferentNrOfTraces.drawInteractive(gr);gr_StructuralForDifferentNrOfTraces.drawInteractive(gr);
						}
						catch(Exception ex)
						{
							IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
							if (executorService != null) { executorService.shutdownNow();executorService = null; }
							throw e;
						}
					}
			if (gr_BCRImprovementForDifferentNrOfTraces != null) gr_BCRImprovementForDifferentNrOfTraces.drawPdf(gr);
			if (gr_BCRForDifferentNrOfTraces != null) gr_BCRForDifferentNrOfTraces.drawPdf(gr);
			if (gr_StructuralImprovementForDifferentNrOfTraces != null) gr_StructuralImprovementForDifferentNrOfTraces.drawPdf(gr);
			if (gr_StructuralForDifferentNrOfTraces != null) gr_StructuralForDifferentNrOfTraces.drawPdf(gr);

			// Same experiment but with different number of sequences, both positive and negative.
			final RBoxPlot<Integer> gr_BCRImprovementForDifferentNrOfTracesPosNeg = new RBoxPlot<Integer>("nr of traces","improvement, BCR",new File(branch+"BCR_vs_tracenumber_posneg.pdf"));
			final RBoxPlot<Integer> gr_BCRForDifferentNrOfTracesPosNeg = new RBoxPlot<Integer>("nr of traces","BCR",new File(branch+"BCR_absolute_vs_tracenumber_posneg.pdf"));
			final RBoxPlot<Integer> gr_StructuralImprovementForDifferentNrOfTracesPosNeg = new RBoxPlot<Integer>("nr of traces","improvement, structural",new File(branch+"structural_vs_tracenumber_posneg.pdf"));
			final RBoxPlot<Integer> gr_StructuralForDifferentNrOfTracesPosNeg = new RBoxPlot<Integer>("nr of traces","structural",new File(branch+"structural_absolute_vs_tracenumber_posneg.pdf"));
			for(final boolean onlyPositives:new boolean[]{false})
				for(final double alphabetMultiplier:new double[]{alphabetMultiplierMax}) 
					for(int traceNum=2;traceNum<traceQuantity*3;traceNum+=2)
					{
						String selection;
						selection = branch+";EVALUATION;"+
								";onlyPositives="+onlyPositives+";";

						final int totalTaskNumber = traceNum;
						try
						{
							int numberOfTasks = 0;
							for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
								for(int sample=0;sample<samplesPerFSM;++sample)
								{
									LearnerRunner learnerRunner = new LearnerRunner(states,sample,totalTaskNumber+numberOfTasks,traceNum, config, converter);
									learnerRunner.setOnlyUsePositives(onlyPositives);
									learnerRunner.setAlphabetMultiplier(alphabetMultiplier);
									learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);learnerRunner.setChunkLen(chunkSize);
									learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
									runner.submit(learnerRunner);
									++numberOfTasks;
								}
							ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for "+selection+" trace num: "+traceNum, numberOfTasks);
							for(int count=0;count < numberOfTasks;++count)
							{
								ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
								
								for(SampleData sample:result.samples)
								{
									if (sample.referenceLearner.differenceBCR.getValue() > 0)
									{
										gr_BCRImprovementForDifferentNrOfTracesPosNeg.add(traceNum,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue());
										gr_BCRForDifferentNrOfTracesPosNeg.add(traceNum,sample.actualLearner.differenceBCR.getValue());
									}
									if (sample.referenceLearner.differenceStructural.getValue() > 0)
									{
										gr_StructuralImprovementForDifferentNrOfTracesPosNeg.add(traceNum,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue());
										gr_StructuralForDifferentNrOfTracesPosNeg.add(traceNum,sample.actualLearner.differenceStructural.getValue());
									}
								}
								progress.next();
							}
							gr_BCRForDifferentNrOfTracesPosNeg.drawInteractive(gr);gr_StructuralForDifferentNrOfTracesPosNeg.drawInteractive(gr);
						}
						catch(Exception ex)
						{
							IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
							if (executorService != null) { executorService.shutdownNow();executorService = null; }
							throw e;
						}
					}
		if (gr_BCRImprovementForDifferentNrOfTracesPosNeg != null) gr_BCRImprovementForDifferentNrOfTracesPosNeg.drawPdf(gr);
		if (gr_BCRForDifferentNrOfTracesPosNeg != null) gr_BCRForDifferentNrOfTracesPosNeg.drawPdf(gr);
		if (gr_StructuralImprovementForDifferentNrOfTracesPosNeg != null) gr_StructuralImprovementForDifferentNrOfTracesPosNeg.drawPdf(gr);
		if (gr_StructuralForDifferentNrOfTracesPosNeg != null) gr_StructuralForDifferentNrOfTracesPosNeg.drawPdf(gr);

		// Same experiment but with different trace length but the same number of sequences
		final RBoxPlot<Double> gr_BCRImprovementForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","improvement, BCR",new File(branch+"BCR_vs_tracelength.pdf"));
		final RBoxPlot<Double> gr_BCRForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","BCR",new File(branch+"BCR_absolute_vs_tracelength.pdf"));
		final RBoxPlot<Double> gr_StructuralImprovementForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","improvement, structural",new File(branch+"structural_vs_tracelength.pdf"));
		final RBoxPlot<Double> gr_StructuralForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","structural",new File(branch+"structural_absolute_vs_tracelength.pdf"));
		final RBoxPlot<Double> gr_TransitionCoverageForDifferentLengthOfTraces = new RBoxPlot<Double>("trace length multiplier","transition coverage",new File(branch+"transitionCoverage_vs_tracelength.pdf"));
		for(final boolean onlyPositives:new boolean[]{true})
			for(double traceMultiplier=0.125;traceMultiplier<10;traceMultiplier*=2.) 
				{
					String selection;
						selection = branch+";EVALUATION;"+
								";onlyPositives="+onlyPositives+";";

					final int totalTaskNumber = traceQuantity;
					try
					{
						int numberOfTasks = 0;
						for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
							for(int sample=0;sample<samplesPerFSM;++sample)
							{
								LearnerRunner learnerRunner = new LearnerRunner(states,sample,totalTaskNumber+numberOfTasks,traceQuantity, config, converter);
								learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
								learnerRunner.setOnlyUsePositives(onlyPositives);
								learnerRunner.setTraceLengthMultiplier(traceMultiplier);learnerRunner.setChunkLen(chunkSize);
								learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
								runner.submit(learnerRunner);
								++numberOfTasks;
							}
						ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for "+selection+" trace length multiplier "+traceMultiplier, numberOfTasks);
						for(int count=0;count < numberOfTasks;++count)
						{
							ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
							
							for(SampleData sample:result.samples)
							{
								if (sample.referenceLearner.differenceBCR.getValue() > 0)
								{
									gr_BCRImprovementForDifferentLengthOfTraces.add(traceMultiplier,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue());
									gr_BCRForDifferentLengthOfTraces.add(traceMultiplier,sample.actualLearner.differenceBCR.getValue());
								}
								if (sample.referenceLearner.differenceStructural.getValue() > 0)
								{
									gr_StructuralImprovementForDifferentLengthOfTraces.add(traceMultiplier,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue());
									gr_StructuralForDifferentLengthOfTraces.add(traceMultiplier,sample.actualLearner.differenceStructural.getValue());
								}
								gr_TransitionCoverageForDifferentLengthOfTraces.add(traceMultiplier,(double)sample.transitionsSampled);
							}
							progress.next();
						}
						
						gr_BCRForDifferentLengthOfTraces.drawInteractive(gr);gr_StructuralForDifferentLengthOfTraces.drawInteractive(gr);gr_TransitionCoverageForDifferentLengthOfTraces.drawInteractive(gr);
					}
					catch(Exception ex)
					{
						IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
						if (executorService != null) { executorService.shutdownNow();executorService = null; }
						throw e;
					}
				}
		if (gr_BCRImprovementForDifferentLengthOfTraces != null) gr_BCRImprovementForDifferentLengthOfTraces.drawPdf(gr);
		if (gr_BCRForDifferentLengthOfTraces != null) gr_BCRForDifferentLengthOfTraces.drawPdf(gr);
		if (gr_StructuralImprovementForDifferentLengthOfTraces != null) gr_StructuralImprovementForDifferentLengthOfTraces.drawPdf(gr);
		if (gr_StructuralForDifferentLengthOfTraces != null) gr_StructuralForDifferentLengthOfTraces.drawPdf(gr);
		if (gr_TransitionCoverageForDifferentLengthOfTraces != null) gr_TransitionCoverageForDifferentLengthOfTraces.drawPdf(gr);

		// Same experiment but with different prefix length but the same number of sequences and their length
		final RBoxPlot<Integer> gr_BCRImprovementForDifferentPrefixlen = new RBoxPlot<Integer>("length of prefix","improvement, BCR",new File(branch+"BCR_vs_prefixLength.pdf"));
		final RBoxPlot<Integer> gr_BCRForDifferentPrefixlen = new RBoxPlot<Integer>("length of prefix","BCR",new File(branch+"BCR_absolute_vs_prefixLength.pdf"));
		final RBoxPlot<Integer> gr_StructuralImprovementForDifferentPrefixlen = new RBoxPlot<Integer>("length of prefix","improvement, structural",new File(branch+"structural_vs_prefixLength.pdf"));
		final RBoxPlot<Integer> gr_StructuralForDifferentPrefixlen = new RBoxPlot<Integer>("length of prefix","structural",new File(branch+"structural_absolute_vs_prefixLength.pdf"));
		final RBoxPlot<String> gr_MarkovAccuracyForDifferentPrefixlen = new RBoxPlot<String>("length of prefix","Markov accuracy",new File(branch+"markov_accuracy_vs_prefixLength.pdf"));
		for(final boolean onlyPositives:new boolean[]{true})
			for(int prefixLength=1;prefixLength<6;++prefixLength) 
				{
					String selection;
						selection = branch+";EVALUATION;"+
								";onlyPositives="+onlyPositives+";";

						final int totalTaskNumber = traceQuantity;
						try
						{
							int numberOfTasks = 0;
							for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
								for(int sample=0;sample<samplesPerFSM;++sample)
								{
									LearnerRunner learnerRunner = new LearnerRunner(states,sample,totalTaskNumber+numberOfTasks,traceQuantity, config, converter);
									learnerRunner.setAlphabetMultiplier(alphabetMultiplierMax);
									learnerRunner.setOnlyUsePositives(onlyPositives);
									learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);learnerRunner.setChunkLen(prefixLength+1);
									learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
									runner.submit(learnerRunner);
									++numberOfTasks;
								}
							ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for "+selection+" using prefix length "+prefixLength, numberOfTasks);
							for(int count=0;count < numberOfTasks;++count)
							{
								ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
								
								for(SampleData sample:result.samples)
								{
									if (sample.referenceLearner.differenceBCR.getValue() > 0)
									{
										gr_BCRImprovementForDifferentPrefixlen.add(prefixLength,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue());
										gr_BCRForDifferentPrefixlen.add(prefixLength,sample.actualLearner.differenceBCR.getValue());
									}
									if (sample.referenceLearner.differenceStructural.getValue() > 0)
									{
										gr_StructuralImprovementForDifferentPrefixlen.add(prefixLength,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue());
										gr_StructuralForDifferentPrefixlen.add(prefixLength,sample.actualLearner.differenceStructural.getValue());
									}
									gr_MarkovAccuracyForDifferentPrefixlen.add(""+prefixLength+",P",(double)sample.markovPrecision,"green");
									gr_MarkovAccuracyForDifferentPrefixlen.add(""+prefixLength+",R",(double)sample.markovRecall,"blue");
								}
								progress.next();
							}
							gr_BCRForDifferentPrefixlen.drawInteractive(gr);gr_StructuralForDifferentPrefixlen.drawInteractive(gr);gr_MarkovAccuracyForDifferentPrefixlen.drawInteractive(gr);
						}
						catch(Exception ex)
						{
							IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
							if (executorService != null) { executorService.shutdownNow();executorService = null; }
							throw e;
						}
						
				}
		if (gr_BCRImprovementForDifferentPrefixlen != null) gr_BCRImprovementForDifferentPrefixlen.drawPdf(gr);
		if (gr_BCRForDifferentPrefixlen != null) gr_BCRForDifferentPrefixlen.drawPdf(gr);
		if (gr_StructuralImprovementForDifferentPrefixlen != null) gr_StructuralImprovementForDifferentPrefixlen.drawPdf(gr);
		if (gr_StructuralForDifferentPrefixlen != null) gr_StructuralForDifferentPrefixlen.drawPdf(gr);
		if (gr_MarkovAccuracyForDifferentPrefixlen != null) gr_MarkovAccuracyForDifferentPrefixlen.drawPdf(gr);
		
		if (executorService != null) { executorService.shutdown();executorService = null; }
	}
}