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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Stack;
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
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.PaperUAS;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.TrueFalseCounter;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

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
		
		class UnusualVertices implements Comparable<UnusualVertices>
		{
			final public long score;
			final public List<CmpVertex> vertices;
			final public List<StatePair> verticesToMerge;
			
			public UnusualVertices(long s, List<CmpVertex> v, List<StatePair> p) 
			{
				score = s;vertices=v;verticesToMerge=p;
			}

			@Override
			public int compareTo(UnusualVertices o) {
				return (int)(score - o.score);
			}

			/* (non-Javadoc)
			 * @see java.lang.Object#hashCode()
			 */
			@Override
			public int hashCode() {
				final int prime = 31;
				int result = 1;
				result = prime * result + getOuterType().hashCode();
				result = prime * result + (int)score;
				result = prime * result
						+ ((vertices == null) ? 0 : vertices.hashCode());
				return result;
			}

			/* (non-Javadoc)
			 * @see java.lang.Object#equals(java.lang.Object)
			 */
			@Override
			public boolean equals(Object obj) {
				if (this == obj)
					return true;
				if (obj == null)
					return false;
				if (!(obj instanceof UnusualVertices))
					return false;
				UnusualVertices other = (UnusualVertices) obj;
				if (!getOuterType().equals(other.getOuterType()))
					return false;
				if (score != other.score)
					return false;
				if (vertices == null) {
					if (other.vertices != null)
						return false;
				} else if (!vertices.equals(other.vertices))
					return false;
				return true;
			}

			private LearnerRunner getOuterType() {
				return LearnerRunner.this;
			}
		}
		
		@Override
		public ThreadResult call() throws Exception 
		{
			final int alphabet = (int)(alphabetMultiplier*states);
			LearnerGraph referenceGraph = null;
			ThreadResult outcome = new ThreadResult();
			MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
			referenceGraph = mg.nextMachine(alphabet,seed, config, converter).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
			
			LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);learnerEval.setLabelConverter(converter);
			final Collection<List<Label>> testSet = PaperUAS.computeEvaluationSet(referenceGraph,states*3,states*alphabet);

			for(int attempt=0;attempt<2;++attempt)
			{// try learning the same machine a few times
				LearnerGraph pta = new LearnerGraph(config);
				RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(attempt),5,null);
				// test sequences will be distributed around 
				final int pathLength = generator.getPathLength();
				// The total number of elements in test sequences (alphabet*states*traceQuantity) will be distributed around (random(pathLength)+1). The total size of PTA is a product of these two.
				// For the purpose of generating long traces, we construct as many traces as there are states but these traces have to be rather long,
				// that is, length of traces will be (random(pathLength)+1)*sequencesPerChunk/states and the number of traces generated will be the same as the number of states.
				final int tracesToGenerate = makeEven(traceQuantity);
				final Random rnd = new Random(seed*31+attempt);
				generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
										
						@Override
						public int getLength() {
							return (int)(traceLengthMultiplier*states*alphabet);
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
				final MarkovModel m= new MarkovModel(chunkLen,true,true);
				m.createMarkovLearner(sPlus, sMinus,false);
				
				pta.clearColours();

				if (!onlyUsePositives)
					assert pta.getStateNumber() > pta.getAcceptStateNumber() : "graph with only accept states but onlyUsePositives is not set";
				else 
					assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
				
				LearnerMarkovPassive learnerOfPairs = null;
				LearnerGraph actualAutomaton = null;
				
				final Configuration deepCopy = pta.config.copy();deepCopy.setLearnerCloneGraph(true);
				LearnerGraph ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);

				// now use pathsToMerge to compute which states can/cannot be merged together.
				LearnerGraph trimmedReference = MarkovPassivePairSelection.trimUncoveredTransitions(pta,referenceGraph);
				
				final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklisting();
				long inconsistencyForTheReferenceGraph = MarkovClassifier.computeInconsistency(referenceGraph, m, checker,false);
				
				learnerOfPairs = new LearnerMarkovPassive(learnerEval,referenceGraph,pta);learnerOfPairs.setMarkovModel(m);

				learnerOfPairs.setScoreComputationOverride(new statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure() {
					
					@SuppressWarnings("unused")
					@Override
					public CmpVertex selectRedNode(LearnerGraph gr,Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
					{
						return tentativeRedNodes.iterator().next();
					}
					
					@SuppressWarnings("unused")
					@Override
					public CmpVertex resolvePotentialDeadEnd(LearnerGraph gr, Collection<CmpVertex> reds, List<PairScore> pairs) 
					{
						return null;
					}
					
					LearnerGraph coregraph = null;
					LearnerGraph extendedGraph = null;
					MarkovClassifier cl=null;
					
					@Override
					public void initComputation(LearnerGraph graph) 
					{
						coregraph = graph;

						cl = new MarkovClassifier(m, coregraph);
					    extendedGraph = cl.constructMarkovTentative();
					}
					
					@Override
					public long overrideScoreComputation(PairScore p) 
					{

						long pairScore = p.getScore();
						
						if (pairScore >= 0)
							pairScore = MarkovScoreComputation.computenewscore(p, extendedGraph);

						return pairScore;
					}

					/** This one returns a set of transitions in all directions. */
					@SuppressWarnings("unused")
					@Override
					public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(CmpVertex currentRed) 
					{
						return null;
					}

				});
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
				dataSample.actualLearner.inconsistency = MarkovClassifier.computeInconsistency(actualAutomaton, m, checker,false);
				LearnerGraph outcomeOfReferenceLearner = new PTAReferenceLearner(learnerEval,referenceGraph,ptaCopy).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
				dataSample.referenceLearner = estimateDifference(referenceGraph, outcomeOfReferenceLearner,testSet);
				dataSample.referenceLearner.inconsistency = MarkovClassifier.computeInconsistency(outcomeOfReferenceLearner, m, checker,false);
				
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
							" Uniquely identifiable by W "+Math.round(100*MarkovClassifier.calculateFractionOfIdentifiedStates(referenceGraph, wset))+"%"
						+ " and by singletons "+Math.round(100*MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(referenceGraph))+"%"
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
	
	
	/** Merges states using a routing relying on PTA, that faster and consumes less memory than the general one. */
	public static class PTAReferenceLearner extends ReferenceLearner
	{

		public PTAReferenceLearner(LearnerEvaluationConfiguration evalCnf, LearnerGraph argReferenceGraph, LearnerGraph argInitialPTA) 
		{
			super(evalCnf, argReferenceGraph, argInitialPTA);
		}

		@Override 
		public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair)
		{
			return MergeStates.mergeAndDeterminize(original, pair);
		}
		
	}
	
	/** Uses the supplied classifier to rank pairs. */
	public static class LearnerMarkovPassive extends LearnerThatCanClassifyPairs
	{
		protected Map<Long,TrueFalseCounter> pairQuality = null;
		private int num_states;
		private int numtraceQuantity;
		private int num_seed;
		private int lengthMultiplier;
		public MarkovModel Markov;
		RedNodeSelectionProcedure computationOverride = null;
		
		public void setPairQualityCounter(Map<Long,TrueFalseCounter> argCounter)
		{
			pairQuality = argCounter;
		}
		
		List<List<List<Label>>> pairsToMerge = null;
		
		public void setPairsToMerge(List<List<List<Label>>> pairs)
		{
			pairsToMerge = pairs;
		}
		
		public List<List<List<Label>>> getPairsToMerge()
		{
			return pairsToMerge;
		}
		
		public void  setlengthMultiplier(int setlengthMultiplier)
		{
			lengthMultiplier = setlengthMultiplier;
		}
		
		public int  getlengthMultiplier()
		{
			return lengthMultiplier;
		}

		public void set_States(int states) 
		{
			num_states=	states;		
		}
		
		public MarkovModel Markov() 
		{
			return Markov;			
		}
			
		public void setMarkovModel(MarkovModel m) 
		{
			Markov=m;
		}

		public void set_traceQuantity(int traceQuantity) 
		{
			numtraceQuantity=traceQuantity;			
		}
		
		public int get_States() 
		{
			return num_states;		
		}
	
		public int get_traceQuantity() 
		{
			return numtraceQuantity;			
		}
		
		public void set_seed(int i) 
		{
			num_seed=i;
		}
		
		public int get_seed() 
		{
			return num_seed;
		}

		public void setScoreComputationOverride(RedNodeSelectionProcedure c)
		{
			computationOverride = c;
		}
		
		
		/** During the evaluation of the red-blue pairs, where all pairs are predicted to be unmergeable, one of the blue states will be returned as red. */
		protected boolean classifierToChooseWhereNoMergeIsAppropriate = false;
		
		/** Used to select next red state based on the subjective quality of the subsequent set of red-blue pairs, as determined by the classifier. */
		protected boolean useClassifierToChooseNextRed = false;
		
		public void setUseClassifierForRed(boolean classifierForRed)
		{
			useClassifierToChooseNextRed = classifierForRed;
		}
		
		public void setUseClassifierToChooseNextRed(boolean classifierToBlockAllMergers)
		{
			classifierToChooseWhereNoMergeIsAppropriate = classifierToBlockAllMergers;
		}

		public LearnerMarkovPassive(LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, final LearnerGraph argInitialPTA) 
		{
			super(evalCnf,argReferenceGraph,argInitialPTA);
		}
		
		public static String refToString(Object obj)
		{
			return obj == null?"null":obj.toString();
		}
		
		@Override 
		public Stack<PairScore> ChooseStatePairs(final LearnerGraph graph)
		{
			Stack<PairScore> outcome = graph.pairscores.chooseStatePairs(LearnerMarkovPassive.this.computationOverride);
			
			if (!outcome.isEmpty())
			{
				PairScore result = null;
				
				result=pickPairQSMLike(outcome);
				assert result!=null;
				assert result.getScore()>=0;

				outcome.clear();outcome.push(result);
			}	
			return outcome;

		}

		@Override 
		public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair)
		{
			return MergeStates.mergeAndDeterminize(original, pair);
		}
	}

	public static void updateGraph(final RBoxPlot<Long> gr_PairQuality, Map<Long,TrueFalseCounter> pairQuality)
	{
		if (gr_PairQuality != null)
		{
			for(Entry<Long,TrueFalseCounter> entry:pairQuality.entrySet())
				gr_PairQuality.add(entry.getKey(), 100*entry.getValue().trueCounter/((double)entry.getValue().trueCounter+entry.getValue().falseCounter));
		}		
	}

	/** Records scores of pairs that are correctly classified and misclassified. */
	protected static void updateStatistics( Map<Long,TrueFalseCounter> pairQuality, LearnerGraph tentativeGraph, LearnerGraph referenceGraph, Collection<PairScore> pairsToConsider)
	{
		if (!pairsToConsider.isEmpty() && pairQuality != null)
		{
			List<PairScore> correctPairs = new ArrayList<PairScore>(pairsToConsider.size()), wrongPairs = new ArrayList<PairScore>(pairsToConsider.size());
			SplitSetOfPairsIntoRightAndWrong(tentativeGraph, referenceGraph, pairsToConsider, correctPairs, wrongPairs);

			
			for(PairScore pair:pairsToConsider)
			{
				if (pair.getQ().isAccept() && pair.getR().isAccept() && pair.getScore() < 150)
					synchronized(pairQuality)
					{
						TrueFalseCounter counter = pairQuality.get(pair.getScore());
						if (counter == null)
						{
							counter = new TrueFalseCounter();pairQuality.put(pair.getScore(),counter);
						}
						if (correctPairs.contains(pair))
							counter.trueCounter++;
						else
							counter.falseCounter++;
					}
			}
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
		config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);
		ConvertALabel converter = new Transform.InternStringLabel();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int ThreadNumber = ExperimentRunner.getCpuNumber();	
		ExecutorService executorService = Executors.newFixedThreadPool(ThreadNumber);
		final int minStateNumber = 20;
		final int samplesPerFSM = 10;
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
		final int traceQuantity=5;
		final String branch = "CAV2014";
		// Stores tasks to complete.
		CompletionService<ThreadResult> runner = new ExecutorCompletionService<ThreadResult>(executorService);

		for(final boolean onlyPositives:new boolean[]{true})
			for(final double alphabetMultiplier:new double[]{alphabetMultiplierMax}) 
			{
					String selection;
						selection = branch+";EVALUATION"+";alphabetMultiplier="+alphabetMultiplier+
								";onlyPositives="+onlyPositives+";";

						final int totalTaskNumber = traceQuantity;
						SquareBagPlot gr_StructuralDiff = new SquareBagPlot("Structural diff Score, original","Structural Score, Markov Learner",new File(selection+"structuraldiff.pdf"),0,1,true);
						SquareBagPlot gr_BCR = new SquareBagPlot("BCR, original","BCR, Markov Learner",new File(selection+"bcr.pdf"),0.5,1,true);
						try
						{
							int numberOfTasks = 0;
							for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
								for(int sample=0;sample<samplesPerFSM;++sample)
								{
									LearnerRunner learnerRunner = new LearnerRunner(states,sample,totalTaskNumber+numberOfTasks,traceQuantity, config, converter);
									learnerRunner.setOnlyUsePositives(onlyPositives);
									learnerRunner.setAlphabetMultiplier(alphabetMultiplier);
									learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);learnerRunner.setChunkLen(chunkSize);
									learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
									runner.submit(learnerRunner);
									++numberOfTasks;
								}
							ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for "+selection, numberOfTasks);
							for(int count=0;count < numberOfTasks;++count)
							{
								ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
								for(SampleData sample:result.samples)
									gr_StructuralDiff.add(sample.referenceLearner.differenceStructural.getValue(),sample.actualLearner.differenceStructural.getValue());
							
								for(SampleData sample:result.samples)
									gr_BCR.add(sample.referenceLearner.differenceBCR.getValue(),sample.actualLearner.differenceBCR.getValue());

								progress.next();
							}
							
						}
						catch(Exception ex)
						{
							IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
							if (executorService != null) { executorService.shutdownNow();executorService = null; }
							throw e;
						}
						if (gr_StructuralDiff != null) gr_StructuralDiff.drawPdf(gr);if (gr_BCR != null) gr_BCR.drawPdf(gr);
			}

		final RBoxPlot<String> gr_BCRImprovementForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","improvement, BCR",new File("BCR_vs_alphabet.pdf"));
		final RBoxPlot<String> gr_StructuralImprovementForDifferentAlphabetSize = new RBoxPlot<String>("alphabet multiplier","improvement, structural",new File("structural_vs_alphabet.pdf"));
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
									learnerRunner.setAlphabetMultiplier(alphabetMultiplier);
									learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);learnerRunner.setChunkLen(chunkSize);
									learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
									runner.submit(learnerRunner);
									++numberOfTasks;
								}
							ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for "+selection, numberOfTasks);
							for(int count=0;count < numberOfTasks;++count)
							{
								ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.

								for(SampleData sample:result.samples)
								{
									if (sample.referenceLearner.differenceBCR.getValue() > 0)
										gr_BCRImprovementForDifferentAlphabetSize.add(String.format("%.2f",alphabetMultiplier),sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue());
									if (sample.referenceLearner.differenceStructural.getValue() > 0)
										gr_StructuralImprovementForDifferentAlphabetSize.add(String.format("%.2f",alphabetMultiplier),sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue());
								}
								progress.next();
							}
						}
						catch(Exception ex)
						{
							IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
							if (executorService != null) { executorService.shutdownNow();executorService = null; }
							throw e;
						}
			}
			if (gr_BCRImprovementForDifferentAlphabetSize != null) gr_BCRImprovementForDifferentAlphabetSize.drawPdf(gr);if (gr_StructuralImprovementForDifferentAlphabetSize != null) gr_StructuralImprovementForDifferentAlphabetSize.drawPdf(gr);
	
		// Same experiment but with different number of sequences.
		final RBoxPlot<Integer> gr_BCRImprovementForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","improvement, BCR",new File("BCR_vs_tracenumber.pdf"));
		final RBoxPlot<Integer> gr_StructuralImprovementForDifferentNrOfTraces = new RBoxPlot<Integer>("nr of traces","improvement, structural",new File("structural_vs_tracenumber.pdf"));
		for(final boolean onlyPositives:new boolean[]{true})
			for(final double alphabetMultiplier:new double[]{alphabetMultiplierMax}) 
				for(int traceNum=1;traceNum<traceQuantity*5;++traceNum)
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
									LearnerRunner learnerRunner = new LearnerRunner(states,sample,totalTaskNumber+numberOfTasks,traceNum, config, converter);
									learnerRunner.setOnlyUsePositives(onlyPositives);
									learnerRunner.setAlphabetMultiplier(alphabetMultiplier);
									learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);learnerRunner.setChunkLen(chunkSize);
									learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
									runner.submit(learnerRunner);
									++numberOfTasks;
								}
							ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for "+selection, numberOfTasks);
							for(int count=0;count < numberOfTasks;++count)
							{
								ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
								
								for(SampleData sample:result.samples)
								{
									if (sample.referenceLearner.differenceBCR.getValue() > 0)
										gr_BCRImprovementForDifferentNrOfTraces.add(traceNum,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue());
									if (sample.referenceLearner.differenceStructural.getValue() > 0)
										gr_StructuralImprovementForDifferentNrOfTraces.add(traceNum,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue());
								}
								progress.next();
							}
						}
						catch(Exception ex)
						{
							IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
							if (executorService != null) { executorService.shutdownNow();executorService = null; }
							throw e;
						}
						if (gr_BCRImprovementForDifferentNrOfTraces != null) gr_BCRImprovementForDifferentNrOfTraces.drawPdf(gr);if (gr_StructuralImprovementForDifferentNrOfTraces != null) gr_StructuralImprovementForDifferentNrOfTraces.drawPdf(gr);
				}


		// Same experiment but with different trace length but the same number of sequences
		final RBoxPlot<Integer> gr_BCRImprovementForDifferentLengthOfTraces = new RBoxPlot<Integer>("nr of traces","improvement, BCR",new File("BCR_vs_tracelength.pdf"));
		final RBoxPlot<Integer> gr_StructuralImprovementForDifferentLengthOfTraces = new RBoxPlot<Integer>("nr of traces","improvement, structural",new File("structural_vs_tracelength.pdf"));
		for(final boolean onlyPositives:new boolean[]{true})
			for(int traceMultiplier=1;traceMultiplier<8;++traceMultiplier) 
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
							ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for "+selection, numberOfTasks);
							for(int count=0;count < numberOfTasks;++count)
							{
								ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
								
								for(SampleData sample:result.samples)
								{
									if (sample.referenceLearner.differenceBCR.getValue() > 0)
										gr_BCRImprovementForDifferentLengthOfTraces.add(traceMultiplier,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue());
									if (sample.referenceLearner.differenceStructural.getValue() > 0)
										gr_StructuralImprovementForDifferentLengthOfTraces.add(traceMultiplier,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue());
								}
								progress.next();
							}
						}
						catch(Exception ex)
						{
							IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
							if (executorService != null) { executorService.shutdownNow();executorService = null; }
							throw e;
						}
						if (gr_BCRImprovementForDifferentLengthOfTraces != null) gr_BCRImprovementForDifferentLengthOfTraces.drawPdf(gr);if (gr_StructuralImprovementForDifferentLengthOfTraces != null) gr_StructuralImprovementForDifferentLengthOfTraces.drawPdf(gr);
				}

		// Same experiment but with different prefix length but the same number of sequences and their length
		final RBoxPlot<Integer> gr_BCRImprovementForDifferentPrefixlen = new RBoxPlot<Integer>("nr of traces","improvement, BCR",new File("BCR_vs_prefixLength.pdf"));
		final RBoxPlot<Integer> gr_StructuralImprovementForDifferentPrefixlen = new RBoxPlot<Integer>("nr of traces","improvement, structural",new File("structural_vs_prefixLength.pdf"));
		for(final boolean onlyPositives:new boolean[]{true})
			for(int prefixLength=1;prefixLength<8;++prefixLength) 
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
									learnerRunner.setTraceLengthMultiplier(traceLengthMultiplierMax);learnerRunner.setChunkLen(prefixLength);
									learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
									runner.submit(learnerRunner);
									++numberOfTasks;
								}
							ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for "+selection, numberOfTasks);
							for(int count=0;count < numberOfTasks;++count)
							{
								ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
								
								for(SampleData sample:result.samples)
								{
									if (sample.referenceLearner.differenceBCR.getValue() > 0)
										gr_BCRImprovementForDifferentPrefixlen.add(prefixLength,sample.actualLearner.differenceBCR.getValue()/sample.referenceLearner.differenceBCR.getValue());
									if (sample.referenceLearner.differenceStructural.getValue() > 0)
										gr_StructuralImprovementForDifferentPrefixlen.add(prefixLength,sample.actualLearner.differenceStructural.getValue()/sample.referenceLearner.differenceStructural.getValue());
								}
								progress.next();
							}
						}
						catch(Exception ex)
						{
							IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
							if (executorService != null) { executorService.shutdownNow();executorService = null; }
							throw e;
						}
						if (gr_BCRImprovementForDifferentPrefixlen != null) gr_BCRImprovementForDifferentPrefixlen.drawPdf(gr);if (gr_StructuralImprovementForDifferentPrefixlen != null) gr_StructuralImprovementForDifferentPrefixlen.drawPdf(gr);
				}

		if (executorService != null) { executorService.shutdown();executorService = null; }
	}
}