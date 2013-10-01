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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;
import java.util.Stack;
import java.util.Timer;
import java.util.TimerTask;
import java.util.TreeMap;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Label;
import statechum.ProgressIndicator;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.analysis.learning.MarkovUniversalLearner;
import statechum.analysis.learning.MarkovUniversalLearner.UpdatablePairDouble;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.PaperUAS;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.CollectionOfPairsEstimator;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.TrueFalseCounter;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

public class MarkovPassivePairSelection extends PairQualityLearner
{
	public static class LearnerRunner implements Callable<ThreadResult>
	{
		protected final Configuration config;
		protected final ConvertALabel converter;
		protected final int states,sample;
		protected boolean onlyUsePositives, pickUniqueFromInitial;
		protected final int seed;
		protected final int traceQuantity;
		protected int lengthMultiplier = 1;
		protected String selectionID;

		public void setSelectionID(String value)
		{
			selectionID = value;
		}
		
		public void setLengthMultiplier(int value)
		{
			lengthMultiplier = value;
		}
		
		/** Whether to filter the collection of traces such that only positive traces are used. */
		public void setOnlyUsePositives(boolean value)
		{
			onlyUsePositives = value;
		}
		
		/** Where a transition that can be uniquely identifying an initial state be used both for mergers and for building a partly-merged PTA. */
		public void setPickUniqueFromInitial(boolean value)
		{
			pickUniqueFromInitial = value;
		}
		
		public LearnerRunner(int argStates, int argSample, int argSeed, int nrOfTraces, Configuration conf, ConvertALabel conv)
		{
			states = argStates;sample = argSample;config = conf;seed = argSeed;traceQuantity=nrOfTraces;converter=conv;
		}
		
		@Override
		public ThreadResult call() throws Exception 
		{
			final int alphabet = 2*states;
			LearnerGraph referenceGraph = null;
			ThreadResult outcome = new ThreadResult();
			Label uniqueFromInitial = null;
			Timer timerToDetectLongRunningAutomata = new Timer("timer_to_detect_lengthy_tasks");
			MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
			do
			{
				referenceGraph = mg.nextMachine(alphabet,seed, config, converter).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
				if (pickUniqueFromInitial)
				{
					Map<Label,CmpVertex> uniques = uniqueFromState(referenceGraph);
					if(!uniques.isEmpty())
					{
						Entry<Label,CmpVertex> entry = uniques.entrySet().iterator().next();
						referenceGraph.setInit(entry.getValue());uniqueFromInitial = entry.getKey();
					}
				}
			}
			while(pickUniqueFromInitial && uniqueFromInitial == null);
			
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
				final int tracesToGenerate = makeEven(states*traceQuantity);
				final Random rnd = new Random(seed*31+attempt);
				generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
										
						@Override
						public int getLength() {
							return (rnd.nextInt(pathLength)+1)*lengthMultiplier;
						}
		
						@Override
						public int getPrefixLength(int len) {
							return len;
						}
					});
				/*
				for(List<Label> seq:referenceGraph.wmethod.computeNewTestSet(1))
				{
					pta.paths.augmentPTA(seq, referenceGraph.getVertex(seq) != null, false, null);
				}*/
				//pta.paths.augmentPTA(referenceGraph.wmethod.computeNewTestSet(referenceGraph.getInit(),1));// this one will not set any states as rejects because it uses shouldbereturned
				//referenceGraph.pathroutines.completeGraph(referenceGraph.nextID(false));
				if (onlyUsePositives)
					pta.paths.augmentPTA(generator.getAllSequences(0).filter(new FilterPredicate() {
						@Override
						public boolean shouldBeReturned(Object name) {
							return ((statechum.analysis.learning.rpnicore.RandomPathGenerator.StateName)name).accept;
						}
					}));
				else
					pta.paths.augmentPTA(generator.getAllSequences(0));// the PTA will have very few reject-states because we are generating few sequences and hence there will be few negative sequences.
					// In order to approximate the behaviour of our case study, we need to compute which pairs are not allowed from a reference graph and use those as if-then automata to start the inference.
				//pta.paths.augmentPTA(referenceGraph.wmethod.computeNewTestSet(referenceGraph.getInit(),1));
		
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
				Set<Label> all_events_in_graph = referenceGraph.learnerCache.getAlphabet();
				MarkovUniversalLearner m= new MarkovUniversalLearner();
				m.CreatingMarkovLearner(sPlus, sMinus, all_events_in_graph);
				
				pta.clearColours();
				synchronized (AbstractLearnerGraph.syncObj) {
					//PaperUAS.computePTASize(selectionID+" attempt: "+attempt+" with unique: ", pta, referenceGraph);
				}
				
				if (!onlyUsePositives)
				{
					assert pta.getStateNumber() > pta.getAcceptStateNumber() : "graph with only accept states but onlyUsePositives is not set";
				
				}
				else assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
				
				LearnerMarkovPassive learnerOfPairs = null;
				LearnerGraph actualAutomaton = null;
				
				if (pickUniqueFromInitial)
				{
					pta = mergeStatesForUnique(pta,uniqueFromInitial);
					learnerOfPairs = new LearnerMarkovPassive(learnerEval,referenceGraph,pta);learnerOfPairs.setMarkovModel(m);
					learnerOfPairs.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueFromInitial}));
					
					actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());

					LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					List<StatePair> pairsList = LearnerThatCanClassifyPairs.buildVerticesToMerge(actualAutomaton,learnerOfPairs.getLabelsLeadingToStatesToBeMerged(),learnerOfPairs.getLabelsLeadingFromStatesToBeMerged());
					if (!pairsList.isEmpty())
					{
						int score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
						if (score < 0)
						{
							learnerOfPairs = new LearnerMarkovPassive(learnerEval,referenceGraph,pta);learnerOfPairs.setMarkovModel(m);
							learnerOfPairs.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueFromInitial}));
							actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
							score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
							throw new RuntimeException("last merge in the learning process was not possible");
						}
						actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge);
					}
				}
				else
				{// not merging based on a unique transition from an initial state
					learnerOfPairs = new LearnerMarkovPassive(learnerEval,referenceGraph,pta);learnerOfPairs.setMarkovModel(m);
					synchronized (AbstractLearnerGraph.syncObj) {
						//PaperUAS.computePTASize(selectionID+" attempt: "+attempt+" no unique: ", pta, referenceGraph);
					}
					
					final int attemptAsFinal = attempt;
					
					TimerTask recordPta = new TimerTask() {
						
						@Override
						public void run() {
							synchronized (AbstractLearnerGraph.syncObj) {
								System.out.println("\nWARNING: "+new Date().toString()+" learner took an excessive amount of time to complete "+selectionID+"attempt="+attemptAsFinal+"; no unique");
							}
						}
					};
					timerToDetectLongRunningAutomata.schedule(recordPta,1000l*60l*20l);
					actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
					recordPta.cancel();
//					  final AbstractLearnerGraph graph_Learnt = actualAutomaton;
//			            final AbstractLearnerGraph graph1=referenceGraph;
//			        GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = 
//			            new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
//			        DirectedSparseGraph gr = gd.showGD(graph_Learnt,graph1,ExperimentRunner.getCpuNumber());
//			        Visualiser.updateFrame(gr, null);
				}
				
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
				SampleData dataSample = new SampleData(null,null);
//					config.setLearnerScoreMode(ScoreMode.CONVENTIONAL);
				dataSample.difference = estimateDifference(referenceGraph,actualAutomaton,testSet);
				LearnerGraph outcomeOfReferenceLearner = new ReferenceLearner(learnerEval,referenceGraph,pta).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
				dataSample.differenceForReferenceLearner = estimateDifference(referenceGraph, outcomeOfReferenceLearner,testSet);
				System.out.println("actual: "+actualAutomaton.getStateNumber()+" from reference learner: "+outcomeOfReferenceLearner.getStateNumber()+ " difference actual is "+dataSample.difference+ " difference ref is "+dataSample.differenceForReferenceLearner);
				outcome.samples.add(dataSample);
			}
			
			timerToDetectLongRunningAutomata.cancel();
			return outcome;
		}

		// Delegates to a specific estimator
		DifferenceToReference estimateDifference(LearnerGraph reference, LearnerGraph actual,@SuppressWarnings("unused") Collection<List<Label>> testSet)
		{
			return DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(reference, actual, config, 1);//estimationOfDifferenceFmeasure(reference, actual,testSet);
		}
	}
	

	/** An extension of {@Link PairScore} with Markov distance. */
	public static class PairScoreWithDistance extends PairScore
	{
		private double distance;
		
		public PairScoreWithDistance(PairScore p, double d) {
			super(p.getQ(), p.getR(), p.getScore(), p.getAnotherScore());distance = d;
		}
		
		double getDistanceScore()
		{
			return distance;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = super.hashCode();
			long temp;
			temp = Double.doubleToLongBits(distance);
			result = prime * result + (int) (temp ^ (temp >>> 32));
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (!super.equals(obj))
				return false;
			if (getClass() != obj.getClass())
				return false;
			PairScoreWithDistance other = (PairScoreWithDistance) obj;
			if (Double.doubleToLongBits(distance) != Double
					.doubleToLongBits(other.distance))
				return false;
			return true;
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
		MarkovUniversalLearner Markov;

		public void setPairQualityCounter(Map<Long,TrueFalseCounter> argCounter)
		{
			pairQuality = argCounter;
		}
		
		public void  setlengthMultiplier(int setlengthMultiplier)
		{
			lengthMultiplier = setlengthMultiplier;
		}
		
		public int  getlengthMultiplier()
		{
			return lengthMultiplier;
		}

		double threshold = 1.5;
		
		public void setThreshold(double value)
		{
			threshold = value;
		}
		
		public void set_States(int states) {
			num_states=	states;		
		}
		public MarkovUniversalLearner Markov() {
			return Markov;			
		}
			
		public void setMarkovModel(MarkovUniversalLearner m) {
			Markov=m;
			
		}

		public void set_traceQuantity(int traceQuantity) {
			numtraceQuantity=traceQuantity;			
		}
		
		public int get_States() {
			return num_states;		
		}
	
		public int get_traceQuantity() {
			return numtraceQuantity;			
		}
		
		public void set_seed(int i) {
			num_seed=i;
			
		}
		
		public int get_seed() {
			return num_seed;
			
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

		/** Where a pair has a zero score but Weka is not confident that this pair should not be merged, where this flag, such a pair will be assumed to be unmergeable. Where there is a clearly wrong pair
		 * detected by Weka, its blue state will be marked red, where no pairs are clearly appropriate for a merger and all of them have zero scores, this flag will cause a blue state in one of them to be marked red.  
		 */
		protected boolean blacklistZeroScoringPairs = false;
		
		
		public void setBlacklistZeroScoringPairs(boolean value)
		{
			blacklistZeroScoringPairs = value;
		}
		
		public LearnerMarkovPassive(LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, final LearnerGraph argInitialPTA) 
		{
			super(evalCnf,argReferenceGraph,argInitialPTA);
		}
		
		/** This method orders the supplied pairs in the order of best to merge to worst to merge. 
		 * We do not simply return the best pair because the next step is to check whether pairs we think are right are classified correctly.
		 * <p/> 
		 * Pairs are supposed to be the ones from {@link LearnerThatCanClassifyPairs#filterPairsBasedOnMandatoryMerge(Stack, LearnerGraph)} where all those not matching mandatory merge conditions are not included.
		 * Inclusion of such pairs will not affect the result but it would be pointless to consider such pairs.
		 * @param learnerGraph 
		 * @param l 
		 * @param m 
		 */
		protected ArrayList<PairScore> classifyPairs(Collection<PairScore> pairs, LearnerGraph tentativeGraph, Map<CmpVertex, Map<Label, UpdatablePairDouble>> l, LearnerGraph learnerGraph, MarkovUniversalLearner m)
		{
			boolean allPairsNegative = true;
			for(PairScore p:pairs)
			{
				assert p.getScore() >= 0;
				
				if (p.getQ().isAccept() || p.getR().isAccept()) // if any are rejects, add with a score of zero, these will always work because accept-reject pairs will not get here and all rejects can be merged.
				{
					allPairsNegative = false;break;
				}
			}
			ArrayList<PairScore> possibleResults = new ArrayList<PairScore>(pairs.size()),nonNegPairs = new ArrayList<PairScore>(pairs.size());
			if (allPairsNegative)
				possibleResults.addAll(pairs);
			else
			{
				for(PairScore p:pairs)
				{
					assert p.getScore() >= 0;
					if (!p.getQ().isAccept() || !p.getR().isAccept()) // if any are rejects, add with a score of zero, these will always work because accept-reject pairs will not get here and all rejects can be merged.
						possibleResults.add(new PairScoreWithDistance(p,0));
					else
						nonNegPairs.add(p);// meaningful pairs, will check with the classifier
				}
				
				for(PairScore p:nonNegPairs)
				{
					try
					{						
						double d=m.computeMMScoreImproved(p, tentativeGraph, l, learnerGraph);
						if(p.getScore() > -1 && d > 0.0)
							possibleResults.add(new PairScoreWithDistance(p, d));						
					}
					catch(Exception ex)
					{
						ex.printStackTrace();
						throw new IllegalArgumentException("failed to classify pair "+p, ex);
					}
				}
			
					
				Collections.sort(possibleResults, new Comparator<PairScore>(){
	
					@Override
					public int compare(PairScore o1, PairScore o2) {
						int outcome = sgn( ((PairScoreWithDistance)o2).getDistanceScore() - ((PairScoreWithDistance)o1).getDistanceScore());  
						if (outcome != 0)
							return outcome;
						outcome = sgn( (long) (o2.getAnotherScore() - o1.getAnotherScore()));// scores are between 100 and 0, hence it is appropriate to cast to int without a risk of overflow.
						if (outcome != 0)
							return outcome;
						return o2.compareTo(o1);
					}}); 			
			}				
			return possibleResults;
		}
		
	
		protected  double howbadPairs(PairScore P, LearnerGraph tentativeGraph, Map<CmpVertex, Map<Label, Double>> l, LearnerGraph learnerGraph, MarkovUniversalLearner m)
		{
			double badestimator = computeBadPairsEstimation(P, tentativeGraph, l, learnerGraph, m);
//			P.Set_distance_score(badestimator);
			return badestimator;		
		}
		protected double computeBadPairsEstimation(PairScore p, LearnerGraph tentativeGraph, Map<CmpVertex, Map<Label, Double>> l, LearnerGraph learnerGraph, MarkovUniversalLearner m) 
		{
			double bad=0.0;
			for(Label L:tentativeGraph.getCache().getAlphabet())
			{
				double pF = 0,pS=0;
				if(l.get(p.firstElem).get(L)==null)
					 pF=0.0;
				else
					pF= l.get(p.firstElem).get(L);

				if(l.get(p.secondElem).get(L)==null)
					pS=0.0;
				else
					pS= l.get(p.secondElem).get(L);
				bad+=Math.min(pF,pS);
			}
			return bad;
		}

		/** Where there does not seem to be anything useful to merge, return the pair clearly incorrectly labelled. */
		protected PairScore getPairToBeLabelledRed(Collection<PairScore> pairs, LearnerGraph tentativeGraph, Map<CmpVertex, Map<Label, UpdatablePairDouble>> l, LearnerGraph learnerGraph, MarkovUniversalLearner m)
		{
			for(PairScore p:pairs)
			{
				assert p.getScore() >= 0;
				
				if (!p.getQ().isAccept() || !p.getR().isAccept()) // if any are rejects, add with a score of zero, these will always work because accept-reject pairs will not get here and all rejects can be merged.
					return null;// negatives can always be merged.
			}
			
			// if we are here, none of the pairs are clear candidates for mergers.

			PairScoreWithDistance pairBestToReturnAsRed = null;
			LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			List<StatePair> pairsList = buildVerticesToMerge(tentativeGraph,labelsLeadingToStatesToBeMerged,labelsLeadingFromStatesToBeMerged);
			for(PairScore p:pairs)
			{
				if (!pairsList.isEmpty() && tentativeGraph.pairscores.computePairCompatibilityScore_general(p, pairsList, verticesToMerge) < 0)
				// This pair cannot be merged, return as red. Note that this computation is deferred to this stage from computePairScores in order to consider 
				// only a small subset of pairs that are apparently compatible but will be incompatible once the mandatory merge conditions are taken into account.
					return p;
				
				// potentially meaningful pair, check with the classifier
				
				try
				{					
					double d=m.computeMMScoreImproved(p, learnerGraph, l,learnerGraph);
//					double d=howbadPairs(p, tentativeGraph, l, learnerGraph, m);
					if (pairBestToReturnAsRed == null || d < pairBestToReturnAsRed.getDistanceScore())
						pairBestToReturnAsRed = new PairScoreWithDistance(p,d);// this is the pair to return.					
				}
				catch(Exception ex)
				{
					ex.printStackTrace();
					throw new IllegalArgumentException("failed to classify pair "+p, ex);
				}
			}

			return pairBestToReturnAsRed;
		}
		
		CollectionOfPairsEstimator redStateEstimator = null;//new QualityEstimatorUsingWeka();
		
		public  ArrayList<PairScoreWithDistance> SplitSetOfPairsIntoRight(LearnerGraph graph, LearnerGraph exlearnerGraph, Collection<PairScore> pairs, Map<CmpVertex, Map<Label, UpdatablePairDouble>> l)
		{						
			ArrayList<PairScoreWithDistance>  correctPairs= new ArrayList<PairScoreWithDistance>();
			for(PairScore p:pairs)
			{		
				if(p.firstElem.isAccept()==false && p.secondElem.isAccept()==false)
				{
					double d=Markov.computeMMScoreImproved(p, graph, l, exlearnerGraph);
		            correctPairs.add(new PairScoreWithDistance(p, d));
				}
				else if(p.firstElem.isAccept()==true && p.secondElem.isAccept()==true)
				{
					double d=Markov.computeMMScoreImproved(p, graph, l, exlearnerGraph);
					if(d != MarkovUniversalLearner.REJECT && p.getScore()> -1)
					{
						correctPairs.add(new PairScoreWithDistance(p, d));
					}	
					
				}
			}

			return correctPairs;
		}
		
		public ArrayList<PairScoreWithDistance> SplitSetOfPairsIntoWrong(LearnerGraph graph, LearnerGraph exlearnerGraph, Collection<PairScore> pairs, Map<CmpVertex, Map<Label, UpdatablePairDouble>> l)
		{						
			ArrayList<PairScoreWithDistance>  WrongPairs= new ArrayList<PairScoreWithDistance>();
			for(PairScore p:pairs)
			{	
				if(p.firstElem.isAccept()==true && p.secondElem.isAccept()==true)
				{
					double d=Markov.computeMMScoreImproved(p, graph, l, exlearnerGraph);
					if(d == MarkovUniversalLearner.REJECT)
					{
						WrongPairs.add(new PairScoreWithDistance(p, d));
				 	}
					

				}
			}
			
			Collections.sort(WrongPairs, new Comparator<PairScoreWithDistance>(){
				
				@Override
				public int compare(PairScoreWithDistance o1,
						PairScoreWithDistance o2) {
					int outcome = sgn( (long) (o2.getDistanceScore() - o1.getDistanceScore()));// scores are between 100 and 0, hence it is appropriate to cast to int without a risk of overflow.
					if (outcome != 0)
						return outcome;
					return o2.compareTo(o1);
				}
			});
			
			Collections.reverse(WrongPairs);
			return WrongPairs;
		}
		
		@Override 
		public Stack<PairScore> ChooseStatePairs(final LearnerGraph graph)
		{
			Stack<PairScore> outcome = graph.pairscores.chooseStatePairs(new PairScoreComputation.RedNodeSelectionProcedure()
			{

				// Here I could use a learner based on metrics of both tentative reds and the perceived quality of the red-blue pairs obtained if I choose any given value.
				// This can be accomplished by doing a clone of the graph and running chooseStatePairs on it with decision procedure that 
				// (a) applies the same rule (of so many) to choose pairs and
				// (b) checks that deadends are flagged. I could iterate this process for a number of decision rules, looking locally for the one that gives best quality of pairs
				// for a particular pairscore decision procedure.
				@Override
				public CmpVertex selectRedNode(LearnerGraph coregraph, @SuppressWarnings("unused") Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
				{
					return LearnerThatUsesWekaResults.selectRedNodeUsingNumberOfNewRedStates(coregraph, tentativeRedNodes, redStateEstimator);
//					return tentativeRedNodes.iterator().next();
				}

				@Override
				public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, @SuppressWarnings("unused") Collection<CmpVertex> reds, List<PairScore> pairs) 
				{
					Map<CmpVertex, Map<Label, UpdatablePairDouble>> l =Markov.Markov_tentative(coregraph, Markov.get_Markov_model(), Markov.get_Markov_model_occurence());
					CmpVertex stateToMarkRed = null;
					ArrayList<PairScoreWithDistance> correctPairs = SplitSetOfPairsIntoRight(coregraph,Markov.get_extension_model(), pairs,l);
					List<PairScoreWithDistance> wrongPairs =SplitSetOfPairsIntoWrong(coregraph,Markov.get_extension_model(), pairs,l);
					if (correctPairs.isEmpty() && !wrongPairs.isEmpty())
					{				
//						List<PairScore> filterbad = pickPairToRed(wrongPairs);// no correct pairs found to merge, return the first wrong pair so the corresponding state is marked as red.	
//						stateToMarkRed=classify_bad(filterbad).getQ();
						stateToMarkRed =wrongPairs.get(0).getQ();
					}
					else
						return null;
					
					return stateToMarkRed;
				}
/*
				private StatePair classify_bad(List<PairScore> filterbad)
				{
					Map <CmpVertex,Integer> reds=new TreeMap<CmpVertex,Integer>();
					Map <CmpVertex,Integer> blues=new TreeMap<CmpVertex,Integer>();

					for(PairScore P:filterbad)
					{
						if(!blues.containsKey(P.getQ()))
							blues.put(P.getQ(), 1);
						else
						{
							blues.put(P.getQ(), blues.get(P.getQ())+1);
						}
						
						if(!reds.containsKey(P.getR()))
							reds.put(P.getR(), 1);
						else
						{
							reds.put(P.getR(), reds.get(P.getR())+1);
						}						
					}
					for(PairScore P:filterbad)
					{
						if(blues.get(P.getQ())==1 && reds.get(P.getR())==1)
						{
					         return P;
						}
					}					
					return filterbad.get(0);
				}
				*/
			});
			
			if (!outcome.isEmpty())
			{
				PairScore result = null;
				result=classifyingByMarkovScore(outcome, graph);
				assert result!=null;
				assert result.getScore()!=-1;
				outcome.clear();outcome.push(result);
			}	
			return outcome;

		}
		
		public PairScore classifyingByMarkovScore(Stack<PairScore> outcome, LearnerGraph graph)
		{
			Map<CmpVertex, Map<Label, UpdatablePairDouble>> l = Markov.Markov_tentative(graph, Markov.get_Markov_model(), Markov.get_Markov_model_occurence());
			PairScore result = null;
			List<PairScore> filteredPairs = filterPairsBasedOnMandatoryMerge(outcome,graph);
			ArrayList<PairScore> possibleResults = new ArrayList<PairScore>();
			possibleResults = classifyPairs(filteredPairs,graph,l,Markov.get_extension_model(),Markov);
 			if(!possibleResults.isEmpty())
			{					
				Stack<PairScore> topofstack1 = MarkovScoreComputation.getChoicesother_Score(possibleResults);

				assert topofstack1!=null;
				if(topofstack1.size() == 1)
					result=topofstack1.pop();
				else
				{
					possibleResults.add(pickPairQSMLike(topofstack1));// no pairs have been provided by the modified algorithm, hence using the default one.
					result = possibleResults.iterator().next();
				}
			}
 			if(result==null)
 			{
 				possibleResults.add(pickPairQSMLike(outcome));// no pairs have been provided by the modified algorithm, hence using the default one.
 				result = possibleResults.iterator().next();
 			}
 			assert result != null;
			return result;
		}
		
		
		public static PairScore pickPairDISLike(Collection<PairScoreWithDistance> pairs)
		{
			assert pairs != null;
			PairScoreWithDistance bestPair=null;
			for(PairScoreWithDistance P:pairs)
			{
				if(bestPair == null || P.getAnotherScore() > bestPair.getAnotherScore())
					bestPair=P;
				else if(P.getAnotherScore() == bestPair.getAnotherScore())
				{
					if(P.getDistanceScore() > bestPair.getDistanceScore())
						bestPair=P;	
                    
					else if(Math.abs(P.getQ().getDepth()-P.getR().getDepth()) < Math.abs(bestPair.getQ().getDepth()-bestPair.getR().getDepth()))
							bestPair=P;	
				}
			}
			return bestPair;
		}
		
		public List<PairScore> pickPairToRed(Collection<PairScore> pairs)
		{
			assert pairs != null;
			List<PairScore> bad =new ArrayList<PairScore>();
			PairScore badPair=null;
			for(PairScore P:pairs)
			{
				if(badPair == null)
					badPair=P;
				else if(P.getScore() < badPair.getScore())
					badPair=P;
			}
			bad.add(badPair);

			for(PairScore P:pairs)
			{
				if(badPair.getScore()==P.getScore() && !bad.contains(P))
				bad.add(P);
			}
			return bad;
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
		config.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);
		ConvertALabel converter = new Transform.InternStringLabel();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int ThreadNumber = ExperimentRunner.getCpuNumber();	
		ExecutorService executorService = Executors.newFixedThreadPool(ThreadNumber);
		final int minStateNumber = 15;
		final int samplesPerFSM = 10;
		final int rangeOfStateNumbers = 4;
		final int stateNumberIncrement = 4;
		final int trainingDataMultiplier = 10;
		// Stores tasks to complete.
		CompletionService<ThreadResult> runner = new ExecutorCompletionService<ThreadResult>(executorService);
		for(final int lengthMultiplier:new int[]{10})
		for(final boolean onlyPositives:new boolean[]{true})
			{
				final int traceQuantity=8;
				for(final boolean useUnique:new boolean[]{false})
				{
					String selection;
					for(final boolean selectingRed:new boolean[]{false})
					for(final boolean classifierToBlockAllMergers:new boolean[]{false})
					for(final double threshold:new double[]{1.0})
					{
						final boolean zeroScoringAsRed = false;
						selection = "TRUNK;EVALUATION;"+";threshold="+threshold+
								";onlyPositives="+onlyPositives+";selectingRed="+selectingRed+";useUnique="+useUnique+";classifierToBlockAllMergers="+classifierToBlockAllMergers+";zeroScoringAsRed="+zeroScoringAsRed+";lengthMultiplier="+lengthMultiplier+";trainingDataMultiplier="+trainingDataMultiplier+";";

						final int totalTaskNumber = traceQuantity;
						final RBoxPlot<Long> gr_PairQuality = new RBoxPlot<Long>("Correct v.s. wrong","%%",new File("percentage_score"+selection.substring(0, 80)+".png"));
						final RBoxPlot<String> gr_QualityForNumberOfTraces = new RBoxPlot<String>("traces","%%",new File("quality_traces"+selection.substring(0, 80)+".png"));
						SquareBagPlot gr_NewToOrig = new SquareBagPlot("orig score","score with learnt selection",new File("new_to_orig"+selection.substring(0, 80)+".png"),0,1,true);
						final Map<Long,TrueFalseCounter> pairQualityCounter = new TreeMap<Long,TrueFalseCounter>();
						try
						{
							int numberOfTasks = 0;
							for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
								for(int sample=0;sample<samplesPerFSM;++sample)
								{
									LearnerRunner learnerRunner = new LearnerRunner(states,sample,totalTaskNumber+numberOfTasks,traceQuantity, config, converter);
									learnerRunner.setPickUniqueFromInitial(useUnique);
									learnerRunner.setOnlyUsePositives(onlyPositives);learnerRunner.setLengthMultiplier(lengthMultiplier);
									learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
									runner.submit(learnerRunner);
									++numberOfTasks;
								}
							ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+numberOfTasks+" tasks for "+selection, numberOfTasks);
							for(int count=0;count < numberOfTasks;++count)
							{
								ThreadResult result = runner.take().get();// this will throw an exception if any of the tasks failed.
								if (gr_NewToOrig != null)
								{
									for(SampleData sample:result.samples)
										gr_NewToOrig.add(sample.differenceForReferenceLearner.getValue(),sample.difference.getValue());
								}
								
								for(SampleData sample:result.samples)
									if (sample.differenceForReferenceLearner.getValue() > 0)
										gr_QualityForNumberOfTraces.add(traceQuantity+"",sample.difference.getValue()/sample.differenceForReferenceLearner.getValue());
								progress.next();
							}
							if (gr_PairQuality != null)
							{
								synchronized(pairQualityCounter)
								{
									updateGraph(gr_PairQuality,pairQualityCounter);
									//gr_PairQuality.drawInteractive(gr);
									//gr_NewToOrig.drawInteractive(gr);
									//if (gr_QualityForNumberOfTraces.size() > 0)
									//	gr_QualityForNumberOfTraces.drawInteractive(gr);
								}
							}
							if (gr_PairQuality != null) gr_PairQuality.drawPdf(gr);
						}
						catch(Exception ex)
						{
							IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
							if (executorService != null) { executorService.shutdownNow();executorService = null; }
							throw e;
						}
						if (gr_NewToOrig != null) gr_NewToOrig.drawPdf(gr);
						if (gr_QualityForNumberOfTraces != null) gr_QualityForNumberOfTraces.drawPdf(gr);
					}
				}
			}
		if (executorService != null) { executorService.shutdown();executorService = null; }
		
	}
}