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
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import soot.jimple.toolkits.scalar.pre.EarliestnessComputation;
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
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.collections.ArrayMapWithSearch;
import statechum.collections.HashMapWithSearch;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

public class Cav2014 extends PairQualityLearner
{

	/** PTA is supposed to be built using walks over a reference graph. If these are random walks, it is possible that some transitions will not be covered. 
	 * For the learning purposes, this is significant because this could make some states more easily identifiable.
	 *  
	 * @param pta walks through the reference graph
	 * @param reference graph to trim 
	 * @return trimmed copy of the reference graph.
	 */
	public static Map<CmpVertex,Set<Label>> identifyUncoveredTransitions(LearnerGraph pta,LearnerGraph reference)
	{
		Map<CmpVertex,Set<Label>> outcome = new TreeMap<CmpVertex,Set<Label>>();
		StatePair reference_pta = new StatePair(reference.getInit(),pta.getInit());
		LinkedList<StatePair> pairsToExplore = new LinkedList<StatePair>();pairsToExplore.add(reference_pta);
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:reference.transitionMatrix.entrySet())
			outcome.put(entry.getKey(), new TreeSet<Label>(entry.getValue().keySet()));
		Set<CmpVertex> visitedInTree = new HashSet<CmpVertex>();
		while(!pairsToExplore.isEmpty())
		{
			reference_pta = pairsToExplore.pop();
			Map<Label,CmpVertex> transitions=pta.transitionMatrix.get(reference_pta.secondElem);
			outcome.get(reference_pta.firstElem).removeAll(transitions.keySet());
			for(Entry<Label,CmpVertex> target:transitions.entrySet())
				if (target.getValue().isAccept())
				{
					if (visitedInTree.contains(target.getValue()))
						throw new IllegalArgumentException("PTA is not a tree");
					visitedInTree.add(target.getValue());
					CmpVertex nextGraphState = reference.transitionMatrix.get(reference_pta.firstElem).get(target.getKey());
					if (nextGraphState == null)
						throw new IllegalArgumentException("coverage has more transitions than the original graph");
					pairsToExplore.add(new StatePair(nextGraphState, target.getValue()));
				}
		}
		
		return outcome;
	}
	
	/** Takes a supplied automaton and removes all transitions that have not been covered by a supplied PTA.
	 * 
	 * @param pta contains covered transitions
	 * @param reference all of the transitions.
	 * @return trimmed reference graph
	 */
	public static LearnerGraph trimUncoveredTransitions(LearnerGraph pta,LearnerGraph reference)
	{
		Configuration shallowCopy = reference.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph outcome = new LearnerGraph(shallowCopy);AbstractLearnerGraph.copyGraphs(reference, outcome);

		for(Entry<CmpVertex,Set<Label>> entry:identifyUncoveredTransitions(pta, reference).entrySet())
		{
			Map<Label,CmpVertex> map = outcome.transitionMatrix.get(entry.getKey());
			for(Label lbl:entry.getValue()) map.remove(lbl);
		}
		
		return outcome;
	}
		
	public void constructMapFromLabelsToStateGroups(LearnerGraph tentativeGraph, Collection<Label> transitionsFromTheSameState)
	{
		Map<Label,Collection<CmpVertex>> labelToStates = 
				tentativeGraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY? new ArrayMapWithSearch<Label,Collection<CmpVertex>>() : new TreeMap<Label,Collection<CmpVertex>>();
		Map<Label,Collection<CmpVertex>> labelFromStates = 
				tentativeGraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY? new ArrayMapWithSearch<Label,Collection<CmpVertex>>() : new TreeMap<Label,Collection<CmpVertex>>();
					
		for(Label lbl:transitionsFromTheSameState) labelFromStates.put(lbl,new ArrayList<CmpVertex>());
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:tentativeGraph.transitionMatrix.entrySet())
			if (entry.getKey().isAccept())
				for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
				{
					Collection<CmpVertex> statesToMerge = labelToStates.get(transition.getKey());
					if (statesToMerge != null && transition.getValue().isAccept()) statesToMerge.add(transition.getValue());

					Collection<CmpVertex> sourceStatesToMerge = labelFromStates.get(transition.getKey());
					if (sourceStatesToMerge != null && transition.getValue().isAccept()) sourceStatesToMerge.add(entry.getKey());
				}
		
	}
	
	/** Given a graph and a collection of sequences, extracts a set of states from the graph where each state is uniquely identified by one of the sequences
	 * 
	 * @param referenceGraph graph of interest
	 * @param collectionOfUniqueSeq sequences to check from all states of this graph
	 * @param correctlyIdentified collection of the vertices of the graph that are uniquely identified using the supplied sequences.
	 * @param incorrectSequences a subset of sequences passed in <i>collectionOfUniqueSeq</i> that do not identify states uniquely.
	 */
	protected static void statesIdentifiedUsingUniques(LearnerGraph referenceGraph, Collection<List<Label>> collectionOfUniqueSeq, Set<CmpVertex> correctlyIdentified,Collection<List<Label>> incorrectSequences)
	{
		for(List<Label> seq:collectionOfUniqueSeq)
		{
			int count=0;CmpVertex unique = null;
			for(CmpVertex v:referenceGraph.transitionMatrix.keySet())
			{
				if (referenceGraph.getVertex(v,seq) != null)
				{
					++count;unique=v;
					if (count > 1)
						break;
				}
			}
			if (count == 1)
			{
				if (correctlyIdentified != null) correctlyIdentified.add(unique);
			}
			else if (count > 1)
			{
				if (incorrectSequences != null) incorrectSequences.add(seq);
			}
		}
	}
	
	/** Given a graph and a collection of sequences, extracts a set of states from the graph where each state accepts one of the supplied sequences. Where multiple states accept one of the sequences,
	 * all of those states are returned. <b>There is no expectation of uniqueness</b>.
	 * 
	 * @param referenceGraph graph of interest
	 * @param collectionOfSeq sequences to check from all states of this graph
	 * @return collection of the vertices of the graph that are uniquely identified using the supplied sequences.
	 */
	protected static Collection<CmpVertex> statesIdentifiedUsingSequences(LearnerGraph referenceGraph, Collection<List<Label>> collectionOfSeq)
	{
		Set<CmpVertex> statesUniquelyIdentified = new TreeSet<CmpVertex>();
		for(List<Label> seq:collectionOfSeq)
		{
			for(CmpVertex v:referenceGraph.transitionMatrix.keySet())
				if (referenceGraph.getVertex(v,seq) != null)
					statesUniquelyIdentified.add(v);
		}
		return statesUniquelyIdentified;
	}
	
	public static List<StatePair> getVerticesToMergeFor(LearnerGraph graph,List<List<List<Label>>> pathsToMerge)
	{
		List<StatePair> listOfPairs = new LinkedList<StatePair>();
		for(List<List<Label>> lotOfPaths:pathsToMerge)
		{
			CmpVertex firstVertex = graph.getVertex(lotOfPaths.get(0));
			for(List<Label> seq:lotOfPaths)
				listOfPairs.add(new StatePair(firstVertex,graph.getVertex(seq)));
		}
		return listOfPairs;
	}
	
	public static Collection<StatePair> constructPairsToMergeBasedOnSetsToMerge(Set<CmpVertex> validStates, Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA)
	{
		List<StatePair> pairsList = new LinkedList<StatePair>();
		for(Set<CmpVertex> groupOfStates:verticesToMergeBasedOnInitialPTA)
		{
			Set<CmpVertex> validStatesInGroup = new TreeSet<CmpVertex>();validStatesInGroup.addAll(groupOfStates);validStatesInGroup.retainAll(validStates);
			if (validStatesInGroup.size() > 1)
			{
				CmpVertex v0=validStatesInGroup.iterator().next();
				for(CmpVertex v:validStatesInGroup)
				{
					if (v != v0)
						pairsList.add(new StatePair(v0,v));
					v0=v;
				}
			}
		}
		return pairsList;
	}

	public static class LearnerRunner implements Callable<ThreadResult>
	{
		protected final Configuration config;
		protected final ConvertALabel converter;
		protected final int states,sample;
		protected boolean onlyUsePositives;
		protected final int seed;
		protected int chunkLen=3;
		protected final int traceQuantity;
		protected String selectionID;
		protected double alphabetMultiplier = 2.;
		
		public void setSelectionID(String value)
		{
			selectionID = value;
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
			final int alphabet = (int)alphabetMultiplier*states;
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
							return states*alphabet;
						}
		
						@Override
						public int getPrefixLength(int len) {
							return len;
						}
					});

				if (onlyUsePositives)
					pta.paths.augmentPTA(generator.getAllSequences(0).filter(new FilterPredicate() {
						@Override
						public boolean shouldBeReturned(Object name) {
							return ((statechum.analysis.learning.rpnicore.RandomPathGenerator.StateName)name).accept;
						}
					}));
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
				LearnerGraph trimmedReference = trimUncoveredTransitions(pta,referenceGraph);
				
				final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklisting();
				long inconsistencyForTheReferenceGraph = MarkovClassifier.computeInconsistency(trimmedReference, m, checker,false);
				System.out.println("Inconsistency of trimmed reference : "+inconsistencyForTheReferenceGraph);
				
				MarkovClassifier ptaClassifier = new MarkovClassifier(m,pta);
				final List<List<Label>> pathsToMerge=ptaClassifier.identifyPathsToMerge(checker);
				
				final Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA=ptaClassifier.buildVerticesToMergeForPaths(pathsToMerge);

				{
					LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					Collection<StatePair> pairsToMergeBasedOnInitial = constructPairsToMergeBasedOnSetsToMerge(pta.transitionMatrix.keySet(),verticesToMergeBasedOnInitialPTA);
					int genScore = pta.pairscores.computePairCompatibilityScore_general(null, pairsToMergeBasedOnInitial, verticesToMerge);
					assert genScore >= 0;
					LearnerGraph merged = MergeStates.mergeCollectionOfVertices(pta, null, verticesToMerge);
					long value = MarkovClassifier.computeInconsistency(merged, m, checker,false);
					statechum.Pair<Double,Double> statesIdentification = MarkovClassifier.calculatePercentageOfIdentifiedStates(referenceGraph,pathsToMerge);
					System.out.println(value+" inconsistencies, "+merged.getStateNumber()+" states, originally "+pta.getStateNumber()+ " "+(MarkovClassifier.checkMergeValidity(referenceGraph,pta,m,pathsToMerge)?"VALID":"INVALID")+" states identification: "+statesIdentification);
				}
				
				
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
					/** Where I have a set of paths to merge because I have identified specific states, this map is constructed that maps vertices to be merged together to the partition number that corresponds to them. */
					Map<CmpVertex,Integer> vertexToPartition = new TreeMap<CmpVertex,Integer>();
					
					@Override
					public void initComputation(LearnerGraph graph) 
					{
						coregraph = graph;

						cl = new MarkovClassifier(m, coregraph);
					    extendedGraph = cl.constructMarkovTentative();
						vertexToPartition.clear();
						int partitionNumber=0;
						for(Set<CmpVertex> set:verticesToMergeBasedOnInitialPTA)
						{
							for(CmpVertex v:set) vertexToPartition.put(v, partitionNumber);
							++partitionNumber;
						}
					}
					
					@Override
					public long overrideScoreComputation(PairScore p) 
					{

						Integer a=vertexToPartition.get(p.getR()), b = vertexToPartition.get(p.getQ());
						long pairScore = p.getScore();
						
						if (pairScore >= 0)
						{			
							if ((a == null || b == null) || (a != b))
								pairScore = (int) MarkovScoreComputation.computenewscore(p, extendedGraph);
							else
								pairScore = Integer.MAX_VALUE;
							/*
							Collection<StatePair> pairsToMergeBasedOnInitial = null;//constructPairsToMergeBasedOnSetsToMerge(coregraph.transitionMatrix.keySet(),verticesToMergeBasedOnInitialPTA);
							long scoreAfterFullMerge = coregraph.pairscores.computePairCompatibilityScore_general(p, pairsToMergeBasedOnInitial, verticesToMerge);
							LearnerGraph merged = MergeStates.mergeCollectionOfVertices(coregraph, null, verticesToMerge);
							long currentInconsistency = MarkovClassifier.computeInconsistency(merged, m, checker,
									false
									)-inconsistencyFromAnEarlierIteration;
							if (a == null || b == null || a != b)
								scoreUsingInconsistencies -= currentInconsistency;
							scoreToReport = markovOutcome != 0?markovOutcome:scoreUsingInconsistencies;*/
							/*
							ArrayList<PairScore> pairOfInterest = new ArrayList<PairScore>(1);pairOfInterest.add(p);
							List<PairScore> correctPairs = new ArrayList<PairScore>(1), wrongPairs = new ArrayList<PairScore>(1);
							SplitSetOfPairsIntoRightAndWrong(coregraph, finalReference, pairOfInterest, correctPairs, wrongPairs);
							if (scoreToReport < 0 && wrongPairs.isEmpty() || scoreToReport >= 0 && correctPairs.isEmpty())
							{
							//if (scoreUsingInconsistencies < 0 && wrongPairs.isEmpty() || scoreUsingInconsistencies >= 0 && correctPairs.isEmpty())
								System.out.println(p+" score: "+pairScore+" markov: "+markovOutcome+" using inconsistencies: "+scoreUsingInconsistencies+", current inconsistency: "+currentInconsistency+ " (earlier: "+inconsistencyFromAnEarlierIteration+")" +
									") Sicco: "+MarkovScoreComputation.computeScoreSicco(coregraph, p)+" "+
										" valid: "+wrongPairs.isEmpty());
							}*/
						}
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
				{
					LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					int genScore = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, constructPairsToMergeBasedOnSetsToMerge(actualAutomaton.transitionMatrix.keySet(),verticesToMergeBasedOnInitialPTA), verticesToMerge);
					assert genScore >= 0;
					actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge);
				}

				SampleData dataSample = new SampleData(null,null);
				dataSample.inconsistencyActual = MarkovClassifier.computeInconsistency(actualAutomaton, m, checker,false);
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
				dataSample.difference = estimateDifference(referenceGraph,actualAutomaton,testSet);
				LearnerGraph outcomeOfReferenceLearner = new ReferenceLearner(learnerEval,referenceGraph,ptaCopy).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
				dataSample.differenceForReferenceLearner = estimateDifference(referenceGraph, outcomeOfReferenceLearner,testSet);
				dataSample.inconsistencyReference = MarkovClassifier.computeInconsistency(outcomeOfReferenceLearner, m, checker,false);
				System.out.println("actual: "+actualAutomaton.getStateNumber()+" from reference learner: "+outcomeOfReferenceLearner.getStateNumber()+ " difference actual is "+dataSample.difference+ " difference ref is "+dataSample.differenceForReferenceLearner);
				outcome.samples.add(dataSample);
			}
			
			return outcome;
		}

		// Delegates to a specific estimator
		DifferenceToReference estimateDifference(LearnerGraph reference, LearnerGraph actual,@SuppressWarnings("unused") Collection<List<Label>> testSet)
		{
//			return DifferenceToReferenceLanguageBCR.estimationOfDifference(reference, actual, testSet);
			return DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(reference, actual, config, 1);//estimationOfDifferenceFmeasure(reference, actual,testSet);
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
		config.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);
		ConvertALabel converter = new Transform.InternStringLabel();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int ThreadNumber = ExperimentRunner.getCpuNumber();	
		ExecutorService executorService = Executors.newFixedThreadPool(ThreadNumber);
		final int minStateNumber = 50;
		final int samplesPerFSM = 5;
		final int rangeOfStateNumbers = 5;
		final int stateNumberIncrement = 5;
		final double alphabetMultiplierMax = 2;
		final int traceQuantity=5;
		
		// Stores tasks to complete.
		CompletionService<ThreadResult> runner = new ExecutorCompletionService<ThreadResult>(executorService);
		for(final boolean onlyPositives:new boolean[]{true})
			for(final double alphabetMultiplier:new double[]{alphabetMultiplierMax}) 
			{
					String selection;
						selection = "TRUNK;EVALUATION;"+
								";onlyPositives="+onlyPositives+";";

						final int totalTaskNumber = traceQuantity;
						final RBoxPlot<Long> gr_PairQuality = new RBoxPlot<Long>("Correct v.s. wrong","%%",new File("percentage_score.pdf"));
						final RBoxPlot<String> gr_QualityForNumberOfTraces = new RBoxPlot<String>("traces","%%",new File("quality_traces.pdf"));
						SquareBagPlot gr_NewToOrig = new SquareBagPlot("Original Score","Score with Markov Learner",new File("new_to_orig.pdf"),0,1,true);
						final Map<Long,TrueFalseCounter> pairQualityCounter = new TreeMap<Long,TrueFalseCounter>();
						try
						{
							int numberOfTasks = 0;
							for(int states=minStateNumber;states < minStateNumber+rangeOfStateNumbers;states+=stateNumberIncrement)
								for(int sample=0;sample<samplesPerFSM;++sample)
								{
									LearnerRunner learnerRunner = new LearnerRunner(states,sample,totalTaskNumber+numberOfTasks,traceQuantity, config, converter);
									learnerRunner.setOnlyUsePositives(onlyPositives);
									learnerRunner.setAlphabetMultiplier(alphabetMultiplier);
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
		if (executorService != null) { executorService.shutdown();executorService = null; }
	

	}
}