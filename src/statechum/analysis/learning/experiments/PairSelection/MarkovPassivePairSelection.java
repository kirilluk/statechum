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

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.JUConstants;
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
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ReferenceLearnerUsingSiccoScoring;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.TrueFalseCounter;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.collections.ArrayMapWithSearchPos;
import statechum.collections.MapWithSearch;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

public class MarkovPassivePairSelection extends PairQualityLearner
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
		for(Entry<CmpVertex, MapWithSearch<Label,Label,CmpVertex>> entry:reference.transitionMatrix.entrySet())
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
	
	/* This is an attempt to make different predictions about the machine of interest that did not get as far as expected. 
	public static LearnerGraph buildFirstOrderMarkovGraph(MarkovClassifier ptaClassifier, LearnerGraph referenceGraph)
	{
		LearnerGraph outcome = new LearnerGraph(ptaClassifier.graph.config);
		if (ptaClassifier.model.getChunkLen() < 2)
			throw new IllegalArgumentException("not enough data for a first-order Markov model");
		
		LearnerGraph trimmedReference = trimUncoveredTransitions(ptaClassifier.graph,referenceGraph);
		
		ptaClassifier.updateMarkov(false);
	
		Map<Label,CmpVertex> states = new TreeMap<Label,CmpVertex>();
		for(Label l:ptaClassifier.graph.getCache().getAlphabet())
		{
			CmpVertex state = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(l.toString()), ptaClassifier.graph.config);states.put(l,state);
			outcome.transitionMatrix.put(state, outcome.createNewRow());
		}
		for(Entry<Label,CmpVertex> state:states.entrySet())
		{
			for(Label label:ptaClassifier.graph.getCache().getAlphabet())
			{
				MarkovOutcome transition = ptaClassifier.model.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{state.getKey(),label}),true));
				if (transition != null)
					if (transition == MarkovOutcome.positive) outcome.transitionMatrix.get(state.getValue()).put(label,states.get(label));
			}
		}		

		List<List<Label>> uniqueSequences = new LinkedList<List<Label>>();
		for(Label l1:ptaClassifier.graph.getCache().getAlphabet())
		{
			boolean nonUnique = false;
			Label unique = null;
			for(Label lbl:ptaClassifier.graph.getCache().getAlphabet())
			{
				if (ptaClassifier.model.predictionsMatrix.containsKey(new Trace(Arrays.asList(new Label[]{l1,lbl}),true)))
				{
					if (unique == null)
						unique = lbl;
					else
					{
						nonUnique = true;break;
					}
				}
			}
			if (unique != null && !nonUnique)
				uniqueSequences.add(Arrays.asList(new Label[]{l1,unique}));
			
		}
		
		List<List<Label>> sequencesUnique2=new LinkedList<List<Label>>();
		for(List<Label> prefix:uniqueSequences)
			{
				boolean nonUnique = false;
				List<Label> unique = null;
				for(Label lbl:ptaClassifier.graph.getCache().getAlphabet())
				{
					List<Label> seq = new LinkedList<Label>(prefix);seq.add(lbl);
					if (ptaClassifier.model.predictionsMatrix.containsKey(new Trace(seq,true)))
					{
						if (unique == null)
							unique = seq;
						else
						{
							nonUnique = true;break;
						}
					}
				}
				
				if (nonUnique == false && unique!= null)
					sequencesUnique2.add(unique);
			}
		
		for(List<Label> seq:sequencesUnique2)
		{
			System.out.println(seq);
			for(CmpVertex vert:trimmedReference.transitionMatrix.keySet())
			{
				CmpVertex target=trimmedReference.getVertex(vert,seq);
				if (target != null)
				{
					System.out.println("\t -> "+target);
					CmpVertex stateOfInterest = trimmedReference.getVertex(vert,seq.subList(0, 2));
					System.out.println("\t "+trimmedReference.transitionMatrix.get(stateOfInterest));
				}
				
			}
		}

		int countTriples = 0, triplesUnique = 0;
		long totalTripleInPTA=0;
		

		List<Long> uniqueFreq = new LinkedList<Long>(), nonUniqueFreq=new LinkedList<Long>();
		List<Label> uniqueElem = new LinkedList<Label>(), nonUniqueElem = new LinkedList<Label>();
		List<Long> tripleCount=new LinkedList<Long>();
		
		for(Entry<Trace,MarkovOutcome> entry:ptaClassifier.model.predictionsMatrix.entrySet())
		{
			
			if (entry.getKey().getList().size() == 1 && entry.getValue() == MarkovOutcome.positive)
			{
				long countTripleInPTA=0;
				for(CmpVertex v:ptaClassifier.graph.transitionMatrix.keySet())
					if (ptaClassifier.graph.getVertex(v, entry.getKey().getList()) != null)
						++countTripleInPTA;
				tripleCount.add(countTripleInPTA);totalTripleInPTA+=countTripleInPTA;
				++countTriples;
				if (checkSeqIsUnique(trimmedReference, entry.getKey().getList()))
				{
					uniqueFreq.add((long)ptaClassifier.model.occurrenceMatrix.get(entry.getKey()).firstElem);uniqueElem.add(entry.getKey().getList().get(0));
					++triplesUnique;
				}
				else
				{
					nonUniqueFreq.add((long)ptaClassifier.model.occurrenceMatrix.get(entry.getKey()).firstElem);nonUniqueElem.add(entry.getKey().getList().get(0));
				}
				Collections.sort(uniqueFreq);Collections.sort(nonUniqueFreq);
			}
			
		}

		System.out.println("triples : "+countTriples+" unique elems: "+triplesUnique+" average in PTA: "+(totalTripleInPTA/(double)tripleCount.size()));
		
		System.out.println("Unique Freq: "+uniqueFreq);
		System.out.println("Non unique Freq: "+nonUniqueFreq);		
		return outcome;
	}
	*/
	/** Uses sideways predictions in order to identify more states to be merged. */
	public static Collection<Set<CmpVertex>> mergeBasedOnInversePredictions(MarkovClassifier ptaClassifier,LearnerGraph referenceGraph,final Collection<List<Label>> pathsOfInterest)
	{/*
		Map<CmpVertex,LearnerGraph> pathsFromEachStateInGraph = PairQualityLearner.constructPathsFromEachState(pta,directionForwardOrInverse);
		ConsistencyChecker checker = new MarkovUniversalLearner.DifferentPredictionsInconsistency();
		List<StatePair> pairsList = PairQualityLearner.buildVerticesToMergeForPath(pathsFromEachStateInGraph,directionForwardOrInverse,pathsOfInterest);
		LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		int score = pta.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
		LearnerGraph merged = MergeStates.mergeCollectionOfVertices(pta, null, verticesToMerge);// after merging all paths of interest, we get this graph.
		final Collection<List<Label>> pathsToMerge2=identifyPathsToMerge(merged,referenceGraph,m,!directionForwardOrInverse);
		/*
		m.updateMarkov(merged,predictForwardOrSideways,false);// now we construct sideways learner ...
		m.constructMarkovTentative(graph,predictForwardOrSideways);// ... and use it to add more transitions.
		*/
		MarkovModel inverseModel = new MarkovModel(ptaClassifier.model.getChunkLen(),true,!ptaClassifier.model.directionForwardOrInverse,false);
		MarkovClassifier cl = new MarkovClassifier(inverseModel,ptaClassifier.graph);cl.updateMarkov(false);
		Collection<Set<CmpVertex>> verticesToMergeUsingSideways=cl.buildVerticesToMergeForPaths(pathsOfInterest);
		return verticesToMergeUsingSideways;
	}
	
	public static LearnerGraph checkIfSingleStateLoopsCanBeFormed(MarkovClassifier ptaClassifier,LearnerGraph referenceGraph,final Collection<List<Label>> pathsOfInterest)
	{
		List<StatePair> pairsList = ptaClassifier.buildVerticesToMergeForPath(pathsOfInterest);
		LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		ptaClassifier.graph.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge, false);
		LearnerGraph merged = MergeStates.mergeCollectionOfVertices(ptaClassifier.graph, null, verticesToMerge, false);// after merging all paths of interest, we get this graph.
		ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistency();
		final long genScoreThreshold = 1;
		int nrOfMergers=0;
		List<StatePair> pairsToMerge = new LinkedList<StatePair>();
		for(Entry<CmpVertex,MapWithSearch<Label,Label,CmpVertex>> entry:merged.transitionMatrix.entrySet())
			for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
				if (merged.transitionMatrix.get(transition.getValue()).containsKey(transition.getKey()))
				{// we have a potential loop
					PairScore p = new PairScore(entry.getKey(),transition.getValue(),0,0);
					ArrayList<PairScore> pairOfInterest = new ArrayList<PairScore>(1);pairOfInterest.add(p);
					List<PairScore> correctPairs = new ArrayList<PairScore>(1), wrongPairs = new ArrayList<PairScore>(1);
					LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(ptaClassifier.graph, referenceGraph, pairOfInterest, correctPairs, wrongPairs);
					
					verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					long genScore = ptaClassifier.graph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge, false);
					LearnerGraph mergedForThisPair = MergeStates.mergeCollectionOfVertices(ptaClassifier.graph, null, verticesToMerge, false);
					long value = MarkovClassifier.computeInconsistency(mergedForThisPair, ptaClassifier.model, checker, false);
					
					boolean decidedToMerge= (value == 0 && genScore >= genScoreThreshold);
					if (decidedToMerge)
					{
						pairsToMerge.add(p);
						++nrOfMergers;
					}
					
					if ( !wrongPairs.isEmpty() && decidedToMerge)
							//(wrongPairs.isEmpty() && value > 0 || genScore < genScoreThreshold) ||  (!wrongPairs.isEmpty() && value == 0 && genScore >= genScoreThreshold))
					{
						System.out.println( p.toString()+(wrongPairs.isEmpty()?"valid, ":"invalid:")+value+ "(score "+genScore+")");
						System.out.println( "R: " + ptaClassifier.graph.transitionMatrix.get(p.getR())+" B: "+ptaClassifier.graph.transitionMatrix.get(p.getQ()));
					}
				}
		System.out.println("mergers identified: "+nrOfMergers);
		/*
		long genScore = graph.pairscores.computePairCompatibilityScore_general(null, pairsToMerge, verticesToMerge);
		LearnerGraph mergedForAllPairs = MergeStates.mergeCollectionOfVertices(graph, null, verticesToMerge);*/
		return merged;
	}
	
	public void constructMapFromLabelsToStateGroups(LearnerGraph tentativeGraph, Collection<Label> transitionsFromTheSameState)
	{
		Map<Label,Collection<CmpVertex>> labelToStates = 
				tentativeGraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY? new ArrayMapWithSearchPos<Label,Label,Collection<CmpVertex>>() : new TreeMap<Label,Collection<CmpVertex>>();
		Map<Label,Collection<CmpVertex>> labelFromStates = 
				tentativeGraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY? new ArrayMapWithSearchPos<Label,Label,Collection<CmpVertex>>() : new TreeMap<Label,Collection<CmpVertex>>();
					
		for(Label lbl:transitionsFromTheSameState) labelFromStates.put(lbl,new ArrayList<CmpVertex>());
		for(Entry<CmpVertex,MapWithSearch<Label,Label,CmpVertex>> entry:tentativeGraph.transitionMatrix.entrySet())
			if (entry.getKey().isAccept())
				for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
				{
					Collection<CmpVertex> statesToMerge = labelToStates.get(transition.getKey());
					if (statesToMerge != null && transition.getValue().isAccept()) statesToMerge.add(transition.getValue());

					Collection<CmpVertex> sourceStatesToMerge = labelFromStates.get(transition.getKey());
					if (sourceStatesToMerge != null && transition.getValue().isAccept()) sourceStatesToMerge.add(entry.getKey());
				}
		
	}
		
	protected static boolean checkSeqIsUnique(LearnerGraph referenceGraph, List<Label> seq)
	{
		boolean outcome = false;
		int count=0;
		for(CmpVertex v:referenceGraph.transitionMatrix.keySet())
		{
			if (referenceGraph.getVertex(v,seq) != null)
			{
				++count;
				if (count > 1)
					break;
			}
		}
		if (count == 1)
			outcome = true;
		
		return outcome;
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
	
	public static CmpVertex checkSeqUniqueOutgoing(LearnerGraph referenceGraph, List<Label> seq)
	{
		CmpVertex vertexOfInterest = null;
		for(CmpVertex v:referenceGraph.transitionMatrix.keySet())
		{
			CmpVertex currTarget = referenceGraph.getVertex(v,seq);
			if (currTarget != null)
			{
				if (vertexOfInterest != null)
					return null;

				vertexOfInterest = v;
			}
		}
		
		return vertexOfInterest;
	}
	
	
	/** Identifies states <i>steps</i> away from the root state and labels the first of them red and others blue. The colour of all other states is removed. When the learner
	 * starts, the exploration begins not from the root state as per blue fringe but from the marked red state. 
	 * The aim is to permit Markov predictive power to be used on arbitrary states, 
	 * without this we cannot predict anything in the vicinity of the root state which has no incoming transitions unless pre-merge is used. 
	 */ 
	public static void labelStatesAwayFromRoot(LearnerGraph graph, int steps)
	{
		graph.clearColours();graph.getInit().setColour(null);
		
		Set<CmpVertex> visited = new HashSet<CmpVertex>();
		Collection<CmpVertex> frontLine = new LinkedList<CmpVertex>(), nextLine = new LinkedList<CmpVertex>(), previousFrontLine = null;
		
		frontLine.add(graph.getInit());visited.add(graph.getInit());
		for(int line=0;line < steps;++line)
		{
			for(CmpVertex vert:frontLine)
				for(CmpVertex next:graph.transitionMatrix.get(vert).values())
					if (!visited.contains(next))
					{
						nextLine.add(next);visited.add(next);
					}
			
			previousFrontLine = frontLine;frontLine = nextLine;nextLine=new LinkedList<CmpVertex>();
		}
		for(CmpVertex blue:frontLine) blue.setColour(JUConstants.BLUE);
		if (frontLine.isEmpty())
			throw new IllegalArgumentException("no states beyond the steps");
		graph.additionalExplorationRoot = previousFrontLine;
		frontLine.iterator().next().setColour(JUConstants.RED);
	}


	
	public static void showInconsistenciesForDifferentMergers(LearnerGraph referenceGraph,MarkovClassifier ptaClassifier, Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA)
	{
		LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		int genScore = ptaClassifier.graph.pairscores.computePairCompatibilityScore_general(null, WaveBlueFringe.constructPairsToMergeBasedOnSetsToMerge(ptaClassifier.graph.transitionMatrix.keySet(),verticesToMergeBasedOnInitialPTA), verticesToMerge, false);
		LearnerGraph graph = MergeStates.mergeCollectionOfVertices(ptaClassifier.graph, null, verticesToMerge, false);
		
		Set<CmpVertex> tr=graph.transform.trimGraph(10, graph.getInit()).transitionMatrix.keySet();
		ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistency();

		//WaveBlueFringe.constructPairsToMergeBasedOnSetsToMerge(graph.transitionMatrix.keySet(),verticesToMergeBasedOnInitialPTA);		
		for(CmpVertex v0:tr)
			for(CmpVertex v1:tr)
				if (v0 != v1)
				{
					PairScore p = new PairScore(v0,v1,0,0);
					ArrayList<PairScore> pairOfInterest = new ArrayList<PairScore>(1);pairOfInterest.add(p);
					List<PairScore> correctPairs = new ArrayList<PairScore>(1), wrongPairs = new ArrayList<PairScore>(1);
					LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(graph, referenceGraph, pairOfInterest, correctPairs, wrongPairs);
					
					verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					genScore = graph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge, false);
					LearnerGraph merged = MergeStates.mergeCollectionOfVertices(graph, null, verticesToMerge, false);
					long value = MarkovClassifier.computeInconsistency(merged, ptaClassifier.model, checker, false);
					if ( (wrongPairs.isEmpty() && value > 0) ||  (!wrongPairs.isEmpty() && value == 0))
					{
						System.out.println( p.toString()+(wrongPairs.isEmpty()?"valid, ":"invalid:")+value+ "(score "+genScore+")");
						System.out.println( "R: " + graph.transitionMatrix.get(p.getR())+" B: "+graph.transitionMatrix.get(p.getQ()));
					}
				}
		
		System.out.println("finished dumping inconsistencies");
	}

	public static class LearnerRunner implements Callable<ThreadResult>
	{
		protected final Configuration config;
		protected final ConvertALabel converter;
		protected final int states,sample;
		protected boolean onlyUsePositives, pickUniqueFromInitial;
		protected final int seed;
		protected int chunkLen=3;
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
			final int alphabet = 2*states;
			LearnerGraph referenceGraph = null;
			ThreadResult outcome = new ThreadResult();
			Label uniqueFromInitial = null;
			MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
			do
			{
				referenceGraph = mg.nextMachine(alphabet,seed, config, converter).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
				if (pickUniqueFromInitial)
				{
					Map<Label,CmpVertex> uniques = LearningSupportRoutines.uniqueFromState(referenceGraph);
					if(!uniques.isEmpty())
					{
						Entry<Label,CmpVertex> entry = uniques.entrySet().iterator().next();
						referenceGraph.setInit(entry.getValue());uniqueFromInitial = entry.getKey();
					}
				}
			}
			while(pickUniqueFromInitial && uniqueFromInitial == null);
			
			LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);learnerEval.setLabelConverter(converter);
			final Collection<List<Label>> testSet = LearningAlgorithms.computeEvaluationSet(referenceGraph,states*3,states*alphabet);
			
			for(int attempt=0;attempt<2;++attempt)
			{// try learning the same machine a few times
				LearnerGraph pta = new LearnerGraph(config);
				RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(attempt),5,null);
				// The total number of elements in test sequences (alphabet*states*traceQuantity) will be distributed around (random(pathLength)+1). The total size of PTA is a product of these two.
				// For the purpose of generating long traces, we construct as many traces as there are states but these traces have to be rather long,
				// that is, length of traces will be (random(pathLength)+1)*sequencesPerChunk/states and the number of traces generated will be the same as the number of states.
				final int tracesToGenerate = LearningSupportRoutines.makeEven(traceQuantity);
				generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
										
						@Override
						public int getLength() {
							return 2*states*alphabet;//(rnd.nextInt(pathLength)+1)*lengthMultiplier;
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
				final MarkovModel m= new MarkovModel(chunkLen,true,true,false);
				m.createMarkovLearner(sPlus, sMinus,false);
				
				pta.clearColours();
				synchronized (AbstractLearnerGraph.syncObj) {
					//PaperUAS.computePTASize(selectionID+" attempt: "+attempt+" with unique: ", pta, referenceGraph);
				}
				
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
				//long inconsistencyForTheReferenceGraph = MarkovClassifier.computeInconsistency(trimmedReference, m, checker,false);
				//System.out.println("Inconsistency of trimmed reference : "+inconsistencyForTheReferenceGraph);
				
				//if (inconsistencyForTheReferenceGraph != 53)
				//	break;// ignore automata where we get good results.
					
				MarkovClassifier ptaClassifier = new MarkovClassifier(m,pta);
				final List<List<Label>> pathsToMerge=ptaClassifier.identifyPathsToMerge(checker);
				final Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA=ptaClassifier.buildVerticesToMergeForPaths(pathsToMerge);

				/*
				List<StatePair> pairsListInitialMerge = ptaClassifier.buildVerticesToMergeForPath(pathsToMerge);
				LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMergeInitialMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
				int scoreInitialMerge = pta.pairscores.computePairCompatibilityScore_general(null, pairsListInitialMerge, verticesToMergeInitialMerge);
				assert scoreInitialMerge >= 0;
				final LearnerGraph ptaAfterInitialMerge = MergeStates.mergeCollectionOfVertices(pta, null, verticesToMergeInitialMerge);
				final CmpVertex vertexWithMostTransitions = findVertexWithMostTransitions(ptaAfterInitialMerge,MarkovClassifier.computeInverseGraph(pta));
				ptaAfterInitialMerge.clearColours();ptaAfterInitialMerge.getInit().setColour(null);vertexWithMostTransitions.setColour(JUConstants.RED);
				ptaClassifier = new MarkovClassifier(m,ptaAfterInitialMerge);// rebuild the classifier
				LearnerGraphND inverseOfPtaAfterInitialMerge = MarkovClassifier.computeInverseGraph(ptaAfterInitialMerge);
				System.out.println("Centre vertex: "+vertexWithMostTransitions+" "+countTransitions(ptaAfterInitialMerge, inverseOfPtaAfterInitialMerge, vertexWithMostTransitions));
				//checkIfSingleStateLoopsCanBeFormed(pta,m,referenceGraph,pathsToMerge,directionForwardOrInverse);
				/*
				System.out.println("initially: "+whatToMerge.size()+" clusters "+whatToMerge+"\nafter sideways "+clustersOfStates.size()+" clusters "+clustersOfStates);
				showInconsistenciesForDifferentMergers(referenceGraph,m,pta,clustersOfStates);
				 */
				
				if (pickUniqueFromInitial)
				{
					pta = LearningSupportRoutines.mergeStatesForUnique(pta,uniqueFromInitial);
					learnerOfPairs = new LearnerMarkovPassive(learnerEval,pta);learnerOfPairs.setMarkovModel(m);
					learnerOfPairs.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueFromInitial}));
					
					actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());

					LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMerge(actualAutomaton,learnerOfPairs.getLabelsLeadingToStatesToBeMerged(),learnerOfPairs.getLabelsLeadingFromStatesToBeMerged());
					if (!pairsList.isEmpty())
					{
						int score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge, false);
						if (score < 0)
						{
							learnerOfPairs = new LearnerMarkovPassive(learnerEval,pta);learnerOfPairs.setMarkovModel(m);
							learnerOfPairs.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueFromInitial}));
							actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
							score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge, false);
							throw new RuntimeException("last merge in the learning process was not possible");
						}
						actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge, false);
					}
				}
				else
				{// not merging based on a unique transition from an initial state
					//learnerEval.config.setGeneralisationThreshold(1);
					learnerOfPairs = new LearnerMarkovPassive(learnerEval,pta);learnerOfPairs.setMarkovModel(m);

					//learnerOfPairs.setPairsToMerge(checkVertices(pta, referenceGraph, m));
					final LearnerGraph finalReferenceGraph = referenceGraph;

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
							PairScore p = LearningSupportRoutines.pickPairQSMLike(pairs);
							LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
							// constructPairsToMergeBasedOnSetsToMerge(coregraph.transitionMatrix.keySet(),verticesToMergeBasedOnInitialPTA)
							int genScore = coregraph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge, false);
							assert genScore >= 0;
							LearnerGraph merged = MergeStates.mergeCollectionOfVertices(coregraph, null, verticesToMerge, false);
							long value = MarkovClassifier.computeInconsistency(merged, m, checker, false);
							inconsistencyFromAnEarlierIteration = value;
							return null;
						}
						
						long inconsistencyFromAnEarlierIteration = 0;
						LearnerGraph coregraph = null;
						
						LearnerGraphND inverseGraph = null;
						/** Where I have a set of paths to merge because I have identified specific states, this map is constructed that maps vertices to be merged together to the partition number that corresponds to them. */
						Map<CmpVertex,Integer> vertexToPartition = new TreeMap<CmpVertex,Integer>();
						
						@Override
						public void initComputation(LearnerGraph graph) 
						{
							coregraph = graph;
							//labelStatesAwayFromRoot(coregraph,m.getChunkLen()-1);
							inverseGraph = (LearnerGraphND)MarkovClassifier.computeInverseGraph(coregraph,true);
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
							/*
							MarkovClassifier cl = new MarkovClassifier(m, coregraph);
							long score = 0;
							Map<Label, MarkovOutcome> predictedFromRed=cl.predictTransitionsFromState(p.getR(), null, m.getChunkLen(), null);
							for(Entry<Label,MarkovOutcome> entry:cl.predictTransitionsFromState(p.getQ(), null, m.getChunkLen(), null).entrySet())
							{
								MarkovOutcome red = predictedFromRed.get(entry.getKey());
								if (red == null || red != entry.getValue())
								{
									score = -1;break;
								}
							}
							
							if (score >= 0)
							{
								LearnerGraph extendedGraph = cl.constructMarkovTentative();
								score = extendedGraph.pairscores.computePairCompatibilityScore(p);
							}
							*/

							long score = p.getScore();//computeScoreUsingMarkovFanouts(coregraph,origInverse,m,callbackAlphabet,p);
							if (score < 0)
								return score;
							long currentInconsistency = 0;
							Integer a=vertexToPartition.get(p.getR()), b = vertexToPartition.get(p.getQ());
							LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
							int genScore = coregraph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge, false);
							if (genScore >= 0)
							{
								LearnerGraph merged = MergeStates.mergeCollectionOfVertices(coregraph, null, verticesToMerge, false);
								currentInconsistency = MarkovClassifier.computeInconsistency(merged, m, checker, 
										false
										//p.getQ().getStringId().equals("P2672") && p.getR().getStringId().equals("P2209")
										)-inconsistencyFromAnEarlierIteration;
								
								if (a == null || b == null || a != b)
									score -= currentInconsistency;
							}
							//System.out.println(p.toString()+", score "+score);
							
							/*
							ArrayList<PairScore> pairOfInterest = new ArrayList<PairScore>(1);pairOfInterest.add(p);
							List<PairScore> correctPairs = new ArrayList<PairScore>(1), wrongPairs = new ArrayList<PairScore>(1);
							SplitSetOfPairsIntoRightAndWrong(coregraph, finalReferenceGraph, pairOfInterest, correctPairs, wrongPairs);
							long score = p.getScore();//computeScoreUsingMarkovFanouts(coregraph,origInverse,m,callbackAlphabet,p);
							if (score < 0)
								return score;
							long currentInconsistency = 0;
							double relativeInconsistency = 0.;
							Integer a=vertexToPartition.get(p.getR()), b = vertexToPartition.get(p.getQ());
							LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
							int genScore = coregraph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge);
							if (genScore >= 0)
								LearnerGraph merged = MergeStates.mergeCollectionOfVertices(coregraph, null, verticesToMerge);
								currentInconsistency = MarkovClassifier.computeInconsistency(merged, m, checker,
										false
										//p.getQ().getStringId().equals("P2672") && p.getR().getStringId().equals("P2209")
										)-inconsistencyFromAnEarlierIteration;
								relativeInconsistency = new MarkovClassifier(m, merged).computeRelativeInconsistency(checker);
							}
							
							// A green state next to a red may have many incoming paths, more than in a PTA, some of which may predict its outgoing transition as non-existent. 
							// When a merge happens this state may be merged into the one with a similar surroundings. In this way, two states with the same in-out inconsistency
							// are merged into the one with that inconsistency, turning two inconsistencies into one and hence reducing the total number of inconsistencies.
							score=genScore;
							if (relativeInconsistency > 5 || relativeInconsistency > genScore)
								score=-1;
							*/
							//System.out.println("pair: "+p+" score: "+score);
							/*
							if (score < 0 && wrongPairs.isEmpty())
								System.out.println("incorrectly blocked merge of "+p+" a="+a+" b="+b+" inconsistency = "+currentInconsistency+" relative: "+relativeInconsistency+" genscore is "+genScore);
							if (score >= 0 && correctPairs.isEmpty())
								System.out.println("invalid merge of "+p+" a="+a+" b="+b+" inconsistency = "+currentInconsistency+" relative: "+relativeInconsistency+" genscore is "+genScore);
							*/

							return score;
						}

						/** This one returns a set of transitions in all directions. */
						@Override
						public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(CmpVertex currentRed) 
						{
							return null;//obtainSurroundingTransitions(coregraph,inverseGraph,currentRed);
						}

					});

					actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
				}

				{
					LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					int genScore = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, WaveBlueFringe.constructPairsToMergeBasedOnSetsToMerge(actualAutomaton.transitionMatrix.keySet(),verticesToMergeBasedOnInitialPTA), verticesToMerge, false);
					assert genScore >= 0;
					actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge, false);
					long chains = 0,tails=0,doubleChains=0;
					for(Entry<CmpVertex,MapWithSearch<Label,Label,CmpVertex>> entry:actualAutomaton.transitionMatrix.entrySet())
					{
						if (entry.getValue().isEmpty())
							++tails;
						if (entry.getValue().size() == 1)
						{
							++chains;
							CmpVertex target=entry.getValue().values().iterator().next();
							if (actualAutomaton.transitionMatrix.get(target).size() == 1)
								++doubleChains;
						}
					}
					//System.out.println("Chains: "+chains+" Tails: "+tails+" Double chains: "+doubleChains);
					
					//System.out.println("Inconsistency for the original: "+new MarkovClassifier(ptaClassifier.model, trimmedReference).countPossibleInconsistencies(checker)+" and for the learnt: "+new MarkovClassifier(ptaClassifier.model, actualAutomaton).countPossibleInconsistencies(checker));
					//actualAutomaton = formLoops(actualAutomaton, m, directionForwardOrInverse);
				}

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

				LearnerGraph outcomeOfReferenceLearner = LearningAlgorithms.constructLearner(learnerEval,ptaCopy,LearningAlgorithms.ScoringToApply.SCORING_SICCO,Configuration.ScoreMode.CONVENTIONAL).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
				dataSample.referenceLearner = estimateDifference(referenceGraph, outcomeOfReferenceLearner,testSet);
				System.out.println("actual: "+actualAutomaton.getStateNumber()+" from reference learner: "+outcomeOfReferenceLearner.getStateNumber()+ " difference actual is "+dataSample.actualLearner+ " difference ref is "+dataSample.referenceLearner);
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

	
	/** Given a graph, computes transitions exiting a supplied state that lead to non-red states.
	 * 
	 * @param coregraph graph to consider
	 * @param currentRed the state of interest
	 * @param ignoreSelf whether to include single-state loops. 
	 * @param whereToAddTransitions collection of transitions to populate, not a map to permit non-deterministic choice.
	 */
	private static <TARGET_A_TYPE,CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>> 
		void addTransitionsFrom(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> coregraph, CmpVertex currentRed,boolean ignoreSelf, Collection<Entry<Label,CmpVertex>> whereToAddTransitions)
	{
		for(final Entry<Label,TARGET_A_TYPE> incoming:coregraph.transitionMatrix.get(currentRed).entrySet())
			for(final CmpVertex v:coregraph.getTargets(incoming.getValue()))
				if (v.getColour() != JUConstants.RED && (ignoreSelf || v != currentRed))
					whereToAddTransitions.add(new Map.Entry<Label,CmpVertex>(){
						final Label key = incoming.getKey();
						final CmpVertex target = v;
						@Override
						public Label getKey() {
							return key;
						}

						@Override
						public CmpVertex getValue() {
							return target;
						}

						@Override
						public CmpVertex setValue(@SuppressWarnings("unused") CmpVertex value) 
						{
							throw new UnsupportedOperationException("changing values of this map entry is not permitted");
						}});
	}
	
	/** Given a graph and its inverse, computes transitions exiting a supplied state.
	 * 
	 * @param coregraph graph to consider
	 * @param current the state of interest
	 * @param ignoreSelf whether to include single-state loops. 
	 */
	private static <TARGET_A_TYPE,CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>> 
		long countTransitionsFrom(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> coregraph, CmpVertex current,boolean ignoreSelf)
	{
		long outcome = 0;
		
		for(final Entry<Label,TARGET_A_TYPE> incoming:coregraph.transitionMatrix.get(current).entrySet())
			for(final CmpVertex v:coregraph.getTargets(incoming.getValue()))
				if ( (ignoreSelf || v != current) )
					++outcome;

		return outcome;
	}
	
	/** Given a graph and its inverse, computes transitions entering/exiting a supplied state that lead to non-red states.
	 * 
	 * @param coregraph graph to consider
	 * @param inverseGraph the inverse of the graph to consider
	 * @param currentRed the state of interest
	 * @return collection of transitions, not a map to permit non-deterministic choice. 
	 */
	public static <TARGET_A_TYPE,TARGET_B_TYPE,
		CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
		CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>>
		Collection<Map.Entry<Label,CmpVertex>> obtainSurroundingTransitions(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> coregraph, AbstractLearnerGraph<TARGET_B_TYPE, CACHE_B_TYPE> inverseGraph, CmpVertex currentRed)
	{
		Collection<Entry<Label,CmpVertex>> surroundingTransitions = new ArrayList<Entry<Label,CmpVertex>>();
		addTransitionsFrom(coregraph, currentRed,true, surroundingTransitions);addTransitionsFrom(inverseGraph, currentRed,false, surroundingTransitions);
		return surroundingTransitions;
	}

	/** Given a graph and its inverse, counts transitions entering/exiting a supplied state.
	 * 
	 * @param coregraph graph to consider
	 * @param inverseGraph the inverse of the graph to consider
	 * @param current the state of interest
	 * @return number of transitions 
	 */
	public static <TARGET_A_TYPE,TARGET_B_TYPE,
		CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
		CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>>
		long countTransitions(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> coregraph, AbstractLearnerGraph<TARGET_B_TYPE, CACHE_B_TYPE> inverseGraph, CmpVertex current)
	{
		return countTransitionsFrom(coregraph, current,true)+countTransitionsFrom(inverseGraph, current,false);
	}

	/** Identifies a vertex with the maximal number of incoming and outgoing transitions.
	 * 
	 * @param coregraph graph to consider
	 * @param inverseGraph the inverse of the graph to consider
	 * @return vertex with the maximal number of incoming and outgoing transitions.
	 */
	public static <TARGET_A_TYPE,TARGET_B_TYPE,
		CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
		CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>>
		CmpVertex findVertexWithMostTransitions(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> coregraph, AbstractLearnerGraph<TARGET_B_TYPE, CACHE_B_TYPE> inverseGraph)
		{
			CmpVertex outcome = coregraph.getInit();
			long maxSize=0;
			for(CmpVertex v:coregraph.transitionMatrix.keySet())
			{
				long size = obtainSurroundingTransitions(coregraph,inverseGraph,v).size();
				if (size > maxSize)
				{
					maxSize = size;outcome = v;
				}
			}
			
			return outcome;
		}
		
	/** An extension of {@Link PairScore} with Markov distance. */
	public static class PairScoreWithDistance extends PairScore
	{
		private double distance;
		
		public PairScoreWithDistance(PairScore p, double d) {
			super(p.getQ(), p.getR(), p.getScore(), p.getAnotherScore());distance = d;
		}
		
		public double getDistanceScore()
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
		
		@Override
		public String toString()
		{
			return "[ "+getQ().getStringId()+"("+getQ().isAccept()+","+getQ().getDepth()+"), "+getR().getStringId()+"("+getR().isAccept()+","+getR().getDepth()+") : "+getScore()+","+getAnotherScore()+","+distance+" ]";
		}
	}
	
	/** Uses the supplied classifier to rank pairs. */
	public static class LearnerMarkovPassive extends ReferenceLearnerUsingSiccoScoring
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
		
		public LearnerMarkovPassive(LearnerEvaluationConfiguration evalCnf, final LearnerGraph argInitialPTA) 
		{
			super(evalCnf,argInitialPTA,false);
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
				result=LearningSupportRoutines.pickPairQSMLike(outcome);
				assert result!=null;
				assert result.getScore()>=0;
/*
 				List<PairScore> correctPairs = new ArrayList<PairScore>(1), wrongPairs = new ArrayList<PairScore>(1);
				List<PairScore> pairs = new ArrayList<PairScore>(1);pairs.add(result);
				SplitSetOfPairsIntoRightAndWrong(graph, referenceGraph, pairs, correctPairs, wrongPairs);
				if (!correctPairs.isEmpty())
					System.out.println("merge correct");
				else
				{
					System.out.println("merge WRONG "+result);
//					Visualiser.updateFrame(graph.transform.trimGraph(3, graph.getInit()), referenceGraph);
//					Visualiser.waitForKey();
				}
				*/
				outcome.clear();outcome.push(result);
			}	
			return outcome;

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
		config.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);//ONLYOVERRIDE);
		ConvertALabel converter = new Transform.InternStringLabel();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int ThreadNumber = ExperimentRunner.getCpuNumber();	
		ExecutorService executorService = Executors.newFixedThreadPool(ThreadNumber);
		final int minStateNumber = 20;
		final int samplesPerFSM = 10;
		final int rangeOfStateNumbers = 4;
		final int stateNumberIncrement = 4;
		final int traceQuantity=5;
		
/*
		LearnerRunner oneExperimentRunner = new LearnerRunner(minStateNumber,0,traceQuantity+2,traceQuantity, config, converter);
		oneExperimentRunner.setPickUniqueFromInitial(false);
		oneExperimentRunner.setOnlyUsePositives(false);oneExperimentRunner.setLengthMultiplier(50);
		//learnerRunner.setSelectionID(selection+"_states"+states+"_sample"+sample);
		oneExperimentRunner.call();
*/
		// Stores tasks to complete.
		CompletionService<ThreadResult> runner = new ExecutorCompletionService<ThreadResult>(executorService);
		for(final int lengthMultiplier:new int[]{150})
		for(final boolean onlyPositives:new boolean[]{true})
			{
				for(final boolean useUnique:new boolean[]{false})
				{
					String selection;
					for(final boolean selectingRed:new boolean[]{false})
					for(final boolean classifierToBlockAllMergers:new boolean[]{false})
					for(final double threshold:new double[]{1.0})
					{
						final boolean zeroScoringAsRed = false;
						selection = "TRUNK;EVALUATION;"+";threshold="+threshold+
								";onlyPositives="+onlyPositives+";selectingRed="+selectingRed+";classifierToBlockAllMergers="+classifierToBlockAllMergers+";zeroScoringAsRed="+zeroScoringAsRed+";lengthMultiplier="+lengthMultiplier+";";

						final int totalTaskNumber = traceQuantity;
						final RBoxPlot<Long> gr_PairQuality = new RBoxPlot<Long>("Correct v.s. wrong","%%",new File("percentage_score"+selection.substring(0, 80)+".pdf"));
						final RBoxPlot<String> gr_QualityForNumberOfTraces = new RBoxPlot<String>("traces","%%",new File("quality_traces"+selection.substring(0, 80)+".pdf"));
						SquareBagPlot gr_NewToOrig = new SquareBagPlot("orig score","score with learnt selection",new File("new_to_orig"+selection.substring(0, 80)+".pdf"),0,1,true);
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
										gr_NewToOrig.add(sample.referenceLearner.getValue(),sample.actualLearner.getValue());
								}
								
								for(SampleData sample:result.samples)
									if (sample.referenceLearner.getValue() > 0)
										gr_QualityForNumberOfTraces.add(traceQuantity+"",sample.actualLearner.getValue()/sample.referenceLearner.getValue());
								progress.next();
							}
							if (gr_PairQuality != null)
							{
								synchronized(pairQualityCounter)
								{
									LearningSupportRoutines.updateGraph(gr_PairQuality,pairQualityCounter);
									//gr_PairQuality.drawInteractive(gr);
									//gr_NewToOrig.drawInteractive(gr);
									//if (gr_QualityForNumberOfTraces.size() > 0)
									//	gr_QualityForNumberOfTraces.drawInteractive(gr);
								}
							}
							if (gr_PairQuality != null) gr_PairQuality.reportResults(gr);
						}
						catch(Exception ex)
						{
							IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
							if (executorService != null) { executorService.shutdownNow();executorService = null; }
							throw e;
						}
						if (gr_NewToOrig != null) gr_NewToOrig.reportResults(gr);
						if (gr_QualityForNumberOfTraces != null) gr_QualityForNumberOfTraces.reportResults(gr);
					}
				}
			}
		if (executorService != null) { executorService.shutdown();executorService = null; }
	

	}
}