package statechum.analysis.learning;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.TreeSet;
import java.util.Map.Entry;

import org.junit.Assert;

import statechum.Configuration;
import statechum.Label;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.experiments.PaperUAS.TracesForSeed.Automaton;
import statechum.analysis.learning.observers.DummyLearner;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

/** Makes it possible to run the same learner with different choices of pairs to see how it affects the outcome. 
 * It is possible not only to record pair selection but also to replay the one recorded before. 
 */
public class RPNIBlueFringeVariability
{
	private Learner learner;
	final List<PairOfPaths> listOfPairsToWrite;
	final Iterator<PairOfPaths> listOfPairsToCheckAgainstIterator;
	final boolean useOptimizedMerge;
	LearnerGraph initPta = null;
	final Configuration config;
	
	VertexID phantomVertex = null;
	
	public Learner getLearner()
	{
		return learner;
	}
	
	/** Constructs an instance of this learner. 
	 * 
	 * @param evaluationConfiguration configuration to initialise with. Uses ifthensequences, configuration and converter.
	 * @param optimisedMerge whether to use a slow ({@link PairScoreComputation#computePairCompatibilityScore_general} or a fast {@link MergeStates#mergeAndDeterminize}. 
	 * Fast merger is much faster but expects to merge a PTA into a graph; the slow one can merge arbitrary states in a graph.
	 * @param lw if non-<i>null</i>, stores the list of pairs encountered while learning.
	 * @param lc if non-<i>null</i>, uses this as a source of pairs to merge. This is used to check that a learner will learn the same automaton when it goes through the same sequences of mergers.
	 */
	public RPNIBlueFringeVariability(final LearnerEvaluationConfiguration evaluationConfiguration, boolean optimisedMerge, final List<PairOfPaths> lw, final List<PairOfPaths> lc) 
	{
		listOfPairsToWrite = lw;useOptimizedMerge = optimisedMerge;
		if (lc != null) listOfPairsToCheckAgainstIterator = lc.iterator();else listOfPairsToCheckAgainstIterator = null;
		final LearnerEvaluationConfiguration bfLearnerInitConfiguration = new LearnerEvaluationConfiguration(evaluationConfiguration.config);
		bfLearnerInitConfiguration.ifthenSequences = evaluationConfiguration.ifthenSequences;
		bfLearnerInitConfiguration.setLabelConverter(evaluationConfiguration.getLabelConverter());
		config = bfLearnerInitConfiguration.config;// our config is a copy of the one supplied as an argument.
		config.setAskQuestions(false); 
		learner = new DummyLearner(new RPNIUniversalLearner(null, bfLearnerInitConfiguration)) 
		{
			
			@Override
			public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
			{
				LearnerGraph outcome = null;
				int extraPhantomVertices = 0;
				if (useOptimizedMerge)
				{
					// Use the old and limited version to compute the merge because the general one is too slow on large graphs and we do not need either to merge arbitrary states or to handle "incompatibles".
					outcome = MergeStates.mergeAndDeterminize(original, pair);outcome.pathroutines.updateDepthLabelling();
				}
				else
				{
					Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new ArrayList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					long score = original.pairscores.computePairCompatibilityScore_general(pair,null,mergedVertices);
					outcome = MergeStates.mergeCollectionOfVertices(original,pair.getR(),mergedVertices, true);
					
					if (score != original.getStateNumber()-outcome.getStateNumber())
					{// This is either a bug somewhere in the merger or (most likely) that the phantomVertex has been removed by the generalised learner. 
					 // We are checking which of these two has happened in the code below.
					 // The computation below is expensive on large graphs but only needs to be done once.
						LinkedHashSet<CmpVertex> removedStates = new LinkedHashSet<CmpVertex>();removedStates.addAll(original.transitionMatrix.keySet());
						removedStates.removeAll(outcome.transitionMatrix.keySet());removedStates.remove(pair.getQ());removedStates.remove(pair.getR());
						Assert.assertEquals(1,removedStates.size());// if it were a phantom vertex, there would only be one of them.
						CmpVertex tentativePhantom = removedStates.iterator().next();
						Set<Label> alphabetUsedOnPhantom = new TreeSet<Label>();alphabetUsedOnPhantom.addAll(original.pathroutines.computeAlphabet());
						for(Entry<Label,CmpVertex> transition:original.transitionMatrix.get(tentativePhantom).entrySet())
						{
							Assert.assertSame(tentativePhantom,transition.getValue());alphabetUsedOnPhantom.remove(transition.getKey());
						}
						Assert.assertEquals(0, alphabetUsedOnPhantom.size());
						extraPhantomVertices = 1;// now certain it was indeed a phantom vertex added when the PTA was initially built.
					}
					
					Assert.assertEquals(score+extraPhantomVertices,original.getStateNumber()-outcome.getStateNumber());
				}
				ScoreMode origScore = original.config.getLearnerScoreMode();original.config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);
				long compatibilityScore = original.pairscores.computePairCompatibilityScore(pair);
				original.config.setLearnerScoreMode(origScore);
				
				Assert.assertEquals(compatibilityScore+1+extraPhantomVertices,original.getStateNumber()-outcome.getStateNumber());
				return outcome;
			}

			@Override 
			public Stack<PairScore> ChooseStatePairs(LearnerGraph graph)
			{
				Stack<PairScore> outcome = graph.pairscores.chooseStatePairs(new PairScoreComputation.RedNodeSelectionProcedure(){

					@Override
					public CmpVertex selectRedNode(LearnerGraph coregraph,	@SuppressWarnings("unused") Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
					{
						CmpVertex redVertex = null;
						if (listOfPairsToWrite != null)
						{
							redVertex = tentativeRedNodes.iterator().next();
							listOfPairsToWrite.add(new PairOfPaths(coregraph, new PairScore(null, redVertex, 0, 0)));
						}
						
						if(listOfPairsToCheckAgainstIterator != null)
						{
							PairOfPaths pair = listOfPairsToCheckAgainstIterator.next();
							Assert.assertNull(pair.getQ());
							redVertex = coregraph.getVertex(pair.getR());
						}
						return redVertex;
					}

					@SuppressWarnings("unused")
					@Override
					public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, Collection<CmpVertex> reds, List<PairScore> pairs) {
						return null;// do not resolve in any way
					}

					@Override
					public void initComputation(@SuppressWarnings("unused") LearnerGraph gr) {
						// dummy
					}

					@Override
					public long overrideScoreComputation(PairScore p) {
						return p.getScore();// dummy
					}

					@Override
					public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(@SuppressWarnings("unused") CmpVertex currentRed) 
					{
						return null;// dummy, ignored if null.
					}
				});
				
				if (!outcome.isEmpty())
				{
					if (listOfPairsToWrite != null)
					{
						//System.out.println("Optimized: "+useOptimizedMerge+", matrix: "+graph.config.getTransitionMatrixImplType()+", pair : "+outcome.peek());
						listOfPairsToWrite.add(new PairOfPaths(graph, outcome.peek()));
					}
					
					if(listOfPairsToCheckAgainstIterator != null)
					{
						PairOfPaths pair = listOfPairsToCheckAgainstIterator.next();
						while(pair.getQ() == null)
							pair = listOfPairsToCheckAgainstIterator.next();// skip red pairs, we are no longer checking with selectRedNode collections of reds where there there is only one choice.
						//System.out.println("Choosing "+pair+" from "+outcome);
						//System.out.println("chosen "+outcome.peek()+", expected "+new PairScore(graph.getVertex(pair.getQ()),graph.getVertex(pair.getR()),0,0));
						pair.rebuildStack(graph, outcome);
					}
				}
				
				return outcome;
			}
		
			@Override 
			public LearnerGraph init(Collection<List<Label>> plus,	Collection<List<Label>> minus) 
			{
				if (initPta != null)
				{
					LearnerGraph graph = decoratedLearner.init(plus,minus);
					LearnerGraph.copyGraphs(initPta, graph);
					return initPta;
				}
				throw new IllegalArgumentException("should not be called");
			}
			
			@Override 
			public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
			{
				LearnerGraph graph = decoratedLearner.init(engine,plusSize,minusSize);

				if (initPta != null)
				{
					LearnerGraph.copyGraphs(initPta, graph);
				}
				return graph;
			}
		};

	}
	
	/** After this method is called, the learner used above no longer looks at the PTA is it given but assumes that the PTA supplied to this call should be returned as a result of initialisation.
	 * This is used to get around the problem of  
	 * @param initPTAArg
	 */
	public void setInitPta(LearnerGraph initPTAArg)
	{
		initPta = initPTAArg;
	}
	
	/** Learns starting with the supplied PTA. */
	public LearnerGraph learn(LearnerGraph initPTAArg)
	{
		setInitPta(initPTAArg);
		LearnerGraph outcome = learner.learnMachine(new LinkedList<List<Label>>(), new LinkedList<List<Label>>());
		if (phantomVertex != null) 
			outcome.transitionMatrix.remove(outcome.findVertex(phantomVertex));
		return outcome;
	}
	
	public LearnerGraph learn(PTASequenceEngine engineArg, boolean useNegatives)
	{
		PTASequenceEngine engine = null;
		if (!useNegatives)
		{
			PTASequenceEngine positives = new PTASequenceEngine();positives.init(new Automaton());
			SequenceSet initSeq = positives.new SequenceSet();initSeq.setIdentity();
			initSeq.cross(engineArg.getData());
			engine = positives;
		}
		else
			engine = engineArg;
		
		LearnerGraph outcome = learner.learnMachine(engine,0,0);
		if (phantomVertex != null) 
			outcome.transitionMatrix.remove(outcome.findVertex(phantomVertex));
		return outcome;
	}
	
}