package statechum.analysis.learning.rpnicore;

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
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.PairOfPaths;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PaperUAS.TracesForSeed.Automaton;
import statechum.analysis.learning.observers.DummyLearner;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.old_generalised_merge_routines.OldMergeStates;
import statechum.analysis.learning.rpnicore.old_generalised_merge_routines.OldPairScoreComputation;
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
	final boolean usePTAMErging;
	LearnerGraph initPta = null;
	final Configuration config;
	
	VertexID phantomVertex = null;
	
	public Learner getLearner()
	{
		return learner;
	}
	
	protected static boolean graphSmallEnoughToTestDifferentAlgorithms(LearnerGraph graph)
	{
		return false;//graph.vertPositiveID < 100;// limiting this to smaller graphs permits tests to run in reasonable time while still doing a good job of testing.
	}
	
	protected static void checkMergers(LearnerGraph graph, StatePair pair,long score, LearnerGraph expectedOutcome)
	{
		if (graphSmallEnoughToTestDifferentAlgorithms(graph))
		{
			// another test is to check that merger from a partial set of equivalence classes produces the same graph, regardless whether auxiliary information is returned as part of mergers or not.
			for(boolean useReducedListOfEquivalenceClasses:new boolean[]{true,false})
			{
				Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> reducedVertices = new ArrayList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
				long otherscore = graph.pairscores.computePairCompatibilityScore_general(pair,null,reducedVertices,useReducedListOfEquivalenceClasses);
				Assert.assertEquals(score, otherscore);
				LearnerGraph outcomeTmp = MergeStates.mergeCollectionOfVertices(graph,pair.getR(),reducedVertices, true);
		        DifferentFSMException diff = WMethod.checkM(expectedOutcome, outcomeTmp);
		        if (diff != null)
		        	throw diff;
		        outcomeTmp = MergeStates.mergeCollectionOfVertices(graph,pair.getR(),reducedVertices, false);
		        diff = WMethod.checkM(expectedOutcome, outcomeTmp);
		        if (diff != null)
		        	throw diff;
			}
			
			// the final test is to check that done by the old generalised score routines produces the same outcome.
			{
				Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> reducedVertices = new ArrayList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
				long otherscore = new OldPairScoreComputation(graph).computePairCompatibilityScore_general(pair,null,reducedVertices);
				Assert.assertEquals(score, otherscore);
				LearnerGraph outcomeTmp = OldMergeStates.mergeCollectionOfVertices(graph,pair.getR(),reducedVertices);
		        DifferentFSMException diff = WMethod.checkM(expectedOutcome, outcomeTmp);
		        if (diff != null)
		        	throw diff;
		        outcomeTmp = MergeStates.mergeCollectionOfVertices(graph,pair.getR(),reducedVertices, true);
		        diff = WMethod.checkM(expectedOutcome, outcomeTmp);
		        if (diff != null)
		        	throw diff;
			}
		}

	}
	/** Constructs an instance of this learner. 
	 * 
	 * @param evaluationConfiguration configuration to initialise with. Uses ifthensequences, configuration and converter.
	 * @param useConventionalPTAMerging whether to use a slow ({@link PairScoreComputation#computePairCompatibilityScore_general} or a fast {@link MergeStates#mergeAndDeterminize}. 
	 * Fast merger is much faster but expects to merge a PTA into a graph; the slow one can merge arbitrary states in a graph.
	 * @param lw if non-<i>null</i>, stores the list of pairs encountered while learning.
	 * @param lc if non-<i>null</i>, uses this as a source of pairs to merge. This is used to check that a learner will learn the same automaton when it goes through the same sequences of mergers.
	 */
	public RPNIBlueFringeVariability(final LearnerEvaluationConfiguration evaluationConfiguration, boolean useConventionalPTAMerging, final List<PairOfPaths> lw, final List<PairOfPaths> lc) 
	{
		listOfPairsToWrite = lw;usePTAMErging = useConventionalPTAMerging;
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
				System.out.println(new java.util.Date()+" (new) started merging "+original.getStateNumber()+" states");
				int extraPhantomVertices = 0;
				if (usePTAMErging)
				{
					// Use the old and limited version to compute the merge because the general one is too slow on large graphs and we do not need either to merge arbitrary states or to handle "incompatibles".
					outcome = MergeStates.mergeAndDeterminize(original, pair);
					
					outcome.pathroutines.updateDepthLabelling();
				}
				else
				{
					Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new ArrayList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					long score = original.pairscores.computePairCompatibilityScore_general(pair,null,mergedVertices,false);
					outcome = MergeStates.mergeCollectionOfVertices(original,pair.getR(),mergedVertices, false);
					outcome.pathroutines.updateDepthLabelling();
					
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
				checkMergers(original,pair,compatibilityScore+1+extraPhantomVertices,outcome);
				System.out.println(new java.util.Date()+" (new) finished merging by removing "+(original.getStateNumber()-outcome.getStateNumber())+" states");
				/*
				if (outcome.getStateNumber() < 6700)
					try {
						outcome.storage.writeGraphML(usePTAMErging+"_"+config.getLearnerScoreMode());
					} catch (IOException e) {
						e.printStackTrace();
					}
					*/
				return outcome;
			}

			@Override 
			public Stack<PairScore> ChooseStatePairs(final LearnerGraph graph)
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
						boolean haveToUseGeneralisedComputation = graph.config.getLearnerScoreMode() == ScoreMode.GENERAL || graph.config.getLearnerScoreMode() == ScoreMode.ONLYOVERRIDE;
						int scoreGeneral = -1;
						if (haveToUseGeneralisedComputation)
						{
							Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
							scoreGeneral = graph.pairscores.computePairCompatibilityScore_general(p, null, collectionOfVerticesToMerge, false);
						}
						
						if (graphSmallEnoughToTestDifferentAlgorithms(graph))
						{
							Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
							int scoreFalse = haveToUseGeneralisedComputation?scoreGeneral:graph.pairscores.computePairCompatibilityScore_general(p, null, collectionOfVerticesToMerge, false);// only compute the generalised score here if required
							int scoreTrue = graph.pairscores.computePairCompatibilityScore_general(p, null, collectionOfVerticesToMerge, true);
							int scoreOld = new OldPairScoreComputation(graph).computePairCompatibilityScore_general(p, null, collectionOfVerticesToMerge);
							Assert.assertEquals(scoreTrue, scoreOld);// ensures that the old computation gets us the same score
							Assert.assertEquals(scoreTrue, scoreFalse);// ensures that regardless whether we update auxiliary information, the computation still gets us the same score
						}						
						if (haveToUseGeneralisedComputation)
							return scoreGeneral;
						return p.getScore();// return the existing score
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
			initSeq.cross(engineArg.getData());// only returns positive sequences
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