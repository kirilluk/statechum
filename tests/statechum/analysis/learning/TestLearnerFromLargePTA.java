/* Copyright (c) 2012 The University of Sheffield, UK.
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
package statechum.analysis.learning;

import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeSet;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.Label;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.InitialConfigurationAndData;
import statechum.analysis.learning.experiments.PaperUAS.TracesForSeed.Automaton;
import statechum.analysis.learning.observers.DummyLearner;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

@RunWith(Parameterized.class)
public class TestLearnerFromLargePTA
{
	/*
   	public void recordInitialConfiguration() throws IOException
    {
        PTASequenceEngine engine = collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber);
        
        FileOutputStream log = new java.io.FileOutputStream(largePTAFileName);
        RPNIBlueFringe ourLearner = new RPNIBlueFringe(0,false,null,null,null);
        LearnerGraph automatonAfterInit = ourLearner.getLearner().init(engine,0,0);
        final Configuration shallowCopy = automatonAfterInit.config.copy();shallowCopy.setLearnerCloneGraph(false);
        LearnerGraph copyOfAutomaton = new LearnerGraph(shallowCopy);LearnerGraph.copyGraphs(automatonAfterInit, copyOfAutomaton);
        ourLearner.setInitPta(copyOfAutomaton);
        RecordProgressDecorator recorder = new RecordProgressDecorator(ourLearner.getLearner(),log,1,learnerInitConfiguration.config,true);
        learnerInitConfiguration.graph = new LearnerGraph(learnerInitConfiguration.config);learnerInitConfiguration.testSet = new LinkedList<List<statechum.Label>>();
		recorder.writeLearnerEvaluationData(learnerInitConfiguration);
		recorder.init(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		recorder.close();
		log.close();// double-close in fact (should already been closed by recorder) but it does not really matter.
    }
    /**
	 * @param args trace file to load.
     * @throws IOException 
	 *
	public static void main(String[] args) throws IOException {
		TestLearnerFromLargePTA paper = new TestLearnerFromLargePTA();
    	Configuration config = Configuration.getDefaultConfiguration().copy();config.setDebugMode(false);paper.learnerInitConfiguration.config = config;
    	paper.loadReducedConfigurationFile(args[0]);
    	
		int offset=1;
    	{
        	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]); 
	    	int maxFrame = paper.getMaxFrame(inputFiles);
	    	paper.divisor = (maxFrame+1)/10;// the +1 ensures that the last class of frames includes the last point.
    	}
    	
    	{
        	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]); 
	    	//paper.loadDataByConcatenation(inputFiles);
	    	paper.recordInitialConfiguration();
	    	
    	}
	}
	*/

	/* Running learners gives the following numbers for an i7:
[running large PTA test Learning (true), listOpt.xml completed in 2439 sec
completed comparing.
..Learning (true), listGen.xml completed in 1986 sec
completed comparing.
..Learning (false), listOpt.xml completed in 6705 sec
completed comparing.
...Learning (false), listGen.xml completed in 9262 sec
completed comparing.
...] 

Total time: 20492 sec

*/
   	
	/** Makes it possible to run the same learner with different choices of pairs to see how it affects the outcome. 
	 * It is possible not only to record pair selection but also to replay the one recorded before. 
	 */
	public static class RPNIBlueFringeTestVariability
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
		public RPNIBlueFringeTestVariability(final LearnerEvaluationConfiguration evaluationConfiguration, boolean optimisedMerge, final List<PairOfPaths> lw, final List<PairOfPaths> lc) 
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
						// Use the old and limited version to compute the merge because the general one is too slow on large graphs and we do not need either to merge arbitrary states or to handle "incompatibles".
						outcome = MergeStates.mergeAndDeterminize(original, pair);
					else
					{
						Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
						long score = original.pairscores.computePairCompatibilityScore_general(pair,null,mergedVertices);
						outcome = MergeStates.mergeCollectionOfVertices(original,pair.getR(),mergedVertices);
						
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
						public CmpVertex selectRedNode(LearnerGraph coregraph,
								@SuppressWarnings("unused") Collection<CmpVertex> reds,
								Collection<CmpVertex> tentativeRedNodes) 
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
						public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<PairScore> pairs) {
							return null;// do not resolve in any way
						}});
					
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
					else
					{
						Set<Label> alphabet = graph.pathroutines.computeAlphabet();
						// Create a state to ensure that the entire alphabet is visible when if-then automata are loaded.
						phantomVertex = graph.nextID(true);
						CmpVertex dummyState = AbstractLearnerGraph.generateNewCmpVertex(phantomVertex, bfLearnerInitConfiguration.config);
						Map<Label,CmpVertex> row = graph.createNewRow();
						for(Label lbl:alphabet) graph.addTransition(row, lbl, dummyState);
						graph.transitionMatrix.put(dummyState, row);
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

	public TestLearnerFromLargePTA(Boolean merger, String pairs,Configuration.STATETREE matrixType)
   	{
   		pairsToUse = pairs;mergerToUse = merger;matrixToUse = matrixType;
   	}
	
   	public static String mergerTypeToXml(boolean optimizedMerge)
   	{
   		if (optimizedMerge) return "listOpt.xml";
   		return "listGen.xml";
   	}
   	
	@Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		for(Configuration.STATETREE matrixType:new Configuration.STATETREE []{Configuration.STATETREE.STATETREE_LINKEDHASH, Configuration.STATETREE.STATETREE_ARRAY})
			for(boolean merger:new boolean[]{true,false})
				for(boolean pairsFromMerger:new boolean[]{true,false})
					result.add(new Object[]{merger,mergerTypeToXml(pairsFromMerger), matrixType});
		
		return result;
	}

	public static String parametersToString(Boolean merger, String pairsFile,Configuration.STATETREE matrixType)
	{
		return "merger:"+(merger?"fast":"general")+", "+matrixType+" , "+pairsFile;
	}

	/** Name of the file containing pairs to be chosen from those determined by ChooseStatePairs. */
	final String pairsToUse;
	/** Whether to use an old merger that relies on merging a PTA into an automaton or a general one that mergers an arbitrary pairs of states. */ 
	final boolean mergerToUse;
	/** The kind of transition matrix to create. */
	final Configuration.STATETREE matrixToUse;
	
	@Test
	public void runCompareTwoLearners() throws IOException
    {
		Transform.InternStringLabel converter = new Transform.InternStringLabel();
		InitialConfigurationAndData initialConfigurationData = PairQualityLearner.loadInitialAndPopulateInitialConfiguration(PairQualityLearner.largePTAFileName, STATETREE.STATETREE_ARRAY, converter);
		
		Configuration learnerConf = initialConfigurationData.learnerInitConfiguration.config.copy();learnerConf.setTransitionMatrixImplType(matrixToUse);
		initialConfigurationData.learnerInitConfiguration.config = learnerConf;// update the initial configuration with the one we shall use during learning.
        FileReader listOptReader = new FileReader(PairQualityLearner.largePTALogsDir+pairsToUse);
        List<PairOfPaths> listOpt=PairOfPaths.readPairs(listOptReader, initialConfigurationData.learnerInitConfiguration.config,converter);
        listOptReader.close();

        long tmStarted = new Date().getTime();
        LearnerGraph graphD=new RPNIBlueFringeTestVariability(initialConfigurationData.learnerInitConfiguration,mergerToUse,null,listOpt).learn(initialConfigurationData.initial.graph);
        long tmFinished = new Date().getTime();
        System.out.println("Learning ("+mergerToUse+"), "+pairsToUse+" completed in "+((tmFinished-tmStarted)/1000)+" sec");tmStarted = tmFinished;
        String outcomeName = PairQualityLearner.largePTALogsDir+"outcome_"+pairsToUse;
        //graphD.storage.writeGraphML(outcomeName);
        LearnerGraph referenceA = new LearnerGraph(initialConfigurationData.learnerInitConfiguration.config);AbstractPersistence.loadGraph(outcomeName, referenceA, converter);
        Assert.assertEquals(matrixToUse,graphD.config.getTransitionMatrixImplType());
        DifferentFSMException diff = WMethod.checkM(referenceA, graphD);
        if (diff != null)
        	throw diff;
    }
}
