package statechum.analysis.learning.experiments.PairSelection;

import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Stack;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.Label;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.MarkovModel.MarkovOutcome;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

public class TestLearnFromTracesUsingMarkov {
	static class InconsistencyComputation implements statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure
	{
		protected MarkovModel Markov;
		protected ConsistencyChecker checker;
		
		public InconsistencyComputation(MarkovModel model, ConsistencyChecker c)
		{
			Markov = model;checker = c;
		}
		
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
		
		long inconsistencyFromAnEarlierIteration = 0;
		LearnerGraph coregraph = null;
		LearnerGraph extendedGraph = null;
		MarkovClassifier cl=null;
		LearnerGraphND inverseGraph = null;
		long currentMillis = System.currentTimeMillis();
		
		@Override
		public void initComputation(LearnerGraph graph) 
		{
			coregraph = graph;
					 				
			long value = MarkovClassifier.computeInconsistency(coregraph, Markov, checker,null, false);
			inconsistencyFromAnEarlierIteration=value;
			cl = new MarkovClassifier(Markov, coregraph);
		    extendedGraph = cl.constructMarkovTentative();
			inverseGraph = (LearnerGraphND)MarkovClassifier.computeInverseGraph(coregraph,true);
			long newMillis = System.currentTimeMillis();
			System.out.println(""+(newMillis-currentMillis)/1000+" step, current inconsistency = "+value);
		}
		
		@Override
		public long overrideScoreComputation(PairScore p) 
		{
			if(p.getQ().isAccept()==false && p.getR().isAccept()==false)
				return 0;
			long score=p.getScore();						
			long currentInconsistency = 0;
			LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			int genScore = coregraph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge);
			if (genScore >= 0)
			{			
				LearnerGraph merged = MergeStates.mergeCollectionOfVertices(coregraph, null, verticesToMerge);
				currentInconsistency = MarkovClassifier.computeInconsistency(merged, Markov, checker, null, false)-inconsistencyFromAnEarlierIteration;
				score=genScore-currentInconsistency;	

				long fastInconsistency = MarkovClassifier.computeInconsistencyOfAMerger(coregraph, verticesToMerge, merged, Markov, cl, checker);						
				Assert.assertEquals(currentInconsistency,fastInconsistency);
			}
			return score;
		}

		/** This one returns a set of transitions in all directions. */
		@Override
		public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(CmpVertex currentRed) 
		{
			return	MarkovPassivePairSelection.obtainSurroundingTransitions(coregraph,inverseGraph,currentRed);
		}
	}
	
	@Test
	public void testInconsistencyComputation() throws IncompatibleStatesException, IOException {
		final int traceQuantity=10;
		final double traceLengthMultiplier=1;
		final double alphabetMultiplier=2;
		final int minStateNumber = 30;
		final int chunkLen = 3;

		Configuration config = Configuration.getDefaultConfiguration().copy();config.setAskQuestions(false);config.setDebugMode(false);config.setGdLowToHighRatio(0.7);config.setRandomPathAttemptFudgeThreshold(1000);
		config.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);config.setLearnerScoreMode(ScoreMode.ONLYOVERRIDE);
		ConvertALabel converter = new Transform.InternStringLabel();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		
		final int states=minStateNumber;
		final int alphabet = (int)(alphabetMultiplier*states);
		final int seed = traceQuantity;
		
		LearnerGraph referenceGraph = null;
		MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
		referenceGraph = mg.nextMachine(alphabet,seed, config, converter).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
		
		LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);learnerEval.setLabelConverter(converter);
		//final Collection<List<Label>> testSet = PaperUAS.computeEvaluationSet(referenceGraph,states*3,PairQualityLearner.makeEven(states*alphabet*traceMultiplier));

		final int attempt=0;
		LearnerGraph pta = new LearnerGraph(config);
		RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(attempt),5,null);
		final int tracesToGenerate = PairQualityLearner.makeEven(traceQuantity);
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

				pta.paths.augmentPTA(generator.getAllSequences(0).filter(new FilterPredicate() {
					@Override
					public boolean shouldBeReturned(Object name) {
						return ((statechum.analysis.learning.rpnicore.RandomPathGenerator.StateName)name).accept;
					}
				}));
	
			List<List<Label>> sPlus = generator.getAllSequences(0).getData(new FilterPredicate() {
				@Override
				public boolean shouldBeReturned(Object name) {
					return ((statechum.analysis.learning.rpnicore.RandomPathGenerator.StateName)name).accept;
				}
			});
			assert sPlus.size() > 0;
			final MarkovModel m= new MarkovModel(chunkLen,true,true);

			new MarkovClassifier(m, pta).updateMarkov(false);
			pta.clearColours();

			assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
			
			final Configuration deepCopy = pta.config.copy();deepCopy.setLearnerCloneGraph(true);
			LearnerGraph ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);

			final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();
			//long inconsistencyForTheReferenceGraph = MarkovClassifier.computeInconsistency(trimmedReference, m, checker,false);

			MarkovClassifier ptaClassifier = new MarkovClassifier(m,pta);
			final List<List<Label>> pathsToMerge=ptaClassifier.identifyPathsToMerge(checker);
			// These vertices are merged first and then the learning start from the root as normal.
			// The reason to learn from the root is a memory cost. if we learn from the middle, we can get a better results
			//final Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA=ptaClassifier.buildVerticesToMergeForPaths(pathsToMerge);

			
			List<StatePair> pairsListInitialMerge = ptaClassifier.buildVerticesToMergeForPath(pathsToMerge);
			LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMergeInitialMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			int scoreInitialMerge = pta.pairscores.computePairCompatibilityScore_general(null, pairsListInitialMerge, verticesToMergeInitialMerge);
			assert scoreInitialMerge >= 0;
			final LearnerGraph ptaAfterInitialMerge = MergeStates.mergeCollectionOfVertices(pta, null, verticesToMergeInitialMerge);
			final CmpVertex vertexWithMostTransitions = MarkovPassivePairSelection.findVertexWithMostTransitions(ptaAfterInitialMerge,MarkovClassifier.computeInverseGraph(pta));
			ptaAfterInitialMerge.clearColours();ptaAfterInitialMerge.getInit().setColour(null);vertexWithMostTransitions.setColour(JUConstants.RED);
			ptaClassifier = new MarkovClassifier(m,ptaAfterInitialMerge);// rebuild the classifier
			LearnerGraphND inverseOfPtaAfterInitialMerge = MarkovClassifier.computeInverseGraph(ptaAfterInitialMerge);
			//System.out.println("Centre vertex: "+vertexWithMostTransitions+" "+MarkovPassivePairSelection.countTransitions(ptaAfterInitialMerge, inverseOfPtaAfterInitialMerge, vertexWithMostTransitions));
			
			// These have been recorded, there is no specific reason for the numbers to be as they are. We need them for regression testing.
			Assert.assertEquals(6425,vertexWithMostTransitions.getIntegerID());
			Assert.assertTrue(vertexWithMostTransitions.isAccept());
			Assert.assertEquals(23, MarkovPassivePairSelection.countTransitions(ptaAfterInitialMerge, inverseOfPtaAfterInitialMerge, vertexWithMostTransitions));
			
			// Check that Markov matrix is the right one
			Map<String,MarkovOutcome> map = new TreeMap<String,MarkovOutcome>();for(Entry<List<Label>, MarkovOutcome> entry:m.computePredictionMatrix().entrySet()) map.put(entry.getKey().toString(), entry.getValue()); 
			Assert.assertEquals("{[L1, L1, L17]=(+), [L1, L1, L49]=(+), [L1, L1, L53]=(+), [L1, L1, L56]=(+), [L1, L13, L21]=(+), [L1, L13, L2]=(+), [L1, L13, L59]=(+), [L1, L13]=(+), [L1, L17, L41]=(+), [L1, L17]=(+), [L1, L19, L45]=(+), [L1, L19, L55]=(+), [L1, L19]=(+), [L1, L1]=(+), [L1, L25, L13]=(+), [L1, L25, L19]=(+), [L1, L25, L25]=(+), [L1, L25]=(+), [L1, L28, L14]=(+), [L1, L28, L32]=(+), [L1, L28, L54]=(+), [L1, L28]=(+), [L1, L29, L23]=(+), [L1, L29, L45]=(+), [L1, L29]=(+), [L1, L49, L17]=(+), [L1, L49, L4]=(+), [L1, L49, L51]=(+), [L1, L49, L55]=(+), [L1, L49]=(+), [L1, L53, L13]=(+), [L1, L53, L21]=(+), [L1, L53, L23]=(+), [L1, L53, L34]=(+), [L1, L53, L3]=(+), [L1, L53, L40]=(+), [L1, L53, L44]=(+), [L1, L53, L45]=(+), [L1, L53, L50]=(+), [L1, L53, L51]=(+), [L1, L53, L54]=(+), [L1, L53]=(+), [L1, L56, L53]=(+), [L1, L56]=(+), [L11, L13, L49]=(+), [L11, L13]=(+), [L11, L21, L9]=(+), [L11, L21]=(+), [L11, L3, L17]=(+), [L11, L3, L4]=(+), [L11, L3, L51]=(+), [L11, L34, L1]=(+), [L11, L34, L4]=(+), [L11, L34]=(+), [L11, L3]=(+), [L11, L40, L19]=(+), [L11, L40]=(+), [L11, L44, L29]=(+), [L11, L44]=(+), [L11, L50, L53]=(+), [L11, L50]=(+), [L11, L51, L30]=(+), [L11, L51, L32]=(+), [L11, L51, L34]=(+), [L11, L51, L41]=(+), [L11, L51, L55]=(+), [L11, L51, L57]=(+), [L11, L51]=(+), [L11]=(+), [L13, L1, L1]=(+), [L13, L1, L29]=(+), [L13, L13, L1]=(+), [L13, L13, L45]=(+), [L13, L13]=(+), [L13, L1]=(+), [L13, L2, L11]=(+), [L13, L2, L35]=(+), [L13, L2, L45]=(+), [L13, L21, L37]=(+), [L13, L21, L40]=(+), [L13, L21]=(+), [L13, L24, L53]=(+), [L13, L24]=(+), [L13, L2]=(+), [L13, L3, L28]=(+), [L13, L33, L53]=(+), [L13, L33]=(+), [L13, L3]=(+), [L13, L41, L40]=(+), [L13, L41]=(+), [L13, L44, L32]=(+), [L13, L44]=(+), [L13, L45, L53]=(+), [L13, L45]=(+), [L13, L47, L2]=(+), [L13, L47]=(+), [L13, L49, L53]=(+), [L13, L49]=(+), [L13, L59, L1]=(+), [L13, L59, L29]=(+), [L13, L59]=(+), [L13, L9, L53]=(+), [L13, L9]=(+), [L13]=(+), [L14, L29, L21]=(+), [L14, L29, L2]=(+), [L14, L29, L49]=(+), [L14, L29, L59]=(+), [L14, L29]=(+), [L14, L39, L33]=(+), [L14, L39, L3]=(+), [L14, L39, L41]=(+), [L14, L39, L44]=(+), [L14, L39, L47]=(+), [L14, L39, L55]=(+), [L14, L39]=(+), [L14, L55, L17]=(+), [L14, L55, L28]=(+), [L14, L55, L53]=(+), [L14, L55, L56]=(+), [L14, L55]=(+), [L14, L59, L53]=(+), [L14, L59]=(+), [L14]=(+), [L16, L2, L13]=(+), [L16, L2, L21]=(+), [L16, L2, L34]=(+), [L16, L2, L3]=(+), [L16, L2, L40]=(+), [L16, L2, L44]=(+), [L16, L2, L50]=(+), [L16, L2, L51]=(+), [L16, L2, L54]=(+), [L16, L26, L53]=(+), [L16, L26]=(+), [L16, L27, L29]=(+), [L16, L27, L39]=(+), [L16, L27, L55]=(+), [L16, L27, L59]=(+), [L16, L27]=(+), [L16, L2]=(+), [L16, L35, L17]=(+), [L16, L35, L28]=(+), [L16, L35, L49]=(+), [L16, L35, L53]=(+), [L16, L35, L56]=(+), [L16, L35]=(+), [L16, L40, L33]=(+), [L16, L40, L3]=(+), [L16, L40, L41]=(+), [L16, L40, L44]=(+), [L16, L40, L47]=(+), [L16, L40, L55]=(+), [L16, L40]=(+), [L16, L44, L2]=(+), [L16, L44, L59]=(+), [L16, L44, L5]=(+), [L16, L44]=(+), [L16, L54, L13]=(+), [L16, L54, L1]=(+), [L16, L54, L24]=(+), [L16, L54, L45]=(+), [L16, L54, L9]=(+), [L16, L54]=(+), [L16]=(+), [L17, L3, L56]=(+), [L17, L33, L53]=(+), [L17, L33]=(+), [L17, L3]=(+), [L17, L41, L13]=(+), [L17, L41, L21]=(+), [L17, L41, L30]=(+), [L17, L41, L31]=(+), [L17, L41, L32]=(+), [L17, L41, L34]=(+), [L17, L41, L41]=(+), [L17, L41, L51]=(+), [L17, L41, L54]=(+), [L17, L41, L55]=(+), [L17, L41, L56]=(+), [L17, L41, L57]=(+), [L17, L41]=(+), [L17, L44, L14]=(+), [L17, L44, L32]=(+), [L17, L44, L54]=(+), [L17, L44]=(+), [L17, L45, L45]=(+), [L17, L45, L55]=(+), [L17, L45]=(+), [L17, L47, L49]=(+), [L17, L47, L59]=(+), [L17, L47]=(+), [L17, L55, L1]=(+), [L17, L55, L29]=(+), [L17, L55, L39]=(+), [L17, L55, L55]=(+), [L17, L55, L59]=(+), [L17, L55]=(+), [L17]=(+), [L19, L13, L49]=(+), [L19, L13]=(+), [L19, L19, L45]=(+), [L19, L19, L55]=(+), [L19, L19]=(+), [L19, L25, L13]=(+), [L19, L25, L19]=(+), [L19, L25]=(+), [L19, L45, L45]=(+), [L19, L45, L55]=(+), [L19, L45]=(+), [L19, L55, L29]=(+), [L19, L55, L39]=(+), [L19, L55, L55]=(+), [L19, L55, L59]=(+), [L19, L55]=(+), [L19]=(+), [L1]=(+), [L2, L11, L34]=(+), [L2, L11, L3]=(+), [L2, L11, L40]=(+), [L2, L11, L44]=(+), [L2, L11, L50]=(+), [L2, L11, L51]=(+), [L2, L11]=(+), [L2, L13, L2]=(+), [L2, L13, L49]=(+), [L2, L13, L59]=(+), [L2, L13]=(+), [L2, L21, L3]=(+), [L2, L21, L9]=(+), [L2, L21]=(+), [L2, L25, L26]=(+), [L2, L25, L27]=(+), [L2, L25, L2]=(+), [L2, L25, L35]=(+), [L2, L25, L40]=(+), [L2, L25, L44]=(+), [L2, L25, L54]=(+), [L2, L25]=(+), [L2, L3, L17]=(+), [L2, L3, L4]=(+), [L2, L3, L51]=(+), [L2, L3, L55]=(+), [L2, L34, L17]=(+), [L2, L34, L1]=(+), [L2, L34, L4]=(+), [L2, L34]=(+), [L2, L35, L11]=(+), [L2, L35, L35]=(+), [L2, L35, L45]=(+), [L2, L35]=(+), [L2, L3]=(+), [L2, L40, L19]=(+), [L2, L40, L37]=(+), [L2, L40]=(+), [L2, L44, L1]=(+), [L2, L44, L29]=(+), [L2, L44]=(+), [L2, L45, L33]=(+), [L2, L45, L3]=(+), [L2, L45, L41]=(+), [L2, L45, L44]=(+), [L2, L45, L55]=(+), [L2, L45]=(+), [L2, L50, L53]=(+), [L2, L50]=(+), [L2, L51, L32]=(+), [L2, L51, L41]=(+), [L2, L51, L51]=(+), [L2, L51, L54]=(+), [L2, L51, L56]=(+), [L2, L51]=(+), [L2, L54, L41]=(+), [L2, L54]=(+), [L21, L3, L1]=(+), [L21, L3, L24]=(+), [L21, L37, L13]=(+), [L21, L37, L19]=(+), [L21, L37, L25]=(+), [L21, L37]=(+), [L21, L3]=(+), [L21, L40, L45]=(+), [L21, L40, L55]=(+), [L21, L40]=(+), [L21, L9, L17]=(+), [L21, L9, L1]=(+), [L21, L9, L4]=(+), [L21, L9]=(+), [L21]=(+), [L23, L53, L23]=(+), [L23, L53, L45]=(+), [L23, L53]=(+), [L23]=(+), [L24, L2, L25]=(+), [L24, L2]=(+), [L24, L5, L8]=(+), [L24, L53, L23]=(+), [L24, L53, L45]=(+), [L24, L53]=(+), [L24, L59, L33]=(+), [L24, L59, L41]=(+), [L24, L59, L55]=(+), [L24, L59]=(+), [L24, L5]=(+), [L24]=(+), [L25, L13, L21]=(+), [L25, L13, L2]=(+), [L25, L13, L49]=(+), [L25, L13, L59]=(+), [L25, L13]=(+), [L25, L19, L45]=(+), [L25, L19, L55]=(+), [L25, L19]=(+), [L25, L2, L13]=(+), [L25, L2, L21]=(+), [L25, L2, L34]=(+), [L25, L2, L3]=(+), [L25, L2, L40]=(+), [L25, L2, L54]=(+), [L25, L25, L13]=(+), [L25, L25, L19]=(+), [L25, L25, L25]=(+), [L25, L25]=(+), [L25, L26, L53]=(+), [L25, L26]=(+), [L25, L27, L29]=(+), [L25, L27, L39]=(+), [L25, L27, L55]=(+), [L25, L27, L59]=(+), [L25, L27]=(+), [L25, L2]=(+), [L25, L35, L17]=(+), [L25, L35, L28]=(+), [L25, L35, L49]=(+), [L25, L35, L53]=(+), [L25, L35, L56]=(+), [L25, L35]=(+), [L25, L40, L33]=(+), [L25, L40, L3]=(+), [L25, L40, L41]=(+), [L25, L40, L44]=(+), [L25, L40, L47]=(+), [L25, L40, L55]=(+), [L25, L40]=(+), [L25, L44, L2]=(+), [L25, L44, L59]=(+), [L25, L44, L5]=(+), [L25, L44]=(+), [L25, L54, L13]=(+), [L25, L54, L1]=(+), [L25, L54, L24]=(+), [L25, L54, L45]=(+), [L25, L54, L9]=(+), [L25, L54]=(+), [L25]=(+), [L26, L53, L23]=(+), [L26, L53, L45]=(+), [L26, L53]=(+), [L26]=(+), [L27, L28, L28]=(+), [L27, L28]=(+), [L27, L29, L21]=(+), [L27, L29, L2]=(+), [L27, L29, L59]=(+), [L27, L29]=(+), [L27, L39, L33]=(+), [L27, L39, L3]=(+), [L27, L39, L41]=(+), [L27, L39, L47]=(+), [L27, L39, L55]=(+), [L27, L39]=(+), [L27, L55, L17]=(+), [L27, L55, L28]=(+), [L27, L55, L49]=(+), [L27, L55, L53]=(+), [L27, L55, L56]=(+), [L27, L55]=(+), [L27, L59, L53]=(+), [L27, L59]=(+), [L27, L8, L1]=(+), [L27, L8]=(+), [L27]=(+), [L28, L14, L29]=(+), [L28, L14, L39]=(+), [L28, L14, L55]=(+), [L28, L14, L59]=(+), [L28, L14]=(+), [L28, L28, L28]=(+), [L28, L28, L8]=(+), [L28, L28]=(+), [L28, L32, L30]=(+), [L28, L32, L31]=(+), [L28, L32, L32]=(+), [L28, L32, L34]=(+), [L28, L32, L41]=(+), [L28, L32, L51]=(+), [L28, L32, L54]=(+), [L28, L32, L55]=(+), [L28, L32, L56]=(+), [L28, L32, L57]=(+), [L28, L32]=(+), [L28, L54, L35]=(+), [L28, L54, L57]=(+), [L28, L54]=(+), [L28, L8, L29]=(+), [L28, L8]=(+), [L28]=(+), [L29, L2, L11]=(+), [L29, L2, L35]=(+), [L29, L2, L45]=(+), [L29, L21, L37]=(+), [L29, L21, L40]=(+), [L29, L21]=(+), [L29, L23, L53]=(+), [L29, L23]=(+), [L29, L2]=(+), [L29, L45, L8]=(+), [L29, L45]=(+), [L29, L49, L53]=(+), [L29, L49]=(+), [L29, L59, L1]=(+), [L29, L59, L29]=(+), [L29, L59]=(+), [L29]=(+), [L2]=(+), [L3, L1, L53]=(+), [L3, L17, L33]=(+), [L3, L17, L41]=(+), [L3, L17, L44]=(+), [L3, L17, L47]=(+), [L3, L17]=(+), [L3, L1]=(+), [L3, L24, L2]=(+), [L3, L24, L59]=(+), [L3, L24, L5]=(+), [L3, L24]=(+), [L3, L28, L14]=(+), [L3, L28, L32]=(+), [L3, L28, L54]=(+), [L3, L28]=(+), [L3, L4, L14]=(+), [L3, L4, L32]=(+), [L3, L4, L54]=(+), [L3, L49, L17]=(+), [L3, L49, L4]=(+), [L3, L49, L51]=(+), [L3, L49, L55]=(+), [L3, L49]=(+), [L3, L4]=(+), [L3, L51, L29]=(+), [L3, L51, L39]=(+), [L3, L51, L55]=(+), [L3, L51, L59]=(+), [L3, L51]=(+), [L3, L53, L13]=(+), [L3, L53, L34]=(+), [L3, L53, L3]=(+), [L3, L53, L40]=(+), [L3, L53, L44]=(+), [L3, L53, L50]=(+), [L3, L53, L51]=(+), [L3, L53, L54]=(+), [L3, L53]=(+), [L3, L55, L53]=(+), [L3, L55]=(+), [L3, L56, L53]=(+), [L3, L56]=(+), [L30, L3, L53]=(+), [L30, L3, L56]=(+), [L30, L33, L53]=(+), [L30, L33]=(+), [L30, L3]=(+), [L30, L41, L51]=(+), [L30, L41, L54]=(+), [L30, L41]=(+), [L30, L44, L32]=(+), [L30, L44, L54]=(+), [L30, L44]=(+), [L30, L47, L2]=(+), [L30, L47, L49]=(+), [L30, L47]=(+), [L30, L55, L1]=(+), [L30, L55, L29]=(+), [L30, L55]=(+), [L30]=(+), [L31, L17, L33]=(+), [L31, L17, L44]=(+), [L31, L17, L47]=(+), [L31, L17, L55]=(+), [L31, L17]=(+), [L31, L4, L14]=(+), [L31, L4, L32]=(+), [L31, L41, L31]=(+), [L31, L41, L41]=(+), [L31, L41, L54]=(+), [L31, L41]=(+), [L31, L4]=(+), [L31, L51, L39]=(+), [L31, L51, L55]=(+), [L31, L51, L59]=(+), [L31, L51]=(+), [L31, L55, L53]=(+), [L31, L55]=(+), [L31]=(+), [L32, L1, L17]=(+), [L32, L1, L28]=(+), [L32, L1, L56]=(+), [L32, L1]=(+), [L32, L29, L23]=(+), [L32, L29, L45]=(+), [L32, L29]=(+), [L32, L30, L33]=(+), [L32, L30, L41]=(+), [L32, L30, L44]=(+), [L32, L30, L47]=(+), [L32, L30, L55]=(+), [L32, L30]=(+), [L32, L31, L17]=(+), [L32, L31, L4]=(+), [L32, L31, L51]=(+), [L32, L31, L55]=(+), [L32, L31]=(+), [L32, L32, L1]=(+), [L32, L32, L29]=(+), [L32, L32]=(+), [L32, L34, L2]=(+), [L32, L34, L49]=(+), [L32, L34, L59]=(+), [L32, L34]=(+), [L32, L41, L17]=(+), [L32, L41, L4]=(+), [L32, L41]=(+), [L32, L51, L3]=(+), [L32, L51, L9]=(+), [L32, L51]=(+), [L32, L54, L17]=(+), [L32, L54, L28]=(+), [L32, L54, L53]=(+), [L32, L54]=(+), [L32, L55, L39]=(+), [L32, L55, L59]=(+), [L32, L55]=(+), [L32, L56, L26]=(+), [L32, L56, L2]=(+), [L32, L56, L35]=(+), [L32, L56, L40]=(+), [L32, L56, L44]=(+), [L32, L56, L54]=(+), [L32, L56]=(+), [L32, L57, L13]=(+), [L32, L57, L27]=(+), [L32, L57, L31]=(+), [L32, L57, L52]=(+), [L32, L57]=(+), [L32]=(+), [L33, L53, L23]=(+), [L33, L53, L45]=(+), [L33, L53]=(+), [L33]=(+), [L34, L1, L13]=(+), [L34, L1, L19]=(+), [L34, L1, L25]=(+), [L34, L17, L45]=(+), [L34, L17, L55]=(+), [L34, L17]=(+), [L34, L1]=(+), [L34, L2, L11]=(+), [L34, L2, L35]=(+), [L34, L21, L37]=(+), [L34, L21, L40]=(+), [L34, L21]=(+), [L34, L2]=(+), [L34, L4, L21]=(+), [L34, L4, L2]=(+), [L34, L4, L49]=(+), [L34, L4, L59]=(+), [L34, L49, L53]=(+), [L34, L49]=(+), [L34, L4]=(+), [L34, L59, L1]=(+), [L34, L59, L29]=(+), [L34, L59]=(+), [L34]=(+), [L35, L11, L13]=(+), [L35, L11, L21]=(+), [L35, L11, L34]=(+), [L35, L11, L40]=(+), [L35, L11, L44]=(+), [L35, L11, L51]=(+), [L35, L11]=(+), [L35, L17, L41]=(+), [L35, L17]=(+), [L35, L28, L14]=(+), [L35, L28, L32]=(+), [L35, L28, L54]=(+), [L35, L28]=(+), [L35, L35, L11]=(+), [L35, L35, L35]=(+), [L35, L35, L45]=(+), [L35, L35]=(+), [L35, L45, L3]=(+), [L35, L45, L41]=(+), [L35, L45, L47]=(+), [L35, L45]=(+), [L35, L49, L17]=(+), [L35, L49, L4]=(+), [L35, L49, L51]=(+), [L35, L49, L55]=(+), [L35, L49]=(+), [L35, L53, L13]=(+), [L35, L53, L21]=(+), [L35, L53, L34]=(+), [L35, L53, L3]=(+), [L35, L53, L40]=(+), [L35, L53, L44]=(+), [L35, L53, L50]=(+), [L35, L53, L51]=(+), [L35, L53, L54]=(+), [L35, L53]=(+), [L35, L56, L53]=(+), [L35, L56]=(+), [L35, L8, L16]=(+), [L35, L8, L1]=(+), [L35, L8, L44]=(+), [L35, L8]=(+), [L35]=(+), [L37, L13, L21]=(+), [L37, L13, L2]=(+), [L37, L13, L49]=(+), [L37, L13, L59]=(+), [L37, L13]=(+), [L37, L19, L45]=(+), [L37, L19, L55]=(+), [L37, L19]=(+), [L37, L25, L13]=(+), [L37, L25, L19]=(+), [L37, L25, L25]=(+), [L37, L25]=(+), [L37, L52, L52]=(+), [L37, L52, L53]=(+), [L37, L52]=(+), [L37, L53, L2]=(+), [L37, L53, L59]=(+), [L37, L53, L5]=(+), [L37, L53]=(+), [L37]=(+), [L39, L3, L17]=(+), [L39, L3, L28]=(+), [L39, L3, L49]=(+), [L39, L3, L53]=(+), [L39, L3, L56]=(+), [L39, L33, L53]=(+), [L39, L33]=(+), [L39, L3]=(+), [L39, L41, L13]=(+), [L39, L41, L44]=(+), [L39, L41, L54]=(+), [L39, L41]=(+), [L39, L44, L14]=(+), [L39, L44, L32]=(+), [L39, L44, L54]=(+), [L39, L44]=(+), [L39, L47, L21]=(+), [L39, L47, L2]=(+), [L39, L47, L49]=(+), [L39, L47, L59]=(+), [L39, L47]=(+), [L39, L55, L1]=(+), [L39, L55, L29]=(+), [L39, L55]=(+), [L39]=(+), [L3]=(+), [L4, L14, L29]=(+), [L4, L14, L39]=(+), [L4, L14, L55]=(+), [L4, L14, L59]=(+), [L4, L14]=(+), [L4, L2, L11]=(+), [L4, L2, L45]=(+), [L4, L21, L37]=(+), [L4, L21, L40]=(+), [L4, L21]=(+), [L4, L2]=(+), [L4, L32, L30]=(+), [L4, L32, L31]=(+), [L4, L32, L32]=(+), [L4, L32, L34]=(+), [L4, L32, L41]=(+), [L4, L32, L55]=(+), [L4, L32, L56]=(+), [L4, L32, L57]=(+), [L4, L32]=(+), [L4, L49, L53]=(+), [L4, L49]=(+), [L4, L54, L35]=(+), [L4, L54, L57]=(+), [L4, L54]=(+), [L4, L59, L1]=(+), [L4, L59, L29]=(+), [L4, L59]=(+), [L40, L19, L13]=(+), [L40, L19, L19]=(+), [L40, L19, L25]=(+), [L40, L19]=(+), [L40, L3, L17]=(+), [L40, L3, L49]=(+), [L40, L3, L53]=(+), [L40, L3, L56]=(+), [L40, L33, L53]=(+), [L40, L33]=(+), [L40, L37, L52]=(+), [L40, L37, L53]=(+), [L40, L37]=(+), [L40, L3]=(+), [L40, L41, L13]=(+), [L40, L41, L34]=(+), [L40, L41, L3]=(+), [L40, L41, L40]=(+), [L40, L41, L44]=(+), [L40, L41, L50]=(+), [L40, L41, L51]=(+), [L40, L41, L54]=(+), [L40, L41]=(+), [L40, L44, L14]=(+), [L40, L44, L32]=(+), [L40, L44, L54]=(+), [L40, L44]=(+), [L40, L45, L45]=(+), [L40, L45, L55]=(+), [L40, L45]=(+), [L40, L47, L21]=(+), [L40, L47, L2]=(+), [L40, L47, L49]=(+), [L40, L47, L59]=(+), [L40, L47]=(+), [L40, L55, L1]=(+), [L40, L55, L29]=(+), [L40, L55, L39]=(+), [L40, L55, L55]=(+), [L40, L55, L59]=(+), [L40, L55]=(+), [L40]=(+), [L41, L1, L13]=(+), [L41, L1, L19]=(+), [L41, L1, L25]=(+), [L41, L13, L21]=(+), [L41, L13, L2]=(+), [L41, L13, L49]=(+), [L41, L13, L59]=(+), [L41, L13]=(+), [L41, L17, L45]=(+), [L41, L17, L55]=(+), [L41, L17]=(+), [L41, L1]=(+), [L41, L21, L3]=(+), [L41, L21, L9]=(+), [L41, L21]=(+), [L41, L3, L4]=(+), [L41, L3, L51]=(+), [L41, L3, L55]=(+), [L41, L30, L33]=(+), [L41, L30, L3]=(+), [L41, L30, L41]=(+), [L41, L30, L44]=(+), [L41, L30, L55]=(+), [L41, L30]=(+), [L41, L31, L17]=(+), [L41, L31, L4]=(+), [L41, L31, L51]=(+), [L41, L31, L55]=(+), [L41, L31]=(+), [L41, L32, L1]=(+), [L41, L32, L29]=(+), [L41, L32]=(+), [L41, L34, L17]=(+), [L41, L34, L1]=(+), [L41, L34, L21]=(+), [L41, L34, L2]=(+), [L41, L34, L49]=(+), [L41, L34, L4]=(+), [L41, L34, L59]=(+), [L41, L34]=(+), [L41, L3]=(+), [L41, L4, L21]=(+), [L41, L4, L2]=(+), [L41, L4, L49]=(+), [L41, L4, L59]=(+), [L41, L40, L19]=(+), [L41, L40, L37]=(+), [L41, L40]=(+), [L41, L41, L17]=(+), [L41, L41, L1]=(+), [L41, L41, L4]=(+), [L41, L41]=(+), [L41, L44, L1]=(+), [L41, L44, L29]=(+), [L41, L44]=(+), [L41, L4]=(+), [L41, L50, L53]=(+), [L41, L50]=(+), [L41, L51, L30]=(+), [L41, L51, L34]=(+), [L41, L51, L3]=(+), [L41, L51, L41]=(+), [L41, L51, L54]=(+), [L41, L51, L55]=(+), [L41, L51, L9]=(+), [L41, L51]=(+), [L41, L54, L17]=(+), [L41, L54, L28]=(+), [L41, L54, L41]=(+), [L41, L54, L49]=(+), [L41, L54, L56]=(+), [L41, L54]=(+), [L41, L55, L29]=(+), [L41, L55, L39]=(+), [L41, L55, L55]=(+), [L41, L55, L59]=(+), [L41, L55]=(+), [L41, L56, L27]=(+), [L41, L56, L2]=(+), [L41, L56, L35]=(+), [L41, L56, L44]=(+), [L41, L56, L54]=(+), [L41, L56]=(+), [L41, L57, L13]=(+), [L41, L57, L27]=(+), [L41, L57, L52]=(+), [L41, L57]=(+), [L41]=(+), [L44, L1, L17]=(+), [L44, L1, L28]=(+), [L44, L1, L53]=(+), [L44, L1, L56]=(+), [L44, L14, L29]=(+), [L44, L14, L39]=(+), [L44, L14, L55]=(+), [L44, L14, L59]=(+), [L44, L14]=(+), [L44, L1]=(+), [L44, L2, L25]=(+), [L44, L29, L23]=(+), [L44, L29, L45]=(+), [L44, L29]=(+), [L44, L2]=(+), [L44, L32, L30]=(+), [L44, L32, L31]=(+), [L44, L32, L32]=(+), [L44, L32, L34]=(+), [L44, L32, L41]=(+), [L44, L32, L54]=(+), [L44, L32, L56]=(+), [L44, L32, L57]=(+), [L44, L32]=(+), [L44, L5, L8]=(+), [L44, L54, L35]=(+), [L44, L54, L57]=(+), [L44, L54]=(+), [L44, L59, L33]=(+), [L44, L59, L3]=(+), [L44, L59, L41]=(+), [L44, L59, L44]=(+), [L44, L59, L47]=(+), [L44, L59, L55]=(+), [L44, L59]=(+), [L44, L5]=(+), [L44]=(+), [L45, L3, L17]=(+), [L45, L3, L28]=(+), [L45, L3, L49]=(+), [L45, L3, L53]=(+), [L45, L3, L56]=(+), [L45, L33, L53]=(+), [L45, L33]=(+), [L45, L3]=(+), [L45, L41, L21]=(+), [L45, L41, L34]=(+), [L45, L41, L50]=(+), [L45, L41, L51]=(+), [L45, L41, L54]=(+), [L45, L41]=(+), [L45, L44, L54]=(+), [L45, L44]=(+), [L45, L45, L45]=(+), [L45, L45, L55]=(+), [L45, L45]=(+), [L45, L47, L49]=(+), [L45, L47]=(+), [L45, L53, L23]=(+), [L45, L53, L45]=(+), [L45, L53]=(+), [L45, L55, L29]=(+), [L45, L55, L39]=(+), [L45, L55, L55]=(+), [L45, L55, L59]=(+), [L45, L55]=(+), [L45, L8, L16]=(+), [L45, L8, L1]=(+), [L45, L8, L44]=(+), [L45, L8]=(+), [L45]=(+), [L47, L2, L11]=(+), [L47, L2, L35]=(+), [L47, L2, L45]=(+), [L47, L21, L37]=(+), [L47, L21, L40]=(+), [L47, L21]=(+), [L47, L2]=(+), [L47, L49, L53]=(+), [L47, L49]=(+), [L47, L59, L1]=(+), [L47, L59, L29]=(+), [L47, L59]=(+), [L47]=(+), [L49, L17, L33]=(+), [L49, L17, L3]=(+), [L49, L17, L41]=(+), [L49, L17, L44]=(+), [L49, L17, L47]=(+), [L49, L17, L55]=(+), [L49, L17]=(+), [L49, L4, L14]=(+), [L49, L4, L32]=(+), [L49, L4, L54]=(+), [L49, L4]=(+), [L49, L51, L29]=(+), [L49, L51, L39]=(+), [L49, L51, L55]=(+), [L49, L51, L59]=(+), [L49, L51]=(+), [L49, L53, L23]=(+), [L49, L53, L45]=(+), [L49, L53]=(+), [L49, L55, L53]=(+), [L49, L55]=(+), [L49]=(+), [L4]=(+), [L5, L8, L16]=(+), [L5, L8, L1]=(+), [L5, L8, L44]=(+), [L5, L8]=(+), [L50, L53, L23]=(+), [L50, L53, L45]=(+), [L50, L53]=(+), [L50]=(+), [L51, L29, L2]=(+), [L51, L29, L49]=(+), [L51, L29, L59]=(+), [L51, L29]=(+), [L51, L3, L1]=(+), [L51, L3, L24]=(+), [L51, L30, L3]=(+), [L51, L30, L44]=(+), [L51, L30]=(+), [L51, L31, L17]=(+), [L51, L31, L51]=(+), [L51, L31]=(+), [L51, L32, L1]=(+), [L51, L32, L29]=(+), [L51, L32]=(+), [L51, L34, L21]=(+), [L51, L34, L2]=(+), [L51, L34]=(+), [L51, L39, L33]=(+), [L51, L39, L3]=(+), [L51, L39, L44]=(+), [L51, L39, L47]=(+), [L51, L39, L55]=(+), [L51, L39]=(+), [L51, L3]=(+), [L51, L41, L17]=(+), [L51, L41, L1]=(+), [L51, L41]=(+), [L51, L51, L3]=(+), [L51, L51, L9]=(+), [L51, L51]=(+), [L51, L54, L17]=(+), [L51, L54, L28]=(+), [L51, L54, L56]=(+), [L51, L54]=(+), [L51, L55, L17]=(+), [L51, L55, L28]=(+), [L51, L55, L29]=(+), [L51, L55, L49]=(+), [L51, L55, L53]=(+), [L51, L55, L56]=(+), [L51, L55, L59]=(+), [L51, L55]=(+), [L51, L56, L35]=(+), [L51, L56, L40]=(+), [L51, L56, L44]=(+), [L51, L56]=(+), [L51, L57, L13]=(+), [L51, L57]=(+), [L51, L59, L53]=(+), [L51, L59]=(+), [L51, L9, L17]=(+), [L51, L9, L1]=(+), [L51, L9, L4]=(+), [L51, L9]=(+), [L51]=(+), [L52, L51, L29]=(+), [L52, L51, L55]=(+), [L52, L51, L59]=(+), [L52, L51]=(+), [L52, L52, L53]=(+), [L52, L52]=(+), [L52, L53, L2]=(+), [L52, L53, L59]=(+), [L52, L53]=(+), [L52, L55, L53]=(+), [L52, L55]=(+), [L52]=(+), [L53, L13, L21]=(+), [L53, L13, L2]=(+), [L53, L13, L49]=(+), [L53, L13, L59]=(+), [L53, L13]=(+), [L53, L2, L25]=(+), [L53, L21, L3]=(+), [L53, L21, L9]=(+), [L53, L21]=(+), [L53, L23, L53]=(+), [L53, L23]=(+), [L53, L2]=(+), [L53, L3, L17]=(+), [L53, L3, L4]=(+), [L53, L3, L51]=(+), [L53, L3, L55]=(+), [L53, L34, L17]=(+), [L53, L34, L1]=(+), [L53, L34, L4]=(+), [L53, L34]=(+), [L53, L3]=(+), [L53, L40, L19]=(+), [L53, L40, L37]=(+), [L53, L40]=(+), [L53, L44, L1]=(+), [L53, L44, L29]=(+), [L53, L44]=(+), [L53, L45, L8]=(+), [L53, L45]=(+), [L53, L5, L8]=(+), [L53, L50, L53]=(+), [L53, L50]=(+), [L53, L51, L31]=(+), [L53, L51, L34]=(+), [L53, L51, L41]=(+), [L53, L51, L51]=(+), [L53, L51, L54]=(+), [L53, L51, L55]=(+), [L53, L51, L56]=(+), [L53, L51]=(+), [L53, L54, L41]=(+), [L53, L54]=(+), [L53, L59, L3]=(+), [L53, L59, L44]=(+), [L53, L59, L55]=(+), [L53, L59]=(+), [L53, L5]=(+), [L53]=(+), [L54, L1, L1]=(+), [L54, L1, L29]=(+), [L54, L13, L13]=(+), [L54, L13, L1]=(+), [L54, L13, L24]=(+), [L54, L13, L45]=(+), [L54, L13, L9]=(+), [L54, L13]=(+), [L54, L17, L41]=(+), [L54, L17]=(+), [L54, L1]=(+), [L54, L24, L53]=(+), [L54, L24]=(+), [L54, L28, L14]=(+), [L54, L28, L32]=(+), [L54, L28, L54]=(+), [L54, L28]=(+), [L54, L35, L8]=(+), [L54, L35]=(+), [L54, L41, L30]=(+), [L54, L41, L31]=(+), [L54, L41, L32]=(+), [L54, L41, L34]=(+), [L54, L41, L41]=(+), [L54, L41, L51]=(+), [L54, L41, L54]=(+), [L54, L41, L55]=(+), [L54, L41, L56]=(+), [L54, L41, L57]=(+), [L54, L41]=(+), [L54, L45, L53]=(+), [L54, L45]=(+), [L54, L49, L4]=(+), [L54, L49]=(+), [L54, L53, L51]=(+), [L54, L53]=(+), [L54, L56, L53]=(+), [L54, L56]=(+), [L54, L57, L53]=(+), [L54, L57]=(+), [L54, L9, L53]=(+), [L54, L9]=(+), [L54]=(+), [L55, L1, L17]=(+), [L55, L1, L28]=(+), [L55, L1, L49]=(+), [L55, L1, L53]=(+), [L55, L1, L56]=(+), [L55, L17, L41]=(+), [L55, L17]=(+), [L55, L1]=(+), [L55, L28, L14]=(+), [L55, L28, L32]=(+), [L55, L28, L54]=(+), [L55, L28]=(+), [L55, L29, L21]=(+), [L55, L29, L23]=(+), [L55, L29, L2]=(+), [L55, L29, L45]=(+), [L55, L29, L49]=(+), [L55, L29, L59]=(+), [L55, L29]=(+), [L55, L39, L33]=(+), [L55, L39, L3]=(+), [L55, L39, L44]=(+), [L55, L39, L47]=(+), [L55, L39, L55]=(+), [L55, L39]=(+), [L55, L49, L4]=(+), [L55, L49, L51]=(+), [L55, L49, L55]=(+), [L55, L49]=(+), [L55, L53, L13]=(+), [L55, L53, L21]=(+), [L55, L53, L23]=(+), [L55, L53, L34]=(+), [L55, L53, L3]=(+), [L55, L53, L40]=(+), [L55, L53, L45]=(+), [L55, L53, L50]=(+), [L55, L53, L51]=(+), [L55, L53]=(+), [L55, L55, L17]=(+), [L55, L55, L28]=(+), [L55, L55, L49]=(+), [L55, L55, L53]=(+), [L55, L55, L56]=(+), [L55, L55]=(+), [L55, L56, L53]=(+), [L55, L56]=(+), [L55, L59, L53]=(+), [L55, L59]=(+), [L55]=(+), [L56, L2, L21]=(+), [L56, L2, L3]=(+), [L56, L2, L51]=(+), [L56, L26, L53]=(+), [L56, L26]=(+), [L56, L27, L59]=(+), [L56, L27]=(+), [L56, L2]=(+), [L56, L35, L49]=(+), [L56, L35, L53]=(+), [L56, L35, L56]=(+), [L56, L35]=(+), [L56, L40, L44]=(+), [L56, L40, L55]=(+), [L56, L40]=(+), [L56, L44, L2]=(+), [L56, L44, L59]=(+), [L56, L44, L5]=(+), [L56, L44]=(+), [L56, L53, L23]=(+), [L56, L53, L45]=(+), [L56, L53]=(+), [L56, L54, L1]=(+), [L56, L54, L24]=(+), [L56, L54, L45]=(+), [L56, L54, L9]=(+), [L56, L54]=(+), [L56]=(+), [L57, L13, L33]=(+), [L57, L13, L3]=(+), [L57, L13, L41]=(+), [L57, L13, L44]=(+), [L57, L13, L47]=(+), [L57, L13]=(+), [L57, L27, L28]=(+), [L57, L27, L8]=(+), [L57, L27]=(+), [L57, L31, L41]=(+), [L57, L31]=(+), [L57, L52, L51]=(+), [L57, L52, L55]=(+), [L57, L52]=(+), [L57, L53, L23]=(+), [L57, L53, L45]=(+), [L57, L53]=(+), [L57]=(+), [L59, L1, L17]=(+), [L59, L1, L28]=(+), [L59, L1, L49]=(+), [L59, L1, L53]=(+), [L59, L1, L56]=(+), [L59, L1]=(+), [L59, L29, L23]=(+), [L59, L29, L45]=(+), [L59, L29]=(+), [L59, L3, L17]=(+), [L59, L3, L28]=(+), [L59, L3, L49]=(+), [L59, L3, L53]=(+), [L59, L3, L56]=(+), [L59, L33, L53]=(+), [L59, L33]=(+), [L59, L3]=(+), [L59, L41, L13]=(+), [L59, L41, L21]=(+), [L59, L41, L34]=(+), [L59, L41, L3]=(+), [L59, L41, L40]=(+), [L59, L41, L44]=(+), [L59, L41, L50]=(+), [L59, L41, L51]=(+), [L59, L41, L54]=(+), [L59, L41]=(+), [L59, L44, L14]=(+), [L59, L44, L32]=(+), [L59, L44, L54]=(+), [L59, L44]=(+), [L59, L47, L21]=(+), [L59, L47, L2]=(+), [L59, L47, L49]=(+), [L59, L47, L59]=(+), [L59, L47]=(+), [L59, L53, L23]=(+), [L59, L53, L45]=(+), [L59, L53]=(+), [L59, L55, L1]=(+), [L59, L55, L29]=(+), [L59, L55]=(+), [L59]=(+), [L5]=(+), [L8, L1, L53]=(+), [L8, L16, L26]=(+), [L8, L16, L27]=(+), [L8, L16, L2]=(+), [L8, L16, L35]=(+), [L8, L16, L40]=(+), [L8, L16, L44]=(+), [L8, L16, L54]=(+), [L8, L16]=(+), [L8, L1]=(+), [L8, L29, L45]=(+), [L8, L29]=(+), [L8, L44, L2]=(+), [L8, L44, L59]=(+), [L8, L44, L5]=(+), [L8, L44]=(+), [L8]=(+), [L9, L1, L13]=(+), [L9, L1, L19]=(+), [L9, L1, L25]=(+), [L9, L17, L45]=(+), [L9, L17, L55]=(+), [L9, L17]=(+), [L9, L1]=(+), [L9, L4, L21]=(+), [L9, L4, L2]=(+), [L9, L4, L49]=(+), [L9, L4]=(+), [L9, L53, L23]=(+), [L9, L53, L45]=(+), [L9, L53]=(+), [L9]=(+)}",
					map.toString());
			LearnerGraph expectedAfterInitialMerge = new LearnerGraph(config);AbstractPersistence.loadGraph("resources/TestLearnFromTracesUsingMarkov.xml", expectedAfterInitialMerge,converter);
			WMethod.checkM(expectedAfterInitialMerge, ptaAfterInitialMerge);// checks that the graph is as expected.
			
			long value = MarkovClassifier.computeInconsistency(ptaAfterInitialMerge, m, checker,null, false);
			Assert.assertEquals(31,value);// check that we computed inconsistencies in the usual way.

			Stack<PairScore> stack = ptaAfterInitialMerge.pairscores.chooseStatePairs(new InconsistencyComputation(m, checker));
			System.out.println(stack);
			LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			int genScore = ptaAfterInitialMerge.pairscores.computePairCompatibilityScore_general(stack.peek(), null, verticesToMerge);
			Assert.assertTrue(genScore >= 0);
			LearnerGraph merged = MergeStates.mergeCollectionOfVertices(ptaAfterInitialMerge, null, verticesToMerge);
			stack = merged.pairscores.chooseStatePairs(new InconsistencyComputation(m, checker));
			System.out.println(stack);
	}

}
