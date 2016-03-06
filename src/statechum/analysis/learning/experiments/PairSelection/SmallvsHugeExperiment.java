package statechum.analysis.learning.experiments.PairSelection;

import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Label;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.PaperUAS;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReference;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceLanguageBCR;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResult;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

public class SmallvsHugeExperiment {
	protected final Configuration config;
	protected final ConvertALabel converter;
	protected final int states,sample;
	protected boolean learnUsingReferenceLearner, onlyUsePositives;
	protected final int seed;
	protected final int traceQuantity;
	protected String selectionID;
	protected final int lengthMultiplier;
	
	public void setSelectionID(String value)
	{
		selectionID = value;
	}
	
	/** Whether to compute the value from QSM as a reference learner. */
	public void setEvaluateAlsoUsingReferenceLearner(boolean value)
	{
		learnUsingReferenceLearner = value;
	}
	
	/** Whether to filter the collection of traces such that only positive traces are used. */
	public void setOnlyUsePositives(boolean value)
	{
		onlyUsePositives = value;
	}
	
	public SmallvsHugeExperiment(int argStates, int argSample, int argSeed, int nrOfTraces, int lengthmult,Configuration conf, ConvertALabel conv)
	{
		states = argStates;sample = argSample;config = conf;seed = argSeed;traceQuantity=nrOfTraces;converter=conv;lengthMultiplier = lengthmult;
	}
	
	public ThreadResult call() throws Exception 
	{
		final int alphabet = 2*states;
		LearnerGraph referenceGraph = null;
		ThreadResult outcome = new ThreadResult();
		Label uniqueFromInitial = null;
		final boolean pickUniqueFromInitial = true;
		MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
		do
		{
			referenceGraph = mg.nextMachine(alphabet,seed, config, converter).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
			if (pickUniqueFromInitial)
			{
				Map<Label,CmpVertex> uniques = LearningSupportRoutines.uniqueFromState(referenceGraph);
				if(!uniques.isEmpty())
				{ 
					// some uniques are loops, hence eliminate them to match our case study
					for(Entry<Label,CmpVertex> entry:uniques.entrySet())
						if (referenceGraph.transitionMatrix.get(entry.getValue()).get(entry.getKey()) != entry.getValue())
						{
							referenceGraph.setInit(entry.getValue());uniqueFromInitial = entry.getKey();break;// found a unique of interest
						}
				}
			}
		}
		while(pickUniqueFromInitial && uniqueFromInitial == null);
		
		LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);learnerEval.setLabelConverter(converter);
		final Collection<List<Label>> testSet = PaperUAS.computeEvaluationSet(referenceGraph,states*3,states*alphabet);
		
		for(int attempt=0;attempt<3;++attempt)
		{// try learning the same machine a few times
			LearnerGraph pta = new LearnerGraph(config);
			RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(attempt),5,referenceGraph.getVertex(Arrays.asList(new Label[]{uniqueFromInitial})));
			//generator.setWalksShouldLeadToInitialState();
			final int tracesToGenerate = LearningSupportRoutines.makeEven(states*traceQuantity);
			final Random rnd = new Random(seed*31+attempt);

			generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
									
					@Override
					public int getLength() {
						return  lengthMultiplier*states;
					}
	
					@Override
					public int getPrefixLength(int len) {
						return len;
					}
				},true,true,null,Arrays.asList(new Label[]{uniqueFromInitial}));
			
			/*
			for(List<Label> seq:referenceGraph.wmethod.computeNewTestSet(1))
			{
				pta.paths.augmentPTA(seq, referenceGraph.getVertex(seq) != null, false, null);
			}*/
			//pta.paths.augmentPTA(referenceGraph.wmethod.computeNewTestSet(referenceGraph.getInit(),1));// this one will not set any states as rejects because it uses shouldbereturned
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
				// This is done below if onlyUsePositives is not set. 
			
			synchronized (AbstractLearnerGraph.syncObj) {
				PaperUAS.computePTASize(selectionID+" with unique "+uniqueFromInitial+" : ", pta, referenceGraph);
			}
			
			pta.clearColours();
			
			if (!onlyUsePositives)
			{// now we have an even positive/negative split, add negatives by encoding them as if-then automata.
				assert pta.getStateNumber() > pta.getAcceptStateNumber() : "graph with only accept states but onlyUsePositives is not set";
				Map<Label,Set<Label>> infeasiblePairs = LearningSupportRoutines.computeInfeasiblePairs(referenceGraph);
				Map<Label,Set<Label>> subsetOfPairs = new TreeMap<Label,Set<Label>>();
				for(Entry<Label,Set<Label>> entry:infeasiblePairs.entrySet())
				{
					Set<Label> value = new TreeSet<Label>();
					if (!entry.getValue().isEmpty()) 
					{// we add a single entry per label, to mimic what was done with UAS study, where labels could not be repeated.
						Label possibleLabels[]=entry.getValue().toArray(new Label[]{});
						if (possibleLabels.length == 1)
							value.add(possibleLabels[0]);
						else
							value.add(possibleLabels[rnd.nextInt(possibleLabels.length)]);
					}
					subsetOfPairs.put(entry.getKey(),value);
				}
				LearningSupportRoutines.addIfThenForPairwiseConstraints(learnerEval,subsetOfPairs);
				LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(learnerEval.ifthenSequences, referenceGraph.pathroutines.computeAlphabet(), learnerEval.config, learnerEval.getLabelConverter()).toArray(new LearnerGraph[0]);
				learnerEval.config.setUseConstraints(false);// do not use if-then during learning (refer to the explanation above)
				int statesToAdd = 1;// we are adding pairwise constraints hence only one has to be added.
				System.out.println(new Date().toString()+" Graph loaded: "+pta.getStateNumber()+" states ("+pta.getAcceptStateNumber()+" accept states), adding at most "+ statesToAdd+" if-then states");
				
				Transform.augmentFromIfThenAutomaton(pta, null, ifthenAutomata, statesToAdd);// we only need  to augment our PTA once (refer to the explanation above).
				Visualiser.updateFrame(pta.transform.trimGraph(4, pta.getInit()), null);
				System.out.println(new Date().toString()+" Graph augmented: "+pta.getStateNumber()+" states ("+pta.getAcceptStateNumber()+" accept states)");
			}
			else 
				assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
			
			LearningAlgorithms.ReferenceLearner learnerOfPairs = null;
			LearnerGraph actualAutomaton = null;
			
			for(Entry<CmpVertex,List<Label>> path: pta.pathroutines.computeShortPathsToAllStates().entrySet())
			{
				boolean accept = path.getKey().isAccept();
				CmpVertex vert = referenceGraph.getVertex(path.getValue());
				boolean shouldBe = vert==null?false:vert.isAccept();
				assert accept == shouldBe: "state "+vert+" is incorrectly annotated as "+accept+" in path "+path;
			}

			{// Perform pre-merge and learn from there
				LearnerGraph reducedPTA = LearningSupportRoutines.mergeStatesForUnique(pta,uniqueFromInitial);
				synchronized (AbstractLearnerGraph.syncObj) {
					PaperUAS.computePTASize(selectionID+" premerge "+uniqueFromInitial+" : ", reducedPTA, referenceGraph);
				}
				Visualiser.updateFrame(reducedPTA.transform.trimGraph(4, pta.getInit()), referenceGraph);
				
				// in these experiments I cannot use SICCO merging because it will stop any mergers with an initial state.
				learnerOfPairs = new LearningAlgorithms.ReferenceLearner(learnerEval, reducedPTA,LearningAlgorithms.ReferenceLearner.ScoringToApply.SCORING_EDSM);
				
				System.out.println("premerge size: "+reducedPTA.getStateNumber()+" states, "+reducedPTA.getAcceptStateNumber()+" accept-states and "+reducedPTA.pathroutines.countEdges()+" transitions");
				actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());

				DifferenceToReference similarityMeasure = getMeasure(actualAutomaton,referenceGraph,testSet);
				System.out.println(sample+" permerge, similarity = "+similarityMeasure.getValue()+" ( "+similarityMeasure+" )");
			}

			{// Use constraints on labels, but no pre-merge
				LearnerGraph ptaToStartFrom = new LearnerGraph(pta, pta.config);
				learnerOfPairs = new LearningAlgorithms.ReferenceLearner(learnerEval, ptaToStartFrom,LearningAlgorithms.ReferenceLearner.ScoringToApply.SCORING_EDSM_1);
				learnerOfPairs.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueFromInitial}));
				
				actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());

				LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
				List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMerge(actualAutomaton,learnerOfPairs.getLabelsLeadingToStatesToBeMerged(),learnerOfPairs.getLabelsLeadingFromStatesToBeMerged());
				if (!pairsList.isEmpty())
				{// now we check that if doing a 'pre-merge' on the final graph is feasible - it is a basic check that the learner has honoured the constraints we have supplied via setLabelsLeadingFromStatesToBeMerged
					int score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge, false);
					if (score < 0)
					{
						learnerOfPairs = new LearningAlgorithms.ReferenceLearner(learnerEval, ptaToStartFrom,LearningAlgorithms.ReferenceLearner.ScoringToApply.SCORING_EDSM_1);
						learnerOfPairs.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueFromInitial}));
						actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
						score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge, false);
						throw new RuntimeException("last merge in the learning process was not possible");
					}
					actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge, false);
				}
				
				DifferenceToReference similarityMeasure = getMeasure(actualAutomaton,referenceGraph,testSet);
				System.out.println(sample+" learner with constraints, similarity = "+similarityMeasure.getValue()+" ( "+similarityMeasure+" )");
			}

			{// not doing anything specific regarding a unique transition from an initial state
				learnerOfPairs = new LearningAlgorithms.ReferenceLearner(learnerEval, pta,LearningAlgorithms.ReferenceLearner.ScoringToApply.SCORING_SICCO);
				actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
				DifferenceToReference similarityMeasure = getMeasure(actualAutomaton,referenceGraph,testSet);
				System.out.println(sample+" generic learner, similarity = "+similarityMeasure.getValue()+" ( "+similarityMeasure+" )");
				System.out.println();
			}
			
		}
		
		return outcome;
	}

	public static DifferenceToReference getMeasure(LearnerGraph actualAutomaton, LearnerGraph referenceGraph, final Collection<List<Label>> testSet)
	{
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
		//Visualiser.updateFrame(actualAutomaton, referenceGraph);
		return DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton, testSet);
	}
	
	public static void main(String []args)
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setAskQuestions(false);config.setDebugMode(false);config.setGdLowToHighRatio(0.7);config.setRandomPathAttemptFudgeThreshold(1000);
		config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
		ConvertALabel converter = new Transform.InternStringLabel();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int ThreadNumber = ExperimentRunner.getCpuNumber();
		
		ExecutorService executorService = Executors.newFixedThreadPool(ThreadNumber);
		final int samplesPerFSM = 4;
		final int minStateNumber = 7;
/*
		for(final double threshold:new double[]{1,1.2,1.5,3,10})
		for(final int ifDepth:new int []{0,1})
		for(final boolean onlyPositives:new boolean[]{true,false})
			for(final boolean zeroScoringAsRed:new boolean[]{true,false})
			for(final boolean selectingRed:new boolean[]{false})
			for(final boolean useUnique:new boolean[]{true,false})*/
			{/*
				String selection = "TRUNK"+"I"+ifDepth+"_"+"T"+threshold+"_"+
						(onlyPositives?"P_":"-")+(selectingRed?"R":"-")+(useUnique?"U":"-")+(zeroScoringAsRed?"Z":"-");
		*/
				for(int traceQuantity=20;traceQuantity<=20;++traceQuantity)
					for(int traceLengthMultiplier=40;traceLengthMultiplier<=40;++traceLengthMultiplier)
				{
					try
					{
						int numberOfTasks = 0;
						for(int states=minStateNumber;states < minStateNumber+15;states+=5)
							for(int sample=0;sample<samplesPerFSM;++sample)
							{
								SmallvsHugeExperiment learnerRunner = new SmallvsHugeExperiment(states,sample,1+numberOfTasks,traceQuantity, traceLengthMultiplier, config, converter);
								learnerRunner.setSelectionID("states"+states+"_sample"+sample);
								learnerRunner.call();
							}
					}
					catch(Exception ex)
					{
						IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
						if (executorService != null) { executorService.shutdown();executorService = null; }
						throw e;
					}
				}
			}
	}
}
