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

package statechum.analysis.learning.experiments;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.Stack;
import java.util.TreeMap;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;
import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.JUConstants;
import statechum.Label;
import statechum.StringLabel;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ReferenceLearnerUsingSiccoScoring;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.MarkovPassivePairSelection;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.collections.ArrayMapWithSearchPos;
import statechum.collections.MapWithSearch;


public class I2cexperiment extends PairQualityLearner
{
	public static class LearnerRunner
	{
		protected final Configuration config;
		protected final ConvertALabel converter;
		protected final int states=0,sample=0;
		protected boolean onlyUsePositives;
		protected int chunkLen=3;
		protected String selectionID;
		LearnerGraph pta = null;
		Collection<List<Label>> testSet=null;
		
		/** Whether we should try learning with zero inconsistencies, to see how heuristics fare. */
		protected boolean disableInconsistenciesInMergers = false;
		
		public void setDisableInconsistenciesInMergers(boolean v)
		{
			disableInconsistenciesInMergers = v;
		}
		
		public void setpta(LearnerGraph ptagraph)
		{
			pta = ptagraph;
		}
		
		
		public void setSelectionID(String value)
		{
			selectionID = value;
		}
		
		/** Whether to filter the collection of traces such that only positive traces are used. */
		public void setOnlyUsePositives(boolean value)
		{
			onlyUsePositives = value;
		}
				
		public void setChunkLen(int len)
		{
			chunkLen = len;
		}
		
		public LearnerRunner(Configuration conf, ConvertALabel conv)
		{
			config = conf;converter=conv;
		}
		
		boolean useCentreVertex = true, useDifferentScoringNearRoot = false, mergeIdentifiedPathsAfterInference = true, useClassifyToOrderPairs = true,useMostConnectedVertexToStartLearning = false;

		public void setlearningParameters(boolean useCentreVertexArg, boolean useDifferentScoringNearRootArg, boolean mergeIdentifiedPathsAfterInferenceArg, boolean useClassifyToOrderPairsArg, boolean useMostConnectedVertexToStartLearningArg)
		{
			useCentreVertex = useCentreVertexArg;useDifferentScoringNearRoot = useDifferentScoringNearRootArg;mergeIdentifiedPathsAfterInference = mergeIdentifiedPathsAfterInferenceArg;useClassifyToOrderPairs = useClassifyToOrderPairsArg;useMostConnectedVertexToStartLearning = useMostConnectedVertexToStartLearningArg; 
		}
		
		public void setPresetLearningParameters(int value)
		{
			switch(value)
			{
			case 0:// learning by not doing pre-merging, starting from root 
				setlearningParameters(false, false, false, false, false);break;
			case 1:// learning by doing pre-merging, starting from most connected vertex. This evaluates numerous pairs and hence is very slow.
				setlearningParameters(true, false, false, true, true);break;
			case 2:// learning by doing pre-merging but starting from root. This seems similar to preset 1 on 20 states.
				setlearningParameters(true, true, false, true, false);break;
			case 3:// learning by not doing pre-merging, starting from root and using a heuristic around root 
				setlearningParameters(false, true, false, true, false);break;
			case 4:// learning by not doing pre-merging, starting from root and not ranking the top IScore candidates with the fanout metric.
				setlearningParameters(false, false, false, false, false);break;
			default:
				throw new IllegalArgumentException("invalid preset number");
			}
		}

		public LearnerGraph learn() throws Exception 
		{
			LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);learnerEval.setLabelConverter(converter);
			
			final MarkovModel m= new MarkovModel(chunkLen,true,true, disableInconsistenciesInMergers);

			new MarkovClassifier(m, pta).updateMarkov(false);// construct Markov chain if asked for.
			
			pta.clearColours();

			if (!onlyUsePositives)
				assert pta.getStateNumber() > pta.getAcceptStateNumber() : "graph with only accept states but onlyUsePositives is not set";
			else 
				assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
			
			EDSM_MarkovLearner learnerOfPairs = null;
			LearnerGraph actualAutomaton = null;
			
			final Configuration deepCopy = pta.config.copy();deepCopy.setLearnerCloneGraph(true);
			LearnerGraph ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);

			final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();

			LearnerGraph ptaToUseForInference = pta;
			Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA=null;
							
			
			if (useCentreVertex)
			{
				final MarkovClassifier ptaClassifier = new MarkovClassifier(m,pta);
				final List<List<Label>> pathsToMerge=ptaClassifier.identifyPathsToMerge(checker);
				// These vertices are merged first and then the learning start from the root as normal.
				// The reason to learn from the root is a memory cost. if we learn from the middle, we can get a better results
				verticesToMergeBasedOnInitialPTA=ptaClassifier.buildVerticesToMergeForPaths(pathsToMerge);

				List<StatePair> pairsListInitialMerge = ptaClassifier.buildVerticesToMergeForPath(pathsToMerge);
				LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMergeInitialMerge = new LinkedList<>();
				int scoreInitialMerge = pta.pairscores.computePairCompatibilityScore_general(null, pairsListInitialMerge, verticesToMergeInitialMerge, false);
				assert scoreInitialMerge >= 0;
				ptaToUseForInference = MergeStates.mergeCollectionOfVertices(pta, null, verticesToMergeInitialMerge, false);
				final CmpVertex vertexWithMostTransitions = MarkovPassivePairSelection.findVertexWithMostTransitions(ptaToUseForInference,MarkovClassifier.computeInverseGraph(pta));
				if (useMostConnectedVertexToStartLearning)
				{
					ptaToUseForInference.clearColours();ptaToUseForInference.getInit().setColour(null);vertexWithMostTransitions.setColour(JUConstants.RED);
				}
				LearnerGraphND inverseOfPtaAfterInitialMerge = MarkovClassifier.computeInverseGraph(ptaToUseForInference);
				System.out.println("Centre vertex: "+vertexWithMostTransitions+" number of transitions: "+MarkovPassivePairSelection.countTransitions(ptaToUseForInference, inverseOfPtaAfterInitialMerge, vertexWithMostTransitions));
			}

			learnerOfPairs = new EDSM_MarkovLearner(learnerEval,ptaToUseForInference,0);learnerOfPairs.setMarkov(m);learnerOfPairs.setChecker(checker);
			learnerOfPairs.setDisableInconsistenciesInMergers(disableInconsistenciesInMergers);

			actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<>(), new LinkedList<>());
			actualAutomaton.setName("i2c_experiment");

    		
    		System.out.println(actualAutomaton.getStateNumber());
			
			if (verticesToMergeBasedOnInitialPTA != null && mergeIdentifiedPathsAfterInference)
			{
				LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<>();
				int genScore = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, constructPairsToMergeBasedOnSetsToMerge(actualAutomaton.transitionMatrix.keySet(),verticesToMergeBasedOnInitialPTA), verticesToMerge, false);
				assert genScore >= 0;
				actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge, true);
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

			return actualAutomaton;
		}

	}
	
	public static void visualiseGraph(LearnerGraph actualAutomaton)
	{
		Map<String,String> labelling = new TreeMap<>();
		for(Entry<CmpVertex, MapWithSearch<Label,Label,CmpVertex>> entry:actualAutomaton.transitionMatrix.entrySet())
			labelling.put(entry.getKey().toString(),Visualiser.extralabelToReplaceExisting+entry.getKey().getStringId());
		DirectedSparseGraph gr = actualAutomaton.pathroutines.getGraph();
		
		gr.addUserDatum(JUConstants.VERTEX, labelling, UserData.SHARED);
		Visualiser.updateFrame(gr,null);	
		Visualiser.waitForKey();		
	}
		
	public static Collection<StatePair> constructPairsToMergeBasedOnSetsToMerge(Set<CmpVertex> validStates, Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA)
	{
		List<StatePair> pairsList = new LinkedList<>();
		for(Set<CmpVertex> groupOfStates:verticesToMergeBasedOnInitialPTA)
		{
			Set<CmpVertex> validStatesInGroup = new TreeSet<>(groupOfStates);
			validStatesInGroup.retainAll(validStates);
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
			
	public static final ScoresForGraph zeroScore;
	static
	{
		zeroScore = new ScoresForGraph();zeroScore.differenceBCR=new DifferenceToReferenceLanguageBCR(0, 0, 0, 0);zeroScore.differenceStructural=new DifferenceToReferenceDiff(0, 0);
	}

	/** Uses the supplied classifier to rank pairs. */
	public static class EDSM_MarkovLearner extends ReferenceLearnerUsingSiccoScoring implements statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure
	{
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
		long comparisonsPerformed = 0;
		
		Map<CmpVertex,Long> inconsistenciesPerVertex = null;
		
		/** Whether we should try learning with zero inconsistencies, to see how heuristics fare. */
		protected boolean disableInconsistenciesInMergers = false;
		
		public void setDisableInconsistenciesInMergers(boolean v)
		{
			disableInconsistenciesInMergers = v;
		}

		@Override
		public void initComputation(LearnerGraph graph) 
		{
			coregraph = graph;

			inconsistencyFromAnEarlierIteration= MarkovClassifier.computeInconsistency(coregraph, Markov, checker,false);
			cl = new MarkovClassifier(Markov, coregraph);
		    //extendedGraph = cl.constructMarkovTentative();
			inverseGraph = (LearnerGraphND)MarkovClassifier.computeInverseGraph(coregraph,true);
			inconsistenciesPerVertex = new ArrayMapWithSearchPos<VertID,CmpVertex,Long>(coregraph.getStateNumber());
		}
		
		@Override // we only need this in order to supply a routine to find surrounding transitions and initComputation
		public long overrideScoreComputation(PairScore p) 
		{
			return computeScoreBasedOnInconsistencies(p);
		}		

		public long computeScoreBasedOnInconsistencies(PairScore p) 
		{
			if(!p.getQ().isAccept() && !p.getR().isAccept())
				return 0;
			++comparisonsPerformed;
			long currentInconsistency = 0;
			List<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<>();//coregraph.getStateNumber()+1);// to ensure arraylist does not reallocate when we fill in the last element
			int genScore = //new OldPairScoreComputation(coregraph).computePairCompatibilityScore_general(p, null, verticesToMerge);
					coregraph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge, false);
			long score= genScore;
			if (genScore >= 0)
			{			
				LearnerGraph merged = //OldMergeStates.mergeCollectionOfVertices(coregraph, null, verticesToMerge); 
						MergeStates.mergeCollectionOfVertices(coregraph, null, verticesToMerge, false);
				if (!disableInconsistenciesInMergers)
					currentInconsistency = MarkovClassifier.computeInconsistencyOfAMerger(coregraph, verticesToMerge, inconsistenciesPerVertex, merged, Markov, cl, checker);
				score=genScore-currentInconsistency;
			}
			return score;
		}

		/** This one returns a set of transitions in all directions. */
		@Override
		public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(CmpVertex currentRed) 
		{
			return	MarkovPassivePairSelection.obtainSurroundingTransitions(coregraph,inverseGraph,currentRed);
		}

		protected MarkovModel Markov;
		protected ConsistencyChecker checker;
		
		private static LearnerEvaluationConfiguration constructConfiguration(LearnerEvaluationConfiguration evalCnf, int threshold)
		{
			Configuration config = evalCnf.config.copy();config.setRejectPositivePairsWithScoresLessThan(threshold);
			LearnerEvaluationConfiguration copy = new LearnerEvaluationConfiguration(config);
			copy.graph = evalCnf.graph;copy.testSet = evalCnf.testSet;
			copy.setLabelConverter(evalCnf.getLabelConverter());
			copy.ifthenSequences = evalCnf.ifthenSequences;copy.labelDetails=evalCnf.labelDetails;
			return copy;
		}
		
		public void setMarkov(MarkovModel m) {
			Markov=m;
		}

		public void setChecker(ConsistencyChecker c) {
			checker=c;
		}

		public EDSM_MarkovLearner(LearnerEvaluationConfiguration evalCnf, final LearnerGraph argInitialPTA, int threshold) 
		{
			super(constructConfiguration(evalCnf,threshold),argInitialPTA,false);
		}

		@Override 
		public Stack<PairScore> ChooseStatePairs(LearnerGraph graph)
		{
			Stack<PairScore> outcome = graph.pairscores.chooseStatePairs(this);
			System.out.println("pairs: "+outcome);
			if (!outcome.isEmpty())
			{
				Stack<PairScore> pairsWithScoresComputedUsingGeneralMerger = outcome;
				/*
				new Stack<PairScore>();
				int count=0;
				for(PairScore p:outcome)
				{
					long inconsistencyScore = computeScoreBasedOnInconsistencies(p);
					if (inconsistencyScore >= 0)
					{
						pairsWithScoresComputedUsingGeneralMerger.push(new PairScore(p.getQ(),p.getR(),inconsistencyScore,p.getAnotherScore()));
						if (++count > 10)
							break;
					}
				}

				Collections.sort(pairsWithScoresComputedUsingGeneralMerger);
				*/
				PairScore chosenPair = null;
				chosenPair = LearningSupportRoutines.pickPairQSMLike(pairsWithScoresComputedUsingGeneralMerger);
				outcome.clear();outcome.push(chosenPair);
			}
			
			return outcome;
		}		
		
		@Override
		public String toString()
		{
			return "EDSM_Markov";
		}
	}

	public static void main(String[] args) throws Exception
	{
		try
		{
			runExperiment(args);
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
	
	/** Alphabet that is expected in a trace to load. */
	protected static String[] alphabet =new String[]{
			"=S","@","=R","=TA","[*9]","-Ta","~SnA","~SnB","~R","#S","#W","#Ta","#s","#RA","[+1]","^Ra","~r",
			"~SwA","~swA","^E","#L","~Xr","?"					
	};
	protected static String alphabetElementToConsiderErr = "?";
	protected static String errElement = "Err";
	private static List<Label> loadTrace(String inputFileName,ConvertALabel converter)
	{
		List<Label> returnValue = new LinkedList<>();
		
        BufferedReader in = null;
        try {
           in = new BufferedReader(new FileReader(inputFileName));
            String fileString;
            while ((fileString = in.readLine()) != null) 
            {
            	String lineOfLog= fileString.trim();
            	while(lineOfLog.length() > 0)
            	{
	            	String aFound = null;
	            	for(String st:alphabet)
	            	if (lineOfLog.startsWith(st))
	            	{
	            		if (aFound != null)
	            			throw new IllegalArgumentException("both "+aFound+" and "+st+" match trace line "+lineOfLog);
	            		aFound = st;
	            	}
	            	if (aFound == null)
	            		throw new IllegalArgumentException("failed to match the beginning of line "+lineOfLog);
	            	lineOfLog = lineOfLog.substring(aFound.length());
	            	if (aFound.equals(alphabetElementToConsiderErr))
	            		aFound =errElement;
	            	returnValue.add(converter.convertLabelToLabel(new StringLabel(aFound)));
            	}
            }
         } catch (IOException e) {
            statechum.Helper.throwUnchecked("failed to read learner initial data", e);
        } finally {
            if (in != null) { try { in.close();in=null; } catch(IOException toBeIgnored) { /* Ignore exception */ } }
        }
        System.out.println("trace length: "+returnValue.size());
        return returnValue;
	}
	/*
	public static void runExperimentCompare(@SuppressWarnings("unused") String args[]) throws Exception
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setAskQuestions(false);config.setDebugMode(false);config.setGdLowToHighRatio(0.7);config.setRandomPathAttemptFudgeThreshold(1000);
		config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);config.setLearnerScoreMode(ScoreMode.ONLYOVERRIDE);
		ConvertALabel converter = new Transform.InternStringLabel();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int chunkSize = 7;
	
		// Inference from a few traces
		final boolean onlyPositives=true;
		LearnerGraph graphRef = new LearnerGraph(config);AbstractPersistence.loadGraph("resources/i2c_study/outcome_i2c_chunk7.xml",graphRef,converter);
		LearnerGraph graph = new LearnerGraph(config);AbstractPersistence.loadGraph("outcome_i2c.xml",graph,converter);
		
		DifferentFSMException diff = WMethod.checkM(graphRef, graph);
		if (diff != null)
			throw diff;
		System.out.println("same graph");
	}
*/
	public static void runExperiment(@SuppressWarnings("unused") String[] args) throws Exception
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setAskQuestions(false);config.setDebugMode(false);config.setGdLowToHighRatio(0.7);config.setRandomPathAttemptFudgeThreshold(1000);
		config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);config.setLearnerScoreMode(ScoreMode.ONLYOVERRIDE);
		config.setHowManyStatesToAddFromIFTHEN(1);
		ConvertALabel converter = new Transform.InternStringLabel();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int chunkSize = 7;
	
		// Inference from a few traces
		final boolean onlyPositives=false;

		LearnerGraph initialPTA = new LearnerGraph(config);
		initialPTA.paths.augmentPTA(loadTrace("resources/i2c_study/log10.txt",converter), true, false,null);
    //System.out.println(initialPta.learnerCache.getAlphabet());
		LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(List.of("ifthenFSM graph1 A-!" + errElement + "->A-" + errElement + "->B-" + errElement + "->B-!" + errElement + "->A / P-" + errElement + "-#Q / P == THEN == B"), initialPTA.pathroutines.computeAlphabet(), config, converter).toArray(new LearnerGraph[0]);
		Transform.augmentFromIfThenAutomaton(initialPTA, null, ifthenAutomata, config.getHowManyStatesToAddFromIFTHEN());// we only need  to augment our PTA once.
	  LearnerRunner learnerRunner = new LearnerRunner(config, converter);
	  learnerRunner.setOnlyUsePositives(onlyPositives);
		learnerRunner.setChunkLen(chunkSize);
		
		learnerRunner.setpta(initialPTA);

		//learnerRunner.setPresetLearningParameters(0);
		learnerRunner.setlearningParameters(false, false, false, false, false);
		learnerRunner.setDisableInconsistenciesInMergers(false);
		System.out.println("started: "+new Date());
		LearnerGraph graph = learnerRunner.learn();
		System.out.println("finished: "+new Date());
		graph.storage.writeGraphML("outcome_i2c.xml");
	}
}