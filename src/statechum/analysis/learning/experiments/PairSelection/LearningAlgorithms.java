package statechum.analysis.learning.experiments.PairSelection;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.concurrent.Callable;

import statechum.Configuration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceDiff;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceLanguageBCR;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.SampleData;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResult;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.TrueFalseCounter;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.linear.GDLearnerGraph;
import statechum.analysis.learning.linear.GDLearnerGraph.HandleRow;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph.StatesToConsider;
import statechum.analysis.learning.rpnicore.PairScoreComputation.AMEquivalenceClassMergingDetails;
import statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure;
import statechum.analysis.learning.rpnicore.PairScoreComputation.SiccoGeneralScoring;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.collections.ArrayMapWithSearch;
import statechum.collections.HashMapWithSearch;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

public class LearningAlgorithms
{
	/** In a number of experiments, evaluation of automata is conducted by exploration either by generating a test set or by generating walks of some length. Both are governed by a bound:
	 * if a learnt automaton has traces that cannot be explored within that bound, it is quite easy to obtain something with a huge number of states that passes all check with flying colours.
	 * This is the maximal number of states permitted in a learnt graph, as a multiplier of a number of states in a reference graph. 
	 */
	public static int maxStateNumberMultiplier = 3;
	
	public static Collection<List<Label>> computeEvaluationSet(LearnerGraph referenceGraph, int seqLength, int numberOfSeq)
	{
	   for(CmpVertex vert:referenceGraph.transitionMatrix.keySet()) 
		   if (!vert.isAccept())
			   throw new IllegalArgumentException("test set generation should not be attempted on an automaton with reject-states");
	   assert numberOfSeq > 0 && seqLength > 0;
	   RandomPathGenerator pathGen = new RandomPathGenerator(referenceGraph,new Random(0),seqLength,referenceGraph.getInit());
	   pathGen.generateRandomPosNeg(numberOfSeq, 1, false, null, true,true,null,null);
	   return  pathGen.getAllSequences(0).getData(PTASequenceEngine.truePred);
	   /*
		Collection<List<Label>> evaluationTestSet = referenceGraph.wmethod.getFullTestSet(1);
		
		RandomPathGenerator pathGen = new RandomPathGenerator(referenceGraph,new Random(0),5,referenceGraph.getInit());
		int wPos=0;
		for(List<Label> seq:evaluationTestSet)
			if (referenceGraph.paths.tracePathPrefixClosed(seq) == AbstractOracle.USER_ACCEPTED) wPos++;
		pathGen.generateRandomPosNeg(2*(evaluationTestSet.size()-2*wPos), 1, false, null, true,false,evaluationTestSet,null);
		evaluationTestSet = pathGen.getAllSequences(0).getData(PTASequenceEngine.truePred);// we replacing the test set with new sequences rather than adding to it because existing sequences could be prefixes of the new ones.
		wPos = 0;
		for(List<Label> seq:evaluationTestSet) if (referenceGraph.paths.tracePathPrefixClosed(seq) == AbstractOracle.USER_ACCEPTED) wPos++;
		return evaluationTestSet;
	    */
	}

	/** Returns a test set to use for evaluation of the supplied reference graph using BCR. */
	public static Collection<List<Label>> buildEvaluationSet(LearnerGraph referenceGraph)
	{
		return computeEvaluationSet(referenceGraph,referenceGraph.getAcceptStateNumber()*maxStateNumberMultiplier,LearningSupportRoutines.makeEven(referenceGraph.getAcceptStateNumber()*referenceGraph.pathroutines.computeAlphabet().size()));
	}
	
	public static class LearnerAbortedException extends RuntimeException
	{

		/**
		 * ID for serialisation
		 */
		private static final long serialVersionUID = 5271079210565150062L;
		
		public static void throwExceptionIfTooManyReds( LearnerGraph graph, int maxNumberOfReds )
		{
			if (maxNumberOfReds >= 0)
			{
				long countOfRed = 0;
				for(CmpVertex v:graph.transitionMatrix.keySet())
					if (v.getColour() == JUConstants.RED)
						if (countOfRed++ > maxNumberOfReds)
							throw new LearnerAbortedException();
			}
		}
	}

	/** This class knows what the reference automaton is and is able to pick correct pairs out of a set to merge. */
	public static abstract class LearnerWithMandatoryMergeConstraints extends RPNIUniversalLearner
	{
		protected final LearnerGraph initialPTA;
		
		public LearnerWithMandatoryMergeConstraints(LearnerEvaluationConfiguration evalCnf, LearnerGraph argInitialPTA) 
		{
			super(null, evalCnf);
			initialPTA = argInitialPTA;
		}

		Collection<Label> labelsLeadingToStatesToBeMerged = new LinkedList<Label>(),labelsLeadingFromStatesToBeMerged = new LinkedList<Label>();
		
		public Collection<Label> getLabelsLeadingToStatesToBeMerged()
		{
			return labelsLeadingToStatesToBeMerged;
		}
		
		public void setLabelsLeadingToStatesToBeMerged(Collection<Label> labels)
		{
			labelsLeadingToStatesToBeMerged = labels;
		}
		
		public Collection<Label> getLabelsLeadingFromStatesToBeMerged()
		{
			return labelsLeadingFromStatesToBeMerged;
		}
		
		public void setLabelsLeadingFromStatesToBeMerged(Collection<Label> labels)
		{
			labelsLeadingFromStatesToBeMerged = labels;
		}		

		@SuppressWarnings("unused")
		@Override 
		public LearnerGraph init(Collection<List<Label>> plus,	Collection<List<Label>> minus) 
		{
			LearnerGraph.copyGraphs(initialPTA, getTentativeAutomaton());
			return initialPTA;
		}
		
		@SuppressWarnings("unused")
		@Override 
		public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
		{
			throw new UnsupportedOperationException();
		}			
		@Override 
		public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair)
		{
			Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			if (original.pairscores.computePairCompatibilityScore_general(pair,null,mergedVertices, false) < 0)
				throw new IllegalArgumentException("elements of the pair "+pair+" are incompatible, orig score was "+original.pairscores.computePairCompatibilityScore(pair));
			LearnerGraph outcome = MergeStates.mergeCollectionOfVertices(original,pair.getR(),mergedVertices,false);

			outcome.pathroutines.updateDepthLabelling();// this is important for the choice of representative vertices in merging of states, this in turn affects IDs of merged states which affects selection of pairs for merging.
			return outcome;
		}
	}
	
	/** This class knows what the reference automaton is and is able to pick correct pairs out of a set to merge. */
	public static class LearnerThatCanClassifyPairs extends ReferenceLearner
	{
		private static LearnerEvaluationConfiguration constructConfiguration(LearnerEvaluationConfiguration evalCnf, int maxNumberOfStates)
		{
			Configuration config = evalCnf.config.copy();config.setOverride_maximalNumberOfStates(maxNumberOfStates);
			LearnerEvaluationConfiguration copy = new LearnerEvaluationConfiguration(evalCnf);copy.config = config;
			return copy;
		}
		
		public LearnerThatCanClassifyPairs(LearnerEvaluationConfiguration evalCnf, LearnerGraph reference, LearnerGraph argInitialPTA,OverrideScoringToApply scoring) 
		{
			super(constructConfiguration(evalCnf,reference.getAcceptStateNumber()*maxStateNumberMultiplier),argInitialPTA,scoring);
			referenceGraph = reference;
		}

		protected boolean allMergersCorrect = true;
		
		public boolean checkAllMergersCorrect()
		{
			return allMergersCorrect;
		}
		
		/** Returns one of the correct pairs.
		 */
		public PairScore pickCorrectPair(Collection<PairScore> pairs, LearnerGraph tentativeGraph)
		{
			List<PairScore> correctPairs = new ArrayList<PairScore>(pairs.size()), wrongPairs = new ArrayList<PairScore>(pairs.size());
					
			LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, referenceGraph, pairs, correctPairs, wrongPairs);
			
			// without sorting the pairs, the learner finds itself in a situation with no valid pairs to choose from.
			Comparator<PairScore> PairComparator = new Comparator<PairScore>(){

				@Override
				// The first element is the one where o2 is greater than o1, i.e. comparison below returns negative.
				public int compare(PairScore o1, PairScore o2) {
					// if o1 is negative and o2 is positive, the outcome is negative.
					int outcome = LearningSupportRoutines.signum( o2.getAnotherScore() - o1.getAnotherScore() );
					return outcome;
					
				}};
				
			Collections.sort(correctPairs,PairComparator);
			if (correctPairs.isEmpty())
			{
				Collections.sort(wrongPairs, PairComparator);
				allMergersCorrect = false;
				return wrongPairs.iterator().next();
			}
			return correctPairs.iterator().next();
		}
		
		
		/** There are cases when no selected pair is actually valid. The method below chooses a state to be marked as red because it is the only choice that we can make. */
		public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, @SuppressWarnings("unused") Collection<CmpVertex> reds, Collection<PairScore> pairs) 
		{
			
			CmpVertex stateToMarkRed = null;
			List<PairScore> correctPairs = new ArrayList<PairScore>(pairs.size()), wrongPairs = new ArrayList<PairScore>(pairs.size());
			LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(coregraph, referenceGraph, pairs, correctPairs, wrongPairs);
			if (correctPairs.isEmpty())
				stateToMarkRed = wrongPairs.get(0).getQ();// no correct pairs found to merge, return the first wrong pair so the corresponding state is marked as red.
			
			return stateToMarkRed;
		}
/*
		@Override
		public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
		{// fast merger
			LearnerGraph outcome = MergeStates.mergeAndDeterminize(original, pair);outcome.pathroutines.updateDepthLabelling();
			return outcome;
		}
		*/
		protected Map<Long,TrueFalseCounter> pairQuality;
		protected LearnerGraph referenceGraph;

		public void setPairQualityCounter(Map<Long,TrueFalseCounter> argCounter, LearnerGraph referenceGraph)
		{
			pairQuality = argCounter;this.referenceGraph = referenceGraph;
		}

		/** This method is called after a final set of pairs is generated. Can be overridden to update statistics on scores of pairs by comparison to the reference graph. */
		@Override
		protected void updatePairQualityStatistics(LearnerGraph graph,List<PairScore> outcome)
		{
			if (pairQuality != null && referenceGraph != null)
				LearningSupportRoutines.updateStatistics(pairQuality, graph,referenceGraph, outcome);
		}
	}
	

	public static class DefaultRedNodeSelectionProcedure implements PairScoreComputation.RedNodeSelectionProcedure
	{
		@Override
		public CmpVertex selectRedNode(@SuppressWarnings("unused") LearnerGraph gr, @SuppressWarnings("unused") Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
		{
			CmpVertex redVertex = tentativeRedNodes.iterator().next();
			return redVertex;
		}

		@SuppressWarnings("unused")
		@Override
		public CmpVertex resolvePotentialDeadEnd(LearnerGraph gr, Collection<CmpVertex> reds, List<PairScore> pairs) 
		{
			return null;
		}

		LearnerGraph coregraph = null;
		
		@Override
		public void initComputation(LearnerGraph gr) {
			coregraph = gr;
		}

		@Override
		public long overrideScoreComputation(PairScore p) {
			return p.getScore();
		}

		@Override
		public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(@SuppressWarnings("unused") CmpVertex currentRed) 
		{
			return null;// dummy, ignored if null.
		}
	}

	/** An enumeration of a number of scoring methods that can be used for learning. Its main use is to iterate through a subset of it, permitting the experiment to run with a range of different scoring methods. */
	public enum ScoringToApply { SCORING_EDSM, SCORING_EDSM_1, SCORING_EDSM_2, SCORING_SICCO, SCORING_SICCO_NIS, SCORING_SICCO_REDBLUE, SCORING_SICCO_RED }
	public static ReferenceLearner constructReferenceLearner(LearnerEvaluationConfiguration evalCnf, LearnerGraph initialPTA, ScoringToApply howToScore) 
	{
		ReferenceLearner outcome = null;
		switch(howToScore)
		{
		case SCORING_EDSM:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, -1);break;
		case SCORING_EDSM_1:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, 1);break;
		case SCORING_EDSM_2:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, 2);break;
		case SCORING_SICCO:
			outcome = new ReferenceLearner(constructLearningConfiguration_Compatibility(evalCnf), initialPTA, ReferenceLearner.OverrideScoringToApply.SCORING_SICCO);break;
		case SCORING_SICCO_NIS:
			outcome = new ReferenceLearner(constructLearningConfiguration_Compatibility(evalCnf), initialPTA, ReferenceLearner.OverrideScoringToApply.SCORING_SICCO_NIS);break;
		case SCORING_SICCO_REDBLUE:
			outcome = new ReferenceLearner(constructLearningConfiguration_Compatibility(evalCnf), initialPTA, ReferenceLearner.OverrideScoringToApply.SCORING_SICCO_REDBLUE);break;
		case SCORING_SICCO_RED:
			outcome = new ReferenceLearner(constructLearningConfiguration_Compatibility(evalCnf), initialPTA, ReferenceLearner.OverrideScoringToApply.SCORING_SICCO_RED);break;
		default:
			throw new IllegalArgumentException("unknown learner "+howToScore);

		}
		return outcome;
	}
	
	/** This one is a reference learner, delegating the computation to the actual learner and adding a series of different heuristics that are not present there. 
	 * The primary purpose of this method is to add an additional check to pair score computation, either based on different heuristics such as Sicco heuristic, or mandatory merge constraints. 
	 * 
	 *  The nae reflects that apart from possibly minor changes to scoring, there are no significant manipulation of PTA taking place.
	 */
	public static class ReferenceLearner extends LearnerWithMandatoryMergeConstraints
	{
		// Where there is a unique transition out an initial state is always the first transition in the traces, SICCO merging rule will stop any mergers into the initial state because 
		// such mergers will always introduce new transitions (the unique transition from the initial state is only present from that state by graph construction). This is why we have
		// the SCORING_SICCO_EXCEPT_FOR_THE_INITIAL_STATE which applies EDSM rule to the initial state and SICCO rule to all other states.
		public enum OverrideScoringToApply { SCORING_NO_OVERRIDE, SCORING_EDSM, SCORING_EDSM_1, SCORING_EDSM_2, SCORING_SICCO, SCORING_SICCO_RECURSIVE, SCORING_SICCO_NIS, SCORING_SICCO_REDBLUE, SCORING_SICCO_RED }
		protected final OverrideScoringToApply scoringMethod;
		
		public ReferenceLearner(LearnerEvaluationConfiguration evalCnf, final LearnerGraph argInitialPTA, OverrideScoringToApply scoring) 
		{
			super(evalCnf, argInitialPTA);this.scoringMethod = scoring;
		}

		/** This method is called after a final set of pairs is generated. Can be overridden to update statistics on scores of pairs by comparison to the reference graph. */
		@SuppressWarnings("unused")
		protected void updatePairQualityStatistics(LearnerGraph graph,List<PairScore> outcome)
		{
		}
		
		/** Override in child classes to permit learning FSM without a limit on state number. Useful in production use but not for research experiments where evaluation explores learnt automata up to a specfic limit. */
		protected boolean permitUnlimitedNumberOfStates()
		{
			return false;
		}
		
		@Override
		public LearnerGraph learnMachine()
		{
			if (!permitUnlimitedNumberOfStates() && config.getOverride_maximalNumberOfStates() < 0)
				throw new IllegalArgumentException("Reference learner requires a limit on a number of states in an automaton to learn");
			
			LearnerGraph outcome = null;
			try
			{
				outcome = super.learnMachine();
			}
			catch(LearnerAbortedException ex)
			{
				outcome = new LearnerGraph(config);
				outcome.getInit().setAccept(false);
			}
			return outcome;
		}

		@Override 
		public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair)
		{
			if (config.getOverride_usePTAMerging())
				return MergeStates.mergeAndDeterminize(original, pair);
			
			return 
				super.MergeAndDeterminize(original, pair);
		}

		@Override 
		public Stack<PairScore> ChooseStatePairs(LearnerGraph graph)
		{
			LearnerAbortedException.throwExceptionIfTooManyReds(graph, config.getOverride_maximalNumberOfStates());
			if (graph.config.getLearnerScoreMode() == ScoreMode.ONLYOVERRIDE) // we only check this here because in the constructor it is too early - derived classes may choose to substitute an alternative scoring method.
				throw new IllegalArgumentException("this method complements an existing scoring routine, it is not a replacement for it, hence ONLYOVERRIDE scoring mode cannot be used");

			Stack<PairScore> outcome = graph.pairscores.chooseStatePairs(new PairScoreComputation.RedNodeSelectionProcedure(){

				// Here I could use a learner based on metrics of both tentative reds and the perceived quality of the red-blue pairs obtained if I choose any given value.
				// This can be accomplished by doing a clone of the graph and running chooseStatePairs on it with decision procedure that 
				// (a) applies the same rule (of so many) to choose pairs and
				// (b) checks that deadends are flagged. I could iterate this process for a number of decision rules, looking locally for the one that gives best quality of pairs
				// for a particular pairscore decision procedure.
				@Override
				public CmpVertex selectRedNode(@SuppressWarnings("unused") LearnerGraph gr, @SuppressWarnings("unused") Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
				{
					CmpVertex redVertex = tentativeRedNodes.iterator().next();
					return redVertex;
				}

				@SuppressWarnings("unused")
				@Override
				public CmpVertex resolvePotentialDeadEnd(LearnerGraph gr, Collection<CmpVertex> reds, List<PairScore> pairs) 
				{
					return null;
				}

				LearnerGraph coregraph = null;
				
				@Override
				public void initComputation(LearnerGraph gr) {
					coregraph = gr;
				}

				@Override
				public long overrideScoreComputation(PairScore p) 
				{
					Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new ArrayList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					long score = p.getScore();
					
					switch(ReferenceLearner.this.scoringMethod)
					{
					case SCORING_SICCO:
						if (p.getScore() >= 0 && coregraph.pairscores.computeSiccoRejectScoreGeneral(p, mergedVertices, SiccoGeneralScoring.S_ONEPAIR) < 0)
							score = -1;
						break;
					case SCORING_SICCO_RECURSIVE:
						if (p.getScore() >= 0 && coregraph.pairscores.computeSiccoRejectScoreGeneral(p, mergedVertices, SiccoGeneralScoring.S_ONEPAIR) < 0)
							score = -1;
						break;
					case SCORING_SICCO_NIS:
						if (p.getScore() >= 0 && p.getQ() != coregraph.getInit() && p.getR() != coregraph.getInit() && coregraph.pairscores.computeSiccoRejectScoreGeneral(p, mergedVertices, SiccoGeneralScoring.S_ONEPAIR) < 0)
							score = -1;
						break;
					case SCORING_SICCO_REDBLUE:
						if (p.getScore() >= 0 && coregraph.pairscores.computeSiccoRejectScoreGeneral(p, mergedVertices, SiccoGeneralScoring.S_RED_BLUE) < 0)
							score = -1;
						break;
					case SCORING_SICCO_RED:
						if (p.getScore() >= 0 && coregraph.pairscores.computeSiccoRejectScoreGeneral(p, mergedVertices, SiccoGeneralScoring.S_RED) < 0)
							score = -1;
						break;
					default:// do nothing since this is the case where nothing needs to be done.
						break;
					}
					
					if (!labelsLeadingToStatesToBeMerged.isEmpty() || !labelsLeadingFromStatesToBeMerged.isEmpty())
						if (LearningSupportRoutines.computeScoreBasedOnMandatoryMerge(p, coregraph, labelsLeadingToStatesToBeMerged, labelsLeadingFromStatesToBeMerged) < 0)
							score = -1;

					return score;
				}

				@Override
				public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(@SuppressWarnings("unused") CmpVertex currentRed) 
				{
					return null;// dummy, ignored if null.
				}
			});
			if (!outcome.isEmpty())
			{
					PairScore chosenPair = LearningSupportRoutines.pickPairQSMLike(outcome);
					updatePairQualityStatistics(graph,outcome);
					outcome.clear();outcome.push(chosenPair);
			}
			
			return outcome;
		}		
	}
	
	static LearnerEvaluationConfiguration constructLearningConfiguration_Compatibility(LearnerEvaluationConfiguration evalCnf)
	{
		Configuration config = evalCnf.config.copy();config.setLearnerScoreMode(Configuration.ScoreMode.COMPATIBILITY);
		LearnerEvaluationConfiguration copy = new LearnerEvaluationConfiguration(evalCnf);copy.config = config;
		return copy;
	}
	
	/** Merges states using a routing relying on PTA, that faster and consumes less memory than the general one. In addition, it aborts learning if the outcome has too many red states. */
	public static class ReferenceLearnerUsingSiccoScoring extends ReferenceLearner
	{

		protected final boolean scoringSiccoRecursive;
		
		public ReferenceLearnerUsingSiccoScoring(LearnerEvaluationConfiguration evalCnf, final LearnerGraph argInitialPTA, boolean SiccoRecursive) 
		{
			super(constructLearningConfiguration_Compatibility(evalCnf),argInitialPTA,SiccoRecursive? OverrideScoringToApply.SCORING_SICCO_RECURSIVE:OverrideScoringToApply.SCORING_SICCO);
			scoringSiccoRecursive = SiccoRecursive;
		}

		
		@Override
		public String toString()
		{
			return scoringSiccoRecursive? "SiccoR":"SiccoN";
		}
	}
	
	/** This one is a reference learner. */
	public static class KTailsReferenceLearner extends ReferenceLearner
	{
		private static LearnerEvaluationConfiguration constructConfiguration(LearnerEvaluationConfiguration evalCnf, boolean allPaths, int k)
		{
			Configuration config = evalCnf.config.copy();config.setLearnerScoreMode(allPaths? Configuration.ScoreMode.KTAILS:Configuration.ScoreMode.KTAILS_ANY);config.setKlimit(k);
			LearnerEvaluationConfiguration copy = new LearnerEvaluationConfiguration(evalCnf);copy.config = config;
			return copy;
		}
		
		public KTailsReferenceLearner(LearnerEvaluationConfiguration evalCnf, final LearnerGraph argInitialPTA, boolean allPaths, int k) 
		{
			super(constructConfiguration(evalCnf,allPaths,k),argInitialPTA,OverrideScoringToApply.SCORING_NO_OVERRIDE);
		}
		
		@Override
		public String toString()
		{
			return (config.getLearnerScoreMode() == Configuration.ScoreMode.KTAILS? "k-tails":"k-tails(a)")+","+config.getKlimit();
		}		
	}
	
	/** This one is a reference EDSM learner, using the provided thresholds for pair rejection (negative means no rejection). */
	public static class EDSMReferenceLearner extends ReferenceLearner
	{
		private static LearnerEvaluationConfiguration constructConfiguration(LearnerEvaluationConfiguration evalCnf, int threshold)
		{
			Configuration config = evalCnf.config.copy();config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);config.setRejectPositivePairsWithScoresLessThan(threshold);
			LearnerEvaluationConfiguration copy = new LearnerEvaluationConfiguration(evalCnf);copy.config = config;
			return copy;
		}

		public EDSMReferenceLearner(LearnerEvaluationConfiguration evalCnf, final LearnerGraph argInitialPTA, int threshold) 
		{
			super(constructConfiguration(evalCnf,threshold), argInitialPTA,OverrideScoringToApply.SCORING_NO_OVERRIDE);
		}

		@Override
		public String toString()
		{
			return config.getLearnerScoreMode()+",>="+config.getRejectPositivePairsWithScoresLessThan();
		}		
	}

	/** Uses the supplied classifier to rank pairs. */
	public static class LearnerThatDelegatesToTheSuppliedClassifier extends LearnerThatCanClassifyPairs
	{
		private int num_states;
		private int numtraceQuantity;
		private int num_seed;
		private int lengthMultiplier;
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

		private static LearnerEvaluationConfiguration constructConfiguration(LearnerEvaluationConfiguration evalCnf)
		{
			Configuration config = evalCnf.config.copy();config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);
			LearnerEvaluationConfiguration copy = new LearnerEvaluationConfiguration(evalCnf);copy.config = config;
			return copy;
		}

		public LearnerThatDelegatesToTheSuppliedClassifier(LearnerEvaluationConfiguration evalCnf,final LearnerGraph argReferenceGraph, final LearnerGraph argInitialPTA) 
		{
			super(constructConfiguration(evalCnf),argReferenceGraph,argInitialPTA,OverrideScoringToApply.SCORING_NO_OVERRIDE);
		}
		
		public static String refToString(Object obj)
		{
			return obj == null?"null":obj.toString();
		}
		
		@Override 
		public Stack<PairScore> ChooseStatePairs(final LearnerGraph graph)
		{
			Stack<PairScore> outcome = graph.pairscores.chooseStatePairs(LearnerThatDelegatesToTheSuppliedClassifier.this.computationOverride);
			
			if (!outcome.isEmpty())
			{
				PairScore result = null;
				
				result=LearningSupportRoutines.pickPairQSMLike(outcome);
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
	
	public static class RedPriorityOverBluePairSelectionRoutine implements statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure
	{
		protected final MarkovModel m;
		
		public RedPriorityOverBluePairSelectionRoutine(MarkovModel mm)
		{
			m=mm;
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

	}
	
	public static class EvaluationOfExisingLearnerRunner implements Callable<ThreadResult>
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
		
		public EvaluationOfExisingLearnerRunner(int argStates, int argSample, int argSeed, int nrOfTraces, Configuration conf, ConvertALabel conv)
		{
			states = argStates;sample = argSample;config = conf;seed = argSeed;traceQuantity=nrOfTraces;converter=conv;
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
			final Collection<List<Label>> testSet = buildEvaluationSet(referenceGraph);

			for(int attempt=0;attempt<2;++attempt)
			{// try learning the same machine a few times
				LearnerGraph pta = new LearnerGraph(config);
				RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(attempt),5,null);
				final int tracesToGenerate = LearningSupportRoutines.makeEven(traceQuantity);
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

				final MarkovModel m= new MarkovModel(chunkLen,true,true,false);
				new MarkovClassifier(m, pta).updateMarkov(false);
				pta.clearColours();

				if (!onlyUsePositives)
					assert pta.getStateNumber() > pta.getAcceptStateNumber() : "graph with only accept states but onlyUsePositives is not set";
				else 
					assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
				
				final Configuration deepCopy = pta.config.copy();deepCopy.setLearnerCloneGraph(true);
				SampleData dataSample=new SampleData();
				dataSample.miscGraphs = new TreeMap<String,ScoresForGraph>();
				List<ReferenceLearner> learnerList = new ArrayList<ReferenceLearner>();
				
				LearnerGraph ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);
				learnerList.add(new ReferenceLearnerUsingSiccoScoring(learnerEval,ptaCopy,true));
				ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);
				learnerList.add(new ReferenceLearnerUsingSiccoScoring(learnerEval,ptaCopy,false));
				ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);
				learnerList.add(new KTailsReferenceLearner(learnerEval,ptaCopy,true,1));
				ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);
				learnerList.add(new KTailsReferenceLearner(learnerEval,ptaCopy,true,2));
				ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);
				learnerList.add(new KTailsReferenceLearner(learnerEval,ptaCopy,false,1));
				ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);
				learnerList.add(new KTailsReferenceLearner(learnerEval,ptaCopy,false,2));
				ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);
				learnerList.add(new EDSMReferenceLearner(learnerEval,ptaCopy,1));
				ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);
				learnerList.add(new EDSMReferenceLearner(learnerEval,ptaCopy,2));
				ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);
				learnerList.add(new EDSMReferenceLearner(learnerEval,ptaCopy,3));
				ptaCopy = new LearnerGraph(deepCopy);LearnerGraph.copyGraphs(pta, ptaCopy);
				learnerList.add(new EDSMReferenceLearner(learnerEval,ptaCopy,4));
				
				for(ReferenceLearner learnerToUse:learnerList)
					try
					{
						dataSample.miscGraphs.put(learnerToUse.toString(),estimateDifference(referenceGraph,learnerToUse.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>()),testSet));
					}
					catch(LearnerAbortedException ex)
					{// the exception is thrown because the learner failed to learn anything completely.
						dataSample.miscGraphs.put(learnerToUse.toString(),zeroScore);
					}
				System.out.println(dataSample);
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
	
	public static final ScoresForGraph zeroScore;
	static
	{
		zeroScore = new ScoresForGraph();zeroScore.differenceBCR=new DifferenceToReferenceLanguageBCR(0, 0, 0, 0);zeroScore.differenceStructural=new DifferenceToReferenceDiff(0, 0);
	}

	/** Checks if the supplied graph has <i>k</i> matching transitions from vA and vB. Expects a graph on the left-hand side to contain sequence and
	 * a the one on the right to form a directed graph, permitting one to merge sequences into an existing machine, thereby avoiding having to perform
	 * pairwise comparisons between millions of states.
	 *  
	 * @param gr graph to consider
	 * @param vA first vertex (should be a sequence)
	 * @param vB second vertex (can be a state machine)
	 * @param k length of paths to consider
	 * @return whether paths of length k are the same and false otherwise.
	 */
	public static boolean checkMatch(LearnerGraph gr, CmpVertex vA, CmpVertex vB, int k)
	{
		if (k == 0)
			return vA.isAccept() == vB.isAccept();
		if (k < 0)
			throw new IllegalArgumentException("k has to be 0 or above");
		Map<Label,CmpVertex> outgoingA = gr.transitionMatrix.get(vA), outgoingB = gr.transitionMatrix.get(vB);
		if (outgoingA.size() == 0)
			return false;
		if (outgoingB.size() == 0)
			return false;
		
		if (outgoingA.size() != 1)
			throw new IllegalArgumentException("the graph should have traces in it with no branches");
		
		Label out = outgoingA.keySet().iterator().next();
		if (!outgoingB.containsKey(out))
			return false;
		
		return checkMatch(gr,outgoingA.get(out), outgoingB.get(out),k-1);
	}
	
	public static LearnerGraph incrementalKtails(Collection<List<Label>> positive, Collection<List<Label>> negative, int k,Configuration config)
	{
		LearnerGraph outcome = null;
		try
		{
			outcome = incrementalKtailsHelper(positive,negative,k,config);
		}
		catch(IncompatibleStatesException e)
		{
			Helper.throwUnchecked("failed to build a graph", e);
		}
		
		return outcome;
	}
	
	public static LearnerGraph ptaKtails(Collection<List<Label>> positive, Collection<List<Label>> negative, int k,Configuration config)
	{
		LearnerGraph outcome = null;
		try
		{
			outcome = traditionalPTAKtailsHelper(positive,negative,k,config);
		}
		catch(IncompatibleStatesException e)
		{
			Helper.throwUnchecked("failed to build a graph", e);
		}
		
		return outcome;
	}
	
	public static LearnerGraph ptaConcurrentKtails(Collection<List<Label>> positive, Collection<List<Label>> negative, int k,Configuration config, String ndFileName)
	{
		
		LearnerGraph pta = new LearnerGraph(config);
		pta.paths.augmentPTA(positive, true, false);pta.paths.augmentPTA(negative, false, false);
		return ptaConcurrentKtails(pta, k,ndFileName);
	}
	
	public static LearnerGraph ptaKtails(LearnerGraph graph, int k)
	{
		LearnerGraph outcome = null;
		try
		{
			outcome = traditionalPTAKtailsHelper(graph,k);
		}
		catch(IncompatibleStatesException e)
		{
			Helper.throwUnchecked("failed to build a graph", e);
		}
		
		return outcome;
	}
	
	public static LearnerGraph traditionalKtails(Collection<List<Label>> positive, Collection<List<Label>> negative, int k,Configuration config)
	{
		LearnerGraph outcome = null;
		try
		{
			outcome = traditionalKtailsHelper(positive,negative,k,config);
		}
		catch(IncompatibleStatesException e)
		{
			Helper.throwUnchecked("failed to build a graph", e);
		}
		
		return outcome;
	}
	
	
	public static LearnerGraph incrementalKtailsHelper(Collection<List<Label>> positive, Collection<List<Label>> negative, int k,Configuration config) throws IncompatibleStatesException
	{
		LearnerGraph currentGraph = new LearnerGraph(config);
		for(List<Label> pos:positive)
			currentGraph=incrementalKtailsHelper(pos,true,k,currentGraph);
		for(List<Label> neg:negative)
			currentGraph=incrementalKtailsHelper(neg,false,k,currentGraph);
		return currentGraph;
	}
	
	/**
	 * Given a map from vertices to vertices to merge with, constructs a non-deterministic graph and then calls 
	 * {@link AbstractPathRoutines#buildDeterministicGraph()} to build a deterministic version of it.
	 *  
	 * @param existingGraph graph with traces from which an ND graph will be built by merging states. 
	 * @param stateToEquivalenceClass determines which states to merge.
	 * @param initial the initial state in the ND graph
	 * @param ndFileName the name of the file to save the graph before running the determinisation, in case it blows and I need to re-run a lengthy experiment. Ignored if null.
	 * @return deterministic version of the provided graph with state mergers described by<i>stateToEquivalenceClass</i> carried out.
	 * @throws IncompatibleStatesException
	 */
	static LearnerGraph constructKTailsNDGraphAndDeterminizeIt(LearnerGraph existingGraph,Map<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> stateToEquivalenceClass,CmpVertex initial, String ndFileName) throws IncompatibleStatesException
	{
		LearnerGraphND ndGraph = new LearnerGraphND(existingGraph.config.copy());
		// we are not building merged vertices, preferring instead to use representative vertices.

		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:existingGraph.transitionMatrix.entrySet())
		{
			EquivalenceClass<CmpVertex,LearnerGraphCachedData> sourceEq = stateToEquivalenceClass.get(entry.getKey());
			CmpVertex considerTransitionsOriginatingFrom = entry.getKey();
			if (sourceEq != null)
				// the current state is part of an equivalence class. Associate all outgoing transitions with a representative state.
				considerTransitionsOriginatingFrom = sourceEq.getRepresentative();
			
			Map<Label,List<CmpVertex>> transitionRow = ndGraph.transitionMatrix.get(considerTransitionsOriginatingFrom);
			if (transitionRow == null)
			{
				transitionRow = ndGraph.createNewRow();ndGraph.transitionMatrix.put(considerTransitionsOriginatingFrom, transitionRow);
			}
			for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
			{
				EquivalenceClass<CmpVertex,LearnerGraphCachedData> targetEq = stateToEquivalenceClass.get(transition.getValue());
				CmpVertex considerTarget = transition.getValue();
				if (targetEq != null)
					considerTarget = targetEq.getRepresentative();
				List<CmpVertex> targetStates = transitionRow.get(transition.getKey());
				if (targetStates == null)
				{
					targetStates = new ArrayList<CmpVertex>();transitionRow.put(transition.getKey(), targetStates);
				}
				targetStates.add(considerTarget);
			}
		}
		ndGraph.setInit(initial);
		if (ndFileName != null)
			try
			{
				ndGraph.storage.writeGraphML(ndFileName);
			} catch (IOException e)
			{
				System.err.println("Failed to write ND graph to "+ndFileName+", exception "+e.getMessage());
				e.printStackTrace();
			}
		return ndGraph.pathroutines.buildDeterministicGraph();
	}
	
	public static LearnerGraph incrementalKtailsHelper(List<Label> sequence, boolean positive, int k,LearnerGraph existingGraph) throws IncompatibleStatesException
	{
		AMEquivalenceClassMergingDetails mergingDetails = new AMEquivalenceClassMergingDetails();mergingDetails.nextEquivalenceClass = 0;
		Pair<Integer,Integer> acceptRejectNumber = existingGraph.getAcceptAndRejectStateNumber();
		Map<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> stateToEquivalenceClass =  
				existingGraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>(acceptRejectNumber.firstElem+1,acceptRejectNumber.secondElem+1):
				new HashMapWithSearch<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>(acceptRejectNumber.firstElem+acceptRejectNumber.secondElem+1);
			EquivalenceClass<CmpVertex,LearnerGraphCachedData> initialEQ = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(mergingDetails.nextEquivalenceClass++,existingGraph);
			initialEQ.mergeWith(existingGraph.getInit(),null);
			stateToEquivalenceClass.put(existingGraph.getInit(), initialEQ);
			if (!positive && sequence.size() == 0)
				throw new IllegalArgumentException("graphs with initial state reject-state are not presently supported");
		
			CmpVertex startForPath = AbstractLearnerGraph.generateNewCmpVertex(existingGraph.nextID(true),existingGraph.config);
			existingGraph.transitionMatrix.put(startForPath,existingGraph.createNewRow());
			existingGraph.paths.augmentPTA(sequence, startForPath, positive, false,null);
			existingGraph.pairscores.mergePair(new StatePair(existingGraph.getInit(),startForPath),stateToEquivalenceClass,mergingDetails);
		
			CmpVertex vA = startForPath;
			Set<CmpVertex> visitedVertices = new LinkedHashSet<CmpVertex>();
			while(vA != null)
			{
				visitedVertices.add(vA);
				
				for(CmpVertex vB:existingGraph.transitionMatrix.keySet())
					if (!visitedVertices.contains(vB) && checkMatch(existingGraph,vA,vB,k))
						existingGraph.pairscores.mergePair(new StatePair(vA,vB),stateToEquivalenceClass,mergingDetails);

				Map<Label,CmpVertex> next = existingGraph.transitionMatrix.get(vA);
				if (!next.isEmpty())
					vA=next.values().iterator().next();
				else
					vA=null;
			}
			return constructKTailsNDGraphAndDeterminizeIt(existingGraph,stateToEquivalenceClass,initialEQ.getRepresentative(),null);	
	}
	
	public static long computeStateScoreKTails(LearnerGraph gr, StatePair pair, int k, boolean anyPath)
	{
		if (!AbstractLearnerGraph.checkCompatible(pair.getR(),pair.getQ(),gr.pairCompatibility))
			return -1;

		boolean anyMatched = false;// we need to distinguish a wave where all (or any) transitions matched from a wave where no transitions were possible. This variable is set when any match is obtained.
		int currentExplorationDepth=1;// when we look at transitions from the initial pair of states, this is depth 1.
		assert pair.getQ() != pair.getR();
		
		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
		if (currentExplorationDepth <= k)
			currentExplorationBoundary.add(pair);
		currentExplorationBoundary.offer(null);
		
		while(true) // we'll do a break at the end of the last wave
		{
			StatePair currentPair = currentExplorationBoundary.remove();
			if (currentPair == null)
			{// we got to the end of a wave
				if (currentExplorationBoundary.isEmpty())
					break;// we are at the end of the last wave, stop looping.

				// mark the end of a wave.
				currentExplorationBoundary.offer(null);currentExplorationDepth++;anyMatched = false;
			}
			else
			{
				Map<Label,CmpVertex> targetRed = gr.transitionMatrix.get(currentPair.getR()),
					targetBlue = gr.transitionMatrix.get(currentPair.getQ());
	
				for(Entry<Label,CmpVertex> redEntry:targetRed.entrySet())
				{
					CmpVertex nextBlueState = targetBlue.get(redEntry.getKey());
					if (nextBlueState != null)
					{// both states can make a transition
						if (!AbstractLearnerGraph.checkCompatible(redEntry.getValue(),nextBlueState,gr.pairCompatibility))
							return -1;// definitely incompatible states, fail regardless whether we should look for a single or all paths. 
						
						anyMatched = true;// mark that in the current wave, we've seen at least one matched pair of transitions.
						
						if (currentExplorationDepth < k)
						{// if our current depth is less than the one to explore, make subsequent steps.
							StatePair nextStatePair = new StatePair(nextBlueState,redEntry.getValue());
							currentExplorationBoundary.offer(nextStatePair);
						}
						// If we did not take the above condition (aka reached the maximal depth to explore), we still cannot break out of a loop even if we have anyPath
						// set to true, because there could be transitions leading to states with different accept-conditions, hence explore all matched transitions.						
					}
					else
					{
						// if the red can make a move, but the blue one cannot, do not merge the pair unless any path is good enough
						if (!anyPath)
							return -1;
					}
				}
				
				for(Entry<Label,CmpVertex> blueEntry:targetBlue.entrySet())
					if (null == targetRed.get(blueEntry.getKey()))
					{
						if (!anyPath)
							return -1;// if blue can make a transition and red cannot, stop unless we are looking for any path
					}
			}
		}
		
		return anyMatched || k == 0?0:-1;// if no transitions matched in a wave, this means that we reached tail-end of a graph before exhausting the exploration depth, thus the score is -1.
	}

	public static LearnerGraph traditionalKtailsHelper(Collection<List<Label>> positive, Collection<List<Label>> negative, int k,Configuration config) throws IncompatibleStatesException
	{
		LearnerGraph graphWithTraces = new LearnerGraph(config);
		AMEquivalenceClassMergingDetails mergingDetails = new AMEquivalenceClassMergingDetails();mergingDetails.nextEquivalenceClass = 0;
		Map<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> stateToEquivalenceClass =  
				config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>(positive.size()+1,negative.size()+1):
				new HashMapWithSearch<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>(positive.size()+negative.size()+1);
			EquivalenceClass<CmpVertex,LearnerGraphCachedData> initialEQ = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(mergingDetails.nextEquivalenceClass++,graphWithTraces);
			initialEQ.mergeWith(graphWithTraces.getInit(),null);
			stateToEquivalenceClass.put(graphWithTraces.getInit(), initialEQ);
			for(List<Label> pos:positive)
			{
				CmpVertex startForPath = AbstractLearnerGraph.generateNewCmpVertex(graphWithTraces.nextID(true),graphWithTraces.config);
				graphWithTraces.transitionMatrix.put(startForPath,graphWithTraces.createNewRow());
				graphWithTraces.paths.augmentPTA(pos, startForPath, true, false,null);
				graphWithTraces.pairscores.mergePair(new StatePair(graphWithTraces.getInit(),startForPath),stateToEquivalenceClass,mergingDetails);
			}
			for(List<Label> neg:negative)
			{
				if (neg.size() == 0)
				{
					//CmpVertex startForPath = AbstractLearnerGraph.generateNewCmpVertex(outcome.nextID(false),outcome.config);
					//outcome.transitionMatrix.put(startForPath,outcome.createNewRow());
					throw new IllegalArgumentException("graphs with initial state reject-state are not presently supported");
				}
				else
				{
					CmpVertex startForPath = AbstractLearnerGraph.generateNewCmpVertex(graphWithTraces.nextID(true),graphWithTraces.config);
					graphWithTraces.transitionMatrix.put(startForPath,graphWithTraces.createNewRow());
					graphWithTraces.paths.augmentPTA(neg, startForPath, false, false,null);
					graphWithTraces.pairscores.mergePair(new StatePair(graphWithTraces.getInit(),startForPath),stateToEquivalenceClass,mergingDetails);
				}
			}
			
			for(CmpVertex vA:graphWithTraces.transitionMatrix.keySet())
			{
				for(CmpVertex vB:graphWithTraces.transitionMatrix.keySet())
					if (vA == vB)
						break;
					else
					{
						if (checkMatch(graphWithTraces,vA,vB,k))
							graphWithTraces.pairscores.mergePair(new StatePair(vA,vB),stateToEquivalenceClass,mergingDetails);
					}
			}
			
			return constructKTailsNDGraphAndDeterminizeIt(graphWithTraces,stateToEquivalenceClass,initialEQ.getRepresentative(),null);	
	}

	public static LearnerGraph traditionalPTAKtailsHelper(Collection<List<Label>> positive, Collection<List<Label>> negative,int k,Configuration config) throws IncompatibleStatesException
	{
		LearnerGraph pta = new LearnerGraph(config);
		pta.paths.augmentPTA(positive, true, false);pta.paths.augmentPTA(negative, false, false);
		return traditionalPTAKtailsHelper(pta,k);
	}
	
	public static LearnerGraph traditionalPTAKtailsHelper(LearnerGraph pta, int k) throws IncompatibleStatesException
	{
		AMEquivalenceClassMergingDetails mergingDetails = new AMEquivalenceClassMergingDetails();mergingDetails.nextEquivalenceClass = 0;
		Pair<Integer,Integer> acceptRejectNumber = pta.getAcceptAndRejectStateNumber();
		Map<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> stateToEquivalenceClass =  
				pta.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>(acceptRejectNumber.firstElem+1,acceptRejectNumber.secondElem+1):
				new HashMapWithSearch<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>(acceptRejectNumber.firstElem+acceptRejectNumber.secondElem+1);
			EquivalenceClass<CmpVertex,LearnerGraphCachedData> initialEQ = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(mergingDetails.nextEquivalenceClass++,pta);
			initialEQ.mergeWith(pta.getInit(),null);
			stateToEquivalenceClass.put(pta.getInit(), initialEQ);
			long time = System.currentTimeMillis(),Cnt=0, mergersCnt=0;
			double total=(double)pta.getAcceptStateNumber()*(pta.getAcceptStateNumber()+1)/2;
			final Map<CmpVertex,List<CmpVertex>> mergedVertices = AbstractLearnerGraph.constructMap(pta.config,pta);
			boolean hasNegativesTentative = false;// if no negatives, no need to check compatibility of vertices for mergers.
			for(CmpVertex v:pta.transitionMatrix.keySet())
				if (!v.isAccept())
				{
					hasNegativesTentative = true;break;
				}
			final boolean hasNegatives = hasNegativesTentative;// set the permanent value for the parallel computation to access via closure.
			System.out.println(new Date()+"started to perform pairwise comparisons, total number of comparisons "+total+" hasNegatives is "+hasNegatives);
			for(CmpVertex vA:pta.transitionMatrix.keySet())
			if (vA.isAccept() || k ==0)
			{
				for(CmpVertex vB:pta.transitionMatrix.keySet())
					if (vA == vB)
						break;
					else
					if (vB.isAccept() || k ==0)
					{
						long curr = System.currentTimeMillis();
						if (curr-time > 60000)
						{
							System.out.println(new Date()+"Progress: "+(100*(double)Cnt/total)+" current value "+Cnt+" completed "+mergersCnt+" mergers");
							time = curr;
						}
						StatePair pair = new StatePair(vA,vB);
						if (computeStateScoreKTails(pta,pair,k,true) >=0)
						{
							boolean mergerPossible = true;
						
							if (hasNegatives)
							{
								mergedVertices.clear();
								mergerPossible = pta.pairscores.computePairCompatibilityScore_internal(pair, mergedVertices) >= 0;
							}
							if (mergerPossible)
								pta.pairscores.mergePair(pair,stateToEquivalenceClass,mergingDetails);
						}
						++Cnt;
					}
			}
			System.out.println(new Date()+"started to make deterministic");
			return constructKTailsNDGraphAndDeterminizeIt(pta,stateToEquivalenceClass,initialEQ.getRepresentative(),null);	
	}
	
	public static LearnerGraph ptaConcurrentKtails(final LearnerGraph pta, final int k, String ndFileName)
	{
		final AMEquivalenceClassMergingDetails mergingDetails = new AMEquivalenceClassMergingDetails();mergingDetails.nextEquivalenceClass = 0;
		Pair<Integer,Integer> acceptRejectNumber = pta.getAcceptAndRejectStateNumber();
		final Map<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> stateToEquivalenceClass =  
				pta.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>(acceptRejectNumber.firstElem+1,acceptRejectNumber.secondElem+1):
				new HashMapWithSearch<CmpVertex,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>(acceptRejectNumber.firstElem+acceptRejectNumber.secondElem+1);
			EquivalenceClass<CmpVertex,LearnerGraphCachedData> initialEQ = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(mergingDetails.nextEquivalenceClass++,pta);
			try
			{
				initialEQ.mergeWith(pta.getInit(),null);
			}
			catch(IncompatibleStatesException e)
			{
				Helper.throwUnchecked("failed to merge states in the construction of the initial eq class", e);
			}
			
			stateToEquivalenceClass.put(pta.getInit(), initialEQ);
			double total=(double)pta.getAcceptStateNumber()*(pta.getAcceptStateNumber()+1)/2;
			final Map<CmpVertex,List<CmpVertex>> mergedVertices = AbstractLearnerGraph.constructMap(pta.config,pta);
			boolean hasNegativesTentative = false;// if no negatives, no need to check compatibility of vertices for mergers.
			for(CmpVertex v:pta.transitionMatrix.keySet())
				if (!v.isAccept())
				{
					hasNegativesTentative = true;break;
				}
			final boolean hasNegatives = hasNegativesTentative;// set the permanent value for the parallel computation to access via closure.
			System.out.println(new Date()+"started to perform pairwise comparisons, total number of comparisons "+total+" hasNegatives is "+hasNegatives);
			int threadNumber = ExperimentRunner.getCpuNumber();
			List<HandleRow<CmpVertex>> handlerList = new LinkedList<HandleRow<CmpVertex>>();
			for(int threadCnt=0;threadCnt<threadNumber;++threadCnt)
			handlerList.add(new HandleRow<CmpVertex>()
			{
				@Override
				public void init(@SuppressWarnings("unused") int threadNo) {
					// No per-thread initialisation is needed.
				}

				@Override
				public void handleEntry(Entry<CmpVertex, Map<Label, CmpVertex>> entryA, @SuppressWarnings("unused") int threadNo)
				{// we are never called with entryA which has been filtered out.
					CmpVertex stateA=entryA.getKey();
					Iterator<Entry<CmpVertex,Map<Label,CmpVertex>>> stateB_It = pta.transitionMatrix.entrySet().iterator();
					while(stateB_It.hasNext())
					{
						CmpVertex stateB = stateB_It.next().getKey();
						if (stateB.equals(stateA)) 
							break; // we only process a triangular subset.
						if (k == 0 || stateB.isAccept())
						{
							StatePair pair = new StatePair(stateA,stateB);
							if (computeStateScoreKTails(pta,pair,k,true) >=0)
							{
								boolean mergerPossible = true;
							
								if (hasNegatives)
								{
									mergedVertices.clear();
									mergerPossible = pta.pairscores.computePairCompatibilityScore_internal(pair, mergedVertices) >= 0;
								}
								if (mergerPossible)
								{
									try
									{
										synchronized(stateToEquivalenceClass)
										{// modifications to equivalence classes have to be made in sync
											pta.pairscores.mergePair(pair,stateToEquivalenceClass,mergingDetails);
										}
									} catch (IncompatibleStatesException e)
									{
										Helper.throwUnchecked("failed to merge states", e);
									}
								}
							}
						}
					}
				}
			});
			
			StatesToConsider filter = new StatesToConsider()
			{
				@Override
				public boolean stateToConsider(CmpVertex vert) 
				{
					if (k == 0)
						return true;
					
					return vert.isAccept();
				}
			};
			GDLearnerGraph.performRowTasks(handlerList, threadNumber, pta.transitionMatrix, filter,GDLearnerGraph.partitionWorkLoadTriangular(threadNumber,pta.transitionMatrix,filter));
			
			System.out.println(new Date()+"started to make deterministic");
			LearnerGraph outcome = null;
			try
			{
				outcome = constructKTailsNDGraphAndDeterminizeIt(pta,stateToEquivalenceClass,initialEQ.getRepresentative(),ndFileName);
			}
			catch(IncompatibleStatesException e)
			{
				Helper.throwUnchecked("failed to merge states in the final merge", e);
			}
			
			return outcome;
	}

}


