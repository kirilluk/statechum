/* Copyright (c) 2016 The University of Sheffield
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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.Stack;
import java.util.Timer;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovScoreComputation;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceDiff;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceLanguageBCR;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.TrueFalseCounter;
import statechum.analysis.learning.linear.GDLearnerGraph;
import statechum.analysis.learning.linear.GDLearnerGraph.HandleRow;
import statechum.analysis.learning.observers.LearnerReturningOriginalGraph;
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
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.collections.ArrayMapWithSearch;
import statechum.collections.HashMapWithSearch;
import statechum.model.testset.PTASequenceEngine;

public class LearningAlgorithms
{
	/** In a number of experiments, evaluation of automata is conducted by exploration either by generating a test set or by generating walks of some length. Both are governed by a bound:
	 * if a learnt automaton has traces that cannot be explored within that bound, it is quite easy to obtain something with a huge number of states that passes all check with flying colours.
	 * This is the maximal number of states permitted in a learnt graph, as a multiplier of a number of states in a reference graph. 
	 */
	public static int maxStateNumberMultiplier = 2;
	
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
		public final boolean timedOut;
		
		public LearnerAbortedException(boolean value)
		{
			timedOut = value;
		}
		
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
							throw new LearnerAbortedException(false);
			}
		}
	}

	/** This class knows what the reference automaton is and is able to pick correct pairs out of a set to merge. */
	public static abstract class LearnerWithMandatoryMergeConstraints extends RPNIUniversalLearner
	{
		protected LearnerGraph initialPTA;
		
		protected boolean taskTimedOut = false;
		/** Negative means the timer was cancelled. */
		protected long startTime = -1;
		
		public class CancelOnTimeout extends java.util.TimerTask
		{
			protected final Timer tmr;
			protected final long threadID;
			protected final double scale;
			
			public CancelOnTimeout(Timer tm, long id, double s)
			{
				tmr = tm;threadID = id;scale = s;
			}
			@SuppressWarnings("synthetic-access")
			@Override
			public void run() {
				// thanks to http://stackoverflow.com/questions/14664897/measure-java-short-time-running-thread-execution-time
				long currTime = java.lang.management.ManagementFactory.getThreadMXBean().getThreadCpuTime(threadID);
				if (startTime >= 0 && config.getTimeOut() >= 0 && currTime >= config.getTimeOut()*1000000L*scale+startTime)
				{
					taskTimedOut = true;startTime = -2;tmr.cancel();
				}
			}
		}
		
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
		
		@Override
		public LearnerGraph init(LearnerGraph initPta)
		{
			if (getTentativeAutomaton() == initPta)
				throw new IllegalArgumentException("the graph to learn from should not be an outcome of learning by this leaner, please make a copy of the graph first");
			initialPTA = initPta;
			return initialPTA;
		}
		
		@SuppressWarnings("unused")
		@Override 
		public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
		{
			throw new UnsupportedOperationException();
		}			
		
		public void checkTimeout()
		{
			if (taskTimedOut) // will be set by the timer thread when the task eats its quota of CPU time.
				throw new LearnerAbortedException(true);
		}

		@Override 
		public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair)
		{
			
			LearnerGraph outcome = null;
			checkTimeout();
			
			if (config.getOverride_usePTAMerging())
			{
				outcome = MergeStates.mergeAndDeterminize(original, pair);
			}
			else
			{
				Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
				if (original.pairscores.computePairCompatibilityScore_general(pair,null,mergedVertices, false) < 0)
					throw new IllegalArgumentException("elements of the pair "+pair+" are incompatible, orig score was "+original.pairscores.computePairCompatibilityScore(pair));
				outcome = MergeStates.mergeCollectionOfVertices(original,pair.getR(),mergedVertices,false);
			}
			outcome.pathroutines.updateDepthLabelling();// this is important for the choice of representative vertices in merging of states, this in turn affects IDs of merged states which affects selection of pairs for merging.
			return outcome;
		}
		
		@Override
		public LearnerGraph learnMachine()
		{
			Timer tmTimer =  null;
			LearnerGraph outcome = null;
			
			if (config.getTimeOut() >= 0)
			{
				startTime = LearningSupportRoutines.getThreadTime();
				tmTimer =  new Timer();tmTimer.schedule(new CancelOnTimeout(tmTimer,Thread.currentThread().getId(),LearningSupportRoutines.getFreqCorrectionValue()),1000,1000);
			}
			try
			{
				outcome = super.learnMachine();
			}
			finally
			{
				if (tmTimer != null)
					tmTimer.cancel();
			}
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
	public enum ScoringToApply { SCORING_EDSM("E0"), SCORING_EDSM_1("E1"), SCORING_EDSM_2("E2"), SCORING_EDSM_3("E3"), SCORING_EDSM_4("E4"), SCORING_EDSM_5("E5"), 
		SCORING_EDSM_6("E6"), SCORING_EDSM_7("E7"), SCORING_EDSM_8("E8"), SCORING_EDSM_9("E9"), SCORING_EDSM_10("E10"), SCORING_EDSM_11("E11"), SCORING_EDSM_12("E12"), 
		SCORING_SICCO("SICCO"), SCORING_SICCO_PTA("SICPTA"),SCORING_SICCO_PTARECURSIVE("SICREC"), SCORING_SICCO_NIS("SICNIS"), SCORING_SICCO_RED("SICRED"),
		SCORING_PTAK_1("KTPTA1"),SCORING_PTAK_2("KTPTA2"),SCORING_PTAK_3("KTPTA3"),SCORING_PTAK_4("KTPTA4"),
		SCORING_KT_1("TAIL1"), SCORING_KT_2("TAIL2"), SCORING_KT_3("TAIL3"), SCORING_KT_4("TAIL4");
		
		public final String name;
		private ScoringToApply(String nameText)
		{
			name = nameText;
		}
	}

	public static Learner constructLearner(LearnerEvaluationConfiguration evalCnf, final LearnerGraph initialPTA, ScoringToApply howToScore, Configuration.ScoreMode scoringForEDSM) 
	{
		Learner outcome = null;
		//final int threadNumber = 1;// we run each experiment on a separate thread hence do not create many threads.
		switch(howToScore)
		{
		case SCORING_EDSM:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, scoringForEDSM, -1);break;
		case SCORING_EDSM_1:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, scoringForEDSM, 1);break;
		case SCORING_EDSM_2:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, scoringForEDSM, 2);break;
		case SCORING_EDSM_3:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, scoringForEDSM, 3);break;
		case SCORING_EDSM_4:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, scoringForEDSM, 4);break;
		case SCORING_EDSM_5:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, scoringForEDSM, 5);break;
		case SCORING_EDSM_6:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, scoringForEDSM, 6);break;
		case SCORING_EDSM_7:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, scoringForEDSM, 7);break;
		case SCORING_EDSM_8:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, scoringForEDSM, 8);break;
		case SCORING_EDSM_9:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, scoringForEDSM, 9);break;
		case SCORING_EDSM_10:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, scoringForEDSM, 10);break;
		case SCORING_EDSM_11:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, scoringForEDSM, 11);break;
		case SCORING_EDSM_12:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, scoringForEDSM, 12);break;
		case SCORING_KT_1:
			outcome = new LearnerReturningOriginalGraph() {
				@Override
				public LearnerGraph constructGraph() {
					return  ptaKtails(LearningSupportRoutines.removeAllNegatives(initialPTA),1);
				}};
			break;
		case SCORING_KT_2:
			outcome = new LearnerReturningOriginalGraph() {
				@Override
				public LearnerGraph constructGraph() {
					return  ptaKtails(LearningSupportRoutines.removeAllNegatives(initialPTA),2);
				}};
			break;
		case SCORING_KT_3:
			outcome = new LearnerReturningOriginalGraph() {
				@Override
				public LearnerGraph constructGraph() {
					return  ptaKtails(LearningSupportRoutines.removeAllNegatives(initialPTA),3);
				}};
			break;
		case SCORING_KT_4:
			outcome = new LearnerReturningOriginalGraph() {
				@Override
				public LearnerGraph constructGraph() {
					return  ptaKtails(LearningSupportRoutines.removeAllNegatives(initialPTA),4);
				}};
			break;
		case SCORING_PTAK_1:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, Configuration.ScoreMode.KTAILS_ANY, 1);break;
		case SCORING_PTAK_2:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, Configuration.ScoreMode.KTAILS_ANY, 2);break;
		case SCORING_PTAK_3:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, Configuration.ScoreMode.KTAILS_ANY, 3);break;
		case SCORING_PTAK_4:
			outcome = new EDSMReferenceLearner(evalCnf, initialPTA, Configuration.ScoreMode.KTAILS_ANY, 4);break;
		case SCORING_SICCO_PTA:
			outcome = new ReferenceLearner(constructLearningConfiguration(evalCnf, scoringForEDSM), initialPTA, ReferenceLearner.OverrideScoringToApply.SCORING_SICCO_PTA);break;
		case SCORING_SICCO_PTARECURSIVE:
			outcome = new ReferenceLearner(constructLearningConfiguration(evalCnf, scoringForEDSM), initialPTA, ReferenceLearner.OverrideScoringToApply.SCORING_SICCO_PTARECURSIVE);break;
		case SCORING_SICCO:
			outcome = new ReferenceLearner(constructLearningConfiguration(evalCnf, scoringForEDSM), initialPTA, ReferenceLearner.OverrideScoringToApply.SCORING_SICCO);break;
		case SCORING_SICCO_NIS:
			outcome = new ReferenceLearner(constructLearningConfiguration(evalCnf, scoringForEDSM), initialPTA, ReferenceLearner.OverrideScoringToApply.SCORING_SICCO_NIS);break;
		case SCORING_SICCO_RED:
			outcome = new ReferenceLearner(constructLearningConfiguration(evalCnf, scoringForEDSM), initialPTA, ReferenceLearner.OverrideScoringToApply.SCORING_SICCO_RED);break;
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
		public enum OverrideScoringToApply { SCORING_NO_OVERRIDE, SCORING_EDSM, SCORING_EDSM_1, SCORING_EDSM_2, SCORING_SICCO, SCORING_SICCO_PTA, SCORING_SICCO_PTARECURSIVE, SCORING_SICCO_NIS, SCORING_SICCO_RED }
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
				LearnerAbortedException.throwExceptionIfTooManyReds(outcome, config.getOverride_maximalNumberOfStates());// this is necessary if the selection of the first pair to merge marks everything red and returns an empty set
			}
			catch(LearnerAbortedException ex)
			{
				outcome = new LearnerGraph(config);
				outcome.getInit().setAccept(false);
				if (ex.timedOut)
					outcome.getInit().setHighlight(true);// abuse highlight to mean 'task timed out'
			}
			return outcome;
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
					case SCORING_SICCO_PTA:
						/*
						boolean negativeSiccoPTA = coregraph.pairscores.computeScoreSicco(p, false) < 0;
						coregraph.pairscores.computePairCompatibilityScore_general(p, null, mergedVertices, false);
						boolean negativeSicco = coregraph.pairscores.computeSiccoRejectScoreGeneral(p, mergedVertices, SiccoGeneralScoring.S_ONEPAIR) < 0;
						if (negativeSiccoPTA != negativeSicco)
						{
							Visualiser.updateFrame(coregraph.transform.trimGraph(4, coregraph.getInit()), null);
							coregraph.pairscores.computeScoreSicco(p, false);
							System.out.println("outgoing: "+coregraph.transitionMatrix.get(coregraph.findVertex(VertexID.parseID("P1000")))+" "+coregraph.transitionMatrix.get(coregraph.findVertex(VertexID.parseID("P1893"))));
							coregraph.pairscores.computeSiccoRejectScoreGeneral(p, mergedVertices, SiccoGeneralScoring.S_ONEPAIR);
						}*/
						if (p.getScore() >= 0 && coregraph.pairscores.computeScoreSicco(p, false) < 0)
							score = -1;
						break;
					case SCORING_SICCO_PTARECURSIVE:
						if (p.getScore() >= 0 && coregraph.pairscores.computeScoreSicco(p, true) < 0)
							score = -1;
						break;
					case SCORING_SICCO:
						if (p.getScore() >= 0)
						{
							coregraph.pairscores.computePairCompatibilityScore_general(p, null, mergedVertices, false);
							if (coregraph.pairscores.computeSiccoRejectScoreGeneral_fastreturn(p, mergedVertices, SiccoGeneralScoring.S_ONEPAIR) < 0)
								score = -1;
						}
						break;
					case SCORING_SICCO_NIS:
						if (p.getScore() >= 0 && p.getQ() != coregraph.getInit() && p.getR() != coregraph.getInit())
						{
							coregraph.pairscores.computePairCompatibilityScore_general(p, null, mergedVertices, false);
							if (coregraph.pairscores.computeSiccoRejectScoreGeneral_fastreturn(p, mergedVertices, SiccoGeneralScoring.S_ONEPAIR) < 0)
								score = -1;
						}
						break;
					case SCORING_SICCO_RED:
						if (p.getScore() >= 0)
						{
							coregraph.pairscores.computePairCompatibilityScore_general(p, null, mergedVertices, false);
							if (coregraph.pairscores.computeSiccoRejectScoreGeneral_fastreturn(p, mergedVertices, SiccoGeneralScoring.S_RED) < 0)
								score = -1;
						}
						break;
					default:// do nothing since this is the case where nothing needs to be done.
						break;
					}
					
					if (!labelsLeadingToStatesToBeMerged.isEmpty() || !labelsLeadingFromStatesToBeMerged.isEmpty())
						if (LearningSupportRoutines.computeScoreBasedOnMandatoryMerge(p, coregraph, labelsLeadingToStatesToBeMerged, labelsLeadingFromStatesToBeMerged) < 0)
							score = -1;
					checkTimeout();

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
	
	/** This learner infers models where all traces start with a specific transition from an initial state. It does not matter whether the graph to start from is a PTA or not.
	 * Such a learner is different from any other learner in that the starting state is not the initial state but the one entered by a unique transition and the advantage
	 * is that it takes into account a wide range of outgoing transitions from that state instead of just one.
	 */
	public static class LearnerWithUniqueFromInitial implements Learner
	{
		final Learner learner;
		final Label uniqueLabel;
		final Configuration config;
		
		@Override
		public LearnerGraph learnMachine() 
		{
			throw new UnsupportedOperationException("Not supported by this learner");
		}

		private static LearnerGraph recolouredInitialPTA(LearnerGraph graph,List<Label> initialSeq)
		{
			LearnerGraph recolouredPTA = new LearnerGraph(graph,graph.config);
			recolouredPTA.getInit().setColour(null);recolouredPTA.getVertex(initialSeq).setColour(JUConstants.RED);
			return recolouredPTA;
		}
		
		public LearnerWithUniqueFromInitial(Learner learnerToUse, LearnerGraph argInitialPTA, Label uniqueFromInitial) 
		{
			uniqueLabel = uniqueFromInitial;config = argInitialPTA.config;
			LearnerGraph initPTA = recolouredInitialPTA(argInitialPTA,Arrays.asList(new Label[]{uniqueFromInitial}));
			learner = learnerToUse;learner.init(initPTA);
		}

		@Override
		public void setTopLevelListener(Learner top)
		{
			learner.setTopLevelListener(top);
		}

		@SuppressWarnings("unused")
		@Override
		public LearnerGraph learnMachine(PTASequenceEngine engine, int plusSize, int minusSize) 
		{
			throw new UnsupportedOperationException("Not supported by this learner");
		}

		@Override
		public LearnerGraph learnMachine(Collection<List<Label>> plus, Collection<List<Label>> minus) 
		{
			LearnerGraph outcome = null;
			try
			{
				outcome = learner.learnMachine(plus, minus);
				LearnerAbortedException.throwExceptionIfTooManyReds(outcome, config.getOverride_maximalNumberOfStates());// this is necessary if the selection of the first pair to merge marks everything red and returns an empty set

				if (outcome.getInit().getColour() == null)
				{// since the initial state only has one transition to the state reached by the uniqueFromInitial, it will not receive a colour and hence will not participate in state merging.
					LearnerGraph tmp = new LearnerGraph(outcome,outcome.config);
					CmpVertex dummyVertex=AbstractLearnerGraph.generateNewCmpVertex(tmp.nextID(true), tmp.config);dummyVertex.setColour(JUConstants.RED);
					Map<Label,CmpVertex> outOfDummy = tmp.createNewRow();
					tmp.transitionMatrix.put(dummyVertex, outOfDummy);outOfDummy.put(uniqueLabel, tmp.getInit());// this is quite an elaborate way to force the learner make the initial state BLUE (as it is reached by a transition from the RED state)
					Stack<PairScore> pairs = learner.ChooseStatePairs(tmp);// now our initial state is considered for mergers for all the red state (and we exclude the dummy from consideration later).
					PairScore initialToMergeWith = null;
					for(PairScore p:pairs)
						if (p.getQ() == tmp.getInit() && p.getR() != dummyVertex)
						{
							initialToMergeWith = p;break;
						}
					if (initialToMergeWith != null)
					{// merge the initial vertex with one of the existing ones
						//tmp=learner.MergeAndDeterminize(tmp, initialToMergeWith);
						Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
						if (tmp.pairscores.computePairCompatibilityScore_general(initialToMergeWith,null,mergedVertices, false) < 0)
							throw new IllegalArgumentException("elements of the pair "+initialToMergeWith+" are incompatible, orig score was "+tmp.pairscores.computePairCompatibilityScore(initialToMergeWith));
						tmp = MergeStates.mergeCollectionOfVertices(tmp,initialToMergeWith.getR(),mergedVertices,false);// this performs the merge and updates the initial state to reflect it.
					}
					tmp.transitionMatrix.remove(dummyVertex);
					//
					outcome = tmp;
				}
			}
			catch(LearnerAbortedException ex)
			{
				outcome = new LearnerGraph(config);
				outcome.getInit().setAccept(false);
			}
			
			//Visualiser.updateFrame(outcome, null);
			return outcome;
		}

		@Override
		public String getResult() {
			return learner.getResult();
		}

		@SuppressWarnings("unused")
		@Override
		public LearnerGraph init(Collection<List<Label>> plus, Collection<List<Label>> minus) {
			throw new UnsupportedOperationException("The initial graph should be set by the constructor of LearnerWithUniqueFromInitial");
		}

		@SuppressWarnings("unused")
		@Override
		public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) {
			throw new UnsupportedOperationException("The initial graph should be set by the constructor of LearnerWithUniqueFromInitial");
		}

		@SuppressWarnings("unused")
		@Override
		public LearnerGraph init(LearnerGraph initPta) {
			throw new UnsupportedOperationException("The initial graph should be set by the constructor of LearnerWithUniqueFromInitial");
		}

		@Override
		public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) {
			return learner.ChooseStatePairs(graph);
		}

		@Override
		public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) {
			return learner.MergeAndDeterminize(original, pair);
		}

		@Override
		public List<List<Label>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) {
			return learner.ComputeQuestions(pair, original, temp);
		}

		@Override
		public List<List<Label>> RecomputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) {
			return learner.RecomputeQuestions(pair, original, temp);
		}

		@Override
		public Pair<Integer, String> CheckWithEndUser(LearnerGraph graph, List<Label> question, int expectedAccept,	List<Boolean> acceptedElements, PairScore pairBeingMerged, Object[] options) {
			return learner.CheckWithEndUser(graph, question, expectedAccept, acceptedElements, pairBeingMerged, options);
		}

		@Override
		public void Restart(RestartLearningEnum mode) {
			learner.Restart(mode);
		}

		@Override
		public void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind, List<Label> sequence, boolean accepted, JUConstants newColour) 
		{
			learner.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
		}

		@Override
		public boolean AddConstraints(LearnerGraph graph, LearnerGraph resultHolder, StringBuffer counterExampleHolder) 
		{
			return learner.AddConstraints(graph, resultHolder, counterExampleHolder);
		}

		@Override
		public ConvertALabel getLabelConverter() {
			return learner.getLabelConverter();
		}
		
	}
	
	static LearnerEvaluationConfiguration constructLearningConfiguration(LearnerEvaluationConfiguration evalCnf,Configuration.ScoreMode scoreMode)
	{
		Configuration config = evalCnf.config.copy();config.setLearnerScoreMode(scoreMode);
		LearnerEvaluationConfiguration copy = new LearnerEvaluationConfiguration(evalCnf);copy.config = config;
		return copy;
	}
	
	/** Merges states using a routing relying on PTA, that faster and consumes less memory than the general one. In addition, it aborts learning if the outcome has too many red states. */
	public static class ReferenceLearnerUsingSiccoScoring extends ReferenceLearner
	{

		protected final boolean scoringSiccoRecursive;
		
		public ReferenceLearnerUsingSiccoScoring(LearnerEvaluationConfiguration evalCnf, final LearnerGraph argInitialPTA, boolean SiccoRecursive) 
		{
			super(constructLearningConfiguration(evalCnf, Configuration.ScoreMode.COMPATIBILITY),argInitialPTA,SiccoRecursive? OverrideScoringToApply.SCORING_SICCO_PTARECURSIVE:OverrideScoringToApply.SCORING_SICCO_PTA);
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
		
		private static LearnerEvaluationConfiguration constructConfiguration(LearnerEvaluationConfiguration evalCnf, Configuration.ScoreMode scoringForEDSM, int threshold)
		{
			Configuration config = evalCnf.config.copy();config.setLearnerScoreMode(scoringForEDSM);config.setRejectPositivePairsWithScoresLessThan(threshold);config.setKlimit(threshold);
			if (scoringForEDSM == Configuration.ScoreMode.KTAILS || scoringForEDSM == Configuration.ScoreMode.KTAILS_ANY)
				config.setRejectPositivePairsWithScoresLessThan(0);// with k-tails, the outcome is zero for a merge and -1 for not. Where the threshold is above zero, mergers will never take place.
			LearnerEvaluationConfiguration copy = new LearnerEvaluationConfiguration(evalCnf);copy.config = config;
			return copy;
		}

		public EDSMReferenceLearner(LearnerEvaluationConfiguration evalCnf, final LearnerGraph argInitialPTA, Configuration.ScoreMode scoringForEDSM, int threshold) 
		{
			super(constructConfiguration(evalCnf,scoringForEDSM,threshold), argInitialPTA,OverrideScoringToApply.SCORING_NO_OVERRIDE);
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
			Configuration config = evalCnf.config.copy();config.setLearnerScoreMode(ScoreMode.GENERAL);
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
	
	public static LearnerGraph ptaConcurrentKtails(Collection<List<Label>> positive, Collection<List<Label>> negative, int k,Configuration config, int threadNumber, String ndFileName)
	{
		
		LearnerGraph pta = new LearnerGraph(config);
		pta.paths.augmentPTA(positive, true, false);pta.paths.augmentPTA(negative, false, false);
		return ptaConcurrentKtails(pta, k,threadNumber, ndFileName);
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
	
	/** This is a learner that is intended to merge traces into a graph of interest one-by-one. After a decision is reached which state 
	 * of the graph is to be added to which state in an existing graph, the outcome is made deterministic and returned.
	 */
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
			
			for(CmpVertex vB:existingGraph.transitionMatrix.keySet()) // this picks up both vertices in the graph we started with and those in the sequence.
				if (!visitedVertices.contains(vB) && checkMatch(existingGraph,vA,vB,k))
					existingGraph.pairscores.mergePair(new StatePair(vA,vB),stateToEquivalenceClass,mergingDetails);

			Map<Label,CmpVertex> next = existingGraph.transitionMatrix.get(vA);
			if (!next.isEmpty())
				vA=next.values().iterator().next();// there can only be one 'next' element because our PTA is built from a sequence.
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

	private static LearnerGraph traditionalKtailsHelper(Collection<List<Label>> positive, Collection<List<Label>> negative, int k,Configuration config) throws IncompatibleStatesException
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

	private static LearnerGraph traditionalPTAKtailsHelper(Collection<List<Label>> positive, Collection<List<Label>> negative,int k,Configuration config) throws IncompatibleStatesException
	{
		LearnerGraph pta = new LearnerGraph(config);
		pta.paths.augmentPTA(positive, true, false);pta.paths.augmentPTA(negative, false, false);
		return traditionalPTAKtailsHelper(pta,k);
	}
	
	private static LearnerGraph traditionalPTAKtailsHelper(LearnerGraph pta, int k) throws IncompatibleStatesException
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
			for(CmpVertex v:pta.transitionMatrix.keySet())
				if (!v.isAccept())
				{
					throw new IllegalArgumentException("k-tails expects all pairs that pass the threshold to be merged, however in the presence of negatives a few acceptable mergers may lead to a contradiction");
				}
			for(CmpVertex vA:pta.transitionMatrix.keySet())
			if (vA.isAccept() || k ==0)
			{
				for(CmpVertex vB:pta.transitionMatrix.keySet())
					if (vA == vB)
						break;
					else
					if (vB.isAccept() || k ==0)
					{
						StatePair pair = new StatePair(vA,vB);
						if (computeStateScoreKTails(pta,pair,k,true) >=0)
							pta.pairscores.mergePair(pair,stateToEquivalenceClass,mergingDetails);
					}
			}
			return constructKTailsNDGraphAndDeterminizeIt(pta,stateToEquivalenceClass,initialEQ.getRepresentative(),null);	
	}
	
	public static LearnerGraph ptaConcurrentKtails(final LearnerGraph pta, final int k, int threadNumber, String ndFileName)
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
			for(CmpVertex v:pta.transitionMatrix.keySet())
				if (!v.isAccept())
				{
					throw new IllegalArgumentException("k-tails expects all pairs that pass the threshold to be merged, however in the presence of negatives a few acceptable mergers may lead to a contradiction");
				}
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
			
			//System.out.println(new Date()+"started to make deterministic");
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


