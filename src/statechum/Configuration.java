/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * StateChum. If not, see <http://www.gnu.org/licenses/>.
 */

package statechum;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;

import static statechum.Helper.throwUnchecked;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import statechum.AttributeMutator.GETMETHOD_KIND;
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.learning.rpnicore.AbstractPersistence;

/**
 * Represents a configuration for a learner. The purpose is a possibility of a
 * global customisation of all objects used by a learner in the course of
 * learning by the same object.
 * <p>
 * When you add to this class, please add the corresponding entries to hashCode
 * and equals.
 * <p>
 * This class is using the built-in <em>clone</em> method, hence all attributes
 * have to be either primitives or immutable (such as <em>String</em>).
 */
public class Configuration implements Cloneable {
	/** Pairs with scores lower than this are not considered for merging. */
	protected int generalisationThreshold = 0;

	/**
	 * Limits the number of pairs returns from a method looking for pairs to
	 * merge. Zero inhibits the limitation.
	 */
	protected int pairsMergedPerHypothesis = 0;

	public Configuration() {
		// the initial values are provided for each variable inline.
	}

	protected final static Configuration defaultConfig = new Configuration(),
			originalDefaultConfiguration = new Configuration();

	public static Configuration getDefaultConfiguration() {
		if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(
				GlobalConfiguration.G_PROPERTIES.ASSERT_ENABLED)))
			assert defaultConfig.equals(originalDefaultConfiguration) : "original configuration has been modified - this should not happen";
		return defaultConfig;
	}

	/**
	 * Reject nodes never get a score of over zero since there are no paths
	 * starting from them in a PTA. Same happens to tail accept-nodes in a PTA.
	 * If one wishes to get positives nodes merged first where all possible
	 * pairs have zero scores, this one can be set to true in which all positive
	 * nodes have their scores artificially inflated by 1.
	 */
	protected boolean bumpPositives = false;

	public enum IDMode {
		NONE, POSITIVE_NEGATIVE, POSITIVE_ONLY
	}

	/**
	 * Some algorithms depend on the order in which elements of a graph are
	 * traversed; this in turn depends on the lexicographic order of their names
	 * (see testDeterminism() for a test which checks that this does not affect
	 * W set). This variable controls the order in which number to nodes in a
	 * PTA are assigned. POSITIVE_ONLY gives sequential numbers;
	 * POSITIVE_NEGATIVE gives negative numbers to reject vertices and positive
	 * ones to accept ones.
	 */
	protected IDMode learnerIdMode = IDMode.NONE; // creation of new vertices is
													// prohibited.

	public enum ScoreMode {
		CONVENTIONAL, COMPATIBILITY, KTAILS, GENERAL
	}

	/**
	 * The are a few ways in which one can compute scores associated to pairs of
	 * states, using a conventional computation, using compatibility scores
	 * (during traversal of pairs, ensures that the red state does not enter the
	 * PTA) and the k-tails traversal (scores are only updated on paths from the
	 * blue state of length k and less). Classical score computation matches two
	 * nodes but does not look at parts of a PTA being traversed during the
	 * computation of those scores. In particular, if there is a transition with
	 * some label a from a blue node and a transition with the same label leads
	 * from the red node to blue one, classical score computation proceeds to
	 * the target states of these two transitions and keeps matching, in effect
	 * comparing a PTA branch with itself which seems strange. This variable
	 * prevents score computation routine to enter the PTA branch which starts
	 * with the blue node.
	 */
	protected ScoreMode learnerScoreMode = ScoreMode.CONVENTIONAL;

	public void setLearnerScoreMode(ScoreMode mode) {
		learnerScoreMode = mode;
	}

	public ScoreMode getLearnerScoreMode() {
		return learnerScoreMode;
	}

	public void setLearnerIdMode(IDMode m) {
		learnerIdMode = m;
	}

	public IDMode getLearnerIdMode() {
		return learnerIdMode;
	}

	/**
	 * In order to check that the old and the new state mergers and question
	 * generators are doing approximately similar things, this mode is
	 * introduced. If set to true, both are run in parallel and consistency
	 * between them is checked.
	 */
	protected boolean consistencyCheckMode = false;

	public boolean isConsistencyCheckMode() {
		return consistencyCheckMode;
	}

	public void setConsistencyCheckMode(boolean cons) {
		consistencyCheckMode = cons;
	}

	/**
	 * Whether to use speculative question asking - this is useful to avoid
	 * numerous restarts by recording the fact that we need to restart but
	 * subsequently continuing to ask questions about the remaining states in a
	 * list of states, and restarting when finished.
	 */
	protected boolean speculativeQuestionAsking = false;

	public boolean isSpeculativeQuestionAsking() {
		return speculativeQuestionAsking;
	}

	public void setSpeculativeQuestionAsking(boolean newValue) {
		speculativeQuestionAsking = newValue;
	}

	/**
	 * When new traces are offered instead of an answer to a question, one may
	 * try to add them both to hard facts and to tentative automaton. If the
	 * latter has been successful, it is then possible to attempt to continue.
	 */
	protected boolean alwaysRestartOnNewTraces = false;

	public boolean isAlwaysRestartOnNewTraces() {
		return alwaysRestartOnNewTraces;
	}

	public void setAlwaysRestartOnNewTraces(boolean newValue) {
		alwaysRestartOnNewTraces = newValue;
	}

	/**
	 * There could be different ways to query the current strategy for asking
	 * questions. The enumeration below includes those implemented:
	 * <ul>
	 * <li>CONVENTIONAL - this is essentially the QSM question generator.</li>
	 * 
	 * <li>SYMMETRIC - when asking questions, this question generator does not
	 * care which state is blue and which is red. Its purpose is to handle graph
	 * which do not have a tree as in QSM. In addition to generality, it also
	 * aims to address the shortcomings in the QSM algorithm. The aim is to
	 * query all transitions from all states which were merged together (from
	 * all equivalence classes). Paths to those states are chosen from both the
	 * two original states A and B merged, considering a path between the two
	 * states (A and B are not necessarily adjacent, even in QSM where they are
	 * a red/blue pair).</li>
	 * <li>
	 * ORIGINAL - uses the original question generator (does not attempt to find
	 * all possible paths from red state to each state in the graph instead
	 * using the first one it finds).</li>
	 * </ul>
	 */
	public enum QuestionGeneratorKind {
		CONVENTIONAL, CONVENTIONAL_IMPROVED, SYMMETRIC, ORIGINAL
	}

	protected QuestionGeneratorKind questionGenerator = QuestionGeneratorKind.CONVENTIONAL;

	public QuestionGeneratorKind getQuestionGenerator() {
		return questionGenerator;
	}

	public void setQuestionGenerator(QuestionGeneratorKind qs) {
		questionGenerator = qs;
	}

	/**
	 * In order to reduce the number of questions asked, it might be useful to
	 * limit the number of paths which get collected when we are looking for all
	 * possible shortest paths between pairs of states (such as between the
	 * initial state and the red one in the original QSM question generator).
	 * Note that this constant is applied selectively by different question
	 * generators.
	 * <p>
	 * The default value is negative, i.e. no limit. Zero would generate empty
	 * questions since question generation algorithms start with so many paths,
	 * then extend them. If no paths are initially produced, the set of
	 * questions will be empty.
	 */
	protected int questionPathUnionLimit = -1;

	public int getQuestionPathUnionLimit() {
		return questionPathUnionLimit;
	}

	public void setQuestionPathUnionLimit(int limit) {
		questionPathUnionLimit = limit;
	}

	/**
	 * For a graph where transitions are annotated with pre/post conditions, we
	 * may wish to perform consistency checks. These checks are listed below in
	 * the order from the most "relaxed" one to the most "stringent" check.
	 * <ul>
	 * <li><em>NONE</em>: no consistency checking is done.</li>
	 * <li><em>ALLABSTRACTSTATESEXIST</em>: all abstract states are satisfiable.
	 * </li>
	 * <li><em>TRANSITIONSFROMALLORNONE</em>: all transitions either can or
	 * cannot be taken, from all abstract states corresponding to each DFA
	 * state. A problem detected at this stage is where a transition can be
	 * taken from some abstract states but not from others.</li>
	 * <li><em>DETERMINISM</em>: for each abstract state, there is at most one
	 * possible outgoing transition. Intersection of the preconditions of
	 * transitions to reject-states does not matter.</li>
	 * </ul>
	 */
	public enum SMTGRAPHDOMAINCONSISTENCYCHECK {
		NONE, ALLABSTRACTSTATESEXIST, TRANSITIONSFROMALLORNONE, DETERMINISM
	};

	protected SMTGRAPHDOMAINCONSISTENCYCHECK smtGraphDomainConsistencyCheck = SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST;

	public SMTGRAPHDOMAINCONSISTENCYCHECK getSmtGraphDomainConsistencyCheck() {
		return smtGraphDomainConsistencyCheck;
	}

	public void setSmtGraphDomainConsistencyCheck(
			SMTGRAPHDOMAINCONSISTENCYCHECK value) {
		smtGraphDomainConsistencyCheck = value;
	}

	/**
	 * For a graph where transitions are annotated with pre/post conditions, we
	 * may wish to perform consistency checks. These checks are listed below in
	 * the order from the most "relaxed" one to the most "stringent" check.
	 * <ul>
	 * <li><em>NONE</em>: no consistency checking is done.</li>
	 * <li><em>RANGEINTERSECTION</em>: for at least one abstract state
	 * associated with a source state of a transition, a postcondition should be
	 * contained in one of the abstract states associated with target state.</li>
	 * <li><em>RANGECONTAINMENT</em>: for all abstract states associated with a
	 * source state of a transition, a postcondition should be included in a
	 * collection of abstract states associated with a target state.</li>
	 * </ul>
	 */
	public enum SMTGRAPHRANGECONSISTENCYCHECK {
		NONE, RANGEINTERSECTION, RANGECONTAINMENT
	};

	protected SMTGRAPHRANGECONSISTENCYCHECK smtGraphRangeConsistencyCheck = SMTGRAPHRANGECONSISTENCYCHECK.RANGEINTERSECTION;

	public SMTGRAPHRANGECONSISTENCYCHECK getSmtGraphRangeConsistencyCheck() {
		return smtGraphRangeConsistencyCheck;
	}

	public void setSmtGraphRangeConsistencyCheck(
			SMTGRAPHRANGECONSISTENCYCHECK value) {
		smtGraphRangeConsistencyCheck = value;
	}

	public int getGeneralisationThreshold() {
		return generalisationThreshold;
	}

	public void setGeneralisationThreshold(int generalisationThresholdArg) {
		this.generalisationThreshold = generalisationThresholdArg;
	}

	public int getPairsMergedPerHypothesis() {
		return pairsMergedPerHypothesis;
	}

	public void setPairsMergedPerHypothesis(int pairsMergedPerHypothesisArg) {
		this.pairsMergedPerHypothesis = pairsMergedPerHypothesisArg;
	}

	public boolean isBumpPositives() {
		return bumpPositives;
	}

	public void setBumpPositives(boolean bumpPositivesArg) {
		this.bumpPositives = bumpPositivesArg;
	}

	/**
	 * Much of the current learning techniques expect prefix-closed automata,
	 * but many of them can be adapted for non-prefix-closed ones. The choice
	 * depends on the variable below.
	 */
	protected boolean prefixClosed = true;

	public boolean isPrefixClosed() {
		return prefixClosed;
	}

	public void setPrefixClosed(boolean closed) {
		prefixClosed = closed;
	}

	/**
	 * Whether we should make an attempt to generate a semi-W set in the
	 * presence of equivalent states. Very useful where a test set aims to
	 * simply generate thorough walks.
	 */
	protected boolean equivalentStatesAllowedForW = false;

	public boolean getEquivalentStatesAllowedForW() {
		return equivalentStatesAllowedForW;
	}

	public void setEquivalentStatesAllowedForW(boolean equiv) {
		equivalentStatesAllowedForW = equiv;
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			IllegalArgumentException ex = new IllegalArgumentException(
					"clone of Configuration failed - should not happen");
			ex.initCause(e);
			throw ex;
		}
	}

	/**
	 * Makes a copy of this configuration. Same as clone() but without a need to
	 * cast the result.
	 */
	public Configuration copy() {
		return (Configuration) clone();
	}

	/**
	 * Whether construction of LearnerGraph from a Jung graph should clone
	 * vertices of that graph.
	 */
	protected boolean LearnerCloneGraph = true;

	/**
	 * Whether LearnerGraph should use StringVertex or DeterministicVertex as
	 * vertices of a graph.
	 */
	protected boolean LearnerUseStrings = true;

	public boolean isLearnerCloneGraph() {
		return LearnerCloneGraph;
	}

	public void setLearnerCloneGraph(boolean learnerCloneGraph) {
		LearnerCloneGraph = learnerCloneGraph;
	}

	public boolean isLearnerUseStrings() {
		return LearnerUseStrings;
	}

	public void setLearnerUseStrings(boolean learnerUseStrings) {
		LearnerUseStrings = learnerUseStrings;
	}

	/**
	 * Selects the type of labels to use in a graph. This primarily affects
	 * parsing of them from text.
	 */
	public enum LABELKIND {
		LABEL_STRING, LABEL_ERLANG
	};

	protected LABELKIND labelKind = LABELKIND.LABEL_STRING;

	public LABELKIND getLabelKind() {
		return labelKind;
	}

	public void setLabelKind(LABELKIND newLabelKind) {
		labelKind = newLabelKind;
	}

	/**
	 * The initial state in a PTA has to be given some name, this is the
	 * default. Important: "" means "generate a numerical identifier" which is
	 * almost always the best choice, but in some specific tests, I'd like to
	 * use one with a known name.
	 */
	protected String defaultInitialPTAName = "";

	public void setDefaultInitialPTAName(String name) {
		defaultInitialPTAName = name;
	}

	public String getDefaultInitialPTAName() {
		return defaultInitialPTAName;
	}

	/**
	 * Used to define the file name to be used for auto-loading answers.
	 * Zero-length means no auto.
	 */
	protected String autoAnswerFileName = "";

	public void setAutoAnswerFileName(String name) {
		autoAnswerFileName = name;
	}

	public String getAutoAnswerFileName() {
		return autoAnswerFileName;
	}

	public static Collection<Object[]> configurationsForTesting() {
		Configuration same = new Configuration();
		same.setLearnerUseStrings(false);
		same.setLearnerCloneGraph(true);
		Configuration clone = new Configuration();
		same.setLearnerUseStrings(false);
		same.setLearnerCloneGraph(false);
		Configuration strings = new Configuration();
		same.setLearnerUseStrings(true);
		same.setLearnerCloneGraph(false);
		return Arrays
				.asList(new Object[][] { { same }, { clone }, { strings } });
	}

	/**
	 * Given a test configuration, returns a textual description of its purpose.
	 * 
	 * @param config
	 *            configuration to consider
	 * @return description.
	 */
	public static String parametersToString(Configuration config) {
		return (config.isLearnerUseStrings() ? "String vertex" : "Jung vertex")
				+ ", " + (config.isLearnerCloneGraph() ? "clone" : "no_clone");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (LearnerCloneGraph ? 1231 : 1237);
		result = prime * result + (LearnerUseStrings ? 1231 : 1237);
		result = prime * result + (bumpPositives ? 1231 : 1237);
		result = prime * result + generalisationThreshold;
		result = prime * result
				+ ((learnerIdMode == null) ? 0 : learnerIdMode.hashCode());
		result = prime
				* result
				+ ((learnerScoreMode == null) ? 0 : learnerScoreMode.hashCode());
		result = prime
				* result
				+ ((questionGenerator == null) ? 0 : questionGenerator
						.hashCode());
		result = prime * result + pairsMergedPerHypothesis;
		result = prime * result + (allowedToCloneNonCmpVertex ? 1231 : 1237);
		result = prime * result + defaultInitialPTAName.hashCode();
		result = prime * result + (debugMode ? 1231 : 1237);
		result = prime * result + certaintyThreshold;
		result = prime * result + minCertaintyThreshold;
		result = prime * result + klimit;
		result = prime * result + (askQuestions ? 1231 : 1237);
		result = prime * result + randomPathAttemptThreshold;
		result = prime * result + randomPathAttemptFudgeThreshold;
		result = prime * result + (generateTextOutput ? 1231 : 1237);
		result = prime * result + (generateDotOutput ? 1231 : 1237);
		result = prime
				* result
				+ ((autoAnswerFileName == null) ? 0 : autoAnswerFileName
						.hashCode());
		result = prime * result + questionPathUnionLimit;
		result = prime * result + (int) (attenuationK * 100);
		result = prime * result + (consistencyCheckMode ? 1231 : 1237);
		result = prime * result + (speculativeQuestionAsking ? 1231 : 1237);
		result = prime * result + (int) (gdKeyPairThreshold * 100);
		result = prime * result + (int) (gdLowToHighRatio * 100);
		result = prime * result + (gdFailOnDuplicateNames ? 1231 : 1237);
		result = prime * result + (learnerOverwriteOutput ? 1231 : 1237);
		result = prime * result + gdMaxNumberOfStatesInCrossProduct;
		result = prime * result + (compressLogs ? 1231 : 1237);
		result = prime * result
				+ ((learnerToUse == null) ? 0 : learnerToUse.hashCode());
		result = prime * result + (useAmber ? 1231 : 1237);
		result = prime * result + (useSpin ? 1231 : 1237);
		result = prime * result + (useLTL ? 1231 : 1237);
		result = prime * result + (useSMT ? 1231 : 1237);
		result = prime * result + initialIDvalue;
		result = prime * result + (useConstraints ? 1231 : 1237);
		result = prime * result
				+ (ignoreDepthInTheChoiceOfRepresentatives ? 1231 : 1237);
		result = prime * result
				+ (ignoreVertexAttributesInLogReplay ? 1231 : 1237);
		result = prime * result + howManyStatesToAddFromIFTHEN;
		result = prime * result + smtGraphDomainConsistencyCheck.hashCode();
		result = prime * result + smtGraphRangeConsistencyCheck.hashCode();
		result = prime * result + (prefixClosed ? 1231 : 1237);
		result = prime * result + gdScoreComputation.hashCode();
		result = prime * result + gdScoreComputationAlgorithm.hashCode();
		result = prime * result
				+ gdScoreComputationAlgorithm_TestSet_ExtraStates;
		result = prime * result
				+ gdScoreComputationAlgorithm_RandomWalk_NumberOfSequences;
		result = prime * result
				+ gdScoreComputationAlgorithm_RandomWalk_ExtraLength;
		result = prime * result + (equivalentStatesAllowedForW ? 1231 : 1237);
		result = prime * result + (gdPropagateDet ? 1231 : 1237);
		result = prime * result
				+ gdScoreComputationAlgorithm_RandomWalk_PathLength;
		result = prime * result + labelKind.hashCode();
		result = prime * result + erlFlushDelay;
		result = prime * result + erlCoverage.hashCode();
		result = prime * result + (legacyXML ? 1231 : 1237);
		result = prime * result
				+ (erlangModuleName == null ? 0 : erlangModuleName.hashCode());
		result = prime * result + (alwaysRestartOnNewTraces ? 1231 : 1237);
		result = prime * result
				+ (erlangSourceFile == null ? 0 : erlangSourceFile.hashCode());
		result = prime * result + (useErlangOutputs ? 1231 : 1237);

		return result;
	}

	public static final double fpAccuracy = 1e-8;

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Configuration))
			return false;
		final Configuration other = (Configuration) obj;
		if (LearnerCloneGraph != other.LearnerCloneGraph)
			return false;
		if (LearnerUseStrings != other.LearnerUseStrings)
			return false;
		if (bumpPositives != other.bumpPositives)
			return false;
		if (generalisationThreshold != other.generalisationThreshold)
			return false;
		if (learnerIdMode == null) {
			if (other.learnerIdMode != null)
				return false;
		} else if (!learnerIdMode.equals(other.learnerIdMode))
			return false;
		if (learnerScoreMode == null) {
			if (other.learnerScoreMode != null)
				return false;
		} else if (!learnerScoreMode.equals(other.learnerScoreMode))
			return false;
		if (questionGenerator == null) {
			if (other.questionGenerator != null)
				return false;
		} else if (!questionGenerator.equals(other.questionGenerator))
			return false;
		if (pairsMergedPerHypothesis != other.pairsMergedPerHypothesis)
			return false;
		if (allowedToCloneNonCmpVertex != other.allowedToCloneNonCmpVertex)
			return false;
		if (!defaultInitialPTAName.equals(other.defaultInitialPTAName))
			return false;
		if (!autoAnswerFileName.equals(other.autoAnswerFileName))
			return false;
		if (debugMode != other.debugMode)
			return false;
		if (certaintyThreshold != other.certaintyThreshold)
			return false;
		if (minCertaintyThreshold != other.minCertaintyThreshold)
			return false;
		if (klimit != other.klimit)
			return false;
		if (askQuestions != other.askQuestions)
			return false;
		if (randomPathAttemptThreshold != other.randomPathAttemptThreshold)
			return false;
		if (randomPathAttemptFudgeThreshold != other.randomPathAttemptFudgeThreshold)
			return false;
		if (generateTextOutput != other.generateTextOutput)
			return false;
		if (generateDotOutput != other.generateDotOutput)
			return false;
		if (questionPathUnionLimit != other.questionPathUnionLimit)
			return false;
		if (Math.abs(attenuationK - other.attenuationK) > fpAccuracy)
			return false;
		if (consistencyCheckMode != other.consistencyCheckMode)
			return false;
		if (speculativeQuestionAsking != other.speculativeQuestionAsking)
			return false;
		if (Math.abs(gdKeyPairThreshold - other.gdKeyPairThreshold) > fpAccuracy)
			return false;
		if (Math.abs(gdLowToHighRatio - other.gdLowToHighRatio) > fpAccuracy)
			return false;
		if (gdFailOnDuplicateNames != other.gdFailOnDuplicateNames)
			return false;
		if (learnerOverwriteOutput != other.learnerOverwriteOutput)
			return false;
		if (gdMaxNumberOfStatesInCrossProduct != other.gdMaxNumberOfStatesInCrossProduct)
			return false;
		if (compressLogs != other.compressLogs)
			return false;
		if (learnerToUse != other.learnerToUse)
			return false;
		if (useAmber != other.useAmber)
			return false;
		if (useSpin != other.useSpin)
			return false;
		if (useLTL != other.useLTL)
			return false;
		if (useSMT != other.useSMT)
			return false;
		if (initialIDvalue != other.initialIDvalue)
			return false;
		if (useConstraints != other.useConstraints)
			return false;
		if (ignoreDepthInTheChoiceOfRepresentatives != other.ignoreDepthInTheChoiceOfRepresentatives)
			return false;
		if (ignoreVertexAttributesInLogReplay != other.ignoreVertexAttributesInLogReplay)
			return false;
		if (howManyStatesToAddFromIFTHEN != other.howManyStatesToAddFromIFTHEN)
			return false;
		if (smtGraphDomainConsistencyCheck != other.smtGraphDomainConsistencyCheck)
			return false;
		if (smtGraphRangeConsistencyCheck != other.smtGraphRangeConsistencyCheck)
			return false;
		if (prefixClosed != other.prefixClosed)
			return false;
		if (gdScoreComputation != other.gdScoreComputation)
			return false;
		if (gdScoreComputationAlgorithm != other.gdScoreComputationAlgorithm)
			return false;
		if (gdScoreComputationAlgorithm_TestSet_ExtraStates != other.gdScoreComputationAlgorithm_TestSet_ExtraStates)
			return false;
		if (gdScoreComputationAlgorithm_RandomWalk_NumberOfSequences != other.gdScoreComputationAlgorithm_RandomWalk_NumberOfSequences)
			return false;
		if (gdScoreComputationAlgorithm_RandomWalk_ExtraLength != other.gdScoreComputationAlgorithm_RandomWalk_ExtraLength)
			return false;
		if (equivalentStatesAllowedForW != other.equivalentStatesAllowedForW)
			return false;
		if (gdPropagateDet != other.gdPropagateDet)
			return false;
		if (gdScoreComputationAlgorithm_RandomWalk_PathLength != other.gdScoreComputationAlgorithm_RandomWalk_PathLength)
			return false;
		if (labelKind != other.labelKind)
			return false;
		if (erlFlushDelay != other.erlFlushDelay)
			return false;
		if (erlCoverage != other.erlCoverage)
			return false;
		if (useErlangOutputs != other.useErlangOutputs)
			return false;
		if (!erlangSourceFile.equals(other.erlangSourceFile))
			return false;
		if (legacyXML != other.legacyXML)
			return false;
		if ((erlangModuleName == null && other.erlangModuleName != null)
				|| (erlangModuleName != null && other.erlangModuleName == null)
				|| (erlangModuleName != null && !erlangModuleName
						.equals(other.erlangModuleName)))
			return false;
		if (alwaysRestartOnNewTraces != other.alwaysRestartOnNewTraces)
			return false;

		return true;
	}

	/**
	 * For testing, I'd like to mix pure Jung routines and LearnerGraph ones,
	 * hence it should be possible to build a LearnerGraph out of whatever Jung
	 * graph I was given. In production use, graphs are either to be built from
	 * GraphML or obtained from LearnerGraph. In both cases these will be
	 * deterministic graphs. For this reason, it is good to be able to detect
	 * when we've been passed a graph which does not consist of deterministic
	 * vertices.
	 */
	protected boolean allowedToCloneNonCmpVertex = false;

	public boolean isAllowedToCloneNonCmpVertex() {
		return allowedToCloneNonCmpVertex;
	}

	public void setAllowedToCloneNonCmpVertex(boolean allowed) {
		allowedToCloneNonCmpVertex = allowed;
	}

	/** Used to pop up auxiliary information when under test. */
	protected boolean debugMode = true;

	public void setDebugMode(boolean debug) {
		this.debugMode = debug;
	}

	public boolean getDebugMode() {
		return debugMode;
	}

	/**
	 * Pairs with very high scores should perhaps be merged anyway. For this
	 * reason, we do not ask questions if we get a score at least that high.
	 * Negative number inhibits the check.
	 */
	protected int certaintyThreshold = -1;

	public void setCertaintyThreshold(int threshold) {
		certaintyThreshold = threshold;
	}

	public int getCertaintyThreshold() {
		return certaintyThreshold;
	}

	/**
	 * Pairs with scores of zero or just over tend to remain at the end,
	 * generating loads of questions. One way to avoid them is to silently merge
	 * those states. If a score is less than the number below, the corresponding
	 * pair will be silently merged.
	 */
	protected int minCertaintyThreshold = -1;

	public void setMinCertaintyThreshold(int minThreshold) {
		this.minCertaintyThreshold = minThreshold;
	}

	public int getMinCertaintyThreshold() {
		return minCertaintyThreshold;
	}

	/**
	 * When doing k-tail merging, this number determines how far we go when
	 * computing a positive/zero number. Negatives are always computed by a full
	 * traversal - without this, we'll be attempting to merge incompatible
	 * vertices. The lowest number is 1 which corresponds to looking just one
	 * transition ahead from the red-blue state pair.
	 */
	protected int klimit;

	public int getKlimit() {
		return klimit;
	}

	public void setKlimit(int limit) {
		klimit = limit;
	}

	/**
	 * Whether we should try auto-answer questions no matter what or ask a user.
	 */
	protected boolean askQuestions = true;

	public void setAskQuestions(boolean ask) {
		askQuestions = ask;
	}

	public boolean getAskQuestions() {
		return askQuestions;
	}

	/**
	 * When looking for random paths of a specific size, one of the easy ways is
	 * to make a number of attempts and fail if cannot get a path of the
	 * expected length.
	 */
	protected int randomPathAttemptThreshold = 100;

	public int getRandomPathAttemptThreshold() {
		return randomPathAttemptThreshold;
	}

	public void setRandomPathAttemptThreshold(int thr) {
		randomPathAttemptThreshold = thr;
	}

	/**
	 * When we failed to generate a path for a specific length, we'll try a
	 * different length. The number of attempts at this is constrained by the
	 * constant below.
	 */
	protected int randomPathAttemptFudgeThreshold = 100;

	public int getRandomPathAttemptFudgeThreshold() {
		return randomPathAttemptFudgeThreshold;
	}

	public void setRandomPathAttemptFudgeThreshold(int thr) {
		randomPathAttemptFudgeThreshold = thr;
	}

	protected boolean generateTextOutput = false;
	protected boolean generateDotOutput = false;

	public boolean isGenerateTextOutput() {
		return generateTextOutput;
	}

	public void setGenerateTextOutput(boolean generateText) {
		generateTextOutput = generateText;
	}

	public boolean isGenerateDotOutput() {
		return generateDotOutput;
	}

	public void setGenerateDotOutput(boolean generateDot) {
		generateDotOutput = generateDot;
	}

	/**
	 * Depth information is used to identify which vertex from a collection of
	 * them to use as a representative of an equivalence class, however when
	 * using legacy logs, we have to use the original (lexicographical) method
	 * to choose representatives. This boolean chooses the specific method to
	 * use.
	 */
	protected boolean ignoreDepthInTheChoiceOfRepresentatives = false;

	public void setIgnoreDepthInTheChoiceOfRepresentatives(boolean value) {
		ignoreDepthInTheChoiceOfRepresentatives = value;
	}

	public boolean isIgnoreDepthInTheChoiceOfRepresentatives() {
		return ignoreDepthInTheChoiceOfRepresentatives;
	}

	/**
	 * One would usually expect to compare graphs in all the detail, including
	 * attributes of states. Old GD-compressed log files do not contain enough
	 * information to reconstruct such information, so we'd like to perform a
	 * more shallow comparison.
	 */
	protected boolean ignoreVertexAttributesInLogReplay = false;

	public void setIgnoreVertexAttributesInLogReplay(boolean newValue) {
		ignoreVertexAttributesInLogReplay = newValue;
	}

	public boolean isIgnoreVertexAttributesInLogReplay() {
		return ignoreVertexAttributesInLogReplay;
	}

	/**
	 * Scores of state pairs can be computed in different ways in GD, the
	 * enumeration below reflects the choices.
	 * <ul>
	 * <li><em>GD_RH</em> means that a system of equations has to be solved
	 * using the scores as the right-hand side as well as k values.</li>
	 * <li><em>GD_DIRECT</em> here scores are used directly, which is the same
	 * as double the scores obtained with RH and k=0, but since there is no need
	 * to solve equations the performance is a lot better.
	 * <li>
	 * </ul>
	 */
	public enum GDScoreComputationEnum {
		GD_RH, GD_DIRECT
	};

	protected GDScoreComputationEnum gdScoreComputation = GDScoreComputationEnum.GD_RH;

	public GDScoreComputationEnum getGdScoreComputation() {
		return gdScoreComputation;
	}

	public void setGdScoreComputation(GDScoreComputationEnum value) {
		gdScoreComputation = value;
	}

	/**
	 * Scoring algorithm to use,
	 * <ul>
	 * <li><em>SCORE_TESTSET</em> do BCR based on a test set generated with W
	 * method with k=gdScoreComputationAlgorithm_TestSet_ExtraStates.</li>
	 * <li><em>SCORE_RANDOMPATHS</em> use random walk with parameters
	 * gdScoreComputationAlgorithm_RandomWalk_NumberOfSequences
	 * gdScoreComputationAlgorithm_RandomWalk_ExtraLength.</li>
	 * <li><em>SCORE_LINEAR</em> count matched transitions.</li>
	 * </ul>
	 */
	public enum GDScoreComputationAlgorithmEnum {
		SCORE_TESTSET, SCORE_RANDOMPATHS, SCORE_LINEAR
	};

	protected GDScoreComputationAlgorithmEnum gdScoreComputationAlgorithm = GDScoreComputationAlgorithmEnum.SCORE_LINEAR;

	public GDScoreComputationAlgorithmEnum getGdScoreComputationAlgorithm() {
		return gdScoreComputationAlgorithm;
	}

	public void setGdScoreComputationAlgorithm(
			GDScoreComputationAlgorithmEnum value) {
		gdScoreComputationAlgorithm = value;
	}

	/**
	 * When scores are generated using SCORE_TESTSET (via W method), this number
	 * indicates how many extra states are to use in test generation.
	 */
	protected int gdScoreComputationAlgorithm_TestSet_ExtraStates;

	public int getGdScoreComputationAlgorithm_TestSet_ExtraStates() {
		return gdScoreComputationAlgorithm_TestSet_ExtraStates;
	}

	public void setGdScoreComputationAlgorithm_TestSet_ExtraStates(int value) {
		if (value < 0)
			throw new IllegalArgumentException("should be non-negative");
		gdScoreComputationAlgorithm_TestSet_ExtraStates = value;
	}

	/**
	 * When scores are generated using Random walks, this number reflects the
	 * number of sequences to generate. The generated sequences will be split
	 * equally into positive and negative sequences.
	 */
	protected int gdScoreComputationAlgorithm_RandomWalk_NumberOfSequences = 20;

	public int getGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences() {
		return gdScoreComputationAlgorithm_RandomWalk_NumberOfSequences;
	}

	public void setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(
			int value) {
		if (value < 0)
			throw new IllegalArgumentException("should be non-negative");
		gdScoreComputationAlgorithm_RandomWalk_NumberOfSequences = value;
	}

	/**
	 * Sets the length of paths generated for random walks. If this value is
	 * zero or negative, uses graph diameter plus the <em>extra length</em>
	 * value.
	 */
	protected int gdScoreComputationAlgorithm_RandomWalk_PathLength = -1;

	public int getGdScoreComputationAlgorithm_RandomWalk_PathLength() {
		return gdScoreComputationAlgorithm_RandomWalk_PathLength;
	}

	public void setGdScoreComputationAlgorithm_RandomWalk_PathLength(int value) {
		gdScoreComputationAlgorithm_RandomWalk_PathLength = value;
	}

	/**
	 * The length of sequences generated with random walks is a diameter of a
	 * graph plus a constant supplied using the variable below.
	 */
	protected int gdScoreComputationAlgorithm_RandomWalk_ExtraLength = 0;

	public int getGdScoreComputationAlgorithm_RandomWalk_ExtraLength() {
		return gdScoreComputationAlgorithm_RandomWalk_ExtraLength;
	}

	public void setGdScoreComputationAlgorithm_RandomWalk_ExtraLength(int value) {
		if (value < 0)
			throw new IllegalArgumentException("should be non-negative");
		gdScoreComputationAlgorithm_RandomWalk_ExtraLength = value;
	}

	/**
	 * Where graphs are deterministic, a single key pair can easily be
	 * propagated without a need to use scores.
	 */
	protected boolean gdPropagateDet = false;

	public boolean getGdPropagateDet() {
		return gdPropagateDet;
	}

	public void setGdPropagateDet(boolean newValue) {
		gdPropagateDet = newValue;
	}

	/**
	 * Considering all pairs of states, we need to determine those of them which
	 * are over a specific threshold, defined as top so many percent (expressed
	 * as a fraction, so top 5% is 0.05).
	 */
	protected double gdKeyPairThreshold = 0.25;

	public double getGdKeyPairThreshold() {
		return gdKeyPairThreshold;
	}

	public void setGdKeyPairThreshold(double value) {
		if (value < 0 || value > 1)
			throw new IllegalArgumentException("threshold " + value
					+ " is invalid, 0..1 is expected (both inclusive)");
		gdKeyPairThreshold = value;
	}

	/**
	 * The highest low-high score ratio for a pair to be considered a key pair.
	 * If some pairs have high absolute scores, they make kill all other
	 * candidates for key pairs. For this reason, we'd like to set
	 * <em>gdKeyPairThreshold</em> not too low and choose a state of B which
	 * should be paired to a state in A where the corresponding pairs's low
	 * score is at most <em>gdLowToHighRatio</em> that of its highest score.
	 * Refer to <em>handleRow</em> part of <em>identifyKeyPairs()</em> for
	 * details.
	 */
	protected double gdLowToHighRatio = 0.5;

	public double getGdLowToHighRatio() {
		return gdLowToHighRatio;
	}

	public void setGdLowToHighRatio(double value) {
		if (value < 0 || value > 1)
			throw new IllegalArgumentException("HighLowRatio " + value
					+ " is invalid, expected 0..1");
		gdLowToHighRatio = value;
	}

	/**
	 * When doing linear, we need a way to attenuate the compatibility score
	 * associated to states into which we have transitions. This values provides
	 * the appropriate attenuation, which has to be under 1, since otherwise the
	 * matrix is likely to be singular. Using a value of 0 would imply we ignore
	 * all outgoing transitions.
	 * <p>
	 * In practice, this value should be rather lower than 1, because otherwise
	 * it'll lead to very high scores being generated. Consider <tt>A-a->A</tt>.
	 * In this case, the equation is <tt>AA=k*AA+1</tt>, so for k=0.9, we get
	 * AA=10. It is not feasible to fudge the case of loops because we can have
	 * <tt>A-a->B-a->C-a->A</tt> where the same problem will occur. For this
	 * reason, it seems reasonable to keep k to around 0.7 or less. In this
	 * case, <tt>testFindKeyPairs2()</tt> obtains scores of 6.6 (66 after
	 * multiplication by 10 and truncation to int).
	 * <p>
	 * <b>It is important not to force <em>totalOutgoing</em> to 1</b> for the
	 * following reason: Consider the case of multiple looping transitions in A,
	 * such as <tt>A-a->A-b->A</tt>. In this case, we get an equation
	 * AA=k*count(outgoing)*AA+const. For values of k of 0.5 or under, there
	 * will be a specific number of outgoing transitions such that the matrix
	 * will be singular. For this reason, the value of constant has to be
	 * somewhere around 0.6..0.8
	 */
	protected double attenuationK = 0.6;

	public double getAttenuationK() {
		return attenuationK;
	}

	/**
	 * Sets the new value of k.
	 * 
	 * @param k
	 *            the new value of attenuation.
	 * @throws IllegalArgumentException
	 *             if k is negative or 1 or over 1.
	 */
	public void setAttenuationK(double k) {
		if (k < 0 || k >= 1)
			throw new IllegalArgumentException(
					"attenuation should be within [0,1[");
		attenuationK = k;
	}

	/** A test-only version of the above, permitting a value of 1. */
	public void setAttenuationK_testOnly(double k) {
		if (k < 0 || k > 1)
			throw new IllegalArgumentException(
					"attenuation should be within [0,1[");
		attenuationK = k;
	}

	/**
	 * When there are states in B with the same names as states of A, it is easy
	 * to confuse between them when we generate a patch for A in
	 * <em>computeGD</em>. For this reason, when this situation occurs we no
	 * longer use original names for vertices of B but instead use the unique
	 * IDs generated when A and B were combined. When this variable is true,
	 * this fallback is not performed and an {@link IllegalArgumentException} is
	 * thrown.
	 */
	protected boolean gdFailOnDuplicateNames = true;

	public boolean getGdFailOnDuplicateNames() {
		return gdFailOnDuplicateNames;
	}

	public void setGdFailOnDuplicateNames(boolean value) {
		gdFailOnDuplicateNames = value;
	}

	/**
	 * All native code (and sometimes JVM) can crash. This tends to happen when
	 * JVM runs out of memory and then runs native code which I presume
	 * allocates some memory. Out-of-memory errors appear to be handled well, but
	 * crashes still occur. For this reason, it makes sense to run experiments
	 * in a separate JVM and re-run those which did not complete due to crash,
	 * assuming non-termination is detected by the learner itself). If this is
	 * done, we have to distinguish a situation with many result files when we'd
	 * like to restart learning overwriting the existing data and the case when
	 * we'd only like to restart those which did not complete. The switch below
	 * makes it possible to choose one of these two modes.
	 */
	protected boolean learnerOverwriteOutput = true;

	public boolean getLearnerOverwriteOutput() {
		return learnerOverwriteOutput;
	}

	public void setLearnerOverwriteOutput(boolean newValue) {
		learnerOverwriteOutput = newValue;
	}

	/**
	 * The number of equations to solve is the square of the number of states in
	 * graphs, hence if the total exceeds a reasonable number, we cannot use
	 * Linear for comparisons and a fallback is to simply use a pair of initial
	 * states and disable backward traversal since it is nondeterministic and in
	 * the absence of a good measure of state similarity we cannot meaningfully
	 * choose between different possible pairs of states.
	 */
	protected int gdMaxNumberOfStatesInCrossProduct = 800 * 800;

	public int getGdMaxNumberOfStatesInCrossProduct() {
		return gdMaxNumberOfStatesInCrossProduct;
	}

	public void setGdMaxNumberOfStatesInCrossProduct(int newValue) {
		gdMaxNumberOfStatesInCrossProduct = newValue;
	}

	/** Whether to store graphs with or without compression in logs. */
	protected boolean compressLogs = true;

	public boolean getCompressLogs() {
		return compressLogs;
	}

	public void setCompressLogs(boolean newValue) {
		compressLogs = newValue;
	}

	/** Whether a learner should be colouring new states with amber/grey. */
	protected boolean useAmber = false;

	public boolean getUseAmber() {
		return useAmber;
	}

	public void setUseAmber(boolean newValue) {
		useAmber = newValue;
	}

	/** Whether a learner should make use of output matching. */
	protected boolean useErlangOutputs = true;

	public boolean getUseErlangOutputs() {
		return useErlangOutputs;
	}

	public void setUseErlangOutputs(boolean newValue) {
		useErlangOutputs = newValue;
	}

	/** Erlang module source file name. */
	protected String erlangSourceFile = null;

	public String getErlangSourceFile() {
		return erlangSourceFile;
	}

	public void setErlangSourceFile(String newValue) {
		try {
			ErlangModule.loadModule(newValue);
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(65);
		}
		erlangSourceFile = newValue;
	}

	/** Whether to use LTL models to reduce the number of questions being asked. */
	protected boolean useLTL = false;

	public boolean getUseLTL() {
		return useLTL;
	}

	public void setUseLTL(boolean newValue) {
		useLTL = newValue;
	}

	/**
	 * Whether to use a constraint solver in order to check whether paths exist
	 * or not.
	 */
	protected boolean useSMT = false;

	public boolean getUseSMT() {
		return useSMT;
	}

	public void setUseSMT(boolean newValue) {
		useSMT = newValue;
	}

	/**
	 * Whether to use SPIN to check LTL models rather than doing it directly,
	 * which is faster for simple models and slower for larger ones. When SPIN
	 * is not used, a "maximal automaton" is built from LTL formulae and is
	 * intersected with tentative automata at every opportunity (to find
	 * counterexamples and update a tentative automaton with negative
	 * information).
	 */
	protected boolean useSpin = true;

	public boolean getUseSpin() {
		return useSpin;
	}

	public void setUseSpin(boolean newValue) {
		useSpin = newValue;
	}

	/**
	 * Determines whether dumping/loading from XML uses legacy implementation or
	 * the current one. This is useful for conversion of logs (.gd) from old to
	 * new format.
	 */
	protected boolean legacyXML = false;

	public boolean getLegacyXML() {
		return legacyXML;
	}

	public void setLegacyXML(boolean newValue) {
		legacyXML = newValue;
	}

	/**
	 * When learning, we might wish to add transitions from a property
	 * automaton. This corresponds to adding waves of transitions where the
	 * first wave starts from state of a tentative automaton, the second wave
	 * from the newly-added states and so on. This parameter determines the
	 * number of waves to add.
	 */
	protected int howManyStatesToAddFromIFTHEN = 0;

	public int getHowManyStatesToAddFromIFTHEN() {
		return howManyStatesToAddFromIFTHEN;
	}

	public void setHowManyStatesToAddFromIFTHEN(int value) {
		howManyStatesToAddFromIFTHEN = value;
	}

	/**
	 * Types of compatibility learners implemented - these are supposed to be
	 * set when traces are recorded for compatibility testing with learners used
	 * in May 2008 and December 2007.
	 */
	public enum LEARNER {
		LEARNER_BLUEFRINGE_MAY2008, LEARNER_BLUEAMBER_MAY2008, LEARNER_BLUEFRINGE_DEC2007
	}

	/**
	 * Selects the kind of learner to use. A learner typically has a lot of
	 * customisation options which are set by a configuration.
	 * <p>
	 * <em>null</em> value here means we are not doing compatibility testing.
	 */
	protected LEARNER learnerToUse = null;

	public LEARNER getLearnerToUse() {
		return learnerToUse;
	}

	public void setLearnerToUse(LEARNER learner) {
		learnerToUse = learner;
	}

	/** The number to start numbering vertices in a PTA from. */
	protected int initialIDvalue = 1000;

	public int getInitialIDvalue() {
		return initialIDvalue;
	}

	public void setInitialIDvalue(int newValue) {
		initialIDvalue = newValue;
	}

	/**
	 * A number of constraints might be in use which may be included in a PTA to
	 * make learning faster. Upon a restart, such constraints are automatically
	 * added since restarts indicate when additional information was added and
	 * hence we need to be able to use it to propagate constraints.
	 */
	protected boolean useConstraints = false;

	public boolean isUseConstraints() {
		return useConstraints;
	}

	public void setUseConstraints(boolean newValue) {
		useConstraints = newValue;
	}

	public static final String configXMLTag = "configuration",
			configVarTag = "var", configVarAttrName = "name",
			configVarAttrValue = "value";

	/**
	 * How long to wait before deciding that no new messages will arrive from a
	 * system being analysed.
	 */
	protected int erlFlushDelay = 10;

	public void setErlFlushDelay(int newValue) {
		erlFlushDelay = newValue;
	}

	public int getErlFlushDelay() {
		return erlFlushDelay;
	}

	public enum ERLCOVERAGE {
		ERLCOV_NONE, ERLCOV_LINE, ERLCOV_FUNCTION
	};

	/**
	 * Determines what kind of coverage analysis to engage in when running
	 * traces.
	 */
	protected ERLCOVERAGE erlCoverage = ERLCOVERAGE.ERLCOV_NONE;

	public void setErlCoverage(ERLCOVERAGE newValue) {
		erlCoverage = newValue;
	}

	public ERLCOVERAGE getErlCoverage() {
		return erlCoverage;
	}

	/**
	 * Specific Erlang module we a working with, null if no module is assigned
	 * to this configuration.
	 */
	protected String erlangModuleName = null;

	public String getErlangModuleName() {
		return erlangModuleName;
	}

	public void setErlangModuleName(String newValue) {
		erlangModuleName = newValue;
	}

	/**
	 * Serialises configuration into XML Only primitive strings, enums and
	 * primitive data types are taken care of. For this reason, this should only
	 * be used on classes such as Configuration where I'd like to serialise them
	 * into a DOM stream rather than use XMLEncoder (see top of
	 * DumpProgressDecorator for an explanation why not XMLEncoder).
	 * 
	 * @param doc
	 *            used to create new nodes
	 * @return an element containing the serialised representation of this
	 *         configuration
	 */
	public Element writeXML(Document doc) {
		Element config = doc.createElement(configXMLTag);
		for (Field var : getClass().getDeclaredFields()) {
			if (var.getType() != Configuration.class && var.getName() != "$VRc"// added
																				// by
																				// eclemma
																				// (coverage
																				// analysis)
					&& !java.lang.reflect.Modifier.isFinal(var.getModifiers())) {
				Method getter = AttributeMutator.getMethod(Configuration.class,
						GETMETHOD_KIND.FIELD_GET, var);
				Element varData = doc.createElement(configVarTag);
				try {
					Object value = getter.invoke(this, new Object[] {});
					if (value != null) {
						varData.setAttribute(configVarAttrName, var.getName());
						varData.setAttribute(configVarAttrValue,
								value.toString());
					}
				} catch (Exception e) {
					throwUnchecked(
							"cannot extract a value of " + var.getName(), e);
				}
				config.appendChild(varData);
				config.appendChild(AbstractPersistence.endl(doc));
			}
		}
		return config;
	}

	/**
	 * Loads configuration from XML node.
	 * 
	 * @param cnf
	 *            XML node to load configuration from.
	 */
	public void readXML(org.w3c.dom.Node cnf) {
		readXML(cnf, false);
	}

	/**
	 * Loads configuration from XML node.
	 * 
	 * @param cnf
	 *            XML node to load configuration from.
	 * @param strict
	 *            whether to throw an exception when XML data refers to unknown
	 *            variables.
	 */
	public void readXML(org.w3c.dom.Node cnf, boolean strict) {
		if (cnf.getNodeType() != org.w3c.dom.Node.ELEMENT_NODE)
			throw new IllegalArgumentException(
					"invalid node type passed to readXML");
		Element config = (Element) cnf;
		if (!config.getNodeName().equals(configXMLTag))
			throw new IllegalArgumentException(
					"configuration cannot be loaded from element "
							+ config.getNodeName());
		NodeList nodes = config.getChildNodes();
		for (int i = 0; i < nodes.getLength(); ++i) {
			org.w3c.dom.Node node = nodes.item(i);
			if (node.getNodeType() != org.w3c.dom.Node.TEXT_NODE) {// ignore all
																	// text
																	// nodes
				if (node.getNodeType() != org.w3c.dom.Node.ELEMENT_NODE
						|| !node.getNodeName().equals(configVarTag))
					throw new IllegalArgumentException("unexpected element "
							+ node.getNodeName() + " in configuration XML");
				org.w3c.dom.Element currentElement = (Element) node;
				assignValue(currentElement.getAttribute(configVarAttrName),
						currentElement.getAttribute(configVarAttrValue), strict);
			}
		}
	}

	/**
	 * Given the name of the attribute and a new value, converts the value into
	 * the correct type and assigns it.
	 * 
	 * @param attr
	 *            name of attribute
	 * @param value
	 *            value
	 * @param strict
	 *            if unknown attributes should cause an exception to be thrown
	 *            rather than just ignored.
	 */
	public void assignValue(String attrName, String attrValue, boolean strict) {
		Field var = null;
		try {
			var = getClass().getDeclaredField(attrName);
			Method setter = AttributeMutator.getMethod(Configuration.class,
					GETMETHOD_KIND.FIELD_SET, var);
			Object value = null;
			String valueAsText = attrValue;
			if (var.getType().equals(Boolean.class)
					|| var.getType().equals(boolean.class)) {
				value = Boolean.valueOf(valueAsText);
			} else if (var.getType().equals(Double.class)
					|| var.getType().equals(double.class)) {
				value = Double.valueOf(valueAsText);
			} else if (var.getType().equals(String.class)) {
				value = valueAsText;
			} else if (var.getType().isEnum()) {
				value = Enum.valueOf((Class<Enum>) var.getType(), valueAsText);
			} else if (var.getType().equals(Integer.class)
					|| var.getType().equals(int.class)) {
				value = Integer.valueOf(valueAsText);
			} else
				throw new IllegalArgumentException("A field " + var
						+ " of Configuration has an unsupported type "
						+ var.getType());

			setter.invoke(this, new Object[] { value });
		} catch (NoSuchFieldException e) {
			if (strict)
				throw new IllegalArgumentException(
						"cannot deserialise unknown field " + attrName);
		} catch (Exception e) {
			throwUnchecked("failed to load value of "
					+ (var == null ? "UNKNOWN" : var.getName()), e);
		}
	}
}