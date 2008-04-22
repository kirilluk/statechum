/*
 * Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

import java.util.Arrays;
import java.util.Collection;

/** Represents a configuration for a learner. The purpose is a possibility of a 
 * global customisation of all objects used by a learner in the course of 
 * learning by the same object.
 * <p> 
 * When you add to this class, please add the corresponding entries to hashCode and equals. 
 */
public class Configuration implements Cloneable
{
	/** Pairs with scores lower than this are not considered for merging. */
	protected int generalisationThreshold=0;
	
	/** Limits the number of pairs returns from a method looking for pairs to merge. 
	 * Zero inhibits the limitation. 
	 */
	protected int pairsMergedPerHypothesis=0;
	
	public Configuration() {}
	
	protected final static Configuration defaultConfig = new Configuration();
	
	public static Configuration getDefaultConfiguration()
	{
		return defaultConfig; 
	}
	
	/** Reject nodes never get a score of over zero since there are no paths starting from
	 * them in a PTA. Same happens to tail accept-nodes in a PTA. If one wishes to get 
	 * positives nodes merged first where all possible pairs have zero scores, this one can
	 * be set to true in which all positive nodes have their scores artificially inflated by 1. 
	 */
	protected boolean bumpPositives = false;
	
	public enum IDMode { NONE, POSITIVE_NEGATIVE, POSITIVE_ONLY };

	/** Some algorithms depend on the order in which elements of a graph are traversed;
	 * this in turn depends on the lexicographic order of their names (see testDeterminism()
	 * for a test which checks that this does not affect W set). This variable controls 
	 * the order in which number to nodes in a PTA are assigned. POSITIVE_ONLY gives 
	 * sequential numbers; POSITIVE_NEGATIVE gives negative numbers to reject vertices
	 * and positive ones to accept ones.
	 */
	protected IDMode learnerIdMode = IDMode.NONE; // creation of new vertices is prohibited.

	public enum ScoreMode { CONVENTIONAL, COMPATIBILITY, KTAILS };
	
	/** The are a few ways in which one can compute scores associated to pairs of states,
	 * using a conventional computation, using compatibility scores (during traversal of pairs,
	 * ensures that the red state does not enter the PTA) and the k-tails traversal
	 * (scores are only updated on paths from the blue state of length k and less).
	 * Classical score computation matches two nodes but does not look at parts of a PTA being 
	 * traversed during the computation of those scores. In particular, if there is a transition
	 * with some label a from a blue node and a transition with the same label leads from the
	 * red node to blue one, classical score computation proceeds to the target states of these
	 * two transitions and keeps matching, in effect comparing a PTA branch with itself which 
	 * seems strange. This variable prevents score computation routine to enter the 
	 * PTA branch which starts with the blue node.  
	 */
	protected ScoreMode learnerScoreMode = ScoreMode.CONVENTIONAL;
	
	public void setLearnerScoreMode(ScoreMode mode)
	{
		learnerScoreMode = mode;
	}
	
	public ScoreMode getLearnerScoreMode()
	{
		return learnerScoreMode;
	}
	
	public void setLearnerIdMode(IDMode m)
	{
		learnerIdMode = m;
	}
	
	public IDMode getMode()
	{
		return learnerIdMode;
	}
	
	/** There could be different ways to query the current strategy for asking questions.
	 * The enumeration below includes those implemented:
	 * <ul>
	 * <li>CONVENTIONAL - this is essentially the QSM question generator.
	 * </li>
	 * 
	 * <li>SYMMETRIC - when asking questions, this question generator does not care which
	 * state is blue and which is red. Its purpose is to handle graph which do not  
	 * have a tree as in QSM. In addition to generality, it also aims to address the shortcomings
	 * in the QSM algorithm. The aim is to query all transitions from all states which were 
	 * merged together (from all equivalence classes). Paths to those states are chosen from
	 * both the two original states A and B merged, considering a path between the two states
	 * (A and B are not necessarily adjacent, even in QSM where they are a red/blue pair).
	 * </li>
	 * </ul>
	 */
	public enum QuestionGeneratorKind { CONVENTIONAL, SYMMETRIC };
	
	protected QuestionGeneratorKind questionGenerator = QuestionGeneratorKind.CONVENTIONAL;
	
	public QuestionGeneratorKind getQuestionGenerator()
	{
		return questionGenerator;
	}
	
	public void setQuestionGenerator(QuestionGeneratorKind qs)
	{
		questionGenerator = qs;
	}
	
	/** In order to reduce the number of questions asked, it might be useful to limit the number of paths
	 * which get collected when we are looking for all possible shortest paths between pairs of states
	 * (such as between the initial state and the red one in the original QSM question generator).
	 * Note that this constant is applied selectively by different question generators.
	 * <p>
	 * The default value is negative, i.e. no limit. Zero would generate empty questions since 
	 * question generation algorithms start with so many paths, then extend them. If no paths
	 * are initially produced, the set of questions will be empty.
	 */
	protected int questionPathUnionLimit = -1;
	
	public int getQuestionPathUnionLimit()
	{
		return questionPathUnionLimit;
	}
	
	public void setQuestionPathUnionLimit(int limit)
	{
		questionPathUnionLimit = limit;
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

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			IllegalArgumentException ex = new IllegalArgumentException("clone of Configuration failed - should not happen");
			ex.initCause(e);throw ex;
		}
	}
	
	/** Whether construction of LearnerGraph from a Jung graph should clone vertices of that graph. */
	protected boolean LearnerCloneGraph = true;
	
	/** Whether LearnerGraph should use StringVertex or DeterministicVertex as vertices of a graph. */
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
	
	/** The initial state in a PTA has to be given some name, this is the default.
	 * Important: "" means "generate a numerical identifier" which is almost
	 * always the best choice, but in some specific tests, I'd like to use
	 * one with a known name. 
	 */
	protected String defaultInitialPTAName = "";
	
	public void setDefaultInitialPTAName(String name)
	{
		defaultInitialPTAName = name;
	}
	
	public String getDefaultInitialPTAName()
	{
		return defaultInitialPTAName;
	}
	
	/** Used to define the file name to be used for auto-loading answers. Zero-length means no auto. */
	protected String autoAnswerFileName = "";
	
	public void setAutoAnswerFileName(String name)
	{
		autoAnswerFileName = name;
	}
	
	public String getAutoAnswerFileName()
	{
		return autoAnswerFileName;
	}
	
	public static Collection<Object[]> configurationsForTesting() 
	{
		Configuration same = new Configuration();same.setLearnerUseStrings(false);same.setLearnerCloneGraph(true);
		Configuration clone = new Configuration();same.setLearnerUseStrings(false);same.setLearnerCloneGraph(false);
		Configuration strings = new Configuration();same.setLearnerUseStrings(true);same.setLearnerCloneGraph(false);
		return Arrays.asList(new Object[][] { { same }, { clone }, { strings }} );
	}

	/* (non-Javadoc)
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
		result = prime * result + ((learnerIdMode == null) ? 0 : learnerIdMode.hashCode());
		result = prime * result + ((learnerScoreMode == null)?0: learnerScoreMode.hashCode());
		result = prime * result + ((questionGenerator == null)?0: questionGenerator.hashCode());
		result = prime * result + pairsMergedPerHypothesis;
		result = prime * result + (allowedToCloneNonCmpVertex? 1231 : 1237);
		result = prime * result + defaultInitialPTAName.hashCode();
		result = prime * result + (debugMode? 1231 : 1237);
		result = prime * result + certaintyThreshold;
		result = prime * result + minCertaintyThreshold;
		result = prime * result + klimit;
		result = prime * result + (askQuestions? 1231 : 1237);
		result = prime * result + randomPathAttemptThreshold;
		result = prime * result + randomPathAttemptFudgeThreshold;
		result = prime * result + (generateTextOutput? 1231 : 1237);
		result = prime * result + ((autoAnswerFileName == null) ?0: autoAnswerFileName.hashCode());
		result = prime * result + questionPathUnionLimit;
		result = prime * result + (int)(attenuationK*100);
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
		if (questionPathUnionLimit != other.questionPathUnionLimit)
			return false;
		if (attenuationK != other.attenuationK)
			return false;
		return true;
	}

	/** For testing, I'd like to mix pure Jung routines and LearnerGraph 
	 * ones, hence it should be possible to build a LearnerGraph
	 * out of whatever Jung graph I was given. In production use, 
	 * graphs are either to be built from GraphML or obtained from 
	 * LearnerGraph. In both cases these will be deterministic graphs.
	 * For this reason, it is good to be able to detect when we've been
	 * passed a graph which does not consist of deterministic vertices.
	 */
	protected boolean allowedToCloneNonCmpVertex = false;
	
	public boolean isAllowedToCloneNonCmpVertex() 
	{
		return allowedToCloneNonCmpVertex;
	}

	public void setAllowedToCloneNonCmpVertex(boolean allowed)
	{
		allowedToCloneNonCmpVertex = allowed;
	}

	/** Used to pop up auxiliary information when under test. */
	protected boolean debugMode;
	
	public void setDebugMode(boolean debug) {
		this.debugMode = debug;
	}

	public boolean getDebugMode()
	{
		return debugMode;
	}
	
	/** Pairs with very high scores should perhaps be merged anyway.
	 * For this reason, we do not ask questions if we get a score at least that high.
	 * Negative number inhibits the check.
	 */ 
	protected int certaintyThreshold = -1;
	
	public void setCertaintyThreshold(int threshold) 
	{
		certaintyThreshold = threshold;
	}

	public int getCertaintyThreshold()
	{
		return certaintyThreshold;
	}

	/** Pairs with scores of zero or just over tend to remain at the end, 
	 * generating loads of questions.
	 * One way to avoid them is to silently merge those states. If a score
	 * is less than the number below, the corresponding pair will be 
	 * silently merged.
	 */ 
	protected int minCertaintyThreshold = -1;
	
	public void setMinCertaintyThreshold(int minThreshold) {
		this.minCertaintyThreshold = minThreshold;
	}

	public int getMinCertaintyThreshold() {
		return minCertaintyThreshold;
	}

	/** When doing k-tail merging, this number determines how far we go when computing
	 * a positive/zero number. Negatives are always computed by a full traversal - 
	 * without this, we'll be attempting to merge incompatible vertices.
	 * The lowest number is 1 which corresponds to looking just one transition ahead
	 * from the red-blue state pair. 
	 */
	protected int klimit;
	
	public int getKlimit() 
	{
		return klimit;
	}

	public void setKlimit(int limit) {
		klimit = limit;
	}

	/** Whether we should try auto-answer questions no matter what or ask a user. */
	protected boolean askQuestions = true;

	public void setAskQuestions(boolean ask)
	{
		askQuestions = ask;
	}
	
	public boolean getAskQuestions()
	{
		return askQuestions;
	}

	/** When looking for random paths of a specific size, one of the easy ways 
	 * is to make a number of attempts and fail if cannot get a path of 
	 * the expected length. 
	 */
	protected int randomPathAttemptThreshold = 100; 
	
	public int getRandomPathAttemptThreshold() 
	{
		return randomPathAttemptThreshold;
	}
	
	public void setRandomPathAttemptThreshold(int thr)
	{
		randomPathAttemptThreshold = thr;
	}
	/** When we failed to generate a path for a specific length, we'll try a different length.
	 * The number of attempts at this is constrained by the constant below.
	 */
	protected int randomPathAttemptFudgeThreshold = 100; 
	
	public int getRandomPathAttemptFudgeThreshold() 
	{
		return randomPathAttemptFudgeThreshold;
	}
	
	public void setRandomPathAttemptFudgeThreshold(int thr)
	{
		randomPathAttemptFudgeThreshold = thr;
	}

	protected boolean generateTextOutput = false;
	
	public boolean isGenerateTextOutput() {
		return generateTextOutput;
	}

	public void setGenerateTextOutput(boolean generateText) {
		generateTextOutput = generateText;
	}
	
	/** When doing linear, we need a way to attenuate the compatibility score associated
	 * to states into which we have transitions. This values provides the appropriate 
	 * attenuation, which has to be under 1, since otherwise the matrix is likely to be
	 * singular. Using a value of 0 would imply we ignore all outgoing transitions.
	 */
	protected double attenuationK=0.9;
	
	public double getAttenuationK()
	{
		return attenuationK;
	}

	/** Sets the new value of k.
	 * 
	 * @param k the new value of attenuation.
	 * @throws IllegalArgumentException if k is negative or 1 or over 1.
	 */
	public void setAttenuationK(double k)
	{
		if (k<0 || k>=1) throw new IllegalArgumentException("attenuation should be within [0,1[");
		attenuationK = k;
	}
	
	/** A test-only version of the above, permitting a valuve of 1. */
	public void setAttenuationK_testOnly(double k)
	{
		if (k<0 || k>1) throw new IllegalArgumentException("attenuation should be within [0,1[");
		attenuationK = k;
	}
}