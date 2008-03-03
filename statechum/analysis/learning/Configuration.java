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

package statechum.analysis.learning;

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
	protected int generalisationThreshold=0;
	protected int pairsMergedPerHypothesis=0;
	
	Configuration() {}
	
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
	
	/** Classical score computation matches two nodes but does not look at parts of a PTA being 
	 * traversed during the computation of those scores. In particular, if there is a transition
	 * with some label a from a blue node and a transition with the same label leads from the
	 * red node to blue one, classical score computation proceeds to the target states of these
	 * two transitions and keeps matching, in effect comparing a PTA branch with itself which 
	 * seems strange. This variable prevents score computation routine to enter the 
	 * PTA branch which starts with the blue node.  
	 */
	protected boolean useCompatibilityScore = false;
	
	public enum IDMode { NONE, POSITIVE_NEGATIVE, POSITIVE_ONLY };

	/** Some algorithms depend on the order in which elements of a graph are traversed;
	 * this in turn depends on the lexicographic order of their names (see testDeterminism()
	 * for a test which checks that this does not affect W set). This variable controls 
	 * the order in which number to nodes in a PTA are assigned. POSITIVE_ONLY gives 
	 * sequential numbers; POSITIVE_NEGATIVE gives negative numbers to reject vertices
	 * and positive ones to accept ones.
	 */
	protected IDMode learnerIdMode = IDMode.NONE; // creation of new vertices is prohibited.

	public void setLearnerIdMode(IDMode m)
	{
		learnerIdMode = m;
	}
	
	public IDMode getMode()
	{
		return learnerIdMode;
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

	public boolean isUseCompatibilityScore() {
		return useCompatibilityScore;
	}

	public void setUseCompatibilityScore(boolean useCompatibilityScoreArg) {
		this.useCompatibilityScore = useCompatibilityScoreArg;
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
	
	/** The initial state in a PTA has to be given some name, this is the default. */
	protected String defaultInitialPTAName = "Init";
	
	public void setDefaultInitialPTAName(String name)
	{
		defaultInitialPTAName = name;
	}
	
	public String getDefaultInitialPTAName()
	{
		return defaultInitialPTAName;
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
		result = prime * result + pairsMergedPerHypothesis;
		result = prime * result + (useCompatibilityScore ? 1231 : 1237);
		result = prime * result + (allowedToCloneNonCmpVertex? 1231 : 1237);
		result = prime * result + defaultInitialPTAName.hashCode();
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
		if (pairsMergedPerHypothesis != other.pairsMergedPerHypothesis)
			return false;
		if (useCompatibilityScore != other.useCompatibilityScore)
			return false;
		if (allowedToCloneNonCmpVertex != other.allowedToCloneNonCmpVertex)
			return false;
		if (!defaultInitialPTAName.equals(other.defaultInitialPTAName))
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
}