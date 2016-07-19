/* Copyright (c) 2016 The University of Sheffield
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

package statechum.analysis.learning.experiments.PairSelection;

import statechum.analysis.learning.experiments.MarkovEDSM.MarkovParameters;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;

public class UseWekaResultsParameters implements ThreadResultID
{
	public UseWekaResultsParameters(int ifDepth) {
		this.ifDepth = ifDepth;
	}
	
	// This duplicates that in a higher-level experiment parameters, however only these parameters are accessible to the learner that uses them, we hence to make a copy here.
	int ifDepth;
	
	/** The length of compound if-then conditions over REL metrics to evaluate. */
	public void setIfdepth(int value)
	{
		ifDepth = value;
	}
	
	public boolean selectingRed,classifierToBlockAllMergers;
	public boolean zeroScoringAsRed = false;

	public final MarkovParameters markovParameters = new MarkovParameters();
	
	double threshold = 1.5;
	
	public void setThreshold(double value)
	{
		threshold = value;
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

	/** Where a pair has a zero score but Weka is not confident that this pair should not be merged, where this flag, such a pair will be assumed to be unmergeable. Where there is a clearly wrong pair
	 * detected by Weka, its blue state will be marked red, where no pairs are clearly appropriate for a merger and all of them have zero scores, this flag will cause a blue state in one of them to be marked red.  
	 */
	protected boolean blacklistZeroScoringPairs = false;

	public void setBlacklistZeroScoringPairs(boolean value)
	{
		blacklistZeroScoringPairs = value;
	}

	@Override
	public String getRowID()
	{
		return "IFD="+ifDepth+"_TH="+threshold+(selectingRed?"_RED":"")+(classifierToBlockAllMergers?"_BL":"")+(zeroScoringAsRed?"_ZR":"");
	}

	@Override
	public String[] getColumnText() 
	{
		return new String[]{"Learner"};
	}

	@Override
	public String getColumnID() 
	{
		return "WithClassifier";
	}

	@Override
	public String[] headerValuesForEachCell() 
	{
		return new String[]{"BCR","Diff","States"};
	}

	@Override
	public String getSubExperimentName() 
	{
		throw new UnsupportedOperationException("this method should not be called");
	}

	@Override
	public int executionTimeInCell() {
		return -1;
	}
}
