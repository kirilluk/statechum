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

import java.util.Map;

import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.TrueFalseCounter;

public class PairQualityParameters implements ThreadResultID  
{
	public final int states,sample,attempt;
	public boolean onlyUsePositives,pickUniqueFromInitial;
	int ifDepth;
	boolean onlyPositives, useUnique;
	int traceQuantity, lengthMultiplier;
	double trainingDataMultiplier;
	int tracesAlphabetMultiplier;
	int seed;
	/** True means using classifier, false means learning classifier. */
	boolean usingClassifierRatherThanLearningClassifier = true;
	String column = null;
	
	public void setColumn(String text)
	{
		column = text;
	}
	
	ThreadResultID innerLearner = null;
	
	public void setExperimentParameters(boolean whetherUseClassifierOrLearnClassifier, int ifDepth,boolean onlyPositives,boolean useUnique,int traceQuantity,int lengthMultiplier,double trainingDataMultiplier)
	{
		this.ifDepth = ifDepth;this.usingClassifierRatherThanLearningClassifier = whetherUseClassifierOrLearnClassifier;this.onlyPositives = onlyPositives;this.useUnique = useUnique;this.traceQuantity = traceQuantity;this.lengthMultiplier = lengthMultiplier;this.trainingDataMultiplier = trainingDataMultiplier;
	}
	
	public void setInnerParameters(ThreadResultID inner)
	{
		innerLearner = inner;
	}
	
	public boolean selectingRed,classifierToBlockAllMergers;
	public boolean zeroScoringAsRed = false;

	public void setLengthMultiplier(int value)
	{
		lengthMultiplier = value;
	}
	
	/** The length of compound if-then conditions over REL metrics to evaluate. */
	public void setIfdepth(int value)
	{
		ifDepth = value;
	}
	
	/** Whether to filter the collection of traces such that only positive traces are used. */
	public void setOnlyUsePositives(boolean value)
	{
		onlyUsePositives = value;
	}
	
	/** Where a transition that can be uniquely identifying an initial state be used both for mergers and for building a partly-merged PTA. */
	public void setPickUniqueFromInitial(boolean value)
	{
		pickUniqueFromInitial = value;
	}

	public Map<Long,TrueFalseCounter> pairQualityCounter = null;
	
	public void setPairQualityCounter(Map<Long,TrueFalseCounter> c)
	{
		pairQualityCounter = c;
	}
	
	public PairQualityParameters(int argStates,int argSample, int argAttempt, int argSeed) 
	{
		states = argStates;sample=argSample;attempt=argAttempt;seed = argSeed;
	}
	
	public String getExperimentID()
	{
		String exp = "ifD="+ifDepth+
		(onlyUsePositives?"_POS":"_PN")+(useUnique?"_U":"")+"_tQU="+traceQuantity+"_tM="+lengthMultiplier+"_tAMr="+tracesAlphabetMultiplier+"_tDM="+trainingDataMultiplier;
		if (innerLearner != null)
			exp+="-"+innerLearner.getRowID();
		return exp;
	}
	
	@Override
	public String getRowID()
	{
		return getExperimentID()+";sample="+sample+";attempt="+attempt+";seed="+seed;
	}

	@Override
	public String[] getColumnText() 
	{
		return new String[]{column};
	}

	@Override
	public String getColumnID() 
	{
		return column;
	}

	@Override
	public String[] headerValuesForEachCell() 
	{
		if (pairQualityCounter != null)
			return new String[]{"BCR","Diff","States","PairQuality","Time"};
		else
			return new String[]{"BCR","Diff","States","Time"};
	}

	@Override
	public String getSubExperimentName()
	{
		if (usingClassifierRatherThanLearningClassifier)
			return "Learning using classifier";
		else
			return "Learning classifiers";
	}

	@Override
	public int executionTimeInCell() 
	{
		if (pairQualityCounter != null)
			return 4;
		else
			return 3;
	}
}
