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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovParameters;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.TrueFalseCounter;

public class PairQualityParameters implements ThreadResultID  
{
	public final int states,sample,attempt;
	public boolean pickUniqueFromInitial;
	int ifDepth;
	boolean onlyUsePositives, useUnique;
	int traceQuantity, traceLengthMultiplier;
	double trainingDataMultiplier;
	int tracesAlphabetMultiplier;
	int seed;
	/** True means using classifier, false means learning classifier. */
	boolean usingClassifierRatherThanLearningClassifier = true;
	String column = null;
	
	public MarkovParameters markovParameters = new MarkovParameters();
	
	
	public PairQualityParameters(PairQualityParameters a)
	{
		states=a.states;sample=a.sample;attempt=a.attempt;
		pickUniqueFromInitial = a.pickUniqueFromInitial;
		ifDepth = a.ifDepth;
		onlyUsePositives = a.onlyUsePositives;useUnique = a.useUnique;
		traceQuantity = a.traceQuantity;traceLengthMultiplier = a.traceLengthMultiplier;
		trainingDataMultiplier = a.trainingDataMultiplier;
		tracesAlphabetMultiplier = a.tracesAlphabetMultiplier;
		seed = a.seed;
		usingClassifierRatherThanLearningClassifier = a.usingClassifierRatherThanLearningClassifier;
		column = a.column;
		markovParameters = new MarkovParameters(a.markovParameters);
		innerLearner = a.innerLearner;
	}
	
	public void setColumn(String text)
	{
		column = text;
	}
	
	ThreadResultID innerLearner = null;
	
	public void setExperimentParameters(boolean whetherUseClassifierOrLearnClassifier, int ifDepth,boolean onlyPositives,boolean useUnique,int alphabetMult, int traceQuantity,int lengthMultiplier,double trainingDataMultiplier)
	{
		this.ifDepth = ifDepth;this.usingClassifierRatherThanLearningClassifier = whetherUseClassifierOrLearnClassifier;this.onlyUsePositives = onlyPositives;this.useUnique = useUnique;this.tracesAlphabetMultiplier = alphabetMult; this.traceQuantity = traceQuantity;this.traceLengthMultiplier = lengthMultiplier;this.trainingDataMultiplier = trainingDataMultiplier;
	}
	
	public void setInnerParameters(ThreadResultID inner)
	{
		innerLearner = inner;
	}
	
	public boolean selectingRed,classifierToBlockAllMergers;
	public boolean zeroScoringAsRed = false;

	public void setLengthMultiplier(int value)
	{
		traceLengthMultiplier = value;
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
	
	public String getIfDepthAsString()
	{
		return "ifD="+ifDepth;
	}
	
	public String getExperimentID()
	{
		String exp = 
		(onlyUsePositives?"POS":"PN")+(useUnique?"_U":"")+"_tQU="+traceQuantity+"_tM="+traceLengthMultiplier+"_tAMr="+tracesAlphabetMultiplier+"_tDM="+trainingDataMultiplier;
		if (innerLearner != null)
			exp+="-"+innerLearner.getRowID();
		return exp;
	}
	
	@Override
	public String getRowID()
	{
		return getExperimentID()+"_S"+states+"_sa="+sample+"_se="+seed+"_"+(innerLearner == null?"":("_"+innerLearner.getRowID()));
	}

	@Override
	public String[] getColumnText() 
	{
		List<String> values = new ArrayList<String>();values.add(column);values.add(Integer.toString(ifDepth));values.addAll(markovParameters.getColumnListForMarkovLearner());
		return values.toArray(new String[]{});
	}

	@Override
	public String getColumnID() 
	{
		return column+getIfDepthAsString()+"_"+markovParameters.getColumnID(true);
	}

	public static final String [] cellheaderStd = new String[]{"BCR","Diff","States","fracS","marPre","marRec","Comparisons","centreCorrect","centerpaths","%transitions"};
	
	@Override
	public String[] headerValuesForEachCell() 
	{
		List<String> cellHeader = Arrays.asList(cellheaderStd);
		
		if (pairQualityCounter != null)
			cellHeader.add("PairQuality");
		cellHeader.add("Time");
		return cellHeader.toArray(new String[]{});
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
		return headerValuesForEachCell().length-1;
	}
}
