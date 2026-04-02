/* Copyright (c) 2016 The University of Sheffield.
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
package statechum.analysis.learning.experiments.MarkovEDSM;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;

public class MarkovLearningParameters implements ThreadResultID 
{
	public ScoringToApply learnerToUse;
	public final int states;
	public final int sample;
	public final int trainingSample;
	public boolean onlyUsePositives;
	public final int seed;
	public double alphabetMultiplier = 1;
	public double traceLengthMultiplier = 1;
	public double tracesAlphabetMultiplier = 0;
	public int traceQuantity,statesMax;
	public double traceLengthMultiplierMax,alphabetMultiplierMax;
	boolean usePrintf = false;
	public int perStateSquaredDensityMultipliedBy10 = 0;
	public final MarkovParameters markovParameters = new MarkovParameters();
	
	public MarkovLearningParameters(ScoringToApply l, int argStates, double argAlphabetMultiplier, int density10, int argSample, int argTrainingSample, int argSeed)
	{
		learnerToUse = l;
		states = argStates;alphabetMultiplier = argAlphabetMultiplier;perStateSquaredDensityMultipliedBy10 = density10;sample = argSample;trainingSample = argTrainingSample;seed = argSeed;
	}
	
	public void setExperimentID(int traceQuantity,double argTraceLengthMultiplierMax,int statesMax,double argAlphabetMultiplierMax)
	{
		this.traceQuantity = traceQuantity;this.traceLengthMultiplierMax = argTraceLengthMultiplierMax;this.statesMax = statesMax;this.alphabetMultiplierMax = argAlphabetMultiplierMax;
	}

	/**
	 * Reflects the name of the experiment attempting inference from a range of FSMs.
	 */
	public String getExperimentID()
	{
		return "tQ="+traceQuantity+"_tMM="+traceLengthMultiplierMax+"_sM="+statesMax+"_aMM="+alphabetMultiplierMax;
	}
	
	public void setUsePrintf(boolean value)
	{
		usePrintf = value;
	}
	
	/** Whether we should try learning with zero inconsistencies, to see how heuristics fare. */
	protected boolean disableInconsistenciesInMergers = false;
	
	public void setDisableInconsistenciesInMergers(boolean v)
	{
		disableInconsistenciesInMergers = v;
	}
	
	public void setTracesAlphabetMultiplier(double evalAlphabetMult)
	{
		tracesAlphabetMultiplier = evalAlphabetMult;
	}
		
	/** Whether to filter the collection of traces such that only positive traces are used. */
	public void setOnlyUsePositives(boolean value)
	{
		onlyUsePositives = value;
	}

	public void setTraceLengthMultiplier(double traceMulti) {
		traceLengthMultiplier=traceMulti;
	}

	@Override
	public String getRowID() {
		return getExperimentID()+"_S="+states+"_m="+alphabetMultiplier+"_d="+perStateSquaredDensityMultipliedBy10+"_sa="+sample+"_tS="+trainingSample+"_se="+seed+(onlyUsePositives?"_POS":"_PN")+
				"_tM="+traceLengthMultiplier+"_tAMr="+tracesAlphabetMultiplier;
	}

	@Override
	public String[] getColumnText() {
		List<String> columnData = new ArrayList<>(Collections.singletonList(learnerToUse.name()));
		if (learnerToUse.isMarkov())
			columnData.addAll(markovParameters.getColumnListForMarkovLearner());
		else
			columnData.addAll(markovParameters.getColumnListForNonMarkovLearner());
			
		return columnData.toArray(new String[]{});
	}

	@Override
	public String getColumnID() 
	{
        return learnerToUse.name()+"-"+markovParameters.getColumnID(learnerToUse.isMarkov());
	}

	public static final String [] cellheaderMarkov = new String[]{"Success","BCR","Diff","M_Invalid","M_Missed","States","I_Ref", "I_Lnt","fracS","marPre","marRec","Comparisons","centreCorrect","centerpaths","%transitions","Time"},
			cellheaderConventional = new String[]{"Success","BCR","Diff","M_Invalid","M_Missed","States","I_Ref", "I_Lnt","centreCorrect","centerpaths","%transitions","Time"};
	
	@Override
	public String[] headerValuesForEachCell() 
	{
		if (learnerToUse.isMarkov())
			return cellheaderMarkov;
		
		return cellheaderConventional;
	}

	@Override
	public String getSubExperimentName()
	{
		return "em";
	}

	@Override
	public int executionTimeInCell() 
	{// here time is always the last value.
		if (learnerToUse.isMarkov())
			return cellheaderMarkov.length-1;
		else
			return cellheaderConventional.length-1;
	}
}
