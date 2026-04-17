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

import java.util.*;

import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;

/** Describes setup of experiments using markov learning. This is different to
 * {@link MarkovParameters} that describe how to learn each automaton.
 */
public abstract class MarkovLearningParameters implements ThreadResultID
{
	public ScoringToApply learnerToUse;
	public final int states;
	public final int sample;
	public final int trainingSample;
	public final int seed;
	public double alphabetMultiplier = 1;
	public double traceLengthMultiplier = 1;
	public int traceQuantity,statesMax;
	public double traceLengthMultiplierMax,alphabetMultiplierMax;
	boolean usePrintf = false;
	public int perStateSquaredDensityMultipliedBy100 = 0;
	public final MarkovParameters markovParameters = new MarkovParameters();
	
	public MarkovLearningParameters(ScoringToApply l, int argStates, double argAlphabetMultiplier, int perStateSquaredDensity10, int argSample, int argTrainingSample, int argSeed)
	{
		learnerToUse = l;
		states = argStates;alphabetMultiplier = argAlphabetMultiplier;
		perStateSquaredDensityMultipliedBy100 = perStateSquaredDensity10;sample = argSample;trainingSample = argTrainingSample;seed = argSeed;
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
	
	public void setTraceLengthMultiplier(double traceMulti) {
		traceLengthMultiplier=traceMulti;
	}

	@Override
	public String getRowID() {
		return getExperimentID()+"_S="+states+"_m="+alphabetMultiplier+"_d="+ perStateSquaredDensityMultipliedBy100 +"_sa="+sample+"_tS="+trainingSample+"_se="+seed+
				"_tM="+traceLengthMultiplier;
	}

	@Override
	/** Reports the meaning of column header values */
	public String[] getColumnText() {
		List<String> columnData = new ArrayList<>(Collections.singletonList(learnerToUse.toString()));
		if (learnerToUse.isMarkov())
			columnData.addAll(markovParameters.getColumnListForMarkovLearner());
		else
			columnData.addAll(markovParameters.getColumnListForNonMarkovLearner());
			
		return columnData.toArray(new String[]{});
	}

	@Override
	/** Reports the values of column header values. */
	public String getColumnID() 
	{
        return learnerToUse.toString()+"-"+markovParameters.getColumnID(learnerToUse.isMarkov());
	}

	@Override
	/** For each row (a specific walk in a specific automaton) and column (learner type and parameters), reports
	 * experiment results.
	 *
	 * % Invalid/missed refer to mergers,
	 * States refer to the difference between expected and learnt states,
	 * I_Ref, I_Lnt are inconsistencies of the original and learnt automata.
	 *
	 * fracS is the %% of states identifiable as singletons,
	 * marPre is Markov precision
	 * marRec is Morkov recall,
	 * %transitions is the %% of transitions in the reference automata covered by walk from which we are learning.
	 * Time (should always be the last one) refers to the wall clock taken by learner to learn.
	 */
	public String[] headerValuesForEachCell() 
	{
		List<String> headers = new LinkedList<>(Arrays.asList("Success","BCR","Diff","Invalid R","Missed R","Invalid Far","Missed Far","Valid mergers","States","I_Ref", "I_Lnt"));
		if (learnerToUse.isMarkov())
			headers.addAll(Arrays.asList("dI_Ave","dI_SD","alwaysPos","fracS","marTPre","marTRec","marHPre","marHRec","Comparisons"));
		if (markovParameters.useCentreVertex)
			headers.addAll(Arrays.asList("centreCorrect","centerpaths"));
		headers.addAll(Arrays.asList("%transitions","Time"));
		return headers.toArray(new String[]{});
	}

//	@Override
//	public String getSubExperimentName()
//	{
//		return "em";
//	}

	@Override
	public int executionTimeInCell() 
	{// here time is always the last value.
		return headerValuesForEachCell().length-1;
	}
}
