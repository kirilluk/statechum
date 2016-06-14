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

import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;

public class MarkovLearningParameters implements ThreadResultID 
{
	enum LearnerToUseEnum
	{
		LEARNER_EDSMMARKOV("edsm_markov"),LEARNER_EDSM2("edsm_2"),LEARNER_EDSM4("edsm_4"),LEARNER_KTAILS_PTA1("kpta=1"),LEARNER_KTAILS_1("k=1"), LEARNER_SICCO("Sicco");
		public final String name;
		private LearnerToUseEnum(String nameText)
		{
			name = nameText;
		}
	}
	public LearnerToUseEnum learnerToUse;
	public final int states;
	public final int sample;
	public final int trainingSample;
	public boolean onlyUsePositives;
	public boolean learnUsingReferenceLearner; 
	public final int seed;
	public int chunkLen=3;
	public int traceQuantityToUse;
	public double alphabetMultiplier = 1;
	public double traceLengthMultiplier = 1;
	public double tracesAlphabetMultiplier = 0;
	public int preset,traceQuantity,statesMax;
	public double traceLengthMultiplierMax,alphabetMultiplierMax;
	boolean usePrintf = false;
	
	public MarkovLearningParameters(LearnerToUseEnum l,int argStates, int argSample, int argTrainingSample, int argSeed, int traceQuantityToUse)
	{
		learnerToUse = l;
		states = argStates;sample = argSample;trainingSample = argTrainingSample;seed = argSeed;this.traceQuantityToUse=traceQuantityToUse;
	}
	
	public void setExperimentID(int chunkLen,int preset,int traceQuantity,double argTraceLengthMultiplierMax,int statesMax,double argAlphabetMultiplierMax)
	{
		this.preset = preset;setPresetLearningParameters(preset);
		this.chunkLen = chunkLen;
		this.traceQuantity = traceQuantity;this.traceLengthMultiplierMax = argTraceLengthMultiplierMax;this.statesMax = statesMax;this.alphabetMultiplierMax = argAlphabetMultiplierMax;
	}
	
	/**
	 * Reflects the name of the experiment attempting inference from a range of FSMs.
	 */
	public String getExperimentID()
	{
		return "ch="+chunkLen+"_tQ="+traceQuantity+"_tMM="+traceLengthMultiplierMax+"_sM="+statesMax+"_aMM="+alphabetMultiplierMax;
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
	
	public void setAlphabetMultiplier(double mult)
	{
		alphabetMultiplier = mult;
	}
	
	public void setTraceLengthMultiplier(double traceMulti) {
		traceLengthMultiplier=traceMulti;
	}

	public void setPresetLearningParameters(int value)
	{
		switch(value)
		{
		case 0:// learning by not doing pre-merging, starting from root 
			setlearningParameters(false, false, false, false, false);break;
		case 1:// learning by doing pre-merging, starting from most connected vertex. This evaluates numerous pairs and hence is very slow.
			setlearningParameters(true, false, false, true, true);break;
		case 2:// learning by doing pre-merging but starting from root. This seems similar to preset 1 on 20 states.
			setlearningParameters(true, true, false, true, false);break;
		case 3:// learning by not doing pre-merging, starting from root and using a heuristic around root 
			setlearningParameters(false, true, false, true, false);break;
		case 4:// learning by not doing pre-merging, starting from root and not ranking the top IScore candidates with the fanout metric.
			setlearningParameters(false, false, false, false, false);break;
		default:
			throw new IllegalArgumentException("invalid preset number");
		}
	}
	boolean useCentreVertex = true, useDifferentScoringNearRoot = false, mergeIdentifiedPathsAfterInference = true, useClassifyToOrderPairs = true,useMostConnectedVertexToStartLearning = false;

	public void setlearningParameters(boolean useCentreVertexArg, boolean useDifferentScoringNearRootArg, boolean mergeIdentifiedPathsAfterInferenceArg, boolean useClassifyToOrderPairsArg, boolean useMostConnectedVertexToStartLearningArg)
	{
		useCentreVertex = useCentreVertexArg;useDifferentScoringNearRoot = useDifferentScoringNearRootArg;mergeIdentifiedPathsAfterInference = mergeIdentifiedPathsAfterInferenceArg;useClassifyToOrderPairs = useClassifyToOrderPairsArg;useMostConnectedVertexToStartLearning = useMostConnectedVertexToStartLearningArg; 
	}

	@Override
	public String getRowID() {
		return getExperimentID()+"_S"+states+"_sa="+sample+"_tS="+trainingSample+"_se="+seed+(onlyUsePositives?"_POS":"_PN")+"_tQU="+traceQuantityToUse+
				"_tM="+traceLengthMultiplier+"_tAMr="+tracesAlphabetMultiplier;
	}

	@Override
	public String[] getColumnText() {
		return new String[]{learnerToUse.name()};
	}

	@Override
	public String getColumnID() {
		return learnerToUse.name();
	}

	public static final String [] cellheaderMarkov = new String[]{"BCR","Diff","states","I_Ref", "I_Lnt","fracS","marPre","marRec","Comparisons","Time"},
			cellheaderConventional = new String[]{"BCR","Diff","States","Time"};
	
	@Override
	public String[] headerValuesForEachCell() 
	{
		if (learnerToUse == LearnerToUseEnum.LEARNER_EDSMMARKOV)
			return cellheaderMarkov;
		
		return cellheaderConventional;
	}

	@Override
	public String getSubExperimentName()
	{
		return "edsm_markov"+preset;
	}

	@Override
	public int executionTimeInCell() 
	{// here time is always the last value.
		if (learnerToUse == LearnerToUseEnum.LEARNER_EDSMMARKOV)
			return cellheaderMarkov.length-1;
		else
			return cellheaderConventional.length-1;
	}
}
