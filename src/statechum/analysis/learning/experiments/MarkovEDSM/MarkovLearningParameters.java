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

import statechum.Configuration;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

public class MarkovLearningParameters implements ThreadResultID 
{
	public final int states,sample,trainingSample;
	public boolean onlyUsePositives;
	public boolean learnUsingReferenceLearner; 
	public final int seed;
	public int chunkLen=3;
	public int traceQuantity;
	public String selectionID;
	public double alphabetMultiplier = 1;
	public double traceLengthMultiplier = 1;
	public double tracesAlphabetMultiplier = 0;

	public MarkovLearningParameters(int argStates, int argSample, int argTrainingSample, int argSeed, int nrOfTraces)
	{
		states = argStates;sample = argSample;trainingSample = argTrainingSample;seed = argSeed;traceQuantity=nrOfTraces;
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
	
	public void setSelectionID(String value)
	{
		selectionID = value;
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
	
	public void setTraceQuantity(int traceQuantity2) {
		traceQuantity=	traceQuantity2;		
	}
	
	public void setChunkLen(int len)
	{
		chunkLen = len;
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
		return ";sample="+sample+";attempt="+attempt+";seed="+seed+";ifDepth="+ifDepth+";onlyPositives="+onlyPositives+";useUnique="+useUnique+";traceQuantity="+traceQuantity+";lengthMultiplier="+lengthMultiplier+";trainingDataMultiplier="+trainingDataMultiplier+";";
	}

	@Override
	public String[] getColumnText() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getColumnID() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String[] headerValuesForEachCell() {
		// TODO Auto-generated method stub
		return null;
	}

}
