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

import java.util.Arrays;
import java.util.List;

public class MarkovParameters 
{
	public int chunkLen=3,preset=0;
	public boolean useAverageOrMax = true;
	public double weightOfInconsistencies = 1.0;
	public int divisorForPathCount=1,expectedWLen=1;
	public int whichMostConnectedVertex = 0;
	
	public MarkovParameters()
	{}
	
	public MarkovParameters(int pr, int chunkLength, double weight, boolean aveOrMax, int divisor, int mostConnectedVertex, int wlen)
	{
		setMarkovParameters(pr,chunkLength,weight,aveOrMax,divisor,mostConnectedVertex,wlen);
	}
	
	public void setMarkovParameters(int pr, int chunkLength, double weight, boolean aveOrMax, int divisor, int mostConnectedVertex, int wlen)
	{
		chunkLen=chunkLength;preset = pr;weightOfInconsistencies = weight;useAverageOrMax = aveOrMax;divisorForPathCount = divisor;whichMostConnectedVertex = mostConnectedVertex;expectedWLen=wlen;setPresetLearningParameters(preset);
	}
	
	public void setPresetLearningParameters(int value)
	{
		switch(value)
		{
		case 0:// learning by not doing pre-merging, starting from root 
			setlearningParameters(false, false, false, false, false, false);break;
		case 1:// learning by doing pre-merging, starting from most connected vertex. This evaluates numerous pairs and hence is very slow.
			setlearningParameters(true, false, false, false, false, true);break;
		case 2:// learning by doing pre-merging but starting from root. 
			setlearningParameters(true, false, false, false, false, false);break;
		case 3:// learning by not doing pre-merging, starting from root and using a heuristic around root 
			setlearningParameters(false, false, true, false, true, false);break;
		case 4:// learning by not doing pre-merging, starting from root and not ranking the top IScore candidates with the fanout metric.
			setlearningParameters(false, false, false, false, false, false);break;
		default:
			throw new IllegalArgumentException("invalid preset number");
		}
	}

	public boolean useCentreVertex = false;
	public boolean useDifferentScoringNearRoot = false;
	public boolean mergeIdentifiedPathsAfterInference = true;
	public boolean useClassifyToOrderPairs = true;
	public boolean useMostConnectedVertexToStartLearning = false;
	public boolean useNewScoreNearRoot = false;
	
	public void setlearningParameters(boolean useCentreVertexArg, boolean newScoreNearRoot, boolean useDifferentScoringNearRootArg, boolean mergeIdentifiedPathsAfterInferenceArg, boolean useClassifyToOrderPairsArg, boolean useMostConnectedVertexToStartLearningArg)
	{
		useCentreVertex = useCentreVertexArg;useNewScoreNearRoot = newScoreNearRoot;useDifferentScoringNearRoot = useDifferentScoringNearRootArg;mergeIdentifiedPathsAfterInference = mergeIdentifiedPathsAfterInferenceArg;useClassifyToOrderPairs = useClassifyToOrderPairsArg;useMostConnectedVertexToStartLearning = useMostConnectedVertexToStartLearningArg; 
	}

	private List<String> getColumnTextForAnyLearner(int spacesAtTheEnd)
	{
		List<String> whatToReturn = Arrays.asList(
				Integer.toString(preset),(useAverageOrMax?"Average":"Max"),Integer.toString(divisorForPathCount),Integer.toString(whichMostConnectedVertex),
				Integer.toString(expectedWLen));
		for(int i=0;i<spacesAtTheEnd;++i)
			whatToReturn.add("");
		return whatToReturn;
	}
	
	public List<String> getColumnListOnlyForMarkov()
	{
		return Arrays.asList(new String[]{Integer.toString(chunkLen), Double.toString(weightOfInconsistencies)});
	}
	
	public List<String> getColumnListForMarkovLearner()
	{
		List<String> whatToReturn = getColumnTextForAnyLearner(0);whatToReturn.addAll(getColumnListOnlyForMarkov());
		return whatToReturn;
	}

	public List<String> getColumnListForNonMarkovLearner()
	{
		return getColumnTextForAnyLearner(getColumnListOnlyForMarkov().size());
	}

	/** Constructs a component of a columnID determined by these parameters.
	 * 
	 * @param useMarkovLearner whether the intention is to use Markov learner or any other. This affects which parameters are reported. Even if non-Markov learner is used, most parameters are still relevant, such as prefix length that is utilised in identification of the best centre vertex to use.
	 * @return
	 */
	public String getColumnID(boolean useMarkovLearner)
	{
		String outcome = Integer.toString(preset);// after identification of a centre vertex many learners can be attempted, Markov is not the only one.
		if (useCentreVertex)
			outcome+="_dv="+(useAverageOrMax?"A":"M")+divisorForPathCount+"_v"+whichMostConnectedVertex+"_wl"+expectedWLen;
		outcome +="_cl="+chunkLen;
		if (useMarkovLearner)
			outcome+="_w"+weightOfInconsistencies+(useNewScoreNearRoot?"_NS":"");
		
		return outcome;
	}
}


