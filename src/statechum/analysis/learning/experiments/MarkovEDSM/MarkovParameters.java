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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class MarkovParameters 
{
	public int chunkLen=3,preset=0;
	public boolean useAverageOrMax = true;

	public int divisorForPathCount=1,expectedWLen=1;
	public int whichMostConnectedVertex = 0;
	
	/** If true, we are looking at sequences of transitions to/from a state of interest. 
	 * If false, we are looking for sets of labels on transitions into/out of a state of interest. Both are 
	 * represented as paths because we need to do a lookup in a collection of paths and numbering of labels 
	 * permits elements such sets to be represented as sequences.
	 */
	public boolean pathsOrSets = true;

	public MarkovParameters()
	{}
	
	@SuppressWarnings("CopyConstructorMissesField") // missing fields are created from preset by setPresetLearningParameters
    public MarkovParameters(MarkovParameters a)
	{
		chunkLen = a.chunkLen;preset = a.preset;
		useAverageOrMax = a.useAverageOrMax;
		divisorForPathCount = a.divisorForPathCount;expectedWLen = a.expectedWLen;
		whichMostConnectedVertex = a.whichMostConnectedVertex;
		pathsOrSets = a.pathsOrSets;

		setPresetLearningParameters(preset);
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + chunkLen;
		result = prime * result + divisorForPathCount;
		result = prime * result + expectedWLen;
		result = prime * result + preset;
		result = prime * result + (useAverageOrMax ? 1231 : 1237);
		result = prime * result + (pathsOrSets ? 1231 : 1237);
        result = prime * result + Double.hashCode(weightOfInconsistencies);
		result = prime * result + whichMostConnectedVertex;
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
		if (!(obj instanceof MarkovParameters))
			return false;
		MarkovParameters other = (MarkovParameters) obj;
		if (chunkLen != other.chunkLen)
			return false;
		if (pathsOrSets != other.pathsOrSets)
			return false;
		if (divisorForPathCount != other.divisorForPathCount)
			return false;
		if (expectedWLen != other.expectedWLen)
			return false;
		if (preset != other.preset)
			return false;
		if (useAverageOrMax != other.useAverageOrMax)
			return false;
		if (Double.doubleToLongBits(weightOfInconsistencies) != Double.doubleToLongBits(other.weightOfInconsistencies))
			return false;
        return whichMostConnectedVertex == other.whichMostConnectedVertex;
    }

	public MarkovParameters(int pr, int chunkLength, boolean argPathsOrSets, double weight, boolean aveOrMax, int divisor, int mostConnectedVertex, int wlen)
	{
		setMarkovParameters(pr,chunkLength,argPathsOrSets,weight,aveOrMax,divisor,mostConnectedVertex,wlen);
	}
	
	public void setMarkovParameters(int pr, int chunkLength, boolean argPathsOrSets, double weight, boolean aveOrMax, int divisor, int mostConnectedVertex, int wlen)
	{
		chunkLen=chunkLength;pathsOrSets = argPathsOrSets;preset = pr;weightOfInconsistencies = weight;
		useAverageOrMax = aveOrMax;divisorForPathCount = divisor;
		whichMostConnectedVertex = mostConnectedVertex;expectedWLen=wlen;
		setPresetLearningParameters(preset);
	}
	
	public void setPresetLearningParameters(int value)
	{
		switch(value)
		{
			case 0:// learning by not doing pre-merging, starting from root
				setlearningParameters(false, false, false, false,  false);break;
			case 1:// learning by doing pre-merging, starting from most connected vertex. This evaluates numerous pairs.
				setlearningParameters(true, false, false, true,  true);break;
			case 2:// learning by doing pre-merging but starting from root.
				setlearningParameters(true, false, false,  false,  false);break;
			case 3:// learning by doing pre-merging but only looking at blue states forward rather than in both directions.
				setlearningParameters(true, false, false,  false,  true);break;
			// alternatives are: learning by not doing pre-merging, starting from root and using a heuristic around root
			// or learning by not doing pre-merging, starting from root and not ranking the top IScore candidates with the fanout metric.
			default:
				throw new IllegalArgumentException("invalid preset number");
		}
	}

	// Values below are assigned by setting a preset.
	
	public boolean useCentreVertex = false;
	public boolean mergeIdentifiedPathsAfterInference = true;
	public boolean useMostConnectedVertexToStartLearning = false;
	public boolean useNewScoreNearRoot = false;
	public double weightOfInconsistencies = 1.0;
	public boolean blue_states_forward_and_backwards;

	public void setlearningParameters(boolean useCentreVertexArg, boolean newScoreNearRoot,
									  boolean mergeIdentifiedPathsAfterInferenceArg,
									  boolean useMostConnectedVertexToStartLearningArg,
									  boolean blue_states_forward_and_backwardsArg)
	{
		useCentreVertex = useCentreVertexArg;useNewScoreNearRoot = newScoreNearRoot;
		mergeIdentifiedPathsAfterInference = mergeIdentifiedPathsAfterInferenceArg;
		useMostConnectedVertexToStartLearning = useMostConnectedVertexToStartLearningArg;
		blue_states_forward_and_backwards = blue_states_forward_and_backwardsArg;
	}

	/** This method is expected to report columns associated with any possible transformation of a PTA into a
	 * directed graph by merging states that Markov believes to be most likely to correspond to the same reference
	 * state.
	 *
	 * @param spacesAtTheEnd how many blank cells to add at the end, intended to make sure the number of cells is the same regardless of how many parameter a learner has got.
	 */
	private List<String> getColumnTextForAnyLearner(int spacesAtTheEnd)
	{
        List<String> whatToReturn = new ArrayList<>(Collections.singletonList(Integer.toString(preset)));
		if (useCentreVertex)
			whatToReturn.addAll(Arrays.asList(
				(useAverageOrMax ? "Ave" : "Max"), Integer.toString(divisorForPathCount),
                Integer.toString(expectedWLen), Boolean.toString(blue_states_forward_and_backwards)));

		for(int i=0;i<spacesAtTheEnd;++i)
			whatToReturn.add("");
		return whatToReturn;
	}
	
	public List<String> getColumnListOnlyForMarkov()
	{
        return new ArrayList<>(Arrays.asList(Integer.toString(chunkLen), Double.toString(weightOfInconsistencies)));
	}
	
	public List<String> getColumnListForMarkovLearner()
	{
		List<String> whatToReturn = getColumnTextForAnyLearner(0);
		whatToReturn.addAll(getColumnListOnlyForMarkov());
		return whatToReturn;
	}

	public List<String> getColumnListForNonMarkovLearner()
	{
		return getColumnTextForAnyLearner(getColumnListOnlyForMarkov().size());
	}

	/** Constructs a component of a columnID determined by these parameters.
	 * 
	 * @param useMarkovLearner whether the intention is to use Markov learner or any other. This affects which parameters are reported. Even if non-Markov learner is used, most parameters are still relevant, such as prefix length that is utilised in identification of the best centre vertex to use.
	 */
	public String getColumnID(boolean useMarkovLearner)
	{
		String outcome = Integer.toString(preset);// after identification of a centre vertex many learners can be attempted, Markov is not the only one.

		if (useCentreVertex)
		{
			outcome+="_dv="+(useAverageOrMax?"A":"M")+"_d="+divisorForPathCount+"_wl="+expectedWLen+"_b="+(blue_states_forward_and_backwards?"T":"F");
		}
		if (useMarkovLearner)
			outcome+="_cl="+chunkLen+"_w="+weightOfInconsistencies;
		
		return outcome;
	}
}
