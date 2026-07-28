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

import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;

import java.util.*;

public class MarkovParameters {
	public int chunkLen = 3, preset = 0;
	public boolean useAverageOrMax = true;

	public int divisorForPathCount = 1, expectedWLen = 1;
	public int whichMostConnectedVertex = 0;

	/**
	 * When evaluating whether a transition that was added to a state due to a merge is supposed to be there or not,
	 * we would check whether it was predicted by the Markov model. Where a transition is not predicted by
	 * some incoming path, two cases are possible, either there is a path in Markov table that is not known
	 * to predict such a transition or where there is not even a path in the table corresponding to an
	 * incoming path. An argument to penalise such mergers (creating paths that are not in the table) is that
	 * if we never saw such a path, probably it should not exist and the merge was a mistake. An alternative
	 * explanation is that path is missing because the table is very incomplete (which it would be for large values of chunkLen) and
	 * thus no penalty should be awarded. In experiments with random 10 and 20-state automata and density 10% ... 30%, penalising
	 * missing paths gives slightly better results although a better strategy is to try both (penalising and not penalising) and pick
	 * the outcome with the smaller inconsistency, even if the two inconsistency values are computed in a different way.
	 */
	public boolean penaliseMissingPaths = true;

	/**
	 * If true, we are looking at sequences of transitions to/from a state of interest.
	 * If false, we are looking for sets of labels on transitions into/out of a state of interest. Both are
	 * represented as paths because we need to do a lookup in a collection of paths and numbering of labels
	 * permits elements such sets to be represented as sequences.
	 */
	public boolean pathsOrSets = true;

	/** The outcome of experiments depends on the order in which blue states are considered, because the first blue to be incompatible with all
	 * the red states becomes red. This means that for some ordering, we get better results compared to other ordering. In contrast, the ordering
	 * of red states does not matter because if a blue state is compatible with any red states, the list if pairs will be ordered in order to pick
	 * the best pair. The purpose of this value is to act as a seed in randomisation of the order of blue states.
	 * A value of zero means 'no shuffling'.
 	 */
	public int seedToShuffleSurroundingStates = 0;

	public void setShuffleSeed(int seedToShuffleSurroundingStates) {
		this.seedToShuffleSurroundingStates = seedToShuffleSurroundingStates;
	}

	public MarkovParameters() {
	}

	@SuppressWarnings("CopyConstructorMissesField") // missing fields are created from preset by setPresetLearningParameters
	public MarkovParameters(MarkovParameters a) {
		chunkLen = a.chunkLen;
		preset = a.preset;
		useAverageOrMax = a.useAverageOrMax;
		divisorForPathCount = a.divisorForPathCount;
		expectedWLen = a.expectedWLen;
		whichMostConnectedVertex = a.whichMostConnectedVertex;
		pathsOrSets = a.pathsOrSets;
		seedToShuffleSurroundingStates = a.seedToShuffleSurroundingStates;
		setPresetLearningParameters(preset);
	}

	@Override
	public int hashCode() {
		return Objects.hash(chunkLen, preset, useAverageOrMax, divisorForPathCount, expectedWLen, whichMostConnectedVertex,
				penaliseMissingPaths, pathsOrSets, seedToShuffleSurroundingStates, useCentreVertex, mergeIdentifiedPathsAfterInference,
				useMostConnectedVertexToStartLearning, useNewScoreNearRoot, weightOfInconsistencies, blue_states_forward_and_backwards);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof MarkovParameters)) return false;
		MarkovParameters that = (MarkovParameters) o;
		return chunkLen == that.chunkLen && preset == that.preset && useAverageOrMax == that.useAverageOrMax &&
				divisorForPathCount == that.divisorForPathCount && expectedWLen == that.expectedWLen &&
				whichMostConnectedVertex == that.whichMostConnectedVertex && penaliseMissingPaths == that.penaliseMissingPaths &&
				pathsOrSets == that.pathsOrSets && seedToShuffleSurroundingStates == that.seedToShuffleSurroundingStates &&
				useCentreVertex == that.useCentreVertex && mergeIdentifiedPathsAfterInference == that.mergeIdentifiedPathsAfterInference &&
				useMostConnectedVertexToStartLearning == that.useMostConnectedVertexToStartLearning &&
				useNewScoreNearRoot == that.useNewScoreNearRoot && blue_states_forward_and_backwards == that.blue_states_forward_and_backwards &&
				Objects.equals(weightOfInconsistencies, that.weightOfInconsistencies);
	}

	public MarkovParameters(int pr, int chunkLength, boolean argPathsOrSets, WeightAndOffsetOfInconsistencies weight, boolean addPenaltyForMissingPaths, boolean aveOrMax, int divisor, int mostConnectedVertex, int wlen) {
		setMarkovParameters(pr, chunkLength, argPathsOrSets, weight, addPenaltyForMissingPaths, aveOrMax, divisor, mostConnectedVertex, wlen);
	}

	public static class WeightAndOffsetOfInconsistencies {
		public final double offset;
		public final double weight;

		public WeightAndOffsetOfInconsistencies(double weight,double offset) {
			this.offset = offset;
			this.weight = weight;
		}

		@Override
		public boolean equals(Object o) {
			if (!(o instanceof WeightAndOffsetOfInconsistencies)) return false;
			WeightAndOffsetOfInconsistencies that = (WeightAndOffsetOfInconsistencies) o;
			return Double.compare(offset, that.offset) == 0 && Double.compare(weight, that.weight) == 0;
		}

		@Override
		public int hashCode() {
			return Objects.hash(offset, weight);
		}
	}

	public void setMarkovParameters(int pr, int chunkLength, boolean argPathsOrSets, WeightAndOffsetOfInconsistencies weight,
									boolean addPenaltyForMissingPaths, boolean aveOrMax, int divisor, int mostConnectedVertex, int wlen)
	{
		chunkLen=chunkLength;pathsOrSets = argPathsOrSets;preset = pr;weightOfInconsistencies = weight;penaliseMissingPaths = addPenaltyForMissingPaths;
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
			case 3:// learning by doing pre-merging but starting from root and looking for blue states in both directions.
				setlearningParameters(true, false, false,  false,  true);break;
            case 4:// learning by doing pre-merging and starting from the most connected vertex, but only looking for blue states forward rather than in both directions.
                setlearningParameters(true, false, false,  true,  false);break;
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
	public WeightAndOffsetOfInconsistencies weightOfInconsistencies = new WeightAndOffsetOfInconsistencies(1,0);
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
        return new ArrayList<>(Arrays.asList(Integer.toString(chunkLen), Double.toString(weightOfInconsistencies.offset),
				Double.toString(weightOfInconsistencies.weight),Boolean.toString(penaliseMissingPaths),
				Integer.toString(seedToShuffleSurroundingStates)));
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
	 * Here I should be using MarkovColumnEnum constants instead of "wl" and the like,
	 * however these are long which would make the text of getExperimentID() nearly
	 * unreadable and I need to keep looking at it in order to understand what file names refer to (getColumnID is part of a file name).
	 * @param useMarkovLearner whether the intention is to use Markov learner or any other. This affects which parameters are reported.
	 *                            Even if non-Markov learner is used, most parameters are still relevant, such as prefix length that is utilised in identification of the best centre vertex to use.
	 */
	public String getColumnID(boolean useMarkovLearner)
	{
		String outcome = Integer.toString(preset);// after identification of a centre vertex many learners can be attempted, Markov is not the only one.

		if (useCentreVertex)
		{
			outcome+="_dv="+(useAverageOrMax?"A":"M")+"_d="+divisorForPathCount+"_wl="+expectedWLen+"_b="+(blue_states_forward_and_backwards?"T":"F");
		}
		if (useMarkovLearner)
			outcome+="_cl="+chunkLen+"_wW="+weightOfInconsistencies.weight+"_wO="+weightOfInconsistencies.offset+"_m="+penaliseMissingPaths+"_sh="+seedToShuffleSurroundingStates;
		
		return outcome;
	}
}
