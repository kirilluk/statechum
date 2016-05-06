/* Copyright (c) 2016 The University of Sheffield.
 * 
 * This file is part of StateChum
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

package statechum.analysis.learning.experiments.PairSelection;

import java.util.Map;

import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.TrueFalseCounter;

public class LearnWithClassifiersResult extends ExperimentResult<PairQualityParameters> {

	public LearnWithClassifiersResult(PairQualityParameters p) {
		super(p);
	}

	public Map<Long,TrueFalseCounter> pairQualityCounter = null;
	
	public void setPairQualityCounter(Map<Long,TrueFalseCounter> c)
	{
		pairQualityCounter = c;
	}
}
