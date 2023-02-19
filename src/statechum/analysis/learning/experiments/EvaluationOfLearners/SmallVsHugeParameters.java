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
package statechum.analysis.learning.experiments.EvaluationOfLearners;

import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;

public class SmallVsHugeParameters extends EvaluationOfLearnersParameters
{
	public SmallVsHugeParameters(ScoreMode scEDSM, ScoringToApply scoring, LearningType type, boolean pta, STATETREE matrix) 
	{
		super(scEDSM, scoring, type, pta, matrix);
	}

	@Override
	public String []getColumnText()
	{
		return new String[]{ learningType.name, 
				(scoringForEDSM==null?"":scoringForEDSM.name),scoringMethod.name,ptaMergersToString(ptaMergers),matrixType.name,Integer.toString(traceQuantity),Integer.toString(lengthmult)}; 
	}

	@Override
	public String[] headerValuesForEachCell() 
	{
		return new String[]{"Success","BCR","Diff","M_Invalid","M_Missed","States","prohibited","traceLength","Time"};
	}

	@Override
	public String getSubExperimentName()
	{
		return "Small_vs_huge_experiments";
	}
	
	@Override
	public int executionTimeInCell() {
		return 8;
	}
}