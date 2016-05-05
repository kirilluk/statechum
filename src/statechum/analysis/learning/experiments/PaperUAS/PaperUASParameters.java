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
package statechum.analysis.learning.experiments.PaperUAS;

import statechum.Configuration;
import statechum.analysis.learning.experiments.EvaluationOfLearners.EvaluationOfLearnersParameters.LearningType;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;

public class PaperUASParameters implements ThreadResultID
{
	public final LearningType learningType;
	public boolean onlyUsePositives = false;
	public String ptaName;
	public String ptaID;
	public boolean useIfThen;
	public final boolean ptaMergers;
	public final ScoringToApply scoringMethod;
	public final Configuration.ScoreMode scoringForEDSM;
	public final Configuration.STATETREE matrixType;
	
	public void setOnlyUsePositives(boolean value)
	{
		onlyUsePositives = value;
	}
	
	public void setParameters(boolean usePositive,boolean ifthen,String argPtaID,String experimentName)
	{
		onlyUsePositives = usePositive;useIfThen=ifthen;ptaName = experimentName;ptaID = argPtaID;
	}
	
	public PaperUASParameters(Configuration.ScoreMode scEDSM,ScoringToApply scoring, LearningType type,boolean pta, Configuration.STATETREE matrix)
	{
		ptaMergers = pta;matrixType = matrix;scoringMethod = scoring;scoringForEDSM = scEDSM;learningType = type;
	}
	
	public static String ptaMergersToString(boolean ptaMergers)
	{
		return (ptaMergers?"PTA":"GEN");
	}
	
	@Override
	public String getColumnID()
	{
		return (onlyUsePositives?"Pos":"posNeg")+"-"+(learningType == null?"":learningType.name)+"-"+(scoringForEDSM==null?"none":scoringForEDSM.name)+"-"+scoringMethod.name+"-"+ptaMergersToString(ptaMergers)+"-"+matrixType.name; 
	}

	@Override
	public String []getColumnText()
	{
		return new String[]{ (onlyUsePositives?"Pos":"posNeg"), (scoringForEDSM==null?"":scoringForEDSM.name),scoringMethod.name,ptaMergersToString(ptaMergers),matrixType.name}; 
	}
	
	
	
	@Override
	public String getRowID() {
		return ptaName+"-"+ptaID;
	}

	@Override
	public String[] headerValuesForEachCell() 
	{
		return new String[]{"BCR","Diff","States"};
	}

}
