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
import statechum.Configuration;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

public class EvaluationOfLearnersParameters implements ThreadResultID
{
	public enum LearningType
	{
		CONVENTIONAL("con"),PREMERGE("pre"),PREMERGEUNIQUE("preU"),PTAPREMERGE("PTApre"),CONSTRAINTS("conS"),CONVENTIONALUNIQUE("conU");
		public final String name;
		LearningType(String n)
		{
			name = n;
		}
		@Override
		public String toString()
		{
			return name;
		}
	}
	public final LearningType learningType;
	public boolean onlyUsePositives = false;
	public final boolean ptaMergers;
	public final ScoringToApply scoringMethod;
	public ScoringToApply secondScoringMethod;
	public final Configuration.ScoreMode scoringForEDSM;
	public final Configuration.STATETREE matrixType;
	public int states, perStateSquaredDensityMultipliedBy10,fsmSample, attempt, seed, traceQuantity, lengthmult;// here some of the parameters are not really used in row or column values because seed can be used to uniquely determine an FSM.
	public double alphabetMultiplier;
	public boolean pickUniqueFromInitial = false;
	
	
	public void setPickUniqueFromInitial(boolean value)
	{
		pickUniqueFromInitial = value;
	}
	
	public String uniqueAsString()
	{
		return pickUniqueFromInitial?"UNIQ":"ANY";
	}
	
	public void setOnlyUsePositives(boolean value)
	{
		onlyUsePositives = value;
	}
	
	public void setParameters(int argStates, double alphabetMult, int density10, int argFsmSample, int argAttempt, int argSeed, int traceQuantity, int lengthmult)
	{
		states = argStates;alphabetMultiplier = alphabetMult;perStateSquaredDensityMultipliedBy10 = density10;fsmSample = argFsmSample;attempt = argAttempt;seed = argSeed;this.traceQuantity = traceQuantity;this.lengthmult = lengthmult;
	}
	
	public EvaluationOfLearnersParameters(Configuration.ScoreMode scEDSM,ScoringToApply scoring, LearningType type,boolean pta, Configuration.STATETREE matrix)
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
		StringBuffer columnIDData = new StringBuffer();
		if (learningType != null) {
			columnIDData.append(learningType.name);columnIDData.append("-");
		}
		columnIDData.append(scoringForEDSM==null?"none":scoringForEDSM.name);

		columnIDData.append("-");columnIDData.append(scoringMethod.name);
		if (secondScoringMethod != null) {
			columnIDData.append(secondScoringMethod.name);columnIDData.append("-");
		}
		columnIDData.append("-");columnIDData.append(ptaMergersToString(ptaMergers));
		columnIDData.append("-");columnIDData.append(matrixType.name);
		columnIDData.append("-");columnIDData.append(traceQuantity);
		columnIDData.append("-");columnIDData.append(lengthmult);
		return columnIDData.toString();
	}

	@Override
	public String []getColumnText()
	{
		List<String> columnTextAsList = new LinkedList<>();
		if (scoringForEDSM !=null)
			columnTextAsList.add(scoringForEDSM.name);

		columnTextAsList.add(scoringMethod.name);
		if (secondScoringMethod != null)
			columnTextAsList.add(secondScoringMethod.name);

		columnTextAsList.addAll(Arrays.asList(new String[]{ptaMergersToString(ptaMergers),matrixType.name,Integer.toString(traceQuantity),Integer.toString(lengthmult)}));

		return columnTextAsList.toArray(new String[columnTextAsList.size()]);
	}

	@Override
	public String getRowID() {
		return states+"-"+alphabetMultiplier+"-"+uniqueAsString()+"-"+LearningSupportRoutines.padString(Integer.toString(fsmSample),'0',4)+"-"+LearningSupportRoutines.padString(Integer.toString(seed),'0',6)+"-D"+perStateSquaredDensityMultipliedBy10+"-A"+attempt;
	}

	@Override
	public String[] headerValuesForEachCell() 
	{
		return new String[]{"Success","BCR","Diff","M_Invalid","M_Missed","States","Time"};
	}

	@Override
	public String getSubExperimentName()
	{
		return "Evaluation of learners";
	}

	@Override
	public int executionTimeInCell() 
	{
		return 6;
	}
}
