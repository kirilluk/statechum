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

public class EvaluationOfLearnersParameters implements ThreadResultID
{
	public static enum LearningType
	{
		CONVENTIONAL("con"),PREMERGE("pre"),PTAPREMERGE("PTApre"),CONSTRAINTS("conS"),CONVENTIONALUNIQUE("conU");
		public final String name;
		private LearningType(String n)
		{
			name = n;
		}
	}
	public final LearningType learningType;
	public boolean onlyUsePositives = false;
	public final boolean ptaMergers;
	public final ScoringToApply scoringMethod;
	public final Configuration.ScoreMode scoringForEDSM;
	public final Configuration.STATETREE matrixType;
	public int states, fsmSample, attempt, seed, traceQuantity, lengthmult;// here some of the parameters are not really used in row or column values because seed can be used to uniquely determine an FSM.
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
	
	public void setParameters(int argStates, int argFsmSample, int argAttempt, int argSeed, int traceQuantity, int lengthmult)
	{
		states = argStates;fsmSample = argFsmSample;attempt = argAttempt;seed = argSeed;this.traceQuantity = traceQuantity;this.lengthmult = lengthmult;
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
		return (learningType == null?"":(learningType.name+"-"))+(scoringForEDSM==null?"none":scoringForEDSM.name)+"-"+scoringMethod.name+"-"+ptaMergersToString(ptaMergers)+"-"+matrixType.name+"-"+traceQuantity+"-"+lengthmult; 
	}

	@Override
	public String []getColumnText()
	{
		return new String[]{ (scoringForEDSM==null?"":scoringForEDSM.name),scoringMethod.name,ptaMergersToString(ptaMergers),matrixType.name,Integer.toString(traceQuantity),Integer.toString(lengthmult)}; 
	}

	@Override
	public String getRowID() {
		return states+"-"+uniqueAsString()+"-"+LearningSupportRoutines.padString(Integer.toString(fsmSample),'0',4)+"-"+LearningSupportRoutines.padString(Integer.toString(seed),'0',6)+"-A"+attempt;
	}

	@Override
	public String[] headerValuesForEachCell() 
	{
		return new String[]{"BCR","Diff","States"};
	}

	@Override
	public String getSubExperimentName()
	{
		return "Evaluation of learners";
	}
}
