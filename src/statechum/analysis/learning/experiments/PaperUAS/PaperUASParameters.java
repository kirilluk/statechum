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
