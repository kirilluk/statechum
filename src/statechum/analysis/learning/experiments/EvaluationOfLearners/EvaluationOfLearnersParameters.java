package statechum.analysis.learning.experiments.EvaluationOfLearners;
import statechum.Configuration;
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
	public int states, fsmSample, attempt, seed, traceQuantity, lengthmult;
	
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
		return (learningType == null?"":learningType.name)+"-"+(scoringForEDSM==null?"none":scoringForEDSM.name)+"-"+scoringMethod.name+"-"+ptaMergersToString(ptaMergers)+"-"+matrixType.name; 
	}

	@Override
	public String []getColumnText()
	{
		return new String[]{ (scoringForEDSM==null?"":scoringForEDSM.name),scoringMethod.name,ptaMergersToString(ptaMergers),matrixType.name}; 
	}
	
	
	
	@Override
	public String getRowID() {
		return states+"-"+fsmSample+"-"+seed+"-A"+attempt;
	}

	@Override
	public String[] headerValuesForEachCell() 
	{
		return new String[]{"BCR","Diff","States"};
	}
}
