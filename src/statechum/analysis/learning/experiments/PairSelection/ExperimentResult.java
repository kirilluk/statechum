package statechum.analysis.learning.experiments.PairSelection;

import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResult;

public class ExperimentResult<EXPERIMENT_PARAMETERS> extends ThreadResult {
	public final EXPERIMENT_PARAMETERS parameters;
	
	public ExperimentResult(EXPERIMENT_PARAMETERS p)
	{
		parameters = p;
	}
}
