package statechum.analysis.learning.experiments.mutation;

import java.util.Map;
import java.util.TreeMap;

/** Represents results of test runs. */
public class ExperimentResult
{
	/** These two enums are different to ensure typechecking by the compiler. */
	public enum DOUBLE_V { MISMATCHED_KEYPAIRS, OBTAINED_TO_EXPECTED, MUTATIONS_TO_TRANSITIONS, ACCURACY_W, ACCURACY_RAND };
	public enum LONG_V { DURATION_GD, DURATION_RAND, DURATION_W };

	protected Map<ExperimentResult.DOUBLE_V,Double> D_varToValue = new TreeMap<ExperimentResult.DOUBLE_V,Double>();
	protected Map<ExperimentResult.LONG_V,Long> L_varToValue = new TreeMap<ExperimentResult.LONG_V,Long>();
	
	public double getValue(ExperimentResult.DOUBLE_V name)
	{
		Double result = D_varToValue.get(name);
		if (result == null) throw new IllegalArgumentException("double variable "+name+" has not previously been assigned");
		return result.doubleValue();
	}
	
	public long getValue(ExperimentResult.LONG_V name)
	{
		Long result = L_varToValue.get(name);
		if (result == null) throw new IllegalArgumentException("long variable "+name+" has not previously been assigned");
		return result.longValue();
	}
	
	public void setValue(ExperimentResult.DOUBLE_V name,double value)
	{
		D_varToValue.put(name,value);
	}
	
	public void setValue(ExperimentResult.LONG_V name,long value)
	{
		L_varToValue.put(name,value);
	}
	
	public boolean experimentValid = false;

	@Override
	public String toString()
	{
		if (!experimentValid) return "invalid experiment";
		return D_varToValue.size()+" double variables "+L_varToValue.size()+" long variables";
	}
	
	public void add(ExperimentResult exp) 
	{
		if (!experimentValid || !exp.experimentValid) throw new IllegalArgumentException("invalid experiment");
		for(ExperimentResult.DOUBLE_V varName:DOUBLE_V.values())
		{
			if (!exp.D_varToValue.containsKey(varName) && D_varToValue.containsKey(varName)) throw new IllegalArgumentException("exp missing value for "+varName);
			if (exp.D_varToValue.containsKey(varName) && !D_varToValue.containsKey(varName)) throw new IllegalArgumentException("result missing value for "+varName);
			if (exp.D_varToValue.containsKey(varName) && D_varToValue.containsKey(varName))
				setValue(varName,getValue(varName)+exp.getValue(varName));
		}
		for(ExperimentResult.LONG_V varName:LONG_V.values())
		{
			if (!exp.L_varToValue.containsKey(varName) && L_varToValue.containsKey(varName)) throw new IllegalArgumentException("exp missing value for "+varName);
			if (exp.L_varToValue.containsKey(varName) && !L_varToValue.containsKey(varName)) throw new IllegalArgumentException("result missing value for "+varName);
			if (exp.L_varToValue.containsKey(varName) && L_varToValue.containsKey(varName))
				setValue(varName,getValue(varName)+exp.getValue(varName));
		}
	}
	
	public ExperimentResult divide(int count) 
	{
		if (count <= 0) throw new IllegalArgumentException("cannot average over "+count+" experiments");
		if (!experimentValid) throw new IllegalArgumentException("invalid experiment");

		ExperimentResult result = new ExperimentResult();result.experimentValid = true;
		for(ExperimentResult.DOUBLE_V varName:D_varToValue.keySet())
			result.setValue(varName,getValue(varName)/count);
		for(ExperimentResult.LONG_V varName:L_varToValue.keySet())
			result.setValue(varName,getValue(varName)/count);
		return result;
	}
}