package statechum.analysis.learning.experiments.mutation;

import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.util.Map;
import java.util.TreeMap;


/** Represents results of test runs. */
public class ExperimentResult
{
	/**
	 * ID for serialization.
	 */
	private static final long serialVersionUID = -4710526839732193138L;

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
	
	public void record(XMLEncoder encoder)
	{
		encoder.writeObject(experimentValid);
		encoder.writeObject(D_varToValue);encoder.writeObject(L_varToValue);
	}

	public void load(XMLDecoder decoder)
	{
		experimentValid = ((Boolean)decoder.readObject()).booleanValue();
		D_varToValue = (Map<ExperimentResult.DOUBLE_V,Double>)decoder.readObject();
		L_varToValue = (Map<ExperimentResult.LONG_V,Long>)decoder.readObject();
	}

	
	public boolean experimentValid = false;

	@Override
	public String toString()
	{
		if (!experimentValid) return "invalid experiment";
		StringBuffer outcome = new StringBuffer(D_varToValue.size()+" double variables "+L_varToValue.size()+" long variables");
		for(DOUBLE_V v:DOUBLE_V.values())
			if (D_varToValue.containsKey(v))
			{
				outcome.append(v);outcome.append(" = ");outcome.append(D_varToValue.get(v));outcome.append('\n');
			}
		
		for(LONG_V v:LONG_V.values())
			if (L_varToValue.containsKey(v))
			{
				outcome.append(v);outcome.append(" = ");outcome.append(L_varToValue.get(v));outcome.append('\n');
			}
		
		return outcome.toString();
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

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((D_varToValue == null) ? 0 : D_varToValue.hashCode());
		result = prime * result
				+ ((L_varToValue == null) ? 0 : L_varToValue.hashCode());
		result = prime * result + (experimentValid ? 1231 : 1237);
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof ExperimentResult))
			return false;
		ExperimentResult other = (ExperimentResult) obj;
		if (D_varToValue == null) {
			if (other.D_varToValue != null)
				return false;
		} else if (!D_varToValue.equals(other.D_varToValue))
			return false;
		if (L_varToValue == null) {
			if (other.L_varToValue != null)
				return false;
		} else if (!L_varToValue.equals(other.L_varToValue))
			return false;
		if (experimentValid != other.experimentValid)
			return false;
		return true;
	}

}