package statechum.analysis.learning.experiments;

import java.util.*;

/**
 * Precision recall can be calculated from set of retrieved data (RET) and relevant data (REL)
 * @author nw
 *
 */

public class PrecisionRecall {
	
	protected double precision, recall, fMeasure;
	
	public PrecisionRecall(){}

	public PrecisionRecall(Collection<List<String>> ret, Collection<List<String>> rel){
		precision = computePrecision(ret, rel);
		recall = computeRecall(ret,rel);
		fMeasure = (2*precision*recall)/(precision+recall);
	}
	
	
	
	protected double computePrecision(Collection<List<String>> ret, Collection<List<String>> rel){
		Collection<List<String>> relret = new HashSet<List<String>>();
		relret.addAll(ret);
		relret.retainAll(rel);
		if(relret.isEmpty())
			return 0;
		
		return (double)relret.size()/(double)ret.size();
	}
	
	protected double computeRecall(Collection<List<String>> ret, Collection<List<String>> rel){
		Collection<List<String>> relret = new HashSet<List<String>>();
		relret.addAll(ret);
		relret.retainAll(rel);
		if(relret.isEmpty())
			return 0;
		
		return (double)relret.size()/(double)rel.size();
	}

	public double getFMeasure() {
		return fMeasure;
	}

	public double getPrecision() {
		return precision;
	}

	public double getRecall() {
		return recall;
	}
	
}
