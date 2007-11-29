package statechum.analysis.learning.experiments;

import java.util.*;

/**
 * Precision recall can be calculated from set of retrieved data (RET) and relevant data (REL)
 * @author nw
 *
 */

public class PrecisionRecall {
	
	private double precision, recall, fMeasure;

	public PrecisionRecall(Collection ret, Collection rel){
		precision = computePrecision(ret, rel);
		recall = computeRecall(ret,rel);
		fMeasure = (2*precision*recall)/(precision+recall);
	}
	
	public PrecisionRecall(Collection retpos, Collection relpos, Collection retneg, Collection relneg){
		double negprecision = computePrecision(retneg, relneg);
		double posprecision = computePrecision(retpos, relpos);
		double negrecall = computeRecall(retneg, relneg);
		double posrecall = computeRecall(retpos, relpos);
		precision = (negprecision/2)+(posprecision/2);
		recall = (negrecall/2)+(posrecall/2);
		fMeasure = (2*precision*recall)/(precision+recall);
	}
	
	private double computePrecision(Collection ret, Collection rel){
		Collection relret = new HashSet();
		relret.addAll(ret);
		relret.retainAll(rel);
		if(relret.isEmpty())
			return 0;
		else
			return (double)relret.size()/(double)ret.size();
	}
	
	private double computeRecall(Collection ret, Collection rel){
		Collection relret = new HashSet();
		relret.addAll(ret);
		relret.retainAll(rel);
		if(relret.isEmpty())
			return 0;
		else
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
