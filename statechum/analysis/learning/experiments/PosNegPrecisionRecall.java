package statechum.analysis.learning.experiments;

import java.util.Collection;
import java.util.List;

public class PosNegPrecisionRecall extends PrecisionRecall {
	
	protected double negprecision, posprecision, negrecall, posrecall;
	
	/**
	 * true precision and recall is computed as the harmonic mean of positive and negative precision
	 * and recall values.
	 * @param retpos
	 * @param relpos
	 * @param retneg
	 * @param relneg
	 */
	public PosNegPrecisionRecall(Collection<List<String>> retpos, Collection<List<String>> relpos, Collection<List<String>> retneg, Collection<List<String>> relneg){
		super();
		negprecision = computePrecision(retneg, relneg);
		posprecision = computePrecision(retpos, relpos);
		negrecall = computeRecall(retneg, relneg);
		posrecall = computeRecall(retpos, relpos);
		precision = computeMean(negprecision, posprecision);
		recall = computeMean(posrecall, negrecall);
		fMeasure = computeMean(precision, recall);
	}
	
	/*
	 * Computes the weighted harmonic mean of a and b, unless a or b are zero, when it simply
	 * computes the arithmetic mean instead.
	 */
	protected double computeMean(double a, double b){
		if(a==0||b==0)
			return (a+b)/2;
		
		return ((2*a*b)/(a+b));
	}

	public double getNegprecision() {
		return negprecision;
	}

	public double getNegrecall() {
		return negrecall;
	}

	public double getPosprecision() {
		return posprecision;
	}

	public double getPosrecall() {
		return posrecall;
	}
	
	

}
