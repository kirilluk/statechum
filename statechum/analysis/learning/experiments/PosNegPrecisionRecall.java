package statechum.analysis.learning.experiments;

import java.util.Collection;

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
	public PosNegPrecisionRecall(Collection retpos, Collection relpos, Collection retneg, Collection relneg){
		super();
		negprecision = computePrecision(retneg, relneg);
		posprecision = computePrecision(retpos, relpos);
		negrecall = computeRecall(retneg, relneg);
		posrecall = computeRecall(retpos, relpos);
		precision = (2*negprecision*posprecision)/(negprecision+posprecision);
		recall = (2*negrecall*posrecall)/(negrecall+posrecall);
		fMeasure = (2*precision*recall)/(precision+recall);
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
