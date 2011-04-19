/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.analysis.learning.PrecisionRecall;

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
	public PosNegPrecisionRecall(Collection<List<String>> retpos, Collection<List<String>> relpos, 
			Collection<List<String>> retneg, Collection<List<String>> relneg){
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
		if(a==0||b==0){
			return (a+b)/2;
		}
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
