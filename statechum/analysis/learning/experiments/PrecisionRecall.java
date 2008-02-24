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

	public PrecisionRecall(Collection ret, Collection rel){
		precision = computePrecision(ret, rel);
		recall = computeRecall(ret,rel);
		fMeasure = (2*precision*recall)/(precision+recall);
	}
	
	
	
	protected double computePrecision(Collection ret, Collection rel){
		Collection relret = new HashSet();
		relret.addAll(ret);
		relret.retainAll(rel);
		if(relret.isEmpty())
			return 0;
		else
			return (double)relret.size()/(double)ret.size();
	}
	
	protected double computeRecall(Collection ret, Collection rel){
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
