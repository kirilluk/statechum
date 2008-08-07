/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

statechum is free software: you can redistribute it and/or modify
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
package statechum.analysis.learning.rpnicore;

import java.util.Collection;
import java.util.LinkedList;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.StatePair;

/**
 * @author kirill
 *
 */
public class GD {
	final LearnerGraph coregraph;
	
	/** Associates this object to ComputeStateScores it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	GD(LearnerGraph computeStateScores) 
	{
		coregraph = computeStateScores;
	}

	//protected CmpVertex initA, initB;
	
	/** Compares the supplied two graphs.
	 * 
	 * @param a first graph
	 * @param b second graph
	 */
	public static void computeGD(LearnerGraph a,LearnerGraph b)
	{
		LearnerGraph grCombined = new LearnerGraph((Configuration)a.config.clone());
		CmpVertex initA = Transform.addToGraph(grCombined, a);
		CmpVertex initB = Transform.addToGraph(grCombined, b);
		grCombined.learnerCache.invalidate();
		
	}
	
	/** Key pairs are have unique left and right parts
	
	/** The main problem is non-uniqueness of CmpVertices used on two machines.
	* For this reason, I have to use two arrays for holding
	* currently found key pairs, left to right and right to left.
	*/
}	
	
