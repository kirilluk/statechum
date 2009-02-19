/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */

package statechum.analysis.learning.rpnicore;

import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;

/** An extension of the cache with elements to support learning. */ 
public class LearnerGraphCachedData extends CachedData<CmpVertex,LearnerGraphCachedData>
{
	LearnerGraphCachedData(LearnerGraph g) {
		super(g);
	}

	/** After merging using mergeAndDeterminize_general,
	 * this variable stores equivalence classes. Used by the pluggable
	 * question generator.
	 */ 
	protected Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedStates = null;
	
	public Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> getMergedStates()
	{
		return mergedStates;
	}
	
	public void setMergedStates(Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> eqClasses)
	{
		mergedStates = eqClasses;
	}
	
	/** The maximal score which can be returned by score computation routines. 
	 */
	protected int maxScore = -1;
	
	/** The state corresponding to the red and blue states after the merge of which this graph was built. */
	protected CmpVertex stateLearnt = null;
	
	@Override
	public void invalidate()
	{
		super.invalidate();
		mergedStates = null;maxScore=-1;stateLearnt = null;vertexToEqClass = null;
	}
	
	/** A map from merged vertices to collections of original vertices they correspond to.
	 */
	Map<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> vertexToEqClass = null; 

	public Map<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> getVertexToEqClass()
	{
		return vertexToEqClass;
	}
	
}
