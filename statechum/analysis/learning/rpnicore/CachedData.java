/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * StateChum. If not, see <http://www.gnu.org/licenses/>.
 */ 

package statechum.analysis.learning.rpnicore;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;


import statechum.DeterministicDirectedSparseGraph.CmpVertex;

/** We'd like to cache a certain amount of data which will need to be rebuilt when the graph changes.
 * This includes rebuilding max score and flow graph, as well as routines used for
 * going linear.
 */
public class CachedData {
	final LearnerGraph coregraph;
	
	/** Associates this object to LinearGraph it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	CachedData(LearnerGraph g)
	{
		coregraph =g;
	}

	/** The flowgraph representing the transition diagram (i.e. parallel 
	 * edges between every pair of states are collapsed into a single 
	 * edge associated with a set of labels of those edges).
	 */   
	private Map<CmpVertex,Map<CmpVertex,Set<String>>> flowgraph = null;
	
	public Map<CmpVertex,Map<CmpVertex,Set<String>>> getFlowgraph()
	{
		if (flowgraph == null) flowgraph = coregraph.paths.getFlowgraph();
		return flowgraph;
	}
	
	/** The map from vertices to the corresponding numbers. Used for space-saving 
	 * computation of W method.
	 */
	private Map<CmpVertex,Integer> stateToNumber = null;
	
	public Map<CmpVertex,Integer> getStateToNumber()
	{
		if (stateToNumber == null)
		{
			stateToNumber = new TreeMap<CmpVertex,Integer>();
			coregraph.buildStateToIntegerMap(null,stateToNumber);
		}
		assert stateToNumber != null;
		return stateToNumber;
	}

	/** The maximal score which can be returned by score computation routines. 
	 */
	protected int maxScore = -1;

	/** The alphabet of the graph. 
	 */
	private Set<String> alphabet = null;
	
	public Set<String> getAlphabet()
	{
		if (alphabet == null)
			alphabet = coregraph.wmethod.computeAlphabet();
		
		assert alphabet != null;
		return alphabet;
	}
	
	/** After merging using mergeAndDeterminize_general,
	 * this variable stores equivalence classes. Used by the pluggable
	 * question generator.
	 */ 
	protected Collection<AMEquivalenceClass> mergedStates = null;
	
	public Collection<AMEquivalenceClass> getMergedStates()
	{
		return mergedStates;
	}
	
	public void setMergedStates(Collection<AMEquivalenceClass> eqClasses)
	{
		mergedStates = eqClasses;
	}
	
	public void invalidate()
	{
		flowgraph=null;maxScore=-1;stateToNumber = null;
		alphabet=null;
		mergedStates = null;
	}
}
