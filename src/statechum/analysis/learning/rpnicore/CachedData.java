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

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;


import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;
import statechum.model.testset.PTASequenceEngine;

/** We'd like to cache a certain amount of data which will need to be rebuilt when the graph changes.
 * This includes rebuilding max score and flow graph, as well as routines used for
 * going linear.
 */
public class CachedData<TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
{
	final AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> coregraph;
	
	/** Associates this object to LinearGraph it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	CachedData(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> g)
	{
		coregraph =g;
	}

	/** The flowgraph representing the transition diagram (i.e. parallel 
	 * edges between every pair of states are collapsed into a single 
	 * edge associated with a set of labels of those edges).
	 */   
	private Map<CmpVertex,Map<CmpVertex,Set<Label>>> flowgraph = null;
	
	public Map<CmpVertex,Map<CmpVertex,Set<Label>>> getFlowgraph()
	{
		if (flowgraph == null) flowgraph = coregraph.pathroutines.getFlowgraph();
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

	/** The alphabet of the graph. 
	 */
	private Set<Label> alphabet = null;
	
	public Set<Label> getAlphabet()
	{
		if (alphabet == null)
			alphabet = coregraph.pathroutines.computeAlphabet();
		
		assert alphabet != null;
		return alphabet;
	}

	/** PTA of questions, so that I could walk through it and mark relevant nodes as visited,
	 * a subsequent call to <em>getData</em> would then return the revised collection of questions.
	 */
	PTASequenceEngine questionsPTA = null;
	
	public PTASequenceEngine getQuestionsPTA()
	{
		return questionsPTA;
	}
	
	public void invalidate()
	{
		flowgraph=null;stateToNumber = null;
		alphabet=null;questionsPTA = null;
	}

}
