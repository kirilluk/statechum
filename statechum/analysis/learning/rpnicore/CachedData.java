/*
 * Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;

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
			stateToNumber = coregraph.wmethod.buildStateToIntegerMap(true,null);
		
		assert stateToNumber != null;
		return stateToNumber;
	}
	
	/** The map from vertices to the corresponding numbers, excluding reject-vertices. 
	 * Used for computation of state-similarity.
	 */
	private Map<CmpVertex,Integer> stateToNumberNoReject = null;
	
	/** An inverse map to the above, excluding reject-vertices. 
	 * Used for computation of state-similarity.
	 */
	private CmpVertex numberToStateNoReject[] = null;
	
	public Map<CmpVertex,Integer> getStateToNumberNoReject()
	{
		if (stateToNumberNoReject == null)
		{
			numberToStateNoReject = new CmpVertex[getAcceptStateNumber()];
			stateToNumberNoReject = coregraph.wmethod.buildStateToIntegerMap(false,numberToStateNoReject);
		}
		assert stateToNumberNoReject != null;
		return stateToNumberNoReject;
	}
	
	public CmpVertex [] getNumberToStateNoReject()
	{
		if (numberToStateNoReject == null)
			getStateToNumberNoReject();
		
		assert numberToStateNoReject != null;
		return numberToStateNoReject;
	}
	
	/** This transition matrix is very similar to the main one except
	 * that all transitions are pointing in the opposite direction. Due to
	 * nondeterminism this produces, target states are represented using lists.
	 * This matrix is used to scan the state comparison matrix columnwise.
	 */
	private Map<CmpVertex,Map<String,List<CmpVertex>>> sortaInverse = null;

	public Map<CmpVertex,Map<String,List<CmpVertex>>> getSortaInverse()
	{
		if (sortaInverse == null) prepareForLinear();
		
		assert sortaInverse != null;
		return sortaInverse;
	}

	/** Contains the number of accept-state in this graph. */
	private int acceptStateNumber = -1;
	
	public int getAcceptStateNumber()
	{
		if (acceptStateNumber < 0)
		{
			acceptStateNumber =0;
			for(CmpVertex vert:coregraph.transitionMatrix.keySet()) if (vert.isAccept()) ++acceptStateNumber;				
		}
		return acceptStateNumber;
	}
	
	/** Consider every state and a map from inputs to transitions leading into
	 * those states with those inputs (<em>sortaInverse</em>).
	 * For a pair of states (A,B), there may be some inputs for which both states
	 * have incoming transitions - this is an intersection
	 * <pre> 
	 * cmnInputs = sortaInverse.get(A).getKeys() INTERSECT sortaInverse.get(B).getKeys().
	 * </pre>
	 * If we consider the number of incoming states in 
	 * <pre>
	 * vertexToInt(getIntsortaInverse.get(cmnInputs).getValues(),getIntsortaInverse.get(cmnInputs).getValues())
	 * </pre>
	 * then these states (plus perhaps one, for a diagonal element) should be included
	 * in a row submitted to UMFPACK.
	 * We aim to estimate this number in order not to reallocate a target array
	 * many times.
	 */
	private int expectedIncomingPerPairOfStates = -1;

	public int getExpectedIncomingPerPairOfStates()
	{
		if (expectedIncomingPerPairOfStates < 0) prepareForLinear();
		return expectedIncomingPerPairOfStates;
	}
	
	/** Updates the cached data with a transition matrix where all transitions point in 
	 * an opposite direction to the current one. The matrix produced is used to scan 
	 * the state comparison matrix columnwise.
	 */
	public void prepareForLinear()
	{
		Map<CmpVertex,Map<String,List<CmpVertex>>> mapSortaInverse = new TreeMap<CmpVertex,Map<String,List<CmpVertex>>>();
		
		// First, we fill the map with empty entries - 
		// it is crucially important to fill in all the entries which can be accessed during the triangular exploration, 
		// otherwise holes will lead to the sequence of numbers explored to be discontinuous, causing a failure.
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
			if (entry.getKey().isAccept())
				mapSortaInverse.put(entry.getKey(),new TreeMap<String,List<CmpVertex>>());
		
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
			if (entry.getKey().isAccept())
			{
				for(Entry<String,CmpVertex> transition:entry.getValue().entrySet())
					if (transition.getValue().isAccept())
					{
						Map<String,List<CmpVertex>> row = mapSortaInverse.get(transition.getValue());
						List<CmpVertex> sourceStates = row.get(transition.getKey());
						if (sourceStates == null)
						{
							sourceStates=new LinkedList<CmpVertex>();row.put(transition.getKey(), sourceStates);
						}
						sourceStates.add(entry.getKey());
					}
			}
		
		sortaInverse = mapSortaInverse;
		
		// Now we need to estimate expectedIncomingPerPairOfStates
		int indegreeSum=0, incomingCnt = 0, maxInDegree = -1;
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:mapSortaInverse.entrySet())
			for(Entry<String,List<CmpVertex>> transition:entry.getValue().entrySet())
			{
				++incomingCnt;
				int size = transition.getValue().size()*entry.getValue().size();indegreeSum+=size;
				if (size > maxInDegree) maxInDegree=size;
			}
		
		// The logic is simple: if maxInDegree is much higher than the 
		// average, double the average indegree, otherwise leave it unchanged.  
		expectedIncomingPerPairOfStates = 2;
		if (incomingCnt > 0) expectedIncomingPerPairOfStates=1+indegreeSum/incomingCnt;// 1 is to account for a diagonal
	}

	/** The maximal score which can be returned by score computation routines. 
	 * Not set if negative, to rebuild do the following:
	 * <pre>
	 * if (learnerCache.maxScore < 0) learnerCache.maxScore = transitionMatrix.size()*wmethod.computeAlphabet().size();
	 * </pre>
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
	
	public void invalidate()
	{
		flowgraph=null;maxScore=-1;stateToNumber = null;stateToNumberNoReject=null;numberToStateNoReject=null;
		sortaInverse = null;expectedIncomingPerPairOfStates = -1;alphabet=null;acceptStateNumber=-1;
	}
}
