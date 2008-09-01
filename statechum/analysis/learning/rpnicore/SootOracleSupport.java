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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.oracles.StringPair;

/**
 * @author Kirill
 *
 */
public class SootOracleSupport {
	final LearnerGraph coregraph;
	
	/** Associates this object to SootOracleSupport it is using for data to operate on. 
	 * Important: the constructor should not access any data in SootOracleSupport 
	 * because it is usually invoked during the construction phase of SootOracleSupport 
	 * when no data is yet available.
	 */
	SootOracleSupport(LearnerGraph g)
	{
		coregraph =g;
	}
	
	/**
	 * Augment every occurrence of the first label in the pair in the PTA
	 * with an edge to the second label in the pair, that is either accepted or not
	 */
	public void augmentPairs(StringPair pair, boolean accepted){
		Collection<CmpVertex> fromVertices = findVertices(pair.getFrom());
		for (CmpVertex vertex : fromVertices) {
			Collection<List<String>> tails = getTails(vertex, new ArrayList<String>(), new HashSet<List<String>>());
			for (List<String> list : tails) {
				addNegativeEdges(vertex, list, pair, accepted);
			}
		}
	}
	
	private void addNegativeEdges(CmpVertex fromVertex,List<String> tail, StringPair pair, boolean accepted){
		Stack<String> callStack = new Stack<String>();
		coregraph.addVertex(fromVertex, accepted, pair.getTo());
		CmpVertex currentVertex = fromVertex;
		for(int i=0;i<tail.size();i++){
			String element = tail.get(i);
			currentVertex = coregraph.transitionMatrix.get(currentVertex).get(element);
			if(element.equals("ret")&&!callStack.isEmpty()){
				callStack.pop();
				if(callStack.isEmpty())
					coregraph.addVertex(currentVertex, accepted, pair.getTo());
			}
			else if (!element.equals("ret"))
				callStack.push(element);
			else if (element.equals("ret")&&callStack.isEmpty())
				return;
		}
	}
	
	private Collection<List<String>> getTails(CmpVertex vertex, ArrayList<String> currentList, Collection<List<String>> collection){
		Map<String,CmpVertex> successors = coregraph.transitionMatrix.get(vertex);
		if(successors.isEmpty()){
			collection.add(currentList);
			return collection;
		}

		Iterator<String> keyIt = successors.keySet().iterator();
		while(keyIt.hasNext()){
			String key = keyIt.next();
			currentList.add(key);
			collection.addAll(getTails(successors.get(key),currentList,collection));
		}
		return collection;
	}
	
	/**
	 *returns set of vertices that are the destination of label
	 */
	private Collection<CmpVertex> findVertices(String label)
	{
		Collection<CmpVertex> vertices = new HashSet<CmpVertex>();
		Iterator<Map<String, CmpVertex>> outgoingEdgesIt = coregraph.transitionMatrix.values().iterator();
		while(outgoingEdgesIt.hasNext()){
			Map<String,CmpVertex> edges = outgoingEdgesIt.next();
			if(edges.keySet().contains(label))
				vertices.add(edges.get(label));
		}
		return vertices;
	}

}
