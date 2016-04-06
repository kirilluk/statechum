/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
 * 
 * statechum is free software: you can redistribute it and/or modify
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
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.Label;
import statechum.Pair;

/**
 * @author Kirill
 *
 */
public class SootOracleSupport {
	final LearnerGraph coregraph;
	final Label ret;
	final ConvertALabel converter = null;
	
	/** Associates this object to SootOracleSupport it is using for data to operate on. 
	 * Important: the constructor should not access any data in SootOracleSupport 
	 * because it is usually invoked during the construction phase of SootOracleSupport 
	 * when no data is yet available.
	 */
	SootOracleSupport(LearnerGraph g)
	{
		coregraph =g;ret = AbstractLearnerGraph.generateNewLabel("ret",coregraph.config,converter);
	}
	
	/**
	 * Augment every occurrence of the first label in the pair in the PTA
	 * with an edge carrying the second label in the pair, that is either accepted or not
	 */
	public void augmentPairs(Pair<Label,Label> pair, boolean accepted){
		Collection<CmpVertex> fromVertices = findVertices(pair.firstElem);
		for (CmpVertex vertex : fromVertices) {
			Collection<List<Label>> tails = getTails(vertex, new LinkedList<Label>(), new HashSet<List<Label>>());
			for (List<Label> list : tails) {
				addNegativeEdges(vertex, list, pair.secondElem, accepted);
			}
		}
	}
	
	private void addNegativeEdges(CmpVertex fromVertex,List<Label> tail, Label label, boolean accepted){
		Stack<Label> callStack = new Stack<Label>();
		coregraph.addVertex(fromVertex, accepted, label);
		CmpVertex currentVertex = fromVertex;
		for(int i=0;i<tail.size();i++){
			Label element = tail.get(i);
			currentVertex = coregraph.transitionMatrix.get(currentVertex).get(element);
			if(element.equals(ret)&&!callStack.isEmpty()){
				callStack.pop();
				if(callStack.isEmpty())
					coregraph.addVertex(currentVertex, accepted, label);
			}
			else if (!element.equals(ret))
				callStack.push(element);
			else if (element.equals(ret)&&callStack.isEmpty())
				return;
		}
	}
	
	private Collection<List<Label>> getTails(CmpVertex vertex, LinkedList<Label> currentList, Collection<List<Label>> collection){
		Map<Label,CmpVertex> successors = coregraph.transitionMatrix.get(vertex);
		if(successors.isEmpty()){
			collection.add(currentList);
			return collection;
		}

		Iterator<Label> keyIt = successors.keySet().iterator();
		while(keyIt.hasNext()){
			Label key = keyIt.next();
			currentList.add(key);
			collection.addAll(getTails(successors.get(key),currentList,collection));
		}
		return collection;
	}
	
	/**
	 *returns set of vertices that are the destination of label
	 */
	private Collection<CmpVertex> findVertices(Label label)
	{
		Collection<CmpVertex> vertices = new HashSet<CmpVertex>();
		Iterator<Map<Label, CmpVertex>> outgoingEdgesIt = coregraph.transitionMatrix.values().iterator();
		while(outgoingEdgesIt.hasNext()){
			Map<Label,CmpVertex> edges = outgoingEdgesIt.next();
			if(edges.keySet().contains(label))
				vertices.add(edges.get(label));
		}
		return vertices;
	}

}
