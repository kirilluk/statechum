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

import java.util.Map;

import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;

/**
 * Some vertices are used to denote infinite graphs,
 * the class is responsible for handling of such vertices.
 * 
 * @author kirill
 *
 */
public class InfVertexHelper {
	public static final String replSeparator = "@";
	
	/** Given a vertex, returns an ID of the "real" vertex corresponding to it,
	 * which is the id of the passed vertex for normal vertices and the "original" ID 
	 * one for the infinite one.
	 * 
	 * @param vert vertex to consider.
	 * @return the original one.
	 */
	public static VertexID getOrigVertexID(CmpVertex vert)
	{
		VertexID result = vert.getID();
		if (vert.getColour() == JUConstants.INF_AMBER)
		{
			String idString = vert.getID().toString();
			if (idString.contains(replSeparator)) // if there is no separator, this is the vertex added in the first place to the graph.
				result = new VertexID(idString.substring(0, idString.indexOf(replSeparator)));
		}
		return result;
	}
	
	/** Given an ID, returns a corresponding ID with a new instance number. 
	 * 
	 * @param vert vertex to consider.
	 * @param graph graph to consider.
	 * @return new vertex
	 */
	public static CmpVertex makeNewID(CmpVertex vert, LearnerGraph graph)
	{
		assert vert.getColour() == JUConstants.INF_AMBER;
		CmpVertex newVertex = AbstractLearnerGraph.generateNewCmpVertex(
				new VertexID(getOrigVertexID(vert).getStringId()+replSeparator+graph.nextID(vert.isAccept()).getStringId()),graph.config);
		newVertex.setAccept(vert.isAccept());newVertex.setHighlight(vert.isHighlight());newVertex.setColour(vert.getColour());
		return newVertex;
	}
	
	/** Given the tentative target state, this method returns the appropriate state,
	 * by adding a replica of an infinite state if needed.
	 * @param vertex source state
	 * @param transitions transitions from this state
	 * @param input to consider
	 * @param graph graph to add new state to
	 * @return resulting state.
	 */
	public static CmpVertex nextState(@SuppressWarnings("unused") CmpVertex vertex,Map<String,CmpVertex> transitions, String input,LearnerGraph graph)
	{
		CmpVertex target = transitions.get(input);
		if (target != null && target.getColour() == JUConstants.INF_AMBER)
		{// infinite vertex
			target=makeNewID(transitions.get(input), graph);
			transitions.put(input, target);graph.transitionMatrix.put(target, graph.createNewRow());
		}
		return target;
	}
	
}
