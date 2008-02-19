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

package statechum.xmachine.model;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.utils.*;

public class JungGraphBuilder {

	private DirectedSparseGraph graph;

	private HashMap statesToNodes, transitionsToEdges;

	public DirectedSparseGraph constructGraph(XMachineModel model) {
		this.graph = new DirectedSparseGraph();
		addNodes(model.getStates());
		addEdges(model.getTransitions());
		removeOrphans();
		return graph;
	}

	private void addNodes(Collection states) {
		statesToNodes = new HashMap();
		Iterator stateIt = states.iterator();
		while (stateIt.hasNext()) {
			State state = (State) stateIt.next();
			DirectedSparseVertex dsv = new DirectedSparseVertex();
			dsv.addUserDatum("name", state.getLabel(), UserData.SHARED);
			statesToNodes.put(state.getLabel(), dsv);
			graph.addVertex(dsv);
		}
	}

	private void addEdges(HashSet transitions) {
		Iterator transitionIt = transitions.iterator();
		transitionsToEdges = new HashMap();
		while (transitionIt.hasNext()) {
			Transition trans = (Transition) transitionIt.next();
			DirectedSparseVertex from = (DirectedSparseVertex) statesToNodes
					.get(trans.getFromState().getLabel());
			DirectedSparseVertex to = (DirectedSparseVertex) statesToNodes
					.get(trans.getToState().getLabel());
			DirectedSparseEdge edge = new DirectedSparseEdge(from, to);
			if (trans.getName() != null) {
				edge.addUserDatum("name", trans.getName(), UserData.SHARED);
			}
			edge.addUserDatum("transition", trans, UserData.SHARED);
			transitionsToEdges.put(trans, edge);
			graph.addEdge(edge);
		}
	}

	private void removeOrphans() {
		Iterator vertexIt = graph.getVertices().iterator();
		HashSet vertexSet = new HashSet();
		while (vertexIt.hasNext()) {
			DirectedSparseVertex v = (DirectedSparseVertex) vertexIt.next();
			if (v.degree() == 0)
				vertexSet.add(v);
		}
		graph.removeVertices(vertexSet);
	}

}
