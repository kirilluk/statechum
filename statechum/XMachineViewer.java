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

package statechum;

import java.awt.*;
import javax.swing.JPanel;
import java.util.*;
import statechum.xmachine.model.*;
import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.decorators.*;
import edu.uci.ics.jung.visualization.*;
import edu.uci.ics.jung.visualization.contrib.*;
import edu.uci.ics.jung.visualization.control.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.algorithms.blockmodel.*;
import edu.uci.ics.jung.utils.*;

public class XMachineViewer {

	private DirectedSparseGraph graph;

	private JPanel panel;

	private VisualizationViewer v;

	private XMachineModel model;

	private boolean edges, vertices, collapsed;

	public XMachineViewer(XMachineModel machine, boolean vertices,
			boolean edges, boolean collapsed) {
		this.vertices = vertices;
		this.edges = edges;
		this.collapsed = collapsed;
		if (collapsed)
			setGraph(getCollapsedGraph(new JungGraphBuilder()
					.constructGraph(machine)));
		else
			setGraph(new JungGraphBuilder().constructGraph(machine));
		model = machine;
	}

	public boolean getEdges() {
		return edges;
	}

	public boolean getVertices() {
		return vertices;
	}

	public JPanel getPanel() {
		return panel;
	}

	private PluggableRenderer labelVertices(PluggableRenderer r) {
		StringLabeller labeller = StringLabeller.getLabeller(graph, "name");
		Iterator labelIt = graph.getVertices().iterator();
		while (labelIt.hasNext()) {
			Vertex v = (Vertex) labelIt.next();
			try {
				labeller.setLabel(v, v.getUserDatum("name").toString());
			} catch (Exception e) {
				System.out.println(e);
			}
		}
		r.setVertexStringer(labeller);
		return r;
	}

	private PluggableRenderer labelEdges(PluggableRenderer render) {
		EdgeStringer stringer = new EdgeStringer() {
			public String getLabel(ArchetypeEdge e) {
				if (e.containsUserDatumKey("name")) {
					String label = e.getUserDatum("name").toString();
					// if(label.length()>25)
					// label = label.substring(0,25);
					return label;
				} else
					return "";
			}
		};
		render.setEdgeStringer(stringer);
		return render;
	}

	public DirectedSparseGraph getCollapsedGraph(DirectedSparseGraph g) {
		this.collapsed = true;
		EquivalenceRelation er = constructClassNameEquivalenceRelation(g);
		DirectedSparseGraph collapsed = (DirectedSparseGraph) GraphCollapser
				.getInstance().getCollapsedGraph(er);
		labelClassVertices(collapsed);
		labelClassEdges(collapsed);
		this.edges = true;
		this.vertices = true;
		return collapsed;
	}

	private void labelClassVertices(DirectedSparseGraph g) {
		Iterator vertexIt = g.getVertices().iterator();
		while (vertexIt.hasNext()) {
			GraphCollapser.CollapsedSparseVertex csv = (GraphCollapser.CollapsedSparseVertex) vertexIt
					.next();
			HashSet rootSet = (HashSet) csv.getRootSet();
			DirectedSparseVertex dsv = (DirectedSparseVertex) rootSet
					.iterator().next();
			String label = dsv.getUserDatum("name").toString();
			if (label.length() > 1)
				label = label.substring(1, label.lastIndexOf('.') - 1);
			csv.addUserDatum("name", label, UserData.SHARED);
		}
	}

	private void labelClassEdges(DirectedSparseGraph g) {
		Iterator edgeIt = g.getEdges().iterator();
		while (edgeIt.hasNext()) {
			GraphCollapser.DirectedCollapsedEdge cse = (GraphCollapser.DirectedCollapsedEdge) edgeIt
					.next();
			HashSet rootSet = (HashSet) cse.getRelevantEdges();
			Iterator rootIt = rootSet.iterator();
			Transition t = new Transition();
			while (rootIt.hasNext()) {
				DirectedSparseEdge e = (DirectedSparseEdge) rootIt.next();
				Transition trans = (Transition) e.getUserDatum("transition");
				t.addFunctions(trans.getFunctions());
			}
			String edgeLabel = new Integer(t.getFunctions().keySet().size())
					.toString();
			cse.addUserDatum("name", edgeLabel, UserData.SHARED);
			cse.addUserDatum("transition", t, UserData.SHARED);
		}
	}

	private void setGraph(DirectedSparseGraph g) {
		graph = g;
		Layout l = new KKLayout(g);
		PluggableRenderer r = new PluggableRenderer();
		r.setEdgePaintFunction(new PickableEdgePaintFunction(r, Color.black,
				Color.cyan));
		if (vertices)
			r = labelVertices(r);
		if (edges)
			r = labelEdges(r);
		v = new VisualizationViewer(l, r, new Dimension(800, 600));
		v.setPickSupport(new ShapePickSupport());
		v.setBackground(Color.WHITE);
		final DefaultModalGraphMouse graphMouse = new DefaultModalGraphMouse();
		graphMouse.setMode(ModalGraphMouse.Mode.PICKING);
		v.setGraphMouse(graphMouse);
		final GraphZoomScrollPane panel = new GraphZoomScrollPane(v);
		this.panel = panel;
	}

	private EquivalenceRelation constructClassNameEquivalenceRelation(
			DirectedSparseGraph g) {
		HashMap classNamesToVertices = new HashMap();
		Iterator vertexIt = g.getVertices().iterator();
		while (vertexIt.hasNext()) {
			DirectedSparseVertex vertex = (DirectedSparseVertex) vertexIt
					.next();
			String label = (String) vertex.getUserDatum("name");
			String className;
			try {
				className = label.substring(1, label.lastIndexOf('.') - 1);
			} catch (Exception e) {
				className = label;
			}
			HashSet setOfVertices;
			if (classNamesToVertices.containsKey(className))
				setOfVertices = (HashSet) classNamesToVertices.get(className);
			else
				setOfVertices = new HashSet();
			setOfVertices.add(vertex);
			classNamesToVertices.put(className, setOfVertices);
		}
		HashSet setOfSets = new HashSet();
		setOfSets.addAll(classNamesToVertices.values());
		return new EquivalenceRelation(setOfSets, g);
	}

	public void setTransformable() {
		DefaultModalGraphMouse graphMouse = (DefaultModalGraphMouse) v
				.getGraphMouse();
		graphMouse.setMode(ModalGraphMouse.Mode.TRANSFORMING);
		v.repaint();
	}

	public void setPickable() {
		DefaultModalGraphMouse graphMouse = (DefaultModalGraphMouse) v
				.getGraphMouse();
		graphMouse.setMode(ModalGraphMouse.Mode.PICKING);
		v.repaint();
	}

	public Transition getPickedEdge() {
		Iterator edgeIt = graph.getEdges().iterator();
		PluggableRenderer currentRenderer = (PluggableRenderer) v.getRenderer();
		while (edgeIt.hasNext()) {
			DirectedEdge edge = (DirectedEdge) edgeIt.next();
			if (currentRenderer.isPicked(edge)) {
				Transition t = (Transition) edge.getUserDatum("transition");
				return t;
			}
		}
		return null;
	}

	public VisualizationViewer getViewer() {
		return v;
	}

	public XMachineModel getModel() {
		return model;
	}

	public boolean isCollapsed() {
		return collapsed;
	}

}
