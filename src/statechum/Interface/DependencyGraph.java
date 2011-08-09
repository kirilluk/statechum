package statechum.Interface;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import statechum.JUConstants;
import statechum.Label;
import statechum.analysis.Erlang.ErlangModule;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class DependencyGraph extends statechum.analysis.learning.Visualiser {

	private static final long serialVersionUID = -7258812066189916399L;
	private Map<String, DirectedSparseVertex> nodeMap;
	private DirectedSparseGraph graph;

	private DirectedSparseVertex getVertex(String node) {
		DirectedSparseVertex v = nodeMap.get(node);
		if (v == null) {
			v = new DirectedSparseVertex();
			v.addUserDatum(JUConstants.LABEL, node, UserData.CLONE);
			v.addUserDatum(JUConstants.COLOUR, JUConstants.BLUE, UserData.SHARED);
			graph.addVertex(v);
			nodeMap.put(node, v);
		}
		return v;
	}

	public DependencyGraph(File folder) {
		System.out.println("Opening folder " + folder.getAbsolutePath());
		nodeMap = new HashMap<String, DirectedSparseVertex>();
		graph = new DirectedSparseGraph();
		for (File f : folder.listFiles()) {
			if (f.getName().endsWith(".erl")) {
				// load the module
				try {
					System.out.println("Loading " + f.getName());
					ErlangModule mod = ErlangModule.loadModule(f);
					DirectedSparseVertex myVertex = getVertex(mod.name);
					myVertex.setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
					
					for (String d : mod.behaviour.getDependencies()) {
						DirectedSparseVertex v = getVertex(d);
						DirectedSparseEdge e = new DirectedSparseEdge(myVertex, v);
						e.setUserDatum(JUConstants.LABEL, new HashSet<Label>(), UserData.CLONE);
						graph.addEdge(e);
					}
				} catch (IOException e) {
					System.out.println("\tFailed to open " + f.getName());
				} catch (Exception e) {
					e.printStackTrace();
					System.out.println("\tFailed to process " + f.getName()+ ", because of "+e.getMessage());
				}

			}
		}
		this.construct(graph, null);
		this.setLocation(10, 10);
		this.setSize(800, 600);

	}

	public static void main(String[] args) {
		if(args.length < 1) {
			return;
		}
		DependencyGraph dg = new DependencyGraph(new File(args[0]));
		dg.pack();
		dg.setVisible(true);
	}
	
}
