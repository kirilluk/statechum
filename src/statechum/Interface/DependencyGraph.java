package statechum.Interface;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import statechum.Configuration;
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
		int succount = 0;
		int failcount = 0;
		System.out.println("Opening folder " + folder.getAbsolutePath());
		nodeMap = new HashMap<String, DirectedSparseVertex>();
		graph = new DirectedSparseGraph();
		for (File f : folder.listFiles()) {
			if (f.getName().endsWith(".erl")) {
				// load the module
				try {
					System.out.println("Loading " + f.getName());
					Configuration config = Configuration.getDefaultConfiguration().copy();ErlangModule.setupErlangConfiguration(config,f);
					ErlangModule mod = ErlangModule.loadModule(config);
					DirectedSparseVertex myVertex = getVertex(mod.name);
					myVertex.setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);

					for (String d : mod.behaviour.getDependencies()) {
						DirectedSparseVertex v = getVertex(d);
						DirectedSparseEdge e = new DirectedSparseEdge(myVertex, v);
						e.setUserDatum(JUConstants.LABEL, new HashSet<Label>(), UserData.CLONE);
						graph.addEdge(e);
					}
					System.out.println("\tDone.");
					succount += 1;
				} catch (IOException e) {
					System.out.println("\tFailed to open " + f.getName());
					succount += 1;
				} catch (Exception e) {
					failcount += 1;
					if (e.getMessage() != null) {
						//if (!e.getMessage().contains("behaviour")) {
							//e.printStackTrace();
						//}
					}
					System.out.println("\tFailed to process " + f.getName() + " - " + e.getMessage() + " <" + e.getClass().getName() + ">");
				}

			}
		}
		System.out.println("" + succount + " processed, " + failcount + " failed");
		/*
		Vertex root = null;
		for(Object v : graph.getVertices()) {
			if(((DirectedSparseVertex) v).getInEdges().size() == 0) {
				root = (Vertex) v;
				break;
			}
		}
		SparseTree tree = new SparseTree(root);
		for(Object v : graph.getVertices()) {
			if(v != root) {
				tree.addVertex((Vertex) v);
			}
			for(Object e : ((Vertex) v).getInEdges()) {
				tree.addEdge((Edge) e);
			}
			for(Object e : ((Vertex) v).getOutEdges()) {
				tree.addEdge((Edge) e);
			}
		}
		viewer.setGraphLayout(new TreeLayout(tree));
		this.construct(tree, null);
		*/
		this.construct(graph, null);
		
	}

	public static void main(String[] args) {
		if (args.length < 1) {
			return;
		}
		DependencyGraph dg = new DependencyGraph(new File(args[0]));
		dg.pack();
		dg.setVisible(true);
		dg.setLocation(10, 10);
		dg.setSize(800, 600);
		dg.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
		System.out.println("Done");
	}

}
