package statechum.Interface;

import java.io.File;
import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;


import statechum.DeterministicDirectedSparseGraph.DeterministicEdge;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.JUConstants;
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.Erlang.ErlangRunner;
import statechum.analysis.Erlang.ErlangRunner.ERL;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;

public class DependencyGraph  {
	/** Used to serialise the coordinates. */
	public static final int graphWindowNumber = 400;

	/** Every graph has a size associated with it, this one contains the one for the next graph to be created. */
	protected static int currentWindowNumber = graphWindowNumber;

	private static final long serialVersionUID = -7258812066189916399L;
	private DirectedSparseGraph dependencyTree = new DirectedSparseGraph();
	private Map<String,DeterministicVertex> nameToVertex = new TreeMap<String,DeterministicVertex>();

	private DeterministicVertex getVertexForModule(String modName) {
		DeterministicVertex v = nameToVertex.get(modName);
		if (v == null) {
			v = new DeterministicVertex(modName);
			v.setColour(JUConstants.BLUE);
			dependencyTree.addVertex(v);
			nameToVertex.put(modName, v);
		}
		return v;
	}
	
	private final DynamicVisualiser viz;
	
	public DependencyGraph()
	{
		viz = new DynamicVisualiser(currentWindowNumber++);
	}
	
	public void load(File folder) 
	{
		System.out.println("Opening folder " + folder.getAbsolutePath());
		dependencyTree.setUserDatum(JUConstants.TITLE,folder.getAbsolutePath().replace(File.separatorChar, '_').replace(':','_'),UserData.SHARED);
		viz.update(null, dependencyTree);
		for (File f : folder.listFiles()) {
			if (f.canRead() && ErlangRunner.validName(f.getName())) {
				// load the module
				String modName = ErlangRunner.getName(f,ERL.MOD);
				
				try {
					System.out.print("Loading " + f.getName()+" ");
					DeterministicVertex moduleVertex = null;
					viz.suspend();
					synchronized (AbstractLearnerGraph.syncObj) {
						moduleVertex = getVertexForModule(modName);
						moduleVertex.setColour(JUConstants.RED);
					}
					viz.resume();
					ErlangModule mod = ErlangModule.loadModule(f);
					
					synchronized (AbstractLearnerGraph.syncObj) {
						viz.suspend();
						for (String d : mod.behaviour.getDependencies()) 
						{
							DeterministicVertex target = getVertexForModule(d);
							DeterministicEdge edge = new DeterministicEdge(moduleVertex, target);
							edge.setUserDatum(JUConstants.LABEL, Collections.EMPTY_SET, UserData.SHARED);
							dependencyTree.addEdge(edge);
						}
						viz.resume();
					}
					System.out.println("done");
				} catch (Exception e) {
					System.out.println("\tfailed because of "+e.getMessage());
					if (e.getMessage() == null || e.getMessage().contains("unimplemented"))
						e.printStackTrace();
				}

			}
		}
	}

	private static class DynamicVisualiser extends Visualiser
	{
		/**
		 * ID for serialization
		 */
		private static final long serialVersionUID = 361251959433005045L;
		
		public DynamicVisualiser(int id)
		{
			super(id);
		}

		public void suspend()
		{
			if (viewer != null)
				viewer.getModel().suspend();
		}

		public void resume()
		{
			if (viewer != null)
				viewer.getModel().restart();
		}
	}
	
	public static void main(final String[] args) {
		if(args.length < 1) {
			System.out.println("Usage: DependencyGraph <directory with erl files>");
			return;
		}
		
		Thread worker = new Thread(new Runnable() {

			@Override
			public void run() {
				new DependencyGraph().load(new File(args[0]));
			}});
		worker.setPriority(Thread.MIN_PRIORITY);
		worker.start();
	}
	
}
