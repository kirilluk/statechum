package statechum.analysis.learning;

import java.io.File;

import statechum.analysis.learning.experiments.ExperimentGraphMLHandler;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.io.GraphMLFile;

public class GraphMLVisualiser extends Visualiser {
	
	public static void main(String[] args){
		File graphDir = new File(args[0]);//new File(System.getProperty("user.dir")+System.getProperty("file.separator")+"resources"+
		//System.getProperty("file.separator")+"TestGraphs"+System.getProperty("file.separator") +args[0]);
		String wholePath = graphDir.getAbsolutePath()+System.getProperty("file.separator");
		GraphMLFile graphmlFile = new GraphMLFile();
		graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
		DirectedSparseGraph dg = new DirectedSparseGraph();
		dg.getEdgeConstraints().clear();
		dg = (DirectedSparseGraph)graphmlFile.load(wholePath+args[1]);
		GraphMLVisualiser gmlVis = new GraphMLVisualiser();
		gmlVis.construct(dg);
	}

}
