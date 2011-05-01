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

package statechum.apps;

import java.io.File;

import statechum.Configuration;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.ExperimentGraphMLHandler;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.io.GraphMLFile;

public class GraphMLVisualiser extends Visualiser {
	
	/**
	 * ID for serialisation
	 */
	private static final long serialVersionUID = 1735419773441272849L;

	public static void main(String[] args){
		File graphDir = new File(args[0]);//new File(System.getProperty("user.dir")+System.getProperty("file.separator")+"resources"+
		//System.getProperty("file.separator")+"TestGraphs"+System.getProperty("file.separator") +args[0]);
		String wholePath = graphDir.getAbsolutePath()+File.separator;
		GraphMLFile graphmlFile = new GraphMLFile();
		graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler(Configuration.getDefaultConfiguration()));
		DirectedSparseGraph dg = new DirectedSparseGraph();
		dg.getEdgeConstraints().clear();
		dg = (DirectedSparseGraph)graphmlFile.load(wholePath+args[1]);
		GraphMLVisualiser gmlVis = new GraphMLVisualiser();
		gmlVis.construct(dg,null);
	}

}
