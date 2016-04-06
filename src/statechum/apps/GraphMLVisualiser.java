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
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;
import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.linear.GD;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;

public class GraphMLVisualiser extends Visualiser {
	
	/**
	 * ID for serialisation
	 */
	private static final long serialVersionUID = 1735419773441272849L;

	protected static DirectedSparseGraph convertToGraphWithoutStateNumbers(LearnerGraphND gr)
	{
		Map<String,String> labelling = new TreeMap<String,String>();
		for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entry:gr.transitionMatrix.entrySet())
			labelling.put(entry.getKey().toString(),Visualiser.extralabelToReplaceExisting+entry.getKey().getStringId());
		DirectedSparseGraph result = gr.pathroutines.getGraph();
		result.addUserDatum(JUConstants.VERTEX, labelling, UserData.SHARED);
		return result;
	}
	
	public static void main(String[] args) throws IOException
	{
		//File graphDir = new File(args[0]);//new File(System.getProperty("user.dir")+System.getProperty("file.separator")+GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.RESOURCES)+
		//System.getProperty("file.separator")+"TestGraphs"+System.getProperty("file.separator") +args[0]);
		//String wholePath = graphDir.getAbsolutePath()+File.separator;
		GlobalConfiguration.getConfiguration().loadConfiguration();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.CLOSE_TERMINATE,"true");

		LearnerGraphND graph0 = new LearnerGraphND(Configuration.getDefaultConfiguration().copy()),graph1 = null;
		File file0 = new File(args[0]);AbstractPersistence.loadGraph(file0, graph0,null);graph0.setName(file0.getName());
		if (args.length > 1)
		{
			graph1 = new LearnerGraphND(Configuration.getDefaultConfiguration().copy());
			File file1 = new File(args[1]);AbstractPersistence.loadGraph(file1, graph1,null);graph1.setName(file1.getName());
			GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
			DirectedSparseGraph gr = gd.showGD(
					graph0,graph1,
					ExperimentRunner.getCpuNumber());
			Visualiser.updateFrame(gr,null);
		}
		else
			Visualiser.updateFrame(convertToGraphWithoutStateNumbers(graph0), null);
		
		Visualiser.waitForKey();
	}
	

}
