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

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;

import java.io.File;
import java.io.IOException;

public class GraphMLVisualiserTrimmed extends Visualiser {
	
	/**
	 * ID for serialisation
	 */
	private static final long serialVersionUID = 1735419773441272849L;

	public static void main(String[] args) throws IOException
	{
		//File graphDir = new File(args[0]);//new File(System.getProperty("user.dir")+System.getProperty("file.separator")+GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.RESOURCES)+
		//System.getProperty("file.separator")+"TestGraphs"+System.getProperty("file.separator") +args[0]);
		//String wholePath = graphDir.getAbsolutePath()+File.separator;
		GlobalConfiguration.getConfiguration().loadConfiguration();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.CLOSE_TERMINATE,"true");

		LearnerGraph graph0 = new LearnerGraph(Configuration.getDefaultConfiguration().copy());
		File file0 = new File(args[0]);AbstractPersistence.loadGraph(file0, graph0,null);graph0.setName(file0.getName());
		Visualiser.updateFrame(graph0.transform.trimGraph(6, graph0.getInit()),null);

//				convertToGraphWithoutStateNumbers(new LearnerGraphND(graph0)), null);
		
		Visualiser.waitForKey();
	}
	

}
