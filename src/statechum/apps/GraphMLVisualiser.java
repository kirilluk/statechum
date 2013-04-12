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

import java.io.IOException;

import statechum.Configuration;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraphND;

public class GraphMLVisualiser extends Visualiser {
	
	/**
	 * ID for serialisation
	 */
	private static final long serialVersionUID = 1735419773441272849L;

	public static void main(String[] args) throws IOException{
		//File graphDir = new File(args[0]);//new File(System.getProperty("user.dir")+System.getProperty("file.separator")+"resources"+
		//System.getProperty("file.separator")+"TestGraphs"+System.getProperty("file.separator") +args[0]);
		//String wholePath = graphDir.getAbsolutePath()+File.separator;
		LearnerGraphND graph0 = new LearnerGraphND(Configuration.getDefaultConfiguration().copy()),graph1 = null;
		AbstractPersistence.loadGraph(args[0], graph0,null);
		if (args.length > 1)
		{
			graph1 = new LearnerGraphND(Configuration.getDefaultConfiguration().copy());;
			AbstractPersistence.loadGraph(args[1], graph1,null);
		}
		Visualiser.updateFrame(graph0, graph1);Visualiser.waitForKey();
	}
	

}
