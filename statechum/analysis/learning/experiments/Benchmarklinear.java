/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

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

package statechum.analysis.learning.experiments;

import java.util.Date;

import statechum.Configuration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import edu.uci.ics.jung.io.GraphMLFile;

public class Benchmarklinear {
	public static void main(String[] args)
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = null;
		
		synchronized (LearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	GraphMLFile graphmlFile = new GraphMLFile();
	    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
	    	graph = new LearnerGraph(graphmlFile.load("resources/W_experiment/3000/N_3000_1.xml"
	    			//"resources/tmp/experiment_tmp.xml"
	    			),config);
		}
		System.out.println("graph loaded");
		long tmStarted = new Date().getTime();//HashSet<List<String>> outcome = new HashSet<List<String>>();
		graph.transform.buildMatrix(AbstractExperiment.getCpuNumber());
		//Collection<List<String>> wset = WMethod.computeWSetOrig(result);outcome.clear();outcome.addAll(wset);
		//WMethod.computeWSet_reducedmemory(result);
		long tmFinished = new Date().getTime();
		System.out.println(" time taken: "+(tmFinished-tmStarted)/1000);

	}
}
