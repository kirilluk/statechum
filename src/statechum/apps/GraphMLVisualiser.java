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

import statechum.Configuration;
import statechum.Configuration.LABELKIND;
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.Erlang.OTPBehaviour.ConverterErlToMod;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
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
		
		Configuration cnf = Configuration.getDefaultConfiguration().copy();cnf.setLabelKind(LABELKIND.LABEL_ERLANG);
		LearnerGraphND grOrig = new LearnerGraphND(cnf),graph1 = null;
		AbstractPersistence.loadGraph(args[0], grOrig,null);

		File file = new File("ErlangExamples/frequency/frequency.erl");
		ErlangModule mod = ErlangModule.loadModule(ErlangModule.setupErlangConfiguration(file));
		final LearnerGraph erlangGraph = new LearnerGraph(grOrig.config);
		AbstractLearnerGraph.interpretLabelsOnGraph(grOrig,erlangGraph,mod.behaviour.new ConverterErlToMod());

		if (args.length > 1)
		{
			graph1 = new LearnerGraphND(Configuration.getDefaultConfiguration().copy());;
			AbstractPersistence.loadGraph(args[1], graph1,null);
		}
		Visualiser.updateFrame(grOrig, graph1);Visualiser.waitForKey();
	}
	

}
