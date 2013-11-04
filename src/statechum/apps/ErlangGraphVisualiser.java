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
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.learning.ErlangOracleVisualiser;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;

public class ErlangGraphVisualiser extends Visualiser {
	
	/**
	 * ID for serialisation
	 */
	private static final long serialVersionUID = 1735419773441272849L;

	/* The two arguments to this one are the graph to load and the .erl source of the module it corresponds to. The graphs should be saved via the "save" entry in the pop-up menu of ErlangOracleVisualiser. */
	public static void main(String[] args) throws IOException
	{
		File file = new File(args[1]);
		Configuration cnf = Configuration.getDefaultConfiguration().copy();ErlangModule.setupErlangConfiguration(cnf,file);
		ErlangModule mod = ErlangModule.loadModule(cnf);

		LearnerGraphND grOrig = new LearnerGraphND(cnf);
		AbstractPersistence.loadGraph(args[0], grOrig,null);

		final LearnerGraph erlangGraph = new LearnerGraph(grOrig.config);
		AbstractLearnerGraph.interpretLabelsOnGraph(grOrig,erlangGraph,mod.behaviour.new ConverterErlToMod());
		
		Visualiser graphVisualiser = new ErlangOracleVisualiser();
		graphVisualiser.update(null,erlangGraph);Visualiser.waitForKey();
	}
	
}
