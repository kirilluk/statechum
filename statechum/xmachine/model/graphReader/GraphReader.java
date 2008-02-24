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

package statechum.xmachine.model.graphReader;

import java.io.File;
import java.util.*;
import javax.swing.*;

import statechum.xmachine.model.*;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.io.*;
import edu.uci.ics.jung.graph.*;

public class GraphReader {

	public XMachineModel readGraph(JFrame f){
		File mlFile = null;
		JFileChooser fc = new JFileChooser();
		fc.setMultiSelectionEnabled(false);
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		int choice = fc.showDialog(f, "Select GraphML file");
		if(choice == JFileChooser.APPROVE_OPTION){
			mlFile = fc.getSelectedFile();
			String name = mlFile.getAbsolutePath();
			GraphMLFile graphmlFile = new GraphMLFile();
			return makeMachine((DirectedSparseGraph)graphmlFile.load(name), name);
		}
		return null;
	}
	
	private XMachineModel makeMachine(DirectedSparseGraph g, String name){
		XMachineModel machine = new XMachineModel(name);
		Iterator edgeIt = g.getEdges().iterator();
		while(edgeIt.hasNext()){
			DirectedSparseEdge e = (DirectedSparseEdge)edgeIt.next();
			Vertex src = e.getSource();
			String fromLabel = String.valueOf(e.getSource().getUserDatum("VERTEX"));
			if(fromLabel.startsWith("Initial"))
				src.setUserDatum("startOrTerminal", "start", UserData.SHARED);
			String toLabel = String.valueOf(e.getDest().getUserDatum("VERTEX"));
			Transition t = new Transition(fromLabel,toLabel);
			TransitionFunction f = new TransitionFunction();
			f.setLabel(String.valueOf(e.getUserDatum("EDGE")));
			t.addFunction(f);
			machine.addTransition(t);
			machine.attachGraph(g);
		}
		return machine;
	}
}
