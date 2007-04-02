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
