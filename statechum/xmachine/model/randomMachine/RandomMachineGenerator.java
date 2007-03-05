package statechum.xmachine.model.randomMachine;

import edu.uci.ics.jung.random.generators.*;
import edu.uci.ics.jung.graph.decorators.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;
import statechum.xmachine.model.*;
import java.util.*;

public class RandomMachineGenerator {

	public XMachineModel generateGraph(){
		ErdosRenyiGenerator random = new ErdosRenyiGenerator(15, 0.2);
		ArchetypeGraph undirectedGraph = random.generateGraph();
		XMachineModel machine =  makeMachine(undirectedGraph);
		return machine;
	}
	
	private XMachineModel makeMachine(ArchetypeGraph g){
		XMachineModel machine = new XMachineModel("random machine");
		Iterator vertexIt = g.getVertices().iterator();
		Random r = new Random();
		Iterator edgeIt = g.getEdges().iterator();
		while(edgeIt.hasNext()){
			ArchetypeEdge e = (ArchetypeEdge)edgeIt.next();
			Object[] vertices = e.getIncidentElements().toArray();
			Vertex from,to;
			if(r.nextBoolean()){
				from = (Vertex)vertices[0];
				to = (Vertex)vertices[1];
			}
			else{
				from = (Vertex)vertices[1];
				to = (Vertex)vertices[0];
			}
			Indexer i = Indexer.newIndexer(g,0);
			String fromLabel = String.valueOf(i.getIndex(from));
			String toLabel = String.valueOf(i.getIndex(to));
			Transition t = new Transition(fromLabel,toLabel);
			machine.addTransition(t);
		}
		return machine;
	}
}
