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

package statechum.xmachine.model.randomMachine;

import edu.uci.ics.jung.random.generators.*;
import edu.uci.ics.jung.graph.decorators.*;
import edu.uci.ics.jung.graph.*;
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
