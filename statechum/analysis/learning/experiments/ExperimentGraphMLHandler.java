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

package statechum.analysis.learning.experiments;

import edu.uci.ics.jung.io.GraphMLFileHandler;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.exceptions.FatalException;
import edu.uci.ics.jung.graph.decorators.StringLabeller;

import java.util.*;

import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.analysis.learning.RPNIBlueFringeLearnerOrig;

public class ExperimentGraphMLHandler extends GraphMLFileHandler {

	protected Edge createEdge(Map attributeMap) {
		Graph mGraph = getGraph();
		StringLabeller mLabeller = getLabeller();
        if (mGraph == null) {
            throw new FatalException("Error parsing graph. Graph element must be specified before edge element.");
        }

        String sourceId = (String) attributeMap.remove("source");
        Vertex sourceVertex =
                mLabeller.getVertex(sourceId);

        String targetId = (String) attributeMap.remove("target");
        Vertex targetVertex =
                 mLabeller.getVertex(targetId);

        String direction = (String) attributeMap.remove("directed");
        boolean directed = true;
        Edge e;
        if(!(sourceVertex.getSuccessors().contains(targetVertex))){
	        if (directed)
	            e = mGraph.addEdge(new DirectedSparseEdge(sourceVertex, targetVertex));
	        else
	            e = mGraph.addEdge(new UndirectedSparseEdge(sourceVertex, targetVertex));
	        for (Iterator keyIt = attributeMap.keySet().iterator();
	             keyIt.hasNext();
	                ) {
	            Object key = keyIt.next();
	            Object value = attributeMap.get(key);
	            e.setUserDatum(key, value, UserData.SHARED);
	            HashSet labels = new HashSet();
	            labels.add(attributeMap.get("EDGE"));
	            e.setUserDatum(JUConstants.LABEL, labels, UserData.SHARED);
	        }
        }
        else{
        	e = RPNIBlueFringeLearnerOrig.findEdge(sourceVertex, targetVertex);
        	HashSet labels = (HashSet)e.getUserDatum(JUConstants.LABEL);
        	labels.add(attributeMap.get("EDGE"));
        }

        return e;
    }
	
	protected ArchetypeVertex createVertex(Map attributeMap) {
		Graph mGraph = getGraph();
		StringLabeller mLabeller = getLabeller();
        if (mGraph == null) {
            throw new FatalException("Error parsing graph. Graph element must be specified before node element.");
        }

        String idString = (String) attributeMap.remove("id");
        ArchetypeVertex vertex = mGraph.addVertex(new DeterministicDirectedSparseGraph.DeterministicVertex(idString));

        try {
            mLabeller.setLabel((Vertex) vertex,idString);
        } catch (StringLabeller.UniqueLabelException ule) {
            throw new FatalException("Ids must be unique");

        }

        for (Iterator keyIt = attributeMap.keySet().iterator();
             keyIt.hasNext();
                ) {
            Object key = keyIt.next();
            Object value = attributeMap.get(key);
            vertex.setUserDatum(key, value, UserData.SHARED);
        }
        String label = attributeMap.get("VERTEX").toString();
        vertex.setUserDatum(JUConstants.LABEL, label, UserData.SHARED);
        if(label.startsWith("Initial")){
        	vertex.addUserDatum("startOrTerminal", "start", UserData.SHARED);
        	vertex.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
        }
        vertex.setUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
        return vertex;
    }


	
}
