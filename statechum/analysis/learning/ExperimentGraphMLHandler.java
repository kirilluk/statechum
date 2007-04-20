package statechum.analysis.learning;

import edu.uci.ics.jung.io.GraphMLFileHandler;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.exceptions.FatalException;
import edu.uci.ics.jung.graph.decorators.StringLabeller;

import java.util.*;

import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;

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
        	e = RPNIBlueFringeLearner.findEdge(sourceVertex, targetVertex);
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

        ArchetypeVertex vertex = mGraph.addVertex(new DeterministicDirectedSparseGraph.DeterministicVertex());
        String idString = (String) attributeMap.remove("id");

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
        	vertex.addUserDatum(JUConstants.PROPERTY, JUConstants.INIT, UserData.SHARED);
        }
        vertex.setUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);
        return vertex;
    }


	
}
