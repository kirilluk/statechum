/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */ 

package statechum.analysis.learning.rpnicore;

import edu.uci.ics.jung.io.GraphMLFileHandler;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.exceptions.FatalException;
import edu.uci.ics.jung.graph.decorators.StringLabeller;

import java.util.*;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex.MiniPair;
import statechum.Label;

public class ExperimentGraphMLHandler<TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>>
	extends GraphMLFileHandler {

	private final Configuration config;
	
	public ExperimentGraphMLHandler(Configuration conf)
	{
		config = conf;
	}
	
	@Override
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

        //String direction = (String) attributeMap.remove("directed");
        //boolean directed = Boolean.parseBoolean(direction);
        boolean directed = true;
        Edge e;
        if(!(sourceVertex.getSuccessors().contains(targetVertex))){
	        if (directed)
	            e = mGraph.addEdge(new DirectedSparseEdge(sourceVertex, targetVertex));
	        else
	            e = mGraph.addEdge(new UndirectedSparseEdge(sourceVertex, targetVertex));
	        
	        for (Iterator keyIt = attributeMap.keySet().iterator();keyIt.hasNext();) 
	        {
	            Object key = keyIt.next();
	            Object value = attributeMap.get(key);
	            e.setUserDatum(key, value, UserData.SHARED);
	        }
	        Set<Label> labels = new TreeSet<Label>();
	        if(attributeMap.get("EDGE")!=null)
	        	labels.add(AbstractLearnerGraph.generateNewLabel((String)attributeMap.get("EDGE"),config));
	        e.setUserDatum(JUConstants.LABEL, labels, UserData.SHARED);
        }
        else{
        	e = DeterministicDirectedSparseGraph.findEdge(sourceVertex, targetVertex);
        	Set<Label> labels = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
        	if(attributeMap.get("EDGE")!=null)
        		labels.add(AbstractLearnerGraph.generateNewLabel((String)attributeMap.get("EDGE"),config));
        }

        return e;
    }
	
	@Override
	protected ArchetypeVertex createVertex(Map attributeMap) {
		Graph mGraph = getGraph();
		StringLabeller mLabeller = getLabeller();
        if (mGraph == null) {
            throw new FatalException("Error parsing graph. Graph element must be specified before node element.");
        }

        String idString = ((String) attributeMap.remove("id")).replaceAll(AbstractPersistence.Initial+" *", "");
        DeterministicDirectedSparseGraph.DeterministicVertex vertex = 
        	new DeterministicDirectedSparseGraph.DeterministicVertex(VertexID.parseID(idString));// this ID will be subsequently modified when we look at the "VERTEX" tag.
        mGraph.addVertex(vertex);

        try {
            mLabeller.setLabel(vertex,idString);
        } catch (StringLabeller.UniqueLabelException ule) {
            throw new FatalException("Ids must be unique");

        }

        for (Iterator keyIt = attributeMap.keySet().iterator();keyIt.hasNext();) 
        {
            Object key = keyIt.next();
            Object value = attributeMap.get(key);
            
            MiniPair p=new MiniPair(key,value);
            vertex.setUserDatum(p.getKey(), p.getValue(), UserData.SHARED);
        }
		
        String label = null;
        
        Object vertexid = attributeMap.get("VERTEX");
        if (vertexid != null)
        	label = vertexid.toString();
        else
        	label = idString;
        
        if(label.startsWith(AbstractPersistence.Initial))
        {
        	vertex.addUserDatum("startOrTerminal", "start", UserData.SHARED);
        	vertex.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
        	label = label.replaceAll(AbstractPersistence.Initial+" *", "");
        }
        else if (label.startsWith(AbstractPersistence.InitialQ0) && vertexid == null)
        {
        	vertex.addUserDatum("startOrTerminal", "start", UserData.SHARED);
        	vertex.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
        }

        vertex.setUserDatum(JUConstants.LABEL, VertexID.parseID(label), UserData.SHARED);
       	if (!vertex.containsUserDatumKey(JUConstants.ACCEPTED))
       		vertex.setUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
        
        return vertex;
    }


	
}
