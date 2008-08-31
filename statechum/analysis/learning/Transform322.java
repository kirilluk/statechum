/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

statechum is free software: you can redistribute it and/or modify
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
package statechum.analysis.learning;

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.io.GraphMLFile;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.experiments.ExperimentGraphMLHandler;

/** Contains XML serialisation methods from Transform version 322.
 * @author kirill
 *
 */
public class Transform322 {
	final FSMStructure coregraph;
	
	/** Associates this object to ComputeStateScores it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	Transform322(FSMStructure g)
	{
		coregraph =g;
	}
	
	/** a marker for an initial state in a graphML file. */
	public static final String Initial = "Initial";
	
	/** Returns the ID of the node, prepending Initial as appropriate for the initial state. */
	protected String transformNodeName(String node)
	{
		return (node == coregraph.init? Initial+" ":"")+node; 
	}

	/** Writes a graph into a graphML file. All vertices are written. */
	public void writeGraphML(String name) throws IOException
	{
		FileWriter writer = new FileWriter(name);writeGraphML(writer);writer.close();
	}
	
	/** Graphml namespace */
	protected static final String graphmlNS="gml";
	
	/** Graphml top-level node tag. */
	public static final String graphmlNodeName = graphmlNS+":graphml";
	
	/** Graphml uri */
	protected static final String graphlmURI="http://graphml.graphdrawing.org/xmlns/graphml";	
	
	protected Element createStateNode(Document doc, String node)
	{
		if (node.contains(Initial))
			throw new IllegalArgumentException("Invalid node name "+node);
		Element nodeElement = doc.createElementNS(graphmlNS,"node");
		nodeElement.setAttribute("id",node);
		nodeElement.setIdAttribute("id", true);
		nodeElement.setAttribute("VERTEX", transformNodeName(node));
		if (!coregraph.accept.get(node)) nodeElement.setAttribute(JUConstants.ACCEPTED,Boolean.toString(coregraph.accept.get(node)));
		return nodeElement;
	}
	
	public static Text endl(Document doc)
	{
		return doc.createTextNode("\n");		
	}
	
	public Element createGraphMLNode(Document doc)
	{
		Element graphElement = doc.createElementNS(graphlmURI,graphmlNodeName);
		Element graphTop = doc.createElementNS(graphmlNS,"graph");
		//graphElement.setAttributeNodeNS(doc.createAttributeNS("http://graphml.graphdrawing.org/xmlns/graphml", "gml:aaaschemaLocation"));
		graphTop.setAttribute("edgedefault", "directed");graphElement.appendChild(graphTop);
		graphTop.appendChild(endl(doc));
		for(Entry<String,Map<String,String>> vert:coregraph.trans.entrySet())
		{
			graphTop.appendChild(createStateNode(doc, vert.getKey()));graphTop.appendChild(endl(doc));
		}
		for(Entry<String,Map<String,String>> vert:coregraph.trans.entrySet())
			for(Entry<String,String> transition:vert.getValue().entrySet())
			{
				Element edge = doc.createElementNS(graphmlNS,"edge");edge.setAttribute("source", vert.getKey());
				edge.setAttribute("target", transition.getValue());edge.setAttribute("directed", "true");
				edge.setAttribute("EDGE", transition.getKey());graphTop.appendChild(edge);
				graphTop.appendChild(endl(doc));
			}
		return graphElement;
	}
	
	/** Writes a graph into a graphML file. All vertices are written.
	 * 
	 * @throws IOException if an I/O error occurs or 
	 * any vertex has a substring "Initial" in it, because this substring is used to designate 
	 * an initial state in the graphml file. Most of the time, "Init" is used instead in the graphs.
	 * @throws ParserConfigurationException 
	 */
	public void writeGraphML(Writer writer) throws IOException
	{
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		Document doc = null;
		try
		{
			factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();
		}
		catch(ParserConfigurationException ex)
		{
			IOException parserEx = new IOException("configuration exception: "+ex);parserEx.initCause(ex);throw parserEx;
		}
		doc.appendChild(createGraphMLNode(doc));
		// based on http://www.exampledepot.com/egs/javax.xml.transform/WriteDom.html
		try {
			Transformer trans = TransformerFactory.newInstance().newTransformer();
			trans.transform(new DOMSource(doc),new StreamResult(writer));
		} catch (Exception e) {
			IOException ex = new IOException("failed to write out XML "+e);ex.initCause(e);
			throw ex;
		}
	}
	
	static class DOMExperimentGraphMLHandler extends ExperimentGraphMLHandler
	{
	    @Override
		public Graph getGraph() {
	        return super.getGraph();
	    }
		
	}

	/** Converts DOM collection of attributes to the SAX one.
	 * @param namedMap what to convert
	 * @return the SAX collection, ready to be passed to a SAX listener. 
	 */
	static protected Attributes Attributes_DOM_to_SAX(NamedNodeMap namedMap)
	{
		AttributesImpl collection = new AttributesImpl();
		if (namedMap != null)
			for(int i=0;i<namedMap.getLength();++i) 
			{
				org.w3c.dom.Node node = namedMap.item(i);
				collection.addAttribute(node.getNamespaceURI(), node.getLocalName(), node.getNodeName(), "attribute", node.getNodeValue());
			}
		return collection;
	}
	
	/** Given a node in a document, loads a graph from this node. 
	 * @param elem the graphml element to load 
	 */
	public static DirectedSparseGraph loadGraph(Element elem)
	{
		if (!elem.getNodeName().equals(Transform322.graphmlNodeName))
			throw new IllegalArgumentException("element does not start with graphml");
		NodeList graphs = elem.getElementsByTagName("graph");
		if (graphs.getLength() < 1)
			throw new IllegalArgumentException("absent graph element");
		if (graphs.getLength() > 1)
			throw new IllegalArgumentException("duplicate graph element");
		Element graphElement = (Element)graphs.item(0);

		DOMExperimentGraphMLHandler graphHandler = new DOMExperimentGraphMLHandler();
    	GraphMLFile graphmlFile = new GraphMLFile();
    	graphmlFile.setGraphMLFileHandler(graphHandler);
    	synchronized(computeStateScores.syncObj)
    	{// multi-core execution understandably fails if I forget to sync on that object
    		try
	    	{
		    	graphHandler.startElement(graphElement.getNamespaceURI(), graphElement.getLocalName(), graphElement.getNodeName(), Attributes_DOM_to_SAX(graphElement.getAttributes())); // so as to applease the lack of any clue Jung has about graphml namespaces
		    	NodeList nodes = graphElement.getChildNodes(); 
		    	for(int i=0;i<nodes.getLength();++i)
		    	{
					org.w3c.dom.Node node = nodes.item(i);
					if (node.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE)
						graphHandler.startElement(node.getNamespaceURI(), node.getLocalName(), node.getNodeName(), Attributes_DOM_to_SAX(node.getAttributes()));
		    	}
	    	}
	    	catch(SAXException e)
	    	{
	    		IllegalArgumentException ex = new IllegalArgumentException("failed to write out XML "+e);ex.initCause(e);
	    		throw ex;
	    	}
    	}
    	return (DirectedSparseGraph)graphHandler.getGraph();
	}

	/** Useful where we aim to check that the learnt machine is the same as 
	 * original. To prevent erroneous mergers, negative information is supplied,
	 * which is incorporated into the final machine. This way, even if the
	 * original machine does not have reject-states, the outcome of merging
	 * will have them. Transitions to those negative states are obviously only
	 * added where there are no transitions in the original one, so if we take 
	 * the original machine and add transitions from all states to reject states 
	 * for undefined inputs (called <em>completeGraph()</em>), the outcome 
	 * of learning will have a subset of transitions to reject-states.
	 *<p>
	 * Throws {@link IllegalArgumentException} if the initial state points to a reject-state. 
	 * This makes sure that the outcome is never an empty graph.
	 * 
	 * @param what an automaton which states are to be removed.
	 * @param config this method makes a copy of an automaton first, hence a configuration is needed.
	 * @return an automaton reduced in the described way.
	 */
	public static FSMStructure removeRejectStates(FSMStructure what,Configuration config)
	{
		if (!what.accept.get(what.init)) throw new IllegalArgumentException("initial state cannot be a reject-state");
		FSMStructure result = what.copy();
		
		// Since we'd like to modify a transition matrix, we iterate through states of the original machine and modify the result.
		for(Entry<String,Map<String,String>> entry:what.trans.entrySet())
			if (!what.accept.get(entry.getKey())) result.trans.remove(entry.getKey());// a copied state should be identical to the original one, so doing remove is appropriate 
			else
			{
				Map<String,String> row = result.trans.get(entry.getKey());
				for(Entry<String,String> target:entry.getValue().entrySet())
					if (!what.accept.get(target.getValue())) 
						row.remove(target.getKey());
			}
		
		return result;
	}
}
