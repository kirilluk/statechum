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
package statechum.analysis.learning.rpnicore;

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Map;
import java.util.TreeMap;
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
import edu.uci.ics.jung.io.GraphMLFile;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.experiments.ExperimentGraphMLHandler;

/** Contains XML serialisation methods from Transform version 322.
 * @author kirill
 *
 */
public class Transform322 {
	final LearnerGraph coregraph;
	
	/** Associates this object to ComputeStateScores it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	Transform322(LearnerGraph g)
	{
		coregraph =g;
	}
	
	/** a marker for an initial state in a graphML file. */
	public static final String Initial = "Initial";
	
	/** Returns the ID of the node, prepending Initial as appropriate for the initial state. */
	protected String transformNodeName(CmpVertex node)
	{
		return (node == coregraph.init? Initial+" ":"")+node.getID().toString(); 
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
	
	protected Element createStateNode(Document doc, CmpVertex node)
	{
		if (node.getID().toString().contains(Initial))
			throw new IllegalArgumentException("Invalid node name "+node);
		Element nodeElement = doc.createElementNS(graphmlNS,"node");
		nodeElement.setAttribute("id",node.getID().toString());
		nodeElement.setIdAttribute("id", true);
		nodeElement.setAttribute("VERTEX", transformNodeName(node));
		if (!node.isAccept()) nodeElement.setAttribute(JUConstants.ACCEPTED.name(),Boolean.toString(node.isAccept()));
		if (node.isHighlight()) nodeElement.setAttribute(JUConstants.HIGHLIGHT.name(),Boolean.toString(node.isHighlight()));
		if (node.getColour() != null) nodeElement.setAttribute(JUConstants.COLOUR.name(),node.getColour().name());
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
		for(Entry<CmpVertex,Map<String,CmpVertex>> vert:coregraph.transitionMatrix.entrySet())
		{
			graphTop.appendChild(createStateNode(doc, vert.getKey()));graphTop.appendChild(endl(doc));
		}
		for(Entry<CmpVertex,Map<String,CmpVertex>> vert:coregraph.transitionMatrix.entrySet())
			for(Entry<String,CmpVertex> transition:vert.getValue().entrySet())
			{
				Element edge = doc.createElementNS(graphmlNS,"edge");edge.setAttribute("source", vert.getKey().getID().toString());
				edge.setAttribute("target", transition.getValue().getID().toString());edge.setAttribute("directed", "true");
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
	public static Graph loadGraph(Element elem)
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
    	return graphHandler.getGraph();
	}

	/** Adds all states and transitions from graph <em>what</em> to graph <em>g</em>.
	 * Very useful for renumbering nodes on graphs loaded from GraphML and such, because
	 * numerical node IDs can be useful. The current implementation does not require this
	 * because it is easy to build a map from existing identifiers to numbers.
	 * <em>WMethod.buildStateToIntegerMap()</em> does exactly this and the outcome is cached
	 * and used by <em>vertexToInt</em> and <em>vertexToIntNR</em>.
	 * <p>
	 * An example of using this method to renumber vertices is shown below:
	 * <pre>
	 * LearnerGraph grTmp = new LearnerGraph(g.config);
	 * CmpVertex newInit = addToGraph(grTmp,g);StatePair whatToMerge = new StatePair(grTmp.init,newInit);
	 * LinkedList<Collection<CmpVertex>> collectionOfVerticesToMerge = new LinkedList<Collection<CmpVertex>>();
	 * grTmp.pairscores.computePairCompatibilityScore_general(whatToMerge,collectionOfVerticesToMerge);
	 * LearnerGraph result = MergeStates.mergeAndDeterminize_general(grTmp, whatToMerge,collectionOfVerticesToMerge);
	 * WMethod.computeWSet(result);
	 * </pre>
	 * @param g target into which to merge what
	 * @param what graph to merge into g.
	 * @param whatToG maps original vertices to those included in the graph <em>g</em>.
	 * @return vertex in g corresponding to the initial vertex in what 
	 */ 
	public static CmpVertex addToGraph(LearnerGraph g, LearnerGraph what,Map<CmpVertex,CmpVertex> whatToG)
	{
		if (whatToG == null) whatToG = new TreeMap<CmpVertex,CmpVertex>();else whatToG.clear();
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:what.transitionMatrix.entrySet())
		{// The idea is to number the new states rather than to clone vertices.
		 // This way, new states get numerical IDs rather than retain the original (potentially text) IDs.
			CmpVertex newVert = LearnerGraph.generateNewCmpVertex(g.nextID(entry.getKey().isAccept()), g.config);
			if (g.findVertex(newVert.getID()) != null) throw new IllegalArgumentException("duplicate vertex with ID "+newVert.getID()+" in graph "+g);
			assert !g.transitionMatrix.containsKey(newVert) : "duplicate vertex "+newVert;
			newVert.setAccept(entry.getKey().isAccept());
			newVert.setHighlight(entry.getKey().isHighlight());
			newVert.setColour(entry.getKey().getColour());
			whatToG.put(entry.getKey(),newVert);
		}

		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:what.transitionMatrix.entrySet())
		{
			Map<String,CmpVertex> row = new TreeMap<String,CmpVertex>();g.transitionMatrix.put(whatToG.get(entry.getKey()),row);
			for(Entry<String,CmpVertex> transition:entry.getValue().entrySet())
				row.put(transition.getKey(), whatToG.get(transition.getValue()));
		}
		g.learnerCache.invalidate();
		return whatToG.get(what.init);
	}
	
	/** Changes states labels on a graph to their numerical equivalents.
	 * 
	 * @param what graph to convert
	 * @return result of conversion.
	 */
	public static LearnerGraph convertToNumerical(LearnerGraph what)
	{
		LearnerGraph result = new LearnerGraph(what.config);result.init = null;result.transitionMatrix.clear();
		result.init = addToGraph(result, what, null);if (what.getName() != null) result.setName(what.getName());
		return result;
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
	public static LearnerGraph removeRejectStates(LearnerGraph what,Configuration config)
	{
		if (!what.init.isAccept()) throw new IllegalArgumentException("initial state cannot be a reject-state");
		LearnerGraph result = what.copy(config);
		
		// Since we'd like to modify a transition matrix, we iterate through states of the original machine and modify the result.
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:what.transitionMatrix.entrySet())
			if (!entry.getKey().isAccept()) result.transitionMatrix.remove(entry.getKey());// a copied state should be identical to the original one, so doing remove is appropriate 
			else
			{
				Map<String,CmpVertex> row = result.transitionMatrix.get(entry.getKey());
				for(Entry<String,CmpVertex> target:entry.getValue().entrySet())
					if (!target.getValue().isAccept()) row.remove(target.getKey());
			}
		
		return result;
	}
}
