/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
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

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import statechum.StatechumXML;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.observers.LearnerSimulator;
import statechum.analysis.learning.observers.ProgressDecorator;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.io.GraphMLFile;
import statechum.Label;

public class AbstractPersistence<TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>>
{
	final AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> coregraph;

	/** Associates this object to AbstractLearnerGraph it is using for data to operate on. 
	 * Important: the constructor should not access any data in AbstractLearnerGraph 
	 * because it is usually invoked during the construction phase of AbstractLearnerGraph 
	 * when no data is yet available.
	 */
	AbstractPersistence(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> computeStateScores) 
	{
		coregraph = computeStateScores;
	}

	/** a marker for an initial state in a graphML file. */
	public static final String Initial = "Initial", InitialQ0="q0";
	
	/** Returns the ID of the node, prepending Initial as appropriate for the initial state. */
	protected String transformNodeName(CmpVertex node)
	{
		return (node == coregraph.getInit()? Initial+" ":"")+node.getID().toString(); 
	}

	/** Writes a graph into a graphML file. All vertices are written. */
	public void writeGraphML(String name) throws IOException
	{
		FileWriter writer = new FileWriter(name);writeGraphML(writer);writer.close();
	}
		
	protected Element createStateNode(Document doc, CmpVertex node)
	{
		if (node.getID().toString().contains(Initial))
			throw new IllegalArgumentException("Invalid node name "+node);
		Element nodeElement = doc.createElementNS(StatechumXML.graphmlNS.toString(),"node");
		nodeElement.setAttribute("id",node.getID().toString());
		nodeElement.setIdAttribute("id", true);
		nodeElement.setAttribute("VERTEX", transformNodeName(node));
		if (!node.isAccept()) nodeElement.setAttribute(JUConstants.ACCEPTED.name(),Boolean.toString(node.isAccept()));
		if (node.isHighlight()) nodeElement.setAttribute(JUConstants.HIGHLIGHT.name(),Boolean.toString(node.isHighlight()));
		if (node.getColour() != null) nodeElement.setAttribute(JUConstants.COLOUR.name(),node.getColour().name());
		if (node.getOrigState() != null) nodeElement.setAttribute(JUConstants.ORIGSTATE.name(),node.getOrigState().toString());
		if (node.getDepth() != JUConstants.intUNKNOWN) nodeElement.setAttribute(JUConstants.DEPTH.name(), Integer.toString(node.getDepth()));
		return nodeElement;
	}
	
	public static Text endl(Document doc)
	{
		return doc.createTextNode("\n");		
	}
	
	public Element createGraphMLNode(Document doc)
	{
		Element graphElement = doc.createElementNS(StatechumXML.graphlmURI.toString(),StatechumXML.graphmlNodeNameNS.toString());
		Element graphTop = doc.createElementNS(StatechumXML.graphmlNS.toString(),"graph");
		//graphElement.setAttributeNodeNS(doc.createAttributeNS("http://graphml.graphdrawing.org/xmlns/graphml", "gml:aaaschemaLocation"));
		graphTop.setAttribute("edgedefault", "directed");graphElement.appendChild(graphTop);
		graphTop.appendChild(endl(doc));
		for(Entry<CmpVertex,Map<Label,TARGET_TYPE>> vert:coregraph.transitionMatrix.getPotentiallyOrderedEntrySet(coregraph.config.getUseOrderedEntrySet()))
		{
			graphTop.appendChild(createStateNode(doc, vert.getKey()));graphTop.appendChild(endl(doc));
		}
		for(Entry<CmpVertex,Map<Label,TARGET_TYPE>> vert:coregraph.transitionMatrix.getPotentiallyOrderedEntrySet(coregraph.config.getUseOrderedEntrySet()))
			for(Entry<Label,TARGET_TYPE> transition:vert.getValue().entrySet())
				for(CmpVertex targetState:coregraph.getTargets(transition.getValue()))
				{
					Element edge = doc.createElementNS(StatechumXML.graphmlNS.toString(),"edge");edge.setAttribute("source", vert.getKey().getID().toString());
					edge.setAttribute("target", targetState.getID().toString());edge.setAttribute("directed", "true");
					edge.setAttribute("EDGE", transition.getKey().toErlangTerm());graphTop.appendChild(edge);
					graphTop.appendChild(endl(doc));
				}
		
		if (!coregraph.pairCompatibility.compatibility.isEmpty())
		{
			Element compatibilityData = doc.createElementNS(StatechumXML.graphmlNS.toString(),graphmlData);compatibilityData.setAttribute(graphmlDataKey, graphmlDataIncompatible);
			Set<CmpVertex> encounteredNodes = new HashSet<CmpVertex>();
			for(Entry<CmpVertex,Map<CmpVertex,JUConstants.PAIRCOMPATIBILITY>> entry:coregraph.pairCompatibility.compatibility.getPotentiallyOrderedEntrySet(coregraph.config.getUseOrderedEntrySet()))
			{
				encounteredNodes.add(entry.getKey());
				for(Entry<CmpVertex,JUConstants.PAIRCOMPATIBILITY> vert:entry.getValue().entrySet())
					if (!encounteredNodes.contains(vert.getKey()))
					{
						compatibilityData.appendChild(ProgressDecorator.writePair(new PairScore(entry.getKey(),vert.getKey(),vert.getValue().getInteger(),JUConstants.intUNKNOWN), doc));compatibilityData.appendChild(endl(doc));
					}
			}
			
			graphTop.appendChild(compatibilityData);graphTop.appendChild(endl(doc));
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
			factory.setFeature(javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();doc.setXmlStandalone(true);
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
	
	/** We need to be able to get access to a graph being loaded and the only 
	 * possible way is to override an appropriate method. 
	 */
	static class DOMExperimentGraphMLHandler<TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>>
		extends ExperimentGraphMLHandler<TARGET_TYPE,CACHE_TYPE>
	{
		public DOMExperimentGraphMLHandler(Configuration config)
		{
			super(config);
		}
		
	    @Override
		public Graph getGraph() {
	        return super.getGraph();
	    }
		
	}

	public static final String graphmlAttribute="attribute", graphmlGraph = "graph", graphmlData="data", 
		graphmlDataKey = "key",graphmlDataIncompatible="key_incompatible";
	
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
				collection.addAttribute(node.getNamespaceURI(), node.getLocalName(), node.getNodeName(), graphmlAttribute, node.getNodeValue());
			}
		return collection;
	}
	
	/** Loads a graph from the supplied XML node.
	 * 
	 * @param elem XML element to load from
	 * @param result graph into which to copy the loaded graph (we are generic hence cannot create an instance ourselves).
	 * @return loaded graph
	 */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>>
		AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> loadGraph(org.w3c.dom.Element elem, 
				AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> result)
	{
		if (!elem.getNodeName().equals(StatechumXML.graphmlNodeNameNS.toString()) && !elem.getNodeName().equals(StatechumXML.graphmlNodeName.toString()))
			throw new IllegalArgumentException("element name "+elem.getNodeName()+" is not graphml");
		NodeList graphs = StatechumXML.getChildWithTag(elem,graphmlGraph);
		if (graphs.getLength() < 1)
			throw new IllegalArgumentException("absent graph element");
		if (graphs.getLength() > 1)
			throw new IllegalArgumentException("duplicate graph element");
		Element graphElement = (Element)graphs.item(0);

		DOMExperimentGraphMLHandler<TARGET_TYPE,CACHE_TYPE> graphHandler = new DOMExperimentGraphMLHandler<TARGET_TYPE,CACHE_TYPE>(result.config);
    	GraphMLFile graphmlFile = new GraphMLFile();
    	graphmlFile.setGraphMLFileHandler(graphHandler);
    	synchronized(AbstractLearnerGraph.syncObj)
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
    	LearnerGraphND loadedGraph = new LearnerGraphND(graphHandler.getGraph(),result.config);
    	AbstractLearnerGraph.copyGraphs(loadedGraph, result);
    	NodeList nodes = graphElement.getChildNodes(); 
    	for(int i=0;i<nodes.getLength();++i)
    	{
			org.w3c.dom.Node node = nodes.item(i);
			if (node.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE)
			{
				if (node.getNodeName().equals(graphmlData))
				{
					if (graphmlDataIncompatible.equals(((Element)node).getAttribute(graphmlDataKey)))
					{
						NodeList children = node.getChildNodes();
						for(int childNum=0;childNum<children.getLength();++childNum)
							if (children.item(childNum).getNodeType() == Node.ELEMENT_NODE)
							{
								PairScore pair=ProgressDecorator.readPair(result, (Element)children.item(childNum));
								CmpVertex a = result.findVertex(pair.firstElem.getID()), b = result.findVertex(pair.secondElem.getID()); 
								if (a == null)
									throw new IllegalArgumentException("Unknown state "+pair.firstElem);
								if (b == null)
									throw new IllegalArgumentException("Unknown state "+pair.secondElem);
								result.addToCompatibility(a, b, JUConstants.PAIRCOMPATIBILITY.compatibilityToJUConstants(pair.getScore()));
							}
						
					}
					else
						throw new IllegalArgumentException("unexpected key "+((Element)node).getAttribute(graphmlDataKey));
				}
				else // a node which is not a "data" node.
					if (!node.getNodeName().equals("node") && !node.getNodeName().equals("edge"))
						throw new IllegalArgumentException("unexpected node "+node.getNodeName()+" in graph");
			}
    	}
    	
    	return result;
	}	
	
	/** Loads a graph from the data in a supplied reader.
	 * @param from reader to load a graph from
	 * @param result graph into which to copy the loaded graph (we are generic hence cannot create an instance ourselves).
	 */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>>
		AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> loadGraph(Reader from, 
				AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> result)
	{
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	GraphMLFile graphmlFile = new GraphMLFile();
	    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler<TARGET_TYPE,CACHE_TYPE>(result.config));
	    	try
	    	{
	    		loadGraph(LearnerSimulator.getDocumentOfXML(from).getDocumentElement(), result);
	    	}
	    	finally
	    	{
	    		try
	    		{ if (from != null) from.close(); }
	    		catch(IOException ex)
	    		{// exception on close is ignored.	    			
	    		}
	    	}
		}
		return result;
	}
	
	/** Loads a graph from a supplied file.
	 *  
	 * @param from where to load from
	 * @param result graph into which to copy the loaded graph (we are generic hence cannot create an instance ourselves).
	 * The configuration of this graph determines types of nodes created, such as whether they are Jung nodes or Strings.
	 * @return created graph.
	*/
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>>
		AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> loadGraph(String fileName,
				AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> result) throws IOException
	{
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	GraphMLFile graphmlFile = new GraphMLFile();
	    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler<TARGET_TYPE,CACHE_TYPE>(result.config));
	    	String fileToLoad = fileName;
	    	if (!new java.io.File(fileToLoad).canRead()) fileToLoad+=".xml";
	    	FileReader is = null;
	    	try
	    	{
	    		is = new FileReader(fileToLoad);
	    		loadGraph(is,result);result.setName(fileName);
	    	}
	    	finally
	    	{
	    		if (is != null)
	    			is.close();
	    	}
	    	
	    	return result;
		}
	}

		/** Loads a graph from a supplied file.
		 *  
		 * @param from where to load from
		 * @param result graph into which to copy the loaded graph (we are generic hence cannot create an instance ourselves).
		 * The configuration of this graph determines types of nodes created, such as whether they are Jung nodes or Strings.
		 * @return created graph.
		*/
		public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>>
			AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> loadGraph(File fileToLoad,
					AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> result) throws IOException
		{
			return loadGraph(fileToLoad.getAbsolutePath(),result);
		}
}
