package statechum.analysis.learning;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;

import static statechum.xmachine.model.testset.WMethod.createLabelToStateMap;
import static statechum.xmachine.model.testset.WMethod.getGraphData;

import junit.framework.Assert;
import junit.framework.JUnit4TestAdapter;

import org.junit.AfterClass;
import org.junit.Test;
import org.junit.BeforeClass;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicEdge;
import statechum.analysis.learning.experiments.ExperimentGraphMLHandler;
import statechum.xmachine.model.testset.WMethod;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.io.GraphMLFile;
import edu.uci.ics.jung.utils.UserData;

import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import javax.swing.SwingUtilities;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

public class TestFSMAlgo {

	static protected StatePair constructPair(String a,String b)
	{
		DirectedSparseVertex aV = new DirectedSparseVertex(), bV = new DirectedSparseVertex();
		aV.addUserDatum(JUConstants.LABEL, a, UserData.SHARED);
		bV.addUserDatum(JUConstants.LABEL, b, UserData.SHARED);
		return new StatePair(aV,bV);
	}

	static protected void checkLess(String a, String b, String c, String d)
	{
		StatePair p = constructPair(a,b), q=constructPair(c,d);
		assertFalse(p.equals(q));
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertFalse(p.hashCode() == q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}
	
	@Test
	public void testStatePairEquality()
	{
		StatePair p = constructPair("a","b"), q=constructPair("a","b");
		assertTrue(p.equals(p));
		assertTrue(p.equals(q));
		assertFalse(p.equals(null));
		assertFalse(p.equals("test"));
		assertFalse(p.equals(constructPair("a","c")));
		assertFalse(p.equals(constructPair("b","b")));
	}
	
	@Test
	public void testStatePairComparison()
	{
		checkLess("a","b","c","d");
		checkLess("a","b","a","c");
		checkLess("a","b","c","b");
	}
	
	/** Given a textual representation of an fsm, builds a corresponding Jung graph
	 * 
	 * @param fsm the textual representation of an FSM
	 * @param name graph name, to be displayed as the caption of the Jung window.
	 * @return Jung graph for it
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public static DirectedSparseGraph buildGraph(String fsm,String name)
	{
		final Map<String,CmpVertex> existingVertices = new HashMap<String,CmpVertex>();
		final Map<StatePair,DeterministicEdge> existingEdges = new HashMap<StatePair,DeterministicEdge>();
		
		final DirectedSparseGraph g = new DirectedSparseGraph();
		g.setUserDatum(JUConstants.TITLE, name,UserData.SHARED);

		new TestFSMParser.fsmParser(fsm).parse(new TestFSMParser.TransitionReceiver()
		{
			public void put(String from, String to, String label, boolean accept) {
				CmpVertex fromVertex = existingVertices.get(from), toVertex = existingVertices.get(to);
				
				if (fromVertex == null)
				{
					fromVertex = new DeterministicDirectedSparseGraph.DeterministicVertex();
					if (existingVertices.isEmpty())
						fromVertex.addUserDatum(JUConstants.PROPERTY, JUConstants.INIT, UserData.SHARED);
					fromVertex.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);
					fromVertex.addUserDatum(JUConstants.LABEL, from, UserData.SHARED);
					existingVertices.put(from, fromVertex);
					g.addVertex(fromVertex);
				}
				else
					if (!Boolean.valueOf(fromVertex.getUserDatum(JUConstants.ACCEPTED).toString()))
						throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+from);

				if (from.equals(to))
				{
					if (!accept) throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+to);
					toVertex = fromVertex;
				}
				else
					if (toVertex == null)
					{
						toVertex = new DeterministicDirectedSparseGraph.DeterministicVertex();
						toVertex.removeUserDatum(JUConstants.ACCEPTED); // in case we've got a reject loop in the same state
						toVertex.addUserDatum(JUConstants.ACCEPTED, Boolean.toString(accept), UserData.SHARED);
						toVertex.addUserDatum(JUConstants.LABEL, to, UserData.SHARED);
						existingVertices.put(to, toVertex);
						g.addVertex(toVertex);
					}
					else
						if (Boolean.valueOf(toVertex.getUserDatum(JUConstants.ACCEPTED).toString()) != accept)
							throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+to);
				
				StatePair pair = new StatePair(fromVertex,toVertex);
				DeterministicEdge edge = existingEdges.get(pair);
				if (edge == null)
				{
					edge = new DeterministicDirectedSparseGraph.DeterministicEdge(fromVertex,toVertex);
					edge.addUserDatum(JUConstants.LABEL, new HashSet<String>(), UserData.CLONE);
					g.addEdge(edge);existingEdges.put(pair,edge);
				}
				
				Set<String> labels = (Set<String>)edge.getUserDatum(JUConstants.LABEL);
				labels.add(label);
			}

			public void accept(String from, String to, String label) {
				put(from,to,label,true);
			}
			public void reject(String from, String to, String label) {
				put(from,to,label,false);
			}
		});
		
		return g;
	}

	/** This data store represents an FSM and is used by tests. */
	public static class FSMStructure
	{
		/** The transition transition diagram, in which every state is mapped to a map between an input (label) and a target state. */
		public final Map<String,Map<String,String>> trans;
		
		/** All states of the machine should be in the domain of this function; 
		 * for a given state, this function will return <pre>true</pre> if it is an accept state and <pre>false</pre> for a reject one.
		 */ 
		public final Map<String,Boolean> accept;
		
		/** The initial state. */
		public String init;
		
		public FSMStructure(Map<String,Map<String,String>> transitions,Map<String,Boolean> a,String initState)
		{
			trans = transitions;accept = a;init = initState;
		}
		
		public FSMStructure()
		{
			trans = new TreeMap<String,Map<String,String>>();accept = new TreeMap<String,Boolean>();
		}
		
		public boolean equals(Object o)
		{
			if (this == o)
				return true;
			if (o == null || !(o instanceof FSMStructure))
				return false;
			
			FSMStructure otherStruct= (FSMStructure)o;
			return 
				accept.equals(otherStruct.accept) &&
				trans.equals(otherStruct.trans) &&
				init.equals(otherStruct.init);		
		}
		
		/** A loader for the graph - needed to ensure XMLEncoder does not complain. */
		public static FSMStructure loadGraph_FSMStructure(
				@SuppressWarnings("unused")	String text)
		{
	    	return WMethod.getGraphData(loadGraph_DirectedSparseGraph(text));
		}
		
		/** A loader for the graph - needed to ensure XMLEncoder does not complain. */
		public static DirectedSparseGraph loadGraph_DirectedSparseGraph(
				@SuppressWarnings("unused")	String text)
		{
	    	GraphMLFile graphmlFile = new GraphMLFile();
	    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
	    	DirectedSparseGraph graph = new DirectedSparseGraph();
	    	graph.getEdgeConstraints().clear();
	    	return (DirectedSparseGraph)graphmlFile.load(new StringReader(text));
		}
		
	    protected void save(String name)
	    {
			try 
			{
				writeGraphML(this, name+".xml");
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
	    }
	    
		/** The standard beginning of our graphML files. */
		public static final String graphML_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns/graphml\"  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\nxsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns/graphml\">\n<graph edgedefault=\"directed\">\n";
		/** The standard ending of our graphML files. */
		public static final String graphML_end = "</graph></graphml>\n"; 
		/** a marker for an initial state in a graphML file. */
		public static final String Initial = "Initial ";
		
		/** Returns the ID of the node, prepending Initial as appropriate for the initial state. */
		static protected String transformNodeName(FSMStructure fsm,String node)
		{
			
			return (node.equals(fsm.init)? Initial:"")+node; 
		}

		/** Writes a graph into a graphML file. All vertices are written. */
		static public void writeGraphML(FSMStructure fsm,String name) throws IOException
		{
			FileWriter writer = new FileWriter(name);writeGraphML(fsm,writer);
		}
		
		/** Graphml namespace */
		protected static final String graphmlNS="gml";

		static protected Element createStateNode(FSMStructure fsm,Document doc, String node)
		{
			if ( node.contains(Initial))
				throw new IllegalArgumentException("Invalid node name "+node);
			Element nodeElement = doc.createElementNS(graphmlNS,"node");
			nodeElement.setAttribute("id",node);
			nodeElement.setIdAttribute("id", true);
			nodeElement.setAttribute("VERTEX", transformNodeName(fsm,node));
			if (fsm.accept.containsKey(node) && !fsm.accept.get(node)) nodeElement.setAttribute(JUConstants.ACCEPTED.toString(),"false");
			return nodeElement;
		}
		
		protected static Text endl(Document doc)
		{
			return doc.createTextNode("\n");		
		}

		static public Element createGraphMLNode(FSMStructure fsm,Document doc)
		{
			Element graphElement = doc.createElementNS("http://graphml.graphdrawing.org/xmlns/graphml",graphmlNS+":graphml");
			Element graphTop = doc.createElementNS(graphmlNS,"graph");
			//graphElement.setAttributeNodeNS(doc.createAttributeNS("http://graphml.graphdrawing.org/xmlns/graphml", "gml:aaaschemaLocation"));
			graphTop.setAttribute("edgedefault", "directed");graphElement.appendChild(graphTop);
			graphTop.appendChild(endl(doc));
			
			for(Entry<String,Map<String,String>> vert:fsm.trans.entrySet())
			{
				graphTop.appendChild(createStateNode(fsm,doc,vert.getKey()));
				graphTop.appendChild(endl(doc));
			}
			
			for(Entry<String,Map<String,String>> vert:fsm.trans.entrySet())
				for(Entry<String,String> transition:vert.getValue().entrySet())
				{
					Element edge = doc.createElement("edge");edge.setAttribute("source", vert.getKey());
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
		static public void writeGraphML(FSMStructure fsm,Writer writer) throws IOException
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
			doc.appendChild(createGraphMLNode(fsm, doc));
			writer.append(doc.toString());
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
			if (!elem.getNodeName().equals(graphmlNS+":graphml"))
				throw new IllegalArgumentException("element does not start with graphml");
			Element graphElement = (Element)elem.getFirstChild();
			//System.out.println(graphElement.getLocalName()+ " "+graphElement.getNodeName());
			if (graphElement == null || !graphElement.getNodeName().equals("graph"))
				throw new IllegalArgumentException("absent graph element");
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
	}
	
	@Test
	public final void testFSMStructureEquals1()
	{
		FSMStructure a=new FSMStructure(),b=new FSMStructure();
		a.init = "A";b.init = "A";
		Assert.assertTrue(a.equals(a));
		Assert.assertTrue(a.equals(b));

		Assert.assertFalse(a.equals(null));
		Assert.assertFalse(a.equals("hello"));
		b.init = "B";Assert.assertFalse(a.equals(b));
	}
	
	@Test
	public final void testFSMStructureEquals2()
	{
		FSMStructure a=getGraphData(buildGraph("A-a->A-b->B", "testFSMStructureEquals2"));
		FSMStructure b=getGraphData(buildGraph("A-a->A-b->B", "testFSMStructureEquals2"));
		Assert.assertTrue(a.equals(b));
	}
	
	@Test
	public final void testFSMStructureEquals3()
	{
		FSMStructure a=getGraphData(buildGraph("A-a->A-b->B", "testFSMStructureEquals2"));
		FSMStructure b=getGraphData(buildGraph("A-a->A-b->B", "testFSMStructureEquals2"));
		
		b.trans.clear();
		Assert.assertFalse(a.equals(b));
	}
	
	@Test
	public final void testFSMStructureEquals4()
	{
		FSMStructure a=getGraphData(buildGraph("A-a->A-b->B", "testFSMStructureEquals2"));
		FSMStructure b=getGraphData(buildGraph("A-a->A-b->B", "testFSMStructureEquals2"));
		
		b.accept.clear();
		Assert.assertFalse(a.equals(b));
	}
	
	
	@Test 
	public void testCreateLabelToStateMap1() // test with empty data
	{
		assertTrue(createLabelToStateMap(new LinkedList<String>(), "junk", null).isEmpty());
		Map<String,String> map = new HashMap<String,String>();
		assertSame(map,createLabelToStateMap(new LinkedList<String>(), "junk", map));assertTrue(map.isEmpty());
	}
	
	@Test 
	public void testCreateLabelToStateMap2() // test for no changes
	{
		Map<String,String> trans = new HashMap<String,String>();
		trans.put("a", "A");trans.put("b", "A");trans.put("c", "B");
		Map<String,String> expected = new HashMap<String,String>();expected.putAll(trans);
		assertSame(trans,WMethod.createLabelToStateMap(new LinkedList<String>(), "junk",trans));
		assertTrue(expected.equals(trans));
	}
	
	@Test 
	public void testCreateLabelToStateMap3() // test for correct data being added
	{
		Map<String,String> trans = new HashMap<String,String>();
		trans.put("a", "A");trans.put("b", "A");trans.put("c", "B");
		Map<String,String> expected = new HashMap<String,String>();expected.putAll(trans);expected.put("e", "A");expected.put("g", "A");
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"g","e"}), "A",trans));
		assertTrue(expected.equals(trans));
	}
	
	@Test 
	public void testCreateLabelToStateMap4() // test for correct data being added
	{
		Map<String,String> trans = new HashMap<String,String>();
		trans.put("a", "A");trans.put("b", "A");trans.put("c", "B");
		Map<String,String> expected = new HashMap<String,String>();expected.putAll(trans);expected.put("e", "D");expected.put("f", "D");
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"f","e"}), "D",trans));
		assertTrue(expected.equals(trans));
	}
	
	@Test 
	public void testCreateLabelToStateMap5() // test for correct data being added
	{
		Map<String,String> trans = new HashMap<String,String>();
		trans.put("a", "A");trans.put("b", "A");trans.put("c", "B");
		Map<String,String> expected = new HashMap<String,String>();expected.putAll(trans);expected.put("e", "B");expected.put("g", "B");
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"g","e"}), "B",trans));
		assertTrue(expected.equals(trans));
	}
	
	@Test 
	public void testCreateLabelToStateMap6() // test for correct data being added when an empty collection is passed
	{
		Map<String,String> trans = new HashMap<String,String>();
		Map<String,String> expected = new HashMap<String,String>();expected.put("e","A");expected.put("b","A");
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), "A",trans));
		assertTrue(expected.equals(trans));
	}

	@Test 
	public void testCreateLabelToStateMap7() // test for correct data being added when null is passed
	{
		Map<String,String> expected = new HashMap<String,String>();expected.put("e","A");expected.put("b","A");
		assertTrue(expected.equals(createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), "A",null)));
	}

	@Test 
	public void testCreateLabelToStateMap8() // test for correct detection of nondeterminism
	{
		Map<String,String> trans = new HashMap<String,String>();trans.put("a", "A");trans.put("b", "A");trans.put("c", "B");
		boolean exceptionThrown = false;
		try
		{
			createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), "A",trans);
		}
		catch(IllegalArgumentException e)
		{
			assertTrue("incorrect exception thrown",e.getMessage().contains("nondeterminism"));
			exceptionThrown = true;
		}
		
		assertTrue("exception not thrown",exceptionThrown);
	}

	@Test 
	public void testCreateLabelToStateMap9() // test for correct detection of nondeterminism
	{
		boolean exceptionThrown = false;
		try
		{
			createLabelToStateMap(Arrays.asList(new String[] {"b","b"}), "A",null);
		}
		catch(IllegalArgumentException e)
		{
			assertTrue("incorrect exception thrown",e.getMessage().contains("nondeterminism"));
			exceptionThrown = true;
		}
		
		assertTrue("exception not thrown",exceptionThrown);
	}

	/** Displays the graph passed as an argument in the Jung window.
	 * @param g the graph to display 
	 */
	public void updateFrame(DirectedSparseGraph g)
	{
		if (visFrame == null)
			visFrame = new Visualiser();
		visFrame.update(null, g);
	}
	
	@Test
	public void testGraphConstruction1()
	{
		FSMStructure expected = new FSMStructure();
		DirectedSparseGraph g = buildGraph("A--a-->B-b->C-c->A","testConstruction1");
		FSMStructure graph = getGraphData(g);
		expected.trans.put("A", createLabelToStateMap(Arrays.asList(new String[] {"a"}),"B",null));
		expected.trans.put("B", createLabelToStateMap(Arrays.asList(new String[] {"b"}),"C",null));
		expected.trans.put("C", createLabelToStateMap(Arrays.asList(new String[] {"c"}),"A",null));
		expected.accept.put("A", true);
		expected.accept.put("B", true);
		expected.accept.put("C", true);
		
		assertEquals("A", graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,graph.trans.equals(expected.trans));
	}

	@Test
	public void testGraphConstruction2()
	{
		FSMStructure expected = new FSMStructure();
		DirectedSparseGraph g = buildGraph("A--a-->B-b->C-c->A-b->B-a-#D","testConstruction2");
		g.setUserDatum(JUConstants.TITLE, "testConstruction2",UserData.SHARED);
		FSMStructure graph = getGraphData(g);
		expected.trans.put("A", createLabelToStateMap(Arrays.asList(new String[] {"a","b"}),"B",null));
		expected.trans.put("B", createLabelToStateMap(Arrays.asList(new String[] {"b"}),"C",createLabelToStateMap(Arrays.asList(new String[] {"a"}),"D",null)));
		expected.trans.put("C", createLabelToStateMap(Arrays.asList(new String[] {"c"}),"A",null));
		expected.trans.put("D", createLabelToStateMap(Collections.EMPTY_LIST,null,null));
		expected.accept.put("A", true);
		expected.accept.put("B", true);
		expected.accept.put("C", true);
		expected.accept.put("D", false);
		
		assertEquals("A", graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,expected.trans.equals(graph.trans));
	}

	@Test
	public void testGraphConstruction3()
	{
		FSMStructure expected = new FSMStructure();
		DirectedSparseGraph g = buildGraph("A--a-->B<-b--C-c->A-b->A-c->A\nB-d->B-p->C\nB-q->C\nB-r->C\n","testConstruction3");
		FSMStructure graph = getGraphData(g);
		expected.trans.put("A", createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),"A",createLabelToStateMap(Arrays.asList(new String[] {"a"}),"B",null)));
		expected.trans.put("B", createLabelToStateMap(Arrays.asList(new String[] {"d"}),"B",createLabelToStateMap(Arrays.asList(new String[] {"r","p","q"}),"C",null)));
		expected.trans.put("C", createLabelToStateMap(Arrays.asList(new String[] {"b"}),"B",createLabelToStateMap(Arrays.asList(new String[] {"c"}),"A",null)));
		expected.accept.put("A", true);
		expected.accept.put("B", true);
		expected.accept.put("C", true);
		
		assertEquals("A", graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,expected.trans.equals(graph.trans));
	}

	@Test
	public void testGraphConstruction4()
	{
		FSMStructure expected = new FSMStructure();
		DirectedSparseGraph g = buildGraph("A--a-->B<-b--D-c->A-b->A-c->A\nB-d->B-p-#C\nB-q-#C\nB-r-#C\n","testConstruction4");
		FSMStructure graph = getGraphData(g);
		expected.trans.put("A", createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),"A",createLabelToStateMap(Arrays.asList(new String[] {"a"}),"B",null)));
		expected.trans.put("B", createLabelToStateMap(Arrays.asList(new String[] {"d"}),"B",createLabelToStateMap(Arrays.asList(new String[] {"r","p","q"}),"C",null)));
		expected.trans.put("D", createLabelToStateMap(Arrays.asList(new String[] {"b"}),"B", createLabelToStateMap(Arrays.asList(new String[] {"c"}),"A",null)));
		expected.trans.put("C", createLabelToStateMap(Collections.EMPTY_LIST,null,null));
		expected.accept.put("A", true);
		expected.accept.put("B", true);
		expected.accept.put("C", false);
		expected.accept.put("D", true);
		
		assertEquals("A", graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,expected.trans.equals(graph.trans));
	}

	@Test
	public void testGraphConstruction5()
	{
		FSMStructure expected = new FSMStructure();
		DirectedSparseGraph g = buildGraph("A--a-->B-b-#C\nA-b->A-c->A\nB-d->B-p-#C\nB-q-#C\nB-r-#C\n","testConstruction5");
		FSMStructure graph = getGraphData(g);
		expected.trans.put("A", createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),"A",createLabelToStateMap(Arrays.asList(new String[] {"a"}),"B",null)));
		expected.trans.put("B", createLabelToStateMap(Arrays.asList(new String[] {"d"}),"B",createLabelToStateMap(Arrays.asList(new String[] {"b","r","p","q"}),"C",null)));
		expected.trans.put("C", createLabelToStateMap(Collections.EMPTY_LIST,null,null));
		expected.accept.put("A", true);
		expected.accept.put("B", true);
		expected.accept.put("C", false);
		
		assertEquals("A", graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,expected.trans.equals(graph.trans));
	}
	
	@Test
	public void testGraphConstruction6() // checks loop support
	{
		FSMStructure expected = new FSMStructure();
		DirectedSparseGraph g = buildGraph("P-c->P<-b-Q_State<-a-P-b->P\nQ_State-a->Q_State","testConstruction6");
		FSMStructure graph = getGraphData(g);
		expected.trans.put("P", createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),"P",createLabelToStateMap(Arrays.asList(new String[] {"a"}),"Q_State",null)));
		expected.trans.put("Q_State", createLabelToStateMap(Arrays.asList(new String[] {"a"}),"Q_State",createLabelToStateMap(Arrays.asList(new String[] {"b"}),"P",null)));
		expected.accept.put("P", true);
		expected.accept.put("Q_State", true);
		
		assertEquals("P", graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,expected.trans.equals(graph.trans));
	}

	@Test
	public void testGraphConstructionFail1a()
	{
		boolean exceptionThrown = false;
		try
		{
			buildGraph("A--a-->B<-b-CONFL\nA-b->A-c->A\nB-d->B-p-#CONFL","testGraphConstructionFail1a");
		}
		catch(IllegalArgumentException e)
		{
			assertTrue("correct exception not thrown",e.getMessage().contains("conflicting") && e.getMessage().contains("CONFL"));
			exceptionThrown = true;
		}
		
		assertTrue("exception not thrown",exceptionThrown);
	}
	
	@Test
	public void testGraphConstructionFail1b()
	{
		boolean exceptionThrown = false;
		try
		{
			buildGraph("A--a-->CONFL-b-#CONFL","testGraphConstructionFail1b");
		}
		catch(IllegalArgumentException e)
		{
			assertTrue("correct exception not thrown",e.getMessage().contains("conflicting") && e.getMessage().contains("CONFL"));
			exceptionThrown = true;
		}
		
		assertTrue("exception not thrown",exceptionThrown);
	}
	
	/** Checks if adding a vertex to a graph causes an exception to be thrown. */
	public static void checkWithVertex(Vertex v,String expectedExceptionString, String testName)
	{
		final DirectedSparseGraph g = buildGraph("A--a-->B<-b-CONFL\nA-b->A-c->A\nB-d->B-p->CONFL",testName);
		getGraphData(g);// without the vertex being added, everything should be fine.
		g.addVertex(v);// add the vertex
		
		boolean exceptionThrown = false;
		try
		{
			getGraphData(g);// now getGraphData should choke.			
		}
		catch(IllegalArgumentException e)
		{
			assertTrue("correct exception not thrown",e.getMessage().contains(expectedExceptionString) );
			exceptionThrown = true;
		}
		
		assertTrue("exception not thrown",exceptionThrown);
	}
	
	@Test
	public void testGraphConstructionFail2()
	{
		DirectedSparseVertex v = new DirectedSparseVertex();
		v.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);v.addUserDatum(JUConstants.LABEL, "B", UserData.SHARED);
		checkWithVertex(v, "multiple", "testGraphConstructionFail2");
	}
	
	@Test
	public void testGraphConstructionFail3()
	{
		DirectedSparseVertex v = new DirectedSparseVertex();
		v.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);v.addUserDatum(JUConstants.LABEL, "CONFL", UserData.SHARED);
		checkWithVertex(v, "multiple", "testGraphConstructionFail3");
	}
	
	@Test
	public void testGraphConstructionFail4()
	{
		DirectedSparseVertex v = new DirectedSparseVertex();
		v.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);v.addUserDatum(JUConstants.LABEL, "Q", UserData.SHARED);v.addUserDatum(JUConstants.PROPERTY, JUConstants.INIT, UserData.SHARED);
		checkWithVertex(v, "duplicate", "testGraphConstructionFail4");
	}
	
	@Test
	public void testGraphConstructionFail5()
	{
		DirectedSparseVertex v = new DirectedSparseVertex();
		v.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);v.addUserDatum(JUConstants.LABEL, "Q", UserData.SHARED);v.addUserDatum(JUConstants.PROPERTY, "aa", UserData.SHARED);
		checkWithVertex(v, "property", "testGraphConstructionFail5");
	}
	
	@Test
	public void testGraphConstructionFail6() // missing initial state in an empty graph
	{
		boolean exceptionThrown = false;
		try
		{
			getGraphData(new DirectedSparseGraph());// now getGraphData should choke.			
		}
		catch(IllegalArgumentException e)
		{
			assertTrue("correct exception not thrown",e.getMessage().contains("missing initial") );
			exceptionThrown = true;
		}
		
		assertTrue("exception not thrown",exceptionThrown);
	}

	@Test
	public final void testGraphConstructionFail7() // unlabelled states
	{
		DirectedSparseGraph g = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum(JUConstants.PROPERTY, JUConstants.INIT, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);g.addVertex(init);
		boolean exceptionThrown = false;
		try
		{
			getGraphData(g);// now getGraphData should choke.			
		}
		catch(IllegalArgumentException e)
		{
			assertTrue("correct exception not thrown",e.getMessage().contains("unlabelled") );
			exceptionThrown = true;
		}
		
		assertTrue("exception not thrown",exceptionThrown);
	}
	
	/** Checks if the passed graph is isomorphic to the provided fsm
	 * 
	 * @param g graph to check
	 * @param fsm the string representation of the machine which the graph should be isomorphic to
	 */
	public void checkEq(DirectedSparseGraph g, String fsm)
	{
		DirectedSparseGraph expectedGraph = buildGraph(fsm,"expected graph");
		final FSMStructure graph = getGraphData(g);
		final FSMStructure expected = getGraphData(expectedGraph);
		assertEquals("incorrect initial state",expected.init, graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,expected.trans.equals(graph.trans));		
	}

	@Test
	public void testCheckEq()
	{
		DirectedSparseGraph g=buildGraph("P-a->Q_State-b->P-c->P","testCheckEq");
		checkEq(g,"P-c->P<-b-Q_State<-a-P");
	}
	
	/** This one is used to indicate that a two machines are not accepting the same language - 
	 * I need to check that it is the incompatibility exception thrown by the <i>checkM</i> 
	 * method and not any other <i>IllegalArgumentException</i>.
	 */
	public static class DifferentFSMException extends IllegalArgumentException 
	{
		/**
		 *  Serialization ID.
		 */
		private static final long serialVersionUID = 6126662147586264877L;

		public DifferentFSMException(String arg)
		{
			super(arg);
		}
	}
	
	
	public static void checkM(DirectedSparseGraph g,String fsm)
	{
		final FSMStructure graph = getGraphData(g);
		final DirectedSparseGraph expectedGraph = buildGraph(fsm,"expected graph");
		final FSMStructure expected = getGraphData(expectedGraph);
		checkM(graph,expected,graph.init,expected.init);
	}
	
	public static class StringPair implements Comparable
	{
		public final String a,b;
		
		public StringPair(String aStr,String bStr)
		{
			a=aStr;b=bStr;
		}

		@Override
		public boolean equals(Object arg0) {
			if (arg0 == null || !(arg0 instanceof StringPair))
				return false;
			StringPair arg = (StringPair)arg0;
			return a.equals(arg.a) && b.equals(arg.b);
		}

		@Override
		public int hashCode() {
			return a.hashCode() ^ b.hashCode();
		}

		@Override
		public String toString() {
			return "( "+a+","+b+" )";
		}

		public int compareTo(Object o) {
			StringPair pB = (StringPair)o;
			int aStr = a.compareTo(pB.a);
			int bStr = b.compareTo(pB.b);
			
			if(aStr != 0)
				return aStr; 
			return bStr;
		}
		
		
	}
	
	static protected void checkStatePairLess(String a, String b, String c, String d)
	{
		StringPair p = new StringPair(a,b), q=new StringPair(c,d);
		assertFalse(p.equals(q));
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertFalse(p.hashCode() == q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}
	
	@Test
	public void testStringPairEquality()
	{
		StringPair p = new StringPair("a","b"), q=new StringPair("a","b");
		assertTrue(p.equals(p));
		assertTrue(p.equals(q));
		assertFalse(p.equals(null));
		assertFalse(p.equals("test"));
		assertFalse(p.equals(new StringPair("a","c")));
		assertFalse(p.equals(new StringPair("b","b")));
		
		assertTrue(p.hashCode() != 0);
		assertTrue(q.hashCode() != 0);
	}
	
	@Test
	public void testStringPairComparison()
	{
		checkStatePairLess("a","b","c","d");
		checkStatePairLess("a","b","a","c");
		checkStatePairLess("a","b","c","b");
	}
	
	/** Checks the equivalence between the two states, stateG of graphA and stateB of graphB.
	 * Unreachable states are ignored. 
	 */
	public static void checkM(FSMStructure graph, FSMStructure expected, String stateGraph, String stateExpected)
	{
		Queue<StringPair> currentExplorationBoundary = new LinkedList<StringPair>();// FIFO queue

		Set<StringPair> statesAddedToBoundary = new HashSet<StringPair>();
		currentExplorationBoundary.add(new StringPair(stateGraph,stateExpected));statesAddedToBoundary.add(new StringPair(stateGraph,stateExpected));
		
		while(!currentExplorationBoundary.isEmpty())
		{
			StringPair statePair = currentExplorationBoundary.remove();
			assert graph.accept.containsKey(statePair.a) : "state "+statePair.a+" is not known to the first graph";
			assert expected.accept.containsKey(statePair.b) : "state "+statePair.b+" is not known to the second graph";
			if (!graph.accept.get(statePair.a).equals(expected.accept.get(statePair.b)))
				throw new DifferentFSMException("states "+statePair.a+" and " + statePair.b+" have a different acceptance labelling between the machines");
						
			Map<String,String> targets = graph.trans.get(statePair.a), expectedTargets = expected.trans.get(statePair.b);
			if (expectedTargets.size() != targets.size())// each of them is equal to the keyset size from determinism
				throw new DifferentFSMException("different number of transitions from state "+statePair);
				
			for(Entry<String,String> labelstate:targets.entrySet())
			{
				String label = labelstate.getKey();
				if (!expectedTargets.containsKey(label))
					throw new DifferentFSMException("no transition with expected label "+label+" from a state corresponding to "+statePair.b);
				String tState = labelstate.getValue();// the original one
				String expectedState = expectedTargets.get(label);
				
				StringPair nextPair = new StringPair(tState,expectedState);
				if (!statesAddedToBoundary.contains(nextPair))
				{
					currentExplorationBoundary.offer(nextPair);
					statesAddedToBoundary.add(nextPair);
				}
			}
		}
		
	}
	
	@Test
	public void testCheckM1()
	{
		checkM(buildGraph("A-a->B-b->C", "testCheck1"), "B-a->C-b->D");
	}
	
	@Test
	public void testCheckM2()
	{
		checkM(buildGraph("A-a->B-b->C-d-#F#-b-A", "testCheck2"), "B-a->C-b->D\nB-b-#REJ\nD-d-#REJ");
	}

	@Test
	public void testCheckM3()
	{
		String another  = "A-a->B-b->C\nC-b-#REJ\nA-d-#REJ";
		String expected = "A-a->B-b->C-b-#F#-d-A";
		checkM(buildGraph(another.replace('A', 'Q').replace('B', 'G').replace('C', 'A'), "testCheck3"), expected);
	}

	@Test
	public void testCheckM4() // multiple reject states
	{
		String another  = "A-a->B-b->C\nC-b-#REJ\nA-d-#REJ\nA-b-#REJ2\nB-a-#REJ2\nB-c-#REJ3";
		String expected = "A-a->B-b->C-b-#F#-d-A-b-#R\nB-a-#R\nU#-c-B";
		checkM(buildGraph(another.replace('A', 'Q').replace('B', 'G').replace('C', 'A'), "testCheck4"), expected);
	}

	@Test
	public void testCheckM5()
	{
		checkM(buildGraph("A-a->B-b->B-a->C", "testCheck5"), "S-a->U<-b-U\nQ<-a-U");
	}

	@Test
	public void testCheckM6()
	{
		final FSMStructure graph = getGraphData(buildGraph("A-a->B-b->B-a->C", "testCheck6"));
		final FSMStructure expected = getGraphData(buildGraph("U<-b-U\nQ<-a-U<-a-S","expected graph"));
		checkM(graph,expected,"A","S");
		checkM(graph,expected,"B","U");
		checkM(graph,expected,"C","Q");
	}

	@Test
	public final void testCheckM_multipleEq1() // equivalent states
	{
		final FSMStructure graph = getGraphData(buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckM_multipleEq1"));
		assertTrue(checkMBoolean(graph,graph,"D","C2"));
		assertTrue(checkMBoolean(graph,graph,"C2","D"));
		
		assertTrue(checkMBoolean(graph,graph,"D1","D2"));
		assertTrue(checkMBoolean(graph,graph,"D2","D1"));

		assertTrue(checkMBoolean(graph,graph,"D2","K2"));
		assertTrue(checkMBoolean(graph,graph,"K2","D2"));

		assertFalse(checkMBoolean(graph,graph,"D2","A1"));
		assertFalse(checkMBoolean(graph,graph,"A1","D2"));

		assertFalse(checkMBoolean(graph,graph,"D2","F1"));
		assertFalse(checkMBoolean(graph,graph,"F1","D2"));
	}

	@Test
	public final void testCheckM_multipleEq2() // equivalent states
	{
		final DirectedSparseGraph g = buildGraph("S-a->A-a->D-a->D-b->A-b->B-a->D\nB-b->C-a->D\nC-b->D\nS-b->N-a->N-b->N","testCheckM_multipleEq2");
		final FSMStructure graph = getGraphData(g);
		List<String> states = Arrays.asList(new String[]{"S","A","B","C","D","N"});
		for(String stA:states)
			for(String stB:states)
				assertTrue("states "+stA+"and "+stB+" should be equivalent",checkMBoolean(graph,graph,stA,stB));
	}
	
	@Test
	public final void testCheckM_multipleEq3() // equivalent states
	{
		final DirectedSparseGraph g = buildGraph("S-a->A-a->D-a->D-b->A-b->B-a->D\nB-b->C-a->D\nC-b->D\nS-b->N-a->M-a->N\nN-b->M-b->N","testCheckM_multipleEq3");
		final FSMStructure graph = getGraphData(g);
		List<String> states = Arrays.asList(new String[]{"S","A","B","C","D","N","M"});
		for(String stA:states)
			for(String stB:states)
				assertTrue("states "+stA+"and "+stB+" should be equivalent",checkMBoolean(graph,graph,stA,stB));
	}
	
	@Test
	public final void testCheckM_multipleEq4() // non-equivalent states
	{
		final DirectedSparseGraph g = buildGraph("A-a->B-a->C-a->A-b->C-b->B","testCheckM_multipleEq4");
		final FSMStructure graph = getGraphData(g);
		List<String> states = Arrays.asList(new String[]{"A","B","C"});
		for(String stA:states)
			for(String stB:states)
				if (stA.equals(stB))
					assertTrue("states "+stA+" and "+stB+" should be equivalent",checkMBoolean(graph,graph,stA,stB));
				else
					assertFalse("states "+stA+" and "+stB+" should not be equivalent",checkMBoolean(graph,graph,stA,stB));
	}
	
	/** Same as checkM, but returns a boolean false instead of an exception. */
	public static boolean checkMBoolean(FSMStructure graph, FSMStructure expected, String stateGraph, String stateExpected)
	{
		try
		{
			checkM(graph,expected,stateGraph,stateExpected);
		}
		catch(DifferentFSMException ex)
		{
			return false;
		}
		return true;
	}
	
	@Test
	public void testCheckM6_f1()
	{
		final FSMStructure graph = getGraphData(buildGraph("A-a->B-b->B-a->C", "testCheck6"));
		final FSMStructure expected = getGraphData(buildGraph("U<-b-U\nQ<-a-U<-a-S","expected graph"));
		Assert.assertTrue(checkMBoolean(graph,graph,"A","A"));
		Assert.assertTrue(checkMBoolean(graph,graph,"B","B"));
		Assert.assertTrue(checkMBoolean(graph,graph,"C","C"));
		Assert.assertTrue(checkMBoolean(expected,expected,"Q","Q"));
		Assert.assertTrue(checkMBoolean(expected,expected,"S","S"));
		
		Assert.assertFalse(checkMBoolean(graph,expected,"A","Q"));
		Assert.assertFalse(checkMBoolean(graph,expected,"A","U"));
		Assert.assertFalse(checkMBoolean(graph,expected,"B","Q"));
		Assert.assertFalse(checkMBoolean(graph,expected,"B","S"));
		Assert.assertFalse(checkMBoolean(graph,expected,"C","U"));
		Assert.assertFalse(checkMBoolean(graph,expected,"C","S"));
	}
	

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD1()
	{
		checkM(buildGraph("A-a->B-b->C", "testCheckMD1"), "B-a->C-b->B");		
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD2() // different reject states
	{
		checkM(buildGraph("A-a->B-b->C", "testCheckMD2"), "B-a->C-b-#D");
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD3() // missing transition
	{
		checkM(buildGraph("A-a->B-b->C\nA-b->B", "testCheckMD3"), "B-a->C-b->D");
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD4() // extra transition
	{
		checkM(buildGraph("A-a->B-b->C", "testCheckMD4"), "B-a->C-b->D\nB-b->C");
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD5() // missing transition
	{
		checkM(buildGraph("A-a->B-b->C\nB-c->B", "testCheckMD5"), "B-a->C-b->D");
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD6() // extra transition
	{
		checkM(buildGraph("A-a->B-b->C", "testCheckMD6"), "B-a->C-b->D\nC-c->C");
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD7() // swapped transitions
	{
		String another  = "A-a->B-b->C\nC-b-#REJ\nA-d-#REJ";
		String expected = "A-a->B-b->C-d-#F#-b-A";
		checkM(buildGraph(another.replace('A', 'Q').replace('B', 'G').replace('C', 'A'), "testCheckMD7"), expected);
	}
	
	/** Given an FSM and a sequence of labels to follow, this one checks whether the sequence is correctly
	 * accepted or not, and if not whether it is rejected at the correct element.
	 * 
	 * @param fsmString a description of an FSM
	 * @param path a sequence of labels to follow
	 * @param ExpectedResult the result to check
	 * @param The name of the vertex which is expected to be returned by getVertex
	 */
	public static void checkPath(String fsmString, String []path, int ExpectedResult, String enteredName)
	{
		assert (enteredName == null)? (ExpectedResult >= 0):true;
		final DirectedSparseGraph g = buildGraph(fsmString, "sample FSM");
		final FSMStructure graph = getGraphData(g);
		assertEquals(ExpectedResult, WMethod.tracePath(graph, Arrays.asList(path)));
		Vertex expected = (enteredName == null)? null:new computeStateScores(g,"SINK").findVertex(enteredName);
		assertSame(expected, RPNIBlueFringeLearner.getVertex(g, Arrays.asList(path)));
	}
	
	/** Given an FSM and a sequence of labels to follow, this one checks whether the sequence is correctly
	 * accepted from a supplied state or not, and if not whether it is rejected at the correct element.
	 * 
	 * @param fsmString a description of an FSM
	 * @param path a sequence of labels to follow
	 * @param ExpectedResult the result to check
	 * @param The name of the vertex which is expected to be returned by getVertex
	 */
	public static void checkPathFrom(String fsmString, String startingState, String []path, int ExpectedResult, String enteredName)
	{
		assert (enteredName == null) == (ExpectedResult >= 0);
		final DirectedSparseGraph g = buildGraph(fsmString, "sample FSM");
		final FSMStructure graph = getGraphData(g);
		assertEquals(ExpectedResult, WMethod.tracePath(graph, Arrays.asList(path),startingState));
		Vertex starting = new computeStateScores(g,"SINK").findVertex(startingState);
		Vertex expected = (enteredName == null)? null:new computeStateScores(g,"SINK").findVertex(enteredName);
		assertSame(expected, RPNIBlueFringeLearner.getVertex(g, starting,Arrays.asList(path)));
	}
	
	@Test
	public void testTracePath()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{}, RPNIBlueFringeLearner.USER_ACCEPTED,"A");
	}
	
	@Test
	public void testTracePath1a()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a"}, RPNIBlueFringeLearner.USER_ACCEPTED,"B");
	}
	
	@Test
	public void testTracePath1b()
	{
		checkPathFrom("A-a->B-b->C-c->D","B",new String[]{"b"},RPNIBlueFringeLearner.USER_ACCEPTED,"C");
	}
	
	@Test
	public void testTracePath2a()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a","b","c"}, RPNIBlueFringeLearner.USER_ACCEPTED,"D");
	}
	
	@Test
	public void testTracePath2b()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a","b","c","d"}, 3,null);
	}
	
	@Test
	public void testTracePath3()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"b"}, 0,null);
	}
	
	@Test
	public void testTracePath4()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a","b","d"}, 2,null);
	}
	
	@Test
	public void testTracePath5a()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"b","a","c"}, 0,null);
	}
	
	@Test
	public void testTracePath5b()
	{
		checkPathFrom("A-a->B-b->C-c->D", "B",new String[]{"c"},0,null);
	}
	
	@Test
	public void testTracePath5c()
	{
		checkPathFrom("A-a->B-b->C-c->D", "Q",new String[]{"c"},0,null);
	}
	
	@Test
	public void testTracePath6()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a","a","c","b"}, 1,null);
	}
	
	@Test
	public void testTracePath7()
	{
		checkPath("A-a->B-b->C-c-#D", new String[]{"a","b","c"}, 2,"D");
	}
	
	@Test
	public void testTracePath8()
	{
		checkPath("A-a->B-b->C-c-#D", new String[]{"a","b","c","d"}, 3,null);
	}
	
	@Test
	public void testTracePath9()
	{
		checkPath("A-a->B-b->C-c-#D", new String[]{"a","b","c","d","e"}, 3,null);
	}
	
	/** Computes an alphabet of a given graph and adds transitions to a 
	 * reject state from all states A and inputs a from which there is no B such that A-a->B
	 * (A-a-#REJECT) gets added. Note: such transitions are even added to reject vertices.
	 * 
	 * @param g the graph to add transitions to
	 * @param reject the name of the reject state, to be added to the graph.
	 * @return true if any transitions have been added
	 */   
	public static boolean completeGraph(DirectedSparseGraph g, String reject)
	{
		DirectedSparseVertex rejectVertex = new DirectedSparseVertex();
		boolean transitionsToBeAdded = false;// whether and new transitions have to be added.
		rejectVertex.addUserDatum(JUConstants.ACCEPTED, "false", UserData.SHARED);
		rejectVertex.addUserDatum(JUConstants.LABEL, reject, UserData.SHARED);
		
		// first pass - computing an alphabet
		Set<String> alphabet = WMethod.computeAlphabet(g);
		
		// second pass - checking if any transitions need to be added.
		Set<String> outLabels = new HashSet<String>();
		Iterator<Vertex> vertexIt = (Iterator<Vertex>)g.getVertices().iterator();
		while(vertexIt.hasNext() && !transitionsToBeAdded)
		{
			Vertex v = vertexIt.next();
			outLabels.clear();
			Iterator<DirectedSparseEdge>outEdgeIt = v.getOutEdges().iterator();
			while(outEdgeIt.hasNext()){
				DirectedSparseEdge outEdge = outEdgeIt.next();
				outLabels.addAll( (Set<String>)outEdge.getUserDatum(JUConstants.LABEL) );
			}
			transitionsToBeAdded = !alphabet.equals(outLabels);
		}
		
		if (transitionsToBeAdded)
		{
			// third pass - adding transitions
			g.addVertex(rejectVertex);
			vertexIt = (Iterator<Vertex>)g.getVertices().iterator();
			while(vertexIt.hasNext())
			{
				Vertex v = vertexIt.next();
				if (v != rejectVertex)
				{// no transitions should start from the reject vertex
					Set<String> outgoingLabels = new TreeSet<String>();outgoingLabels.addAll(alphabet);
					
					Iterator<DirectedSparseEdge>outEdgeIt = v.getOutEdges().iterator();
					while(outEdgeIt.hasNext()){
						DirectedSparseEdge outEdge = outEdgeIt.next();
						outgoingLabels.removeAll( (Set<String>)outEdge.getUserDatum(JUConstants.LABEL) );
					}
					if (!outgoingLabels.isEmpty())
					{
						// add a transition
						DirectedSparseEdge edge = new DirectedSparseEdge(v,rejectVertex);
						edge.addUserDatum(JUConstants.LABEL, outgoingLabels, UserData.CLONE);
						g.addEdge(edge);
					}
				}
			}
		}
		
		return transitionsToBeAdded;
	}

	@Test
	public void completeComputeAlphabet0()
	{
		Set<String> alphabet = WMethod.computeAlphabet(new DirectedSparseGraph());
		Assert.assertTrue(alphabet.isEmpty());
	}

	@Test
	public void completeComputeAlphabet1()
	{
		Set<String> alphabet = WMethod.computeAlphabet(buildGraph("A-a->A", "completeComputeAlphabet1"));
		Set<String> expected = new HashSet<String>();expected.addAll( Arrays.asList(new String[] {"a"}));
		Assert.assertTrue(alphabet.equals(expected));
		DirectedSparseGraph g = buildGraph("A-a->A", "completeGraphTest1");Assert.assertFalse(completeGraph(g,"REJECT"));
	}

	@Test
	public void completeComputeAlphabet2()
	{
		Set<String> alphabet = WMethod.computeAlphabet(buildGraph("A-a->A<-b-A", "completeComputeAlphabet2"));
		Set<String> expected = new HashSet<String>();expected.addAll( Arrays.asList(new String[] {"a","b"}));
		Assert.assertTrue(alphabet.equals(expected));
		DirectedSparseGraph g = buildGraph("A-a->A", "completeGraphTest1");Assert.assertFalse(completeGraph(g,"REJECT"));
	}

	@Test
	public void completeComputeAlphabet3()
	{
		Set<String> alphabet = WMethod.computeAlphabet(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "completeComputeAlphabet3"));
		Set<String> expected = new HashSet<String>();expected.addAll( Arrays.asList(new String[] {"a","b","c","d"}));
		Assert.assertTrue(alphabet.equals(expected));
		DirectedSparseGraph g = buildGraph("A-a->A", "completeGraphTest1");Assert.assertFalse(completeGraph(g,"REJECT"));
	}

	@Test
	public void completeGraphTest1()
	{
		DirectedSparseGraph g = buildGraph("A-a->A", "completeGraphTest1");Assert.assertFalse(completeGraph(g,"REJECT"));
		checkM(g, "A-a->A");		
	}
	
	@Test
	public void completeGraphTest2()
	{
		DirectedSparseGraph g = buildGraph("A-a->B-a->A", "completeGraphTest2");Assert.assertFalse(completeGraph(g,"REJECT"));
		checkM(g, "A-a->A");		
	}
	
	@Test
	public void completeGraphTest3()
	{
		DirectedSparseGraph g = buildGraph("A-a->A<-b-A", "completeGraphTest3");Assert.assertFalse(completeGraph(g,"REJECT"));
		checkM(g, "A-b->A-a->A");		
	}
	
	@Test
	public void completeGraphTest4()
	{
		DirectedSparseGraph g = buildGraph("A-a->B-b->A", "completeGraphTest4");Assert.assertTrue(completeGraph(g,"REJECT"));
		checkM(g, "A-a->B-b->A\nA-b-#REJECT#-a-B");		
	}
	
	@Test
	public void completeGraphTest4b()
	{
		DirectedSparseGraph g = buildGraph("A-a->B-b->A-b->A", "completeGraphTest4b");Assert.assertTrue(completeGraph(g,"REJECT"));
		checkM(g, "A-a->B-b->A-b->A\nREJECT#-a-B");		
	}

	@Test
	public void completeGraphTest5()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B", "completeGraphTest5");Assert.assertTrue(completeGraph(g,"REJECT"));
		checkM(g, "A-a->A-b->B-c->B\nA-c-#REJECT#-a-B-b-#REJECT");		
	}	
	
	@Test
	public void completeGraphTest6()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C", "completeGraphTest6");Assert.assertTrue(completeGraph(g,"REJECT"));
		checkM(g, "A-a->A-b->B-c->B-a->C\nA-c-#REJECT#-b-B\nC-a-#REJECT\nC-b-#REJECT\nC-c-#REJECT");		
	}	
	
	@Test
	public void completeGraphTest7()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "completeGraphTest7");Assert.assertTrue(completeGraph(g,"REJECT"));
		final FSMStructure graph = getGraphData(g);
		final FSMStructure expected = getGraphData(buildGraph("A-a->A-b->B-c->B-a->C\nA-c-#REJECT\nA-d-#REJECT\nB-b-#REJECT\nB-d-#REJECT\nC-a-#REJECT\nC-b-#REJECT\nC-c-#REJECT\nC-d-#REJECT\nS-a-#REJECT\nS-b-#REJECT\nS-c-#REJECT\nS-d-#REJECT\nQ-a-#REJECT\nQ-b-#REJECT\nQ-c-#REJECT\nQ-d->S","expected graph"));
		Assert.assertTrue(checkMBoolean(graph,expected,"A","A"));
		Assert.assertTrue(checkMBoolean(graph,expected,"B","B"));
		Assert.assertTrue(checkMBoolean(graph,expected,"Q","Q"));
		Assert.assertTrue(checkMBoolean(graph,expected,"S","S"));
		Assert.assertTrue(checkMBoolean(graph,expected,"REJECT","REJECT"));
	}	

	@Test
	public void testFindVertex1()
	{
		Assert.assertNull(RPNIBlueFringeLearner.findVertex("aa", "bb", new DirectedSparseGraph()));
	}
	
	@Test
	public void testFindVertex2()
	{
		Assert.assertNull(RPNIBlueFringeLearner.findVertex("aa", "bb", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex2")));
	}
		
	@Test
	public void testFindVertex3()
	{
		Assert.assertNull(RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "D", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex3")));
	}

	@Test
	public void testFindVertex4()
	{
		Vertex v = RPNIBlueFringeLearner.findVertex(JUConstants.PROPERTY, JUConstants.INIT, buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex4"));
		Assert.assertEquals("A", v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public void testFindVertex5()
	{
		Vertex v =  RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "A", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex5"));
		Assert.assertEquals("A", v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public void testFindVertex6()
	{
		Vertex v =  RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "C", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex6"));
		Assert.assertEquals("C", v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public void testFindVertex7()
	{
		Vertex v = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "S", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex7"));
		Assert.assertEquals("S", v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public void testFindVertex8()
	{
		Vertex v = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "Q", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex8"));
		Assert.assertEquals("Q", v.getUserDatum(JUConstants.LABEL));
	}

	
	/** Builds a set of sequences from a two-dimensional array, where each element corresponds to a sequence.
	 * 
	 * @param data source data
	 * @return a set of sequences to apply to an RPNI learner
	 */
	public static Set<List<String>> buildSet(String [][] data)
	{
		Set<List<String>> result = new HashSet<List<String>>();
		for(String []seq:data)
		{
			result.add(Arrays.asList(seq));
		}
		return result;
	}
	
	/** Builds a set of sequences from a two-dimensional array, where each element corresponds to a sequence.
	 * 
	 * @param data source data
	 * @return a set of sequences to apply to an RPNI learner
	 */
	public static List<List<String>> buildList(String [][] data)
	{
		List<List<String>> result = new LinkedList<List<String>>();result.addAll(buildSet(data));
		return result;
	}
	
	@Test
	public void testBuildSet1()
	{
		assertTrue(buildSet(new String[] []{}).isEmpty());
	}

	@Test
	public void testBuildSet2()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(new LinkedList<String>());
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{}})));
	}

	@Test
	public void testBuildSet3A()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		expectedResult.add(new LinkedList<String>());
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{},new String[]{"a","b","c"}})));
	}

	@Test
	public void testBuildSet3B()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{"a","b","c"}})));
	}

	@Test
	public void testBuildSet4()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		expectedResult.add(new LinkedList<String>());
		expectedResult.add(Arrays.asList(new String[]{"g","t"}));
		expectedResult.add(Arrays.asList(new String[]{"h","q","i"}));
		assertTrue(expectedResult.equals(buildSet(new String[] []{
				new String[]{"a","b","c"},new String[]{"h","q","i"}, new String[] {},new String[]{"g","t"} })));
	}

	/** Converts a transition into an FSM structure, by taking a copy.
	 * 
	 * @param tTable table, where tTable[source][input]=targetstate
	 * @param vFrom the order in which elements from tTable are to be used.
	 * @param rejectNumber the value of an entry in a tTable which is used to denote an absence of a transition.
	 * @return the constructed transition structure.
	 */
	public static FSMStructure convertTableToFSMStructure(final int [][]tTable, final int []vFrom, int rejectNumber)
	{
		if (vFrom.length == 0 || tTable.length == 0) throw new IllegalArgumentException("array is zero-sized");
		int alphabetSize = tTable[vFrom[0]].length;
		if (alphabetSize == 0) throw new IllegalArgumentException("alphabet is zero-sized");
		String stateName[] = new String[tTable.length];for(int i=0;i < tTable.length;++i) stateName[i]="S"+i;
		String inputName[] = new String[alphabetSize];for(int i=0;i < alphabetSize;++i) inputName[i]="i"+i;
		FSMStructure fsm = new FSMStructure();
		fsm.init = stateName[vFrom[0]];
		Set<String> statesUsed = new HashSet<String>();
		for(int i=0;i<vFrom.length;++i)
		{
			int currentState = vFrom[i];
			if (currentState == rejectNumber) throw new IllegalArgumentException("reject number in vFrom");
			if (tTable[currentState].length != alphabetSize) throw new IllegalArgumentException("rows of inconsistent size");
			Map<String,String> row = new LinkedHashMap<String,String>();
			fsm.accept.put(stateName[currentState], true);
			for(int input=0;input < tTable[currentState].length;++input)
				if (tTable[currentState][input] != rejectNumber)
				{
					int nextState = tTable[currentState][input];
					if (nextState < 0 || nextState > tTable.length)
						throw new IllegalArgumentException("transition from state "+currentState+" leads to an invalid state "+nextState);
					row.put(inputName[input], stateName[nextState]);
					statesUsed.add(stateName[nextState]);
				}
			fsm.trans.put(stateName[currentState], row);
		}
		statesUsed.removeAll(fsm.accept.keySet());
		if (!statesUsed.isEmpty())
			throw new IllegalArgumentException("Some states in the transition table are not included in vFrom");
		return fsm;
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testConvertTableToFSMStructure1a()
	{
		int [][]table = new int[][] {
			{4,5,1,6}, 
			{7,7}
		};
		convertTableToFSMStructure(table, new int[0], -1);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testConvertTableToFSMStructure1b()
	{
		int [][]table = new int[][] {
			{}, 
			{1,1}
		};
		convertTableToFSMStructure(table, new int[]{1,0}, -1);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testConvertTableToFSMStructure2()
	{
		int [][]table = new int[][] {
				{1,0,1,0}, 
				{0,1}
			};
			convertTableToFSMStructure(table, new int[]{0,1}, -1);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testConvertTableToFSMStructure3()
	{
		int [][]table = new int[][] {
				{1,0,1,0}, 
				{0,1,0,1}
			};
			convertTableToFSMStructure(table, new int[]{0,-1}, -1);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testConvertTableToFSMStructure4a()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	2}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		FSMStructure fsm = convertTableToFSMStructure(table, new int[]{0,1,2,3}, -1);
		checkM(fsm, getGraphData(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0\nS2-i0->S0\nS2-i1->S0\nS2-i2->S0\nS2-i3->S0", "testConvertTableToFSMStructure4")), "S0", "S0");
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testConvertTableToFSMStructure4b()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	2}, 
			{0, 3,	0,	-1},
			{0,0,0,-4},
			{-1,-1,-1,-1}
		};
		FSMStructure fsm = convertTableToFSMStructure(table, new int[]{0,1,2,3}, -1);
		checkM(fsm, getGraphData(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0\nS2-i0->S0\nS2-i1->S0\nS2-i2->S0\nS2-i3->S0", "testConvertTableToFSMStructure4")), "S0", "S0");
	}
	
	@Test
	public final void testConvertTableToFSMStructure5()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		FSMStructure fsm = convertTableToFSMStructure(table, new int[]{0,1,3}, -1);
		checkM(fsm, getGraphData(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure4")), "S0", "S0");
	}
	
	@Test
	public final void testConvertTableToFSMStructure6()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		FSMStructure fsm = convertTableToFSMStructure(table, new int[]{1,0,3}, -1);
		checkM(fsm, getGraphData(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure4")), "S0", "S0");
	}

	@Test
	public final void testConvertTableToFSMStructure7()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		FSMStructure fsm = convertTableToFSMStructure(table, new int[]{3,0,1}, -1);
		checkM(fsm, getGraphData(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure4")), "S0", "S0");
	}
	
	@Test
	public final void testConvertTableToFSMStructure8()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		FSMStructure fsm = convertTableToFSMStructure(table, new int[]{3,0,1,0,1,1}, -1);
		checkM(fsm, getGraphData(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure4")), "S0", "S0");
	}

	@Test
	public final void testGetNonRepeatingNumbers0()
	{
		int data[] = DeterministicDirectedSparseGraph.getNonRepeatingNumbers(0, 0); 
		Assert.assertEquals(0,data.length);
	}
	
	@Test
	public final void testGetNonRepeatingNumbers1()
	{
		int data[] = DeterministicDirectedSparseGraph.getNonRepeatingNumbers(1, 0); 
		Assert.assertEquals(1,data.length);Assert.assertEquals(0, data[0]);
	}
	
	@Test
	public final void testGetNonRepeatingNumbers2()
	{
		int data[] = DeterministicDirectedSparseGraph.getNonRepeatingNumbers(2, 0); 
		Assert.assertEquals(2,data.length);
		if (data[0] == 0)
			Assert.assertEquals(1, data[1]);
		else
		{
			Assert.assertEquals(1, data[0]);Assert.assertEquals(0, data[1]);
		}
	}
	
	@Test
	public final void assertsEnabled()
	{
		boolean assertsOn = false;
		assert assertsOn = true;
		
		Assert.assertTrue("asserts have to be enabled", assertsOn);
	}
	
	/** Holds the JFrame to see the graphs being dealt with. Usage:
	 * <pre>
	 * 		updateFrame(g);// a public method
	 * </pre>
	 * where <i>g</i> is the graph to be displayed.
	 */
	protected static Visualiser visFrame = null;
	
	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{		
		visFrame = null;
	}

	@AfterClass
	public static void cleanUp()
	{
		try {
			if (visFrame != null)
			{
				SwingUtilities.invokeAndWait(new Runnable() 
				{
					public void run()
					{
							visFrame.setVisible(false);
							visFrame.dispose();
					}
				});
			}
		} catch (InterruptedException e) {
			// cannot do anything with this
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// cannot do anything with this
			e.printStackTrace();
		}
	}	

	/** In order to be able to use old junit runner. */
	public static junit.framework.Test suite()
	{
		return new JUnit4TestAdapter(TestFSMAlgo.class);
	}
}
