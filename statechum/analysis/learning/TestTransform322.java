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

import static statechum.Helper.checkForCorrectException;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.TestRpniLearner;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.experiments.ExperimentGraphMLHandler;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.io.GraphMLFile;
import statechum.analysis.learning.TestFSMAlgo.DifferentFSMException;
import statechum.xmachine.model.testset.WMethod;

/**
 * @author kirill
 *
 */
public class TestTransform322 {
	private final String relabelFSM = "A-a->B-a->C-b->B-c->B";
	/** Converts the existing checkM into its equivalent in version 322. */
	public static DifferentFSMException checkM(FSMStructure A, String Ainit, FSMStructure B, String Binit)
	{
		DifferentFSMException result = null;
		try {
			TestFSMAlgo.checkM(A, B, Ainit, Binit);
		} catch (DifferentFSMException e) {
			result = e;
		}
		return result;
	}
	
	/** Converts the existing checkM into its equivalent in version 322. */
	public static DifferentFSMException checkM(FSMStructure A, FSMStructure B)
	{
		DifferentFSMException result = null;
		try {
			TestFSMAlgo.checkM(A, B, A.init, B.init);
		} catch (DifferentFSMException e) {
			result = e;
		}
		return result;
	}
	
	/** The standard beginning of our graphML files. */
	public static final String graphML_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><"+Transform322.graphmlNodeName+" xmlns:gml=\"http://graphml.graphdrawing.org/xmlns/graphml\"><graph edgedefault=\"directed\" xmlns=\"gml\">\n";
	/** The standard ending of our graphML files. */
	public static final String graphML_end = "</graph></"+Transform322.graphmlNodeName+">"; 

	protected static final String graphml_beginning = graphML_header+
		"<node VERTEX=\"Initial A\" id=\"A\"/>\n"+
		"<node ";
	
	protected static final String graphml_ending = "id=\"B\"/>\n"+ 
		"<node VERTEX=\"C\" id=\"C\"/>\n"+
		"<edge EDGE=\"a\" directed=\"true\" source=\"A\" target=\"B\"/>\n"+// since I'm using TreeMap, transitions should be alphabetically ordered.
		"<edge EDGE=\"a\" directed=\"true\" source=\"B\" target=\"C\"/>\n"+
		"<edge EDGE=\"c\" directed=\"true\" source=\"B\" target=\"B\"/>\n"+
		"<edge EDGE=\"b\" directed=\"true\" source=\"C\" target=\"B\"/>\n"+
		graphML_end;
	
	@Test(expected=IllegalArgumentException.class)
	public final void testGraphMLwriter_fail() throws IOException
	{
		FSMStructure fsm = new FSMStructure(TestFSMAlgo.buildGraph(relabelFSM.replaceAll("A", Transform322.Initial+"_str"), "testRelabel1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();
		fsm.transform322.writeGraphML(writer);
	}

	/** Removes all whitespace characters from the string, by iterating through it. 
	 * 
	 * @param str string to transform
	 * @return result of transformation
	 */
	public String removeWhiteSpace(String str)
	{
		StringBuffer result = new StringBuffer();for(int i=0;i<str.length();++i) if (!Character.isWhitespace(str.charAt(i))) result.append(str.charAt(i));
		return result.toString();
	}
	
	@Test
	public final void testRemoveWhiteSpace1()
	{
		Assert.assertEquals("",removeWhiteSpace(""));
	}
	
	@Test
	public final void testRemoveWhiteSpace2()
	{
		Assert.assertEquals("test",removeWhiteSpace("test"));
	}
	
	@Test
	public final void testRemoveWhiteSpace3()
	{
		Assert.assertEquals("thisisatest343*()",removeWhiteSpace("this is\r a\n\t test 343 *()\n\n\t"));
	}
	
	@Test
	public final void testGraphMLwriter1() throws IOException
	{
		FSMStructure fsm = new FSMStructure(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();
		fsm.transform322.writeGraphML(writer);
		Assert.assertEquals(removeWhiteSpace(graphml_beginning+"VERTEX=\"B\" "+graphml_ending),
				removeWhiteSpace(writer.toString()));
	}
	
	@Test
	public final void testGraphMLwriter2() throws IOException
	{
		FSMStructure fsm = new FSMStructure(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();
		fsm.accept.put(fsm.findVertex("B"),false);
		fsm.transform322.writeGraphML(writer);
		Assert.assertEquals(removeWhiteSpace(graphml_beginning+
				"VERTEX=\"B\" "+JUConstants.ACCEPTED+"=\"false\""+
				graphml_ending),
				removeWhiteSpace(writer.toString()));
	}
	
	@Test
	public final void testGraphMLwriter_loadnode1() throws IOException
	{
		FSMStructure fsm = new FSMStructure(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
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
		FSMStructure actual = new FSMStructure(Transform322.loadGraph(fsm.transform322.createGraphMLNode(doc)),Configuration.getDefaultConfiguration());
		Assert.assertNull(checkM(fsm,fsm.init,actual,actual.init));
		Assert.assertEquals(fsm.init, actual.init);
	}
	
	@Test
	public final void testGraphMLwriter_loadnode2() throws IOException
	{
		FSMStructure fsm = new FSMStructure(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		fsm.findVertex("B");fsm.findVertex("B");fsm.accept.put(fsm.findVertex("B"),false);
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
		FSMStructure actual = new FSMStructure(Transform322.loadGraph(fsm.transform322.createGraphMLNode(doc)),Configuration.getDefaultConfiguration());
		Assert.assertNull(checkM(fsm,fsm.init,actual,actual.init));
		Assert.assertEquals(fsm.init, actual.init);
	}
	
	/** No graph element. */
	@Test
	public final void testGraphMLwriter_loadnode_fail1a() throws IOException
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
		final Document document = doc;
		checkForCorrectException(new whatToRun() { public void run() {
			Transform322.loadGraph(document.createElement("junk"));
		}},IllegalArgumentException.class,"element does not start with graphml");
	}

	/** No graph element. */
	@Test
	public final void testGraphMLwriter_loadnode_fail2a() throws IOException
	{
		FSMStructure fsm = new FSMStructure(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
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
		final org.w3c.dom.Element elem = fsm.transform322.createGraphMLNode(doc);elem.removeChild(elem.getFirstChild());
		checkForCorrectException(new whatToRun() { public void run() {
			Transform322.loadGraph(elem);
		}},IllegalArgumentException.class,"absent graph element");
	}
	
	/** No graph element. */
	@Test
	public final void testGraphMLwriter_loadnode_fail2b() throws IOException
	{
		FSMStructure fsm = new FSMStructure(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
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
		final org.w3c.dom.Element elem = fsm.transform322.createGraphMLNode(doc);elem.replaceChild(doc.createElement("something"), elem.getFirstChild());
		
		checkForCorrectException(new whatToRun() { public void run() {
			Transform322.loadGraph(elem);
		}},IllegalArgumentException.class,"absent graph element");
	}
	
	/** Duplicate graph elements. */
	@Test
	public final void testGraphMLwriter_loadnode_fail2c() throws IOException
	{
		FSMStructure fsm = new FSMStructure(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
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
		final org.w3c.dom.Element elem = fsm.transform322.createGraphMLNode(doc);elem.appendChild(doc.createElement("graph"));
		
		checkForCorrectException(new whatToRun() { public void run() {
			Transform322.loadGraph(elem);
		}},IllegalArgumentException.class,"duplicate graph element");
	}
	
	/** A helper method which saves a given graph and subsequently verifies that the graph loads back.
	 * 
	 * @param gr the graph to save and then load.
	 * @throws IOException
	 */
	public void checkLoading(FSMStructure gr) throws IOException
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		StringWriter writer = new StringWriter();gr.transform322.writeGraphML(writer);
		FSMStructure loaded = FSMStructure.loadGraph(new StringReader(writer.toString()),config);

		Assert.assertTrue(!WMethod.checkUnreachableStates(gr));Assert.assertTrue(!WMethod.checkUnreachableStates(loaded));
		Assert.assertNull(checkM(gr,loaded));
		
		for(List<String> stateCoverSeq:WMethod.computeStateCover(gr))
		{
			String a = gr.getVertex(stateCoverSeq), b=loaded.getVertex(stateCoverSeq);
			Assert.assertEquals(gr.accept.get(a),loaded.accept.get(b));
		}

	}

	@Test
	public final void testGraphMLWriter3() throws IOException
	{
		FSMStructure fsm = new FSMStructure(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		checkLoading(fsm);
	}
	
	@Test
	public final void testGraphMLWriter4() throws IOException
	{
		FSMStructure fsm = new FSMStructure(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		fsm.accept.put(fsm.findVertex("BB1"),false);
		checkLoading(fsm);
	}

	@Test
	public final void testGraphMLWriter5() throws IOException
	{
		FSMStructure fsm = new FSMStructure(TestFSMAlgo.buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1"),Configuration.getDefaultConfiguration());
		checkLoading(fsm);
	}

	@Test
	public final void testGraphMLWriter_load__despite_Initial() throws IOException
	{
		FSMStructure gr = new FSMStructure(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();gr.transform322.writeGraphML(writer);
		synchronized (computeStateScores.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	GraphMLFile graphmlFile = new GraphMLFile();
	    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
	    	Graph brokenGraph=graphmlFile.load(new StringReader(writer.toString().replace("VERTEX=\"BB1\"", "VERTEX=\""+Transform322.Initial+" BB1\"")));
	    	try
	    	{
	    		new FSMStructure(brokenGraph,Configuration.getDefaultConfiguration());
	    	}
	    	catch(IllegalArgumentException ex)
	    	{
	    		Assert.assertTrue(ex.getMessage().contains("duplicate initial state"));
	    	}
		}		
		
	}
}
