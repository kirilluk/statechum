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

import static statechum.Helper.checkForCorrectException;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;

import statechum.ArrayOperations;
import statechum.Configuration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.DeterministicDirectedSparseGraph.CmpVertex.IllegalUserDataException;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.TestFSMAlgo;
import static statechum.analysis.learning.TestFSMAlgo.buildGraph;
import statechum.analysis.learning.TestRpniLearner;
import statechum.analysis.learning.experiments.ExperimentGraphMLHandler;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import edu.uci.ics.jung.exceptions.FatalException;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.io.GraphMLFile;

/**
 * @author kirill
 *
 */
public class TestTransform322 {
	private LearnerGraph g = new LearnerGraph(buildGraph("A-a->A-b->B",	"testToBooleans"),Configuration.getDefaultConfiguration());
	private StringBuffer resultDescr = new StringBuffer();	
	private List<List<String>> buildListList(String [][]list_of_seq)
	{
		List<List<String>> result = new LinkedList<List<String>>();
		for(String []seq:list_of_seq)
			result.add(Arrays.asList(seq));
		return result;
	}
	
	@Test
	public final void testToBooleans0()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{},
				Transform.wToBooleans(g,g.findVertex("A"),
				buildListList(new String [][]{})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}
	
	@Test
	public final void testToBooleans1()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{true,true},
				Transform.wToBooleans(g,g.findVertex("A"),
				buildListList(new String [][]{
						new String[] {"a"},
						new String[] {"b"}
				})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}

	@Test
	public final void testToBooleans2()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{true,false},
				Transform.wToBooleans(g,g.findVertex("A"),
				buildListList(new String [][]{
						new String[] {"a"},
						new String[] {"c"}
				})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}

	@Test
	public final void testToBooleans3()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{true,false,false,true},
				Transform.wToBooleans(g,g.findVertex("A"),
				buildListList(new String [][]{
						new String[] {"a"},
						new String[] {"b","b"},
						new String[] {"a","a","a","c"},
						new String[] {"a","a","a","b"},
				})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}

	@Test
	public final void testToBooleans4()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{false,false,false,false},
				Transform.wToBooleans(g,g.findVertex("B"),
				buildListList(new String [][]{
						new String[] {"a"},
						new String[] {"b","b"},
						new String[] {"a","a","a","c"},
						new String[] {"a","a","a","b"},
				})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}
	
	private final String relabelFSM = "A-a->B-a->C-b->B-c->B";
	
	@Test(expected=IllegalArgumentException.class)
	public final void testRelabel_fail1()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		Transform.relabel(fsm,-1,"c");
	}
	
	@Test
	public final void testRelabel0_1()
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.wmethod.computeAlphabet();Assert.assertTrue(origAlphabet.isEmpty());
		Transform.relabel(fsm,-1,"new");Set<String> newAlphabet = fsm.wmethod.computeAlphabet();Assert.assertTrue(newAlphabet.isEmpty());
	}
	
	@Test
	public final void testRelabel0_2()
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.wmethod.computeAlphabet();Assert.assertTrue(origAlphabet.isEmpty());
		Transform.relabel(fsm,0,"new");Set<String> newAlphabet = fsm.wmethod.computeAlphabet();Assert.assertTrue(newAlphabet.isEmpty());
	}
	@Test
	public final void testRelabel0_3()
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.wmethod.computeAlphabet();Assert.assertTrue(origAlphabet.isEmpty());
		Transform.relabel(fsm,1,"new");Set<String> newAlphabet = fsm.wmethod.computeAlphabet();Assert.assertTrue(newAlphabet.isEmpty());
	}
	
	@Test
	public final void testRelabel1_1()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.wmethod.computeAlphabet();
		Transform.relabel(fsm,-1,"new");Set<String> newAlphabet = fsm.wmethod.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<String> diffOrigNew = new TreeSet<String>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertTrue(diffOrigNew.isEmpty());		
	}

	@Test
	public final void testRelabel1_2()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.wmethod.computeAlphabet();
		Transform.relabel(fsm,0,"new");Set<String> newAlphabet = fsm.wmethod.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<String> diffOrigNew = new TreeSet<String>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertTrue(diffOrigNew.isEmpty());		
	}

	@Test
	public final void testRelabel2()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.wmethod.computeAlphabet();
		Transform.relabel(fsm,1,"new");Set<String> newAlphabet = fsm.wmethod.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<String> diffOrigNew = new TreeSet<String>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertEquals(1,diffOrigNew.size());		
	}

	@Test
	public final void testRelabel3()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.wmethod.computeAlphabet();
		Transform.relabel(fsm,2,"new");Set<String> newAlphabet = fsm.wmethod.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<String> diffOrigNew = new TreeSet<String>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertEquals(2,diffOrigNew.size());		
	}


	@Test
	public final void testRelabel4()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.wmethod.computeAlphabet();
		Transform.relabel(fsm,origAlphabet.size(),"new");Set<String> newAlphabet = fsm.wmethod.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Assert.assertTrue(newAlphabet.equals(origAlphabet));		
	}

	/** Converts the existing checkM into its eqivalent in version 322. */
	public static DifferentFSMException checkM(LearnerGraph A, CmpVertex Ainit, LearnerGraph B, CmpVertex Binit)
	{
		DifferentFSMException result = null;
		try {
			WMethod.checkM(A, B, Ainit, Binit);
		} catch (DifferentFSMException e) {
			result = e;
		}
		return result;
	}
	
	/** Converts the existing checkM into its eqivalent in version 322. */
	public static DifferentFSMException checkM(LearnerGraph A, LearnerGraph B)
	{
		DifferentFSMException result = null;
		try {
			WMethod.checkM(A, B);
		} catch (DifferentFSMException e) {
			result = e;
		}
		return result;
	}
	
	@Test
	public final void testAddToGraph0_1()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = new LearnerGraph(Configuration.getDefaultConfiguration());
		fsmToAdd.init.setColour(JUConstants.BLUE);fsmToAdd.init.setHighlight(true);
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = Transform322.addToGraph(fsm, fsmToAdd,oldToNew);
		
		Assert.assertNull(checkM(fsmSrc,fsmSrc.init, fsm,fsm.init));
		Assert.assertNull(checkM(fsmToAdd,fsmToAdd.init,fsm,newA));
		Assert.assertEquals(JUConstants.BLUE, newA.getColour());
		Assert.assertTrue(newA.isHighlight());
		Assert.assertEquals(1, oldToNew.size());Assert.assertSame(newA, oldToNew.get(fsmToAdd.init));
		
		Assert.assertFalse(fsm.transitionMatrix.get(fsm.init).isEmpty());
		Assert.assertTrue(fsm.transitionMatrix.get(newA).isEmpty());
	}

	@Test
	public final void testAddToGraph0_2()
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		fsmToAdd.init.setColour(JUConstants.BLUE);fsmToAdd.init.setHighlight(true);
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = Transform322.addToGraph(fsm, fsmToAdd,oldToNew);
		
		Assert.assertNull(checkM(fsmSrc,fsmSrc.init, fsm,fsm.init));
		Assert.assertNull(checkM(fsmToAdd,fsmToAdd.init,fsm,newA));
		Assert.assertTrue(fsm.transitionMatrix.get(fsm.init).isEmpty());
		Assert.assertFalse(fsm.transitionMatrix.get(newA).isEmpty());

		Assert.assertEquals(fsmToAdd.getStateNumber(), oldToNew.size());
		Assert.assertSame(newA, oldToNew.get(fsmToAdd.init));
		for(CmpVertex oldVert:fsmToAdd.transitionMatrix.keySet())
			Assert.assertNull(checkM(fsm,oldToNew.get(oldVert),fsmToAdd,oldVert));
	}

	@Test
	public final void testAddToGraph0_3()
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = new LearnerGraph(Configuration.getDefaultConfiguration());
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = Transform322.addToGraph(fsm, fsmToAdd,oldToNew);

		Assert.assertNull(checkM(fsmSrc,fsmSrc.init, fsm,fsm.init));
		Assert.assertNull(checkM(fsmToAdd,fsmToAdd.init,fsm,newA));
		
		Assert.assertTrue(fsm.transitionMatrix.get(fsm.init).isEmpty());
		Assert.assertTrue(fsm.transitionMatrix.get(newA).isEmpty());

		Assert.assertEquals(fsmToAdd.getStateNumber(), oldToNew.size());
		Assert.assertSame(newA, oldToNew.get(fsmToAdd.init));
		for(CmpVertex oldVert:fsmToAdd.transitionMatrix.keySet())
			Assert.assertNull(checkM(fsm,oldToNew.get(oldVert),fsmSrc,oldVert));
	}

	@Test
	public final void testAddToGraph1()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a-#Q", "testAddToGraph1"),Configuration.getDefaultConfiguration());
		fsmToAdd.init.setColour(JUConstants.BLUE);fsmToAdd.init.setHighlight(true);
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = Transform322.addToGraph(fsm, fsmToAdd,oldToNew);

		Assert.assertNull(checkM(fsmSrc,fsmSrc.init, fsm,fsm.init));
		Assert.assertNull(checkM(fsmToAdd,fsmToAdd.init,fsm,newA));
		Assert.assertEquals(JUConstants.BLUE, newA.getColour());
		Assert.assertTrue(newA.isHighlight());

		Assert.assertEquals(fsmToAdd.getStateNumber(), oldToNew.size());
		Assert.assertSame(newA, oldToNew.get(fsmToAdd.init));
		for(CmpVertex oldVert:fsmToAdd.transitionMatrix.keySet())
			Assert.assertNull(checkM(fsm,oldToNew.get(oldVert),fsmToAdd,oldVert));
	}
	
	@Test
	public final void testAddToGraph2()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->Q", "testAddToGraph1"),Configuration.getDefaultConfiguration());
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = Transform322.addToGraph(fsm, fsmToAdd,oldToNew);

		Assert.assertNull(checkM(fsmSrc,fsmSrc.init, fsm,fsm.init));
		Assert.assertNull(checkM(fsmToAdd,fsmToAdd.init,fsm,newA));
		
		StatePair whatToMerge = new StatePair(fsm.init,newA);
		LinkedList<Collection<CmpVertex>> collectionOfVerticesToMerge = new LinkedList<Collection<CmpVertex>>();
		Assert.assertTrue(0 < fsm.pairscores.computePairCompatibilityScore_general(whatToMerge,collectionOfVerticesToMerge));
		LearnerGraph result = MergeStates.mergeAndDeterminize_general(fsm, whatToMerge,collectionOfVerticesToMerge);
		Assert.assertNull(checkM(fsmSrc,fsmSrc.init,result,result.init));

		Assert.assertEquals(fsmToAdd.getStateNumber(), oldToNew.size());
		Assert.assertSame(newA, oldToNew.get(fsmToAdd.init));
		for(CmpVertex oldVert:fsmToAdd.transitionMatrix.keySet())
			Assert.assertNull(checkM(fsm,oldToNew.get(oldVert),fsmToAdd,oldVert));
	}

	/** The standard beginning of our graphML files. */
	public static final String graphML_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><"+Transform322.graphmlNodeName+" xmlns:gml=\"http://graphml.graphdrawing.org/xmlns/graphml\"><graph edgedefault=\"directed\" xmlns=\"gml\">\n";
	/** The standard ending of our graphML files. */
	public static final String graphML_end = "</graph></"+Transform322.graphmlNodeName+">"; 

	protected static final String graphml_beginning = graphML_header+
		"<node "+JUConstants.COLOUR.name()+"=\""+JUConstants.RED.name()+"\" VERTEX=\"Initial A\" id=\"A\"/>\n"+
		"<node ";
	
	protected static final String graphml_ending = "VERTEX=\"B\" id=\"B\"/>\n"+ 
		"<node VERTEX=\"C\" id=\"C\"/>\n"+
		"<edge EDGE=\"a\" directed=\"true\" source=\"A\" target=\"B\"/>\n"+// since I'm using TreeMap, transitions should be alphabetically ordered.
		"<edge EDGE=\"a\" directed=\"true\" source=\"B\" target=\"C\"/>\n"+
		"<edge EDGE=\"c\" directed=\"true\" source=\"B\" target=\"B\"/>\n"+
		"<edge EDGE=\"b\" directed=\"true\" source=\"C\" target=\"B\"/>\n"+
		graphML_end;
	
	@Test(expected=IllegalArgumentException.class)
	public final void testGraphMLwriter_fail() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM.replaceAll("A", Transform322.Initial+"_str"), "testRelabel1"),Configuration.getDefaultConfiguration());
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
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();
		fsm.transform322.writeGraphML(writer);
		Assert.assertEquals(removeWhiteSpace(graphml_beginning+graphml_ending),
				removeWhiteSpace(writer.toString()));
	}
	
	@Test
	public final void testGraphMLwriter2() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();
		fsm.findVertex("B").setColour(JUConstants.BLUE);fsm.findVertex("B").setHighlight(true);fsm.findVertex("B").setAccept(false);
		fsm.transform322.writeGraphML(writer);
		Assert.assertEquals(removeWhiteSpace(graphml_beginning+
				" "+JUConstants.ACCEPTED.name()+"=\"false\""+
				" "+JUConstants.COLOUR.name()+"=\""+JUConstants.BLUE.name()+"\""+
				" "+JUConstants.HIGHLIGHT.name()+"=\"true\""+
				graphml_ending),
				removeWhiteSpace(writer.toString()));
	}
	
	@Test
	public final void testGraphMLwriter_loadnode1() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
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
		LearnerGraph actual = new LearnerGraph(Transform322.loadGraph(fsm.transform322.createGraphMLNode(doc)),Configuration.getDefaultConfiguration());
		Assert.assertNull(checkM(fsm,fsm.init,actual,actual.init));
		Assert.assertEquals(fsm.init, actual.init);
	}
	
	@Test
	public final void testGraphMLwriter_loadnode2() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		fsm.findVertex("B").setColour(JUConstants.BLUE);fsm.findVertex("B").setHighlight(true);fsm.findVertex("B").setAccept(false);
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
		LearnerGraph actual = new LearnerGraph(Transform322.loadGraph(fsm.transform322.createGraphMLNode(doc)),Configuration.getDefaultConfiguration());
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
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
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
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
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
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
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
	public void checkLoading(LearnerGraph gr) throws IOException
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		StringWriter writer = new StringWriter();gr.transform322.writeGraphML(writer);
		LearnerGraph loaded = LearnerGraph.loadGraph(new StringReader(writer.toString()),config);

		Assert.assertTrue(!gr.wmethod.checkUnreachableStates());Assert.assertTrue(!loaded.wmethod.checkUnreachableStates());
		Assert.assertNull(checkM(gr,loaded));
		for(Entry<CmpVertex,LinkedList<String>> entry:gr.wmethod.computeShortPathsToAllStates().entrySet())
		{
			CmpVertex v=entry.getKey(),other = loaded.paths.getVertex(entry.getValue());
			Assert.assertEquals(v.isAccept(),other.isAccept());
			Assert.assertEquals(v.isHighlight(),other.isHighlight());
			if (v.getColour() == null) 
				Assert.assertNull(other.getColour());
			else
				Assert.assertEquals(v.getColour(), other.getColour());
		}

		Assert.assertTrue(ids_are_valid(loaded));
	}

	/** Checks if simply creating vertices we may get an ID with existing number. */
	protected static final boolean ids_are_valid(LearnerGraph gr)
	{
		final Configuration config = gr.config.copy();config.setLearnerCloneGraph(false);
		LearnerGraph graph = gr.copy(config);
		
		// Now we check that we can create lots of vertices and none of their numbers 
		// will be included 
		{
			LearnerGraph Plus = graph.copy(config);
			for(int i=0;i<1000;++i)
			{
				VertexID id = Plus.nextID(true);
				if (graph.findVertex(id) != null) return false;
			}
		}

		{
			LearnerGraph Minus = graph.copy(config);
			for(int i=0;i<1000;++i)
			{
				VertexID id = Minus.nextID(false);
				if (graph.findVertex(id) != null) return false;
			}
		}

		return true;
	}
	
	@Test
	public final void testConvertToNumeric1()
	{
		LearnerGraph graph = Transform322.convertToNumerical(new LearnerGraph(Configuration.getDefaultConfiguration()));
		Assert.assertTrue(graph.wmethod.checkGraphNumeric());
		Assert.assertTrue(ids_are_valid(graph));
	}
	
	@Test
	public final void testConvertToNumeric2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B", "testConvertToNumeric2"),Configuration.getDefaultConfiguration());
		LearnerGraph graph = Transform322.convertToNumerical(gr);
		Assert.assertFalse(gr.wmethod.checkGraphNumeric());
		Assert.assertTrue(graph.wmethod.checkGraphNumeric());
		Assert.assertTrue(ids_are_valid(gr));
		Assert.assertTrue(ids_are_valid(graph));
		LearnerGraph graph2 = Transform322.convertToNumerical(graph);
		Assert.assertTrue(graph2.wmethod.checkGraphNumeric());
		Assert.assertTrue(ids_are_valid(graph2));
	}
	
	@Test
	public final void testConvertToNumeric3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a-#B\nA-b->C-a->D-a-#E\nD-b->A\nC-c->C", "testConvertToNumeric2"),Configuration.getDefaultConfiguration());
		LearnerGraph graph = Transform322.convertToNumerical(gr);
		Assert.assertFalse(gr.wmethod.checkGraphNumeric());
		Assert.assertTrue(graph.wmethod.checkGraphNumeric());
		Assert.assertTrue(ids_are_valid(gr));
		Assert.assertTrue(ids_are_valid(graph));
		LearnerGraph graph2 = Transform322.convertToNumerical(graph);
		Assert.assertTrue(graph2.wmethod.checkGraphNumeric());
		Assert.assertTrue(ids_are_valid(graph2));
	}
		
	@Test
	public final void testGraphMLWriter3() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		checkLoading(fsm);
	}
	
	@Test
	public final void testGraphMLWriter4() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		fsm.findVertex("BD2").setHighlight(true);
		fsm.findVertex("BB1").setAccept(false);fsm.findVertex("BB1").setColour(JUConstants.RED);
		fsm.findVertex("B").setColour(JUConstants.RED);
		fsm.findVertex("A").setColour(JUConstants.BLUE);
		checkLoading(fsm);
	}

	@Test
	public final void testGraphMLWriter5() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1"),Configuration.getDefaultConfiguration());
		checkLoading(fsm);
	}
	
	@Test
	public final void testGraphMLWriter_fail_on_load_boolean() throws IOException
	{
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();gr.transform322.writeGraphML(writer);
		synchronized (LearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	GraphMLFile graphmlFile = new GraphMLFile();
	    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
	    	try
	    	{
	    		new LearnerGraph(graphmlFile.load(new StringReader(writer.toString().replace("ACCEPTED=\"false\"", "ACCEPTED=\"aa\""))),Configuration.getDefaultConfiguration());
	    	}
	    	catch(FatalException ex)
	    	{
	    		Assert.assertTrue(ex.getCause() instanceof IllegalUserDataException);
	    	}
		}		
		
	}
	
	@Test
	public final void testGraphMLWriter_fail_on_load_colour() throws IOException
	{
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();gr.transform322.writeGraphML(writer);
		synchronized (LearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	GraphMLFile graphmlFile = new GraphMLFile();
	    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
	    	
	    	try
	    	{
	    		new LearnerGraph(graphmlFile.load(new StringReader(writer.toString().replace("COLOUR=\"red\"", "COLOUR=\"aa\""))),Configuration.getDefaultConfiguration());
	    	}
	    	catch(FatalException ex)
	    	{
	    		Assert.assertTrue(ex.getCause() instanceof IllegalUserDataException);
	    	}
		}		
		
	}
	
	@Test
	public final void testGraphMLWriter_load__despite_Initial() throws IOException
	{
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();gr.transform322.writeGraphML(writer);
		synchronized (LearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	GraphMLFile graphmlFile = new GraphMLFile();
	    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
	    	Graph brokenGraph=graphmlFile.load(new StringReader(writer.toString().replace("VERTEX=\"BB1\"", "VERTEX=\""+Transform322.Initial+" BB1\"")));
	    	try
	    	{
	    		new LearnerGraph(brokenGraph,Configuration.getDefaultConfiguration());
	    	}
	    	catch(IllegalArgumentException ex)
	    	{
	    		Assert.assertTrue(ex.getMessage().contains("are both labelled as initial"));
	    	}
		}		
		
	}
}
