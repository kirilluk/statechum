/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * StateChum. If not, see <http://www.gnu.org/licenses/>.
 */ 
package statechum.analysis.learning.rpnicore;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildGraph;

public class TestEquivalenceChecking {
	public TestEquivalenceChecking()
	{
		mainConfiguration = Configuration.getDefaultConfiguration();
		mainConfiguration.setAllowedToCloneNonCmpVertex(true);
	}
	
	org.w3c.dom.Document doc = null;
	
	/** Make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	@Before
	public void beforeTest()
	{
		config = mainConfiguration.copy();

		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		try
		{
			factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();
		}
		catch(ParserConfigurationException e)
		{
			statechum.Helper.throwUnchecked("failed to construct DOM document",e);
		}
	}

	/** The configuration to use when running tests. */
	Configuration config = null, mainConfiguration = null;

	/** Checks if the passed graph is isomorphic to the provided fsm
	 * 
	 * @param g graph to check
	 * @param fsm the string representation of the machine which the graph should be isomorphic to
	 */
	public void checkEq(DirectedSparseGraph g, String fsm)
	{
		DirectedSparseGraph expectedGraph = buildGraph(fsm,"expected graph");
		final LearnerGraph graph = new LearnerGraph(g,Configuration.getDefaultConfiguration());
		final LearnerGraph expected = new LearnerGraph(expectedGraph,Configuration.getDefaultConfiguration());
		
		assertEquals("incorrect data",true,expected.equals(graph));
	}

	@Test
	public void testCheckEq()
	{
		DirectedSparseGraph g=buildGraph("P-a->Q_State-b->P-c->P","testCheckEq");
		checkEq(g,"P-c->P<-b-Q_State<-a-P");
	}
	
	/** Verifies the equivalence of a supplied graph to the supplied machine. */
	public static void checkM(String fsm,DirectedSparseGraph g,Configuration conf)
	{
		final LearnerGraphND graph = new LearnerGraphND(g,conf);
		final DirectedSparseGraph expectedGraph = buildGraph(fsm,"expected graph");
		final LearnerGraphND expected = new LearnerGraphND(expectedGraph,conf);
		DifferentFSMException result = WMethod.checkM(expected,graph);
		Assert.assertNull(result==null?"":result.toString(),result);
	}

	/** Verifies the equivalence of a supplied graph to the supplied machine, 
	 * the special version for testing the above method. */
	public static void checkM_test(String fsm,DirectedSparseGraph g,Configuration conf)
	{
		final LearnerGraph graph = new LearnerGraph(g,conf);
		final DirectedSparseGraph expectedGraph = buildGraph(fsm,"expected graph");
		final LearnerGraph expected = new LearnerGraph(expectedGraph,conf);
		throw WMethod.checkM(graph,expected);
	}

	@Test
	public void testCheckM1()
	{
		checkM("B-a->C-b->D", buildGraph("A-a->B-b->C", "testCheck1"),config);
	}
	
	@Test
	public void testCheckM2()
	{
		checkM("B-a->C-b->D\nB-b-#REJ\nD-d-#REJ",buildGraph("A-a->B-b->C-d-#F#-b-A", "testCheck2"), config);
	}

	@Test
	public void testCheckM3()
	{
		String another  = "A-a->B-b->C\nC-b-#REJ\nA-d-#REJ";
		String expected = "A-a->B-b->C-b-#F#-d-A";
		checkM(expected,buildGraph(another.replace('A', 'Q').replace('B', 'G').replace('C', 'A'), "testCheck3"), config);
	}

	/** multiple reject states. */
	@Test
	public void testCheckM4()
	{
		String another  = "A-a->B-b->C\nC-b-#REJ\nA-d-#REJ\nA-b-#REJ2\nB-a-#REJ2\nB-c-#REJ3";
		String expected = "A-a->B-b->C-b-#F#-d-A-b-#R\nB-a-#R\nU#-c-B";
		checkM(expected,buildGraph(another.replace('A', 'Q').replace('B', 'G').replace('C', 'A'), "testCheck4"), config);
	}

	/** multiple reject states and a non-deterministic graph. */
	@Test
	public void testCheckM4_ND()
	{
		String another  = "A-a->B-b->C\nC-b-#REJ\nA-d-#REJ\nA-b-#REJ2\nB-a-#REJ2\nB-c-#REJ3\n"+
			"A-d-#REJ\nB-c-#REJ2";
		String expected = "A-a->B-b->C-b-#F#-d-A-b-#R\nB-a-#R\nU#-c-B\n"+
			"A-d-#F\nB-c-#R";
		checkM(expected,buildGraph(another.replace('A', 'Q').replace('B', 'G').replace('C', 'A'), "testCheck4"), config);
	}

	@Test
	public void testCheckM5()
	{
		checkM("S-a->U<-b-U\nQ<-a-U",buildGraph("A-a->B-b->B-a->C", "testCheck5"), config);
	}

	@Test
	public void testCheckM6()
	{
		final LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-b->B-a->C", "testCheck6"),config);
		final LearnerGraph expected = new LearnerGraph(buildGraph("U<-b-U\nQ<-a-U<-a-S","expected graph"),config);
		Assert.assertNull(WMethod.checkM(graph,graph.findVertex("A"),expected,expected.findVertex("S"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(graph,graph.findVertex("B"),expected,expected.findVertex("U"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(graph,graph.findVertex("C"),expected,expected.findVertex("Q"),WMethod.VERTEX_COMPARISON_KIND.NONE));
	}

	@Test
	public final void testCheckM_multipleEq1() // equivalent states
	{
		final LearnerGraph graph = new LearnerGraph(buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckM_multipleEq1"),config);
		Assert.assertNull(WMethod.checkM(graph,graph.findVertex("D"),graph,graph.findVertex("C2"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(graph,graph.findVertex("C2"),graph,graph.findVertex("D"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		
		Assert.assertNull(WMethod.checkM(graph,graph.findVertex("D1"),graph,graph.findVertex("D2"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(graph,graph.findVertex("D2"),graph,graph.findVertex("D1"),WMethod.VERTEX_COMPARISON_KIND.NONE));

		Assert.assertNull(WMethod.checkM(graph,graph.findVertex("D2"),graph,graph.findVertex("K2"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(graph,graph.findVertex("K2"),graph,graph.findVertex("D2"),WMethod.VERTEX_COMPARISON_KIND.NONE));

		Assert.assertNotNull(WMethod.checkM(graph,graph.findVertex("D2"),graph,graph.findVertex("A1"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNotNull(WMethod.checkM(graph,graph.findVertex("A1"),graph,graph.findVertex("D2"),WMethod.VERTEX_COMPARISON_KIND.NONE));

		Assert.assertNotNull(WMethod.checkM(graph,graph.findVertex("D2"),graph,graph.findVertex("F1"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNotNull(WMethod.checkM(graph,graph.findVertex("F1"),graph,graph.findVertex("D2"),WMethod.VERTEX_COMPARISON_KIND.NONE));
	}

	@Test
	public final void testCheckM_multipleEq2() // equivalent states
	{
		final DirectedSparseGraph g = buildGraph("S-a->A-a->D-a->D-b->A-b->B-a->D\nB-b->C-a->D\nC-b->D\nS-b->N-a->N-b->N","testCheckM_multipleEq2");
		final LearnerGraph graph = new LearnerGraph(g,Configuration.getDefaultConfiguration());
		List<String> states = Arrays.asList(new String[]{"S","A","B","C","D","N"});
		for(String stA:states)
			for(String stB:states)
				Assert.assertNull("states "+stA+"and "+stB+" should be equivalent",
						WMethod.checkM(graph,graph.findVertex(stA),graph,graph.findVertex(stB),WMethod.VERTEX_COMPARISON_KIND.NONE));
	}
	
	@Test
	public final void testCheckM_multipleEq3() // equivalent states
	{
		final DirectedSparseGraph g = buildGraph("S-a->A-a->D-a->D-b->A-b->B-a->D\nB-b->C-a->D\nC-b->D\nS-b->N-a->M-a->N\nN-b->M-b->N","testCheckM_multipleEq3");
		final LearnerGraph graph = new LearnerGraph(g,Configuration.getDefaultConfiguration());
		List<String> states = Arrays.asList(new String[]{"S","A","B","C","D","N","M"});
		for(String stA:states)
			for(String stB:states)
				Assert.assertNull("states "+stA+"and "+stB+" should be equivalent",
						WMethod.checkM(graph,graph.findVertex(stA),graph,graph.findVertex(stB),WMethod.VERTEX_COMPARISON_KIND.NONE));
	}
	
	@Test
	public final void testCheckM_multipleEq4() // non-equivalent states
	{
		final DirectedSparseGraph g = buildGraph("A-a->B-a->C-a->A-b->C-b->B","testCheckM_multipleEq4");
		final LearnerGraph graph = new LearnerGraph(g,Configuration.getDefaultConfiguration());
		List<String> states = Arrays.asList(new String[]{"A","B","C"});
		for(String stA:states)
			for(String stB:states)
				if (stA.equals(stB))
					Assert.assertNull("states "+stA+" and "+stB+" should be equivalent",
							WMethod.checkM(graph,graph.findVertex(stA),graph,graph.findVertex(stB),WMethod.VERTEX_COMPARISON_KIND.NONE));
				else
					Assert.assertNotNull("states "+stA+" and "+stB+" should not be equivalent",
							WMethod.checkM(graph,graph.findVertex(stA),graph,graph.findVertex(stB),WMethod.VERTEX_COMPARISON_KIND.NONE));
	}
	
	@Test
	public void testCheckM6_f1()
	{
		final LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-b->B-a->C", "testCheck6"), Configuration.getDefaultConfiguration());
		final LearnerGraph expected = new LearnerGraph(buildGraph("U<-b-U\nQ<-a-U<-a-S","expected graph"),Configuration.getDefaultConfiguration());
		Assert.assertNull(WMethod.checkM(graph,graph.findVertex("A"),graph,graph.findVertex("A"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(graph,graph.findVertex("B"),graph,graph.findVertex("B"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(graph,graph.findVertex("C"),graph,graph.findVertex("C"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(expected,expected.findVertex("Q"),expected,expected.findVertex("Q"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(expected,expected.findVertex("S"),expected,expected.findVertex("S"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		
		Assert.assertNotNull(WMethod.checkM(graph,graph.findVertex("A"),expected,expected.findVertex("Q"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNotNull(WMethod.checkM(graph,graph.findVertex("A"),expected,expected.findVertex("U"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNotNull(WMethod.checkM(graph,graph.findVertex("B"),expected,expected.findVertex("Q"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNotNull(WMethod.checkM(graph,graph.findVertex("B"),expected,expected.findVertex("S"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNotNull(WMethod.checkM(graph,graph.findVertex("C"),expected,expected.findVertex("U"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNotNull(WMethod.checkM(graph,graph.findVertex("C"),expected,expected.findVertex("S"),WMethod.VERTEX_COMPARISON_KIND.NONE));
	}
	

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD1()
	{
		checkM_test("B-a->C-b->B",buildGraph("A-a->B-b->C", "testCheckMD1"), config);		
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD2() // different reject states
	{
		checkM_test("B-a->C-b-#D",buildGraph("A-a->B-b->C", "testCheckMD2"), config);
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD3() // missing transition
	{
		checkM_test("B-a->C-b->D",buildGraph("A-a->B-b->C\nA-b->B", "testCheckMD3"), config);
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD4() // extra transition
	{
		checkM_test("B-a->C-b->D\nB-b->C",buildGraph("A-a->B-b->C", "testCheckMD4"), config);
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD5() // missing transition
	{
		checkM_test("B-a->C-b->D",buildGraph("A-a->B-b->C\nB-c->B", "testCheckMD5"), config);
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD6() // extra transition
	{
		checkM_test("B-a->C-b->D\nC-c->C",buildGraph("A-a->B-b->C", "testCheckMD6"), config);
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD7() // swapped transitions
	{
		String another  = "A-a->B-b->C\nC-b-#REJ\nA-d-#REJ";
		String expected = "A-a->B-b->C-d-#F#-b-A";
		checkM_test(expected,buildGraph(another.replace('A', 'Q').replace('B', 'G').replace('C', 'A'), "testCheckMD7"), config);
	}


}
