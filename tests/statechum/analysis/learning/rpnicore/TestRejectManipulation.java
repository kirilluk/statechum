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

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import static statechum.analysis.learning.rpnicore.FsmParser.buildGraph;
import static statechum.analysis.learning.rpnicore.TestEquivalenceChecking.checkM;

public class TestRejectManipulation {
	public TestRejectManipulation()
	{
		mainConfiguration = Configuration.getDefaultConfiguration().copy();
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

	/** Given a graph, this one calls completeGraph and checks that the returned value is whetherToBeCompleted
	 * and that the final graph is equivalent to the one provided. Note that this also checks that in case
	 * a graph does not need to be completed, the result of completion is the same graph.
	 * 
	 * @param originalGraph the graph to complete
	 * @param expectedOutcome the graph to be expected
	 * @param whetherToBeCompleted whether graph "machine" is incomplete.
	 * @param testName the name of the test to give to the above graph.
	 */
	private void completeGraphTestHelper(String originalGraph, String expectedOutcome, boolean whetherToBeCompleted, String testName)
	{
		DirectedSparseGraph g = buildGraph(originalGraph, testName);
		Assert.assertEquals(whetherToBeCompleted,DeterministicDirectedSparseGraph.completeGraph(g,"REJECT"));checkM(expectedOutcome,g, config);		
		LearnerGraphND fsm = new LearnerGraphND(buildGraph(originalGraph, testName),config);
		Assert.assertEquals(whetherToBeCompleted,fsm.pathroutines.completeGraph(
				new VertexID("REJECT")));
		Assert.assertNull(WMethod.checkM(fsm, new LearnerGraphND(buildGraph(expectedOutcome,testName),config)));				
	}
	
	/** Checks that passing a name of an existing state causes an exception to be thrown. */
	@Test(expected=IllegalArgumentException.class)
	public void complete_fail()
	{
		new LearnerGraph(buildGraph("A-a->A-b->B-c->B", "complete_fail"),config).pathroutines.completeGraph(
				new VertexID("B"));
	}
	
	@Test
	public void completeGraphTest1()
	{
		completeGraphTestHelper("A-a->A", "A-a->A",false,"completeGraphTest1");
	}
	
	@Test
	public void completeGraphTest2()
	{
		completeGraphTestHelper("A-a->B-a->A", "A-a->A", false, "completeGraphTest2");
	}
	
	@Test
	public void completeGraphTest3()
	{
		completeGraphTestHelper("A-a->A<-b-A", "A-b->A-a->A", false, "completeGraphTest3");
	}
	
	@Test
	public void completeGraphTest4a()
	{
		completeGraphTestHelper("A-a->B-b->A", "A-a->B-b->A\nA-b-#REJECT#-a-B", true, "completeGraphTest4a");
	}
	
	@Test
	public void completeGraphTest4b()
	{
		completeGraphTestHelper("A-a->B-b->A-b->A", "A-a->B-b->A-b->A\nREJECT#-a-B", true, "completeGraphTest4b");
	}

	@Test
	public void completeGraphTest5()
	{
		completeGraphTestHelper("A-a->A-b->B-c->B", "A-a->A-b->B-c->B\nA-c-#REJECT#-a-B-b-#REJECT", true, "completeGraphTest5");
	}	
	
	@Test
	public void completeGraphTest6()
	{
		completeGraphTestHelper("A-a->A-b->B-c->B-a->C", "A-a->A-b->B-c->B-a->C\nA-c-#REJECT#-b-B\nC-a-#REJECT\nC-b-#REJECT\nC-c-#REJECT", true, "completeGraphTest6");
	}	
	
	/** It is very hard to find good examples for completing a non-deterministic graph because the completeGraph
	 * routine simply adds transitions to reject state and since during conversion to a deterministic graph
	 * vertices get mixed, the most likely outcome is a graph which cannot be made deterministic due to 
	 * an inconsistency.
	 * @throws IncompatibleStatesException
	 */
	@Test
	public void completeGraphTest6_ND()
	{
		//String expectedGraph = "A-a->AB-a->AC\nA-b->B-c->B-a->C\nAB-c->B\nA-c-#REJECT#-b-B\nC-a-#REJECT\nC-b-#REJECT\nC-c-#REJECT\nAC-a-#REJECT\nAC-b-#REJECT\nAC-c-#REJECT\nAB-b-#REJECT";
		LearnerGraphND graph = new LearnerGraphND(buildGraph("A-a->B\nA-a->C\nA-b->A", "completeGraphTest6_ND_a"),Configuration.getDefaultConfiguration());
		LearnerGraphND expected = new LearnerGraphND(buildGraph("A-a->B\nREJECT#-b-B-a-#REJECT\nA-b->A", "completeGraphTest6_ND_a"),Configuration.getDefaultConfiguration());
		Assert.assertTrue(graph.pathroutines.completeGraph(
				new VertexID("REJECT")));
		Assert.assertNull(WMethod.checkM(expected,graph));
	}	
	
	@Test
	public void completeGraphTest7()
	{
		String fsmOrig = "A-a->A-b->B-c->B-a->C\nQ-d->S",
			fsmExpected = "A-a->A-b->B-c->B-a->C\nA-c-#REJECT\nA-d-#REJECT\nB-b-#REJECT\nB-d-#REJECT\nC-a-#REJECT\nC-b-#REJECT\nC-c-#REJECT\nC-d-#REJECT\nS-a-#REJECT\nS-b-#REJECT\nS-c-#REJECT\nS-d-#REJECT\nQ-a-#REJECT\nQ-b-#REJECT\nQ-c-#REJECT\nQ-d->S";
		completeGraphTestHelper(fsmOrig,fsmExpected,true,"completeGraphTest7");
		
		// Additional checking.
		DirectedSparseGraph g = buildGraph(fsmOrig, "completeGraphTest7");
		final LearnerGraph graph = new LearnerGraph(g,config);
		Assert.assertTrue(graph.pathroutines.completeGraph(
				new DeterministicDirectedSparseGraph.VertexID("REJECT")));
		final LearnerGraph expected = new LearnerGraph(buildGraph(fsmExpected,"completeGraphTest7"),config);
		Assert.assertNull(WMethod.checkM(expected,expected.findVertex("A"),expected,expected.findVertex("A"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(expected,expected.findVertex("B"),expected,expected.findVertex("B"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(expected,expected.findVertex("Q"),expected,expected.findVertex("Q"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(expected,expected.findVertex("S"),expected,expected.findVertex("S"),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(expected,expected.findVertex("REJECT"),expected,expected.findVertex("REJECT"),WMethod.VERTEX_COMPARISON_KIND.NONE));
	}
	
	@Test
	public final void testLTL_complete2()
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("init-a->init-c->B-b->B", "testLTL_ba_graph3"),config);
		LearnerGraph result = new LearnerGraph(graph.config);AbstractPathRoutines.completeMatrix(graph,result);
		LearnerGraph expected = new LearnerGraph(FsmParser.buildGraph("A-a->A-c->B-b->B\n"+
				"A-b-#R1\n"+"B-a-#R2\n"+"B-c-#R3"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	@Test
	public final void testRemoveRejects1()
	{
		String fsmOrig = "A-a->A-b->B-a-#C\nB-b-#D", fsmExpected = "A-a->A-b->B";
		LearnerGraph actual = new LearnerGraph(config);AbstractPathRoutines.removeRejectStates(new LearnerGraph(
				buildGraph(fsmOrig, "testRemoveRejects1A"), config),actual);
		Assert.assertNull(WMethod.checkM(actual, new LearnerGraph(buildGraph(fsmExpected, "testRemoveRejects1B"), config)));
	}

	@Test
	public final void testRemoveRejects2()
	{
		String fsmOrig = "A-a->A-b->B-a-#C\nB-b-#D\nA-d-#T", fsmExpected = "A-a->A-b->B";
		LearnerGraph actual = new LearnerGraph(config);AbstractPathRoutines.removeRejectStates(new LearnerGraph(
				buildGraph(fsmOrig, "testRemoveRejects2A"), config),actual);
		Assert.assertNull(WMethod.checkM(actual, new LearnerGraph(buildGraph(fsmExpected, "testRemoveRejects2B"), config)));
	}

	@Test
	public final void testRemoveRejects3()
	{
		String fsm = "A-a->A-b->B";
		LearnerGraph actual = new LearnerGraph(config);AbstractPathRoutines.removeRejectStates(new LearnerGraph(
				buildGraph(fsm, "testRemoveRejects3"), config),actual);
		Assert.assertNull(WMethod.checkM(actual, new LearnerGraph(buildGraph(fsm, "testRemoveRejects3"), config)));
	}

	@Test
	public final void testRemoveRejects3_ND()
	{
		String fsm = "A-a->A-b->B\nA-a->B";
		LearnerGraphND actual = new LearnerGraphND(config);AbstractPathRoutines.removeRejectStates(new LearnerGraphND(
				buildGraph(fsm, "testRemoveRejects3_ND"), config),actual);
		Assert.assertNull(WMethod.checkM(actual, new LearnerGraphND(buildGraph(fsm, "testRemoveRejects3_ND"), config)));
	}

	@Test
	public final void testRemoveRejects4()
	{
		LearnerGraph actual = new LearnerGraph(config);AbstractPathRoutines.removeRejectStates(new LearnerGraph(
				config),actual);
		Assert.assertNull(WMethod.checkM(actual, new LearnerGraph(config)));
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveRejects_fail1()
	{
		LearnerGraph graph = new LearnerGraph(config);graph.getInit().setAccept(false);
		AbstractPathRoutines.removeRejectStates(graph,new LearnerGraph(config));
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveRejects_fail2()
	{
		String fsmOrig = "A-a->A-b->B-a-#C\nB-b-#D";
		LearnerGraph graph = new LearnerGraph(buildGraph(fsmOrig, "testRemoveRejects1A"), config);
		graph.getInit().setAccept(false);
		AbstractPathRoutines.removeRejectStates(graph,new LearnerGraph(config));
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveRejects_fail3()
	{
		String fsmOrig = "A-a-#D";
		LearnerGraph graph = new LearnerGraph(buildGraph(fsmOrig, "testRemoveRejects_fail3"), config);
		graph.getInit().setAccept(false);
		AbstractPathRoutines.removeRejectStates(graph,new LearnerGraph(config));
	}
}
