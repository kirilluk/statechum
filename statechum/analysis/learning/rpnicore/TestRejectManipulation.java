/*
 * Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildGraph;
import static statechum.analysis.learning.rpnicore.TestEquivalenceChecking.checkM;

public class TestRejectManipulation {
	public TestRejectManipulation()
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
		LearnerGraph.testMode=true;

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
		LearnerGraph fsm = new LearnerGraph(buildGraph(originalGraph, testName),config);
		Assert.assertEquals(whetherToBeCompleted,fsm.transform.completeGraph(
				new VertexID("REJECT")));
		WMethod.checkM(fsm, new LearnerGraph(buildGraph(expectedOutcome,testName),config));				
	}
	
	/** Checks that passing a name of an existing state causes an exception to be thrown. */
	@Test(expected=IllegalArgumentException.class)
	public void complete_fail()
	{
		new LearnerGraph(buildGraph("A-a->A-b->B-c->B", "complete_fail"),config).transform.completeGraph(
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
	
	@Test
	public void completeGraphTest7()
	{
		String fsmOrig = "A-a->A-b->B-c->B-a->C\nQ-d->S",
			fsmExpected = "A-a->A-b->B-c->B-a->C\nA-c-#REJECT\nA-d-#REJECT\nB-b-#REJECT\nB-d-#REJECT\nC-a-#REJECT\nC-b-#REJECT\nC-c-#REJECT\nC-d-#REJECT\nS-a-#REJECT\nS-b-#REJECT\nS-c-#REJECT\nS-d-#REJECT\nQ-a-#REJECT\nQ-b-#REJECT\nQ-c-#REJECT\nQ-d->S";
		completeGraphTestHelper(fsmOrig,fsmExpected,true,"completeGraphTest7");
		
		// Additional checking.
		DirectedSparseGraph g = buildGraph(fsmOrig, "completeGraphTest7");
		final LearnerGraph graph = new LearnerGraph(g,config);
		Assert.assertTrue(graph.transform.completeGraph(
				new DeterministicDirectedSparseGraph.VertexID("REJECT")));
		final LearnerGraph expected = new LearnerGraph(buildGraph(fsmExpected,"completeGraphTest7"),config);
		Assert.assertNull(WMethod.checkM(expected,expected.findVertex("A"),expected,expected.findVertex("A")));
		Assert.assertNull(WMethod.checkM(expected,expected.findVertex("B"),expected,expected.findVertex("B")));
		Assert.assertNull(WMethod.checkM(expected,expected.findVertex("Q"),expected,expected.findVertex("Q")));
		Assert.assertNull(WMethod.checkM(expected,expected.findVertex("S"),expected,expected.findVertex("S")));
		Assert.assertNull(WMethod.checkM(expected,expected.findVertex("REJECT"),expected,expected.findVertex("REJECT")));
	}
	
	@Test
	public final void testRemoveRejects1()
	{
		String fsmOrig = "A-a->A-b->B-a-#C\nB-b-#D", fsmExpected = "A-a->A-b->B";
		LearnerGraph actual = Transform.removeRejectStates(new LearnerGraph(buildGraph(fsmOrig, "testRemoveRejects1A"), config),config);
		WMethod.checkM(actual, new LearnerGraph(buildGraph(fsmExpected, "testRemoveRejects1B"), config));
	}

	@Test
	public final void testRemoveRejects2()
	{
		String fsmOrig = "A-a->A-b->B-a-#C\nB-b-#D\nA-d-#T", fsmExpected = "A-a->A-b->B";
		LearnerGraph actual = Transform.removeRejectStates(new LearnerGraph(buildGraph(fsmOrig, "testRemoveRejects2A"), config),config);
		WMethod.checkM(actual, new LearnerGraph(buildGraph(fsmExpected, "testRemoveRejects2B"), config));
	}

	@Test
	public final void testRemoveRejects3()
	{
		String fsm = "A-a->A-b->B";
		LearnerGraph actual = Transform.removeRejectStates(new LearnerGraph(buildGraph(fsm, "testRemoveRejects3"), config),config);
		WMethod.checkM(actual, new LearnerGraph(buildGraph(fsm, "testRemoveRejects3"), config));
	}

	@Test
	public final void testRemoveRejects4()
	{
		LearnerGraph actual = Transform.removeRejectStates(new LearnerGraph(config),config);
		WMethod.checkM(actual, new LearnerGraph(config));
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveRejects_fail1()
	{
		LearnerGraph graph = new LearnerGraph(config);graph.init.setAccept(false);
		Transform.removeRejectStates(graph,config);
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveRejects_fail2()
	{
		String fsmOrig = "A-a->A-b->B-a-#C\nB-b-#D";
		LearnerGraph graph = new LearnerGraph(buildGraph(fsmOrig, "testRemoveRejects1A"), config);
		graph.init.setAccept(false);
		Transform.removeRejectStates(graph,config);
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveRejects_fail3()
	{
		String fsmOrig = "A-a-#D";
		LearnerGraph graph = new LearnerGraph(buildGraph(fsmOrig, "testRemoveRejects_fail3"), config);
		graph.init.setAccept(false);
		Transform.removeRejectStates(graph,config);
	}
}
