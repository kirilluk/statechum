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

import static org.junit.Assert.assertTrue;

import junit.framework.Assert;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Test;
import org.junit.BeforeClass;

import statechum.ArrayOperations;
import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import static statechum.Helper.whatToRun;
import static statechum.analysis.learning.rpnicore.FsmParser.buildGraph;

public class TestFSMAlgo {

	public TestFSMAlgo()
	{
		mainConfiguration = Configuration.getDefaultConfiguration();
		mainConfiguration.setAllowedToCloneNonCmpVertex(true);
	}
	
	org.w3c.dom.Document doc = null;
	
	/** Make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	@Before
	public final void beforeTest()
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

	@Test
	public final void completeComputeAlphabet0()
	{
		Set<String> alphabet = DeterministicDirectedSparseGraph.computeAlphabet(new DirectedSparseGraph());
		Assert.assertTrue(alphabet.isEmpty());
	}

	/** Tests alphabet computation in the presence of unreachable states. */
	@Test
	public final void testComputeFSMAlphabet1()
	{
		Set<String> expected = new HashSet<String>();
		expected.add("p");
		DirectedSparseGraph g = buildGraph("A-p->A","testComputeFSMAlphabet1");
		Assert.assertEquals(expected, new LearnerGraphND(g,config).pathroutines.computeAlphabet());
		Assert.assertEquals(expected, DeterministicDirectedSparseGraph.computeAlphabet(g));
	}

	@Test
	public final void testComputeFSMAlphabet2()
	{
		DirectedSparseGraph g = buildGraph("A-a->A<-b-A", "completeComputeAlphabet3");
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {"a","b"}));
		Assert.assertEquals(expected, new LearnerGraphND(g,config).pathroutines.computeAlphabet());
		Assert.assertEquals(expected, DeterministicDirectedSparseGraph.computeAlphabet(g));				
	}
	
	/** Tests alphabet computation in the presence of unreachable states. */
	@Test
	public final void testComputeFSMAlphabet3()
	{
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[]{"p","d","b","c","a"}));
		DirectedSparseGraph g = buildGraph("A-p->A-b->B-c->B-a-#C\nQ-d->S-c->S","testComputeFSMAlphabet3");
		Assert.assertEquals(expected, new LearnerGraphND(g,config).pathroutines.computeAlphabet());
		Assert.assertEquals(expected, DeterministicDirectedSparseGraph.computeAlphabet(g));				
	}


	@Test
	public final void testComputeFSMAlphabet4() {
		DirectedSparseGraph g = buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S-a-#T","testComputeFSMAlphabet4");
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[]{"p","d","b","c","a"}));
		Assert.assertEquals(expected, new LearnerGraphND(g,config).pathroutines.computeAlphabet());
		Assert.assertEquals(expected, DeterministicDirectedSparseGraph.computeAlphabet(g));				
	}

	@Test
	public final void completeComputeAlphabet5()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-a->S\nA-c->A\nB-b->B\nC-a->C-b->C-c->C\nQ-b->Q-c->Q\nS-a->S-b->S-c->S", "completeComputeAlphabet5");
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {"a","b","c"}));
		Assert.assertEquals(expected, new LearnerGraphND(g,config).pathroutines.computeAlphabet());
		Assert.assertEquals(expected, DeterministicDirectedSparseGraph.computeAlphabet(g));				

		LearnerGraph clone = new LearnerGraph(g,config);
		Assert.assertFalse( clone.pathroutines.completeGraph(new VertexID("REJECT")));
		Assert.assertFalse(DeterministicDirectedSparseGraph.completeGraph(g,"REJECT"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testFindVertex0()
	{
		DeterministicDirectedSparseGraph.findVertex(JUConstants.JUNKVERTEX, null, new DirectedSparseGraph());
	}

	@Test
	public final void testFindVertex1()
	{
		Assert.assertNull(DeterministicDirectedSparseGraph.findVertex(JUConstants.JUNKVERTEX, "bb", new DirectedSparseGraph()));
	}
	
	@Test
	public final void testFindVertex2()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex2");
		//Visualiser.updateFrame(g, g);Visualiser.waitForKey();
		Assert.assertNull(DeterministicDirectedSparseGraph.findVertex(JUConstants.JUNKVERTEX, "bb", g));
	}
		
	@Test
	public final void testFindVertex3()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex3");
		//Visualiser.updateFrame(g, null);Visualiser.waitForKey();
		Assert.assertNull(DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, "D", g));
	}

	@Test
	public final void testFindVertex4a()
	{
		Vertex v = DeterministicDirectedSparseGraph.findVertex(JUConstants.INITIAL, "anything", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex4a"));
		Assert.assertNull(v);
	}

	@Test
	public final void testFindVertex4b()
	{
		Vertex v =  DeterministicDirectedSparseGraph.findVertex(JUConstants.INITIAL, true, buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex4b"));
		Assert.assertEquals(new VertexID("A"), v.getUserDatum(JUConstants.LABEL));
	}

	@Test
	public final void testFindVertex5()
	{
		Vertex v =  DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("A"), buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex5"));
		Assert.assertEquals(new VertexID("A"), v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public final void testFindVertex6()
	{
		Vertex v =  DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("C"), buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex6"));
		Assert.assertEquals(new VertexID("C"), v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public final void testFindVertex7()
	{
		Vertex v = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("S"), buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex7"));
		Assert.assertEquals(new VertexID("S"), v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public final void testFindVertex8()
	{
		Vertex v = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("Q"), buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex8"));
		Assert.assertEquals(new VertexID("Q"), v.getUserDatum(JUConstants.LABEL));
	}

	
	@Test
	public final void testFindInitial1()
	{
		Vertex v = DeterministicDirectedSparseGraph.findInitial(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"));
		Assert.assertEquals(new VertexID("A"), v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public final void testFindInitial2()
	{
		Vertex v = DeterministicDirectedSparseGraph.findInitial(new DirectedSparseGraph());
		Assert.assertNull(v);
	}


	private final boolean checkIncompatible(LearnerGraph gr,StatePair pair)
	{
		return !AbstractLearnerGraph.checkCompatible(pair.getQ(), pair.getR(), gr.pairCompatibility);
	}
	
	/** Adding A, B as incompatible states. */
	@Test
	public final void testAddToIncompatibles1()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"),Configuration.getDefaultConfiguration());
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		
		grf.addToCompatibility(grf.findVertex("B"),grf.findVertex("A"), JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("C"),grf.findVertex("A"))));
	}
	
	/** Adding B, A as incompatible states. */
	@Test
	public final void testAddToIncompatibles2()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"),Configuration.getDefaultConfiguration());
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		
		grf.addToCompatibility(grf.findVertex("A"),grf.findVertex("B"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("C"),grf.findVertex("A"))));
	}
	
	/** Adding B, A as incompatible states twice. */
	@Test
	public final void testAddToIncompatibles3()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"),Configuration.getDefaultConfiguration());
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		
		grf.addToCompatibility(grf.findVertex("A"),grf.findVertex("B"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grf.addToCompatibility(grf.findVertex("A"),grf.findVertex("B"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("C"),grf.findVertex("A"))));
	}
	
	/** Adding B, A as incompatible states twice. */
	@Test
	public final void testAddToIncompatibles4()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"),Configuration.getDefaultConfiguration());
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		
		grf.addToCompatibility(grf.findVertex("A"),grf.findVertex("B"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grf.addToCompatibility(grf.findVertex("B"),grf.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("C"),grf.findVertex("A"))));
	}
	
	/** Adding B, A as incompatible states twice. */
	@Test
	public final void testAddToIncompatibles5()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"),Configuration.getDefaultConfiguration());
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		
		grf.addToCompatibility(grf.findVertex("A"),grf.findVertex("B"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grf.addToCompatibility(grf.findVertex("C"),grf.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("C"),grf.findVertex("A"))));
	}
	
	/** Removing B, A as incompatible states. */
	@Test
	public final void testRemoveFromIncompatibles1()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d-#S", "testRemoveFromIncompatibles1"),Configuration.getDefaultConfiguration());
		grf.addToCompatibility(grf.findVertex("A"),grf.findVertex("B"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		grf.removeFromIncompatibles(grf.findVertex("A"),grf.findVertex("B"));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
	}
	
	/** Removing B, A as incompatible states twice. */
	@Test
	public final void testRemoveFromIncompatibles2()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d-#S", "testRemoveFromIncompatibles1"),Configuration.getDefaultConfiguration());
		grf.addToCompatibility(grf.findVertex("A"),grf.findVertex("B"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		grf.removeFromIncompatibles(grf.findVertex("A"),grf.findVertex("B"));
		grf.removeFromIncompatibles(grf.findVertex("A"),grf.findVertex("B"));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
	}
	
	/** Removing B, A as incompatible states. */
	@Test
	public final void testRemoveFromIncompatibles3()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d-#S", "testRemoveFromIncompatibles1"),Configuration.getDefaultConfiguration());
		grf.addToCompatibility(grf.findVertex("A"),grf.findVertex("B"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grf.removeFromIncompatibles(grf.findVertex("A"),grf.findVertex("S"));
		grf.removeFromIncompatibles(grf.findVertex("A"),grf.findVertex("C"));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("C"),grf.findVertex("A"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("S"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("S"),grf.findVertex("A"))));
	}
	
	/** Tests that removing a vertex from a collection of incompatibles may also remove a row. */
	@Test
	public final void testRemoveFromIncompatibles4()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d-#S", "testRemoveFromIncompatibles1"),Configuration.getDefaultConfiguration());
		grf.addToCompatibility(grf.findVertex("A"),grf.findVertex("B"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grf.removeFromIncompatibles(grf.findVertex("A"),grf.findVertex("B"));
		Assert.assertTrue(grf.pairCompatibility.compatibility.isEmpty());
	}
	
	/** Checking that copying a graph clones the array. */
	@Test
	public final void testIncompatibles5()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"),Configuration.getDefaultConfiguration());

		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		LearnerGraph graph2 = new LearnerGraph(grf,Configuration.getDefaultConfiguration());
		
		grf.addToCompatibility(grf.findVertex("B"),grf.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("C"),grf.findVertex("A"))));

		Assert.assertFalse(checkIncompatible(graph2,new StatePair(graph2.findVertex("A"),graph2.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(graph2,new StatePair(graph2.findVertex("A"),graph2.findVertex("B"))));
		Assert.assertFalse(checkIncompatible(graph2,new StatePair(graph2.findVertex("B"),graph2.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(graph2,new StatePair(graph2.findVertex("A"),graph2.findVertex("C"))));
		Assert.assertFalse(checkIncompatible(graph2,new StatePair(graph2.findVertex("C"),graph2.findVertex("A"))));
		
		graph2.addToCompatibility(grf.findVertex("C"),grf.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(checkIncompatible(grf,new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertFalse(checkIncompatible(grf,new StatePair(grf.findVertex("C"),grf.findVertex("A"))));

		Assert.assertFalse(checkIncompatible(graph2,new StatePair(graph2.findVertex("A"),graph2.findVertex("A"))));
		Assert.assertFalse(checkIncompatible(graph2,new StatePair(graph2.findVertex("A"),graph2.findVertex("B"))));
		Assert.assertFalse(checkIncompatible(graph2,new StatePair(graph2.findVertex("B"),graph2.findVertex("A"))));
		Assert.assertTrue(checkIncompatible(graph2,new StatePair(graph2.findVertex("A"),graph2.findVertex("C"))));
		Assert.assertTrue(checkIncompatible(graph2,new StatePair(graph2.findVertex("C"),graph2.findVertex("A"))));
	}
	
	/** Tests that construction of incompatibles from information in equivalence classes works. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testConstuctionOfIncompatibles1() throws IncompatibleStatesException
	{
		LearnerGraphND gr = new LearnerGraphND(buildGraph("S-a->A-b->C-c->G\nS-a->B-b->D-c->H\nB-b->E-c->I\nS-a->F", "testConstuctionOfIncompatiblesA"),Configuration.getDefaultConfiguration());
		gr.addToCompatibility(gr.findVertex("F"), gr.findVertex("D"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphND expected = new LearnerGraphND(buildGraph("S-a->A-b->C-c->G", "testConstuctionOfIncompatiblesB"),Configuration.getDefaultConfiguration());
		expected.addToCompatibility(expected.findVertex("A"), expected.findVertex("C"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertNull(WMethod.checkM_and_colours(expected,gr.pathroutines.buildDeterministicGraph(),VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** Tests that construction of incompatibles from information in equivalence classes works. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testConstuctionOfIncompatibles2() throws IncompatibleStatesException
	{
		LearnerGraphND gr = new LearnerGraphND(buildGraph("S-a->A-b->C-c->G\nS-a->B-b->D-c->H\nB-b->E-c->I\nS-a->F", "testConstuctionOfIncompatiblesA"),Configuration.getDefaultConfiguration());
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("H"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("D"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphND expected = new LearnerGraphND(buildGraph("S-a->A-b->C-c->G", "testConstuctionOfIncompatiblesB"),Configuration.getDefaultConfiguration());
		expected.addToCompatibility(expected.findVertex("A"), expected.findVertex("C"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);expected.addToCompatibility(gr.findVertex("A"), gr.findVertex("G"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertNull(WMethod.checkM_and_colours(expected,gr.pathroutines.buildDeterministicGraph(),VERTEX_COMPARISON_KIND.DEEP));
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
	
	/** Builds a map from an array, where each element corresponds to a pair of a string array 
	 * (representing a sequence) and a string (representing flags associated with this sequence).
	 * 
	 * @param data source data
	 * @return a string->string map
	 */
	public static Map<String,String> buildStringMap(Object [][] data)
	{
		Map<String,String> result = new HashMap<String,String>();
		for(Object[] str:data)
		{
			if (str.length != 2)
				throw new IllegalArgumentException("more than two elements in sequence "+str);
			if (str[0] == null || str[1] == null || !(str[0] instanceof String[]) || !(str[1] instanceof String))
				throw new IllegalArgumentException("invalid data in array");
			result.put(ArrayOperations.seqToString(Arrays.asList((String[])str[0])),(String)str[1]);
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
		List<List<String>> result = new LinkedList<List<String>>();
		for(String []seq:data)
		{
			result.add(Arrays.asList(seq));
		}
		return result;
	}
	
	@Test
	public final void testBuildSet1()
	{
		assertTrue(buildSet(new String[] []{}).isEmpty());
	}

	@Test
	public final void testBuildSet2()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(new LinkedList<String>());
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{}})));
	}

	@Test
	public final void testBuildSet3A()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		expectedResult.add(new LinkedList<String>());
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{},new String[]{"a","b","c"}})));
	}

	@Test
	public final void testBuildSet3B()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{"a","b","c"}})));
	}

	@Test
	public final void testBuildSet4()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		expectedResult.add(new LinkedList<String>());
		expectedResult.add(Arrays.asList(new String[]{"g","t"}));
		expectedResult.add(Arrays.asList(new String[]{"h","q","i"}));
		assertTrue(expectedResult.equals(buildSet(new String[] []{
				new String[]{"a","b","c"},new String[]{"h","q","i"}, new String[] {},new String[]{"g","t"} })));
	}

	@Test
	public final void testBuildStringMap1()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
		})));
	}
	
	@Test
	public final void testBuildStringMap2()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"a"},"value2"},
				new Object[]{new String[]{"b"},"value3"}
		})));
	}
	
	@Test
	public final void testBuildStringMap3()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value2"},
				new Object[]{new String[]{"a"},"value1"},
				new Object[]{new String[]{"b"},"value3"}
		})));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap4()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new String[]{"a"}},// an invalid sequence
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap5()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{},// an invalid sequence - too few elements
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap6()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new String[]{"a"},"c","d"},// an invalid sequence - too many elements
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap7()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new Object(),"c"},// an invalid sequence - wrong type of the first element
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap8()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{"text","c"},// an invalid sequence - wrong type of the first element
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap9()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new String[]{"a"},new Object()},// an invalid sequence - wrong type of the second element
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap10()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{null,"value"},// an invalid sequence - null in the first element
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap11()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new String[]{"a"}, null},// an invalid sequence - null in the second element
				new Object[]{new String[]{"b"},null}
		})));
	}
	
	public final void checkForCorrectException(final int [][]tTable, final int []vFrom, String exceptionString)
	{
		statechum.Helper.checkForCorrectException(new whatToRun() { public void run() {
			LearnerGraph.convertTableToFSMStructure(tTable, vFrom, -1	,config);
		}}, IllegalArgumentException.class,exceptionString);
	}
	
	/** Zero-sized array. */
	@Test
	public final void testConvertTableToFSMStructure0()
	{
		int [][]table = new int[][] {
		};
		checkForCorrectException(table, new int[0], "array is zero-sized");
	}

	/** Zero-sized alphabet. */
	@Test
	public final void testConvertTableToFSMStructure1a()
	{
		int [][]table = new int[][] {
			{}, 
			{1,1}
		};
		checkForCorrectException(table, new int[]{0,1}, "alphabet is zero-sized");
	}
	
	/** "rows of inconsistent size" */
	@Test
	public final void testConvertTableToFSMStructure1b()
	{
		int [][]table = new int[][] {
			{}, 
			{1,1}
		};
		checkForCorrectException(table, new int[]{1,0}, "rows of inconsistent size");
	}
	
	/** "rows of inconsistent size" */
	@Test
	public final void testConvertTableToFSMStructure2()
	{
		int [][]table = new int[][] {
				{1,0,1,0}, 
				{0,1}
			};
		checkForCorrectException(table, new int[]{0,1}, "rows of inconsistent size");
	}
	
	/** Reject number in vfrom. */
	@Test
	public final void testConvertTableToFSMStructure3()
	{
		int [][]table = new int[][] {
				{1,0,1,0}, 
				{0,1,0,1}
			};
		checkForCorrectException(table, new int[]{0,-1}, "reject number in vFrom");
	}
	
	/** Transition to illegal state 6 */
	@Test
	public final void testConvertTableToFSMStructure4a()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	2}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		checkForCorrectException(table, new int[]{0,1,2,3}, "leads to an invalid state");
	}
	
	/** Transition to illegal state -4 */
	@Test
	public final void testConvertTableToFSMStructure4b()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	2}, 
			{0, 3,	0,	-1},
			{0,0,0,-4},
			{-1,-1,-1,-1}
		};
		checkForCorrectException(table, new int[]{0,1,2,3}, "leads to an invalid state");
	}

	@Test
	public final void testConvertTableToFSMStructure_missing_elements_in_vFrom()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		checkForCorrectException(table, new int[]{0,1}, "Some states in the transition table are not included in vFrom");
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
		LearnerGraph fsm = LearnerGraph.convertTableToFSMStructure(table, new int[]{0,1,3}, -1	,config);
		Assert.assertNull(WMethod.checkM(fsm, fsm.findVertex("S0"),
				new LearnerGraph(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure5"),config), 
				fsm.findVertex("S0"),WMethod.VERTEX_COMPARISON_KIND.NONE));
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
		LearnerGraph fsm = LearnerGraph.convertTableToFSMStructure(table, new int[]{1,0,3}, -1	,config);
		Assert.assertNull(WMethod.checkM(fsm, fsm.findVertex("S0"), 
				new LearnerGraph(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure6"),config), 
				fsm.findVertex("S0"),WMethod.VERTEX_COMPARISON_KIND.NONE));
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
		LearnerGraph fsm = LearnerGraph.convertTableToFSMStructure(table, new int[]{3,0,1}, -1	,config);
		Assert.assertNull(WMethod.checkM(fsm, fsm.findVertex("S0"), 
				new LearnerGraph(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure7"),config), 
				fsm.findVertex("S0"),WMethod.VERTEX_COMPARISON_KIND.NONE));
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
		LearnerGraph fsm = LearnerGraph.convertTableToFSMStructure(table, new int[]{3,0,1,0,1,1}, -1	,config);
		Assert.assertNull(WMethod.checkM(fsm, fsm.findVertex("S0"), 
				new LearnerGraph(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure8"),config), 
				fsm.findVertex("S0"),WMethod.VERTEX_COMPARISON_KIND.NONE));
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
	public final void testGetNonRepeatingNumbers3()
	{
		final int size = 200;
		int data[] = DeterministicDirectedSparseGraph.getNonRepeatingNumbers(size, 1); 
		Assert.assertEquals(size,data.length);
		boolean values[] = new boolean[size];
		for(int i=0;i<size;++i) { Assert.assertFalse(values[data[i]]);values[data[i]]=true; }
		//System.out.println(Arrays.toString(data));
	}

	@Test
	public final void computeShortPathsToAllStates1()
	{
		LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->B\nA-a->C","computeShortPathsToAllStates1"),Configuration.getDefaultConfiguration());
		Map<CmpVertex,List<String>> expected = new TreeMap<CmpVertex,List<String>>();
		expected.put(graph.findVertex("A"), Arrays.asList(new String[]{}));
		expected.put(graph.findVertex("B"), Arrays.asList(new String[]{"a"}));
		expected.put(graph.findVertex("C"), Arrays.asList(new String[]{"a"}));
		Assert.assertEquals(expected,graph.pathroutines.computeShortPathsToAllStates(graph.findVertex("A")));
	}
	
	@Test
	public final void computeShortPathsToAllStates2()
	{
		LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->B\nA-a->C-b-#D","computeShortPathsToAllStates1"),Configuration.getDefaultConfiguration());
		Map<CmpVertex,List<String>> expected = new TreeMap<CmpVertex,List<String>>();
		expected.put(graph.findVertex("A"), Arrays.asList(new String[]{}));
		expected.put(graph.findVertex("B"), Arrays.asList(new String[]{"a"}));
		expected.put(graph.findVertex("C"), Arrays.asList(new String[]{"a"}));
		expected.put(graph.findVertex("D"), Arrays.asList(new String[]{"a","b"}));
		Assert.assertEquals(expected,graph.pathroutines.computeShortPathsToAllStates(graph.findVertex("A")));
	}
	
	@Test
	public final void computeShortPathsToAllStates3()
	{
		LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->B\nA-a->C-b-#D","computeShortPathsToAllStates1"),Configuration.getDefaultConfiguration());
		Map<CmpVertex,List<String>> expected = new TreeMap<CmpVertex,List<String>>();
		expected.put(graph.findVertex("B"), Arrays.asList(new String[]{}));
		Assert.assertEquals(expected,graph.pathroutines.computeShortPathsToAllStates(graph.findVertex("B")));
	}
	
	@Test
	public final void computeShortPathsToAllStates4()
	{
		LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->B\nA-a->C-b-#D","computeShortPathsToAllStates1"),Configuration.getDefaultConfiguration());
		Map<CmpVertex,List<String>> expected = new TreeMap<CmpVertex,List<String>>();
		expected.put(graph.findVertex("C"), Arrays.asList(new String[]{}));
		expected.put(graph.findVertex("D"), Arrays.asList(new String[]{"b"}));
		Assert.assertEquals(expected,graph.pathroutines.computeShortPathsToAllStates(graph.findVertex("C")));
	}
	
	/** Given a graph and a name of a state, this method builds a set of states containing this single state. */
	//private static Set<CmpVertex> buildSingletonStateSet(LearnerGraph 
	
	/** Tests <em>buildVertexToEqClassMap</em>. */
	@Test
	public final void testBuildVertexToEqClassMap1()
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-b->A\nA-b->C-b-#D","testBuildVertexToEqClassMap1"),Configuration.getDefaultConfiguration());
		Assert.assertNull(graph.learnerCache.getVertexToEqClass());
		graph.merger.buildVertexToEqClassMap(null);
		Assert.assertEquals(4,graph.learnerCache.getVertexToEqClass().size());

		for(String vertex:new String[]{"A","B","C","D"})
		{
			Set<CmpVertex> expectedSet = new TreeSet<CmpVertex>();expectedSet.add(graph.findVertex(VertexID.parseID(vertex)));
			Assert.assertEquals(expectedSet,graph.learnerCache.getVertexToEqClass().get(graph.findVertex(VertexID.parseID(vertex))).getStates());
		}
		
		// Now update the map and check that it did not change
		graph.merger.buildVertexToEqClassMap(null);
		Assert.assertEquals(4,graph.learnerCache.getVertexToEqClass().size());

		for(String vertex:new String[]{"A","B","C","D"})
		{
			Set<CmpVertex> expectedSet = new TreeSet<CmpVertex>();expectedSet.add(graph.findVertex(VertexID.parseID(vertex)));
			Assert.assertEquals(expectedSet,graph.learnerCache.getVertexToEqClass().get(graph.findVertex(VertexID.parseID(vertex))).getStates());
		}
	}
	
	/** Tests <em>buildVertexToEqClassMap</em>. */
	@Test
	public final void testBuildVertexToEqClassMap2()
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a->D\nC-b->C1\nD-b->D1-b->D2","testBuildVertexToEqClassMap2"),Configuration.getDefaultConfiguration());
		LearnerGraph mergedAB = MergeStates.mergeAndDeterminize_general(graph, new StatePair(graph.findVertex("A"),graph.findVertex("B")));
		
		Assert.assertNull(graph.learnerCache.getVertexToEqClass());
		Assert.assertNull(mergedAB.learnerCache.getVertexToEqClass());

		graph.merger.buildVertexToEqClassMap(null);
		Assert.assertEquals(7,graph.learnerCache.getVertexToEqClass().size());
		for(String vertex:new String[]{"A","B","C","D","C1","D1","D2"})
		{
			Set<CmpVertex> expectedSet = new TreeSet<CmpVertex>();expectedSet.add(graph.findVertex(VertexID.parseID(vertex)));
			Assert.assertEquals(expectedSet,graph.learnerCache.getVertexToEqClass().get(graph.findVertex(VertexID.parseID(vertex))).getStates());
		}
	}
	
	/** Tests <em>buildVertexToEqClassMap</em>. */
	@Test
	public final void testBuildVertexToEqClassMap3()
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a->D\nC-b->C1\nD-b->D1-b->D2","testBuildVertexToEqClassMap2"),Configuration.getDefaultConfiguration());
		graph.merger.buildVertexToEqClassMap(null);
		LearnerGraph mergedAB = MergeStates.mergeAndDeterminize_general(graph, new StatePair(graph.findVertex("A"),graph.findVertex("B")));
		
		Assert.assertNotNull(graph.learnerCache.getVertexToEqClass());
		Assert.assertNotNull(mergedAB.learnerCache.getVertexToEqClass());
		
		Set<CmpVertex> expectedSet = new TreeSet<CmpVertex>();
		
		expectedSet.clear();
		for(String vertex:new String[]{"A","B","C","D"}) expectedSet.add(graph.findVertex(VertexID.parseID(vertex)));
		Assert.assertEquals(expectedSet,mergedAB.learnerCache.getVertexToEqClass().get(graph.findVertex(VertexID.parseID("A"))).getStates());
		
		expectedSet.clear();
		for(String vertex:new String[]{"C1","D1"}) expectedSet.add(graph.findVertex(VertexID.parseID(vertex)));
		Assert.assertEquals(expectedSet,mergedAB.learnerCache.getVertexToEqClass().get(graph.findVertex(VertexID.parseID("C1"))).getStates());

		expectedSet.clear();
		for(String vertex:new String[]{"D2"}) expectedSet.add(graph.findVertex(VertexID.parseID(vertex)));
		Assert.assertEquals(expectedSet,mergedAB.learnerCache.getVertexToEqClass().get(graph.findVertex(VertexID.parseID("D2"))).getStates());
		
		// Now update the matrix and check that it did not change
		mergedAB.merger.buildVertexToEqClassMap(null);

		expectedSet.clear();
		for(String vertex:new String[]{"A","B","C","D"}) expectedSet.add(graph.findVertex(VertexID.parseID(vertex)));
		Assert.assertEquals(expectedSet,mergedAB.learnerCache.getVertexToEqClass().get(graph.findVertex(VertexID.parseID("A"))).getStates());
		
		expectedSet.clear();
		for(String vertex:new String[]{"C1","D1"}) expectedSet.add(graph.findVertex(VertexID.parseID(vertex)));
		Assert.assertEquals(expectedSet,mergedAB.learnerCache.getVertexToEqClass().get(graph.findVertex(VertexID.parseID("C1"))).getStates());

		expectedSet.clear();
		for(String vertex:new String[]{"D2"}) expectedSet.add(graph.findVertex(VertexID.parseID(vertex)));
		Assert.assertEquals(expectedSet,mergedAB.learnerCache.getVertexToEqClass().get(graph.findVertex(VertexID.parseID("D2"))).getStates());
	}
	
	/** Tests <em>buildVertexToEqClassMap</em>. */
	@Test
	public final void testBuildVertexToEqClassMap4()
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a->D\nC-b->C1\nD-b->D1-b->D2","testBuildVertexToEqClassMap2"),Configuration.getDefaultConfiguration());
		graph.merger.buildVertexToEqClassMap(null);
		LearnerGraph mergedAB = MergeStates.mergeAndDeterminize_general(graph, new StatePair(graph.findVertex("A"),graph.findVertex("B")));
		LearnerGraph mergedAll = MergeStates.mergeAndDeterminize_general(mergedAB, new StatePair(graph.findVertex("A"),graph.findVertex("C1")));
		
		Assert.assertNotNull(graph.learnerCache.getVertexToEqClass());
		Assert.assertNotNull(mergedAB.learnerCache.getVertexToEqClass());
		Assert.assertNotNull(mergedAll.learnerCache.getVertexToEqClass());
		
		Set<CmpVertex> expectedSet = new TreeSet<CmpVertex>();
		
		expectedSet.clear();
		for(String vertex:new String[]{"A","B","C","D","C1","D1","D2"}) expectedSet.add(graph.findVertex(VertexID.parseID(vertex)));
		Assert.assertEquals(expectedSet,mergedAll.learnerCache.getVertexToEqClass().get(graph.findVertex(VertexID.parseID("A"))).getStates());

		// Now update the matrix and check that it did not change
		mergedAll.merger.buildVertexToEqClassMap(null);
		
		expectedSet.clear();
		for(String vertex:new String[]{"A","B","C","D","C1","D1","D2"}) expectedSet.add(graph.findVertex(VertexID.parseID(vertex)));
		Assert.assertEquals(expectedSet,mergedAll.learnerCache.getVertexToEqClass().get(graph.findVertex(VertexID.parseID("A"))).getStates());
	}
	
	/** Similar to the above, but this time a new vertex is added to the intermediate graph and the matrix is updated. */
	@Test
	public final void testBuildVertexToEqClassMap5()
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a->D\nC-b->C1\nD-b->D1-b->D2","testBuildVertexToEqClassMap2"),Configuration.getDefaultConfiguration());
		graph.merger.buildVertexToEqClassMap(null);
		LearnerGraph mergedAB = MergeStates.mergeAndDeterminize_general(graph, new StatePair(graph.findVertex("A"),graph.findVertex("B")));
		CmpVertex newVertex = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("D3"), mergedAB.config);
		mergedAB.transitionMatrix.put(newVertex, mergedAB.createNewRow());
		mergedAB.addTransition(mergedAB.transitionMatrix.get(mergedAB.findVertex("D2")), "b", newVertex);
		LearnerGraph mergedAll = MergeStates.mergeAndDeterminize_general(mergedAB, new StatePair(graph.findVertex("A"),graph.findVertex("C1")));
		
		Assert.assertNotNull(graph.learnerCache.getVertexToEqClass());
		Assert.assertNotNull(mergedAB.learnerCache.getVertexToEqClass());
		Assert.assertNotNull(mergedAll.learnerCache.getVertexToEqClass());
		
		Set<CmpVertex> expectedSet = new TreeSet<CmpVertex>();
		
		expectedSet.clear();
		for(String vertex:new String[]{"A","B","C","D","C1","D1","D2"}) expectedSet.add(graph.findVertex(VertexID.parseID(vertex)));
		for(String vertex:new String[]{"D3"}) expectedSet.add(mergedAB.findVertex(VertexID.parseID(vertex)));
		Assert.assertEquals(expectedSet,mergedAll.learnerCache.getVertexToEqClass().get(graph.findVertex(VertexID.parseID("A"))).getStates());
	}
	
	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{		
		Visualiser.disposeFrame();
	}

	@AfterClass
	public static void cleanUp()
	{
		Visualiser.disposeFrame();
	}
}
