/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static statechum.analysis.learning.TestFSMAlgo.buildGraph;
import static statechum.analysis.learning.rpnicore.LearnerGraph.createLabelToStateMap;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.StringVertex;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.TestRpniLearner;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class TestGraphConstruction 
{
	public TestGraphConstruction()
	{
		mainConfiguration = Configuration.getDefaultConfiguration();
	}
	
	/** Make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	@Before
	public void beforeTest()
	{
		
		config = (Configuration)mainConfiguration.clone();
		differentA = new LearnerGraph(buildGraph("Q-a->A-b->B", "testFSMStructureEquals2"),config);
		differentB = new LearnerGraph(buildGraph("A-b->A\nB-b->B", "testFSMStructureEquals2"),config);

		confJung = (Configuration)config.clone();confJung.setLearnerUseStrings(false);confJung.setLearnerCloneGraph(true);
		confString = (Configuration)config.clone();confString.setLearnerUseStrings(true);confString.setLearnerCloneGraph(true);
		confSame = (Configuration)config.clone();confSame.setLearnerUseStrings(false);confSame.setLearnerCloneGraph(false);
		testGraph = buildGraph("A-a->A-b->B-c->B\nA-c-#C\nB-b->B", "testFSMStructureClone");
		testGraphJung=new LearnerGraph(testGraph,(Configuration)confJung.clone());// clone here makes it possible to change the configuration later without affecting objects in this object
		testGraphString=new LearnerGraph(testGraph,(Configuration)confString.clone());
		testGraphSame=new LearnerGraph(testGraph,(Configuration)confSame.clone());
	}

	/** Graph used in tests of cloning. */
	private DirectedSparseGraph testGraph = null;
	/** FSMs used in testing of cloning. */
	private LearnerGraph testGraphJung,testGraphString,testGraphSame;
	
	/** The configuration to use when running tests. */
	private Configuration config = null, mainConfiguration = null;
	
	/** Configuration settings used to test creation/cloning of graphs. */
	private Configuration confJung,confString,confSame;
	
	/** Used as arguments to equalityTestingHelper. */
	private LearnerGraph differentA = null, differentB = null;
	
	/** Non graph vertex to copy. */
	@Test(expected = IllegalArgumentException.class) 
	public final void testVertexClone_fail1()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(true);
		LearnerGraph.cloneCmpVertex("junk", conf);
	}
	
	/** Non-CmpVertex copying denied. */
	@Test(expected = IllegalArgumentException.class) 
	public final void testVertexClone_fail2()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(false);
		DirectedSparseVertex vertex = new DirectedSparseVertex();vertex.addUserDatum(JUConstants.LABEL, "name", UserData.SHARED);
		LearnerGraph.cloneCmpVertex(vertex, conf);
	}
	
	/** Unlabelled copying denied. */
	@Test(expected = IllegalArgumentException.class) 
	public final void testVertexClone_fail3()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(false);
		DirectedSparseVertex vertex = new DirectedSparseVertex();
		LearnerGraph.cloneCmpVertex(vertex, conf);
	}
	
	
	/** Normal copying successful. */
	@Test
	public final void testVertexClone1()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseVertex vertex = new DirectedSparseVertex();vertex.addUserDatum(JUConstants.LABEL, "name", UserData.SHARED);
		CmpVertex result = LearnerGraph.cloneCmpVertex(vertex, conf);
		Assert.assertEquals("name", result.getName());
		Assert.assertTrue(result.isAccept());Assert.assertFalse(result.isHighlight());Assert.assertNull(result.getColour());
	}
	
	/** Checking that attributes are preserved. */
	@Test
	public final void testVertexClone2()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseVertex vertex = new DirectedSparseVertex();vertex.addUserDatum(JUConstants.LABEL, "name", UserData.SHARED);
		vertex.addUserDatum(JUConstants.HIGHLIGHT, 1, UserData.SHARED);
		vertex.addUserDatum(JUConstants.COLOUR, JUConstants.BLUE, UserData.SHARED);
		vertex.addUserDatum(JUConstants.ACCEPTED, false, UserData.SHARED);
		
		CmpVertex result = LearnerGraph.cloneCmpVertex(vertex, conf);
		Assert.assertEquals("name", result.getName());
		Assert.assertEquals(JUConstants.BLUE, result.getColour());
		Assert.assertFalse(result.isAccept());Assert.assertTrue(result.isHighlight());
		
		vertex.removeUserDatum(JUConstants.ACCEPTED);vertex.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
		Assert.assertFalse(result.isAccept());
	}
	
	/** Checking that cloning can returns the same vertex regardless of the value of setLearnerUseStrings. */
	@Test
	public final void testVertexClone3()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(false);
		conf.setLearnerUseStrings(false);conf.setLearnerCloneGraph(false);
		CmpVertex vA = new DeterministicVertex("test vertex");vA.setColour(JUConstants.RED);
		Assert.assertSame(vA, LearnerGraph.cloneCmpVertex(vA, conf));
	}
	
	/** Checking that cloning can returns the same vertex regardless of the value of setLearnerUseStrings. */
	@Test
	public final void testVertexClone4()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(false);
		conf.setLearnerUseStrings(true);conf.setLearnerCloneGraph(false);
		CmpVertex vA = new DeterministicVertex("test vertex");vA.setColour(JUConstants.RED);
		Assert.assertSame(vA, LearnerGraph.cloneCmpVertex(vA, conf));
	}
	
	private static void testColourHelper(CmpVertex vert)
	{
		Assert.assertNull(vert.getColour());
		vert.setColour(JUConstants.RED);Assert.assertSame(JUConstants.RED, vert.getColour());
		vert.setColour(JUConstants.BLUE);Assert.assertSame(JUConstants.BLUE, vert.getColour());
		vert.setColour(null);Assert.assertNull(vert.getColour());
		
		// Now check invalid colours
		try
		{
			vert.setColour(JUConstants.HIGHLIGHT);
			Assert.fail("exception expected here");
		}
		catch(IllegalArgumentException ex)
		{// exception is expected here, continue			
		}
		Assert.assertNull(vert.getColour());

		// check that settting of an invalid colour does not mess up the previous one.
		vert.setColour(JUConstants.BLUE);Assert.assertSame(JUConstants.BLUE, vert.getColour());
		try
		{
			vert.setColour(JUConstants.ACCEPTED);
			Assert.fail("exception expected here");
		}
		catch(IllegalArgumentException ex)
		{// exception is expected here, continue			
		}
		Assert.assertSame(JUConstants.BLUE, vert.getColour());
	}
	
	/** Tests that I can assign colours meaningfully on Deterministic and String vertices. */
	@Test
	public final void testColourSetting1()
	{
		testColourHelper(new DeterministicVertex("testA"));
	}
	
	/** Tests that I can assign colours meaningfully on Deterministic and String vertices. */
	@Test
	public final void testColourSetting2()
	{
		testColourHelper(new StringVertex("testB"));
	}
	
	private static void cloneTestHelper(CmpVertex vert, Configuration conf)
	{
		vert.setAccept(true);
		CmpVertex vert_clone = LearnerGraph.cloneCmpVertex(vert, conf);
		Assert.assertNotSame(vert, vert_clone);Assert.assertEquals("test vertex", vert_clone.getName());Assert.assertEquals(vert, vert_clone);
		
		vert.setAccept(false);
		Assert.assertEquals(JUConstants.RED, vert_clone.getColour());
		Assert.assertFalse(vert.equals(vert_clone));		
	}
	
	/** Checking that cloning is faithful. */
	@Test
	public final void testVertexClone5()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(false);
		conf.setLearnerCloneGraph(true);
		
		conf.setLearnerUseStrings(true);
		CmpVertex vA = new DeterministicVertex("test vertex");vA.setColour(JUConstants.RED);
		cloneTestHelper(vA, conf);
	}
	
	/** Checking that cloning is faithful. */
	@Test
	public final void testVertexClone6()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(false);
		conf.setLearnerCloneGraph(true);
		
		conf.setLearnerUseStrings(false);
		CmpVertex vB = new DeterministicVertex("test vertex");vB.setColour(JUConstants.RED);
		cloneTestHelper(vB, conf);
	}

	@Test
	public final void testFSMStructureEquals1()
	{
		LearnerGraph a=new LearnerGraph(config),b=new LearnerGraph(config);
		a.init = new StringVertex("A");b.init = new StringVertex("A");
		Assert.assertTrue(a.equals(a));
		Assert.assertTrue(a.equals(b));

		Assert.assertFalse(a.equals(null));
		Assert.assertFalse(a.equals("hello"));
		b.init = new StringVertex("B");Assert.assertFalse(a.equals(b));
	}

	/** Tests that graphs of different kind look the same. */
	@Test
	public final void testFSMStructureEquals2()
	{
		Assert.assertTrue(testGraphString.init instanceof StringVertex);
		Assert.assertTrue(testGraphJung.init instanceof DeterministicVertex);
		Assert.assertTrue(testGraphSame.init instanceof DeterministicVertex);
		TestFSMAlgo.equalityTestingHelper(testGraphJung,testGraphString,differentA,differentB);
		TestFSMAlgo.equalityTestingHelper(testGraphJung,testGraphSame,differentA,differentB);
		TestFSMAlgo.equalityTestingHelper(testGraphString,testGraphSame,differentA,differentB);
	}

	@Test
	public final void testCopyGraph0()
	{
		DirectedSparseGraph g=new DirectedSparseGraph();
		g.addVertex(new DirectedSparseVertex());
		g.addVertex(new DirectedSparseVertex());
		DirectedSparseGraph copy = DeterministicDirectedSparseGraph.copy(g);
		Assert.assertTrue(copy.getEdges().isEmpty() && copy.getVertices().isEmpty());
	}
	
	/** Yet another test that copy works. */
	@Test
	public final void testCopyGraph1()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-a->S1", "testCopyGraph");
		DirectedSparseGraph copy=DeterministicDirectedSparseGraph.copy(g);
		LearnerGraph gS = new LearnerGraph(g,config),gC = new LearnerGraph(copy,config);
		
		Assert.assertTrue(gS.equals(gC));
	}
	
	/** Yet another test that copy works. */
	@Test
	public final void testCopyGraph2()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+TestRpniLearner.PTA3, "testCopyGraph");
		DirectedSparseGraph copy=DeterministicDirectedSparseGraph.copy(g);
		LearnerGraph gS = new LearnerGraph(g,config),gCopy = new LearnerGraph(copy,config);
		
		Assert.assertTrue(gS.equals(gCopy));
		
		// now test if all clones are faithful
		for(Edge e:(Set<Edge>)g.getEdges())
			((Set<String>)e.getUserDatum(JUConstants.LABEL)).add("junk");
		
		LearnerGraph gS_Modified = new LearnerGraph(copy,config);
		
		Assert.assertTrue(gS_Modified.equals(gCopy));
	}
	
	/** Yet another test that copy works. */
	@Test
	public final void testCopyGraph3() // this one tests that clone works
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+TestRpniLearner.PTA3, "testCopyGraph");
		LearnerGraph orig = new LearnerGraph(g,config);
		LearnerGraph copy;
		copy = orig.copy(orig.config);
		LearnerGraph gS = new LearnerGraph(orig.paths.getGraph(),config),
			gCopy = new LearnerGraph(copy.paths.getGraph(),config);
		
		Assert.assertTrue(gS.equals(gCopy));
		
		// now test if all clones are faithful by clearing the first graph.
		orig.initPTA();
		
		LearnerGraph gS_afterChange = new LearnerGraph(orig.paths.getGraph(),config), 
			gCopy_afterChange = new LearnerGraph(copy.paths.getGraph(),config);
		
		Assert.assertTrue(gCopy_afterChange.equals(gCopy));
		Assert.assertTrue(gCopy_afterChange.equals(gS));
		Assert.assertFalse(gS_afterChange.equals(gCopy));
		Assert.assertFalse(gS_afterChange.equals(gS));		
	}

	/** Tests that (1) it is possible to convert to a different type of graph 
	 * using clone and (2) getGraph does its job. 
	 */
	@Test
	public final void testFSMStructureClone3()
	{
		ArrayList<LearnerGraph> origGraphs = new ArrayList<LearnerGraph>(3);origGraphs.add(testGraphJung);origGraphs.add(testGraphSame);origGraphs.add(testGraphString);
		List<LearnerGraph> same = new LinkedList<LearnerGraph>();
		
		for(LearnerGraph g:origGraphs)
		{
			same.add(g);
			Configuration copyConfig = (Configuration)g.config.clone();
			copyConfig.setLearnerUseStrings(false);copyConfig.setLearnerCloneGraph(true);
			LearnerGraph cloneJung = g.copy(copyConfig);
			Assert.assertTrue(cloneJung.init instanceof DeterministicVertex);

			copyConfig.setLearnerUseStrings(true);copyConfig.setLearnerCloneGraph(true);
			LearnerGraph cloneStrings = g.copy(copyConfig);
			Assert.assertTrue(cloneStrings.init instanceof StringVertex);

			copyConfig.setLearnerUseStrings(false);copyConfig.setLearnerCloneGraph(false);
			LearnerGraph cloneSame = g.copy(copyConfig);
			Assert.assertTrue(cloneSame.init.getClass().equals(g.init.getClass()));
			Assert.assertSame(cloneSame.init, g.init);
			if (g.config == confSame) // if the graph we are playing with is the 
				Assert.assertSame(cloneSame.init, testGraphSame.init);// verify (of sorts) that the same vertices are being used.
			same.add(cloneJung);same.add(cloneStrings);same.add(cloneSame);
			
			// Now add results of getGraph, considering all combinations of "getGraph" configuration and "new LearnerGraph" configuration
			for(Configuration conf:Arrays.asList(new Configuration[]{confJung,confSame,confString}))
			{
				g.config.setLearnerUseStrings(false);g.config.setLearnerCloneGraph(true);
				same.add(new LearnerGraph(g.paths.getGraph(),conf));
				g.config.setLearnerUseStrings(true);g.config.setLearnerCloneGraph(true);
				same.add(new LearnerGraph(g.paths.getGraph(),conf));
				
				DirectedSparseGraph someGraph = g.paths.OrigGetGraph("someName");
				Assert.assertEquals("someName", someGraph.getUserDatum(JUConstants.TITLE));
				same.add(new LearnerGraph(someGraph,conf));
				
				g.config.setLearnerUseStrings(false);g.config.setLearnerCloneGraph(false);
				DirectedSparseGraph almostOrigGraph = g.paths.getGraph("new Name");
				Assert.assertEquals("new Name", almostOrigGraph.getUserDatum(JUConstants.TITLE));
				Assert.assertEquals(DeterministicDirectedSparseGraph.findInitial(almostOrigGraph), g.init);
				same.add(new LearnerGraph(almostOrigGraph,conf));
			}
		}
		
		for(LearnerGraph gFirst:origGraphs)
			for(LearnerGraph gSecond:origGraphs)
				TestFSMAlgo.equalityTestingHelper(gFirst,gSecond,differentA,differentB);
	}

	@Test 
	public void testCreateLabelToStateMap1() // test with empty data
	{
		assertTrue(createLabelToStateMap(new LinkedList<String>(), new StringVertex("junk"), null).isEmpty());
		Map<String,CmpVertex> map = new HashMap<String,CmpVertex>();
		assertSame(map,createLabelToStateMap(new LinkedList<String>(), new StringVertex("junk"), map));assertTrue(map.isEmpty());
	}
	
	@Test 
	public void testCreateLabelToStateMap2() // test for no changes
	{
		Map<String,CmpVertex> trans = new HashMap<String,CmpVertex>();
		trans.put("a", new StringVertex("A"));trans.put("b", new StringVertex("A"));trans.put("c", new StringVertex("B"));
		Map<String,CmpVertex> expected = new HashMap<String,CmpVertex>();expected.putAll(trans);
		assertSame(trans,createLabelToStateMap(new LinkedList<String>(), new StringVertex("junk"),trans));
		assertTrue(expected.equals(trans));
	}
	
	@Test 
	public void testCreateLabelToStateMap3() // test for correct data being added
	{
		Map<String,CmpVertex> trans = new HashMap<String,CmpVertex>();
		trans.put("a", new StringVertex("A"));trans.put("b", new StringVertex("A"));trans.put("c", new StringVertex("B"));
		Map<String,CmpVertex> expected = new HashMap<String,CmpVertex>();expected.putAll(trans);expected.put("e", new StringVertex("A"));expected.put("g", new StringVertex("A"));
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"g","e"}), new StringVertex("A"),trans));
		assertTrue(expected.equals(trans));
	}
	
	@Test 
	public void testCreateLabelToStateMap4() // test for correct data being added
	{
		Map<String,CmpVertex> trans = new HashMap<String,CmpVertex>();
		trans.put("a", new StringVertex("A"));trans.put("b", new StringVertex("A"));trans.put("c", new StringVertex("B"));
		Map<String,CmpVertex> expected = new HashMap<String,CmpVertex>();expected.putAll(trans);expected.put("e", new StringVertex("D"));expected.put("f", new StringVertex("D"));
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"f","e"}), new StringVertex("D"),trans));
		assertTrue(expected.equals(trans));
	}
	
	@Test 
	public void testCreateLabelToStateMap5() // test for correct data being added
	{
		Map<String,CmpVertex> trans = new HashMap<String,CmpVertex>();
		trans.put("a", new StringVertex("A"));trans.put("b", new StringVertex("A"));trans.put("c", new StringVertex("B"));
		Map<String,CmpVertex> expected = new HashMap<String,CmpVertex>();expected.putAll(trans);expected.put("e", new StringVertex("B"));expected.put("g", new StringVertex("B"));
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"g","e"}), new StringVertex("B"),trans));
		assertTrue(expected.equals(trans));
	}
	
	@Test 
	public void testCreateLabelToStateMap6() // test for correct data being added when an empty collection is passed
	{
		Map<String,CmpVertex> trans = new HashMap<String,CmpVertex>();
		Map<String,CmpVertex> expected = new HashMap<String,CmpVertex>();expected.put("e",new StringVertex("A"));expected.put("b",new StringVertex("A"));
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), new StringVertex("A"),trans));
		assertTrue(expected.equals(trans));
	}

	@Test 
	public void testCreateLabelToStateMap7() // test for correct data being added when null is passed
	{
		Map<String,CmpVertex> expected = new HashMap<String,CmpVertex>();expected.put("e",new StringVertex("A"));expected.put("b",new StringVertex("A"));
		assertTrue(expected.equals(createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), new StringVertex("A"),null)));
	}

	@Test 
	public void testCreateLabelToStateMap8() // test for correct detection of nondeterminism
	{
		Map<String,CmpVertex> trans = new HashMap<String,CmpVertex>();trans.put("a", new StringVertex("A"));trans.put("b", new StringVertex("A"));trans.put("c", new StringVertex("B"));
		boolean exceptionThrown = false;
		try
		{
			createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), new StringVertex("A"),trans);
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
			createLabelToStateMap(Arrays.asList(new String[] {"b","b"}), new StringVertex("A"),null);
		}
		catch(IllegalArgumentException e)
		{
			assertTrue("incorrect exception thrown",e.getMessage().contains("nondeterminism"));
			exceptionThrown = true;
		}
		
		assertTrue("exception not thrown",exceptionThrown);
	}

	@Test
	public void testGraphConstruction1()
	{
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = new LearnerGraph(buildGraph("A--a-->B-b->C-c->A","testConstruction1"),config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		expected.transitionMatrix.put(A, createLabelToStateMap(Arrays.asList(new String[] {"a"}),B,null));
		expected.transitionMatrix.put(B, createLabelToStateMap(Arrays.asList(new String[] {"b"}),C,null));
		expected.transitionMatrix.put(C, createLabelToStateMap(Arrays.asList(new String[] {"c"}),A,null));
		expected.init = expected.findVertex("A");
		
		assertEquals("A", graph.init.getName());
		//Visualiser.updateFrame(graph.paths.getGraph(), expected.paths.getGraph());Visualiser.waitForKey();
		assertEquals("incorrect transition set",true,graph.transitionMatrix.equals(expected.transitionMatrix));
		TestFSMAlgo.equalityTestingHelper(graph,expected,differentA,differentB);		
	}

	@Test
	public void testGraphConstruction2()
	{
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = new LearnerGraph(buildGraph("A--a-->B-b->C-c->A-b->B-a-#D","testConstruction2"),config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		CmpVertex D = new StringVertex("D");D.setAccept(false);
		expected.transitionMatrix.put(A, createLabelToStateMap(Arrays.asList(new String[] {"a","b"}),new StringVertex("B"),null));
		expected.transitionMatrix.put(B, createLabelToStateMap(Arrays.asList(new String[] {"b"}),new StringVertex("C"),createLabelToStateMap(Arrays.asList(new String[] {"a"}),D,null)));
		expected.transitionMatrix.put(C, createLabelToStateMap(Arrays.asList(new String[] {"c"}),new StringVertex("A"),null));
		expected.transitionMatrix.put(D, createLabelToStateMap(Collections.EMPTY_LIST,null,null));
		expected.init = expected.findVertex("A");
		
		assertEquals("A", graph.init.getName());
		//Visualiser.updateFrame(graph.paths.getGraph(), expected.paths.getGraph());Visualiser.waitForKey();
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		TestFSMAlgo.equalityTestingHelper(graph,expected,differentA,differentB);		
	}

	@Test
	public void testGraphConstruction3()
	{
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = new LearnerGraph(buildGraph("A--a-->B<-b--C-c->A-b->A-c->A\nB-d->B-p->C\nB-q->C\nB-r->C\n","testConstruction3"),config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		expected.transitionMatrix.put(A, createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),new StringVertex("A"),createLabelToStateMap(Arrays.asList(new String[] {"a"}),B,null)));
		expected.transitionMatrix.put(B, createLabelToStateMap(Arrays.asList(new String[] {"d"}),B,createLabelToStateMap(Arrays.asList(new String[] {"r","p","q"}),C,null)));
		expected.transitionMatrix.put(C, createLabelToStateMap(Arrays.asList(new String[] {"b"}),B,createLabelToStateMap(Arrays.asList(new String[] {"c"}),A,null)));
		expected.init = expected.findVertex("A");
		
		assertEquals("A", graph.init.getName());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		TestFSMAlgo.equalityTestingHelper(graph,expected,differentA,differentB);		
	}

	@Test
	public void testGraphConstruction4()
	{
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = new LearnerGraph(buildGraph("A--a-->B<-b--D-c->A-b->A-c->A\nB-d->B-p-#C\nB-q-#C\nB-r-#C\n","testConstruction4"),config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C"),D = new StringVertex("D");
		C.setAccept(false);
		expected.transitionMatrix.put(A, createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),new StringVertex("A"),createLabelToStateMap(Arrays.asList(new String[] {"a"}),B,null)));
		expected.transitionMatrix.put(B, createLabelToStateMap(Arrays.asList(new String[] {"d"}),B,createLabelToStateMap(Arrays.asList(new String[] {"r","p","q"}),C,null)));
		expected.transitionMatrix.put(D, createLabelToStateMap(Arrays.asList(new String[] {"b"}),B, createLabelToStateMap(Arrays.asList(new String[] {"c"}),A,null)));
		expected.transitionMatrix.put(C, createLabelToStateMap(Collections.EMPTY_LIST,null,null));
		expected.init = expected.findVertex("A");
		expected.findVertex("A").setAccept(true);
		expected.findVertex("B").setAccept(true);
		expected.findVertex("C").setAccept(false);
		expected.findVertex("D").setAccept(true);
		
		assertEquals("A", graph.init.getName());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		TestFSMAlgo.equalityTestingHelper(graph,expected,differentA,differentB);		
	}

	@Test
	public void testGraphConstruction5()
	{
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = new LearnerGraph(buildGraph("A--a-->B-b-#C\nA-b->A-c->A\nB-d->B-p-#C\nB-q-#C\nB-r-#C\n","testConstruction5"),config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		C.setAccept(false);
		expected.transitionMatrix.put(A, createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),A,createLabelToStateMap(Arrays.asList(new String[] {"a"}),B,null)));
		expected.transitionMatrix.put(B, createLabelToStateMap(Arrays.asList(new String[] {"d"}),B,createLabelToStateMap(Arrays.asList(new String[] {"b","r","p","q"}),C,null)));
		expected.transitionMatrix.put(C, createLabelToStateMap(Collections.EMPTY_LIST,null,null));
		expected.init = expected.findVertex("A");
		
		assertEquals("A", graph.init.getName());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		TestFSMAlgo.equalityTestingHelper(graph,expected,differentA,differentB);		
	}
	
	@Test
	public void testGraphConstruction6() // checks loop support
	{
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = new LearnerGraph(buildGraph("P-c->P<-b-Q_State<-a-P-b->P\nQ_State-a->Q_State","testConstruction6"),config);
		CmpVertex P = new StringVertex("P"), Q_State = new StringVertex("Q_State");
		expected.transitionMatrix.put(P, createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),P,createLabelToStateMap(Arrays.asList(new String[] {"a"}),Q_State,null)));
		expected.transitionMatrix.put(Q_State, createLabelToStateMap(Arrays.asList(new String[] {"a"}),Q_State,createLabelToStateMap(Arrays.asList(new String[] {"b"}),P,null)));
		expected.init = expected.findVertex("P");
		
		assertEquals("P", graph.init.getName());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		TestFSMAlgo.equalityTestingHelper(graph,expected,differentA,differentB);
	}
}
