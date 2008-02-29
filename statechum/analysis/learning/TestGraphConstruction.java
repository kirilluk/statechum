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
package statechum.analysis.learning;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static statechum.xmachine.model.testset.WMethod.createLabelToStateMap;
import static statechum.xmachine.model.testset.WMethod.getGraphData;
import static statechum.analysis.learning.TestFSMAlgo.buildGraph;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import junit.framework.Assert;

import org.junit.Test;

import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.WMethod;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class TestGraphConstruction {
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
		v.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);v.addUserDatum(JUConstants.LABEL, "B", UserData.SHARED);
		checkWithVertex(v, "multiple", "testGraphConstructionFail2");
	}
	
	@Test
	public void testGraphConstructionFail3()
	{
		DirectedSparseVertex v = new DirectedSparseVertex();
		v.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);v.addUserDatum(JUConstants.LABEL, "CONFL", UserData.SHARED);
		checkWithVertex(v, "multiple", "testGraphConstructionFail3");
	}
	
	@Test
	public void testGraphConstructionFail4()
	{
		DirectedSparseVertex v = new DirectedSparseVertex();
		v.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);v.addUserDatum(JUConstants.LABEL, "Q", UserData.SHARED);v.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		checkWithVertex(v, "duplicate", "testGraphConstructionFail4");
	}
	
	@Test
	public void testGraphConstructionFail5a()
	{
		DirectedSparseVertex v = new DirectedSparseVertex();
		v.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);v.addUserDatum(JUConstants.LABEL, "Q", UserData.SHARED);v.addUserDatum(JUConstants.INITIAL, "aa", UserData.SHARED);
		checkWithVertex(v, "invalid init property", "testGraphConstructionFail5a");
	}
	
	@Test
	public void testGraphConstructionFail5b()
	{
		DirectedSparseVertex v = new DirectedSparseVertex();
		v.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);v.addUserDatum(JUConstants.LABEL, "Q", UserData.SHARED);v.addUserDatum(JUConstants.INITIAL, false, UserData.SHARED);
		checkWithVertex(v, "invalid init property", "testGraphConstructionFail5b");
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
		init.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);g.addVertex(init);
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

}
