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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static statechum.analysis.learning.rpnicore.LearnerGraph.createLabelToStateMap;
import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildGraph;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.StringVertex;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;
import static statechum.analysis.learning.rpnicore.TestEqualityComparisonAndHashCode.equalityTestingHelper;

public final class TestGraphConstruction 
{
	public TestGraphConstruction()
	{
		mainConfiguration = Configuration.getDefaultConfiguration();
		mainConfiguration.setAllowedToCloneNonCmpVertex(true);
	}
	
	/** The configuration to use when running tests. */
	private Configuration config = null, mainConfiguration = null;
	
	/** Configuration settings used to test creation/cloning of graphs. */
	private Configuration confString,confSame;

	private LearnerGraph differentA = null, differentB = null;
	
	/** Make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	@Before
	public void beforeTest()
	{
		config = mainConfiguration.copy();
		differentA = new LearnerGraph(buildGraph("Q-a->A-b->B", "testFSMStructureEquals2"),config);
		differentB = new LearnerGraph(buildGraph("A-b->A\nB-b->B", "testFSMStructureEquals2"),config);
		confString = config.copy();confString.setLearnerUseStrings(true);confString.setLearnerCloneGraph(true);
		confSame = config.copy();confSame.setLearnerUseStrings(false);confSame.setLearnerCloneGraph(false);
	}
	
	/** test with empty data. */
	@Test 
	public final void testCreateLabelToStateMap1() 
	{
		assertTrue(createLabelToStateMap(new LinkedList<String>(), new StringVertex("junk"), null).isEmpty());
		Map<String,List<CmpVertex>> map = new HashMap<String,List<CmpVertex>>();
		assertSame(map,createLabelToStateMap(new LinkedList<String>(), new StringVertex("junk"), map));assertTrue(map.isEmpty());
	}
	
	/** test for no changes when nothing is to be added. */
	@Test 
	public final void testCreateLabelToStateMap2()
	{
		Map<String,List<CmpVertex>> trans = new HashMap<String,List<CmpVertex>>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),B=Arrays.asList(new CmpVertex[]{new StringVertex("B")});
		trans.put("a", A);trans.put("b", A);trans.put("c", B);
		Map<String,List<CmpVertex>> expected = new HashMap<String,List<CmpVertex>>();expected.putAll(trans);
		assertSame(trans,createLabelToStateMap(new LinkedList<String>(), new StringVertex("junk"),trans));
		assertEquals(expected,trans);
	}
	
	/** test for correct data being added when two transitions lead to the same state as one of the existing states. */
	@Test 
	public final void testCreateLabelToStateMap3()
	{
		Map<String,List<CmpVertex>> trans = new HashMap<String,List<CmpVertex>>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),B=Arrays.asList(new CmpVertex[]{new StringVertex("B")});
		trans.put("a", A);trans.put("b", A);trans.put("c", B);
		Map<String,List<CmpVertex>> expected = new HashMap<String,List<CmpVertex>>();expected.putAll(trans);expected.put("e", A);expected.put("g", A);
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"g","e"}), new StringVertex("A"),trans));
		assertEquals(expected,trans);
	}
	
	/** test for correct data being added when two transitions lead to a new state. */
	@Test 
	public final void testCreateLabelToStateMap4()
	{
		Map<String,List<CmpVertex>> trans = new HashMap<String,List<CmpVertex>>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),
			B=Arrays.asList(new CmpVertex[]{new StringVertex("B")}),
			D=Arrays.asList(new CmpVertex[]{new StringVertex("D")});
		trans.put("a", A);trans.put("b", A);trans.put("c", B);
		Map<String,List<CmpVertex>> expected = new HashMap<String,List<CmpVertex>>();expected.putAll(trans);expected.put("e",D);expected.put("f", D);
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"f","e"}), new StringVertex("D"),trans));
		assertEquals(expected,trans);
	}
	
	/** test for correct data being added when two transitions lead to the same state as one of the existing states. */
	@Test 
	public final void testCreateLabelToStateMap5()
	{
		Map<String,List<CmpVertex>> trans = new HashMap<String,List<CmpVertex>>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),
			B=Arrays.asList(new CmpVertex[]{new StringVertex("B")});
		trans.put("a", A);trans.put("b", A);trans.put("c", B);
		Map<String,List<CmpVertex>> expected = new HashMap<String,List<CmpVertex>>();expected.putAll(trans);expected.put("e", B);expected.put("g", B);
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"g","e"}), new StringVertex("B"),trans));
		assertEquals(expected,trans);
	}
	
	/** test for correct data being added when an empty collection is passed. */
	@Test 
	public final void testCreateLabelToStateMap6()
	{
		Map<String,List<CmpVertex>> trans = new HashMap<String,List<CmpVertex>>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")});
		Map<String,List<CmpVertex>> expected = new HashMap<String,List<CmpVertex>>();expected.put("e",A);expected.put("b",A);
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), new StringVertex("A"),trans));
		assertEquals(expected,trans);
	}

	/** test for correct data being added when null is passed. */
	@Test 
	public final void testCreateLabelToStateMap7()
	{
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")});
		Map<String,List<CmpVertex>> expected = new HashMap<String,List<CmpVertex>>();expected.put("e",A);expected.put("b",A);
		assertEquals(expected,createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), new StringVertex("A"),null));
	}

	/** Test for the correct handling of duplicate transitions, between a new transition and an existing one.
	 * This test cannot be replicated with conversion from Jung graphs because each vertex is a set of 
	 * labels, hence no duplicate labels can be placed on edges. 
	 */
	@Test 
	public final void testCreateLabelToStateMap8()
	{
		final Map<String,List<CmpVertex>> trans = new HashMap<String,List<CmpVertex>>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),
			B=Arrays.asList(new CmpVertex[]{new StringVertex("B")});
		trans.put("a", A);trans.put("b", A);trans.put("c", B);
		checkForCorrectException(new whatToRun() { public void run() {
			createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), new StringVertex("A"),trans);
		}},IllegalArgumentException.class,"duplicate");
	}

	/** Test for the correct handling of duplicate transitions, between two new transitions.
	 * This test cannot be replicated with conversion from Jung graphs because each vertex is a set of 
	 * labels, hence no duplicate labels can be placed on edges. 
	 */
	@Test 
	public final void testCreateLabelToStateMap9() // test for correct detection of nondeterminism
	{
		checkForCorrectException(new whatToRun() { public void run() {
			createLabelToStateMap(Arrays.asList(new String[] {"b","b"}), new StringVertex("A"),null);
		}},IllegalArgumentException.class,"duplicate");
	}
	
	/** test for correct data being added in the case of nondeterminism. */
	@Test 
	public final void testCreateLabelToStateMap_nondet1()
	{
		Map<String,List<CmpVertex>> trans = createLabelToStateMap(Arrays.asList(new String[] {"t"}), new StringVertex("A"),
			createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), new StringVertex("A"),
				createLabelToStateMap(Arrays.asList(new String[] {"f"}), new StringVertex("C"),null)));
		List<CmpVertex> 
			A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),
			C=Arrays.asList(new CmpVertex[]{new StringVertex("C")}),
			AC=Arrays.asList(new CmpVertex[]{new StringVertex("A"),new StringVertex("C")});
			Map<String,List<CmpVertex>> expected = new HashMap<String,List<CmpVertex>>();
			expected.put("e",AC);expected.put("b",AC);expected.put("f", C);expected.put("t", A);
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), new StringVertex("C"),trans));
		assertEquals(expected,trans);
	}

	/** Given a deterministic row represented in a non-deterministic form, converts that row to a deterministic form.
	 *  
	 * @param map what to convert
	 * @param oldToNew map used to convert vertices from those in the original map to those to be used in the result map. If null, no conversion is performed.
	 * @param from the source state of the outgoing transitions - used in an error message
	 * @return result of conversion 
	 */
	public static Map<String,CmpVertex> convertRowToDet(LearnerGraph graph,Map<String,List<CmpVertex>> map, Map<CmpVertex,CmpVertex> oldToNew, CmpVertex from)
	{
		Map<String,CmpVertex> result = graph.createNewRow();
		for(Entry<String,List<CmpVertex>> rowEntry:map.entrySet())
		{
			if (rowEntry.getValue().size() != 1)
				throw new IllegalArgumentException("non-deterministic or empty target state for transition from state "+from+" with label "+rowEntry.getKey());
			CmpVertex target = rowEntry.getValue().iterator().next();if (oldToNew != null) target = oldToNew.get(target);
			assert target != null;
			result.put(rowEntry.getKey(),target);
		}
		return result;
	}

	/** Empty row. */
	@Test
	public final void testRowConversion0()
	{
		CmpVertex A = new StringVertex("A");
		LearnerGraph graph = new LearnerGraph(confString);
		Map<String,CmpVertex> actual=convertRowToDet(graph,Collections.EMPTY_MAP, null, A),
			expected = graph.createNewRow();
		assertEquals(expected,actual);
	}

	/** Row with a single entry. */
	@Test
	public final void testRowConversion1()
	{
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B");
		LearnerGraph graph = new LearnerGraph(confString);
		Map<String,CmpVertex> actual=convertRowToDet(graph,createLabelToStateMap(Arrays.asList(new String[] {"a"}),B,null), null, A),
			expected = graph.createNewRow();
		expected.put("a", B);
		assertEquals(expected,actual);
	}

	/** Row with a number of entries including a loop. */
	@Test
	public final void testRowConversion2()
	{
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		LearnerGraph graph = new LearnerGraph(confString);
		Map<String,CmpVertex> actual=convertRowToDet(graph,
						createLabelToStateMap(Arrays.asList(new String[] {"a","b"}),B,
								createLabelToStateMap(Arrays.asList(new String[] {"e"}),A,
										createLabelToStateMap(Arrays.asList(new String[] {"c","d"}),C,null))), null, A),
			expected = graph.createNewRow();
		expected.put("a", B);expected.put("b", B);expected.put("c", C);expected.put("d", C);expected.put("e", A);
		assertEquals(expected,actual);
	}

	/** Row with multiple entries and a map for renumbering of vertices. */
	@Test
	public final void testRowConversion2_renumbered()
	{
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C"),
			newA = new StringVertex("newA"),newB = new StringVertex("newB"), newC = new StringVertex("newC");
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();oldToNew.put(A,newA);oldToNew.put(B,newB);oldToNew.put(C,newC);
		LearnerGraph graph = new LearnerGraph(confString);
		Map<String,CmpVertex> actual=convertRowToDet(graph,
						createLabelToStateMap(Arrays.asList(new String[] {"a","b"}),B,
								createLabelToStateMap(Arrays.asList(new String[] {"e"}),A,
										createLabelToStateMap(Arrays.asList(new String[] {"c","d"}),C,null))), oldToNew, A),
			expected = graph.createNewRow();
		expected.put("a", newB);expected.put("b", newB);expected.put("c", newC);expected.put("d", newC);expected.put("e", newA);
		assertEquals(expected,actual);
	}

	/** Non-determinism on transition a. */
	@Test
	public final void testRowConversion_nondet1()
	{
		final CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		final LearnerGraph graph = new LearnerGraph(confString);
		
		checkForCorrectException(new whatToRun() { public void run() {
			convertRowToDet(graph,createLabelToStateMap(Arrays.asList(new String[] {"a","b"}),B,createLabelToStateMap(Arrays.asList(new String[] {"a"}),C,null)), null,A);
		}},IllegalArgumentException.class,"non-deterministic");
	}

	/** Transition with an empty target state list. */
	@Test
	public final void testRowConversion_nondet2()
	{
		final CmpVertex A = new StringVertex("A");
		final LearnerGraph graph = new LearnerGraph(confString);
		
		checkForCorrectException(new whatToRun() { public void run() {
			Map<String,List<CmpVertex>> nondetRow = new TreeMap<String,List<CmpVertex>>();nondetRow.put("a", new LinkedList<CmpVertex>());
			convertRowToDet(graph,nondetRow, null,A);
		}},IllegalArgumentException.class,"non-deterministic");
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public final void testGraphConstruction1a()
	{
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = new LearnerGraph(buildGraph("A--a-->B-b->C-c->A","testConstruction1"),config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"a"}),B,null), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"b"}),C,null), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"c"}),A,null), null, C));
		expected.init = expected.findVertex("A");
		
		assertEquals("A", graph.init.getID().toString());
		assertEquals("incorrect transition set",true,graph.transitionMatrix.equals(expected.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB);		
	}
	
	/** Tests that deterministic graphs are correctly built. */
	@Test
	public final void testGraphConstruction1b()
	{
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = new LearnerGraph(buildGraph("A--a-->B-b->C-c->A","testConstruction1"),config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"a"}),B,null), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"b"}),C,null), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"c"}),A,null), null, C));
		expected.init = expected.findVertex("A");
		
		assertEquals("A", graph.init.getID().toString());
		assertEquals("incorrect transition set",true,graph.transitionMatrix.equals(expected.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB);		
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public final void testGraphConstruction2()
	{
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = new LearnerGraph(buildGraph("A--a-->B-b->C-c->A-b->B-a-#D","testConstruction2"),config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		CmpVertex D = new StringVertex("D");D.setAccept(false);
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"a","b"}),
				new StringVertex("B"),null), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"b"}),
				new StringVertex("C"),createLabelToStateMap(Arrays.asList(new String[] {"a"}),D,null)), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"c"}),
				new StringVertex("A"),null), null, C));
		expected.transitionMatrix.put(D, convertRowToDet(expected,createLabelToStateMap(Collections.EMPTY_LIST,null,null), null, D));
		expected.init = expected.findVertex("A");
		
		assertEquals("A", graph.init.getID().toString());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB);		
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public final void testGraphConstruction3()
	{
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = new LearnerGraph(buildGraph("A--a-->B<-b--C-c->A-b->A-c->A\nB-d->B-p->C\nB-q->C\nB-r->C\n","testConstruction3"),config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),
				new StringVertex("A"),createLabelToStateMap(Arrays.asList(new String[] {"a"}),B,null)), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"d"}),B,
				createLabelToStateMap(Arrays.asList(new String[] {"r","p","q"}),C,null)), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"b"}),B,
				createLabelToStateMap(Arrays.asList(new String[] {"c"}),A,null)), null, C));
		expected.init = expected.findVertex("A");
		
		assertEquals("A", graph.init.getID().toString());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB);		
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public final void testGraphConstruction4()
	{
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = new LearnerGraph(buildGraph("A--a-->B<-b--D-c->A-b->A-c->A\nB-d->B-p-#C\nB-q-#C\nB-r-#C\n","testConstruction4"),config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C"),D = new StringVertex("D");
		C.setAccept(false);
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),
				new StringVertex("A"),createLabelToStateMap(Arrays.asList(new String[] {"a"}),B,null)), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"d"}),B,
				createLabelToStateMap(Arrays.asList(new String[] {"r","p","q"}),C,null)), null, B));
		expected.transitionMatrix.put(D, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"b"}),B, 
				createLabelToStateMap(Arrays.asList(new String[] {"c"}),A,null)), null, D));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(Collections.EMPTY_LIST,null,null), null, C));
		expected.init = expected.findVertex("A");
		expected.findVertex("A").setAccept(true);
		expected.findVertex("B").setAccept(true);
		expected.findVertex("C").setAccept(false);
		expected.findVertex("D").setAccept(true);
		
		assertEquals("A", graph.init.getID().toString());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB);		
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public final void testGraphConstruction5()
	{
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = new LearnerGraph(buildGraph("A--a-->B-b-#C\nA-b->A-c->A\nB-d->B-p-#C\nB-q-#C\nB-r-#C\n","testConstruction5"),config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		C.setAccept(false);
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),A,
				createLabelToStateMap(Arrays.asList(new String[] {"a"}),B,null)), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"d"}),B,
				createLabelToStateMap(Arrays.asList(new String[] {"b","r","p","q"}),C,null)), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(Collections.EMPTY_LIST,null,null), null, C));
		expected.init = expected.findVertex("A");
		
		assertEquals("A", graph.init.getID().toString());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB);		
	}
	
	/** checks support for loops. */
	@Test
	public final void testGraphConstruction6()
	{
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = new LearnerGraph(buildGraph("P-c->P<-b-Q_State<-a-P-b->P\nQ_State-a->Q_State","testConstruction6"),config);
		CmpVertex P = new StringVertex("P"), Q_State = new StringVertex("Q_State");
		expected.transitionMatrix.put(P, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),P,
				createLabelToStateMap(Arrays.asList(new String[] {"a"}),Q_State,null)), null, P));
		expected.transitionMatrix.put(Q_State, convertRowToDet(expected,createLabelToStateMap(Arrays.asList(new String[] {"a"}),Q_State,
				createLabelToStateMap(Arrays.asList(new String[] {"b"}),P,null)), null, Q_State));
		expected.init = expected.findVertex("P");
		
		assertEquals("P", graph.init.getID().toString());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB);
	}
}
