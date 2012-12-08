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
import static statechum.analysis.learning.rpnicore.FsmParser.buildGraph;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;

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
import statechum.Helper;
import statechum.Label;
import statechum.StringVertex;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;
import static statechum.analysis.learning.rpnicore.TestEqualityComparisonAndHashCode.equalityTestingHelper;

public final class TestGraphConstruction 
{
	public TestGraphConstruction()
	{
		mainConfiguration = Configuration.getDefaultConfiguration().copy();
		mainConfiguration.setAllowedToCloneNonCmpVertex(true);
	}
	
	/** The configuration to use when running tests. */
	Configuration config = null;

	/** Converts arrays of labels to lists of labels using config - it does not really matter which configuration is used 
	 * because all of them start from a default one and do not modify label type.
	 * 
	 * @param labels what to convert
	 * @return the outcome of conversion.
	 */
	protected List<Label> labelList(String [] labels)
	{
		return AbstractLearnerGraph.buildList(Arrays.asList(labels),config);
	}
	
	private Configuration mainConfiguration = null;
	
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
		differentA = buildLearnerGraph("Q-a->A-b->B", "testFSMStructureEquals2",config);
		differentB = buildLearnerGraph("A-b->A\nB-b->B", "testFSMStructureEquals2",config);
		confString = config.copy();confString.setLearnerUseStrings(true);confString.setLearnerCloneGraph(true);
		confSame = config.copy();confSame.setLearnerUseStrings(false);confSame.setLearnerCloneGraph(false);
	}
	
	/** test with empty data. */
	@Test 
	public final void testCreateLabelToStateMap1() 
	{
		assertTrue(createLabelToStateMap(new LinkedList<Label>(), new StringVertex("junk"), null).isEmpty());
		Map<Label,List<CmpVertex>> map = new HashMap<Label,List<CmpVertex>>();
		assertSame(map,createLabelToStateMap(new LinkedList<Label>(), new StringVertex("junk"), map));assertTrue(map.isEmpty());
	}
	
	protected Label lb(String label)
	{
		return AbstractLearnerGraph.generateNewLabel(label, config);
	}
	
	/** test for no changes when nothing is to be added. */
	@Test 
	public final void testCreateLabelToStateMap2()
	{
		Map<Label,List<CmpVertex>> trans = new HashMap<Label,List<CmpVertex>>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),B=Arrays.asList(new CmpVertex[]{new StringVertex("B")});
		trans.put(lb("a"), A);trans.put(lb("b"), A);trans.put(lb("c"), B);
		Map<Label,List<CmpVertex>> expected = new HashMap<Label,List<CmpVertex>>();expected.putAll(trans);
		assertSame(trans,createLabelToStateMap(new LinkedList<Label>(), new StringVertex("junk"),trans));
		assertEquals(expected,trans);
	}
	
	/** test for correct data being added when two transitions lead to the same state as one of the existing states. */
	@Test 
	public final void testCreateLabelToStateMap3()
	{
		Map<Label,List<CmpVertex>> trans = new HashMap<Label,List<CmpVertex>>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),B=Arrays.asList(new CmpVertex[]{new StringVertex("B")});
		trans.put(lb("a"), A);trans.put(lb("b"), A);trans.put(lb("c"), B);
		Map<Label,List<CmpVertex>> expected = new HashMap<Label,List<CmpVertex>>();expected.putAll(trans);expected.put(lb("e"), A);expected.put(lb("g"), A);
		assertSame(trans,createLabelToStateMap(labelList(new String[] {"g","e"}), new StringVertex("A"),trans));
		assertEquals(expected,trans);
	}
	
	/** test for correct data being added when two transitions lead to a new state. */
	@Test 
	public final void testCreateLabelToStateMap4()
	{
		Map<Label,List<CmpVertex>> trans = new HashMap<Label,List<CmpVertex>>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),
			B=Arrays.asList(new CmpVertex[]{new StringVertex("B")}),
			D=Arrays.asList(new CmpVertex[]{new StringVertex("D")});
		trans.put(lb("a"), A);trans.put(lb("b"), A);trans.put(lb("c"), B);
		Map<Label,List<CmpVertex>> expected = new HashMap<Label,List<CmpVertex>>();expected.putAll(trans);expected.put(lb("e"),D);expected.put(lb("f"), D);
		assertSame(trans,createLabelToStateMap(labelList(new String[] {"f","e"}), new StringVertex("D"),trans));
		assertEquals(expected,trans);
	}
	
	/** test for correct data being added when two transitions lead to the same state as one of the existing states. */
	@Test 
	public final void testCreateLabelToStateMap5()
	{
		Map<Label,List<CmpVertex>> trans = new HashMap<Label,List<CmpVertex>>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),
			B=Arrays.asList(new CmpVertex[]{new StringVertex("B")});
		trans.put(lb("a"), A);trans.put(lb("b"), A);trans.put(lb("c"), B);
		Map<Label,List<CmpVertex>> expected = new HashMap<Label,List<CmpVertex>>();expected.putAll(trans);expected.put(lb("e"), B);expected.put(lb("g"), B);
		assertSame(trans,createLabelToStateMap(labelList(new String[] {"g","e"}), new StringVertex("B"),trans));
		assertEquals(expected,trans);
	}
	
	/** test for correct data being added when an empty collection is passed. */
	@Test 
	public final void testCreateLabelToStateMap6()
	{
		Map<Label,List<CmpVertex>> trans = new HashMap<Label,List<CmpVertex>>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")});
		Map<Label,List<CmpVertex>> expected = new HashMap<Label,List<CmpVertex>>();expected.put(lb("e"),A);expected.put(lb("b"),A);
		assertSame(trans,createLabelToStateMap(labelList(new String[] {"b","e"}), new StringVertex("A"),trans));
		assertEquals(expected,trans);
	}

	/** test for correct data being added when null is passed. */
	@Test 
	public final void testCreateLabelToStateMap7()
	{
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")});
		Map<Label,List<CmpVertex>> expected = new HashMap<Label,List<CmpVertex>>();expected.put(lb("e"),A);expected.put(lb("b"),A);
		assertEquals(expected,createLabelToStateMap(labelList(new String[] {"b","e"}), new StringVertex("A"),null));
	}

	/** Test for the correct handling of duplicate transitions, between a new transition and an existing one.
	 * This test cannot be replicated with conversion from Jung graphs because each vertex is a set of 
	 * labels, hence no duplicate labels can be placed on edges. 
	 */
	@Test 
	public final void testCreateLabelToStateMap8()
	{
		final Map<Label,List<CmpVertex>> trans = new HashMap<Label,List<CmpVertex>>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),
			B=Arrays.asList(new CmpVertex[]{new StringVertex("B")});
		trans.put(lb("a"), A);trans.put(lb("b"), A);trans.put(lb("c"), B);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			createLabelToStateMap(labelList(new String[] {"b","e"}), new StringVertex("A"),trans);
		}},IllegalArgumentException.class,"duplicate");
	}

	/** Test for the correct handling of duplicate transitions, between two new transitions.
	 * This test cannot be replicated with conversion from Jung graphs because each vertex is a set of 
	 * labels, hence no duplicate labels can be placed on edges. 
	 */
	@Test 
	public final void testCreateLabelToStateMap9() // test for correct detection of nondeterminism
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			createLabelToStateMap(labelList(new String[] {"b","b"}), new StringVertex("A"),null);
		}},IllegalArgumentException.class,"duplicate");
	}
	
	/** test for correct data being added in the case of nondeterminism. */
	@Test 
	public final void testCreateLabelToStateMap_nondet1()
	{
		Map<Label,List<CmpVertex>> trans = createLabelToStateMap(labelList(new String[] {"t"}), new StringVertex("A"),
			createLabelToStateMap(AbstractLearnerGraph.buildList(Arrays.asList(new String[] {"b","e"}),config), new StringVertex("A"),
				createLabelToStateMap(AbstractLearnerGraph.buildList(Arrays.asList(new String[] {"f"}),config), new StringVertex("C"),null)));
		List<CmpVertex> 
			A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),
			C=Arrays.asList(new CmpVertex[]{new StringVertex("C")}),
			AC=Arrays.asList(new CmpVertex[]{new StringVertex("A"),new StringVertex("C")});
			Map<Label,List<CmpVertex>> expected = new HashMap<Label,List<CmpVertex>>();
			expected.put(lb("e"),AC);expected.put(lb("b"),AC);expected.put(lb("f"), C);expected.put(lb("t"), A);
		assertSame(trans,createLabelToStateMap(labelList(new String[] {"b","e"}), new StringVertex("C"),trans));
		assertEquals(expected,trans);
	}

	/** Given a deterministic row represented in a non-deterministic form, converts that row to a deterministic form.
	 *  
	 * @param map what to convert
	 * @param oldToNew map used to convert vertices from those in the original map to those to be used in the result map. If null, no conversion is performed.
	 * @param from the source state of the outgoing transitions - used in an error message
	 * @return result of conversion 
	 */
	public static Map<Label,CmpVertex> convertRowToDet(LearnerGraph graph,Map<Label,List<CmpVertex>> map, Map<CmpVertex,CmpVertex> oldToNew, CmpVertex from)
	{
		Map<Label,CmpVertex> result = graph.createNewRow();
		for(Entry<Label,List<CmpVertex>> rowEntry:map.entrySet())
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
		Map<Label,CmpVertex> actual=convertRowToDet(graph,Collections.EMPTY_MAP, null, A),
			expected = graph.createNewRow();
		assertEquals(expected,actual);
	}

	/** Row with a single entry. */
	@Test
	public final void testRowConversion1()
	{
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B");
		LearnerGraph graph = new LearnerGraph(confString);
		Map<Label,CmpVertex> actual=convertRowToDet(graph,createLabelToStateMap(labelList(new String[] {"a"}),B,null), null, A),
			expected = graph.createNewRow();
		expected.put(lb("a"), B);
		assertEquals(expected,actual);
	}

	/** Row with a number of entries including a loop. */
	@Test
	public final void testRowConversion2()
	{
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		LearnerGraph graph = new LearnerGraph(confString);
		Map<Label,CmpVertex> actual=convertRowToDet(graph,
						createLabelToStateMap(labelList(new String[] {"a","b"}),B,
								createLabelToStateMap(labelList(new String[] {"e"}),A,
										createLabelToStateMap(labelList(new String[] {"c","d"}),C,null))), null, A),
			expected = graph.createNewRow();
		expected.put(lb("a"), B);expected.put(lb("b"), B);expected.put(lb("c"), C);expected.put(lb("d"), C);expected.put(lb("e"), A);
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
		Map<Label,CmpVertex> actual=convertRowToDet(graph,
						createLabelToStateMap(labelList(new String[] {"a","b"}),B,
								createLabelToStateMap(labelList(new String[] {"e"}),A,
										createLabelToStateMap(labelList(new String[] {"c","d"}),C,null))), oldToNew, A),
			expected = graph.createNewRow();
		expected.put(lb("a"), newB);expected.put(lb("b"), newB);expected.put(lb("c"), newC);expected.put(lb("d"), newC);expected.put(lb("e"), newA);
		assertEquals(expected,actual);
	}

	/** Non-determinism on transition a. */
	@Test
	public final void testRowConversion_nondet1()
	{
		final CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		final LearnerGraph graph = new LearnerGraph(confString);
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			convertRowToDet(graph,createLabelToStateMap(labelList(new String[] {"a","b"}),B,createLabelToStateMap(labelList(new String[] {"a"}),C,null)), null,A);
		}},IllegalArgumentException.class,"non-deterministic");
	}

	/** Transition with an empty target state list. */
	@Test
	public final void testRowConversion_nondet2()
	{
		final CmpVertex A = new StringVertex("A");
		final LearnerGraph graph = new LearnerGraph(confString);
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			Map<Label,List<CmpVertex>> nondetRow = new TreeMap<Label,List<CmpVertex>>();nondetRow.put(lb("a"), new LinkedList<CmpVertex>());
			convertRowToDet(graph,nondetRow, null,A);
		}},IllegalArgumentException.class,"non-deterministic");
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public final void testGraphConstruction1a()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("A--a-->B-b->C-c->A","testConstruction1",config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"a"}),B,null), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b"}),C,null), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"c"}),A,null), null, C));
		expected.setInit(expected.findVertex("A"));
		
		assertEquals("A", graph.getInit().getID().toString());
		assertEquals("incorrect transition set",true,graph.transitionMatrix.equals(expected.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB, true);		
	}
	
	/** Tests that deterministic graphs are correctly built. */
	@Test
	public final void testGraphConstruction1b()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("A--a-->B-b->C-c->A","testConstruction1",config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"a"}),B,null), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b"}),C,null), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"c"}),A,null), null, C));
		expected.setInit(expected.findVertex("A"));
		
		assertEquals("A", graph.getInit().getID().toString());
		assertEquals("incorrect transition set",true,graph.transitionMatrix.equals(expected.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB, true);		
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public final void testGraphConstruction2()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("A--a-->B-b->C-c->A-b->B-a-#D","testConstruction2",config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		CmpVertex D = new StringVertex("D");D.setAccept(false);
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"a","b"}),
				new StringVertex("B"),null), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b"}),
				new StringVertex("C"),createLabelToStateMap(labelList(new String[] {"a"}),D,null)), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"c"}),
				new StringVertex("A"),null), null, C));
		expected.transitionMatrix.put(D, convertRowToDet(expected,createLabelToStateMap(Collections.EMPTY_LIST,null,null), null, D));
		expected.setInit(expected.findVertex("A"));
		
		assertEquals("A", graph.getInit().getID().toString());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB, true);		
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public final void testGraphConstruction3()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("A--a-->B<-b--C-c->A-b->A-c->A\nB-d->B-p->C\nB-q->C\nB-r->C\n","testConstruction3",config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b","c"}),
				new StringVertex("A"),createLabelToStateMap(labelList(new String[] {"a"}),B,null)), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"d"}),B,
				createLabelToStateMap(labelList(new String[] {"r","p","q"}),C,null)), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b"}),B,
				createLabelToStateMap(labelList(new String[] {"c"}),A,null)), null, C));
		expected.setInit(expected.findVertex("A"));
		
		assertEquals("A", graph.getInit().getID().toString());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB, true);		
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public final void testGraphConstruction4()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("A--a-->B<-b--D-c->A-b->A-c->A\nB-d->B-p-#C\nB-q-#C\nB-r-#C\n","testConstruction4",config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C"),D = new StringVertex("D");
		C.setAccept(false);
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b","c"}),
				new StringVertex("A"),createLabelToStateMap(labelList(new String[] {"a"}),B,null)), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"d"}),B,
				createLabelToStateMap(labelList(new String[] {"r","p","q"}),C,null)), null, B));
		expected.transitionMatrix.put(D, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b"}),B, 
				createLabelToStateMap(labelList(new String[] {"c"}),A,null)), null, D));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(Collections.EMPTY_LIST,null,null), null, C));
		expected.setInit(expected.findVertex("A"));
		expected.findVertex("A").setAccept(true);
		expected.findVertex("B").setAccept(true);
		expected.findVertex("C").setAccept(false);
		expected.findVertex("D").setAccept(true);
		
		assertEquals("A", graph.getInit().getID().toString());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB, true);		
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public final void testGraphConstruction5()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("A--a-->B-b-#C\nA-b->A-c->A\nB-d->B-p-#C\nB-q-#C\nB-r-#C\n","testConstruction5",config);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		C.setAccept(false);
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b","c"}),A,
				createLabelToStateMap(labelList(new String[] {"a"}),B,null)), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"d"}),B,
				createLabelToStateMap(labelList(new String[] {"b","r","p","q"}),C,null)), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(Collections.EMPTY_LIST,null,null), null, C));
		expected.setInit(expected.findVertex("A"));
		
		assertEquals("A", graph.getInit().getID().toString());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB, true);		
	}
	
	/** checks support for loops. */
	@Test
	public final void testGraphConstruction6()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("P-c->P<-b-Q_State<-a-P-b->P\nQ_State-a->Q_State","testConstruction6",config);
		CmpVertex P = new StringVertex("P"), Q_State = new StringVertex("Q_State");
		expected.transitionMatrix.put(P, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b","c"}),P,
				createLabelToStateMap(labelList(new String[] {"a"}),Q_State,null)), null, P));
		expected.transitionMatrix.put(Q_State, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"a"}),Q_State,
				createLabelToStateMap(labelList(new String[] {"b"}),P,null)), null, Q_State));
		expected.setInit(expected.findVertex("P"));
		
		assertEquals("P", graph.getInit().getID().toString());
		assertEquals("incorrect transition set",true,expected.transitionMatrix.equals(graph.transitionMatrix));
		equalityTestingHelper(graph,expected,differentA,differentB, true);
	}

	/** Tests that references to unknown vertices cause an exception because we do not know whether
	 * that vertex should be accept or reject.
	 */
	@Test 
	public void testGraphConstruction_fail()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			buildGraph("A = THEN ==B","testGraphConstruction_fail",config);
		}},IllegalArgumentException.class,"unknown vertex");
	}

}
