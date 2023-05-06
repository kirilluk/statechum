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

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized.Parameters;
import junit_runners.ParameterizedWithName;
import junit_runners.ParameterizedWithName.ParametersToString;
import statechum.*;
import statechum.Configuration.LABELKIND;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.collections.MapWithSearch;

import java.util.*;
import java.util.Map.Entry;

import static org.junit.Assert.*;
import static statechum.TestHelper.checkForCorrectException;
import static statechum.analysis.learning.rpnicore.FsmParserStatechum.buildLearnerGraph;
import static statechum.analysis.learning.rpnicore.LearnerGraph.createLabelToStateMap;
import static statechum.analysis.learning.rpnicore.TestEqualityComparisonAndHashCode.equalityTestingHelper;

@RunWith(ParameterizedWithName.class)
public final class TestGraphConstruction extends TestWithMultipleConfigurations
{
	public TestGraphConstruction(Configuration conf)
	{
		super(conf);
		mainConfiguration.setAllowedToCloneNonCmpVertex(true);
	}
	
	/** The configuration to use in these tests. */
	Configuration config;
	
	@Parameters
	public static Collection<Object[]> data() 
	{
		return TestWithMultipleConfigurations.data();
	}
	
	@ParametersToString
	public static String parametersToString(Configuration config)
	{
		return TestWithMultipleConfigurations.parametersToString(config);
	}
	
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
		differentA = buildLearnerGraph("Q-a->A-b->B", "testFSMStructureEquals2",config,converter);
		differentB = buildLearnerGraph("A-b->A\nB-b->B", "testFSMStructureEquals2",config,converter);
		confString = config.copy();confString.setLearnerUseStrings(true);confString.setLearnerCloneGraph(true);
		confSame = config.copy();confSame.setLearnerUseStrings(false);confSame.setLearnerCloneGraph(false);
	}
	
	/** test with empty data. */
	@Test 
	public void testCreateLabelToStateMap1()
	{
		assertTrue(createLabelToStateMap(new LinkedList<>(), new StringVertex("junk"), null).isEmpty());
		Map<Label,List<CmpVertex>> map = new HashMap<>();
		assertSame(map,createLabelToStateMap(new LinkedList<>(), new StringVertex("junk"), map));assertTrue(map.isEmpty());
	}
	
	private Label lb(String label)
	{
		return AbstractLearnerGraph.generateNewLabel(label, config,converter);
	}
	
	@Test
	public void testCreateLabelIntern1()
	{
		final Configuration conf = config.copy();conf.setLabelKind(LABELKIND.LABEL_STRING);
		final statechum.analysis.learning.rpnicore.Transform.InternStringLabel intern = null;
		final Label lbl = AbstractLearnerGraph.generateNewLabel("A", conf,intern);
		TestHelper.checkForCorrectException(
				lbl::toInt,
				UnsupportedOperationException.class,"string labels");
	}
	
	
	@Test
	public void testCreateLabelIntern3()
	{
		final Configuration conf = config.copy();conf.setLabelKind(LABELKIND.LABEL_STRING);
		final statechum.analysis.learning.rpnicore.Transform.InternStringLabel intern = new statechum.analysis.learning.rpnicore.Transform.InternStringLabel();
		final Label lbl1 = intern.convertLabelToLabel(AbstractLearnerGraph.generateNewLabel("A", conf,intern));
		final Label lbl2 = intern.convertLabelToLabel(AbstractLearnerGraph.generateNewLabel("A", conf,intern));
		assertEquals(0,lbl1.toInt());
		assertEquals(0,lbl2.toInt());
		final Label lbl3 = intern.convertLabelToLabel(AbstractLearnerGraph.generateNewLabel("B", conf,intern));
		assertEquals(1,lbl3.toInt());		
	}
	
	@Test
	public void testCreateErlangLabelFailure1()
	{
		final Configuration conf = config.copy();conf.setLabelKind(LABELKIND.LABEL_ERLANG);
		TestHelper.checkForCorrectException(
				() -> AbstractLearnerGraph.generateNewLabel(33,conf,converter),
				IllegalArgumentException.class,"No parser");
	}
	
	/** test for no changes when nothing is to be added. */
	@Test 
	public void testCreateLabelToStateMap2()
	{
		Map<Label,List<CmpVertex>> trans = new HashMap<>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),B=Arrays.asList(new CmpVertex[]{new StringVertex("B")});
		trans.put(lb("a"), A);trans.put(lb("b"), A);trans.put(lb("c"), B);
		Map<Label, List<CmpVertex>> expected = new HashMap<>(trans);
		assertSame(trans,createLabelToStateMap(new LinkedList<>(), new StringVertex("junk"),trans));
		assertEquals(expected,trans);
	}
	
	/** test for correct data being added when two transitions lead to the same state as one of the existing states. */
	@Test 
	public void testCreateLabelToStateMap3()
	{
		Map<Label,List<CmpVertex>> trans = new HashMap<>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),B=Arrays.asList(new CmpVertex[]{new StringVertex("B")});
		trans.put(lb("a"), A);trans.put(lb("b"), A);trans.put(lb("c"), B);
		Map<Label, List<CmpVertex>> expected = new HashMap<>(trans);
		expected.put(lb("e"), A);expected.put(lb("g"), A);
		assertSame(trans,createLabelToStateMap(labelList(new String[] {"g","e"}), new StringVertex("A"),trans));
		assertEquals(expected,trans);
	}
	
	/** test for correct data being added when two transitions lead to a new state. */
	@Test 
	public void testCreateLabelToStateMap4()
	{
		Map<Label,List<CmpVertex>> trans = new HashMap<>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),
			B=Arrays.asList(new CmpVertex[]{new StringVertex("B")}),
			D=Arrays.asList(new CmpVertex[]{new StringVertex("D")});
		trans.put(lb("a"), A);trans.put(lb("b"), A);trans.put(lb("c"), B);
		Map<Label, List<CmpVertex>> expected = new HashMap<>(trans);
		expected.put(lb("e"),D);expected.put(lb("f"), D);
		assertSame(trans,createLabelToStateMap(labelList(new String[] {"f","e"}), new StringVertex("D"),trans));
		assertEquals(expected,trans);
	}
	
	/** test for correct data being added when two transitions lead to the same state as one of the existing states. */
	@Test 
	public void testCreateLabelToStateMap5()
	{
		Map<Label,List<CmpVertex>> trans = new HashMap<>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),
			B=Arrays.asList(new CmpVertex[]{new StringVertex("B")});
		trans.put(lb("a"), A);trans.put(lb("b"), A);trans.put(lb("c"), B);
		Map<Label, List<CmpVertex>> expected = new HashMap<>(trans);
		expected.put(lb("e"), B);expected.put(lb("g"), B);
		assertSame(trans,createLabelToStateMap(labelList(new String[] {"g","e"}), new StringVertex("B"),trans));
		assertEquals(expected,trans);
	}
	
	/** test for correct data being added when an empty collection is passed. */
	@Test 
	public void testCreateLabelToStateMap6()
	{
		Map<Label,List<CmpVertex>> trans = new HashMap<>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")});
		Map<Label,List<CmpVertex>> expected = new HashMap<>();expected.put(lb("e"),A);expected.put(lb("b"),A);
		assertSame(trans,createLabelToStateMap(labelList(new String[] {"b","e"}), new StringVertex("A"),trans));
		assertEquals(expected,trans);
	}

	/** test for correct data being added when null is passed. */
	@Test 
	public void testCreateLabelToStateMap7()
	{
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")});
		Map<Label,List<CmpVertex>> expected = new HashMap<>();expected.put(lb("e"),A);expected.put(lb("b"),A);
		assertEquals(expected,createLabelToStateMap(labelList(new String[] {"b","e"}), new StringVertex("A"),null));
	}

	/** Test for the correct handling of duplicate transitions, between a new transition and an existing one.
	 * This test cannot be replicated with conversion from Jung graphs because each vertex is a set of 
	 * labels, hence no duplicate labels can be placed on edges. 
	 */
	@Test 
	public void testCreateLabelToStateMap8()
	{
		final Map<Label,List<CmpVertex>> trans = new HashMap<>();
		List<CmpVertex> A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),
			B=Arrays.asList(new CmpVertex[]{new StringVertex("B")});
		trans.put(lb("a"), A);trans.put(lb("b"), A);trans.put(lb("c"), B);
		checkForCorrectException(
				() -> createLabelToStateMap(labelList(new String[] {"b","e"}), new StringVertex("A"),trans),
				IllegalArgumentException.class,"duplicate");
	}

	/** Test for the correct handling of duplicate transitions, between two new transitions.
	 * This test cannot be replicated with conversion from Jung graphs because each vertex is a set of 
	 * labels, hence no duplicate labels can be placed on edges. 
	 */
	@Test 
	public void testCreateLabelToStateMap9() // test for correct detection of nondeterminism
	{
		checkForCorrectException(
				() -> createLabelToStateMap(labelList(new String[] {"b","b"}), new StringVertex("A"),null),
				IllegalArgumentException.class,"duplicate");
	}
	
	/** test for correct data being added in the case of nondeterminism. */
	@Test 
	public void testCreateLabelToStateMap_nondet1()
	{
		Map<Label,List<CmpVertex>> trans = createLabelToStateMap(labelList(new String[] {"t"}), new StringVertex("A"),
			createLabelToStateMap(AbstractLearnerGraph.buildList(Arrays.asList("b","e"),config,converter), new StringVertex("A"),
				createLabelToStateMap(AbstractLearnerGraph.buildList(Collections.singletonList("f"),config,converter), new StringVertex("C"),null)));
		List<CmpVertex> 
			A=Arrays.asList(new CmpVertex[]{new StringVertex("A")}),
			C=Arrays.asList(new CmpVertex[]{new StringVertex("C")}),
			AC=Arrays.asList(new CmpVertex[]{new StringVertex("A"),new StringVertex("C")});
			Map<Label,List<CmpVertex>> expected = new HashMap<>();
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
	public static MapWithSearch<Label,Label,CmpVertex> convertRowToDet(LearnerGraph graph, Map<Label,List<CmpVertex>> map, Map<CmpVertex,CmpVertex> oldToNew, CmpVertex from)
	{
		MapWithSearch<Label,Label,CmpVertex> result = graph.createNewRow();
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
	public void testRowConversion0()
	{
		CmpVertex A = new StringVertex("A");
		LearnerGraph graph = new LearnerGraph(confString);
		@SuppressWarnings("unchecked")
		Map<Label,CmpVertex> actual=convertRowToDet(graph,Collections.EMPTY_MAP, null, A),
			expected = graph.createNewRow();
		assertEquals(expected,actual);
	}

	/** Row with a single entry. */
	@Test
	public void testRowConversion1()
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
	public void testRowConversion2()
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
	public void testRowConversion2_renumbered()
	{
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C"),
			newA = new StringVertex("newA"),newB = new StringVertex("newB"), newC = new StringVertex("newC");
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<>();oldToNew.put(A,newA);oldToNew.put(B,newB);oldToNew.put(C,newC);
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
	public void testRowConversion_nondet1()
	{
		final CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		final LearnerGraph graph = new LearnerGraph(confString);
		
		checkForCorrectException(
				() -> convertRowToDet(graph,createLabelToStateMap(labelList(new String[] {"a","b"}),B,
						createLabelToStateMap(labelList(new String[] {"a"}),C,null)), null,A),
				IllegalArgumentException.class,"non-deterministic");
	}

	/** Transition with an empty target state list. */
	@Test
	public void testRowConversion_nondet2()
	{
		final CmpVertex A = new StringVertex("A");
		final LearnerGraph graph = new LearnerGraph(confString);
		
		checkForCorrectException(
				() -> {
					Map<Label, List<CmpVertex>> nondetRow = new TreeMap<>();
					nondetRow.put(lb("a"), new LinkedList<>());
					convertRowToDet(graph, nondetRow, null, A);
		},IllegalArgumentException.class,"non-deterministic");
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public void testGraphConstruction1a()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("A--a-->B-b->C-c->A","testConstruction1",config,converter);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"a"}),B,null), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b"}),C,null), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"c"}),A,null), null, C));
		expected.setInit(expected.findVertex("A"));
		
		assertEquals("A", graph.getInit().getStringId());
		assertEquals("incorrect transition set", graph.transitionMatrix, expected.transitionMatrix);
		equalityTestingHelper(graph,expected,differentA,differentB, true);		
	}
	
	/** Tests that deterministic graphs are correctly built. */
	@Test
	public void testGraphConstruction1b()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("A--a-->B-b->C-c->A","testConstruction1",config,converter);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"a"}),B,null), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b"}),C,null), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"c"}),A,null), null, C));
		expected.setInit(expected.findVertex("A"));
		
		assertEquals("A", graph.getInit().getStringId());
		assertEquals("incorrect transition set", graph.transitionMatrix, expected.transitionMatrix);
		equalityTestingHelper(graph,expected,differentA,differentB, true);		
	}

	@SuppressWarnings("unchecked")
	public static final List<Label> emptyCollectionOfLabels = Collections.EMPTY_LIST;
	
	/** Tests that deterministic graphs are correctly built. */
	@Test
	public void testGraphConstruction2()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("A--a-->B-b->C-c->A-b->B-a-#D","testConstruction2",config,converter);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		CmpVertex D = new StringVertex("D");D.setAccept(false);
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"a","b"}),
				new StringVertex("B"),null), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b"}),
				new StringVertex("C"),createLabelToStateMap(labelList(new String[] {"a"}),D,null)), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"c"}),
				new StringVertex("A"),null), null, C));
		expected.transitionMatrix.put(D, convertRowToDet(expected,createLabelToStateMap(emptyCollectionOfLabels,null,null), null, D));
		expected.setInit(expected.findVertex("A"));
		
		assertEquals("A", graph.getInit().getStringId());
		assertEquals("incorrect transition set", expected.transitionMatrix, graph.transitionMatrix);
		equalityTestingHelper(graph,expected,differentA,differentB, true);		
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public void testGraphConstruction3()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("A--a-->B<-b--C-c->A-b->A-c->A\nB-d->B-p->C\nB-q->C\nB-r->C\n","testConstruction3",config,converter);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b","c"}),
				new StringVertex("A"),createLabelToStateMap(labelList(new String[] {"a"}),B,null)), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"d"}),B,
				createLabelToStateMap(labelList(new String[] {"r","p","q"}),C,null)), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b"}),B,
				createLabelToStateMap(labelList(new String[] {"c"}),A,null)), null, C));
		expected.setInit(expected.findVertex("A"));
		
		assertEquals("A", graph.getInit().getStringId());
		assertEquals("incorrect transition set", expected.transitionMatrix, graph.transitionMatrix);
		equalityTestingHelper(graph,expected,differentA,differentB, true);		
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public void testGraphConstruction4()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("A--a-->B<-b--D-c->A-b->A-c->A\nB-d->B-p-#C\nB-q-#C\nB-r-#C\n","testConstruction4",config,converter);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C"),D = new StringVertex("D");
		C.setAccept(false);
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b","c"}),
				new StringVertex("A"),createLabelToStateMap(labelList(new String[] {"a"}),B,null)), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"d"}),B,
				createLabelToStateMap(labelList(new String[] {"r","p","q"}),C,null)), null, B));
		expected.transitionMatrix.put(D, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b"}),B, 
				createLabelToStateMap(labelList(new String[] {"c"}),A,null)), null, D));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(emptyCollectionOfLabels,null,null), null, C));
		expected.setInit(expected.findVertex("A"));
		expected.findVertex("A").setAccept(true);
		expected.findVertex("B").setAccept(true);
		expected.findVertex("C").setAccept(false);
		expected.findVertex("D").setAccept(true);
		
		assertEquals("A", graph.getInit().getStringId());
		assertEquals("incorrect transition set", expected.transitionMatrix, graph.transitionMatrix);
		equalityTestingHelper(graph,expected,differentA,differentB, true);		
	}

	/** Tests that deterministic graphs are correctly built. */
	@Test
	public void testGraphConstruction5()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("A--a-->B-b-#C\nA-b->A-c->A\nB-d->B-p-#C\nB-q-#C\nB-r-#C\n","testConstruction5",config,converter);
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C = new StringVertex("C");
		C.setAccept(false);
		expected.transitionMatrix.put(A, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b","c"}),A,
				createLabelToStateMap(labelList(new String[] {"a"}),B,null)), null, A));
		expected.transitionMatrix.put(B, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"d"}),B,
				createLabelToStateMap(labelList(new String[] {"b","r","p","q"}),C,null)), null, B));
		expected.transitionMatrix.put(C, convertRowToDet(expected,createLabelToStateMap(emptyCollectionOfLabels,null,null), null, C));
		expected.setInit(expected.findVertex("A"));
		
		assertEquals("A", graph.getInit().getStringId());
		assertEquals("incorrect transition set", expected.transitionMatrix, graph.transitionMatrix);
		equalityTestingHelper(graph,expected,differentA,differentB, true);		
	}
	
	/** checks support for loops. */
	@Test
	public void testGraphConstruction6()
	{
		confString.setUseOrderedEntrySet(true);config.setUseOrderedEntrySet(true);
		LearnerGraph expected = new LearnerGraph(confString);expected.initEmpty();
		LearnerGraph graph = buildLearnerGraph("P-c->P<-b-Q<-a-P-b->P\nQ-a->Q","testConstruction6",config,converter);
		CmpVertex P = new StringVertex("P"), Q_State = new StringVertex("Q");
		expected.transitionMatrix.put(P, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"b","c"}),P,
				createLabelToStateMap(labelList(new String[] {"a"}),Q_State,null)), null, P));
		expected.transitionMatrix.put(Q_State, convertRowToDet(expected,createLabelToStateMap(labelList(new String[] {"a"}),Q_State,
				createLabelToStateMap(labelList(new String[] {"b"}),P,null)), null, Q_State));
		expected.setInit(expected.findVertex("P"));
		
		assertEquals("P", graph.getInit().getStringId());
		assertEquals("incorrect transition set", expected.transitionMatrix, graph.transitionMatrix);
		equalityTestingHelper(graph,expected,differentA,differentB, true);
	}

	/** Tests that references to unknown vertices cause an exception because we do not know whether
	 * that vertex should be accept or reject.
	 */
	@Test 
	public void testGraphConstruction_fail()
	{
		TestHelper.checkForCorrectException(
				() -> buildLearnerGraph("A = THEN ==B","testGraphConstruction_fail",config,converter),
				IllegalArgumentException.class,"unknown vertex");
	}

}
