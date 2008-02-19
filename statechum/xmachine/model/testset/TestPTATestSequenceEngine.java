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

package statechum.xmachine.model.testset;

import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.PTATestSequenceEngine.Node;
import statechum.xmachine.model.testset.PTATestSequenceEngine.sequenceSet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class TestPTATestSequenceEngine {

	static void vertifyPTA(PTATestSequenceEngine en, String[][] expected) {
		Set<List<String>> actual = new HashSet<List<String>>();
		actual.addAll(en.getData());
		Set<List<String>> expectedSet = TestFSMAlgo.buildSet(expected);
		assertTrue("expected: " + expectedSet + " received : " + actual,
				expectedSet.equals(actual));
	}

	@Test
	public final void testGetData_Empty1() {
		DirectedSparseGraph g = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, false, UserData.SHARED);
		init.addUserDatum(JUConstants.LABEL, "A", UserData.SHARED);
		g.addVertex(init);
		PTATestSequenceEngine en = new PTA_FSMStructure(WMethod.getGraphData(g));
		vertifyPTA(en, new String[][] { new String[] {} });
	}

	@Test
	public final void testGetData_Empty2() {
		DirectedSparseGraph g = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
		init.addUserDatum(JUConstants.LABEL, "A", UserData.SHARED);
		g.addVertex(init);
		PTATestSequenceEngine en = new PTA_FSMStructure(WMethod.getGraphData(g));
		vertifyPTA(en, new String[][] { new String[] {} });
	}

	@Test
	public final void testNodeEquality() {
		PTATestSequenceEngine engine = new PTATestSequenceEngine();
		Node a = engine.new Node("A"), b = engine.new Node("A"), c = engine.new Node(
				"B");
		assertTrue(a.equals(a));
		assertTrue(b.equals(b));
		assertTrue(engine.rejectNode.equals(engine.rejectNode));
		assertFalse(a.equals(b));
		assertFalse(b.equals(a));
		assertFalse(a.equals(c));
		assertFalse(a.equals(engine.rejectNode));

		assertFalse(a.equals("test"));
		assertFalse(a.equals(null));
	}

	@Test
	public final void testNode1() {
		PTATestSequenceEngine engine = new PTATestSequenceEngine();
		Node a = engine.new Node("A");

		assertTrue(a.isAccept());
		assertTrue(engine.new Node("test").isAccept());
		assertFalse(engine.rejectNode.isAccept());
	}

	@Test
	public final void testNode2() {
		PTATestSequenceEngine engine = new PTATestSequenceEngine();
		Node a = engine.new Node("A"), b = engine.new Node("A"), c = engine.new Node(
				"B");
		assertEquals("A", a.getState());
		assertEquals("A", b.getState());
		assertEquals("B", c.getState());

		int aID = a.getID(), bID = b.getID(), cID = c.getID();
		assertTrue(aID > 0 && bID > 0 && cID > 0);
		assertTrue(aID != bID && aID != cID && aID != bID);

		assertTrue(a.hashCode() != b.hashCode() && a.hashCode() != c.hashCode()
				&& b.hashCode() != c.hashCode());
		assertTrue(a.hashCode() != engine.rejectNode.hashCode());
		assertTrue(b.hashCode() != engine.rejectNode.hashCode());
		assertTrue(c.hashCode() != engine.rejectNode.hashCode());
	}

	private PTATestSequenceEngine en = null;

	private FSMStructure fsm = null;

	@Before
	public final void setUp() {
		fsm = WMethod.getGraphData(TestFSMAlgo.buildGraph(
				"A-a->B-a->A-b-#C\nB-b->D-c->E", "test_sequenceSet1-3_4"));
		en = new PTA_FSMStructure(fsm);
	}

	@Test
	public final void test_sequenceSet1() {
		sequenceSet seq = en.new sequenceSet();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(Arrays
				.asList(new String[] { "a" }))); // appending anything to an
													// empty sequence produces
													// an empty sequence.
		vertifyPTA(en, new String[][] { new String[] {} });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet2() // an input which exists
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(Arrays
				.asList(new String[] { "a" })));
		vertifyPTA(en, new String[][] { new String[] { "a" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] { new Object[] {
						new String[] { "a" },
						PTATestSequenceEngine.DebugDataValues.booleanToString(
								true, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet3() // the one which does not exist
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(Arrays
				.asList(new String[] { "c" })));
		vertifyPTA(en, new String[][] { new String[] { "c" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet4() // the one which enters a reject
											// state
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(Arrays
				.asList(new String[] { "b" })));
		vertifyPTA(en, new String[][] { new String[] { "b" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet5() // a composition of sequenceSet with
											// an input which exists
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "a" })));
		vertifyPTA(en, new String[][] { new String[] { "a", "a" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] { new Object[] {
						new String[] { "a", "a" },
						PTATestSequenceEngine.DebugDataValues.booleanToString(
								true, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet6() // a composition of sequenceSet with
											// the one which does not exist
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(
				Arrays.asList(new String[] { "c" })).crossWithSet(
				Arrays.asList(new String[] { "a" })));
		vertifyPTA(en, new String[][] { new String[] { "c" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet7() // a composition of sequenceSet with
											// the one which enters a reject
											// state
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(
				Arrays.asList(new String[] { "b" })).crossWithSet(
				Arrays.asList(new String[] { "a" })));
		vertifyPTA(en, new String[][] { new String[] { "b" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet_2_1() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "b" })).crossWithSet(
				Arrays.asList(new String[] { "c" })));
		vertifyPTA(en,
				new String[][] { new String[] { "a", "a", "a", "b", "c" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] { new Object[] {
						new String[] { "a", "a", "a", "b", "c" },
						PTATestSequenceEngine.DebugDataValues.booleanToString(
								true, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet2_2() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "b" })).crossWithSet(
				Arrays.asList(new String[] { "c" })).crossWithSet(
				Arrays.asList(new String[] { "a" })));
		vertifyPTA(en, new String[][] { new String[] { "a", "a", "a", "b", "c",
				"a" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet2_3() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "b" })).crossWithSet(
				Arrays.asList(new String[] { "c" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "b" })));
		vertifyPTA(en, new String[][] { new String[] { "a", "a", "a", "b", "c",
				"a" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet2_4() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(
				Arrays.asList(new String[] { "c" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "b" })).crossWithSet(
				Arrays.asList(new String[] { "c" })).crossWithSet(
				Arrays.asList(new String[] { "a" })));
		vertifyPTA(en, new String[][] { new String[] { "c" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet2_5() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		sequenceSet tempE = seq.crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "b" })).crossWithSet(
				Arrays.asList(new String[] { "c" }));
		Map<String, String> actual2 = en.getDebugDataMap(tempE
				.crossWithSet(Arrays.asList(new String[] { "a" })));
		Map<String, String> actual3 = en.getDebugDataMap(tempE
				.crossWithSet(Arrays.asList(new String[] { "b" })));
		Map<String, String> actual1 = en.getDebugDataMap(tempE);// if I do this
																// before PTA is
																// updated, the
																// long path
																// returned by
																// getDebugDataMap
																// will have its
																// nodes marked
																// as leaves,
																// but after PTA
																// is built,
																// they are no
																// long leaves.
		vertifyPTA(en, new String[][] {
				new String[] { "a", "a", "a", "b", "c", "a" },
				new String[] { "a", "a", "a", "b", "c", "b" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] { new Object[] {
						new String[] { "a", "a", "a", "b", "c" },
						PTATestSequenceEngine.DebugDataValues.booleanToString(
								false, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual1,
				expected.equals(actual1));

		expected = TestFSMAlgo.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual2,
				expected.equals(actual2));
		Assert.assertTrue("expected: " + expected + ", actual: " + actual3,
				expected.equals(actual3));
	}

	@Test
	public final void test_sequenceSet2_6() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		sequenceSet temp0 = seq.crossWithSet(
				Arrays.asList(new String[] { "b" })).crossWithSet(
				Arrays.asList(new String[] { "a" }));

		sequenceSet tempE = seq.crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "b" })).crossWithSet(
				Arrays.asList(new String[] { "c" }));
		Map<String, String> actual3 = en.getDebugDataMap(tempE
				.crossWithSet(Arrays.asList(new String[] { "a" })));
		Map<String, String> actual4 = en.getDebugDataMap(tempE
				.crossWithSet(Arrays.asList(new String[] { "b" })));
		Map<String, String> actual2 = en.getDebugDataMap(tempE);
		Map<String, String> actual1 = en.getDebugDataMap(temp0);
		vertifyPTA(en, new String[][] {
				new String[] { "a", "a", "a", "b", "c", "a" },
				new String[] { "a", "a", "a", "b", "c", "b" },
				new String[] { "b" } });

		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] { new Object[] {
						new String[] { "a", "a", "a", "b", "c" },
						PTATestSequenceEngine.DebugDataValues.booleanToString(
								false, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual2,
				expected.equals(actual2));
		expected = TestFSMAlgo.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual1,
				expected.equals(actual1));
		Assert.assertTrue("expected: " + expected + ", actual: " + actual3,
				expected.equals(actual3));
		Assert.assertTrue("expected: " + expected + ", actual: " + actual4,
				expected.equals(actual4));
	}

	@Test
	public final void test_sequenceSet3_1() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(Arrays
				.asList(new String[] { "b", "a", "d" })));
		vertifyPTA(en, new String[][] { new String[] { "b" },
				new String[] { "a" }, new String[] { "d" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] { new Object[] {
						new String[] { "a" },
						PTATestSequenceEngine.DebugDataValues.booleanToString(
								true, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet3_2() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(
				Arrays.asList(new String[] { "b", "a", "d" })).crossWithSet(
				Arrays.asList(new String[] { "b", "a", "d" })));
		vertifyPTA(en, new String[][] { new String[] { "b" },
				new String[] { "a", "a" }, new String[] { "a", "d" },
				new String[] { "a", "b" }, new String[] { "d" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {
						new Object[] {
								new String[] { "a", "a" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(true, true) },
						new Object[] {
								new String[] { "a", "b" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(true, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet3_3() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		sequenceSet temp = seq.crossWithSet(Arrays.asList(new String[] { "b",
				"a", "d" }));
		sequenceSet temp2 = temp.crossWithSet(Arrays.asList(new String[] { "b",
				"a", "d" }));
		Map<String, String> actual3 = en.getDebugDataMap(temp2
				.crossWithSet(Arrays.asList(new String[] { "u", "a", "d" })));
		Map<String, String> actual1 = en.getDebugDataMap(temp);
		Map<String, String> actual2 = en.getDebugDataMap(temp2);
		vertifyPTA(en, new String[][] { new String[] { "b" },
				new String[] { "d" }, new String[] { "a", "a", "a" },
				new String[] { "a", "b", "u" }, new String[] { "a", "b", "a" },
				new String[] { "a", "b", "d" }, new String[] { "a", "a", "d" },
				new String[] { "a", "a", "u" }, new String[] { "a", "d" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] { new Object[] {
						new String[] { "a" },
						PTATestSequenceEngine.DebugDataValues.booleanToString(
								false, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual1,
				expected.equals(actual1));
		expected = TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[] {
						new String[] { "a", "a" },
						PTATestSequenceEngine.DebugDataValues.booleanToString(
								false, true) },
				new Object[] {
						new String[] { "a", "b" },
						PTATestSequenceEngine.DebugDataValues.booleanToString(
								false, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual2,
				expected.equals(actual2));
		expected = TestFSMAlgo.buildStringMap(new Object[][] { new Object[] {
				new String[] { "a", "a", "a" },
				PTATestSequenceEngine.DebugDataValues.booleanToString(true,
						true) }, });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual3,
				expected.equals(actual3));
	}

	@Test
	public final void test_sequenceSet3_4() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		sequenceSet temp = seq.crossWithSet(Arrays.asList(new String[] { "b",
				"a", "d" }));
		sequenceSet temp_0 = temp.crossWithSet(
				Arrays.asList(new String[] { "b", "a", "d" })).crossWithSet(
				Arrays.asList(new String[] { "u", "a", "d" }));
		sequenceSet temp_1 = temp.crossWithSet(Arrays.asList(new String[] {
				"u", "a", "d" }));
		sequenceSet temp2 = temp.crossWithSet(
				Arrays.asList(new String[] { "u", "a", "d" })).crossWithSet(
				Arrays.asList(new String[] { "a" })).crossWithSet(
				Arrays.asList(new String[] { "b" })).crossWithSet(
				Arrays.asList(new String[] { "c" }));
		Map<String, String> actual3_1 = en.getDebugDataMap(temp2);// before
																	// PTA is
																	// updated,
																	// the long
																	// path
																	// should be
																	// a leaf
																	// ...
		Map<String, String> actual4 = en.getDebugDataMap(temp2
				.crossWithSet(Arrays.asList(new String[] { "b", "a" })));
		Map<String, String> actual3_2 = en.getDebugDataMap(temp2);// after it
																	// is
																	// updated,
																	// that path
																	// should no
																	// longer be
																	// a leaf.
		Map<String, String> actual1 = en.getDebugDataMap(temp_0);
		Map<String, String> actual2 = en.getDebugDataMap(temp_1);
		vertifyPTA(en, new String[][] { new String[] { "b" },
				new String[] { "d" },
				new String[] { "a", "a", "a", "b", "c", "b" },
				new String[] { "a", "a", "a", "b", "c", "a" },
				new String[] { "a", "b", "u" }, new String[] { "a", "b", "a" },
				new String[] { "a", "b", "d" }, new String[] { "a", "a", "d" },
				new String[] { "a", "a", "u" }, new String[] { "a", "d" },
				new String[] { "a", "u" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] { new Object[] {
						new String[] { "a", "a", "a" },
						PTATestSequenceEngine.DebugDataValues.booleanToString(
								false, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual1,
				expected.equals(actual1));
		expected = TestFSMAlgo.buildStringMap(new Object[][] { new Object[] {
				new String[] { "a", "a" },
				PTATestSequenceEngine.DebugDataValues.booleanToString(false,
						true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual2,
				expected.equals(actual2));
		expected = TestFSMAlgo.buildStringMap(new Object[][] { new Object[] {
				new String[] { "a", "a", "a", "b", "c" },
				PTATestSequenceEngine.DebugDataValues.booleanToString(true,
						true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual3_1,
				expected.equals(actual3_1));
		expected = TestFSMAlgo.buildStringMap(new Object[][] { new Object[] {
				new String[] { "a", "a", "a", "b", "c" },
				PTATestSequenceEngine.DebugDataValues.booleanToString(false,
						true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual3_2,
				expected.equals(actual3_2));
		expected = TestFSMAlgo.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual4,
				expected.equals(actual4));
	}

	@Test
	public final void test_sequenceSet3_5() // a more complex composition
	{
		fsm = WMethod.getGraphData(TestFSMAlgo.buildGraph(
				"A-a->B-a->A-b-#C\nA-d->M-a->N\nB-b->D-c->E",
				"test_sequenceSet3_5"));
		en = new PTA_FSMStructure(fsm);
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		sequenceSet temp2 = seq.crossWithSet(
				Arrays.asList(new String[] { "b", "a", "d" })).crossWithSet(
				Arrays.asList(new String[] { "b", "a", "d" }));
		Map<String, String> actual2 = en.getDebugDataMap(temp2
				.crossWithSet(Arrays.asList(new String[] { "a" })));
		Map<String, String> actual1 = en.getDebugDataMap(temp2);
		vertifyPTA(en, new String[][] { new String[] { "b" },
				new String[] { "a", "a", "a" }, new String[] { "a", "b", "a" },
				new String[] { "a", "d" }, new String[] { "d", "a", "a" },
				new String[] { "d", "b" }, new String[] { "d", "d" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {
						new Object[] {
								new String[] { "a", "a" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(false, true) },
						new Object[] {
								new String[] { "d", "a" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(false, true) },
						new Object[] {
								new String[] { "a", "b" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(false, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual1,
				expected.equals(actual1));
		expected = TestFSMAlgo.buildStringMap(new Object[][] { new Object[] {
				new String[] { "a", "a", "a" },
				PTATestSequenceEngine.DebugDataValues.booleanToString(true,
						true) }, });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual2,
				expected.equals(actual2));
	}

	@Test
	public final void test_sequenceSet3_6() // a more complex composition
	{
		fsm = WMethod.getGraphData(TestFSMAlgo.buildGraph("A-a->A-b->B",
				"sample automaton"));
		en = new PTA_FSMStructure(fsm);
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(
				Arrays.asList(new String[] { "b", "a" })).crossWithSet(
				Arrays.asList(new String[] { "b", "a" })).crossWithSet(
				Arrays.asList(new String[] { "a", "b" })));
		vertifyPTA(en, new String[][] { new String[] { "b", "b" },
				new String[] { "b", "a" }, new String[] { "a", "a", "b" },
				new String[] { "a", "a", "a" }, new String[] { "a", "b", "a" },
				new String[] { "a", "b", "b" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {
						new Object[] {
								new String[] { "a", "a", "a" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(true, true) },
						new Object[] {
								new String[] { "a", "a", "b" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(true, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet_testing_shouldBeReturned1() // a test
																	// for
																	// shouldBeReturned
	{
		final FSMStructure machine = WMethod.getGraphData(TestFSMAlgo
				.buildGraph("A-a->A-b->B", "sample automaton"));
		en = new PTA_FSMStructure(machine) {
			{
				fsm = machine;
				init(new FSM() {
					@Override
					public boolean shouldBeReturned(Object elem) {
						// elem is null for REJECT states
						return !(elem == null) && elem.toString().equals("B");
					}
				});
			}
		};
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(
				Arrays.asList(new String[] { "b", "a" })).crossWithSet(
				Arrays.asList(new String[] { "b", "a" })));
		vertifyPTA(en, new String[][] { new String[] { "a", "b" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {
						new Object[] {
								new String[] { "a", "a" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(true, false) },
						new Object[] {
								new String[] { "a", "b" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(true, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet_testing_shouldBeReturned2() // a test
																	// for
																	// shouldBeReturned
	{
		final FSMStructure machine = WMethod.getGraphData(TestFSMAlgo
				.buildGraph("A-a->A-b->B", "sample automaton"));
		en = new PTA_FSMStructure(machine) {
			{
				fsm = machine;
				init(new FSM() {
					@Override
					public boolean shouldBeReturned(Object elem) {
						return false;
					}
				});
			}
		};
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(
				Arrays.asList(new String[] { "b", "a" })).crossWithSet(
				Arrays.asList(new String[] { "b", "a" })));
		vertifyPTA(en, new String[][] {});
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {
						new Object[] {
								new String[] { "a", "a" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(true, false) },
						new Object[] {
								new String[] { "a", "b" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(true, false) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet_testing_shouldBeReturned3() // a test
																	// for
																	// shouldBeReturned
	{
		final FSMStructure machine = WMethod.getGraphData(TestFSMAlgo
				.buildGraph("A-a->A-b->B", "sample automaton"));
		en = new PTA_FSMStructure(machine) {
			{
				fsm = machine;
				init(new FSM() {
					@Override
					public boolean shouldBeReturned(Object elem) {
						// elem is null for REJECT states
						return !(elem == null);// all paths which exist
					}
				});
			}
		};
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.crossWithSet(
				Arrays.asList(new String[] { "b", "a" })).crossWithSet(
				Arrays.asList(new String[] { "b", "a" })));
		vertifyPTA(en, new String[][] { new String[] { "a", "b" },
				new String[] { "a", "a" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {
						new Object[] {
								new String[] { "a", "a" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(true, true) },
						new Object[] {
								new String[] { "a", "b" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(true, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet4_1() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.cross(TestFSMAlgo
				.buildList(new String[][] { new String[] { "a", "b", "c" } })));
		vertifyPTA(en, new String[][] { new String[] { "a", "b", "c" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] { new Object[] {
						new String[] { "a", "b", "c" },
						PTATestSequenceEngine.DebugDataValues.booleanToString(
								true, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet4_2() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		Map<String, String> actual = en.getDebugDataMap(seq.cross(TestFSMAlgo
				.buildList(new String[][] { new String[] { "a", "b", "c" },
						new String[] { "a" },
						new String[] { "a", "b", "c", "d" },

				})));
		vertifyPTA(en, new String[][] { new String[] { "a", "b", "c", "d" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {
						new Object[] {
								new String[] { "a" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(false, true) },
						new Object[] {
								new String[] { "a", "b", "c" },
								PTATestSequenceEngine.DebugDataValues
										.booleanToString(false, true) } });
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet4_3() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		sequenceSet temp = seq.cross(TestFSMAlgo
				.buildList(new String[][] { new String[] { "a", "b", "c" } }));
		Map<String, String> actual = en.getDebugDataMap(temp.cross(TestFSMAlgo
				.buildList(new String[][] { new String[] { "a", "b", "c" },
						new String[] { "c" } })));
		vertifyPTA(en, new String[][] { new String[] { "a", "b", "c", "c" },
				new String[] { "a", "b", "c", "a" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet5_1() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		seq.cross(
				TestFSMAlgo.buildList(new String[][] { new String[] { "a", "b",
						"c" } })).crossWithSet(new LinkedList<String>());
		Map<String, String> actual = en.getDebugDataMap(en.new sequenceSet()
				.cross(TestFSMAlgo.buildList(new String[][] {// here the new
																// sequenceSet
																// is empty,
																// hence
																// whatever I
																// do, there
																// should be no
																// changes
						new String[] { "a", "b", "c", "d" },
								new String[] { "c" } })));
		vertifyPTA(en, new String[][] { new String[] { "a", "b", "c" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet5_2() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();
		seq.setIdentity();
		seq.crossWithSequence(Arrays.asList(new String[] { "a", "b", "c" }))
				.crossWithSet(new LinkedList<String>());
		Map<String, String> actual = en.getDebugDataMap(en.new sequenceSet()
				.cross(TestFSMAlgo.buildList(new String[][] {// here the new
																// sequenceSet
																// is empty,
																// hence
																// whatever I
																// do, there
																// should be no
																// changes
						new String[] { "a", "b", "c", "d" },
								new String[] { "c" } })));
		vertifyPTA(en, new String[][] { new String[] { "a", "b", "c" } });
		Map<String, String> expected = TestFSMAlgo
				.buildStringMap(new Object[][] {});
		Assert.assertTrue("expected: " + expected + ", actual: " + actual,
				expected.equals(actual));
	}
}
