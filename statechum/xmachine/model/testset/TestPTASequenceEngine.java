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

import statechum.Configuration;
import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.xmachine.model.testset.PTASequenceEngine.Node;
import statechum.xmachine.model.testset.PTASequenceEngine.SequenceSet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static statechum.analysis.learning.TestFSMAlgo.equalityTestingHelper;

public class TestPTASequenceEngine 
{
	private PTASequenceEngine en = null; 
	LearnerGraph fsm = null;
	
	/** Set up the graphs to use. Additionally,  
	 * make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	@Before
	public final void setUp()
	{
		config = (Configuration)mainConfiguration.clone();config.setAllowedToCloneNonCmpVertex(true);
		fsm = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->A-b-#C\nB-b->D-c->E", "TestPTATestSequenceEngine"),config);
		en = new PTA_FSMStructure(fsm);		
	}
	

	/** The working configuration to use when running tests. */
	private Configuration config = null;
	
	/** Each test starts with this configuration. */
	private Configuration mainConfiguration = Configuration.getDefaultConfiguration();
	
	/** Checks that the supplied engine has a specific number of sequences in it 
	 * which are all returned when getData() is performed on it.
	 * 
	 * @param en engine to check
	 * @param engineSize expected size
	 * @param expected expected sequences to be returned from the engine in response to getData()
	 */
	static void vertifyPTA(PTASequenceEngine en, int engineSize, String [][] expected)
	{
		Set<List<String>> actualA = new HashSet<List<String>>();actualA.addAll(en.getData());
		Set<List<String>> actualB = new HashSet<List<String>>();actualB.addAll(en.filter().getData());
		Set<List<String>> actualC = new HashSet<List<String>>();actualC.addAll(en.getDataORIG());
		Set<List<String>> actualD = new HashSet<List<String>>();actualD.addAll(en.filter().getDataORIG());
		Set<List<String>> expectedSet = TestFSMAlgo.buildSet(expected);
		assertTrue("expected: "+expectedSet+" received : "+actualA,expectedSet.equals(actualA));
		assertTrue("expected: "+expectedSet+" received : "+actualB,expectedSet.equals(actualB));
		assertTrue("expected: "+expectedSet+" received : "+actualC,expectedSet.equals(actualC));
		assertTrue("expected: "+expectedSet+" received : "+actualD,expectedSet.equals(actualD));
		assertEquals(engineSize, en.numberOfLeafNodes());
	}
	
	@Test
	public final void testGetData_Empty1()
	{
		DirectedSparseGraph g = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, false, UserData.SHARED);
		init.addUserDatum(JUConstants.LABEL, "A", UserData.SHARED);
		g.addVertex(init);
		PTASequenceEngine engine = new PTA_FSMStructure(new LearnerGraph(g,config));
		vertifyPTA(engine, 1, new String[][] { 
				new String[] {}
			});
	}

	@Test
	public final void testGetData_Empty2()
	{
		DirectedSparseGraph g = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum(JUConstants.INITIAL, true,UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
		init.addUserDatum(JUConstants.LABEL, "A", UserData.SHARED);
		g.addVertex(init);
		PTASequenceEngine engine = new PTA_FSMStructure(new LearnerGraph(g,config));
		vertifyPTA(engine, 1, new String[][] {
				new String[] {}
		});
	}

	@Test
	public final void testNodeEquality()
	{
		PTASequenceEngine engine = new PTASequenceEngine();
		Node a = engine.new Node("A"), b = engine.new Node("A"), c = engine.new Node("B");
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
	public final void testNode1()
	{
		PTASequenceEngine engine = new PTASequenceEngine();
		Node a = engine.new Node("A");
		
		assertTrue(a.isAccept());
		assertTrue(engine.new Node("test").isAccept());
		assertFalse(engine.rejectNode.isAccept());
	}

	@Test
	public final void testNode2()
	{
		PTASequenceEngine engine = new PTASequenceEngine();
		Node a = engine.new Node("A"), b = engine.new Node("A"), c = engine.new Node("B");
		assertEquals("A", a.getState());
		assertEquals("A", b.getState());
		assertEquals("B", c.getState());
		
		int aID = a.getID(), bID = b.getID(), cID = c.getID();
		assertTrue(aID > 0 && bID > 0 && cID > 0);
		assertTrue(aID != bID && aID != cID && aID != bID);
		
		assertTrue(a.hashCode() != b.hashCode() && a.hashCode() != c.hashCode() && b.hashCode() != c.hashCode());
		assertTrue(a.hashCode() != engine.rejectNode.hashCode());
		assertTrue(b.hashCode() != engine.rejectNode.hashCode());
		assertTrue(c.hashCode() != engine.rejectNode.hashCode());
	}

	/** Checks that the two ways of obtaining debug data return the same results. */
	public static final Map<String,String> getDebugDataMap(PTASequenceEngine engine, SequenceSet set)
	{
		Map<String,String> actualA = engine.getDebugDataMapDepth(set),
			actualB = engine.getDebugDataMapBreadth(set);
		Assert.assertEquals(actualA, actualB);
		return actualA;
	}
	
	@Test
	public final void test_sequenceSet1()
	{
		SequenceSet seq = en.new SequenceSet();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"a"}))); // appending anything to an empty sequence produces an empty sequence.
		vertifyPTA(en, 1, new String[][] {
				new String[] {} // there is only an empty path but since reject-nodes are returned, this path is returned.
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test
	public final void test_sequenceSet2() // an input which exists
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test
	public final void test_sequenceSet3() // the one which does not exist
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"c"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"c"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test
	public final void test_sequenceSet4() // the one which enters a reject state
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"b"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"b"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test
	public final void test_sequenceSet5() // a composition of sequenceSet with an input which exists
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","a"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test
	public final void test_sequenceSet6() // a composition of sequenceSet with the one which does not exist
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"c"}))
			.crossWithSet(Arrays.asList(new String[] {"a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"c"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test
	public final void test_sequenceSet7() // a composition of sequenceSet with the one which enters a reject state
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"b"}))
			.crossWithSet(Arrays.asList(new String[] {"a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"b"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test
	public final void test_sequenceSet_2_1() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"b"}))
			.crossWithSet(Arrays.asList(new String[] {"c"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","a","a","b","c"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a","b","c"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet2_2() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"b"}))
			.crossWithSet(Arrays.asList(new String[] {"c"}))
			.crossWithSet(Arrays.asList(new String[] {"a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","a","a","b","c","a"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet2_3() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"b"}))
			.crossWithSet(Arrays.asList(new String[] {"c"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"b"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","a","a","b","c","a"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet2_4() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"c"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"b"}))
			.crossWithSet(Arrays.asList(new String[] {"c"}))
			.crossWithSet(Arrays.asList(new String[] {"a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"c"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet2_5() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet tempE = 
			 seq.crossWithSet(Arrays.asList(new String[] {"a"}))
				.crossWithSet(Arrays.asList(new String[] {"a"}))
				.crossWithSet(Arrays.asList(new String[] {"a"}))
				.crossWithSet(Arrays.asList(new String[] {"b"}))
				.crossWithSet(Arrays.asList(new String[] {"c"}));
		Map<String,String> actual2 = getDebugDataMap(en,tempE.crossWithSet(Arrays.asList(new String[] {"a"})));
		Map<String,String> actual3 = getDebugDataMap(en,tempE.crossWithSet(Arrays.asList(new String[] {"b"})));
		Map<String,String> actual1 = getDebugDataMap(en,tempE);// if I do this before PTA is updated, the long path returned by getDebugDataMap will have its nodes marked as leaves, but after PTA is built, they are no long leaves.
		vertifyPTA(en, 2, new String[][] {
				new String[] {"a","a","a","b","c","a"},
				new String[] {"a","a","a","b","c","b"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a","b","c"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual1, expected.equals(actual1));

		expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual2, expected.equals(actual2));
		Assert.assertTrue("expected: "+expected+", actual: "+actual3, expected.equals(actual3));
	}

	@Test
	public final void test_sequenceSet2_6() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet temp0=seq.crossWithSet(Arrays.asList(new String[] {"b"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}));

		SequenceSet tempE = 
			seq.crossWithSet(Arrays.asList(new String[] {"a"}))
				.crossWithSet(Arrays.asList(new String[] {"a"}))
				.crossWithSet(Arrays.asList(new String[] {"a"}))
				.crossWithSet(Arrays.asList(new String[] {"b"}))
				.crossWithSet(Arrays.asList(new String[] {"c"}));
		Map<String,String> actual3 = getDebugDataMap(en,tempE.crossWithSet(Arrays.asList(new String[] {"a"})));
		Map<String,String> actual4 = getDebugDataMap(en,tempE.crossWithSet(Arrays.asList(new String[] {"b"})));
		Map<String,String> actual2 = getDebugDataMap(en,tempE);
		Map<String,String> actual1 = getDebugDataMap(en,temp0);
		vertifyPTA(en, 3, new String[][] {
				new String[] {"a","a","a","b","c","a"},
				new String[] {"a","a","a","b","c","b"},
				new String[] {"b"}
		});

		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a","b","c"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual2, expected.equals(actual2));
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual1, expected.equals(actual1));
		Assert.assertTrue("expected: "+expected+", actual: "+actual3, expected.equals(actual3));
		Assert.assertTrue("expected: "+expected+", actual: "+actual4, expected.equals(actual4));
	}

	@Test
	public final void test_sequenceSet3_1() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"b","a","d"})));
		vertifyPTA(en, 3, new String[][] {
				new String[] {"b"},
				new String[] {"a"},
				new String[] {"d"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet3_2() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"b","a","d"}))
			.crossWithSet(Arrays.asList(new String[] {"b","a","d"})));
		vertifyPTA(en, 5, new String[][] {
				new String[] {"b"},
				new String[] {"a","a"},
				new String[] {"a","d"},
				new String[] {"a","b"},
				new String[] {"d"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet3_3() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet temp = seq.crossWithSet(Arrays.asList(new String[] {"b","a","d"}));
		SequenceSet temp2 = temp.crossWithSet(Arrays.asList(new String[] {"b","a","d"}));
		Map<String,String> actual3 = getDebugDataMap(en,temp2.crossWithSet(Arrays.asList(new String[] {"u","a","d"})));
		Map<String,String> actual1 = getDebugDataMap(en,temp);
		Map<String,String> actual2 = getDebugDataMap(en,temp2);
		vertifyPTA(en, 9, new String[][] {
				new String[] {"b"},
				new String[] {"d"},
				new String[] {"a","a","a"},
				new String[] {"a","b","u"},
				new String[] {"a","b","a"},
				new String[] {"a","b","d"},
				new String[] {"a","a","d"},
				new String[] {"a","a","u"},
				new String[] {"a","d"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual1, expected.equals(actual1));
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)},
				new Object[]{new String[] {"a","b"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual2, expected.equals(actual2));
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual3, expected.equals(actual3));
	}

	@Test
	public final void test_sequenceSet3_4() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet temp = seq.crossWithSet(Arrays.asList(new String[] {"b","a","d"}));
		SequenceSet temp_0 = temp.crossWithSet(Arrays.asList(new String[] {"b","a","d"}))
			.crossWithSet(Arrays.asList(new String[] {"u","a","d"}));
		SequenceSet temp_1 = temp.crossWithSet(Arrays.asList(new String[] {"u","a","d"}));
		SequenceSet temp2 = temp.crossWithSet(Arrays.asList(new String[] {"u","a","d"}))
		.crossWithSet(Arrays.asList(new String[] {"a"}))
		.crossWithSet(Arrays.asList(new String[] {"b"}))
		.crossWithSet(Arrays.asList(new String[] {"c"}));
		Map<String,String> actual3_1 = getDebugDataMap(en,temp2);// before PTA is updated, the long path should be a leaf ...
		Map<String,String> actual4 = getDebugDataMap(en,temp2.crossWithSet(Arrays.asList(new String[] {"b","a"})));
		Map<String,String> actual3_2 = getDebugDataMap(en,temp2);// after it is updated, that path should no longer be a leaf.
		Map<String,String> actual1 = getDebugDataMap(en,temp_0);
		Map<String,String> actual2 = getDebugDataMap(en,temp_1);
		vertifyPTA(en, 11, new String[][] {
				new String[] {"b"},
				new String[] {"d"},
				new String[] {"a","a","a","b","c","b"},
				new String[] {"a","a","a","b","c","a"},
				new String[] {"a","b","u"},
				new String[] {"a","b","a"},
				new String[] {"a","b","d"},
				new String[] {"a","a","d"},
				new String[] {"a","a","u"},
				new String[] {"a","d"},
				new String[] {"a","u"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual1, expected.equals(actual1));
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual2, expected.equals(actual2));
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a","b","c"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual3_1, expected.equals(actual3_1));
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a","b","c"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual3_2, expected.equals(actual3_2));
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual4, expected.equals(actual4));
	}

	@Test
	public final void test_sequenceSet3_5() // a more complex composition
	{
		fsm = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->A-b-#C\nA-d->M-a->N\nB-b->D-c->E", "test_sequenceSet3_5"),config);
		en = new PTA_FSMStructure(fsm);		
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet temp2 = seq.crossWithSet(Arrays.asList(new String[] {"b","a","d"}))
			.crossWithSet(Arrays.asList(new String[] {"b","a","d"}));
		Map<String,String> actual2 = getDebugDataMap(en,temp2.crossWithSet(Arrays.asList(new String[] {"a"})));
		Map<String,String> actual1 = getDebugDataMap(en,temp2);
		vertifyPTA(en, 7, new String[][] {
				new String[] {"b"},
				new String[] {"a","a","a"},
				new String[] {"a","b","a"},
				new String[] {"a","d"},
				new String[] {"d","a","a"},
				new String[] {"d","b"},
				new String[] {"d","d"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)},
				new Object[]{new String[] {"d","a"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)},
				new Object[]{new String[] {"a","b"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual1, expected.equals(actual1));
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual2, expected.equals(actual2));
	}

	@Test
	public final void test_sequenceSet3_6() // a more complex composition
	{
		fsm = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B", "test_sequenceSet3_6"),config);
		en = new PTA_FSMStructure(fsm);		
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"b","a"}))
			.crossWithSet(Arrays.asList(new String[] {"b","a"}))
			.crossWithSet(Arrays.asList(new String[] {"a","b"})));
		vertifyPTA(en, 6, new String[][] {
				new String[] {"b","b"},
				new String[] {"b","a"},
				new String[] {"a","a","b"},
				new String[] {"a","a","a"},
				new String[] {"a","b","a"},
				new String[] {"a","b","b"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","a","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	/** Test that it is possible to selectively filter out paths which terminate at specific states. */ 
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned1() // a test for shouldBeReturned
	{
		final LearnerGraph machine = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B", "test_sequenceSet_testing_shouldBeReturned1"),config);
		en = new PTA_FSMStructure(machine) {
			{ 
				init(machine.new FSMImplementation(){
					@Override
					public boolean shouldBeReturned(Object elem) {
						// elem is null for REJECT states
						return !(elem == null) && elem.toString().equals("B");
					}			
				}); 
			}
		};
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"b","a"}))
			.crossWithSet(Arrays.asList(new String[] {"b","a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","b"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, false)},
				new Object[]{new String[] {"a","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** Test that the outcome of filter is not affected by subsequent changes in the original graph. */ 
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned_filter_is_a_copy()
	{
		final LearnerGraph machine = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B-b->B\nA-d->A\nB-c->D-a->D", "test_sequenceSet_testing_shouldBeReturned_filter_is_a_copy"),config);
		en = new PTA_FSMStructure(machine) {
			{ 
				init(machine.new FSMImplementation(){
					@Override
					public boolean shouldBeReturned(Object elem) {
						// elem is null for REJECT states
						return elem != null && (elem.toString().equals("B") || elem.toString().equals("D"));
					}			
				}); 
			}
		};
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet nodeD = seq.crossWithSet(Arrays.asList(new String[] {"b","a"}))
			.crossWithSet(Arrays.asList(new String[] {"b","a"}));
		PTASequenceEngine filtered = en.filter();
		vertifyPTA(filtered, 2, new String[][] {
				new String[] {"a","b"},
				new String[] {"b","b"}
		});
		// now modify the original PTA
		nodeD.crossWithSet(Arrays.asList(new String[]{"a","c"}));
		vertifyPTA(en, 2, new String[][] {
				new String[] {"a","b","c"},
				new String[] {"b","b","c"}
		});
		// and check that filtered version is not affected
		vertifyPTA(filtered, 2, new String[][] {
				new String[] {"a","b"},
				new String[] {"b","b"}
		});
		
		// now modify the filtered one
		SequenceSet filteredSeqSet = filtered.new SequenceSet();filteredSeqSet.setIdentity();
		filteredSeqSet.crossWithSequence(Arrays.asList(new String[]{"b","b","b","b"}));
		// and check that the filtered one has been modified but the original one is unchanged
		vertifyPTA(en, 2, new String[][] {
				new String[] {"a","b","c"},
				new String[] {"b","b","c"}
		});
		vertifyPTA(filtered, 2, new String[][] {
				new String[] {"a","b"},
				new String[] {"b","b","b","b"}
		});
	}

	/** Test that it is possible to selectively filter out paths which terminate at specific states. */ 
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned1b()
	{
		final LearnerGraph machine = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B-b->B\nA-d->A", "test_sequenceSet_testing_shouldBeReturned1b"),config);
		en = new PTA_FSMStructure(machine) {
			{ 
				init(machine.new FSMImplementation(){
					@Override
					public boolean shouldBeReturned(Object elem) {
						// elem is null for REJECT states
						return elem != null && elem.toString().equals("B");
					}			
				}); 
			}
		};
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a","b"},
				new String[] {"d","d"},
				new String[] {"c"}
				
		})).crossWithSet(Arrays.asList(new String[] {"b","a"})));
		vertifyPTA(en, 2, new String[][] {
				new String[] {"a","a","b","b"},
				new String[] {"d","d","b"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","b","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"d","d","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"d","d","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, false)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	/** Test that it is possible to selectively filter out paths which terminate at specific states. */ 
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned1c()
	{
		final LearnerGraph machine = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B-b->B\nA-d->C-d->C", "test_sequenceSet_testing_shouldBeReturned1c"),config);
		en = new PTA_FSMStructure(machine) {
			{ 
				init(machine.new FSMImplementation(){
					@Override
					public boolean shouldBeReturned(Object elem) {
						// elem is null for REJECT states
						return elem != null && elem.toString().equals("B");
					}			
				}); 
			}
		};
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a","b"},
				new String[] {"d","d"},
				new String[] {"c"}
				
		})).crossWithSet(Arrays.asList(new String[] {"b","a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","a","b","b"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","b","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	/** Test that it is possible to selectively filter out paths which terminate at specific states. 
	 * In this PTA, there are two different paths with accept on tail nodes.	
	 */ 
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned1d()
	{
		final LearnerGraph machine = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B-b->B\nA-d->C-d->C\nB-c->D-c->D-a->D-b->D", "test_sequenceSet_testing_shouldBeReturned1d"),config);
		en = new PTA_FSMStructure(machine) {
			{ 
				init(machine.new FSMImplementation(){
					@Override
					public boolean shouldBeReturned(Object elem) {
						// elem is null for REJECT states
						return elem != null && elem.toString().equals("B");
					}			
				}); 
			}
		};
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a","b"},
				new String[] {"a","a","c","c"},
				new String[] {"d","d"},
				new String[] {"c"}
				
		})).crossWithSet(Arrays.asList(new String[] {"b","a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","a","b","b"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","b","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	/** Test that if I wish to return all paths in a PTA, they will be returned. */ 
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned1e()
	{
		final LearnerGraph machine = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B-b->B\nA-d->C-d->C\nB-c->D-c->D-a->D-b->D", "test_sequenceSet_testing_shouldBeReturned1d"),config);
		en = new PTA_FSMStructure(machine) {
			{ 
				init(machine.new FSMImplementation(){
					@Override
					public boolean shouldBeReturned(@SuppressWarnings("unused")	Object elem) {
						// elem is null for REJECT states
						return true;
					}			
				}); 
			}
		};
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a","b","c"}
				
		})).crossWithSet(Arrays.asList(new String[] {"b","a"})));
		vertifyPTA(en, 2, new String[][] {
				new String[] {"a","a","b","c","a"},
				new String[] {"a","a","b","c","b"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","b","c","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","a","b","c","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}


	@Test
	public final void test_sequenceSet_testing_shouldBeReturned2() // a test for shouldBeReturned
	{
		final LearnerGraph machine = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B", "test_sequenceSet_testing_shouldBeReturned2"),config);
		en = new PTA_FSMStructure(machine) {
			{ 
				init(machine.new FSMImplementation(){
					@Override
					public boolean shouldBeReturned(@SuppressWarnings("unused")	Object elem) {
						return false;
					}			
				}); 
			}
		};
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"b","a"}))
			.crossWithSet(Arrays.asList(new String[] {"b","a"})));
		vertifyPTA(en, 0, new String[][] {
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, false)},
				new Object[]{new String[] {"a","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, false)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet_testing_shouldBeReturned3() // a test for shouldBeReturned
	{
		final LearnerGraph machine = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B", "test_sequenceSet_testing_shouldBeReturned3"),config);
		en = new PTA_FSMStructure(machine) {
			{ 
				init(machine.new FSMImplementation(){
					@Override
					public boolean shouldBeReturned(Object elem) {
						// elem is null for REJECT states
						return !(elem == null);// return all paths which exist
					}			
				}); 
			}
		};
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(Arrays.asList(new String[] {"b","a"}))
			.crossWithSet(Arrays.asList(new String[] {"b","a"})));
		vertifyPTA(en, 2, new String[][] {
				new String[] {"a","b"},
				new String[] {"a","a"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	/** Test that where we do not return negative states and there is 
	 * only one state in the graph which is negative, the outcome is zero.
	 */
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned4()
	{
		DirectedSparseGraph g = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, false, UserData.SHARED);
		init.addUserDatum(JUConstants.LABEL, "A", UserData.SHARED);
		g.addVertex(init);
		final LearnerGraph machine = new LearnerGraph(g,config);
		en = new PTA_FSMStructure(machine) {
			{ 
				init(machine.new FSMImplementation(){
					@Override
					public boolean shouldBeReturned(Object elem) {
						// elem is null for REJECT states
						return elem != null;// all paths which exist
					}			
				}); 
			}
		};
		vertifyPTA(en, 0, new String[][] { 
			});
	}
	
	@Test
	public final void test_sequenceSet4_1() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"}
		})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","b","c"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","b","c"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet4_2() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet result = seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"},
				new String[] {"a"},
				new String[] {"a","b","c","d"},
				
		}));
		Map<String,String> actual = getDebugDataMap(en,result);
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","b","c","d"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)},
				new Object[]{new String[] {"a","b","c"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
		Assert.assertFalse(result.isEmpty());
	}
	
	@Test
	public final void test_sequenceSet4_3() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet temp = seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"}
		}));
		SequenceSet result = temp.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"},
				new String[] {"c"}
		}));
		Map<String,String> actual = getDebugDataMap(en,result);
		vertifyPTA(en, 2, new String[][] {
				new String[] {"a","b","c","c"},
				new String[] {"a","b","c","a"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
		Assert.assertTrue(result.isEmpty());
	}
	
	@Test
	public final void test_sequenceSet5_1() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"}
		})).crossWithSet(new LinkedList<String>());
		Map<String,String> actual = getDebugDataMap(en,en.new SequenceSet().cross(TestFSMAlgo.buildList(new String[][] {// here the new sequenceSet is empty, hence whatever I do, there should be no changes
				new String[] {"a","b","c","d"},
				new String[] {"c"}
		})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","b","c"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	@Test
	public final void test_sequenceSet5_2() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		seq.crossWithSequence(Arrays.asList(new String[] {"a","b","c"})).crossWithSet(new LinkedList<String>());
		Map<String,String> actual = getDebugDataMap(en,en.new SequenceSet().cross(TestFSMAlgo.buildList(new String[][] {// here the new sequenceSet is empty, hence whatever I do, there should be no changes
				new String[] {"a","b","c","d"},
				new String[] {"c"}
		})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","b","c"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** Test for equality of different sequenceSets. */
	@Test
	public final void test_sequenceSet_equality_differentcontainer()
	{
		SequenceSet seqStart1 = en.new SequenceSet();
		SequenceSet seqStart2 = en.new SequenceSet();
		SequenceSet seqDifferent1 = en.new SequenceSet();seqDifferent1.setIdentity();
		PTA_FSMStructure engine2 = new PTA_FSMStructure(fsm);		
		equalityTestingHelper(seqStart1,seqStart2,seqDifferent1,engine2.new SequenceSet());
	}
	
	/** Test for equality of different sequenceSets. */
	@Test
	public final void test_sequenceSet_equality0a()
	{
		SequenceSet seqStart1 = en.new SequenceSet();
		SequenceSet seqStart2 = seqStart1.crossWithSequence(Arrays.asList(
				new String[] {"a"}
		));
		SequenceSet seqDifferent1 = en.new SequenceSet();seqDifferent1.setIdentity();
		SequenceSet seqDifferent2 = en.new SequenceSet();seqDifferent2.setIdentity();seqDifferent2.crossWithSequence(Arrays.asList(
				new String[] {"a"}
		));
		equalityTestingHelper(seqStart1,seqStart2,seqDifferent1,seqDifferent2);
	}
	
	/** Test for equality of different sequenceSets. */
	@Test
	public final void test_sequenceSet_equality0b()
	{
		SequenceSet seqStart1 = en.new SequenceSet();
		SequenceSet seqStart2 = en.new SequenceSet();
		SequenceSet seqDifferent1 = en.new SequenceSet();seqDifferent1.setIdentity();
		SequenceSet seqDifferent2 = en.new SequenceSet();seqDifferent2.setIdentity();seqDifferent2.crossWithSequence(Arrays.asList(
				new String[] {"a"}
		));
		equalityTestingHelper(seqStart1,seqStart2,seqDifferent1,seqDifferent2);
	}
	
	/** Test for equality of different sequenceSets. */
	@Test
	public final void test_sequenceSet_equality0c()
	{
		SequenceSet seqStart1 = en.new SequenceSet();seqStart1.crossWithSequence(Arrays.asList(
				new String[] {"t"}
		));
		SequenceSet seqStart2 = en.new SequenceSet();seqStart2.crossWithSequence(Arrays.asList(
				new String[] {"t"}
		));
		SequenceSet seqDifferent1 = en.new SequenceSet();seqDifferent1.setIdentity();
		SequenceSet seqDifferent2 = en.new SequenceSet();seqDifferent2.setIdentity();seqDifferent2.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a"}
				
		}));
		equalityTestingHelper(seqStart1,seqStart2,seqDifferent1,seqDifferent2);
	}
	
	/** Test for equality of different sequenceSets. */
	@Test
	public final void test_sequenceSet_equality1()
	{
		SequenceSet seqStart1 = en.new SequenceSet();seqStart1.setIdentity();
		SequenceSet seqStart2 = en.new SequenceSet();seqStart2.setIdentity();
		SequenceSet seqDifferent1 = en.new SequenceSet();
		SequenceSet seqDifferent2 = seqStart1.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a"}
		}));
		equalityTestingHelper(seqStart1,seqStart2,seqDifferent1,seqDifferent2);
	}
	
	/** Test for equality of different sequenceSets. */
	@Test
	public final void test_sequenceSet_equality2()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqStartOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		}));
		
		SequenceSet seqStartTwo = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c","a","a"}
		}));
		
		SequenceSet seqDifferent1 = en.new SequenceSet();
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a"}
		}));
		equalityTestingHelper(seqStartOne,seqStartTwo,seqDifferent1,seqDifferent2);
	}
	
	/** Test for equality of different sequenceSets. */
	@Test
	public final void test_sequenceSet_equality3()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqStartOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		}));
		
		SequenceSet seqStartTwo = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a"},
				new String[] {"c","a","a"}
				
		})).cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a"},
				new String[] {"c","a","a"}
		}));
		
		SequenceSet seqDifferent1 = en.new SequenceSet();
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a"}
		}));
		equalityTestingHelper(seqStartOne,seqStartTwo,seqDifferent1,seqDifferent2);
	}

	/** Tests containsSequence. */
	@Test
	public final void test_containsSequence()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		}));
		assertTrue(en.containsSequence(Arrays.asList(new String[]{"a","a"})));
		assertTrue(en.containsSequence(Arrays.asList(new String[]{"c"})));
		assertFalse(en.containsSequence(Arrays.asList(new String[]{"a","a","b"})));
		assertTrue(en.containsSequence(Arrays.asList(new String[]{"a"})));
		assertTrue(en.containsSequence(Arrays.asList(new String[]{})));
		assertFalse(en.containsSequence(Arrays.asList(new String[]{"c","c"})));
		assertFalse(en.containsSequence(Arrays.asList(new String[]{"b"})));
	}
	
	/** Test for Union: adding something to an empty set. */
	@Test
	public final void test_sequenceSet_union0()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		}));
		SequenceSet seqTwo = en.new SequenceSet();seqTwo.unite(seqOne);
		SequenceSet seqDifferent1 = seqTwo.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
		));
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
		));

		equalityTestingHelper(seqOne,seqTwo,seqDifferent1,seqDifferent2);
	}
	
	/** Test for Union: adding an empty set to something. */
	@Test
	public final void test_sequenceSet_union1()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		}));
		SequenceSet seqA = en.new SequenceSet();seqOne.unite(seqA);
		SequenceSet seqTwo = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		}));
		SequenceSet seqDifferent1 = seqTwo.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
		));
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
		));

		equalityTestingHelper(seqOne,seqTwo,seqDifferent1,seqDifferent2);
	}
	
	/** Test for Union: adding a set to itself. */
	@Test
	public final void test_sequenceSet_union2()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqA = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		}));
		SequenceSet seqTwo = en.new SequenceSet();seqTwo.unite(seqA);seqTwo.unite(seqTwo);
		List<List<String>> arg = new LinkedList<List<String>>();
		Set<List<String>> setOfStrings = TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}});
		arg.addAll(setOfStrings);arg.addAll(setOfStrings);
		SequenceSet seqOne = seqStart.cross(arg);

		SequenceSet seqDifferent1 = seqTwo.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
		));
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
		));

		equalityTestingHelper(seqOne,seqTwo,seqDifferent1,seqDifferent2);
	}
	
	/** Test for Union: adding a set from a different engine. */
	@Test(expected=IllegalArgumentException.class)
	public final void test_sequenceSet_union3()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		}));
		SequenceSet seqTwo = new PTA_FSMStructure(fsm).new SequenceSet();
		seqTwo.unite(seqOne);
	}
	
	/** Test for Union. */
	@Test
	public final void test_sequenceSet_union4()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqSecondStart = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		}));
		
		SequenceSet seqDifferent1 = seqSecondStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
		));
		
		SequenceSet seqOne = seqSecondStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {}
		}));
		SequenceSet seqOneB = seqSecondStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"}
		}));
		
		seqOne.unite(seqOneB);
		
		SequenceSet seqTwo = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a",	"a","b","c"},
				new String[] {"a","a"},
		}));
		
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"},
		}));
	
		equalityTestingHelper(seqOne,seqTwo,seqDifferent1,seqDifferent2);
	}

	@Test
	public final void test_stringCollectionSize0()
	{
		fsm = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B", "test_sequenceSet3_6"),config);
		en = new PTA_FSMStructure(fsm);		
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		vertifyPTA(en, 1, new String[][] {
				new String[] {}
		});
		int expectedCompressed = 0, expectedUncompressed = 0;
		assertEquals(expectedCompressed, PTASequenceEngine.stringCollectionSize(en).secondElem.intValue());
		assertEquals(expectedUncompressed, PTASequenceEngine.stringCollectionSize(en).firstElem.intValue());
		assertEquals(expectedUncompressed, PTASequenceEngine.ORIGstringCollectionSize(en));
	}
	
	@Test
	public final void test_stringCollectionSize1()
	{
		fsm = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B", "test_sequenceSet3_6"),config);
		en = new PTA_FSMStructure(fsm);		
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		seq.crossWithSequence(Arrays.asList(new String[] {"b","a"}));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"b","a"}
		});
		int expectedCompressed = 2, expectedUncompressed = 2;
		assertEquals(expectedCompressed, PTASequenceEngine.stringCollectionSize(en).secondElem.intValue());
		assertEquals(expectedUncompressed, PTASequenceEngine.stringCollectionSize(en).firstElem.intValue());
		assertEquals(expectedUncompressed, PTASequenceEngine.ORIGstringCollectionSize(en));
	}
	
	@Test
	public final void test_stringCollectionSize2()
	{
		fsm = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B", "test_sequenceSet3_6"),config);
		en = new PTA_FSMStructure(fsm);		
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"b","a"}))
			.crossWithSet(Arrays.asList(new String[] {"b","a"}))
			.crossWithSet(Arrays.asList(new String[] {"a","b"}));
		vertifyPTA(en, 6, new String[][] {
				new String[] {"b","b"},
				new String[] {"b","a"},
				new String[] {"a","a","b"},
				new String[] {"a","a","a"},
				new String[] {"a","b","a"},
				new String[] {"a","b","b"}
		});
		int expectedCompressed = 10, expectedUncompressed = 4+3*4;
		assertEquals(expectedCompressed, PTASequenceEngine.stringCollectionSize(en).secondElem.intValue());
		assertEquals(expectedUncompressed, PTASequenceEngine.stringCollectionSize(en).firstElem.intValue());
		assertEquals(expectedUncompressed, PTASequenceEngine.ORIGstringCollectionSize(en));
	}
}
