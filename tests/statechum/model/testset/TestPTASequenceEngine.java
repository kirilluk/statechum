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

package statechum.model.testset;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import junit_runners.ParameterizedWithName;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

import statechum.*;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;

import static org.junit.Assert.*;
import static statechum.analysis.learning.rpnicore.FsmParserStatechum.buildLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.TestWithMultipleConfigurations;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.model.testset.PTASequenceEngine.Node;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

import static statechum.analysis.learning.rpnicore.TestEqualityComparisonAndHashCode.equalityTestingHelper;

@RunWith(ParameterizedWithName.class)
public class TestPTASequenceEngine extends TestWithMultipleConfigurations
{
	private PTASequenceEngine en = null, engine_testLimitToGraph= null; 
	LearnerGraph fsm = null;
	
	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		return TestWithMultipleConfigurations.data();
	}
	
	@junit_runners.ParameterizedWithName.ParametersToString
	public static String parametersToString(Configuration config)
	{
		return TestWithMultipleConfigurations.parametersToString(config);
	}
	
	public TestPTASequenceEngine(Configuration argConfig)
	{
		super(argConfig);
	}
	
	/** Set up the graphs to use. Additionally,  
	 * make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	@Before
	public final void setUp()
	{
		mainConfiguration.setAllowedToCloneNonCmpVertex(true);
		fsm = buildLearnerGraph("A-a->B-a->A-b-#C\nB-b->D-c->E", "TestPTATestSequenceEngine",mainConfiguration,converter);
		en = new PTA_FSMStructure(fsm,null);		
		engine_testLimitToGraph = new PTA_FSMStructure(buildLearnerGraph(
				"A-a->B-a->F-b-#C\nB-c->D\nA-c->A\nB-b->D-c->E", "TestPTATestSequenceEngine",mainConfiguration,converter),null);
		//Visualiser.updateFrame(buildLearnerGraph(
		//		"A-a->B-a->F-b-#C\nB-c->D\nA-c->A\nB-b->D-c->E", "TestPTATestSequenceEngine",config), null);
		//Visualiser.waitForKey();
	}
	
	/** Checks that the supplied engine has a specific number of sequences in it 
	 * which are all returned when getData() is performed on it. Text in the <em>expected</em>
	 * set is turned into actual labels using <em>config</em> configuration. 
	 * 
	 * @param ptaToVerify engine to check
	 * @param engineSize expected size
	 * @param expected expected sequences to be returned from the engine in response to getData()
	 * @param config configuration to use
	 * @param converter label intern engine to use.
	 */
	static void vertifyPTA(PTASequenceEngine ptaToVerify, int engineSize, String [][] expected, Configuration config, ConvertALabel converter)
	{
		Set<List<Label>> actualA = new HashSet<>(ptaToVerify.getData());
		Set<List<Label>> actualB = new HashSet<>(ptaToVerify.filter(ptaToVerify.getFSM_filterPredicate()).getData());
		Set<List<Label>> actualC = new HashSet<>(ptaToVerify.getDataORIG());
		Set<List<Label>> actualD = new HashSet<>(ptaToVerify.filter(ptaToVerify.getFSM_filterPredicate()).getDataORIG());
		Set<List<Label>> expectedSet = TestFSMAlgo.buildSet(expected,config,converter);
		assertEquals("expected: " + expectedSet + " received : " + actualA, expectedSet, actualA);
		assertEquals("expected: " + expectedSet + " received : " + actualB, expectedSet, actualB);
		assertEquals("expected: " + expectedSet + " received : " + actualC, expectedSet, actualC);
		assertEquals("expected: " + expectedSet + " received : " + actualD, expectedSet, actualD);
		assertEquals(engineSize, ptaToVerify.numberOfLeafNodes());
	}
	
	/** Checks that the supplied engine has a specific number of sequences in it 
	 * which are all returned when getData() is performed on it. Text in the <em>expected</em>
	 * set is turned into actual labels using <em>config</em> configuration. 
	 * 
	 * @param ptaToVerify engine to check
	 * @param engineSize expected size
	 * @param expected expected sequences to be returned from the engine in response to getData()
	 */
	void vertifyPTA(PTASequenceEngine ptaToVerify, int engineSize, String [][] expected)
	{
		vertifyPTA(ptaToVerify,engineSize,expected,mainConfiguration,converter);
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
		PTASequenceEngine enVerySmall = new PTA_FSMStructure(new LearnerGraph(g,mainConfiguration),null);
		vertifyPTA(enVerySmall, 1, new String[][] { 
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
		PTASequenceEngine enVerySmall = new PTA_FSMStructure(new LearnerGraph(g,mainConfiguration),null);
		vertifyPTA(enVerySmall, 1, new String[][] {
				new String[] {}
		});
	}

	@Test
	public final void testNodeEquality()
	{
		PTASequenceEngine engine = new PTASequenceEngine();
		Node a = engine.new Node("A"), b = engine.new Node("A"), c = engine.new Node("B");
		assertEquals(a, a);
		assertEquals(b, b);
		assertEquals(engine.rejectNode, engine.rejectNode);
		assertNotEquals(a, b);
		assertNotEquals(b, a);
		assertNotEquals(a, c);
		assertNotEquals(a, engine.rejectNode);


		assertNotEquals("test", a);
		assertNotEquals(null, a);
	}

	@SuppressWarnings("unused")
	@Test(expected = IllegalArgumentException.class)
	public final void testNode0_fail()
	{
		PTASequenceEngine engine = new PTASequenceEngine();
		engine.new Node(null);
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
		
		int aID = a.toInt(), bID = b.toInt(), cID = c.toInt();
		assertTrue(aID > 0 && bID > 0 && cID > 0);
		assertTrue(aID != bID && aID != cID && aID != bID);
		
		assertTrue(a.hashCode() != b.hashCode() && a.hashCode() != c.hashCode() && b.hashCode() != c.hashCode());
		assertTrue(a.hashCode() != engine.rejectNode.hashCode());
		assertTrue(b.hashCode() != engine.rejectNode.hashCode());
		assertTrue(c.hashCode() != engine.rejectNode.hashCode());
	}

	/** Similar to testNode2 but uses a helper method. */
	@Test
	public final void testNode3()
	{
		PTASequenceEngine engine = new PTASequenceEngine();
		Node a = engine.new Node("A"), c = engine.new Node("B"),d = engine.new Node("C");
		equalityTestingHelper(a,a,c,d, true);
	}
	
	/** Similar to testNode2 but uses a helper method. */
	@Test
	public final void testNode_toString()
	{
		PTASequenceEngine engine = new PTASequenceEngine();
		Node a = engine.new Node("A");
		Assert.assertEquals("1(A)", a.toString());
		Assert.assertEquals("REJECT",engine.rejectNode.toString());
	}
	
	/** Checks that the two ways of obtaining debug data return the same results. */
	public static Map<String,String> getDebugDataMap(PTASequenceEngine engine, SequenceSet set)
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
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"a"}))); // appending anything to an empty sequence produces an empty sequence.
		vertifyPTA(en, 1, new String[][] {
				new String[] {} // there is only an empty path but since reject-nodes are returned, this path is returned.
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}
	
	@Test
	public final void test_sequenceSet2() // an input which exists
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}
	
	@Test
	public final void test_sequenceSet3() // the one which does not exist
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"c"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"c"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}
	
	@Test
	public final void test_sequenceSet4() // the one which enters a reject state
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"b"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"b"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}
	
	@Test
	public final void test_sequenceSet5() // a composition of sequenceSet with an input which exists
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"a"}))
			.crossWithSet(labelList(new String[] {"a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","a"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}
	
	@Test
	public final void test_sequenceSet6() // a composition of sequenceSet with the one which does not exist
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"c"}))
			.crossWithSet(labelList(new String[] {"a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"c"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}
	
	@Test
	public final void test_sequenceSet7() // a composition of sequenceSet with the one which enters a reject state
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"b"}))
			.crossWithSet(labelList(new String[] {"a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"b"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}
	
	@Test
	public final void test_sequenceSet_2_1() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"a"}))
			.crossWithSet(labelList(new String[] {"a"}))
			.crossWithSet(labelList(new String[] {"a"}))
			.crossWithSet(labelList(new String[] {"b"}))
			.crossWithSet(labelList(new String[] {"c"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","a","a","b","c"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a","b","c"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}

	@Test
	public final void test_sequenceSet2_2() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"a"}))
			.crossWithSet(labelList(new String[] {"a"}))
			.crossWithSet(labelList(new String[] {"a"}))
			.crossWithSet(labelList(new String[] {"b"}))
			.crossWithSet(labelList(new String[] {"c"}))
			.crossWithSet(labelList(new String[] {"a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","a","a","b","c","a"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}

	@Test
	public final void test_sequenceSet2_3() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"a"}))
			.crossWithSet(labelList(new String[] {"a"}))
			.crossWithSet(labelList(new String[] {"a"}))
			.crossWithSet(labelList(new String[] {"b"}))
			.crossWithSet(labelList(new String[] {"c"}))
			.crossWithSet(labelList(new String[] {"a"}))
			.crossWithSet(labelList(new String[] {"b"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","a","a","b","c","a"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}

	@Test
	public final void test_sequenceSet2_4() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"c"}))
			.crossWithSet(labelList(new String[] {"a"}))
			.crossWithSet(labelList(new String[] {"a"}))
			.crossWithSet(labelList(new String[] {"b"}))
			.crossWithSet(labelList(new String[] {"c"}))
			.crossWithSet(labelList(new String[] {"a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"c"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}

	@Test
	public final void test_sequenceSet2_5() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet tempE = 
			 seq.crossWithSet(labelList(new String[] {"a"}))
				.crossWithSet(labelList(new String[] {"a"}))
				.crossWithSet(labelList(new String[] {"a"}))
				.crossWithSet(labelList(new String[] {"b"}))
				.crossWithSet(labelList(new String[] {"c"}));
		Map<String,String> actual2 = getDebugDataMap(en,tempE.crossWithSet(labelList(new String[] {"a"})));
		Map<String,String> actual3 = getDebugDataMap(en,tempE.crossWithSet(labelList(new String[] {"b"})));
		Map<String,String> actual1 = getDebugDataMap(en,tempE);// if I do this before PTA is updated, the long path returned by getDebugDataMap will have its nodes marked as leaves, but after PTA is built, they are no long leaves.
		vertifyPTA(en, 2, new String[][] {
				new String[] {"a","a","a","b","c","a"},
				new String[] {"a","a","a","b","c","b"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a","b","c"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual1, expected, actual1);

		expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual2, expected, actual2);
		assertEquals("expected: " + expected + ", actual: " + actual3, expected, actual3);
	}

	@Test
	public final void test_sequenceSet2_6() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet temp0=seq.crossWithSet(labelList(new String[] {"b"}))
			.crossWithSet(labelList(new String[] {"a"}));

		SequenceSet tempE = 
			seq.crossWithSet(labelList(new String[] {"a"}))
				.crossWithSet(labelList(new String[] {"a"}))
				.crossWithSet(labelList(new String[] {"a"}))
				.crossWithSet(labelList(new String[] {"b"}))
				.crossWithSet(labelList(new String[] {"c"}));
		Map<String,String> actual3 = getDebugDataMap(en,tempE.crossWithSet(labelList(new String[] {"a"})));
		Map<String,String> actual4 = getDebugDataMap(en,tempE.crossWithSet(labelList(new String[] {"b"})));
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
		assertEquals("expected: " + expected + ", actual: " + actual2, expected, actual2);
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual1, expected, actual1);
		assertEquals("expected: " + expected + ", actual: " + actual3, expected, actual3);
		assertEquals("expected: " + expected + ", actual: " + actual4, expected, actual4);
	}

	@Test
	public final void test_sequenceSet3_1() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"b","a","d"})));
		vertifyPTA(en, 3, new String[][] {
				new String[] {"b"},
				new String[] {"a"},
				new String[] {"d"}
		});
		Map<String,String>  expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}

	@Test
	public final void test_sequenceSet3_2() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"b","a","d"}))
			.crossWithSet(labelList(new String[] {"b","a","d"})));
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
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}

	@Test
	public final void test_sequenceSet3_3() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet temp = seq.crossWithSet(labelList(new String[] {"b","a","d"}));
		SequenceSet temp2 = temp.crossWithSet(labelList(new String[] {"b","a","d"}));
		Map<String,String> actual3 = getDebugDataMap(en,temp2.crossWithSet(labelList(new String[] {"u","a","d"})));
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
		assertEquals("expected: " + expected + ", actual: " + actual1, expected, actual1);
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)},
				new Object[]{new String[] {"a","b"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual2, expected, actual2);
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
		});
		assertEquals("expected: " + expected + ", actual: " + actual3, expected, actual3);
	}

	@Test
	public final void test_sequenceSet3_4() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet temp = seq.crossWithSet(labelList(new String[] {"b","a","d"}));
		SequenceSet temp_0 = temp.crossWithSet(labelList(new String[] {"b","a","d"}))
			.crossWithSet(labelList(new String[] {"u","a","d"}));
		SequenceSet temp_1 = temp.crossWithSet(labelList(new String[] {"u","a","d"}));
		SequenceSet temp2 = temp.crossWithSet(labelList(new String[] {"u","a","d"}))
		.crossWithSet(labelList(new String[] {"a"}))
		.crossWithSet(labelList(new String[] {"b"}))
		.crossWithSet(labelList(new String[] {"c"}));
		Map<String,String> actual3_1 = getDebugDataMap(en,temp2);// before PTA is updated, the long path should be a leaf ...
		Map<String,String> actual4 = getDebugDataMap(en,temp2.crossWithSet(labelList(new String[] {"b","a"})));
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
		assertEquals("expected: " + expected + ", actual: " + actual1, expected, actual1);
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual2, expected, actual2);
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a","b","c"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual3_1, expected, actual3_1);
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a","b","c"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual3_2, expected, actual3_2);
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual4, expected, actual4);
	}

	@Test
	public final void test_sequenceSet3_5() // a more complex composition
	{
		fsm = buildLearnerGraph("A-a->B-a->A-b-#C\nA-d->M-a->N\nB-b->D-c->E", "test_sequenceSet3_5",mainConfiguration,converter);
		en = new PTA_FSMStructure(fsm,null);		
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet temp2 = seq.crossWithSet(labelList(new String[] {"b","a","d"}))
			.crossWithSet(labelList(new String[] {"b","a","d"}));
		Map<String,String> actual2 = getDebugDataMap(en,temp2.crossWithSet(labelList(new String[] {"a"})));
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
		assertEquals("expected: " + expected + ", actual: " + actual1, expected, actual1);
		expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
		});
		assertEquals("expected: " + expected + ", actual: " + actual2, expected, actual2);
	}

	@Test
	public final void test_sequenceSet3_6() // a more complex composition
	{
		fsm = buildLearnerGraph("A-a->A-b->B", "test_sequenceSet3_6",mainConfiguration,converter);
		en = new PTA_FSMStructure(fsm,null);		
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"b","a"}))
			.crossWithSet(labelList(new String[] {"b","a"}))
			.crossWithSet(labelList(new String[] {"a","b"})));
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
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}

	/** Test that it is possible to selectively filter out paths which terminate at specific states. */ 
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned1() // a test for shouldBeReturned
	{
		final LearnerGraph machine = buildLearnerGraph("A-a->A-b->B", "test_sequenceSet_testing_shouldBeReturned1",mainConfiguration,converter);
		en = new PTA_FSMStructure(machine,null) {
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
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"b","a"}))
			.crossWithSet(labelList(new String[] {"b","a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","b"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, false)},
				new Object[]{new String[] {"a","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}
	
	/** Test that the outcome of filter is not affected by subsequent changes in the original graph. */ 
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned_filter_is_a_copy()
	{
		final LearnerGraph machine = buildLearnerGraph("A-a->A-b->B-b->B\nA-d->A\nB-c->D-a->D", "test_sequenceSet_testing_shouldBeReturned_filter_is_a_copy",mainConfiguration,converter);
		en = new PTA_FSMStructure(machine,null) {
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
		SequenceSet nodeD = seq.crossWithSet(labelList(new String[] {"b","a"}))
			.crossWithSet(labelList(new String[] {"b","a"}));
		PTASequenceEngine filtered = en.filter(en.getFSM_filterPredicate());
		vertifyPTA(filtered, 2, new String[][] {
				new String[] {"a","b"},
				new String[] {"b","b"}
		});
		// now modify the original PTA
		nodeD.crossWithSet(labelList(new String[]{"a","c"}));
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
		filteredSeqSet.crossWithSequence(labelList(new String[]{"b","b","b","b"}));
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
		final LearnerGraph machine = buildLearnerGraph("A-a->A-b->B-b->B\nA-d->A", "test_sequenceSet_testing_shouldBeReturned1b",mainConfiguration,converter);
		en = new PTA_FSMStructure(machine,null) {
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
				
		},mainConfiguration,converter)).crossWithSet(labelList(new String[] {"b","a"})));
		vertifyPTA(en, 2, new String[][] {
				new String[] {"a","a","b","b"},
				new String[] {"d","d","b"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","b","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"d","d","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"d","d","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, false)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}

	/** Test that it is possible to selectively filter out paths which terminate at specific states. */ 
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned1c()
	{
		final LearnerGraph machine = buildLearnerGraph("A-a->A-b->B-b->B\nA-d->C-d->C", "test_sequenceSet_testing_shouldBeReturned1c",mainConfiguration,converter);
		en = new PTA_FSMStructure(machine,null) {
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
				
		},mainConfiguration,converter)).crossWithSet(labelList(new String[] {"b","a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","a","b","b"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","b","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}

	/** Test that it is possible to selectively filter out paths which terminate at specific states. 
	 * In this PTA, there are two different paths with accept on tail nodes.	
	 */ 
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned1d()
	{
		final LearnerGraph machine = buildLearnerGraph("A-a->A-b->B-b->B\nA-d->C-d->C\nB-c->D-c->D-a->D-b->D", "test_sequenceSet_testing_shouldBeReturned1d",mainConfiguration,converter);
		en = new PTA_FSMStructure(machine,null) {
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
				
		},mainConfiguration,converter)).crossWithSet(labelList(new String[] {"b","a"})));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","a","b","b"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","b","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}

	/** Test that if I wish to return all paths in a PTA, they will be returned. */ 
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned1e()
	{
		final LearnerGraph machine = buildLearnerGraph("A-a->A-b->B-b->B\nA-d->C-d->C\nB-c->D-c->D-a->D-b->D", "test_sequenceSet_testing_shouldBeReturned1d",mainConfiguration,converter);
		en = new PTA_FSMStructure(machine,null) {
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
				
		},mainConfiguration,converter)).crossWithSet(labelList(new String[] {"b","a"})));
		vertifyPTA(en, 2, new String[][] {
				new String[] {"a","a","b","c","a"},
				new String[] {"a","a","b","c","b"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a","b","c","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","a","b","c","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}


	/** Test for shouldBeReturned from the initial state. */
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned2a() // a test for shouldBeReturned
	{
		final LearnerGraph machine = buildLearnerGraph("A-a->A-b->B", "test_sequenceSet_testing_shouldBeReturned2a",mainConfiguration,converter);
		en = new PTA_FSMStructure(machine,null) {
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
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"b","a"}))
			.crossWithSet(labelList(new String[] {"b","a"})));
		vertifyPTA(en, 0, new String[][] {
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, false)},
				new Object[]{new String[] {"a","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, false)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}
	
	/** Test for shouldBeReturned from a non-initial state. */
	@Test
	public final void test_sequenceSet_testing_shouldBeReturned2b() // a test for shouldBeReturned
	{
		final LearnerGraph machine = buildLearnerGraph("P-a->P\nA-a->A-b->B", "test_sequenceSet_testing_shouldBeReturned2b",mainConfiguration,converter);
		en = new PTA_FSMStructure(machine,machine.findVertex(VertexID.parseID("A")));
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"b","a"}))
			.crossWithSet(labelList(new String[] {"b","a"})));
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}
	
	/** Checks that the non-existing initial state or an invalid one is rejected. */
	@SuppressWarnings("unused")
	@Test
	public final void testCreateFSMStructureFail1()
	{
		final LearnerGraph mach = buildLearnerGraph("D-a->A-a->A-b-#B","testPrecisionRecall2b",mainConfiguration,converter);
		new PTA_FSMStructure(mach,mach.findVertex(VertexID.parseID("A")));
		
		TestHelper.checkForCorrectException(
				() -> new PTA_FSMStructure(mach,AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("Vert"), mach.config)),
				IllegalArgumentException.class,"is not a valid state of the graph");
	}
	
	/** Checks that the non-existing initial state or an invalid one is rejected. */
	@SuppressWarnings("unused")
	@Test
	public final void testCreateFSMStructureFail2()
	{
		final LearnerGraph mach = buildLearnerGraph("D-a->A-a->A-b-#B","testPrecisionRecall2b",mainConfiguration,converter);
		final LearnerGraph sameMach = buildLearnerGraph("D-a->A-a->A-b-#B","testPrecisionRecall2b",mainConfiguration,converter);
		new PTA_FSMStructure(mach,mach.findVertex(VertexID.parseID("A")));
		
		TestHelper.checkForCorrectException(
				() -> new PTA_FSMStructure(mach,sameMach.findVertex(VertexID.parseID("A"))),
				IllegalArgumentException.class,"is not a valid state of the graph");
	}

	@Test
	public final void test_sequenceSet_testing_shouldBeReturned3() // a test for shouldBeReturned
	{
		final LearnerGraph machine = buildLearnerGraph("A-a->A-b->B", "test_sequenceSet_testing_shouldBeReturned3",mainConfiguration,converter);
		en = new PTA_FSMStructure(machine,null) {
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
		Map<String,String> actual = getDebugDataMap(en,seq.crossWithSet(labelList(new String[] {"b","a"}))
			.crossWithSet(labelList(new String[] {"b","a"})));
		vertifyPTA(en, 2, new String[][] {
				new String[] {"a","b"},
				new String[] {"a","a"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","a"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","b"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
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
		final LearnerGraph machine = new LearnerGraph(g,mainConfiguration);
		en = new PTA_FSMStructure(machine,null) {
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
		},mainConfiguration,converter)));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","b","c"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","b","c"}, PTASequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}

	@Test
	public final void test_sequenceSet4_2() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet result = seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"},
				new String[] {"a"},
				new String[] {"a","b","c","d"},
				
		},mainConfiguration,converter));
		Map<String,String> actual = getDebugDataMap(en,result);
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","b","c","d"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)},
				new Object[]{new String[] {"a","b","c"}, PTASequenceEngine.DebugDataValues.booleanToString(false, true)}
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
		Assert.assertFalse(result.isEmpty());
	}
	
	@Test
	public final void test_sequenceSet4_3() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		SequenceSet temp = seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"}
		},mainConfiguration,converter));
		SequenceSet result = temp.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"},
				new String[] {"c"}
		},mainConfiguration,converter));
		Map<String,String> actual = getDebugDataMap(en,result);
		vertifyPTA(en, 2, new String[][] {
				new String[] {"a","b","c","c"},
				new String[] {"a","b","c","a"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
		Assert.assertTrue(result.isEmpty());
	}
	
	@Test
	public final void test_sequenceSet5_1() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"}
		},mainConfiguration,converter)).crossWithSet(new LinkedList<>());
		Map<String,String> actual = getDebugDataMap(en,en.new SequenceSet().cross(TestFSMAlgo.buildList(new String[][] {// here the new sequenceSet is empty, hence whatever I do, there should be no changes
				new String[] {"a","b","c","d"},
				new String[] {"c"}
		},mainConfiguration,converter)));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","b","c"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}

	@Test
	public final void test_sequenceSet5_2() // a more complex composition
	{
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		seq.crossWithSequence(labelList(new String[] {"a","b","c"})).crossWithSet(new LinkedList<>());
		Map<String,String> actual = getDebugDataMap(en,en.new SequenceSet().cross(TestFSMAlgo.buildList(new String[][] {// here the new sequenceSet is empty, hence whatever I do, there should be no changes
				new String[] {"a","b","c","d"},
				new String[] {"c"}
		},mainConfiguration,converter)));
		vertifyPTA(en, 1, new String[][] {
				new String[] {"a","b","c"}
		});
		Map<String,String> expected=TestFSMAlgo.buildStringMap(new Object[][] {
		});
		assertEquals("expected: " + expected + ", actual: " + actual, expected, actual);
	}
	
	/** Test for equality of different sequenceSets. */
	@Test
	public final void test_sequenceSet_equality_differentcontainer()
	{
		SequenceSet seqStart1 = en.new SequenceSet();
		SequenceSet seqStart2 = en.new SequenceSet();
		SequenceSet seqDifferent1 = en.new SequenceSet();seqDifferent1.setIdentity();
		PTA_FSMStructure engine2 = new PTA_FSMStructure(fsm,null);		
		equalityTestingHelper(seqStart1,seqStart2,seqDifferent1,engine2.new SequenceSet(), true);
	}
	
	/** Test for equality of different sequenceSets. */
	@Test
	public final void test_sequenceSet_equality0a()
	{
		SequenceSet seqStart1 = en.new SequenceSet();
		SequenceSet seqStart2 = seqStart1.crossWithSequence(labelList(
				new String[] {"a"}
		));
		SequenceSet seqDifferent1 = en.new SequenceSet();seqDifferent1.setIdentity();
		SequenceSet seqDifferent2 = en.new SequenceSet();seqDifferent2.setIdentity();seqDifferent2.crossWithSequence(labelList(
				new String[] {"a"}
		));
		equalityTestingHelper(seqStart1,seqStart2,seqDifferent1,seqDifferent2, true);
	}
	
	/** Test for equality of different sequenceSets. */
	@Test
	public final void test_sequenceSet_equality0b()
	{
		SequenceSet seqStart1 = en.new SequenceSet();
		SequenceSet seqStart2 = en.new SequenceSet();
		SequenceSet seqDifferent1 = en.new SequenceSet();seqDifferent1.setIdentity();
		SequenceSet seqDifferent2 = en.new SequenceSet();seqDifferent2.setIdentity();seqDifferent2.crossWithSequence(labelList(
				new String[] {"a"}
		));
		equalityTestingHelper(seqStart1,seqStart2,seqDifferent1,seqDifferent2, true);
	}
	
	/** Test for equality of different sequenceSets. */
	@Test
	public final void test_sequenceSet_equality0c()
	{
		SequenceSet seqStart1 = en.new SequenceSet();seqStart1.crossWithSequence(labelList(
				new String[] {"t"}
		));
		SequenceSet seqStart2 = en.new SequenceSet();seqStart2.crossWithSequence(labelList(
				new String[] {"t"}
		));
		SequenceSet seqDifferent1 = en.new SequenceSet();seqDifferent1.setIdentity();
		SequenceSet seqDifferent2 = en.new SequenceSet();seqDifferent2.setIdentity();seqDifferent2.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a"}
				
		},mainConfiguration,converter));
		equalityTestingHelper(seqStart1,seqStart2,seqDifferent1,seqDifferent2, true);
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
		},mainConfiguration,converter));
		equalityTestingHelper(seqStart1,seqStart2,seqDifferent1,seqDifferent2, true);
	}
	
	/** Test for equality of different sequenceSets. */
	@Test
	public final void test_sequenceSet_equality2()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqStartOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		},mainConfiguration,converter));
		
		SequenceSet seqStartTwo = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c","a","a"}
		},mainConfiguration,converter));
		
		SequenceSet seqDifferent1 = en.new SequenceSet();
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a"}
		},mainConfiguration,converter));
		equalityTestingHelper(seqStartOne,seqStartTwo,seqDifferent1,seqDifferent2, true);
	}
	
	/** Test for equality of different sequenceSets. */
	@Test
	public final void test_sequenceSet_equality3()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqStartOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		},mainConfiguration,converter));
		
		SequenceSet seqStartTwo = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a"},
				new String[] {"c","a","a"}
				
		},mainConfiguration,converter)).cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a"},
				new String[] {"c","a","a"}
		},mainConfiguration,converter));
		
		SequenceSet seqDifferent1 = en.new SequenceSet();
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a"}
		},mainConfiguration,converter));
		equalityTestingHelper(seqStartOne,seqStartTwo,seqDifferent1,seqDifferent2, true);
	}

	/** Tests that we choke on an empty graph. */
	@Test(expected=IllegalArgumentException.class)
	public final void test_containsSequence1_fail()
	{
		DirectedSparseGraph g = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, false, UserData.SHARED);
		init.addUserDatum(JUConstants.LABEL, "A", UserData.SHARED);
		g.addVertex(init);
		PTASequenceEngine engine = new PTA_FSMStructure(new LearnerGraph(g,mainConfiguration),null);
		engine.containsSequence(new ArrayList<>());
	}
	
	/** Tests containsSequence. */
	@Test
	public final void test_containsSequence2()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		},mainConfiguration,converter));
		assertTrue(en.containsSequence(labelList(new String[]{"c"})));
		assertTrue(en.containsAsLeaf(labelList(new String[]{"c"})));
		assertFalse(en.containsSequence(labelList(new String[]{"a","a","b"})));
		assertFalse(en.containsAsLeaf(labelList(new String[]{"a","a","b"})));
		assertFalse(en.containsSequence(labelList(new String[]{"a","a","a"})));
		assertFalse(en.containsAsLeaf(labelList(new String[]{"a","a","a"})));
		assertTrue (en.containsSequence(labelList(new String[]{"a","a"})));
		assertTrue (en.containsAsLeaf(labelList(new String[]{"a","a"})));
		assertTrue (en.containsSequence(labelList(new String[]{"a"})));
		assertFalse(en.containsAsLeaf(labelList(new String[]{"a"})));
		assertTrue(en.containsSequence(labelList(new String[]{})));
		assertFalse(en.containsAsLeaf(labelList(new String[]{})));
		assertFalse(en.containsSequence(labelList(new String[]{"c","c"})));
		assertFalse(en.containsAsLeaf(labelList(new String[]{"c","c"})));
		assertFalse(en.containsSequence(labelList(new String[]{"b"})));
		assertFalse(en.containsAsLeaf(labelList(new String[]{"b"})));
	}
	
	/** Tests extendsLeaf. */
	@Test
	public final void test_extendsLeaf1()
	{
		SequenceSet seqStart = engine_testLimitToGraph.new SequenceSet();seqStart.setIdentity();
		assertFalse(engine_testLimitToGraph.extendsLeaf(labelList(new String[]{})));
		assertFalse(engine_testLimitToGraph.extendsLeaf(labelList(new String[]{"a"})));
	}
	
	/** Tests extendsLeaf. */
	@Test
	public final void test_extendsLeaf2()
	{
		SequenceSet seqStart = engine_testLimitToGraph.new SequenceSet();seqStart.setIdentity();
		seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","c","b"},
				new String[] {"c","c","c","c"}
		},mainConfiguration,converter));
		
		assertFalse(engine_testLimitToGraph.extendsLeaf(labelList(new String[]{})));
		assertFalse(engine_testLimitToGraph.extendsLeaf(labelList(new String[]{"a"})));
		assertFalse(engine_testLimitToGraph.extendsLeaf(labelList(new String[]{"c","c"})));
		assertFalse(engine_testLimitToGraph.extendsLeaf(labelList(new String[]{"a","c","b"})));
		assertTrue(engine_testLimitToGraph.extendsLeaf(labelList(new String[]{"a","c","b","a"})));
		assertTrue(engine_testLimitToGraph.extendsLeaf(labelList(new String[]{"c","c","c","c","c"})));
		assertFalse(engine_testLimitToGraph.extendsLeaf(labelList(new String[]{"c","c","c","c"})));
		assertFalse(engine_testLimitToGraph.extendsLeaf(labelList(new String[]{"c","c","c"})));
	}
	
	/** Test for Union: adding something to an empty set. */
	@Test
	public final void test_sequenceSet_union0()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		},mainConfiguration,converter));
		SequenceSet seqTwo = en.new SequenceSet();seqTwo.unite(seqOne);
		SequenceSet seqDifferent1 = seqTwo.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));

		equalityTestingHelper(seqOne,seqTwo,seqDifferent1,seqDifferent2, true);
	}
	
	/** Test for Union: adding an empty set to something. */
	@Test
	public final void test_sequenceSet_union1()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		},mainConfiguration,converter));
		SequenceSet seqA = en.new SequenceSet();seqOne.unite(seqA);
		SequenceSet seqTwo = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		},mainConfiguration,converter));
		SequenceSet seqDifferent1 = seqTwo.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));

		equalityTestingHelper(seqOne,seqTwo,seqDifferent1,seqDifferent2, true);
	}
	
	/** Test for Union: adding a set to itself. */
	@Test
	public final void test_sequenceSet_union2()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqA = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		},mainConfiguration,converter));
		SequenceSet seqTwo = en.new SequenceSet();seqTwo.unite(seqA);seqTwo.unite(seqTwo);
		List<List<Label>> arg = new LinkedList<>();
		Set<List<Label>> setOfStrings = TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}},mainConfiguration,converter);
		arg.addAll(setOfStrings);arg.addAll(setOfStrings);
		SequenceSet seqOne = seqStart.cross(arg);

		SequenceSet seqDifferent1 = seqTwo.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));

		equalityTestingHelper(seqOne,seqTwo,seqDifferent1,seqDifferent2, true);
	}
	
	/** Test for Union: adding a set from a different engine. */
	@Test(expected=IllegalArgumentException.class)
	public final void test_sequenceSet_union3()
	{
		SequenceSet seqStart = en.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"c"}
		},mainConfiguration,converter));
		SequenceSet seqTwo = new PTA_FSMStructure(fsm,null).new SequenceSet();
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
		},mainConfiguration,converter));
		
		SequenceSet seqDifferent1 = seqSecondStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));
		
		SequenceSet seqOne = seqSecondStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {}
		},mainConfiguration,converter));
		SequenceSet seqOneB = seqSecondStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"}
		},mainConfiguration,converter));
		
		seqOne.unite(seqOneB);
		
		SequenceSet seqTwo = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"a","a",	"a","b","c"}
		},mainConfiguration,converter));
		
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"},
		},mainConfiguration,converter));
	
		equalityTestingHelper(seqOne,seqTwo,seqDifferent1,seqDifferent2, true);
	}
	
	@Test
	public final void test_sequenceSet_limitTo0()
	{
		SequenceSet seqStart = engine_testLimitToGraph.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","c"},
				new String[] {"a","a"},
				new String[] {"c"}
		},mainConfiguration,converter));seqOne.limitTo(0);
		Assert.assertTrue(seqOne.isEmpty());
	}
	
	@Test
	public final void test_sequenceSet_limitTo1()
	{
		SequenceSet seqStart = engine_testLimitToGraph.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"},
				new String[] {"a","c"},
				new String[] {"c"}
		},mainConfiguration,converter));seqOne.limitTo(1);
		Assert.assertFalse(seqOne.isEmpty());
		SequenceSet seqTwo = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","a"}}
				,mainConfiguration,converter
		));
		SequenceSet seqDifferent1 = seqTwo.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));

		equalityTestingHelper(seqOne,seqTwo,seqDifferent1,seqDifferent2, true);
	}

	@Test
	public final void test_sequenceSet_limitTo2()
	{
		SequenceSet seqStart = engine_testLimitToGraph.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","c"},
				new String[] {"a","a"},
				new String[] {"c"}
		},mainConfiguration,converter));
		seqOne.limitTo(2);
		Assert.assertFalse(seqOne.isEmpty());
		SequenceSet seqTwo = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","c"},
				new String[] {"a","a" }
				},mainConfiguration,converter));
		SequenceSet seqDifferent1 = seqTwo.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));

		equalityTestingHelper(seqOne,seqTwo,seqDifferent1,seqDifferent2, true);
	}

	@Test
	public final void test_sequenceSet_limitTo3()
	{
		SequenceSet seqStart = engine_testLimitToGraph.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","c"},
				new String[] {"a","a"},
				new String[] {"c"}
		},mainConfiguration,converter));seqOne.limitTo(2000);
		Assert.assertFalse(seqOne.isEmpty());
		SequenceSet seqTwo = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","c"},
				new String[] {"a","a"},
				new String[] {"c"}
				},mainConfiguration,converter));
		SequenceSet seqDifferent1 = seqTwo.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));

		equalityTestingHelper(seqOne,seqTwo,seqDifferent1,seqDifferent2, true);
	}

	@Test
	public final void test_sequenceSet_limitTo4()
	{
		SequenceSet seqStart = engine_testLimitToGraph.new SequenceSet();seqStart.setIdentity();
		SequenceSet seqOne = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","c"},
				new String[] {"a","a"},
				new String[] {"c"}
		},mainConfiguration,converter));seqOne.limitTo(-1);
		Assert.assertFalse(seqOne.isEmpty());
		SequenceSet seqTwo = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","c"},
				new String[] {"a","a"},
				new String[] {"c"}
				},mainConfiguration,converter));
		SequenceSet seqDifferent1 = seqTwo.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));
		SequenceSet seqDifferent2 = seqStart.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b"}}
				,mainConfiguration,converter
		));

		equalityTestingHelper(seqOne,seqTwo,seqDifferent1,seqDifferent2, true);
	}

	@Test
	public final void test_stringCollectionSize0()
	{
		fsm = buildLearnerGraph("A-a->A-b->B", "test_sequenceSet3_6",mainConfiguration,converter);
		en = new PTA_FSMStructure(fsm,null);		
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
		fsm = buildLearnerGraph("A-a->A-b->B", "test_sequenceSet3_6",mainConfiguration,converter);
		en = new PTA_FSMStructure(fsm,null);		
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		seq.crossWithSequence(labelList(new String[] {"b","a"}));
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
		fsm = buildLearnerGraph("A-a->A-b->B", "test_sequenceSet3_6",mainConfiguration,converter);
		en = new PTA_FSMStructure(fsm,null);		
		SequenceSet seq = en.new SequenceSet();seq.setIdentity();
		seq.crossWithSet(labelList(new String[] {"b","a"}))
			.crossWithSet(labelList(new String[] {"b","a"}))
			.crossWithSet(labelList(new String[] {"a","b"}));
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
