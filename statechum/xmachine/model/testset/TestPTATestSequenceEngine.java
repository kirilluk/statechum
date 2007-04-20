package statechum.xmachine.model.testset;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import junit.framework.JUnit4TestAdapter;

import org.junit.Before;
import org.junit.Test;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

import statechum.JUConstants;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.PTATestSequenceEngine.Node;
import statechum.xmachine.model.testset.PTATestSequenceEngine.sequenceSet;
import sun.security.provider.certpath.BuildStep;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class TestPTATestSequenceEngine 
{

	private void vertifyPTA(PTATestSequenceEngine en, String [][] expected)
	{
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(en.getData());
		Set<List<String>> expectedSet = TestFSMAlgo.buildSet(expected);
		assertTrue("expected: "+expectedSet+" received : "+actual,expectedSet.equals(actual));	
	}
	
	@Test
	public final void testGetData_Empty1()
	{
		DirectedSparseGraph g = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum("property", "init", UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, "false", UserData.SHARED);
		init.addUserDatum(JUConstants.LABEL, "A", UserData.SHARED);
		g.addVertex(init);
		PTATestSequenceEngine en = new PTA_FSMStructure(WMethod.getGraphData(g));
		vertifyPTA(en, new String[][] { 
				new String[] {}
			});
	}

	@Test
	public final void testGetData_Empty2()
	{
		DirectedSparseGraph g = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum("property", "init", UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);
		init.addUserDatum(JUConstants.LABEL, "A", UserData.SHARED);
		g.addVertex(init);
		PTATestSequenceEngine en = new PTA_FSMStructure(WMethod.getGraphData(g));
		vertifyPTA(en, new String[][] {
				new String[] {}
		});
	}

	@Test
	public final void testNodeEquality()
	{
		PTATestSequenceEngine engine = new PTATestSequenceEngine();
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
		PTATestSequenceEngine engine = new PTATestSequenceEngine();
		Node a = engine.new Node("A");
		
		assertTrue(a.isAccept());
		assertTrue(engine.new Node("test").isAccept());
		assertFalse(engine.rejectNode.isAccept());
	}

	@Test
	public final void testNode2()
	{
		PTATestSequenceEngine engine = new PTATestSequenceEngine();
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

	private PTATestSequenceEngine en = null; 
	private FSMStructure fsm = null;
	
	@Before
	public final void setUp()
	{
		fsm = WMethod.getGraphData(TestFSMAlgo.buildGraph("A-a->B-a->A-b-#C\nB-b->D-c->E", "sample automaton"));
		en = new PTA_FSMStructure(fsm);		
	}
	
	@Test
	public final void test_sequenceSet1()
	{
		sequenceSet seq = en.new sequenceSet();
		seq.crossWithSet(Arrays.asList(new String[] {"a"})); // appending anything to an empty sequence produces an empty sequence.
		vertifyPTA(en, new String[][] {
				new String[] {}
		});
	}
	
	@Test
	public final void test_sequenceSet2() // an input which exists
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"a"}));
		vertifyPTA(en, new String[][] {
				new String[] {"a"}
		});
	}
	
	@Test
	public final void test_sequenceSet3() // the one which does not exist
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"c"}));
		vertifyPTA(en, new String[][] {
				new String[] {"c"}
		});
	}
	
	@Test
	public final void test_sequenceSet4() // the one which enters a reject state
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"b"}));
		vertifyPTA(en, new String[][] {
				new String[] {"b"}
		});
	}
	
	@Test
	public final void test_sequenceSet5() // a composition of sequenceSet with an input which exists
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}));
		vertifyPTA(en, new String[][] {
				new String[] {"a","a"}
		});
	}
	
	@Test
	public final void test_sequenceSet6() // a composition of sequenceSet with the one which does not exist
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"c"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}));
		vertifyPTA(en, new String[][] {
				new String[] {"c"}
		});
	}
	
	@Test
	public final void test_sequenceSet7() // a composition of sequenceSet with the one which enters a reject state
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"b"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}));
		vertifyPTA(en, new String[][] {
				new String[] {"b"}
		});
	}
	
	@Test
	public final void test_sequenceSet_2_1() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"b"}))
			.crossWithSet(Arrays.asList(new String[] {"c"}));
		vertifyPTA(en, new String[][] {
				new String[] {"a","a","a","b","c"}
		});
	}

	@Test
	public final void test_sequenceSet2_2() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"b"}))
			.crossWithSet(Arrays.asList(new String[] {"c"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}));
		vertifyPTA(en, new String[][] {
				new String[] {"a","a","a","b","c","a"}
		});
	}

	@Test
	public final void test_sequenceSet2_3() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"b"}))
			.crossWithSet(Arrays.asList(new String[] {"c"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"b"}));
		vertifyPTA(en, new String[][] {
				new String[] {"a","a","a","b","c","a"}
		});
	}

	@Test
	public final void test_sequenceSet2_4() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"c"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"b"}))
			.crossWithSet(Arrays.asList(new String[] {"c"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}));
		vertifyPTA(en, new String[][] {
				new String[] {"c"}
		});
	}

	@Test
	public final void test_sequenceSet2_5() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		sequenceSet tempE = 
			seq.crossWithSet(Arrays.asList(new String[] {"a"}))
				.crossWithSet(Arrays.asList(new String[] {"a"}))
				.crossWithSet(Arrays.asList(new String[] {"a"}))
				.crossWithSet(Arrays.asList(new String[] {"b"}))
				.crossWithSet(Arrays.asList(new String[] {"c"}));
		tempE.crossWithSet(Arrays.asList(new String[] {"a"}));
		tempE.crossWithSet(Arrays.asList(new String[] {"b"}));
		vertifyPTA(en, new String[][] {
				new String[] {"a","a","a","b","c","a"},
				new String[] {"a","a","a","b","c","b"}
		});
	}

	@Test
	public final void test_sequenceSet2_6() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"b"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}));

		sequenceSet tempE = 
			seq.crossWithSet(Arrays.asList(new String[] {"a"}))
				.crossWithSet(Arrays.asList(new String[] {"a"}))
				.crossWithSet(Arrays.asList(new String[] {"a"}))
				.crossWithSet(Arrays.asList(new String[] {"b"}))
				.crossWithSet(Arrays.asList(new String[] {"c"}));
		tempE.crossWithSet(Arrays.asList(new String[] {"a"}));
		tempE.crossWithSet(Arrays.asList(new String[] {"b"}));
		vertifyPTA(en, new String[][] {
				new String[] {"a","a","a","b","c","a"},
				new String[] {"a","a","a","b","c","b"},
				new String[] {"b"}
		});
	}

	@Test
	public final void test_sequenceSet3_1() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"b","a","d"}));
		vertifyPTA(en, new String[][] {
				new String[] {"b"},
				new String[] {"a"},
				new String[] {"d"}
		});
	}

	@Test
	public final void test_sequenceSet3_2() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"b","a","d"}))
			.crossWithSet(Arrays.asList(new String[] {"b","a","d"}));
		vertifyPTA(en, new String[][] {
				new String[] {"b"},
				new String[] {"a","a"},
				new String[] {"a","d"},
				new String[] {"a","b"},
				new String[] {"d"}
		});
	}

	@Test
	public final void test_sequenceSet3_3() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		sequenceSet temp = seq.crossWithSet(Arrays.asList(new String[] {"b","a","d"}));
		temp.crossWithSet(Arrays.asList(new String[] {"b","a","d"}))
			.crossWithSet(Arrays.asList(new String[] {"u","a","d"}));
		vertifyPTA(en, new String[][] {
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
	}

	@Test
	public final void test_sequenceSet3_4() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		sequenceSet temp = seq.crossWithSet(Arrays.asList(new String[] {"b","a","d"}));
		temp.crossWithSet(Arrays.asList(new String[] {"b","a","d"}))
			.crossWithSet(Arrays.asList(new String[] {"u","a","d"}));
		temp.crossWithSet(Arrays.asList(new String[] {"u","a","d"}));
		temp.crossWithSet(Arrays.asList(new String[] {"u","a","d"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}))
			.crossWithSet(Arrays.asList(new String[] {"b"}))
			.crossWithSet(Arrays.asList(new String[] {"c"}))
			.crossWithSet(Arrays.asList(new String[] {"b","a"}));
		
		vertifyPTA(en, new String[][] {
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
	}

	@Test
	public final void test_sequenceSet3_5() // a more complex composition
	{
		fsm = WMethod.getGraphData(TestFSMAlgo.buildGraph("A-a->B-a->A-b-#C\nA-d->M-a->N\nB-b->D-c->E", "sample automaton"));
		en = new PTA_FSMStructure(fsm);		
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"b","a","d"}))
			.crossWithSet(Arrays.asList(new String[] {"b","a","d"}))
			.crossWithSet(Arrays.asList(new String[] {"a"}));
		vertifyPTA(en, new String[][] {
				new String[] {"b"},
				new String[] {"a","a","a"},
				new String[] {"a","b","a"},
				new String[] {"a","d"},
				new String[] {"d","a","a"},
				new String[] {"d","b"},
				new String[] {"d","d"}
		});
	}

	@Test
	public final void test_sequenceSet3_6() // a more complex composition
	{
		fsm = WMethod.getGraphData(TestFSMAlgo.buildGraph("A-a->A-b->B", "sample automaton"));
		en = new PTA_FSMStructure(fsm);		
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSet(Arrays.asList(new String[] {"b","a"}))
			.crossWithSet(Arrays.asList(new String[] {"b","a"}))
			.crossWithSet(Arrays.asList(new String[] {"a","b"}));
		vertifyPTA(en, new String[][] {
				new String[] {"b","b"},
				new String[] {"b","a"},
				new String[] {"a","a","b"},
				new String[] {"a","a","a"},
				new String[] {"a","b","a"},
				new String[] {"a","b","b"}
		});
	}

	@Test
	public final void test_sequenceSet4_1() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"}
		}));
		vertifyPTA(en, new String[][] {
				new String[] {"a","b","c"}
		});
	}

	@Test
	public final void test_sequenceSet4_2() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"},
				new String[] {"a"},
				new String[] {"a","b","c","d"},
				
		}));
		vertifyPTA(en, new String[][] {
				new String[] {"a","b","c","d"}
		});
	}
	
	@Test
	public final void test_sequenceSet4_3() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		sequenceSet temp = seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"}
		}));
		temp.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"},
				new String[] {"c"}
		}));
		vertifyPTA(en, new String[][] {
				new String[] {"a","b","c","c"},
				new String[] {"a","b","c","a"}
		});
	}
	
	@Test
	public final void test_sequenceSet5_1() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.cross(TestFSMAlgo.buildList(new String[][] {
				new String[] {"a","b","c"}
		})).crossWithSet(new LinkedList<String>());
		en.new sequenceSet().cross(TestFSMAlgo.buildList(new String[][] {// here the new sequenceSet is empty, hence whatever I do, there should be no changes
				new String[] {"a","b","c","d"},
				new String[] {"c"}
		}));
		vertifyPTA(en, new String[][] {
				new String[] {"a","b","c"}
		});
	}

	@Test
	public final void test_sequenceSet5_2() // a more complex composition
	{
		sequenceSet seq = en.new sequenceSet();seq.setIdentity();
		seq.crossWithSequence(Arrays.asList(new String[] {"a","b","c"})).crossWithSet(new LinkedList<String>());
		en.new sequenceSet().cross(TestFSMAlgo.buildList(new String[][] {// here the new sequenceSet is empty, hence whatever I do, there should be no changes
				new String[] {"a","b","c","d"},
				new String[] {"c"}
		}));
		vertifyPTA(en, new String[][] {
				new String[] {"a","b","c"}
		});
	}

	/** In order to be able to use old junit runner. */
	public static junit.framework.Test suite()
	{
		return new JUnit4TestAdapter(TestPTATestSequenceEngine.class);
	}
}
