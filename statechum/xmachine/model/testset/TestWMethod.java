/**
 * 
 */
package statechum.xmachine.model.testset;

import static org.junit.Assert.*;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Map.Entry;

import javax.swing.SwingUtilities;

import junit.framework.Assert;
import junit.framework.JUnit4TestAdapter;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

import statechum.JUConstants;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.WMethod.EquivalentStatesException;
import static statechum.analysis.learning.TestFSMAlgo.buildGraph;
import static statechum.analysis.learning.TestFSMAlgo.buildSet;
import static statechum.analysis.learning.TestFSMAlgo.buildList;
import static statechum.xmachine.model.testset.WMethod.getGraphData;
import static statechum.xmachine.model.testset.WMethod.cross;
import static statechum.xmachine.model.testset.WMethod.crossWithSet;
import static statechum.xmachine.model.testset.WMethod.crossWithSet_One;
import static statechum.xmachine.model.testset.PrefixFreeCollection.isPrefix;

/**
 * @author kirr
 *
 */
public class TestWMethod {


	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover1() {
		Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{},new String[]{"b"},new String[]{"b","a"}}),
			actual = new HashSet<List<String>>();
			actual.addAll(WMethod.computeStateCover(getGraphData(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S","computeStateCover1"))));
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover2() {
		Collection<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{},new String[]{"b"},new String[]{"b","a"}}),
			actual = new HashSet<List<String>>();
			actual.addAll(WMethod.computeStateCover(getGraphData(buildGraph("A-d->A-b->B-c->B-a->C\nQ-d->S","computeStateCover2"))));
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover3() {
		Collection<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{},new String[]{"b"},new String[]{"d"},new String[]{"b","a"}}),
			actual = new HashSet<List<String>>();
			actual.addAll(WMethod.computeStateCover(getGraphData(buildGraph("A-a->A\nD<-d-A-b->B-c->B-a->C\nQ-d->S","computeStateCover3"))));
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover4() {
		Collection<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{},new String[]{"a"},new String[]{"b"},new String[]{"d"},new String[]{"b","a"},new String[]{"b","a","a"}}),
			actual = new HashSet<List<String>>();
			actual.addAll(WMethod.computeStateCover(getGraphData(buildGraph("A-a->S\nD<-d-A-b->B-c->B-a->C-a->Q\nQ-d->S","computeStateCover3"))));
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix0() {
		Assert.assertTrue(isPrefix(Arrays.asList(new String[]{"a","b","c"}),Arrays.asList(new String[]{})));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix1() {
		Assert.assertTrue(isPrefix(Arrays.asList(new String[]{"a","b","c"}),Arrays.asList(new String[]{"a"})));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix2() {
		Assert.assertFalse(isPrefix(Arrays.asList(new String[]{"a","b","c"}),Arrays.asList(new String[]{"a","b","c","d"})));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix3() {
		Assert.assertTrue(isPrefix(Arrays.asList(new String[]{"a","b","c"}),Arrays.asList(new String[]{"a","b","c"})));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross1a() {
		Set<List<String>> actual = new HashSet<List<String>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}), 
		buildList(new String[][] {new String[]{"c","d"}, new String[]{"q","w"}})));
		Set<List<String>> expected = buildSet(new String[][] {
						new String[]{"a","b","c","d"},
						new String[]{"a","b","q","w"},
						new String[]{"z","x","c","d"},
						new String[]{"z","x","q","w"}});
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross1b() {
		Set<List<String>> actual = new HashSet<List<String>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{},new String[]{"a","b"},new String[]{"z","x"}}), 
				buildList(new String[][] {new String[]{},new String[]{"c","d"}, new String[]{"q","w"}})));
		Set<List<String>> expected = buildSet(new String[][] {
						new String[]{},
						new String[]{"a","b"},new String[]{"z","x"},
						new String[]{"c","d"},new String[]{"q","w"},
						new String[]{"a","b","c","d"},
						new String[]{"a","b","q","w"},
						new String[]{"z","x","c","d"},
						new String[]{"z","x","q","w"}});
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross2() {
		Set<List<String>> actual = new HashSet<List<String>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{"a","b"}}), 
				buildList(new String[][] {new String[]{"c","d"}, new String[]{"q","w"}})));
		Set<List<String>> expected = buildSet(new String[][] {
						new String[]{"a","b","c","d"},
						new String[]{"a","b","q","w"}});
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross3() {
		Set<List<String>> actual = new HashSet<List<String>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}), 
				buildList(new String[][] {new String[]{"c","d"}})));
		Set<List<String>> expected = buildSet(new String[][] {
						new String[]{"a","b","c","d"},
						new String[]{"z","x","c","d"}});
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross4() {
		Set<List<String>> actual = new HashSet<List<String>>();
		actual.addAll(cross(buildList(new String[][] {}), 
				buildList(new String[][] {new String[]{"c","d"}, new String[]{"q","w"}})));
				
		Assert.assertTrue(actual.isEmpty());		
	}				

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross5() {
		Set<List<String>> actual = new HashSet<List<String>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}), 
				buildList(new String[][] {})));
				
		Assert.assertTrue(actual.isEmpty());		
	}
	
	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet1() {
		Set<List<String>> actualA = new HashSet<List<String>>(), actualB = new HashSet<List<String>>();
		List<String> with = Arrays.asList(new String[]{"p","q"});
		actualA.addAll(crossWithSet(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}),with));
		actualB.addAll(crossWithSet_One(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}),with));
		Set<List<String>> expected = buildSet(new String[][] {
				new String[]{"a","b","p"},
				new String[]{"z","x","p"},
				new String[]{"a","b","q"},
				new String[]{"z","x","q"}});
		Assert.assertTrue("expected: "+expected+" actual: "+actualA,expected.equals(actualA));
		Assert.assertTrue("expected: "+expected+" actual: "+actualB,expected.equals(actualB));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet2() {
		Set<List<String>> actualA = new HashSet<List<String>>(), actualB = new HashSet<List<String>>();
		List<String> with = Arrays.asList(new String[]{});
		actualA.addAll(crossWithSet(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}),with));
		actualB.addAll(crossWithSet_One(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}),with));
		Assert.assertEquals(0, actualA.size());
		Assert.assertEquals(0, actualB.size());
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet3() {
		Set<List<String>> actualA = new HashSet<List<String>>(), actualB = new HashSet<List<String>>();
		List<String> with = Arrays.asList(new String[]{"p","q"});
		actualA.addAll(crossWithSet(buildList(new String[][] {}),with));
		actualB.addAll(crossWithSet(buildList(new String[][] {}),with));
		Assert.assertEquals(0, actualA.size());
		Assert.assertEquals(0, actualB.size());
	}

	@Test
	public final void testCrossWithSet4() {
		Set<List<String>> actualA = new HashSet<List<String>>(), actualB = new HashSet<List<String>>();
		List<String> with = Arrays.asList(new String[]{"p"});
		LinkedList<List<String>> data = new LinkedList<List<String>>();
		LinkedList<String> seq = new LinkedList<String>();
		seq.clear();seq.addAll(Arrays.asList(new String[]{"a","b"}));data.add((List<String>)seq.clone());
		seq.clear();seq.addAll(Arrays.asList(new String[]{"z","x"}));data.add((List<String>)seq.clone());// cannot use buildList here because it returns a collection of immutable sequences
		
		actualA.addAll(crossWithSet((LinkedList<List<String>>)data.clone(),with));
		actualB.addAll(crossWithSet_One((LinkedList<List<String>>)data.clone(),with));
		Set<List<String>> expected = buildSet(new String[][] {
				new String[]{"a","b","p"},
				new String[]{"z","x","p"}});
		Assert.assertTrue("expected: "+expected+" actual: "+actualA,expected.equals(actualA));
		Assert.assertTrue("expected: "+expected+" actual: "+actualB,expected.equals(actualB));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet5() {
		Set<List<String>> actualA = new HashSet<List<String>>(), actualB = new HashSet<List<String>>();
		List<String> with = Arrays.asList(new String[]{});
		actualA.addAll(crossWithSet(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}),with));
		actualB.addAll(crossWithSet_One(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}),with));
		Assert.assertEquals(0, actualA.size());
		Assert.assertEquals(0, actualB.size());
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet6() {
		Set<List<String>> actualA = new HashSet<List<String>>(), actualB = new HashSet<List<String>>();
		List<String> with = Arrays.asList(new String[]{"p"});
		actualA.addAll(crossWithSet(buildList(new String[][] {}),with));
		actualB.addAll(crossWithSet(buildList(new String[][] {}),with));
		Assert.assertEquals(0, actualA.size());
		Assert.assertEquals(0, actualB.size());
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#getStimuli}.
	 */
	@Test
	public final void testGetStimuli1() {
		Assert.assertTrue(WMethod.makeSingleton(WMethod.computeAlphabet(new DirectedSparseGraph())).isEmpty());
	}	

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#getStimuli}.
	 */
	@Test
	public final void testGetStimuli2() {
		Set<List<String>> singleton = new HashSet<List<String>>();
		singleton.addAll(WMethod.makeSingleton(WMethod.computeAlphabet(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testGetStimuli2"))));
		Assert.assertTrue(singleton
				.equals(buildSet(new String[][]{
						new String[]{"p"},
						new String[]{"d"},
						new String[]{"b"},
						new String[]{"c"},
						new String[]{"a"}
				})));
	}

	@Test
	public final void testComputeFSMAlphabet1()
	{
		Set<String> expected = new HashSet<String>();
		expected.add("p");
		Assert.assertTrue(WMethod.computeAlphabet(getGraphData(buildGraph("A-p->A","testComputeFSMAlphabet1"))).
				equals(expected));
				
	}
	
	@Test
	public final void testComputeFSMAlphabet2()
	{
		Set<String> expected = new HashSet<String>();
		expected.add("p");expected.add("d");expected.add("b");expected.add("c");expected.add("a");
		Assert.assertTrue(WMethod.computeAlphabet(getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testComputeFSMAlphabet2"))).
				equals(expected));
				
	}
	
	@Test
	public final void testTruncateSeq1()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq1"));
		Assert.assertEquals(Arrays.asList(new String[]{"p","b"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"p","b"})));
	}

	@Test
	public final void testTruncateSeq2()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq2"));
		Assert.assertEquals(Arrays.asList(new String[]{"p","b","c"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"p","b","c"})));
	}
	
	@Test
	public final void testTruncateSeq3()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq3"));
		Assert.assertEquals(Arrays.asList(new String[]{}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{})));
	}

	@Test
	public final void testTruncateSeq4()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq4"));
		Assert.assertEquals(Arrays.asList(new String[]{"p","b","z"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"p","b","z","e"})));
	}

	@Test
	public final void testTruncateSeq5()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq5"));
		Assert.assertEquals(Arrays.asList(new String[]{"p","b","d"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"p","b","d"})));
	}

	@Test
	public final void testTruncateSeq5b()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-f->S","testTruncateSeq5b"));
		Assert.assertEquals(Arrays.asList(new String[]{"p","b","d"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"p","b","d"})));
	}

	@Test
	public final void testTruncateSeq6()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq6"));
		Assert.assertEquals(Arrays.asList(new String[]{"a"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"a"})));
	}
	
	@Test
	public final void testTruncateSeq7()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq7"));
		Assert.assertEquals(Arrays.asList(new String[]{"p","p","p"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"p","p","p"})));
	}
	
	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence0() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence0"));
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence0b() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence0b"));
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{}));

		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence1() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence1"));
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","b"}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence2() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence2"));
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","b"}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence3() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence3"));
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","b","c"}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{"p","b","c"}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence4() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence4"));
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","b"},
				new String[]{"p","p"}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{"p","p"}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence5() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence5"));
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","b"},
				new String[]{"p","p","p"}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{"p"}));
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{"p","p"}));
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{"p","p","p"}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendAllSequences(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testAppendAllSequences0() {
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendAllSequences0"));
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","p","p"},
				new String[]{"g"}
			});
		
		WMethod.appendAllSequences(fsm, sequences, buildList(new String[][] {
				new String[]{"p"},new String[]{"p","p"},new String[] {}, new String [] {"g"},new String[]{"p","p","p"}}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendAllSequences(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testAppendAllSequences0b() {
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendAllSequences0b"));
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		Set<List<String>> expected = buildSet(new String[][]{
			});
		
		WMethod.appendAllSequences(fsm, sequences, buildList(new String[][] {}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendAllSequences(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testAppendAllSequences1() {
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendAllSequences1"));
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","b"},
				new String[]{"g"},
				new String[]{"p","p","p"}
			});
		
		WMethod.appendAllSequences(fsm, sequences, buildList(new String[][] {
				new String[]{"p"},new String[]{"p","p"},new String[] {}, new String [] {"g"},new String[]{"p","p","p"}}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	public static void testWsetconstruction(String machine, boolean equivalentExpected)
	{
		DirectedSparseGraph g = buildGraph(machine,"testWset");
		FSMStructure fsm = getGraphData(g);//visFrame.update(null, g);
		try
		{
			List<List<String>> wset = WMethod.computeWSet(fsm);
			Assert.assertEquals(false, equivalentExpected);
			//System.out.println("states: "+fsm.accept.size()+" Wset has "+wset.size()+" elements");
			for(Entry<String,Boolean> stateA:fsm.accept.entrySet())
			{
				for(Entry<String,Boolean> stateB:fsm.accept.entrySet())
					if (!stateA.getKey().equals(stateB.getKey()))
					{
						boolean foundString = false;
						Iterator<List<String>> pathIt = wset.iterator();
						while(pathIt.hasNext() && !foundString)
						{
							List<String> path = pathIt.next();
							int aResult = WMethod.tracePath(fsm, path, stateA.getKey()),
								bResult = WMethod.tracePath(fsm, path, stateB.getKey());
							
							if ( (aResult == RPNIBlueFringeLearner.USER_ACCEPTED && bResult >= 0) ||
									(bResult == RPNIBlueFringeLearner.USER_ACCEPTED && aResult >= 0))
								foundString = true;
						}
						
						if (!foundString)
							fail("W set does not distinguish between "+stateA.getKey()+" and "+stateB.getKey());
					}
			}
		}
		catch(EquivalentStatesException e)
		{
			Assert.assertEquals(true, equivalentExpected);
			TestFSMAlgo.checkM(fsm,fsm,e.getA(),e.getB());
		}
		
	}
	
	@Test
	public final void testWset1()
	{
		testWsetconstruction("A-p->A-b->B-c->B-a->C",false);
	}
	
	@Test
	public final void testWset2()
	{
		testWsetconstruction("A-a->C-b->Q\nB-a->D-a->Q",false);
	}
	
	@Test
	public final void testWset3() // equivalent states
	{
		testWsetconstruction("A-a->C-b->Q\nB-a->D-b->Q",true);
	}

	@Test
	public final void testWset4()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-a->A","testWset"));
		Assert.assertTrue(WMethod.computeWSet(fsm).isEmpty());
	}

	@Test
	public final void testWset5a() // equivalent states
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3",
				true);
	}
	
	@Test
	public final void testWset5b() // equivalent states
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3",
				true);
	}
	
	@Test
	public final void testWset6() // no equivalent states
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-m->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-z->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3",
				false);
	}

	@Test
	public final void testWset7()
	{
		testWsetconstruction("A-a->B-a->C-a->A-b->C-b->B",false);
	}
	
	@Test
	public final void testWset8()
	{
		testWsetconstruction("S-a->A-a->D-a->D-b->A-b->B-a->D\nB-b->C-a->D\nC-b->D\nS-b->N-a->M-a->N\nN-b->M-b->N",true);
	}
	
	@Test
	public final void testWset9()
	{
		testWsetconstruction("A-a->D\nB-a->C\nA-b->B\nD-b->C",false);
	}
	
	@Test
	public final void testCheckEquivalentStates1() // equivalent states
	{
		DirectedSparseGraph g = buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1");
		FSMStructure fsm = getGraphData(g);
		Assert.assertEquals(true, WMethod.checkEquivalentStates(fsm));
	}
	
	@Test
	public final void testCheckEquivalentStates2() // no equivalent states
	{
		DirectedSparseGraph g = buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-m->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-z->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates2");
		FSMStructure fsm = getGraphData(g);
		Assert.assertEquals(false, WMethod.checkEquivalentStates(fsm));
	}

	@Test
	public final void testCheckEquivalentStates3() // no equivalent states
	{
		int [][]table = new int[][] {
				{1,-1},
				{-1,1}
			};
		FSMStructure fsm = TestFSMAlgo.convertTableToFSMStructure(table, new int[]{0,1}, -1);
		Assert.assertEquals(false, WMethod.checkEquivalentStates(fsm));
	}

	@Test
	public final void testCheckEquivalentStates4() // equivalent states
	{
		int [][]table = new int[][] {
				{1,-1},
				{1,-1}
			};
		FSMStructure fsm = TestFSMAlgo.convertTableToFSMStructure(table, new int[]{0,1}, -1);
		Assert.assertEquals(true, WMethod.checkEquivalentStates(fsm));
	}
	
	private void checkTestGenerationResult(WMethod testGenerator, String [][] expected)
	{
		Set<List<String>> expectedSet = buildSet(expected),
			actualA = new HashSet<List<String>>(),actualB = new HashSet<List<String>>();
		actualA.addAll(testGenerator.computeOldTestSet());actualB.addAll(testGenerator.computeNewTestSet());
		assertTrue("old test generator failure, received "+actualA,expectedSet.equals(actualA));
		assertTrue("new test generator failure, received "+actualB,expectedSet.equals(actualB));
	}
	
	@Test
	public final void testTestGeneration1()
	{
		DirectedSparseGraph g = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum(JUConstants.LABEL, "A", UserData.SHARED);
		init.addUserDatum(JUConstants.PROPERTY, JUConstants.INIT, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);
		g.addVertex(init);
		WMethod wm = new WMethod(g,0);
		checkTestGenerationResult(wm, new String[][] {
				new String[] {}
			});
	}
	
	@Test
	public final void testTestGeneration2()
	{
		WMethod wm = new WMethod(buildGraph("A-a->A", "testTestGeneration2"),0);
		checkTestGenerationResult(wm, new String[][] {
				new String[] {"a"}
			});
	}
	
	@Test
	public final void testTestGeneration3()
	{
		WMethod wm = new WMethod(buildGraph("A-a->A", "testTestGeneration3"),1);
		checkTestGenerationResult(wm, new String[][] {
				new String[] {"a","a"}
			});
	}
	
	@Test
	public final void testTestGeneration4()
	{
		WMethod wm = new WMethod(buildGraph("A-a->A-b->B", "testTestGeneration4"),0);
		checkTestGenerationResult(wm, new String[][] {
				new String[] {"a","a"},
				new String[] {"b","a"},
				new String[] {"b","b"}
			});
	}
	
	@Test
	public final void testTestGeneration5()
	{
		WMethod wm = new WMethod(buildGraph("A-a->A-b->B", "testTestGeneration5"),1);
		checkTestGenerationResult(wm, new String[][] {
				new String[] {"a","a","a"},
				new String[] {"a","b","a"},
				new String[] {"b","a"},
				new String[] {"b","b"}
			});
	}

	@Test
	public final void testTestGeneration6()
	{
		WMethod wm = new WMethod(buildGraph(
				"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA<-c-A2<-c-A3\n"+"A<-d-A5<-a-A3-b->A4-f->AA4-a->S\n"+
				"A-d->A\nA5-b->AA6", "testTestGeneration6"),0);
		Set<List<String>> 
			actualA = new HashSet<List<String>>(),actualB = new HashSet<List<String>>();
			actualA.addAll(wm.computeOldTestSet());actualB.addAll(wm.computeNewTestSet());
			assertTrue("the two test generators return different values, old returns "+
					actualA+" and the new one - "+actualB,
			actualA.equals(actualB));
	}
	
	@Test
	public final void testTestGeneration7()
	{
		WMethod wm = new WMethod(buildGraph(
				"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA<-c-A2<-c-A3\n"+"A<-d-A5<-a-A3-b->A4-f->AA4-a->S\n"+
				"A-d->A\nA5-b->AA6", "testTestGeneration6"),2);
		Set<List<String>> 
			actualA = new HashSet<List<String>>(),actualB = new HashSet<List<String>>();
			actualA.addAll(wm.computeOldTestSet());actualB.addAll(wm.computeNewTestSet());
			assertTrue("the two test generators return different values, old returns "+
					actualA+" and the new one - "+actualB,
			actualA.equals(actualB));
	}
	
	@Test
	public final void testCheckUnreachable1()
	{
		assertFalse(WMethod.checkUnreachableStates(WMethod.getGraphData(buildGraph("A-a->A", "testCheckUnreachable1"))));	
	}
	
	@Test
	public final void testCheckUnreachable2()
	{
		assertFalse(WMethod.checkUnreachableStates(WMethod.getGraphData(buildGraph("A-a->A-c->C\nB-a->A\nC-b->B", "testCheckUnreachable2"))));	
	}
	
	@Test
	public final void testCheckUnreachable3()
	{
		assertTrue(WMethod.checkUnreachableStates(WMethod.getGraphData(buildGraph("A-a->A-c->C\nB-a->A", "testCheckUnreachable3"))));	
	}
	
	
	@Test 
	public final void testBucketCollection0()
	{
		HashBucketPrefixFreeCollection c = new HashBucketPrefixFreeCollection();
		Assert.assertEquals(0, c.getData().size());
	}
	
	@Test 
	public final void testBucketCollection1()
	{
		HashBucketPrefixFreeCollection c = new HashBucketPrefixFreeCollection();
		c.addSequence(new LinkedList<String>());
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testBucketCollection2()
	{
		HashBucketPrefixFreeCollection c = new HashBucketPrefixFreeCollection();
		c.addSequence(new LinkedList<String>());
		c.addSequence(new LinkedList<String>());
		c.addSequence(new LinkedList<String>());
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testBucketCollection3()
	{
		HashBucketPrefixFreeCollection c = new HashBucketPrefixFreeCollection();
		c.addSequence(new LinkedList<String>());
		c.addSequence(Arrays.asList(new String[]{"a"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a"}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testBucketCollection4()
	{
		HashBucketPrefixFreeCollection c = new HashBucketPrefixFreeCollection();
		c.addSequence(new LinkedList<String>());
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{"a","a"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","a"}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testBucketCollection5()
	{
		HashBucketPrefixFreeCollection c = new HashBucketPrefixFreeCollection();
		c.addSequence(new LinkedList<String>());
		c.addSequence(Arrays.asList(new String[]{"a","a"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","a"}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testBucketCollection6()
	{
		HashBucketPrefixFreeCollection c = new HashBucketPrefixFreeCollection();
		c.addSequence(new LinkedList<String>());
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{}));
		c.addSequence(Arrays.asList(new String[]{"a","a"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","a"}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testBucketCollection7()
	{
		HashBucketPrefixFreeCollection c = new HashBucketPrefixFreeCollection();
		c.addSequence(new LinkedList<String>());
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{}));
		c.addSequence(Arrays.asList(new String[]{"a","b"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testBucketCollection8()
	{
		HashBucketPrefixFreeCollection c = new HashBucketPrefixFreeCollection();
		c.addSequence(new LinkedList<String>());
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{}));
		c.addSequence(Arrays.asList(new String[]{"a","b"}));
		c.addSequence(Arrays.asList(new String[]{"a","c","d"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
				new String[]{"a","c","d"}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(2, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	/** Holds the JFrame to see the graphs being dealt with. Usage:
	 * <pre>
	 * 		updateFrame(g);// a public method
	 * </pre>
	 * where <i>g</i> is the graph to be displayed.
	 */
	protected static Visualiser visFrame = null;
	
	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{
		visFrame = null;
	}

	@AfterClass
	public static void cleanUp()
	{
		try {
			if (visFrame != null)
			{
				SwingUtilities.invokeAndWait(new Runnable() 
				{
					public void run()
					{
							visFrame.setVisible(false);
							visFrame.dispose();
					}
				});
			}
		} catch (InterruptedException e) {
			// cannot do anything with this
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// cannot do anything with this
			e.printStackTrace();
		}
	}
	
	/** In order to be able to use old junit runner. */
	public static junit.framework.Test suite()
	{
		return new JUnit4TestAdapter(TestWMethod.class);
	}
}
