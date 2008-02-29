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

import static org.junit.Assert.*;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
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
import statechum.Pair;
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

	public static void testWsetconstruction(String machine, boolean equivalentExpected, boolean reductionExpected)
	{
		DirectedSparseGraph g = buildGraph(machine,"testWset");
		FSMStructure fsm = getGraphData(g);//visFrame.update(null, g);
		Set<List<String>> origWset = new HashSet<List<String>>(); 
		try
		{
			origWset.addAll(WMethod.computeWSetOrig(fsm));
			Assert.assertEquals(false, equivalentExpected);
			checkW_is_corrent(fsm, origWset);			
		}
		catch(EquivalentStatesException e)
		{
			Assert.assertEquals(true, equivalentExpected);
			TestFSMAlgo.checkM(fsm,fsm,e.getA(),e.getB());
		}
		
		try
		{
			Set<List<String>> wset = new HashSet<List<String>>();wset.addAll(WMethod.computeWSet(fsm));
			Assert.assertEquals(false, equivalentExpected);
			checkW_is_corrent(fsm, wset);
			int reduction = origWset.size() - wset.size();
			Assert.assertTrue(reduction >= 0 || !reductionExpected);
		}
		catch(EquivalentStatesException e)
		{
			Assert.assertEquals(true, equivalentExpected);
			TestFSMAlgo.checkM(fsm,fsm,e.getA(),e.getB());
		}
	}

	/** Given an FSM and a W set, checks if it is a valid W set and throws if not.
	 * 
	 * @param fsm the machine
	 * @param wset the set to check validity of.
	 */
	private static void checkW_is_corrent(FSMStructure fsm, Collection<List<String>> wset)
	{
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
	
	
	@Test
	public final void testWset1()
	{
		testWsetconstruction("A-p->A-b->B-c->B-a->C",false,true);
	}
	
	@Test
	public final void testWset2()
	{
		testWsetconstruction("A-a->C-b->Q\nB-a->D-a->Q",false,true);
	}
	
	@Test
	public final void testWset3() // equivalent states
	{
		testWsetconstruction("A-a->C-b->Q\nB-a->D-b->Q",true,true);
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
				true,true);
	}
	
	@Test
	public final void testWset5b() // equivalent states
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3",
				true,true);
	}
	
	@Test
	public final void testWset6() // no equivalent states
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-m->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-z->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3",
				false,true);
	}

	@Test
	public final void testWset7()
	{
		testWsetconstruction("A-a->B-a->C-a->A-b->C-b->B",false,true);
	}
	
	@Test
	public final void testWset8()
	{
		testWsetconstruction("S-a->A-a->D-a->D-b->A-b->B-a->D\nB-b->C-a->D\nC-b->D\nS-b->N-a->M-a->N\nN-b->M-b->N",true,true);
	}
	
	@Test
	public final void testWset9()
	{
		testWsetconstruction("A-a->D\nB-a->C\nA-b->B\nD-b->C",false,true);
	}
	
	@Test
	public final void testWset10()
	{
		String machineOrig = "0--a4->0--a2->0--a5->2\n0--a7->4\n0--a9->3\n0--a0->1\n1--a5->0\n1--a3->0\n4--a1->0\n4--a8->3\n3--a4->1\n3--a6->2\n2--a8->4\n2--a4->0\n3--a9->0",
			machine = null;
		int a = 4, b = 2;
		machine = machineOrig.replaceAll(a+"--", "Q"+"--").replaceAll(">"+a, ">"+"Q")
			.replaceAll(b+"--", a+"--").replaceAll(">"+b, ">"+a)
			.replaceAll("Q"+"--", b+"--").replaceAll(">"+"Q", ">"+b);
		testWsetconstruction(machine,false,true);
	}

	@Test
	public final void testWset11()
	{
		testWsetconstruction("0-a0->1\n0-a1->9\n0-a2->6\n0-a3->1\n0-a5->0\n0-a7->7\n0-a10->7\n0-a12->5\n1-a0->9\n1-a1->5\n1-a3->3\n1-a6->3\n1-a8->7\n1-a14->9\n1-a17->9\n1-a18->6\n2-a0->8\n2-a2->8\n2-a3->6\n2-a4->4\n2-a7->3\n2-a9->2\n2-a10->4\n2-a15->5\n3-a0->5\n3-a1->2\n3-a2->2\n3-a7->3\n3-a9->8\n3-a10->0\n3-a15->6\n3-a16->5\n4-a0->8\n4-a4->8\n4-a5->0\n4-a7->4\n4-a11->0\n4-a12->3\n4-a16->0\n4-a19->5\n5-a0->1\n5-a2->1\n5-a5->6\n5-a6->2\n5-a7->9\n5-a9->0\n5-a11->3\n5-a19->5\n6-a0->1\n6-a2->4\n6-a4->7\n6-a9->8\n6-a10->0\n6-a12->1\n6-a18->1\n6-a19->3\n7-a1->6\n7-a5->4\n7-a7->9\n7-a10->9\n7-a12->7\n7-a13->4\n7-a14->6\n7-a15->9\n8-a2->7\n8-a4->1\n8-a5->6\n8-a6->4\n8-a9->0\n8-a11->2\n8-a13->2\n8-a14->7\n9-a2->7\n9-a3->3\n9-a5->4\n9-a6->2\n9-a9->5\n9-a11->2\n9-a16->8\n9-a17->8\n",false,true);
	}

	@Test
	public final void testWset12()
	{
		testWsetconstruction("0-a0->1\n0-a1->8\n0-a2->7\n0-a7->3\n0-a9->3\n0-a11->8\n0-a12->6\n0-a15->0\n1-a2->0\n1-a4->6\n1-a6->4\n1-a12->5\n1-a13->5\n1-a16->8\n1-a18->6\n1-a19->2\n2-a2->2\n2-a5->7\n2-a8->0\n2-a10->8\n2-a12->8\n2-a13->1\n2-a14->5\n2-a16->8\n3-a3->3\n3-a6->2\n3-a8->7\n3-a10->4\n3-a11->6\n3-a14->9\n3-a15->3\n3-a16->7\n4-a0->4\n4-a3->1\n4-a5->6\n4-a6->7\n4-a10->7\n4-a12->3\n4-a17->4\n4-a18->4\n5-a0->0\n5-a6->3\n5-a7->0\n5-a11->0\n5-a14->4\n5-a16->3\n5-a17->3\n5-a18->4\n6-a0->6\n6-a2->2\n6-a4->1\n6-a10->9\n6-a11->2\n6-a12->1\n6-a17->5\n6-a19->9\n7-a1->5\n7-a2->9\n7-a3->5\n7-a5->1\n7-a7->2\n7-a10->1\n7-a11->0\n7-a16->9\n8-a3->9\n8-a4->9\n8-a5->6\n8-a6->8\n8-a7->6\n8-a12->8\n8-a17->5\n8-a18->9\n9-a1->7\n9-a5->5\n9-a9->1\n9-a10->7\n9-a15->2\n9-a17->0\n9-a18->2\n9-a19->4\n",false,true);
	}

	@Test
	public final void testWset13()
	{
		testWsetconstruction("0-a0->1\n0-a1->9\n0-a2->5\n0-a4->9\n0-a5->5\n0-a10->9\n0-a11->7\n0-a12->9\n0-a15->8\n0-a16->0\n0-a17->3\n0-a18->8\n1-a0->7\n1-a2->0\n1-a3->2\n1-a4->7\n1-a5->6\n1-a7->6\n1-a8->2\n1-a11->0\n1-a15->0\n1-a17->4\n1-a18->1\n1-a19->4\n2-a0->5\n2-a4->7\n2-a5->0\n2-a6->1\n2-a8->9\n2-a10->9\n2-a11->6\n2-a12->2\n2-a13->6\n2-a15->3\n2-a16->1\n2-a17->0\n3-a1->8\n3-a3->3\n3-a4->5\n3-a6->4\n3-a7->6\n3-a9->1\n3-a10->4\n3-a11->3\n3-a15->6\n3-a16->6\n3-a17->5\n3-a19->6\n4-a0->4\n4-a1->0\n4-a2->5\n4-a3->3\n4-a6->2\n4-a7->2\n4-a8->8\n4-a9->0\n4-a10->5\n4-a16->2\n4-a17->5\n4-a19->4\n5-a0->9\n5-a2->6\n5-a5->1\n5-a7->5\n5-a8->4\n5-a9->2\n5-a11->4\n5-a12->7\n5-a15->7\n5-a17->1\n5-a18->7\n5-a19->7\n6-a4->4\n6-a6->7\n6-a7->9\n6-a9->2\n6-a10->8\n6-a12->3\n6-a13->7\n6-a14->1\n6-a15->8\n6-a16->0\n6-a17->1\n6-a18->2\n7-a1->6\n7-a3->1\n7-a5->2\n7-a6->0\n7-a7->6\n7-a8->3\n7-a9->0\n7-a10->5\n7-a11->4\n7-a15->8\n7-a17->8\n7-a19->2\n8-a0->7\n8-a1->3\n8-a3->9\n8-a4->7\n8-a5->6\n8-a6->1\n8-a8->3\n8-a9->0\n8-a10->3\n8-a11->9\n8-a14->8\n8-a18->4\n9-a0->4\n9-a1->9\n9-a5->8\n9-a6->5\n9-a7->3\n9-a9->9\n9-a10->3\n9-a13->8\n9-a14->5\n9-a15->1\n9-a16->8\n9-a17->2\n",false,false);
	}

	public interface FsmPermutator {
		/** Returns an array representing an order in which elements of an FSM should be placed in a string. */
		Object [] getPermutation(Object [] from);
	}
	
	public class EmptyPermutator implements FsmPermutator {
		/** Returns an array representing an order in which elements of an FSM should be placed in a string. */
		public Object [] getPermutation(Object [] from)
		{
			Object [] result = new Object [from.length];
			System.arraycopy(from, 0, result, 0, from.length);
			
			return result;
		}
	}

	public class RandomPermutator implements FsmPermutator {
		private RandomPermutator()
		{}
		
		private Random rnd = null;
		
		public RandomPermutator(int randomArg)
		{
			rnd = new Random(randomArg);
		}
		/** Returns an array representing an order in which elements of an FSM should be placed in a string. */
		public Object [] getPermutation(Object [] from)
		{
			Object [] result = new Object [from.length];
			System.arraycopy(from, 0, result, 0, from.length);
			
			for(int i=0;i< from.length;++i)
			{
				int first = rnd.nextInt(from.length), second = rnd.nextInt(from.length);
				Object firstObj = result[first];result[first]=result[second];result[second]=firstObj;
			}
			return result;
		}
	}
	
	public static void testWsetDeterministic(String machine, FsmPermutator perm, String testName)
	{
		DirectedSparseGraph g = buildGraph(machine,"testDeterminism_"+testName);
		FSMStructure fsm = getGraphData(g);//visFrame.update(null, g);
		Set<List<String>> origWset = new HashSet<List<String>>();origWset.addAll(WMethod.computeWSet(fsm));
		checkW_is_corrent(fsm, origWset);

		List<Pair<String,String>> transitionList = new LinkedList<Pair<String,String>>();
		for(Entry<String,Map<String,String>> row:fsm.trans.entrySet())
			for(Entry<String,String> nextState:row.getValue().entrySet())
				transitionList.add(new Pair<String,String>(row.getKey(),nextState.getKey()));
		
		Object [] permutation = perm.getPermutation(transitionList.toArray());
		Assert.assertEquals(transitionList.size(), permutation.length);
		StringBuffer newFsm = new StringBuffer();
		for(int i=0;i<permutation.length;++i)
		{
			Pair<String,String> p = (Pair<String,String>)permutation[i];
			String from = p.firstElem, label = p.secondElem;
			newFsm.append("\n"+from+"-"+label+"->"+fsm.trans.get(from).get(label));
		}
		FSMStructure permFsm = getGraphData(buildGraph(newFsm.toString(), "testDeterminism_"+testName+"_perm"));
		permFsm.init = fsm.init;
		TestFSMAlgo.checkM(fsm,permFsm,fsm.init,permFsm.init);
		
		Set<List<String>> newWset = new HashSet<List<String>>();newWset.addAll(WMethod.computeWSet(permFsm));
		checkW_is_corrent(fsm, newWset);
		
		Assert.assertTrue(origWset.equals(newWset));
	}

	@Test
	public final void testDeterminism()
	{
		String machine = "0-a0->1\n0-a1->9\n0-a2->6\n0-a3->1\n0-a5->0\n0-a7->7\n0-a10->7\n0-a12->5\n1-a0->9\n1-a1->5\n1-a3->3\n1-a6->3\n1-a8->7\n1-a14->9\n1-a17->9\n1-a18->6\n2-a0->8\n2-a2->8\n2-a3->6\n2-a4->4\n2-a7->3\n2-a9->2\n2-a10->4\n2-a15->5\n3-a0->5\n3-a1->2\n3-a2->2\n3-a7->3\n3-a9->8\n3-a10->0\n3-a15->6\n3-a16->5\n4-a0->8\n4-a4->8\n4-a5->0\n4-a7->4\n4-a11->0\n4-a12->3\n4-a16->0\n4-a19->5\n5-a0->1\n5-a2->1\n5-a5->6\n5-a6->2\n5-a7->9\n5-a9->0\n5-a11->3\n5-a19->5\n6-a0->1\n6-a2->4\n6-a4->7\n6-a9->8\n6-a10->0\n6-a12->1\n6-a18->1\n6-a19->3\n7-a1->6\n7-a5->4\n7-a7->9\n7-a10->9\n7-a12->7\n7-a13->4\n7-a14->6\n7-a15->9\n8-a2->7\n8-a4->1\n8-a5->6\n8-a6->4\n8-a9->0\n8-a11->2\n8-a13->2\n8-a14->7\n9-a2->7\n9-a3->3\n9-a5->4\n9-a6->2\n9-a9->5\n9-a11->2\n9-a16->8\n9-a17->8\n";
		testWsetDeterministic(machine, new EmptyPermutator(), "testDeterminism1_empty");
		for(int i=0;i<100;++i)
			testWsetDeterministic(machine, new RandomPermutator(i), "testDeterminism1_random");
	}
	
	
	/** Adds an entry to the supplied map of pairs of states to labels which distinguish among those states.
	 * 
	 * @param distinguishingLabels map of pairs of states to labels which distinguish among those states
	 * @param stateA first state
	 * @param stateB second state
	 * @param labels labels to distinguish between states.
	 */
	private final static void addDistLabels(Map<String,Map<String,Set<String>>> distinguishingLabels, String stateA, String stateB, String [] labels)
	{
		Set<String> distLabels = new HashSet<String>();Map<String,Set<String>> stateToDist = new HashMap<String,Set<String>>();
		distLabels.clear();distLabels.addAll(Arrays.asList(labels));stateToDist.put(stateB, distLabels);
		distinguishingLabels.put(stateA,stateToDist);		
	}
	
	@Test
	public final void testComputeTopLabe0()
	{
		Map<String,Map<String,Set<String>>> distinguishingLabels = new HashMap<String,Map<String,Set<String>>>();
		Assert.assertNull(WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabel1()
	{
		Map<String,Map<String,Set<String>>> distinguishingLabels = new HashMap<String,Map<String,Set<String>>>();
		Assert.assertNull(WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabe2()
	{
		Map<String,Map<String,Set<String>>> distinguishingLabels = new HashMap<String,Map<String,Set<String>>>();
		addDistLabels(distinguishingLabels, "0","1",new String[] {"a"});
		Assert.assertEquals("a",WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabe3()
	{
		Map<String,Map<String,Set<String>>> distinguishingLabels = new HashMap<String,Map<String,Set<String>>>();
		addDistLabels(distinguishingLabels, "0","1",new String[] {"a"});
		addDistLabels(distinguishingLabels, "0","2",new String[] {"a"});
		Assert.assertEquals("a",WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabe4()
	{
		Map<String,Map<String,Set<String>>> distinguishingLabels = new HashMap<String,Map<String,Set<String>>>();
		addDistLabels(distinguishingLabels, "1","2",new String[] {"a"});
		addDistLabels(distinguishingLabels, "0","1",new String[] {"b"});
		addDistLabels(distinguishingLabels, "3","1",new String[] {"b"});
		Assert.assertEquals("b",WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabe5()
	{
		Map<String,Map<String,Set<String>>> distinguishingLabels = new HashMap<String,Map<String,Set<String>>>();
		addDistLabels(distinguishingLabels, "1","2",new String[] {"a"});
		addDistLabels(distinguishingLabels, "0","1",new String[] {"b"});
		addDistLabels(distinguishingLabels, "3","1",new String[] {"b"});
		addDistLabels(distinguishingLabels, "4","1",new String[] {});// empty entry
		Assert.assertEquals("b",WMethod.computeTopLabel(distinguishingLabels));
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
		assertTrue("old test generator failure, received "+actualA+" expected "+expectedSet,expectedSet.equals(actualA));
		assertTrue("new test generator failure, received "+actualB+" expected "+expectedSet,expectedSet.equals(actualB));
	}
	
	@Test
	public final void testTestGeneration1()
	{
		DirectedSparseGraph g = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum(JUConstants.LABEL, "A", UserData.SHARED);
		init.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
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
				"A-d->A\nA5-b->AA6", "testTestGeneration7"),2);
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
			
	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{
		Visualiser.disposeFrame();
	}

	@AfterClass
	public static void cleanUp()
	{
		Visualiser.disposeFrame();
	}
	
	/** In order to be able to use old junit runner. */
	public static junit.framework.Test suite()
	{
		return new JUnit4TestAdapter(TestWMethod.class);
	}
}
