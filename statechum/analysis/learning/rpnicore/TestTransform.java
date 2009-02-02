/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
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
import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import statechum.StatechumXML;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import edu.uci.ics.jung.exceptions.FatalException;

import statechum.ArrayOperations;
import statechum.Configuration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.DeterministicDirectedSparseGraph.CmpVertex.IllegalUserDataException;
import statechum.DeterministicDirectedSparseGraph.VertexID.VertKind;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.TestRpniLearner;
import statechum.analysis.learning.observers.TestWriteReadPair;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.GD.ChangesCounter;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildGraph;
import static statechum.analysis.learning.rpnicore.Transform.HammingDistance;

public class TestTransform {
	@Test(expected=IllegalArgumentException.class)
	public final void testHammingDistance0()
	{
		HammingDistance(
				Arrays.asList(new Boolean[]{true}), Arrays.asList(new Boolean[]{}));
	}

	@Test 
	public final void testHammingDistance1()
	{
		Assert.assertEquals(0, HammingDistance(
				Arrays.asList(new Boolean[]{}),
				Arrays.asList(new Boolean[]{})
		));
	}
	
	@Test 
	public final void testHammingDistance2()
	{
		Assert.assertEquals(0, HammingDistance(
				Arrays.asList(new Boolean[]{false}),
				Arrays.asList(new Boolean[]{false})
		));
	}
	
	@Test 
	public final void testHammingDistance3()
	{
		Assert.assertEquals(1, HammingDistance(
				Arrays.asList(new Boolean[]{false}),
				Arrays.asList(new Boolean[]{true})
		));
	}
	
	@Test 
	public final void testHammingDistance4()
	{
		Assert.assertEquals(1, HammingDistance(
				Arrays.asList(new Boolean[]{true,false,false}),
				Arrays.asList(new Boolean[]{true,true,false})
		));
	}
	
	@Test 
	public final void testHammingDistance5()
	{
		Assert.assertEquals(3, HammingDistance(
				Arrays.asList(new Boolean[]{true,true,false}),
				Arrays.asList(new Boolean[]{false,false,true})
		));
	}

	private LearnerGraph g = new LearnerGraph(buildGraph("A-a->A-b->B",	"testToBooleans"),Configuration.getDefaultConfiguration());
	private StringBuffer resultDescr = new StringBuffer();	
	private List<List<String>> buildListList(String [][]list_of_seq)
	{
		List<List<String>> result = new LinkedList<List<String>>();
		for(String []seq:list_of_seq)
			result.add(Arrays.asList(seq));
		return result;
	}
	
	@Test
	public final void testToBooleans0()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{},
				Transform.wToBooleans(g,g.findVertex("A"),
				buildListList(new String [][]{})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}
	
	@Test
	public final void testToBooleans1()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{true,true},
				Transform.wToBooleans(g,g.findVertex("A"),
				buildListList(new String [][]{
						new String[] {"a"},
						new String[] {"b"}
				})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}

	@Test
	public final void testToBooleans2()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{true,false},
				Transform.wToBooleans(g,g.findVertex("A"),
				buildListList(new String [][]{
						new String[] {"a"},
						new String[] {"c"}
				})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}

	@Test
	public final void testToBooleans3()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{true,false,false,true},
				Transform.wToBooleans(g,g.findVertex("A"),
				buildListList(new String [][]{
						new String[] {"a"},
						new String[] {"b","b"},
						new String[] {"a","a","a","c"},
						new String[] {"a","a","a","b"},
				})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}

	@Test
	public final void testToBooleans4()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{false,false,false,false},
				Transform.wToBooleans(g,g.findVertex("B"),
				buildListList(new String [][]{
						new String[] {"a"},
						new String[] {"b","b"},
						new String[] {"a","a","a","c"},
						new String[] {"a","a","a","b"},
				})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}
	
	private final String relabelFSM = "A-a->B-a->C-b->B-c->B";
	
	@Test(expected=IllegalArgumentException.class)
	public final void testRelabel_fail1()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		AbstractPathRoutines.relabel(fsm,-1,"c");
	}
	
	@Test
	public final void testRelabel0_1()
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.pathroutines.computeAlphabet();Assert.assertTrue(origAlphabet.isEmpty());
		AbstractPathRoutines.relabel(fsm,-1,"new");Set<String> newAlphabet = fsm.pathroutines.computeAlphabet();Assert.assertTrue(newAlphabet.isEmpty());
	}
	
	@Test
	public final void testRelabel0_2()
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.pathroutines.computeAlphabet();Assert.assertTrue(origAlphabet.isEmpty());
		AbstractPathRoutines.relabel(fsm,0,"new");Set<String> newAlphabet = fsm.pathroutines.computeAlphabet();Assert.assertTrue(newAlphabet.isEmpty());
	}
	@Test
	public final void testRelabel0_3()
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.pathroutines.computeAlphabet();Assert.assertTrue(origAlphabet.isEmpty());
		AbstractPathRoutines.relabel(fsm,1,"new");Set<String> newAlphabet = fsm.pathroutines.computeAlphabet();Assert.assertTrue(newAlphabet.isEmpty());
	}
	
	@Test
	public final void testRelabel1_1()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,-1,"new");Set<String> newAlphabet = fsm.pathroutines.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<String> diffOrigNew = new TreeSet<String>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertTrue(diffOrigNew.isEmpty());		
	}

	@Test
	public final void testRelabel1_2()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,0,"new");Set<String> newAlphabet = fsm.pathroutines.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<String> diffOrigNew = new TreeSet<String>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertTrue(diffOrigNew.isEmpty());		
	}

	@Test
	public final void testRelabel2()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,1,"new");Set<String> newAlphabet = fsm.pathroutines.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<String> diffOrigNew = new TreeSet<String>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertEquals(1,diffOrigNew.size());		
	}

	@Test
	public final void testRelabel3()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,2,"new");Set<String> newAlphabet = fsm.pathroutines.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<String> diffOrigNew = new TreeSet<String>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertEquals(2,diffOrigNew.size());		
	}


	@Test
	public final void testRelabel4()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,origAlphabet.size(),"new");Set<String> newAlphabet = fsm.pathroutines.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Assert.assertTrue(newAlphabet.equals(origAlphabet));		
	}
	
	public static final String relabelFSM_ND = "A-a->B-a->C-b->B-c->B\nA-a->C";
	
	@Test
	public final void testRelabel3_ND()
	{
		LearnerGraphND fsm = new LearnerGraphND(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,2,"new");Set<String> newAlphabet = fsm.pathroutines.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<String> diffOrigNew = new TreeSet<String>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertEquals(2,diffOrigNew.size());		
	}


	@Test
	public final void testRelabel4_ND()
	{
		LearnerGraphND fsm = new LearnerGraphND(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		Set<String> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,origAlphabet.size(),"new");Set<String> newAlphabet = fsm.pathroutines.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Assert.assertTrue(newAlphabet.equals(origAlphabet));		
	}

	/** Tests that graph relabelling works correctly on an empty graph. */
	@Test
	public final void testStateRelabelling0()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraphND A = new LearnerGraphND(config);A.initEmpty();
		Map<CmpVertex,CmpVertex> whatToG = new TreeMap<CmpVertex,CmpVertex>();
		whatToG.put(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),config),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("T"),config));
		whatToG.put(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),config),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("U"), config));
		LearnerGraphND actual = new LearnerGraphND(config);actual.initEmpty();
		AbstractLearnerGraph.addAndRelabelGraphs(A, whatToG, actual);
		Assert.assertTrue(actual.pairCompatibility.isEmpty());
		Assert.assertTrue(actual.transitionMatrix.isEmpty());
	}
	
	/** Tests that graph relabelling works correctly. */
	@Test
	public final void testStateRelabelling1()
	{
		LearnerGraphND A = new LearnerGraphND(TestFSMAlgo.buildGraph("A-a->B\nA-a->C\nB-a->D\nB-a->A", "testStateRelabelling1"),Configuration.getDefaultConfiguration()),
			expected = new LearnerGraphND(TestFSMAlgo.buildGraph("T-a->U\nT-a->R\nU-a->S\nU-a->T", "testStateRelabelling1"),Configuration.getDefaultConfiguration());
		A.addToCompatibility(A.findVertex("B"), A.findVertex("D"),JUConstants.INCOMPATIBLE);
		A.addToCompatibility(A.findVertex("D"), A.findVertex("C"),JUConstants.INCOMPATIBLE);
		expected.addToCompatibility(expected.findVertex("U"), expected.findVertex("S"),JUConstants.INCOMPATIBLE);
		expected.addToCompatibility(expected.findVertex("S"), expected.findVertex("R"),JUConstants.INCOMPATIBLE);
		Map<CmpVertex,CmpVertex> whatToG = new TreeMap<CmpVertex,CmpVertex>();
		whatToG.put(A.findVertex("A"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("T"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("B"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("U"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("C"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("R"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("D"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("S"), Configuration.getDefaultConfiguration()));
		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		AbstractLearnerGraph.addAndRelabelGraphs(A, whatToG, actual);actual.init = actual.findVertex("T");
		Assert.assertNull(WMethod.checkM_and_colours(expected, actual, VERTEX_COMPARISON_KIND.DEEP));		
	}
	
	/** Tests that graph relabelling works correctly, this one with some compatible and incompatible states. */
	@Test
	public final void testStateRelabelling2()
	{
		LearnerGraphND A = new LearnerGraphND(TestFSMAlgo.buildGraph("A-a->B\nA-a->C\nB-a->D\nB-a->A", "testStateRelabelling1"),Configuration.getDefaultConfiguration()),
			expected = new LearnerGraphND(TestFSMAlgo.buildGraph("T-a->U\nT-a->R\nU-a->S\nU-a->T", "testStateRelabelling1"),Configuration.getDefaultConfiguration());
		A.addToCompatibility(A.findVertex("B"), A.findVertex("D"),JUConstants.INCOMPATIBLE);
		A.addToCompatibility(A.findVertex("D"), A.findVertex("C"),JUConstants.MERGED);
		A.addToCompatibility(A.findVertex("D"), A.findVertex("A"),JUConstants.MERGED);
		expected.addToCompatibility(expected.findVertex("U"), expected.findVertex("S"),JUConstants.INCOMPATIBLE);
		expected.addToCompatibility(expected.findVertex("S"), expected.findVertex("R"),JUConstants.MERGED);
		expected.addToCompatibility(expected.findVertex("S"), expected.findVertex("T"),JUConstants.MERGED);
		Map<CmpVertex,CmpVertex> whatToG = new TreeMap<CmpVertex,CmpVertex>();
		whatToG.put(A.findVertex("A"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("T"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("B"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("U"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("C"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("R"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("D"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("S"), Configuration.getDefaultConfiguration()));
		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		AbstractLearnerGraph.addAndRelabelGraphs(A, whatToG, actual);actual.init = actual.findVertex("T");
		Assert.assertNull(WMethod.checkM_and_colours(expected, actual, VERTEX_COMPARISON_KIND.DEEP));		
	}
	
	@Test
	public final void testAddToGraph0_1() throws IncompatibleStatesException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = new LearnerGraph(Configuration.getDefaultConfiguration());
		helperAddToGraph0_1(fsm, fsmSrc,fsmToAdd);
	}
	
	@Test
	public final void testAddToGraph0_1_ND() throws IncompatibleStatesException
	{
		LearnerGraphND fsm = new LearnerGraphND(TestFSMAlgo.buildGraph(relabelFSM_ND, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraphND fsmSrc = new LearnerGraphND(TestFSMAlgo.buildGraph(relabelFSM_ND, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraphND fsmToAdd = new LearnerGraphND(Configuration.getDefaultConfiguration());
		helperAddToGraph0_1(fsm, fsmSrc,fsmToAdd);
	}

	private final <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
		void helperAddToGraph0_1(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsm,
				AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmSrc,
				AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmToAdd) throws IncompatibleStatesException
	{
		fsmToAdd.init.setColour(JUConstants.BLUE);fsmToAdd.init.setHighlight(true);
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = AbstractPathRoutines.addToGraph(fsm, fsmToAdd,oldToNew);
		Assert.assertNull(WMethod.checkM(fsmSrc, fsm));Assert.assertTrue(fsm.pairCompatibility.isEmpty());
		LearnerGraph addedPartOfFsm = fsm.pathroutines.buildDeterministicGraph(newA);
		Assert.assertNull(WMethod.checkM(fsmToAdd,addedPartOfFsm));
		Assert.assertEquals(JUConstants.BLUE, newA.getColour());
		Assert.assertTrue(newA.isHighlight());
		Assert.assertEquals(1, oldToNew.size());Assert.assertSame(newA, oldToNew.get(fsmToAdd.init));
		
		Assert.assertFalse(fsm.transitionMatrix.get(fsm.init).isEmpty());
		Assert.assertTrue(fsm.transitionMatrix.get(newA).isEmpty());
	}

	@Test
	public final void testAddToGraph0_2() throws IncompatibleStatesException
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		helperAddToGraph0_2(fsm, fsmSrc,fsmToAdd);
	}
	
	@Test
	public final void testAddToGraph0_2_ND() throws IncompatibleStatesException
	{
		LearnerGraphND fsm = new LearnerGraphND(Configuration.getDefaultConfiguration());
		LearnerGraphND fsmSrc = new LearnerGraphND(Configuration.getDefaultConfiguration());
		LearnerGraphND fsmToAdd = new LearnerGraphND(TestFSMAlgo.buildGraph(relabelFSM_ND, "testRelabel1"),Configuration.getDefaultConfiguration());
		helperAddToGraph0_2(fsm, fsmSrc,fsmToAdd);
	}
	
	private final <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
	void helperAddToGraph0_2(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsm,
			AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmSrc,
			AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmToAdd) throws IncompatibleStatesException
	{
		fsmToAdd.init.setColour(JUConstants.BLUE);fsmToAdd.init.setHighlight(true);
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = AbstractPathRoutines.addToGraph(fsm, fsmToAdd,oldToNew);
		
		Assert.assertNull(WMethod.checkM(fsmSrc, fsm));Assert.assertTrue(fsm.pairCompatibility.isEmpty());
		LearnerGraph addedPartOfFsm = fsm.pathroutines.buildDeterministicGraph(newA);
		Assert.assertNull(WMethod.checkM(fsmToAdd,addedPartOfFsm));
		Assert.assertTrue(fsm.transitionMatrix.get(fsm.init).isEmpty());
		Assert.assertFalse(fsm.transitionMatrix.get(newA).isEmpty());

		Assert.assertEquals(fsmToAdd.getStateNumber(), oldToNew.size());
		Assert.assertSame(newA, oldToNew.get(fsmToAdd.init));
		for(CmpVertex oldVert:fsmToAdd.transitionMatrix.keySet())
			Assert.assertNull(WMethod.checkM(fsm,oldToNew.get(oldVert),fsmToAdd,oldVert,WMethod.VERTEX_COMPARISON_KIND.NONE));
	}

	@Test
	public final void testAddToGraph0_3() throws IncompatibleStatesException
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = new LearnerGraph(Configuration.getDefaultConfiguration());
		helperAddToGraph0_3(fsm, fsmSrc,fsmToAdd);
	}

	@Test
	public final void testAddToGraph0_3_ND() throws IncompatibleStatesException
	{
		LearnerGraphND fsm = new LearnerGraphND(Configuration.getDefaultConfiguration());
		LearnerGraphND fsmSrc = new LearnerGraphND(Configuration.getDefaultConfiguration());
		LearnerGraphND fsmToAdd = new LearnerGraphND(Configuration.getDefaultConfiguration());
		helperAddToGraph0_3(fsm, fsmSrc,fsmToAdd);
	}

	private final <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
	void helperAddToGraph0_3(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsm,
			AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmSrc,
			AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmToAdd) throws IncompatibleStatesException
	{
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = AbstractPathRoutines.addToGraph(fsm, fsmToAdd,oldToNew);

		Assert.assertNull(WMethod.checkM(fsmSrc, fsm));Assert.assertTrue(fsm.pairCompatibility.isEmpty());
		LearnerGraph addedPartOfFsm = fsm.pathroutines.buildDeterministicGraph(newA);
		Assert.assertNull(WMethod.checkM(fsmToAdd,addedPartOfFsm));
		
		Assert.assertTrue(fsm.transitionMatrix.get(fsm.init).isEmpty());
		Assert.assertTrue(fsm.transitionMatrix.get(newA).isEmpty());

		Assert.assertEquals(fsmToAdd.getStateNumber(), oldToNew.size());
		Assert.assertSame(newA, oldToNew.get(fsmToAdd.init));
		for(CmpVertex oldVert:fsmToAdd.transitionMatrix.keySet())
			Assert.assertNull(WMethod.checkM(fsm,oldToNew.get(oldVert),fsmSrc,oldVert,WMethod.VERTEX_COMPARISON_KIND.NONE));
	}

	@Test
	public final void testAddToGraph1() throws IncompatibleStatesException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a-#Q", "testAddToGraph1"),Configuration.getDefaultConfiguration());
		helperAddToGraph1(fsm, fsmSrc,fsmToAdd);
	}
	
	@Test
	public final void testAddToGraph1_ND() throws IncompatibleStatesException
	{
		LearnerGraphND fsm = new LearnerGraphND(TestFSMAlgo.buildGraph(relabelFSM_ND, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraphND fsmSrc = new LearnerGraphND(TestFSMAlgo.buildGraph(relabelFSM_ND, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraphND fsmToAdd = new LearnerGraphND(TestFSMAlgo.buildGraph("A-a->B-a-#Q", "testAddToGraph1"),Configuration.getDefaultConfiguration());
		helperAddToGraph1(fsm, fsmSrc,fsmToAdd);
	}
	
	private final <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
	void helperAddToGraph1(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsm,
			AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmSrc,
			AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmToAdd) throws IncompatibleStatesException
	{
		fsmToAdd.init.setColour(JUConstants.BLUE);fsmToAdd.init.setHighlight(true);
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = AbstractPathRoutines.addToGraph(fsm, fsmToAdd,oldToNew);

		Assert.assertNull(WMethod.checkM(fsmSrc, fsm));
		LearnerGraph addedPartOfFsm = fsm.pathroutines.buildDeterministicGraph(newA);
		Assert.assertNull(WMethod.checkM(fsmToAdd,addedPartOfFsm));
		Assert.assertEquals(JUConstants.BLUE, newA.getColour());
		Assert.assertTrue(newA.isHighlight());

		Assert.assertEquals(fsmToAdd.getStateNumber(), oldToNew.size());
		Assert.assertSame(newA, oldToNew.get(fsmToAdd.init));
		for(CmpVertex oldVert:fsmToAdd.transitionMatrix.keySet())
			Assert.assertNull(WMethod.checkM(fsm,oldToNew.get(oldVert),fsmToAdd,oldVert,WMethod.VERTEX_COMPARISON_KIND.NONE));
	}
	
	@Test
	public final void testAddToGraph2()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->Q", "testAddToGraph1"),Configuration.getDefaultConfiguration());

		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = AbstractPathRoutines.addToGraph(fsm, fsmToAdd,oldToNew);

		Assert.assertNull(WMethod.checkM(fsmSrc,fsmSrc.init, fsm,fsm.init,WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(fsmToAdd,fsmToAdd.init,fsm,newA,WMethod.VERTEX_COMPARISON_KIND.NONE));
		
		StatePair whatToMerge = new StatePair(fsm.init,newA);
		LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		Assert.assertTrue(0 < fsm.pairscores.computePairCompatibilityScore_general(whatToMerge,collectionOfVerticesToMerge));
		LearnerGraph result = MergeStates.mergeAndDeterminize_general(fsm, whatToMerge,collectionOfVerticesToMerge);
		Assert.assertNull(WMethod.checkM(fsmSrc,fsmSrc.init,result,result.init,WMethod.VERTEX_COMPARISON_KIND.NONE));

		Assert.assertEquals(fsmToAdd.getStateNumber(), oldToNew.size());
		Assert.assertSame(newA, oldToNew.get(fsmToAdd.init));
		for(CmpVertex oldVert:fsmToAdd.transitionMatrix.keySet())
			Assert.assertNull(WMethod.checkM(fsm,oldToNew.get(oldVert),fsmToAdd,oldVert,WMethod.VERTEX_COMPARISON_KIND.NONE));
	}

	@Test
	public final void testAddToGraphIncompatibles()
	{
		LearnerGraphND fsm = new LearnerGraphND(TestFSMAlgo.buildGraph(relabelFSM_ND, "testRelabel1"),Configuration.getDefaultConfiguration());
		CmpVertex oldA = fsm.findVertex(VertexID.parseID("A")), oldB = fsm.findVertex(VertexID.parseID("B")), oldC= fsm.findVertex(VertexID.parseID("C"));
		fsm.addToCompatibility(oldB,oldC,JUConstants.INCOMPATIBLE);
		LearnerGraphND fsmSrc = new LearnerGraphND(TestFSMAlgo.buildGraph(relabelFSM_ND, "testRelabel1"),Configuration.getDefaultConfiguration());
		fsmSrc.addToCompatibility(fsmSrc.findVertex(VertexID.parseID("B")), fsmSrc.findVertex(VertexID.parseID("C")),JUConstants.INCOMPATIBLE);
		LearnerGraphND fsmToAdd = new LearnerGraphND(TestFSMAlgo.buildGraph("A-a->B-a-#Q", "testAddToGraph1"),Configuration.getDefaultConfiguration());
		CmpVertex newB=fsmToAdd.findVertex(VertexID.parseID("B")), newA=fsmToAdd.findVertex(VertexID.parseID("A"));
		fsmToAdd.addToCompatibility(newB,newA,JUConstants.INCOMPATIBLE);
		
		fsmToAdd.init.setColour(JUConstants.BLUE);fsmToAdd.init.setHighlight(true);
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex addedA = AbstractPathRoutines.addToGraph(fsm, fsmToAdd,oldToNew);
		Assert.assertSame(addedA,oldToNew.get(newA));
		Assert.assertTrue("missing state "+newA+" in oldToNew",oldToNew.containsKey(newA));
		Assert.assertFalse(newA.equals(oldToNew.get(newA)));
		Assert.assertTrue("missing state "+newB+" in oldToNew",oldToNew.containsKey(newB));
		Assert.assertFalse(newB.equals(oldToNew.get(newB)));
		CmpVertex newQ = fsmToAdd.findVertex(VertexID.parseID("Q"));
		Assert.assertTrue("missing state "+newQ+" in oldToNew",oldToNew.containsKey(newQ));
		Assert.assertFalse(newQ.equals(oldToNew.get(newQ)));
		Assert.assertEquals(3,oldToNew.size());
		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(oldToNew.get(newA), oldToNew.get(newB), fsm.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(oldA, oldB, fsm.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(oldToNew.get(newA), oldB, fsm.pairCompatibility));
		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(oldC, oldB, fsm.pairCompatibility));
		Assert.assertEquals(4,fsm.pairCompatibility.size());
	}
	
	/** The standard beginning of our graphML files. */
	public static final String graphML_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><"+StatechumXML.graphmlNodeNameNS.toString()+" xmlns:gml=\"http://graphml.graphdrawing.org/xmlns/graphml\"><graph edgedefault=\"directed\" xmlns=\"gml\">\n";
	/** The standard ending of our graphML files. */
	public static final String graphML_end = "</graph></"+StatechumXML.graphmlNodeNameNS.toString()+">"; 

	protected static final String graphml_beginning = graphML_header+
		"<node VERTEX=\"Initial A\" id=\"A\"/>\n"+
		"<node ";
	
	protected static final String graphml_nodes_edges = "VERTEX=\"B\" id=\"B\"/>\n"+ 
		"<node VERTEX=\"C\" id=\"C\"/>\n"+
		"<edge EDGE=\"a\" directed=\"true\" source=\"A\" target=\"B\"/>\n"+// since I'm using TreeMap, transitions should be alphabetically ordered.
		"<edge EDGE=\"a\" directed=\"true\" source=\"B\" target=\"C\"/>\n"+
		"<edge EDGE=\"c\" directed=\"true\" source=\"B\" target=\"B\"/>\n"+
		"<edge EDGE=\"b\" directed=\"true\" source=\"C\" target=\"B\"/>\n";

	protected static final String graphml_ending = graphml_nodes_edges+ 
		graphML_end;
	
	@Test(expected=IllegalArgumentException.class)
	public final void testGraphMLwriter_fail() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM.replaceAll("A", AbstractPersistence.Initial+"_str"), "testRelabel1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();
		fsm.storage.writeGraphML(writer);
	}

	/** Removes all whitespace characters from the string, by iterating through it. 
	 * 
	 * @param str string to transform
	 * @return result of transformation
	 */
	public String removeWhiteSpace(String str)
	{
		StringBuffer result = new StringBuffer();for(int i=0;i<str.length();++i) if (!Character.isWhitespace(str.charAt(i))) result.append(str.charAt(i));
		return result.toString();
	}
	
	@Test
	public final void testRemoveWhiteSpace1()
	{
		Assert.assertEquals("",removeWhiteSpace(""));
	}
	
	@Test
	public final void testRemoveWhiteSpace2()
	{
		Assert.assertEquals("test",removeWhiteSpace("test"));
	}
	
	@Test
	public final void testRemoveWhiteSpace3()
	{
		Assert.assertEquals("thisisatest343*()",removeWhiteSpace("this is\r a\n\t test 343 *()\n\n\t"));
	}
	
	@Test
	public final void testGraphMLwriter1() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();
		fsm.storage.writeGraphML(writer);
		Assert.assertEquals(removeWhiteSpace(graphml_beginning+graphml_ending),
				removeWhiteSpace(writer.toString()));
	}
	
	@Test
	public final void testGraphMLwriter2() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();
		fsm.findVertex("B").setColour(JUConstants.BLUE);fsm.findVertex("B").setHighlight(true);fsm.findVertex("B").setAccept(false);
		fsm.findVertex("B").setOrigState(new VertexID("P4500"));fsm.findVertex("B").setDepth(5);
		fsm.storage.writeGraphML(writer);
		Assert.assertEquals(removeWhiteSpace(graphml_beginning+
				" "+JUConstants.ACCEPTED.name()+"=\"false\""+
				" "+JUConstants.COLOUR.name()+"=\""+JUConstants.BLUE.name()+"\""+
				" "+JUConstants.DEPTH.name()+"=\""+5+"\""+
				" "+JUConstants.HIGHLIGHT.name()+"=\"true\""+
				" "+JUConstants.ORIGSTATE.name()+"=\""+"P4500"+"\""+
				graphml_ending),
				removeWhiteSpace(writer.toString()));
	}
	
	@Test
	public final void testRelationToInt()
	{
		Assert.assertEquals(AbstractPersistence.compatibilityToInt(JUConstants.INCOMPATIBLE),JUConstants.intSTATEPAIR_INCOMPATIBLE);
		Assert.assertEquals(AbstractPersistence.compatibilityToInt(JUConstants.MERGED),JUConstants.intSTATEPAIR_MERGED);
	}
	
	@Test
	public final void testRelationToInt_fail()
	{
		for(JUConstants constant:JUConstants.values())
			if (constant != JUConstants.INCOMPATIBLE && constant != JUConstants.MERGED)
			{
				final JUConstants value = constant;
				Helper.checkForCorrectException(new whatToRun() { public void run() {
					AbstractPersistence.compatibilityToInt(value);
				}}, IllegalArgumentException.class,"not a valid");
			}
	}
	
	@Test
	public final void testIntToRelation()
	{
		Assert.assertEquals(AbstractPersistence.compatibilityToJUConstants(JUConstants.intSTATEPAIR_INCOMPATIBLE), JUConstants.INCOMPATIBLE);
		Assert.assertEquals(AbstractPersistence.compatibilityToJUConstants(JUConstants.intSTATEPAIR_MERGED), JUConstants.MERGED);
	}
	
	@Test
	public final void testIntToRelation_fail()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			AbstractPersistence.compatibilityToJUConstants(JUConstants.intUNKNOWN);
		}}, IllegalArgumentException.class,"not a valid");
	}
	
	@Test
	public final void testGraphMLwriter_incompatible1() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("A"),JUConstants.INCOMPATIBLE);
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("C"),JUConstants.INCOMPATIBLE);
		StringWriter writer = new StringWriter();
		fsm.findVertex("B").setColour(JUConstants.BLUE);fsm.findVertex("B").setHighlight(true);fsm.findVertex("B").setAccept(false);
		fsm.findVertex("B").setOrigState(new VertexID("P4500"));fsm.findVertex("B").setDepth(5);

		fsm.storage.writeGraphML(writer);
		Assert.assertEquals(removeWhiteSpace(graphml_beginning+
				" "+JUConstants.ACCEPTED.name()+"=\"false\""+
				" "+JUConstants.COLOUR.name()+"=\""+JUConstants.BLUE.name()+"\""+
				" "+JUConstants.DEPTH.name()+"=\""+5+"\""+
				" "+JUConstants.HIGHLIGHT.name()+"=\"true\""+
				" "+JUConstants.ORIGSTATE.name()+"=\""+"P4500"+"\""+
				graphml_nodes_edges+
				"<"+AbstractPersistence.graphmlData+" "+AbstractPersistence.graphmlDataKey+"=\""+AbstractPersistence.graphmlDataIncompatible+"\">"+
				TestWriteReadPair.pairToXML(new PairScore(fsm.findVertex("A"),fsm.findVertex("B"),JUConstants.intSTATEPAIR_INCOMPATIBLE,JUConstants.intUNKNOWN))+"\n"+
				TestWriteReadPair.pairToXML(new PairScore(fsm.findVertex("B"),fsm.findVertex("C"),JUConstants.intSTATEPAIR_INCOMPATIBLE,JUConstants.intUNKNOWN))+"\n"+
				"</"+AbstractPersistence.graphmlData+">"+
				graphML_end),
				removeWhiteSpace(writer.toString()));
	}
	
	/** Similar to the one above but with different values of compatibility. */
	@Test
	public final void testGraphMLwriter_incompatible2() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("A"),JUConstants.INCOMPATIBLE);
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("C"),JUConstants.MERGED);
		StringWriter writer = new StringWriter();
		fsm.findVertex("B").setColour(JUConstants.BLUE);fsm.findVertex("B").setHighlight(true);fsm.findVertex("B").setAccept(false);
		fsm.findVertex("B").setOrigState(new VertexID("P4500"));fsm.findVertex("B").setDepth(5);

		fsm.storage.writeGraphML(writer);
		Assert.assertEquals(removeWhiteSpace(graphml_beginning+
				" "+JUConstants.ACCEPTED.name()+"=\"false\""+
				" "+JUConstants.COLOUR.name()+"=\""+JUConstants.BLUE.name()+"\""+
				" "+JUConstants.DEPTH.name()+"=\""+5+"\""+
				" "+JUConstants.HIGHLIGHT.name()+"=\"true\""+
				" "+JUConstants.ORIGSTATE.name()+"=\""+"P4500"+"\""+
				graphml_nodes_edges+
				"<"+AbstractPersistence.graphmlData+" "+AbstractPersistence.graphmlDataKey+"=\""+AbstractPersistence.graphmlDataIncompatible+"\">"+
				TestWriteReadPair.pairToXML(new PairScore(fsm.findVertex("A"),fsm.findVertex("B"),JUConstants.intSTATEPAIR_INCOMPATIBLE,JUConstants.intUNKNOWN))+"\n"+
				TestWriteReadPair.pairToXML(new PairScore(fsm.findVertex("B"),fsm.findVertex("C"),JUConstants.intSTATEPAIR_MERGED,JUConstants.intUNKNOWN))+"\n"+
				"</"+AbstractPersistence.graphmlData+">"+
				graphML_end),
				removeWhiteSpace(writer.toString()));
	}
	
	public static final LearnerGraph loadLearnerGraph(Element elem,Configuration config)
	{
		LearnerGraph result = new LearnerGraph(config);AbstractPersistence.loadGraph(elem, result);return result;
	}
	
	public static final LearnerGraph loadLearnerGraph(Reader reader,Configuration config)
	{
		LearnerGraph result = new LearnerGraph(config);AbstractPersistence.loadGraph(reader, result);return result;
	}
	
	@Test
	public final void testGraphMLwriter_loadnode1()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		Document doc = null;
		try
		{
			factory.setFeature(javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();
		}
		catch(ParserConfigurationException ex)
		{
			Helper.throwUnchecked("configuration exception: ",ex);
		}
		LearnerGraph actual = loadLearnerGraph(fsm.storage.createGraphMLNode(doc),Configuration.getDefaultConfiguration());
		Assert.assertNull(WMethod.checkM_and_colours(fsm, actual,WMethod.VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertEquals(fsm.init, actual.init);
	}
	
	@Test
	public final void testGraphMLwriter_loadnode2()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		fsm.findVertex("B").setColour(JUConstants.BLUE);fsm.findVertex("B").setHighlight(true);fsm.findVertex("B").setAccept(false);
		fsm.findVertex("B").setOrigState(VertexID.parseID("P4500"));fsm.findVertex("B").setDepth(5);
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		Document doc = null;
		try
		{
			factory.setFeature(javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();
		}
		catch(ParserConfigurationException ex)
		{
			Helper.throwUnchecked("configuration exception: ",ex);
		}
		LearnerGraph actual = loadLearnerGraph(fsm.storage.createGraphMLNode(doc),Configuration.getDefaultConfiguration());
		Assert.assertNull(WMethod.checkM_and_colours(fsm, actual,WMethod.VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertEquals(fsm.init, actual.init);
	}
	
	/** No graph element. */
	@Test
	public final void testGraphMLwriter_loadnode_fail1a()
	{
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		Document doc = null;
		try
		{
			factory.setFeature(javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();
		}
		catch(ParserConfigurationException ex)
		{
			Helper.throwUnchecked("configuration exception: ",ex);
		}
		final Document document = doc;
		checkForCorrectException(new whatToRun() { public void run() {
			loadLearnerGraph(document.createElement("junk"),Configuration.getDefaultConfiguration());
		}},IllegalArgumentException.class,"element name junk");
	}

	/** No graph element. */
	@Test
	public final void testGraphMLwriter_loadnode_fail2a()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		Document doc = null;
		try
		{
			factory.setFeature(javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();
		}
		catch(ParserConfigurationException ex)
		{
			Helper.throwUnchecked("configuration exception: ",ex);
		}
		final org.w3c.dom.Element elem = fsm.storage.createGraphMLNode(doc);elem.removeChild(elem.getFirstChild());
		checkForCorrectException(new whatToRun() { public void run() {
			loadLearnerGraph(elem,Configuration.getDefaultConfiguration());
		}},IllegalArgumentException.class,"absent graph element");
	}
	
	/** No graph element. */
	@Test
	public final void testGraphMLwriter_loadnode_fail2b()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		Document doc = null;
		try
		{
			factory.setFeature(javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();
		}
		catch(ParserConfigurationException ex)
		{
			Helper.throwUnchecked("configuration exception: ",ex);
		}
		final org.w3c.dom.Element elem = fsm.storage.createGraphMLNode(doc);elem.replaceChild(doc.createElement("something"), elem.getFirstChild());
		
		checkForCorrectException(new whatToRun() { public void run() {
			loadLearnerGraph(elem,Configuration.getDefaultConfiguration());
		}},IllegalArgumentException.class,"absent graph element");
	}
	
	/** Duplicate graph elements. */
	@Test
	public final void testGraphMLwriter_loadnode_fail2c()
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(relabelFSM, "testRelabel1"),Configuration.getDefaultConfiguration());
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		Document doc = null;
		try
		{
			factory.setFeature(javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();
		}
		catch(ParserConfigurationException ex)
		{
			Helper.throwUnchecked("configuration exception: ",ex);
		}
		final org.w3c.dom.Element elem = fsm.storage.createGraphMLNode(doc);elem.appendChild(doc.createElement("graph"));
		
		checkForCorrectException(new whatToRun() { public void run() {
			loadLearnerGraph(elem,Configuration.getDefaultConfiguration());
		}},IllegalArgumentException.class,"duplicate graph element");
	}
	
	/** A helper method which saves a given graph and subsequently verifies that the graph loads back.
	 * 
	 * @param gr the graph to save and then load.
	 * @throws IOException
	 */
	public void checkLoading(LearnerGraph gr) throws IOException
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		StringWriter writer = new StringWriter();gr.storage.writeGraphML(writer);
		LearnerGraph loaded = loadLearnerGraph(new StringReader(writer.toString()),config);

		Assert.assertTrue(!gr.pathroutines.checkUnreachableStates());Assert.assertTrue(!loaded.pathroutines.checkUnreachableStates());
		Assert.assertNull(WMethod.checkM(gr,gr.init,loaded,loaded.init,VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertTrue(ids_are_valid(loaded));
		Assert.assertEquals(gr.pairCompatibility,loaded.pairCompatibility);
	}

	/** Checks if simply creating vertices we may get an ID with existing number. */
	protected static final boolean ids_are_valid(LearnerGraph gr)
	{
		final Configuration config = gr.config.copy();config.setLearnerCloneGraph(false);
		LearnerGraph graph = new LearnerGraph(gr,config);
		
		// Now we check that we can create lots of vertices and none of their numbers 
		// will be included 
		{
			LearnerGraph Plus = new LearnerGraph(gr,config);
			for(int i=0;i<1000;++i)
			{
				VertexID id = Plus.nextID(true);
				if (graph.findVertex(id) != null) return false;
			}
		}

		{
			LearnerGraph Minus = new LearnerGraph(gr,config);
			for(int i=0;i<1000;++i)
			{
				VertexID id = Minus.nextID(false);
				if (graph.findVertex(id) != null) return false;
			}
		}

		return true;
	}
	
	@Test
	public final void testVertIDSetter()
	{
		LearnerGraph graph = new LearnerGraph(Configuration.getDefaultConfiguration());
		Assert.assertTrue(ids_are_valid(graph));
		graph.setIDNumbers();
		Assert.assertTrue(ids_are_valid(graph));
		final int currentPlus = graph.vertPositiveID, currentMinus = graph.vertNegativeID; 
		graph.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(new VertexID(VertKind.NEGATIVE,currentMinus+10),graph.config),
				graph.createNewRow());
		Assert.assertFalse(ids_are_valid(graph));
		Assert.assertFalse(ids_are_valid(graph));// check that ids_are_valid did not mess up the IDs
		graph.setIDNumbers();
		Assert.assertTrue(ids_are_valid(graph));

		graph.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(new VertexID(VertKind.POSITIVE,currentPlus+10),graph.config),
				graph.createNewRow());
		Assert.assertFalse(ids_are_valid(graph));
		Assert.assertFalse(ids_are_valid(graph));// check that ids_are_valid did not mess up the IDs
		graph.setIDNumbers();
		Assert.assertTrue(ids_are_valid(graph));
		
		graph.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(new VertexID(VertKind.NEUTRAL,currentPlus+4),graph.config),
				graph.createNewRow());
		Assert.assertTrue(ids_are_valid(graph));
		graph.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(new VertexID(VertKind.NEGATIVE,currentPlus+4),graph.config),
				graph.createNewRow());
		Assert.assertTrue(ids_are_valid(graph));
		
		// check an ID just over the current one.
		graph.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(new VertexID(VertKind.POSITIVE,currentPlus+11),graph.config),
				graph.createNewRow());
		Assert.assertFalse(ids_are_valid(graph));
		Assert.assertFalse(ids_are_valid(graph));// check that ids_are_valid did not mess up the IDs
		graph.setIDNumbers();
		Assert.assertTrue(ids_are_valid(graph));
		
	}
	
	@Test
	public final void testConvertToNumeric1()
	{
		LearnerGraph graph = new LearnerGraph(Configuration.getDefaultConfiguration());
		AbstractPathRoutines.convertToNumerical(new LearnerGraph(Configuration.getDefaultConfiguration()),graph);
		Assert.assertTrue(graph.wmethod.checkGraphNumeric());
		Assert.assertTrue(ids_are_valid(graph));
	}
	
	@Test
	public final void testConvertToNumeric2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B", "testConvertToNumeric2"),Configuration.getDefaultConfiguration());
		LearnerGraph graph = new LearnerGraph(gr.config);AbstractPathRoutines.convertToNumerical(gr,graph);
		Assert.assertFalse(gr.wmethod.checkGraphNumeric());
		Assert.assertTrue(graph.wmethod.checkGraphNumeric());
		Assert.assertTrue(ids_are_valid(gr));
		Assert.assertTrue(ids_are_valid(graph));
		LearnerGraph graph2 = new LearnerGraph(graph.config);AbstractPathRoutines.convertToNumerical(graph,graph2);
		Assert.assertTrue(graph2.wmethod.checkGraphNumeric());
		Assert.assertTrue(ids_are_valid(graph2));
	}
	
	@Test
	public final void testConvertToNumeric3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a-#B\nA-b->C-a->D-a-#E\nD-b->A\nC-c->C", "testConvertToNumeric2"),Configuration.getDefaultConfiguration());
		LearnerGraph graph = new LearnerGraph(gr.config);AbstractPathRoutines.convertToNumerical(gr,graph);
		Assert.assertFalse(gr.wmethod.checkGraphNumeric());
		Assert.assertTrue(graph.wmethod.checkGraphNumeric());
		Assert.assertTrue(ids_are_valid(gr));
		Assert.assertTrue(ids_are_valid(graph));
		LearnerGraph graph2 = new LearnerGraph(graph.config);AbstractPathRoutines.convertToNumerical(graph,graph2);
		Assert.assertTrue(graph2.wmethod.checkGraphNumeric());
		Assert.assertTrue(ids_are_valid(graph2));
	}
		
	@Test
	public final void testGraphMLWriter3() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		checkLoading(fsm);
	}
	
	@Test
	public final void testGraphMLWriter4() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		fsm.findVertex("BD2").setHighlight(true);
		fsm.findVertex("BB1").setAccept(false);fsm.findVertex("BB1").setColour(JUConstants.RED);
		fsm.findVertex("B").setColour(JUConstants.RED);
		fsm.findVertex("A").setColour(JUConstants.BLUE);
		checkLoading(fsm);
	}

	@Test
	public final void testGraphMLWriter5() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1"),Configuration.getDefaultConfiguration());
		checkLoading(fsm);
	}
	
	@Test
	public final void testGraphMLWriter6() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1"),Configuration.getDefaultConfiguration());
		checkLoading(fsm);
	}

	/** Loading graphs with reject pairs. */
	@Test
	public final void testGraphMLWriter_incompatible1() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1"),Configuration.getDefaultConfiguration());
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("C"),JUConstants.INCOMPATIBLE);
		checkLoading(fsm);
	}
	
	/** Loading graphs with reject pairs. */
	@Test
	public final void testGraphMLWriter_incompatible2() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1"),Configuration.getDefaultConfiguration());
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("A"),JUConstants.INCOMPATIBLE);
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("C"),JUConstants.INCOMPATIBLE);
		checkLoading(fsm);
	}
	
	/** Loading graphs with reject pairs. */
	@Test
	public final void testGraphMLWriter_incompatible3() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1"),Configuration.getDefaultConfiguration());
		for(CmpVertex vert:fsm.transitionMatrix.keySet())
			for(CmpVertex vert2:fsm.transitionMatrix.keySet())
				if (vert != vert2)
					fsm.addToCompatibility(vert,vert2,JUConstants.INCOMPATIBLE);
		checkLoading(fsm);
	}
	
	/** Tests that a graph with no VERTEX nodes will be loaded (the initial state is q0). 
	 * @throws IOException */
	@Test
	public final void testGraphMLwriter_loadnode_noVERTEX() throws IOException
	{
		String text = graphML_header+"<node id=\""+AbstractPersistence.InitialQ0+"\"/>\n"+
		"<node id=\"B\"/>\n"+ 
		"<node id=\"C\" "+JUConstants.DEPTH.name()+"=\""+5+"\" />\n"+
		"<edge EDGE=\"a\" directed=\"true\" source=\""+AbstractPersistence.InitialQ0+"\" target=\"B\"/>\n"+// since I'm using TreeMap, transitions should be alphabetically ordered.
		"<edge EDGE=\"a\" directed=\"true\" source=\"B\" target=\"C\"/>\n"+
		"<edge EDGE=\"c\" directed=\"true\" source=\"B\" target=\"B\"/>\n"+
		"<edge EDGE=\"b\" directed=\"true\" source=\"C\" target=\"B\"/>\n"+
		graphML_end;
		final String FSMq0 = AbstractPersistence.InitialQ0+"-a->B-a->C-b->B-c->B";
		LearnerGraph fsm = new LearnerGraph(TestFSMAlgo.buildGraph(FSMq0,"testGraphMLwriter_loadnode_noVERTEX"),Configuration.getDefaultConfiguration());
		fsm.findVertex("C").setDepth(5);
		LearnerGraph loaded = new LearnerGraph(Configuration.getDefaultConfiguration().copy());
		AbstractPersistence.loadGraph(new StringReader(text), loaded);

		Assert.assertTrue(!fsm.pathroutines.checkUnreachableStates());Assert.assertTrue(!loaded.pathroutines.checkUnreachableStates());
		Assert.assertNull(WMethod.checkM(fsm,fsm.init,loaded,loaded.init,VERTEX_COMPARISON_KIND.DEEP));
		for(Entry<CmpVertex,LinkedList<String>> entry:fsm.pathroutines.computeShortPathsToAllStates().entrySet())
		{
			CmpVertex v=entry.getKey(),other = loaded.paths.getVertex(entry.getValue());
			Assert.assertEquals(v.isAccept(),other.isAccept());
			Assert.assertEquals(v.isHighlight(),other.isHighlight());
			if (v.getColour() == null) 
				Assert.assertNull(other.getColour());
			else
				Assert.assertEquals(v.getColour(), other.getColour());
		}

		Assert.assertTrue(ids_are_valid(loaded));
	}

	/** Tests that a graph with multiple initial states (q0 and Initial) will fail to load  
	 * @throws IOException */
	@Test
	public final void testGraphMLwriter_loadnode_noVERTEX_fail1()
	{
		final String text = graphML_header+"<node id=\""+AbstractPersistence.InitialQ0+"\"/>\n"+
		"<node VERTEX=\"Initial B\" id=\"B\" />\n"+ 
		"<node VERTEX=\"C\" id=\"C\"/>\n"+
		"<edge EDGE=\"a\" directed=\"true\" source=\""+AbstractPersistence.InitialQ0+"\" target=\"B\"/>\n"+// since I'm using TreeMap, transitions should be alphabetically ordered.
		"<edge EDGE=\"a\" directed=\"true\" source=\"B\" target=\"C\"/>\n"+
		"<edge EDGE=\"c\" directed=\"true\" source=\"B\" target=\"B\"/>\n"+
		"<edge EDGE=\"b\" directed=\"true\" source=\"C\" target=\"B\"/>\n"+
		graphML_end;
		final LearnerGraph loaded = new LearnerGraph(Configuration.getDefaultConfiguration().copy());
		
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			AbstractPersistence.loadGraph(new StringReader(text), loaded);}}, 
		IllegalArgumentException.class, "vertices B and q0 are both");
	}

	@Test
	public final void testGraphMLWriter_fail_on_load_boolean() throws IOException
	{
		final LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	checkForCorrectException(new whatToRun() { public void run() {
	    		loadLearnerGraph(new StringReader(writer.toString().replace("ACCEPTED=\"false\"", "ACCEPTED=\"aa\"")),Configuration.getDefaultConfiguration());
	    	}},IllegalUserDataException.class,"invalid colour");
		}		
	}
	
	@Test
	public final void testGraphMLWriter_fail_on_load_colour() throws IOException
	{
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	try
	    	{
	    		loadLearnerGraph(new StringReader(writer.toString().replace("COLOUR=\"red\"", "COLOUR=\"aa\"")),Configuration.getDefaultConfiguration());
	    	}
	    	catch(FatalException ex)
	    	{
	    		Assert.assertTrue(ex.getCause() instanceof IllegalUserDataException);
	    	}
		}		
	}
	
	@Test
	public final void testGraphMLWriter_fail_on_load_depth() throws IOException
	{
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	try
	    	{
	    		loadLearnerGraph(new StringReader(writer.toString().replace("COLOUR=\"red\"", "DEPTH=\"aa\"")),Configuration.getDefaultConfiguration());
	    	}
	    	catch(FatalException ex)
	    	{
	    		Assert.assertTrue(ex.getCause() instanceof IllegalUserDataException);
	    	}
		}		
	}
	
	@Test
	public final void testGraphMLWriter_fail_on_load_pairs() throws IOException
	{
		final LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("A"),JUConstants.INCOMPATIBLE);
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("S"),JUConstants.INCOMPATIBLE);
		gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	checkForCorrectException(new whatToRun() { public void run() {
	    		loadLearnerGraph(new StringReader(writer.toString().replace(AbstractPersistence.graphmlDataIncompatible, "AA")),Configuration.getDefaultConfiguration());
	    	}},IllegalArgumentException.class,"unexpected key");
		}		
	}
	
	/** Unknown state in a pair. */
	@Test
	public final void testGraphMLWriter_fail_on_load_pairs_unknownstate1() throws IOException
	{
		final LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("A"),JUConstants.INCOMPATIBLE);
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("S"),JUConstants.INCOMPATIBLE);
		gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	checkForCorrectException(new whatToRun() { public void run() {
	    		final String Q_S = StatechumXML.ATTR_Q.name()+"=\""+gr.findVertex("S").getID().toString(),
	    			R_S = StatechumXML.ATTR_R.name()+"=\""+gr.findVertex("S").getID().toString();
	    		String xmlRepresentation = writer.toString(), brokenRepresentation = null;
	    		if (xmlRepresentation.contains(Q_S))
	    			brokenRepresentation = xmlRepresentation.replace(Q_S, StatechumXML.ATTR_Q.name()+"=\""+"T");
	    		else
	    			if (xmlRepresentation.contains(R_S))
	    				brokenRepresentation = xmlRepresentation.replace(R_S, StatechumXML.ATTR_R.name()+"=\""+"T");
	    			else
	    				Assert.fail("unexpected XML representation");
	    		loadLearnerGraph(new StringReader(brokenRepresentation),Configuration.getDefaultConfiguration());
	    	}},IllegalArgumentException.class,"Unknown state T");
		}		
	}
	
	/** Unknown state in a pair. */
	@Test
	public final void testGraphMLWriter_fail_on_load_pairs_unknownstate2() throws IOException
	{
		final LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("A"),JUConstants.INCOMPATIBLE);
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("S"),JUConstants.INCOMPATIBLE);
		gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	checkForCorrectException(new whatToRun() { public void run() {
	    		final String Q_A = StatechumXML.ATTR_Q.name()+"=\""+gr.findVertex("A").getID().toString(),
    			R_A = StatechumXML.ATTR_R.name()+"=\""+gr.findVertex("A").getID().toString();
	    		String xmlRepresentation = writer.toString(), brokenRepresentation = null;
	    		if (xmlRepresentation.contains(Q_A))
	    			brokenRepresentation = xmlRepresentation.replace(Q_A, StatechumXML.ATTR_Q.name()+"=\""+"T");
	    		else
	    			if (xmlRepresentation.contains(R_A))
	    				brokenRepresentation = xmlRepresentation.replace(R_A, StatechumXML.ATTR_R.name()+"=\""+"T");
	    			else
	    				Assert.fail("unexpected XML representation");
	
	    		loadLearnerGraph(new StringReader(brokenRepresentation),Configuration.getDefaultConfiguration());
	    	}},IllegalArgumentException.class,"Unknown state T");
		}		
	}
	
	/** Unknown compatibility code in a pair. */
	@Test
	public final void testGraphMLWriter_fail_on_load_pairs_unknowncompatibilitycode1() throws IOException
	{
		final LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("A"),JUConstants.INCOMPATIBLE);
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("S"),JUConstants.INCOMPATIBLE);
		gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	checkForCorrectException(new whatToRun() { public void run() {
	    		loadLearnerGraph(new StringReader(writer.toString().replace(
	    				StatechumXML.ATTR_SCORE.name()+"=\""+JUConstants.intSTATEPAIR_INCOMPATIBLE,
    				StatechumXML.ATTR_SCORE.name()+"=\"6")),Configuration.getDefaultConfiguration());
	    	}},IllegalArgumentException.class,"6 is not a valid compatibility");
		}		
	}
	
	/** Invalid compatibility code in a pair. */
	@Test
	public final void testGraphMLWriter_fail_on_load_pairs_unknowncompatibilitycode2() throws IOException
	{
		final LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("A"),JUConstants.INCOMPATIBLE);
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("S"),JUConstants.INCOMPATIBLE);
		gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	checkForCorrectException(new whatToRun() { public void run() {
	    		loadLearnerGraph(new StringReader(writer.toString().replace(
	    				StatechumXML.ATTR_SCORE.name()+"=\""+JUConstants.intSTATEPAIR_INCOMPATIBLE,
    				StatechumXML.ATTR_SCORE.name()+"=\"AA")),Configuration.getDefaultConfiguration());
	    	}},IllegalArgumentException.class,"failed to read a score in a pair");
		}		
	}
	
	@Test
	public final void testGraphMLWriter_fail_on_load_invalid_node() throws IOException
	{
		final LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("A"),JUConstants.INCOMPATIBLE);
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("S"),JUConstants.INCOMPATIBLE);
		gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	checkForCorrectException(new whatToRun() { public void run() {
	    		loadLearnerGraph(new StringReader(writer.toString().replace(AbstractPersistence.graphmlData, "AA")),Configuration.getDefaultConfiguration());
	    	}},IllegalArgumentException.class,"unexpected node");
		}		
	}
	
	@Test
	public final void testGraphMLWriter_load_despite_Initial() throws IOException
	{
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1"),Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	try
	    	{
	    		loadLearnerGraph(new StringReader(writer.toString().replace("VERTEX=\"BB1\"", "VERTEX=\""+AbstractPersistence.Initial+" BB1\"")),Configuration.getDefaultConfiguration());
	    	}
	    	catch(IllegalArgumentException ex)
	    	{
	    		Assert.assertTrue(ex.getMessage().contains("are both labelled as initial"));
	    	}
		}		
	}
	
	@Test
	public final void testComputeHamming()
	{
		Assert.assertEquals("Hamming distances min: 1 max: 1", new Transform(g).ComputeHamming(false));
	}
	
	/** Tests merging of the two automata on page 18 of "why_nondet_does_not_matter.xoj" */
	@Test
	public final void testAugmentFromMax1_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph("H-a->A-a->B-b->C\nH-c->B\nH-d->B", "testAugmentFromMax1_gr"),config);
		LearnerGraph max = new LearnerGraph(TestFSMAlgo.buildGraph("I-a->D-a-#E\nI-d-#E\nI-c->F-b->G", "testAugmentFromMax1_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true, config, true);
		TestEquivalenceChecking.checkM("H-a->A-a-#BE\nH-d-#BE\nH-c->BF-b->C", result.pathroutines.getGraph(), config);
		Assert.assertEquals(5,result.getStateNumber());
	}
	
	/** Tests merging of the two automata on page 18 of "why_nondet_does_not_matter.xoj" */
	@Test
	public final void testAugmentFromMax1_nonoverride()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph("H-a->A-a->B-b->C\nH-c->B\nH-d->B", "testAugmentFromMax1_gr"),config);
		final LearnerGraph max = new LearnerGraph(TestFSMAlgo.buildGraph("I-a->D-a-#E\nI-d-#E\nI-c->F-b->G", "testAugmentFromMax1_max"),config);
		checkForCorrectException(new whatToRun() {	public void run() throws NumberFormatException 
		{
			Transform.augmentFromMAX(gr, max, false, true,config, true);
		}}, IllegalArgumentException.class, "incompatible");
	}
	
	/** Tests merging of the two automata on page 18 of "why_nondet_does_not_matter.xoj" */
	@Test
	public final void testAugmentFromMax1_BA()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		String automatonWithReject = "I-a->D-a-#E\nI-d-#E\nI-c->F-b->G";
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(automatonWithReject, "testAugmentFromMax1_max"),config);
		LearnerGraph max = new LearnerGraph(TestFSMAlgo.buildGraph("H-a->A-a->B-b->C\nH-c->B\nH-d->B", "testAugmentFromMax1_gr"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}

	/** Tests merging of the two automata on page 17 of "why_nondet_does_not_matter.xoj" */
	@Test
	public final void testAugmentFromMax2_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr"),config);
		LearnerGraph max = new LearnerGraph(TestFSMAlgo.buildGraph("B-b->D-b->F-a->F-b->B", "testAugmentFromMax2_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}

	/** Tests merging of the two automata on page 17 of "why_nondet_does_not_matter.xoj" */
	@Test
	public final void testAugmentFromMax3_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->C-b->C-a->E-b-#G\n"+
				"A-c->A\nC-c->C", "testAugmentFromMax3_gr"),config);
		LearnerGraph max = new LearnerGraph(TestFSMAlgo.buildGraph("B-b->D-b->F-a->F-b->B\n"+
				"B-c->D-c->F-c->B", "testAugmentFromMax3_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}
	
	@Test
	public final void testAugmentFromMax4_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		String origGraph = "A-b->A-a->A-c->B-c->C\n";
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph(origGraph, "testAugmentFromMax4_gr"),config);
		LearnerGraph max = new LearnerGraph(TestFSMAlgo.buildGraph("E-a->F-a->G-a->H", "testAugmentFromMax4_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config,true);
		Assert.assertNull(result);
	}

	@Test
	public final void testAugmentFromMax5_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->A-c->B-c->C\n", "testAugmentFromMax4_gr"),config);
		LearnerGraph max = new LearnerGraph(TestFSMAlgo.buildGraph("E-a->F-a->G-a->H-a-#I", "testAugmentFromMax5_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config,true);
		TestEquivalenceChecking.checkM("AE-a->AF-a->AG-a->AH-a-#I\n"+
				"AE-b->P-c->B-c->C\nP-a->P-b->P\nAE-c->B\nAF-b->P\nAF-c->B\nAG-b->P\nAG-c->B\nAH-b->P\nAH-c->B", result.pathroutines.getGraph(), config);
	}
	
	@Test
	public final void testAugmentFromMax6_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->A-c->B-c->C\n", "testAugmentFromMax4_gr"),config);
		LearnerGraph max = new LearnerGraph(config);max.init.setAccept(false);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config,true);
		Assert.assertNull(WMethod.checkM(max, result));
	}
	
	@Test
	public final void testAugmentFromMax6_AB_nooverride()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->A-c->B-c->C\n", "testAugmentFromMax4_gr"),config);
		final LearnerGraph max = new LearnerGraph(config);max.init.setAccept(false);
		checkForCorrectException(new whatToRun() {	public void run() throws NumberFormatException 
			{
				Transform.augmentFromMAX(gr, max, false, true,config, true);
			}}, IllegalArgumentException.class, "incompatible");
	}

	@Test
	public final void testAugmentFromMax6_BA()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(config);gr.init.setAccept(false);
		LearnerGraph max = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->A-c->B-c->C\n", "testAugmentFromMax4_gr"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config,true);
		Assert.assertNull(result);
	}

	@Test
	public final void testAugmentFromMax7_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr"),config);
		LearnerGraph max = new LearnerGraph(TestFSMAlgo.buildGraph("B-b->D-b->F-a->F-b->B\nD-a-#E", "testAugmentFromMax7_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		TestEquivalenceChecking.checkM("AB-b->AD-b->AF-b->AB\nAF-a->CF-b->CB-b->CD-b->CF-a->EF-b-#G\n"+
				"AB-a->C-b->C-a->E-b-#G\nCB-a->E\nAD-a-#H\nCD-a-#H", result.pathroutines.getGraph(), config);
	}

	@Test
	public final void testAugmentFromMax7_AB_nooverride()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr"),config);
		final LearnerGraph max = new LearnerGraph(TestFSMAlgo.buildGraph("B-b->D-b->F-a->F-b->B\nD-a-#E", "testAugmentFromMax7_max"),config);
		checkForCorrectException(new whatToRun() {	public void run() throws NumberFormatException 
			{
				Transform.augmentFromMAX(gr, max, false, true,config, true);
			}}, IllegalArgumentException.class, "incompatible");
	}
	
	@Test
	public final void testAugmentFromMax8_a()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr"),config);
		LearnerGraph max = new LearnerGraph(config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}
	
	@Test
	public final void testAugmentFromMax8_b()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr"),config);
		LearnerGraph max = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->A-c->A-d->A", "testAugmentFromMax7_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}
	
	@Test
	public final void testCountMatchedTransitions1()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a->D-a->A","testCountMatchedTransitions1big"),config);
		LearnerGraph small = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A","testCountMatchedTransitions1small"),config);
		
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(4,Transform.countSharedTransitions(big, small).matched);
	}
	
	@Test
	public final void testCountMatchedTransitions2()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a->D-a->A","testCountMatchedTransitions1big"),config);
		LearnerGraph small = new LearnerGraph(config);
		
		Assert.assertEquals(4,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).matched);
	}

	@Test
	public final void testCountMatchedTransitions3()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a->D-a->A\nB-b->B","testCountMatchedTransitions3big"),config);
		LearnerGraph small = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A","testCountMatchedTransitions3small"),config);
		
		Assert.assertEquals(1,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(4,Transform.countSharedTransitions(big, small).matched);
	}

	@Test
	public final void testCountMatchedTransitions4()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->A\nB-c->C-b->C","testCountMatchedTransitions4big"),config);
		LearnerGraph small = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A","testCountMatchedTransitions4small"),config);
		
		Assert.assertEquals(2,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(2,Transform.countSharedTransitions(big, small).matched);
	}

	@Test
	public final void testCountMatchedTransitions5()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->A\nB-c->C-b->C","testCountMatchedTransitions4big"),config);
		LearnerGraph small = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->A\nB-c->D-b->E-b->F","testCountMatchedTransitions5small"),config);
		
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(5,Transform.countSharedTransitions(big, small).matched);
	}

	/** Tests identification of self-loops. */
	@Test
	public final void testCountMatchedTransitions6()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A","testCountMatchedTransitions4big"),config);
		LearnerGraph small = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->A","testCountMatchedTransitions5small"),config);
		
		Assert.assertEquals(1,Transform.countSharedTransitions(big, small).Tx);
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(2,Transform.countSharedTransitions(big, small).matched);
	}

	/** Tests identification of self-loops. */
	@Test
	public final void testCountMatchedTransitions7()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->B-c->B","testCountMatchedTransitions7big"),config);
		LearnerGraph small = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->B-a->C-c->D","testCountMatchedTransitions7small"),config);
		
		Assert.assertEquals(2,Transform.countSharedTransitions(big, small).Tx);
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(3,Transform.countSharedTransitions(big, small).matched);
	}

	/** Tests identification of self-loops. */
	@Test
	public final void testCountMatchedTransitions8()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->B-c->B-a->A","testCountMatchedTransitions8big"),config);
		LearnerGraph small = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->B-a->C-c->D-c->E-a->F-b->G","testCountMatchedTransitions8small"),config);
		
		Assert.assertEquals(2,Transform.countSharedTransitions(big, small).Tx);
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(6,Transform.countSharedTransitions(big, small).matched);
	}

	@Test
	public final void testCountMatchedTransitions_fail1()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph big = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a->D-a->A","testCountMatchedTransitions_fail1big"),config);
		final LearnerGraph small = new LearnerGraph(config);small.init.setAccept(false);
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			Transform.countSharedTransitions(big, small);
		}},DifferentFSMException.class,"have a different acceptance");
	}

	@Test
	public final void testCountMatchedTransitions_fail2()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph big = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a->D-a->A","testCountMatchedTransitions1big"),config);
		final LearnerGraph small = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a-#C","testCountMatchedTransitions_fail2small"),config);
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			Transform.countSharedTransitions(big, small);
		}},DifferentFSMException.class,"have a different acceptance");
	}
	
	@Test
	public final void testCountMatchedTransitions_fail3()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph big = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a->D-a->A","testCountMatchedTransitions_fail3big"),config);
		final LearnerGraph small = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->A\nB-b->C","testCountMatchedTransitions_fail3small"),config);
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			Transform.countSharedTransitions(big, small);
		}},IllegalArgumentException.class,"not contained");
	}
	
	@Test
	public final void testCountMatchedTransitions_fail4()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph big = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->A\nB-c->C-b->C","testCountMatchedTransitions4big"),config);
		final LearnerGraph small = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a->D\nB-c->D-c->D","testCountMatchedTransitions_fail4small"),config);
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			Transform.countSharedTransitions(big, small);
		}},IllegalArgumentException.class,"small graph is not contained in the large one, from [ C, D ] unmatched transition c to (nothing_in_big,D)");
	}
	
	@Test
	public final void testCountMatchedTransitions_fail5()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph big = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->A\nB-c->C-b->C","testCountMatchedTransitions4big"),config);
		final LearnerGraph small = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a->A\nB-c->D-b->E-b->F","testCountMatchedTransitions_fail5small"),config);
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			Transform.countSharedTransitions(big, small);
		}},IllegalArgumentException.class,"small graph is not contained in the large one, from [ A, B ] unmatched transition c to (nothing_in_big,D)");
	}
	
	@Test
	public final void testQuanteKoschke1()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		Assert.assertEquals(0.24,
			Transform.QuanteKoschkeDifference(
				new LearnerGraph(TestFSMAlgo.buildGraph("A-create->B-push->B","testQuanteKoschke1a"),config),
				new LearnerGraph(TestFSMAlgo.buildGraph("A-create->B-pop->D","testQuanteKoschke1b"),config)),
			0.01
			);
	}
	
	/** Unmatched loops. */
	@Test
	public final void testQuanteKoschke2()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph grA=new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->D-a->A\nA-b->B\nD-e->D\nB-f->B", "testQuanteKoschke2A"),config);
		LearnerGraph grB=new LearnerGraph(TestFSMAlgo.buildGraph("A-a->C-a->E-a->C\nA-b->C-q->C\nE-p->E", "testQuanteKoschke2B"),config);
		Assert.assertEquals(0.22,
			Transform.QuanteKoschkeDifference(grA,grB),
			0.01
			);
		ChangesCounter counter = new ChangesCounter(grA,grB,null);
		//Visualiser.updateFrame(grA, grB);Visualiser.waitForKey();
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, 1, counter,config);
		double ourDifference = ((double)counter.getAdded()+counter.getRemoved())/(grA.countEdges()+grB.countEdges());
		//System.out.println("removed: "+counter.getRemoved()+" out of "+grA.countEdges()+", added: "+counter.getAdded()+" to produce "+grB.countEdges()+"; our difference: "+our);
		Assert.assertEquals(0.5, ourDifference,0.01);
	}
	
	/** Matched loops. */
	@Test
	public final void testQuanteKoschke3()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph grA=new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->D-a->A\nA-b->B\nD-e->D\nB-f->B", "testQuanteKoschke2A"),config);
		LearnerGraph grB=new LearnerGraph(TestFSMAlgo.buildGraph("A-a->C-a->E-a->A\nA-b->C-q->C\nE-p->E", "testQuanteKoschke3B"),config);
		Assert.assertEquals(0.23,
			Transform.QuanteKoschkeDifference(grA,grB),
			0.01
			);
		ChangesCounter counter = new ChangesCounter(grA,grB,null);
		//Visualiser.updateFrame(grA, grB);Visualiser.waitForKey();
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, 1, counter,config);
		double ourDifference = ((double)counter.getAdded()+counter.getRemoved())/(grA.countEdges()+grB.countEdges());
		//System.out.println("removed: "+counter.getRemoved()+" out of "+grA.countEdges()+", added: "+counter.getAdded()+" to produce "+grB.countEdges()+"; our difference: "+our);
		Assert.assertEquals(0.33, ourDifference,0.01);
	}
	
}
