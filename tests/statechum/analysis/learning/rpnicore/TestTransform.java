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
import statechum.Label;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraphND;
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

	private LearnerGraph g = buildLearnerGraph("A-a->A-b->B",	"testToBooleans",Configuration.getDefaultConfiguration());
	private StringBuffer resultDescr = new StringBuffer();	
	private List<List<Label>> buildListList(String [][]list_of_seq)
	{
		List<List<Label>> result = new LinkedList<List<Label>>();
		for(String []seq:list_of_seq)
			result.add(AbstractLearnerGraph.buildList(Arrays.asList(seq),g.config));
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
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		AbstractPathRoutines.relabel(fsm,-1,"c");
	}
	
	@Test
	public final void testRelabel0_1()
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		Set<Label> origAlphabet = fsm.pathroutines.computeAlphabet();Assert.assertTrue(origAlphabet.isEmpty());
		AbstractPathRoutines.relabel(fsm,-1,"new");Set<Label> newAlphabet = fsm.pathroutines.computeAlphabet();Assert.assertTrue(newAlphabet.isEmpty());
	}
	
	@Test
	public final void testRelabel0_2()
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		Set<Label> origAlphabet = fsm.pathroutines.computeAlphabet();Assert.assertTrue(origAlphabet.isEmpty());
		AbstractPathRoutines.relabel(fsm,0,"new");Set<Label> newAlphabet = fsm.pathroutines.computeAlphabet();Assert.assertTrue(newAlphabet.isEmpty());
	}
	@Test
	public final void testRelabel0_3()
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		Set<Label> origAlphabet = fsm.pathroutines.computeAlphabet();Assert.assertTrue(origAlphabet.isEmpty());
		AbstractPathRoutines.relabel(fsm,1,"new");Set<Label> newAlphabet = fsm.pathroutines.computeAlphabet();Assert.assertTrue(newAlphabet.isEmpty());
	}
	
	@Test
	public final void testRelabel1_1()
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		Set<Label> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,-1,"new");Set<Label> newAlphabet = fsm.pathroutines.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<Label> diffOrigNew = new TreeSet<Label>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertTrue(diffOrigNew.isEmpty());		
	}

	@Test
	public final void testRelabel1_2()
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		Set<Label> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,0,"new");Set<Label> newAlphabet = fsm.pathroutines.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<Label> diffOrigNew = new TreeSet<Label>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertTrue(diffOrigNew.isEmpty());		
	}

	@Test
	public final void testRelabel2()
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		Set<Label> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,1,"new");Set<Label> newAlphabet = fsm.pathroutines.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<Label> diffOrigNew = new TreeSet<Label>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertEquals(1,diffOrigNew.size());		
	}

	@Test
	public final void testRelabel3()
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		Set<Label> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,2,"new");Set<Label> newAlphabet = fsm.pathroutines.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<Label> diffOrigNew = new TreeSet<Label>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertEquals(2,diffOrigNew.size());		
	}


	@Test
	public final void testRelabel4()
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		Set<Label> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,origAlphabet.size(),"new");Set<Label> newAlphabet = fsm.pathroutines.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Assert.assertTrue(newAlphabet.equals(origAlphabet));		
	}
	
	public static final String relabelFSM_ND = "A-a->B-a->C-b->B-c->B\nA-a->C";
	
	@Test
	public final void testRelabel3_ND()
	{
		LearnerGraphND fsm = buildLearnerGraphND(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		Set<Label> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,2,"new");Set<Label> newAlphabet = fsm.pathroutines.computeAlphabet();
		Assert.assertEquals(origAlphabet.size(), newAlphabet.size());
		Set<Label> diffOrigNew = new TreeSet<Label>();diffOrigNew.addAll(origAlphabet);diffOrigNew.retainAll(newAlphabet);
		Assert.assertEquals(2,diffOrigNew.size());		
	}


	@Test
	public final void testRelabel4_ND()
	{
		LearnerGraphND fsm = buildLearnerGraphND(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		Set<Label> origAlphabet = fsm.pathroutines.computeAlphabet();
		AbstractPathRoutines.relabel(fsm,origAlphabet.size(),"new");Set<Label> newAlphabet = fsm.pathroutines.computeAlphabet();
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
		Assert.assertTrue(actual.pairCompatibility.compatibility.isEmpty());
		Assert.assertTrue(actual.transitionMatrix.isEmpty());
	}
	
	/** Tests that graph relabelling works correctly. */
	@Test
	public final void testStateRelabelling1()
	{
		LearnerGraphND A = buildLearnerGraphND("NA-s->A-a->B\nA-a->C\nB-a->D\nB-a->A", "testStateRelabelling1",Configuration.getDefaultConfiguration()),
			expected = buildLearnerGraphND("NE-s->T-a->U\nT-a->R\nU-a->S\nU-a->T", "testStateRelabelling1",Configuration.getDefaultConfiguration());
		A.addToCompatibility(A.findVertex("B"), A.findVertex("D"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		A.addToCompatibility(A.findVertex("D"), A.findVertex("C"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		expected.addToCompatibility(expected.findVertex("U"), expected.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		expected.addToCompatibility(expected.findVertex("S"), expected.findVertex("R"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Map<CmpVertex,CmpVertex> whatToG = new TreeMap<CmpVertex,CmpVertex>();
		whatToG.put(A.findVertex("NA"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("NE"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("A"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("T"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("B"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("U"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("C"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("R"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("D"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("S"), Configuration.getDefaultConfiguration()));
		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		AbstractLearnerGraph.addAndRelabelGraphs(A, whatToG, actual);actual.setInit(actual.findVertex("NE"));
		Assert.assertNull(WMethod.checkM_and_colours(expected, actual, VERTEX_COMPARISON_KIND.DEEP));		
	}
	
	/** Tests that graph relabelling works correctly, this one with some compatible and incompatible states. */
	@Test
	public final void testStateRelabelling2()
	{
		LearnerGraphND A = buildLearnerGraphND("NA-s->A-a->B\nA-a->C\nB-a->D\nB-a->A", "testStateRelabelling1",Configuration.getDefaultConfiguration()),
			expected = buildLearnerGraphND("NE-s->T-a->U\nT-a->R\nU-a->S\nU-a->T", "testStateRelabelling1",Configuration.getDefaultConfiguration());
		A.addToCompatibility(A.findVertex("B"), A.findVertex("D"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		A.addToCompatibility(A.findVertex("D"), A.findVertex("C"),JUConstants.PAIRCOMPATIBILITY.MERGED);
		A.addToCompatibility(A.findVertex("D"), A.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.MERGED);
		expected.addToCompatibility(expected.findVertex("U"), expected.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		expected.addToCompatibility(expected.findVertex("S"), expected.findVertex("R"),JUConstants.PAIRCOMPATIBILITY.MERGED);
		expected.addToCompatibility(expected.findVertex("S"), expected.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.MERGED);
		Map<CmpVertex,CmpVertex> whatToG = new TreeMap<CmpVertex,CmpVertex>();
		whatToG.put(A.findVertex("NA"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("NE"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("A"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("T"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("B"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("U"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("C"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("R"), Configuration.getDefaultConfiguration()));
		whatToG.put(A.findVertex("D"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("S"), Configuration.getDefaultConfiguration()));
		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		AbstractLearnerGraph.addAndRelabelGraphs(A, whatToG, actual);actual.setInit(actual.findVertex("NE"));
		Assert.assertNull(WMethod.checkM_and_colours(expected, actual, VERTEX_COMPARISON_KIND.DEEP));		
	}
	
	@Test
	public final void testAddToGraph0_1() throws IncompatibleStatesException
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = new LearnerGraph(Configuration.getDefaultConfiguration());
		helperAddToGraph0_1(fsm, fsmSrc,fsmToAdd);
	}
	
	@Test
	public final void testAddToGraph0_1_ND() throws IncompatibleStatesException
	{
		LearnerGraphND fsm = buildLearnerGraphND(relabelFSM_ND, "testRelabel1",Configuration.getDefaultConfiguration());
		LearnerGraphND fsmSrc = buildLearnerGraphND(relabelFSM_ND, "testRelabel1",Configuration.getDefaultConfiguration());
		LearnerGraphND fsmToAdd = new LearnerGraphND(Configuration.getDefaultConfiguration());
		helperAddToGraph0_1(fsm, fsmSrc,fsmToAdd);
	}

	private final <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
		void helperAddToGraph0_1(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsm,
				AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmSrc,
				AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmToAdd) throws IncompatibleStatesException
	{
		fsmToAdd.getInit().setColour(JUConstants.BLUE);fsmToAdd.getInit().setHighlight(true);
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = AbstractPathRoutines.addToGraph(fsm, fsmToAdd,oldToNew);
		Assert.assertNull(WMethod.checkM(fsmSrc, fsm));Assert.assertTrue(fsm.pairCompatibility.compatibility.isEmpty());
		LearnerGraph addedPartOfFsm = fsm.pathroutines.buildDeterministicGraph(newA);
		Assert.assertNull(WMethod.checkM(fsmToAdd,addedPartOfFsm));
		Assert.assertEquals(JUConstants.BLUE, newA.getColour());
		Assert.assertTrue(newA.isHighlight());
		Assert.assertEquals(1, oldToNew.size());Assert.assertSame(newA, oldToNew.get(fsmToAdd.getInit()));
		
		Assert.assertFalse(fsm.transitionMatrix.get(fsm.getInit()).isEmpty());
		Assert.assertTrue(fsm.transitionMatrix.get(newA).isEmpty());
	}

	@Test
	public final void testAddToGraph0_2() throws IncompatibleStatesException
	{
		LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		helperAddToGraph0_2(fsm, fsmSrc,fsmToAdd);
	}
	
	@Test
	public final void testAddToGraph0_2_ND() throws IncompatibleStatesException
	{
		LearnerGraphND fsm = new LearnerGraphND(Configuration.getDefaultConfiguration());
		LearnerGraphND fsmSrc = new LearnerGraphND(Configuration.getDefaultConfiguration());
		LearnerGraphND fsmToAdd = buildLearnerGraphND(relabelFSM_ND, "testRelabel1",Configuration.getDefaultConfiguration());
		helperAddToGraph0_2(fsm, fsmSrc,fsmToAdd);
	}
	
	private final <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
	void helperAddToGraph0_2(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsm,
			AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmSrc,
			AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmToAdd) throws IncompatibleStatesException
	{
		fsmToAdd.getInit().setColour(JUConstants.BLUE);fsmToAdd.getInit().setHighlight(true);
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = AbstractPathRoutines.addToGraph(fsm, fsmToAdd,oldToNew);
		
		Assert.assertNull(WMethod.checkM(fsmSrc, fsm));Assert.assertTrue(fsm.pairCompatibility.compatibility.isEmpty());
		LearnerGraph addedPartOfFsm = fsm.pathroutines.buildDeterministicGraph(newA);
		Assert.assertNull(WMethod.checkM(fsmToAdd,addedPartOfFsm));
		Assert.assertTrue(fsm.transitionMatrix.get(fsm.getInit()).isEmpty());
		Assert.assertFalse(fsm.transitionMatrix.get(newA).isEmpty());

		Assert.assertEquals(fsmToAdd.getStateNumber(), oldToNew.size());
		Assert.assertSame(newA, oldToNew.get(fsmToAdd.getInit()));
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

		Assert.assertNull(WMethod.checkM(fsmSrc, fsm));Assert.assertTrue(fsm.pairCompatibility.compatibility.isEmpty());
		LearnerGraph addedPartOfFsm = fsm.pathroutines.buildDeterministicGraph(newA);
		Assert.assertNull(WMethod.checkM(fsmToAdd,addedPartOfFsm));
		
		Assert.assertTrue(fsm.transitionMatrix.get(fsm.getInit()).isEmpty());
		Assert.assertTrue(fsm.transitionMatrix.get(newA).isEmpty());

		Assert.assertEquals(fsmToAdd.getStateNumber(), oldToNew.size());
		Assert.assertSame(newA, oldToNew.get(fsmToAdd.getInit()));
		for(CmpVertex oldVert:fsmToAdd.transitionMatrix.keySet())
			Assert.assertNull(WMethod.checkM(fsm,oldToNew.get(oldVert),fsmSrc,oldVert,WMethod.VERTEX_COMPARISON_KIND.NONE));
	}

	@Test
	public final void testAddToGraph1() throws IncompatibleStatesException
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = buildLearnerGraph("A-a->B-a-#Q", "testAddToGraph1",Configuration.getDefaultConfiguration());
		helperAddToGraph1(fsm, fsmSrc,fsmToAdd);
	}
	
	@Test
	public final void testAddToGraph1_ND() throws IncompatibleStatesException
	{
		LearnerGraphND fsm = buildLearnerGraphND(relabelFSM_ND, "testRelabel1",Configuration.getDefaultConfiguration());
		LearnerGraphND fsmSrc = buildLearnerGraphND(relabelFSM_ND, "testRelabel1",Configuration.getDefaultConfiguration());
		LearnerGraphND fsmToAdd = buildLearnerGraphND("A-a->B-a-#Q", "testAddToGraph1",Configuration.getDefaultConfiguration());
		helperAddToGraph1(fsm, fsmSrc,fsmToAdd);
	}
	
	private final <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
	void helperAddToGraph1(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsm,
			AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmSrc,
			AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> fsmToAdd) throws IncompatibleStatesException
	{
		fsmToAdd.getInit().setColour(JUConstants.BLUE);fsmToAdd.getInit().setHighlight(true);
		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = AbstractPathRoutines.addToGraph(fsm, fsmToAdd,oldToNew);

		Assert.assertNull(WMethod.checkM(fsmSrc, fsm));
		LearnerGraph addedPartOfFsm = fsm.pathroutines.buildDeterministicGraph(newA);
		Assert.assertNull(WMethod.checkM(fsmToAdd,addedPartOfFsm));
		Assert.assertEquals(JUConstants.BLUE, newA.getColour());
		Assert.assertTrue(newA.isHighlight());

		Assert.assertEquals(fsmToAdd.getStateNumber(), oldToNew.size());
		Assert.assertSame(newA, oldToNew.get(fsmToAdd.getInit()));
		for(CmpVertex oldVert:fsmToAdd.transitionMatrix.keySet())
			Assert.assertNull(WMethod.checkM(fsm,oldToNew.get(oldVert),fsmToAdd,oldVert,WMethod.VERTEX_COMPARISON_KIND.NONE));
	}
	
	@Test
	public final void testAddToGraph2()
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		LearnerGraph fsmSrc = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		LearnerGraph fsmToAdd = buildLearnerGraph("A-a->B-a->Q", "testAddToGraph1",Configuration.getDefaultConfiguration());

		Map<CmpVertex,CmpVertex> oldToNew = new TreeMap<CmpVertex,CmpVertex>();
		CmpVertex newA = AbstractPathRoutines.addToGraph(fsm, fsmToAdd,oldToNew);

		Assert.assertNull(WMethod.checkM(fsmSrc,fsmSrc.getInit(), fsm,fsm.getInit(),WMethod.VERTEX_COMPARISON_KIND.NONE));
		Assert.assertNull(WMethod.checkM(fsmToAdd,fsmToAdd.getInit(),fsm,newA,WMethod.VERTEX_COMPARISON_KIND.NONE));
		
		StatePair whatToMerge = new StatePair(fsm.getInit(),newA);
		LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		Assert.assertTrue(0 < fsm.pairscores.computePairCompatibilityScore_general(whatToMerge,collectionOfVerticesToMerge));
		LearnerGraph result = MergeStates.mergeAndDeterminize_general(fsm, whatToMerge,collectionOfVerticesToMerge);
		Assert.assertNull(WMethod.checkM(fsmSrc,fsmSrc.getInit(),result,result.getInit(),WMethod.VERTEX_COMPARISON_KIND.NONE));

		Assert.assertEquals(fsmToAdd.getStateNumber(), oldToNew.size());
		Assert.assertSame(newA, oldToNew.get(fsmToAdd.getInit()));
		for(CmpVertex oldVert:fsmToAdd.transitionMatrix.keySet())
			Assert.assertNull(WMethod.checkM(fsm,oldToNew.get(oldVert),fsmToAdd,oldVert,WMethod.VERTEX_COMPARISON_KIND.NONE));
	}

	@Test
	public final void testAddToGraphIncompatibles()
	{
		LearnerGraphND fsm = buildLearnerGraphND(relabelFSM_ND, "testRelabel1",Configuration.getDefaultConfiguration());
		CmpVertex oldA = fsm.findVertex(VertexID.parseID("A")), oldB = fsm.findVertex(VertexID.parseID("B")), oldC= fsm.findVertex(VertexID.parseID("C"));
		fsm.addToCompatibility(oldB,oldC,JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphND fsmSrc = buildLearnerGraphND(relabelFSM_ND, "testRelabel1",Configuration.getDefaultConfiguration());
		fsmSrc.addToCompatibility(fsmSrc.findVertex(VertexID.parseID("B")), fsmSrc.findVertex(VertexID.parseID("C")),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphND fsmToAdd = buildLearnerGraphND("A-a->B-a-#Q", "testAddToGraph1",Configuration.getDefaultConfiguration());
		CmpVertex newB=fsmToAdd.findVertex(VertexID.parseID("B")), newA=fsmToAdd.findVertex(VertexID.parseID("A"));
		fsmToAdd.addToCompatibility(newB,newA,JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		
		fsmToAdd.getInit().setColour(JUConstants.BLUE);fsmToAdd.getInit().setHighlight(true);
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
		Assert.assertEquals(4,fsm.pairCompatibility.compatibility.size());
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
		LearnerGraph fsm = buildLearnerGraph(relabelFSM.replaceAll("A", AbstractPersistence.Initial+"_str"), "testRelabel1",Configuration.getDefaultConfiguration());
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
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		StringWriter writer = new StringWriter();
		fsm.storage.writeGraphML(writer);
		Assert.assertEquals(removeWhiteSpace(graphml_beginning+graphml_ending),
				removeWhiteSpace(writer.toString()));
	}
	
	@Test
	public final void testGraphMLwriter2() throws IOException
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
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
	public final void testIntToRelation()
	{
		Assert.assertEquals(JUConstants.PAIRCOMPATIBILITY.compatibilityToJUConstants(JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE.getInteger()), JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertEquals(JUConstants.PAIRCOMPATIBILITY.compatibilityToJUConstants(JUConstants.PAIRCOMPATIBILITY.MERGED.getInteger()), JUConstants.PAIRCOMPATIBILITY.MERGED);
		Assert.assertEquals(JUConstants.PAIRCOMPATIBILITY.compatibilityToJUConstants(JUConstants.PAIRCOMPATIBILITY.THEN.getInteger()), JUConstants.PAIRCOMPATIBILITY.THEN);
	}
	
	@Test
	public final void testIntToRelation_fail()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			JUConstants.PAIRCOMPATIBILITY.compatibilityToJUConstants(JUConstants.intUNKNOWN);
		}}, IllegalArgumentException.class,"not a valid");
	}
	
	@Test
	public final void testGraphMLwriter_incompatible1() throws IOException
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("C"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
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
				TestWriteReadPair.pairToXML(new PairScore(fsm.findVertex("A"),fsm.findVertex("B"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE.getInteger(),JUConstants.intUNKNOWN))+"\n"+
				TestWriteReadPair.pairToXML(new PairScore(fsm.findVertex("B"),fsm.findVertex("C"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE.getInteger(),JUConstants.intUNKNOWN))+"\n"+
				"</"+AbstractPersistence.graphmlData+">"+
				graphML_end),
				removeWhiteSpace(writer.toString()));
	}
	
	/** Similar to the one above but with different values of compatibility. */
	@Test
	public final void testGraphMLwriter_incompatible2() throws IOException
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("C"),JUConstants.PAIRCOMPATIBILITY.MERGED);
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
				TestWriteReadPair.pairToXML(new PairScore(fsm.findVertex("A"),fsm.findVertex("B"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE.getInteger(),JUConstants.intUNKNOWN))+"\n"+
				TestWriteReadPair.pairToXML(new PairScore(fsm.findVertex("B"),fsm.findVertex("C"),JUConstants.PAIRCOMPATIBILITY.MERGED.getInteger(),JUConstants.intUNKNOWN))+"\n"+
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
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
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
		Assert.assertEquals(fsm.getInit(), actual.getInit());
	}
	
	@Test
	public final void testGraphMLwriter_loadnode2()
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
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
		Assert.assertEquals(fsm.getInit(), actual.getInit());
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
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loadLearnerGraph(document.createElement("junk"),Configuration.getDefaultConfiguration());
		}},IllegalArgumentException.class,"element name junk");
	}

	/** No graph element. */
	@Test
	public final void testGraphMLwriter_loadnode_fail2a()
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
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
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loadLearnerGraph(elem,Configuration.getDefaultConfiguration());
		}},IllegalArgumentException.class,"absent graph element");
	}
	
	/** No graph element. */
	@Test
	public final void testGraphMLwriter_loadnode_fail2b()
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
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
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loadLearnerGraph(elem,Configuration.getDefaultConfiguration());
		}},IllegalArgumentException.class,"absent graph element");
	}
	
	/** Duplicate graph elements. */
	@Test
	public final void testGraphMLwriter_loadnode_fail2c()
	{
		LearnerGraph fsm = buildLearnerGraph(relabelFSM, "testRelabel1",Configuration.getDefaultConfiguration());
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
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
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
		Assert.assertNull(WMethod.checkM(gr,gr.getInit(),loaded,loaded.getInit(),VERTEX_COMPARISON_KIND.DEEP));
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
		LearnerGraph gr = buildLearnerGraph("A-a->B", "testConvertToNumeric2",Configuration.getDefaultConfiguration());
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
		LearnerGraph gr = buildLearnerGraph("A-a-#B\nA-b->C-a->D-a-#E\nD-b->A\nC-c->C", "testConvertToNumeric2",Configuration.getDefaultConfiguration());
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
		LearnerGraph fsm = buildLearnerGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1",Configuration.getDefaultConfiguration());
		checkLoading(fsm);
	}
	
	@Test
	public final void testGraphMLWriter4() throws IOException
	{
		LearnerGraph fsm = buildLearnerGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1",Configuration.getDefaultConfiguration());
		fsm.findVertex("BD2").setHighlight(true);
		fsm.findVertex("BB1").setAccept(false);fsm.findVertex("BB1").setColour(JUConstants.RED);
		fsm.findVertex("B").setColour(JUConstants.RED);
		fsm.findVertex("A").setColour(JUConstants.BLUE);
		checkLoading(fsm);
	}

	@Test
	public final void testGraphMLWriter5() throws IOException
	{
		LearnerGraph fsm = buildLearnerGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1",Configuration.getDefaultConfiguration());
		checkLoading(fsm);
	}
	
	@Test
	public final void testGraphMLWriter6() throws IOException
	{
		LearnerGraph fsm = buildLearnerGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1",Configuration.getDefaultConfiguration());
		checkLoading(fsm);
	}

	/** Loading graphs with reject pairs. */
	@Test
	public final void testGraphMLWriter_incompatible1() throws IOException
	{
		LearnerGraph fsm = buildLearnerGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1",Configuration.getDefaultConfiguration());
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("C"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		checkLoading(fsm);
	}
	
	/** Loading graphs with reject pairs. */
	@Test
	public final void testGraphMLWriter_incompatible2() throws IOException
	{
		LearnerGraph fsm = buildLearnerGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1",Configuration.getDefaultConfiguration());
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		fsm.addToCompatibility(fsm.findVertex("B"), fsm.findVertex("C"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		checkLoading(fsm);
	}
	
	/** Loading graphs with reject pairs. */
	@Test
	public final void testGraphMLWriter_incompatible3() throws IOException
	{
		LearnerGraph fsm = buildLearnerGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1",Configuration.getDefaultConfiguration());
		for(CmpVertex vert:fsm.transitionMatrix.keySet())
			for(CmpVertex vert2:fsm.transitionMatrix.keySet())
				if (vert != vert2)
					fsm.addToCompatibility(vert,vert2,JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
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
		LearnerGraph fsm = buildLearnerGraph(FSMq0,"testGraphMLwriter_loadnode_noVERTEX",Configuration.getDefaultConfiguration());
		fsm.findVertex("C").setDepth(5);
		LearnerGraph loaded = new LearnerGraph(Configuration.getDefaultConfiguration().copy());
		AbstractPersistence.loadGraph(new StringReader(text), loaded);

		Assert.assertTrue(!fsm.pathroutines.checkUnreachableStates());Assert.assertTrue(!loaded.pathroutines.checkUnreachableStates());
		Assert.assertNull(WMethod.checkM(fsm,fsm.getInit(),loaded,loaded.getInit(),VERTEX_COMPARISON_KIND.DEEP));
		for(Entry<CmpVertex,LinkedList<Label>> entry:fsm.pathroutines.computeShortPathsToAllStates().entrySet())
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
		
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			AbstractPersistence.loadGraph(new StringReader(text), loaded);}}, 
		IllegalArgumentException.class, "vertices B and q0 are both");
	}

	@Test
	public final void testGraphMLWriter_fail_on_load_boolean() throws IOException
	{
		final LearnerGraph gr = buildLearnerGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1",Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	checkForCorrectException(new whatToRun() { public @Override void run() {
	    		loadLearnerGraph(new StringReader(writer.toString().replace("ACCEPTED=\"false\"", "ACCEPTED=\"aa\"")),Configuration.getDefaultConfiguration());
	    	}},IllegalUserDataException.class,"invalid colour");
		}		
	}
	
	@Test
	public final void testGraphMLWriter_fail_on_load_colour() throws IOException
	{
		LearnerGraph gr = buildLearnerGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1",Configuration.getDefaultConfiguration());
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
		LearnerGraph gr = buildLearnerGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1",Configuration.getDefaultConfiguration());
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
		final LearnerGraph gr = buildLearnerGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1",Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	checkForCorrectException(new whatToRun() { public @Override void run() {
	    		loadLearnerGraph(new StringReader(writer.toString().replace(AbstractPersistence.graphmlDataIncompatible, "AA")),Configuration.getDefaultConfiguration());
	    	}},IllegalArgumentException.class,"unexpected key");
		}		
	}
	
	/** Unknown state in a pair. */
	@Test
	public final void testGraphMLWriter_fail_on_load_pairs_unknownstate1() throws IOException
	{
		final LearnerGraph gr = buildLearnerGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1",Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	checkForCorrectException(new whatToRun() { public @Override void run() {
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
		final LearnerGraph gr = buildLearnerGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1",Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	checkForCorrectException(new whatToRun() { public @Override void run() {
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
		final LearnerGraph gr = buildLearnerGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1",Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	checkForCorrectException(new whatToRun() { public @Override void run() {
	    		loadLearnerGraph(new StringReader(writer.toString().replace(
	    				StatechumXML.ATTR_SCORE.name()+"=\""+JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE.getInteger(),
    				StatechumXML.ATTR_SCORE.name()+"=\"6")),Configuration.getDefaultConfiguration());
	    	}},IllegalArgumentException.class,"6 is not a valid compatibility");
		}		
	}
	
	/** Invalid compatibility code in a pair. */
	@Test
	public final void testGraphMLWriter_fail_on_load_pairs_unknowncompatibilitycode2() throws IOException
	{
		final LearnerGraph gr = buildLearnerGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1",Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	checkForCorrectException(new whatToRun() { public @Override void run() {
	    		loadLearnerGraph(new StringReader(writer.toString().replace(
	    				StatechumXML.ATTR_SCORE.name()+"=\""+JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE.getInteger(),
    				StatechumXML.ATTR_SCORE.name()+"=\"AA")),Configuration.getDefaultConfiguration());
	    	}},IllegalArgumentException.class,"failed to read a score in a pair");
		}		
	}
	
	@Test
	public final void testGraphMLWriter_fail_on_load_invalid_node() throws IOException
	{
		final LearnerGraph gr = buildLearnerGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1",Configuration.getDefaultConfiguration());
		final StringWriter writer = new StringWriter();
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("A"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		gr.addToCompatibility(gr.findVertex("B"), gr.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		gr.storage.writeGraphML(writer);
		synchronized (AbstractLearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	
	    	checkForCorrectException(new whatToRun() { public @Override void run() {
	    		loadLearnerGraph(new StringReader(writer.toString().replace(AbstractPersistence.graphmlData, "AA")),Configuration.getDefaultConfiguration());
	    	}},IllegalArgumentException.class,"unexpected node");
		}		
	}
	
	@Test
	public final void testGraphMLWriter_load_despite_Initial() throws IOException
	{
		LearnerGraph gr = buildLearnerGraph(TestRpniLearner.largeGraph1_invalid5, "testMerge_fail1",Configuration.getDefaultConfiguration());
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

	@Test
	public final void testCountMatchedTransitions1()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = buildLearnerGraph("A-a->B-a->C-a->D-a->A","testCountMatchedTransitions1big",config);
		LearnerGraph small = buildLearnerGraph("A-a->A","testCountMatchedTransitions1small",config);
		
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(4,Transform.countSharedTransitions(big, small).matched);
	}
	
	@Test
	public final void testCountMatchedTransitions2()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = buildLearnerGraph("A-a->B-a->C-a->D-a->A","testCountMatchedTransitions1big",config);
		LearnerGraph small = new LearnerGraph(config);
		
		Assert.assertEquals(4,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).matched);
	}

	@Test
	public final void testCountMatchedTransitions3()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = buildLearnerGraph("A-a->B-a->C-a->D-a->A\nB-b->B","testCountMatchedTransitions3big",config);
		LearnerGraph small = buildLearnerGraph("A-a->A","testCountMatchedTransitions3small",config);
		
		Assert.assertEquals(1,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(4,Transform.countSharedTransitions(big, small).matched);
	}

	@Test
	public final void testCountMatchedTransitions4()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = buildLearnerGraph("A-a->B-a->A\nB-c->C-b->C","testCountMatchedTransitions4big",config);
		LearnerGraph small = buildLearnerGraph("A-a->A","testCountMatchedTransitions4small",config);
		
		Assert.assertEquals(2,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(2,Transform.countSharedTransitions(big, small).matched);
	}

	@Test
	public final void testCountMatchedTransitions5()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = buildLearnerGraph("A-a->B-a->A\nB-c->C-b->C","testCountMatchedTransitions4big",config);
		LearnerGraph small = buildLearnerGraph("A-a->B-a->A\nB-c->D-b->E-b->F","testCountMatchedTransitions5small",config);
		
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(5,Transform.countSharedTransitions(big, small).matched);
	}

	/** Tests identification of self-loops. */
	@Test
	public final void testCountMatchedTransitions6()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = buildLearnerGraph("A-a->A","testCountMatchedTransitions4big",config);
		LearnerGraph small = buildLearnerGraph("A-a->B-a->A","testCountMatchedTransitions5small",config);
		
		Assert.assertEquals(1,Transform.countSharedTransitions(big, small).Tx);
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(2,Transform.countSharedTransitions(big, small).matched);
	}

	/** Tests identification of self-loops. */
	@Test
	public final void testCountMatchedTransitions7a()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = buildLearnerGraph("A-b->A-a->B-c->B","testCountMatchedTransitions7big",config);
		LearnerGraph small = buildLearnerGraph("A-b->B-a->C-c->D","testCountMatchedTransitions7small",config);
		
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).Tx);
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(3,Transform.countSharedTransitions(big, small).matched);
	}

	/** Tests identification of self-loops. */
	@Test
	public final void testCountMatchedTransitions7b()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = buildLearnerGraph("A-b->A-a->B-c->B","testCountMatchedTransitions7big",config);
		LearnerGraph small = buildLearnerGraph("A-b->A2-b->A3-b->B-a->C-c->D","testCountMatchedTransitions7bsmall",config);
		
		Assert.assertEquals(2,Transform.countSharedTransitions(big, small).Tx);
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(5,Transform.countSharedTransitions(big, small).matched);
	}

	/** Tests identification of self-loops. */
	@Test
	public final void testCountMatchedTransitions8()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph big = buildLearnerGraph("A-b->A-a->B-c->B-a->A","testCountMatchedTransitions8big",config);
		LearnerGraph small = buildLearnerGraph("A-b->B-a->C-c->D-c->E-a->F-b->G","testCountMatchedTransitions8small",config);
		
		Assert.assertEquals(2,Transform.countSharedTransitions(big, small).Tx);
		Assert.assertEquals(0,Transform.countSharedTransitions(big, small).Nx);
		Assert.assertEquals(6,Transform.countSharedTransitions(big, small).matched);
	}

	@Test
	public final void testCountMatchedTransitions_fail1()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph big = buildLearnerGraph("A-a->B-a->C-a->D-a->A","testCountMatchedTransitions_fail1big",config);
		final LearnerGraph small = new LearnerGraph(config);small.getInit().setAccept(false);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			Transform.countSharedTransitions(big, small);
		}},DifferentFSMException.class,"have a different acceptance");
	}

	@Test
	public final void testCountMatchedTransitions_fail2()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph big = buildLearnerGraph("A-a->B-a->C-a->D-a->A","testCountMatchedTransitions1big",config);
		final LearnerGraph small = buildLearnerGraph("A-a->B-a-#C","testCountMatchedTransitions_fail2small",config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			Transform.countSharedTransitions(big, small);
		}},DifferentFSMException.class,"have a different acceptance");
	}
	
	@Test
	public final void testCountMatchedTransitions_fail3()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph big = buildLearnerGraph("A-a->B-a->C-a->D-a->A","testCountMatchedTransitions_fail3big",config);
		final LearnerGraph small = buildLearnerGraph("A-a->B-a->A\nB-b->C","testCountMatchedTransitions_fail3small",config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			Transform.countSharedTransitions(big, small);
		}},IllegalArgumentException.class,"not contained");
	}
	
	@Test
	public final void testCountMatchedTransitions_fail4()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph big = buildLearnerGraph("A-a->B-a->A\nB-c->C-b->C","testCountMatchedTransitions4big",config);
		final LearnerGraph small = buildLearnerGraph("A-a->B-a->C-a->D\nB-c->D-c->D","testCountMatchedTransitions_fail4small",config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			Transform.countSharedTransitions(big, small);
		}},IllegalArgumentException.class,"small graph is not contained in the large one, from [ C, D ] unmatched transition c to (nothing_in_big,D)");
	}
	
	@Test
	public final void testCountMatchedTransitions_fail5()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph big = buildLearnerGraph("A-a->B-a->A\nB-c->C-b->C","testCountMatchedTransitions4big",config);
		final LearnerGraph small = buildLearnerGraph("A-a->B-a->C-a->A\nB-c->D-b->E-b->F","testCountMatchedTransitions_fail5small",config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			Transform.countSharedTransitions(big, small);
		}},IllegalArgumentException.class,"small graph is not contained in the large one, from [ A, B ] unmatched transition c to (nothing_in_big,D)");
	}

	@Test
	public final void testQuanteKoschke1()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		Assert.assertEquals(0.24,
			Transform.QuanteKoschkeDifference(
				buildLearnerGraph("A-create->B-push->B","testQuanteKoschke1a",config),
				buildLearnerGraph("A-create->B-pop->D","testQuanteKoschke1b",config)),
			0.01
			);
	}
	
	/** Unmatched loops. */
	@Test
	public final void testQuanteKoschke2()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph grA=buildLearnerGraph("A-a->B-a->D-a->A\nA-b->B\nD-e->D\nB-f->B", "testQuanteKoschke2A",config);
		LearnerGraph grB=buildLearnerGraph("A-a->C-a->E-a->C\nA-b->C-q->C\nE-p->E", "testQuanteKoschke2B",config);
		Assert.assertEquals(0.25,
			Transform.QuanteKoschkeDifference(grA,grB),
			0.01
			);
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		//Visualiser.updateFrame(grA, grB);Visualiser.waitForKey();
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, 1, counter,config);
		double ourDifference = ((double)counter.getAdded()+counter.getRemoved())/(grA.pathroutines.countEdges()+grB.pathroutines.countEdges());
		//System.out.println("removed: "+counter.getRemoved()+" out of "+grA.countEdges()+", added: "+counter.getAdded()+" to produce "+grB.countEdges()+"; our difference: "+our);
		Assert.assertEquals(0.5, ourDifference,0.01);
	}
	
	/** Matched loops. */
	@Test
	public final void testQuanteKoschke3()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph grA=buildLearnerGraph("A-a->B-a->D-a->A\nA-b->B\nD-e->D\nB-f->B", "testQuanteKoschke2A",config);
		LearnerGraph grB=buildLearnerGraph("A-a->C-a->E-a->A\nA-b->C-q->C\nE-p->E", "testQuanteKoschke3B",config);
		Assert.assertEquals(0.23,
			Transform.QuanteKoschkeDifference(grA,grB),
			0.01
			);
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		//Visualiser.updateFrame(grA, grB);Visualiser.waitForKey();
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, 1, counter,config);
		double ourDifference = ((double)counter.getAdded()+counter.getRemoved())/(grA.pathroutines.countEdges()+grB.pathroutines.countEdges());
		//System.out.println("removed: "+counter.getRemoved()+" out of "+grA.countEdges()+", added: "+counter.getAdded()+" to produce "+grB.countEdges()+"; our difference: "+our);
		Assert.assertEquals(0.33, ourDifference,0.01);
	}
	
	@Test
	public final void testInterpretLabelsOnGraph0()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = new LearnerGraph(config);
		final LearnerGraph outcome = graph.transform.interpretLabelsOnGraph(graph.pathroutines.computeAlphabet(),true,null);
		DifferentFSMException ex= WMethod.checkM_and_colours(graph, outcome, VERTEX_COMPARISON_KIND.NONE);
		Assert.assertNull(ex==null?"":ex.toString(),ex);
	}
	
	@Test
	public final void testInterpretLabelsOnGraph1()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B-a->A\nB-c->C-b->C","testCountMatchedTransitions4big",config);
		final LearnerGraph outcome = graph.transform.interpretLabelsOnGraph(graph.pathroutines.computeAlphabet(),true,null);
		DifferentFSMException ex= WMethod.checkM_and_colours(graph, outcome, VERTEX_COMPARISON_KIND.DEEP);
		Assert.assertNull(ex==null?"":ex.toString(),ex);
	}
	
	@Test
	public final void testInterpretLabelsOnGraph2()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a || c->B-a->A\nB-c->C-b->C","testInterpretLabelsOnGraph2a",config);
		final LearnerGraph expected = buildLearnerGraph("A-a->B\nA-c->B-a->A\nB-c->C-b->C","testInterpretLabelsOnGraph2b",config);
		final LearnerGraph outcome = graph.transform.interpretLabelsOnGraph(graph.pathroutines.computeAlphabet(),true,null);
		DifferentFSMException ex= WMethod.checkM_and_colours(expected, outcome, VERTEX_COMPARISON_KIND.DEEP);
		Assert.assertNull(ex==null?"":ex.toString(),ex);
	}
	
	@Test
	public final void testInterpretLabelsOnGraph3()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B-a->A\nB-c->C-c && a->C","testInterpretLabelsOnGraph3a",config);
		final LearnerGraph expected = buildLearnerGraph("A-a->B-a->A\nB-c->C","testInterpretLabelsOnGraph3b",config);
		final LearnerGraph outcome = graph.transform.interpretLabelsOnGraph(graph.pathroutines.computeAlphabet(),true,null);
		DifferentFSMException ex= WMethod.checkM_and_colours(expected, outcome, VERTEX_COMPARISON_KIND.DEEP);
		Assert.assertNull(ex==null?"":ex.toString(),ex);
	}
	
	@Test
	public final void testInterpretLabelsOnGraph4()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B-a->A\nB-c->C-1->C","testInterpretLabelsOnGraph3a",config);
		final LearnerGraph expected = buildLearnerGraph("A-a->B-a->A\nB-c->C-a->C-c->C","testInterpretLabelsOnGraph3b",config);
		Set<Label> alphabet = graph.pathroutines.computeAlphabet();alphabet.remove(AbstractLearnerGraph.generateNewLabel("1", config));
		final LearnerGraph outcome = graph.transform.interpretLabelsOnGraph(alphabet,true,null);
		DifferentFSMException ex= WMethod.checkM_and_colours(expected, outcome, VERTEX_COMPARISON_KIND.DEEP);
		Assert.assertNull(ex==null?"":ex.toString(),ex);
	}
	
	@Test
	public final void testInterpretLabelsOnGraph5()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B-a->A\nB-c->C-(1 && (a || b))->C","testInterpretLabelsOnGraph3a",config);
		final LearnerGraph expected = buildLearnerGraph("A-a->B-a->A\nB-c->C-a->C-b->C","testInterpretLabelsOnGraph3b",config);
		Set<Label> alphabet = graph.pathroutines.computeAlphabet();alphabet.remove(AbstractLearnerGraph.generateNewLabel("1", config));alphabet.add(AbstractLearnerGraph.generateNewLabel("b",config));
		final LearnerGraph outcome = graph.transform.interpretLabelsOnGraph(alphabet,true,null);
		DifferentFSMException ex= WMethod.checkM_and_colours(expected, outcome, VERTEX_COMPARISON_KIND.DEEP);
		Assert.assertNull(ex==null?"":ex.toString(),ex);
	}
	
	@Test
	public final void testInterpretLabelsOnGraph6()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("I-load->A / A- !exit && !close ->A-close->I / R-close->P-edit || save || close-#R1 / P-load->A1 / A==THEN==R","testInterpretLabelsOnGraph6a",config);
		final LearnerGraph expected = buildLearnerGraph("I-load->A / A-edit->A-save->A-load->A-close->I / R-close->P-edit-#R1 / P-save-#R2 / P-load->A1 / P-close-#R4 / A==THEN==R","testInterpretLabelsOnGraph6b",config);
		Set<Label> alphabet = new TreeSet<Label>();alphabet.addAll(AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"load","save","edit","close","exit"}),config));
		final LearnerGraph outcome = graph.transform.interpretLabelsOnGraph(alphabet,true,null);
		expected.addTransition(expected.transitionMatrix.get(expected.findVertex(VertexID.parseID("A"))), AbstractLearnerGraph.generateNewLabel("to_R",config),expected.findVertex(VertexID.parseID("R")));
		outcome.addTransition(outcome.transitionMatrix.get(outcome.findVertex(VertexID.parseID("A"))), AbstractLearnerGraph.generateNewLabel("to_R",config),outcome.findVertex(VertexID.parseID("R")));
		//Visualiser.updateFrame(PathRoutines.convertPairAssociationsToTransitions(expected,config), PathRoutines.convertPairAssociationsToTransitions(outcome,config));
		DifferentFSMException ex= WMethod.checkM_and_colours(expected, outcome, VERTEX_COMPARISON_KIND.NONE);
		Assert.assertNull(ex==null?"":ex.toString(),ex);
	}
	
	@Test
	public final void testInterpretLabelsOnGraph7()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-!exit->A / R-exit->P-1-#R1 / A==THEN==R","testInterpretLabelsOnGraph7a",config);
		final LearnerGraph expected = buildLearnerGraph("A-edit->A-save->A-load->A-close->A / R-exit->P-edit-#R1 / P-save-#R2 / P-load-#R3 / P-exit-#R4 / P-close-#R5 / A==THEN==R","testInterpretLabelsOnGraph7b",config);
		Set<Label> alphabet = new TreeSet<Label>();alphabet.addAll(AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"load","save","edit","close","exit"}),config));
		final LearnerGraph outcome = graph.transform.interpretLabelsOnGraph(alphabet,true,null);
		expected.addTransition(expected.transitionMatrix.get(expected.findVertex(VertexID.parseID("A"))), AbstractLearnerGraph.generateNewLabel("to_R",config),expected.findVertex(VertexID.parseID("R")));
		outcome.addTransition(outcome.transitionMatrix.get(outcome.findVertex(VertexID.parseID("A"))), AbstractLearnerGraph.generateNewLabel("to_R",config),outcome.findVertex(VertexID.parseID("R")));
		DifferentFSMException ex= WMethod.checkM_and_colours(expected, outcome, VERTEX_COMPARISON_KIND.NONE);
		Assert.assertNull(ex==null?"":ex.toString(),ex);
	}
}
