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

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildGraph;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.rpnicore.LearnerGraph.StatesToConsider;

/* Note that most methods are tested by TestLTL_to_ba. */
public class TestTransitionMatrixND {
	@Before
	public final void beforeTest()
	{
		config = Configuration.getDefaultConfiguration().copy();		
		ba=new LTL_to_ba(config);ba.alphabet = new HashSet<String>();
		ba.alphabet.addAll(Arrays.asList(new String[]{"a","b","c"}));
		
		expectedFromASEExample = new LearnerGraph(TestFSMAlgo.buildGraph(
				"I-close->1\nI-edit->I1\nI-save->I1\nI-load->I1\n"+
				"1-load->I1-close->1\n"+
				"I1-edit->I1-save->I1-load->I1\n","testLTL_bigger"),config);
	}
	// ,"load","save","edit","close"
	protected Configuration config = null;
	protected LTL_to_ba ba = null;
	protected LearnerGraph expectedFromASEExample = null;
	
	/** Tests of adding to graph. */
	@Test
	public final void testLTL_add1()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-c->B-b->B", "testLTL_add1"),config);
		
		LearnerGraph result = TransitionMatrixND.UniteTransitionMatrices(new TransitionMatrixND(graph),graphToAdd).buildDeterministicGraph();
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B\n"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	/** Tests of adding to graph. */
	@Test
	public final void testLTL_add2()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1"),config);
		
		LearnerGraph result = TransitionMatrixND.UniteTransitionMatrices(new TransitionMatrixND(graph),graphToAdd).buildDeterministicGraph();
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	/** Tests of adding to graph. */
	@Test
	public final void testLTL_add3()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init-b->S", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1"),config);
		
		LearnerGraph result = TransitionMatrixND.UniteTransitionMatrices(new TransitionMatrixND(graph),graphToAdd).buildDeterministicGraph();
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"+
				"A-b->S"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	/** Tests of adding to graph. */
	@Test
	public final void testLTL_add4()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init-d->S", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1"),config);
		
		LearnerGraph result = TransitionMatrixND.UniteTransitionMatrices(new TransitionMatrixND(graph),graphToAdd).buildDeterministicGraph();
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}

	@Test
	public final void testLTL_add5()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("P-a->Q-a->S-a->U\nP-b->R-a->T", "testLTL_add5_A"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->B-a->B-b-#C", "testLTL_add5_B"),config);

		LearnerGraph result = TransitionMatrixND.UniteTransitionMatrices(new TransitionMatrixND(graph),graphToAdd).buildDeterministicGraph();
		Assert.assertNull(WMethod.checkM(graphToAdd,result));
	}
	
	/** A simple graph. */
	@Test
	public final void testConvertToND1()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A","testConvertToND2"),Configuration.getDefaultConfiguration());
		LearnerGraph actual = new LearnerGraph(new TransitionMatrixND(graph));
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph));
	}
	
	/** Empty graph. */
	@Test
	public final void testConvertToND2()
	{
		LearnerGraph graph = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph actual = new LearnerGraph(new TransitionMatrixND(graph));
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph));
	}
	
	/** Empty graph with a single state-reject state. */
	@Test
	public final void testConvertToND3()
	{
		LearnerGraph graph = new LearnerGraph(Configuration.getDefaultConfiguration());
		graph.init.setAccept(false);
		LearnerGraph actual = new LearnerGraph(new TransitionMatrixND(graph));
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph));
	}
	
	/** A not so simple graph. */
	@Test
	public final void testConvertToND4()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C","testConvertToND4"),Configuration.getDefaultConfiguration());
		LearnerGraph actual = new LearnerGraph(new TransitionMatrixND(graph));
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph));
	}
	
	/** A not so simple graph with multiple disconnected parts. One state has no outgoing or incoming transitions. */
	@Test
	public final void testConvertToND5()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C","testConvertToND5"),Configuration.getDefaultConfiguration());
		graph.transitionMatrix.put(AbstractTransitionMatrix.generateNewCmpVertex(new VertexID("S"), graph.config), graph.createNewRow());
		LearnerGraph actual = new LearnerGraph(new TransitionMatrixND(graph));
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph));
		Assert.assertNull(WMethod.checkM(graph, graph.findVertex("S"), actual, actual.findVertex("S")));
	}
	
	/** A not so simple graph with multiple disconnected parts. */
	@Test
	public final void testConvertToND6()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C\nU-a->V-a->U","testConvertToND6"),Configuration.getDefaultConfiguration());
		LearnerGraph actual = new LearnerGraph(new TransitionMatrixND(graph));
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph));
		Assert.assertNull(WMethod.checkM(graph, graph.findVertex("U"), actual, actual.findVertex("U")));
	}
	
	/** Given two ND graph, compares them but prior to that,
	 * sorts target states for each transition (this is needed to compare 
	 * such graphs without having to think about unreachable parts of a graph).
	 * Important: graphs are modified as a result of such a "comparison" since 
	 * I do not have support for cloning non-deterministic graphs - it is not 
	 * needed since they are usually just transient results of some computations.
	 * 
	 * @param A the first graph to compare.
	 * @param B the second graph to compare.
	 */
	private static void compareGraphs(TransitionMatrixND A, TransitionMatrixND B)
	{
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:A.transitionMatrix.entrySet())
			for(Entry<String,List<CmpVertex>> mapEntry:entry.getValue().entrySet())
			Collections.sort(mapEntry.getValue());
		
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:B.transitionMatrix.entrySet())
			for(Entry<String,List<CmpVertex>> mapEntry:entry.getValue().entrySet())
			Collections.sort(mapEntry.getValue());
		
		Assert.assertEquals(A.transitionMatrix,B.transitionMatrix);
		Assert.assertEquals(A.init,B.init);
	}
	
	/** A simple graph. */
	@Test
	public final void testBuildInverse1()
	{
		TransitionMatrixND graph = new TransitionMatrixND(TestFSMAlgo.buildGraph("A-a->A","testConvertToND2"),Configuration.getDefaultConfiguration());
		TransitionMatrixND expected = new TransitionMatrixND(TestFSMAlgo.buildGraph("A-a->A","testConvertToND2"),Configuration.getDefaultConfiguration());

		TransitionMatrixND actual = new TransitionMatrixND(Configuration.getDefaultConfiguration());
		TransitionMatrixND.buildInverse(graph, TransitionMatrixND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** Empty graph. */
	@Test
	public final void testBuildInverse2()
	{
		TransitionMatrixND graph = new TransitionMatrixND(Configuration.getDefaultConfiguration());graph.initPTA();
		TransitionMatrixND expected = new TransitionMatrixND(Configuration.getDefaultConfiguration());expected.initPTA();

		TransitionMatrixND actual = new TransitionMatrixND(Configuration.getDefaultConfiguration());
		TransitionMatrixND.buildInverse(graph, TransitionMatrixND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** Empty graph with a single state-reject state. */
	@Test
	public final void testBuildInverse3()
	{
		TransitionMatrixND graph = new TransitionMatrixND(Configuration.getDefaultConfiguration());graph.initPTA();
		graph.init.setAccept(false);
		TransitionMatrixND expected = new TransitionMatrixND(Configuration.getDefaultConfiguration());expected.initPTA();
		expected.init.setAccept(false);

		TransitionMatrixND actual = new TransitionMatrixND(Configuration.getDefaultConfiguration());
		TransitionMatrixND.buildInverse(graph, TransitionMatrixND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A not so simple graph. */
	@Test
	public final void testBuildInverse4()
	{
		TransitionMatrixND graph = new TransitionMatrixND(TestFSMAlgo.buildGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C",
				"testConvertToND4"),Configuration.getDefaultConfiguration());
		TransitionMatrixND expected = new TransitionMatrixND(TestFSMAlgo.buildGraph("A-c->B\nA-d->B\nC-c->A\nC-a->B\nD-b->B-a->A",
			"testConvertToND4"),Configuration.getDefaultConfiguration());
		expected.findVertex("D").setAccept(false);
		TransitionMatrixND actual = new TransitionMatrixND(Configuration.getDefaultConfiguration());
		TransitionMatrixND.buildInverse(graph, TransitionMatrixND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A not so simple graph with multiple disconnected parts. One state has no outgoing or incoming transitions. */
	@Test
	public final void testBuildInverse5()
	{
		TransitionMatrixND graph = new TransitionMatrixND(TestFSMAlgo.buildGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C",
			"testConvertToND5"),Configuration.getDefaultConfiguration());
		graph.transitionMatrix.put(AbstractTransitionMatrix.generateNewCmpVertex(new VertexID("S"), graph.config), graph.createNewRow());
		
		TransitionMatrixND expected = new TransitionMatrixND(TestFSMAlgo.buildGraph("A-c->B\nA-d->B\nC-c->A\nC-a->B\nD-b->B-a->A",
			"testConvertToND5"),Configuration.getDefaultConfiguration());
		expected.transitionMatrix.put(AbstractTransitionMatrix.generateNewCmpVertex(new VertexID("S"), graph.config), graph.createNewRow());
		
		TransitionMatrixND actual = new TransitionMatrixND(Configuration.getDefaultConfiguration());
		TransitionMatrixND.buildInverse(graph, TransitionMatrixND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A not so simple graph with multiple disconnected parts. */
	@Test
	public final void testBuildInverse6()
	{
		TransitionMatrixND graph = new TransitionMatrixND(TestFSMAlgo.buildGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C\nU-b->V-a->U",
			"testConvertToND6"),Configuration.getDefaultConfiguration());
		TransitionMatrixND expected = new TransitionMatrixND(TestFSMAlgo.buildGraph("A-c->B\nA-d->B\nC-c->A\nC-a->B\nD-b->B-a->A\nU-a->V-b->U",
			"testConvertToND6"),Configuration.getDefaultConfiguration());

		TransitionMatrixND actual = new TransitionMatrixND(Configuration.getDefaultConfiguration());
		TransitionMatrixND.buildInverse(graph, TransitionMatrixND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}

	/** A helper method to create an instance of a filter. The trouble with filters is that
	 * some need an instance of LearnerGraph and others do not. We hence try all of them.
	 * 
	 * @return an instance of filter.
	 */
	public static StatesToConsider createInstanceOfFilter(Class<? extends StatesToConsider> filterClass,LearnerGraph gr)
	{
		StatesToConsider filter = null;
		try {
			java.lang.reflect.Constructor<? extends StatesToConsider> constructor = filterClass.getConstructor(new Class []{});
			filter = constructor.newInstance(new Object[]{});
		} catch (Exception e) {
			// if we failed, filter stays at null, we'll try again.
		}
		
		if (filter == null)
			try {
				java.lang.reflect.Constructor<? extends StatesToConsider> constructor = filterClass.getConstructor(new Class []{LearnerGraph.class});
				filter = constructor.newInstance(new Object[]{gr});
			} catch (Exception e) {
				Assert.fail("failed to create an instance of a filter");
			}
		return filter;
	}
	
	/** Tests that main sets built for the supplied graph honour ignored states.
	 * 
	 * @param graph graph to consider
	 * @param graphName graph name
	 * @param filter which states to filter out
	 * @param expectedIgnored which states are expected to remain after filtering.
	 */
	private final void checkConsideringIgnoredStates(String graph, String graphName, Class<? extends StatesToConsider> filterClass, String [] expectedIgnored)
	{
		LearnerGraph gr=new LearnerGraph(buildGraph(graph,graphName),Configuration.getDefaultConfiguration());
		StatesToConsider filter = createInstanceOfFilter(filterClass, gr);
		for(boolean direction:new boolean[]{false,true})
		{
			LearnerGraphND ndGraph = new LearnerGraphND(gr,filter, direction);
			Map<CmpVertex,Integer> state_to_int_map = new TreeMap<CmpVertex,Integer>();
			CmpVertex [] numberToStateNoReject = gr.buildStateToIntegerMap(filter,state_to_int_map);
			Assert.assertEquals(state_to_int_map.size(), numberToStateNoReject.length);
			Collection<CmpVertex> states_int = state_to_int_map.keySet();
			Assert.assertEquals(ndGraph.getStateNumber(),state_to_int_map.values().size());
			Assert.assertArrayEquals(states_int.toArray(),numberToStateNoReject);
			
			Set<CmpVertex> expectedIgnoredStates = new TreeSet<CmpVertex>();for(String st:expectedIgnored) expectedIgnoredStates.add(gr.findVertex(st));
			Assert.assertEquals(states_int,expectedIgnoredStates);
			
			// The forward matrix is not filtered
			checkSource_Target_are_expected(ndGraph.matrixForward,gr.transitionMatrix.keySet());
			checkSource_Target_are_expected(ndGraph.matrixInverse,expectedIgnoredStates);
		}
	}
	
	private final void checkSource_Target_are_expected(TransitionMatrixND matrixND,
			Set<CmpVertex> expected)
	{
		Assert.assertEquals("source states mismatch",expected, matrixND.transitionMatrix.keySet());
		Set<CmpVertex> targetSet = new HashSet<CmpVertex>();
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:matrixND.transitionMatrix.entrySet())
			for(List<CmpVertex> list:entry.getValue().values())
				targetSet.addAll(list);
		// target states are supposed to be a subset of the main set of states, hence a subset of the expected states
		targetSet.removeAll(expected);
		Assert.assertTrue("target states mismatch",targetSet.isEmpty());
	}
	
	/** Tests that the transition matrices are built correctly by LearnerGraphND */
	@Test
	public final void TestDomainCompatibility1()
	{
		checkConsideringIgnoredStates("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R\nC-f-#T","TestFindIncompatibleStatesB",
				TransitionMatrixND.ignoreRejectStatesClass.class,new String[]{"A","Q","C","D"});
	}
	
	/** Tests that the transition matrices are built correctly by LearnerGraphND */
	@Test
	public final void TestDomainCompatibility2()
	{
		checkConsideringIgnoredStates("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R\nC-f-#T","TestFindIncompatibleStatesB",
				TransitionMatrixND.ignoreNoneClass.class,new String[]{"A","Q","C","D","R","T"});
	}
	
	/** Tests that the transition matrices are built correctly by LearnerGraphND */
	@Test
	public final void TestDomainCompatibility3()
	{
		checkConsideringIgnoredStates("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R\nC-f-#T","TestFindIncompatibleStatesB",
				TransitionMatrixND.ignoreZeroClass.class,new String[]{"A","C","D"});
	}

}
