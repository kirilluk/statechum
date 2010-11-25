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

import static statechum.analysis.learning.rpnicore.FsmParser.buildGraph;

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

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.Configuration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph.StatesToConsider;

/* Note that many methods are also tested by TestLTL_to_ba. */
public class TestLearnerGraphND {
	@Before
	public final void beforeTest()
	{
		config = Configuration.getDefaultConfiguration().copy();		
		ba=new LTL_to_ba(config);ba.alphabet = new HashSet<String>();
		ba.alphabet.addAll(Arrays.asList(new String[]{"a","b","c"}));
		
		expectedFromASEExample = new LearnerGraph(FsmParser.buildGraph(
				"I-close->1\nI-edit->I1\nI-save->I1\nI-load->I1\n"+
				"1-load->I1-close->1\n"+
				"I1-edit->I1-save->I1-load->I1\n","testLTL_bigger"),config);
	}
	// ,"load","save","edit","close"
	protected Configuration config = null;
	protected LTL_to_ba ba = null;
	protected LearnerGraph expectedFromASEExample = null;
	
	
	/** Tests that conversion from non-deterministic matrix to a deterministic one works. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testbuildDeterministicGraph_empty() throws IncompatibleStatesException
	{
		LearnerGraphND graph = new LearnerGraphND(config);graph.initEmpty();
		LearnerGraph expected = new LearnerGraph(config);expected.initEmpty();
		Assert.assertTrue(expected.equals(graph.pathroutines.buildDeterministicGraph()));
	}
	
	/** Tests that conversion from non-deterministic matrix to a deterministic one works. 
	 * A single accept-state.
	 * @throws IncompatibleStatesException */
	@Test
	public final void testbuildDeterministicGraph0a() throws IncompatibleStatesException
	{
		LearnerGraphND graph = new LearnerGraphND(config);
		LearnerGraph expected = new LearnerGraph(config);
		Assert.assertTrue(expected.equals(graph.pathroutines.buildDeterministicGraph()));
	}
	
	/** Tests that conversion from non-deterministic matrix to a deterministic one works. 
	 * A single reject-state.
	 * @throws IncompatibleStatesException */
	@Test
	public final void testbuildDeterministicGraph0b() throws IncompatibleStatesException
	{
		LearnerGraphND graph = new LearnerGraphND(config);graph.getInit().setAccept(false);
		LearnerGraph converted = graph.pathroutines.buildDeterministicGraph();
		Assert.assertTrue(converted.getInit().getID().equals(VertexID.parseID("P1000")));// the original name is preserved ...
		Assert.assertFalse(converted.getInit().isAccept());// as is the accept condition.
		Assert.assertEquals(1,converted.transitionMatrix.size());
		Assert.assertTrue(converted.transitionMatrix.get(converted.getInit()).isEmpty());
	}
	
	/** Tests that conversion from non-deterministic matrix to a deterministic one works. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testbuildDeterministicGraph1a() throws IncompatibleStatesException
	{
		LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->B\nA-a->C\nA-a->D", "testbuildDeterministicGraph1_a"),config);
		LearnerGraph expected = new LearnerGraph(FsmParser.buildGraph("A-a->B", "testbuildDeterministicGraph1_b"),config);
		Assert.assertTrue(expected.equals(graph.pathroutines.buildDeterministicGraph()));
	}
	
	/** Tests that conversion from non-deterministic matrix to a deterministic one works. 
	 * This is a minor variation on the above which checks that depth is honoured.
	 * @throws IncompatibleStatesException */
	@Test
	public final void testbuildDeterministicGraph1b() throws IncompatibleStatesException
	{
		LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->B\nA-a->C\nA-a->D", "testbuildDeterministicGraph1_a"),config);
		graph.findVertex(VertexID.parseID("C")).setDepth(3);
		LearnerGraph expected = new LearnerGraph(FsmParser.buildGraph("A-a->C", "testbuildDeterministicGraph1_b"),config);
		expected.findVertex(VertexID.parseID("C")).setDepth(3);
		Assert.assertTrue(expected.equals(graph.pathroutines.buildDeterministicGraph()));
	}
	
	/** Tests that conversion from non-deterministic matrix to a deterministic one works. 
	 * This is a minor variation on the above which checks that depth is not honoured if we say so.
	 * @throws IncompatibleStatesException */
	@Test
	public final void testbuildDeterministicGraph1c() throws IncompatibleStatesException
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setIgnoreDepthInTheChoiceOfRepresentatives(true);
		LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->B\nA-a->C\nA-a->D", "testbuildDeterministicGraph1_a"),conf);
		graph.findVertex(VertexID.parseID("C")).setDepth(3);
		
		LearnerGraph expected = new LearnerGraph(FsmParser.buildGraph("A-a->B", "testbuildDeterministicGraph1_b"),conf);
		LearnerGraph g = graph.pathroutines.buildDeterministicGraph();
		Assert.assertTrue(expected.equals(g));
	}
	
	/** Tests that conversion from non-deterministic matrix to a deterministic one works. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testbuildDeterministicGraph2() throws IncompatibleStatesException
	{
		LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a-#B\nA-a-#C\nA-a-#D", "testbuildDeterministicGraph1_a"),config);
		LearnerGraph expected = new LearnerGraph(FsmParser.buildGraph("A-a-#B", "testbuildDeterministicGraph1_b"),config);
		Assert.assertTrue(expected.equals(graph.pathroutines.buildDeterministicGraph()));
	}
	
	/** Tests that conversion from non-deterministic matrix to a deterministic one works. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testbuildDeterministicGraph3() throws IncompatibleStatesException
	{
		LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->B\nA-a->C\nA-a->D\nB-b->C\nA-c->C", "testbuildDeterministicGraph2_a"),config);
		LearnerGraph expected = new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C\nA-c->C", "testbuildDeterministicGraph2_b"),config);
		Assert.assertTrue(expected.equals(graph.pathroutines.buildDeterministicGraph()));
	}
	
	/** Tests that it is not possible to build a deterministic graph if some vertices are not compatible. */ 
	@Test
	public final void testbuildDeterministicGraph_fail1()
	{
		final LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->B\nA-a->C\nA-a->D\nB-b->C\nA-c->C", "testbuildDeterministicGraph2_a"),config);
		graph.addToCompatibility(graph.findVertex("B"), graph.findVertex("C"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			graph.pathroutines.buildDeterministicGraph();
		}}, IncompatibleStatesException.class,"");
	}
	
	/** Tests that it is not possible to build a deterministic graph if some vertices are not compatible. */ 
	@Test
	public final void testbuildDeterministicGraph_fail2()
	{
		final LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->B\nA-a->C\nA-a-#D\nB-b->C\nA-c->C", "testbuildDeterministicGraph_fail2"),config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			graph.pathroutines.buildDeterministicGraph();
		}}, IncompatibleStatesException.class,"");
	}
	
	/** Tests that it is not possible to build a deterministic graph if some vertices are not compatible. */ 
	@Test
	public final void testbuildDeterministicGraph_fail3()
	{
		final LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph(complexgraphND,"testbuildDeterministicGraphComplexND"),Configuration.getDefaultConfiguration());
		graph.addToCompatibility(graph.findVertex("A"),graph.findVertex("E"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			graph.pathroutines.buildDeterministicGraph();
		}}, IncompatibleStatesException.class,"");
	}
	
	public static final String complexgraphND = "A-a->B-a->D-a->A-a->C-a->E-a->C\nA-b->B-f->B\nA-b->C-q->C\nD-e->D\nE-p->E",
		complexgraphD = "A-a->BC-a->DE-a->AC-a->BCE-a->DEC-a->ACE-a->BCE\n"+
			"A-b->BC-f->B-f->B-a->D-e->D-a->A\n"+
			"BC-q->C-q->C-a->E-p->E-a->C\n"+
			"DE-p->E\nBCE-p->E\nDEC-p->E\nACE-p->E\n"+
			"AC-q->C\nBCE-q->C\nDEC-q->C\nACE-q->C\n"+
			"DE-e->D\nBCE-f->B\nDEC-e->D\nAC-b->BC\nACE-b->BC";
	
	/** Tests conversion of a deterministic graph to a deterministic one. */
	@Test
	public final void testbuildDeterministicGraph_detTodet()
	{
		DirectedSparseGraph jungGraph = FsmParser.buildGraph("A-a->B-a->D-a->A-c->C-a->E-a->A\nA-b->B-f->B","testbuildDeterministicGraph_detTodet");
		final LearnerGraphND graph = new LearnerGraphND(jungGraph,Configuration.getDefaultConfiguration());
		Assert.assertNull(WMethod.checkM(new LearnerGraph(jungGraph,Configuration.getDefaultConfiguration()), graph));
	}
	
	/** Tests a more complex case of conversion of a non-deterministic graph to a deterministic one. 
	 * @throws IncompatibleStatesException 
	 */
	@Test
	public final void testbuildDeterministicGraphComplex()
	{
		final LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph(complexgraphND,"testbuildDeterministicGraphComplexND"),Configuration.getDefaultConfiguration());
		Assert.assertNull(WMethod.checkM(new LearnerGraph(FsmParser.buildGraph(complexgraphD,"testbuildDeterministicGraphComplexD"),Configuration.getDefaultConfiguration()),graph));
	}

	/** The supplied state does not exist. */
	@Test
	public final void testBuildDeterministicGraph_missingState1()
	{
		final LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph(complexgraphND,"testbuildDeterministicGraphComplexND"),Configuration.getDefaultConfiguration());
		final CmpVertex junkVertex = AbstractLearnerGraph.generateNewCmpVertex(new VertexID("junk"), graph.config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			graph.pathroutines.buildDeterministicGraph(junkVertex);
		}}, IllegalArgumentException.class,"the supplied state");
		
	}
	
	/** The supplied state does not exist. */
	@Test
	public final void testBuildDeterministicGraph_missingState2()
	{
		final LearnerGraphND graph = new LearnerGraphND(Configuration.getDefaultConfiguration());graph.initEmpty();
		final CmpVertex junkVertex = AbstractLearnerGraph.generateNewCmpVertex(new VertexID("junk"), graph.config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			graph.pathroutines.buildDeterministicGraph(junkVertex);
		}}, IllegalArgumentException.class,"non-null");
		
	}
	
	/** Tests adding to graph. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testLTL_add1() throws IncompatibleStatesException
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("init-a->init", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(FsmParser.buildGraph("A-c->B-b->B", "testLTL_add1"),config);
		
		LearnerGraph result = LearnerGraphND.UniteTransitionMatrices(new LearnerGraphND(graph,graph.config),graphToAdd).pathroutines.buildDeterministicGraph();
		LearnerGraph expected = new LearnerGraph(FsmParser.buildGraph("A-a->A-c->B-b->B\n"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	/** Tests adding to graph. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testLTL_add2() throws IncompatibleStatesException
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("init-a->init", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(FsmParser.buildGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1"),config);
		
		LearnerGraph result = LearnerGraphND.UniteTransitionMatrices(new LearnerGraphND(graph,graph.config),graphToAdd).pathroutines.buildDeterministicGraph();
		LearnerGraph expected = new LearnerGraph(FsmParser.buildGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	/** Tests adding to graph. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testLTL_add3() throws IncompatibleStatesException
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("init-a->init-b->S", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(FsmParser.buildGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1"),config);
		
		LearnerGraph result = LearnerGraphND.UniteTransitionMatrices(new LearnerGraphND(graph,graph.config),graphToAdd).pathroutines.buildDeterministicGraph();
		LearnerGraph expected = new LearnerGraph(FsmParser.buildGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"+
				"A-b->S"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	/** Tests adding to graph. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testLTL_add4() throws IncompatibleStatesException
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("init-a->init-d->S", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(FsmParser.buildGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1"),config);
		
		LearnerGraph result = LearnerGraphND.UniteTransitionMatrices(new LearnerGraphND(graph,graph.config),graphToAdd).pathroutines.buildDeterministicGraph();
		LearnerGraph expected = new LearnerGraph(FsmParser.buildGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}

	/** Tests adding to graph. 
	 * @throws IncompatibleStatesException 
	 */
	@Test
	public final void testLTL_add5() throws IncompatibleStatesException
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("P-a->Q-a->S-a->U\nP-b->R-a->T", "testLTL_add5_A"),config);
		LearnerGraph graphToAdd = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->B-a->B-b-#C", "testLTL_add5_B"),config);

		LearnerGraph result = LearnerGraphND.UniteTransitionMatrices(new LearnerGraphND(graph,graph.config),graphToAdd).pathroutines.buildDeterministicGraph();
		Assert.assertNull(WMethod.checkM(graphToAdd,result));
	}
	
	/** A simple graph. */
	@Test
	public final void testConvertToND1()
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->A","testConvertToND2"),Configuration.getDefaultConfiguration());
		LearnerGraph actual = new LearnerGraph(new LearnerGraphND(graph,graph.config),config);
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** Empty graph. */
	@Test
	public final void testConvertToND2()
	{
		LearnerGraph graph = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph actual = new LearnerGraph(new LearnerGraphND(graph,graph.config),config);
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** Empty graph with a single state-reject state. */
	@Test
	public final void testConvertToND3()
	{
		LearnerGraph graph = new LearnerGraph(Configuration.getDefaultConfiguration());
		graph.getInit().setAccept(false);
		LearnerGraph actual = new LearnerGraph(new LearnerGraphND(graph,graph.config),config);
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** A not so simple graph. */
	@Test
	public final void testConvertToND4()
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C","testConvertToND4"),Configuration.getDefaultConfiguration());
		LearnerGraph actual = new LearnerGraph(new LearnerGraphND(graph,graph.config),config);
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** A not so simple graph with multiple disconnected parts. One state has no outgoing or incoming transitions. */
	@Test
	public final void testConvertToND5()
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C","testConvertToND5"),Configuration.getDefaultConfiguration());
		graph.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(new VertexID("S"), graph.config), graph.createNewRow());
		LearnerGraph actual = new LearnerGraph(new LearnerGraphND(graph,graph.config),config);
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertNull(WMethod.checkM(graph, graph.findVertex("S"), actual, actual.findVertex("S"),WMethod.VERTEX_COMPARISON_KIND.NONE));
	}
	
	/** A not so simple graph with multiple disconnected parts. */
	@Test
	public final void testConvertToND6()
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C\nU-a->V-a->U","testConvertToND6"),Configuration.getDefaultConfiguration());
		LearnerGraph actual = new LearnerGraph(new LearnerGraphND(graph,graph.config),config);
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertNull(WMethod.checkM(graph, graph.findVertex("U"), actual, actual.findVertex("U"),WMethod.VERTEX_COMPARISON_KIND.NONE));
	}
	
	/** Given two ND graph, compares them but prior to that,
	 * sorts target states for each transition (this is needed to compare 
	 * such graphs without having to think about unreachable parts of a graph).
	 * 
	 * @param Aarg the first graph to compare.
	 * @param Barg the second graph to compare.
	 */
	private static void compareGraphs(LearnerGraphND Aarg, LearnerGraphND Barg)
	{
		LearnerGraphND A = new LearnerGraphND(Aarg,Aarg.config);LearnerGraphND B = new LearnerGraphND(Barg,Barg.config); 
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:A.transitionMatrix.entrySet())
			for(Entry<String,List<CmpVertex>> mapEntry:entry.getValue().entrySet())
			Collections.sort(mapEntry.getValue());
		
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:B.transitionMatrix.entrySet())
			for(Entry<String,List<CmpVertex>> mapEntry:entry.getValue().entrySet())
			Collections.sort(mapEntry.getValue());
		
		Assert.assertEquals(A.transitionMatrix,B.transitionMatrix);
		Assert.assertEquals(A.getInit(),B.getInit());
	}
	
	/** An empty graph. */
	@Test
	public final void testBuildInverse1()
	{
		LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->A","testConvertToND2"),Configuration.getDefaultConfiguration());
		LearnerGraphND expected = new LearnerGraphND(FsmParser.buildGraph("A-a->A","testConvertToND2"),Configuration.getDefaultConfiguration());

		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		LearnerGraphND.buildInverse(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** Simple graph. */
	@Test
	public final void testBuildInverse2()
	{
		LearnerGraphND graph = new LearnerGraphND(Configuration.getDefaultConfiguration());graph.initPTA();
		LearnerGraphND expected = new LearnerGraphND(Configuration.getDefaultConfiguration());expected.initPTA();

		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());
		LearnerGraphND.buildInverse(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A graph with a single state-reject state. */
	@Test
	public final void testBuildInverse3()
	{
		LearnerGraphND graph = new LearnerGraphND(Configuration.getDefaultConfiguration());graph.initPTA();
		graph.getInit().setAccept(false);
		LearnerGraphND expected = new LearnerGraphND(Configuration.getDefaultConfiguration());expected.initPTA();
		expected.getInit().setAccept(false);

		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());
		LearnerGraphND.buildInverse(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A not so simple graph. */
	@Test
	public final void testBuildInverse4()
	{
		LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C",
				"testConvertToND4"),Configuration.getDefaultConfiguration());
		LearnerGraphND expected = new LearnerGraphND(FsmParser.buildGraph("A-c->B\nA-d->B\nC-c->A\nC-a->B\nD-b->B-a->A",
			"testConvertToND4"),Configuration.getDefaultConfiguration());
		expected.findVertex("D").setAccept(false);
		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		LearnerGraphND.buildInverse(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A not so simple graph with multiple disconnected parts. One state has no outgoing or incoming transitions. */
	@Test
	public final void testBuildInverse5()
	{
		LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C",
			"testConvertToND5"),Configuration.getDefaultConfiguration());
		graph.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(new VertexID("S"), graph.config), graph.createNewRow());
		
		LearnerGraphND expected = new LearnerGraphND(FsmParser.buildGraph("A-c->B\nA-d->B\nC-c->A\nC-a->B\nD-b->B-a->A",
			"testConvertToND5"),Configuration.getDefaultConfiguration());
		expected.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(new VertexID("S"), graph.config), graph.createNewRow());
		
		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		LearnerGraphND.buildInverse(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A not so simple graph with multiple disconnected parts. */
	@Test
	public final void testBuildInverse6()
	{
		LearnerGraphND graph = new LearnerGraphND(FsmParser.buildGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C\nU-b->V-a->U",
			"testConvertToND6"),Configuration.getDefaultConfiguration());
		LearnerGraphND expected = new LearnerGraphND(FsmParser.buildGraph("A-c->B\nA-d->B\nC-c->A\nC-a->B\nD-b->B-a->A\nU-a->V-b->U",
			"testConvertToND6"),Configuration.getDefaultConfiguration());

		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		LearnerGraphND.buildInverse(graph, LearnerGraphND.ignoreNone, actual);
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
			GDLearnerGraph ndGraph = new GDLearnerGraph(gr,filter, direction);
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
	
	private final void checkSource_Target_are_expected(LearnerGraphND matrixND,
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
	
	/** Tests that the transition matrices are built correctly by GDLearnerGraph */
	@Test
	public final void TestDomainCompatibility1()
	{
		checkConsideringIgnoredStates("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R\nC-f-#T","TestFindIncompatibleStatesB",
				LearnerGraphND.ignoreRejectStatesClass.class,new String[]{"A","Q","C","D"});
	}
	
	/** Tests that the transition matrices are built correctly by GDLearnerGraph */
	@Test
	public final void TestDomainCompatibility2()
	{
		checkConsideringIgnoredStates("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R\nC-f-#T","TestFindIncompatibleStatesB",
				LearnerGraphND.ignoreNoneClass.class,new String[]{"A","Q","C","D","R","T"});
	}
	
	/** Tests that the transition matrices are built correctly by GDLearnerGraph */
	@Test
	public final void TestDomainCompatibility3()
	{
		checkConsideringIgnoredStates("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R\nC-f-#T","TestFindIncompatibleStatesB",
				LearnerGraphND.ignoreZeroClass.class,new String[]{"A","C","D"});
	}

}
