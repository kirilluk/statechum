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

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraphND;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
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
import org.junit.runner.RunWith;
import org.junit.runners.ParameterizedWithName;
import org.junit.runners.ParameterizedWithName.ParametersToString;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.Helper;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper.whatToRun;
import statechum.Label;
import statechum.analysis.learning.linear.GDLearnerGraph;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph.StatesToConsider;

/* Note that many methods are also tested by TestLTL_to_ba. */
@RunWith(ParameterizedWithName.class)
public class TestLearnerGraphND extends TestWithMultipleConfigurations
{
	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		return TestWithMultipleConfigurations.data();
	}
	
	@ParametersToString
	public static String parametersToString(Configuration config)
	{
		return TestWithMultipleConfigurations.parametersToString(config);
	}

	public TestLearnerGraphND(Configuration conf)
	{
		super(conf);
	}
	

	@Before
	public final void beforeTest()
	{
		config = mainConfiguration.copy();
		ba=new LTL_to_ba(config,converter);ba.setAlphabet(new HashSet<Label>());
		for(Label lbl:labelList(new String[]{"a","b","c"})) ba.alphabet.put(lbl,lbl);
		
		expectedFromASEExample = buildLearnerGraph(
				"I-close->1\nI-edit->I1\nI-save->I1\nI-load->I1\n"+
				"1-load->I1-close->1\n"+
				"I1-edit->I1-save->I1-load->I1\n","testLTL_bigger",config,converter);
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
		Assert.assertTrue(converted.getInit().equals(VertexID.parseID("P1000")));// the original name is preserved ...
		Assert.assertFalse(converted.getInit().isAccept());// as is the accept condition.
		Assert.assertEquals(1,converted.transitionMatrix.size());
		Assert.assertTrue(converted.transitionMatrix.get(converted.getInit()).isEmpty());
	}
	
	/** Tests that conversion from non-deterministic matrix to a deterministic one works. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testbuildDeterministicGraph1a() throws IncompatibleStatesException
	{
		LearnerGraphND graph = buildLearnerGraphND("A-a->B\nA-a->C\nA-a->D", "testbuildDeterministicGraph1_a",config,converter);
		LearnerGraph expected = buildLearnerGraph("A-a->B", "testbuildDeterministicGraph1_b",config,converter);
		Assert.assertTrue(expected.equals(graph.pathroutines.buildDeterministicGraph()));
	}
	
	/** Tests that conversion from non-deterministic matrix to a deterministic one works. 
	 * This is a minor variation on the above which checks that depth is honoured.
	 * @throws IncompatibleStatesException */
	@Test
	public final void testbuildDeterministicGraph1b() throws IncompatibleStatesException
	{
		LearnerGraphND graph = buildLearnerGraphND("A-a->B\nA-a->C\nA-a->D", "testbuildDeterministicGraph1_a",config,converter);
		graph.findVertex(VertexID.parseID("C")).setDepth(3);
		LearnerGraph expected = buildLearnerGraph("A-a->C", "testbuildDeterministicGraph1_b",config,converter);
		expected.findVertex(VertexID.parseID("C")).setDepth(3);
		Assert.assertTrue(expected.equals(graph.pathroutines.buildDeterministicGraph()));
	}
	
	/** Tests that conversion from non-deterministic matrix to a deterministic one works. 
	 * This is a minor variation on the above which checks that depth is not honoured if we say so.
	 * @throws IncompatibleStatesException */
	@Test
	public final void testbuildDeterministicGraph1c() throws IncompatibleStatesException
	{
		Configuration conf = config.copy();conf.setIgnoreDepthInTheChoiceOfRepresentatives(true);
		LearnerGraphND graph = buildLearnerGraphND("A-a->B\nA-a->C\nA-a->D", "testbuildDeterministicGraph1_a",conf,converter);
		graph.findVertex(VertexID.parseID("C")).setDepth(3);
		
		LearnerGraph expected = buildLearnerGraph("A-a->B", "testbuildDeterministicGraph1_b",conf,converter);
		LearnerGraph g = graph.pathroutines.buildDeterministicGraph();
		Assert.assertTrue(expected.equals(g));
	}
	
	/** Tests that conversion from non-deterministic matrix to a deterministic one works. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testbuildDeterministicGraph2() throws IncompatibleStatesException
	{
		LearnerGraphND graph = buildLearnerGraphND("A-a-#B\nA-a-#C\nA-a-#D", "testbuildDeterministicGraph1_a",config,converter);
		LearnerGraph expected = buildLearnerGraph("A-a-#B", "testbuildDeterministicGraph1_b",config,converter);
		Assert.assertTrue(expected.equals(graph.pathroutines.buildDeterministicGraph()));
	}
	
	/** Tests that conversion from non-deterministic matrix to a deterministic one works. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testbuildDeterministicGraph3() throws IncompatibleStatesException
	{
		LearnerGraphND graph = buildLearnerGraphND("A-a->B\nA-a->C\nA-a->D\nB-b->C\nA-c->C", "testbuildDeterministicGraph2_a",config,converter);
		LearnerGraph expected = buildLearnerGraph("A-a->B-b->C\nA-c->C", "testbuildDeterministicGraph2_b",config,converter);
		Assert.assertTrue(expected.equals(graph.pathroutines.buildDeterministicGraph()));
	}
	
	/** Tests that it is not possible to build a deterministic graph if some vertices are not compatible. */ 
	@Test
	public final void testbuildDeterministicGraph_fail1()
	{
		final LearnerGraphND graph = buildLearnerGraphND("A-a->B\nA-a->C\nA-a->D\nB-b->C\nA-c->C", "testbuildDeterministicGraph2_a",config,converter);
		graph.addToCompatibility(graph.findVertex("B"), graph.findVertex("C"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			graph.pathroutines.buildDeterministicGraph();
		}}, IncompatibleStatesException.class,"");
	}
	
	/** Tests that it is not possible to build a deterministic graph if some vertices are not compatible. */ 
	@Test
	public final void testbuildDeterministicGraph_fail2()
	{
		final LearnerGraphND graph = buildLearnerGraphND("A-a->B\nA-a->C\nA-a-#D\nB-b->C\nA-c->C", "testbuildDeterministicGraph_fail2",config,converter);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			graph.pathroutines.buildDeterministicGraph();
		}}, IncompatibleStatesException.class,"");
	}
	
	/** Tests that it is not possible to build a deterministic graph if some vertices are not compatible. */ 
	@Test
	public final void testbuildDeterministicGraph_fail3()
	{
		final LearnerGraphND graph = buildLearnerGraphND(complexgraphND,"testbuildDeterministicGraphComplexND",config,converter);
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
		DirectedSparseGraph jungGraph = buildLearnerGraphND("A-a->B-a->D-a->A-c->C-a->E-a->A\nA-b->B-f->B","testbuildDeterministicGraph_detTodet",config,converter).pathroutines.getGraph();
		final LearnerGraphND graph = new LearnerGraphND(jungGraph,config);
		Assert.assertNull(WMethod.checkM(new LearnerGraph(jungGraph,config), graph));
	}
	
	/** Tests a more complex case of conversion of a non-deterministic graph to a deterministic one. 
	 * @throws IncompatibleStatesException 
	 */
	@Test
	public final void testbuildDeterministicGraphComplex()
	{
		final LearnerGraphND graph = buildLearnerGraphND(complexgraphND,"testbuildDeterministicGraphComplexND",config,converter);
		Assert.assertNull(WMethod.checkM(buildLearnerGraph(complexgraphD,"testbuildDeterministicGraphComplexD",config,converter),graph));
	}

	/** The supplied state does not exist. */
	@Test
	public final void testBuildDeterministicGraph_missingState1()
	{
		final LearnerGraphND graph = buildLearnerGraphND(complexgraphND,"testbuildDeterministicGraphComplexND",config,converter);
		final CmpVertex junkVertex = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("junk"), graph.config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			graph.pathroutines.buildDeterministicGraph(junkVertex);
		}}, IllegalArgumentException.class,"the supplied state");
		
	}
	
	/** The supplied state does not exist. */
	@Test
	public final void testBuildDeterministicGraph_missingState2()
	{
		final LearnerGraphND graph = new LearnerGraphND(config);graph.initEmpty();
		final CmpVertex junkVertex = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("junk"), graph.config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			graph.pathroutines.buildDeterministicGraph(junkVertex);
		}}, IllegalArgumentException.class,"non-null");
		
	}
	
	/** Tests adding to graph. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testLTL_add1() throws IncompatibleStatesException
	{
		LearnerGraph graph = buildLearnerGraph("init-a->init", "testLTL_ba_graph3",config,converter);
		LearnerGraph graphToAdd = buildLearnerGraph("A-c->B-b->B", "testLTL_add1",config,converter);
		
		LearnerGraph result = LearnerGraphND.UniteTransitionMatrices(new LearnerGraphND(graph,graph.config),graphToAdd).pathroutines.buildDeterministicGraph();
		LearnerGraph expected = buildLearnerGraph("A-a->A-c->B-b->B\n"
				, "testLTL_complete2",config,converter);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	/** Tests adding to graph. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testLTL_add2() throws IncompatibleStatesException
	{
		LearnerGraph graph = buildLearnerGraph("init-a->init", "testLTL_ba_graph3",config,converter);
		LearnerGraph graphToAdd = buildLearnerGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1",config,converter);
		
		LearnerGraph result = LearnerGraphND.UniteTransitionMatrices(new LearnerGraphND(graph,graph.config),graphToAdd).pathroutines.buildDeterministicGraph();
		LearnerGraph expected = buildLearnerGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"
				, "testLTL_complete2",config,converter);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	/** Tests adding to graph. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testLTL_add3() throws IncompatibleStatesException
	{
		LearnerGraph graph = buildLearnerGraph("I-a->I-b->S", "testLTL_ba_graph3",config,converter);
		LearnerGraph graphToAdd = buildLearnerGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1",config,converter);
		
		LearnerGraph result = LearnerGraphND.UniteTransitionMatrices(new LearnerGraphND(graph,graph.config),graphToAdd).pathroutines.buildDeterministicGraph();
		LearnerGraph expected = buildLearnerGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"+
				"A-b->S"
				, "testLTL_complete2",config,converter);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	/** Tests adding to graph. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testLTL_add4() throws IncompatibleStatesException
	{
		LearnerGraph graph = buildLearnerGraph("I-a->I-d->S", "testLTL_ba_graph3",config,converter);
		LearnerGraph graphToAdd = buildLearnerGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1",config,converter);
		
		LearnerGraph result = LearnerGraphND.UniteTransitionMatrices(new LearnerGraphND(graph,graph.config),graphToAdd).pathroutines.buildDeterministicGraph();
		LearnerGraph expected = buildLearnerGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"
				, "testLTL_complete2",config,converter);
		Assert.assertNull(WMethod.checkM(result,expected));
	}

	/** Tests adding to graph. 
	 * @throws IncompatibleStatesException 
	 */
	@Test
	public final void testLTL_add5() throws IncompatibleStatesException
	{
		LearnerGraph graph = buildLearnerGraph("P-a->Q-a->S-a->U\nP-b->R-a->T", "testLTL_add5_A",config,converter);
		LearnerGraph graphToAdd = buildLearnerGraph("A-b->A-a->B-a->B-b-#C", "testLTL_add5_B",config,converter);

		LearnerGraph result = LearnerGraphND.UniteTransitionMatrices(new LearnerGraphND(graph,graph.config),graphToAdd).pathroutines.buildDeterministicGraph();
		Assert.assertNull(WMethod.checkM(graphToAdd,result));
	}
	
	/** A simple graph. */
	@Test
	public final void testConvertToND1()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->A","testConvertToND2",config,converter);
		LearnerGraph actual = new LearnerGraph(new LearnerGraphND(graph,graph.config),config);
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** Empty graph. */
	@Test
	public final void testConvertToND2()
	{
		LearnerGraph graph = new LearnerGraph(config);
		LearnerGraph actual = new LearnerGraph(new LearnerGraphND(graph,graph.config),config);
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** Empty graph with a single state-reject state. */
	@Test
	public final void testConvertToND3()
	{
		LearnerGraph graph = new LearnerGraph(config);
		graph.getInit().setAccept(false);
		LearnerGraph actual = new LearnerGraph(new LearnerGraphND(graph,graph.config),config);
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** A not so simple graph. */
	@Test
	public final void testConvertToND4()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C","testConvertToND4",config,converter);
		LearnerGraph actual = new LearnerGraph(new LearnerGraphND(graph,graph.config),config);
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** A not so simple graph with multiple disconnected parts. One state has no outgoing or incoming transitions. */
	@Test
	public final void testConvertToND5()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C","testConvertToND5",config,converter);
		graph.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("S"), graph.config), graph.createNewRow());
		LearnerGraph actual = new LearnerGraph(new LearnerGraphND(graph,graph.config),config);
		Assert.assertNull(WMethod.checkM_and_colours(actual, graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertNull(WMethod.checkM(graph, graph.findVertex("S"), actual, actual.findVertex("S"),WMethod.VERTEX_COMPARISON_KIND.NONE));
	}
	
	/** A not so simple graph with multiple disconnected parts. */
	@Test
	public final void testConvertToND6()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C\nU-a->V-a->U","testConvertToND6",config,converter);
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
		for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entry:A.transitionMatrix.entrySet())
			for(Entry<Label,List<CmpVertex>> mapEntry:entry.getValue().entrySet())
			Collections.sort(mapEntry.getValue(),new Comparator<CmpVertex>(){

				@Override
				public int compare(CmpVertex arg0, CmpVertex arg1) {
					return arg0.compareTo(arg1);
				}
				
			});
		
		for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entry:B.transitionMatrix.entrySet())
			for(Entry<Label,List<CmpVertex>> mapEntry:entry.getValue().entrySet())
			Collections.sort(mapEntry.getValue(),new Comparator<CmpVertex>(){

				@Override
				public int compare(CmpVertex arg0, CmpVertex arg1) {
					return arg0.compareTo(arg1);
				}
				
			});
		
		Assert.assertEquals(A.transitionMatrix,B.transitionMatrix);
		Assert.assertEquals(A.getInit(),B.getInit());
	}
	
	/** Simple graph. */
	@Test
	public final void testBuildInverse1()
	{
		LearnerGraphND graph = buildLearnerGraphND("A-a->A","testConvertToND2",config,converter);
		LearnerGraphND expected = buildLearnerGraphND("A-a->A","testConvertToND2",config,converter);

		LearnerGraphND actual = new LearnerGraphND(config);actual.initEmpty();
		AbstractPathRoutines.buildInverse(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** Simple graph. */
	@Test
	public final void testBuildForward1()
	{
		LearnerGraphND graph = buildLearnerGraphND("A-a->A","testConvertToND2",config,converter);
		LearnerGraphND expected = buildLearnerGraphND("A-a->A","testConvertToND2",config,converter);

		LearnerGraphND actual = new LearnerGraphND(config);actual.initEmpty();
		AbstractPathRoutines.buildForward(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** An empty graph. */
	@Test
	public final void testBuildInverse2()
	{
		LearnerGraphND graph = new LearnerGraphND(config);graph.initPTA();
		LearnerGraphND expected = new LearnerGraphND(config);expected.initPTA();

		LearnerGraphND actual = new LearnerGraphND(config);
		AbstractPathRoutines.buildInverse(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** An empty graph. */
	@Test
	public final void testBuildForward2()
	{
		LearnerGraphND graph = new LearnerGraphND(config);graph.initPTA();
		LearnerGraphND expected = new LearnerGraphND(config);expected.initPTA();

		LearnerGraphND actual = new LearnerGraphND(config);
		AbstractPathRoutines.buildForward(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A graph with a single state-reject state. */
	@Test
	public final void testBuildInverse3()
	{
		LearnerGraphND graph = new LearnerGraphND(config);graph.initPTA();
		graph.getInit().setAccept(false);
		LearnerGraphND expected = new LearnerGraphND(config);expected.initPTA();
		expected.getInit().setAccept(false);

		LearnerGraphND actual = new LearnerGraphND(config);
		AbstractPathRoutines.buildInverse(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A graph with a single state-reject state. */
	@Test
	public final void testBuildForward3()
	{
		LearnerGraphND graph = new LearnerGraphND(config);graph.initPTA();
		graph.getInit().setAccept(false);
		LearnerGraphND expected = new LearnerGraphND(config);expected.initPTA();
		expected.getInit().setAccept(false);

		LearnerGraphND actual = new LearnerGraphND(config);
		AbstractPathRoutines.buildForward(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A not so simple graph. */
	@Test
	public final void testBuildInverse4()
	{
		LearnerGraphND graph = buildLearnerGraphND("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C",
				"testConvertToND4",config,converter);
		LearnerGraphND expected = buildLearnerGraphND("A-c->B\nA-d->B\nC-c->A\nC-a->B\nD-b->B-a->A",
			"testConvertToND4",config,converter);
		expected.findVertex("D").setAccept(false);
		LearnerGraphND actual = new LearnerGraphND(config);actual.initEmpty();
		AbstractPathRoutines.buildInverse(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A not so simple graph. */
	@Test
	public final void testBuildForward4()
	{
		String graphStr = "A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C";
		LearnerGraphND graph = buildLearnerGraphND(graphStr,"testConvertToND4",config,converter);
		LearnerGraphND expected = buildLearnerGraphND(graphStr,"testConvertToND4",config,converter);
		expected.findVertex("D").setAccept(false);
		LearnerGraphND actual = new LearnerGraphND(config);actual.initEmpty();
		AbstractPathRoutines.buildForward(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A not so simple graph with multiple disconnected parts. One state has no outgoing or incoming transitions. */
	@Test
	public final void testBuildInverse5()
	{
		LearnerGraphND graph = buildLearnerGraphND("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C",
			"testConvertToND5",config,converter);
		graph.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("S"), graph.config), graph.createNewRow());
		
		LearnerGraphND expected = buildLearnerGraphND("A-c->B\nA-d->B\nC-c->A\nC-a->B\nD-b->B-a->A",
			"testConvertToND5",config,converter);
		expected.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("S"), graph.config), graph.createNewRow());
		
		LearnerGraphND actual = new LearnerGraphND(config);actual.initEmpty();
		AbstractPathRoutines.buildInverse(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A not so simple graph with multiple disconnected parts. One state has no outgoing or incoming transitions. */
	@Test
	public final void testBuildForward5()
	{
		String graphStr = "A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C";
		LearnerGraphND graph = buildLearnerGraphND(graphStr,"testConvertToND5",config,converter);
		graph.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("S"), graph.config), graph.createNewRow());
		
		LearnerGraphND expected = buildLearnerGraphND(graphStr,"testConvertToND5",config,converter);
		expected.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("S"), graph.config), graph.createNewRow());
		
		LearnerGraphND actual = new LearnerGraphND(config);actual.initEmpty();
		AbstractPathRoutines.buildForward(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}
	
	/** A not so simple graph with multiple disconnected parts. */
	@Test
	public final void testBuildInverse6()
	{
		LearnerGraphND graph = buildLearnerGraphND("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C\nU-b->V-a->U",
			"testConvertToND6",config,converter);
		LearnerGraphND expected = buildLearnerGraphND("A-c->B\nA-d->B\nC-c->A\nC-a->B\nD-b->B-a->A\nU-a->V-b->U",
			"testConvertToND6",config,converter);

		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		AbstractPathRoutines.buildInverse(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}

	/** A not so simple graph with multiple disconnected parts. */
	@Test
	public final void testBuildForward6()
	{
		String graphStr = "A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C\nU-b->V-a->U";
		LearnerGraphND graph = buildLearnerGraphND(graphStr,"testConvertToND6",config,converter);
		LearnerGraphND expected = buildLearnerGraphND(graphStr,"testConvertToND6",config,converter);

		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		AbstractPathRoutines.buildForward(graph, LearnerGraphND.ignoreNone, actual);
		compareGraphs(expected, actual);
	}

	/** A not so simple graph with multiple disconnected parts and a filter. */
	@Test
	public final void testBuildInverse7()
	{
		LearnerGraphND graph = buildLearnerGraphND("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C\nU-b->V-a->U",
			"testConvertToND6",config,converter);
		LearnerGraphND expected = buildLearnerGraphND("C-c->A\nU-a->V-b->U","testBuildInverse7",config,converter);
		expected.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("D"), graph.config), graph.createNewRow());
		expected.setInit(expected.findVertex("A"));
		
		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		AbstractPathRoutines.buildInverse(graph, new StatesToConsider() {

			@Override
			public boolean stateToConsider(CmpVertex vert) {
				return !vert.getStringId().equals("B");
			}
		}, actual);
		compareGraphs(expected, actual);
	}

	/** A not so simple graph with multiple disconnected parts and a filter. */
	@Test
	public final void testBuildInverse8()
	{
		LearnerGraphND graph = buildLearnerGraphND("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C\nU-b->V-a->U",
			"testConvertToND6",config,converter);
		
		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		AbstractPathRoutines.buildInverse(graph, new StatesToConsider() {

			@Override
			public boolean stateToConsider(@SuppressWarnings("unused") CmpVertex vert) {
				return false;
			}
		}, actual);
		Assert.assertTrue(actual.transitionMatrix.isEmpty());Assert.assertNull(actual.getInit());
	}

	/** A not so simple graph with multiple disconnected parts and a filter. */
	@Test
	public final void testBuildForward7()
	{
		LearnerGraphND graph = buildLearnerGraphND("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C\nU-b->V-a->U","testConvertToND6",config,converter);
		LearnerGraphND expected = buildLearnerGraphND("A-c->C\nU-b->V-a->U","testBuildForward7",config,converter);
		expected.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("D"), graph.config), graph.createNewRow());
		
		LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		AbstractPathRoutines.buildForward(graph, new StatesToConsider() {

			@Override
			public boolean stateToConsider(CmpVertex vert) {
				return !vert.getStringId().equals("B");
			}
		}, actual);

		compareGraphs(expected, actual);
	}

	/** A not so simple graph with multiple disconnected parts and a filter. */
	@Test
	public final void testBuildForward8()
	{
		final LearnerGraphND graph = buildLearnerGraphND("A-a->B-a->C\nA<-c-B-b-#D\nB-d->A-c->C\nU-b->V-a->U","testConvertToND6",config,converter);
		final LearnerGraphND actual = new LearnerGraphND(Configuration.getDefaultConfiguration());actual.initEmpty();
		
		AbstractPathRoutines.buildForward(graph, new StatesToConsider() {
			@Override
			public boolean stateToConsider(@SuppressWarnings("unused") CmpVertex vert) {
				return false;
			}
		},actual);
		Assert.assertNull(actual.getInit());
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
	 * @param expectedToRemain which states are expected to remain after filtering.
	 */
	private final void checkConsideringIgnoredStates(String graph, String graphName, Class<? extends StatesToConsider> filterClass, String [] expectedToRemain)
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		LearnerGraph gr=buildLearnerGraph(graph,graphName,conf,null);
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
			
			Set<CmpVertex> expectedIgnoredStates = new TreeSet<CmpVertex>();for(String st:expectedToRemain) expectedIgnoredStates.add(gr.findVertex(st));
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
		for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entry:matrixND.transitionMatrix.entrySet())
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
