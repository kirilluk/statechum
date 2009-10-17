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

import java.awt.Color;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.Configuration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.Configuration.QuestionGeneratorKind;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.DeterministicDirectedSparseGraph.VertexID.VertKind;
import statechum.Helper.whatToRun;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.TestRpniLearner;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.LearnerGraph.NonExistingPaths;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;
import statechum.apps.QSMTool;
import statechum.model.testset.PTASequenceEngine;

final public class TestAugmentUsingIFTHEN 
{
	/** Tests merging of the two automata depicted on page 18 of "why_nondet_does_not_matter.xoj" */
	@Test
	public final void testAugmentFromMax1_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(FsmParser.buildGraph("H-a->A-a->B-b->C\nH-c->B\nH-d->B", "testAugmentFromMax1_gr"),config);
		LearnerGraph max = new LearnerGraph(FsmParser.buildGraph("I-a->D-a-#E\nI-d-#E\nI-c->F-b->G", "testAugmentFromMax1_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true, config, true);
		TestEquivalenceChecking.checkM("H-a->A-a-#BE\nH-d-#BE\nH-c->BF-b->C", result.pathroutines.getGraph(), config);
		Assert.assertEquals(5,result.getStateNumber());
	}
	
	/** Tests merging of the two automata on page 18 of "why_nondet_does_not_matter.xoj" */
	@Test
	public final void testAugmentFromMax1_nonoverride()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph gr = new LearnerGraph(FsmParser.buildGraph("H-a->A-a->B-b->C\nH-c->B\nH-d->B", "testAugmentFromMax1_gr"),config);
		final LearnerGraph max = new LearnerGraph(FsmParser.buildGraph("I-a->D-a-#E\nI-d-#E\nI-c->F-b->G", "testAugmentFromMax1_max"),config);
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
		LearnerGraph gr = new LearnerGraph(FsmParser.buildGraph(automatonWithReject, "testAugmentFromMax1_max"),config);
		LearnerGraph max = new LearnerGraph(FsmParser.buildGraph("H-a->A-a->B-b->C\nH-c->B\nH-d->B", "testAugmentFromMax1_gr"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}

	/** Tests merging of the two automata on page 17 of "why_nondet_does_not_matter.xoj" */
	@Test
	public final void testAugmentFromMax2_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr"),config);
		LearnerGraph max = new LearnerGraph(FsmParser.buildGraph("B-b->D-b->F-a->F-b->B", "testAugmentFromMax2_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}

	/** Tests merging of the two automata on page 17 of "why_nondet_does_not_matter.xoj" */
	@Test
	public final void testAugmentFromMax3_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->C-b->C-a->E-b-#G\n"+
				"A-c->A\nC-c->C", "testAugmentFromMax3_gr"),config);
		LearnerGraph max = new LearnerGraph(FsmParser.buildGraph("B-b->D-b->F-a->F-b->B\n"+
				"B-c->D-c->F-c->B", "testAugmentFromMax3_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}
	
	@Test
	public final void testAugmentFromMax4_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		String origGraph = "A-b->A-a->A-c->B-c->C\n";
		LearnerGraph gr = new LearnerGraph(FsmParser.buildGraph(origGraph, "testAugmentFromMax4_gr"),config);
		LearnerGraph max = new LearnerGraph(FsmParser.buildGraph("E-a->F-a->G-a->H", "testAugmentFromMax4_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config,true);
		Assert.assertNull(result);
	}

	@Test
	public final void testAugmentFromMax5_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->A-c->B-c->C\n", "testAugmentFromMax4_gr"),config);
		LearnerGraph max = new LearnerGraph(FsmParser.buildGraph("E-a->F-a->G-a->H-a-#I", "testAugmentFromMax5_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config,true);
		TestEquivalenceChecking.checkM("AE-a->AF-a->AG-a->AH-a-#I\n"+
				"AE-b->P-c->B-c->C\nP-a->P-b->P\nAE-c->B\nAF-b->P\nAF-c->B\nAG-b->P\nAG-c->B\nAH-b->P\nAH-c->B", result.pathroutines.getGraph(), config);
	}
	
	@Test
	public final void testAugmentFromMax6_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->A-c->B-c->C\n", "testAugmentFromMax4_gr"),config);
		LearnerGraph max = new LearnerGraph(config);max.getInit().setAccept(false);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config,true);
		Assert.assertNull(WMethod.checkM(max, result));
	}
	
	@Test
	public final void testAugmentFromMax6_AB_nooverride()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph gr = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->A-c->B-c->C\n", "testAugmentFromMax4_gr"),config);
		final LearnerGraph max = new LearnerGraph(config);max.getInit().setAccept(false);
		checkForCorrectException(new whatToRun() {	public void run() throws NumberFormatException 
			{
				Transform.augmentFromMAX(gr, max, false, true,config, true);
			}}, IllegalArgumentException.class, "incompatible");
	}

	@Test
	public final void testAugmentFromMax6_BA()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(config);gr.getInit().setAccept(false);
		LearnerGraph max = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->A-c->B-c->C\n", "testAugmentFromMax4_gr"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config,true);
		Assert.assertNull(result);
	}

	@Test
	public final void testAugmentFromMax7_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr"),config);
		LearnerGraph max = new LearnerGraph(FsmParser.buildGraph("B-b->D-b->F-a->F-b->B\nD-a-#E", "testAugmentFromMax7_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		TestEquivalenceChecking.checkM("AB-b->AD-b->AF-b->AB\nAF-a->CF-b->CB-b->CD-b->CF-a->EF-b-#G\n"+
				"AB-a->C-b->C-a->E-b-#G\nCB-a->E\nAD-a-#H\nCD-a-#H", result.pathroutines.getGraph(), config);
	}

	@Test
	public final void testAugmentFromMax7_AB_nooverride()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph gr = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr"),config);
		final LearnerGraph max = new LearnerGraph(FsmParser.buildGraph("B-b->D-b->F-a->F-b->B\nD-a-#E", "testAugmentFromMax7_max"),config);
		checkForCorrectException(new whatToRun() {	public void run() throws NumberFormatException 
			{
				Transform.augmentFromMAX(gr, max, false, true,config, true);
			}}, IllegalArgumentException.class, "incompatible");
	}
	
	@Test
	public final void testAugmentFromMax8_a()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr"),config);
		LearnerGraph max = new LearnerGraph(config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}
	
	@Test
	public final void testAugmentFromMax8_b()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr"),config);
		LearnerGraph max = new LearnerGraph(FsmParser.buildGraph("A-a->A-b->A-c->A-d->A", "testAugmentFromMax7_max"),config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}
	
	static void compareGraphs(LearnerGraph A, LearnerGraph B)
	{
		DifferentFSMException ex= WMethod.checkM_and_colours(A, B, VERTEX_COMPARISON_KIND.NONE);
		Assert.assertNull(ex==null?"":ex.toString(),ex);
		
		// reachability of all states ensures that transition structures are isomorphic.
		Assert.assertEquals(A.getStateNumber(),A.pathroutines.computeShortPathsToAllStates().size());
		Assert.assertEquals(B.getStateNumber(),B.pathroutines.computeShortPathsToAllStates().size());
	}
	
	private static final String ifthenA = "A-a->B-a->C-b->B / P-b->Q-a->R / S-c->S / T-d->N / S=THEN=C=THEN=P / B=THEN=T";
	private static final String ifthenB = "A-a->B-a->C-b->B / P-b->Q-a->R / S-c->S / T-d->N / S=THEN=C=THEN=P / B=THEN=T=THEN=A";
	
	@Test
	public final void testCheckIFTHEN1()
	{
		Transform.checkTHEN_disjoint_from_IF(new LearnerGraph(FsmParser.buildGraph(ifthenA,"ifthen"),Configuration.getDefaultConfiguration()));
	}
	
	@Test
	public final void testCheckIFTHEN2()
	{
		Transform.checkTHEN_disjoint_from_IF(new LearnerGraph(FsmParser.buildGraph(ifthenB,"ifthen"),Configuration.getDefaultConfiguration()));
	}
	
	@Test
	public final void testCheckIFTHEN_fail0a()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			Transform.checkTHEN_disjoint_from_IF(new LearnerGraph(Configuration.getDefaultConfiguration()));
		}}, IllegalArgumentException.class,"no THEN states");
	}
	
	@Test
	public final void testCheckIFTHEN_fail0b()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			LearnerGraph graph = new LearnerGraph(Configuration.getDefaultConfiguration());graph.getInit().setAccept(false);
			Transform.checkTHEN_disjoint_from_IF(graph);
		}}, IllegalArgumentException.class,"no THEN states");
	}
	
	@Test
	public final void testCheckIFTHEN_fail1()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			Transform.checkTHEN_disjoint_from_IF(new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-b->B / P-b->Q-a->R / S-c->S / T-d->N","testCheckIFTHEN_fail1"),Configuration.getDefaultConfiguration()));
		}}, IllegalArgumentException.class,"no THEN states");
	}
	
	@Test
	public final void testCheckIFTHEN_fail2()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			Transform.checkTHEN_disjoint_from_IF(new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-b->B / P-b->Q-a->R / S-c->S / T-d->N / S=THEN=C=THEN=P","testCheckIFTHEN_fail2"),Configuration.getDefaultConfiguration()));
		}}, IllegalArgumentException.class,"unreachable");
	}
	
	@Test
	public final void testCheckIFTHEN_fail3()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			Transform.checkTHEN_disjoint_from_IF(new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-b->B / P-b->Q-a->B / S-c->S / T-d->N / S=THEN=C=THEN=P / B=THEN=T","testCheckIFTHEN_fail2"),Configuration.getDefaultConfiguration()));
		}}, IllegalArgumentException.class,"are shared between");
	}
	
	@Test
	public final void testCheckIFTHEN_fail4()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			Transform.checkTHEN_disjoint_from_IF(new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-b->B / P-b->Q-a->Q / S-c->S / T-d->N / S=THEN=C=THEN=P / S=THEN=T","testCheckIFTHEN_fail2"),Configuration.getDefaultConfiguration()));
		}}, IllegalArgumentException.class,"do not belong");
	}
	
	@Test
	public final void testbuildIfThenAutomata1()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		String ltlFormula = "!([](a->X[]b))";
		Collection<LearnerGraph> automata = Transform.buildIfThenAutomata(Arrays.asList(new String[]{
				QSMTool.cmdLTL+" "+ltlFormula}), new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C-c->D", "testbuildIfThenAutomata1"), config),config);
		Iterator<LearnerGraph> graphIter = automata.iterator();

		LearnerGraph topGraph = graphIter.next(), expectedTop = new LearnerGraph(FsmParser.buildGraph("I-a->A-b->A / I-b->IA-a->A / I-c->IA-b->IA-c->IA / P-c-#P1 / P-a-#P2 / A = THEN = P / " +
				"I - transition_to_THEN ->P","!("+ltlFormula+")"),config);
		topGraph.addTransition(topGraph.transitionMatrix.get(topGraph.getInit()), "transition_to_THEN", topGraph.findVertex(VertexID.parseID("P"+(topGraph.vertPositiveID-1))));
		graphIter = automata.iterator();
		compareGraphs(expectedTop,graphIter.next());
		Assert.assertFalse(graphIter.hasNext());
	}
	
	/** Same as above, but more automata. */
	@Test
	public final void testbuildIfThenAutomata2()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		String ltlFormulaA = "a", ltlFormulaB = "b";

		Collection<LearnerGraph> automata = Transform.buildIfThenAutomata(Arrays.asList(new String[]{
				QSMTool.cmdLTL+" "+ltlFormulaA,
				QSMTool.cmdIFTHENAUTOMATON+" graphA A-a->B / P-a->P == THEN == A",
				QSMTool.cmdLTL+" "+ltlFormulaB,
				QSMTool.cmdIFTHENAUTOMATON+" graphB "+ifthenA
			}), new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C-c->D-d->E", "testbuildIfThenAutomata1"), config),config);
		Iterator<LearnerGraph> graphIter = automata.iterator();

		LearnerGraph topGraph = graphIter.next(), expectedTop = new LearnerGraph(FsmParser.buildGraph("I-c->A / I-d->A / A-a->A-b->A-c->A-d->A / P2#-b-P-a-#P1 / I = THEN = P / " +
				"I - transition_to_THEN ->P","!("+ltlFormulaA+"||"+ltlFormulaB+")"),config);
		topGraph.addTransition(topGraph.transitionMatrix.get(topGraph.getInit()), "transition_to_THEN", topGraph.findVertex(VertexID.parseID("P"+(topGraph.vertPositiveID-1))));
		LearnerGraph next = null;
		compareGraphs(expectedTop, topGraph);Assert.assertEquals("LTL",topGraph.getName());
		
		next=graphIter.next();Assert.assertEquals("graphA", next.getName());
		next.addTransition(next.transitionMatrix.get(next.getInit()), "transition_to_THEN", next.findVertex("P"));
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B / P-a->P == THEN == A-transition_to_THEN->P","1"),config),next);
		
		next=graphIter.next();Assert.assertEquals("graphB", next.getName());
		next.addTransition(next.transitionMatrix.get(next.getInit()), "transition_to_THEN_P", next.findVertex("P"));
		next.addTransition(next.transitionMatrix.get(next.getInit()), "transition_to_THEN_S", next.findVertex("S"));
		next.addTransition(next.transitionMatrix.get(next.getInit()), "transition_to_THEN_T", next.findVertex("T"));
		compareGraphs(new LearnerGraph(FsmParser.buildGraph(ifthenA+" / A-transition_to_THEN_P->P / A-transition_to_THEN_S->S / A-transition_to_THEN_T->T","2"),config),next);
		Assert.assertFalse(graphIter.hasNext());
	}
	
	/** No LTL but some automata. */
	@Test
	public final void testbuildIfThenAutomata3()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();

		Collection<LearnerGraph> automata = Transform.buildIfThenAutomata(Arrays.asList(new String[]{
				QSMTool.cmdIFTHENAUTOMATON+" graphA A-a->B / P-a->P == THEN == A",
				QSMTool.cmdIFTHENAUTOMATON+" graphB "+ifthenA
			}), new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C-c->D-d->E", "testbuildIfThenAutomata1"), config),config);
		Iterator<LearnerGraph> graphIter = automata.iterator();

		LearnerGraph next = null;
		
		next=graphIter.next();Assert.assertEquals("graphA", next.getName());
		next.addTransition(next.transitionMatrix.get(next.getInit()), "transition_to_THEN", next.findVertex("P"));
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B / P-a->P == THEN == A-transition_to_THEN->P","1"),config),next);
		
		next=graphIter.next();Assert.assertEquals("graphB", next.getName());
		next.addTransition(next.transitionMatrix.get(next.getInit()), "transition_to_THEN_P", next.findVertex("P"));
		next.addTransition(next.transitionMatrix.get(next.getInit()), "transition_to_THEN_S", next.findVertex("S"));
		next.addTransition(next.transitionMatrix.get(next.getInit()), "transition_to_THEN_T", next.findVertex("T"));
		compareGraphs(new LearnerGraph(FsmParser.buildGraph(ifthenA+" / A-transition_to_THEN_P->P / A-transition_to_THEN_S->S / A-transition_to_THEN_T->T","2"),config),next);
		Assert.assertFalse(graphIter.hasNext());
	}

	/** An automaton without a name. */
	@Test
	public final void testbuildIfThenAutomata_fail()
	{
		final Configuration config = Configuration.getDefaultConfiguration();

		Helper.checkForCorrectException(new whatToRun() { public void run() {
		Transform.buildIfThenAutomata(Arrays.asList(new String[]{
				QSMTool.cmdLTL+" !a",
				QSMTool.cmdIFTHENAUTOMATON+" graphA"}), new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C-c->D", "testbuildIfThenAutomata1"), config),config);
		}}, IllegalArgumentException.class,"missing automata name");
	}
	
	/** Tests the construction of a PTA from questions. */
	@Test
	public final void testBuildPTAofQuestions1()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);config.setQuestionGenerator(QuestionGeneratorKind.CONVENTIONAL_IMPROVED);
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph(
				"A1-a->B1-b->A2-a->B2-b->A3-a->B3-b->A4-a->B4-b->A5 /"+
				"A2-c->A2_U1-c->A2_U2-e->A2_U3 /"+
				"A3-c->A3_U1-c->A3_U2 / A3_U1-d-#A3_U4 /"+
				"A4-c->A4_U1-c->A4_U2-f-#A4_U3 /"+
				"A5-c->A5_U1-c->A5_U2"
				, "testBuildPTAofQuestions1"),config);
		StatePair pair = new StatePair(graph.findVertex("A1"),graph.findVertex("A2"));
		LearnerGraph merged = MergeStates.mergeAndDeterminize_general(graph, pair);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A1-a->B1-b->A1-c->C-d-#R4 / C-c->CC / CC-f-#R3 / CC-e->D", "expected"),config),merged);
		PTASequenceEngine questions = ComputeQuestions.getQuestionPta(pair, graph, merged, null);
		LearnerGraph updatedGraphExpected = new LearnerGraph(graph,config),
			updatedGraphActual = ComputeQuestions.constructGraphWithQuestions(pair, graph, merged);

		for(List<String> path:questions.getData())
			updatedGraphExpected.paths.augmentPTA(path,merged.paths.getVertex(path).isAccept(),false,null);

		Set<List<String>> expectedQuestions = TestFSMAlgo.buildSet(new String[][] {
				new String[]{"c", "d"},
				new String[]{"c", "c", "e"},
				new String[]{"c", "c", "f"}
		}), actualQuestions = new LinkedHashSet<List<String>>();actualQuestions.addAll(questions.getData());
		Assert.assertEquals(expectedQuestions,actualQuestions);
		//updatedGraphExpected.paths.augmentPTA(Arrays.asList("c", "d"),false,false,null);
		//updatedGraphExpected.paths.augmentPTA(Arrays.asList("c", "c", "e"),true,false,null);
		//updatedGraphExpected.paths.augmentPTA(Arrays.asList("c", "c", "f"),false,false,null);
		compareGraphs(updatedGraphExpected,updatedGraphActual);                                               
	}

	private void checkQuestionAugmentation(String fsm, String name)
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph(fsm,name),config);
		StatePair pair = new StatePair(graph.findVertex("A"),graph.findVertex("B"));
		LearnerGraph merged = MergeStates.mergeAndDeterminize_general(graph, pair);
		PTASequenceEngine questions = ComputeQuestions.computeQS_general(pair, graph, merged, new ComputeQuestions.QSMQuestionGenerator());
		LearnerGraph updatedGraphExpected = new LearnerGraph(graph,config),updatedGraphActual = new LearnerGraph(graph,config);
		updatedGraphActual.transitionMatrix.putAll(((NonExistingPaths)questions.getFSM()).getNonExistingTransitionMatrix());
		updatedGraphActual.learnerCache.invalidate();
		
		for(List<String> path:questions.getData())
			updatedGraphExpected.paths.augmentPTA(path,merged.paths.getVertex(path).isAccept(),false,null);

		compareGraphs(updatedGraphExpected,updatedGraphActual);                                               
	}
	
	/** Using a machine from TestRpniLearner. */
	@Test
	public final void testBuildPTAofQuestions2()
	{
		checkQuestionAugmentation("A-a->B-a->C-b->D\n"+
				"A-b->E",
				"testPairCompatible1");
	}

	@Test
	public final void testBuildPTAofQuestions3()
	{
		checkQuestionAugmentation("A-p->B\n"+
				"A-a->P1-c->B1-b->C1-e->D1\n"+
				"B-a->P2-c->B\n"+
				"A-b->C2-e->D2\n"+
				"B-b->C3-e->D3",
				"testPairCompatible_general_A");
	}

	@Test
	public final void testBuildPTAofQuestions4()
	{
		checkQuestionAugmentation("A-p->B\n"+
				"A-a->B\nA-b->B\nA-e->B\n"+
				"B-e->B4-c->D3-a->T1\n"+
				"B-e->B4-d->C3-e->T1\n"+
				"B-c->D1-a->T2\n"+
				"B-b->B5-c->D2-a->T3\n"+
				"B-a->B1-d->C1-e->T4\n"+
				"B1-a->B2-a->B3-d->C2-e->T5",
				"testPairCompatible_general_B");
	}

	@Test
	public final void testBuildPTAofQuestions5()
	{
		checkQuestionAugmentation("A-p->B\n"+
				"A-a->B\nA-b->B\nA-e->B\n"+
				"B4-c->D3-a->T1\n"+
				"B-e->B10-e->B11-e->B12-e->B13-e->B14-e->B15-e->B4-d->C3-e->T1\n"+
				"B-c->D1-a->T2\n"+
				"B-b->B5-c->D2-a->T3\n"+
				"B-a->B1-d->C1-e->T4\n"+
				"B1-a->B2-a->B3-d->C2-e->T5",
				"testPairCompatible_general_C");
	}

	/** Using a machine from TestRpniLearner. */
	@Test
	public final void testBuildPTAofQuestions6()
	{
		checkQuestionAugmentation(TestRpniLearner.testGeneralD_fsm,
				"testPairCompatible5");
	}
	
	private static final String ifthenC = "A-a->B-b->C-a->B / P-a->Q-b->R / S-c->S / T-d->N / S=THEN=C=THEN=P / B=THEN=T";

	/** Nothing to augment. */
	@Test
	public final void testPerformAugment1() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-c->B", "testPerformAugment1"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph(ifthenC, "ifthenC"), config)};
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 5);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-c->B", "testPerformAugment1"),config), graph);
	}
	
	/** One state is augmented. */
	@Test
	public final void testPerformAugment2a() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B", "testPerformAugment2a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph(ifthenC, "ifthenC"), config)};
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 5);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B-d->C", "testPerformAugment2b"),config), graph);
	}
	
	/** Nothing is augmented because the collection of properties is empty. */
	@Test
	public final void testPerformAugment2b() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B", "testPerformAugment2a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{};
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 5);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B", "testPerformAugment1"),config), graph);
	}
	
	/** Cannot augment: depth is zero. */
	@Test
	public final void testPerformAugment3() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B", "testPerformAugment2a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph(ifthenC, "ifthenC"), config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 0);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->BC", "testPerformAugment3"),config), graph);
	}
	
	/** Two states are augmented. */
	@Test
	public final void testPerformAugment4() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C", "testPerformAugment4a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph(ifthenC, "ifthenC"), config)};
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 1);
		//Visualiser.updateFrame(graph, null);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B-d->U / B-b->C-a->Q / C-c->R", "testPerformAugment4b"),config), graph);
	}
	
	/** Two states are augmented a bit further. */
	@Test
	public final void testPerformAugment5() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C", "testPerformAugment4a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph(ifthenC, "ifthenC"), config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B-d->N1 / B-b->C-a->Q-b->R1/ Q-d->N2 / C-c->S1-c->S2", "testPerformAugment5b"),config), graph);
	}
	
	/** Contradiction between a new state and a graph, first when everything is ok. */
	@Test
	public final void testPerformAugment6() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B", "testPerformAugment6"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B-a->B /  T-b-#N / B=THEN=T", "testPerformAugment_fail1"), config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B-b-#N1", "testPerformAugment6b"),config), graph);
	}
	
	/** Non-existing vertices in a tentative automaton. */
	@Test
	public final void testPerformAugment_fail0()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C", "testPerformAugment_fail0"),config);
		graph.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(new VertexID(VertKind.NONEXISTING,90),config),graph.createNewRow());
		Helper.checkForCorrectException(new whatToRun() { public void run() throws IncompatibleStatesException {
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B-a->B /  T-b-#N / B=THEN=T", "testPerformAugment_fail1"), config)}; 
			Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		}},IllegalArgumentException.class,"non-existing vertices");
	}
	
	/** Contradiction between a new state and a graph. */
	@Test
	public final void testPerformAugment_fail1()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C", "testPerformAugment_fail1a"),config);
		Helper.checkForCorrectException(new whatToRun() { public void run() throws IncompatibleStatesException {
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B-a->B /  T-b-#N / B=THEN=T", "testPerformAugment_fail1"), config)}; 
			Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		}},AugmentFromIfThenAutomatonException.class,"cannot merge a tentative state");
	}
	
	/** Contradiction between a new state and a graph after unrolling the property a few times. */
	@Test
	public final void testPerformAugment_fail2()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a->D-b->E", "testPerformAugment_fail1a"),config);
		Helper.checkForCorrectException(new whatToRun() { public void run() throws IncompatibleStatesException {
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B-a->B /  T-b-#N / B=THEN=T", "testPerformAugment_fail1"), config)}; 
			Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		}},AugmentFromIfThenAutomatonException.class,"cannot merge a tentative state");
	}
	
	/** Contradiction between states added by THEN graphs, first when everything is ok. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testPerformAugment7() throws IncompatibleStatesException
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B", "testPerformAugment_fail1a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B-a->B /  T-c->T1-c->T2-c-#T3 / R-c->R1-c->R2-b->R3 / R=THEN=B=THEN=T", "testPerformAugment_fail1"), config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B-c->T1-c->T2", "testPerformAugment6b"),config), graph);
	}
	
	/** Contradiction between states added by THEN graphs, first when everything is ok. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testPerformAugment8() throws IncompatibleStatesException
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B", "testPerformAugment_fail1a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B-a->B /  T-c->T1-c->T2-c-#T3 / R-c->R1-c->R2-b->R3 / R=THEN=B=THEN=T", "testPerformAugment_fail1"), config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 3);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B-c->T1-c->T2-c-#T3 / T2-b->T4", "testPerformAugment6b"),config), graph);
	}
	
	/** Contradiction between states added by THEN graphs, first when everything is ok. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testPerformAugment9() throws IncompatibleStatesException
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B", "testPerformAugment_fail1a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B-a->B /  T-c->T1-c->T2-c-#T3 / R-c->R1-c->R2-b->R3 / R=THEN=B=THEN=T", "testPerformAugment_fail1"), config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 7);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B-c->T1-c->T2-c-#T3 / T2-b->T4", "testPerformAugment6b"),config), graph);
	}
	
	/** Not yet a contradiction between states added by THEN graphs - the depth of exploration is 
	 * too low to hit it. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testPerformAugment10() throws IncompatibleStatesException
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B", "testPerformAugment_fail1a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B-a->B /  T-c->T1-c->T2-b-#T3 / R-c->R1-c->R2-b->R3 / R=THEN=B=THEN=T", "testPerformAugment_fail1"), config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B-c->T1-c->T2", "testPerformAugment6b"),config), graph);
	}
	
	/** Contradiction between states added by THEN graphs. */
	@Test
	public final void testPerformAugment_fail3()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B", "testPerformAugment_fail1a"),config);
		Helper.checkForCorrectException(new whatToRun() { public void run() throws IncompatibleStatesException {
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B-a->B /  T-c->T1-c->T2-b-#T3 / R-c->R1-c->R2-b->R3 / R=THEN=B=THEN=T", "testPerformAugment_fail1"), config)};
			Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 3);
		}},AugmentFromIfThenAutomatonException.class,"cannot merge a tentative state");
	}
	
	/** Incompatibility between an existing state and a new one. */
	@Test
	public final void testPerformAugment_fail4()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-b-#C", "testPerformAugment_fail4a"),config);
		Helper.checkForCorrectException(new whatToRun() { public void run() throws IncompatibleStatesException {
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B-a->B /  T-b->N / B=THEN=T", "testPerformAugment_fail4"), config)}; 
			Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		}},AugmentFromIfThenAutomatonException.class,"cannot merge a tentative state");
	}
	
	/** Reject-states cannot be extended even if they match a property. 
	 * @throws AugmentFromIfThenAutomatonException */
	@Test
	public final void testPerformAugment_reject1() throws AugmentFromIfThenAutomatonException
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a-#B", "testPerformAugment_reject1a"),config);
		LearnerGraph ifthen = new LearnerGraph(FsmParser.buildGraph("A-a->B-a->B /  T-b->N / B=THEN=T", "testPerformAugment_fail4"), config);
		ifthen.findVertex("T").setAccept(false);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{ifthen}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a-#B", "testPerformAugment_reject1b"),config), graph);
	}
	
	/** Another example of a contradiction between a tentative graph and the property. */
	@Test
	public final void testPerformAugment_fail5()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-b-#C", "testPerformAugment_fail5a"),config);
		Helper.checkForCorrectException(new whatToRun() { public void run() throws IncompatibleStatesException {
			LearnerGraph ifthen = new LearnerGraph(FsmParser.buildGraph("A-a->B-a->B /  T-b->N-s->R / B=THEN=T", "testPerformAugment_fail5"), config);
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{ifthen}; 
			Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		}},AugmentFromIfThenAutomatonException.class,"cannot merge a tentative");
	}
	
	/** Dummy property. */
	@Test
	public final void testPerformAugment11() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B", "testPerformAugment11a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B / P-b->P / A==THEN==P", "testPerformAugment11"), config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 4);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B / A-b->C-b->D-b->E-b->F", "testPerformAugment11b"),config), graph);
	}
	
	/** Dummy limited property. */
	@Test
	public final void testPerformAugment12() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B", "testPerformAugment11a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B / P-b->Q-c->R / A==THEN==P", "testPerformAugment11"), config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 4);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B / A-b->C-c->D", "testPerformAugment11b"),config), graph);
	}
	
	/** Dummy limited property and the first automaton does not match at all - which does not matter. */
	@Test
	public final void testPerformAugment13() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-s->B", "testPerformAugment11a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B / P-b->Q-c->R / A==THEN==P", "testPerformAugment11"), config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 4);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-s->B / A-b->C-c->D", "testPerformAugment11b"),config), graph);
	}
	
	/** Infinitely matching property. */
	@Test
	public final void testPerformAugment14() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C-a->B", "testPerformAugment14a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C / P-a->Q-b->P-c->S / C==THEN==P", "testPerformAugment14"), config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 400);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C-a->B / C-c->D", "testPerformAugment14b"),config), graph);
	}

	/** Two properties where there is one which matches after a while and another short one which depends on the result of the match of the first one. */ 
	@Test
	public final void testPerformAugment15() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-b->A", "testPerformAugment15a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{
				new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C-a->D-b->E / P-a->Q-c->S / E==THEN==P", "testPerformAugment15c"), config),
				new LearnerGraph(FsmParser.buildGraph("A-a->B-c->C / P-d->Q / C==THEN==P", "testPerformAugment15d"), config)				
		}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 400);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("bA-a->bB-b->bA / bB-c->bC-d->bD", "testPerformAugment15b"),config), graph);
	}
	
	/** Two properties where there is one which matches after a while and another short one which depends on the result of the match of the first one. */ 
	@Test
	public final void testPerformAugment16() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-b->A", "testPerformAugment15a"),config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{
				new LearnerGraph(FsmParser.buildGraph("A-a->B-b->C-a->D-b->E / B-c->F-d->G / P-a->Q-c->S / T-e->U / E==THEN==P / G==THEN==T", "testPerformAugment16c"), config),
				new LearnerGraph(FsmParser.buildGraph("1A-a->1B-c->1C / 1P-d->1Q / 1C==THEN==1P", "testPerformAugment16d"), config)				
		}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 400);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("bA-a->bB-b->bA / bB-c->bC-d->bD-e->bE", "testPerformAugment16b"),config), graph);
	}
	
	/** Tests how properties can answer questions. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testQuestionAnswering1() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph(
				"A1-a->B1-b->A2-a->B2-b->A3-a->B3-b->A4-a->B4-b->A5 /"+
				"A2-c->A2_U1-c->A2_U2-e->A2_U3 /"+
				"A3-c->A3_U1-c->A3_U2 / A3_U1-d-#A3_U4 /"+
				"A4-c->A4_U1-c->A4_U2-f-#A4_U3 /"+
				"A5-c->A5_U1-c->A5_U2"
				, "testBuildPTAofQuestions1"),config);
		//Visualiser.updateFrame(graph, null);
		StatePair pair = new StatePair(graph.findVertex("A1"),graph.findVertex("A2"));
		LearnerGraph merged = MergeStates.mergeAndDeterminize_general(graph, pair);
		compareGraphs(new LearnerGraph(FsmParser.buildGraph("A1-a->B1-b->A1-c->C-d-#R4/C-c->CC/CC-f-#R3/CC-e->D", "testQuestionAnswering2b"),config),merged);
		PTASequenceEngine questions = ComputeQuestions.computeQS_general(pair, graph, merged, new ComputeQuestions.QSMQuestionGenerator());
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph("A-s->B / P-c->Q-d-#R / S-c->S1-c->S2-e->S3 / S==THEN==A==THEN==P", "testPerformAugment11"), config)}; 
		Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
		List<List<String>> questionList = questions.getData();
		Assert.assertEquals(1,questionList.size());
		Assert.assertEquals(Arrays.asList(new String[]{"c","c","f"}), questionList.iterator().next());
	}

	@Test
	public final void testConversionOfAssociationsToTransitions1()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B / P-b->Q-c->R / ", "testConversionOfAssociationsToTransitions1a"), config);
		DirectedSparseGraph graphAfterConversion = PathRoutines.convertPairAssociationsToTransitions(graph, config);
		graph.pairCompatibility.compatibility.clear();
		LearnerGraph obtainedGraph = new LearnerGraph(graphAfterConversion,config);
		WMethod.checkM_and_colours(new LearnerGraph(FsmParser.buildGraph("A-a->B / P-b->Q-c->R / ",
				"testConversionOfAssociationsToTransitions1b"),config),obtainedGraph ,VERTEX_COMPARISON_KIND.DEEP);
		Assert.assertNull(graphAfterConversion.getUserDatum(JUConstants.VERTEX));
		Assert.assertTrue(
				((Map<String,Map<String,Map<String,Color>>>)graphAfterConversion.getUserDatum(JUConstants.EDGE)).isEmpty());
	}
	
	@Test
	public final void testConversionOfAssociationsToTransitions2()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B / P-b->Q-c->R / A==THEN==P / B=INCOMPATIBLE=Q=MERGED=R", "testConversionOfAssociationsToTransitions2a"), config);
		DirectedSparseGraph graphAfterConversion = PathRoutines.convertPairAssociationsToTransitions(graph, config);
		graph.pairCompatibility.compatibility.clear();
		LearnerGraph obtainedGraph = new LearnerGraph(graphAfterConversion,config);
		WMethod.checkM_and_colours(new LearnerGraph(FsmParser.buildGraph("A-a->B / P-b->Q-c->R / "+
				"A-"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.THEN.name()+"->P / "+
				"P-"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.THEN.name()+"->A / "+
				
				"B-"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.INCOMPATIBLE.name()+"->Q / "+
				"Q-"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.INCOMPATIBLE.name()+"->B / "+
				
				"Q-"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.MERGED.name()+"->R / "+
				"R-"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.MERGED.name()+"->Q / ", 
				"testConversionOfAssociationsToTransitions2b"),config), obtainedGraph,VERTEX_COMPARISON_KIND.DEEP);

		Assert.assertNull(graphAfterConversion.getUserDatum(JUConstants.VERTEX));
		Assert.assertEquals("{A={"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.THEN.name()+"={P=java.awt.Color[r=255,g=255,b=0]}}, " +
				"B={"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.INCOMPATIBLE.name()+"={Q=java.awt.Color[r=255,g=255,b=0]}}, " +
				"P={"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.THEN.name()+"={A=java.awt.Color[r=255,g=255,b=0]}}, " +
				"Q={"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.INCOMPATIBLE.name()+"={B=java.awt.Color[r=255,g=255,b=0]}, "+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.MERGED.name()+"={R=java.awt.Color[r=255,g=255,b=0]}}, " +
				"R={"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.MERGED.name()+"={Q=java.awt.Color[r=255,g=255,b=0]}}}",
				((Map<String,Map<String,Map<String,Color>>>)graphAfterConversion.getUserDatum(JUConstants.EDGE)).toString());

	}

	public static final class TestQuestionPTA
	{
		private final static String ifthen_sc = "I-s->A-c->B / P-d-#R / P-c->T1-e->T2 / P-a->T / P==THEN==B",
		ifthen_s = "I-s->B / P-c->Q-d-#R / P-a->T / P-b->T1 / P==THEN==B",
		graphWithAppendixAfterMerging = "A-s->A1 / "+
		"A1-a->B1-b->A2-a->B2-b->A3-a->B3-b->A4-a->B4-b->A5 /"+
		"A2-c->A2_U1-c->A2_U2-e->A2_U3 /"+
		"A3-c->A3_U1-c->A3_U2 / A3_U1-d-#A3_U4 /"+
		"A4-c->A4_U1-c->A4_U2-f-#A4_U3 /"+
		"A5-c->A5_U1-c->A5_U2";
	
		Configuration config;

		LearnerGraph graph = null;
		private StatePair pair = null;
		private LearnerGraph merged =null;
		private PTASequenceEngine questions = null;
		private DirectedSparseGraph origGraph = null;
		
		@Before
		public final void beforeTest()
		{
			config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
			origGraph = FsmParser.buildGraph(graphWithAppendixAfterMerging, "graphWithAppendixAfterMerging");
			graph = new LearnerGraph(origGraph,config);
			pair = new StatePair(graph.findVertex("A1"),graph.findVertex("A2"));
			merged = MergeStates.mergeAndDeterminize_general(graph, pair);
			compareGraphs(new LearnerGraph(FsmParser.buildGraph("A-s->A1-a->B1-b->A1-c->C-d-#R4/C-c->CC/CC-f-#R3/CC-e->D", "testQuestionAnswering2b"),config),merged);
			questions = ComputeQuestions.computeQS_general(pair, graph, merged, new ComputeQuestions.QSMQuestionGenerator());
			Assert.assertEquals(3,questions.getData().size());
		}
		
		/** Tests that the question part is matched to the IF part only if it matches the THEN part.
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering2() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph(ifthen_sc, "ifthen_sc"), config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			List<List<String>> questionList = questions.getData();
			Assert.assertEquals(3,questionList.size());
			Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","d"},new String[]{"s","c","c","f"},new String[]{"s","c","c","e"}});
			Set<List<String>> actual = new LinkedHashSet<List<String>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Tests that the question part is matched to the IF part only if it matches the THEN part.
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering3a() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph(ifthen_s, "ifthen_s"), config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			List<List<String>> questionList = questions.getData();
			Assert.assertEquals(2,questionList.size());
			Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"},new String[]{"s","c","c","e"}});
			Set<List<String>> actual = new LinkedHashSet<List<String>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		
			// now do the same again - should not change anything. 
			ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph(ifthen_s, "ifthen_s"), config)};
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			questionList = questions.getData();
			Assert.assertEquals(2,questionList.size());
			actual = new LinkedHashSet<List<String>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Same as above but do the two (identical) things at the same time. */
		@Test
		public final void testQuestionAnswering3b() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{
					new LearnerGraph(FsmParser.buildGraph(ifthen_s, "ifthen_s"), config), 
					new LearnerGraph(FsmParser.buildGraph(ifthen_s, "ifthen_s"), config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			List<List<String>> questionList = questions.getData();
			Assert.assertEquals(2,questionList.size());
			Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"},new String[]{"s","c","c","e"}});
			Set<List<String>> actual = new LinkedHashSet<List<String>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		
			// now do the same again - should not change anything. 
			ifthenCollection = new LearnerGraph[]{
					new LearnerGraph(FsmParser.buildGraph(ifthen_s, "ifthen_s"), config),
					new LearnerGraph(FsmParser.buildGraph(ifthen_s, "ifthen_s"), config)};
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			questionList = questions.getData();
			Assert.assertEquals(2,questionList.size());
			actual = new LinkedHashSet<List<String>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Tests that the question part is matched to the IF part only if it matches the THEN part.
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering4a() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph(ifthen_s, "ifthen_s"), config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph(ifthen_sc, "ifthen_sc"), config)};
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			List<List<String>> questionList = questions.getData();
			Assert.assertEquals(1,questionList.size());
			Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}});
			Set<List<String>> actual = new LinkedHashSet<List<String>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Same as above but do the two (identical) things at the same time.
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering4b() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{
					new LearnerGraph(FsmParser.buildGraph(ifthen_s, "ifthen_s"), config), 
					new LearnerGraph(FsmParser.buildGraph(ifthen_sc, "ifthen_sc"), config)
			};
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			List<List<String>> questionList = questions.getData();
			Assert.assertEquals(1,questionList.size());
			Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}});
			Set<List<String>> actual = new LinkedHashSet<List<String>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Tests that the question part is matched to the IF part only if it has been answered by a user.
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering5() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph(ifthen_sc, "ifthen_sc"), config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			graph.learnerCache.questionsPTA=questions;
			graph.transform.AugmentNonExistingMatrixWith(Arrays.asList(new String[]{"s","c","d"}), false);
			Assert.assertEquals(2,questions.getData().size());
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
	
			List<List<String>> questionList = questions.getData();
			Assert.assertEquals(1,questionList.size());
			Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}});
			Set<List<String>> actual = new LinkedHashSet<List<String>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Tests that the question part is matched to the IF part only if it has been answered by a user.
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering6() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{new LearnerGraph(FsmParser.buildGraph(ifthen_sc, "ifthen_sc"), config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			graph.learnerCache.questionsPTA=questions;
			graph.transform.AugmentNonExistingMatrixWith(Arrays.asList(new String[]{"s","c"}), true);
			Assert.assertEquals(3,questions.getData().size());
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
	
			List<List<String>> questionList = questions.getData();
			Assert.assertEquals(1,questionList.size());
			Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}});
			Set<List<String>> actual = new LinkedHashSet<List<String>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Tests marking of questions as answered. */
		@Test
		public final void testQuestionMarking1()
		{
			graph.learnerCache.questionsPTA = questions;
			graph.transform.AugmentNonExistingMatrixWith(Arrays.asList(new String[]{"s","c"}), true);
			Assert.assertEquals(3,questions.getData().size());
			graph.transform.AugmentNonExistingMatrixWith(Arrays.asList(new String[]{"s","c","d"}), false);
			Assert.assertEquals(2,questions.getData().size());
		}
		
		/** Tests marking of questions as answered. */
		@Test
		public final void testQuestionMarking_unmatched()
		{
			graph.learnerCache.questionsPTA = questions;
			Assert.assertFalse(graph.transform.AugmentNonExistingMatrixWith(Arrays.asList(new String[]{"s","c"}), false));
			Assert.assertFalse(graph.transform.AugmentNonExistingMatrixWith(Arrays.asList(new String[]{"s","c","c"}), false));
			Assert.assertFalse(graph.transform.AugmentNonExistingMatrixWith(Arrays.asList(new String[]{"s","c","d"}), true));
		}
		
		/** The integration of test of questions, similar to <em>testQuestionAnswering4</em> but 
		 * using different methods. */
		@Test
		public final void testQuestions_and_marking1a()
		{
			//Visualiser.updateFrame(graph, null);
			//Visualiser.updateFrame(ComputeQuestions.constructGraphWithQuestions(pair,graph,merged), new LearnerGraph(FsmParser.buildGraph(ifthen_s, "ifthen_s"),config));
			//Visualiser.updateFrame(ComputeQuestions.constructGraphWithQuestions(pair,graph,merged),new LearnerGraph(FsmParser.buildGraph(ifthen_sc, "ifthen_sc"),config));
			List<List<String>> qs = ComputeQuestions.computeQS(pair, graph, merged, new LearnerGraph[] {
					new LearnerGraph(FsmParser.buildGraph(ifthen_s, "ifthen_s"),config),
					new LearnerGraph(FsmParser.buildGraph(ifthen_sc, "ifthen_sc"),config)
			});
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			Assert.assertEquals(1,qs.size());
			Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}});
			Set<List<String>> actual = new LinkedHashSet<List<String>>();actual.addAll(qs);
			Assert.assertEquals(expected,actual);
		}
		
		/** Almost the same as the above, but the order of elements in the array of properties is different. */
		@Test
		public final void testQuestions_and_marking1b()
		{
			List<List<String>> qs = ComputeQuestions.computeQS(pair, graph, merged, new LearnerGraph[] {
					new LearnerGraph(FsmParser.buildGraph(ifthen_sc, "ifthen_sc"),config),
					new LearnerGraph(FsmParser.buildGraph(ifthen_s, "ifthen_s"),config)
			});
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			Assert.assertEquals(1,qs.size());
			Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}});
			Set<List<String>> actual = new LinkedHashSet<List<String>>();actual.addAll(qs);
			Assert.assertEquals(expected,actual);
		}
		
		/** The integration of test of questions, similar to <em>testQuestionAnswering6</em> but 
		 * using different methods. */
		@Test
		public final void testQuestions_and_marking2()
		{
			LearnerGraph[] properties = new LearnerGraph[]{
					new LearnerGraph(FsmParser.buildGraph(ifthen_sc, "ifthen_sc"),config)};
			List<List<String>> qs = ComputeQuestions.computeQS(pair, graph, merged, properties);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			Assert.assertEquals(3,qs.size());
			
			graph.transform.AugmentNonExistingMatrixWith(Arrays.asList(new String[]{"s","c"}), true);
			qs = ComputeQuestions.RecomputeQS(pair, graph, merged, properties);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			Assert.assertEquals(1,qs.size());

			Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}});
			Set<List<String>> actual = new LinkedHashSet<List<String>>();actual.addAll(qs);
			Assert.assertEquals(expected,actual);
		}

		/** Interaction between <em>computeQS</em> and <em>recomputeQS</em>. */
		@Test
		public final void testQuestions_and_marking3()
		{
			LearnerGraph [] properties = new LearnerGraph[] {
					new LearnerGraph(FsmParser.buildGraph(ifthen_sc, "ifthen_sc"),config)};
			List<List<String>> qs = ComputeQuestions.computeQS(pair, graph, merged, properties);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			Assert.assertEquals(3,qs.size());

			graph.transform.AugmentNonExistingMatrixWith(Arrays.asList(new String[]{"s","c"}), true);
			// the above call should not affect computeQS
			qs = ComputeQuestions.computeQS(pair, graph, merged, properties);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			Assert.assertEquals(3,qs.size());
		}
	}
}
