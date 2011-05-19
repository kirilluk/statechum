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

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
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
import statechum.Label;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.TestRpniLearner;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.LearnerGraph.NonExistingPaths;
import statechum.analysis.learning.rpnicore.PathRoutines.EdgeAnnotation;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;
import statechum.apps.QSMTool;
import statechum.model.testset.PTASequenceEngine;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;

final public class TestAugmentUsingIFTHEN 
{
	/** Converts arrays of labels to lists of labels using config - it does not really matter which configuration is used 
	 * because all of them start from a default one and do not modify label type.
	 * 
	 * @param labels what to convert
	 * @return the outcome of conversion.
	 */
	protected static List<Label> labelList(String [] labels)
	{
		return AbstractLearnerGraph.buildList(Arrays.asList(labels),Configuration.getDefaultConfiguration());
	}

	/** Tests merging of the two automata depicted on page 18 of "why_nondet_does_not_matter.xoj" */
	@Test
	public final void testAugmentFromMax1_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = buildLearnerGraph("H-a->A-a->B-b->C\nH-c->B\nH-d->B", "testAugmentFromMax1_gr",config);
		LearnerGraph max = buildLearnerGraph("I-a->D-a-#E\nI-d-#E\nI-c->F-b->G", "testAugmentFromMax1_max",config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true, config, true);
		TestEquivalenceChecking.checkM("H-a->A-a-#BE\nH-d-#BE\nH-c->BF-b->C", result.pathroutines.getGraph(), config);
		Assert.assertEquals(5,result.getStateNumber());
	}
	
	/** Tests merging of the two automata on page 18 of "why_nondet_does_not_matter.xoj" */
	@Test
	public final void testAugmentFromMax1_nonoverride()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph gr = buildLearnerGraph("H-a->A-a->B-b->C\nH-c->B\nH-d->B", "testAugmentFromMax1_gr",config);
		final LearnerGraph max = buildLearnerGraph("I-a->D-a-#E\nI-d-#E\nI-c->F-b->G", "testAugmentFromMax1_max",config);
		checkForCorrectException(new whatToRun() {	public @Override void run() throws NumberFormatException 
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
		LearnerGraph gr = buildLearnerGraph(automatonWithReject, "testAugmentFromMax1_max",config);
		LearnerGraph max = buildLearnerGraph("H-a->A-a->B-b->C\nH-c->B\nH-d->B", "testAugmentFromMax1_gr",config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}

	/** Tests merging of the two automata on page 17 of "why_nondet_does_not_matter.xoj" */
	@Test
	public final void testAugmentFromMax2_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = buildLearnerGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr",config);
		LearnerGraph max = buildLearnerGraph("B-b->D-b->F-a->F-b->B", "testAugmentFromMax2_max",config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}

	/** Tests merging of the two automata on page 17 of "why_nondet_does_not_matter.xoj" */
	@Test
	public final void testAugmentFromMax3_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = buildLearnerGraph("A-b->A-a->C-b->C-a->E-b-#G\n"+
				"A-c->A\nC-c->C", "testAugmentFromMax3_gr",config);
		LearnerGraph max = buildLearnerGraph("B-b->D-b->F-a->F-b->B\n"+
				"B-c->D-c->F-c->B", "testAugmentFromMax3_max",config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}
	
	@Test
	public final void testAugmentFromMax4_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		String origGraph = "A-b->A-a->A-c->B-c->C\n";
		LearnerGraph gr = buildLearnerGraph(origGraph, "testAugmentFromMax4_gr",config);
		LearnerGraph max = buildLearnerGraph("E-a->F-a->G-a->H", "testAugmentFromMax4_max",config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config,true);
		Assert.assertNull(result);
	}

	@Test
	public final void testAugmentFromMax5_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = buildLearnerGraph("A-b->A-a->A-c->B-c->C\n", "testAugmentFromMax4_gr",config);
		LearnerGraph max = buildLearnerGraph("E-a->F-a->G-a->H-a-#I", "testAugmentFromMax5_max",config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config,true);
		TestEquivalenceChecking.checkM("AE-a->AF-a->AG-a->AH-a-#I\n"+
				"AE-b->P-c->B-c->C\nP-a->P-b->P\nAE-c->B\nAF-b->P\nAF-c->B\nAG-b->P\nAG-c->B\nAH-b->P\nAH-c->B", result.pathroutines.getGraph(), config);
	}
	
	@Test
	public final void testAugmentFromMax6_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = buildLearnerGraph("A-b->A-a->A-c->B-c->C\n", "testAugmentFromMax4_gr",config);
		LearnerGraph max = new LearnerGraph(config);max.getInit().setAccept(false);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config,true);
		Assert.assertNull(WMethod.checkM(max, result));
	}
	
	@Test
	public final void testAugmentFromMax6_AB_nooverride()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph gr = buildLearnerGraph("A-b->A-a->A-c->B-c->C\n", "testAugmentFromMax4_gr",config);
		final LearnerGraph max = new LearnerGraph(config);max.getInit().setAccept(false);
		checkForCorrectException(new whatToRun() {	public @Override void run() throws NumberFormatException 
			{
				Transform.augmentFromMAX(gr, max, false, true,config, true);
			}}, IllegalArgumentException.class, "incompatible");
	}

	@Test
	public final void testAugmentFromMax6_BA()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = new LearnerGraph(config);gr.getInit().setAccept(false);
		LearnerGraph max = buildLearnerGraph("A-b->A-a->A-c->B-c->C\n", "testAugmentFromMax4_gr",config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config,true);
		Assert.assertNull(result);
	}

	@Test
	public final void testAugmentFromMax7_AB()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = buildLearnerGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr",config);
		LearnerGraph max = buildLearnerGraph("B-b->D-b->F-a->F-b->B\nD-a-#E", "testAugmentFromMax7_max",config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		TestEquivalenceChecking.checkM("AB-b->AD-b->AF-b->AB\nAF-a->CF-b->CB-b->CD-b->CF-a->EF-b-#G\n"+
				"AB-a->C-b->C-a->E-b-#G\nCB-a->E\nAD-a-#H\nCD-a-#H", result.pathroutines.getGraph(), config);
	}

	@Test
	public final void testAugmentFromMax7_AB_nooverride()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph gr = buildLearnerGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr",config);
		final LearnerGraph max = buildLearnerGraph("B-b->D-b->F-a->F-b->B\nD-a-#E", "testAugmentFromMax7_max",config);
		checkForCorrectException(new whatToRun() {	public @Override void run() throws NumberFormatException 
			{
				Transform.augmentFromMAX(gr, max, false, true,config, true);
			}}, IllegalArgumentException.class, "incompatible");
	}
	
	@Test
	public final void testAugmentFromMax8_a()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = buildLearnerGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr",config);
		LearnerGraph max = new LearnerGraph(config);
		LearnerGraph result = Transform.augmentFromMAX(gr, max, true, true,config, true);
		Assert.assertNull(result);
	}
	
	@Test
	public final void testAugmentFromMax8_b()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph gr = buildLearnerGraph("A-b->A-a->C-b->C-a->E-b-#G", "testAugmentFromMax2_gr",config);
		LearnerGraph max = buildLearnerGraph("A-a->A-b->A-c->A-d->A", "testAugmentFromMax7_max",config);
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
		Transform.checkTHEN_disjoint_from_IF(buildLearnerGraph(ifthenA,"ifthen",Configuration.getDefaultConfiguration()));
	}
	
	@Test
	public final void testCheckIFTHEN2()
	{
		Transform.checkTHEN_disjoint_from_IF(buildLearnerGraph(ifthenB,"ifthen",Configuration.getDefaultConfiguration()));
	}
	
	@Test
	public final void testCheckIFTHEN_fail0a()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			Transform.checkTHEN_disjoint_from_IF(new LearnerGraph(Configuration.getDefaultConfiguration()));
		}}, IllegalArgumentException.class,"no THEN states");
	}
	
	@Test
	public final void testCheckIFTHEN_fail0b()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			LearnerGraph graph = new LearnerGraph(Configuration.getDefaultConfiguration());graph.getInit().setAccept(false);
			Transform.checkTHEN_disjoint_from_IF(graph);
		}}, IllegalArgumentException.class,"no THEN states");
	}
	
	@Test
	public final void testCheckIFTHEN_fail1()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			Transform.checkTHEN_disjoint_from_IF(buildLearnerGraph("A-a->B-a->C-b->B / P-b->Q-a->R / S-c->S / T-d->N","testCheckIFTHEN_fail1",Configuration.getDefaultConfiguration()));
		}}, IllegalArgumentException.class,"no THEN states");
	}
	
	@Test
	public final void testCheckIFTHEN_fail2()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			Transform.checkTHEN_disjoint_from_IF(buildLearnerGraph("A-a->B-a->C-b->B / P-b->Q-a->R / S-c->S / T-d->N / S=THEN=C=THEN=P","testCheckIFTHEN_fail2",Configuration.getDefaultConfiguration()));
		}}, IllegalArgumentException.class,"unreachable");
	}
	
	@Test
	public final void testCheckIFTHEN_fail3()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			Transform.checkTHEN_disjoint_from_IF(buildLearnerGraph("A-a->B-a->C-b->B / P-b->Q-a->B / S-c->S / T-d->N / S=THEN=C=THEN=P / B=THEN=T","testCheckIFTHEN_fail2",Configuration.getDefaultConfiguration()));
		}}, IllegalArgumentException.class,"are shared between");
	}
	
	@Test
	public final void testCheckIFTHEN_fail4()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			Transform.checkTHEN_disjoint_from_IF(buildLearnerGraph("A-a->B-a->C-b->B / P-b->Q-a->Q / S-c->S / T-d->N / S=THEN=C=THEN=P / S=THEN=T","testCheckIFTHEN_fail2",Configuration.getDefaultConfiguration()));
		}}, IllegalArgumentException.class,"do not belong");
	}
	
	@Test
	public final void testbuildIfThenAutomata1()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		String ltlFormula = "!([](a->X[]b))";
		Collection<LearnerGraph> automata = Transform.buildIfThenAutomata(Arrays.asList(new String[]{
				QSMTool.cmdLTL+" "+ltlFormula}), buildLearnerGraph("A-a->B-b->C-c->D", "testbuildIfThenAutomata1", config),config);
		Iterator<LearnerGraph> graphIter = automata.iterator();

		LearnerGraph topGraph = graphIter.next(), expectedTop = buildLearnerGraph("I-a->A-b->A / I-b->IA-a->A / I-c->IA-b->IA-c->IA / P-c-#P1 / P-a-#P2 / A = THEN = P / " +
				"I - transition_to_THEN ->P","!("+ltlFormula+")",config);
		topGraph.addTransition(topGraph.transitionMatrix.get(topGraph.getInit()), AbstractLearnerGraph.generateNewLabel("transition_to_THEN",config), topGraph.findVertex(VertexID.parseID("P"+(topGraph.vertPositiveID-1))));
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
			}), buildLearnerGraph("A-a->B-b->C-c->D-d->E", "testbuildIfThenAutomata1", config),config);
		Iterator<LearnerGraph> graphIter = automata.iterator();

		LearnerGraph topGraph = graphIter.next(), expectedTop = buildLearnerGraph("I-c->A / I-d->A / A-a->A-b->A-c->A-d->A / P2#-b-P-a-#P1 / I = THEN = P / " +
				"I - transition_to_THEN ->P","!("+ltlFormulaA+"||"+ltlFormulaB+")",config);
		topGraph.addTransition(topGraph.transitionMatrix.get(topGraph.getInit()), AbstractLearnerGraph.generateNewLabel("transition_to_THEN",config), topGraph.findVertex(VertexID.parseID("P"+(topGraph.vertPositiveID-1))));
		LearnerGraph next = null;
		compareGraphs(expectedTop, topGraph);Assert.assertEquals("LTL",topGraph.getName());
		
		next=graphIter.next();Assert.assertEquals("graphA", next.getName());
		next.addTransition(next.transitionMatrix.get(next.getInit()), AbstractLearnerGraph.generateNewLabel("transition_to_THEN",config), next.findVertex("P"));
		compareGraphs(buildLearnerGraph("A-a->B / P-a->P == THEN == A-transition_to_THEN->P","1",config),next);
		
		next=graphIter.next();Assert.assertEquals("graphB", next.getName());
		next.addTransition(next.transitionMatrix.get(next.getInit()), AbstractLearnerGraph.generateNewLabel("transition_to_THEN_P",config), next.findVertex("P"));
		next.addTransition(next.transitionMatrix.get(next.getInit()), AbstractLearnerGraph.generateNewLabel("transition_to_THEN_S",config), next.findVertex("S"));
		next.addTransition(next.transitionMatrix.get(next.getInit()), AbstractLearnerGraph.generateNewLabel("transition_to_THEN_T",config), next.findVertex("T"));
		compareGraphs(buildLearnerGraph(ifthenA+" / A-transition_to_THEN_P->P / A-transition_to_THEN_S->S / A-transition_to_THEN_T->T","2",config),next);
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
			}), buildLearnerGraph("A-a->B-b->C-c->D-d->E", "testbuildIfThenAutomata1", config),config);
		Iterator<LearnerGraph> graphIter = automata.iterator();

		LearnerGraph next = null;
		
		next=graphIter.next();Assert.assertEquals("graphA", next.getName());
		next.addTransition(next.transitionMatrix.get(next.getInit()), AbstractLearnerGraph.generateNewLabel("transition_to_THEN",config), next.findVertex("P"));
		compareGraphs(buildLearnerGraph("A-a->B / P-a->P == THEN == A-transition_to_THEN->P","1",config),next);
		
		next=graphIter.next();Assert.assertEquals("graphB", next.getName());
		next.addTransition(next.transitionMatrix.get(next.getInit()), AbstractLearnerGraph.generateNewLabel("transition_to_THEN_P",config), next.findVertex("P"));
		next.addTransition(next.transitionMatrix.get(next.getInit()), AbstractLearnerGraph.generateNewLabel("transition_to_THEN_S",config), next.findVertex("S"));
		next.addTransition(next.transitionMatrix.get(next.getInit()), AbstractLearnerGraph.generateNewLabel("transition_to_THEN_T",config), next.findVertex("T"));
		compareGraphs(buildLearnerGraph(ifthenA+" / A-transition_to_THEN_P->P / A-transition_to_THEN_S->S / A-transition_to_THEN_T->T","2",config),next);
		Assert.assertFalse(graphIter.hasNext());
	}

	/** An automaton without a name. */
	@Test
	public final void testbuildIfThenAutomata_fail()
	{
		final Configuration config = Configuration.getDefaultConfiguration();

		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
		Transform.buildIfThenAutomata(Arrays.asList(new String[]{
				QSMTool.cmdLTL+" !a",
				QSMTool.cmdIFTHENAUTOMATON+" graphA"}), buildLearnerGraph("A-a->B-b->C-c->D", "testbuildIfThenAutomata1", config),config);
		}}, IllegalArgumentException.class,"missing automata name");
	}
	
	/** Tests the construction of a PTA from questions. */
	@Test
	public final void testBuildPTAofQuestions1()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);config.setQuestionGenerator(QuestionGeneratorKind.CONVENTIONAL_IMPROVED);
		LearnerGraph graph = buildLearnerGraph(
				"A1-a->B1-b->A2-a->B2-b->A3-a->B3-b->A4-a->B4-b->A5 /"+
				"A2-c->A2_U1-c->A2_U2-e->A2_U3 /"+
				"A3-c->A3_U1-c->A3_U2 / A3_U1-d-#A3_U4 /"+
				"A4-c->A4_U1-c->A4_U2-f-#A4_U3 /"+
				"A5-c->A5_U1-c->A5_U2"
				, "testBuildPTAofQuestions1",config);
		StatePair pair = new StatePair(graph.findVertex("A1"),graph.findVertex("A2"));
		LearnerGraph merged = MergeStates.mergeAndDeterminize_general(graph, pair);
		compareGraphs(buildLearnerGraph("A1-a->B1-b->A1-c->C-d-#R4 / C-c->CC / CC-f-#R3 / CC-e->D", "expected",config),merged);
		PTASequenceEngine questions = ComputeQuestions.getQuestionPta(pair, graph, merged, null);
		LearnerGraph updatedGraphExpected = new LearnerGraph(graph,config),
			updatedGraphActual = ComputeQuestions.constructGraphWithQuestions(pair, graph, merged);

		for(List<Label> path:questions.getData())
			updatedGraphExpected.paths.augmentPTA(path,merged.paths.getVertex(path).isAccept(),false,null);

		Set<List<Label>> expectedQuestions = TestFSMAlgo.buildSet(new String[][] {
				new String[]{"c", "d"},
				new String[]{"c", "c", "e"},
				new String[]{"c", "c", "f"}
		},config), actualQuestions = new LinkedHashSet<List<Label>>();actualQuestions.addAll(questions.getData());
		Assert.assertEquals(expectedQuestions,actualQuestions);
		//updatedGraphExpected.paths.augmentPTA(asList("c", "d"),false,false,null);
		//updatedGraphExpected.paths.augmentPTA(asList("c", "c", "e"),true,false,null);
		//updatedGraphExpected.paths.augmentPTA(asList("c", "c", "f"),false,false,null);
		compareGraphs(updatedGraphExpected,updatedGraphActual);                                               
	}

	/** Tests that PTA of questions is correctly generated by verifying that generated 
	 * questions cover all new transitions.
	 * @param fsm machine to experiment with (states <em>A</em> and <em>B</em> are merged)
	 * @param name how the machine is to be called.
	 */
	private void checkQuestionAugmentation(String fsm, String name)
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
		LearnerGraph graph = buildLearnerGraph(fsm,name,config);
		StatePair pair = new StatePair(graph.findVertex("A"),graph.findVertex("B"));
		LearnerGraph merged = MergeStates.mergeAndDeterminize_general(graph, pair);
		PTASequenceEngine questions = ComputeQuestions.computeQS_general(pair, graph, merged, new ComputeQuestions.QSMQuestionGenerator());
		LearnerGraph updatedGraphExpected = new LearnerGraph(graph,config),updatedGraphActual = new LearnerGraph(graph,config);
		updatedGraphActual.transitionMatrix.putAll(((NonExistingPaths)questions.getFSM()).getNonExistingTransitionMatrix());
		updatedGraphActual.learnerCache.invalidate();
		
		for(List<Label> path:questions.getData())
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
		LearnerGraph graph = buildLearnerGraph("A-c->B", "testPerformAugment1",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthenC, "ifthenC", config)};
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 5);
		compareGraphs(buildLearnerGraph("A-c->B", "testPerformAugment1",config), graph);
	}
	
	/** One state is augmented. */
	@Test
	public final void testPerformAugment2a() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B", "testPerformAugment2a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthenC, "ifthenC", config)};
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 5);
		compareGraphs(buildLearnerGraph("A-a->B-d->C", "testPerformAugment2b",config), graph);
	}
	
	/** Nothing is augmented because the collection of properties is empty. */
	@Test
	public final void testPerformAugment2b() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B", "testPerformAugment2a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{};
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 5);
		compareGraphs(buildLearnerGraph("A-a->B", "testPerformAugment1",config), graph);
	}
	
	/** Cannot augment: depth is zero. */
	@Test
	public final void testPerformAugment3() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B", "testPerformAugment2a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthenC, "ifthenC", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 0);
		compareGraphs(buildLearnerGraph("A-a->BC", "testPerformAugment3",config), graph);
	}
	
	/** Two states are augmented. */
	@Test
	public final void testPerformAugment4() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B-b->C", "testPerformAugment4a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthenC, "ifthenC", config)};
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 1);
		//Visualiser.updateFrame(graph, null);
		compareGraphs(buildLearnerGraph("A-a->B-d->U / B-b->C-a->Q / C-c->R", "testPerformAugment4b",config), graph);
	}
	
	/** Two states are augmented a bit further - two steps. */
	@Test
	public final void testPerformAugment5a() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B-b->C", "testPerformAugment4a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthenC, "ifthenC", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		compareGraphs(buildLearnerGraph("A-a->B-d->N1 / B-b->C-a->B1-b->C1/ B1-d->N2 / C-c->S1-c->S2", "testPerformAugment5a",config), graph);
	}
	
	/** Two states are augmented a bit further - three steps. */
	@Test
	public final void testPerformAugment5b() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B-b->C", "testPerformAugment4a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthenC, "ifthenC", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 3);
		compareGraphs(buildLearnerGraph("A-a->B-d->N1 / B-b->C-a->B1-b->C1-a->B2/ B1-d->N2 / C-c->S1-c->S2-c->S3 / C1-c->S11", "testPerformAugment5b",config), graph);
	}
	
	/** Two states are augmented a bit further - four steps. */
	@Test
	public final void testPerformAugment5c() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B-b->C", "testPerformAugment4a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthenC, "ifthenC", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 4);
		compareGraphs(buildLearnerGraph("A-a->B-d->N1 / B-b->C-a->B1-b->C1-a->B2-b->C2/ B1-d->N2 / B2-d->N3 / C-c->S1-c->S2-c->S3-c->S4 / C1-c->S11-c->S12", "testPerformAugment5c",config), graph);
	}
	
	/** Two states are augmented a bit further - five steps. */
	@Test
	public final void testPerformAugment5d() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B-b->C", "testPerformAugment4a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthenC, "ifthenC", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 5);
		compareGraphs(buildLearnerGraph("A-a->B-d->N1 / B-b->C-a->B1-b->C1-a->B2-b->C2-a->B3/ B1-d->N2 / B2-d->N3 / C-c->S1-c->S2-c->S3-c->S4-c->S5"+
				"/ C1-c->S11-c->S12-c->S13 / C2-c->S21", "testPerformAugment5d",config), graph);
	}
	
	
	/** Non-existing vertices in a tentative automaton. */
	@Test
	public final void testPerformAugment_fail0()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B-b->C", "testPerformAugment_fail0",config);
		graph.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(new VertexID(VertKind.NONEXISTING,90),config),graph.createNewRow());
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-a->B /  T-b-#N / B=THEN=T", "testPerformAugment_fail1", config)}; 
			Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		}},IllegalArgumentException.class,"non-existing vertices");
	}
	
	/** Contradiction between a new state and a graph, first when everything is ok. */
	@Test
	public final void testPerformAugment6a() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B", "testPerformAugment6a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-a->B /  T-b-#N / B=THEN=T", "testPerformAugment_fail1", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		compareGraphs(buildLearnerGraph("A-a->B-b-#N1", "testPerformAugment6a",config), graph);
	}
	
	/** Contradiction between a new state and a graph, first when everything is ok. */
	@Test
	public final void testPerformAugment6b() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B-b-#C", "testPerformAugment6b",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-a->B /  T-b-#N / B=THEN=T", "testPerformAugment_fail1", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		compareGraphs(buildLearnerGraph("A-a->B-b-#N1", "testPerformAugment6a",config), graph);
	}

	/** Contradiction between a new state and a graph, first when everything is ok. */
	@Test
	public final void testPerformAugment6c() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B-b-#C / B-a->D", "testPerformAugment6c",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-a->B /  T-b-#N / B=THEN=T", "testPerformAugment_fail1", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		compareGraphs(buildLearnerGraph("A-a->B-b-#N1 / B-a->D-b-#N2", "testPerformAugment6c",config), graph);
	}
	
	/** Contradiction between a new state and a graph, first when everything is ok. */
	@Test
	public final void testPerformAugment6d() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B-b-#C / B-a->D-b-#N2", "testPerformAugment6d",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-a->B /  T-b-#N / B=THEN=T", "testPerformAugment_fail1", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		compareGraphs(buildLearnerGraph("A-a->B-b-#N1 / B-a->D-b-#N2", "testPerformAugment6c",config), graph);
	}
	
	
	/** Contradiction between a new state and a graph. */
	@Test
	public final void testPerformAugment_fail1()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B-b->C", "testPerformAugment_fail1a",config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-a->B /  T-b-#N / B=THEN=T", "testPerformAugment_fail1", config)}; 
			Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		}},AugmentFromIfThenAutomatonException.class,"cannot merge a tentative state");
	}
	
	/** Contradiction between a new state and a graph after unrolling the property a few times. */
	@Test
	public final void testPerformAugment_fail2()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B-a->C-a->D-b->E", "testPerformAugment_fail1a",config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-a->B /  T-b-#N / B=THEN=T", "testPerformAugment_fail1", config)}; 
			Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		}},AugmentFromIfThenAutomatonException.class,"cannot merge a tentative state");
	}
	
	/** Contradiction between states added by THEN graphs, first when everything is ok. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testPerformAugment7() throws IncompatibleStatesException
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B", "testPerformAugment_fail1a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-a->B /  T-c->T1-c->T2-c-#T3 / R-c->R1-c->R2-b->R3 / R=THEN=B=THEN=T", "testPerformAugment_fail1", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		compareGraphs(buildLearnerGraph("A-a->B-c->T1-c->T2", "testPerformAugment6b",config), graph);
	}
	
	/** Contradiction between states added by THEN graphs, first when everything is ok. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testPerformAugment8() throws IncompatibleStatesException
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B", "testPerformAugment_fail1a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-a->B /  T-c->T1-c->T2-c-#T3 / R-c->R1-c->R2-b->R3 / R=THEN=B=THEN=T", "testPerformAugment_fail1", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 3);
		compareGraphs(buildLearnerGraph("A-a->B-c->T1-c->T2-c-#T3 / T2-b->T4", "testPerformAugment6b",config), graph);
	}
	
	/** Contradiction between states added by THEN graphs, first when everything is ok. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testPerformAugment9() throws IncompatibleStatesException
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B", "testPerformAugment_fail1a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-a->B /  T-c->T1-c->T2-c-#T3 / R-c->R1-c->R2-b->R3 / R=THEN=B=THEN=T", "testPerformAugment_fail1", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 7);
		compareGraphs(buildLearnerGraph("A-a->B-c->T1-c->T2-c-#T3 / T2-b->T4", "testPerformAugment6b",config), graph);
	}
	
	/** Not yet a contradiction between states added by THEN graphs - the depth of exploration is 
	 * too low to hit it. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testPerformAugment10() throws IncompatibleStatesException
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B", "testPerformAugment_fail1a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-a->B /  T-c->T1-c->T2-b-#T3 / R-c->R1-c->R2-b->R3 / R=THEN=B=THEN=T", "testPerformAugment_fail1", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		compareGraphs(buildLearnerGraph("A-a->B-c->T1-c->T2", "testPerformAugment6b",config), graph);
	}
	
	/** Contradiction between states added by THEN graphs. */
	@Test
	public final void testPerformAugment_fail3()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B", "testPerformAugment_fail1a",config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-a->B /  T-c->T1-c->T2-b-#T3 / R-c->R1-c->R2-b->R3 / R=THEN=B=THEN=T", "testPerformAugment_fail1", config)};
			Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 3);
		}},AugmentFromIfThenAutomatonException.class,"cannot merge a tentative state");
	}
	
	/** Incompatibility between an existing state and a new one. */
	@Test
	public final void testPerformAugment_fail4()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B-b-#C", "testPerformAugment_fail4a",config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-a->B /  T-b->N / B=THEN=T", "testPerformAugment_fail4", config)}; 
			Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		}},AugmentFromIfThenAutomatonException.class,"cannot merge a tentative state");
	}
	
	/** Reject-states cannot be extended even if they match a property. 
	 * @throws AugmentFromIfThenAutomatonException */
	@Test
	public final void testPerformAugment_reject1() throws AugmentFromIfThenAutomatonException
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a-#B", "testPerformAugment_reject1a",config);
		LearnerGraph ifthen = buildLearnerGraph("A-a->B-a->B /  T-b->N / B=THEN=T", "testPerformAugment_fail4", config);
		ifthen.findVertex("T").setAccept(false);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{ifthen}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		compareGraphs(buildLearnerGraph("A-a-#B", "testPerformAugment_reject1b",config), graph);
	}
	
	/** Another example of a contradiction between a tentative graph and the property. */
	@Test
	public final void testPerformAugment_fail5()
	{
		final Configuration config = Configuration.getDefaultConfiguration();
		final LearnerGraph graph = buildLearnerGraph("A-a->B-b-#C", "testPerformAugment_fail5a",config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			LearnerGraph ifthen = buildLearnerGraph("A-a->B-a->B /  T-b->N-s->R / B=THEN=T", "testPerformAugment_fail5", config);
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{ifthen}; 
			Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 2);
		}},AugmentFromIfThenAutomatonException.class,"cannot merge a tentative");
	}
	
	/** Dummy property - dummy means satisfied as long as the initial state of a tentative automaton is accept. */
	@Test
	public final void testPerformAugment11() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B", "testPerformAugment11a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B / P-b->P / A==THEN==P", "testPerformAugment11", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 4);
		compareGraphs(buildLearnerGraph("A-a->B / A-b->C-b->D-b->E-b->F", "testPerformAugment11b",config), graph);
	}
	
	/** Dummy limited property. */
	@Test
	public final void testPerformAugment12() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B", "testPerformAugment11a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B / P-b->Q-c->R / A==THEN==P", "testPerformAugment11", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 4);
		compareGraphs(buildLearnerGraph("A-a->B / A-b->C-c->D", "testPerformAugment11b",config), graph);
	}
	
	/** Dummy limited property and the first automaton does not match at all - which does not matter since property is dummy. */
	@Test
	public final void testPerformAugment13() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-s->B", "testPerformAugment11a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B / P-b->Q-c->R / A==THEN==P", "testPerformAugment11", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 4);
		compareGraphs(buildLearnerGraph("A-s->B / A-b->C-c->D", "testPerformAugment11b",config), graph);
	}
	
	/** Infinitely matching property - this test is similar to a testPerformAugment5 series. */
	@Test
	public final void testPerformAugment14() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B-b->C-a->B", "testPerformAugment14a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-a->B-b->C / P-a->Q-b->P-c->S / C==THEN==P", "testPerformAugment14", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 400);
		compareGraphs(buildLearnerGraph("A-a->B-b->C-a->B / C-c->D", "testPerformAugment14b",config), graph);
	}

	/** Two properties where there is one which matches after a while and another short one which depends on the result of the match of the first one. */ 
	@Test
	public final void testPerformAugment15() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B-b->A", "testPerformAugment15a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{
				buildLearnerGraph("A-a->B-b->C-a->D-b->E / P-a->Q-c->S / E==THEN==P", "testPerformAugment15c", config),
				buildLearnerGraph("A-a->B-c->C / P-d->Q / C==THEN==P", "testPerformAugment15d", config)				
		}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 400);
		compareGraphs(buildLearnerGraph("bA-a->bB-b->bA / bB-c->bC-d->bD", "testPerformAugment15b",config), graph);
	}
	
	/** Two properties where there is one which matches after a while and another short one which depends on the result of the match of the first one. */ 
	@Test
	public final void testPerformAugment16a() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B-b->A", "testPerformAugment15a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{
				buildLearnerGraph("A-a->B-b->C-a->D-b->E / B-c->F-d->G / P-a->Q-c->S / T-e->U / E==THEN==P / G==THEN==T", "testPerformAugment16c", config),
				buildLearnerGraph("1A-a->1B-c->1C / 1P-d->1Q / 1C==THEN==1P", "testPerformAugment16a", config)				
		}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 400);
		compareGraphs(buildLearnerGraph("bA-a->bB-b->bA / bB-c->bC-d->bD-e->bE", "testPerformAugment16a",config), graph);
	}
	
	/** Two properties where there is one which matches after a while and another short one which depends on the result of the match of the first one. */ 
	@Test
	public final void testPerformAugment16b() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B-b->A", "testPerformAugment15a",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{
				buildLearnerGraph("A-a->B-b->C-a->D-b->E / B-c->F-d->G / P-a->Q-c->S / T-e->U / E==THEN==P / G==THEN==T /"+
						"D-c->D2-d->R / RB1-d->RB2 / R == THEN == RB1 ", "testPerformAugment16c", config),
				buildLearnerGraph("1A-a->1B-c->1C / 1P-d->1Q / 1C==THEN==1P", "testPerformAugment16a", config)				
		}; 
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 400);
		compareGraphs(buildLearnerGraph("bA-a->bB-b->bA / bB-c->bC-d->bD-e->bE / bD-d->bF", "testPerformAugment16b",config), graph);
	}
	
	
	static String ifthen_ab_to_c = "A0-a->B0-b->C0-a->B0 / C0-b->A0 / D0-c->E0 / C0 == THEN == D0",
		ifthen_a_to_c = "A1-a->B1-b->A1 / C1-c->D1 / B1 == THEN == C1",
		ifthen_c_to_cc = "A2-a->A2-b->A2-c->B2-c->B2 / B2-a->A2 / B2-b->A2 / C2-c->D2 / B2 == THEN == C2",
		ifthen_ccc_to_ab = "A3-a->A3-b->A3-c->B3-c->C3-c->D3-c->C3 / B3-a->A3 / B3-b->A3 / C3-a->A3 / C3-b->A3 / D3-a->A3 / D3-b->A3 / E3-a->F3-b->G3 / D3 == THEN == E3";
	
	/** Three if-then automata, two of which recursively expand each other. */
	@Test
	public final void testPerformAugment17() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-b->B-c->A-c->B", "testPerformAugment17",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{
				buildLearnerGraph(ifthen_a_to_c, "ifthen_a_to_c", config),
				buildLearnerGraph(ifthen_c_to_cc, "ifthen_c_to_cc", config),			
				buildLearnerGraph(ifthen_ccc_to_ab, "ifthen_ccc_to_ab", config)
		};
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 9);
		//Visualiser.updateFrame(graph, null);Visualiser.waitForKey();
		compareGraphs(buildLearnerGraph("A-b->B-c->A-c->B / "+
				"A-a->A1-b->B1 / "+
				"B-a->A2-b->B2 / "+
				"A1-c->C1-c->C2-c->C3-c->C4-c->C5-c->C6-c->C7-c->C8 / "+
				"C3-a->A3-b->B3 / "+"C5-a->A3-b->B3 / "+"C7-a->A4 / "
				, "testPerformAugment17b",config), graph);
	}
	
	/** Three if-then automata, two of which recursively expand each other. */
	@Test
	public final void testPerformAugment18() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-b->B-c->A-c->B", "testPerformAugment17",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{
				buildLearnerGraph(ifthen_ab_to_c, "ifthen_ab_to_c", config),
				buildLearnerGraph(ifthen_c_to_cc, "ifthen_c_to_cc", config),			
				buildLearnerGraph(ifthen_ccc_to_ab, "ifthen_ccc_to_ab", config)
		};
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 9);
		compareGraphs(buildLearnerGraph("A-b->B-c->A-c->B / "+
				"A-a->A1-b->B1 / "+
				"B-a->A2-b->B2 / "+
				"B1-c->C1-c->C2-c->C3-c->C4-c->C5-c->C6-c->C7 / "+
				"C3-a->A3-b->B3 / "+"C5-a->A3-b->B3 "
				, "testPerformAugment17b",config), graph);
	}
	
	/** Three if-then automata, two of which recursively expand each other - very similar to the above but operates on a tree. */
	@Test
	public final void testPerformAugment19() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = buildLearnerGraph("A-a->B-b->C-a->D-b->E-a->F-b->G", "testPerformAugment19",config);
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{
				buildLearnerGraph(ifthen_ab_to_c, "ifthen_ab_to_c", config),
				buildLearnerGraph(ifthen_c_to_cc, "ifthen_c_to_cc", config),			
				buildLearnerGraph(ifthen_ccc_to_ab, "ifthen_ccc_to_ab", config)
		};
		Transform.augmentFromIfThenAutomaton(graph, null, ifthenCollection, 7);
		//Visualiser.updateFrame(graph, null);Visualiser.waitForKey();
		compareGraphs(buildLearnerGraph("A-a->B-b->C-a->D-b->E-a->F-b->G / "+
				"C-c->C11-c->C12-c->C13-c->C14-c->C15-c->C16-c->C17 / C13-a->C13A-b->C13B / C15-a->C15A-b->C15B /"+
				"E-c->C21-c->C22-c->C23-c->C24-c->C25-c->C26-c->C27 / C23-a->C23A-b->C23B / C25-a->C25A-b->C25B /"+
				"G-c->C31-c->C32-c->C33-c->C34-c->C35-c->C36-c->C37 / C33-a->C33A-b->C33B / C35-a->C35A-b->C35B"
				, "testPerformAugment17b",config), graph);
	}
	
	
	/** Tests how properties can answer questions. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testQuestionAnswering1() throws IncompatibleStatesException
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
		LearnerGraph graph = buildLearnerGraph(
				"A1-a->B1-b->A2-a->B2-b->A3-a->B3-b->A4-a->B4-b->A5 /"+
				"A2-c->A2_U1-c->A2_U2-e->A2_U3 /"+
				"A3-c->A3_U1-c->A3_U2 / A3_U1-d-#A3_U4 /"+
				"A4-c->A4_U1-c->A4_U2-f-#A4_U3 /"+
				"A5-c->A5_U1-c->A5_U2"
				, "testBuildPTAofQuestions1",config);
		StatePair pair = new StatePair(graph.findVertex("A1"),graph.findVertex("A2"));
		LearnerGraph merged = MergeStates.mergeAndDeterminize_general(graph, pair);
		compareGraphs(buildLearnerGraph("A1-a->B1-b->A1-c->C-d-#R4/C-c->CC/CC-f-#R3/CC-e->D", "testQuestionAnswering2b",config),merged);
		PTASequenceEngine questions = ComputeQuestions.computeQS_general(pair, graph, merged, new ComputeQuestions.QSMQuestionGenerator());
		// the IF part we're augmenting with is a dummy one
		LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph("A-s->B / P-c->Q-d-#R / S-c->S1-c->S2-e->S3 / S==THEN==A==THEN==P", "testQuestionAnswering1", config)}; 
		Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
		List<List<Label>> questionList = questions.getData();
		Assert.assertEquals(1,questionList.size());
		Assert.assertEquals(labelList(new String[]{"c","c","f"}), questionList.iterator().next());
	}

	@Test
	public final void testConversionOfAssociationsToTransitions1()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
		LearnerGraph graph = buildLearnerGraph("A-a->B / P-b->Q-c->R / ", "testConversionOfAssociationsToTransitions1a", config);
                DirectedSparseGraph graphAfterConversion = graph.pathroutines.getGraph();
                PathRoutines.convertPairAssociationsToTransitions(graphAfterConversion,graph, config);
		graph.pairCompatibility.compatibility.clear();
		LearnerGraph obtainedGraph = new LearnerGraph(graphAfterConversion,config);
		WMethod.checkM_and_colours(buildLearnerGraph("A-a->B / P-b->Q-c->R / ",
				"testConversionOfAssociationsToTransitions1b",config),obtainedGraph ,VERTEX_COMPARISON_KIND.DEEP);
		Assert.assertNull(graphAfterConversion.getUserDatum(JUConstants.VERTEX));
		Assert.assertTrue(
				((EdgeAnnotation)graphAfterConversion.getUserDatum(JUConstants.EDGE)).isEmpty());
	}
	
	@Test
	public final void testConversionOfAssociationsToTransitions2()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
		LearnerGraph graph = buildLearnerGraph("A-a->B / P-b->Q-c->R / A==THEN==P / B=INCOMPATIBLE=Q=MERGED=R", "testConversionOfAssociationsToTransitions2a", config);
                DirectedSparseGraph graphAfterConversion = graph.pathroutines.getGraph();
		PathRoutines.convertPairAssociationsToTransitions(graphAfterConversion,graph, config);
		graph.pairCompatibility.compatibility.clear();
		LearnerGraph obtainedGraph = new LearnerGraph(graphAfterConversion,config);
		WMethod.checkM_and_colours(buildLearnerGraph("A-a->B / P-b->Q-c->R / "+
				"A-"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.THEN.name()+"->P / "+
				"P-"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.THEN.name()+"->A / "+
				
				"B-"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.INCOMPATIBLE.name()+"->Q / "+
				"Q-"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.INCOMPATIBLE.name()+"->B / "+
				
				"Q-"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.MERGED.name()+"->R / "+
				"R-"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.MERGED.name()+"->Q / ", 
				"testConversionOfAssociationsToTransitions2b",config), obtainedGraph,VERTEX_COMPARISON_KIND.DEEP);

		Assert.assertNull(graphAfterConversion.getUserDatum(JUConstants.VERTEX));
		Assert.assertEquals("{A={"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.THEN.name()+"={P=java.awt.Color[r=255,g=255,b=0]}}, " +
				"B={"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.INCOMPATIBLE.name()+"={Q=java.awt.Color[r=255,g=255,b=0]}}, " +
				"P={"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.THEN.name()+"={A=java.awt.Color[r=255,g=255,b=0]}}, " +
				"Q={"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.INCOMPATIBLE.name()+"={B=java.awt.Color[r=255,g=255,b=0]}, "+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.MERGED.name()+"={R=java.awt.Color[r=255,g=255,b=0]}}, " +
				"R={"+PathRoutines.associationPrefix+PAIRCOMPATIBILITY.MERGED.name()+"={Q=java.awt.Color[r=255,g=255,b=0]}}}",
				((EdgeAnnotation)graphAfterConversion.getUserDatum(JUConstants.EDGE)).toString());

	}

	public static final class TestQuestionPTA
	{
		private final static String ifthen_sc = "I-s->A-c->B / P-d-#R / P-c->T1-e->T2 / P-a->T / P==THEN==B",// "if" part requires "s c" to be confirmed and rules out "s c d" and "s c c e"
			ifthen_sc_unsat = "I-s->A-c-#B / P-d-#R / P-c->T1-e->T2 / P-a->T / P==THEN==B",// "if" part attempts to match a reject-state - this will not be the case
		ifthen_s = "I-s->B / P-c->Q-d-#R / P-a->T / P-b->T1 / P==THEN==B",// the B state in these "if" graphs corresponds to A.._U1 series in "graphWithAppendixAfterMerging"
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
			origGraph = FsmParser.buildGraph(graphWithAppendixAfterMerging, "graphWithAppendixAfterMerging",config);
			graph = new LearnerGraph(origGraph,config);
			
			pair = new StatePair(graph.findVertex("A1"),graph.findVertex("A2"));
			merged = MergeStates.mergeAndDeterminize_general(graph, pair);
			
			//Visualiser.updateFrame(graph, merged);Visualiser.waitForKey();
			compareGraphs(buildLearnerGraph("A-s->A1-a->B1-b->A1-c->C-d-#R4/C-c->CC/CC-f-#R3/CC-e->D", "testQuestionAnswering2b",config),merged);
			questions = ComputeQuestions.computeQS_general(pair, graph, merged, new ComputeQuestions.QSMQuestionGenerator());
			Assert.assertEquals(3,questions.getData().size());// whether questions are correctly generated is tested in "testQuestionAnswering2"
		}
		
		/** Tests that the question part is matched to the IF part only if it matches the THEN part.
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering2() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthen_sc, "ifthen_sc", config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			List<List<Label>> questionList = questions.getData();
			Assert.assertEquals(3,questionList.size());
			Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","d"},new String[]{"s","c","c","f"},new String[]{"s","c","c","e"}},config);
			Set<List<Label>> actual = new LinkedHashSet<List<Label>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Tests that the question part is matched to the IF part only if it matches the THEN part.
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering3a() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthen_s, "ifthen_s", config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			List<List<Label>> questionList = questions.getData();
			Assert.assertEquals(2,questionList.size());
			Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"},new String[]{"s","c","c","e"}},config);
			Set<List<Label>> actual = new LinkedHashSet<List<Label>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		
			// now do the same again - should not change anything. 
			ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthen_s, "ifthen_s", config)};
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			questionList = questions.getData();
			Assert.assertEquals(2,questionList.size());
			actual = new LinkedHashSet<List<Label>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Same as above but do the two (identical) things at the same time. */
		@Test
		public final void testQuestionAnswering3b() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{
					buildLearnerGraph(ifthen_s, "ifthen_s", config), 
					buildLearnerGraph(ifthen_s, "ifthen_s", config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			List<List<Label>> questionList = questions.getData();
			Assert.assertEquals(2,questionList.size());
			Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"},new String[]{"s","c","c","e"}},config);
			Set<List<Label>> actual = new LinkedHashSet<List<Label>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		
			// now do the same again - should not change anything. 
			ifthenCollection = new LearnerGraph[]{
					buildLearnerGraph(ifthen_s, "ifthen_s", config),
					buildLearnerGraph(ifthen_s, "ifthen_s", config)};
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			questionList = questions.getData();
			Assert.assertEquals(2,questionList.size());
			actual = new LinkedHashSet<List<Label>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Tests that the question part is matched to the IF part only if it matches the THEN part. 
		 * Note that "if_then_sc" depends on the outcome of "if_then_s".
		 * 
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering4a() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthen_s, "ifthen_s", config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthen_sc, "ifthen_sc", config)};
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			List<List<Label>> questionList = questions.getData();
			Assert.assertEquals(1,questionList.size());
			Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}},config);
			Set<List<Label>> actual = new LinkedHashSet<List<Label>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Same as above but do the two (identical) things at the same time.
		 * Note that "if_then_sc" depends on the outcome of "if_then_s".
		 * 
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering4b() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{
					buildLearnerGraph(ifthen_s, "ifthen_s", config), 
					buildLearnerGraph(ifthen_sc, "ifthen_sc", config)
			};
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			List<List<Label>> questionList = questions.getData();
			Assert.assertEquals(1,questionList.size());
			Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}},config);
			Set<List<Label>> actual = new LinkedHashSet<List<Label>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Tests that the question part is matched to the IF part only if it has been answered by a user.
		 * First, with IF part which cannot be satisfied.
		 * 
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering5a() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthen_sc, "ifthen_sc", config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			graph.learnerCache.questionsPTA=questions;
			Assert.assertEquals(3,questions.getData().size());
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
	
			List<List<Label>> questionList = questions.getData();
			Assert.assertEquals(3,questionList.size());
			Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","d"},new String[]{"s","c","c","f"},new String[]{"s","c","c","e"}},config);
			Set<List<Label>> actual = new LinkedHashSet<List<Label>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Tests that the question part is matched to the IF part only if it has been answered by a user.
		 * First, with IF part which cannot be satisfied.
		 * 
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering5b() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthen_sc_unsat, "ifthen_sc", config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			graph.learnerCache.questionsPTA=questions;
			Assert.assertEquals(3,questions.getData().size());
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
	
			List<List<Label>> questionList = questions.getData();
			Assert.assertEquals(3,questionList.size());
			Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","d"},new String[]{"s","c","c","f"},new String[]{"s","c","c","e"}}, config);
			Set<List<Label>> actual = new LinkedHashSet<List<Label>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Tests that the question part is matched to the IF part only if it has been answered by a user.
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering6() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthen_sc, "ifthen_sc", config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			graph.learnerCache.questionsPTA=questions;
			graph.transform.AugmentNonExistingMatrixWith(labelList(new String[]{"s","c","d"}), false);
			Assert.assertEquals(2,questions.getData().size());
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
	
			List<List<Label>> questionList = questions.getData();
			Assert.assertEquals(1,questionList.size());
			Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}},config);
			Set<List<Label>> actual = new LinkedHashSet<List<Label>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Tests that the question part is matched to the IF part only if it has been answered by a user.
		 * @throws IncompatibleStatesException */
		@Test
		public final void testQuestionAnswering7() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthen_sc, "ifthen_sc", config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			graph.learnerCache.questionsPTA=questions;
			graph.transform.AugmentNonExistingMatrixWith(labelList(new String[]{"s","c"}), true);
			Assert.assertEquals(3,questions.getData().size());
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
	
			List<List<Label>> questionList = questions.getData();
			Assert.assertEquals(1,questionList.size());
			Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}},config);
			Set<List<Label>> actual = new LinkedHashSet<List<Label>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Tests that recursive properties are correctly handled. */
		@Test
		public final void testQuestionAnswering8() throws IncompatibleStatesException
		{
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{buildLearnerGraph(ifthen_sc, "ifthen_sc", config)}; 
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			graph.learnerCache.questionsPTA=questions;
			graph.transform.AugmentNonExistingMatrixWith(labelList(new String[]{"s","c"}), true);
			Assert.assertEquals(3,questions.getData().size());
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
	
			List<List<Label>> questionList = questions.getData();
			Assert.assertEquals(1,questionList.size());
			Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}},config);
			Set<List<Label>> actual = new LinkedHashSet<List<Label>>();actual.addAll(questionList);
			Assert.assertEquals(expected,actual);
		}
		
		/** Three if-then automata, two of which recursively expand each other - very similar to the above but operates on a tree. */
		@Test
		public final void testQuestionAnswering9() throws IncompatibleStatesException
		{
			origGraph = FsmParser.buildGraph("I-c->I-s->A-c->A-a->B-b->C-a->D-b->E-a->F-b->G / "+
					"C-c->C11-c->C12-c->C13-c->C14-c->C15-c->C16-c->C17 / C13-a->C13A-b->C13B / C15-a->C15A-b->C15B /"+
					"E-c->C21-c->C22-c->C23-c->C24-c->C25-c->C26-c->C27 / C23-a->C23A-b->C23B / C25-a->C25A-b->C25B /"+
					"G-c->C31-c->C32-c->C33-c->C34-c->C35-c->C36-c->C37 / C33-a->C33A-b->C33B / C35-a->C35A-b->C35B", "testQuestionAnswering9",config);
			Configuration defaultConfig = Configuration.getDefaultConfiguration();
			graph = new LearnerGraph(origGraph,defaultConfig);
			LearnerGraph[] ifthenCollection = new LearnerGraph[]{
					buildLearnerGraph(ifthen_ab_to_c, "ifthen_ab_to_c", defaultConfig),
					buildLearnerGraph(ifthen_c_to_cc, "ifthen_c_to_cc", defaultConfig),			
					buildLearnerGraph(ifthen_ccc_to_ab, "ifthen_ccc_to_ab", defaultConfig)
			};
			pair = new StatePair(graph.findVertex("I"),graph.findVertex("A"));
			merged = MergeStates.mergeAndDeterminize_general(graph, pair);
			LearnerGraph expectedMergedGraph = buildLearnerGraph("A-s->A-c->A-a->B-b->C-a->D-b->E-a->F-b->G / "+
					"C-c->C11-c->C12-c->C13-c->C14-c->C15-c->C16-c->C17 / C13-a->C13A-b->C13B / C15-a->C15A-b->C15B /"+
					"E-c->C21-c->C22-c->C23-c->C24-c->C25-c->C26-c->C27 / C23-a->C23A-b->C23B / C25-a->C25A-b->C25B /"+
					"G-c->C31-c->C32-c->C33-c->C34-c->C35-c->C36-c->C37 / C33-a->C33A-b->C33B / C35-a->C35A-b->C35B", "testQuestionAnswering9b",defaultConfig);
			//Visualiser.updateFrame(graph, merged);Visualiser.waitForKey();
			compareGraphs(expectedMergedGraph,merged);
			questions = ComputeQuestions.computeQS_general(pair, graph, merged, new ComputeQuestions.QSMQuestionGenerator());
			graph.learnerCache.questionsPTA=questions;
			Assert.assertEquals(19,questions.getData().size());
			Transform.augmentFromIfThenAutomaton(graph, (NonExistingPaths)questions.getFSM(), ifthenCollection, 0);
			compareGraphs(new LearnerGraph(origGraph,defaultConfig),graph);// check that augment did not modify the automaton
			List<List<Label>> questionList = questions.getData();
			Assert.assertEquals(13,questionList.size());
		}

		final static LearnerGraph 
			ifthen1=buildLearnerGraph("I-a->A-b->B / P-c->C / P==THEN==B","ifthenA",Configuration.getDefaultConfiguration()),
			ifthen2=buildLearnerGraph("I-a->I-b->I-c->A / A-a->I / A-c->A / A-b->B / B-a->I / B-b->I / B-c->A / P-a->C / P==THEN==B","ifthenA",Configuration.getDefaultConfiguration()),
			ifthen3=buildLearnerGraph("I-a->I-b->I-c->A / A-a->I / A-c->A / A-b->B / B-a->I / B-b->I / B-c->A / P-a-#C / P==THEN==B","ifthenA",Configuration.getDefaultConfiguration());
		
		/** Tests <em>mapPathToConfirmedElements</em> with an empty sequence. */
		@Test
		public final void testMapPathToConfirmedElements1Ea()
		{
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			Assert.assertTrue(PathRoutines.mapPathToConfirmedElements(hardFacts,new LinkedList<Label>(),new LearnerGraph[]{})
					.isEmpty());
		}
		
		/** Tests <em>mapPathToConfirmedElements</em> with a sequence containing a non-existing element. */
		@Test
		public final void testMapPathToConfirmedElements1Eb()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,
					AbstractLearnerGraph.buildList(Arrays.asList(new String[]{
					"u"}),config),new LearnerGraph[]{});
			Assert.assertEquals(Arrays.asList(new Boolean[]{null}),result);
		}
		

		/** Tests <em>mapPathToConfirmedElements</em> when there are no ifthen automata (empty set of automata). */
		@Test
		public final void testMapPathToConfirmedElements1Ec()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"s","t"}), true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,
					AbstractLearnerGraph.buildList(Arrays.asList(new String[]{
					"s","v","j"}),config),new LearnerGraph[]{});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,null,null}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em> with an empty sequence. */
		@Test
		public final void testMapPathToConfirmedElements1Na()
		{
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			Assert.assertTrue(PathRoutines.mapPathToConfirmedElements(hardFacts,new LinkedList<Label>(),null)
					.isEmpty());
		}
		
		/** Tests <em>mapPathToConfirmedElements</em> with a sequence containing a non-existing element. */
		@Test
		public final void testMapPathToConfirmedElements1Nb()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,
					labelList(new String[]{"u"}),null);
			Assert.assertEquals(Arrays.asList(new Boolean[]{null}),result);
		}
		

		/** Tests <em>mapPathToConfirmedElements</em> when there are no ifthen automata (null argument). */
		@Test
		public final void testMapPathToConfirmedElements1Nc()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"s","t"}), 
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,
					labelList(new String[]{"s","v","j"}),null);
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,null,null}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements2a()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"s","t"}), true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"s"}), new LearnerGraph[]{});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true}),result);
		}
		
		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements2b()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"s","t"}),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"s"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements3a()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"s","t"}), 
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"s","t"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,true}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements3b()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"s","t"}),
					false, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"s","t"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,false}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements4a()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"s","t"}),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"s","t","q"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,true,null}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements4b()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"s","t"}),
					false, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"s","t","q"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,false,null}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. 
		 * Path matches if-then but "then" element does not confirm anything. 
		 */
		@Test
		public final void testMapPathToConfirmedElements5a()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"s","t"}),config),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","q"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{null,null,null}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. 
		 * Path matches if-then and "then" element confirms a path
		 */
		@Test
		public final void testMapPathToConfirmedElements5b()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"s","t"}),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","c"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{null,null,true}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. 
		 * Path matches if-then and "then" element confirms a path
		 */
		@Test
		public final void testMapPathToConfirmedElements5c()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"a","b"}),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","s"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,true,null}),result);
		}
		
		/** Tests <em>mapPathToConfirmedElements</em>. 
		 * Path matches if-then and "then" element confirms a path
		 */
		@Test
		public final void testMapPathToConfirmedElements5d()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"a","t"}),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","c"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,null,true}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. 
		 * Path matches if-then and "then" element confirms a path
		 */
		@Test
		public final void testMapPathToConfirmedElements5e()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"a","b"}),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","c"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,true,true}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. 
		 * Path matches if-then and "then" element confirms a path
		 */
		@Test
		public final void testMapPathToConfirmedElements5f()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"a","b"}),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","f"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,true,null}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. 
		 */
		@Test
		public final void testMapPathToConfirmedElements6a()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"a","s"}),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","c","c"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,null,true,null}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements6b()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"a","s"}),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","c","b"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,null,true,null}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements6c()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"a","s"}), 
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","c","b","a"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,null,true,null,true}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements6d()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"a","s"}),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","c","b","s"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,null,true,null,null}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements6e()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"a","s"}),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","c","c","b","s"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,null,true,null,null,null}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements6f()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"a","s"}), 
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","c","c","b","a"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,null,true,null,null,true}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements7a()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"a","b"}),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","c","c","b","a"}), new LearnerGraph[]{ifthen1,ifthen3});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,true,true,null,null,false}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements7b()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"a","b"}),
					true, false, JUConstants.BLUE);
			List<Boolean> result = PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","c","c","b","a","u"}), new LearnerGraph[]{ifthen1,ifthen2});
			Assert.assertEquals(Arrays.asList(new Boolean[]{true,true,true,null,null,true,null}),result);
		}

		/** Tests <em>mapPathToConfirmedElements</em>. */
		@Test
		public final void testMapPathToConfirmedElements7c()
		{// questions are [[s, c, d], [s, c, c, f], [s, c, c, e]] where only s exists in the original graph
			final LearnerGraph hardFacts = new LearnerGraph(Configuration.getDefaultConfiguration());hardFacts.initPTA();
			hardFacts.paths.augmentPTA(labelList(new String[]{"a","b"}),
					true, false, JUConstants.BLUE);
			Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
				PathRoutines.mapPathToConfirmedElements(hardFacts,labelList(new String[]{
					"a","b","c","c","b","a","u"}), new LearnerGraph[]{ifthen1,ifthen3});
			}},IllegalArgumentException.class,"is invalid: either of true/false");
		}

		/** Tests <em>identifyTheOnlyChoice</em>. */
		@Test
		public final void testIdentifyTheOnlyChoice1()
		{
			Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
				PathRoutines.identifyTheOnlyChoice(Arrays.asList(new Boolean[]{}));
			}},IllegalArgumentException.class,"an empty path");
		}
		

		/** Tests <em>identifyTheOnlyChoice</em>. */
		@Test
		public final void testIdentifyTheOnlyChoice2()
		{
			Assert.assertEquals(new Integer(2),PathRoutines.identifyTheOnlyChoice(Arrays.asList(new Boolean[]{true,true,false})));
		}
		
		/** Tests <em>identifyTheOnlyChoice</em>. */
		@Test
		public final void testIdentifyTheOnlyChoice3()
		{
			Assert.assertEquals(new Integer(AbstractOracle.USER_ACCEPTED),PathRoutines.identifyTheOnlyChoice(Arrays.asList(new Boolean[]{true,true,true})));
		}
		
		/** Tests <em>identifyTheOnlyChoice</em>. */
		@Test
		public final void testIdentifyTheOnlyChoice4()
		{
			Assert.assertNull(PathRoutines.identifyTheOnlyChoice(Arrays.asList(new Boolean[]{true,true,null})));
		}
		
		/** Tests <em>identifyTheOnlyChoice</em>. */
		@Test
		public final void testIdentifyTheOnlyChoice5()
		{
			Assert.assertNull(PathRoutines.identifyTheOnlyChoice(Arrays.asList(new Boolean[]{true,false,true})));
		}
		
		/** Tests <em>identifyTheOnlyChoice</em>. */
		@Test
		public final void testIdentifyTheOnlyChoice6()
		{
			Assert.assertNull(PathRoutines.identifyTheOnlyChoice(Arrays.asList(new Boolean[]{true,null,true})));
		}
		
		/** Tests <em>identifyTheOnlyChoice</em>. */
		@Test
		public final void testIdentifyTheOnlyChoice7()
		{
			Assert.assertNull(PathRoutines.identifyTheOnlyChoice(Arrays.asList(new Boolean[]{false,true,true})));
		}
		
		/** Tests <em>identifyTheOnlyChoice</em>. */
		@Test
		public final void testIdentifyTheOnlyChoice8()
		{
			Assert.assertNull(PathRoutines.identifyTheOnlyChoice(Arrays.asList(new Boolean[]{null,true,true})));
		}
		
		/** Tests <em>identifyTheOnlyChoice</em>. */
		@Test
		public final void testIdentifyTheOnlyChoice9()
		{
			Assert.assertNull(PathRoutines.identifyTheOnlyChoice(Arrays.asList(new Boolean[]{null,null,true})));
		}
		
		/** Tests <em>identifyTheOnlyChoice</em>. */
		@Test
		public final void testIdentifyTheOnlyChoice10()
		{
			Assert.assertNull(PathRoutines.identifyTheOnlyChoice(Arrays.asList(new Boolean[]{null,false,true})));
		}
		
		/** Tests marking of questions as answered. */
		@Test
		public final void testQuestionMarking1()
		{
			graph.learnerCache.questionsPTA = questions;
			graph.transform.AugmentNonExistingMatrixWith(labelList(new String[]{"s","c"}), true);
			Assert.assertEquals(3,questions.getData().size());
			graph.transform.AugmentNonExistingMatrixWith(labelList(new String[]{"s","c","d"}), false);
			Assert.assertEquals(2,questions.getData().size());
		}
		
		/** Tests marking of questions as answered. */
		@Test
		public final void testQuestionMarking_unmatched()
		{
			graph.learnerCache.questionsPTA = questions;
			Assert.assertFalse(graph.transform.AugmentNonExistingMatrixWith(labelList(new String[]{"s","c"}), false));
			Assert.assertFalse(graph.transform.AugmentNonExistingMatrixWith(labelList(new String[]{"s","c","c"}), false));
			Assert.assertFalse(graph.transform.AugmentNonExistingMatrixWith(labelList(new String[]{"s","c","d"}), true));
		}
		
		/** The integration of test of questions, similar to <em>testQuestionAnswering4</em> but 
		 * using different methods. */
		@Test
		public final void testQuestions_and_marking1a()
		{
			//Visualiser.updateFrame(graph, null);
			//Visualiser.updateFrame(ComputeQuestions.constructGraphWithQuestions(pair,graph,merged), buildLearnerGraph(ifthen_s, "ifthen_s",config));
			//Visualiser.updateFrame(ComputeQuestions.constructGraphWithQuestions(pair,graph,merged),buildLearnerGraph(ifthen_sc, "ifthen_sc",config));
			List<List<Label>> qs = ComputeQuestions.computeQS(pair, graph, merged, new LearnerGraph[] {
					buildLearnerGraph(ifthen_s, "ifthen_s",config),
					buildLearnerGraph(ifthen_sc, "ifthen_sc",config)
			});
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			Assert.assertEquals(1,qs.size());
			Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}},config);
			Set<List<Label>> actual = new LinkedHashSet<List<Label>>();actual.addAll(qs);
			Assert.assertEquals(expected,actual);
		}
		
		/** Almost the same as the above, but the order of elements in the array of properties is different. */
		@Test
		public final void testQuestions_and_marking1b()
		{
			List<List<Label>> qs = ComputeQuestions.computeQS(pair, graph, merged, new LearnerGraph[] {
					buildLearnerGraph(ifthen_sc, "ifthen_sc",config),
					buildLearnerGraph(ifthen_s, "ifthen_s",config)
			});
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			Assert.assertEquals(1,qs.size());
			Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}},config);
			Set<List<Label>> actual = new LinkedHashSet<List<Label>>();actual.addAll(qs);
			Assert.assertEquals(expected,actual);
		}
		
		/** The integration of test of questions, similar to <em>testQuestionAnswering6</em> but 
		 * using different methods. */
		@Test
		public final void testQuestions_and_marking2()
		{
			LearnerGraph[] properties = new LearnerGraph[]{
					buildLearnerGraph(ifthen_sc, "ifthen_sc",config)};
			List<List<Label>> qs = ComputeQuestions.computeQS(pair, graph, merged, properties);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			Assert.assertEquals(3,qs.size());
			
			graph.transform.AugmentNonExistingMatrixWith(labelList(new String[]{"s","c"}), true);
			qs = ComputeQuestions.RecomputeQS(pair, graph, merged, properties);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			Assert.assertEquals(1,qs.size());

			Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{"s","c","c","f"}},config);
			Set<List<Label>> actual = new LinkedHashSet<List<Label>>();actual.addAll(qs);
			Assert.assertEquals(expected,actual);
		}

		/** Interaction between <em>computeQS</em> and <em>recomputeQS</em>. */
		@Test
		public final void testQuestions_and_marking3()
		{
			LearnerGraph [] properties = new LearnerGraph[] {
					buildLearnerGraph(ifthen_sc, "ifthen_sc",config)};
			List<List<Label>> qs = ComputeQuestions.computeQS(pair, graph, merged, properties);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			Assert.assertEquals(3,qs.size());

			graph.transform.AugmentNonExistingMatrixWith(labelList(new String[]{"s","c"}), true);
			// the above call should not affect computeQS
			qs = ComputeQuestions.computeQS(pair, graph, merged, properties);
			compareGraphs(new LearnerGraph(origGraph,config),graph);// check that augment did not modify the automaton
			Assert.assertEquals(3,qs.size());
		}
	}
}
