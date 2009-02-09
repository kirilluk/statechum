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

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;
import statechum.apps.QSMTool;

final public class TestAugmentUsingIFTHEN 
{
	/** Tests merging of the two automata depicted on page 18 of "why_nondet_does_not_matter.xoj" */
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
	
	private void compareGraphs(LearnerGraph A, LearnerGraph B)
	{
		DifferentFSMException ex= WMethod.checkM_and_colours(A, B, VERTEX_COMPARISON_KIND.NONE);
		Assert.assertNull(ex==null?"":ex.toString(),ex);
		
		// reachability of all states ensures that transition structures are isomorphic.
		Assert.assertEquals(A.getStateNumber(),A.pathroutines.computeShortPathsToAllStates().size());
		Assert.assertEquals(B.getStateNumber(),B.pathroutines.computeShortPathsToAllStates().size());
	}
	
	@Test
	public final void testbuildIfThenAutomata1()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		String ltlFormula = "[](a->X[]b)";
		Collection<LearnerGraph> automata = Transform.buildIfThenAutomata(Arrays.asList(new String[]{
				QSMTool.cmdLTL+" "+ltlFormula}), new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-c->D", "testbuildIfThenAutomata1"), config),config);
		Iterator<LearnerGraph> graphIter = automata.iterator();

		LearnerGraph topGraph = graphIter.next(), expectedTop = new LearnerGraph(TestFSMAlgo.buildGraph("I-a->A-b->A | I-b->IA-a->A | I-c->IA-b->IA-c->IA | P-c-#P1 | P-a-#P2 | A = THEN = P | " +
				"I - transition_to_THEN ->P",ltlFormula),config);
		topGraph.addTransition(topGraph.transitionMatrix.get(topGraph.init), "transition_to_THEN", topGraph.findVertex(VertexID.parseID("P"+(topGraph.vertPositiveID-1))));
		graphIter = automata.iterator();
		compareGraphs(expectedTop,graphIter.next());
		Assert.assertFalse(graphIter.hasNext());
	}
	
}
