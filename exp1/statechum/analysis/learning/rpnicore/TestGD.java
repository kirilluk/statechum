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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;

import statechum.DeterministicDirectedSparseGraph;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.StatechumXML;
import javax.xml.parsers.DocumentBuilderFactory;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.rpnicore.GD.ChangesRecorder;
import statechum.analysis.learning.rpnicore.GD.LearnerGraphMutator;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;
import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;

/**
 * @author kirill
 *
 */
public class TestGD {

	protected Configuration cloneConfig = null;
	
	@Before
	public final void beforeTest()
	{
		 cloneConfig = Configuration.getDefaultConfiguration().copy();cloneConfig.setLearnerCloneGraph(false);		
	}
	
	/** Tests that supplied states are not cloned when I ask them not to be cloned. 
	 * <em>grAnother</em> serves as a holder for vertices I'd like to add, it is not really used for any other purpose. 
	 */
	@Test
	public final void testAddTransitions1()
	{
		LearnerGraph gr = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d-#C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "c", grAnother.findVertex("C"));
		Assert.assertEquals(3,gr.getStateNumber());// the first state is the default initial one.
		Assert.assertSame(gr.findVertex("B"),grAnother.findVertex("B"));
		Assert.assertEquals(gr.findVertex("B"),grAnother.findVertex("B"));
		Assert.assertSame(gr.findVertex("C"),grAnother.findVertex("C"));
		Assert.assertEquals(gr.findVertex("C"),grAnother.findVertex("C"));
		
		// Now check the transition matrix
		Assert.assertEquals(gr.findVertex("C"),gr.transitionMatrix.get(gr.findVertex("B")).get("c"));
		Assert.assertEquals(1,gr.pathroutines.countEdges());
	}
	
	/** Tests that supplied states are not cloned when there is already a state with that ID. */
	@Test
	public final void testAddTransitions2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("B-a->D","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d-#C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("\nB-c-#C\nB-a->D","testAddTransitions0E"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "c", grAnother.findVertex("C"));
		Assert.assertEquals(3,gr.getStateNumber());
		Assert.assertNotSame(gr.findVertex("B"),grAnother.findVertex("B"));
		Assert.assertEquals(gr.findVertex("B"),grAnother.findVertex("B"));

		// Now check the transition matrix
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** Tests that supplied states are not cloned when there is already a state with that ID and cloned otherwise. */
	@Test
	public final void testAddTransitions3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d-#C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("T-e->B\nT-a-#C\nB-c-#C\nB-e->Q\n","testAddTransitions0E"),Configuration.getDefaultConfiguration());
		cloneConfig.setLearnerCloneGraph(true);expected.findVertex(VertexID.parseID("T")).setColour(JUConstants.BLUE);
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		CmpVertex Q = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("Q"), cloneConfig);
		CmpVertex B = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"), cloneConfig);
		patcher.addTransition(B, "c", grAnother.findVertex("C"));
		patcher.addTransition(B, "e", Q);
		CmpVertex newT = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("T"), cloneConfig);
		newT.setColour(JUConstants.BLUE);
		patcher.addTransition(newT, "e", B);
		Assert.assertEquals(4,gr.getStateNumber());
		Assert.assertNotSame(gr.findVertex("C"),grAnother.findVertex("C"));
		Assert.assertEquals(gr.findVertex("C"),grAnother.findVertex("C"));
		Assert.assertNotSame(Q,gr.findVertex("Q"));
		Assert.assertEquals(Q,gr.findVertex("Q"));
		
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** Tests that states can be forcefully added and their attributes override those which already exist but the existing states remain. */
	@Test
	public final void testAddState1()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("\nT-a-#C","testAddTransitions0E"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		CmpVertex updatedC = grAnother.findVertex("C"), newD = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("D"), cloneConfig);
		updatedC.setDepth(4);newD.setDepth(3);
		CmpVertex origC = gr.findVertex("C");
		
		// add the two states
		patcher.addVertex(updatedC);
		patcher.addVertex(newD);
		
		Assert.assertEquals(3,gr.getStateNumber());
		Assert.assertSame(origC,gr.findVertex("C"));Assert.assertNotSame(updatedC,gr.findVertex("C"));Assert.assertSame(newD,gr.findVertex("D"));
		Configuration clone = cloneConfig.copy();clone.setLearnerCloneGraph(true);
		expected.findVertex("C").setDepth(4);
		expected.transitionMatrix.put(AbstractLearnerGraph.cloneCmpVertex(newD, clone),expected.createNewRow());
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(expected.findVertex("D"),gr.findVertex("D")));
	}

	/** Tests that the addition to pairs of incompatibles states works. */
	@Test
	public final void testAddIncompatibles1()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C\nQ-a->Q\nT-longpath->Q","testAddIncompatibles1a"), Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("T-a-#C\nQ-a->Q\nT-longpath->Q","testAddIncompatibles1b"),Configuration.getDefaultConfiguration());
		expected.addToCompatibility(expected.findVertex("T"), expected.findVertex("Q"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addToCompatibility(gr.findVertex("T"), gr.findVertex("Q"), JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** Tests that the addition to pairs of incompatibles states works. */
	@Test
	public final void testAddIncompatibles2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C\nQ-a->Q\nT-longpath->Q","testAddIncompatibles1a"), Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("T-a-#C\nQ-a->Q\nT-longpath->Q","testAddIncompatibles1b"),Configuration.getDefaultConfiguration());
		expected.addToCompatibility(expected.findVertex("T"), expected.findVertex("Q"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addToCompatibility(gr.findVertex("T"), gr.findVertex("C"), JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		patcher.addToCompatibility(gr.findVertex("T"), gr.findVertex("Q"), JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		patcher.removeFromCompatibility(gr.findVertex("C"), gr.findVertex("T"));
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** Tests that non-existing vertices cannot be added or removed. */
	@Test
	public final void testAddIncompatibles3()
	{
		final LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C\nQ-a->Q","testAddIncompatibles1a"), Configuration.getDefaultConfiguration());
		final LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addToCompatibility(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("S"), cloneConfig),gr.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertNotNull(gr.findVertex(VertexID.parseID("S")));
		patcher.addToCompatibility(gr.findVertex("T"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("U"), cloneConfig),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertNotNull(gr.findVertex(VertexID.parseID("U")));
	}
	
	/** Tests that non-existing vertices cannot be added or removed. */
	@Test
	public final void testAddIncompatibles4()
	{
		final LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C\nQ-a->Q","testAddIncompatibles1a"), Configuration.getDefaultConfiguration());
		final LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.removeFromCompatibility(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("S"), cloneConfig),gr.findVertex("T"));
		Assert.assertNotNull(gr.findVertex(VertexID.parseID("S")));
		patcher.removeFromCompatibility(gr.findVertex("T"),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("U"), cloneConfig));
		Assert.assertNotNull(gr.findVertex(VertexID.parseID("U")));
	}
	
	/** Tests the relabelling process. */
	@Test
	public final void testRelabelling1()
	{
		LearnerGraph gr = new LearnerGraph(Configuration.getDefaultConfiguration()), result = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(Configuration.getDefaultConfiguration());
		final LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.relabel(result);
		Assert.assertEquals(expected,result);
	}
	
	/** Tests the relabelling process. */
	@Test
	public final void testRelabelling2()
	{
		LearnerGraph gr = new LearnerGraph(Configuration.getDefaultConfiguration()), result = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(Configuration.getDefaultConfiguration());
		gr.initEmpty();expected.initEmpty();
		final LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.relabel(result);
		Assert.assertEquals(expected,result);
	}
	
	/** Tests that non-existing vertices cannot be mentioned on the left-hand side of pairs to relabel. */
	@Test
	public final void testRelabelling_fail1()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C\nQ-a->Q","testAddIncompatibles1a"), Configuration.getDefaultConfiguration());
		final LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addRelabelling(VertexID.parseID("Q"), VertexID.parseID("U"));
		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() {
			patcher.addRelabelling(VertexID.parseID("S"), VertexID.parseID("S"));
		}},IllegalArgumentException.class,"does not exist");
	}
	
	/** Duplicate vertex after relabelling. */
	@Test
	public final void testRelabelling_fail2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C\nQ-a->Q","testAddIncompatibles1a"), Configuration.getDefaultConfiguration());
		final LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addRelabelling(VertexID.parseID("T"), VertexID.parseID("Q"));
		Helper.checkForCorrectException(new whatToRun() { @Override public void run() {
			patcher.relabel(new LearnerGraph(Configuration.getDefaultConfiguration()));
		}},IllegalArgumentException.class,"duplicate");
	}
	
	/** Tests the relabelling process with an empty collection of pairs to relabel with. */
	@Test
	public final void testRelabelling3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C\nQ-a->Q","testAddIncompatibles1a"), Configuration.getDefaultConfiguration()), result = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("T-a-#C\nQ-a->Q","testAddIncompatibles1b"),Configuration.getDefaultConfiguration());
		final LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.relabel(result);
		Assert.assertEquals(expected,result);
	}
	
	/** Tests the relabelling process with a collection of pairs to relabel with. */
	@Test
	public final void testRelabelling4()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C\nQ-a->Q","testRelabelling4a"), Configuration.getDefaultConfiguration()), result = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a-#C\nB-a->B","testRelabelling4b"),Configuration.getDefaultConfiguration());
		final LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addRelabelling(VertexID.parseID("T"), VertexID.parseID("A"));
		patcher.addRelabelling(VertexID.parseID("Q"), VertexID.parseID("B"));
		patcher.relabel(result);
		Assert.assertEquals(expected,result);
	}
	
	/** Tests that inconsistent acceptance conditions are detected. */
	@Test(expected=IllegalArgumentException.class)
	public final void testAddTransitions4()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d->C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "c", grAnother.findVertex("C"));
	}
	
	/** Tests that inconsistent acceptance conditions are detected. */
	@Test(expected=IllegalArgumentException.class)
	public final void testAddTransitions5()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("C-d->A","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("C"), "c", grAnother.findVertex("A"));
	}
	
	/** Tests that I cannot replace an existing transition. Deterministic case. */
	@Test(expected=IllegalArgumentException.class)
	public final void testAddTransitions6a()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("T"), "a", grAnother.findVertex("C"));
	}
	
	/** Tests that I cannot replace an existing transition. Non-deterministic case. */
	@Test(expected=IllegalArgumentException.class)
	public final void testAddTransitions6b()
	{
		LearnerGraphND gr = new LearnerGraphND(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraphND grAnother = new LearnerGraphND(buildGraph("T-a-#C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> patcher = new LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("T"), "a", grAnother.findVertex("C"));
	}
	
	/** Tests that supplied states are cloned when I ask them to be cloned. */
	@Test
	public final void testAddTransitions7()
	{
		LearnerGraph gr = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d->C","testAddTransitions7a"),Configuration.getDefaultConfiguration());
		cloneConfig.setLearnerCloneGraph(true);
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "c", grAnother.findVertex("C"));
		Assert.assertNotSame(gr.findVertex("B"),grAnother.findVertex("B"));
		Assert.assertEquals(gr.findVertex("B"),grAnother.findVertex("B"));
		Assert.assertNotSame(gr.findVertex("C"),grAnother.findVertex("C"));
		Assert.assertEquals(gr.findVertex("C"),grAnother.findVertex("C"));

		// Now check the transition matrix
		Assert.assertEquals(gr.findVertex("C"),gr.transitionMatrix.get(gr.findVertex("B")).get("c"));
		Assert.assertEquals(1,gr.pathroutines.countEdges());
	}

	/** Test that I can add transitions in a non-deterministic case. */
	@Test
	public final void testAddTransitions8()
	{
		LearnerGraphND gr = new LearnerGraphND(buildGraph("A-a->B-a->C\nA-b->D","testAddTransitions8a"),Configuration.getDefaultConfiguration());
		LearnerGraphND grAnother = new LearnerGraphND(buildGraph("B-d->C\nB-d->D","testAddTransitions8b"),Configuration.getDefaultConfiguration());
		grAnother.getInit().setColour(null);
		LearnerGraphND expected = new LearnerGraphND(buildGraph("A-a->B-a->C\nA-b->D"+"\nB-d->C\nB-d->B\nB-d->D","testAddTransitions8"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> patcher = new LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "d", grAnother.findVertex("C"));
		patcher.addTransition(grAnother.findVertex("B"), "d", grAnother.findVertex("B"));
		patcher.addTransition(grAnother.findVertex("B"), "d", grAnother.findVertex("D"));
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}

	/** Test that I can add transitions in a non-deterministic case. */
	@Test
	public final void testAddTransitions9()
	{
		LearnerGraphND gr = new LearnerGraphND(buildGraph("A-a->B-a->C\nA-b->D","testAddTransitions9a"),Configuration.getDefaultConfiguration());
		LearnerGraphND grAnother = new LearnerGraphND(buildGraph("B-d->C\nB-d->D\nF-a->E","testAddTransitions9b"),Configuration.getDefaultConfiguration());
		grAnother.getInit().setColour(null);
		LearnerGraphND expected = new LearnerGraphND(buildGraph("A-a->B-a->C\nA-b->D"+"\nB-a->F-d->C\nF-d->B\nF-d->E","testAddTransitions8"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> patcher = new LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("F"), "d", grAnother.findVertex("C"));
		patcher.addTransition(grAnother.findVertex("F"), "d", grAnother.findVertex("B"));
		patcher.addTransition(grAnother.findVertex("B"), "a", grAnother.findVertex("F"));
		patcher.addTransition(grAnother.findVertex("F"), "d", grAnother.findVertex("E"));
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}

	/** As in many other tests here, <em>grAnother</em> is a container for vertices I use, so that I do not have to do 
	 * <em>AbstractLearnerGraph.generateNewCmpVertex</em>.
	 */
	@Test
	public final void testAddTransitions_init1()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d-#C","testAddTransitionsG1A"),Configuration.getDefaultConfiguration());
		grAnother.getInit().setColour(null);
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D"+"\nB-d-#C","testAddTransitionsG1E"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "d", grAnother.findVertex("C"));
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}

	/** Adding a transition from an initial state. */
	@Test
	public final void testAddTransitions_init2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#C","testAddTransitionsG2A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D"+"\nA-d-#C","testAddTransitionsG2E"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("A"), "d", grAnother.findVertex("C"));
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** Adding a transition to an initial state. */
	@Test
	public final void testAddTransitions_init3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d->A","testAddTransitionsG3A"),Configuration.getDefaultConfiguration());
		grAnother.findVertex("A").setColour(JUConstants.AMBER);grAnother.getInit().setColour(null);
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D"+"\nB-d->A","testAddTransitionsG3E"),Configuration.getDefaultConfiguration());
		expected.findVertex("A").setColour(JUConstants.AMBER);
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "d", grAnother.findVertex("A"));
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}
	
	@Test
	public final void testAddInit1()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grB = new LearnerGraph(buildGraph("C-b->T","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		gr.setInit(null);
		patcher.setInitial(grB.findVertex("T"));Assert.assertSame(gr.findVertex("T"),gr.getInit());
	}
	
	@Test
	public final void testAddInit2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grB = new LearnerGraph(buildGraph("T-u-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		gr.setInit(null);
		patcher.setInitial(grB.findVertex("C"));Assert.assertSame(gr.findVertex("C"),gr.getInit());
	}
	
	@Test
	public final void testAddInit3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grB = new LearnerGraph(buildGraph("C-b-#T\nC-c->Q","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		gr.setInit(null);
		patcher.setInitial(grB.findVertex("Q"));Assert.assertSame(gr.findVertex("Q"),gr.getInit());
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void testAddInit4()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grB = new LearnerGraph(buildGraph("C-b-#T\nC-c->Q","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		gr.setInit(null);
		patcher.setInitial(grB.findVertex("T"));
	}
	
	@Test
	public final void testRemoveTransitions1()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B\nA-b-#D","testRemoveTransitions1E"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.removeTransition(grAnother.findVertex("B"), "a", grAnother.findVertex("C"));
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** remove a transition from an initial state. */
	@Test
	public final void testRemoveTransitions2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C","testRemoveTransitions2E"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.removeTransition(grAnother.findVertex("A"), "b", grAnother.findVertex("D"));
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** remove a transition from an initial state. */
	@Test
	public final void testRemoveTransitions3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->A","testAddTransitions3"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.removeTransition(grAnother.findVertex("B"), "b", grAnother.findVertex("A"));
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** remove a loop around an initial state. */
	@Test
	public final void testRemoveTransitions4()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nA-u->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.removeTransition(grAnother.findVertex("A"), "u", grAnother.findVertex("A"));
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** Tests that a transition with a non-existing label cannot be removed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveTransitions5()
	{
		final LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nA-b->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		final LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		final LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.removeTransition(grAnother.findVertex("A"), "c", grAnother.findVertex("A"));
	}

	/** Tests that a transition with a non-existing target state cannot be removed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveTransitions6()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nA-b->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.removeTransition(grAnother.findVertex("A"), "b", grAnother.findVertex("T"));
	}

	/** Tests that an empty graph cannot be compressed. */
	@Test
	public final void testRemoveDangling1()
	{
		LearnerGraph gr = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.removeDanglingStates();
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}

	
	/** Tests that dangling states are compressed. */
	@Test
	public final void testRemoveDangling2a()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->T","testRemoveDangling1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C","testRemoveDangling2E"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.removeTransition(gr.findVertex("B"), "b", gr.findVertex("T"));
		patcher.removeTransition(gr.findVertex("A"), "b", gr.findVertex("D"));
		patcher.removeDanglingStates();
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}

	/** Tests that dangling states are compressed. */
	@Test
	public final void testRemoveDangling2b()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->T","testRemoveDangling1"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.removeTransition(gr.findVertex("B"), "b", gr.findVertex("T"));
		patcher.removeTransition(gr.findVertex("A"), "a", gr.findVertex("B"));
		patcher.removeTransition(gr.findVertex("B"), "a", gr.findVertex("C"));
		patcher.removeTransition(gr.findVertex("A"), "b", gr.findVertex("D"));
		patcher.removeDanglingStates();
		Assert.assertEquals(1, gr.getStateNumber());
		Assert.assertNotNull(gr.findVertex("A"));// the initial state
		Assert.assertSame(gr.getInit(), gr.findVertex("A"));
	}

	/** Tests that states added explicitly are preserved. */
	@Test
	public final void testRemoveDangling3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->T","testRemoveDangling1"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.removeTransition(gr.findVertex("B"), "b", gr.findVertex("T"));
		patcher.removeTransition(gr.findVertex("A"), "a", gr.findVertex("B"));
		patcher.removeTransition(gr.findVertex("B"), "a", gr.findVertex("C"));
		patcher.removeTransition(gr.findVertex("A"), "b", gr.findVertex("D"));
		patcher.addVertex(gr.findVertex("D"));
		patcher.removeDanglingStates();
		Assert.assertEquals(2, gr.getStateNumber());
		Assert.assertNotNull(gr.findVertex("A"));// the initial state
		Assert.assertSame(gr.getInit(), gr.findVertex("A"));
		Assert.assertNotNull(gr.findVertex("D"));
	}
	
	/** Tests that a state with a transition leading to it is not compressed. */
	@Test
	public final void testRemoveDangling4()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->T","testRemoveDangling1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->T","testRemoveDangling1"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator<CmpVertex,LearnerGraphCachedData> patcher = new LearnerGraphMutator<CmpVertex,LearnerGraphCachedData>(gr, cloneConfig,null);
		patcher.removeDanglingStates();
		Assert.assertNull(WMethod.checkM_and_colours(expected, gr, VERTEX_COMPARISON_KIND.DEEP));
	}
	
	
	protected static final Document createDoc()
	{
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		Document doc = null;
		try
		{
			factory.setFeature(javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();
		}
		catch(Exception ex)
		{
			statechum.Helper.throwUnchecked("configuration exception: ",ex);
		}
		
		return doc;
	}
	
	@Test
	public final void testEmpty1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			GD.ChangesRecorder.getGraphElement(createDoc().createElement("junk"),"whatever",false,true);
		}},IllegalArgumentException.class,"unexpected element");
	}

	@Test
	public final void testEmpty2()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			GD.ChangesRecorder.getGraphElement(createDoc().createElement("junk"),"whatever",true,true);
		}},IllegalArgumentException.class,"unexpected element");
	}

	@Test
	public final void testNoElem1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			GD.ChangesRecorder.getGraphElement(createDoc().createElement(StatechumXML.gdGD.toString()),"whatever",false,true);
		}},IllegalArgumentException.class,"no element");
	}

	@Test
	public final void testNoElem2()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			GD.ChangesRecorder.getGraphElement(createDoc().createElement(StatechumXML.gdGD.toString()),"whatever",true,true);
		}},IllegalArgumentException.class,"no element");
	}

	private static final String graphholder = "graphholder";
	
	/** graph holder does not contain any children elements. */
	@Test
	public final void testElemNoGraph1()
	{
		final Document doc = createDoc();
		final Element top = doc.createElement(StatechumXML.gdGD.toString()),subElement=doc.createElement(graphholder);
		top.appendChild(subElement);

		checkForCorrectException(new whatToRun() { public @Override void run() {
			GD.ChangesRecorder.getGraphElement(top,graphholder,false,true);
		}},IllegalArgumentException.class,"no nodes");
	}

	@Test
	public final void testElemNoGraph2()
	{
		Document doc = createDoc();
		Element top = doc.createElement(StatechumXML.gdGD.toString()),subElement=doc.createElement(graphholder);
		top.appendChild(subElement);
		Assert.assertNull(GD.ChangesRecorder.getGraphElement(top,graphholder,true,true));
	}

	/** The only child of the graph holder is not a node (it is a chunk of text). */
	@Test
	public final void testElemNoGraph3()
	{
		final Document doc = createDoc();
		final Element top = doc.createElement(StatechumXML.gdGD.toString()),subElement=doc.createElement(graphholder);
		top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));

		checkForCorrectException(new whatToRun() { public @Override void run() {
			GD.ChangesRecorder.getGraphElement(top,graphholder,false,true);
		}},IllegalArgumentException.class,"no nodes");
	}

	@Test
	public final void testElemNoGraph4()
	{
		Document doc = createDoc();
		Element top = doc.createElement(StatechumXML.gdGD.toString()),subElement=doc.createElement(graphholder);
		top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
		Assert.assertNull(GD.ChangesRecorder.getGraphElement(top,graphholder,true,true));
	}

	@Test
	public final void testElemGraph()
	{
		Document doc = createDoc();
		Element top = doc.createElement(StatechumXML.gdGD.toString()),subElement=doc.createElement(graphholder);
		top.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(doc.createTextNode("junk"));
		Element graphElem = doc.createElement("mygraph");
		top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(graphElem);
		Assert.assertSame(graphElem,GD.ChangesRecorder.getGraphElement(top,graphholder,false,true));
	}

	@Test
	public final void testElemDuplicateGraph1()
	{
		final Document doc = createDoc();
		final Element top = doc.createElement(StatechumXML.gdGD.toString()),subElement=doc.createElement(graphholder);
		Element graphElem = doc.createElement("mygraph");
		top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(graphElem);subElement.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(doc.createElement("anotherelement"));
		subElement.appendChild(doc.createTextNode("junk"));

		checkForCorrectException(new whatToRun() { public @Override void run() {
			GD.ChangesRecorder.getGraphElement(top,graphholder,false,true);
		}},IllegalArgumentException.class,"more than one node");
	}

	@Test
	public final void testElemDuplicateGraph2()
	{
		final Document doc = createDoc();
		final Element top = doc.createElement(StatechumXML.gdGD.toString()),subElement=doc.createElement(graphholder);
		Element graphElem = doc.createElement("mygraph");
		top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(graphElem);subElement.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(doc.createElement("anotherelement"));
		subElement.appendChild(doc.createTextNode("junk"));
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			GD.ChangesRecorder.getGraphElement(top,graphholder,true,true);
		}},IllegalArgumentException.class,"more than one node");
	}

	@Test
	public final void testElemJunkElementInGD()
	{
		Document doc = createDoc();
		Element top = doc.createElement(StatechumXML.gdGD.toString()),subElement=doc.createElement(graphholder);
		Element graphElem = doc.createElement("mygraph");
		top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(graphElem);
		top.appendChild(doc.createElement("smth"));
		Assert.assertSame(graphElem,GD.ChangesRecorder.getGraphElement(top,graphholder,false,true));
	}
	
	@Test
	public final void testElemDuplicateHolder1()
	{
		final Document doc = createDoc();
		final Element top = doc.createElement(StatechumXML.gdGD.toString()),subElement=doc.createElement(graphholder);
		Element graphElem = doc.createElement("mygraph");
		top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(graphElem);
		top.appendChild(doc.createElement(graphholder));

		checkForCorrectException(new whatToRun() { public @Override void run() {
			GD.ChangesRecorder.getGraphElement(top,graphholder,false,true);
		}},IllegalArgumentException.class,"duplicate holder");
	}

	@Test
	public final void testElemDuplicateHolder2()
	{
		final Document doc = createDoc();
		final Element top = doc.createElement(StatechumXML.gdGD.toString()),subElement=doc.createElement(graphholder);
		Element graphElem = doc.createElement("mygraph");
		top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(graphElem);
		top.appendChild(doc.createElement(graphholder));

		checkForCorrectException(new whatToRun() { public @Override void run() {
			GD.ChangesRecorder.getGraphElement(top,graphholder,true,true);
		}},IllegalArgumentException.class,"duplicate holder");
	}

	/** Checks that if checking is off, duplicate elements are permitted. */
	@Test
	public final void testElemDuplicateHolder3()
	{
		Document doc = createDoc();
		Element top = doc.createElement(StatechumXML.gdGD.toString()),subElement=doc.createElement(graphholder);
		Element graphElem = doc.createElement("mygraph");
		top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(graphElem);
		top.appendChild(doc.createElement(graphholder));
		GD.ChangesRecorder.getGraphElement(top,graphholder,true,false);
	}

	@Test
	public final void testWriteAndLoad1()
	{
		LearnerGraphND graph = new LearnerGraphND(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraphND removed = new LearnerGraphND(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraphND added = new LearnerGraphND(buildGraph("A-d-#E\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		ChangesRecorder patcher = new ChangesRecorder(removed,added,null);

		ChangesRecorder.applyGD(graph, patcher.writeGD(createDoc()));
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#E\nA-c->A","testWriteAndLoad1"),Configuration.getDefaultConfiguration());
		Assert.assertNull(WMethod.checkM_and_colours(expected, graph, VERTEX_COMPARISON_KIND.DEEP));
	}

	@Test
	public final void testWriteAndLoad2()
	{
		LearnerGraphND graph = new LearnerGraphND(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraphND removed = new LearnerGraphND(buildGraph("A-d-#D\nA-a->B\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraphND added = new LearnerGraphND(buildGraph("A-d-#E\nB-a-#C\nB-c->B\nA-q->B","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		ChangesRecorder patcher = new ChangesRecorder(removed,added,null);

		ChangesRecorder.applyGD(graph, patcher.writeGD(createDoc()));
		LearnerGraph expected = new LearnerGraph(buildGraph("A-q->B-a-#C\nA-d-#E\nA-c->A\nB-c->B","testWriteAndLoad1"),Configuration.getDefaultConfiguration());
		Assert.assertNull(WMethod.checkM_and_colours(expected, graph, VERTEX_COMPARISON_KIND.DEEP));
	}
	
	@Test
	public final void testWriteAndLoad3()
	{
		LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		ChangesRecorder patcher = new ChangesRecorder(null);
		patcher.addTransition(graph.findVertex("B"), "c", graph.findVertex("B"));
		patcher.removeTransition(graph.findVertex("A"), "a", graph.findVertex("B"));
		patcher.addTransition(graph.findVertex("A"), "q", graph.findVertex("B"));
		patcher.setInitial(graph.findVertex("A"));
		ChangesRecorder.applyGD(graph, patcher.writeGD(createDoc()));
		LearnerGraph expected = new LearnerGraph(buildGraph("A-q->B-a-#C\nA-d-#D\nA-c->A\nB-c->B","testWriteAndLoad1"),Configuration.getDefaultConfiguration());
		WMethod.checkM_and_colours(expected, graph, VERTEX_COMPARISON_KIND.DEEP);
	}
	
	/** Tests that initial state has to be assigned. */
	@Test
	public final void testWriteAndLoad4()
	{
		LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		final ChangesRecorder patcher = new ChangesRecorder(null);
		patcher.addTransition(graph.findVertex("B"), "c", graph.findVertex("B"));
		patcher.removeTransition(graph.findVertex("A"), "a", graph.findVertex("B"));
		patcher.addTransition(graph.findVertex("A"), "q", graph.findVertex("B"));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			patcher.writeGD(createDoc());}},
		IllegalArgumentException.class,"init state is was not defined");
	}
	
	/** Test that a graph with incompatible states, relabelling and dangling states is handled correctly. */ 
	@Test
	public final void testWriteAndLoad5()
	{
		LearnerGraphND graph = new LearnerGraphND(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nA-a->S-a->S","testWriteAndLoad5"),Configuration.getDefaultConfiguration());
		graph.findVertex("B").setDepth(5);graph.findVertex("D").setColour(JUConstants.AMBER);
		
		graph.addToCompatibility(graph.findVertex("A"), graph.findVertex("B"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		ChangesRecorder patcher = new ChangesRecorder(null);
		patcher.addTransition(graph.findVertex("B"), "c", graph.findVertex("B"));
		patcher.removeTransition(graph.findVertex("A"), "a", graph.findVertex("B"));
		patcher.removeFromCompatibility(graph.findVertex("B"), graph.findVertex("A"));
		patcher.addToCompatibility(graph.findVertex("B"), graph.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		patcher.addTransition(graph.findVertex("A"), "q", graph.findVertex("B"));
		patcher.setInitial(graph.findVertex("A"));
		patcher.addRelabelling(VertexID.parseID("A"), VertexID.parseID("U"));
		patcher.addRelabelling(VertexID.parseID("C"), VertexID.parseID("R"));
		
		CmpVertex danglingVertex = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("TEST"), cloneConfig);
		danglingVertex.setColour(JUConstants.BLUE);
		patcher.addVertex(danglingVertex);
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
		LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> graphPatcher = new LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(graph,config,null);
		ChangesRecorder.loadDiff(graphPatcher, patcher.writeGD(createDoc()));
		graphPatcher.removeDanglingStates();
		LearnerGraphND result = new LearnerGraphND(Configuration.getDefaultConfiguration());
		graphPatcher.relabel(result); 
		
		LearnerGraphND expected = new LearnerGraphND(buildGraph("U-q->B-a-#R\nU-d-#D\nU-c->U\nB-c->B\nU-a->S-a->S","testWriteAndLoad1"),Configuration.getDefaultConfiguration());
		expected.addToCompatibility(expected.findVertex("S"), expected.findVertex("B"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		expected.transitionMatrix.put(danglingVertex,expected.createNewRow());
		expected.findVertex("B").setDepth(5);expected.findVertex("D").setColour(JUConstants.AMBER);
	
		Assert.assertEquals(expected.getStateNumber(), graph.getStateNumber());
		Assert.assertNull(WMethod.checkM_and_colours(expected, result, VERTEX_COMPARISON_KIND.DEEP));
	}

	
	protected final java.util.Map<CmpVertex,CmpVertex> newToOrig = new java.util.TreeMap<CmpVertex,CmpVertex>();
	
	/** Displays the supplied list of pairs, converting names of states
	 * from the notation of the combined graph to their original names. 
	 * 
	 * @param listOfPairs pairs to display
	 * @param renaming renaming to apply, not used if null
	 */
	protected static void aprintListOfPairs(List<PairScore> listOfPairs,java.util.Map<CmpVertex,CmpVertex> renaming)
	{
		System.out.print("[ ");
		for(PairScore pair:listOfPairs)
		{
			CmpVertex q=pair.getQ(),r=pair.getR();
			if (renaming != null)
			{
				CmpVertex newQ=renaming.get(q),newR=renaming.get(r);
				if (newQ != null) q=newQ;if (newR != null) r=newR;
			}
			System.out.print(new PairScore(q,r,pair.getScore(),pair.getAnotherScore())+" ");
		}
				
		System.out.println("]");
	}
	
	@Test
	public final void testSortingOfWaves0()
	{
		List<PairScore> wave = java.util.Arrays.asList(new PairScore[]{}),
		expected= java.util.Arrays.asList(new PairScore[]{});
		GD.sortWave(wave);
		Assert.assertEquals(expected,wave);
	}
	
	@Test
	public final void testSortingOfWaves1()
	{
		LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		PairScore A=new PairScore(graph.findVertex("A"),graph.findVertex("B"),10,0),
			B=new PairScore(graph.findVertex("B"),graph.findVertex("A"),100,0),
			C=new PairScore(graph.findVertex("A"),graph.findVertex("B"),12,0),
			D=new PairScore(graph.findVertex("B"),graph.findVertex("A"),90,0),
			E=new PairScore(graph.findVertex("A"),graph.findVertex("B"),10,0);

		List<PairScore> wave = java.util.Arrays.asList(new PairScore[]{A,B,C,D,E}),
		expected= java.util.Arrays.asList(new PairScore[]{B,D,C,A,E});
		GD.sortWave(wave);
		Assert.assertEquals(expected,wave);
	}

	/** Creates a sorted array of increasing random numbers between 0 and <em>upTo</em>.
	 *  
	 * @param rd random number generator
	 * @param upTo up the largest+1 number in the array to return
	 * @param count how many elements to create
	 * @return array of random numbers
	 */
	public static List<Integer> chooseRandomly(Random rd,int upTo,int count)
	{
		if (upTo < count || upTo < 0) 
			throw new IllegalArgumentException("cannot generate more than "+upTo+" numbers, "+count+" requested");
		boolean [] numbersChosen = new boolean[upTo];Arrays.fill(numbersChosen, false);
		List<Integer> result = new ArrayList<Integer>(count);
		for(int randomNum=0;randomNum<count;++randomNum) 
		{
			int nextRnd = rd.nextInt(upTo-randomNum);
			int nextPosition = 0,i=0;while(numbersChosen[nextPosition]) nextPosition++;
			// at this point, numbersChosen[nextPosition]=false			
			while(i<nextRnd)
			{
				++i;++nextPosition;
				while(numbersChosen[nextPosition]) nextPosition++;
				// at this point, numbersChosen[nextPosition]=false
			}
			// at every iteration, the loop steps past the last hole and 
			numbersChosen[nextPosition]=true;result.add(nextPosition);
		}
		
		if (GlobalConfiguration.getConfiguration().isAssertEnabled())
		{
			Set<Integer> data = new TreeSet<Integer>();data.addAll(result);
			assert data.size() == count;
		}
		
		return result;
	}
	
	@Test
	public final void testChooseRandomly1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			chooseRandomly(null,4,5);}},
		IllegalArgumentException.class,"cannot generate more");
	}
	
	@Test
	public final void testChooseRandomly2()
	{
		Assert.assertTrue(chooseRandomly(new Random(0),0,0).isEmpty());
	}	
	
	@Test
	public final void testChooseRandomly3()
	{
		Assert.assertTrue(chooseRandomly(new Random(0),4,0).isEmpty());
	}	
	
	@Test
	public final void testChooseRandomly4()
	{
		Assert.assertEquals(Arrays.asList(new Integer[]{0}), chooseRandomly(new Random(0),1,1));
	}	

	@Test
	public final void testChooseRandomly5()
	{
		Set<Integer> expected=new TreeSet<Integer>();expected.add(1);
		Assert.assertEquals(Arrays.asList(new Integer[]{1}), chooseRandomly(new Random(0),2,1));
	}	
	
	@Test
	public final void testChooseRandomly6()
	{
		Assert.assertEquals(Arrays.asList(new Integer[]{5,1,4,0,2}), chooseRandomly(new Random(10),8,5));
	}
	
	@Test
	public final void testChooseRandomly7()
	{
		Assert.assertEquals(Arrays.asList(new Integer[]{3,1,0,2,4}), chooseRandomly(new Random(10),5,5));
	}

	@Test
	public final void testChooseRandomly8()
	{
		Assert.assertEquals(Arrays.asList(new Integer[]{73,47,64,70,14}), chooseRandomly(new Random(10),80,5));
	}
/*
	@Test
	public final void testComputeGD_big5()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		final String path = "resources/LargeGraphs/";
		LearnerGraph grA = Transform.convertToNumerical(LearnerGraph.loadGraph(path+"experiment_500", config));
		LearnerGraph grB = Transform.convertToNumerical(LearnerGraph.loadGraph(path+"experiment_501", config));
		GD gd = new GD();
		LearnerGraph graph = Transform.convertToNumerical(LearnerGraph.loadGraph(path+"experiment_500", config));
		long tmStarted = new Date().getTime();
		System.out.println("loaded ");
		Element xml = gd.computeGDToXML(grA, grB, 2, createDoc());
		System.out.println("patch created "+ (new Date().getTime()-tmStarted)/1000);
		tmStarted = new Date().getTime();
		ChangesRecorder.applyGD(graph, xml);
		System.out.println("patch applied "+ (new Date().getTime()-tmStarted)/1000);
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
*/
	
	public static class ProgressIndicator
	{
		private int cnt=0,p=1;
		private final int total;

		public ProgressIndicator(String nameArg,int t)
		{
			total = t;
			// if total <=0, little will be displayed, I presume this is ok.
			System.out.print("["+nameArg+" ");
			if (total <= 0) 
			{
				end();p=10;// the assignment to p is for testing
			}
		}
		
		public void next()
		{
			if (cnt < total)
			{// ignore invalid use
				++cnt;
				
				while(cnt > total*p/10)
				{
					++p;System.out.print(".");
				}
				if (cnt == total) end();
			}
		}
		
		private void end()
		{
			System.out.print("] ");
		}
		
		/** For testing only. */
		int getP()
		{
			return p;
		}
	}
	
	@Test
	public final void testProgressIndicator1()
	{
		ProgressIndicator pr = new ProgressIndicator("A",-1);
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
	}
	
	@Test
	public final void testProgressIndicator2()
	{
		ProgressIndicator pr = new ProgressIndicator("A",0);
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
	}
	
	@Test
	public final void testProgressIndicator3()
	{
		ProgressIndicator pr = new ProgressIndicator("A",1);
		Assert.assertEquals(1, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
	}
	
	@Test
	public final void testProgressIndicator4()
	{
		ProgressIndicator pr = new ProgressIndicator("A",2);
		Assert.assertEquals(1, pr.getP());
		pr.next();
		Assert.assertEquals(5, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
	}
	
	@Test
	public final void testProgressIndicator5()
	{
		ProgressIndicator pr = new ProgressIndicator("A",3);
		Assert.assertEquals(1, pr.getP());
		pr.next();
		Assert.assertEquals(4, pr.getP());
		pr.next();
		Assert.assertEquals(7, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
	}
	
	@Test
	public final void testProgressIndicator6()
	{
		ProgressIndicator pr = new ProgressIndicator("A",10);
		Assert.assertEquals(1, pr.getP());
		for(int i=0;i<10;++i)
		{
			pr.next();
			Assert.assertEquals(i+1, pr.getP());
		}
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
	}
	
	@Test
	public final void testProgressIndicator7()
	{
		ProgressIndicator pr = new ProgressIndicator("A",15);
		Assert.assertEquals(1, pr.getP());
		pr.next();
		Assert.assertEquals(1, pr.getP());
		pr.next();
		Assert.assertEquals(2, pr.getP());
	}
	

}
