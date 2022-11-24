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

package statechum.analysis.learning.experiments;

import static statechum.Helper.checkForCorrectException;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.rpnicore.FsmParserStatechum;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

/**
 * @author kirill
 *
 */
public class TestForestFireGenerator {

	protected ForestFireLabelledStateMachineGenerator gen = null;
	Configuration config = Configuration.getDefaultConfiguration();
	ConvertALabel converter = null;
	
	@SuppressWarnings("unchecked")
	@Before
	public void before()
	{
		gen = new ForestFireLabelledStateMachineGenerator(0.3,0.2,0.2,0.1,10,0,config,converter);
		gen.machine = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C\nA-b->B-c->C", "testRandomVertexSelection1",config,converter).pathroutines.getGraph();
		gen.vertices = new LinkedList<DeterministicVertex>();gen.vertices.addAll(gen.machine.getVertices());
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testRandomFail1()
	{
		gen.randomInt(0);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testRandomFail2()
	{
		gen.randomInt(-2);
	}
	
	@Test
	public void testRandom1()
	{
		Assert.assertEquals(0, gen.randomInt(1));
		Assert.assertEquals(0, gen.randomInt(1));
		Assert.assertEquals(0, gen.randomInt(1));
		Assert.assertEquals(0, gen.randomInt(1));
	}

	/** Checks that all values are eventually generated. */
	@Test
	public void testRandom2()
	{
		final int maxCounter = 1000;
		for(int upTo:new int[]{1,23,24,25,26,27,100})
		{
			Set<Integer> values = new TreeSet<Integer>();
			int counter=0;
			while(values.size() < upTo && counter < maxCounter)
			{
				int value = gen.randomInt(upTo);
				Assert.assertTrue(value >= 0 && value <= upTo-1);
				values.add(value);++counter;
			}
			Assert.assertTrue("failed to hit "+upTo,counter < maxCounter);
		}
	}
	
	@Test
	public void testRandomVertexSelection1()
	{
		final int maxCounter = 1000;
		Set<VertID> values = new HashSet<VertID>();
		int counter=0;
		while(counter < maxCounter)
		{
			values.add(gen.selectRandom(null));counter++;
		}
		Assert.assertTrue("failed to choose one of the vertices, chosen "+values.size(),values.size() == 3);
	}
	
	@Test
	public void testRandomVertexSelection2()
	{
		final int maxCounter = 1000;
		Set<DeterministicVertex> blocked = new HashSet<DeterministicVertex>();blocked.add(gen.selectRandom(null));// add one of the vertices.
		Set<VertID> values = new HashSet<VertID>();
		int counter=0;
		while(counter < maxCounter)
		{
			values.add(gen.selectRandom(blocked));counter++;
		}
		Assert.assertTrue("failed to choose one of the vertices, chosen "+values.size(),values.size() == 2);
	}
	
	/** Empty set of vertices. */
	@Test
	public void testRandomVertexSelectionFail1()
	{
		gen.vertices = new LinkedList<DeterministicVertex>();

		checkForCorrectException(new whatToRun() {
			public @Override void run() throws IllegalArgumentException {
				gen.selectRandom(null);
			}
		},IllegalArgumentException.class,"");
	}

	/** All vertices are blocked. */
	@Test
	public void testRandomVertexSelectionFail2()
	{
		final Set<DeterministicVertex> blocked = new HashSet<DeterministicVertex>();
		while(blocked.size() < 3)
			blocked.add(gen.selectRandom(blocked));

		checkForCorrectException(new whatToRun() {
			public @Override void run() throws IllegalArgumentException {
				gen.selectRandom(blocked);
			}
		},IllegalArgumentException.class,"");
	}

	@Test
	public void testAddEdge1()
	{
		DeterministicVertex vA = (DeterministicVertex) DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,VertexID.parseID("C"),gen.machine), 
		vB = (DeterministicVertex) DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,VertexID.parseID("B"),gen.machine);

		LearnerGraph outcome = new LearnerGraph(gen.machine,Configuration.getDefaultConfiguration());
		Assert.assertEquals(0,outcome.getTransitionMatrix().get(outcome.findVertex(VertexID.parseID("C"))).size());
		Assert.assertTrue(gen.addEdgeInternal(vA, vB));// this adds a numeric label
		outcome = new LearnerGraph(gen.machine,Configuration.getDefaultConfiguration());
		Assert.assertEquals(1,outcome.getTransitionMatrix().get(outcome.findVertex(VertexID.parseID("C"))).size());
	}
	
	/** Parallel edge. */
	@Test
	public void testAddEdge2()
	{
		DeterministicVertex vA = (DeterministicVertex) DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,VertexID.parseID("A"),gen.machine), 
		vB = (DeterministicVertex) DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,VertexID.parseID("B"),gen.machine);

		LearnerGraph outcome = new LearnerGraph(gen.machine,Configuration.getDefaultConfiguration());
		Assert.assertEquals(2,outcome.getTransitionMatrix().get(outcome.findVertex(VertexID.parseID("A"))).size());
		Assert.assertTrue(gen.addEdgeInternal(vA, vB));// this adds a numeric label
		outcome = new LearnerGraph(gen.machine,Configuration.getDefaultConfiguration());
		Assert.assertEquals(3,outcome.getTransitionMatrix().get(outcome.findVertex(VertexID.parseID("A"))).size());
	}
	
	@SuppressWarnings("unchecked")
	public Set<DeterministicVertex> getGenMachineVertices()
	{
		return gen.machine.getVertices();
	}
	
	/** Alphabet size zero. */
	@Test
	public void testAddEdgeFail1()
	{
		gen = new ForestFireLabelledStateMachineGenerator(0.3,0.2,0.2,0.1,0,0 ,config,converter);
		gen.machine = FsmParserStatechum.buildLearnerGraph("A-L0->B / A-L1->C / A-L2->D", "testAddEdgeFail2" ,config,converter).pathroutines.getGraph();
		gen.vertices = new LinkedList<DeterministicVertex>();gen.vertices.addAll(getGenMachineVertices());
		final DeterministicVertex vA = (DeterministicVertex) DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,VertexID.parseID("A"),gen.machine), 
		vB = (DeterministicVertex) DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,VertexID.parseID("B"),gen.machine);
		
		Assert.assertFalse(gen.addEdgeInternal(vA, vB));// this fails to add a label
	}

	@Test
	public void testAddEdgeFail2()
	{
		gen = new ForestFireLabelledStateMachineGenerator(0.3,0.2,0.2,0.1,2,0 ,config,converter);
		gen.machine = FsmParserStatechum.buildLearnerGraph("A-L0->B / A-L1->C / A-L2->D", "testAddEdgeFail2" ,config,converter).pathroutines.getGraph();
		gen.vertices = new LinkedList<DeterministicVertex>();gen.vertices.addAll(getGenMachineVertices());
		final DeterministicVertex vA = (DeterministicVertex) DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,VertexID.parseID("A"),gen.machine), 
		vB = (DeterministicVertex) DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,VertexID.parseID("B"),gen.machine);
		
		Assert.assertFalse(gen.addEdgeInternal(vA, vB));// this fails to add a label
	}

	@Test
	public void testSelectVertices()
	{
		gen.machine = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C\nA-b->B-c->C-a->D-a->E", "testRandomVertexSelection1" ,config,converter).pathroutines.getGraph();
		gen.vertices = new LinkedList<DeterministicVertex>();gen.vertices.addAll(getGenMachineVertices());
		final DeterministicVertex 
			vA = (DeterministicVertex) DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,VertexID.parseID("A"),gen.machine), 
			vB = (DeterministicVertex) DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,VertexID.parseID("B"),gen.machine),
			vC = (DeterministicVertex) DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,VertexID.parseID("C"),gen.machine),
			vD = (DeterministicVertex) DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,VertexID.parseID("D"),gen.machine),
			vE = (DeterministicVertex) DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,VertexID.parseID("E"),gen.machine);
		Assert.assertNotNull(vA);Assert.assertNotNull(vB);Assert.assertNotNull(vC);Assert.assertNotNull(vD);Assert.assertNotNull(vE);
		
		/* No vertices to be selected. */
		Assert.assertTrue(gen.selectVertices(gen.vertices, 0).isEmpty());
		
		/* Lots to be selected. */
		Assert.assertTrue(gen.selectVertices(gen.vertices, 10).equals(gen.vertices));
		
		/* All existing ones to be selected. */
		Assert.assertTrue(gen.selectVertices(gen.vertices, 5).equals(gen.vertices));
		
		Set<DeterministicVertex> selectedVertices = new HashSet<DeterministicVertex>(), existingVertices = new HashSet<DeterministicVertex>();existingVertices.addAll(gen.vertices);
		selectedVertices.clear();selectedVertices.addAll(gen.selectVertices(gen.vertices, 5));Assert.assertTrue(selectedVertices.equals(existingVertices));

		/* Chosen vertices are different. */
		selectedVertices.clear();selectedVertices.addAll(gen.selectVertices(gen.vertices, 4));Assert.assertEquals(4,selectedVertices.size());

		/* A single vertex can be chosen. */
		selectedVertices.clear();selectedVertices.addAll(gen.selectVertices(gen.vertices, 1));Assert.assertEquals(1,selectedVertices.size());
	}
	
	@Test
	public void testEffectiveDiameter1()
	{
		gen.machine = new DirectedSparseGraph();
		gen.vertices = new LinkedList<DeterministicVertex>();gen.vertices.addAll(getGenMachineVertices());
		Assert.assertEquals(0,ForestFireStateMachineGenerator.getEffectiveDiameter(gen.machine));
	}

	@Test
	public void testEffectiveDiameter2()
	{
		Assert.assertEquals(1,ForestFireStateMachineGenerator.getEffectiveDiameter(gen.machine));
	}

	@Test
	public void testEffectiveDiameter3()
	{
		gen.machine = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C-a->D-a->A", "testEffectiveDiameter3" ,config,converter).pathroutines.getGraph();
		gen.vertices = new LinkedList<DeterministicVertex>();gen.vertices.addAll(getGenMachineVertices());
		Assert.assertEquals(3,ForestFireStateMachineGenerator.getEffectiveDiameter(gen.machine));
	}
	
	@Test
	public void testEffectiveDiameter4()
	{
		gen.machine = FsmParserStatechum.buildLearnerGraph("A-a->A", "testEffectiveDiameter4" ,config,converter).pathroutines.getGraph();
		gen.vertices = new LinkedList<DeterministicVertex>();gen.vertices.addAll(getGenMachineVertices());
		// AA 1
		Assert.assertEquals(0,ForestFireStateMachineGenerator.getEffectiveDiameter(gen.machine));
	}
	
	@Test
	public void testEffectiveDiameter5()
	{
		gen.machine = FsmParserStatechum.buildLearnerGraph("A-a->B", "testEffectiveDiameter4" ,config,converter).pathroutines.getGraph();
		gen.vertices = new LinkedList<DeterministicVertex>();gen.vertices.addAll(getGenMachineVertices());
		// AA 1
		Assert.assertEquals(1,ForestFireStateMachineGenerator.getEffectiveDiameter(gen.machine));
	}
}
