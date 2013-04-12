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
package statechum.analysis.learning.experiments.mutation;

import static org.junit.Assert.*;
import static statechum.Helper.checkForCorrectException;

import java.util.List;
import java.util.Map;
import java.util.Random;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper.whatToRun;
import statechum.Label;
import statechum.analysis.learning.experiments.mutation.GraphMutator.FailureToMutateException;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraphND;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;

public class TestGraphMutator {

	Configuration config = Configuration.getDefaultConfiguration();
	GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> mutator = null;
	
	void checkContainsTransition(String from, String to, String labelAsString)
	{
		LearnerGraphND mut = (LearnerGraphND)mutator.getMutated();Map<CmpVertex,Map<Label,List<CmpVertex>>> matrix = mut.getTransitionMatrix();
		Label label = AbstractLearnerGraph.generateNewLabel(labelAsString, mut.config);
		Assert.assertTrue(matrix.get(mut.findVertex(VertexID.parseID(from))).get(label).contains(mut.findVertex(VertexID.parseID(to))));
	}
	
	/** Attempts to add a transition once. */
	@Test
	public void testAddTransition1()
	{
		Configuration conf = config.copy();conf.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		LearnerGraphND gr = buildLearnerGraphND("A-a->B", "testAddTransition1",conf,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeBetweenExistingStates();
		
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(2,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("B", "B", "a");
	}
	
	/** Attempts to add a transition a few times. Once the graph is full, exceptions are thrown. */
	@Test
	public void testAddTransition2()
	{
		Configuration conf = config.copy();conf.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		LearnerGraphND gr = buildLearnerGraphND("A-a->B", "testAddTransition1",conf,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeBetweenExistingStates();
		mutator.addEdgeBetweenExistingStates();
		for(int i=0;i< 9;++i)
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.addEdgeBetweenExistingStates();
		}},FailureToMutateException.class,"duplicate");
		mutator.addEdgeBetweenExistingStates();
		for(int i=0;i< 100;++i)
			try { mutator.addEdgeBetweenExistingStates();fail("should have thrown FailureToMutateException"); } catch(FailureToMutateException ex) {}

		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
	}
	
	/** Attempts to add a transition to a bigger graph. */
	@Test
	public void testAddTransition3()
	{
		Configuration conf = config.copy();conf.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		LearnerGraphND gr = buildLearnerGraphND("A-a->B-b->B", "testAddTransition3",conf,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeBetweenExistingStates();
		
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(3,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("B", "B", "a");
		//Visualiser.updateFrame(mutator.getMutated(), null);Visualiser.waitForKey();
	}
	
	/** Attempts to add a transition to a bigger graph. */
	@Test
	public void testAddTransition4()
	{
		Configuration conf = config.copy();conf.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		LearnerGraphND gr = buildLearnerGraphND("A-a->B-b->B", "testAddTransition3",conf,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeBetweenExistingStates();
		mutator.addEdgeBetweenExistingStates();
		for(int i=0;i< 2;++i)
			try { mutator.addEdgeBetweenExistingStates();
			fail("should have thrown FailureToMutateException on iter "+i); } catch(FailureToMutateException ex) {}
		mutator.addEdgeBetweenExistingStates();
		//Visualiser.updateFrame(mutator.getMutated(), null);Visualiser.waitForKey();
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(5,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("B", "B", "a");
		checkContainsTransition("B", "B", "b");
		checkContainsTransition("A", "B", "a");
		checkContainsTransition("B", "A", "b");
		checkContainsTransition("B", "A", "a");
	}
	
	/** Attempts to add a transition to a bigger graph. */
	@Test
	public void testAddTransition5()
	{
		Configuration conf = config.copy();conf.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		LearnerGraphND gr = buildLearnerGraphND("A-a->B-b->B", "testAddTransition3",conf,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeBetweenExistingStates();
		mutator.addEdgeBetweenExistingStates();
		for(int i=0;i< 2;++i)
			try { mutator.addEdgeBetweenExistingStates();
			fail("should have thrown FailureToMutateException on iter "+i); } catch(FailureToMutateException ex) {}
		mutator.addEdgeBetweenExistingStates();
		for(int i=0;i< 5;++i)
			try { mutator.addEdgeBetweenExistingStates();
			fail("should have thrown FailureToMutateException on iter "+i); } catch(FailureToMutateException ex) {}
		mutator.addEdgeBetweenExistingStates();
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(6,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("B", "B", "a");
		checkContainsTransition("B", "B", "b");
		checkContainsTransition("A", "B", "a");
		checkContainsTransition("A", "B", "b");
		checkContainsTransition("B", "A", "b");
		checkContainsTransition("B", "A", "a");
	}
	
	/** Attempts to remove a transition once. */
	@Test
	public void testRemoveTransition1()
	{
		LearnerGraphND gr = buildLearnerGraphND("A-a->B", "testAddTransition1",config,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.removeEdge();
		
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(0,mutator.getMutated().pathroutines.countEdges());
	}
	
	/** Attempts to add an edge to an empty graph should fail - there is no alphabet. */
	@Test
	public void testRemoveAndThenAddTransition1()
	{
		LearnerGraphND gr = buildLearnerGraphND("A-a->B", "testAddTransition1",config,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.removeEdge();
		for(int i=0;i< 100;++i)
			try { mutator.addEdgeBetweenExistingStates();fail("should have thrown FailureToMutateException"); } catch(FailureToMutateException ex) {}
		
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(0,mutator.getMutated().pathroutines.countEdges());
	}
	
	/** Attempts to remove a transition repeatedly. */
	@Test
	public void testRemoveTransition2()
	{
		LearnerGraphND gr = buildLearnerGraphND("A-a->B", "testAddTransition1",config,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.removeEdge();
		for(int i=0;i< 100;++i)
			try { mutator.removeEdge();fail("should have thrown FailureToMutateException"); } catch(FailureToMutateException ex) {}
		
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(0,mutator.getMutated().pathroutines.countEdges());
	}
	
	/** Attempts to remove a transition from a bigger graph. */
	@Test
	public void testRemoveTransition3()
	{
		LearnerGraphND gr = buildLearnerGraphND("A-a->B-b->B / A-b->B", "testRemoveTransition3",config,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.removeEdge();
		
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(2,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("A", "B", "a");
		checkContainsTransition("A", "B", "b");
	}
	
	/** Attempts to remove a transition from a bigger graph. */
	@Test
	public void testRemoveTransition4()
	{
		LearnerGraphND gr = buildLearnerGraphND("A-a->B-b->B / A-b->B", "testRemoveTransition3",config,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.removeEdge();
		mutator.removeEdge();
		
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(1,mutator.getMutated().pathroutines.countEdges());
		//Visualiser.updateFrame(mutator.getMutated(), null);Visualiser.waitForKey();
		checkContainsTransition("A", "B", "b");
	}
	
	public static final int veryHighMutationValue = 1000; 
	
	/** Attempts to remove a state from a graph. */
	@Test
	public void testRemoveStateFail0()
	{
		LearnerGraphND gr = new LearnerGraphND(config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.removeState(veryHighMutationValue);
		}},FailureToMutateException.class,"number of states");

	}
	
	/** Attempts to remove a state from a graph. */
	@Test
	public void testRemoveStateFail1()
	{
		LearnerGraphND gr = buildLearnerGraphND("A-a->B-b->B / A-b->B", "testRemoveTransition3",config,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.removeState(veryHighMutationValue);
		}},FailureToMutateException.class,"number of states");

	}
	
	/** Attempts to remove a state from a graph. */
	@Test
	public void testRemoveStateFail2()
	{
		LearnerGraphND gr = buildLearnerGraphND("A-a->A", "testRemoveStateFail2",config,null);
		mutator = new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.removeState(veryHighMutationValue);
		}},FailureToMutateException.class,"number of states");

	}
	
	/** Attempts to remove state C from a bigger graph. */
	@Test
	public void testRemoveState1()
	{
		LearnerGraphND gr = buildLearnerGraphND("A-a->B-b->B-c->C / A-b->B", "testRemoveTransition3",config,null);
		mutator = new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		Assert.assertEquals(2,mutator.removeState(2));
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		//Visualiser.updateFrame(mutator.getMutated(), null);Visualiser.waitForKey();
		Assert.assertEquals(3,mutator.getMutated().pathroutines.countEdges());
		Assert.assertNull(mutator.getMutated().findVertex(VertexID.parseID("C")) );
		Assert.assertNotNull(mutator.getMutated().findVertex(VertexID.parseID("B")) );
		checkContainsTransition("A", "B", "b");
		checkContainsTransition("A", "B", "a");
		checkContainsTransition("B", "B", "b");
	}
	
	/** Attempts to remove state B from a bigger graph. */
	@Test
	public void testRemoveState2()
	{
		Random r = new Random(3);
		for(int i=0;i<2;++i) r.nextInt();
		LearnerGraphND gr = buildLearnerGraphND("A-a->B-b->B-c->C / A-b->B", "testRemoveTransition3",config,null);
		mutator = new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,r);
		Assert.assertEquals(5,mutator.removeState(5));
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(0,mutator.getMutated().pathroutines.countEdges());
		Assert.assertNull(mutator.getMutated().findVertex(VertexID.parseID("C")));
		Assert.assertNull(mutator.getMutated().findVertex(VertexID.parseID("B")));
	}
	
	/** Attempts to remove state B from a bigger graph where there is not enough mutation budget for it, hence C is chosen. */
	@Test
	public void testRemoveState3()
	{
		Random r = new Random(3);
		for(int i=0;i<2;++i) r.nextInt();
		LearnerGraphND gr = buildLearnerGraphND("A-a->B-b->B-c->C / A-b->B", "testRemoveTransition3",config,null);
		mutator = new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,r);
		Assert.assertEquals(2,mutator.removeState(4));
		Assert.assertNull(mutator.getMutated().findVertex(VertexID.parseID("C")));
		Assert.assertNotNull(mutator.getMutated().findVertex(VertexID.parseID("B")));
	}
	
	/** Attempts to remove a state B from a bigger graph where there is not enough mutation budget for it, hence C is chosen. */
	@Test
	public void testRemoveStateFail3()
	{
		Random r = new Random(3);
		for(int i=0;i<2;++i) r.nextInt();
		LearnerGraphND gr = buildLearnerGraphND("A-a->B-b->B-c->C / A-b->B", "testRemoveTransition3",config,null);
		mutator = new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,r);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.removeState(1);
		}},FailureToMutateException.class,"is empty");
	}
	
	/** Here we add a transition, then attempt to remove a state and check that it fails. */
	@Test
	public void testRemoveStateFail4()
	{
		LearnerGraphND gr = buildLearnerGraphND("A-a->A", "testAddEdgeToNewState1",config,null);
		mutator = new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		Assert.assertEquals(2,mutator.addEdgeToNewState(2));
		Assert.assertEquals(2,mutator.addEdgeToNewState(2));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.removeState(veryHighMutationValue);
		}},FailureToMutateException.class,"mutation is too similar");
	}
	
	/** Here we add a transition, then attempt to remove a state and check that it fails. */
	@Test
	public void testRemoveStateFail5()
	{
		LearnerGraphND gr = buildLearnerGraphND("A-a->A", "testAddEdgeToNewState1",config,null);
		mutator = new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		Assert.assertEquals(2,mutator.addEdgeToNewState(2));
		Assert.assertEquals(2,mutator.addEdgeToNewState(2));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.removeState(veryHighMutationValue);
		}},FailureToMutateException.class,"mutation is too similar");
	}

	/** Attempts add a transition to a new state. */
	@Test
	public void testAddEdgeToNewStateFailure1()
	{
		LearnerGraphND gr = new LearnerGraphND(config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.addEdgeToNewState(veryHighMutationValue);
		}},FailureToMutateException.class,"is empty");
	}

	@Test
	public void testAddEdgeToNewStateFailure2()
	{
		LearnerGraphND gr = buildLearnerGraphND("A-a->A", "testAddEdgeToNewState1",config,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.addEdgeToNewState(1);
		}},FailureToMutateException.class,"only one is available");
	}
	
	@Test
	public void testAddEdgeToNewState1()
	{
		LearnerGraphND gr = buildLearnerGraphND("A-a->A", "testAddEdgeToNewState1",config,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		Assert.assertEquals(2,mutator.addEdgeToNewState(2));
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		
		Assert.assertEquals(2,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("A", "added_0", "a");
	}

	@Test
	public void testAddEdgeToNewState2()
	{
		Configuration conf = config.copy();conf.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		LearnerGraphND gr = buildLearnerGraphND("A-a->B-b->B-c->C / A-b->B", "testRemoveTransition3",conf,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		Assert.assertEquals(2,mutator.addEdgeToNewState(2));
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		
		Assert.assertEquals(5,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("C", "added_0", "c");
		//Visualiser.updateFrame(mutator.getMutated(), null);Visualiser.waitForKey();
	}

	@Test
	public void testAddEdgeToNewState3()
	{
		Configuration conf = config.copy();conf.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		LearnerGraphND gr = buildLearnerGraphND("A-a->B-b->B-c->C / A-b->B", "testRemoveTransition3",conf,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		Assert.assertEquals(2,mutator.addEdgeToNewState(2));
		Assert.assertEquals(2,mutator.addEdgeToNewState(2));
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		
		Assert.assertEquals(6,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("C", "added_0", "c");
		checkContainsTransition("A", "added_1", "b");
	}

	/** Here we add a transition, then attempt to remove it and check that it fails. */
	@Test
	public void testAddEdgeToNewState4()
	{
		LearnerGraphND gr = buildLearnerGraphND("A-a->A", "testAddEdgeToNewState1",config,null);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		Assert.assertEquals(2,mutator.addEdgeToNewState(2));
		mutator.removeEdge();// happens to remove A-a->A
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.removeEdge();
		}},FailureToMutateException.class,"mutation is too similar");
		
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		
		Assert.assertEquals(1,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("A", "added_0", "a");
	}
	
}
