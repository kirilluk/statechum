package statechum.analysis.learning.experiments;

import static org.junit.Assert.*;
import static statechum.Helper.checkForCorrectException;

import java.util.List;
import java.util.Map;
import java.util.Random;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.GraphMutator.FailureToMutateException;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;

public class TestGraphMutator {

	Configuration config = Configuration.getDefaultConfiguration();
	GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> mutator = null;
	
	void checkContainsTransition(String from, String to, String label)
	{
		LearnerGraphND mut = (LearnerGraphND)mutator.getMutated();Map<CmpVertex,Map<String,List<CmpVertex>>> matrix = mut.getTransitionMatrix();
		Assert.assertTrue(matrix.get(mut.findVertex(VertexID.parseID(from))).get(label).contains(mut.findVertex(VertexID.parseID(to))));
	}
	
	/** Attempts to add a transition once. */
	@Test
	public void testAddTransition1()
	{
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B", "testAddTransition1"),config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeBetweenExistingStates();
		
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(2,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("B", "B", "a");
	}
	
	/** Attempts to add a transition twice, all other attempts should throw. */
	@Test
	public void testAddTransition2()
	{
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B", "testAddTransition1"),config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeBetweenExistingStates();
		
		for(int i=0;i< 100;++i)
			try { mutator.addEdgeBetweenExistingStates();fail("should have thrown FailureToMutateException"); } catch(FailureToMutateException ex) {}
		try { mutator.addEdgeBetweenExistingStates();fail("should have thrown FailureToMutateException"); } catch(FailureToMutateException ex) {}

		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
	}
	
	/** Attempts to add a transition to a bigger graph. */
	@Test
	public void testAddTransition3()
	{
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B-b->B", "testAddTransition3"),config);
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
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B-b->B", "testAddTransition3"),config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeBetweenExistingStates();
		for(int i=0;i< 4;++i)
			try { mutator.addEdgeBetweenExistingStates();
			fail("should have thrown FailureToMutateException on iter "+i); } catch(FailureToMutateException ex) {}
		mutator.addEdgeBetweenExistingStates();
		//Visualiser.updateFrame(mutator.getMutated(), null);Visualiser.waitForKey();
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(4,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("B", "B", "a");
		checkContainsTransition("B", "B", "b");
		checkContainsTransition("A", "B", "a");
		checkContainsTransition("A", "B", "b");
	}
	
	/** Attempts to add a transition to a bigger graph. */
	@Test
	public void testAddTransition5()
	{
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B-b->B", "testAddTransition3"),config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeBetweenExistingStates();
		for(int i=0;i< 4;++i)
			try { mutator.addEdgeBetweenExistingStates();
			fail("should have thrown FailureToMutateException on iter "+i); } catch(FailureToMutateException ex) {}
		mutator.addEdgeBetweenExistingStates();
		
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(4,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("B", "B", "a");
		checkContainsTransition("A", "B", "b");
	}
	
	/** Attempts to remove a transition once. */
	@Test
	public void testRemoveTransition1()
	{
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B", "testAddTransition1"),config);
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
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B", "testAddTransition1"),config);
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
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B", "testAddTransition1"),config);
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
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B-b->B / A-b->B", "testRemoveTransition3"),config);
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
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B-b->B / A-b->B", "testRemoveTransition3"),config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.removeEdge();
		mutator.removeEdge();
		
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(1,mutator.getMutated().pathroutines.countEdges());
		//Visualiser.updateFrame(mutator.getMutated(), null);Visualiser.waitForKey();
		checkContainsTransition("A", "B", "b");
	}
	
	/** Attempts to remove a state from a graph. */
	@Test
	public void testRemoveStateFail0()
	{
		LearnerGraphND gr = new LearnerGraphND(config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.removeState();
		}},FailureToMutateException.class,"number of states");

	}
	
	/** Attempts to remove a state from a graph. */
	@Test
	public void testRemoveStateFail1()
	{
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B-b->B / A-b->B", "testRemoveTransition3"),config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.removeState();
		}},FailureToMutateException.class,"number of states");

	}
	
	/** Attempts to remove a state from a graph. */
	@Test
	public void testRemoveStateFail2()
	{
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->A", "testRemoveStateFail2"),config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.removeState();
		}},FailureToMutateException.class,"number of states");

	}
	
	/** Attempts to remove state C from a bigger graph. */
	@Test
	public void testRemoveState1()
	{
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B-b->B-c->C / A-b->B", "testRemoveTransition3"),config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.removeState();
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		//Visualiser.updateFrame(mutator.getMutated(), null);Visualiser.waitForKey();
		Assert.assertEquals(3,mutator.getMutated().pathroutines.countEdges());
		Assert.assertNull(mutator.getMutated().findVertex(VertexID.parseID("C")) );
		Assert.assertNotNull(mutator.getMutated().findVertex(VertexID.parseID("B")) );
		checkContainsTransition("A", "B", "b");
		checkContainsTransition("A", "B", "a");
		checkContainsTransition("B", "B", "b");
	}
	
	/** Attempts to state B from a bigger graph. */
	@Test
	public void testRemoveState2()
	{
		Random r = new Random(3);
		for(int i=0;i<2;++i) r.nextInt();
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B-b->B-c->C / A-b->B", "testRemoveTransition3"),config);
			mutator = 
				new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,r);
		mutator.removeState();
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		Assert.assertEquals(0,mutator.getMutated().pathroutines.countEdges());
		Assert.assertNull(mutator.getMutated().findVertex(VertexID.parseID("C")));
		Assert.assertNull(mutator.getMutated().findVertex(VertexID.parseID("B")));
	}
	
	/** Here we add a transition, then attempt to remove a state and check that it fails. */
	@Test
	public void testRemoveStateFail()
	{
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->A", "testAddEdgeToNewState1"),config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeToNewState();mutator.addEdgeToNewState();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.removeState();
		}},FailureToMutateException.class,"newly-added");
	}
	
	/** Attempts add a transition to a new state. */
	@Test
	public void testAddEdgeToNewStateFailure1()
	{
		LearnerGraphND gr = new LearnerGraphND(config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.addEdgeToNewState();
		}},FailureToMutateException.class,"empty");
	}
	
	@Test
	public void testAddEdgeToNewState1()
	{
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->A", "testAddEdgeToNewState1"),config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeToNewState();
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		
		Assert.assertEquals(2,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("A", "added_0", "a");
	}
	
	@Test
	public void testAddEdgeToNewState2()
	{
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B-b->B-c->C / A-b->B", "testRemoveTransition3"),config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeToNewState();
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		
		Assert.assertEquals(5,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("C", "added_0", "c");
		//Visualiser.updateFrame(mutator.getMutated(), null);Visualiser.waitForKey();
	}
	
	@Test
	public void testAddEdgeToNewState3()
	{
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->B-b->B-c->C / A-b->B", "testRemoveTransition3"),config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeToNewState();
		mutator.addEdgeToNewState();
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		
		Assert.assertEquals(6,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("C", "added_0", "c");
		checkContainsTransition("A", "added_1", "b");
	}
	
	/** Here we add a transition, then attempt to remove it and check that it fails. */
	@Test
	public void testAddEdgeToNewState4()
	{
		LearnerGraphND gr = new LearnerGraphND(FsmParser.buildGraph("A-a->A", "testAddEdgeToNewState1"),config);
		mutator = 
			new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(gr,new Random(3));
		mutator.addEdgeToNewState();
		mutator.removeEdge();// happens to remove A-a->A
		checkForCorrectException(new whatToRun() { public @Override void run() {
			mutator.removeEdge();
		}},FailureToMutateException.class,"newly-added");
		
		mutator.getMutated().pathroutines.checkConsistency(mutator.getMutated());
		
		Assert.assertEquals(1,mutator.getMutated().pathroutines.countEdges());
		checkContainsTransition("A", "added_0", "a");
	}
	
}
