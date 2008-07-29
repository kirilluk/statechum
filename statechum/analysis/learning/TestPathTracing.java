/*
 * Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * StateChum. If not, see <http://www.gnu.org/licenses/>.
 */ 
package statechum.analysis.learning;

import static org.junit.Assert.assertEquals;
import static statechum.analysis.learning.TestFSMAlgo.buildGraph;

import java.util.Arrays;
import java.util.Collection;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

@RunWith(Parameterized.class)
public class TestPathTracing {
	@Parameters
	public static Collection<Object[]> data() 
	{
		return Configuration.configurationsForTesting();
	}
	
	public TestPathTracing(Configuration conf)
	{
		mainConfiguration = conf;
	}

	/** Make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	@Before
	public void beforeTest()
	{
		config = (Configuration)mainConfiguration.clone();
		LearnerGraph.testMode=true;
	}

	/** The configuration to use when running tests. */
	private Configuration config = null, mainConfiguration = null;
	
	/** Given an FSM and a sequence of labels to follow, this one checks whether the sequence is correctly
	 * accepted or not, and if not whether it is rejected at the correct element.
	 * 
	 * @param fsmString a description of an FSM
	 * @param path a sequence of labels to follow
	 * @param ExpectedResult the result to check
	 * @param The name of the vertex which is expected to be returned by getVertex
	 * @param config the configuration to pass to LearnerGraph
	 */
	public void checkPath(String fsmString, String []path, int ExpectedResult, String enteredName, Configuration conf)
	{
		assert (enteredName == null)? (ExpectedResult >= 0):true;
		final DirectedSparseGraph g = buildGraph(fsmString, "sample FSM");
		final LearnerGraph fsm = new LearnerGraph(g,config);
		assertEquals(ExpectedResult, fsm.paths.tracePath(Arrays.asList(path)));
		CmpVertex expected = (enteredName == null)? null:new LearnerGraph(g, conf).findVertex(enteredName);
		Vertex receivedA = RPNIBlueFringeLearnerOrig.getVertex(g, Arrays.asList(path));
		CmpVertex receivedB = new LearnerGraph(g,conf).paths.getVertex(Arrays.asList(path));
		if (expected == null)
		{
			Assert.assertNull(receivedA);Assert.assertNull(receivedB);
		}
		else
		{
			assertEquals(expected.getID(),receivedA.getUserDatum(JUConstants.LABEL));
			assertEquals(expected.getID(),receivedB.getID());
			assertEquals(expected.isAccept(),DeterministicDirectedSparseGraph.isAccept(receivedA));
			assertEquals(expected.isAccept(),receivedB.isAccept());
		}
	}
	
	/** Given an FSM and a sequence of labels to follow, this one checks whether the sequence is correctly
	 * accepted from a supplied state or not, and if not whether it is rejected at the correct element.
	 * 
	 * @param fsmString a description of an FSM
	 * @param path a sequence of labels to follow
	 * @param ExpectedResult the result to check
	 * @param The name of the vertex which is expected to be returned by getVertex
	 * @param config the configuration to pass to LearnerGraph
	 */
	public void checkPathFrom(String fsmString, String startingState, String []path, int ExpectedResult, String enteredName, Configuration conf)
	{
		assert (enteredName == null) == (ExpectedResult >= 0);
		final DirectedSparseGraph g = buildGraph(fsmString, "sample FSM");
		final LearnerGraph fsm = new LearnerGraph(g,config);
		assertEquals(ExpectedResult, fsm.paths.tracePath(Arrays.asList(path),fsm.findVertex(startingState)));
		Vertex starting = DeterministicDirectedSparseGraph.findVertexNamed(new VertexID(startingState),g);
		CmpVertex expected = (enteredName == null)? null:new LearnerGraph(g, conf).findVertex(new VertexID(enteredName));
		Vertex received = RPNIBlueFringeLearnerOrig.getVertex(g, starting, Arrays.asList(path));
		if (expected == null)
			Assert.assertNull(received);
		else
		{
			assertEquals(expected.getID(),received.getUserDatum(JUConstants.LABEL));
			assertEquals(expected.isAccept(),DeterministicDirectedSparseGraph.isAccept(received));
		}
	}
	
	@Test
	public void testTracePath()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{}, AbstractOracle.USER_ACCEPTED,"A",config);
	}
	
	@Test
	public void testTracePath1a()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a"}, AbstractOracle.USER_ACCEPTED,"B",config);
	}
	
	@Test
	public void testTracePath1b()
	{
		checkPathFrom("A-a->B-b->C-c->D","B",new String[]{"b"},AbstractOracle.USER_ACCEPTED,"C",config);
	}
	
	@Test
	public void testTracePath2a()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a","b","c"}, AbstractOracle.USER_ACCEPTED,"D",config);
	}
	
	@Test
	public void testTracePath2b()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a","b","c","d"}, 3,null,config);
	}
	
	@Test
	public void testTracePath3()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"b"}, 0,null,config);
	}
	
	@Test
	public void testTracePath4()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a","b","d"}, 2,null,config);
	}
	
	@Test
	public void testTracePath5a()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"b","a","c"}, 0,null,config);
	}
	
	@Test
	public void testTracePath5b()
	{
		checkPathFrom("A-a->B-b->C-c->D", "B",new String[]{"c"},0,null,config);
	}
	
	@Test
	public void testTracePath5c()
	{
		checkPathFrom("A-a->B-b->C-c->D", "Q",new String[]{"c"},0,null,config);
	}
	
	@Test
	public void testTracePath6()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a","a","c","b"}, 1,null,config);
	}
	
	@Test
	public void testTracePath7()
	{
		checkPath("A-a->B-b->C-c-#D", new String[]{"a","b","c"}, 2,"D",config);
	}
	
	@Test
	public void testTracePath8()
	{
		checkPath("A-a->B-b->C-c-#D", new String[]{"a","b","c","d"}, 3,null,config);
	}
	
	@Test
	public void testTracePath9()
	{
		checkPath("A-a->B-b->C-c-#D", new String[]{"a","b","c","d","e"}, 3,null,config);
	}
}
