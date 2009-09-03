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
import static statechum.analysis.learning.rpnicore.FsmParser.buildGraph;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

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
	
	/** Given a test configuration, returns a textual description of its purpose. 
	 * 
	 * @param config configuration to consider
	 * @return description.
	 */ 
	public static String parametersToString(Configuration config)
	{
		return Configuration.parametersToString(config);
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
		config = mainConfiguration.copy();
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
		assertEquals(ExpectedResult, fsm.paths.tracePathPrefixClosed(Arrays.asList(path)));
		CmpVertex expected = (enteredName == null)? null:new LearnerGraph(g, conf).findVertex(enteredName);
		Vertex receivedA = Test_Orig_RPNIBlueFringeLearner.getVertex(g, Arrays.asList(path));
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
	 * @param rejectStates states which are supposed to be marked reject (this method was written before I had support for
	 * reject-transitions interpretation in string representation of graphs)
	 * @param path a sequence of labels to follow
	 * @param ExpectedResult the result to check
	 * @param The name of the vertex which is expected to be returned by getVertex
	 * @param config the configuration to pass to LearnerGraph
	 */
	public void checkPathFrom(String fsmString, List<String> rejectStates, String startingState,String []path, 
			int ExpectedResult, String enteredName, Configuration conf)
	{
		assert (enteredName == null) == (ExpectedResult >= 0);
		final DirectedSparseGraph g = buildGraph(fsmString, "sample FSM");
		final LearnerGraph fsm = new LearnerGraph(g,config);
		if (rejectStates != null) 
			for(String reject:rejectStates)
				fsm.findVertex(VertexID.parseID(reject)).setAccept(false);
		
		assertEquals(ExpectedResult, fsm.paths.tracePath(Arrays.asList(path),fsm.findVertex(startingState), conf.isPrefixClosed()));
		Vertex starting = DeterministicDirectedSparseGraph.findVertexNamed(new VertexID(startingState),g);
		CmpVertex expected = (enteredName == null)? null:new LearnerGraph(g, conf).findVertex(new VertexID(enteredName));
		Vertex received = Test_Orig_RPNIBlueFringeLearner.getVertex(g, starting, Arrays.asList(path));

		if (expected != null)
		{
			assertEquals(expected.getID(),received.getUserDatum(JUConstants.LABEL));
			assertEquals(expected.isAccept(),DeterministicDirectedSparseGraph.isAccept(received));
		}
	}

	@Test
	public void testTraceEmptyPath1()
	{
		checkPathFrom("A-a->B-b->C-c->D", Arrays.asList(new String[]{}), "A", 
				new String[]{}, AbstractOracle.USER_ACCEPTED,"A",config);
	}

	@Test
	public void testTraceEmptyPath2()
	{
		checkPathFrom("A-a->B-b->C-c->D", Arrays.asList(new String[]{"A"}), "A", 
				new String[]{}, 0,null,config);
	}

	@Test
	public void testTraceEmptyPath3()
	{
		checkPathFrom("A-a->B-b->C-c->D", Arrays.asList(new String[]{"B","C","D"}), "A", 
				new String[]{}, AbstractOracle.USER_ACCEPTED,"A",config);
	}

	@Test
	public void testTracePath1a()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a"}, AbstractOracle.USER_ACCEPTED,"B",config);
	}

	@Test
	public void testTracePath1b()
	{
		checkPathFrom("A-a->B-b->C-c->D",null,"B",new String[]{"b"},AbstractOracle.USER_ACCEPTED,"C",config);
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
		checkPathFrom("A-a->B-b->C-c->D", null, "B",new String[]{"c"},0,null,config);
	}
	
	@Test
	public void testTracePath5c()
	{
		checkPathFrom("A-a->B-b->C-c->D", null, "Q",new String[]{"c"},0,null,config);
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
		checkPath("A-a->B-b->C-c-#D", new String[]{"a","b","c","d"}, 2,null,config);
	}
	
	@Test
	public void testTracePath9()
	{
		checkPath("A-a->B-b->C-c-#D", new String[]{"a","b","c","d","e"}, 2,null,config);
	}
	
	@Test
	public void testTracePath10()
	{
		checkPathFrom("A-a->B-b->C-c->D", Arrays.asList(new String[]{"B","C","D"}),"A",
				new String[]{"a","b","c","d","e"}, 0,null,config);
	}
	
	@Test
	public void testTracePath11a()
	{
		config.setPrefixClosed(true);
		checkPathFrom("A-a->B-b->C-c->D-a->D", Arrays.asList(new String[]{"B","C"}),"A",
				new String[]{"a","b","c","a","a"}, 0,null,config);
	}
	
	@Test
	public void testTracePath11b()
	{
		config.setPrefixClosed(false);
		checkPathFrom("A-a->B-b->C-c->D-a->D", Arrays.asList(new String[]{"B","C"}),"A",
				new String[]{"a","b","c","a","a"}, AbstractOracle.USER_ACCEPTED,"D",config);
	}
	
	@Test
	public void testTracePath12a()
	{
		config.setPrefixClosed(true);
		checkPathFrom("A-a->B-b->C-c->D-a->D", Arrays.asList(new String[]{"B","C","D"}),"A",
				new String[]{"a","b","c","a","a"}, 0,null,config);
	}
	
	@Test
	public void testTracePath12b()
	{
		config.setPrefixClosed(false);
		checkPathFrom("A-a->B-b->C-c->D-a->D", Arrays.asList(new String[]{"B","C","D"}),"A",
				new String[]{"a","b","c","a","a"}, 4,null,config);
	}
	
	@Test
	public void testTracePath13a()
	{
		config.setPrefixClosed(true);
		checkPathFrom("A-a->B-b->C-c->D-a->D-b-#E", Arrays.asList(new String[]{"B","C"}),"A",
				new String[]{"a","b","c","a","a","b"}, 0,null,config);
	}
	
	@Test
	public void testTracePath13b()
	{
		config.setPrefixClosed(false);
		checkPathFrom("A-a->B-b->C-c->D-a->D-b-#E", Arrays.asList(new String[]{"B","C"}),"A",
				new String[]{"a","b","c","a","a","b"},5,null,config);
	}
	
	@Test
	public void testTracePath14a()
	{
		config.setPrefixClosed(true);
		checkPathFrom("A-a->B-b->C-c->D-a->D-b-#E", Arrays.asList(new String[]{"C"}),"A",
				new String[]{"a","b","c","a","a","c"}, 1,null,config);
	}
	
	@Test
	public void testTracePath14b()
	{
		config.setPrefixClosed(false);
		checkPathFrom("A-a->B-b->C-c->D-a->D-b-#E", Arrays.asList(new String[]{"C"}),"A",
				new String[]{"a","b","c","a","a","c"}, 5,null,config);
	}
	
}
