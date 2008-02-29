/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.analysis.learning;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import static statechum.xmachine.model.testset.WMethod.getGraphData;

import junit.framework.Assert;

import org.junit.AfterClass;
import org.junit.Test;
import org.junit.BeforeClass;

import statechum.ArrayOperations;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.DeterministicEdge;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.RPNIBlueFringeLearner.OrigStatePair;
import statechum.xmachine.model.testset.WMethod;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;
import static statechum.analysis.learning.Visualiser.isGraphTransformationDebug;

public class TestFSMAlgo {

	static protected StatePair constructPair(String a,String b)
	{
		DeterministicVertex aV = new DeterministicVertex(a), bV = new DeterministicVertex(b);
		return new StatePair(aV,bV);
	}

	static protected OrigStatePair constructOrigPair(String a,String b)
	{
		DirectedSparseVertex aV = new DirectedSparseVertex(), bV = new DirectedSparseVertex();
		aV.addUserDatum(JUConstants.LABEL, a, UserData.SHARED);
		bV.addUserDatum(JUConstants.LABEL, b, UserData.SHARED);
		return new OrigStatePair(aV,bV);
	}

	@SuppressWarnings("unchecked")
	static private void checkLessHelper(Comparable p, Comparable q)
	{
		assertFalse(p.equals(q));
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertFalse(p.hashCode() == q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}
	
	static protected void checkLess(String a, String b, String c, String d)
	{
		checkLessHelper(constructPair(a,b),constructPair(c,d));
		checkLessHelper(constructOrigPair(a,b),constructOrigPair(c,d));
	}
	
	@SuppressWarnings("unchecked")
	static private void PairEqualityTestingHelper(Comparable p, Comparable q)
	{
		assertTrue(p.equals(p));
		assertTrue(p.equals(q));
		assertFalse(p.equals(null));
		assertFalse(p.equals("test"));
		assertFalse(p.equals(constructPair("a","c")));
		assertFalse(p.equals(constructPair("b","b")));
	}
	
	@Test
	public void testStatePairEquality()
	{
		PairEqualityTestingHelper(constructPair("a","b"), constructPair("a","b"));
		PairEqualityTestingHelper(constructOrigPair("a","b"), constructOrigPair("a","b"));
	}
	
	@Test
	public void testStatePairComparison()
	{
		checkLess("a","b","c","d");
		checkLess("a","b","a","c");
		checkLess("a","b","c","b");
	}
	
	/** Given a textual representation of an fsm, builds a corresponding Jung graph
	 * 
	 * @param fsm the textual representation of an FSM
	 * @param name graph name, to be displayed as the caption of the Jung window.
	 * @return Jung graph for it
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public static DirectedSparseGraph buildGraph(String fsm,String name)
	{
		final Map<String,DeterministicVertex> existingVertices = new HashMap<String,DeterministicVertex>();
		final Map<StatePair,DeterministicEdge> existingEdges = new HashMap<StatePair,DeterministicEdge>();
		
		final DirectedSparseGraph g = new DirectedSparseGraph();
		g.setUserDatum(JUConstants.TITLE, name,UserData.SHARED);

		new TestFSMParser.fsmParser(fsm).parse(new TestFSMParser.TransitionReceiver()
		{
			public void put(String from, String to, String label, boolean accept) {
				DeterministicVertex fromVertex = existingVertices.get(from), toVertex = existingVertices.get(to);
				
				if (fromVertex == null)
				{
					fromVertex = new DeterministicDirectedSparseGraph.DeterministicVertex(from);
					if (existingVertices.isEmpty())
						fromVertex.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
					fromVertex.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
					existingVertices.put(from, fromVertex);
					g.addVertex(fromVertex);
				}
				else
					if (!Boolean.valueOf(fromVertex.getUserDatum(JUConstants.ACCEPTED).toString()))
						throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+from);

				if (from.equals(to))
				{
					if (!accept) throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+to);
					toVertex = fromVertex;
				}
				else
					if (toVertex == null)
					{
						toVertex = new DeterministicDirectedSparseGraph.DeterministicVertex(to);
						toVertex.removeUserDatum(JUConstants.ACCEPTED); // in case we've got a reject loop in the same state
						toVertex.addUserDatum(JUConstants.ACCEPTED, accept, UserData.SHARED);
						existingVertices.put(to, toVertex);
						g.addVertex(toVertex);
					}
					else
						if (RPNIBlueFringeLearner.isAccept(toVertex) != accept)
							throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+to);
				
				StatePair pair = new StatePair(fromVertex,toVertex);
				DeterministicEdge edge = existingEdges.get(pair);
				if (edge == null)
				{
					edge = new DeterministicDirectedSparseGraph.DeterministicEdge(fromVertex,toVertex);
					edge.addUserDatum(JUConstants.LABEL, new HashSet<String>(), UserData.CLONE);
					g.addEdge(edge);existingEdges.put(pair,edge);
				}
				
				Set<String> labels = (Set<String>)edge.getUserDatum(JUConstants.LABEL);
				labels.add(label);
			}

			public void accept(String from, String to, String label) {
				put(from,to,label,true);
			}
			public void reject(String from, String to, String label) {
				put(from,to,label,false);
			}
		});
		
		if (isGraphTransformationDebug(g))
		{
			Visualiser.updateFrame(g, null);System.out.println("******** PROCESSING "+name+" **********\n");
		}
		return g;
	}

	/** This data store represents an FSM and is used by tests. */
	public static class FSMStructure
	{
		/** The transition transition diagram, in which every state is mapped to a map between an input (label) and a target state. */
		public final Map<String,Map<String,String>> trans;
		
		/** All states of the machine should be in the domain of this function; 
		 * for a given state, this function will return <pre>true</pre> if it is an accept state and <pre>false</pre> for a reject one.
		 */ 
		public final Map<String,Boolean> accept;// TODO: to test consistency of this one against elements of CmpVertex once strings have been replaced with cmpvertices
		
		/** The initial state. */
		public String init;
		
		public FSMStructure(Map<String,Map<String,String>> transitions,Map<String,Boolean> a,String initState)
		{
			trans = transitions;accept = a;init = initState;
		}
		
		public FSMStructure()
		{
			trans = new TreeMap<String,Map<String,String>>();accept = new TreeMap<String,Boolean>();
		}
		
		public boolean equals(Object o)
		{
			if (this == o)
				return true;
			if (o == null || !(o instanceof FSMStructure))
				return false;
			
			FSMStructure otherStruct= (FSMStructure)o;
			return 
				accept.equals(otherStruct.accept) &&
				trans.equals(otherStruct.trans) &&
				init.equals(otherStruct.init);		
		}
		
	}
		
	/** Checks if the passed graph is isomorphic to the provided fsm
	 * 
	 * @param g graph to check
	 * @param fsm the string representation of the machine which the graph should be isomorphic to
	 */
	public void checkEq(DirectedSparseGraph g, String fsm)
	{
		DirectedSparseGraph expectedGraph = buildGraph(fsm,"expected graph");
		final FSMStructure graph = getGraphData(g);
		final FSMStructure expected = getGraphData(expectedGraph);
		assertEquals("incorrect initial state",expected.init, graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,expected.trans.equals(graph.trans));		
	}

	@Test
	public void testCheckEq()
	{
		DirectedSparseGraph g=buildGraph("P-a->Q_State-b->P-c->P","testCheckEq");
		checkEq(g,"P-c->P<-b-Q_State<-a-P");
	}
	
	/** This one is used to indicate that a two machines are not accepting the same language - 
	 * I need to check that it is the incompatibility exception thrown by the <i>checkM</i> 
	 * method and not any other <i>IllegalArgumentException</i>.
	 */
	public static class DifferentFSMException extends IllegalArgumentException 
	{
		/**
		 *  Serialization ID.
		 */
		private static final long serialVersionUID = 6126662147586264877L;

		public DifferentFSMException(String arg)
		{
			super(arg);
		}
	}
	
	
	public static void checkM(DirectedSparseGraph g,String fsm)
	{
		final FSMStructure graph = getGraphData(g);
		final DirectedSparseGraph expectedGraph = buildGraph(fsm,"expected graph");
		final FSMStructure expected = getGraphData(expectedGraph);
		checkM(graph,expected,graph.init,expected.init);
	}
	
	public static class StringPair extends Pair<String,String> implements Comparable<StringPair>
	{		
		public StringPair(String aStr,String bStr)
		{
			super(aStr,bStr);
		}

		@Override
		public String toString() {
			return "( "+firstElem+","+secondElem+" )";
		}

		public int compareTo(StringPair pB) {
			int aStr = firstElem.compareTo(pB.firstElem);
			int bStr = secondElem.compareTo(pB.secondElem);
			
			if(aStr != 0)
				return aStr; 
			return bStr;
		}
	}
	
	static protected void checkStatePairLess(String a, String b, String c, String d)
	{
		StringPair p = new StringPair(a,b), q=new StringPair(c,d);
		assertFalse(p.equals(q));
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertFalse(p.hashCode() == q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}
	
	@Test
	public void testStringPairEquality()
	{
		StringPair p = new StringPair("a","b"), q=new StringPair("a","b");
		assertTrue(p.equals(p));
		assertTrue(p.equals(q));
		assertFalse(p.equals(null));
		assertFalse(p.equals("test"));
		assertFalse(p.equals(new StringPair("a","c")));
		assertFalse(p.equals(new StringPair("b","b")));
		
		assertTrue(p.hashCode() != 0);
		assertTrue(q.hashCode() != 0);
	}
	
	@Test
	public void testStringPairComparison()
	{
		checkStatePairLess("a","b","c","d");
		checkStatePairLess("a","b","a","c");
		checkStatePairLess("a","b","c","b");
	}
	
	/** Checks the equivalence between the two states, stateG of graphA and stateB of graphB.
	 * Unreachable states are ignored. 
	 */
	public static void checkM(FSMStructure graph, FSMStructure expected, String stateGraph, String stateExpected)
	{
		Queue<StringPair> currentExplorationBoundary = new LinkedList<StringPair>();// FIFO queue

		Set<StringPair> statesAddedToBoundary = new HashSet<StringPair>();
		currentExplorationBoundary.add(new StringPair(stateGraph,stateExpected));statesAddedToBoundary.add(new StringPair(stateGraph,stateExpected));
		
		while(!currentExplorationBoundary.isEmpty())
		{
			StringPair statePair = currentExplorationBoundary.remove();
			assert graph.accept.containsKey(statePair.firstElem) : "state "+statePair.firstElem+" is not known to the first graph";
			assert expected.accept.containsKey(statePair.secondElem) : "state "+statePair.secondElem+" is not known to the second graph";
			if (!graph.accept.get(statePair.firstElem).equals(expected.accept.get(statePair.secondElem)))
				throw new DifferentFSMException("states "+statePair.firstElem+" and " + statePair.secondElem+" have a different acceptance labelling between the machines");
						
			Map<String,String> targets = graph.trans.get(statePair.firstElem), expectedTargets = expected.trans.get(statePair.secondElem);
			if (expectedTargets.size() != targets.size())// each of them is equal to the keyset size from determinism
				throw new DifferentFSMException("different number of transitions from state "+statePair);
				
			for(Entry<String,String> labelstate:targets.entrySet())
			{
				String label = labelstate.getKey();
				if (!expectedTargets.containsKey(label))
					throw new DifferentFSMException("no transition with expected label "+label+" from a state corresponding to "+statePair.secondElem);
				String tState = labelstate.getValue();// the original one
				String expectedState = expectedTargets.get(label);
				
				StringPair nextPair = new StringPair(tState,expectedState);
				if (!statesAddedToBoundary.contains(nextPair))
				{
					currentExplorationBoundary.offer(nextPair);
					statesAddedToBoundary.add(nextPair);
				}
			}
		}
		
	}
	
	@Test
	public void testCheckM1()
	{
		checkM(buildGraph("A-a->B-b->C", "testCheck1"), "B-a->C-b->D");
	}
	
	@Test
	public void testCheckM2()
	{
		checkM(buildGraph("A-a->B-b->C-d-#F#-b-A", "testCheck2"), "B-a->C-b->D\nB-b-#REJ\nD-d-#REJ");
	}

	@Test
	public void testCheckM3()
	{
		String another  = "A-a->B-b->C\nC-b-#REJ\nA-d-#REJ";
		String expected = "A-a->B-b->C-b-#F#-d-A";
		checkM(buildGraph(another.replace('A', 'Q').replace('B', 'G').replace('C', 'A'), "testCheck3"), expected);
	}

	@Test
	public void testCheckM4() // multiple reject states
	{
		String another  = "A-a->B-b->C\nC-b-#REJ\nA-d-#REJ\nA-b-#REJ2\nB-a-#REJ2\nB-c-#REJ3";
		String expected = "A-a->B-b->C-b-#F#-d-A-b-#R\nB-a-#R\nU#-c-B";
		checkM(buildGraph(another.replace('A', 'Q').replace('B', 'G').replace('C', 'A'), "testCheck4"), expected);
	}

	@Test
	public void testCheckM5()
	{
		checkM(buildGraph("A-a->B-b->B-a->C", "testCheck5"), "S-a->U<-b-U\nQ<-a-U");
	}

	@Test
	public void testCheckM6()
	{
		final FSMStructure graph = getGraphData(buildGraph("A-a->B-b->B-a->C", "testCheck6"));
		final FSMStructure expected = getGraphData(buildGraph("U<-b-U\nQ<-a-U<-a-S","expected graph"));
		checkM(graph,expected,"A","S");
		checkM(graph,expected,"B","U");
		checkM(graph,expected,"C","Q");
	}

	@Test
	public final void testCheckM_multipleEq1() // equivalent states
	{
		final FSMStructure graph = getGraphData(buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckM_multipleEq1"));
		assertTrue(checkMBoolean(graph,graph,"D","C2"));
		assertTrue(checkMBoolean(graph,graph,"C2","D"));
		
		assertTrue(checkMBoolean(graph,graph,"D1","D2"));
		assertTrue(checkMBoolean(graph,graph,"D2","D1"));

		assertTrue(checkMBoolean(graph,graph,"D2","K2"));
		assertTrue(checkMBoolean(graph,graph,"K2","D2"));

		assertFalse(checkMBoolean(graph,graph,"D2","A1"));
		assertFalse(checkMBoolean(graph,graph,"A1","D2"));

		assertFalse(checkMBoolean(graph,graph,"D2","F1"));
		assertFalse(checkMBoolean(graph,graph,"F1","D2"));
	}

	@Test
	public final void testCheckM_multipleEq2() // equivalent states
	{
		final DirectedSparseGraph g = buildGraph("S-a->A-a->D-a->D-b->A-b->B-a->D\nB-b->C-a->D\nC-b->D\nS-b->N-a->N-b->N","testCheckM_multipleEq2");
		final FSMStructure graph = getGraphData(g);
		List<String> states = Arrays.asList(new String[]{"S","A","B","C","D","N"});
		for(String stA:states)
			for(String stB:states)
				assertTrue("states "+stA+"and "+stB+" should be equivalent",checkMBoolean(graph,graph,stA,stB));
	}
	
	@Test
	public final void testCheckM_multipleEq3() // equivalent states
	{
		final DirectedSparseGraph g = buildGraph("S-a->A-a->D-a->D-b->A-b->B-a->D\nB-b->C-a->D\nC-b->D\nS-b->N-a->M-a->N\nN-b->M-b->N","testCheckM_multipleEq3");
		final FSMStructure graph = getGraphData(g);
		List<String> states = Arrays.asList(new String[]{"S","A","B","C","D","N","M"});
		for(String stA:states)
			for(String stB:states)
				assertTrue("states "+stA+"and "+stB+" should be equivalent",checkMBoolean(graph,graph,stA,stB));
	}
	
	@Test
	public final void testCheckM_multipleEq4() // non-equivalent states
	{
		final DirectedSparseGraph g = buildGraph("A-a->B-a->C-a->A-b->C-b->B","testCheckM_multipleEq4");
		final FSMStructure graph = getGraphData(g);
		List<String> states = Arrays.asList(new String[]{"A","B","C"});
		for(String stA:states)
			for(String stB:states)
				if (stA.equals(stB))
					assertTrue("states "+stA+" and "+stB+" should be equivalent",checkMBoolean(graph,graph,stA,stB));
				else
					assertFalse("states "+stA+" and "+stB+" should not be equivalent",checkMBoolean(graph,graph,stA,stB));
	}
	
	/** Same as checkM, but returns a boolean false instead of an exception. */
	public static boolean checkMBoolean(FSMStructure graph, FSMStructure expected, String stateGraph, String stateExpected)
	{
		try
		{
			checkM(graph,expected,stateGraph,stateExpected);
		}
		catch(DifferentFSMException ex)
		{
			return false;
		}
		return true;
	}
	
	@Test
	public void testCheckM6_f1()
	{
		final FSMStructure graph = getGraphData(buildGraph("A-a->B-b->B-a->C", "testCheck6"));
		final FSMStructure expected = getGraphData(buildGraph("U<-b-U\nQ<-a-U<-a-S","expected graph"));
		Assert.assertTrue(checkMBoolean(graph,graph,"A","A"));
		Assert.assertTrue(checkMBoolean(graph,graph,"B","B"));
		Assert.assertTrue(checkMBoolean(graph,graph,"C","C"));
		Assert.assertTrue(checkMBoolean(expected,expected,"Q","Q"));
		Assert.assertTrue(checkMBoolean(expected,expected,"S","S"));
		
		Assert.assertFalse(checkMBoolean(graph,expected,"A","Q"));
		Assert.assertFalse(checkMBoolean(graph,expected,"A","U"));
		Assert.assertFalse(checkMBoolean(graph,expected,"B","Q"));
		Assert.assertFalse(checkMBoolean(graph,expected,"B","S"));
		Assert.assertFalse(checkMBoolean(graph,expected,"C","U"));
		Assert.assertFalse(checkMBoolean(graph,expected,"C","S"));
	}
	

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD1()
	{
		checkM(buildGraph("A-a->B-b->C", "testCheckMD1"), "B-a->C-b->B");		
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD2() // different reject states
	{
		checkM(buildGraph("A-a->B-b->C", "testCheckMD2"), "B-a->C-b-#D");
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD3() // missing transition
	{
		checkM(buildGraph("A-a->B-b->C\nA-b->B", "testCheckMD3"), "B-a->C-b->D");
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD4() // extra transition
	{
		checkM(buildGraph("A-a->B-b->C", "testCheckMD4"), "B-a->C-b->D\nB-b->C");
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD5() // missing transition
	{
		checkM(buildGraph("A-a->B-b->C\nB-c->B", "testCheckMD5"), "B-a->C-b->D");
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD6() // extra transition
	{
		checkM(buildGraph("A-a->B-b->C", "testCheckMD6"), "B-a->C-b->D\nC-c->C");
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD7() // swapped transitions
	{
		String another  = "A-a->B-b->C\nC-b-#REJ\nA-d-#REJ";
		String expected = "A-a->B-b->C-d-#F#-b-A";
		checkM(buildGraph(another.replace('A', 'Q').replace('B', 'G').replace('C', 'A'), "testCheckMD7"), expected);
	}
	
	/** Computes an alphabet of a given graph and adds transitions to a 
	 * reject state from all states A and inputs a from which there is no B such that A-a->B
	 * (A-a-#REJECT) gets added. Note: such transitions are even added to reject vertices.
	 * 
	 * @param g the graph to add transitions to
	 * @param reject the name of the reject state, to be added to the graph.
	 * @return true if any transitions have been added
	 */   
	public static boolean completeGraph(DirectedSparseGraph g, String reject)
	{
		DirectedSparseVertex rejectVertex = new DirectedSparseVertex();
		boolean transitionsToBeAdded = false;// whether and new transitions have to be added.
		rejectVertex.addUserDatum(JUConstants.ACCEPTED, false, UserData.SHARED);
		rejectVertex.addUserDatum(JUConstants.LABEL, reject, UserData.SHARED);
		
		// first pass - computing an alphabet
		Set<String> alphabet = WMethod.computeAlphabet(g);
		
		// second pass - checking if any transitions need to be added.
		Set<String> outLabels = new HashSet<String>();
		Iterator<Vertex> vertexIt = (Iterator<Vertex>)g.getVertices().iterator();
		while(vertexIt.hasNext() && !transitionsToBeAdded)
		{
			Vertex v = vertexIt.next();
			outLabels.clear();
			Iterator<DirectedSparseEdge>outEdgeIt = v.getOutEdges().iterator();
			while(outEdgeIt.hasNext()){
				DirectedSparseEdge outEdge = outEdgeIt.next();
				outLabels.addAll( (Set<String>)outEdge.getUserDatum(JUConstants.LABEL) );
			}
			transitionsToBeAdded = !alphabet.equals(outLabels);
		}
		
		if (transitionsToBeAdded)
		{
			// third pass - adding transitions
			g.addVertex(rejectVertex);
			vertexIt = (Iterator<Vertex>)g.getVertices().iterator();
			while(vertexIt.hasNext())
			{
				Vertex v = vertexIt.next();
				if (v != rejectVertex)
				{// no transitions should start from the reject vertex
					Set<String> outgoingLabels = new TreeSet<String>();outgoingLabels.addAll(alphabet);
					
					Iterator<DirectedSparseEdge>outEdgeIt = v.getOutEdges().iterator();
					while(outEdgeIt.hasNext()){
						DirectedSparseEdge outEdge = outEdgeIt.next();
						outgoingLabels.removeAll( (Set<String>)outEdge.getUserDatum(JUConstants.LABEL) );
					}
					if (!outgoingLabels.isEmpty())
					{
						// add a transition
						DirectedSparseEdge edge = new DirectedSparseEdge(v,rejectVertex);
						edge.addUserDatum(JUConstants.LABEL, outgoingLabels, UserData.CLONE);
						g.addEdge(edge);
					}
				}
			}
		}
		
		return transitionsToBeAdded;
	}

	@Test
	public void completeComputeAlphabet0()
	{
		Set<String> alphabet = WMethod.computeAlphabet(new DirectedSparseGraph());
		Assert.assertTrue(alphabet.isEmpty());
	}

	@Test
	public void completeComputeAlphabet1()
	{
		Set<String> alphabet = WMethod.computeAlphabet(buildGraph("A-a->A", "completeComputeAlphabet1"));
		Set<String> expected = new HashSet<String>();expected.addAll( Arrays.asList(new String[] {"a"}));
		Assert.assertTrue(alphabet.equals(expected));
		DirectedSparseGraph g = buildGraph("A-a->A", "completeGraphTest1");Assert.assertFalse(completeGraph(g,"REJECT"));
	}

	@Test
	public void completeComputeAlphabet2()
	{
		Set<String> alphabet = WMethod.computeAlphabet(buildGraph("A-a->A<-b-A", "completeComputeAlphabet2"));
		Set<String> expected = new HashSet<String>();expected.addAll( Arrays.asList(new String[] {"a","b"}));
		Assert.assertTrue(alphabet.equals(expected));
		DirectedSparseGraph g = buildGraph("A-a->A", "completeGraphTest1");Assert.assertFalse(completeGraph(g,"REJECT"));
	}

	@Test
	public void completeComputeAlphabet3()
	{
		Set<String> alphabet = WMethod.computeAlphabet(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "completeComputeAlphabet3"));
		Set<String> expected = new HashSet<String>();expected.addAll( Arrays.asList(new String[] {"a","b","c","d"}));
		Assert.assertTrue(alphabet.equals(expected));
		DirectedSparseGraph g = buildGraph("A-a->A", "completeGraphTest1");Assert.assertFalse(completeGraph(g,"REJECT"));
	}

	@Test
	public void completeGraphTest1()
	{
		DirectedSparseGraph g = buildGraph("A-a->A", "completeGraphTest1");Assert.assertFalse(completeGraph(g,"REJECT"));
		checkM(g, "A-a->A");		
	}
	
	@Test
	public void completeGraphTest2()
	{
		DirectedSparseGraph g = buildGraph("A-a->B-a->A", "completeGraphTest2");Assert.assertFalse(completeGraph(g,"REJECT"));
		checkM(g, "A-a->A");		
	}
	
	@Test
	public void completeGraphTest3()
	{
		DirectedSparseGraph g = buildGraph("A-a->A<-b-A", "completeGraphTest3");Assert.assertFalse(completeGraph(g,"REJECT"));
		checkM(g, "A-b->A-a->A");		
	}
	
	@Test
	public void completeGraphTest4()
	{
		DirectedSparseGraph g = buildGraph("A-a->B-b->A", "completeGraphTest4");Assert.assertTrue(completeGraph(g,"REJECT"));
		checkM(g, "A-a->B-b->A\nA-b-#REJECT#-a-B");		
	}
	
	@Test
	public void completeGraphTest4b()
	{
		DirectedSparseGraph g = buildGraph("A-a->B-b->A-b->A", "completeGraphTest4b");Assert.assertTrue(completeGraph(g,"REJECT"));
		checkM(g, "A-a->B-b->A-b->A\nREJECT#-a-B");		
	}

	@Test
	public void completeGraphTest5()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B", "completeGraphTest5");Assert.assertTrue(completeGraph(g,"REJECT"));
		checkM(g, "A-a->A-b->B-c->B\nA-c-#REJECT#-a-B-b-#REJECT");		
	}	
	
	@Test
	public void completeGraphTest6()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C", "completeGraphTest6");Assert.assertTrue(completeGraph(g,"REJECT"));
		checkM(g, "A-a->A-b->B-c->B-a->C\nA-c-#REJECT#-b-B\nC-a-#REJECT\nC-b-#REJECT\nC-c-#REJECT");		
	}	
	
	@Test
	public void completeGraphTest7()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "completeGraphTest7");Assert.assertTrue(completeGraph(g,"REJECT"));
		final FSMStructure graph = getGraphData(g);
		final FSMStructure expected = getGraphData(buildGraph("A-a->A-b->B-c->B-a->C\nA-c-#REJECT\nA-d-#REJECT\nB-b-#REJECT\nB-d-#REJECT\nC-a-#REJECT\nC-b-#REJECT\nC-c-#REJECT\nC-d-#REJECT\nS-a-#REJECT\nS-b-#REJECT\nS-c-#REJECT\nS-d-#REJECT\nQ-a-#REJECT\nQ-b-#REJECT\nQ-c-#REJECT\nQ-d->S","expected graph"));
		Assert.assertTrue(checkMBoolean(graph,expected,"A","A"));
		Assert.assertTrue(checkMBoolean(graph,expected,"B","B"));
		Assert.assertTrue(checkMBoolean(graph,expected,"Q","Q"));
		Assert.assertTrue(checkMBoolean(graph,expected,"S","S"));
		Assert.assertTrue(checkMBoolean(graph,expected,"REJECT","REJECT"));
	}	

	@Test(expected = IllegalArgumentException.class)
	public void testFindVertex0()
	{
		RPNIBlueFringeLearner.findVertex(JUConstants.JUNKVERTEX, null, new DirectedSparseGraph());
	}

	@Test
	public void testFindVertex1()
	{
		Assert.assertNull(RPNIBlueFringeLearner.findVertex(JUConstants.JUNKVERTEX, "bb", new DirectedSparseGraph()));
	}
	
	@Test
	public void testFindVertex2()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex2");
		//Visualiser.updateFrame(g, g);Visualiser.waitForKey();
		Assert.assertNull(RPNIBlueFringeLearner.findVertex(JUConstants.JUNKVERTEX, "bb", g));
	}
		
	@Test
	public void testFindVertex3()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex3");
		//Visualiser.updateFrame(g, null);Visualiser.waitForKey();
		Assert.assertNull(RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "D", g));
	}

	@Test
	public void testFindVertex4a()
	{
		Vertex v = RPNIBlueFringeLearner.findVertex(JUConstants.INITIAL, "anything", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex4a"));
		Assert.assertNull(v);
	}

	@Test
	public void testFindVertex4b()
	{
		Vertex v =  RPNIBlueFringeLearner.findVertex(JUConstants.INITIAL, true, buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex4b"));
		Assert.assertEquals("A", v.getUserDatum(JUConstants.LABEL));
	}

	@Test
	public void testFindVertex5()
	{
		Vertex v =  RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "A", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex5"));
		Assert.assertEquals("A", v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public void testFindVertex6()
	{
		Vertex v =  RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "C", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex6"));
		Assert.assertEquals("C", v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public void testFindVertex7()
	{
		Vertex v = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "S", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex7"));
		Assert.assertEquals("S", v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public void testFindVertex8()
	{
		Vertex v = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "Q", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex8"));
		Assert.assertEquals("Q", v.getUserDatum(JUConstants.LABEL));
	}

	
	@Test
	public void testFindInitial1()
	{
		Vertex v = RPNIBlueFringeLearner.findInitial(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"));
		Assert.assertEquals("A", v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public void testFindInitial2()
	{
		Vertex v = RPNIBlueFringeLearner.findInitial(new DirectedSparseGraph());
		Assert.assertNull(v);
	}

	/** Builds a set of sequences from a two-dimensional array, where each element corresponds to a sequence.
	 * 
	 * @param data source data
	 * @return a set of sequences to apply to an RPNI learner
	 */
	public static Set<List<String>> buildSet(String [][] data)
	{
		Set<List<String>> result = new HashSet<List<String>>();
		for(String []seq:data)
		{
			result.add(Arrays.asList(seq));
		}
		return result;
	}
	
	/** Builds a map from an array, where each element corresponds to a pair of a string array 
	 * (representing a sequence) and a string (representing flags associated with this sequence).
	 * 
	 * @param data source data
	 * @return a string->string map
	 */
	public static Map<String,String> buildStringMap(Object [][] data)
	{
		Map<String,String> result = new HashMap<String,String>();
		for(Object[] str:data)
		{
			if (str.length != 2)
				throw new IllegalArgumentException("more than two elements in sequence "+str);
			if (str[0] == null || str[1] == null || !(str[0] instanceof String[]) || !(str[1] instanceof String))
				throw new IllegalArgumentException("invalid data in array");// TODO: to test that this exception is thrown.
			result.put(ArrayOperations.seqToString(Arrays.asList((String[])str[0])),(String)str[1]);
		}
		return result;
	}
	
	/** Builds a set of sequences from a two-dimensional array, where each element corresponds to a sequence.
	 * 
	 * @param data source data
	 * @return a set of sequences to apply to an RPNI learner
	 */
	public static List<List<String>> buildList(String [][] data)
	{
		List<List<String>> result = new LinkedList<List<String>>();result.addAll(buildSet(data));
		return result;
	}
	
	@Test
	public void testBuildSet1()
	{
		assertTrue(buildSet(new String[] []{}).isEmpty());
	}

	@Test
	public void testBuildSet2()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(new LinkedList<String>());
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{}})));
	}

	@Test
	public void testBuildSet3A()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		expectedResult.add(new LinkedList<String>());
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{},new String[]{"a","b","c"}})));
	}

	@Test
	public void testBuildSet3B()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{"a","b","c"}})));
	}

	@Test
	public void testBuildSet4()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		expectedResult.add(new LinkedList<String>());
		expectedResult.add(Arrays.asList(new String[]{"g","t"}));
		expectedResult.add(Arrays.asList(new String[]{"h","q","i"}));
		assertTrue(expectedResult.equals(buildSet(new String[] []{
				new String[]{"a","b","c"},new String[]{"h","q","i"}, new String[] {},new String[]{"g","t"} })));
	}

	@Test
	public void testBuildStringMap1()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
		})));
	}
	
	@Test
	public void testBuildStringMap2()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"a"},"value2"},
				new Object[]{new String[]{"b"},"value3"}
		})));
	}
	
	@Test
	public void testBuildStringMap3()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value2"},
				new Object[]{new String[]{"a"},"value1"},
				new Object[]{new String[]{"b"},"value3"}
		})));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testBuildStringMap4()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new String[]{"a"}},// an invalid sequence
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testBuildStringMap5()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{},// an invalid sequence - too few elements
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testBuildStringMap6()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new String[]{"a"},"c","d"},// an invalid sequence - too many elements
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testBuildStringMap7()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new Object(),"c"},// an invalid sequence - wrong type of the first element
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testBuildStringMap8()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{"text","c"},// an invalid sequence - wrong type of the first element
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testBuildStringMap9()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new String[]{"a"},new Object()},// an invalid sequence - wrong type of the second element
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testBuildStringMap10()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new String[]{"a"},new Object()},// an invalid sequence - null in the first element
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testBuildStringMap11()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{null, "a"},// an invalid sequence - null in the second element
				new Object[]{new String[]{"b"},null}
		})));
	}

	/** Converts a transition into an FSM structure, by taking a copy.
	 * 
	 * @param tTable table, where tTable[source][input]=targetstate
	 * @param vFrom the order in which elements from tTable are to be used.
	 * @param rejectNumber the value of an entry in a tTable which is used to denote an absence of a transition.
	 * @return the constructed transition structure.
	 */
	public static FSMStructure convertTableToFSMStructure(final int [][]tTable, final int []vFrom, int rejectNumber)
	{
		if (vFrom.length == 0 || tTable.length == 0) throw new IllegalArgumentException("array is zero-sized");
		int alphabetSize = tTable[vFrom[0]].length;
		if (alphabetSize == 0) throw new IllegalArgumentException("alphabet is zero-sized");
		String stateName[] = new String[tTable.length];for(int i=0;i < tTable.length;++i) stateName[i]="S"+i;
		String inputName[] = new String[alphabetSize];for(int i=0;i < alphabetSize;++i) inputName[i]="i"+i;
		FSMStructure fsm = new FSMStructure();
		fsm.init = stateName[vFrom[0]];
		Set<String> statesUsed = new HashSet<String>();
		for(int i=0;i<vFrom.length;++i)
		{
			int currentState = vFrom[i];
			if (currentState == rejectNumber) throw new IllegalArgumentException("reject number in vFrom");
			if (tTable[currentState].length != alphabetSize) throw new IllegalArgumentException("rows of inconsistent size");
			Map<String,String> row = new LinkedHashMap<String,String>();
			fsm.accept.put(stateName[currentState], true);
			for(int input=0;input < tTable[currentState].length;++input)
				if (tTable[currentState][input] != rejectNumber)
				{
					int nextState = tTable[currentState][input];
					if (nextState < 0 || nextState > tTable.length)
						throw new IllegalArgumentException("transition from state "+currentState+" leads to an invalid state "+nextState);
					row.put(inputName[input], stateName[nextState]);
					statesUsed.add(stateName[nextState]);
				}
			fsm.trans.put(stateName[currentState], row);
		}
		statesUsed.removeAll(fsm.accept.keySet());
		if (!statesUsed.isEmpty())
			throw new IllegalArgumentException("Some states in the transition table are not included in vFrom");
		return fsm;
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testConvertTableToFSMStructure1a()
	{
		int [][]table = new int[][] {
			{4,5,1,6}, 
			{7,7}
		};
		convertTableToFSMStructure(table, new int[0], -1);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testConvertTableToFSMStructure1b()
	{
		int [][]table = new int[][] {
			{}, 
			{1,1}
		};
		convertTableToFSMStructure(table, new int[]{1,0}, -1);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testConvertTableToFSMStructure2()
	{
		int [][]table = new int[][] {
				{1,0,1,0}, 
				{0,1}
			};
			convertTableToFSMStructure(table, new int[]{0,1}, -1);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testConvertTableToFSMStructure3()
	{
		int [][]table = new int[][] {
				{1,0,1,0}, 
				{0,1,0,1}
			};
			convertTableToFSMStructure(table, new int[]{0,-1}, -1);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testConvertTableToFSMStructure4a()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	2}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		FSMStructure fsm = convertTableToFSMStructure(table, new int[]{0,1,2,3}, -1);
		checkM(fsm, getGraphData(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0\nS2-i0->S0\nS2-i1->S0\nS2-i2->S0\nS2-i3->S0", "testConvertTableToFSMStructure4")), "S0", "S0");
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testConvertTableToFSMStructure4b()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	2}, 
			{0, 3,	0,	-1},
			{0,0,0,-4},
			{-1,-1,-1,-1}
		};
		FSMStructure fsm = convertTableToFSMStructure(table, new int[]{0,1,2,3}, -1);
		checkM(fsm, getGraphData(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0\nS2-i0->S0\nS2-i1->S0\nS2-i2->S0\nS2-i3->S0", "testConvertTableToFSMStructure4")), "S0", "S0");
	}
	
	@Test
	public final void testConvertTableToFSMStructure5()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		FSMStructure fsm = convertTableToFSMStructure(table, new int[]{0,1,3}, -1);
		checkM(fsm, getGraphData(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure4")), "S0", "S0");
	}
	
	@Test
	public final void testConvertTableToFSMStructure6()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		FSMStructure fsm = convertTableToFSMStructure(table, new int[]{1,0,3}, -1);
		checkM(fsm, getGraphData(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure4")), "S0", "S0");
	}

	@Test
	public final void testConvertTableToFSMStructure7()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		FSMStructure fsm = convertTableToFSMStructure(table, new int[]{3,0,1}, -1);
		checkM(fsm, getGraphData(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure4")), "S0", "S0");
	}
	
	@Test
	public final void testConvertTableToFSMStructure8()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		FSMStructure fsm = convertTableToFSMStructure(table, new int[]{3,0,1,0,1,1}, -1);
		checkM(fsm, getGraphData(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure4")), "S0", "S0");
	}

	@Test
	public final void assertsEnabled()
	{
		boolean assertsOn = false;
		assert assertsOn = true;
		
		Assert.assertTrue("asserts have to be enabled", assertsOn);
	}
		
	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{		
		Visualiser.disposeFrame();
	}

	@AfterClass
	public static void cleanUp()
	{
		Visualiser.disposeFrame();
	}
}
