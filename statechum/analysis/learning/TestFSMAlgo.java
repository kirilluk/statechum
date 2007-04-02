package statechum.analysis.learning;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import junit.framework.Assert;
import junit.framework.AssertionFailedError;

import org.junit.AfterClass;
import org.junit.Test;
import org.junit.BeforeClass;

import statechum.JUConstants;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.SwingUtilities;

public class TestFSMAlgo {

	/** Used to receive state transitions extracted from textual FSM representation. */
	private interface TransitionReceiver
	{
		public void accept(String from, String to, String label);
		public void reject(String from, String to, String label);
	}
	
	protected static class fsmParser {
		public static final int LABEL=0;
		public static final int LARROW=1;
		public static final int RARROW=2;
		public static final int LARROWREJ=3;
		public static final int RARROWREJ=4;
		public static final int DASH =5;
		public static final int NEWL =6;
		
		private final String text;
		private final Matcher lexer;

		public fsmParser(String whatToParse)
		{
			text = "\n"+whatToParse;
			lexer = Pattern.compile("([^\n #\\055<>]+)|( *<\\055+ *)|( *\\055+> *)|( *#\\055+ *)|( *\\055+# *)|( *\\055+ *)|( *\n *)").matcher(text);
		}
		
		protected boolean isFinished()
		{
			return lexer.regionStart() == lexer.regionEnd();
		}
		
		private String lastMatch = null;
		
		protected void throwException(String errMsg)
		{
			throw new IllegalArgumentException(errMsg+" starting from "+text.substring(lexer.regionStart()));
		}
		
		protected int getMatchType()
		{
			if (!lexer.lookingAt())
				throwException("failed to lex");
			
			int i=1;
			for(;i<lexer.groupCount()+1 && lexer.group(i) == null;++i);
			if (i == lexer.groupCount()+1)
				throwException("failed to lex (group number is out of boundary)");

			lastMatch = lexer.group(i);
			lexer.region(lexer.end(i),lexer.regionEnd());
			return i-1;// to bring it to 0..max from 1..max+1
		}
		
		protected String getMatch()
		{
			return lastMatch;
		}
		
		public void parse(TransitionReceiver receiver)
		{
			String currentState = null;
			do {					
				int left = getMatchType();
				if (left == NEWL)
				{
					while(left == NEWL && !isFinished())
						left = getMatchType();
					if (left == NEWL && isFinished())
						break;// finished parsing
					if (left != fsmParser.LABEL)
						throwException("state name expected");// there should be a state name after a newline
					currentState = getMatch();
					left=getMatchType();
				}
				
				if (left != fsmParser.LARROW && left != fsmParser.LARROWREJ && left != fsmParser.DASH)
					throwException("a left arrow or a dash expected here");
				
				if (getMatchType() != fsmParser.LABEL)
					throwException("label expected");
				String label = getMatch();
				int right = getMatchType();
				if (left == fsmParser.LARROW || left == fsmParser.LARROWREJ)
				{
					if (right != fsmParser.DASH)
						throwException("a dash was expected here");
				}
				else
				if (right != fsmParser.RARROW && right != fsmParser.RARROWREJ)
					throwException("a right-arrow was expected here");
				
				if (getMatchType() != fsmParser.LABEL)
					throwException("state name expected");
				String anotherState = getMatch();
				
				if (left == fsmParser.LARROW)
					receiver.accept(anotherState, currentState, label);
				else
					if (left == fsmParser.LARROWREJ)
						receiver.reject(anotherState, currentState, label);
				else
					if (right == fsmParser.RARROW)
						receiver.accept(currentState, anotherState, label);
					else
						receiver.reject(currentState, anotherState, label);

				currentState = anotherState;
			} while(!isFinished());
			
		}
	}
	
	protected static class bufferMatcher implements TransitionReceiver {
		final String [] elements;
		final String text;
		
		public bufferMatcher(String st,String [] data)
		{
			elements = data;text = st;
			assertEquals("incorrect number of elements in the array",true,elements.length % 4 == 0);
		}

		private int i=0;
		
		public void accept(String from, String to, String label) {
			assertEquals("wrong from string "+from,elements[i++],from);
			assertEquals("wrong to string "+from,elements[i++],to);
			assertEquals("wrong label string "+from,elements[i++],label);
			assertEquals("wrong tag","ACCEPT",elements[i++]);
		}
		
		public void reject(String from, String to, String label) {
			assertEquals("wrong from string "+from,elements[i++],from);
			assertEquals("wrong to string "+from,elements[i++],to);
			assertEquals("wrong label string "+from,elements[i++],label);
			assertEquals("wrong tag","REJECT",elements[i++]);
		}
		
		public void match()
		{
			try
			{
				fsmParser p = new fsmParser(text);
				p.parse(this);
			}
			catch(IllegalArgumentException e)
			{
				Error th = new AssertionFailedError();th.initCause(e);
				throw th;
			}
			assertEquals("incorrect number of elements in the array",elements.length,i);
		}
	}
	
	@Test
	public void testFsmParse1() {
		new bufferMatcher(" A-b->C1<-d0-P----a->C\n A- b ->B-a->U",
			new String [] {
				"A", "C1", "b",	 "ACCEPT",
				"P", "C1", "d0", "ACCEPT",
				"P", "C", "a",	 "ACCEPT",
				"A", "B", "b",	 "ACCEPT",
				"B", "U", "a",	 "ACCEPT",
			}).match();
	}

	@Test
	public void testFsmParse2() {
		new bufferMatcher(" \n \n",
			new String [] {
			}).match();
	}

	@Test
	public void testFsmParse3() {
		new bufferMatcher("",
			new String [] {
			}).match();
	}

	@Test
	public void testFsmParse4() {
		new bufferMatcher(" A_string-b->C1<-d0-P----a->C\n A- b ->B-a->U",
			new String [] {
				"A_string", "C1", "b", "ACCEPT",
				"P", "C1", "d0",	 "ACCEPT",
				"P", "C", "a",	 	"ACCEPT",
				"A", "B", "b",	 	"ACCEPT",
				"B", "U", "a",	 	"ACCEPT",
			}).match();
	}

	@Test
	public void testFsmParse5() {
		new bufferMatcher(" A-b->C.1  ---d0->P--a->C\n A- b.g ->B-a->Qst.ate",
			new String [] {
				"A", "C.1", "b",	 "ACCEPT",
				"C.1", "P", "d0",	 "ACCEPT",
				"P", "C", "a",	 	 "ACCEPT",
				"A", "B", "b.g",	 "ACCEPT",
				"B", "Qst.ate", "a", "ACCEPT",
			}).match();
	}
		
	@Test
	public void testFsmParse6() {
		new bufferMatcher(" A-b->C.1  ---d0->P--a->C\n A- b.g ->B-a->Qst.ate-c->B-a->C",
			new String [] {
				"A", "C.1", "b",	 "ACCEPT",
				"C.1", "P", "d0",	 "ACCEPT",
				"P", "C", "a",	 	"ACCEPT",
				"A", "B", "b.g",	 "ACCEPT",
				"B", "Qst.ate", "a", "ACCEPT",
				"Qst.ate","B","c",	"ACCEPT",
				"B","C","a",	 	"ACCEPT",
			}).match();
	}

	@Test
	public void testFsmParse7() {
		new bufferMatcher(" A-b-#C.1  ---d0->P--a->C\n A- b.g ->B-a->Qst.ate-c->B-a->C",
			new String [] {
				"A", "C.1", "b",	 "REJECT",
				"C.1", "P", "d0",	 "ACCEPT",
				"P", "C", "a",	 	"ACCEPT",
				"A", "B", "b.g",	 "ACCEPT",
				"B", "Qst.ate", "a", "ACCEPT",
				"Qst.ate","B","c",	"ACCEPT",
				"B","C","a",	 	"ACCEPT",
			}).match();
	}

	@Test
	public void testFsmParse8() {
		new bufferMatcher(" A-b->C.1  ---d0->P--a->C\n A- b.g -#B-a-#Qst.ate-c->B-a->C",
			new String [] {
				"A", "C.1", "b",	 "ACCEPT",
				"C.1", "P", "d0",	 "ACCEPT",
				"P", "C", "a",	 	"ACCEPT",
				"A", "B", "b.g",	 "REJECT",
				"B", "Qst.ate", "a", "REJECT",
				"Qst.ate","B","c",	"ACCEPT",
				"B","C","a",	 	"ACCEPT",
			}).match();
	}

	@Test
	public void testFsmParse9() {
		new bufferMatcher(" A_string-b-#C1#-d0-P----a->C\n A- b ->B-a->U",
			new String [] {
				"A_string", "C1", "b", "REJECT",
				"P", "C1", "d0",	 "REJECT",
				"P", "C", "a",	 	"ACCEPT",
				"A", "B", "b",	 	"ACCEPT",
				"B", "U", "a",	 	"ACCEPT",
			}).match();
	}

	@Test
	public void testFsmParse10() {
		new bufferMatcher(" A_string-b-#C1#-d0-P----a-#C\n A- b -#B-a-#U",
			new String [] {
				"A_string", "C1", "b", "REJECT",
				"P", "C1", "d0",	 "REJECT",
				"P", "C", "a",	 	"REJECT",
				"A", "B", "b",	 	"REJECT",
				"B", "U", "a",	 	"REJECT",
			}).match();
	}

	@Test
	public void testFsmParse11() {
		new bufferMatcher("P-c->P<-b-Q_State<-a-P",
			new String [] {
				"P", "P", "c", "ACCEPT",
				"Q_State", "P", "b",	 "ACCEPT",
				"P", "Q_State", "a",	 	"ACCEPT"
			}).match();
	}
		
	protected static void checkEx(String whatToParse, String exceptionSubString)
	{
		boolean exceptionThrown = false;
		try
		{
			new fsmParser(whatToParse).parse(new TransitionReceiver()
			{
				public void accept(String from, String to, String label) {
					// do nothing at all
				}
				public void reject(String from, String to, String label) {
					// do nothing at all
				}
			});
		}
		catch(IllegalArgumentException e)
		{
			assertTrue("wrong exception thrown: "+e.getMessage()+
					" instead of "+exceptionSubString+" - related exception",
					e.getMessage().contains(exceptionSubString));
			exceptionThrown = true;
		}
		
		assertTrue("exception related to "+exceptionSubString+" was not thrown",exceptionThrown);
	}
	
	@Test 
	public void testFsmParseFail1()
	{
		checkEx("A","lex");
	}
	
	@Test 
	public void testFsmParseFail2()
	{
		checkEx("-","state");
	}
	
	@Test 
	public void testFsmParseFail3()
	{
		checkEx("A -","lex");
	}
		
	@Test 
	public void testFsmParseFail4()
	{
		checkEx(" -A","state");
	}
	
	@Test 
	public void testFsmParseFail5A()
	{
		checkEx("A ->","left");
	}
	
	@Test 
	public void testFsmParseFail5B()
	{
		checkEx("A -#","left");
	}

	@Test 
	public void testFsmParseFail6()
	{
		checkEx("A b","lex");
	}
	
	@Test 
	public void testFsmParseFail7()
	{
		checkEx("A - -",JUConstants.LABEL);
	}
	
	@Test 
	public void testFsmParseFail8()
	{
		checkEx("A - b","lex");
	}
	
	@Test 
	public void testFsmParseFail9A()
	{
		checkEx("A - b\n\n","arrow");
	}

	@Test 
	public void testFsmParseFail9B()
	{
		checkEx("A - b","lex");
	}


	@Test 
	public void testFsmParseFail10()
	{
		checkEx("A - b\nN","arrow");
	}
	
	@Test 
	public void testFsmParseFail11()
	{
		checkEx("A - b - ","right");
	}

	@Test 
	public void testFsmParseFail12A()
	{
		checkEx("A <- b -> N","dash");
	}

	@Test 
	public void testFsmParseFail12B()
	{
		checkEx("A #- b -> N","dash");
	}

	@Test 
	public void testFsmParseFail12C()
	{
		checkEx("A <- b -# N","dash");
	}

	@Test 
	public void testFsmParseFail12D()
	{
		checkEx("A #- b -# N","dash");
	}

	@Test 
	public void testFsmParseFail13()
	{
		checkEx("A <- b <-N","dash");
	}

	@Test 
	public void testFsmParseFail14()
	{
		checkEx("A <- b ->","dash");
	}

	@Test 
	public void testFsmParseFail15()
	{
		checkEx("A <- b - C -","lex");
	}

	@Test 
	public void testFsmParseFail16()
	{
		checkEx("A <- b - C ->","left");
	}

	@Test 
	public void testFsmParseFail17()
	{
		checkEx("A <- b - C -","lex");
	}

	@Test 
	public void testFsmParseFail18()
	{
		checkEx("A <- b - C - -",JUConstants.LABEL);
	}

	@Test 
	public void testFsmParseFail19()
	{
		checkEx("A <- b - C - b","lex");
	}

	@Test 
	public void testFsmParseFail20()
	{
		checkEx("A <- b - C - b -","right");
	}

	@Test 
	public void testFsmParseFail21()
	{
		checkEx("A <- b - C - b ->","lex");
	}

	static protected StatePair constructPair(String a,String b)
	{
		DirectedSparseVertex aV = new DirectedSparseVertex(), bV = new DirectedSparseVertex();
		aV.addUserDatum(JUConstants.LABEL, a, UserData.SHARED);
		bV.addUserDatum(JUConstants.LABEL, b, UserData.SHARED);
		return new StatePair(aV,bV);
	}

	static protected void checkLess(String a, String b, String c, String d)
	{
		StatePair p = constructPair(a,b), q=constructPair(c,d);
		assertFalse(p.equals(q));
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertFalse(p.hashCode() == q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}
	
	@Test
	public void testStatePairEquality()
	{
		StatePair p = constructPair("a","b"), q=constructPair("a","b");
		assertTrue(p.equals(p));
		assertTrue(p.equals(q));
		assertFalse(p.equals(null));
		assertFalse(p.equals("test"));
		assertFalse(p.equals(constructPair("a","c")));
		assertFalse(p.equals(constructPair("b","b")));
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
		final Map<String,DirectedSparseVertex> existingVertices = new HashMap<String,DirectedSparseVertex>();
		final Map<StatePair,DirectedSparseEdge> existingEdges = new HashMap<StatePair,DirectedSparseEdge>();
		
		final DirectedSparseGraph g = new DirectedSparseGraph();
		g.setUserDatum(JUConstants.TITLE, name,UserData.SHARED);

		new fsmParser(fsm).parse(new TransitionReceiver()
		{
			public void put(String from, String to, String label, boolean accept) {
				DirectedSparseVertex fromVertex = existingVertices.get(from), toVertex = existingVertices.get(to);
				
				if (fromVertex == null)
				{
					fromVertex = new DirectedSparseVertex();
					if (existingVertices.isEmpty())
						fromVertex.addUserDatum("property", "init", UserData.SHARED);
					fromVertex.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);
					fromVertex.addUserDatum(JUConstants.LABEL, from, UserData.SHARED);
					existingVertices.put(from, fromVertex);
					g.addVertex(fromVertex);
				}
				else
					if (!Boolean.valueOf(fromVertex.getUserDatum(JUConstants.ACCEPTED).toString()))
						throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+from);

				if (from.equals(to))
					toVertex = fromVertex;
				else
					if (toVertex == null)
					{
						toVertex = new DirectedSparseVertex();
						toVertex.removeUserDatum(JUConstants.ACCEPTED); // in case we've got a reject loop in the same state
						toVertex.addUserDatum(JUConstants.ACCEPTED, Boolean.toString(accept), UserData.SHARED);
						toVertex.addUserDatum(JUConstants.LABEL, to, UserData.SHARED);
						existingVertices.put(to, toVertex);
						g.addVertex(toVertex);
					}
					else
						if (Boolean.valueOf(toVertex.getUserDatum(JUConstants.ACCEPTED).toString()) != accept)
							throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+to);
				
				StatePair pair = new StatePair(fromVertex,toVertex);
				DirectedSparseEdge edge = existingEdges.get(pair);
				if (edge == null)
				{
					edge = new DirectedSparseEdge(fromVertex,toVertex);
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
		
		return g;
	}

	/** This data store represents an FSM and is used by tests. */
	public static class FSMStructure
	{
		/** The transition transition diagram, in which every state is mapped to a map between an input (label) and a target state. */
		public Map<String,Map<String,String>> trans;
		
		/** All states of the machine should be in the domain of this function; 
		 * for a given state, this function will return <pre>true</pre> if it is an accept state and <pre>false</pre> for a reject one.
		 */ 
		public Map<String,Boolean> accept;
		
		/** The initial state. */
		public String init;
		
		public FSMStructure(Map<String,Map<String,String>> transitions,Map<String,Boolean> a,String initState)
		{
			trans = transitions;accept = a;init = initState;
		}
		
		public FSMStructure()
		{
			trans = new HashMap<String,Map<String,String>>();accept = new HashMap<String,Boolean>();
		}
	}

	/** Builds fsm structures corresponding to the Jung graph passed as an argument. The aim is
	 * to use the constructed structure for the comparison of different Jung graphs.
	 * 
	 * @param g graph from which to extract data
	 * @return the class storing transition information.
	 */
	public static FSMStructure getGraphData(DirectedSparseGraph g)
	{
		Iterator<DirectedSparseEdge> edgeIt = (Iterator<DirectedSparseEdge>)g.getEdges().iterator();
		FSMStructure extractedFSM = new FSMStructure();
		while(edgeIt.hasNext())
		{
			DirectedSparseEdge edge = edgeIt.next();
			Vertex fromVertex = edge.getSource(), toVertex = edge.getDest();
			String from = (String)fromVertex.getUserDatum(JUConstants.LABEL),
				to = (String)toVertex.getUserDatum(JUConstants.LABEL);
			Map<String,String> labelToTargetState = extractedFSM.trans.get(from);
			if (labelToTargetState == null)
			{
				labelToTargetState = new HashMap<String,String>();extractedFSM.trans.put(from, labelToTargetState);
			}
			createLabelToStateMap((Set<String>)edge.getUserDatum(JUConstants.LABEL),to,labelToTargetState);
		}
		
		Iterator<Vertex> vertexIt = (Iterator<Vertex>)g.getVertices().iterator();
		while(vertexIt.hasNext())
		{
			Vertex v = vertexIt.next();
			String name = (String)v.getUserDatum(JUConstants.LABEL);
			if (extractedFSM.accept.containsKey(name))
				throw new IllegalArgumentException("multiple states with the same name "+name);
			
			extractedFSM.accept.put(name, 
					new Boolean(v.getUserDatum(JUConstants.ACCEPTED).toString()));
			Object initp = v.getUserDatum("property");
			if (initp != null)
			{
				if (!"init".equals(initp.toString()))
					throw new IllegalArgumentException("invalid init property");

				if (extractedFSM.init != null)
					throw new IllegalArgumentException("duplicate initial state "+name);

				extractedFSM.init = name;
			}
		}
		if (extractedFSM.init == null)
			throw new IllegalArgumentException("missing initial state");
		
		return extractedFSM;
	}
	
	/** Given a set of labels and a target state, this method adds to a supplied map an association 
	 * of every label with the specified target state.
	 * 
	 * @param labels labels
	 * @param to target state
	 * @param map a map associating state <i>to</i> with each of the labels. If this is <i>null</i>, a new map is created.
	 * @return an updated map.
	 */ 
	protected static Map<String,String> createLabelToStateMap(Collection<String> labels,String to,Map<String,String> map)
	{
		Map<String,String> result = (map == null)? new HashMap<String,String>() : map;
		for(String label:labels)
		{
			if (result.containsKey(label))
				throw new IllegalArgumentException("nondeterminism detected for label "+label);
			result.put(label,to);
		}
		return result;
	}
	
	@Test 
	public void testCreateLabelToStateMap1() // test with empty data
	{
		assertTrue(createLabelToStateMap(new LinkedList<String>(), "junk", null).isEmpty());
		Map<String,String> map = new HashMap<String,String>();
		assertSame(map,createLabelToStateMap(new LinkedList<String>(), "junk", map));assertTrue(map.isEmpty());
	}
	
	@Test 
	public void testCreateLabelToStateMap2() // test for no changes
	{
		Map<String,String> trans = new HashMap<String,String>();
		trans.put("a", "A");trans.put("b", "A");trans.put("c", "B");
		Map<String,String> expected = new HashMap<String,String>();expected.putAll(trans);
		assertSame(trans,createLabelToStateMap(new LinkedList<String>(), "junk",trans));
		assertTrue(expected.equals(trans));
	}
	
	@Test 
	public void testCreateLabelToStateMap3() // test for correct data being added
	{
		Map<String,String> trans = new HashMap<String,String>();
		trans.put("a", "A");trans.put("b", "A");trans.put("c", "B");
		Map<String,String> expected = new HashMap<String,String>();expected.putAll(trans);expected.put("e", "A");expected.put("g", "A");
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"g","e"}), "A",trans));
		assertTrue(expected.equals(trans));
	}
	
	@Test 
	public void testCreateLabelToStateMap4() // test for correct data being added
	{
		Map<String,String> trans = new HashMap<String,String>();
		trans.put("a", "A");trans.put("b", "A");trans.put("c", "B");
		Map<String,String> expected = new HashMap<String,String>();expected.putAll(trans);expected.put("e", "D");expected.put("f", "D");
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"f","e"}), "D",trans));
		assertTrue(expected.equals(trans));
	}
	
	@Test 
	public void testCreateLabelToStateMap5() // test for correct data being added
	{
		Map<String,String> trans = new HashMap<String,String>();
		trans.put("a", "A");trans.put("b", "A");trans.put("c", "B");
		Map<String,String> expected = new HashMap<String,String>();expected.putAll(trans);expected.put("e", "B");expected.put("g", "B");
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"g","e"}), "B",trans));
		assertTrue(expected.equals(trans));
	}
	
	@Test 
	public void testCreateLabelToStateMap6() // test for correct data being added when an empty collection is passed
	{
		Map<String,String> trans = new HashMap<String,String>();
		Map<String,String> expected = new HashMap<String,String>();expected.put("e","A");expected.put("b","A");
		assertSame(trans,createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), "A",trans));
		assertTrue(expected.equals(trans));
	}

	@Test 
	public void testCreateLabelToStateMap7() // test for correct data being added when null is passed
	{
		Map<String,String> expected = new HashMap<String,String>();expected.put("e","A");expected.put("b","A");
		assertTrue(expected.equals(createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), "A",null)));
	}

	@Test 
	public void testCreateLabelToStateMap8() // test for correct detection of nondeterminism
	{
		Map<String,String> trans = new HashMap<String,String>();trans.put("a", "A");trans.put("b", "A");trans.put("c", "B");
		boolean exceptionThrown = false;
		try
		{
			createLabelToStateMap(Arrays.asList(new String[] {"b","e"}), "A",trans);
		}
		catch(IllegalArgumentException e)
		{
			assertTrue("incorrect exception thrown",e.getMessage().contains("nondeterminism"));
			exceptionThrown = true;
		}
		
		assertTrue("exception not thrown",exceptionThrown);
	}

	@Test 
	public void testCreateLabelToStateMap9() // test for correct detection of nondeterminism
	{
		boolean exceptionThrown = false;
		try
		{
			createLabelToStateMap(Arrays.asList(new String[] {"b","b"}), "A",null);
		}
		catch(IllegalArgumentException e)
		{
			assertTrue("incorrect exception thrown",e.getMessage().contains("nondeterminism"));
			exceptionThrown = true;
		}
		
		assertTrue("exception not thrown",exceptionThrown);
	}

	/** Displays the graph passed as an argument in the Jung window.
	 * @param g the graph to display 
	 */
	public void updateFrame(DirectedSparseGraph g)
	{
		visFrame.update(null, g);
	}
	
	@Test
	public void testGraphConstruction1()
	{
		FSMStructure expected = new FSMStructure();
		DirectedSparseGraph g = buildGraph("A--a-->B-b->C-c->A","testConstruction1");
		updateFrame(g);
		FSMStructure graph = getGraphData(g);
		expected.trans.put("A", createLabelToStateMap(Arrays.asList(new String[] {"a"}),"B",null));
		expected.trans.put("B", createLabelToStateMap(Arrays.asList(new String[] {"b"}),"C",null));
		expected.trans.put("C", createLabelToStateMap(Arrays.asList(new String[] {"c"}),"A",null));
		expected.accept.put("A", true);
		expected.accept.put("B", true);
		expected.accept.put("C", true);
		
		assertEquals("A", graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,graph.trans.equals(expected.trans));
	}

	@Test
	public void testGraphConstruction2()
	{
		FSMStructure expected = new FSMStructure();
		DirectedSparseGraph g = buildGraph("A--a-->B-b->C-c->A-b->B-a-#D","testConstruction2");
		g.setUserDatum(JUConstants.TITLE, "testConstruction2",UserData.SHARED);
		updateFrame(g);
		FSMStructure graph = getGraphData(g);
		expected.trans.put("A", createLabelToStateMap(Arrays.asList(new String[] {"a","b"}),"B",null));
		expected.trans.put("B", createLabelToStateMap(Arrays.asList(new String[] {"b"}),"C",createLabelToStateMap(Arrays.asList(new String[] {"a"}),"D",null)));
		expected.trans.put("C", createLabelToStateMap(Arrays.asList(new String[] {"c"}),"A",null));
		expected.accept.put("A", true);
		expected.accept.put("B", true);
		expected.accept.put("C", true);
		expected.accept.put("D", false);
		
		assertEquals("A", graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,expected.trans.equals(graph.trans));
	}

	@Test
	public void testGraphConstruction3()
	{
		FSMStructure expected = new FSMStructure();
		DirectedSparseGraph g = buildGraph("A--a-->B<-b--C-c->A-b->A-c->A\nB-d->B-p->C\nB-q->C\nB-r->C\n","testConstruction3");
		updateFrame(g);
		FSMStructure graph = getGraphData(g);
		expected.trans.put("A", createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),"A",createLabelToStateMap(Arrays.asList(new String[] {"a"}),"B",null)));
		expected.trans.put("B", createLabelToStateMap(Arrays.asList(new String[] {"d"}),"B",createLabelToStateMap(Arrays.asList(new String[] {"r","p","q"}),"C",null)));
		expected.trans.put("C", createLabelToStateMap(Arrays.asList(new String[] {"b"}),"B",createLabelToStateMap(Arrays.asList(new String[] {"c"}),"A",null)));
		expected.accept.put("A", true);
		expected.accept.put("B", true);
		expected.accept.put("C", true);
		
		assertEquals("A", graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,expected.trans.equals(graph.trans));
	}

	@Test
	public void testGraphConstruction4()
	{
		FSMStructure expected = new FSMStructure();
		DirectedSparseGraph g = buildGraph("A--a-->B<-b--D-c->A-b->A-c->A\nB-d->B-p-#C\nB-q-#C\nB-r-#C\n","testConstruction4");
		updateFrame(g);
		FSMStructure graph = getGraphData(g);
		expected.trans.put("A", createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),"A",createLabelToStateMap(Arrays.asList(new String[] {"a"}),"B",null)));
		expected.trans.put("B", createLabelToStateMap(Arrays.asList(new String[] {"d"}),"B",createLabelToStateMap(Arrays.asList(new String[] {"r","p","q"}),"C",null)));
		expected.trans.put("D", createLabelToStateMap(Arrays.asList(new String[] {"b"}),"B", createLabelToStateMap(Arrays.asList(new String[] {"c"}),"A",null)));
		expected.accept.put("A", true);
		expected.accept.put("B", true);
		expected.accept.put("C", false);
		expected.accept.put("D", true);
		
		assertEquals("A", graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,expected.trans.equals(graph.trans));
	}

	@Test
	public void testGraphConstruction5()
	{
		FSMStructure expected = new FSMStructure();
		DirectedSparseGraph g = buildGraph("A--a-->B-b-#C\nA-b->A-c->A\nB-d->B-p-#C\nB-q-#C\nB-r-#C\n","testConstruction5");
		visFrame.update(null, g);
		FSMStructure graph = getGraphData(g);
		expected.trans.put("A", createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),"A",createLabelToStateMap(Arrays.asList(new String[] {"a"}),"B",null)));
		expected.trans.put("B", createLabelToStateMap(Arrays.asList(new String[] {"d"}),"B",createLabelToStateMap(Arrays.asList(new String[] {"b","r","p","q"}),"C",null)));
		expected.accept.put("A", true);
		expected.accept.put("B", true);
		expected.accept.put("C", false);
		
		assertEquals("A", graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,expected.trans.equals(graph.trans));
	}
	
	@Test
	public void testGraphConstruction6() // checks loop support
	{
		FSMStructure expected = new FSMStructure();
		DirectedSparseGraph g = buildGraph("P-c->P<-b-Q_State<-a-P-b->P\nQ_State-a->Q_State","testConstruction6");
		visFrame.update(null, g);
		FSMStructure graph = getGraphData(g);
		expected.trans.put("P", createLabelToStateMap(Arrays.asList(new String[] {"b","c"}),"P",createLabelToStateMap(Arrays.asList(new String[] {"a"}),"Q_State",null)));
		expected.trans.put("Q_State", createLabelToStateMap(Arrays.asList(new String[] {"a"}),"Q_State",createLabelToStateMap(Arrays.asList(new String[] {"b"}),"P",null)));
		expected.accept.put("P", true);
		expected.accept.put("Q_State", true);
		
		assertEquals("P", graph.init);
		assertEquals("incorrect vertice set",true,expected.accept.equals(graph.accept));
		assertEquals("incorrect transition set",true,expected.trans.equals(graph.trans));
	}

	@Test
	public void testGraphConstructionFail1()
	{
		boolean exceptionThrown = false;
		try
		{
			buildGraph("A--a-->B<-b-CONFL\nA-b->A-c->A\nB-d->B-p-#CONFL","testGraphConstructionFail1");
		}
		catch(IllegalArgumentException e)
		{
			assertTrue("correct exception not thrown",e.getMessage().contains("conflicting") && e.getMessage().contains("CONFL"));
			exceptionThrown = true;
		}
		
		assertTrue("exception not thrown",exceptionThrown);
	}
	
	/** Checks if adding a vertex to a graph causes an exception to be thrown. */
	public static void checkWithVertex(Vertex v,String expectedExceptionString, String testName)
	{
		final DirectedSparseGraph g = buildGraph("A--a-->B<-b-CONFL\nA-b->A-c->A\nB-d->B-p->CONFL",testName);
		getGraphData(g);// without the vertex being added, everything should be fine.
		g.addVertex(v);// add the vertex
		
		boolean exceptionThrown = false;
		try
		{
			getGraphData(g);// now getGraphData should choke.			
		}
		catch(IllegalArgumentException e)
		{
			assertTrue("correct exception not thrown",e.getMessage().contains(expectedExceptionString) );
			exceptionThrown = true;
		}
		
		assertTrue("exception not thrown",exceptionThrown);
	}
	
	@Test
	public void testGraphConstructionFail2()
	{
		DirectedSparseVertex v = new DirectedSparseVertex();
		v.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);v.addUserDatum(JUConstants.LABEL, "B", UserData.SHARED);
		checkWithVertex(v, "multiple", "testGraphConstructionFail2");
	}
	
	@Test
	public void testGraphConstructionFail3()
	{
		DirectedSparseVertex v = new DirectedSparseVertex();
		v.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);v.addUserDatum(JUConstants.LABEL, "CONFL", UserData.SHARED);
		checkWithVertex(v, "multiple", "testGraphConstructionFail3");
	}
	
	@Test
	public void testGraphConstructionFail4()
	{
		DirectedSparseVertex v = new DirectedSparseVertex();
		v.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);v.addUserDatum(JUConstants.LABEL, "Q", UserData.SHARED);v.addUserDatum("property", "init", UserData.SHARED);
		checkWithVertex(v, "duplicate", "testGraphConstructionFail4");
	}
	
	@Test
	public void testGraphConstructionFail5()
	{
		DirectedSparseVertex v = new DirectedSparseVertex();
		v.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);v.addUserDatum(JUConstants.LABEL, "Q", UserData.SHARED);v.addUserDatum("property", "aa", UserData.SHARED);
		checkWithVertex(v, "property", "testGraphConstructionFail5");
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
		updateFrame(expectedGraph);
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
	
	/** Checks the equivalence between the two states, stateG of graphA and stateB of graphB.
	 * Unreachable states are ignored. 
	 */
	public static void checkM(FSMStructure graph, FSMStructure expected, String stateGraph, String stateExpected)
	{
		List<String> currentExplorationBoundary = new LinkedList<String>();

		Map<String,String> morphism = new HashMap<String,String>();
		Set<String> statesAddedToBoundary = new HashSet<String>();
		currentExplorationBoundary.add(stateGraph);statesAddedToBoundary.add(stateGraph);
		morphism.put(stateGraph,stateExpected);
		
		for(int i=0;i < currentExplorationBoundary.size();++i)
		{
			String state = currentExplorationBoundary.get(i);
			String mappedState = morphism.get(state);assert(mappedState != null);
			if (!graph.accept.get(state).equals(expected.accept.get(mappedState)))
				throw new DifferentFSMException("state "+mappedState+" has a different acceptance labelling between the machines");
						
			Map<String,String> targets = graph.trans.get(state), expectedTargets = expected.trans.get(mappedState);
			if (expectedTargets == null)
			{
				if (targets != null)
					throw new DifferentFSMException("not expecting any transitions from "+mappedState+" state");
			}
			else
			{
				if (targets == null)
					throw new DifferentFSMException("expected transitions from "+mappedState+" state but there were none");
				
				if (expectedTargets.size() != targets.size())// each of them is equal to the keyset size from determinism
					throw new DifferentFSMException("different number of transitions from state "+mappedState);
					
				for(String label:targets.keySet())
				{
					if (!expectedTargets.containsKey(label))
						throw new DifferentFSMException("no transition with expected label "+label+" from a state corresponding to "+mappedState);
					String tState = targets.get(label);// the original one
					String targetMappedState = morphism.get(tState); // the state which corresponds to this state
					String expectedState = expectedTargets.get(label);
					if (targetMappedState != null) // we've already mapped this state
					{
						if (!expectedState.equals(targetMappedState))
							throw new DifferentFSMException("transition "+mappedState+" -- "+label+" leads to the wrong state");
					}
					else
						morphism.put(tState,expectedState);// record the mapping
					
					if (!statesAddedToBoundary.contains(tState))
					{
						currentExplorationBoundary.add(tState);
						statesAddedToBoundary.add(tState);
					}
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
	
	public static int tracePath(String init, Map<String,Map<String,String>> trans,Map<String,Boolean> accept, List<String> path)
	{
		String current = init;
		int pos = -1;
		for(String label:path)
		{
			++pos;
			Map<String,String> exitingTrans = trans.get(current);
			if (exitingTrans == null || (current = exitingTrans.get(label)) == null)
				return pos;
		}
		return accept.get(current).booleanValue()? RPNIBlueFringeLearner.USER_ACCEPTED:pos;
	}

	/** Given an FSM and a sequence of labels to follow, this one checks whether the sequence is correctly
	 * accepted or not, and if not whether it is rejected at the correct element.
	 * 
	 * @param fsmString a description of an FSM
	 * @param path a sequence of labels to follow
	 * @param ExpectedResult the result to check
	 */
	public static void checkPath(String fsmString, String []path, int ExpectedResult)
	{
		final FSMStructure graph = getGraphData(buildGraph(fsmString, "sample FSM"));
		assertEquals(ExpectedResult, tracePath(graph.init, graph.trans, graph.accept, Arrays.asList(path)));
	}
	
	@Test
	public void testTracePath()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{}, RPNIBlueFringeLearner.USER_ACCEPTED);
	}
	
	@Test
	public void testTracePath1()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a"}, RPNIBlueFringeLearner.USER_ACCEPTED);
	}
	
	@Test
	public void testTracePath2()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a","b","c"}, RPNIBlueFringeLearner.USER_ACCEPTED);
	}
	
	@Test
	public void testTracePath3()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"b"}, 0);
	}
	
	@Test
	public void testTracePath4()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a","b","d"}, 2);
	}
	
	@Test
	public void testTracePath5()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"b","a","c"}, 0);
	}
	
	@Test
	public void testTracePath6()
	{
		checkPath("A-a->B-b->C-c->D", new String[]{"a","a","c","b"}, 1);
	}
	
	@Test
	public void testTracePath7()
	{
		checkPath("A-a->B-b->C-c-#D", new String[]{"a","b","c"}, 2);
	}
	
	@Test
	public void testTracePath8()
	{
		checkPath("A-a->B-b->C-c-#D", new String[]{"a","b","c","d"}, 3);
	}
	
	@Test
	public void testTracePath9()
	{
		checkPath("A-a->B-b->C-c-#D", new String[]{"a","b","c","d","e"}, 3);
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
		rejectVertex.addUserDatum(JUConstants.ACCEPTED, "false", UserData.SHARED);
		rejectVertex.addUserDatum(JUConstants.LABEL, reject, UserData.SHARED);
		
		HashSet<String> alphabet = new HashSet<String>();

		// first pass - computing an alphabet
		Iterator<Vertex> vertexIt = (Iterator<Vertex>)g.getVertices().iterator();
		while(vertexIt.hasNext())
		{
			Vertex v = vertexIt.next();
			Iterator<DirectedSparseEdge>outEdgeIt = v.getOutEdges().iterator();
			while(outEdgeIt.hasNext()){
				DirectedSparseEdge outEdge = outEdgeIt.next();
				alphabet.addAll( (Set<String>)outEdge.getUserDatum(JUConstants.LABEL) );
			}
		}
		
		// second pass - checking if any transitions need to be added.
		Set<String> outLabels = new HashSet<String>();
		vertexIt = (Iterator<Vertex>)g.getVertices().iterator();
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
					Set<String> outgoingLabels = (Set<String>)alphabet.clone();
					
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
		updateFrame(g);
		checkM(g, "A-a->A-b->B-c->B-a->C\nA-c-#REJECT#-b-B\nC-a-#REJECT\nC-b-#REJECT\nC-c-#REJECT");		
	}	
	
	@Test
	public void completeGraphTest7()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "completeGraphTest7");Assert.assertTrue(completeGraph(g,"REJECT"));
		updateFrame(g);
		final FSMStructure graph = getGraphData(g);
		final FSMStructure expected = getGraphData(buildGraph("A-a->A-b->B-c->B-a->C\nA-c-#REJECT\nA-d-#REJECT\nB-b-#REJECT\nB-d-#REJECT\nC-a-#REJECT\nC-b-#REJECT\nC-c-#REJECT\nC-d-#REJECT\nS-a-#REJECT\nS-b-#REJECT\nS-c-#REJECT\nS-d-#REJECT\nQ-a-#REJECT\nQ-b-#REJECT\nQ-c-#REJECT\nQ-d->S","expected graph"));
		Assert.assertTrue(checkMBoolean(graph,expected,"A","A"));
		Assert.assertTrue(checkMBoolean(graph,expected,"B","B"));
		Assert.assertTrue(checkMBoolean(graph,expected,"Q","Q"));
		Assert.assertTrue(checkMBoolean(graph,expected,"S","S"));
		Assert.assertTrue(checkMBoolean(graph,expected,"REJECT","REJECT"));
	}	
	
	/** Holds the JFrame to see the graphs being dealt with. Usage:
	 * <pre>
	 * 		updateFrame(g);// a public method
	 * </pre>
	 * where <i>g</i> is the graph to be displayed.
	 */
	protected static Visualiser visFrame = null;
	
	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{
		visFrame = new Visualiser();
	}

	@AfterClass
	public static void cleanUp()
	{
		try {
			SwingUtilities.invokeAndWait(new Runnable() 
			{
				public void run()
				{
					visFrame.setVisible(false);
					visFrame.dispose();
				}
			});
		} catch (InterruptedException e) {
			// cannot do anything with this
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// cannot do anything with this
			e.printStackTrace();
		}
	}	
}
