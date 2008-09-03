/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

statechum is free software: you can redistribute it and/or modify
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
package statechum.analysis.learning.rpnicore;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.LTL_to_ba.OPERATION;

import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;

/**
 * @author kirill
 *
 */
public class TestLTL_to_ba {
	@Before
	public final void beforeTest()
	{
		config = Configuration.getDefaultConfiguration().copy();		
		ba=new LTL_to_ba(config,null);ba.alphabet = new HashSet<String>();
		ba.alphabet.addAll(Arrays.asList(new String[]{"a","b","c"}));
		
		expectedFromASEExample = new LearnerGraph(TestFSMAlgo.buildGraph(
				"I-close->1\nI-edit->I1\nI-save->I1\nI-load->I1\n"+
				"1-load->I1-close->1\n"+
				"I1-edit->I1-save->I1-load->I1\n","testLTL_bigger"),config);
	}
	// ,"load","save","edit","close"
	protected Configuration config = null;
	protected LTL_to_ba ba = null;
	protected LearnerGraph expectedFromASEExample = null;
	
	@Test
	public final void testLtlConcat1()
	{
		Assert.assertEquals("",LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{})).toString());
	}
	
	@Test
	public final void testLtlConcat2()
	{
		Assert.assertEquals("a",LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{" a ","","  "})).toString());
	}
	
	@Test
	public final void testLtlConcat3()
	{
		Assert.assertEquals("a || b",LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{" a ","","  ","  b "," "})).toString());
	}
	
	@Test
	public final void testLtlConcat4()
	{
		Assert.assertEquals("a || test || c",LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{" a ","","  ","  test ","c "})).toString());
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void testLTL_fail1()
	{
		LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{"  */ "}));
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testLTL_fail2()
	{
		LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{"  /* "}));
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testLTL_fail3()
	{
		LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{"a "," ","b", " this is /* blah ) blah blah"}));
	}
	
	
	@Test
	public final void testLTL_ba_fail1()
	{
		final String text=LTL_to_ba.baError+":";
		checkForCorrectException(new whatToRun() { public void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"syntax");
	}
	
	@Test
	public final void testLTL_ba_fail2()
	{
		final String text="this is a test\nError:";
		checkForCorrectException(new whatToRun() { public void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"failed to lex");
	}
	
	@Test
	public final void testLTL_ba_fail3()
	{
		final String text="/* some text */";
		checkForCorrectException(new whatToRun() { public void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"failed to find the start of automaton");
	}
	
	@Test
	public final void testLTL_ba_fail4()
	{
		final String text="never { /* (G((close)-> X((load) R !((save) || (edit) || (close))))) */\n\n\n"+
		"junk"
		;
		checkForCorrectException(new whatToRun() { public void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"failed to lex");
	}
	
	@Test
	public final void testLTL_ba_fail5()
	{
		final String text="never { /* (G((close)-> X((load) R !((save) || (edit) || (close))))) */\n\n\n"+
		"some_state : \n"+
		"some_state : \n"
		;
		checkForCorrectException(new whatToRun() { public void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"expected if");
	}
	
	@Test
	public final void testLTL_ba_fail6()
	{
		final String text="never { /* (G((close)-> X((load) R !((save) || (edit) || (close))))) */\n\n\n"+
		"some_state : \n"+
		"if\n"+
		"fi;\n"+
		"more_state:\n"+"text:"
		;
		checkForCorrectException(new whatToRun() { public void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"expected if");
	}
	
	@Test
	public final void testLTL_ba_fail7()
	{
		final String text="never { /* (G((close)-> X((load) R !((save) || (edit) || (close))))) */\n\n\n"+
		"some_state : \n"+
		"if\n"+
		":: (a) -> goto state\n"+
		"if"
		;
		checkForCorrectException(new whatToRun() { public void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"unexpected lexTRANSITION");
	}
	
	@Test
	public final void testLTL_ba_fail8()
	{
		final String text="never { /* (G((close)-> X((load) R !((save) || (edit) || (close))))) */\n\n\n"+
		"some_state : \n"+
		"if\n"+
		":: (a) -> goto state\n"+
		"another:"
		;
		checkForCorrectException(new whatToRun() { public void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"unexpected lexTRANSITION");
	}
	
	/** Tests label interpretation. */
	@Test(expected=IllegalArgumentException.class)
	public final void testLabelInterpretation_fail1()
	{
		ba.interpretInputLabel("");
	}
		
	/** Tests label interpretation: unknown label. */
	@Test
	public final void testLabelInterpretation_fail2()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			ba.interpretInputLabel("d");
		}},IllegalArgumentException.class,"unrecognised label");
	}
	
	@Test
	public final void testLabelInterpretationA()
	{
		Set<String> set = ba.interpretInputLabel("a");
		Assert.assertEquals(1, set.size());Assert.assertEquals("a",set.iterator().next());
	}
	
	@Test
	public final void testLabelInterpretationB()
	{
		Set<String> set = ba.interpretInputLabel("1");
		Assert.assertEquals(ba.alphabet, set);
	}
	
	/** Checks that a very simple expression applied to the initial data returns the expected value.
	 * 
	 * @param expected expected result
	 * @param initial initial data
	 * @param oper operation to apply
	 * @param label label to use
	 */
	public void checkOperation(String [] expected,String [] initial,OPERATION oper, String label)
	{
		Set<String> actual = new TreeSet<String>();actual.addAll(Arrays.asList(initial));
		ba.performOperation(actual, oper, ba.interpretInputLabel(label));
		
		checkOutcome(expected,actual);
	}
	
	public void checkOutcome(String [] expected, Set<String> actual)
	{
		Set<String> exp = new TreeSet<String>();exp.addAll(Arrays.asList(expected));
		Assert.assertEquals(exp,actual);
	}
	
	/** Tests operations. */
	@Test
	public final void testOp1()
	{
		checkOperation(new String[]{"a"},new String[]{},OPERATION.ASSIGN,"a");
	}

	protected String [] getAlphabet()
	{
		String[] result = new String[ba.alphabet.size()];
		ba.alphabet.toArray(result);
		return result;
	}
	
	/** Tests operations. */
	@Test
	public final void testOp2()
	{
		checkOperation(new String[]{"a"},getAlphabet(),OPERATION.AND,"a");
		checkOperation(new String[]{"b","c"},getAlphabet(),OPERATION.NEG,"a");
	}
	
	/** Tests operations. */
	@Test
	public final void testOp3()
	{
		checkOperation(new String[]{"a","b","c"},getAlphabet(),OPERATION.AND,"1");
		checkOperation(new String[]{},getAlphabet(),OPERATION.NEG,"1");
	}

	/** Tests operations. */
	@Test
	public final void testOp4()
	{
		checkOperation(new String[]{"b","c"},new String[]{"c"},OPERATION.OR,"b");
		checkOperation(new String[]{},new String[]{"c"},OPERATION.AND,"b");
	}

	protected Set<String> interpretUnary(String str)
	{
		ba.buildExprLexer(str);
		return ba.interpretUnary();
	}
	/** Tests interpretation of unary expressions. */
	@Test
	public final void testUn1()
	{
		checkOutcome(new String[]{"a"},interpretUnary("a"));
	}
	
	/** Tests interpretation of unary expressions. */
	@Test
	public final void testUn2()
	{
		checkOutcome(new String[]{"b","c"},interpretUnary("!a"));
	}
	
	/** Tests interpretation of unary expressions. */
	@Test
	public final void testUn3()
	{
		checkOutcome(new String[]{"a","b","c"},interpretUnary("1"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr1()
	{
		checkOutcome(new String[]{"a"},ba.interpretString("a"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr2()
	{
		checkOutcome(new String[]{"a","b","c"},ba.interpretString("1"));
	}
	/** Tests parser of expressions. */
	@Test
	public final void testExpr3()
	{
		checkOutcome(new String[]{},ba.interpretString("!1"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr4()
	{
		checkOutcome(new String[]{"a"},ba.interpretString("a && a"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr5()
	{
		checkOutcome(new String[]{},ba.interpretString("a && b"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr6()
	{
		checkOutcome(new String[]{"a","b"},ba.interpretString("a || b"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr7()
	{
		checkOutcome(new String[]{"c"},ba.interpretString("a && b || c"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr8()
	{
		checkOutcome(new String[]{"a"},ba.interpretString("(a)"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr9()
	{
		checkOutcome(new String[]{"a"},ba.interpretString("((a))"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr10()
	{
		checkOutcome(new String[]{"a","b"},ba.interpretString("((a) || b)"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr11()
	{
		checkOutcome(new String[]{"a","b"},ba.interpretString("((a || (b)))"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr12a()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			ba.interpretString("");
		}},IllegalArgumentException.class,"unexpected end");
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr12b()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			ba.interpretString("()");
		}},IllegalArgumentException.class,"unexpected end");
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr12c()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			ba.interpretString("b && (!())");
		}},IllegalArgumentException.class,"unexpected end");
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr13()
	{
		checkOutcome(new String[]{"c","b"},ba.interpretString("((!a || (b)))"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr14()
	{
		checkOutcome(new String[]{"c"},ba.interpretString("!((a || (b)))"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr15()
	{
		checkOutcome(new String[]{"c"},ba.interpretString("((1 || (b)) && c)"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr16()
	{
		checkOutcome(new String[]{},ba.interpretString("(!(1 || (b)) && c)"));
	}

	/** Tests parser of expressions. */
	@Test
	public final void testExpr17()
	{
		checkOutcome(new String[]{"c"},ba.interpretString("(!(a || (b)) && c)"));
	}
	
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr18()
	{
		checkOutcome(new String[]{"a"},ba.interpretString("a && !!!b"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr19()
	{
		checkOutcome(new String[]{"a"},ba.interpretString("a && !!(!b && !((b && 1)))"));
	}
	
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr20()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			ba.interpretString("a | b");
		}},IllegalArgumentException.class,"failed to lex ");
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr21()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			ba.interpretString("a || || b");
		}},IllegalArgumentException.class,"expected word ");
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr22()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			ba.interpretString("a || && b");
		}},IllegalArgumentException.class,"expected word ");
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr23a()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			ba.interpretString("a || b c ");
		}},IllegalArgumentException.class,"expected binary operation ");
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr23b()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			ba.interpretString("a || b (c) ");
		}},IllegalArgumentException.class,"expected binary operation ");
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr23c()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			ba.interpretString("a || b !(c) ");
		}},IllegalArgumentException.class,"expected binary operation ");
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr24()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			ba.interpretString("a || b)");
		}},IllegalArgumentException.class,"extra tokens at the end ");
	}
	
	/** Converts LTL into a deterministic automaton without adding reject-transitions or merging with the initial graph. */
	protected LearnerGraph loadWithoutMerging(String text)
	{
		ba.parse(text);
		LearnerGraph result = null;
		synchronized(LearnerGraph.syncObj)
		{
			result = ba.buildDeterministicGraph(ba.matrix);
		}
		return result;
	}
	
	@Test
	public final void testLTL_ba_empty()
	{
		final String text="never { /* (G((close)-> X((load) R !((save) || (edit) || (close))))) */\n\n\n"+
		"state_init: false;\n}\n"
		;
		LearnerGraph expected = new LearnerGraph(config);expected.init.setAccept(false);
		Assert.assertNull(WMethod.checkM(expected,loadWithoutMerging(text)));
	}
	
	/** Very simple graph */
	@Test
	public final void testLTL_ba_graph0a()
	{
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_init:\n"+
		"if :: (a) -> goto accept_init \n"+
		":: (b) -> goto state_b\n"+
		"fi;\n"+
		"state_b: if :: (a) -> goto state_b fi;"+
		"}\n";
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B-a->B", "testLTL_ba_graph0"),config);
		expected.findVertex("B").setAccept(false);
		Assert.assertNull(WMethod.checkM(expected,loadWithoutMerging(text)));
	}
	
	/** Very simple graph with a state containing no outgoing transitions. */
	@Test
	public final void testLTL_ba_graph0b()
	{
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_init:\n"+
		"if :: (a) -> goto accept_init \n"+
		":: (b) -> goto state_b\n"+
		"fi;\n"+
		"state_b: false; "+
		"}\n";
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B", "testLTL_ba_graph0"),config);
		expected.findVertex("B").setAccept(false);
		Assert.assertNull(WMethod.checkM(expected,loadWithoutMerging(text)));
	}
	
	/** Very simple graph with an unreachable state */
	@Test
	public final void testLTL_ba_graph0c()
	{
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_init:\n"+
		"if :: (a) -> goto accept_init \n"+
		":: (b) -> goto state_b\n"+
		"fi;\n"+
		"state_b: if :: (a) -> goto state_b fi;"+
		"state_unreachable: false;\n"+
		"state_unreachable: false;\n"+
		"}\n";
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B-a->B", "testLTL_ba_graph0"),config);
		expected.findVertex("B").setAccept(false);
		Assert.assertNull(WMethod.checkM(expected,loadWithoutMerging(text)));
	}
	
	/** Absent initial state */
	@Test
	public final void testLTL_ba_no_initial()
	{
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_A:\n"+
		"if :: (a) -> goto accept_A \n"+
		":: (b) -> goto state_b\n"+
		"fi;\n"+
		"state_b: if :: (a) -> goto state_b fi;"+
		"}\n";
		checkForCorrectException(new whatToRun() { public void run() {
			ba.parse(text);ba.buildDeterministicGraph(ba.matrix);
		}},IllegalArgumentException.class,"absent initial state");
	}
	
	/** Conjunction of labels */
	@Test
	public final void testLTL_ba_graph1()
	{
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_init:\n"+
		"if :: (!a && b) -> goto accept_init fi;\n"+
		"}\n";
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A", "testLTL_ba_graph1"),config);
		Assert.assertNull(WMethod.checkM(expected,loadWithoutMerging(text)));
	}
	
	/** Conjunction of labels */
	@Test
	public final void testLTL_ba_graph2()
	{
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_init: if\n"+
		":: (!a && !b) -> goto accept_init \n"+
		"fi;}\n";
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-c->A", "testLTL_ba_graph2"),config);
		Assert.assertNull(WMethod.checkM(expected,loadWithoutMerging(text)));
	}
	
	/** Conjunction of labels */
	@Test
	public final void testLTL_ba_graph3()
	{
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_init: if\n"+
		":: (a) -> goto accept_init \n"+
		":: (c) -> goto accept_A\n"+
		"fi;\n"+
		"accept_A: if\n"+
		":: (b) -> goto accept_A\n"+
		"fi;}\n";
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B", "testLTL_ba_graph3"),config);
		Assert.assertNull(WMethod.checkM(expected,loadWithoutMerging(text)));
	}
	
	/** Conjunction of labels */
	@Test
	public final void testLTL_ba_graph4()
	{
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_init: if\n"+
		":: (a) -> goto accept_init \n"+
		":: (c) -> goto accept_A\n"+
		"fi;\n"+
		"accept_A: if\n"+
		":: (!b && b) -> goto accept_A\n"+
		"fi;}\n";
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B", "testLTL_ba_graph4"),config);
		Assert.assertNull(WMethod.checkM(expected,loadWithoutMerging(text)));
	}
	
	/** Conjunction of labels */
	@Test
	public final void testLTL_ba_graph5()
	{
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_init: if\n"+
		":: (a) -> goto accept_init \n"+
		":: (c) -> goto accept_A\n"+
		"fi;\n"+
		"accept_A: if\n"+
		":: (1) -> goto accept_A\n"+
		"fi;}\n";
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-a->B\nB-b->B\nB-c->B", "testLTL_ba_graph4"),config);
		Assert.assertNull(WMethod.checkM(expected,loadWithoutMerging(text)));
	}
	
	/** Testing of a routine to turn a non-deterministic graph into a deterministic one. */
	@Test
	public final void testLTL_ba_nd1()
	{
		final String init=LTL_to_ba.initStateName;
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_"+init+": if\n"+
		":: (a) -> goto accept_D \n"+
		":: (a) -> goto accept_C \n"+
		":: (b) -> goto accept_B \n"+
		":: (c) -> goto accept_D\n"+
		"fi;\n"+
		"accept_B: if\n"+
		":: (a) -> goto accept_C \n"+
		"fi;\n"+
		"accept_C: if\n"+
		":: (b) -> goto accept_G\n"+
		"fi;\n"+
		"accept_D: if\n"+
		":: (b) -> goto accept_F \n"+
		":: (c) -> goto accept_E\n"+
		"fi;\n"+
		"accept_E: if\n"+
		":: (a) -> goto accept_B\n"+
		"fi;\n"+
		"accept_F: if\n"+
		":: (a) -> goto accept_"+init+"\n"+
		"fi;\n"+
		"accept_G: if\n"+
		":: (a) -> goto accept_B\n"+
		"fi;}\n";
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph(
				"A-c->D\nA-a->CD\nA-b->B\n"+
				"D-b->F-a->A\n"+"D-c->E\n"+
				"B-a->C-b->G-a->B\n"+
				"CD-b->FG\nCD-c->E-a->B\n"+
				"FG-a->AB\n"+
				"AB-a->CD\nAB-b->B\nAB-c->D\n"
				, "testLTL_ba_nd1"),config);
		Assert.assertNull(WMethod.checkM(expected,loadWithoutMerging(text)));
	}
	
	@Test
	public final void testLTL_ba_nd2()
	{
		final String init=LTL_to_ba.initStateName;
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_"+init+": if\n"+
		":: (a) -> goto accept_A \n"+
		":: (a) -> goto accept_D \n"+
		"fi;\n"+
		"accept_A: if\n"+
		":: (a) -> goto accept_B \n"+
		"fi;\n"+
		"accept_B: if\n"+
		":: (a) -> goto accept_C\n"+
		":: (b) -> goto accept_B\n"+
		"fi;\n"+
		"accept_C: if\n"+
		":: (a) -> goto accept_"+init+"\n"+
		"fi;\n"+
		"accept_D: if\n"+
		":: (a) -> goto accept_E \n"+
		"fi;\n"+
		"accept_E: if\n"+
		":: (a) -> goto accept_"+init+"\n"+
		":: (c) -> goto accept_E\n"+
		"fi;}\n";
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph(
				"I-a->AD\n"+
				"AD-a->BE\n"+
				"BE-a->CI\n"+
				"CI-a->IAD\n"+
				"IAD-a->ABDE\n"+
				"ABDE-a->IBCE\n"+
				"IBCE-a->IACD\n"+
				"IACD-a->IABDE\n"+
				"IABDE-a->IABCDE\n"+
				"IABCDE-a->IABCDE\n"+
				"BE-b->B-a->C-a->I\n"+"ABDE-b->B\n"+"IBCE-b->B\n"+"IABDE-b->B\n"+"IABCDE-b->B\n"+"B-b->B\n"+
				"BE-c->E-a->I\n"+"ABDE-c->E\n"+"IBCE-c->E\n"+"IABDE-c->E\n"+"IABCDE-c->E\n"+"E-c->E\n"
				, "testLTL_ba_nd2"),config);
		Assert.assertNull(WMethod.checkM(expected,loadWithoutMerging(text)));
	}

	/** Inconsistent accept/reject labelling between states F and G. */
	@Test
	public final void testLTL_ba_nd_fail1()
	{
		final String init=LTL_to_ba.initStateName;
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_"+init+": if\n"+
		":: (a) -> goto accept_D \n"+
		":: (a) -> goto accept_C \n"+
		":: (b) -> goto accept_B \n"+
		":: (c) -> goto accept_D\n"+
		"fi;\n"+
		"accept_B: if\n"+
		":: (a) -> goto accept_C \n"+
		"fi;\n"+
		"accept_C: if\n"+
		":: (b) -> goto accept_G\n"+
		"fi;\n"+
		"accept_D: if\n"+
		":: (b) -> goto reject_F \n"+
		":: (c) -> goto accept_E\n"+
		"fi;\n"+
		"accept_E: if\n"+
		":: (a) -> goto accept_B\n"+
		"fi;\n"+
		"reject_F: if\n"+
		":: (a) -> goto accept_"+init+"\n"+
		"fi;\n"+
		"accept_G: if\n"+
		":: (a) -> goto accept_B\n"+
		"fi;}\n";
		checkForCorrectException(new whatToRun() { public void run() { 
			ba.parse(text);ba.buildDeterministicGraph(ba.matrix);
		}},IllegalArgumentException.class,"inconsistent labelling");
	}
	
	/** Inconsistent accept/reject labelling between states A (initial) and B. */
	@Test
	public final void testLTL_ba_nd_fail2()
	{
		final String init=LTL_to_ba.initStateName;
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_"+init+": if\n"+
		":: (a) -> goto accept_D \n"+
		":: (a) -> goto accept_C \n"+
		":: (b) -> goto reject_B \n"+
		":: (c) -> goto accept_D\n"+
		"fi;\n"+
		"reject_B: if\n"+
		":: (a) -> goto accept_C \n"+
		"fi;\n"+
		"accept_C: if\n"+
		":: (b) -> goto accept_G\n"+
		"fi;\n"+
		"accept_D: if\n"+
		":: (b) -> goto accept_F \n"+
		":: (c) -> goto accept_E\n"+
		"fi;\n"+
		"accept_E: if\n"+
		":: (a) -> goto reject_B\n"+
		"fi;\n"+
		"accept_F: if\n"+
		":: (a) -> goto accept_"+init+"\n"+
		"fi;\n"+
		"accept_G: if\n"+
		":: (a) -> goto reject_B\n"+
		"fi;}\n";
		checkForCorrectException(new whatToRun() { public void run() { 
			ba.parse(text);ba.buildDeterministicGraph(ba.matrix);
		}},IllegalArgumentException.class,"inconsistent labelling");
	}
	
	/** A very simple automaton. */
	@Test
	public final void testLTL_bigger()
	{
		ba=new LTL_to_ba(config,null);ba.alphabet = new HashSet<String>();
		ba.alphabet.addAll(Arrays.asList(new String[]{"load","save","edit","close"}));
		LearnerGraph actual = loadWithoutMerging("never { /* (G((close)-> X((load) R !((save) || (edit) || (close))))) */\n\n\n"+
				"			accept_init :    /* init */\n"+
				"				if\n"+
				"				:: (!close) -> goto accept_init\n"+
				"				:: (1) -> goto accept_S2\n"+
				"				fi;\n"+
				"			accept_S2 :    /* 1 */\n"+
				"				if\n"+
				"				:: (!close && !edit && !save) -> goto accept_S2\n"+
				"				:: (!close && !edit && !save && load) -> goto accept_init\n"+
				"				fi;\n"+
				"			}\n");
		Assert.assertNull(WMethod.checkM(actual,expectedFromASEExample));
	}
	
	/** This one is an integration test of ltl2ba. First test : syntax error. */
	@Test
	public final void testLTL_integration_syntax()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			ba.runLTL2BA("&");
		}},IllegalArgumentException.class,"syntax");
	}
	
	/** This one is an integration test of ltl2ba. Second test : empty automaton. */
	@Test
	public final void testLTL_integration_empty()
	{
		ba=new LTL_to_ba(config,null);ba.alphabet = new HashSet<String>();
		ba.alphabet.addAll(Arrays.asList(new String[]{"load","save","edit","close"}));
		LearnerGraph expected = new LearnerGraph(config);expected.init.setAccept(false);
		ba.runLTL2BA("false");
		Assert.assertNull(WMethod.checkM(ba.buildDeterministicGraph(ba.matrix),expected));
	}
	
	/** This one is an integration test of ltl2ba. Third test : a bigger automaton. */
	@Test
	public final void testLTL_integration_bigger()
	{
		ba=new LTL_to_ba(config,null);ba.alphabet = new HashSet<String>();
		ba.alphabet.addAll(Arrays.asList(new String[]{"load","save","edit","close"}));
		ba.runLTL2BA("([]((close)-> X((load) V !((save) || (edit) || (close)))))");
		Assert.assertNull(WMethod.checkM(ba.buildDeterministicGraph(ba.matrix),expectedFromASEExample));
	}
	
	/** This one is an integration test of ltl2ba. Third test : completing a bigger automaton. */
	@Test
	public final void testLTL_integration_bigger2()
	{
		ba=new LTL_to_ba(config,null);ba.alphabet = new HashSet<String>();
		ba.alphabet.addAll(Arrays.asList(new String[]{"load","save","edit","close"}));
		ba.runLTL2BA("([]((close)-> X((load) V !((save) || (edit) || (close)))))");
		LearnerGraph result = ba.buildDeterministicGraph(ba.completeMatrix(ba.buildDeterministicGraph(ba.matrix)));
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph(
				"I-close->1\nI-edit->I1\nI-save->I1\nI-load->I1\n"+
				"1-load->I1-close->1\n"+
				"I1-edit->I1-save->I1-load->I1\n"+
				"1-edit-#R1\n"+"1-save-#R2\n"+"1-close-#R3\n"
				,"testLTL_bigger"),config);
		Assert.assertNull(WMethod.checkM(expected,result));
	}

	@Test
	public final void testLTL_complete2()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init-c->B-b->B", "testLTL_ba_graph3"),config);
		LearnerGraph result = ba.buildDeterministicGraph(ba.completeMatrix(graph));
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B\n"+
				"A-b-#R1\n"+"B-a-#R2\n"+"B-c-#R3"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(expected,result));
	}
	
	/** Tests of adding to graph. */
	@Test
	public final void testLTL_add1()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-c->B-b->B", "testLTL_add1"),config);
		
		Map<CmpVertex,Map<String,List<CmpVertex>>> matrix = new TreeMap<CmpVertex,Map<String,List<CmpVertex>>>();
		LearnerGraphND.buildForward(graph,LearnerGraphND.ignoreNone,matrix);
		LTL_to_ba.addFromMatrix(matrix,graphToAdd);
		LearnerGraph result = ba.buildDeterministicGraph(matrix);
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B\n"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(expected,result));
	}
	
	/** Tests of adding to graph. */
	@Test
	public final void testLTL_add2()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1"),config);
		
		Map<CmpVertex,Map<String,List<CmpVertex>>> matrix = new TreeMap<CmpVertex,Map<String,List<CmpVertex>>>();
		LearnerGraphND.buildForward(graph,LearnerGraphND.ignoreNone,matrix);
		LTL_to_ba.addFromMatrix(matrix,graphToAdd);
		LearnerGraph result = ba.buildDeterministicGraph(matrix);
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(expected,result));
	}
	
	/** Tests of adding to graph. */
	@Test
	public final void testLTL_add3()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init-b->S", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1"),config);
		
		Map<CmpVertex,Map<String,List<CmpVertex>>> matrix = new TreeMap<CmpVertex,Map<String,List<CmpVertex>>>();
		LearnerGraphND.buildForward(graph,LearnerGraphND.ignoreNone,matrix);
		LTL_to_ba.addFromMatrix(matrix,graphToAdd);
		LearnerGraph result = ba.buildDeterministicGraph(matrix);
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"+
				"A-b->S"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(expected,result));
	}
	
	/** Tests of adding to graph. */
	@Test
	public final void testLTL_add4()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init-d->S", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1"),config);
		
		Map<CmpVertex,Map<String,List<CmpVertex>>> matrix = new TreeMap<CmpVertex,Map<String,List<CmpVertex>>>();
		LearnerGraphND.buildForward(graph,LearnerGraphND.ignoreNone,matrix);
		LTL_to_ba.addFromMatrix(matrix,graphToAdd);
		LearnerGraph result = ba.buildDeterministicGraph(matrix);
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(expected,result));
	}
	
	/** Tests the all-final states property. */
	@Test
	public final void testLTL_integration_subsystem_nonfinal()
	{
		ba=new LTL_to_ba(config,null);ba.alphabet = new HashSet<String>();
		ba.alphabet.addAll(Arrays.asList(new String[]{"load","save","edit","close","open"}));
		checkForCorrectException(new whatToRun() { public void run() {
			ba.ltlToBA(Arrays.asList(new String[]{"([](close-><>open))"}));
			ba.buildDeterministicGraph(ba.matrix);
		}},IllegalArgumentException.class,"not all states are accept");
	}
	@Test
	public final void testLTL_integration_subsystem()
	{
		ba=new LTL_to_ba(config,new LearnerGraph(TestFSMAlgo.buildGraph("A-load->B-edit->C-edit->D-save->E-close->F", "testLTL_integration_subsystem"),config));ba.alphabet = new HashSet<String>();
		ba.alphabet.addAll(Arrays.asList(new String[]{"load","save","edit","close"}));
		ba.ltlToBA(Arrays.asList(new String[]{"([]((close)-> X((load) V !((save) || (edit) || (close)))))"}));
		Assert.assertNull(WMethod.checkM(ba.buildDeterministicGraph(ba.matrix),expectedFromASEExample));
	}
}
