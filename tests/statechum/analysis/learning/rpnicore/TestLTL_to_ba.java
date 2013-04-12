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

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.Label;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.LTL_to_ba.OPERATION;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.apps.QSMTool;
import statechum.collections.TreeMapWithSearch;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraphND;

import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;

/**
 * @author kirill
 *
 */
public class TestLTL_to_ba 
{
	/** Converts arrays of labels to lists of labels using config - it does not really matter which configuration is used 
	 * because all of them start from a default one and do not modify label type.
	 * 
	 * @param labels what to convert
	 * @return the outcome of conversion.
	 */
	protected List<Label> labelList(String [] labels)
	{
		return AbstractLearnerGraph.buildList(Arrays.asList(labels),config,converter);
	}
	
	private void ConstructAlphabet(LTL_to_ba baArg,String[] labels)
	{
		baArg.setAlphabet(labelList(labels));
	}
	
	@Before
	public final void beforeTest()
	{
		config = Configuration.getDefaultConfiguration().copy();		
		ba=new LTL_to_ba(config,converter);ConstructAlphabet(ba,new String[]{"a","b","c"});
		
		expectedFromASEExample = buildLearnerGraph(
				"I-close->1\nI-edit->I1\nI-save->I1\nI-load->I1\n"+
				"1-load->I1-close->1\n"+
				"I1-edit->I1-save->I1-load->I1\n","testLTL_bigger",config,converter);
	}
	// ,"load","save","edit","close"
	protected Configuration config = null;
	protected ConvertALabel converter = null;
	
	protected LTL_to_ba ba = null;
	protected LearnerGraph expectedFromASEExample = null;
	
	@Test
	public final void testLtlConcat1a()
	{
		Assert.assertEquals("",LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{})).toString());
	}
	
	@Test
	public final void testLtlConcat1b()
	{
		Assert.assertEquals("",LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{QSMTool.cmdIFTHENAUTOMATON+" junk","junk"})).toString());
	}
	
	@Test
	public final void testLtlConcat1c()
	{
		LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{"  */ ",QSMTool.cmdIFTHENAUTOMATON+"  */ "}));
	}
	
	@Test
	public final void testLtlConcat2()
	{
		Assert.assertEquals("a",LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{QSMTool.cmdLTL+" a ",QSMTool.cmdLTL+"",QSMTool.cmdLTL+"  "})).toString());
	}
	
	@Test
	public final void testLtlConcat3()
	{
		Assert.assertEquals("a || b",LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{QSMTool.cmdLTL+" a ",QSMTool.cmdLTL+"",QSMTool.cmdLTL+"  ",QSMTool.cmdLTL+"  b ",QSMTool.cmdLTL+" "})).toString());
	}
	
	@Test
	public final void testLtlConcat4()
	{
		Assert.assertEquals("a || test || c",LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{
				QSMTool.cmdLTL+" a ",QSMTool.cmdLTL+"",QSMTool.cmdLTL+"  ",QSMTool.cmdLTL+"  test ",QSMTool.cmdLTL+" c "})).toString());
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void testLTL_fail1()
	{
		LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{QSMTool.cmdLTL+"  */ "}));
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testLTL_fail2()
	{
		LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{QSMTool.cmdLTL+"  /* "}));
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testLTL_fail3()
	{
		LTL_to_ba.concatenateLTL(Arrays.asList(new String[]{QSMTool.cmdLTL+" a ",QSMTool.cmdLTL+" ",QSMTool.cmdLTL+" b", QSMTool.cmdLTL+" this is /* blah ) blah blah"}));
	}
	

	/** Tests that finding an existing state retains it. */
	@Test
	public final void testFindInitialState1()
	{
		LearnerGraphND graph = buildLearnerGraphND("Avertex-a->Bvertex-b->Cvertex", "testFindInitialState1",config,converter);
		Map<String,CmpVertex> map = new TreeMapWithSearch<String,CmpVertex>(10);for(CmpVertex vert:graph.transitionMatrix.keySet()) map.put(vert.getStringId(), vert);
		Assert.assertEquals("Avertex", LTL_to_ba.findInitialState("A",map).getStringId());
	}
	
	/** Tests a search of a different initial state. */
	@Test
	public final void testFindInitialState2()
	{
		LearnerGraphND graph = buildLearnerGraphND("Avertex-a->Bvertex-b->Cvertex", "testFindInitialState1",config,converter);
		Map<String,CmpVertex> map = new TreeMapWithSearch<String,CmpVertex>(10);for(CmpVertex vert:graph.transitionMatrix.keySet()) map.put(vert.getStringId(), vert);
		Assert.assertEquals("Bvertex", LTL_to_ba.findInitialState("B",map).getStringId());
	}
	
	/** Tests that a search for an initial state may fail. */
	@Test
	public final void testFindInitialState_fail1()
	{
		final LearnerGraphND graph = buildLearnerGraphND("Avertex-a->Bvertex-b->Cvertex", "testFindInitialState1",config,converter);
		final Map<String,CmpVertex> map = new TreeMapWithSearch<String,CmpVertex>(10);for(CmpVertex vert:graph.transitionMatrix.keySet()) map.put(vert.getStringId(), vert);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			LTL_to_ba.findInitialState("Q",map);
		}}, IllegalArgumentException.class,"missing state");
	}
	
	/** Tests that a search for an initial state may fail, using an empty string which matches the first vertex encountered. */
	@Test
	public final void testFindInitialState_fail2()
	{
		final LearnerGraphND graph = buildLearnerGraphND("Avertex-a->Bvertex-b->Cvertex", "testCopyVertex",config,converter);
		Map<String,CmpVertex> map = new TreeMapWithSearch<String,CmpVertex>(10);for(CmpVertex vert:graph.transitionMatrix.keySet()) map.put(vert.getStringId(), vert);
		Assert.assertEquals("Avertex",graph.getInit().getStringId());
		Assert.assertNotNull(LTL_to_ba.findInitialState("",map));// this may choose any vertex, we rely on it choosing the smallest one lexicographically via the use of TreeMap
		Assert.assertEquals("Avertex", LTL_to_ba.findInitialState("",map).getStringId());
	}
	
	@Test
	public final void testLTL_ba_fail1()
	{
		final String text=LTL_to_ba.baError+":";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"syntax");
	}
	
	@Test
	public final void testLTL_ba_fail2()
	{
		final String text="this is a test\nError:";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"failed to lex");
	}
	
	@Test
	public final void testLTL_ba_fail3()
	{
		final String text="/* some text */";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"failed to find the start of automaton");
	}
	
	@Test
	public final void testLTL_ba_fail4()
	{
		final String text="never { /* (G((close)-> X((load) R !((save) || (edit) || (close))))) */\n\n\n"+
		"junk"
		;
		checkForCorrectException(new whatToRun() { public @Override void run() {
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
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"expected if");
	}
	
	/** No expression what a state should behave like. */
	@Test
	public final void testLTL_ba_fail6a()
	{
		final String text="never { /* (G((close)-> X((load) R !((save) || (edit) || (close))))) */\n\n\n"+
		"some_state : \n"+
		"if\n"+
		"fi;\n"+
		"more_state:\n"+"text:"
		;
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"expected if");
	}
	
	/** Invalid description of a state. */
	@Test
	public final void testLTL_ba_fail6b()
	{
		final String text="never { /* (G((close)-> X((load) R !((save) || (edit) || (close))))) */\n\n\n"+
		"some_state : \n"+
		"if\n"+
		"fi;\n"+
		"more_state:\njunk\n"+"text:"
		;
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"failed to lex starting from \"junk");
	}
	
	/** skip command followed with a semicolon - this might be a correct promela expression but we parse a 
	 * restricted subset of it.
	 */
	@Test
	public final void testLTL_ba_fail6c()
	{
		final String text="never { /* (G((close)-> X((load) R !((save) || (edit) || (close))))) */\n\n\n"+
		"some_state : \n"+
		"if\n"+
		"fi;\n"+
		"more_state:\nskip;\n"+"text:"
		;
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.parse(text);
		}},IllegalArgumentException.class,"failed to lex starting from \";");
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
		checkForCorrectException(new whatToRun() { public @Override void run() {
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
		checkForCorrectException(new whatToRun() { public @Override void run() {
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
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.interpretInputLabel("d");
		}},IllegalArgumentException.class,"unrecognised label");
	}
	
	@Test
	public final void testLabelInterpretationA()
	{
		Set<Label> set = ba.interpretInputLabel("a");
		Assert.assertEquals(1, set.size());Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("a",config),set.iterator().next());
	}
	
	@Test
	public final void testLabelInterpretationB()
	{
		Set<Label> set = ba.interpretInputLabel("1");
		Assert.assertEquals(ba.alphabet.keySet(), set);
	}
	
	/** Checks that a very simple expression applied to the initial data returns the expected value.
	 * 
	 * @param expected expected result
	 * @param initial initial data
	 * @param oper operation to apply
	 * @param label label to use
	 */
	public void checkOperation(String [] expected,Label [] initial,OPERATION oper, String label)
	{
		Set<Label> actual = new TreeSet<Label>();actual.addAll(Arrays.asList(initial));
		ba.performOperation(actual, oper, ba.interpretInputLabel(label));
		
		checkOutcome(expected,actual);
	}
	
	public void checkOutcome(String [] expected, Set<Label> actual)
	{
		Set<Label> exp = new TreeSet<Label>();exp.addAll(labelList(expected));
		Assert.assertEquals(exp,actual);
	}
	
	/** Tests operations. */
	@Test
	public final void testOp1()
	{
		checkOperation(new String[]{"a"},new Label[]{},OPERATION.ASSIGN,"a");
	}

	protected Label [] getAlphabet()
	{
		Label[] result = new Label[ba.alphabet.size()];
		ba.alphabet.keySet().toArray(result);
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
		checkOperation(new String[]{"b","c"},new Label[]{AbstractLearnerGraph.generateNewLabel("c",config)},OPERATION.OR,"b");
		checkOperation(new String[]{},new Label[]{AbstractLearnerGraph.generateNewLabel("c",config)},OPERATION.AND,"b");
	}

	protected Set<Label> interpretUnary(String str)
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
		checkOutcome(new String[]{"a"},ba.computeAlphabet("a"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr2()
	{
		checkOutcome(new String[]{"a","b","c"},ba.interpretString("1"));
		checkOutcome(new String[]{"1"},ba.computeAlphabet("1"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr3()
	{
		checkOutcome(new String[]{},ba.interpretString("!1"));
		checkOutcome(new String[]{"1"},ba.computeAlphabet("!1"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr4()
	{
		checkOutcome(new String[]{"a"},ba.interpretString("a && a"));
		checkOutcome(new String[]{"a"},ba.computeAlphabet("a && a"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr5()
	{
		checkOutcome(new String[]{},ba.interpretString("a && b"));
		checkOutcome(new String[]{"a","b"},ba.computeAlphabet("a && b"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr6()
	{
		checkOutcome(new String[]{"a","b"},ba.interpretString("a || b"));
		checkOutcome(new String[]{"a","b"},ba.computeAlphabet("a || b"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr7()
	{
		checkOutcome(new String[]{"c"},ba.interpretString("a && b || c"));
		checkOutcome(new String[]{"a","b","c"},ba.computeAlphabet("a && b || c"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr8()
	{
		checkOutcome(new String[]{"a"},ba.interpretString("(a)"));
		checkOutcome(new String[]{"a"},ba.computeAlphabet("(a)"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr9()
	{
		checkOutcome(new String[]{"a"},ba.interpretString("((a))"));
		checkOutcome(new String[]{"a"},ba.computeAlphabet("((a))"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr10()
	{
		checkOutcome(new String[]{"a","b"},ba.interpretString("((a) || b)"));
		checkOutcome(new String[]{"a","b"},ba.computeAlphabet("((a) || b)"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr11()
	{
		checkOutcome(new String[]{"a","b"},ba.interpretString("((a || (b)))"));
		checkOutcome(new String[]{"a","b"},ba.computeAlphabet("((a || (b)))"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr12a()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.interpretString("");
		}},IllegalArgumentException.class,"unexpected end");
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.computeAlphabet("");
		}},IllegalArgumentException.class,"unexpected end");
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr12b()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.interpretString("()");
		}},IllegalArgumentException.class,"unexpected end");

		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.computeAlphabet("()");
		}},IllegalArgumentException.class,"unexpected end");
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr12c()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.interpretString("b && (!())");
		}},IllegalArgumentException.class,"unexpected end");
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.computeAlphabet("b && (!())");
		}},IllegalArgumentException.class,"unexpected end");
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr13()
	{
		checkOutcome(new String[]{"c","b"},ba.interpretString("((!a || (b)))"));
		checkOutcome(new String[]{"a","b"},ba.computeAlphabet("((!a || (b)))"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr14()
	{
		checkOutcome(new String[]{"c"},ba.interpretString("!((a || (b)))"));
		checkOutcome(new String[]{"a","b"},ba.computeAlphabet("!((a || (b)))"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr15()
	{
		checkOutcome(new String[]{"c"},ba.interpretString("((1 || (b)) && c)"));
		checkOutcome(new String[]{"1","b","c"},ba.computeAlphabet("((1 || (b)) && c)"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr16()
	{
		checkOutcome(new String[]{},ba.interpretString("(!(1 || (b)) && c)"));
		checkOutcome(new String[]{"1","b","c"},ba.computeAlphabet("(!(1 || (b)) && c)"));
	}

	/** Tests parser of expressions. */
	@Test
	public final void testExpr17()
	{
		checkOutcome(new String[]{"c"},ba.interpretString("(!(a || (b)) && c)"));
		checkOutcome(new String[]{"c","a","b"},ba.computeAlphabet("(!(a || (b)) && c)"));
	}
	
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr18()
	{
		checkOutcome(new String[]{"a"},ba.interpretString("a && !!!b"));
		checkOutcome(new String[]{"a","b"},ba.computeAlphabet("a && !!!b"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr19()
	{
		checkOutcome(new String[]{"a"},ba.interpretString("a && !!(!b && !((b && 1)))"));
		checkOutcome(new String[]{"a","b","1"},ba.computeAlphabet("a && !!(!b && !((b && 1)))"));
	}
	
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr20()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.interpretString("a | b");
		}},IllegalArgumentException.class,"failed to lex ");
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.computeAlphabet("a | b");
		}},IllegalArgumentException.class,"failed to lex ");
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr21()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.interpretString("a || || b");
		}},IllegalArgumentException.class,"expected word ");
		
		checkOutcome(new String[]{"a","b"},ba.computeAlphabet("a || || b"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr22()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.interpretString("a || && b");
		}},IllegalArgumentException.class,"expected word ");

		checkOutcome(new String[]{"a","b"},ba.computeAlphabet("a || && b"));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr23a()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.interpretString("a || b c ");
		}},IllegalArgumentException.class,"expected binary operation ");

		checkOutcome(new String[]{"a","b","c"},ba.computeAlphabet("a || b c "));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr23b()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.interpretString("a || b (c) ");
		}},IllegalArgumentException.class,"expected binary operation ");

		checkOutcome(new String[]{"a","b","c"},ba.computeAlphabet("a || b (c) "));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr23c()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.interpretString("a || b !(c) ");
		}},IllegalArgumentException.class,"expected binary operation ");

		checkOutcome(new String[]{"a","b","c"},ba.computeAlphabet("a || b !(c) "));
	}
	
	/** Tests parser of expressions. */
	@Test
	public final void testExpr24()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.interpretString("a || b)");
		}},IllegalArgumentException.class,"extra tokens at the end ");

		checkForCorrectException(new whatToRun() { public @Override void run() {
			checkOutcome(new String[]{"a","b"},ba.computeAlphabet("a || b)"));
		}},IllegalArgumentException.class,"extra tokens at the end ");
	}
	
	/** Converts LTL into a deterministic automaton without adding reject-transitions. */
	protected LearnerGraph loadLTLFromOutputOfLTL2BA(String text)
	{
		ba.parse(text);
		LearnerGraph result = null;
		synchronized(AbstractLearnerGraph.syncObj)
		{
			try {
				result = ba.matrixFromLTL.pathroutines.buildDeterministicGraph();
			} catch (IncompatibleStatesException e) {
				Helper.throwUnchecked("could not build deterministic graph", e);
			}
		}
		return result;
	}
	
	@Test
	public final void testLTL_ba_empty()
	{
		final String text="never { /* (G((close)-> X((load) R !((save) || (edit) || (close))))) */\n\n\n"+
		"state_init: false;\n}\n"
		;
		LearnerGraph expected = new LearnerGraph(config);expected.getInit().setAccept(false);
		Assert.assertNull(WMethod.checkM(expected,loadLTLFromOutputOfLTL2BA(text)));
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
		LearnerGraph expected = buildLearnerGraph("A-a->A-b->B-a->B", "testLTL_ba_graph0",config,converter);
		expected.findVertex("B").setAccept(false);
		Assert.assertNull(WMethod.checkM(expected,loadLTLFromOutputOfLTL2BA(text)));
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
		"state_b: false;\n"+
		"}\n";
		LearnerGraph expected = buildLearnerGraph("A-a->A-b->B", "testLTL_ba_graph0",config,converter);
		expected.findVertex("B").setAccept(false);
		Assert.assertNull(WMethod.checkM(expected,loadLTLFromOutputOfLTL2BA(text)));
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
		LearnerGraph expected = buildLearnerGraph("A-a->A-b->B-a->B", "testLTL_ba_graph0",config,converter);
		expected.findVertex("B").setAccept(false);
		Assert.assertNull(WMethod.checkM(expected,loadLTLFromOutputOfLTL2BA(text)));
	}
	
	/** Very simple graph with a skip (accepting all inputs) state */
	@Test
	public final void testLTL_ba_graph0d()
	{
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_init:\n"+
		"if :: (a) -> goto accept_init \n"+
		":: (b) -> goto state_b\n"+
		"fi;\n"+
		"state_b: skip\n"+
		"}\n";
		LearnerGraph expected = buildLearnerGraph("A-a->A-b->B-a->B-b->B-c->B", "testLTL_ba_graph0",config,converter);
		expected.findVertex("B").setAccept(false);
		Assert.assertNull(WMethod.checkM(expected,loadLTLFromOutputOfLTL2BA(text)));
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
		checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException {
			ba.parse(text);ba.matrixFromLTL.pathroutines.buildDeterministicGraph();
		}},IllegalArgumentException.class,"missing state");
	}
	
	/** Conjunction of labels */
	@Test
	public final void testLTL_ba_graph1()
	{
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_init:\n"+
		"if :: (!a && b) -> goto accept_init fi;\n"+
		"}\n";
		LearnerGraph expected = buildLearnerGraph("A-b->A", "testLTL_ba_graph1",config,converter);
		Assert.assertNull(WMethod.checkM(expected,loadLTLFromOutputOfLTL2BA(text)));
	}
	
	/** Conjunction of labels */
	@Test
	public final void testLTL_ba_graph2()
	{
		final String text=LTL_to_ba.baStart+"{\n"+
		"accept_init: if\n"+
		":: (!a && !b) -> goto accept_init \n"+
		"fi;}\n";
		LearnerGraph expected = buildLearnerGraph("A-c->A", "testLTL_ba_graph2",config,converter);
		Assert.assertNull(WMethod.checkM(expected,loadLTLFromOutputOfLTL2BA(text)));
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
		LearnerGraph expected = buildLearnerGraph("A-a->A-c->B-b->B", "testLTL_ba_graph3",config,converter);
		Assert.assertNull(WMethod.checkM(expected,loadLTLFromOutputOfLTL2BA(text)));
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
		LearnerGraph expected = buildLearnerGraph("A-a->A-c->B", "testLTL_ba_graph4",config,converter);
		Assert.assertNull(WMethod.checkM(expected,loadLTLFromOutputOfLTL2BA(text)));
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
		LearnerGraph expected = buildLearnerGraph("A-a->A-c->B-a->B\nB-b->B\nB-c->B", "testLTL_ba_graph4",config,converter);
		Assert.assertNull(WMethod.checkM(expected,loadLTLFromOutputOfLTL2BA(text)));
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
		LearnerGraph expected = buildLearnerGraph(
				"A-c->D\nA-a->CD\nA-b->B\n"+
				"D-b->F-a->A\n"+"D-c->E\n"+
				"B-a->C-b->G-a->B\n"+
				"CD-b->FG\nCD-c->E-a->B\n"+
				"FG-a->AB\n"+
				"AB-a->CD\nAB-b->B\nAB-c->D\n"
				, "testLTL_ba_nd1",config,converter);
		Assert.assertNull(WMethod.checkM(expected,loadLTLFromOutputOfLTL2BA(text)));
	}
	
	/** construction of deterministic graphs from non-deterministic ones. */
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
		LearnerGraph expected = buildLearnerGraph(
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
				, "testLTL_ba_nd2",config,converter);
		Assert.assertNull(WMethod.checkM(expected,loadLTLFromOutputOfLTL2BA(text)));
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
		checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException { 
			ba.parse(text);ba.matrixFromLTL.pathroutines.buildDeterministicGraph();
		}},IncompatibleStatesException.class,"cannot add state");
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
		checkForCorrectException(new whatToRun() { public @Override void run() throws IncompatibleStatesException { 
			ba.parse(text);ba.matrixFromLTL.pathroutines.buildDeterministicGraph();
		}},IncompatibleStatesException.class,"cannot add state");
	}
	
	/** A very simple automaton. */
	@Test
	public final void testLTL_bigger()
	{
		ba=new LTL_to_ba(config,converter);
		ConstructAlphabet(ba,new String[]{"load","save","edit","close"});
		LearnerGraph actual = loadLTLFromOutputOfLTL2BA("never { /* (G((close)-> X((load) R !((save) || (edit) || (close))))) */\n\n\n"+
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

	/** This one is an integration test of ltl2ba. First test : cannot run ltl2ba. */
	@Test
	public final void testLTL_integration_cannotrun()
	{
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			try
			{
				
				ba.runLTL2BA("&","junk");
			}
			finally
			{
				
			}
		}},IllegalArgumentException.class,"failed to run ltl2ba");
	}
	

	/** This one is an integration test of ltl2ba. First test : syntax error. */
	@Test
	public final void testLTL_integration_syntax()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.runLTL2BA("&",GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LTL2BA));
		}},IllegalArgumentException.class,"syntax");
	}
	
	/** This one is an integration test of ltl2ba. Second test : empty automaton. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testLTL_integration_empty() throws IncompatibleStatesException
	{
		ba=new LTL_to_ba(config,converter);
		ConstructAlphabet(ba,new String[]{"load","save","edit","close"});
		LearnerGraph expected = new LearnerGraph(config);expected.getInit().setAccept(false);
		ba.runLTL2BA("false",GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LTL2BA));
		Assert.assertNull(WMethod.checkM(expected,ba.matrixFromLTL.pathroutines.buildDeterministicGraph()));
	}
	
	/** This one is an integration test of ltl2ba. Third test : a bigger automaton. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testLTL_integration_bigger() throws IncompatibleStatesException
	{
		ba=new LTL_to_ba(config,converter);
		ConstructAlphabet(ba,new String[]{"load","save","edit","close"});
		ba.runLTL2BA("([]((close)-> X((load) V !((save) || (edit) || (close)))))",GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LTL2BA));
		Assert.assertNull(WMethod.checkM(expectedFromASEExample,ba.matrixFromLTL.pathroutines.buildDeterministicGraph()));
	}
	
	/** This one is an integration test of ltl2ba. Third test : completing a bigger automaton. 
	 * @throws IncompatibleStatesException */
	@Test
	public final void testLTL_integration_bigger2() throws IncompatibleStatesException
	{
		ba=new LTL_to_ba(config,converter);
		ConstructAlphabet(ba,new String[]{"load","save","edit","close"});
		ba.runLTL2BA("([]((close)-> X((load) V !((save) || (edit) || (close)))))",GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LTL2BA));
		LearnerGraph result = new LearnerGraph(ba.matrixFromLTL.config);
		AbstractPathRoutines.completeMatrix(ba.matrixFromLTL.pathroutines.buildDeterministicGraph(),result);
		LearnerGraph expected = buildLearnerGraph(
				"I-close->1\nI-edit->I1\nI-save->I1\nI-load->I1\n"+
				"1-load->I1-close->1\n"+
				"I1-edit->I1-save->I1-load->I1\n"+
				"1-edit-#R1\n"+"1-save-#R2\n"+"1-close-#R3\n"
				,"testLTL_bigger",config,converter);
		Assert.assertNull(WMethod.checkM(result,expected));
	}

	/** Tests the all-final states property. */
	@Test
	public final void testLTL_integration_subsystem_nonfinal()
	{
		ba=new LTL_to_ba(config,converter);
		ConstructAlphabet(ba,new String[]{"load","save","edit","close","open"});
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ba.ltlToBA(Arrays.asList(new String[]{QSMTool.cmdLTL+" ([](close-><>open))"}),null,false,GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LTL2BA));
		}},IllegalArgumentException.class,"not all states are accept");
	}

	private LearnerGraph automatonLoadedFromLTL(LTL_to_ba ltl)
	{
		LearnerGraph automatonLoadedFromLTL = new LearnerGraph(config);
		synchronized(AbstractLearnerGraph.syncObj)
		{
			try {
				AbstractPathRoutines.completeMatrix(ltl.getLTLgraph().pathroutines.buildDeterministicGraph(),automatonLoadedFromLTL);
			} catch (IncompatibleStatesException e) {
				Helper.throwUnchecked("invalid graph returned by ltl2ba", e);
			}
		}
		return automatonLoadedFromLTL;
	}
	
	/** Augments the supplied graph with the one obtained from LTL. The two graphs are expected 
	 * to have identical alphabets, otherwise LTL graph will be invalid.
	 * 
	 * @param what
	 * @return
	 */
	public LearnerGraph uniteAndDeterminise(LearnerGraph what, LearnerGraph automatonLoadedFromLTL)
	{

		LearnerGraphND automaton = LearnerGraphND.UniteTransitionMatrices(automatonLoadedFromLTL,new LearnerGraphND(what,config));
		LearnerGraph result = null;
		synchronized(AbstractLearnerGraph.syncObj)
		{
			try {
				result = automaton.pathroutines.buildDeterministicGraph();
			} catch (IncompatibleStatesException e) {
				Helper.throwUnchecked("invalid graph returned by ltl2ba", e);
			}
		}
		return result;
	}

	@Test
	public final void testLTL_uniteAndDeterminise()
	{
		ba=new LTL_to_ba(config,converter);
		ConstructAlphabet(ba,new String[]{"load","save","edit","close"});
		LearnerGraph whatToAugment = buildLearnerGraph("A-load->B-edit->C-edit->D-save->E-close->F", "testLTL_integration_subsystem",config,converter);
		ba.ltlToBA(Arrays.asList(new String[]{QSMTool.cmdLTL+" ([]((close)-> X((load) V !((save) || (edit) || (close)))))"}),whatToAugment,false,GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LTL2BA));
		LearnerGraph result = new LearnerGraph(config);
		LearnerGraph automatonLoadedFromLTL = automatonLoadedFromLTL(ba);
		AbstractPathRoutines.removeRejectStates(automatonLoadedFromLTL, result);
		Assert.assertNull(WMethod.checkM(uniteAndDeterminise(whatToAugment,result),expectedFromASEExample));
	}
	
	/** Checks that multiple calls to Augment return the same result. */
	@Test
	public final void testLTL_uniteAndDeterminise2()
	{
		ba=new LTL_to_ba(config,converter);
		ConstructAlphabet(ba,new String[]{"load","save","edit","close"});
		LearnerGraph whatToAugment = buildLearnerGraph("A-load->B-edit->C-edit->D-save->E-close->F", "testLTL_integration_subsystem",config,converter);
		ba.ltlToBA(Arrays.asList(new String[]{QSMTool.cmdLTL+" ([]((close)-> X((load) V !((save) || (edit) || (close)))))"}),whatToAugment,false,GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LTL2BA));
		LearnerGraph result = new LearnerGraph(config);
		LearnerGraph automatonLoadedFromLTL = automatonLoadedFromLTL(ba);
		AbstractPathRoutines.removeRejectStates(automatonLoadedFromLTL, result);
		Assert.assertNull(WMethod.checkM(uniteAndDeterminise(whatToAugment,result),expectedFromASEExample));
		Assert.assertNull(WMethod.checkM(uniteAndDeterminise(whatToAugment,result),expectedFromASEExample));
		Assert.assertNull(WMethod.checkM(uniteAndDeterminise(whatToAugment,result),expectedFromASEExample));
	}
	
	/** Checks that multiple calls to Augment return the same result and that augmenting with a single-state graph returns the internal graph. */
	@Test
	public final void testLTL_uniteAndDeterminise3()
	{
		ba=new LTL_to_ba(config,converter);
		ConstructAlphabet(ba,new String[]{"load","save","edit","close"});
		LearnerGraph whatToAugment = buildLearnerGraph("A-load->B-edit->C-edit->D-save->E-close->F", "testLTL_integration_subsystem",config,converter);
		ba.ltlToBA(Arrays.asList(new String[]{QSMTool.cmdLTL+" ([]((close)-> X((load) V !((save) || (edit) || (close)))))"}),whatToAugment,false,GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LTL2BA));

		LearnerGraph automatonLoadedFromLTL = automatonLoadedFromLTL(ba), expected  = automatonLoadedFromLTL(ba);
		Assert.assertNull(WMethod.checkM(uniteAndDeterminise(new LearnerGraph(Configuration.getDefaultConfiguration()),automatonLoadedFromLTL),expected));
		Assert.assertNull(WMethod.checkM(uniteAndDeterminise(new LearnerGraph(Configuration.getDefaultConfiguration()),automatonLoadedFromLTL),expected));
		Assert.assertNull(WMethod.checkM(uniteAndDeterminise(new LearnerGraph(Configuration.getDefaultConfiguration()),automatonLoadedFromLTL),expected));
	}
}
