/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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
package statechum.analysis.learning.rpnicore;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.Pair;
import statechum.Configuration.SMTGRAPHDOMAINCONSISTENCYCHECK;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.Smt;
import statechum.analysis.learning.rpnicore.LabelRepresentation.AbstractState;
import statechum.analysis.learning.rpnicore.LabelRepresentation.Label;

import static statechum.analysis.learning.rpnicore.LabelRepresentation.INITMEM;
import static statechum.analysis.learning.rpnicore.LabelRepresentation.ENDL;
import statechum.analysis.learning.AbstractOracle;
import statechum.apps.QSMTool;

public class TestSmtLabelRepresentation {
	private static final String _N = LabelRepresentation.varNewSuffix;
	private static final String _M = LabelRepresentation.varOldSuffix;
	
	Configuration config = null;
	
	@Before
	public void beforeTest()
	{
		config = Configuration.getDefaultConfiguration().copy(); 
	}

	@AfterClass
	public static void afterTest()
	{
		Smt.loadLibrary();
		Smt.reopenStdOut();
	}
	
	@Test
	public void testNoLabels1()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		Assert.assertNull(lbls.labelMapConstructionOfOperations);
		Assert.assertNull(lbls.labelMapConstructionOfDataTraces);
		Assert.assertNull(lbls.labelMapFinal);
	}

	@Test
	public void testNoLabels2()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.labelMapConstructionOfOperations = new TreeMap<String,Label>();
		lbls.parseLabel(null);
		lbls.parseLabel("");
		Assert.assertTrue(lbls.labelMapConstructionOfOperations.isEmpty());
		Assert.assertNull(lbls.labelMapConstructionOfDataTraces);
		Assert.assertNull(lbls.labelMapFinal);
	}
		
	@Test
	public void testCreateLabels_error1()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.labelMapConstructionOfOperations = new TreeMap<String,Label>();
			lbls.parseLabel(INITMEM+" ");
		}}, IllegalArgumentException.class,"expected details for label");
	}
	
	@Test
	public void testCreateLabels_error2()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.labelMapConstructionOfOperations = new TreeMap<String,Label>();
			lbls.parseLabel(INITMEM+" JUNK");
		}}, IllegalArgumentException.class,"expected [PRE");
	}
	
	@Test
	public void testCreateLabels_error3()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.labelMapConstructionOfOperations = new TreeMap<String,Label>();
			lbls.parseLabel(INITMEM+"  "+LabelRepresentation.OP_DATA.PRE);
		}}, IllegalArgumentException.class,"expected specification for label");
	}

	@Test
	public void testCreateLabels_error4()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.labelMapConstructionOfOperations = new TreeMap<String,Label>();
			lbls.parseLabel(INITMEM+"  "+LabelRepresentation.OP_DATA.PRE+"  ");
		}}, IllegalArgumentException.class,"expected specification for label");
	}

	@Test
	public void testCreateLabels1()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.labelMapConstructionOfOperations = new TreeMap<String,Label>();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.OP_DATA.PRE.name()+" varDecl");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.OP_DATA.PRE.name()+" varDecl2");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.OP_DATA.POST.name()+" memory0");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.OP_DATA.POST.name()+" memory1");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.OP_DATA.POST.name()+" memory2");
		Assert.assertEquals(1,lbls.labelMapConstructionOfOperations.size());
		Label l = lbls.labelMapConstructionOfOperations.entrySet().iterator().next().getValue();
		Assert.assertEquals(INITMEM,lbls.labelMapConstructionOfOperations.entrySet().iterator().next().getKey());
		Assert.assertEquals(INITMEM,l.getName());
		Assert.assertEquals("memory0\nmemory1\nmemory2",l.post.text);
		Assert.assertEquals("varDecl\nvarDecl2",l.pre.text);
	}
	
	@Test
	public void testCreateLabels2()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.labelMapConstructionOfOperations = new TreeMap<String,Label>();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.OP_DATA.PRE.name()+" varDecl");
		lbls.parseLabel("A"+" "+LabelRepresentation.OP_DATA.POST.name()+" postA and            more");
		lbls.parseLabel("A"+" "+LabelRepresentation.OP_DATA.POST.name()+"     details of postcondition     of A     ");
		lbls.parseLabel("B"+" "+LabelRepresentation.OP_DATA.PRE.name()+" value of precondition of B ");
		Assert.assertEquals(3,lbls.labelMapConstructionOfOperations.size());

		{
			Label l = lbls.labelMapConstructionOfOperations.get(INITMEM);
			Assert.assertEquals(INITMEM,l.getName());
			Assert.assertNull(l.post.text);
			Assert.assertEquals("varDecl",l.pre.text);
		}

		{
			Label l = lbls.labelMapConstructionOfOperations.get("A");
			Assert.assertEquals("A",l.getName());
			Assert.assertNull(l.pre.text);
			Assert.assertEquals("postA and more\ndetails of postcondition of A",l.post.text);
		}

		{
			Label l = lbls.labelMapConstructionOfOperations.get("B");
			Assert.assertEquals("B",l.getName());
			Assert.assertNull(l.post.text);
			Assert.assertEquals("value of precondition of B",l.pre.text);
		}
	}

	@Test
	public void testEquals()
	{
		LabelRepresentation lblsA = new LabelRepresentation();
		lblsA.labelMapConstructionOfOperations = new TreeMap<String,Label>();
		{
			lblsA.parseLabel(INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDecl"+_N);
			lblsA.parseLabel(INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " initCond"+_N);
			lblsA.parseLabel("A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA");
			lblsA.parseLabel("A"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA");
			lblsA.parseLabel("B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB");
			lblsA.parseLabel("B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB");
		}
		LabelRepresentation lblsB = new LabelRepresentation();
		lblsB.labelMapConstructionOfOperations = new TreeMap<String,Label>();
		{
			lblsB.parseLabel(INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ "    varDecl"+_N);
			lblsB.parseLabel(INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " initCond"+_N);
			lblsB.parseLabel("A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA");
			lblsB.parseLabel("A"+" "+LabelRepresentation.OP_DATA.POST+ "    somePostcondA");
			lblsB.parseLabel("B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB");
			lblsB.parseLabel("B"+" "+LabelRepresentation.OP_DATA.POST+ "       somePostcondB");
		}
		LabelRepresentation lblsDiffA = new LabelRepresentation();
		lblsDiffA.labelMapConstructionOfOperations = new TreeMap<String,Label>();
		{
			lblsDiffA.parseLabel(INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ "    varDecl"+_N);
			lblsDiffA.parseLabel(INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " initCond"+_N);
			lblsDiffA.parseLabel("A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA");
			lblsDiffA.parseLabel("A"+" "+LabelRepresentation.OP_DATA.POST+ "    somePostcondA");
			lblsDiffA.parseLabel("B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB");
			lblsDiffA.parseLabel("B"+" "+LabelRepresentation.OP_DATA.POST+ "       somePostcondB");
			lblsDiffA.parseLabel("B"+" "+LabelRepresentation.OP_DATA.POST+ "       somePostcondB");
		}
		LabelRepresentation lblsDiffB = new LabelRepresentation();
		lblsDiffB.labelMapConstructionOfOperations = new TreeMap<String,Label>();
		TestEqualityComparisonAndHashCode.equalityTestingHelper(lblsA,lblsA,lblsDiffA,lblsDiffB);
	}

	final String __P = LabelRepresentation.delimiterString,__N = LabelRepresentation.delimiterString+"-";
	
	@Test
	public void testRelabel()
	{
		Assert.assertEquals("text more var"+__P+"6 text var"+__N+"3 smth"+__P+"6 again ",
				LabelRepresentation.toCurrentMem("text more var"+_N+" text var"+_M+" smth"+_N+" again ", 6,-3));
		Assert.assertEquals("text more var"+__N+"6 text var"+__P+"7 smth"+__N+"6 again ",
				LabelRepresentation.toCurrentMem("text more var"+_N+" text var"+_M+" smth"+_N+" again ", -6,7));
		Assert.assertEquals("text more text var"+__P+"8 again ",
				LabelRepresentation.toCurrentMem("text more text var"+_M+" again ", 6,8));
		Assert.assertEquals("true",
				LabelRepresentation.toCurrentMem("", 6,4));
	}
	
	/** No exception since args are unused. */
	@Test
	public void testRelabel_unknownIgnored()
	{
		LabelRepresentation.toCurrentMem("some text ",JUConstants.intUNKNOWN,5);
		LabelRepresentation.toCurrentMem("some text ",7,JUConstants.intUNKNOWN);
	}
	
	@Test
	public void testRelabel_fail1()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation.toCurrentMem("some "+_M+" text ",5,JUConstants.intUNKNOWN);
		}}, IllegalArgumentException.class,"previous number should");
	}
	
	@Test
	public void testRelabel_fail2()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation.toCurrentMem("some "+_N+" text ",JUConstants.intUNKNOWN,6);
		}}, IllegalArgumentException.class,"current number should");
	}
	
	@Test
	public void testAppendToString1()
	{
		Assert.assertEquals("some text",LabelRepresentation.appendToString(" some text",null));
		Assert.assertEquals(ENDL+"some text",LabelRepresentation.appendToString("some text",""));
		Assert.assertNull(LabelRepresentation.appendToString("  ",null));
		Assert.assertEquals(" __  ",LabelRepresentation.appendToString(""," __  "));
		Assert.assertEquals(" __  ",LabelRepresentation.appendToString("   "," __  "));
		Assert.assertEquals(" __  ",LabelRepresentation.appendToString(null," __  "));
		Assert.assertNull(LabelRepresentation.appendToString(null,null));
	}
	
	@Test
	public void testAppendToString2()
	{
		Assert.assertEquals("data"+ENDL+"some text",LabelRepresentation.appendToString(" some text ","data"));
	}
	
	@Test
	public void testParseCollectionInvalidCommand()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseCollection(Arrays.asList(new String[]{
					"A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA",
			}));
		}}, IllegalArgumentException.class,"invalid command");
	}
	
	@Test
	public void testParseCollectionMissingInitial()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA",
					QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA",
					QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB",
					QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"}));
		}}, IllegalArgumentException.class,"missing initial");
	}
	
	/** Uses _M in postcondition - prohibited because we'd like args to toCurrentMem to be different but INITMEM only
	 * uses one argument. Thus the _N is used and _M is prohibited.
	 */
	@Test
	public void testParseCollection_init_invalid_POST()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA",
					QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA"+LabelRepresentation.varOldSuffix,
					QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB",
					QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"}));
		}}, IllegalArgumentException.class,LabelRepresentation.INITMEM+" should not refer");
	}
	
	/** Uses _M in postcondition - prohibited because we'd like args to toCurrentMem to be different but INITMEM only
	 * uses one argument. Thus the _N is used and _M is prohibited.
	 */
	@Test
	public void testParseCollection_init_invalid_PRE()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA"+LabelRepresentation.varOldSuffix,
					QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA",
					QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB",
					QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"}));
		}}, IllegalArgumentException.class,LabelRepresentation.INITMEM+" should not refer");
	}
	
	@Test
	public void testParseCollectionInvalidFunctionName1()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " (define varDecl"+_N+"::int)",
					QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " ("+LabelRepresentation.functionArg+")"}));
		}}, IllegalArgumentException.class,"invalid function name");
	}

	@Test
	public void testParseCollectionInvalidFunctionName_no_failure_if_does_not_start_with_frg()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " (define varDecl"+_N+"::int)",
				QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " (a"+LabelRepresentation.functionArg+")"}));
	}

	@Test
	public void testParseCollectionInvalidFunctionName2()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " define varDecl"+_N+"::int)",
					QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " (func"+LabelRepresentation.delimiterString+")"}));
		}}, IllegalArgumentException.class,"invalid function name");
	}

	@Test
	public void testParseCollectionInvalidArgumentName()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " define varDecl"+_N+"::int)",
					QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " (func "+LabelRepresentation.functionArg+"arg )"}));
		}}, IllegalArgumentException.class,"invalid argument name");
	}

	/** This one is ok because the argument name does not start with a reserved name. */
	@Test
	public void testParseCollectionInvalidArgumentNameOk()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " (define varDecl"+_N+"::int)",
				QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " (func a"+LabelRepresentation.functionArg+"arg )"}));
	}

	@Test
	public void testParseCollectionRepeatConstruction()
	{
		final LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " (define varDecl"+_N+"::int)",
				QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " (func )"}));
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " (define varDecl"+_N+"::int)",
					QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " (func )"}));
		}}, IllegalArgumentException.class,"operations already built");
	}

	@Test
	public void testCreateConjunctionEmpty1()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " define varDecl"+_N+"",
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " decl"+_N,
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " (somePrecondA)",
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.POST+ " (somePostcondA)",
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " (somePrecondB)",
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " (somePostcondB)"}));
		lbls.buildVertexToAbstractStateMap(new LearnerGraph(FsmParser.buildGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1"), Configuration.getDefaultConfiguration()),null);
		Pair<String,String> state = lbls.getConjunctionForPath(Arrays.asList(new Label[]{}),null);
		int number = 4;
		Assert.assertEquals("define varDecl"+__P+number+" decl"+__P+number+ENDL,state.firstElem);
		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[]"+ENDL+"true"+ENDL, state.secondElem);
	}
	
	@Test
	public void testCreateConjunctionEmpty2()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDecl"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " (= initCond"+_N+" 7)",
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " (somePrecondA)",
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.POST+ " (somePostcondA)",
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " (somePrecondB)",
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " (somePostcondB)"}));
		lbls.buildVertexToAbstractStateMap(new LearnerGraph(FsmParser.buildGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1"), Configuration.getDefaultConfiguration()),null);
		Pair<String,String>  state = lbls.getConjunctionForPath(Arrays.asList(new Label[]{}),null);
		int number = 4;
		Assert.assertEquals("varDecl"+__P+number+ENDL,state.firstElem);
		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[]"+ENDL+
				"(and"+ENDL+LabelRepresentation.commentForInit+ENDL+"(= initCond"+__P+number+" 7)"+ENDL+')'+ENDL,state.secondElem);
	}
	
	@Test
	public void testbuildVertexToAbstractStateMapUnknown1()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDecl"+_N,
				QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " initCond"+_N,
				QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA",
				QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA",
				QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB",
				QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"}));
			lbls.buildVertexToAbstractStateMap(new LearnerGraph(FsmParser.buildGraph("stA-unknown_label->stD", "testCreateConjunctionUnknown1"), Configuration.getDefaultConfiguration()),null);
		}}, IllegalArgumentException.class,"unknown label unknown_label");
	}
	
	@Test
	public void testbuildVertexToAbstractStateMapUnknown2()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDecl"+_N,
				QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " initCond"+_N,
				QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA",
				QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA",
				QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB",
				QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"}));
			lbls.buildVertexToAbstractStateMap(new LearnerGraph(FsmParser.buildGraph("stA-A->stB-B->stC-unknown_label->stD", "testCreateConjunctionUnknown2"), Configuration.getDefaultConfiguration()),null);
		}}, IllegalArgumentException.class,"unknown label unknown_label");
	}
	
	@Test
	public void testCreateConjunction_mismatchedLength()
	{
		final LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " initCond"+_N,
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_M,
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA"+_N,
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_M,
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N}));
		lbls.buildVertexToAbstractStateMap(new LearnerGraph(FsmParser.buildGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1"), Configuration.getDefaultConfiguration()),null);
	
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			lbls.getConjunctionForPath(
					Arrays.asList(new Label[]{lbls.labelMapConstructionOfDataTraces.get("A"),lbls.labelMapConstructionOfDataTraces.get("B")}),
					Arrays.asList(new String[]{}));
		}}, IllegalArgumentException.class,"mismatched length");
	}
	
	@Test
	public void testCreateConjunction_constructionIncomplete()
	{
		final LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
				QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
				QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " initCond"+_N,
				QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_N,
				QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA"+_N,
				QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_N,
				QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N}));
		
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			lbls.getConjunctionForPath(
					Arrays.asList(new Label[]{lbls.labelMapConstructionOfDataTraces.get("A"),lbls.labelMapConstructionOfDataTraces.get("B")}),null);
		}}, IllegalArgumentException.class,"construction incomplete");
	}
		
	@Test
	public void testCreateConjunction1()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N+"",
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N+"",
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " initCond"+_N+"",
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_M+"",
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA"+_N+"",
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_M+"",
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N+""}));
		lbls.buildVertexToAbstractStateMap(new LearnerGraph(FsmParser.buildGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1"), Configuration.getDefaultConfiguration()),null);
		Pair<String,String> state = lbls.getConjunctionForPath(
				Arrays.asList(new Label[]{lbls.labelMapFinal.get("A"),lbls.labelMapFinal.get("B")}),null);
		int number = 4;
		Assert.assertEquals("varDeclP"+__P+(number+0)+" varDeclQ"+__P+(number+0)+ENDL+
				"varDeclP"+__P+(number+1)+" varDeclQ"+__P+(number+1)+ENDL+
				"varDeclP"+__P+(number+2)+" varDeclQ"+__P+(number+2)+ENDL,state.firstElem);

		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[A,B]"+ENDL+
				"(and"+ENDL+LabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+(number+0)+ENDL+
				LabelRepresentation.commentForLabel+"A"+ENDL+
				"somePrecondA"+__P+(number+0)+ENDL+
				"somePostcondA"+__P+(number+1)+ENDL+
				LabelRepresentation.commentForLabel+"B"+ENDL+
				"somePrecondB"+__P+(number+1)+ENDL+
				"somePostcondB"+__P+(number+2)+ENDL+')'+ENDL,
				state.secondElem);
	}
	
	@Test
	public final void testCreateAbstractState1()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDecl"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " initCond"+_N,
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA",
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA",
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB",
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"}));
		lbls.buildVertexToAbstractStateMap(new LearnerGraph(FsmParser.buildGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1"), Configuration.getDefaultConfiguration()),null);
		AbstractState state = lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("P"),config),0);
		Assert.assertEquals("varDecl"+__P+"0",state.variableDeclarations);
		Assert.assertEquals(LabelRepresentation.commentForInit+ENDL+"initCond"+__P+"0",state.abstractState);
		Assert.assertNull(state.lastLabel);
		Assert.assertNull(state.previousState);
		Assert.assertEquals(0,state.stateNumber);
	}
	
	@Test
	public final void testCreateAbstractState2()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " initCond"+_N,
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_M,
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA"+_N,
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_M,
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N}));
		lbls.buildVertexToAbstractStateMap(new LearnerGraph(FsmParser.buildGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1"), Configuration.getDefaultConfiguration()),null);
		int number0 = 10,number1=15,number2=20;
		AbstractState stateInit = lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("Init"),config),number0);
		AbstractState stateAfterA = lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("AfterA"),config),stateInit,lbls.labelMapFinal.get("A"),number1);
		AbstractState stateAfterB = lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("AfterB"),config),stateAfterA,lbls.labelMapFinal.get("B"),number2);
		Assert.assertEquals("varDeclP"+__P+number0+" varDeclQ"+__P+number0+ENDL+
				"varDeclP"+__P+number1+" varDeclQ"+__P+number1+ENDL+
				"varDeclP"+__P+number2+" varDeclQ"+__P+number2,stateAfterB.variableDeclarations);

		Assert.assertEquals(LabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+number0+ENDL+
				LabelRepresentation.commentForLabel+"A"+ENDL+
				"somePrecondA"+__P+number0+ENDL+
				"somePostcondA"+__P+number1+ENDL+
				LabelRepresentation.commentForLabel+"B"+ENDL+
				"somePrecondB"+__P+number1+ENDL+
				"somePostcondB"+__P+number2,
				stateAfterB.abstractState);
		Assert.assertEquals("AfterB",stateAfterB.vertex.toString());
		Assert.assertSame(lbls.labelMapFinal.get("B"),stateAfterB.lastLabel);
		Assert.assertEquals(number2,stateAfterB.stateNumber);
	}
	
	private LabelRepresentation testCreateConjunction2_internal()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_M,
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_M,
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N}));
		lbls.buildVertexToAbstractStateMap(new LearnerGraph(FsmParser.buildGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1"), Configuration.getDefaultConfiguration()),null);
		Pair<String,String> state = lbls.getConjunctionForPath(
				Arrays.asList(new Label[]{lbls.labelMapFinal.get("A"),lbls.labelMapFinal.get("B")}),null);
		int number = 4;
		Assert.assertEquals("varDeclP"+__P+(number+0)+" varDeclQ"+__P+(number+0)+ENDL+
				"varDeclP"+__P+(number+1)+" varDeclQ"+__P+(number+1)+ENDL+
				"varDeclP"+__P+(number+2)+" varDeclQ"+__P+(number+2)+ENDL, state.firstElem);

		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[A,B]"+ENDL+
				"(and"+ENDL+LabelRepresentation.commentForInit+ENDL+
				LabelRepresentation.commentForLabel+"A"+ENDL+
				"somePrecondA"+__P+(number+0)+ENDL+
				LabelRepresentation.commentForLabel+"B"+ENDL+
				"somePrecondB"+__P+(number+1)+ENDL+
				"somePostcondB"+__P+(number+2)+ENDL+
				')'+ENDL,
				state.secondElem
				);
		
		return lbls;
	}
	
	@Test
	public void testCreateConjunction2()
	{
		testCreateConjunction2_internal();
	}

	/** Tests that creation of successive conjunctions returns the correct numbers for abstract states. */
	@Test
	public void testCreateConjunction3()
	{
		LabelRepresentation lbls = testCreateConjunction2_internal();
		Pair<String,String> state = lbls.getConjunctionForPath(
				Arrays.asList(new Label[]{lbls.labelMapFinal.get("A"),lbls.labelMapFinal.get("B")}),null);
		int number = 7;
		Assert.assertEquals("varDeclP"+__P+(number+0)+" varDeclQ"+__P+(number+0)+ENDL+
				"varDeclP"+__P+(number+1)+" varDeclQ"+__P+(number+1)+ENDL+
				"varDeclP"+__P+(number+2)+" varDeclQ"+__P+(number+2)+ENDL, state.firstElem);

		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[A,B]"+ENDL+
				"(and"+ENDL+LabelRepresentation.commentForInit+ENDL+
				LabelRepresentation.commentForLabel+"A"+ENDL+
				"somePrecondA"+__P+(number+0)+ENDL+
				LabelRepresentation.commentForLabel+"B"+ENDL+
				"somePrecondB"+__P+(number+1)+ENDL+
				"somePostcondB"+__P+(number+2)+ENDL+')'+ENDL,
				state.secondElem
				);
	}

	private LabelRepresentation simpleLabel()
	{
		final LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"add"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
		return lbls;
	}
	
	@Test
	public void testCheckConsistency_constructionIncomplete()
	{
		final LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
				QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
				QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " initCond"+_N,
				QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_N,
				QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA"+_N,
				QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_N,
				QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N}));
		
		final LearnerGraph graph = new LearnerGraph(config);
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.NONE);
			lbls.checkConsistency(graph,config);
		}}, IllegalArgumentException.class,"construction incomplete");
	}

	/** Initial accept-state should have a satisfiable abstract state. */
	@Test
	public final void testAugmentCheck1()
	{
		final LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " (and (= m"+_N+" 0) (= m"+_N+" 1))",
			QSMTool.cmdOperation+" "+"add"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.PRE+ " (> m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
		final LearnerGraph graph = new LearnerGraph(config);
		lbls.buildVertexToAbstractStateMap(graph, null);
		
		// no check - everything seems ok.
		config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.NONE);
		Assert.assertNull(lbls.checkConsistency(graph,config));

		// when checking states, an error should be reported.
		config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
		Assert.assertTrue(lbls.checkConsistency(graph,config).getMessage().contains("has an abstract state inconsistent with the accept condition"));
	}

	/** Initial reject-state may not have a satisfiable abstract state. */
	@Test
	public final void testAugmentCheck2()
	{
		final LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"add"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.PRE+ " (> m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
		final LearnerGraph graph = new LearnerGraph(config);graph.init.setAccept(false);
		lbls.buildVertexToAbstractStateMap(graph, null);
		
		// no check - everything seems ok.
		config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.NONE);
		Assert.assertNull(lbls.checkConsistency(graph,config));

		// when checking states, an error should be reported.
		config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
		Assert.assertTrue(lbls.checkConsistency(graph,config).getMessage().contains("has an abstract state inconsistent with the accept condition"));
	}

	/** Augmenting reject-paths. */
	@Test
	public final void testAugmentCheck3()
	{
		final LabelRepresentation lbls = simpleLabel();
		final LearnerGraph graph = new LearnerGraph(config);

		final List<String> sequence = Arrays.asList(new String[]{"remove"});
		graph.paths.augmentPTA(sequence,false, false, null);
		
		lbls.buildVertexToAbstractStateMap(graph, null);
		config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
		Assert.assertNull(lbls.checkConsistency(graph,config));
	}

	/** Augmenting reject-paths. */
	@Test
	public final void testAugmentCheck4()
	{
		final LabelRepresentation lbls = simpleLabel();
		final LearnerGraph graph = new LearnerGraph(config);
		
		final List<String> sequence = Arrays.asList(new String[]{"remove"});
		graph.paths.augmentPTA(sequence,true, false, null);
		
		lbls.buildVertexToAbstractStateMap(graph, null);
		config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
		Assert.assertTrue(lbls.checkConsistency(graph,config).getMessage().contains("has an abstract state inconsistent with the accept condition"));
	}

	/** Augmenting reject-paths. */
	@Test
	public final void testAugmentCheck5()
	{
		final LabelRepresentation lbls = simpleLabel();
		final LearnerGraph graph = new LearnerGraph(config);
		final List<String> sequence = Arrays.asList(new String[]{"add","remove","remove"});
		graph.paths.augmentPTA(sequence,false, false, null);
		
		lbls.buildVertexToAbstractStateMap(graph, null);
		config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
		Assert.assertNull(lbls.checkConsistency(graph,config));
	}

	/** Augmenting reject-paths. */
	@Test
	public final void testAugmentCheck6()
	{
		final LabelRepresentation lbls = simpleLabel();
		final LearnerGraph graph = new LearnerGraph(config);

		final List<String> sequence = Arrays.asList(new String[]{"add","remove","remove"});
		graph.paths.augmentPTA(sequence,true, false, null);

		lbls.buildVertexToAbstractStateMap(graph, null);
		config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
		Assert.assertTrue(lbls.checkConsistency(graph,config).getMessage().contains("has an abstract state inconsistent with the accept condition"));
	}

	/** Augmenting accept-paths. */
	@Test
	public final void testCheck7()
	{
		final LabelRepresentation lbls = simpleLabel();
		final LearnerGraph graph = new LearnerGraph(config);

		final List<String> sequence = Arrays.asList(new String[]{"add","remove","add"});
		graph.paths.augmentPTA(sequence,true, false, null);

		lbls.buildVertexToAbstractStateMap(graph, null);
		config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
		Assert.assertNull(lbls.checkConsistency(graph,config));
	}

	/** Checks that it is possible to check that all states can be entered. */
	@Test
	public final void testCheck8()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"add"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-add->B-add->C-add->D\nB-remove->E-add->F","testUpdateScore"), config);
		lbls.buildVertexToAbstractStateMap(graph,null);
		
		config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
		Assert.assertNull(lbls.checkConsistency(graph,config));
	}
	
	/** Checks that it is possible to check that all states can be entered. */
	@Test
	public final void testCheck9()
	{
		final LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"add"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-add->B\nA-remove->S","testAbstractStateSatisfiability2"), config);
		lbls.buildVertexToAbstractStateMap(graph,null);
		
		config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
		Assert.assertTrue(lbls.checkConsistency(graph,config).getMessage().contains("has an abstract state inconsistent with the accept condition"));
	}

	// FIXME: to test multiple abstract states per single DFA state (using what I did in TestFSMAlgo), allnone, inclusion of a postcondition and determinism.
	
	/** The <em>testCreateIDToStateMap1</em> is in TestFSMAlgo. */
	@Test
	public final void testCreateIDToStateMap2()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " initCond"+_N,
			QSMTool.cmdOperation+" "+"a"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_M,
			QSMTool.cmdOperation+" "+"a"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA"+_N,
			QSMTool.cmdOperation+" "+"b"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_M,
			QSMTool.cmdOperation+" "+"b"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N}));
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a-#D\nB-b->E", "createLemmas1"),config);
		lbls.buildVertexToAbstractStateMap(graph, null);

		//for(Entry<VertexID,AbstractState> entry:lbls.idToState.entrySet())
		//	System.out.println(entry.getKey()+" "+entry.getValue().abstractState);
		Assert.assertEquals(5,graph.learnerCache.getVertexToAbstractState().size());
		for(Entry<CmpVertex,Collection<AbstractState>> entry:graph.learnerCache.getVertexToAbstractState().entrySet())
			Assert.assertEquals(1,entry.getValue().size());
		int varNumber = 0;
		Assert.assertEquals(
				LabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+varNumber,
				extractAbstractStateFrom(graph,"A").abstractState);
		
		Assert.assertEquals(
				LabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+(varNumber+0)+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+0)+ENDL+
				"somePostcondA"+__P+(varNumber+1),
				extractAbstractStateFrom(graph,"B").abstractState);
		
		Assert.assertEquals(
				LabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+(varNumber+0)+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+0)+ENDL+
				"somePostcondA"+__P+(varNumber+1)+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+1)+ENDL+
				"somePostcondA"+__P+(varNumber+2),
				extractAbstractStateFrom(graph,"C").abstractState);
		
		Assert.assertEquals(
				LabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+(varNumber+0)+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+0)+ENDL+
				"somePostcondA"+__P+(varNumber+1)+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+1)+ENDL+
				"somePostcondA"+__P+(varNumber+2)+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+2)+ENDL+
				"somePostcondA"+__P+(varNumber+4),
				extractAbstractStateFrom(graph,"D").abstractState);
		
		Assert.assertEquals(
				LabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+(varNumber+0)+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+0)+ENDL+
				"somePostcondA"+__P+(varNumber+1)+ENDL+
				LabelRepresentation.commentForLabel+"b"+ENDL+
				"somePrecondB"+__P+(varNumber+1)+ENDL+
				"somePostcondB"+__P+(varNumber+3),
				extractAbstractStateFrom(graph,"E").abstractState);
	}
	
	
	@Test
	public void testSolvingConstraints()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"add"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-add->B-remove->C-remove-#D\nB-add->E", "testSolvingConstraints"),config);
		lbls.buildVertexToAbstractStateMap(graph, null);

		Pair<String,String> state = null;
		state = lbls.getConjunctionForPath(Arrays.asList(new Label[]{lbls.labelMapFinal.get("remove")}),null);
		Assert.assertFalse(lbls.checkSatisfiability(state.firstElem, state.secondElem));

		state = lbls.getConjunctionForPath(Arrays.asList(new Label[]{}),null);
		Assert.assertTrue(lbls.checkSatisfiability(state.firstElem, state.secondElem));

		state = lbls.getConjunctionForPath(Arrays.asList(new Label[]{lbls.labelMapFinal.get("add"),lbls.labelMapFinal.get("remove")}),null);
		Assert.assertTrue(lbls.checkSatisfiability(state.firstElem, state.secondElem));

		state = lbls.getConjunctionForPath(Arrays.asList(new Label[]{lbls.labelMapFinal.get("add"),lbls.labelMapFinal.get("remove"),lbls.labelMapFinal.get("remove")}),null);
		Assert.assertFalse(lbls.checkSatisfiability(state.firstElem, state.secondElem));
	}
	
	/** Obtains an abstract state corresponding to the specific state in a specific graph. */
	private AbstractState extractAbstractStateFrom(LearnerGraph graph, String stateName)
	{
		Collection<AbstractState> abstractStates = graph.learnerCache.getVertexToAbstractState().get(graph.findVertex(VertexID.parseID(stateName)));
		Assert.assertEquals(1,abstractStates.size());
		return abstractStates.iterator().next();
	}
	
	/** Checks that we can use abstract states to compute state compatibility. */
	@Test
	public final void testUpdateScore()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"add"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-add->B-add->C-add->D\nB-remove->E-add->F","testUpdateScore"), config);
		lbls.buildVertexToAbstractStateMap(graph,null);
		
		config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
		Assert.assertNull(lbls.checkConsistency(graph,config));

		Assert.assertTrue(lbls.abstractStatesCompatible(extractAbstractStateFrom(graph,"A"), extractAbstractStateFrom(graph,"A")));
		Assert.assertFalse(lbls.abstractStatesCompatible(extractAbstractStateFrom(graph,"A"), extractAbstractStateFrom(graph,"B")));
		Assert.assertFalse(lbls.abstractStatesCompatible(extractAbstractStateFrom(graph,"B"), extractAbstractStateFrom(graph,"A")));
		
		Assert.assertTrue(lbls.abstractStatesCompatible(extractAbstractStateFrom(graph,"A"), extractAbstractStateFrom(graph,"E")));
		Assert.assertTrue(lbls.abstractStatesCompatible(extractAbstractStateFrom(graph,"B"), extractAbstractStateFrom(graph,"F")));

		Assert.assertFalse(lbls.abstractStatesCompatible(extractAbstractStateFrom(graph,"D"), extractAbstractStateFrom(graph,"F")));
	}

	@Test
	public final void testCheckWithEndUser()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"add"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-add->B","testUpdateScore"), config);
		lbls.buildVertexToAbstractStateMap(graph,null);
		
		Assert.assertEquals(AbstractOracle.USER_ACCEPTED,lbls.CheckWithEndUser(Arrays.asList(new String[]{})));
		Assert.assertEquals(0,lbls.CheckWithEndUser(Arrays.asList(new String[]{"remove"})));
		Assert.assertEquals(AbstractOracle.USER_ACCEPTED,lbls.CheckWithEndUser(Arrays.asList(new String[]{"add"})));
		Assert.assertEquals(AbstractOracle.USER_ACCEPTED,lbls.CheckWithEndUser(Arrays.asList(new String[]{"add","remove"})));
		Assert.assertEquals(2,lbls.CheckWithEndUser(Arrays.asList(new String[]{"add","remove","remove"})));
		Assert.assertEquals(AbstractOracle.USER_ACCEPTED,lbls.CheckWithEndUser(Arrays.asList(new String[]{"add","remove","add","add"})));
	}
	
	@Test
	public final void testCheckWithEndUser_fail()
	{
		final LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"add"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.PRE+ " (> m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"remove"+" "+LabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-add->B","testUpdateScore"), config);
		lbls.buildVertexToAbstractStateMap(graph,null);
		
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			lbls.CheckWithEndUser(Arrays.asList(new String[]{"aa"}));
		}},IllegalArgumentException.class,"unknown label");
	}
}
