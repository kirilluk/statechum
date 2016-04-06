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
package statechum.analysis.learning.smt;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.ParameterizedWithName;

import statechum.Label;
import statechum.Configuration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.Pair;
import statechum.Configuration.SMTGRAPHDOMAINCONSISTENCYCHECK;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.Smt;
import statechum.analysis.learning.rpnicore.TestEqualityComparisonAndHashCode;
import statechum.analysis.learning.smt.SmtLabelRepresentation.AbstractState;
import statechum.analysis.learning.smt.SmtLabelRepresentation.FUNC_DATA;
import statechum.analysis.learning.smt.SmtLabelRepresentation.SMTLabel;
import statechum.analysis.learning.smt.SmtLabelRepresentation.VARIABLEUSE;

import static statechum.analysis.learning.smt.SmtLabelRepresentation.INITMEM;
import static statechum.analysis.learning.smt.SmtLabelRepresentation.ENDL;
import static statechum.analysis.learning.smt.SmtLabelRepresentation.toCurrentMem;
import static statechum.analysis.learning.smt.SmtLabelRepresentation.generateFreshVariable;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;
import statechum.analysis.learning.AbstractOracle;
import statechum.apps.QSMTool;

public class TestSmtLabelRepresentation 
{
	private static final String _N = SmtLabelRepresentation.varNewSuffix;
	private static final String _M = SmtLabelRepresentation.varOldSuffix;
	
	Configuration config = null;
	ConvertALabel converter = null;
	
	/** Converts arrays of labels to lists of labels using config - it does not really matter which configuration is used 
	 * because all of them start from a default one and do not modify label type.
	 * 
	 * @param labels what to convert
	 * @return the outcome of conversion.
	 */
	protected List<statechum.Label> labelList(String [] labels)
	{
		return AbstractLearnerGraph.buildList(Arrays.asList(labels),config,converter);
	}
	
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
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		Assert.assertNull(lbls.labelMapConstructionOfOperations);
		Assert.assertNull(lbls.labelMapConstructionOfDataTraces);
		Assert.assertNull(lbls.labelMapFinal);
	}

	@Test
	public void testNoLabels2()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.labelMapConstructionOfOperations = new TreeMap<Label,SMTLabel>();
		lbls.parseLabel(null);
		lbls.parseLabel("");
		Assert.assertTrue(lbls.labelMapConstructionOfOperations.isEmpty());
		Assert.assertNull(lbls.labelMapConstructionOfDataTraces);
		Assert.assertNull(lbls.labelMapFinal);
	}
		
	@Test
	public void testCreateLabels_error1()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.labelMapConstructionOfOperations = new TreeMap<Label,SMTLabel>();
			lbls.parseLabel(INITMEM+" ");
		}}, IllegalArgumentException.class,"expected details for label");
	}
	
	@Test
	public void testCreateLabels_error2()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.labelMapConstructionOfOperations = new TreeMap<Label,SMTLabel>();
			lbls.parseLabel(INITMEM+" JUNK");
		}}, IllegalArgumentException.class,"expected [PRE");
	}
	
	@Test
	public void testCreateLabels_error3()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.labelMapConstructionOfOperations = new TreeMap<Label,SMTLabel>();
			lbls.parseLabel(INITMEM+"  "+SmtLabelRepresentation.OP_DATA.PRE);
		}}, IllegalArgumentException.class,"expected specification for label");
	}

	@Test
	public void testCreateLabels_error4()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.labelMapConstructionOfOperations = new TreeMap<Label,SMTLabel>();
			lbls.parseLabel(INITMEM+"  "+SmtLabelRepresentation.OP_DATA.PRE+"  ");
		}}, IllegalArgumentException.class,"expected specification for label");
	}

	@Test
	public void testCreateLabels1()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.labelMapConstructionOfOperations = new TreeMap<Label,SMTLabel>();
		lbls.parseLabel(INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE.name()+" varDecl");
		lbls.parseLabel(INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE.name()+" varDecl2");
		lbls.parseLabel(INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST.name()+" memory0");
		lbls.parseLabel(INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST.name()+" memory1");
		lbls.parseLabel(INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST.name()+" memory2");
		Assert.assertEquals(1,lbls.labelMapConstructionOfOperations.size());
		SMTLabel l = lbls.labelMapConstructionOfOperations.entrySet().iterator().next().getValue();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel(INITMEM, config,converter),lbls.labelMapConstructionOfOperations.entrySet().iterator().next().getKey());
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel(INITMEM, config,converter),l.getName());
		Assert.assertEquals("memory0\nmemory1\nmemory2",l.post.text);
		Assert.assertEquals("varDecl\nvarDecl2",l.pre.text);
	}
	
	@Test
	public void testCreateLabels2()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.labelMapConstructionOfOperations = new TreeMap<Label,SMTLabel>();
		lbls.parseLabel(INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE.name()+" varDecl");
		lbls.parseLabel("A"+" "+SmtLabelRepresentation.OP_DATA.POST.name()+" postA and            more");
		lbls.parseLabel("A"+" "+SmtLabelRepresentation.OP_DATA.POST.name()+"     details of postcondition     of A     ");
		lbls.parseLabel("B"+" "+SmtLabelRepresentation.OP_DATA.PRE.name()+" value of precondition of B ");
		Assert.assertEquals(3,lbls.labelMapConstructionOfOperations.size());

		{
			Label initmem = AbstractLearnerGraph.generateNewLabel(INITMEM, config,converter);
			SMTLabel l = lbls.labelMapConstructionOfOperations.get(initmem);
			Assert.assertEquals(initmem,l.getName());
			Assert.assertNull(l.post.text);
			Assert.assertEquals("varDecl",l.pre.text);
		}

		{
			Label labelA = AbstractLearnerGraph.generateNewLabel("A", config,converter);
			SMTLabel l = lbls.labelMapConstructionOfOperations.get(labelA);
			Assert.assertEquals(labelA,l.getName());
			Assert.assertNull(l.pre.text);
			Assert.assertEquals("postA and more\ndetails of postcondition of A",l.post.text);
		}

		{
			Label labelB = AbstractLearnerGraph.generateNewLabel("B", config,converter);
			SMTLabel l = lbls.labelMapConstructionOfOperations.get(labelB);
			Assert.assertEquals(labelB,l.getName());
			Assert.assertNull(l.post.text);
			Assert.assertEquals("value of precondition of B",l.pre.text);
		}
	}

	@Test
	public void testEquals()
	{
		SmtLabelRepresentation lblsA = new SmtLabelRepresentation(config,converter);
		lblsA.labelMapConstructionOfOperations = new TreeMap<Label,SMTLabel>();
		{
			lblsA.parseLabel(INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDecl"+_N);
			lblsA.parseLabel(INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " initCond"+_N);
			lblsA.parseLabel("A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA");
			lblsA.parseLabel("A"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA");
			lblsA.parseLabel("B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB");
			lblsA.parseLabel("B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB");
		}
		SmtLabelRepresentation lblsB = new SmtLabelRepresentation(config,converter);
		lblsB.labelMapConstructionOfOperations = new TreeMap<Label,SMTLabel>();
		{
			lblsB.parseLabel(INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ "    varDecl"+_N);
			lblsB.parseLabel(INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " initCond"+_N);
			lblsB.parseLabel("A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA");
			lblsB.parseLabel("A"+" "+SmtLabelRepresentation.OP_DATA.POST+ "    somePostcondA");
			lblsB.parseLabel("B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB");
			lblsB.parseLabel("B"+" "+SmtLabelRepresentation.OP_DATA.POST+ "       somePostcondB");
		}
		SmtLabelRepresentation lblsDiffA = new SmtLabelRepresentation(config,converter);
		lblsDiffA.labelMapConstructionOfOperations = new TreeMap<Label,SMTLabel>();
		{
			lblsDiffA.parseLabel(INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ "    varDecl"+_N);
			lblsDiffA.parseLabel(INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " initCond"+_N);
			lblsDiffA.parseLabel("A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA");
			lblsDiffA.parseLabel("A"+" "+SmtLabelRepresentation.OP_DATA.POST+ "    somePostcondA");
			lblsDiffA.parseLabel("B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB");
			lblsDiffA.parseLabel("B"+" "+SmtLabelRepresentation.OP_DATA.POST+ "       somePostcondB");
			lblsDiffA.parseLabel("B"+" "+SmtLabelRepresentation.OP_DATA.POST+ "       somePostcondB");
		}
		SmtLabelRepresentation lblsDiffB = new SmtLabelRepresentation(config,converter);
		lblsDiffB.labelMapConstructionOfOperations = new TreeMap<Label,SMTLabel>();
		TestEqualityComparisonAndHashCode.equalityTestingHelper(lblsA,lblsA,lblsDiffA,lblsDiffB, true);
	}

	public final String __P = SmtLabelRepresentation.delimiterString,__N = SmtLabelRepresentation.delimiterString+"-";
	
	@Test
	public void testRelabel()
	{
		Assert.assertEquals("text more var"+__P+"6 text var"+__N+"3 smth"+__P+"6 again ",
				SmtLabelRepresentation.toCurrentMem("text more var"+_N+" text var"+_M+" smth"+_N+" again ", 6,-3));
		Assert.assertEquals("text more var"+__N+"6 text var"+__P+"7 smth"+__N+"6 again ",
				SmtLabelRepresentation.toCurrentMem("text more var"+_N+" text var"+_M+" smth"+_N+" again ", -6,7));
		Assert.assertEquals("text more text var"+__P+"8 again ",
				SmtLabelRepresentation.toCurrentMem("text more text var"+_M+" again ", 6,8));
		Assert.assertEquals("true",
				SmtLabelRepresentation.toCurrentMem("", 6,4));
	}
	
	/** No exception since args are unused. */
	@Test
	public void testRelabel_unknownIgnored()
	{
		SmtLabelRepresentation.toCurrentMem("some text ",JUConstants.intUNKNOWN,5);
		SmtLabelRepresentation.toCurrentMem("some text ",7,JUConstants.intUNKNOWN);
	}
	
	@Test
	public void testRelabel_fail1()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation.toCurrentMem("some "+_M+" text ",5,JUConstants.intUNKNOWN);
		}}, IllegalArgumentException.class,"previous number should");
	}
	
	@Test
	public void testRelabel_fail2()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation.toCurrentMem("some "+_N+" text ",JUConstants.intUNKNOWN,6);
		}}, IllegalArgumentException.class,"current number should");
	}
	
	@Test
	public void testAppendToString1()
	{
		Assert.assertEquals("some text",SmtLabelRepresentation.appendToString(" some text",null));
		Assert.assertEquals(ENDL+"some text",SmtLabelRepresentation.appendToString("some text",""));
		Assert.assertNull(SmtLabelRepresentation.appendToString("  ",null));
		Assert.assertEquals(" __  ",SmtLabelRepresentation.appendToString(""," __  "));
		Assert.assertEquals(" __  ",SmtLabelRepresentation.appendToString("   "," __  "));
		Assert.assertEquals(" __  ",SmtLabelRepresentation.appendToString(null," __  "));
		Assert.assertNull(SmtLabelRepresentation.appendToString(null,null));
	}
	
	@Test
	public void testAppendToString2()
	{
		Assert.assertEquals("data"+ENDL+"some text",SmtLabelRepresentation.appendToString(" some text ","data"));
	}
	
	@Test
	public void testParseCollectionInvalidCommand()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.parseCollection(Arrays.asList(new String[]{
					"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA",
			}));
		}}, IllegalArgumentException.class,"invalid command");
	}
	
	@Test
	public void testParseCollectionMissingInitial()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA",
					QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA",
					QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB",
					QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"}));
		}}, IllegalArgumentException.class,"missing initial");
	}
	
	/** Uses _M in postcondition - prohibited because we'd like args to toCurrentMem to be different but INITMEM only
	 * uses one argument. Thus the _N is used and _M is prohibited.
	 */
	@Test
	public void testParseCollection_init_invalid_POST()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA",
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA"+SmtLabelRepresentation.varOldSuffix,
					QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB",
					QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"}));
		}}, IllegalArgumentException.class,SmtLabelRepresentation.INITMEM+" should not refer");
	}
	
	/** Uses _M in postcondition - prohibited because we'd like args to toCurrentMem to be different but INITMEM only
	 * uses one argument. Thus the _N is used and _M is prohibited.
	 */
	@Test
	public void testParseCollection_init_invalid_PRE()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA"+SmtLabelRepresentation.varOldSuffix,
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA",
					QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB",
					QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"}));
		}}, IllegalArgumentException.class,SmtLabelRepresentation.INITMEM+" should not refer");
	}
	
	@Test
	public void testParseCollectionInvalidFunctionName1()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (define varDecl"+_N+"::int)",
					QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ("+SmtLabelRepresentation.functionArg+")"}));
		}}, IllegalArgumentException.class,"invalid function name");
	}

	@Test
	public void testParseCollectionInvalidFunctionName_no_failure_if_does_not_start_with_frg()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (define varDecl"+_N+"::int)",
				QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (a"+SmtLabelRepresentation.functionArg+")"}));
	}

	@Test
	public void testParseCollectionInvalidFunctionName2()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " define varDecl"+_N+"::int)",
					QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (func"+SmtLabelRepresentation.delimiterString+")"}));
		}}, IllegalArgumentException.class,"invalid function name");
	}

	@Test
	public void testParseCollectionInvalidArgumentName()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " define varDecl"+_N+"::int)",
					QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (func "+SmtLabelRepresentation.functionArg+"arg )"}));
		}}, IllegalArgumentException.class,"invalid argument name");
	}

	/** This one is ok because the argument name does not start with a reserved name. */
	@Test
	public void testParseCollectionInvalidArgumentNameOk()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (define varDecl"+_N+"::int)",
				QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (func a"+SmtLabelRepresentation.functionArg+"arg )"}));
	}

	@Test
	public void testParseCollectionRepeatConstruction()
	{
		final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (define varDecl"+_N+"::int)",
				QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (func )"}));
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (define varDecl"+_N+"::int)",
					QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (func )"}));
		}}, IllegalArgumentException.class,"operations already built");
	}

	@Test
	public void testCreateConjunctionEmpty1()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " define varDecl"+_N+"",
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " decl"+_N,
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (somePrecondA)",
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (somePostcondA)",
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (somePrecondB)",
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (somePostcondB)"}));
		lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1", config,converter),null,true);
		Pair<String,String> state = lbls.getConjunctionForPath(Arrays.asList(new SMTLabel[]{}),null);
		int number = 4;
		Assert.assertEquals("define varDecl"+__P+number+" decl"+__P+number+ENDL,state.firstElem);
		Assert.assertEquals(SmtLabelRepresentation.commentForNewSeq+"[]"+ENDL+"true"+ENDL, state.secondElem);
	}
	
	@Test
	public void testCreateConjunctionEmpty2()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDecl"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= initCond"+_N+" 7)",
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (somePrecondA)",
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (somePostcondA)",
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (somePrecondB)",
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (somePostcondB)"}));
		lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1", config,converter),null,true);
		Pair<String,String>  state = lbls.getConjunctionForPath(Arrays.asList(new SMTLabel[]{}),null);
		int number = 4;
		Assert.assertEquals("varDecl"+__P+number+ENDL,state.firstElem);
		Assert.assertEquals(SmtLabelRepresentation.commentForNewSeq+"[]"+ENDL+
				"(and"+ENDL+SmtLabelRepresentation.commentForInit+ENDL+"(= initCond"+__P+number+" 7)"+ENDL+')'+ENDL,state.secondElem);
	}
	
	@Test
	public void testbuildVertexToAbstractStateMapUnknown1()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDecl"+_N,
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " initCond"+_N,
				QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA",
				QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA",
				QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB",
				QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"}));
			lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-unknown_label->stD", "testCreateConjunctionUnknown1", config,converter),null,true);
		}}, IllegalArgumentException.class,"unknown label unknown_label");
	}
	
	@Test
	public void testbuildVertexToAbstractStateMapUnknown2()
	{
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDecl"+_N,
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " initCond"+_N,
				QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA",
				QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA",
				QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB",
				QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"}));
			lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-A->stB-B->stC-unknown_label->stD", "testCreateConjunctionUnknown2", config,converter),null,true);
		}}, IllegalArgumentException.class,"unknown label unknown_label");
	}
	
	@Test
	public void testCreateConjunction_mismatchedLength()
	{
		final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " initCond"+_N,
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_M,
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA"+_N,
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_M,
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N}));
		lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1", config,converter),null,true);
	
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			lbls.getConjunctionForPath(
					Arrays.asList(new SMTLabel[]{
							lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("A",lbls.config,lbls.converter)),
							lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("B",lbls.config,lbls.converter))}),
					Arrays.asList(new SmtLabelRepresentation.CompositionOfFunctions[]{}));
		}}, IllegalArgumentException.class,"mismatched length");
	}
	
	@Test
	public void testCreateConjunction_constructionIncomplete1()
	{
		final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " initCond"+_N,
				QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_N,
				QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA"+_N,
				QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_N,
				QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N}));
		
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			lbls.getConjunctionForPath(
					Arrays.asList(new SMTLabel[]{
							lbls.labelMapConstructionOfDataTraces.get(AbstractLearnerGraph.generateNewLabel("A",lbls.config,lbls.converter)),
							lbls.labelMapConstructionOfDataTraces.get(AbstractLearnerGraph.generateNewLabel("B",lbls.config,lbls.converter))}),null);
		}}, IllegalArgumentException.class,"construction incomplete");
	}
		
	@Test
	public void testCreateConjunction1()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N+"",
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N+"",
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " initCond"+_N+"",
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_M+"",
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA"+_N+"",
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_M+"",
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N+""}));
		lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1", config,converter),null,true);
		Pair<String,String> state = lbls.getConjunctionForPath(
				Arrays.asList(new SMTLabel[]{
						lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("A",lbls.config,lbls.converter)),
						lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("B",lbls.config,lbls.converter))}),null);
		int number = 4;
		Assert.assertEquals("varDeclP"+__P+(number+0)+" varDeclQ"+__P+(number+0)+ENDL+
				"varDeclP"+__P+(number+1)+" varDeclQ"+__P+(number+1)+ENDL+
				"varDeclP"+__P+(number+2)+" varDeclQ"+__P+(number+2)+ENDL,state.firstElem);

		Assert.assertEquals(SmtLabelRepresentation.commentForNewSeq+"[A,B]"+ENDL+
				"(and"+ENDL+SmtLabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+(number+0)+ENDL+
				SmtLabelRepresentation.commentForLabel+"A"+ENDL+
				"somePrecondA"+__P+(number+0)+ENDL+
				"somePostcondA"+__P+(number+1)+ENDL+
				SmtLabelRepresentation.commentForLabel+"B"+ENDL+
				"somePrecondB"+__P+(number+1)+ENDL+
				"somePostcondB"+__P+(number+2)+ENDL+')'+ENDL,
				state.secondElem);
	}
	
	public final static List<String> declsForTestsOfAbstractStates = Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " initCond"+_N,
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA"+_N,
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_M,
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N,
			QSMTool.cmdOperation+" "+"IO1"+" "+SmtLabelRepresentation.OP_DATA.POST+ " m"+_N+"=m"+_M, // this postcondition is what I'll use as an IO			
			QSMTool.cmdOperation+" "+"IO2"+" "+SmtLabelRepresentation.OP_DATA.POST+ " m"+_N+"=-m"+_M // this postcondition is what I'll use as an IO			
				});
	
	@Test
	public final void testCreateAbstractState1()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDecl"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " initCond"+_N,
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA",
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA",
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB",
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"}));
		lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1", config,converter),null,true);
		AbstractState state = lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("P"),config),0);
		Assert.assertEquals("varDecl"+__P+"0",state.variableDeclarations);
		Assert.assertEquals(SmtLabelRepresentation.commentForInit+ENDL+"initCond"+__P+"0",state.abstractState);
		Assert.assertNull(state.lastLabel);
		Assert.assertNull(state.previousState);
		Assert.assertEquals(0,state.stateNumber);
	}
	
	/** Previous label not null but IO is null. */
	@Test
	public final void testCreateAbstractState2()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		List<String> decls = new LinkedList<String>();decls.addAll(declsForTestsOfAbstractStates);
		decls.add(QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_M);
		lbls.parseCollection(decls);
		lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1", config,converter),null,true);
		int number0 = 10,number1=15,number2=20;
		AbstractState stateInit = lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("Init"),config),number0);
		AbstractState stateAfterA = lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("AfterA"),config),stateInit,
				lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("A",lbls.config,lbls.converter)),null,number1);
		AbstractState stateAfterB = lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("AfterB"),config),stateAfterA,
				lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("B",lbls.config,lbls.converter)),null,number2);
		Assert.assertEquals("varDeclP"+__P+number0+" varDeclQ"+__P+number0+ENDL+
				"varDeclP"+__P+number1+" varDeclQ"+__P+number1+ENDL+
				"varDeclP"+__P+number2+" varDeclQ"+__P+number2,stateAfterB.variableDeclarations);

		Assert.assertEquals(SmtLabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+number0+ENDL+
				SmtLabelRepresentation.commentForLabel+"A"+ENDL+
				"somePrecondA"+__P+number0+ENDL+
				"somePostcondA"+__P+number1+ENDL+
				SmtLabelRepresentation.commentForLabel+"B"+ENDL+
				"somePrecondB"+__P+number1+ENDL+
				"somePostcondB"+__P+number2,
				stateAfterB.abstractState);
		Assert.assertEquals("AfterB",stateAfterB.vertex.toString());
		Assert.assertSame(lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("B",lbls.config,lbls.converter)),stateAfterB.lastLabel);
		Assert.assertEquals(number2,stateAfterB.stateNumber);
	}
	
	/** Construction of abstract labels which use a non-null IO. */
	@Test
	public final void testCreateAbstractState3()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(declsForTestsOfAbstractStates);
		lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1", config,converter),null,true);
		int number0 = 10,number1=15,number2=20;
		AbstractState stateInit = lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("Init"),config),number0);
		AbstractState stateAfterA = lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("AfterA"),config),stateInit,
				lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("A",lbls.config,lbls.converter)),
				lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("IO1",lbls.config,lbls.converter)).post,number1);
		AbstractState stateAfterB = lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("AfterB"),config),stateAfterA,
				lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("B",lbls.config,lbls.converter)),
				lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("IO2",lbls.config,lbls.converter)).post,number2);
		Assert.assertEquals("varDeclP"+__P+number0+" varDeclQ"+__P+number0+ENDL+
				"varDeclP"+__P+number1+" varDeclQ"+__P+number1+ENDL+
				"varDeclP"+__P+number2+" varDeclQ"+__P+number2,stateAfterB.variableDeclarations);

		Assert.assertEquals(SmtLabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+number0+ENDL+
				SmtLabelRepresentation.commentForLabel+"A"+ENDL+
				"m"+__P+number1+"=m"+__P+number0+ENDL+
				"true"+ENDL+// the precondition of A is not defined, hence considered always true and is replaced with "true".
				"somePostcondA"+__P+number1+ENDL+
				SmtLabelRepresentation.commentForLabel+"B"+ENDL+
				"m"+__P+number2+"=-m"+__P+number1+ENDL+
				"somePrecondB"+__P+number1+ENDL+
				"somePostcondB"+__P+number2,
				stateAfterB.abstractState);
		Assert.assertEquals("AfterB",stateAfterB.vertex.toString());
		Assert.assertSame(lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("B",lbls.config,lbls.converter)),stateAfterB.lastLabel);
		Assert.assertEquals(number2,stateAfterB.stateNumber);
	}

	/** Previous state is null. */
	@Test
	public final void testCreateAbstractState_fail1()
	{
		final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(declsForTestsOfAbstractStates);
		lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1", config,converter),null,true);
		
		Helper.checkForCorrectException(new whatToRun() { @SuppressWarnings("unused")
		public @Override void run() {
			lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("AfterA"),config),null,
					lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("A",lbls.config,lbls.converter)),
							lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("IO1",lbls.config,lbls.converter)).post,7);
		}},IllegalArgumentException.class, "previous state");
	}
	
	/** Previous label is null. */
	@Test
	public final void testCreateAbstractState_fail2()
	{
		final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(declsForTestsOfAbstractStates);
		lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1", config,converter),null,true);
		
		Helper.checkForCorrectException(new whatToRun() { @SuppressWarnings("unused")
		public @Override void run() {
			AbstractState stateInit = lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("Init"),config),6);
			lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("AfterA"),config),stateInit,null,null,7);
		}},IllegalArgumentException.class, "previous state");
	}
	
	/** Previous label is null but previous io is not. */
	@Test
	public final void testCreateAbstractState_fail3()
	{
		final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(declsForTestsOfAbstractStates);
		lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1", config,converter),null,true);
		
		Helper.checkForCorrectException(new whatToRun() { @SuppressWarnings("unused")
		public @Override void run() {
			AbstractState stateInit = lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("Init"),config),6);
			lbls.new AbstractState(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("AfterA"),config),stateInit,null,
					lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("IO1",lbls.config,lbls.converter)).post,7);
		}},IllegalArgumentException.class, "previous state");
	}
	
	private SmtLabelRepresentation testCreateConjunction2_internal()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_M,
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_M,
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N}));
		lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1", config,converter),null,true);
		Pair<String,String> state = lbls.getConjunctionForPath(
				Arrays.asList(new SMTLabel[]{
						lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("A",lbls.config,lbls.converter)),
						lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("B",lbls.config,lbls.converter))}),null);
		int number = 4;
		Assert.assertEquals("varDeclP"+__P+(number+0)+" varDeclQ"+__P+(number+0)+ENDL+
				"varDeclP"+__P+(number+1)+" varDeclQ"+__P+(number+1)+ENDL+
				"varDeclP"+__P+(number+2)+" varDeclQ"+__P+(number+2)+ENDL, state.firstElem);

		Assert.assertEquals(SmtLabelRepresentation.commentForNewSeq+"[A,B]"+ENDL+
				"(and"+ENDL+SmtLabelRepresentation.commentForInit+ENDL+
				SmtLabelRepresentation.commentForLabel+"A"+ENDL+
				"somePrecondA"+__P+(number+0)+ENDL+
				SmtLabelRepresentation.commentForLabel+"B"+ENDL+
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
		SmtLabelRepresentation lbls = testCreateConjunction2_internal();
		Pair<String,String> state = lbls.getConjunctionForPath(
				Arrays.asList(new SMTLabel[]{
						lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("A",lbls.config,lbls.converter)),
						lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("B",lbls.config,lbls.converter))}),null);
		int number = 7;
		Assert.assertEquals("varDeclP"+__P+(number+0)+" varDeclQ"+__P+(number+0)+ENDL+
				"varDeclP"+__P+(number+1)+" varDeclQ"+__P+(number+1)+ENDL+
				"varDeclP"+__P+(number+2)+" varDeclQ"+__P+(number+2)+ENDL, state.firstElem);

		Assert.assertEquals(SmtLabelRepresentation.commentForNewSeq+"[A,B]"+ENDL+
				"(and"+ENDL+SmtLabelRepresentation.commentForInit+ENDL+
				SmtLabelRepresentation.commentForLabel+"A"+ENDL+
				"somePrecondA"+__P+(number+0)+ENDL+
				SmtLabelRepresentation.commentForLabel+"B"+ENDL+
				"somePrecondB"+__P+(number+1)+ENDL+
				"somePostcondB"+__P+(number+2)+ENDL+')'+ENDL,
				state.secondElem
				);
	}

	/** Tests that conjunctions may include sequences of IO. */
	@Test
	public final void testCreateConjunction4()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_M,
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_M,
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N,
			QSMTool.cmdOperation+" "+"IO1"+" "+SmtLabelRepresentation.OP_DATA.POST+ " m"+_N+"=m"+_M, // this postcondition is what I'll use as an IO			
			QSMTool.cmdOperation+" "+"IO2"+" "+SmtLabelRepresentation.OP_DATA.POST+ " m"+_N+"=-m"+_M // this postcondition is what I'll use as an IO			
			}));
		lbls.buildVertexToAbstractStateMap(buildLearnerGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1", config,converter),null,true);
		Pair<String,String> state = lbls.getConjunctionForPath(
				Arrays.asList(new SMTLabel[]{
						lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("A",lbls.config,lbls.converter)),
						lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("B",lbls.config,lbls.converter))}),
				Arrays.asList(new SmtLabelRepresentation.CompositionOfFunctions[]{
						lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("IO1",lbls.config,lbls.converter)).post,
								lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("IO2",lbls.config,lbls.converter)).post}));
		int number = 4;
		Assert.assertEquals("varDeclP"+__P+(number+0)+" varDeclQ"+__P+(number+0)+ENDL+
				"varDeclP"+__P+(number+1)+" varDeclQ"+__P+(number+1)+ENDL+
				"varDeclP"+__P+(number+2)+" varDeclQ"+__P+(number+2)+ENDL, state.firstElem);

		Assert.assertEquals(SmtLabelRepresentation.commentForNewSeq+"[A(m"+_N+"=m"+_M+"),B(m"+_N+"=-m"+_M+")]"+ENDL+
				"(and"+ENDL+SmtLabelRepresentation.commentForInit+ENDL+
				SmtLabelRepresentation.commentForLabel+"A"+ENDL+
				"m"+__P+(number+1)+"=m"+__P+(number+0)+ENDL+
				"somePrecondA"+__P+(number+0)+ENDL+
				SmtLabelRepresentation.commentForLabel+"B"+ENDL+
				"m"+__P+(number+2)+"=-m"+__P+(number+1)+ENDL+
				"somePrecondB"+__P+(number+1)+ENDL+
				"somePostcondB"+__P+(number+2)+ENDL+
				')'+ENDL,
				state.secondElem
				);
		
	}
	
	
	@Test
	public void testCheckConsistency_constructionIncomplete()
	{
		final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " initCond"+_N,
				QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_N,
				QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA"+_N,
				QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_N,
				QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N}));
		
		final LearnerGraph graph = new LearnerGraph(config);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run()
		{
			config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.NONE);
			lbls.checkConsistency(graph,config);
		}}, IllegalArgumentException.class,"construction incomplete");
	}

	/** Tests that data traces cannot be re-added to a graph with an existing labelling of abstract states. */
	@Test
	public final void testDataTracesToAbstractStates_fail()
	{
		final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " define varDecl"+_N+"",
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " decl"+_N,
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (somePrecondA)",
			QSMTool.cmdOperation+" "+"A"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (somePostcondA)",
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (somePrecondB)",
			QSMTool.cmdOperation+" "+"B"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (somePostcondB)"}));
		final LearnerGraph graph = buildLearnerGraph("stA-A->stB-B->stC-A->stD", "testCreateConjunction1",config,converter);
		lbls.buildVertexToAbstractStateMap(graph,null,true);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			lbls.addAbstractStatesFromTraces(graph);
		}}, IllegalArgumentException.class,"data traces should not be added to a graph with existing");
	}

	/** This one tests that regardless of whether we've got low-level functions, all the checks can be 
	 * carried out. The difference between having low-level functions and not is that in 
	 * their presence, all abstract states are always loaded into yices context. 
	 */
	@RunWith(ParameterizedWithName.class)
	public static class TestChecksInTwoContexts
	{
		Configuration config = null;
		ConvertALabel converter = null;
		
		/** Converts arrays of labels to lists of labels using config - it does not really matter which configuration is used 
		 * because all of them start from a default one and do not modify label type.
		 * 
		 * @param labels what to convert
		 * @return the outcome of conversion.
		 */
		protected List<statechum.Label> labelList(String [] labels)
		{
			return AbstractLearnerGraph.buildList(Arrays.asList(labels),config,converter);
		}
		
		final boolean lowLevel;
		
		public TestChecksInTwoContexts(Boolean useLowLevel)
		{
			lowLevel = useLowLevel.booleanValue();
		}
		
		@Before
		public void beforeTest()
		{
			config = Configuration.getDefaultConfiguration().copy(); 
		}
		
		@org.junit.runners.Parameterized.Parameters
		public static Collection<Object []> data() 
		{
			Collection<Object[]> result = new LinkedList<Object[]>();
			result.add(new Object[]{new Boolean(true)});result.add(new Object[]{new Boolean(false)});
			
			return result;
		}

		@org.junit.runners.ParameterizedWithName.ParametersToString
		public static String parametersToString(Boolean useLowLevel)
		{
			return useLowLevel.booleanValue()?"using low-level functions":"without low-level functions";
		}

		/** Checks that we can use abstract states to compute state compatibility. */
		@Test
		public final void testUpdateScore()
		{
			final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);lbls.usingLowLevelFunctions = lowLevel;
			lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
			LearnerGraph graph = buildLearnerGraph("A-add->B-add->C-add->D\nB-remove->E-add->F","testUpdateScore", config,converter);
			lbls.buildVertexToAbstractStateMap(graph,null,true);
			
			config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
			Assert.assertNull(lbls.checkConsistency(graph,config));
	
			Assert.assertTrue(lbls.abstractStatesCompatible(extractAbstractStateFrom(graph,"A"), extractAbstractStateFrom(graph,"A")));
			Assert.assertFalse(lbls.abstractStatesCompatible(extractAbstractStateFrom(graph,"A"), extractAbstractStateFrom(graph,"B")));
			Assert.assertFalse(lbls.abstractStatesCompatible(extractAbstractStateFrom(graph,"B"), extractAbstractStateFrom(graph,"A")));
			
			Assert.assertTrue(lbls.abstractStatesCompatible(extractAbstractStateFrom(graph,"A"), extractAbstractStateFrom(graph,"E")));
			Assert.assertTrue(lbls.abstractStatesCompatible(extractAbstractStateFrom(graph,"B"), extractAbstractStateFrom(graph,"F")));
	
			Assert.assertFalse(lbls.abstractStatesCompatible(extractAbstractStateFrom(graph,"D"), extractAbstractStateFrom(graph,"F")));
		}

		/** Initial accept-state should have a satisfiable abstract state. */
		@Test
		public final void testAugmentCheck1()
		{
			final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);lbls.usingLowLevelFunctions = lowLevel;
			lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (and (= m"+_N+" 0) (= m"+_N+" 1))",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
			final LearnerGraph graph = new LearnerGraph(config);
			lbls.buildVertexToAbstractStateMap(graph, null,true);
			
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
			final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);lbls.usingLowLevelFunctions = lowLevel;
			lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
			final LearnerGraph graph = new LearnerGraph(config);graph.getInit().setAccept(false);
			lbls.buildVertexToAbstractStateMap(graph, null,true);
			
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
			final SmtLabelRepresentation lbls = simpleLabel();
			final LearnerGraph graph = new LearnerGraph(config);
	
			final List<statechum.Label> sequence = labelList(new String[]{"remove"});
			graph.paths.augmentPTA(sequence,false, false, null);
			
			lbls.buildVertexToAbstractStateMap(graph, null,true);
			config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
			Assert.assertNull(lbls.checkConsistency(graph,config));
		}
		
		SmtLabelRepresentation simpleLabel()
		{
			final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);lbls.usingLowLevelFunctions = lowLevel;
			lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
			return lbls;
		}
	
		/** Augmenting reject-paths. */
		@Test
		public final void testAugmentCheck4()
		{
			final SmtLabelRepresentation lbls = simpleLabel();
			final LearnerGraph graph = new LearnerGraph(config);
			
			final List<statechum.Label> sequence = labelList(new String[]{"remove"});
			graph.paths.augmentPTA(sequence,true, false, null);
			
			lbls.buildVertexToAbstractStateMap(graph, null,true);
			config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
			Assert.assertTrue(lbls.checkConsistency(graph,config).getMessage().contains("has an abstract state inconsistent with the accept condition"));
		}
	
		/** Augmenting reject-paths. */
		@Test
		public final void testAugmentCheck5()
		{
			final SmtLabelRepresentation lbls = simpleLabel();
			final LearnerGraph graph = new LearnerGraph(config);
			final List<statechum.Label> sequence = labelList(new String[]{"add","remove","remove"});
			graph.paths.augmentPTA(sequence,false, false, null);
			
			lbls.buildVertexToAbstractStateMap(graph, null,true);
			config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
			Assert.assertNull(lbls.checkConsistency(graph,config));
		}
	
		/** Augmenting reject-paths. */
		@Test
		public final void testAugmentCheck6()
		{
			final SmtLabelRepresentation lbls = simpleLabel();
			final LearnerGraph graph = new LearnerGraph(config);
	
			final List<statechum.Label> sequence = labelList(new String[]{"add","remove","remove"});
			graph.paths.augmentPTA(sequence,true, false, null);
	
			lbls.buildVertexToAbstractStateMap(graph, null,true);
			config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
			Assert.assertTrue(lbls.checkConsistency(graph,config).getMessage().contains("has an abstract state inconsistent with the accept condition"));
		}
	
		/** Augmenting accept-paths. */
		@Test
		public final void testCheck7()
		{
			final SmtLabelRepresentation lbls = simpleLabel();
			final LearnerGraph graph = new LearnerGraph(config);
	
			final List<statechum.Label> sequence = labelList(new String[]{"add","remove","add"});
			graph.paths.augmentPTA(sequence,true, false, null);
	
			lbls.buildVertexToAbstractStateMap(graph, null,true);
			config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
			Assert.assertNull(lbls.checkConsistency(graph,config));
		}
	
		/** Checks that it is possible to check that all states can be entered. */
		@Test
		public final void testCheck8()
		{
			SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
			LearnerGraph graph = buildLearnerGraph("A-add->B-add->C-add->D\nB-remove->E-add->F","testUpdateScore", config,converter);
			lbls.buildVertexToAbstractStateMap(graph,null,true);
			
			config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
			Assert.assertNull(lbls.checkConsistency(graph,config));
		}
		
		/** Checks that it is possible to check that all states can be entered. */
		@Test
		public final void testCheck9()
		{
			final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
			lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
			LearnerGraph graph = buildLearnerGraph("A-add->B\nA-remove->S","testAbstractStateSatisfiability2", config,converter);
			lbls.buildVertexToAbstractStateMap(graph,null,true);
			
			config.setSmtGraphDomainConsistencyCheck(SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST);
			Assert.assertTrue(lbls.checkConsistency(graph,config).getMessage().contains("has an abstract state inconsistent with the accept condition"));
		}
	} // TestChecksInBothContexts.
	
	// FIXME: to test allnone, inclusion of a postcondition and determinism.
	
	/** The <em>testCreateIDToStateMap1</em> is in TestFSMAlgo. */
	@Test
	public final void testCreateIDToStateMap2()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " initCond"+_N,
			QSMTool.cmdOperation+" "+"a"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_M,
			QSMTool.cmdOperation+" "+"a"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA"+_N,
			QSMTool.cmdOperation+" "+"b"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_M,
			QSMTool.cmdOperation+" "+"b"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N}));
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->C-a->D\nB-b->E", "testCreateIDToStateMap2",config,converter);
		lbls.buildVertexToAbstractStateMap(graph, null,true);

		//for(Entry<VertexID,AbstractState> entry:lbls.idToState.entrySet())
		//	System.out.println(entry.getKey()+" "+entry.getValue().abstractState);
		Assert.assertEquals(5,graph.getVertexToAbstractState().size());
		for(Entry<CmpVertex,Collection<AbstractState>> entry:graph.getVertexToAbstractState().entrySet())
			Assert.assertEquals(1,entry.getValue().size());
		int varNumber = 0;
		Assert.assertEquals(
				SmtLabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+varNumber,
				extractAbstractStateFrom(graph,"A").abstractState);
		
		Assert.assertEquals(
				SmtLabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+(varNumber+0)+ENDL+
				SmtLabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+0)+ENDL+
				"somePostcondA"+__P+(varNumber+1),
				extractAbstractStateFrom(graph,"B").abstractState);
		
		Assert.assertEquals(
				SmtLabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+(varNumber+0)+ENDL+
				SmtLabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+0)+ENDL+
				"somePostcondA"+__P+(varNumber+1)+ENDL+
				SmtLabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+1)+ENDL+
				"somePostcondA"+__P+(varNumber+2),
				extractAbstractStateFrom(graph,"C").abstractState);
		
		Assert.assertEquals(
				SmtLabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+(varNumber+0)+ENDL+
				SmtLabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+0)+ENDL+
				"somePostcondA"+__P+(varNumber+1)+ENDL+
				SmtLabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+1)+ENDL+
				"somePostcondA"+__P+(varNumber+2)+ENDL+
				SmtLabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+2)+ENDL+
				"somePostcondA"+__P+(varNumber+4),
				extractAbstractStateFrom(graph,"D").abstractState);
		
		Assert.assertEquals(
				SmtLabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+(varNumber+0)+ENDL+
				SmtLabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+0)+ENDL+
				"somePostcondA"+__P+(varNumber+1)+ENDL+
				SmtLabelRepresentation.commentForLabel+"b"+ENDL+
				"somePrecondB"+__P+(varNumber+1)+ENDL+
				"somePostcondB"+__P+(varNumber+3),
				extractAbstractStateFrom(graph,"E").abstractState);
	}
	
	/** The <em>testCreateIDToStateMap1</em> is in TestFSMAlgo. 
	 * Similar to testCreateIDToStateMap3 but checks that reject-state is ignored.
	 */
	@Test
	public final void testCreateIDToStateMap3()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclP"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDeclQ"+_N,
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " initCond"+_N,
			QSMTool.cmdOperation+" "+"a"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA"+_M,
			QSMTool.cmdOperation+" "+"a"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA"+_N,
			QSMTool.cmdOperation+" "+"b"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB"+_M,
			QSMTool.cmdOperation+" "+"b"+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"+_N}));
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->C-a-#D\nB-b->E", "testCreateIDToStateMap2",config,converter);
		lbls.buildVertexToAbstractStateMap(graph, null,true);

		//for(Entry<VertexID,AbstractState> entry:lbls.idToState.entrySet())
		//	System.out.println(entry.getKey()+" "+entry.getValue().abstractState);
		Assert.assertEquals(4,graph.getVertexToAbstractState().size());
		for(Entry<CmpVertex,Collection<AbstractState>> entry:graph.getVertexToAbstractState().entrySet())
			Assert.assertEquals(1,entry.getValue().size());
		int varNumber = 0;
		Assert.assertEquals(
				SmtLabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+varNumber,
				extractAbstractStateFrom(graph,"A").abstractState);
		
		Assert.assertEquals(
				SmtLabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+(varNumber+0)+ENDL+
				SmtLabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+0)+ENDL+
				"somePostcondA"+__P+(varNumber+1),
				extractAbstractStateFrom(graph,"B").abstractState);
		
		Assert.assertEquals(
				SmtLabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+(varNumber+0)+ENDL+
				SmtLabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+0)+ENDL+
				"somePostcondA"+__P+(varNumber+1)+ENDL+
				SmtLabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+1)+ENDL+
				"somePostcondA"+__P+(varNumber+2),
				extractAbstractStateFrom(graph,"C").abstractState);
		
		Assert.assertFalse(graph.getVertexToAbstractState().containsKey(graph.findVertex(VertexID.parseID("D"))));
		
		Assert.assertEquals(
				SmtLabelRepresentation.commentForInit+ENDL+
				"initCond"+__P+(varNumber+0)+ENDL+
				SmtLabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA"+__P+(varNumber+0)+ENDL+
				"somePostcondA"+__P+(varNumber+1)+ENDL+
				SmtLabelRepresentation.commentForLabel+"b"+ENDL+
				"somePrecondB"+__P+(varNumber+1)+ENDL+
				"somePostcondB"+__P+(varNumber+3),
				extractAbstractStateFrom(graph,"E").abstractState);
	}
	
	@Test
	public void testSolvingConstraints()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
			QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
			QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
		LearnerGraph graph = buildLearnerGraph("A-add->B-remove->C-remove-#D\nB-add->E", "testSolvingConstraints",config,converter);
		lbls.buildVertexToAbstractStateMap(graph, null,true);

		Pair<String,String> state = null;
		state = lbls.getConjunctionForPath(Arrays.asList(new SMTLabel[]{
				lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("remove",lbls.config,lbls.converter))}),null);
		Assert.assertFalse(lbls.checkSatisfiability(state.firstElem, state.secondElem));

		state = lbls.getConjunctionForPath(Arrays.asList(new SMTLabel[]{}),null);
		Assert.assertTrue(lbls.checkSatisfiability(state.firstElem, state.secondElem));

		state = lbls.getConjunctionForPath(Arrays.asList(new SMTLabel[]{
				lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("add",lbls.config,lbls.converter)),
				lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("remove",lbls.config,lbls.converter))}),null);
		Assert.assertTrue(lbls.checkSatisfiability(state.firstElem, state.secondElem));

		state = lbls.getConjunctionForPath(Arrays.asList(new SMTLabel[]{
				lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("add",lbls.config,lbls.converter)),
				lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("remove",lbls.config,lbls.converter)),
				lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("remove",lbls.config,lbls.converter))}),null);
		Assert.assertFalse(lbls.checkSatisfiability(state.firstElem, state.secondElem));
	}
	
	/** Obtains an abstract state corresponding to the specific state in a specific graph. */
	static AbstractState extractAbstractStateFrom(LearnerGraph graph, String stateName)
	{
		Collection<AbstractState> abstractStates = graph.getVertexToAbstractState().get(graph.findVertex(VertexID.parseID(stateName)));
		Assert.assertEquals(1,abstractStates.size());
		return abstractStates.iterator().next();
	}
	
	@Test
	public final void testCheckWithEndUser()
	{
		SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
			QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
			QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
		LearnerGraph graph = buildLearnerGraph("A-add->B","testUpdateScore", config,converter);
		lbls.buildVertexToAbstractStateMap(graph,null,true);
		
		Assert.assertEquals(AbstractOracle.USER_ACCEPTED,lbls.CheckWithEndUser(labelList(new String[]{})));
		Assert.assertEquals(0,lbls.CheckWithEndUser(labelList(new String[]{"remove"})));
		Assert.assertEquals(AbstractOracle.USER_ACCEPTED,lbls.CheckWithEndUser(labelList(new String[]{"add"})));
		Assert.assertEquals(AbstractOracle.USER_ACCEPTED,lbls.CheckWithEndUser(labelList(new String[]{"add","remove"})));
		Assert.assertEquals(2,lbls.CheckWithEndUser(labelList(new String[]{"add","remove","remove"})));
		Assert.assertEquals(AbstractOracle.USER_ACCEPTED,lbls.CheckWithEndUser(labelList(new String[]{"add","remove","add","add"})));
	}
	
	@Test
	public final void testCheckWithEndUser_fail()
	{
		final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
			QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
			QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_N+" 0)",
			QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))"}));
		LearnerGraph graph = buildLearnerGraph("A-add->B","testUpdateScore", config,converter);
		lbls.buildVertexToAbstractStateMap(graph,null,true);
		
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			lbls.CheckWithEndUser(labelList(new String[]{"aa"}));
		}},IllegalArgumentException.class,"unknown label");
	}

	/** Tests contained in this class require a "personal" beforeTest method, otherwise if 
	 * beforeTest fails, all tests in TestSmtLabelRepresentation will fail. 
	 */
	public static class TestFeaturesOfAbstractStates
	{
		SmtLabelRepresentation lbls;
		LearnerGraph graph;
		ConvertALabel converter = null;
		
		/** Converts arrays of labels to lists of labels using config - it does not really matter which configuration is used 
		 * because all of them start from a default one and do not modify label type.
		 * 
		 * @param labels what to convert
		 * @return the outcome of conversion.
		 */
		protected List<statechum.Label> labelList(String [] labels)
		{
			return AbstractLearnerGraph.buildList(Arrays.asList(labels),graph.config,converter);
		}
		
		public final String __P = SmtLabelRepresentation.delimiterString,__N = SmtLabelRepresentation.delimiterString+"-";

		public final Collection<String> sampleSpecification = Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define a"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" a"+_N+"))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))",
				QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.ARITY+" 2",
				QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0::int)",
				QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"1::int)",
				QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"2::int)",
				
				// There are a few data traces to be added.
				QSMTool.cmdDataTrace+" + add((= a"+_N+" 2)) remove",
				QSMTool.cmdDataTrace+" + add((= a"+_N+" 1)) add((= a"+_N+" 1)) remove",
			});
		
		@Before
		public final void beforeTest()
		{
			Configuration cnf = Configuration.getDefaultConfiguration().copy();
			lbls = new SmtLabelRepresentation(cnf,converter);
			lbls.parseCollection(sampleSpecification);
			
			graph = new LearnerGraph(cnf);
			graph.paths.augmentPTA(lbls.getSPlus(), true, false);
			graph.paths.augmentPTA(lbls.getSMinus(), false, false);
			
			lbls.buildVertexToAbstractStateMap(graph, null,true);
		}
		
		/** Tests that abstract states can be correctly added from data traces. 
		 * We do not need to test that sets of abstract states are merged correctly because 
		 * TestFSMAlgo does this already. 
		 */
		@Test
		public final void testDataTracesToAbstractStates1()
		{
			
			Map<CmpVertex,List<statechum.Label>> paths = graph.pathroutines.computeShortPathsToAllStates();
			CmpVertex vertexWithTwoAbstractStates = null;
			for(Entry<CmpVertex,List<statechum.Label>> entry:paths.entrySet())
			{
				Collection<AbstractState> abstractStates = graph.getVertexToAbstractState().get(entry.getKey());//graph.findVertex(VertexID.parseID(stateName)));
				Assert.assertEquals(entry.getValue().size() == 1?2:1,abstractStates.size());
				if (entry.getValue().size() == 1)
				{
					Assert.assertNull("there has to be only one vertex with multiple abstract states associated with it",vertexWithTwoAbstractStates);
					vertexWithTwoAbstractStates = entry.getKey();
				}
			}
			Assert.assertNotNull("there has to be one vertex with multiple abstract states associated with it",vertexWithTwoAbstractStates);
			
			int varNumberInit =0,varNumber1=1,varNumber2=3;
			Collection<AbstractState> abstractStates = graph.getVertexToAbstractState().get(vertexWithTwoAbstractStates);
			Iterator<AbstractState> abIterator = abstractStates.iterator();
			Assert.assertEquals(
					SmtLabelRepresentation.commentForInit+ENDL+
					"(= m"+__P+(varNumberInit)+" 0)"+ENDL+
					SmtLabelRepresentation.commentForLabel+"add"+ENDL+
					"(= a"+__P+(varNumber1)+" 2)"+ENDL+
					"true"+ENDL+
					"(= m"+__P+(varNumber1)+" (+ m"+__P+(varNumberInit)+" a"+__P+(varNumber1)+"))",
					abIterator.next().abstractState);
			Assert.assertEquals(
					SmtLabelRepresentation.commentForInit+ENDL+
					"(= m"+__P+(varNumberInit)+" 0)"+ENDL+
					SmtLabelRepresentation.commentForLabel+"add"+ENDL+
					"(= a"+__P+(varNumber2)+" 1)"+ENDL+
					"true"+ENDL+
					"(= m"+__P+(varNumber2)+" (+ m"+__P+(varNumberInit)+" a"+__P+(varNumber2)+"))",
					abIterator.next().abstractState);
			Assert.assertFalse(abIterator.hasNext());
		}
		
		/** Tests that abstract states have not been modified when we do buildVertexToAbstractStateMap again.
		 * Since abstract states are immutable, we can simply compare the maps. */
		@Test
		public final void testUnmodifiedAbstractStates()
		{
			Map<CmpVertex,Collection<SmtLabelRepresentation.AbstractState>> orig = new TreeMap<CmpVertex,Collection<SmtLabelRepresentation.AbstractState>>();
			orig.putAll(graph.getVertexToAbstractState());
			lbls.buildVertexToAbstractStateMap(graph, null,true);
			Assert.assertEquals(orig,graph.getVertexToAbstractState());
		}
		
		/** Tests that when new paths are added, we can update the map. */
		@Test
		public final void testAddNewPathAndUpdateMap()
		{
			Map<CmpVertex,Collection<SmtLabelRepresentation.AbstractState>> orig = new TreeMap<CmpVertex,Collection<SmtLabelRepresentation.AbstractState>>();
			orig.putAll(graph.getVertexToAbstractState());
			
			Set<CmpVertex> origVertices = new TreeSet<CmpVertex>();origVertices.addAll(graph.transitionMatrix.keySet());
			
			// I cannot use AugmentPTA to add a new state because it will flush the cache.
			CmpVertex newState = AbstractLearnerGraph.generateNewCmpVertex(graph.nextID(false), graph.config);
			graph.transitionMatrix.put(newState, graph.createNewRow());
			graph.addTransition(graph.transitionMatrix.get(
					graph.paths.getVertex(labelList(new String[]{"add","remove"}))), 
					AbstractLearnerGraph.generateNewLabel("remove",graph.config,converter), newState);
			Assert.assertEquals(origVertices.size()+1,graph.getStateNumber());
			
			lbls.buildVertexToAbstractStateMap(graph, null,true);
			for(CmpVertex newVertex:graph.transitionMatrix.keySet())
				if (origVertices.contains(newVertex))
					Assert.assertEquals(orig.get(newVertex),graph.getVertexToAbstractState().get(newVertex));
				else
					Assert.assertEquals(1,graph.getVertexToAbstractState().get(newVertex).size());
		}
		
		/** Test the consistency checking conditions of buildVertexToAbstractStateMap.
		 * The first one is intersection. 
		 */
		@Test
		public final void testBuildVertexToAbstractStateMapConsistency_fail1()
		{
			graph.getVertexToAbstractState().get(graph.paths.getVertex(labelList(new String[]{"add","remove"}))).addAll(
					graph.getVertexToAbstractState().get(graph.getInit()));
			Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
				lbls.buildVertexToAbstractStateMap(graph, null,true);
			}},IllegalArgumentException.class,"classes with DFA state");
		}
		
		/** Test the consistency checking conditions of buildVertexToAbstractStateMap.
		 * This one is the inclusion of the domain of getVertexToAbstractState in the set of states.
		 */
		@Test
		public final void testBuildVertexToAbstractStateMapConsistency_fail2()
		{
			CmpVertex newState = AbstractLearnerGraph.generateNewCmpVertex(graph.nextID(false), graph.config);
			// now add an abstract state to it.
			Collection<AbstractState> newAbstractStates = new LinkedList<AbstractState>();newAbstractStates.add(lbls.new AbstractState(newState,99));
			graph.getVertexToAbstractState().put(newState,newAbstractStates);
			
			Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
				lbls.buildVertexToAbstractStateMap(graph, null,true);
			}},IllegalArgumentException.class,"do not feature");
		}
		
		/** Test the consistency checking conditions of buildVertexToAbstractStateMap.
		 * This one is the inclusion of the set of states in the domain of getVertexToAbstractState.
		 */
		@Test
		public final void testBuildVertexToAbstractStateMapConsistency_fail3()
		{
			CmpVertex newState = AbstractLearnerGraph.generateNewCmpVertex(graph.nextID(true), graph.config);
			newState.setAccept(true);
			// now add an unreachable accept-state to our graph.
			graph.transitionMatrix.put(newState, graph.createNewRow());
			
			Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
				lbls.buildVertexToAbstractStateMap(graph, null,true);
			}},IllegalArgumentException.class,"are not in the vertex");
		}
		
		/** Test the consistency checking conditions of buildVertexToAbstractStateMap.
		 * This one is the inclusion of the set of states in the domain of getVertexToAbstractState.
		 * A reject-vertex should be ignored.
		 */
		@Test
		public final void testBuildVertexToAbstractStateMapConsistency_4()
		{
			CmpVertex newState = AbstractLearnerGraph.generateNewCmpVertex(graph.nextID(false), graph.config);
			newState.setAccept(false);
			// now add an unreachable reject-state to our graph.
			graph.transitionMatrix.put(newState, graph.createNewRow());
			
			lbls.buildVertexToAbstractStateMap(graph, null,true);
		}
	} // end of class TestFeaturesOfAbstractStates

	/** Tests that the arguments of functions are collected. */
	@Test
	public final void testAssociationsOfArgsToValues()
	{		
		final SmtLabelRepresentation lbls = new SmtLabelRepresentation(config,converter);
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define a"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (func 0 m"+_N+"))",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" (func a"+_N+" (func 77 m"+_M+"))))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))",
				QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.ARITY+" 2",
				QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0::int)",
				QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"1::int)",
				QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"2::int)",
				QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.CONSTRAINT+" (> "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"1 0)",
				
				// There are a few data traces to be added.
				QSMTool.cmdDataTrace+" + add((= a"+_N+" 2)) remove",
				QSMTool.cmdDataTrace+" + add((= a"+_N+" (func 1 (func m"+_M+" 20)))) add((= a"+_N+" (func (func 55 m"+_M+") (+ 8 m"+_M+")))) remove",
		}));
		
		final LearnerGraph graph = new LearnerGraph(Configuration.getDefaultConfiguration());
		graph.paths.augmentPTA(lbls.getSPlus(), true, false);
		graph.paths.augmentPTA(lbls.getSMinus(), false, false);
		
		lbls.buildVertexToAbstractStateMap(graph, null,true);

		int varNumberInit =0,varNumber11=1,varNumber12=2,varNumber21=3,varNumber22=4,varNumber23=5;
	
		String 
			FUNC0_0_POST_0=toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 0, 0),varNumberInit,varNumberInit),// func inside Init
			fArg0_0 = toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 0, JUConstants.intUNKNOWN),varNumberInit,varNumberInit),
			// trace 1
			FUNC1_1_POST_0=toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 0, 0),varNumber11,varNumberInit),
			fArg11_POST_0 = toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 0, JUConstants.intUNKNOWN),varNumber11,varNumberInit),
			FUNC1_1_POST_1=toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 1, 0),varNumber11,varNumberInit),
			fArg11_POST_1 = toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 1, JUConstants.intUNKNOWN),varNumber11,varNumberInit),
			// trace 2
			FUNC2_1_IO_0=toCurrentMem(generateFreshVariable("func", VARIABLEUSE.IO, 0, 0),varNumber21,varNumberInit),
			FUNC2_1_IO_1=toCurrentMem(generateFreshVariable("func", VARIABLEUSE.IO, 1, 0),varNumber21,varNumberInit),
			fArg21_IO_0 = toCurrentMem(generateFreshVariable("func", VARIABLEUSE.IO, 0, JUConstants.intUNKNOWN),varNumber21,varNumberInit),
			FUNC2_1_POST_0=toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 0, 0),varNumber21,varNumberInit),
			FUNC2_1_POST_1=toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 1, 0),varNumber21,varNumberInit),
			fArg21_IO_1 = toCurrentMem(generateFreshVariable("func", VARIABLEUSE.IO, 1, JUConstants.intUNKNOWN),varNumber21,varNumberInit),
			fArg21_POST_0 = toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 0, JUConstants.intUNKNOWN),varNumber21,varNumberInit),
			fArg21_POST_1 = toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 1, JUConstants.intUNKNOWN),varNumber21,varNumberInit),
			FUNC2_2_IO_0=toCurrentMem(generateFreshVariable("func", VARIABLEUSE.IO, 0, 0),varNumber22,varNumber21),
			FUNC2_2_IO_1=toCurrentMem(generateFreshVariable("func", VARIABLEUSE.IO, 1, 0),varNumber22,varNumber21),
			FUNC2_2_POST_0=toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 0, 0),varNumber22,varNumber21),
			FUNC2_2_POST_1=toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 1, 0),varNumber22,varNumber21),
			fArg22_IO_0 = toCurrentMem(generateFreshVariable("func", VARIABLEUSE.IO, 0, JUConstants.intUNKNOWN),varNumber22,varNumber21),
			fArg22_IO_1 = toCurrentMem(generateFreshVariable("func", VARIABLEUSE.IO, 1, JUConstants.intUNKNOWN),varNumber22,varNumber21),
			fArg22_POST_0 = toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 0, JUConstants.intUNKNOWN),varNumber22,varNumber21),
			fArg22_POST_1 = toCurrentMem(generateFreshVariable("func", VARIABLEUSE.POST, 1, JUConstants.intUNKNOWN),varNumber22,varNumber21);
		
		StringBuffer expectedDecls = new StringBuffer();
		for(int arg=0;arg<=2;++arg)
			expectedDecls.append("(define "+generateFreshVariable("func", VARIABLEUSE.POST, 0, arg)+"::int)"+ENDL);
		for(int arg=0;arg<=2;++arg)
			expectedDecls.append("(define "+generateFreshVariable("func", VARIABLEUSE.POST, 1, arg)+"::int)"+ENDL);
		
		final String expectedCompDeclarations = SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
				expectedDecls.toString(),SmtLabelRepresentation.blockVARDECLS);

		Assert.assertEquals("",lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("add",lbls.config,lbls.converter)).pre.varDeclarations);
		Assert.assertEquals(expectedCompDeclarations,lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("add",lbls.config,lbls.converter)).post.varDeclarations);
		Assert.assertEquals("",lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("remove",lbls.config,lbls.converter)).pre.varDeclarations);
		Assert.assertEquals("",lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("remove",lbls.config,lbls.converter)).post.varDeclarations);
		
		StringBuffer expectedTrace = new StringBuffer();
		String init = "(define m"+_N+"::nat) (define a"+_N+"::nat)";
		String tmpPost = "";for(int use=0;use<2;++use) for(int arg=0;arg<=2;++arg) tmpPost=tmpPost+"(define "+generateFreshVariable("func", VARIABLEUSE.POST, use, arg)+"::int)"+ENDL;
		String FUNCPOST = SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(tmpPost,SmtLabelRepresentation.blockVARDECLS)+ENDL;
		tmpPost = "";for(int use=0;use<2;++use) for(int arg=0;arg<=2;++arg) tmpPost=tmpPost+"(define "+generateFreshVariable("func", VARIABLEUSE.IO, use, arg)+"::int)"+ENDL;
		String FUNCIO = SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(tmpPost,SmtLabelRepresentation.blockVARDECLS)+ENDL;
		
		expectedTrace.append(toCurrentMem(init, 0, 0));expectedTrace.append(ENDL);
		expectedTrace.append(toCurrentMem(init, 1, 1));
		expectedTrace.append(toCurrentMem(FUNCPOST, 1, 0));
		expectedTrace.append(toCurrentMem(init, 2, 2));expectedTrace.append(ENDL);
		
		expectedTrace.append(toCurrentMem(FUNCIO, JUConstants.intUNKNOWN, 0));
		expectedTrace.append(toCurrentMem(init, 3, 3));
		expectedTrace.append(toCurrentMem(FUNCPOST, 3, 0));
		expectedTrace.append(toCurrentMem(FUNCIO, JUConstants.intUNKNOWN, 3));
		expectedTrace.append(toCurrentMem(init, 4, 4));
		expectedTrace.append(toCurrentMem(FUNCPOST, 4, 3));
		expectedTrace.append(toCurrentMem(init, 5, 5));expectedTrace.append(ENDL);
		
		StringBuffer traceAxioms = new StringBuffer();
		traceAxioms.append(SmtLabelRepresentation.commentForInit);traceAxioms.append(ENDL);
		// 0 0
		traceAxioms.append("(= m"+__P+varNumberInit+" "+FUNC0_0_POST_0+")");
		traceAxioms.append(SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
			"(= "+FUNC0_0_POST_0+" (func "+fArg0_0+"1 "+fArg0_0+"2))(= "+fArg0_0+"1 0)(= "+fArg0_0+"2 m"+__P+varNumberInit+")"+ENDL+
			"(> "+fArg0_0+"1 0)"+ENDL
			,
			SmtLabelRepresentation.blockVARS));
		// 1 1
		traceAxioms.append("(= a"+__P+(varNumber11)+" 2)");traceAxioms.append(ENDL);traceAxioms.append("true");traceAxioms.append(ENDL);
		traceAxioms.append("(= m"+__P+(varNumber11)+" (+ m"+__P+(varNumberInit)+" "+FUNC1_1_POST_1+"))");
		traceAxioms.append(SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
			"(= "+FUNC1_1_POST_0+" (func "+fArg11_POST_0+"1 "+fArg11_POST_0+"2))(= "+fArg11_POST_0+"1 77)(= "+fArg11_POST_0+"2 m"+__P+(varNumberInit)+")"+ENDL+
			"(> "+fArg11_POST_0+"1 0)"+ENDL+
			"(= "+FUNC1_1_POST_1+" (func "+fArg11_POST_1+"1 "+fArg11_POST_1+"2))(= "+fArg11_POST_1+"1 a"+__P+(varNumber11)+")(= "+fArg11_POST_1+"2 "+FUNC1_1_POST_0+")"+ENDL+
			"(> "+fArg11_POST_1+"1 0)"+ENDL
			,
			SmtLabelRepresentation.blockVARS));
		// 1 2
		traceAxioms.append("(> m"+__P+(varNumber11)+" 0)");traceAxioms.append(ENDL);
		traceAxioms.append("(= m"+__P+(varNumber12)+" (- m"+__P+(varNumber11)+" 1))");
		
		// 2 1
		traceAxioms.append("(= a"+__P+(varNumber21)+" "+FUNC2_1_IO_1+")");
		traceAxioms.append(SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
			"(= "+FUNC2_1_IO_0+" (func "+fArg21_IO_0+"1 "+fArg21_IO_0+"2))(= "+fArg21_IO_0+"1 m"+__P+(varNumberInit)+")(= "+fArg21_IO_0+"2 20)"+ENDL+
			"(> "+fArg21_IO_0+"1 0)"+ENDL+
			"(= "+FUNC2_1_IO_1+" (func "+fArg21_IO_1+"1 "+fArg21_IO_1+"2))(= "+fArg21_IO_1+"1 1)(= "+fArg21_IO_1+"2 "+FUNC2_1_IO_0+")"+ENDL+
			"(> "+fArg21_IO_1+"1 0)"+ENDL
			,
			SmtLabelRepresentation.blockVARS));traceAxioms.append(ENDL);
		traceAxioms.append("true");traceAxioms.append(ENDL);
		traceAxioms.append("(= m"+__P+(varNumber21)+" (+ m"+__P+(varNumberInit)+" "+FUNC2_1_POST_1+"))");
		traceAxioms.append(SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
			"(= "+FUNC2_1_POST_0+" (func "+fArg21_POST_0+"1 "+fArg21_POST_0+"2))(= "+fArg21_POST_0+"1 77)(= "+fArg21_POST_0+"2 m"+__P+(varNumberInit)+")"+ENDL+
			"(> "+fArg21_POST_0+"1 0)"+ENDL+
			"(= "+FUNC2_1_POST_1+" (func "+fArg21_POST_1+"1 "+fArg21_POST_1+"2))(= "+fArg21_POST_1+"1 a"+__P+(varNumber21)+")(= "+fArg21_POST_1+"2 "+FUNC2_1_POST_0+")"+ENDL+
			"(> "+fArg21_POST_1+"1 0)"+ENDL
			,
			SmtLabelRepresentation.blockVARS));
		// 2 2
		traceAxioms.append("(= a"+__P+(varNumber22)+" "+FUNC2_2_IO_1+")");
		traceAxioms.append(SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
			"(= "+FUNC2_2_IO_0+" (func "+fArg22_IO_0+"1 "+fArg22_IO_0+"2))(= "+fArg22_IO_0+"1 55)(= "+fArg22_IO_0+"2 m"+__P+(varNumber21)+")"+ENDL+
			"(> "+fArg22_IO_0+"1 0)"+ENDL+
			"(= "+FUNC2_2_IO_1+" (func "+fArg22_IO_1+"1 "+fArg22_IO_1+"2))(= "+fArg22_IO_1+"1 "+FUNC2_2_IO_0+")(= "+fArg22_IO_1+"2 (+ 8 m"+__P+(varNumber21)+"))"+ENDL+
			"(> "+fArg22_IO_1+"1 0)"+ENDL
			,
			SmtLabelRepresentation.blockVARS));traceAxioms.append(ENDL);traceAxioms.append("true");traceAxioms.append(ENDL);
		traceAxioms.append("(= m"+__P+(varNumber22)+" (+ m"+__P+(varNumber21)+" "+FUNC2_2_POST_1+"))");
		traceAxioms.append(SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
			"(= "+FUNC2_2_POST_0+" (func "+fArg22_POST_0+"1 "+fArg22_POST_0+"2))(= "+fArg22_POST_0+"1 77)(= "+fArg22_POST_0+"2 m"+__P+(varNumber21)+")"+ENDL+
			"(> "+fArg22_POST_0+"1 0)"+ENDL+
			"(= "+FUNC2_2_POST_1+" (func "+fArg22_POST_1+"1 "+fArg22_POST_1+"2))(= "+fArg22_POST_1+"1 a"+__P+(varNumber22)+")(= "+fArg22_POST_1+"2 "+FUNC2_2_POST_0+")"+ENDL+
			"(> "+fArg22_POST_1+"1 0)"+ENDL
			,
			SmtLabelRepresentation.blockVARS));
		
		// 2 3
		traceAxioms.append("(> m"+__P+(varNumber22)+" 0)");traceAxioms.append(ENDL);
		traceAxioms.append("(= m"+__P+(varNumber23)+" (- m"+__P+(varNumber22)+" 1))");

		Assert.assertEquals(expectedTrace.toString(),lbls.tracesVars.toString());
		Assert.assertEquals(SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(expectedTrace.toString()+ENDL+SmtLabelRepresentation.assertString+"(and "+traceAxioms.toString()+"))",SmtLabelRepresentation.blockDATATRACES),lbls.knownTraces);
		
		StringBuffer finalTextAdd = new StringBuffer();
		
		String 
			FUNC_ADD_0 = generateFreshVariable("func", VARIABLEUSE.POST, 0, 0),
			FUNC_ADD_1 = generateFreshVariable("func", VARIABLEUSE.POST, 1, 0),
			fArg_ADD_0 = generateFreshVariable("func", VARIABLEUSE.POST, 0, JUConstants.intUNKNOWN),
			fArg_ADD_1 = generateFreshVariable("func", VARIABLEUSE.POST, 1, JUConstants.intUNKNOWN)
			;
		finalTextAdd.append("(= m"+_N+" (+ m"+_M+" "+FUNC_ADD_1+"))");
		finalTextAdd.append(SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
			"(= "+FUNC_ADD_0+" (func "+fArg_ADD_0+"1 "+fArg_ADD_0+"2))(= "+fArg_ADD_0+"1 77)(= "+fArg_ADD_0+"2 m"+_M+")"+ENDL+
			"(> "+fArg_ADD_0+"1 0)"+ENDL+
			"(= "+FUNC_ADD_1+" (func "+fArg_ADD_1+"1 "+fArg_ADD_1+"2))(= "+fArg_ADD_1+"1 a"+_N+")(= "+fArg_ADD_1+"2 "+FUNC_ADD_0+")"+ENDL+
			"(> "+fArg_ADD_1+"1 0)"+ENDL
			,
			SmtLabelRepresentation.blockVARS));
		StringBuffer values = new StringBuffer();
		values.append(";; "+FUNC_ADD_0+ENDL);
		values.append("(or ");
		for(String varName:new String[]{fArg0_0,fArg11_POST_0,fArg11_POST_1,fArg21_IO_0,fArg21_IO_1,fArg21_POST_0,fArg21_POST_1,fArg22_IO_0,fArg22_IO_1,fArg22_POST_0,fArg22_POST_1})
			values.append("(and (= "+fArg_ADD_0+"1 "+varName+"1)(= "+fArg_ADD_0+"2 "+varName+"2))"+ENDL);
		values.append(")"+ENDL);
		values.append(";; "+FUNC_ADD_1+ENDL);
		values.append("(or ");
		for(String varName:new String[]{fArg0_0,fArg11_POST_0,fArg11_POST_1,fArg21_IO_0,fArg21_IO_1,fArg21_POST_0,fArg21_POST_1,fArg22_IO_0,fArg22_IO_1,fArg22_POST_0,fArg22_POST_1})
			values.append("(and (= "+fArg_ADD_1+"1 "+varName+"1)(= "+fArg_ADD_1+"2 "+varName+"2))"+ENDL);
		values.append(")"+ENDL);
		finalTextAdd.append(SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
				values.toString()
				,
				SmtLabelRepresentation.blockVALUES));

		Assert.assertEquals("",lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("add",lbls.config,lbls.converter)).pre.finalText);
		Assert.assertEquals("(> m"+_M+" 0)",lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("remove",lbls.config,lbls.converter)).pre.finalText);
		Assert.assertEquals("(= m"+_N+" (- m"+_M+" 1))",lbls.labelMapFinal.get(AbstractLearnerGraph.generateNewLabel("remove",lbls.config,lbls.converter)).post.finalText);		
	}
}
