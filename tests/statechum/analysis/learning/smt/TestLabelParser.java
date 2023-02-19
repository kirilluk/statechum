/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
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
package statechum.analysis.learning.smt;

import static statechum.analysis.learning.smt.SmtLabelRepresentation.INITMEM;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.*;
import statechum.TestHelper.whatToRun;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.smt.SmtLabelRepresentation.CompositionOfFunctions;
import statechum.analysis.learning.smt.SmtLabelRepresentation.FUNC_DATA;
import statechum.analysis.learning.smt.SmtLabelRepresentation.OP_DATA;
import statechum.analysis.learning.smt.SmtLabelRepresentation.TraceWithData;
import statechum.analysis.learning.smt.SmtLabelRepresentation.VARIABLEUSE;
import statechum.apps.QSMTool;

public class TestLabelParser {
	private static final String _N = SmtLabelRepresentation.varNewSuffix;
	private static final String _M = SmtLabelRepresentation.varOldSuffix;

	SmtLabelRepresentation lbls;
	
	protected final Configuration config = Configuration.getDefaultConfiguration(); 
	protected final ConvertALabel converter = null;
	
	/** Converts arrays of labels to lists of labels using config - it does not really matter which configuration is used 
	 * because all of them start from a default one and do not modify label type.
	 * 
	 * @param labels what to convert
	 * @return the outcome of conversion.
	 */
	protected Label[] asArray(String [] labels)
	{// this is only used for testing hence efficiency is not significant.
		return AbstractLearnerGraph.buildList(Arrays.asList(labels),lbls.config,converter).toArray(new Label[0]);
	}
	
	@Before
	public final void beforeTest()
	{
		lbls = new SmtLabelRepresentation(config,converter);
	
	}
	
	void parseDataTrace(String whatToParse)
	{
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))",
				QSMTool.cmdDataTrace+" "+whatToParse
		}));
	
	}
	
	/** Checks that the <em>lowLevelFunctions</em> variable is set correctly. */
	@Test
	public final void testDetectWhenToUseLowLevelFunctions()
	{
		List<String> decl = Arrays.asList(new String[] {
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))",
				QSMTool.cmdDataTrace+" + callA((= 0 (fn input"+_M+" 7)) (< output"+_N+" 8)) callB callC((and (fn 5 input"+_M+") (> input"+_M+" output"+_N+")))"
			});

		{
			final SmtLabelRepresentation l = new SmtLabelRepresentation(config,converter);
			List<String> data = new LinkedList<String>();data.addAll(decl);data.addAll(Arrays.asList(new String[] {
					QSMTool.cmdLowLevelFunction+" fn ARITY 2",
					QSMTool.cmdLowLevelFunction+" fn CONSTRAINT (< 6 "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0)",
					QSMTool.cmdLowLevelFunction+" fn DECL (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"2::int)",
					QSMTool.cmdLowLevelFunction+" fn DECL (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"1::int)",
					QSMTool.cmdLowLevelFunction+" fn DECL (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0::int)",
					QSMTool.cmdLowLevelFunction+" fn CONSTRAINARGS true",
			}));
			l.parseCollection(data);
			Assert.assertTrue(l.usingLowLevelFunctions);
		}

		{
			final SmtLabelRepresentation l = new SmtLabelRepresentation(config,converter);
			List<String> data = new LinkedList<String>();data.addAll(decl);data.addAll(Arrays.asList(new String[] {
					QSMTool.cmdLowLevelFunction+" fn ARITY 0",
					QSMTool.cmdLowLevelFunction+" fn CONSTRAINT (< 6 "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0)",
					QSMTool.cmdLowLevelFunction+" fn DECL (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0::int)",
					QSMTool.cmdLowLevelFunction+" fn CONSTRAINARGS true",
			}));
			Assert.assertFalse(l.usingLowLevelFunctions);
		}

		{
			final SmtLabelRepresentation l = new SmtLabelRepresentation(config,converter);
			l.parseCollection(decl);
			Assert.assertFalse(l.usingLowLevelFunctions);
		}

		{
			final SmtLabelRepresentation l = new SmtLabelRepresentation(config,converter);
			List<String> data = new LinkedList<String>();data.addAll(decl);data.addAll(Arrays.asList(new String[] {
					QSMTool.cmdLowLevelFunction+" fn ARITY 2",
					QSMTool.cmdLowLevelFunction+" fn CONSTRAINT (< 6 "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0)",
					QSMTool.cmdLowLevelFunction+" fn DECL (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"2::int)",
					QSMTool.cmdLowLevelFunction+" fn DECL (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"1::int)",
					QSMTool.cmdLowLevelFunction+" fn DECL (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0::int)",
					QSMTool.cmdLowLevelFunction+" fn CONSTRAINARGS false",
			}));
			l.parseCollection(data);
			Assert.assertFalse(l.usingLowLevelFunctions);
		}
	}

	/** Missing pre/post etc. The line ends with a space. */
	@Test
	public final void testFunctionParser_fail0a()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdLowLevelFunction+" = "
			}));
		}},IllegalArgumentException.class,"expected details");
	}
	
	/** Missing pre/post etc, this time the line ends right after function name. */
	@Test
	public final void testFunctionParser_fail0b()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdLowLevelFunction+" ="
			}));
		}},IllegalArgumentException.class,"expected details");
	}
	
	/** Invalid pre/post. */
	@Test
	public final void testFunctionParser_fail0c()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdLowLevelFunction+" = AA"
			}));
		}},IllegalArgumentException.class,"but got: AA");
	}
	
	/** Missing arity argument. */
	@Test
	public final void testFunctionParser_fail0d()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdLowLevelFunction+" = ARITY",
					QSMTool.cmdLowLevelFunction+" = CONSTRAINARGS true",
			}));
		}},IllegalArgumentException.class,"expected specification");
	}
	
	/** Invalid arity. */
	@Test
	public final void testFunctionParser_fail1()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdLowLevelFunction+" = ARITY aa",
					QSMTool.cmdLowLevelFunction+" = CONSTRAINARGS true",
			}));
		}},IllegalArgumentException.class,"which is not a number");
	}
	
	/** Negative arity. */
	@Test
	public final void testFunctionParser_fail2()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdLowLevelFunction+" = ARITY -8",
					QSMTool.cmdLowLevelFunction+" = CONSTRAINARGS true",
			}));
		}},IllegalArgumentException.class,"an invalid number");
	}
	
	/** Missing types of arguments. */
	@Test
	public final void testFunctionParser_fail3()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
					QSMTool.cmdLowLevelFunction+" = ARITY 2",
					QSMTool.cmdLowLevelFunction+" = CONSTRAINARGS true",
			}));
		}},IllegalArgumentException.class,"types of return value and arguments");
	}
	
	/** Arity is already known. */
	@Test
	public final void testFunctionParser_fail4()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
					QSMTool.cmdLowLevelFunction+" = ARITY 2",
					QSMTool.cmdLowLevelFunction+" = ARITY 2",
					QSMTool.cmdLowLevelFunction+" = CONSTRAINARGS true",
			}));
		}},IllegalArgumentException.class,"the arity of = is already known");
	}
		
	/** Missing declaration of a return type of a function. */
	@Test
	public final void testFunctionParser_fail5()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
					QSMTool.cmdLowLevelFunction+" = ARITY 2",
			}));
		}},IllegalArgumentException.class,"types of return value and arguments is missing");
	}
		
	/** Missing declaration of a return type of a function. */
	@Test
	public final void testFunctionParser_fail6()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
					QSMTool.cmdLowLevelFunction+" = ARITY 1",
					QSMTool.cmdLowLevelFunction+" = DECL (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"1::int)",
			}));
		}},IllegalArgumentException.class,"missing a declaration for the return value");
	}
		
	/** Missing declaration of one of the arguments of a function. */
	@Test
	public final void testFunctionParser_fail7()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			lbls.parseCollection(Arrays.asList(new String[]{
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
					QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
					QSMTool.cmdLowLevelFunction+" = ARITY 2",
					QSMTool.cmdLowLevelFunction+" = DECL (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0::int)",
					QSMTool.cmdLowLevelFunction+" = DECL (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"1::int)"
			}));
		}},IllegalArgumentException.class,"missing a declaration for argument 2");
	}
		
	@Test
	public final void testTraceParser1()
	{
		parseDataTrace("");
		Assert.assertTrue(lbls.traces.isEmpty());
		Assert.assertTrue(lbls.getSPlus().isEmpty());
		Assert.assertTrue(lbls.getSMinus().isEmpty());
	}
	
	/** Sets of both positive and negative sequences.*/
	@Test
	public final void testTraceParser2a()
	{
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))",
				QSMTool.cmdDataTrace+" + callA () callB",
				QSMTool.cmdDataTrace+" - callA () callC",
				QSMTool.cmdDataTrace+" + callA () callB callD"}));
		Assert.assertEquals(3,lbls.traces.size());
		Set<List<Label>> positives = new LinkedHashSet<List<Label>>();positives.addAll(lbls.getSPlus());
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"callA","callB"},new String[]{"callA","callB", "callD"}},config,converter),positives);
		Set<List<Label>> negatives = new LinkedHashSet<List<Label>>();negatives.addAll(lbls.getSMinus());
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"callA","callC"}},config,converter),negatives);
	}
	
	/** Empty set of negatives. */
	@Test
	public final void testTraceParser2b()
	{
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))",
				QSMTool.cmdDataTrace+" + callA () callB",
				QSMTool.cmdDataTrace+" + callT () callC",
				QSMTool.cmdDataTrace+" + callA () callB callD"}));
		Assert.assertEquals(3,lbls.traces.size());
		Set<List<Label>> positives = new LinkedHashSet<List<Label>>();positives.addAll(lbls.getSPlus());
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"callA","callB"},new String[]{"callA","callB", "callD"},new String[]{"callT","callC"}},config,converter),positives);
		Set<List<Label>> negatives = new LinkedHashSet<List<Label>>();negatives.addAll(lbls.getSMinus());
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{},config,converter),negatives);
	}
	
	@Test
	public final void testTraceParser_invalidtype()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			parseDataTrace("callA () callB");
		}},IllegalArgumentException.class,"invalid data trace type");
	}
	
	void checkParsingSimpleTrace(String whatToParse)
	{
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))",
				QSMTool.cmdDataTrace+" "+whatToParse
		}));
	
		Assert.assertEquals(1,lbls.traces.size());
		Set<List<Label>> positives = new LinkedHashSet<List<Label>>();positives.addAll(lbls.getSPlus());
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"callA","callB"}},config,converter),positives);
		Set<List<Label>> negatives = new LinkedHashSet<List<Label>>();negatives.addAll(lbls.getSMinus());
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{},config,converter),negatives);
		
		TraceWithData trace = lbls.traces.iterator().next();
		for(CompositionOfFunctions func:trace.arguments)
			Assert.assertEquals("",func.text);
	}

	@Test
	public final void testTraceParser3()
	{
		checkParsingSimpleTrace("+ callA() callB");
	}
	
	@Test
	public final void testTraceParser4()
	{
		checkParsingSimpleTrace("+ callA callB");
	}
	
	/** Brackets without a function name. */
	@Test
	public final void testTraceParser_fail1()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			parseDataTrace("+ callA ()()callB");
		}},IllegalArgumentException.class,"multiple groups of arguments for label");
	}
	
	/** Brackets without a function name. */	
	@Test
	public final void testTraceParser_fail2()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			parseDataTrace("+ () callA");
		}},IllegalArgumentException.class,"arguments without a label");
	}
	
	@Test
	public final void testTraceParser5a()
	{
		parseDataTrace("+ callA((= input 7) (< output 8)) callB callC((and (= 5 input) (> input output)))");
		Assert.assertEquals(1,lbls.traces.size());
		TraceWithData trace = lbls.traces.iterator().next();
		Assert.assertArrayEquals(asArray(new String[]{"callA","callB","callC"}), trace.traceDetails.toArray());
		Iterator<CompositionOfFunctions> compIterator = trace.arguments.iterator();
		Assert.assertEquals("(= input 7) (< output 8)",compIterator.next().text);
		Assert.assertEquals("",compIterator.next().text);
		Assert.assertEquals("(and (= 5 input) (> input output))",compIterator.next().text);
		Assert.assertFalse(compIterator.hasNext());
	}

	/** Checks the outcome of parsing functions in detail. */
	@Test
	public final void testTraceParser5c()
	{
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))",
				QSMTool.cmdDataTrace+" + callA((= 0 (fn input"+_M+" 7)) (< output"+_N+" 8)) callB callC((and (fn 5 input"+_M+") (> input"+_M+" output"+_N+")))",
				QSMTool.cmdLowLevelFunction+" fn ARITY 2",
				QSMTool.cmdLowLevelFunction+" fn CONSTRAINT (< 6 "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0)",
				QSMTool.cmdLowLevelFunction+" fn DECL (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0::int)",
				QSMTool.cmdLowLevelFunction+" fn DECL (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"1::int)",
				QSMTool.cmdLowLevelFunction+" fn DECL (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"2::int)",
				QSMTool.cmdLowLevelFunction+" fn CONSTRAINARGS true",
		}));
		Assert.assertEquals(1,lbls.traces.size());
		TraceWithData trace = lbls.traces.iterator().next();
		Assert.assertArrayEquals(asArray(new String[]{"callA","callB","callC"}), trace.traceDetails.toArray());
		Iterator<CompositionOfFunctions> compIterator = trace.arguments.iterator();
		final String expectedCompDeclarations = SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
				"(define "+SmtLabelRepresentation.generateFreshVariable("fn", VARIABLEUSE.IO, 0, 0)+"::int)"+SmtLabelRepresentation.ENDL+
				"(define "+SmtLabelRepresentation.generateFreshVariable("fn", VARIABLEUSE.IO, 0, 1)+"::int)"+SmtLabelRepresentation.ENDL+
				"(define "+SmtLabelRepresentation.generateFreshVariable("fn", VARIABLEUSE.IO, 0, 2)+"::int)"+SmtLabelRepresentation.ENDL,SmtLabelRepresentation.blockVARDECLS);

		CompositionOfFunctions comp = compIterator.next();
		Assert.assertEquals("(= 0 "+SmtLabelRepresentation.generateFreshVariable("fn", VARIABLEUSE.IO, 0, 0)+") (< output"+_N+" 8)",comp.text);
		Assert.assertEquals(expectedCompDeclarations,comp.varDeclarations);
		Assert.assertEquals(1,comp.variablesUsedForArgs.size());
		Assert.assertArrayEquals(new String[]{SmtLabelRepresentation.generateFreshVariable("fn", VARIABLEUSE.IO, 0, JUConstants.intUNKNOWN)}, comp.variablesUsedForArgs.get(lbls.functionMap.get("fn")).toArray());

		comp = compIterator.next();
		Assert.assertEquals("",comp.text);
		Assert.assertEquals("",comp.varDeclarations);
		Assert.assertEquals(0,comp.variablesUsedForArgs.size());

		comp = compIterator.next();
		Assert.assertEquals("(and "+SmtLabelRepresentation.generateFreshVariable("fn", VARIABLEUSE.IO, 0, 0)+" (> input"+_M+" output"+_N+"))",comp.text);
		Assert.assertEquals(expectedCompDeclarations,comp.varDeclarations);
		Assert.assertEquals(1,comp.variablesUsedForArgs.size());
		Assert.assertArrayEquals(new String[]{SmtLabelRepresentation.generateFreshVariable("fn", VARIABLEUSE.IO, 0, JUConstants.intUNKNOWN)}, comp.variablesUsedForArgs.get(lbls.functionMap.get("fn")).toArray());
		Assert.assertFalse(compIterator.hasNext());
	}
	
	
	/** Composite expression as an argument to a call. */
	@Test
	public final void testTraceParser6a()
	{
		parseDataTrace("+ callA((= input 7) (< output 8)) callB callC((and (= 5 input) (> (func output input) output)))");
		Assert.assertEquals(1,lbls.traces.size());
		TraceWithData trace = lbls.traces.iterator().next();
		Assert.assertArrayEquals(asArray(new String[]{"callA","callB","callC"}), trace.traceDetails.toArray());
		Iterator<CompositionOfFunctions> compIterator = trace.arguments.iterator();
		Assert.assertEquals("(= input 7) (< output 8)",compIterator.next().text);
		Assert.assertEquals("",compIterator.next().text);
		Assert.assertEquals("(and (= 5 input) (> (func output input) output))",compIterator.next().text);
		Assert.assertFalse(compIterator.hasNext());
	}
	
	/** Composite expression as an argument to a call, this time we process arguments to this function. */
	@Test
	public final void testTraceParser6b()
	{
		parseDataTrace("+ callA((= input 7) (< output 8)) callB callC((and (= 5 input) (> (func output input) output)))");
		Assert.assertEquals(1,lbls.traces.size());
		TraceWithData trace = lbls.traces.iterator().next();
		Assert.assertArrayEquals(asArray(new String[]{"callA","callB","callC"}), trace.traceDetails.toArray());
		Iterator<CompositionOfFunctions> compIterator = trace.arguments.iterator();
		Assert.assertEquals("(= input 7) (< output 8)",compIterator.next().text);
		Assert.assertEquals("",compIterator.next().text);
		Assert.assertEquals("(and (= 5 input) (> (func output input) output))",compIterator.next().text);
		Assert.assertFalse(compIterator.hasNext());
	}
	
	/** Here a call argument is not enclosed in braces, for us this is fine, for Yices it will not be. */
	@Test
	public final void testTraceParser7()
	{
		parseDataTrace("+ callA(and (= 5 input) (> input output))");
	}
	
	/** Spaces before function name. */
	@Test
	public final void testTraceParser8()
	{
		parseDataTrace("+ callA(( = input 7) (< output 8)) callB callC((and (= 5 input) (> ( func output input) output)))");
	}
	
	/** Labels may contain reserved words . */
	@Test
	public final void testTraceParser9()
	{
		parseDataTrace("+ "+SmtLabelRepresentation.functionArg+"_smth(( = input 7) (< output 8)) callB callC((and (= 5 input) (> ( func output input) output)))");
	}
	
	/** Missing closing brace in the first operation which picks the second expression and fails because of "((".*/
	@Test
	public final void testTraceParser_fail_missing_closing_in_first_1()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			parseDataTrace("+ callA((= input  (< output 8)) callB callC( (and (= 5 input) (> (func output input) output)))");
		}},IllegalArgumentException.class,"missing name of a function");
	}
	
	/** Missing closing brace in the first operation which picks the second expression and fails because
	 * there is not enough closing brackets at the end.
	 */
	@Test
	public final void testTraceParser_fail_missing_closing_in_first_2()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			parseDataTrace("+ callA((= input  (< output 8)) callB callC(n (and (= 5 input) (> (func output input) output)))");
		}},IllegalArgumentException.class,"unexpected end of input");
	}
	
	/** Too many closing braces in the first operation.*/
	@Test
	public final void testTraceParser_fail3()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			parseDataTrace("+ callA((= input  (< output 8)))) callB callC((and (= 5 input) (> (func output input) output)))");
		}},IllegalArgumentException.class,"unexpected closing brace");
	}
	
	/** Missing closing brace in the second expression picks the third expression and chokes due to ((. */
	@Test
	public final void testTraceParser_fail4()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			parseDataTrace("+ callA((= input 7) (< output 8)) callB( callC((and (= 5 input) (> (func output input) output))))");
		}},IllegalArgumentException.class,"missing name of a function");
	}
	
	/** Missing closing brace in the second expression picks the third expression but fails because the 
	 * third expression contains as many braces as needed for it but there is nothing to match the second expression. */
	@Test
	public final void testTraceParser_fail5()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			parseDataTrace("+ callA((= input 7) (< output 8)) callB( callC(n (and (= 5 input) (> (func output input) output)))");
		}},IllegalArgumentException.class,"unexpected end of input");
	}
	
	/** Parses a chunk of text
	 * 
	 * @param whatToParse precondition of an operation to parse
	 * @param argsToFunc whether to add a function with two arguments.
	 */
	void checkParsingPre(String whatToParse,boolean argsToFunc)
	{
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))",
				(!argsToFunc?QSMTool.cmdComment+" ":QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.ARITY+" 2"),
				(!argsToFunc?QSMTool.cmdComment+" ":QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0::int)"),
				(!argsToFunc?QSMTool.cmdComment+" ":QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"1::int)"),
				(!argsToFunc?QSMTool.cmdComment+" ":QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"2::int)"),
				(!argsToFunc?QSMTool.cmdComment+" ":QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.CONSTRAINT+" (> 55 "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0)"),
				(!argsToFunc?QSMTool.cmdComment+" ":QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.CONSTRAINT+" (> "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"1 "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"2)"),
				QSMTool.cmdOperation+" "+"func "+OP_DATA.PRE+" "+whatToParse
		}));
	}
	
	/** Missing closing brace. */
	@Test
	public final void testTraceParser_fail6a()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			checkParsingPre("(and (= 5 input"+_M+") (> (func output"+_N+" input"+_M+") output"+_N+")",true);
		}},IllegalArgumentException.class,"unexpected end of input");
	}

	/** Missing closing brace. */
	@Test
	public final void testTraceParser_fail6b()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			checkParsingPre("(and (= 5 input"+_M+") (> (func output"+_N+" input"+_M+") output"+_N+")",false);
		}},IllegalArgumentException.class,"unexpected end of input");
	}
		
	/** An empty chunk of text to parse (PRE/POST/IO). */
	@Test
	public final void testPreParser_fail0a1()
	{
		checkParsingPre("(and)",false);
		final SmtLabelParser parser = new SmtLabelParser();
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			parser.interpretPrePostCondition("", lbls.new FunctionVariablesHandler(VARIABLEUSE.PRE));
		}},IllegalArgumentException.class,"unexpected end of expression");
	}

	/** An empty chunk of text to parse (LABEL). */
	@Test
	public final void testLabelParser_fail0a2()
	{
		checkParsingPre("(and)",false);
		final SmtLabelParser parser = new SmtLabelParser();
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			parser.interpretTrace("", lbls.new FunctionVariablesHandler(VARIABLEUSE.PRE));
		}},IllegalArgumentException.class,"unexpected end of expression");
	}

	/** An empty chunk of text to parse - lexical analysis fails. */
	@Test
	public final void testPreParser_fail0b()
	{
		checkParsingPre("(and)",false);
		final SmtLabelParser parser = new SmtLabelParser();
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			parser.interpretPrePostCondition("   ", lbls.new FunctionVariablesHandler(VARIABLEUSE.PRE));
		}},IllegalArgumentException.class,"failed to lex");
	}

	/** An empty chunk of text to parse. */
	@Test
	public final void testPreParser_fail0c()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			checkParsingPre("()",false);
		}},IllegalArgumentException.class,"missing function name");
	}

	/** Operation used before being defined. */
	@Test
	public final void testPreParser_fail1()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			checkParsingPre("",false);
		}},IllegalArgumentException.class,"expected specification for label func");
	}
	
	/** Missing chunk of expression. */
	@Test
	public final void testPreParser_fail2()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			checkParsingPre("(",false);
		}},IllegalArgumentException.class,"unexpected end of expression");
	}
	
	/** Does not start with an opening brace - this is ok since we could have a boolean var here. */
	@Test
	public final void testPreParser3()
	{
		checkParsingPre("a",false);
	}
	
	/** Missing chunk of expression. */
	@Test
	public final void testPreParser_fail4()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			checkParsingPre("(and (",false);
		}},IllegalArgumentException.class,"unexpected end of expression");
	}
	
	/** Missing chunk of expression. */
	@Test
	public final void testPreParser_fail5()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			checkParsingPre("(and (or",false);
		}},IllegalArgumentException.class,"unexpected end of input");
	}
	

	/** Various stuff at the end of expression - interpreted as variables since they are not braces. */
	@Test
	public final void testTraceParser_fail7a()
	{
		checkParsingPre("(and (= 5 input"+_M+") (> func output"+_N+" input"+_M+") output"+_N+" ) a b ",true);
	}	
	
	/** Too many closing braces since one opening one is missing. */
	@Test
	public final void testTraceParser_fail7b()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			checkParsingPre("(and (= 5 input"+_M+") (> func output"+_N+" input"+_M+") output"+_N+"))",false);
		}},IllegalArgumentException.class,"extra text at the end of expression");
	}
	
	/** A few nested terms. This method test that variables are correctly associated with functions 
	 * in a more complex expression than those seen above. 
	 */
	@Test
	public final void testTraceParser10()
	{
		checkParsingPre("(and (= 5 input"+_M+") (> (func (func a input"+_M+") (func b (func 4 3))) output"+_N+"))",true);
		// useCount term
		// 0 (func a input"+_M+")
		// 1 (func 4 3)
		// 2 (func b FUNC_1)
		// 3 (func FUNC_0 FUNC_2)
		StringBuffer expectedDecls = new StringBuffer();
		for(int fnUse=0;fnUse < 4;++fnUse)
			for(int arg=0;arg<3;++arg)
				expectedDecls.append("(define "+SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, fnUse, arg)+"::int)"+SmtLabelRepresentation.ENDL);
				
		final String expectedCompDeclarations = SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
				expectedDecls.toString(),SmtLabelRepresentation.blockVARDECLS);

		CompositionOfFunctions comp = lbls.labelMapConstructionOfDataTraces.get(AbstractLearnerGraph.generateNewLabel("func",lbls.config,converter)).pre;
		Assert.assertEquals(expectedCompDeclarations,comp.varDeclarations);
		
		String 
			FUNC0=SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 0, 0),
			FUNC1=SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 1, 0),
			FUNC2=SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 2, 0),
			FUNC3=SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 3, 0),
			arg0_1 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 0, 1),
			arg0_2 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 0, 2),
			arg1_1 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 1, 1),
			arg1_2 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 1, 2),
			arg2_1 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 2, 1),
			arg2_2 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 2, 2),
			arg3_1 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 3, 1),
			arg3_2 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 3, 2);
		
		Assert.assertEquals("(and (= 5 input"+_M+") (> "+SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 3, 0)+" output"+_N+"))"+
				SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
						"(= "+FUNC0+" (func "+arg0_1+" "+arg0_2+"))(= "+arg0_1+" a)(= "+arg0_2+" input"+_M+")"+SmtLabelRepresentation.ENDL+
						"(> 55 "+FUNC0+")"+SmtLabelRepresentation.ENDL+
						"(> "+arg0_1+" "+arg0_2+")"+SmtLabelRepresentation.ENDL+
						"(= "+FUNC1+" (func "+arg1_1+" "+arg1_2+"))(= "+arg1_1+" 4)(= "+arg1_2+" 3)"+SmtLabelRepresentation.ENDL+
						"(> 55 "+FUNC1+")"+SmtLabelRepresentation.ENDL+
						"(> "+arg1_1+" "+arg1_2+")"+SmtLabelRepresentation.ENDL+
						"(= "+FUNC2+" (func "+arg2_1+" "+arg2_2+"))(= "+arg2_1+" b)(= "+arg2_2+" "+FUNC1+")"+SmtLabelRepresentation.ENDL+
						"(> 55 "+FUNC2+")"+SmtLabelRepresentation.ENDL+
						"(> "+arg2_1+" "+arg2_2+")"+SmtLabelRepresentation.ENDL+
						"(= "+FUNC3+" (func "+arg3_1+" "+arg3_2+"))(= "+arg3_1+" "+FUNC0+")(= "+arg3_2+" "+FUNC2+")"+SmtLabelRepresentation.ENDL+
						"(> 55 "+FUNC3+")"+SmtLabelRepresentation.ENDL+
						"(> "+arg3_1+" "+arg3_2+")"+SmtLabelRepresentation.ENDL
				,
				SmtLabelRepresentation.blockVARS
				),
				comp.relabelledText);
		Assert.assertEquals(expectedCompDeclarations,comp.varDeclarations);
		Assert.assertEquals(1,comp.variablesUsedForArgs.size());
		Assert.assertArrayEquals(new String[]{
				SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 0, JUConstants.intUNKNOWN),
				SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 1, JUConstants.intUNKNOWN),
				SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 2, JUConstants.intUNKNOWN),
				SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 3, JUConstants.intUNKNOWN)}, comp.variablesUsedForArgs.get(lbls.functionMap.get("func")).toArray());
	}
	
	
	/** a few operations. */
	@Test
	public final void testTraceParser11()
	{
		checkParsingPre("(and (= (func (+ y (func (+ x 3) (func (+ y 6)) input"+_M+")) 90) 5) (> (func output"+_N+" (func a input"+_M+")) (func b (func (+ x 3)))))",false);
		CompositionOfFunctions comp = lbls.labelMapConstructionOfDataTraces.get(AbstractLearnerGraph.generateNewLabel("func",lbls.config,converter)).pre;
		Assert.assertEquals("",comp.varDeclarations);
		Assert.assertEquals("(and (= (func (+ y (func (+ x 3) (func (+ y 6)) input"+_M+")) 90) 5) (> (func output"+_N+" (func a input"+_M+")) (func b (func (+ x 3)))))",comp.relabelledText);
		Assert.assertEquals(0,comp.variablesUsedForArgs.size());
	}
	
	/** A few operations. Some of them have a constraint, some others do not. */
	@Test
	public final void testTraceParser12()
	{
		lbls.parseCollection(Arrays.asList(new String[]{
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.PRE+ " ( define m"+_N+"::nat )",
				QSMTool.cmdOperation+" "+INITMEM+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" 0)",
				QSMTool.cmdOperation+" "+"add"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (+ m"+_M+" 1))",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.PRE+ " (> m"+_M+" 0)",
				QSMTool.cmdOperation+" "+"remove"+" "+SmtLabelRepresentation.OP_DATA.POST+ " (= m"+_N+" (- m"+_M+" 1))",
				QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.ARITY+" 2",
				QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0::int)",
				QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"1::int)",
				QSMTool.cmdLowLevelFunction+" func "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"2::int)",
				QSMTool.cmdLowLevelFunction+" funcT "+FUNC_DATA.ARITY+" 0",
				QSMTool.cmdLowLevelFunction+" funcT "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0::int)",
				QSMTool.cmdLowLevelFunction+" funcOne "+FUNC_DATA.ARITY+" 1",
				QSMTool.cmdLowLevelFunction+" funcOne "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"0::int)",
				QSMTool.cmdLowLevelFunction+" funcOne "+FUNC_DATA.DECL+" (define "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"1::int)",
				QSMTool.cmdLowLevelFunction+" funcOne "+FUNC_DATA.CONSTRAINT+" (> "+SmtLabelRepresentation.functionArg+SmtLabelRepresentation.delimiterString+"1 0)",
				QSMTool.cmdOperation+" "+"func "+OP_DATA.PRE+" "+
"(and (= (funcT) (+ y (func (funcOne (+ x 3)) (+ 2 (func (+ input"+_M+" (funcT)) output"+_N+" ))))) (> (func output"+_N+" (funcOne (funcT))) input"+_M+"))"
		}));

		String 
			FUNC_0=SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 0, 0),
			FUNC_1=SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 1, 0),
			FUNC_2=SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 2, 0),
			One_0=SmtLabelRepresentation.generateFreshVariable("funcOne", VARIABLEUSE.PRE, 0, 0),
			One_1=SmtLabelRepresentation.generateFreshVariable("funcOne", VARIABLEUSE.PRE, 1, 0),
			T_0=SmtLabelRepresentation.generateFreshVariable("funcT", VARIABLEUSE.PRE, 0, 0),
			T_1=SmtLabelRepresentation.generateFreshVariable("funcT", VARIABLEUSE.PRE, 1, 0),
			T_2=SmtLabelRepresentation.generateFreshVariable("funcT", VARIABLEUSE.PRE, 2, 0),
			fArg0_1 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 0, 1),
			fArg0_2 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 0, 2),
			fArg1_1 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 1, 1),
			fArg1_2 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 1, 2),
			fArg2_1 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 2, 1),
			fArg2_2 = SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 2, 2),
			OneArg0_1 = SmtLabelRepresentation.generateFreshVariable("funcOne", VARIABLEUSE.PRE, 0, 1),
			OneArg1_1 = SmtLabelRepresentation.generateFreshVariable("funcOne", VARIABLEUSE.PRE, 1, 1);
		
		StringBuffer expectedDecls = new StringBuffer();
		expectedDecls.append(("(define "+SmtLabelRepresentation.generateFreshVariable("funcT", VARIABLEUSE.PRE, 0, 0)+"::int)"+SmtLabelRepresentation.ENDL));
		for(int arg=0;arg<2;++arg)
			expectedDecls.append("(define "+SmtLabelRepresentation.generateFreshVariable("funcOne", VARIABLEUSE.PRE, 0, arg)+"::int)"+SmtLabelRepresentation.ENDL);
		expectedDecls.append(("(define "+SmtLabelRepresentation.generateFreshVariable("funcT", VARIABLEUSE.PRE, 1, 0)+"::int)"+SmtLabelRepresentation.ENDL));
		for(int arg=0;arg<3;++arg)
			expectedDecls.append("(define "+SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 0, arg)+"::int)"+SmtLabelRepresentation.ENDL);
		for(int arg=0;arg<3;++arg)
			expectedDecls.append("(define "+SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 1, arg)+"::int)"+SmtLabelRepresentation.ENDL);
		expectedDecls.append(("(define "+SmtLabelRepresentation.generateFreshVariable("funcT", VARIABLEUSE.PRE, 2, 0)+"::int)"+SmtLabelRepresentation.ENDL));
		for(int arg=0;arg<2;++arg)
			expectedDecls.append("(define "+SmtLabelRepresentation.generateFreshVariable("funcOne", VARIABLEUSE.PRE, 1, arg)+"::int)"+SmtLabelRepresentation.ENDL);
		for(int arg=0;arg<3;++arg)
			expectedDecls.append("(define "+SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 2, arg)+"::int)"+SmtLabelRepresentation.ENDL);
			
		final String expectedCompDeclarations = SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
				expectedDecls.toString(),SmtLabelRepresentation.blockVARDECLS);

		CompositionOfFunctions comp = lbls.labelMapConstructionOfDataTraces.get(AbstractLearnerGraph.generateNewLabel("func",lbls.config,converter)).pre;
		Assert.assertEquals(expectedCompDeclarations,comp.varDeclarations);

		// funcT
		// 0,1,2
		// (T_0)
		
		// funcOne
		// 0 (= One_0 (funcOne OneArg0_1))(= OneArg0_1 (+ x 3))
		// 1 (= One_1 (funcOne OneArg1_1))(= OneArg1_1 T_2)
		
		// func
		// 0 (= FUNC_0 (func fArg0_1 fArg0_2))(= fArg0_1 (+ input"+_M+" T_1))(= fArg0_2 output"+_N+")"
		// 1 (= FUNC_1 (func fArg1_1 fArg1_2))(= fArg1_1 One_0)(= fArg1_2 (+ 2 FUNC_0))
		// 2 (= FUNC_2 (func fArg2_1 fArg2_2))(= fArg2_1 output"+_N+")(= fArg2_2 One_1)
		
		Assert.assertEquals("(and (= "+T_0+" (+ y "+FUNC_1+")) (> "+FUNC_2+" input"+_M+"))"+
				SmtLabelRepresentation.encloseInBeginEndIfNotEmpty(
						"(= "+T_0+" (funcT))"+SmtLabelRepresentation.ENDL+
						"(= "+One_0+" (funcOne "+OneArg0_1+"))(= "+OneArg0_1+" (+ x 3))"+SmtLabelRepresentation.ENDL+
						"(> "+OneArg0_1+" 0)"+SmtLabelRepresentation.ENDL+
						"(= "+T_1+" (funcT))"+SmtLabelRepresentation.ENDL+
						"(= "+FUNC_0+" (func "+fArg0_1+" "+fArg0_2+"))(= "+fArg0_1+" (+ input"+_M+" "+T_1+"))(= "+fArg0_2+" output"+_N+")"+SmtLabelRepresentation.ENDL+
						"(= "+FUNC_1+" (func "+fArg1_1+" "+fArg1_2+"))(= "+fArg1_1+" "+One_0+")(= "+fArg1_2+" (+ 2 "+FUNC_0+"))"+SmtLabelRepresentation.ENDL+
						"(= "+T_2+" (funcT))"+SmtLabelRepresentation.ENDL+
						"(= "+One_1+" (funcOne "+OneArg1_1+"))(= "+OneArg1_1+" "+T_2+")"+SmtLabelRepresentation.ENDL+
						"(> "+OneArg1_1+" 0)"+SmtLabelRepresentation.ENDL+
						"(= "+FUNC_2+" (func "+fArg2_1+" "+fArg2_2+"))(= "+fArg2_1+" output"+_N+")(= "+fArg2_2+" "+One_1+")"+SmtLabelRepresentation.ENDL
				,
				SmtLabelRepresentation.blockVARS
				),
				comp.relabelledText);

		Assert.assertEquals(3,comp.variablesUsedForArgs.size());
		Assert.assertArrayEquals(new String[]{
				SmtLabelRepresentation.generateFreshVariable("funcT", VARIABLEUSE.PRE, 0, JUConstants.intUNKNOWN),
				SmtLabelRepresentation.generateFreshVariable("funcT", VARIABLEUSE.PRE, 1, JUConstants.intUNKNOWN),
				SmtLabelRepresentation.generateFreshVariable("funcT", VARIABLEUSE.PRE, 2, JUConstants.intUNKNOWN)}, comp.variablesUsedForArgs.get(lbls.functionMap.get("funcT")).toArray());
		Assert.assertArrayEquals(new String[]{
				SmtLabelRepresentation.generateFreshVariable("funcOne", VARIABLEUSE.PRE, 0, JUConstants.intUNKNOWN),
				SmtLabelRepresentation.generateFreshVariable("funcOne", VARIABLEUSE.PRE, 1, JUConstants.intUNKNOWN)}, comp.variablesUsedForArgs.get(lbls.functionMap.get("funcOne")).toArray());
		Assert.assertArrayEquals(new String[]{
				SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 0, JUConstants.intUNKNOWN),
				SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 1, JUConstants.intUNKNOWN),
				SmtLabelRepresentation.generateFreshVariable("func", VARIABLEUSE.PRE, 2, JUConstants.intUNKNOWN)}, comp.variablesUsedForArgs.get(lbls.functionMap.get("func")).toArray());
	}
	
	/** Function without arguments - should not happen because the whole point of collecting arguments
	 * is for functions with some positive (and fixed in advance) number of arguments. */
	@Test
	public final void testTraceParser_fail8()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			checkParsingPre("(and (= 5 input"+_M+") (> (func output"+_N+" (func )))  output"+_N+")",true);
		}},IllegalArgumentException.class,"function func should take 2 arguments");
	}
	
	/** Function without arguments - we do not ask for arguments to be collected, hence an exception should not be thrown. */
	@Test
	public final void testTraceParser13()
	{
		checkParsingPre("(and (= 5 input"+_M+") (> (func output"+_N+" (func )))  output"+_N+")",false);
	}

	/** Inconsistent number of arguments to a function. */
	public final void testTraceParser_fail9()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			checkParsingPre("(and (= 5 input"+_M+") (> (func output"+_N+" (func a input"+_M+") (func b (func 8))) output"+_N+"))",true);
		}},IllegalArgumentException.class,"function func should take 2 arguments");
	}

	/** Inconsistent number of arguments to a function - exception not thrown because we did not ask for this particular function to be extracted. */
	@Test
	public final void testTraceParser14()
	{
		checkParsingPre("(and (= 5 input"+_M+") (> (func output (func a input"+_M+") (func b (func 8))) output"+_N+"))",false);
	}
	
	/** Too many arguments to a function. */
	@Test
	public final void testTraceParser_fail10a()
	{
		TestHelper.checkForCorrectException(new whatToRun() { public @Override void run() {
			checkParsingPre("(and (= 5 input"+_M+") (> (func output (func (func b 4 6) input"+_M+")) output"+_N+"))",true);
		}},IllegalArgumentException.class,"function func should take 2 arguments");
	}

	/** Inconsistent number of arguments to a function - not thrown since we did not parse details. */
	@Test
	public final void testTraceParser_fail10b()
	{
		checkParsingPre("(and (= 5 input"+_M+") (> (func output (func (func b 4 6) input"+_M+")) output"+_N+"))",false);
	}

}
