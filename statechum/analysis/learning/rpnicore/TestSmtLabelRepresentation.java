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

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.Helper;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.Smt;
import statechum.analysis.learning.rpnicore.LabelRepresentation.Label;

import static statechum.analysis.learning.rpnicore.LabelRepresentation.INITMEM;
import static statechum.analysis.learning.rpnicore.LabelRepresentation.ENDL;

public class TestSmtLabelRepresentation {
	Configuration config = null;
	
	@Before
	public void beforeTest()
	{
		config = Configuration.getDefaultConfiguration().copy(); 
	}
	
	@Test
	public void testNoLabels1()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		Assert.assertTrue(lbls.labelMap.isEmpty());
	}

	@Test
	public void testNoLabels2()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(null);
		lbls.parseLabel("");
		Assert.assertTrue(lbls.labelMap.isEmpty());
	}
		
	@Test
	public void testCreateLabels_error1()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseLabel(INITMEM+" ");
		}}, IllegalArgumentException.class,"expected details for label");
	}
	
	@Test
	public void testCreateLabels_error2()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseLabel(INITMEM+" JUNK");
		}}, IllegalArgumentException.class,"expected [PRE");
	}
	
	@Test
	public void testCreateLabels_error3()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseLabel(INITMEM+"  "+LabelRepresentation.XM_DATA.PRE);
		}}, IllegalArgumentException.class,"expected specification for label");
	}

	@Test
	public void testCreateLabels_error4()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseLabel(INITMEM+"  "+LabelRepresentation.XM_DATA.PRE+"  ");
		}}, IllegalArgumentException.class,"expected specification for label");
	}

	@Test
	public void testCreateLabels1()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE.name()+" varDecl");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE.name()+" varDecl2");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST.name()+" memory0");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST.name()+" memory1");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST.name()+" memory2");
		Assert.assertEquals(1,lbls.labelMap.size());
		Label l = lbls.labelMap.entrySet().iterator().next().getValue();
		Assert.assertEquals(INITMEM,lbls.labelMap.entrySet().iterator().next().getKey());
		Assert.assertEquals(INITMEM,l.getName());
		Assert.assertEquals("memory0\nmemory1\nmemory2",l.post);
		Assert.assertEquals("varDecl\nvarDecl2",l.pre);
	}
	
	@Test
	public void testCreateLabels2()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE.name()+" varDecl");
		lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.POST.name()+" postA and            more");
		lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.POST.name()+"     details of postcondition     of A     ");
		lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE.name()+" value of precondition of B ");
		Assert.assertEquals(3,lbls.labelMap.size());

		{
			Label l = lbls.labelMap.get(INITMEM);
			Assert.assertEquals(INITMEM,l.getName());
			Assert.assertNull(l.post);
			Assert.assertEquals("varDecl",l.pre);
		}

		{
			Label l = lbls.labelMap.get("A");
			Assert.assertEquals("A",l.getName());
			Assert.assertNull(l.pre);
			Assert.assertEquals("postA and more\ndetails of postcondition of A",l.post);
		}

		{
			Label l = lbls.labelMap.get("B");
			Assert.assertEquals("B",l.getName());
			Assert.assertNull(l.post);
			Assert.assertEquals("value of precondition of B",l.pre);
		}
	}

	@Test
	public void testEquals()
	{
		LabelRepresentation lblsA = new LabelRepresentation();
		{
			lblsA.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDecl_N");
			lblsA.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " initCond_N");
			lblsA.parseLabel("A"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA");
			lblsA.parseLabel("A"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA");
			lblsA.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB");
			lblsA.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB");
		}
		LabelRepresentation lblsB = new LabelRepresentation();
		{
			lblsB.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ "    varDecl_N");
			lblsB.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " initCond_N");
			lblsB.parseLabel("A"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA");
			lblsB.parseLabel("A"+" "+LabelRepresentation.XM_DATA.POST+ "    somePostcondA");
			lblsB.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB");
			lblsB.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ "       somePostcondB");
		}
		LabelRepresentation lblsDiffA = new LabelRepresentation();
		{
			lblsDiffA.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ "    varDecl_N");
			lblsDiffA.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " initCond_N");
			lblsDiffA.parseLabel("A"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA");
			lblsDiffA.parseLabel("A"+" "+LabelRepresentation.XM_DATA.POST+ "    somePostcondA");
			lblsDiffA.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB");
			lblsDiffA.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ "       somePostcondB");
			lblsDiffA.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ "       somePostcondB");
		}
		LabelRepresentation lblsDiffB = new LabelRepresentation();
		TestEqualityComparisonAndHashCode.equalityTestingHelper(lblsA,lblsA,lblsDiffA,lblsDiffB);
	}
	
	@Test
	public void testRelabel()
	{
		Assert.assertEquals("text more var_6 text var_@3 smth_6 again ",
				LabelRepresentation.toCurrentMem("text more var_N text var_M smth_N again ", 6,-3));
		Assert.assertEquals("text more var_@6 text var_7 smth_@6 again ",
				LabelRepresentation.toCurrentMem("text more var_N text var_M smth_N again ", -6,7));
		Assert.assertEquals("text more text var_8 again ",
				LabelRepresentation.toCurrentMem("text more text var_M again ", 6,8));
		Assert.assertEquals("",
				LabelRepresentation.toCurrentMem("", 6,4));
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
	public void testCreateConjunctionMissingInitial()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA");
			lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA");
			lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB");
			lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB");
			lbls.getConjunctionForPath(Arrays.asList(new String[]{}));
		}}, IllegalArgumentException.class,"missing initial");
	}
	
	@Test
	public void testCreateLemmaMissingInitial()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA");
			lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA");
			lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB");
			lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB");
			lbls.getLemma(new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a-#D\nB-b->E", "createLemmas1"),
					Configuration.getDefaultConfiguration().copy()));
		}}, IllegalArgumentException.class,"missing initial");
	}
	
	@Test
	public void testCreateConjunctionEmpty1()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDecl_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " decl_N");
		lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA");
		lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA");
		lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB");
		lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB");
		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[]"+ENDL+"varDecl_0"+ENDL+"decl_0"+ENDL,
				lbls.getConjunctionForPath(Arrays.asList(new String[]{})));
	}
	
	@Test
	public void testCreateConjunctionEmpty2()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDecl_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " initCond_N");
		lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA");
		lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA");
		lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB");
		lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB");
		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[]"+ENDL+"varDecl_0"+ENDL+
				LabelRepresentation.assertString+ENDL+"initCond_0"+ENDL+')'+ENDL,
				lbls.getConjunctionForPath(Arrays.asList(new String[]{})));
	}
	
	@Test
	public void testCreateConjunctionUnknown1()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDecl_N");
			lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " initCond_N");
			lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA");
			lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA");
			lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB");
			lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB");
			lbls.getConjunctionForPath(Arrays.asList(new String[]{"unknown_label"}));
		}}, IllegalArgumentException.class,"unknown label unknown_label");
	}
	
	@Test
	public void testCreateConjunctionUnknown2()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDecl_N");
			lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " initCond_N");
			lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA");
			lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA");
			lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB");
			lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB");
			lbls.getConjunctionForPath(Arrays.asList(new String[]{"A","B","unknown_label"}));
		}}, IllegalArgumentException.class,"unknown label unknown_label");
	}
	
	@Test
	public void testCreateLemmasUnknown()
	{
		Helper.checkForCorrectException(new whatToRun() { public void run()
		{
			LabelRepresentation lbls = new LabelRepresentation();
			lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDecl_N");
			lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " initCond_N");
			lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA");
			lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA");
			lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB");
			lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB");
			lbls.getLemma(new LearnerGraph(TestFSMAlgo.buildGraph("A-a1->B-a2->C-a3-#D\nB-b->E", "createLemmas1"),
					Configuration.getDefaultConfiguration().copy()));
		}}, IllegalArgumentException.class,"unknown label a1");
	}
	
	@Test
	public void testCreateConjunction1()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclP_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclQ_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " initCond_N");
		lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA_N");
		lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA_N");
		lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB_N");
		lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB_N");
		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[A, B]"+ENDL+
				"varDeclP_0"+ENDL+"varDeclQ_0"+ENDL+
				"varDeclP_1"+ENDL+"varDeclQ_1"+ENDL+
				"varDeclP_2"+ENDL+"varDeclQ_2"+ENDL+
				LabelRepresentation.assertString+ENDL+"initCond_0"+ENDL+')'+ENDL+
				LabelRepresentation.commentForLabel+"A"+ENDL+
				LabelRepresentation.assertString+ENDL+"somePrecondA_0"+ENDL+')'+ENDL+
				LabelRepresentation.assertString+ENDL+"somePostcondA_1"+ENDL+')'+ENDL+
				LabelRepresentation.commentForLabel+"B"+ENDL+
				LabelRepresentation.assertString+ENDL+"somePrecondB_1"+ENDL+')'+ENDL+
				LabelRepresentation.assertString+ENDL+"somePostcondB_2"+ENDL+')'+ENDL
				,
				lbls.getConjunctionForPath(Arrays.asList(new String[]{"A","B"})));
	}
	
	@Test
	public void testCreateConjunction2()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclP_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclQ_N");
		lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA_N");
		lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB_N");
		lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB_N");
		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[A, B]"+ENDL+
				"varDeclP_0"+ENDL+"varDeclQ_0"+ENDL+
				"varDeclP_1"+ENDL+"varDeclQ_1"+ENDL+
				"varDeclP_2"+ENDL+"varDeclQ_2"+ENDL+
				LabelRepresentation.commentForLabel+"A"+ENDL+
				LabelRepresentation.assertString+ENDL+"somePrecondA_0"+ENDL+')'+ENDL+
				LabelRepresentation.commentForLabel+"B"+ENDL+
				LabelRepresentation.assertString+ENDL+"somePrecondB_1"+ENDL+')'+ENDL+
				LabelRepresentation.assertString+ENDL+"somePostcondB_2"+ENDL+')'+ENDL
				,
				lbls.getConjunctionForPath(Arrays.asList(new String[]{"A","B"})));
	}

	/** A typical case. */
	@Test
	public void testCreateLemmas1()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclP_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclQ_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " initCond_N");
		lbls.parseLabel("a"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA_N");
		lbls.parseLabel("a"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA_N");
		lbls.parseLabel("b"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB_N");
		lbls.parseLabel("b"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB_N");
		Assert.assertEquals(
				"varDeclP_@1"+ENDL+
				"varDeclQ_@1"+ENDL+
				"varDeclP_@2"+ENDL+
				"varDeclQ_@2"+ENDL+
				"varDeclP_@3"+ENDL+
				"varDeclQ_@3"+ENDL+
				"varDeclP_@4"+ENDL+
				"varDeclQ_@4"+ENDL+
				"varDeclP_@5"+ENDL+
				"varDeclQ_@5"+ENDL+
				LabelRepresentation.commentForLabel+"from @1-a->@2"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1)"+ENDL+
				" (and  somePrecondA_@2 somePostcondA_@2))"+ENDL+
				LabelRepresentation.commentForLabel+"from @2-b->@3"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePrecondA_@2 somePostcondA_@2)"+ENDL+
				" (and  somePrecondB_@3 somePostcondB_@3))"+ENDL+
				LabelRepresentation.commentForLabel+"from @2-a->@4"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePrecondA_@2 somePostcondA_@2)"+ENDL+
				" (and  somePrecondA_@4 somePostcondA_@4))"+ENDL+
				LabelRepresentation.commentForLabel+"from @4-a-#@5"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePrecondA_@2 somePostcondA_@2 somePrecondA_@4 somePostcondA_@4)"+ENDL+
				" (not  somePrecondA_@5))"+ENDL+
				";; END OF LEMMAS"+ENDL
				,
				lbls.getLemma(new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a-#D\nB-b->E", "createLemmas1"),
						Configuration.getDefaultConfiguration().copy())));
	}

	/** Empty precondition on a transition to a reject-state. */
	@Test
	public void testCreateLemmas2()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclP_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclQ_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " initCond_N");
		lbls.parseLabel("a"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA_N");
		lbls.parseLabel("b"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB_N");
		lbls.parseLabel("b"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB_N");
		Assert.assertEquals(
				"varDeclP_@1"+ENDL+
				"varDeclQ_@1"+ENDL+
				"varDeclP_@2"+ENDL+
				"varDeclQ_@2"+ENDL+
				"varDeclP_@3"+ENDL+
				"varDeclQ_@3"+ENDL+
				"varDeclP_@4"+ENDL+
				"varDeclQ_@4"+ENDL+
				"varDeclP_@5"+ENDL+
				"varDeclQ_@5"+ENDL+
				LabelRepresentation.commentForLabel+"from @1-a->@2"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1)"+ENDL+
				" (and  somePostcondA_@2))"+ENDL+
				LabelRepresentation.commentForLabel+"from @2-b->@3"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePostcondA_@2)"+ENDL+
				" (and  somePrecondB_@3 somePostcondB_@3))"+ENDL+
				LabelRepresentation.commentForLabel+"from @2-a->@4"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePostcondA_@2)"+ENDL+
				" (and  somePostcondA_@4))"+ENDL+
				";; END OF LEMMAS"+ENDL
				,
				lbls.getLemma(new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a-#D\nB-b->E", "createLemmas1"),
						Configuration.getDefaultConfiguration().copy())));
	}

	/** Empty postcondition */
	@Test
	public void testCreateLemmas3()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclP_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclQ_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " initCond_N");
		lbls.parseLabel("a"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA_N");
		lbls.parseLabel("a"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA_N");
		lbls.parseLabel("b"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB_N");
		Assert.assertEquals(
				"varDeclP_@1"+ENDL+
				"varDeclQ_@1"+ENDL+
				"varDeclP_@2"+ENDL+
				"varDeclQ_@2"+ENDL+
				"varDeclP_@3"+ENDL+
				"varDeclQ_@3"+ENDL+
				"varDeclP_@4"+ENDL+
				"varDeclQ_@4"+ENDL+
				"varDeclP_@5"+ENDL+
				"varDeclQ_@5"+ENDL+
				LabelRepresentation.commentForLabel+"from @1-a->@2"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1)"+ENDL+
				" (and  somePrecondA_@2 somePostcondA_@2))"+ENDL+
				LabelRepresentation.commentForLabel+"from @2-b->@3"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePrecondA_@2 somePostcondA_@2)"+ENDL+
				" (and  somePrecondB_@3))"+ENDL+
				LabelRepresentation.commentForLabel+"from @2-a->@4"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePrecondA_@2 somePostcondA_@2)"+ENDL+
				" (and  somePrecondA_@4 somePostcondA_@4))"+ENDL+
				LabelRepresentation.commentForLabel+"from @4-a-#@5"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePrecondA_@2 somePostcondA_@2 somePrecondA_@4 somePostcondA_@4)"+ENDL+
				" (not  somePrecondA_@5))"+ENDL+
				";; END OF LEMMAS"+ENDL
				,
				lbls.getLemma(new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a-#D\nB-b->E", "createLemmas1"),
						Configuration.getDefaultConfiguration().copy())));
	}

	/** A more complex set of lemmas. */
	@Test
	public void testCreateLemmas4()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclP_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclQ_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " initCond_N");
		lbls.parseLabel("a"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA_N");
		lbls.parseLabel("a"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA_N");
		lbls.parseLabel("b"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB_N");
		lbls.parseLabel("b"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB_N");
		//lbls.labelMap.get("b").pre=null;
		//lbls.labelMap.get("b").post=null;
		
		lbls.parseLabel("c"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondC_N");
		lbls.parseLabel("c"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondC_N");
		Assert.assertEquals(
				"varDeclP_@1"+ENDL+
				"varDeclQ_@1"+ENDL+
				"varDeclP_@2"+ENDL+
				"varDeclQ_@2"+ENDL+
				"varDeclP_@3"+ENDL+
				"varDeclQ_@3"+ENDL+
				"varDeclP_@4"+ENDL+
				"varDeclQ_@4"+ENDL+
				"varDeclP_@5"+ENDL+
				"varDeclQ_@5"+ENDL+
				"varDeclP_@6"+ENDL+
				"varDeclQ_@6"+ENDL+
				LabelRepresentation.commentForLabel+"from @1-a->@2"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1)"+ENDL+
				" (and  somePrecondA_@2 somePostcondA_@2))"+ENDL+
				LabelRepresentation.commentForLabel+"from @2-b->@3"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePrecondA_@2 somePostcondA_@2)"+ENDL+
				" (and  somePrecondB_@3 somePostcondB_@3))"+ENDL+
				LabelRepresentation.commentForLabel+"from @3-c->@4"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePrecondA_@2 somePostcondA_@2 somePrecondB_@3 somePostcondB_@3)"+ENDL+
				" (and  somePrecondC_@4 somePostcondC_@4))"+ENDL+
				LabelRepresentation.commentForLabel+"from @2-a->@5"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePrecondA_@2 somePostcondA_@2)"+ENDL+
				" (and  somePrecondA_@5 somePostcondA_@5))"+ENDL+
				LabelRepresentation.commentForLabel+"from @5-a-#@6"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePrecondA_@2 somePostcondA_@2 somePrecondA_@5 somePostcondA_@5)"+ENDL+
				" (not  somePrecondA_@6))"+ENDL+
				";; END OF LEMMAS"+ENDL
				,
				lbls.getLemma(new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a-#D\nB-b->E-c->F", "createLemmas1"),
						Configuration.getDefaultConfiguration().copy())));
	}

	/** Empty pre and post-conditions. */
	@Test
	public void testCreateLemmas5()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclP_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclQ_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " initCond_N");
		lbls.parseLabel("a"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA_N");
		lbls.parseLabel("a"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA_N");
		lbls.parseLabel("b"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB_N");
		lbls.parseLabel("b"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB_N");
		lbls.labelMap.get("b").pre=null;
		lbls.labelMap.get("b").post=null;
		
		lbls.parseLabel("c"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondC_N");
		lbls.parseLabel("c"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondC_N");
		Assert.assertEquals(
				"varDeclP_@1"+ENDL+
				"varDeclQ_@1"+ENDL+
				"varDeclP_@2"+ENDL+
				"varDeclQ_@2"+ENDL+
				"varDeclP_@3"+ENDL+
				"varDeclQ_@3"+ENDL+
				"varDeclP_@4"+ENDL+
				"varDeclQ_@4"+ENDL+
				"varDeclP_@5"+ENDL+
				"varDeclQ_@5"+ENDL+
				"varDeclP_@6"+ENDL+
				"varDeclQ_@6"+ENDL+
				LabelRepresentation.commentForLabel+"from @1-a->@2"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1)"+ENDL+
				" (and  somePrecondA_@2 somePostcondA_@2))"+ENDL+
				LabelRepresentation.commentForLabel+"from @3-c->@4"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePrecondA_@2 somePostcondA_@2)"+ENDL+
				" (and  somePrecondC_@4 somePostcondC_@4))"+ENDL+
				LabelRepresentation.commentForLabel+"from @2-a->@5"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePrecondA_@2 somePostcondA_@2)"+ENDL+
				" (and  somePrecondA_@5 somePostcondA_@5))"+ENDL+
				LabelRepresentation.commentForLabel+"from @5-a-#@6"+ENDL+
				LabelRepresentation.assertString+"(implies (and initCond_@1 somePrecondA_@2 somePostcondA_@2 somePrecondA_@5 somePostcondA_@5)"+ENDL+
				" (not  somePrecondA_@6))"+ENDL+
				";; END OF LEMMAS"+ENDL
				,
				lbls.getLemma(new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a-#D\nB-b->E-c->F", "createLemmas1"),
						Configuration.getDefaultConfiguration().copy())));
	}

	
	@Test
	public void testSolvingConstraints()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " ( define m_N::nat )");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N 0)");
		lbls.parseLabel("add"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (+ m_M 1))");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.PRE+ " (> m_N 0)");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (- m_M 1))");
		Smt.loadLibrary();
		Smt.closeStdOut();
		Smt smt = new Smt();

		smt.pushContext();
		smt.loadData(lbls.getConjunctionForPath(Arrays.asList(new String[]{"remove"})));
		Assert.assertFalse(smt.check());
		smt.popContext();

		smt.pushContext();
		smt.loadData(lbls.getConjunctionForPath(Arrays.asList(new String[]{})));
		Assert.assertTrue(smt.check());
		smt.popContext();

		smt.pushContext();
		smt.loadData(lbls.getConjunctionForPath(Arrays.asList(new String[]{"add","remove"})));
		Assert.assertTrue(smt.check());
		smt.popContext();


		smt.pushContext();
		smt.loadData(lbls.getConjunctionForPath(Arrays.asList(new String[]{"add","remove","remove"})));
		Assert.assertFalse(smt.check());
		smt.popContext();

		Smt.reopenStdOut();
	}
}
