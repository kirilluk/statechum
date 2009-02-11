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
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.Helper;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.Smt;
import statechum.analysis.learning.rpnicore.LabelRepresentation.AbstractState;
import statechum.analysis.learning.rpnicore.LabelRepresentation.Label;

import static statechum.analysis.learning.rpnicore.LabelRepresentation.INITMEM;
import static statechum.analysis.learning.rpnicore.LabelRepresentation.ENDL;
import statechum.analysis.learning.AbstractOracle;

public class TestSmtLabelRepresentation {
	Configuration config = null;
	
	@Before
	public void beforeTest()
	{
		config = Configuration.getDefaultConfiguration().copy(); 
	}

	@AfterClass
	public static void afterTest()
	{
		Smt.reopenStdOut();
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
			lbls.constructPathAxioms(new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a-#D\nB-b->E", "createLemmas1"),
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
		AbstractState state = lbls.getConjunctionForPath(Arrays.asList(new String[]{}));
		Assert.assertEquals("varDecl_0"+ENDL+"decl_0"+ENDL,state.variableDeclarations);
		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[]"+ENDL+"true"+ENDL, state.abstractState);
		Assert.assertEquals(0,state.stateNumber);
		Assert.assertNull(state.lastLabel);
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
		AbstractState state = lbls.getConjunctionForPath(Arrays.asList(new String[]{}));
		Assert.assertEquals("varDecl_0"+ENDL,state.variableDeclarations);
		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[]"+ENDL+"(and"+ENDL+"initCond_0"+ENDL+')'+ENDL,state.abstractState);
		Assert.assertEquals(0,state.stateNumber);
		Assert.assertNull(state.lastLabel);
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
			lbls.constructPathAxioms(new LearnerGraph(FsmParser.buildGraph("A-a1->B-a2->C-a3-#D\nB-b->E", "createLemmas1"),
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
		AbstractState state = lbls.getConjunctionForPath(Arrays.asList(new String[]{"A","B"}));
		Assert.assertEquals("varDeclP_0"+ENDL+"varDeclQ_0"+ENDL+
				"varDeclP_1"+ENDL+"varDeclQ_1"+ENDL+
				"varDeclP_2"+ENDL+"varDeclQ_2"+ENDL,state.variableDeclarations);

		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[A, B]"+ENDL+
				"(and"+ENDL+"initCond_0"+ENDL+
				LabelRepresentation.commentForLabel+"A"+ENDL+
				"somePrecondA_0"+ENDL+
				"somePostcondA_1"+ENDL+
				LabelRepresentation.commentForLabel+"B"+ENDL+
				"somePrecondB_1"+ENDL+
				"somePostcondB_2"+ENDL+')'+ENDL,
				state.abstractState);
		Assert.assertEquals(2,state.stateNumber);
		Assert.assertSame(lbls.labelMap.get("B"),state.lastLabel);
	}
	
	
	private LabelRepresentation testCreateConjunction2_internal()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclP_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclQ_N");
		lbls.parseLabel("A"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA_N");
		lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB_N");
		lbls.parseLabel("B"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB_N");
		AbstractState state = lbls.getConjunctionForPath(Arrays.asList(new String[]{"A","B"}));
		Assert.assertEquals("varDeclP_0"+ENDL+"varDeclQ_0"+ENDL+
				"varDeclP_1"+ENDL+"varDeclQ_1"+ENDL+
				"varDeclP_2"+ENDL+"varDeclQ_2"+ENDL, state.variableDeclarations);

		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[A, B]"+ENDL+
				"(and"+ENDL+
				LabelRepresentation.commentForLabel+"A"+ENDL+
				"somePrecondA_0"+ENDL+
				LabelRepresentation.commentForLabel+"B"+ENDL+
				"somePrecondB_1"+ENDL+
				"somePostcondB_2"+ENDL+')'+ENDL,
				state.abstractState
				);
		Assert.assertEquals(2,state.stateNumber);
		Assert.assertSame(lbls.labelMap.get("B"),state.lastLabel);
		
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
		AbstractState state = lbls.getConjunctionForPath(Arrays.asList(new String[]{"A","B"}));
		Assert.assertEquals("varDeclP_3"+ENDL+"varDeclQ_3"+ENDL+
				"varDeclP_4"+ENDL+"varDeclQ_4"+ENDL+
				"varDeclP_5"+ENDL+"varDeclQ_5"+ENDL, state.variableDeclarations);

		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[A, B]"+ENDL+
				"(and"+ENDL+
				LabelRepresentation.commentForLabel+"A"+ENDL+
				"somePrecondA_3"+ENDL+
				LabelRepresentation.commentForLabel+"B"+ENDL+
				"somePrecondB_4"+ENDL+
				"somePostcondB_5"+ENDL+')'+ENDL,
				state.abstractState
				);
		Assert.assertEquals(5,state.stateNumber);
		Assert.assertSame(lbls.labelMap.get("B"),state.lastLabel);
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
		//for(Entry<VertexID,AbstractState> entry:lbls.idToState.entrySet())
		//	System.out.println(entry.getKey()+" "+entry.getValue().abstractState);
		String axioms = lbls.constructPathAxioms(new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a-#D\nB-b->E", "createLemmas1"),
				Configuration.getDefaultConfiguration().copy()));
		String expected = "";
		for(int i=0;i<=11;++i)
			expected+=
				"varDeclP_"+i+ENDL+
				"varDeclQ_"+i+ENDL;
		Assert.assertEquals(
			expected+
				LabelRepresentation.commentForTransition+"A(0)-a->B(1)"+ENDL+
				LabelRepresentation.assertString+"(implies"+ENDL+
				LabelRepresentation.commentForNewSeq+"[]"+ENDL+
				"(and"+ENDL+
				"initCond_0"+ENDL+
				")"+ENDL+
				"\t(and somePrecondA_0 somePostcondA_1)))"+ENDL+
				LabelRepresentation.commentForTransition+"B(3)-b->E(4)"+ENDL+
				LabelRepresentation.assertString+"(implies"+ENDL+
				LabelRepresentation.commentForNewSeq+"[a]"+ENDL+
				"(and"+ENDL+
				"initCond_2"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_2"+ENDL+
				"somePostcondA_3"+ENDL+
				")"+ENDL+
				"\t(and somePrecondB_3 somePostcondB_4)))"+ENDL+
				LabelRepresentation.commentForTransition+"B(6)-a->C(7)"+ENDL+
				LabelRepresentation.assertString+"(implies"+ENDL+
				LabelRepresentation.commentForNewSeq+"[a]"+ENDL+
				"(and"+ENDL+
				"initCond_5"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_5"+ENDL+
				"somePostcondA_6"+ENDL+
				")"+ENDL+
				"\t(and somePrecondA_6 somePostcondA_7)))"+ENDL+
				LabelRepresentation.commentForTransition+"C(10)-a-#D(11)"+ENDL+
				LabelRepresentation.assertString+"(implies"+ENDL+
				LabelRepresentation.commentForNewSeq+"[a, a]"+ENDL+
				"(and"+ENDL+
				"initCond_8"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_8"+ENDL+
				"somePostcondA_9"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_9"+ENDL+
				"somePostcondA_10"+ENDL+
				")"+ENDL+
				"\t(not somePrecondA_10)))"+ENDL+
				";; END OF PATH AXIOMS"+ENDL
				,
				axioms);
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
		String axioms = lbls.constructPathAxioms(new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a-#D\nB-b->E", "createLemmas1"),
				Configuration.getDefaultConfiguration().copy()));

		String expected = "";
		for(int i=0;i<=11;++i)
			expected+=
				"varDeclP_"+i+ENDL+
				"varDeclQ_"+i+ENDL;
		Assert.assertEquals(
			expected+
				LabelRepresentation.commentForTransition+"A(0)-a->B(1)"+ENDL+
				LabelRepresentation.assertString+"(implies"+ENDL+
				LabelRepresentation.commentForNewSeq+"[]"+ENDL+
				"(and"+ENDL+
				"initCond_0"+ENDL+
				")"+ENDL+
				"\t(and somePostcondA_1)))"+ENDL+
				LabelRepresentation.commentForTransition+"B(3)-b->E(4)"+ENDL+
				LabelRepresentation.assertString+"(implies"+ENDL+
				LabelRepresentation.commentForNewSeq+"[a]"+ENDL+
				"(and"+ENDL+
				"initCond_2"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePostcondA_3"+ENDL+
				")"+ENDL+
				"\t(and somePrecondB_3 somePostcondB_4)))"+ENDL+
				LabelRepresentation.commentForTransition+"B(6)-a->C(7)"+ENDL+
				LabelRepresentation.assertString+"(implies"+ENDL+
				LabelRepresentation.commentForNewSeq+"[a]"+ENDL+
				"(and"+ENDL+
				"initCond_5"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePostcondA_6"+ENDL+
				")"+ENDL+
				"\t(and somePostcondA_7)))"+ENDL+
				";; END OF PATH AXIOMS"+ENDL
				,
				axioms);
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
		String axioms = lbls.constructPathAxioms(new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a-#D\nB-b->E", "createLemmas1"),
				Configuration.getDefaultConfiguration().copy()));
		String expected = "";
		for(int i=0;i<=11;++i)
			expected+=
				"varDeclP_"+i+ENDL+
				"varDeclQ_"+i+ENDL;
		Assert.assertEquals(
			expected+
				LabelRepresentation.commentForTransition+"A(0)-a->B(1)"+ENDL+
				LabelRepresentation.assertString+"(implies"+ENDL+
				LabelRepresentation.commentForNewSeq+"[]"+ENDL+
				"(and"+ENDL+
				"initCond_0"+ENDL+
				")"+ENDL+
				"\t(and somePrecondA_0 somePostcondA_1)))"+ENDL+
				LabelRepresentation.commentForTransition+"B(3)-b->E(4)"+ENDL+
				LabelRepresentation.assertString+"(implies"+ENDL+
				LabelRepresentation.commentForNewSeq+"[a]"+ENDL+
				"(and"+ENDL+
				"initCond_2"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_2"+ENDL+
				"somePostcondA_3"+ENDL+
				")"+ENDL+
				"\t(and somePrecondB_3)))"+ENDL+
				LabelRepresentation.commentForTransition+"B(6)-a->C(7)"+ENDL+
				LabelRepresentation.assertString+"(implies"+ENDL+
				LabelRepresentation.commentForNewSeq+"[a]"+ENDL+
				"(and"+ENDL+
				"initCond_5"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_5"+ENDL+
				"somePostcondA_6"+ENDL+
				")"+ENDL+
				"\t(and somePrecondA_6 somePostcondA_7)))"+ENDL+
				LabelRepresentation.commentForTransition+"C(10)-a-#D(11)"+ENDL+
				LabelRepresentation.assertString+"(implies"+ENDL+
				LabelRepresentation.commentForNewSeq+"[a, a]"+ENDL+
				"(and"+ENDL+
				"initCond_8"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_8"+ENDL+
				"somePostcondA_9"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_9"+ENDL+
				"somePostcondA_10"+ENDL+
				")"+ENDL+
				"\t(not somePrecondA_10)))"+ENDL+
				";; END OF PATH AXIOMS"+ENDL
				,
				axioms);
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
		lbls.parseLabel("c"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondC_N");
		lbls.parseLabel("c"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondC_N");
		String axioms = lbls.constructPathAxioms(new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a-#D\nB-b->E-c->F", "createLemmas1"),
				Configuration.getDefaultConfiguration().copy()));
		String expected = "";
		for(int i=0;i<=15;++i)
			expected+=
				"varDeclP_"+i+ENDL+
				"varDeclQ_"+i+ENDL;
		Assert.assertEquals(
			expected+
			LabelRepresentation.commentForTransition+"A(0)-a->B(1)"+ENDL+
			LabelRepresentation.assertString+"(implies"+ENDL+
			LabelRepresentation.commentForNewSeq+"[]"+ENDL+
			"(and"+ENDL+
			"initCond_0"+ENDL+
			")"+ENDL+
			"\t(and somePrecondA_0 somePostcondA_1)))"+ENDL+
			LabelRepresentation.commentForTransition+"B(3)-b->E(4)"+ENDL+
			LabelRepresentation.assertString+"(implies"+ENDL+
			LabelRepresentation.commentForNewSeq+"[a]"+ENDL+
			"(and"+ENDL+
			"initCond_2"+ENDL+
			LabelRepresentation.commentForLabel+"a"+ENDL+
			"somePrecondA_2"+ENDL+
			"somePostcondA_3"+ENDL+
			")"+ENDL+
			"\t(and somePrecondB_3 somePostcondB_4)))"+ENDL+
			LabelRepresentation.commentForTransition+"E(7)-c->F(8)"+ENDL+
			LabelRepresentation.assertString+"(implies"+ENDL+
			LabelRepresentation.commentForNewSeq+"[a, b]"+ENDL+
			"(and"+ENDL+
			"initCond_5"+ENDL+
			LabelRepresentation.commentForLabel+"a"+ENDL+
			"somePrecondA_5"+ENDL+
			"somePostcondA_6"+ENDL+
			LabelRepresentation.commentForLabel+"b"+ENDL+
			"somePrecondB_6"+ENDL+
			"somePostcondB_7"+ENDL+
			")"+ENDL+
			"\t(and somePrecondC_7 somePostcondC_8)))"+ENDL+
			LabelRepresentation.commentForTransition+"B(10)-a->C(11)"+ENDL+
			LabelRepresentation.assertString+"(implies"+ENDL+
			LabelRepresentation.commentForNewSeq+"[a]"+ENDL+
			"(and"+ENDL+
			"initCond_9"+ENDL+
			LabelRepresentation.commentForLabel+"a"+ENDL+
			"somePrecondA_9"+ENDL+
			"somePostcondA_10"+ENDL+
			")"+ENDL+
			"\t(and somePrecondA_10 somePostcondA_11)))"+ENDL+
			LabelRepresentation.commentForTransition+"C(14)-a-#D(15)"+ENDL+
			LabelRepresentation.assertString+"(implies"+ENDL+
			LabelRepresentation.commentForNewSeq+"[a, a]"+ENDL+
			"(and"+ENDL+
			"initCond_12"+ENDL+
			LabelRepresentation.commentForLabel+"a"+ENDL+
			"somePrecondA_12"+ENDL+
			"somePostcondA_13"+ENDL+
			LabelRepresentation.commentForLabel+"a"+ENDL+
			"somePrecondA_13"+ENDL+
			"somePostcondA_14"+ENDL+
			")"+ENDL+
			"\t(not somePrecondA_14)))"+ENDL+
			";; END OF PATH AXIOMS"+ENDL
				,axioms);
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
		String axioms = lbls.constructPathAxioms(new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a-#D\nB-b->E-c->F", "createLemmas1"),
				Configuration.getDefaultConfiguration().copy()));
		String expected = "";
		for(int i=0;i<=15;++i)
			expected+=
				"varDeclP_"+i+ENDL+
				"varDeclQ_"+i+ENDL;
		Assert.assertEquals(
			expected+
			LabelRepresentation.commentForTransition+"A(0)-a->B(1)"+ENDL+
			LabelRepresentation.assertString+"(implies"+ENDL+
			LabelRepresentation.commentForNewSeq+"[]"+ENDL+
			"(and"+ENDL+
			"initCond_0"+ENDL+
			")"+ENDL+
			"\t(and somePrecondA_0 somePostcondA_1)))"+ENDL+
			LabelRepresentation.commentForTransition+"E(7)-c->F(8)"+ENDL+
			LabelRepresentation.assertString+"(implies"+ENDL+
			LabelRepresentation.commentForNewSeq+"[a, b]"+ENDL+
			"(and"+ENDL+
			"initCond_5"+ENDL+
			LabelRepresentation.commentForLabel+"a"+ENDL+
			"somePrecondA_5"+ENDL+
			"somePostcondA_6"+ENDL+
			LabelRepresentation.commentForLabel+"b"+ENDL+
			")"+ENDL+
			"\t(and somePrecondC_7 somePostcondC_8)))"+ENDL+
			LabelRepresentation.commentForTransition+"B(10)-a->C(11)"+ENDL+
			LabelRepresentation.assertString+"(implies"+ENDL+
			LabelRepresentation.commentForNewSeq+"[a]"+ENDL+
			"(and"+ENDL+
			"initCond_9"+ENDL+
			LabelRepresentation.commentForLabel+"a"+ENDL+
			"somePrecondA_9"+ENDL+
			"somePostcondA_10"+ENDL+
			")"+ENDL+
			"\t(and somePrecondA_10 somePostcondA_11)))"+ENDL+
			LabelRepresentation.commentForTransition+"C(14)-a-#D(15)"+ENDL+
			LabelRepresentation.assertString+"(implies"+ENDL+
			LabelRepresentation.commentForNewSeq+"[a, a]"+ENDL+
			"(and"+ENDL+
			"initCond_12"+ENDL+
			LabelRepresentation.commentForLabel+"a"+ENDL+
			"somePrecondA_12"+ENDL+
			"somePostcondA_13"+ENDL+
			LabelRepresentation.commentForLabel+"a"+ENDL+
			"somePrecondA_13"+ENDL+
			"somePostcondA_14"+ENDL+
			")"+ENDL+
			"\t(not somePrecondA_14)))"+ENDL+
			";; END OF PATH AXIOMS"+ENDL
				,axioms);
	}

	/** Tests that Augment and mapVerticesToAbstractStates give the same results. */
	@Test
	public final void testCreateIDToStateMap1()
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a-#D\nB-b->E", "createLemmas1"),config);
		LabelRepresentation lblsMap = new LabelRepresentation();
		lblsMap.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclP_N");
		lblsMap.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclQ_N");
		lblsMap.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " initCond_N");
		lblsMap.parseLabel("a"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA_N");
		lblsMap.parseLabel("a"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA_N");
		lblsMap.parseLabel("b"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB_N");
		lblsMap.parseLabel("b"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB_N");
		lblsMap.mapVerticesToAbstractStates(graph);
		Assert.assertEquals(5,lblsMap.idToState.size());

		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclP_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclQ_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " initCond_N");
		lbls.parseLabel("a"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA_N");
		lbls.parseLabel("a"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA_N");
		lbls.parseLabel("b"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB_N");
		lbls.parseLabel("b"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB_N");

		lbls.AugmentAbstractStates(null,Arrays.asList(new String[]{}), graph,true);// initial state
		Assert.assertEquals(1,lbls.idToState.size());
		Assert.assertTrue(lbls.idToState.containsKey(VertexID.parseID("A")));
		AbstractState stateInit = lbls.idToState.get(VertexID.parseID("A"));
		
		lbls.AugmentAbstractStates(null,Arrays.asList(new String[]{}), graph,true);// and again
		Assert.assertEquals(1,lbls.idToState.size());
		Assert.assertTrue(lbls.idToState.containsKey(VertexID.parseID("A")));
		Assert.assertSame(stateInit,lbls.idToState.get(VertexID.parseID("A")));

		lbls.AugmentAbstractStates(null,Arrays.asList(new String[]{"a","b"}), graph,true);
		Assert.assertEquals(3,lbls.idToState.size());
		Assert.assertTrue(lbls.idToState.containsKey(VertexID.parseID("A")));

		lbls.AugmentAbstractStates(null,Arrays.asList(new String[]{"a","a","a"}), graph,true);
		Assert.assertEquals(lblsMap.idToState.size(), lbls.idToState.size());
		Assert.assertEquals(lblsMap.idToState, lbls.idToState);
	}
	
	private LabelRepresentation simpleLabel()
	{
		final LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " ( define m_N::nat )");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N 0)");
		lbls.parseLabel("add"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (+ m_M 1))");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.PRE+ " (> m_N 0)");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (- m_M 1))");
		return lbls;
	}
	
	/** Initial accept-state should have a satisfiable abstract state. */
	@Test
	public final void testAugmentFailure1()
	{
		final LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " ( define m_N::nat )");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " (and (= m_N 0) (= m_N 1))");
		lbls.parseLabel("add"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (+ m_M 1))");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.PRE+ " (> m_N 0)");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (- m_M 1))");
		final LearnerGraph graph = new LearnerGraph(config);
		Smt.loadLibrary();Smt.closeStdOut();final Smt smt = new Smt();
		final List<String> sequence = Arrays.asList(new String[]{});

		Helper.checkForCorrectException(new whatToRun() { public void run() {
			lbls.AugmentAbstractStates(smt,sequence, graph,true);// initial state
		}},IllegalArgumentException.class,"unsatisfiable");
	}

	/** Initial reject-state may not have a satisfiable abstract state. */
	@Test
	public final void testAugmentFailure2()
	{
		final LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " ( define m_N::nat )");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " (and (= m_N 0) (= m_N 1))");
		lbls.parseLabel("add"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (+ m_M 1))");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.PRE+ " (> m_N 0)");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (- m_M 1))");
		final LearnerGraph graph = new LearnerGraph(config);
		Smt.loadLibrary();Smt.closeStdOut();final Smt smt = new Smt();
		final List<String> sequence = Arrays.asList(new String[]{});
		
		lbls.AugmentAbstractStates(smt,sequence, graph,false);// initial state
	}

	/** Augmenting reject-paths. */
	@Test
	public final void testAugmentFailure3()
	{
		final LabelRepresentation lbls = simpleLabel();
		final LearnerGraph graph = new LearnerGraph(config);
		Smt.loadLibrary();Smt.closeStdOut();final Smt smt = new Smt();

		final List<String> sequence = Arrays.asList(new String[]{"remove"});
		graph.paths.augmentPTA(sequence,false, false, null);
		
		lbls.AugmentAbstractStates(smt,sequence, graph,false);
	}

	/** Augmenting reject-paths. */
	@Test
	public final void testAugmentFailure4()
	{
		final LabelRepresentation lbls = simpleLabel();
		final LearnerGraph graph = new LearnerGraph(config);
		Smt.loadLibrary();Smt.closeStdOut();final Smt smt = new Smt();

		final List<String> sequence = Arrays.asList(new String[]{"remove"});
		
		graph.paths.augmentPTA(sequence,true, false, null);
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			lbls.AugmentAbstractStates(smt,sequence, graph,true);
		}},IllegalArgumentException.class,"unsatisfiable");
	}

	/** Augmenting reject-paths. */
	@Test
	public final void testAugmentFailure5()
	{
		final LabelRepresentation lbls = simpleLabel();
		final LearnerGraph graph = new LearnerGraph(config);
		Smt.loadLibrary();Smt.closeStdOut();final Smt smt = new Smt();

		final List<String> sequence = Arrays.asList(new String[]{"add","remove","remove"});
		
		graph.paths.augmentPTA(sequence,false, false, null);
		lbls.AugmentAbstractStates(smt,sequence, graph,false);
	}

	/** Augmenting reject-paths. */
	@Test
	public final void testAugmentFailure6()
	{
		final LabelRepresentation lbls = simpleLabel();
		final LearnerGraph graph = new LearnerGraph(config);
		Smt.loadLibrary();Smt.closeStdOut();final Smt smt = new Smt();

		final List<String> sequence = Arrays.asList(new String[]{"add","remove","remove"});
		
		graph.paths.augmentPTA(sequence,true, false, null);
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			lbls.AugmentAbstractStates(smt,sequence, graph,true);
		}},IllegalArgumentException.class,"unsatisfiable");
	}

	/** Augmenting accept-paths. */
	@Test
	public final void testAugment7()
	{
		final LabelRepresentation lbls = simpleLabel();
		final LearnerGraph graph = new LearnerGraph(config);
		Smt.loadLibrary();Smt.closeStdOut();final Smt smt = new Smt();

		final List<String> sequence = Arrays.asList(new String[]{"add","remove","add"});
		
		graph.paths.augmentPTA(sequence,true, false, null);
		lbls.AugmentAbstractStates(smt,sequence, graph,true);
	}

	
	@Test
	public final void testCreateIDToStateMap2()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclP_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " varDeclQ_N");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " initCond_N");
		lbls.parseLabel("a"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondA_N");
		lbls.parseLabel("a"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondA_N");
		lbls.parseLabel("b"+" "+LabelRepresentation.XM_DATA.PRE+ " somePrecondB_N");
		lbls.parseLabel("b"+" "+LabelRepresentation.XM_DATA.POST+ " somePostcondB_N");
		lbls.mapVerticesToAbstractStates(new LearnerGraph(FsmParser.buildGraph("A-a->B-a->C-a-#D\nB-b->E", "createLemmas1"),
				config));
		//for(Entry<VertexID,AbstractState> entry:lbls.idToState.entrySet())
		//	System.out.println(entry.getKey()+" "+entry.getValue().abstractState);
		Assert.assertEquals(5,lbls.idToState.size());
		
		Assert.assertEquals(
				LabelRepresentation.commentForNewSeq+"[]"+ENDL+
				"(and"+ENDL+
				"initCond_0"+ENDL+
				")"+ENDL,
				lbls.idToState.get(VertexID.parseID("A")).abstractState);
		Assert.assertEquals(
				LabelRepresentation.commentForNewSeq+"[a]"+ENDL+
				"(and"+ENDL+
				"initCond_1"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_1"+ENDL+
				"somePostcondA_2"+ENDL+
				")"+ENDL,
				lbls.idToState.get(VertexID.parseID("B")).abstractState);
		Assert.assertEquals(
				LabelRepresentation.commentForNewSeq+"[a, a]"+ENDL+
				"(and"+ENDL+
				"initCond_6"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_6"+ENDL+
				"somePostcondA_7"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_7"+ENDL+
				"somePostcondA_8"+ENDL+
				")"+ENDL,
				lbls.idToState.get(VertexID.parseID("C")).abstractState);
		Assert.assertEquals(
				LabelRepresentation.commentForNewSeq+"[a, a, a]"+ENDL+
				"(and"+ENDL+
				"initCond_9"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_9"+ENDL+
				"somePostcondA_10"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_10"+ENDL+
				"somePostcondA_11"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_11"+ENDL+
				"somePostcondA_12"+ENDL+
				")"+ENDL,
				lbls.idToState.get(VertexID.parseID("D")).abstractState);
		Assert.assertEquals(
				LabelRepresentation.commentForNewSeq+"[a, b]"+ENDL+
				"(and"+ENDL+
				"initCond_3"+ENDL+
				LabelRepresentation.commentForLabel+"a"+ENDL+
				"somePrecondA_3"+ENDL+
				"somePostcondA_4"+ENDL+
				LabelRepresentation.commentForLabel+"b"+ENDL+
				"somePrecondB_4"+ENDL+
				"somePostcondB_5"+ENDL+
				")"+ENDL,
				lbls.idToState.get(VertexID.parseID("E")).abstractState);
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

		AbstractState state = null;
		smt.pushContext();
		state = lbls.getConjunctionForPath(Arrays.asList(new String[]{"remove"}));
		smt.loadData(LabelRepresentation.getAssertionFromAbstractState(state));
		Assert.assertFalse(smt.check());
		smt.popContext();

		smt.pushContext();
		state = lbls.getConjunctionForPath(Arrays.asList(new String[]{}));
		smt.loadData(LabelRepresentation.getAssertionFromAbstractState(state));
		Assert.assertTrue(smt.check());
		smt.popContext();

		smt.pushContext();
		state = lbls.getConjunctionForPath(Arrays.asList(new String[]{"add","remove"}));
		smt.loadData(LabelRepresentation.getAssertionFromAbstractState(state));
		Assert.assertTrue(smt.check());
		smt.popContext();


		smt.pushContext();
		state = lbls.getConjunctionForPath(Arrays.asList(new String[]{"add","remove","remove"}));
		smt.loadData(LabelRepresentation.getAssertionFromAbstractState(state));
		Assert.assertFalse(smt.check());
		smt.popContext();

		Smt.reopenStdOut();
	}
	
	/** Checks that it is possible to check that all states can be entered. */
	@Test
	public final void testAbstractStateSatisfiability1()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " ( define m_N::nat )");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N 0)");
		lbls.parseLabel("add"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (+ m_M 1))");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.PRE+ " (> m_N 0)");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (- m_M 1))");
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-add->B-add->C-add->D\nB-remove->E-add->F","testUpdateScore"), config);
		lbls.mapVerticesToAbstractStates(graph);
		
		Smt.loadLibrary();
		Smt.closeStdOut();
		Smt smt = new Smt();
		lbls.checkAllStatesExist(smt);
	}
	
	/** Checks that it is possible to check that all states can be entered. */
	@Test
	public final void testAbstractStateSatisfiability2()
	{
		final LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " ( define m_N::nat )");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N 0)");
		lbls.parseLabel("add"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (+ m_M 1))");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.PRE+ " (> m_N 0)");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (- m_M 1))");
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-add->B\nA-remove->S","testAbstractStateSatisfiability2"), config);
		lbls.mapVerticesToAbstractStates(graph);
		
		Smt.loadLibrary();
		Smt.closeStdOut();
		final Smt smt = new Smt();
		Helper.checkForCorrectException(new whatToRun() { public void run() {
			lbls.checkAllStatesExist(smt);
		}},IllegalArgumentException.class,"has an unsatisfiable abstract state");
	}
	
	/** Checks that we can use abstract states to compute state compatibility. */
	@Test
	public final void testUpdateScore()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " ( define m_N::nat )");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N 0)");
		lbls.parseLabel("add"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (+ m_M 1))");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.PRE+ " (> m_N 0)");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (- m_M 1))");
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-add->B-add->C-add->D\nB-remove->E-add->F","testUpdateScore"), config);
		lbls.mapVerticesToAbstractStates(graph);
		
		Smt.loadLibrary();
		Smt.closeStdOut();
		Smt smt = new Smt();
		
		lbls.checkAllStatesExist(smt);
		Assert.assertTrue(lbls.abstractStatesCompatible(smt, VertexID.parseID("A"), VertexID.parseID("A")));
		Assert.assertFalse(lbls.abstractStatesCompatible(smt, VertexID.parseID("A"), VertexID.parseID("B")));
		Assert.assertFalse(lbls.abstractStatesCompatible(smt, VertexID.parseID("B"), VertexID.parseID("A")));
		
		Assert.assertTrue(lbls.abstractStatesCompatible(smt, VertexID.parseID("A"), VertexID.parseID("E")));
		Assert.assertTrue(lbls.abstractStatesCompatible(smt, VertexID.parseID("B"), VertexID.parseID("F")));
	}

	@Test
	public final void testCheckWithEndUser()
	{
		LabelRepresentation lbls = new LabelRepresentation();
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.PRE+ " ( define m_N::nat )");
		lbls.parseLabel(INITMEM+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N 0)");
		lbls.parseLabel("add"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (+ m_M 1))");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.PRE+ " (> m_N 0)");
		lbls.parseLabel("remove"+" "+LabelRepresentation.XM_DATA.POST+ " (= m_N (- m_M 1))");
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-add->B","testUpdateScore"), config);
		lbls.mapVerticesToAbstractStates(graph);
		
		Smt.loadLibrary();
		Smt.closeStdOut();
		Smt smt = new Smt();
		
		Assert.assertEquals(AbstractOracle.USER_ACCEPTED,lbls.CheckWithEndUser(smt, graph, Arrays.asList(new String[]{})));
		Assert.assertEquals(0,lbls.CheckWithEndUser(smt, graph, Arrays.asList(new String[]{"remove"})));
		Assert.assertEquals(AbstractOracle.USER_ACCEPTED,lbls.CheckWithEndUser(smt, graph, Arrays.asList(new String[]{"add"})));
		Assert.assertEquals(AbstractOracle.USER_ACCEPTED,lbls.CheckWithEndUser(smt, graph, Arrays.asList(new String[]{"add","remove"})));
		Assert.assertEquals(2,lbls.CheckWithEndUser(smt, graph, Arrays.asList(new String[]{"add","remove","remove"})));
		Assert.assertEquals(AbstractOracle.USER_ACCEPTED,lbls.CheckWithEndUser(smt, graph, Arrays.asList(new String[]{"add","remove","add","add"})));
	}		
}
