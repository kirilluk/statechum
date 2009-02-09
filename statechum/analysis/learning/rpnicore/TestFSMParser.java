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

import static org.junit.Assert.assertEquals;


import junit.framework.AssertionFailedError;

import org.junit.Test;

import statechum.JUConstants;
import statechum.JUConstants.PAIRCOMPATIBILITY;

/** Tests the low-level parser for graphs. High-level functionality is tested
 * using <em>TestGraphConstruction</em>
 * 
 * @author kirill
 *
 */
public class TestFSMParser {
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
		
		public void pairCompatibility(String stateA, JUConstants.PAIRCOMPATIBILITY compat, String stateB)
		{
			assertEquals("wrong A string "+stateA,elements[i++],stateA);
			assertEquals("wrong compatibility tag",elements[i++],compat.name());
			assertEquals("wrong B string "+stateB,elements[i++],stateB);
			++i;// ignore the last one
		}
		
		public void match()
		{
			try
			{
				FsmParser p = new FsmParser(text);
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
		new bufferMatcher(" A-b->C1<-d0-P----a->C | A- b ->B-a->U",
			new String [] {
				"A", "C1", "b",	 "ACCEPT",
				"P", "C1", "d0", "ACCEPT",
				"P", "C", "a",	 "ACCEPT",
				"A", "B", "b",	 "ACCEPT",
				"B", "U", "a",	 "ACCEPT",
			}).match();
	}

	@Test
	public void testFsmParse3() {
		new bufferMatcher(" \n \n",
			new String [] {
			}).match();
	}

	@Test
	public void testFsmParse4() {
		new bufferMatcher(" \n | | \n",
			new String [] {
			}).match();
	}

	@Test
	public void testFsmParse5() {
		new bufferMatcher("",
			new String [] {
			}).match();
	}

	@Test
	public void testFsmParse6() {
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
	public void testFsmParse7() {
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
	public void testFsmParse8() {
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
	public void testFsmParse9() {
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
	public void testFsmParse10() {
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
	public void testFsmParse11() {
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
	public void testFsmParse12() {
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
	public void testFsmParse13() {
		new bufferMatcher("P-c->P<-b-Q_State<-a-P",
			new String [] {
				"P", "P", "c", "ACCEPT",
				"Q_State", "P", "b",	 "ACCEPT",
				"P", "Q_State", "a",	 	"ACCEPT"
			}).match();
	}
		
	@Test
	public void testFsmParse_compatibility1() {
		new bufferMatcher(" A-b->C1<-d0-P----a->C | A- b ->B-a->U | A== MERGED == B = INCOMPATIBLE == C",
			new String [] {
				"A", "C1", "b",	 "ACCEPT",
				"P", "C1", "d0", "ACCEPT",
				"P", "C", "a",	 "ACCEPT",
				"A", "B", "b",	 "ACCEPT",
				"B", "U", "a",	 "ACCEPT",
				"A", JUConstants.PAIRCOMPATIBILITY.MERGED.name(), "B","",
				"B", JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE.name(), "C",""
			}).match();
	}

	@Test
	public void testFsmParse_compatibility2() {
		new bufferMatcher(" A-b->C1<-d0-P----a->C | A== MERGED == B | A- b ->B-a->U | A== MERGED == B = INCOMPATIBLE == C",
			new String [] {
				"A", "C1", "b",	 "ACCEPT",
				"P", "C1", "d0", "ACCEPT",
				"P", "C", "a",	 "ACCEPT",
				"A", JUConstants.PAIRCOMPATIBILITY.MERGED.name(), "B","",
				"A", "B", "b",	 "ACCEPT",
				"B", "U", "a",	 "ACCEPT",
				"A", JUConstants.PAIRCOMPATIBILITY.MERGED.name(), "B","",
				"B", JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE.name(), "C",""
			}).match();
	}

	protected static void checkEx(final String whatToParse, String exceptionSubString)
	{
		statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() { public void run() {
			new FsmParser(whatToParse).parse(new TransitionReceiver()
			{
				public void accept(@SuppressWarnings("unused") String from, 
						@SuppressWarnings("unused")	String to, 
						@SuppressWarnings("unused")	String label) 
				{
					// do nothing at all
				}
				public void reject(@SuppressWarnings("unused") String from, 
						@SuppressWarnings("unused")	String to, 
						@SuppressWarnings("unused")	String label) 
				{
					// do nothing at all
				}
				public void pairCompatibility(@SuppressWarnings("unused") String stateA, 
						@SuppressWarnings("unused") PAIRCOMPATIBILITY pairRelation, @SuppressWarnings("unused") String stateB) 
				{
					// do nothing at all
				}
			});
		}},IllegalArgumentException.class,exceptionSubString);
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
		checkEx("A - -","label expected");
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
		checkEx("A <- b - C - -","label expected");
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

	@Test 
	public void testFsmParseFail_compat1()
	{
		checkEx("A = b","lex");
	}

	@Test 
	public void testFsmParseFail_compat2()
	{
		checkEx("A = MERGED ==","lex");
	}

	@Test 
	public void testFsmParseFail_compat3()
	{
		checkEx("A = INCOMPATIBLE == B ==","lex");
	}

	@Test 
	public void testFsmParseFail_compat4()
	{
		checkEx("A - b ==B","right");
	}

	@Test 
	public void testFsmParseFail_compat5()
	{
		checkEx("A = b --B","equiv on the left");
	}

	@Test 
	public void testFsmParseFail_compat6()
	{
		checkEx("A <- b ==B","dash was");
	}

	@Test 
	public void testFsmParseFail_compat7()
	{
		checkEx("A = b ->B","equiv on the left");
	}

	@Test 
	public void testFsmParseFail_compat8()
	{
		checkEx("A = b -#B","equiv on the left");
	}

	@Test 
	public void testFsmParseFail_compat9()
	{
		checkEx("A = b ==B","No enum");
	}



}
