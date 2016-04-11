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

import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.Label;
import statechum.Pair;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

public class TestLoadAnswers {
	
	protected final Configuration config = Configuration.getDefaultConfiguration();
	protected final ConvertALabel converter = null;
	
	/** This method passes the right arguments to the object under test.
	*/
	private List<Label> arrayToLabels(String [] question)
	{
		return AbstractLearnerGraph.buildList(Arrays.asList(question), config,converter);
	}
	
	@Test
	public void testLoadAnswers1()
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		Assert.assertEquals(0,sa.getCount());
	}
	
	@Test
	public void testLoadAnswers2() throws IOException
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader(""));
		Assert.assertEquals(0,sa.getCount());
	}
	
	@Test
	public void testLoadAnswers3a() throws IOException
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader(""));
		Assert.assertEquals(null,sa.getAnswer(Arrays.asList(new Label[]{})));
	}

	/** If the answer is not user-provided, ignore it. */
	@Test
	public void testLoadAnswers3b() throws IOException
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader(RPNILearner.QUESTION_SPIN+" even more junk"));
		Assert.assertEquals(null,sa.getAnswer(Arrays.asList(new Label[]{})));
	}

	/** If the answer is not user-provided, ignore it. */
	@Test
	public void testLoadAnswers3c() throws IOException
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader("junk"+RPNILearner.QUESTION_USER+" even more junk"));
		Assert.assertEquals(null,sa.getAnswer(Arrays.asList(new Label[]{})));
	}

	/** If the answer is not user-provided, ignore it. */
	@Test
	public void testLoadAnswers3d() throws IOException
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader("junk"+RPNILearner.QUESTION_USER+" even more\n junk"));
		Assert.assertEquals(null,sa.getAnswer(Arrays.asList(new Label[]{})));
	}

	@Test
	public void testLoadAnswers4() throws IOException
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader(""+RPNILearner.QUESTION_USER+"[test] <yes>"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_ACCEPTED,null), 
				sa.getAnswer(AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"test"}),config,converter)));
	}
	
	@Test
	public void testLoadAnswers5A() throws IOException
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader(" \t\t  "+RPNILearner.QUESTION_USER+"  [test] <no> at position 5, junk"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(new Pair<Integer,String>(5,null), sa.getAnswer(arrayToLabels(new String[]{"test"})));
	}

	@Test
	public void testLoadAnswers5B() throws IOException
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader("\n\n"+RPNILearner.QUESTION_USER+"[test] <no> at position 5, junk\n"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(new Pair<Integer,String>(5,null), sa.getAnswer(arrayToLabels(new String[]{"test"})));
	}

	@Test
	public void testLoadAnswers5C() throws IOException
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader("  "+RPNILearner.QUESTION_AUTO+" [test] <no> at position 5, junk"));
		Assert.assertEquals(0,sa.getCount());
	}
	
	@Test
	public void testLoadAnswers5D() throws IOException
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader("  "+RPNILearner.QUESTION_USER+" [test] <ltl> some ltl formula"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_LTL,"some ltl formula"), sa.getAnswer(arrayToLabels(new String[]{"test"})));
	}

	@Test
	public void testLoadAnswers5E() throws IOException
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader("  "+RPNILearner.QUESTION_AUTO+" [test] <ltl> some ltl, formula"));
		Assert.assertEquals(0,sa.getCount());
	}


	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail1() throws IOException
	{
		new StoredAnswers(config,null).setAnswers(new StringReader(RPNILearner.QUESTION_USER+"junk"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail2() throws IOException
	{
		new StoredAnswers(config,null).setAnswers(new StringReader(RPNILearner.QUESTION_USER+"[valid string] junk"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail3a() throws IOException
	{
		new StoredAnswers(config,null).setAnswers(new StringReader(RPNILearner.QUESTION_USER+"[valid string] <no>"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail3b() throws IOException
	{
		new StoredAnswers(config,null).setAnswers(new StringReader(RPNILearner.QUESTION_USER+"[valid string] <ltl>"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail3c() throws IOException
	{
		new StoredAnswers(config,null).setAnswers(new StringReader(RPNILearner.QUESTION_USER+"[valid string] <ltl>    "));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail4() throws IOException
	{
		new StoredAnswers(config,null).setAnswers(new StringReader(RPNILearner.QUESTION_USER+"[valid string] <no> at position "));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail5() throws IOException
	{
		new StoredAnswers(config,null).setAnswers(new StringReader(RPNILearner.QUESTION_USER+"[valid string] <no> at position 6"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail6() throws IOException
	{
		new StoredAnswers(config,null).setAnswers(new StringReader(RPNILearner.QUESTION_USER+"[valid string] <no> at position 7,\n"+RPNILearner.QUESTION_USER+"junk"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail7() throws IOException
	{
		new StoredAnswers(config,null).setAnswers(new StringReader(RPNILearner.QUESTION_USER+" junk\n\n[valid string] <no> at position 7,\n"));
	}
	
	@Test
	public void testLoadAnswersIgnore1() throws IOException 
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader("\n\n"+RPNILearner.QUESTION_USER+" [test] "+RPNILearner.QUESTION_IGNORE+"\n"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_IGNORED,null), sa.getAnswer(arrayToLabels(new String[]{"test"})));
	}
	
	@Test
	public void testLoadAnswersIgnore2() throws IOException 
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader("\n\n"+RPNILearner.QUESTION_USER+" [testA, testB] "+RPNILearner.QUESTION_IGNORE+"   \n"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_IGNORED,null), sa.getAnswer(arrayToLabels(new String[]{"testA","testB"})));
		Assert.assertNull(sa.getAnswer(arrayToLabels(new String[]{"testA"})));
	}
	
	/** Junk after the trace to be ignored. */
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersIgnoreFailure1() throws IOException 
	{
		new StoredAnswers(config,null).setAnswers(new StringReader("\n\n"+RPNILearner.QUESTION_USER+" [test] <ignore> at position 5, junk\n"));
	}
	
	@Test
	public void testLoadAnswersIncompatible1() throws IOException 
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader("\n\n"+RPNILearner.QUESTION_USER+" [testA, testB] "+RPNILearner.QUESTION_INCOMPATIBLE+" nodeA nodeB  \n"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_INCOMPATIBLE,"nodeA nodeB"), sa.getAnswer(arrayToLabels(new String[]{"testA","testB"})));
		Assert.assertNull(sa.getAnswer(arrayToLabels(new String[]{"testA"})));
	}
	
	/** Missing text of the pair. */
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersIncompatibleFail1() throws IOException 
	{
		new StoredAnswers(config,null).setAnswers(new StringReader("\n\n"+RPNILearner.QUESTION_USER+" [test]"+RPNILearner.QUESTION_INCOMPATIBLE+"\n"));
	}

	@Test
	public void testLoadAnswersNewTrace1() throws IOException 
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader("\n\n"+RPNILearner.QUESTION_USER+" [testA, testB] "+RPNILearner.QUESTION_NEWTRACE+"   + elemA elemB  \n"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_NEWTRACE,"+ elemA elemB"), sa.getAnswer(arrayToLabels(new String[]{"testA","testB"})));
		Assert.assertNull(sa.getAnswer(arrayToLabels(new String[]{"testA"})));
	}
	
	@Test
	public void testLoadAnswersNewTrace2() throws IOException 
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader("\n\n"+RPNILearner.QUESTION_USER+" [testA, testB] "+RPNILearner.QUESTION_NEWTRACE+"   + elemA elemB // elemC elemA / \n"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals("+ elemA elemB // elemC elemA /",sa.getAnswer(arrayToLabels(new String[]{"testA","testB"})).secondElem);
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_NEWTRACE,"+ elemA elemB // elemC elemA /"), sa.getAnswer(arrayToLabels(new String[]{"testA","testB"})));
		Assert.assertNull(sa.getAnswer(arrayToLabels(new String[]{"testA"})));
	}
	
	/** Missing text of the trace. */
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersNewTraceFail1() throws IOException 
	{
		new StoredAnswers(config,null).setAnswers(new StringReader("\n\n"+RPNILearner.QUESTION_USER+" [test]"+RPNILearner.QUESTION_NEWTRACE+"\n"));
	}
	
	@Test
	public void testLoadAnswers6() throws IOException
	{
		StoredAnswers sa = new StoredAnswers(config,null);
		sa.setAnswers(new StringReader(""+RPNILearner.QUESTION_USER+"[test] <no> at position 5, junk\n "
				+RPNILearner.QUESTION_USER+" [some_text, more_of_it] <yes> whatever\n\n\n"
				+""+RPNILearner.QUESTION_USER+"[teststr, another, more] <no> at position 0, junk\n"				
				+""+RPNILearner.QUESTION_USER+" [ difficult_one] <ltl> some ltl 1\n"
				+RPNILearner.QUESTION_USER+"[teststr, a, more] <no> at position 2, junk\n"
				+RPNILearner.QUESTION_AUTO+" this junk should be ignored\n"
				+RPNILearner.QUESTION_SPIN+" this junk should be ignored\n"
				+""+RPNILearner.QUESTION_USER+"[teststr, p, more] <yes> junk\n"
				+""+RPNILearner.QUESTION_USER+" [ difficult_second_one] <ltl> some ltl 2\n"

				+""+RPNILearner.QUESTION_USER+" [some, trace] "+RPNILearner.QUESTION_INCOMPATIBLE+" elem1 elem2\n"
				+""+RPNILearner.QUESTION_USER+" [trace, A] "+RPNILearner.QUESTION_IGNORE+"\n"
				+""+RPNILearner.QUESTION_USER+" [trace, B] "+RPNILearner.QUESTION_NEWTRACE+" + a b c \n"
		));
		Assert.assertEquals(10,sa.getCount());
		Assert.assertEquals(new Pair<Integer,String>(5,null), sa.getAnswer(arrayToLabels(new String[]{"test"})));
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_ACCEPTED,null), sa.getAnswer(arrayToLabels(new String[]{"some_text","more_of_it"})));
		Assert.assertEquals(new Pair<Integer,String>(0,null), sa.getAnswer(arrayToLabels(new String[]{"teststr","another", "more"})));
		Assert.assertEquals(new Pair<Integer,String>(2,null), sa.getAnswer(arrayToLabels(new String[]{"teststr","a", "more"})));
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_ACCEPTED,null), sa.getAnswer(arrayToLabels(new String[]{"teststr","p", "more"})));
		Assert.assertEquals(null, sa.getAnswer(arrayToLabels(new String[]{"unknown","p", "more"})));
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_LTL,"some ltl 1"), sa.getAnswer(arrayToLabels(new String[]{"difficult_one"})));
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_LTL,"some ltl 2"), sa.getAnswer(arrayToLabels(new String[]{"difficult_second_one"})));
		
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_INCOMPATIBLE,"elem1 elem2"), sa.getAnswer(arrayToLabels(new String[]{"some","trace"})));		
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_IGNORED,null), sa.getAnswer(arrayToLabels(new String[]{"trace","A"})));		
		Assert.assertEquals(new Pair<Integer,String>(AbstractOracle.USER_NEWTRACE,"+ a b c"), sa.getAnswer(arrayToLabels(new String[]{"trace","B"})));		
	}
}
