package statechum.analysis.learning;

import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;

import junit.framework.JUnit4TestAdapter;

import org.junit.Assert;
import org.junit.Test;

import statechum.analysis.learning.oracles.*;
import statechum.xmachine.model.testset.TestWMethod;

public class TestLoadAnswers {
	@Test
	public void testLoadAnswers1() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		Assert.assertEquals(0,sa.getCount());
	}
	
	@Test
	public void testLoadAnswers2() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader(""));
		Assert.assertEquals(0,sa.getCount());
	}
	
	@Test
	public void testLoadAnswers3() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader(""));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_CANCELLED,sa.getAnswer(Arrays.asList(new String[]{})));
	}

	@Test
	public void testLoadAnswers4() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("[test] <yes>"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(RPNIBlueFringeLearner.USER_ACCEPTED, sa.getAnswer(Arrays.asList(new String[]{"test"})));
	}
	
	@Test
	public void testLoadAnswers5A() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader(" \t\t    [test] <no> at position 5, junk"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays.asList(new String[]{"test"})));
	}

	@Test
	public void testLoadAnswers5B() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("\n\n[test] <no> at position 5, junk\n"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays.asList(new String[]{"test"})));
	}

	@Test
	public void testLoadAnswers5C() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("  "+RPNIBlueFringeLearner.QUESTION_AUTO+" [test] <no> at position 5, junk"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays.asList(new String[]{"test"})));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail1() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("junk"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail2() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] junk"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail3() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] <no>"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail4() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] <no> at position "));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail5() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] <no> at position 6"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail6() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] <no> at position 7,\njunk"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail7() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader(" junk\n\n[valid string] <no> at position 7,\n"));
	}
	
	
	@Test
	public void testLoadAnswers6() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("[test] <no> at position 5, junk\n "
				+RPNIBlueFringeLearner.QUESTION_AUTO+" [some text, more of it] <yes> whatever\n\n\n"
				+"[teststr, another, more] <no> at position 0, junk\n"				
				+RPNIBlueFringeLearner.QUESTION_AUTO+"[teststr, a, more] <no> at position 2, junk\n"				
				+"[teststr, p, more] <yes> junk\n"				
		));
		Assert.assertEquals(5,sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays.asList(new String[]{"test"})));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_ACCEPTED, sa.getAnswer(Arrays.asList(new String[]{"some text","more of it"})));
		Assert.assertEquals(0, sa.getAnswer(Arrays.asList(new String[]{"teststr","another", "more"})));
		Assert.assertEquals(2, sa.getAnswer(Arrays.asList(new String[]{"teststr","a", "more"})));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_ACCEPTED, sa.getAnswer(Arrays.asList(new String[]{"teststr","p", "more"})));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_CANCELLED, sa.getAnswer(Arrays.asList(new String[]{"unknown","p", "more"})));
	}

	
	/** In order to be able to use old junit runner. */
	public static junit.framework.Test suite()
	{
		return new JUnit4TestAdapter(TestLoadAnswers.class);
	}
}
