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

import junit.framework.JUnit4TestAdapter;

import org.junit.Assert;
import org.junit.Test;

import statechum.analysis.learning.oracles.*;
import statechum.xmachine.model.testset.TestWMethod;

public class TestLoadAnswers {
	@Test
	public void testLoadAnswers1() throws IOException {
		StoredAnswers sa = new StoredAnswers();
		Assert.assertEquals(0, sa.getCount());
	}

	@Test
	public void testLoadAnswers2() throws IOException {
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader(""));
		Assert.assertEquals(0, sa.getCount());
	}

	@Test
	public void testLoadAnswers3() throws IOException {
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader(""));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_CANCELLED, sa
				.getAnswer(Arrays.asList(new String[] {})));
	}

	@Test
	public void testLoadAnswers4() throws IOException {
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("[test] <yes>"));
		Assert.assertEquals(1, sa.getCount());
		Assert.assertEquals(RPNIBlueFringeLearner.USER_ACCEPTED, sa
				.getAnswer(Arrays.asList(new String[] { "test" })));
	}

	@Test
	public void testLoadAnswers5A() throws IOException {
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader(
				" \t\t    [test] <no> at position 5, junk"));
		Assert.assertEquals(1, sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays
				.asList(new String[] { "test" })));
	}

	@Test
	public void testLoadAnswers5B() throws IOException {
		StoredAnswers sa = new StoredAnswers();
		sa
				.setAnswers(new StringReader(
						"\n\n[test] <no> at position 5, junk\n"));
		Assert.assertEquals(1, sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays
				.asList(new String[] { "test" })));
	}

	@Test
	public void testLoadAnswers5C() throws IOException {
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("  "
				+ RPNIBlueFringeLearner.QUESTION_AUTO
				+ " [test] <no> at position 5, junk"));
		Assert.assertEquals(1, sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays
				.asList(new String[] { "test" })));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail1() throws IOException {
		new StoredAnswers().setAnswers(new StringReader("junk"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail2() throws IOException {
		new StoredAnswers().setAnswers(new StringReader("[valid string] junk"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail3() throws IOException {
		new StoredAnswers().setAnswers(new StringReader("[valid string] <no>"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail4() throws IOException {
		new StoredAnswers().setAnswers(new StringReader(
				"[valid string] <no> at position "));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail5() throws IOException {
		new StoredAnswers().setAnswers(new StringReader(
				"[valid string] <no> at position 6"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail6() throws IOException {
		new StoredAnswers().setAnswers(new StringReader(
				"[valid string] <no> at position 7,\njunk"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail7() throws IOException {
		new StoredAnswers().setAnswers(new StringReader(
				" junk\n\n[valid string] <no> at position 7,\n"));
	}

	@Test
	public void testLoadAnswers6() throws IOException {
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("[test] <no> at position 5, junk\n "
				+ RPNIBlueFringeLearner.QUESTION_AUTO
				+ " [some text, more of it] <yes> whatever\n\n\n"
				+ "[teststr, another, more] <no> at position 0, junk\n"
				+ RPNIBlueFringeLearner.QUESTION_AUTO
				+ "[teststr, a, more] <no> at position 2, junk\n"
				+ "[teststr, p, more] <yes> junk\n"));
		Assert.assertEquals(5, sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays
				.asList(new String[] { "test" })));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_ACCEPTED, sa
				.getAnswer(Arrays.asList(new String[] { "some text",
						"more of it" })));
		Assert.assertEquals(0, sa.getAnswer(Arrays.asList(new String[] {
				"teststr", "another", "more" })));
		Assert.assertEquals(2, sa.getAnswer(Arrays.asList(new String[] {
				"teststr", "a", "more" })));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_ACCEPTED, sa
				.getAnswer(Arrays
						.asList(new String[] { "teststr", "p", "more" })));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_CANCELLED, sa
				.getAnswer(Arrays
						.asList(new String[] { "unknown", "p", "more" })));
	}
}
