/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

statechum is free software: you can redistribute it and/or modify
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
package statechum.analysis.learning.observers;

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;

import java.io.StringReader;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.Pair;
import statechum.Configuration.IDMode;
import statechum.analysis.learning.RPNILearner;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.rpnicore.LearnerGraph;

/** Tests that AutoAnswers works.
 * 
 * @author kirill
 *
 */
public class TestAutoAnswers {

	private String partA = 				
		RPNILearner.QUESTION_USER+"[c, a, c] <no> at position 2, element c\n"+
		RPNILearner.QUESTION_USER+"[c, a, b, p, a] <no> at position 2, element b\n"+
		RPNILearner.QUESTION_USER+"[c, b, b] <no> at position 2, element b\n"+
		RPNILearner.QUESTION_USER+"[c, b, p, e] <yes>\n"+
		RPNILearner.QUESTION_USER+"[c, a, p, a] <no> at position 2, element p\n"+
		RPNILearner.QUESTION_USER+"[c, p, a] <no> at position 1, element p\n"+
		RPNILearner.QUESTION_USER+"[p, a] <no> at position 0, element p\n"+
		RPNILearner.QUESTION_USER+"[e, c, b, p] <yes>\n"+
		RPNILearner.QUESTION_USER+"[e, c, e, c] <yes>\n"+
		RPNILearner.QUESTION_USER+"[e, a] <no> at position 1, element a\n"+
		RPNILearner.QUESTION_USER+"[e, p] <no> at position 1, element p\n",
		partB = 
			RPNILearner.QUESTION_USER+"[e] <yes>\n"+
			RPNILearner.QUESTION_USER+"[e, p, a] <no> at position 1, element p\n"+
			RPNILearner.QUESTION_USER+"[e, a, a] <no> at position 1, element a\n"+
			RPNILearner.QUESTION_USER+"[e, c, a, a] <yes>\n"+
			RPNILearner.QUESTION_USER+"[e, c, b, c] <no> at position 3, element c\n"+
			RPNILearner.QUESTION_USER+"[c, e, c] <yes>\n"+
			RPNILearner.QUESTION_USER+"[e, c] <yes>\n"+
			RPNILearner.QUESTION_USER+"[e, c, a, c] <no> at position 3, element c\n"+
			RPNILearner.QUESTION_USER+"[e, c, a, p] <no> at position 3, element p\n"+
			RPNILearner.QUESTION_USER+"[e, c, a, b] <no> at position 3, element b\n",

	partC = 
		RPNILearner.QUESTION_USER+"[e, c, a, e, c] <yes>\n"+
		RPNILearner.QUESTION_USER+"[c, b, c] <no> at position 2, element c\n"+
		RPNILearner.QUESTION_USER+"[e, c, c] <no> at position 2, element c\n"+
		RPNILearner.QUESTION_USER+"[e, c, p] <no> at position 2, element p\n"+
		RPNILearner.QUESTION_USER+"[e, c, b, b] <no> at position 3, element b\n"+
		RPNILearner.QUESTION_USER+"[e, c, b, p, a] <no> at position 4, element a\n"+
		RPNILearner.QUESTION_USER+"[e, c, b, p, e] <yes>\n"+
		RPNILearner.QUESTION_USER+"[e, c, b, p, e, e] <yes>\n"+
		RPNILearner.QUESTION_USER+"[e, b] <no> at position 1, element b\n"+
		RPNILearner.QUESTION_USER+"[e, e] <yes>\n";	
	
	// The machine I'm talking of is the following:
	// A-e->A-c->B-b->C-p->G-e->A\nB-a->D-a->E-a->D\nE-b->F-p->G\n
	// B-e->A\nC-e->A\nD-e->A\nE-e->A\nF-e->A\nG-e->A\n
	
	@Test
	public void testAuto0()
	{
		//Visualiser.updateFrame(new LearnerGraph(TestFSMAlgo.buildGraph("A-e->A-c->B-b->C-p->G-e->A\nB-a->D-a->E-a->D\nE-b->F-p->G\n"+
		//"B-e->A\nC-e->A\nD-e->A\nE-e->A\nF-e->A\nG-e->A\n","testAutoAnswers0"),Configuration.getDefaultConfiguration()),null);
		Configuration testConfig = Configuration.getDefaultConfiguration().copy();
		testConfig.setGdFailOnDuplicateNames(false);
		testConfig.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);

		RPNILearner learner = new RPNIUniversalLearner(null,null,testConfig)
		{
			@Override
			public Pair<Integer,String> CheckWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					List<String> question, 
					@SuppressWarnings("unused")	final Object [] moreOptions)
			{
				Assert.fail("all answers should have been provided by AutoAnswers, but got "+question);
				return null;
			}
		};

		AutoAnswers ans = new AutoAnswers(learner);
		ans.loadAnswers(new StringReader(
				partA+partB+partC
		));
		ans.learnMachine(
			buildSet(new String[][]{
				new String[] { "c","b","p","e","e" },
				new String[] { "c","a","a","b","p", "e" },
				new String[] { "c","a","e","c" },
				new String[] { "c","a","a","a","a","b","p" }}),
			buildSet(new String[][]{
				new String[] { "c", "a", "a", "b", "p", "a" },
				new String[] { "c", "b", "p", "a" },
				new String[] { "c", "c" },
				new String[] { "b" },
				new String[] { "a" }
			}));
	}	
	
	@Test
	public void testAuto1()
	{
		Configuration testConfig = Configuration.getDefaultConfiguration().copy();
		testConfig.setGdFailOnDuplicateNames(false);
		testConfig.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);

		final AutoAnswers semiUser = new AutoAnswers(null);
		semiUser.loadAnswers(new StringReader(
				partA
		));
		
		RPNILearner learner = new RPNIUniversalLearner(null,null,testConfig)
		{
			@Override
			public Pair<Integer,String> CheckWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					@SuppressWarnings("unused")	List<String> question, 
					@SuppressWarnings("unused")	final Object [] moreOptions)
			{
				Pair<Integer,String> result = semiUser.ans.getAnswer(question);
				Assert.assertNotNull(result);// if we got here, the answer should be known.
				return result;
			}
		};

		AutoAnswers ans = new AutoAnswers(learner);
		ans.loadAnswers(new StringReader(
				partB+partC
		));
		ans.learnMachine(
			buildSet(new String[][]{
				new String[] { "c","b","p","e","e" },
				new String[] { "c","a","a","b","p", "e" },
				new String[] { "c","a","e","c" },
				new String[] { "c","a","a","a","a","b","p" }}),
			buildSet(new String[][]{
				new String[] { "c", "a", "a", "b", "p", "a" },
				new String[] { "c", "b", "p", "a" },
				new String[] { "c", "c" },
				new String[] { "b" },
				new String[] { "a" }
			}));
	}
	
	@Test
	public void testAuto2()
	{
		Configuration testConfig = Configuration.getDefaultConfiguration().copy();
		testConfig.setGdFailOnDuplicateNames(false);
		testConfig.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);

		RPNILearner learner = new RPNIUniversalLearner(null,null,testConfig)
		{
			@Override
			public Pair<Integer,String> CheckWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					@SuppressWarnings("unused")	List<String> question, 
					@SuppressWarnings("unused")	final Object [] moreOptions)
			{
				Assert.fail("all answers should have been provided by AutoAnswers");
				return null;
			}
		};

		AutoAnswers ans1 = new AutoAnswers(learner);
		ans1.loadAnswers(new StringReader(
				partA+partC
		));
		AutoAnswers ans2 = new AutoAnswers(ans1);
		ans2.loadAnswers(new StringReader(
				partB
		));
		ans2.learnMachine(
			buildSet(new String[][]{
				new String[] { "c","b","p","e","e" },
				new String[] { "c","a","a","b","p", "e" },
				new String[] { "c","a","e","c" },
				new String[] { "c","a","a","a","a","b","p" }}),
			buildSet(new String[][]{
				new String[] { "c", "a", "a", "b", "p", "a" },
				new String[] { "c", "b", "p", "a" },
				new String[] { "c", "c" },
				new String[] { "b" },
				new String[] { "a" }
			}));
	}
}
