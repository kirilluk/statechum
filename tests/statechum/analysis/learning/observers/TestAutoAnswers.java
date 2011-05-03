/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
 * 
 * statechum is free software: you can redistribute it and/or modify
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
package statechum.analysis.learning.observers;

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;
import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration.LABELKIND;
import statechum.Label;
import statechum.Configuration;
import statechum.Pair;
import statechum.Configuration.IDMode;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.learning.RPNILearner;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.analysis.learning.PairScore;

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

	@Test
	public void testPrettyPrintTrace0()
	{
		Assert.assertEquals("[]",RPNILearner.questionToString(Arrays.asList(new Label[]{})));
	}
	
	@Test
	public void testPrettyPrintTrace1() throws IOException
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLabelKind(LABELKIND.LABEL_ERLANG);
		File file = new File("ErlangExamples/locker/locker.erl");
		final ErlangModule mod = ErlangModule.loadModule(file);config.setErlangModuleName(mod.getName());
		final String LBL1 = "{call, read}", LBL2 = "{call, lock}";
		final LearnerGraph gr = buildLearnerGraph("A- "+LBL1+" ->B-"+LBL2+"->B", "testConvertToModuleFailure1", config);
		Iterator<Label> lblIter = gr.pathroutines.computeAlphabet().iterator();
		ErlangLabel lbl1 = (ErlangLabel)lblIter.next(),lbl2 = (ErlangLabel)lblIter.next();
		List<Label> trace = Arrays.asList(new Label[]{lbl1,lbl2,lbl2});
		Assert.assertEquals("[{?F(),'call','read'},{?F(),'call','lock'},{?F(),'call','lock'}]",RPNILearner.questionToString(trace));
	}
	

	
	// The machine I'm talking of is the following:
	// A-e->A-c->B-b->C-p->G-e->A\nB-a->D-a->E-a->D\nE-b->F-p->G\n
	// B-e->A\nC-e->A\nD-e->A\nE-e->A\nF-e->A\nG-e->A\n
	
	@Test
	public void testAuto0()
	{
		//Visualiser.updateFrame(new LearnerGraph(FsmParser.buildGraph("A-e->A-c->B-b->C-p->G-e->A\nB-a->D-a->E-a->D\nE-b->F-p->G\n"+
		//"B-e->A\nC-e->A\nD-e->A\nE-e->A\nF-e->A\nG-e->A\n","testAutoAnswers0"),Configuration.getDefaultConfiguration()),null);
		Configuration testConfig = Configuration.getDefaultConfiguration().copy();
		testConfig.setGdFailOnDuplicateNames(false);
		testConfig.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);

		RPNILearner learner = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,testConfig,null,null))
		{
			@Override
			public Pair<Integer,String> CheckWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					List<Label> question, @SuppressWarnings("unused") int responseForNoRestart,
					@SuppressWarnings("unused") List<Boolean> acceptedElements,
					@SuppressWarnings("unused") PairScore pairBeingMerged,
					@SuppressWarnings("unused")	final Object [] moreOptions)
			{
				Assert.fail("all answers should have been provided by AutoAnswers, but got "+question);
				return null;
			}
		};

		AutoAnswers ans = new AutoAnswers(learner);
		ans.loadAnswers(new StringReader(
				partA+partB+partC
		),testConfig);
		ans.learnMachine(
			buildSet(new String[][]{
				new String[] { "c","b","p","e","e" },
				new String[] { "c","a","a","b","p", "e" },
				new String[] { "c","a","e","c" },
				new String[] { "c","a","a","a","a","b","p" }},testConfig),
			buildSet(new String[][]{
				new String[] { "c", "a", "a", "b", "p", "a" },
				new String[] { "c", "b", "p", "a" },
				new String[] { "c", "c" },
				new String[] { "b" },
				new String[] { "a" }
			},testConfig));
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
		),testConfig);
		
		RPNILearner learner = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,testConfig,null,null))
		{
			@Override
			public Pair<Integer,String> CheckWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					List<Label> question, @SuppressWarnings("unused") int responseForNoRestart,
					@SuppressWarnings("unused") List<Boolean> acceptedElements,
					@SuppressWarnings("unused") PairScore pairBeingMerged,
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
		),testConfig);
		ans.learnMachine(
			buildSet(new String[][]{
				new String[] { "c","b","p","e","e" },
				new String[] { "c","a","a","b","p", "e" },
				new String[] { "c","a","e","c" },
				new String[] { "c","a","a","a","a","b","p" }},testConfig),
			buildSet(new String[][]{
				new String[] { "c", "a", "a", "b", "p", "a" },
				new String[] { "c", "b", "p", "a" },
				new String[] { "c", "c" },
				new String[] { "b" },
				new String[] { "a" }
			},testConfig));
	}
	
	@Test
	public void testAuto2()
	{
		Configuration testConfig = Configuration.getDefaultConfiguration().copy();
		testConfig.setGdFailOnDuplicateNames(false);
		testConfig.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);

		RPNILearner learner = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,testConfig,null,null))
		{
			@Override
			public Pair<Integer,String> CheckWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					@SuppressWarnings("unused")	List<Label> question, @SuppressWarnings("unused") int responseForNoRestart,
					@SuppressWarnings("unused") List<Boolean> acceptedElements,
					@SuppressWarnings("unused") PairScore pairBeingMerged,
					@SuppressWarnings("unused")	final Object [] moreOptions)
			{
				Assert.fail("all answers should have been provided by AutoAnswers");
				return null;
			}
		};

		AutoAnswers ans1 = new AutoAnswers(learner);
		ans1.loadAnswers(new StringReader(
				partA+partC
		),testConfig);
		AutoAnswers ans2 = new AutoAnswers(ans1);
		ans2.loadAnswers(new StringReader(
				partB
		),testConfig);
		ans2.learnMachine(
			buildSet(new String[][]{
				new String[] { "c","b","p","e","e" },
				new String[] { "c","a","a","b","p", "e" },
				new String[] { "c","a","e","c" },
				new String[] { "c","a","a","a","a","b","p" }},testConfig),
			buildSet(new String[][]{
				new String[] { "c", "a", "a", "b", "p", "a" },
				new String[] { "c", "b", "p", "a" },
				new String[] { "c", "c" },
				new String[] { "b" },
				new String[] { "a" }
			},testConfig));
	}
	
	@Test
	public void testAuto3()
	{
		Configuration testConfig = Configuration.getDefaultConfiguration().copy();
		testConfig.setGdFailOnDuplicateNames(false);
		testConfig.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);

		RPNILearner learner = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,testConfig,null,null))
		{
			@Override
			public Pair<Integer,String> CheckWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					@SuppressWarnings("unused")	List<Label> question, @SuppressWarnings("unused") int responseForNoRestart,
					@SuppressWarnings("unused") List<Boolean> acceptedElements,
					@SuppressWarnings("unused") PairScore pairBeingMerged,
					@SuppressWarnings("unused")	final Object [] moreOptions)
			{
				Assert.fail("all answers should have been provided by AutoAnswers");
				return null;
			}
		};

		AutoAnswers ans2 = new AutoAnswers(learner);
		ans2.loadAnswers(new StringReader(
				RPNILearner.QUESTION_USER+" [c, a, c] "+RPNILearner.QUESTION_INCOMPATIBLE+" P1002 P1001\n"+
				RPNILearner.QUESTION_USER+" [c, a, p, a] "+RPNILearner.QUESTION_IGNORE+"\n"+
				RPNILearner.QUESTION_USER+" [c, b, a, a] "+RPNILearner.QUESTION_IGNORE+"\n"+
				RPNILearner.QUESTION_USER+" [c, b, a, b] "+RPNILearner.QUESTION_NEWTRACE+" -[ [c, b, a]]\n"+
				RPNILearner.QUESTION_USER+" [p, a] <no> at position 0, element p\n"+
				RPNILearner.QUESTION_USER+" [c, b, e, e] <yes>\n"+
				RPNILearner.QUESTION_USER+" [c, b, p, p] <no> at position 3, element p\n"+
				RPNILearner.QUESTION_USER+" [e, e] <yes>\n"+
				RPNILearner.QUESTION_USER+" [c, b, e, a] <no> at position 3, element a\n"+
				RPNILearner.QUESTION_USER+" [c, b, e, p, a] <no> at position 3, element p\n"+
				RPNILearner.QUESTION_USER+" [e, a] <no> at position 1, element a\n"+
				RPNILearner.QUESTION_USER+" [e, e, a] <no> at position 2, element a\n"+
				RPNILearner.QUESTION_USER+" [e, e, b] <no> at position 2, element b\n"+
				RPNILearner.QUESTION_USER+" [e, e, e] <yes>\n"+
				RPNILearner.QUESTION_USER+" [e, e, p] <no> at position 2, element p\n"+
				RPNILearner.QUESTION_USER+" [e, p, a] <no> at position 1, element p\n"+
				RPNILearner.QUESTION_USER+" [e, b] <no> at position 1, element b\n"+
				RPNILearner.QUESTION_USER+" [e, c, c] <no> at position 2, element c\n"+
				RPNILearner.QUESTION_USER+" [e, c, b, a] <no> at position 3, element a\n"+
				RPNILearner.QUESTION_USER+" [e, c, a, e, c] <yes>\n"+
				RPNILearner.QUESTION_USER+" [e, c, b, e, a] <no> at position 4, element a\n"+
				RPNILearner.QUESTION_USER+" [e, c, b, e, e] <yes>\n"+
				RPNILearner.QUESTION_USER+" [e, c, b, e, p] <no> at position 4, element p\n"+
				RPNILearner.QUESTION_USER+" [e, c, b, p, a] <no> at position 4, element a\n"+
				RPNILearner.QUESTION_USER+" [e, c, b, p, p] <no> at position 4, element p\n"+
				RPNILearner.QUESTION_USER+" [e, c, b, p, e, e] <yes>\n"+
				RPNILearner.QUESTION_USER+" [e, c, a, a, b, p, a] <no> at position 6, element a\n"+
				RPNILearner.QUESTION_USER+" [e, c, a, a, b, p, e] <yes>\n"+
				RPNILearner.QUESTION_USER+" [e, c, a, a, a, a, b, p] <yes>\n"+
				RPNILearner.QUESTION_USER+" [c, a, c] "+RPNILearner.QUESTION_INCOMPATIBLE+" P1002 P1001\n"
		),testConfig);
		ans2.learnMachine(
			buildSet(new String[][]{
				new String[] { "c","b","p","e","e" },
				new String[] { "c","a","a","b","p", "e" },
				new String[] { "c","a","e","c" },
				new String[] { "c","a","a","a","a","b","p" }},testConfig),
			buildSet(new String[][]{
				new String[] { "c", "a", "a", "b", "p", "a" },
				new String[] { "c", "b", "p", "a" },
				new String[] { "c", "c" },
				new String[] { "b" },
				new String[] { "a" }
			},testConfig));
	}
	
	@Test
	public void testAuto4()
	{
		Configuration testConfig = Configuration.getDefaultConfiguration().copy();testConfig.setAlwaysRestartOnNewTraces(true);
		testConfig.setGdFailOnDuplicateNames(false);
		testConfig.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);

		RPNILearner learner = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,testConfig,null,null))
		{
			@Override
			public Pair<Integer,String> CheckWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					@SuppressWarnings("unused")	List<Label> question, @SuppressWarnings("unused") int responseForNoRestart,
					@SuppressWarnings("unused") List<Boolean> acceptedElements,
					@SuppressWarnings("unused") PairScore pairBeingMerged,
					@SuppressWarnings("unused")	final Object [] moreOptions)
			{
				Assert.fail("all answers should have been provided by AutoAnswers");
				return null;
			}
		};
		AutoAnswers ans2 = new AutoAnswers(learner);
		ans2.loadAnswers(new StringReader(
				RPNILearner.QUESTION_USER+" [c, a, c] "+RPNILearner.QUESTION_INCOMPATIBLE+" P1002 P1001\n"+
				RPNILearner.QUESTION_USER+" [c, a, p, a] "+RPNILearner.QUESTION_IGNORE+"\n"+
				RPNILearner.QUESTION_USER+" [c, b, a, a] "+RPNILearner.QUESTION_IGNORE+"\n"+
				RPNILearner.QUESTION_USER+" [c, b, a, b] "+RPNILearner.QUESTION_NEWTRACE+" "+
				"+ [[e, e, e], [ e, e], [e, c, a, e, c], [ c, b, e, e,]]"+
				"- [[c, b, a], [ p ], [c, b, p, p], "+
				"[c, b, e, a], "+
				"[c, b, e, p], [e, a], [e, e, p], "+
				"[e, e, a], [e, e, b], [ e, p], "+
				"[e, b], [e, c, c], [e, c, b, a]]\n"+
				
				RPNILearner.QUESTION_USER+" [e, c, b, e, a] "+RPNILearner.QUESTION_NEWTRACE+
				"- [[e, c, b, e, a]], + [[ e, c, b, e, e]] "+
				" - [[ e, c, b, e, p], [ e, c, b, p, a], [ e, c, b, p, p]] "+
				"+ [[ e, c, b, p, e, e]], - [[e, c, a, a, b, p, a]], "+
				"+ [[e, c, a, a, b, p, e], [ e, c, a, a, a, a, b, p]] \n"
		),testConfig);
		ans2.learnMachine(
				buildSet(new String[][]{
					new String[] { "c","b","p","e","e" },
					new String[] { "c","a","a","b","p", "e" },
					new String[] { "c","a","e","c" },
					new String[] { "c","a","a","a","a","b","p" }},testConfig),
				buildSet(new String[][]{
					new String[] { "c", "a", "a", "b", "p", "a" },
					new String[] { "c", "b", "p", "a" },
					new String[] { "c", "c" },
					new String[] { "b" },
					new String[] { "a" }
				},testConfig));
		}

}
