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
package statechum.analysis.learning.experiments;

import java.io.FileNotFoundException;
import java.util.List;

import org.junit.Test;

import statechum.DeterministicDirectedSparseGraph;
import statechum.Pair;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.RPNIBlueFringeLearnerTestComponentOpt;
import statechum.analysis.learning.observers.LearnerSimulator;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.observers.Test_LearnerComparator;
import statechum.analysis.learning.observers.ProgressDecorator.ELEM_KINDS;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;

/**
 * Makes sure a learner performs exactly as the record in a log.
 * 
 * @author kirill
 *
 */
public class Test_CheckLearnerAgainstLog {
	public void check(String logFileName) throws FileNotFoundException
	{
		final LearnerSimulator simulator = new LearnerSimulator(new java.io.FileInputStream(logFileName),true);
		final LearnerEvaluationConfiguration eval1 = simulator.readLearnerConstructionData();
		final org.w3c.dom.Element nextElement = simulator.expectNextElement(ELEM_KINDS.ELEM_INIT.name());
		final ProgressDecorator.InitialData initial = simulator.readInitialData(nextElement);
		simulator.setNextElement(nextElement);
		RPNIBlueFringeLearner learner2 = new RPNIBlueFringeLearnerTestComponentOpt(null,eval1.config)
		{
			@Override
			public Pair<Integer,String> checkWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					List<String> question, 
					@SuppressWarnings("unused")	final Object [] moreOptions)
			{
				return new Pair<Integer,String>(eval1.graph.paths.tracePath(question),null);
			}
		};
		
		/*
		RecordProgressDecorator recorder = null;
		java.io.FileOutputStream out = null;
		try {
			out = new java.io.FileOutputStream("resources/tmp.xml");
			recorder = new RecordProgressDecorator(learner2.getLearner(),out,1,eval1.config,true);
		} catch (IOException e) {
			statechum.Helper.throwUnchecked("could not open log file for writing", e);
		}
		recorder.writeLearnerEvaluationData(eval1);
		learner2.getLearner().setTopLevelListener(recorder);
		recorder.learnMachine(initial.plus, initial.minus);
		try {
			out.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		//WMethod.checkM(initial.graph, learner2.getLearner().init(initial.plus, initial.minus));
		 
		 */
		new Test_LearnerComparator(learner2.getLearner(),simulator).learnMachine(initial.plus, initial.minus);
	}
	
	@Test
	public final void test1() throws FileNotFoundException
	{
		DeterministicDirectedSparseGraph.VertexID.comparisonKind=DeterministicDirectedSparseGraph.VertexID.ComparisonKind.COMPARISON_NORM;
		check("resources/logs/2_25Inputs_75_11.xml_log-100.xml");
	}
	@Test
	public final void test2() throws FileNotFoundException
	{
		DeterministicDirectedSparseGraph.VertexID.comparisonKind=DeterministicDirectedSparseGraph.VertexID.ComparisonKind.COMPARISON_NORM;
		check("resources/logs/3_25Inputs_75_12.xml_log-100.xml");
	}
	@Test
	public final void test3() throws FileNotFoundException
	{
		DeterministicDirectedSparseGraph.VertexID.comparisonKind=DeterministicDirectedSparseGraph.VertexID.ComparisonKind.COMPARISON_LEXICOGRAPHIC_ORIG;
		check("resources/logs/0_25Inputs_75_1.xml_log-100.xml");
	}
	
	@Test
	public final void test4() throws FileNotFoundException
	{
		DeterministicDirectedSparseGraph.VertexID.comparisonKind=DeterministicDirectedSparseGraph.VertexID.ComparisonKind.COMPARISON_NORM;
		check("resources/logs/4_25Inputs_75_37.xml_log-100.xml");
	}
}
