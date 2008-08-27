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

import statechum.Configuration;
import statechum.Pair;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.observers.LearnerSimulator;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.observers.RecordProgressDecorator;
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
public class Test_CheckLearnerAgainstLog 
{
	/** This method is a useful troubleshooting aid - the log it creates should be
	 * identical to that we are playing, but if something is wrong, it is easier
	 * to spot problems in a log than via a debugger.
	 */
	public void copyLogIntoAnotherLog(String logFileName)
	{
		try {
			final LearnerSimulator simulator = new LearnerSimulator(new java.io.FileInputStream(logFileName),true);
			final LearnerEvaluationConfiguration evalData = simulator.readLearnerConstructionData();
			final org.w3c.dom.Element nextElement = simulator.expectNextElement(ELEM_KINDS.ELEM_INIT.name());
			final ProgressDecorator.InitialData initial = simulator.readInitialData(nextElement);
			simulator.setNextElement(nextElement);
	
			RPNIBlueFringeLearner learner2 = new RPNIBlueFringeLearner(null,evalData.config)
			{
				@Override
				public Pair<Integer,String> CheckWithEndUser(
						@SuppressWarnings("unused")	LearnerGraph model,
						List<String> question, 
						@SuppressWarnings("unused")	final Object [] moreOptions)
				{
					return new Pair<Integer,String>(evalData.graph.paths.tracePath(question),null);
				}
			};
	
			RecordProgressDecorator recorder = null;
			java.io.FileOutputStream out = null;
			Configuration progressRecorderConfig = evalData.config.copy();progressRecorderConfig.setCompressLogs(false);
				out = new java.io.FileOutputStream("resources/tmp.xml");
				recorder = new RecordProgressDecorator(learner2,out,1,progressRecorderConfig,true);
			recorder.writeLearnerEvaluationData(evalData);
			learner2.setTopLevelListener(recorder);
			recorder.learnMachine(initial.plus, initial.minus);
		} catch (java.io.IOException e) {
			statechum.Helper.throwUnchecked("failure reading/writing log files", e);
		}
	}
	
	public void check(String logFileName) throws FileNotFoundException
	{
		// now a simulator to a learner
		final LearnerSimulator simulator = new LearnerSimulator(new java.io.FileInputStream(logFileName),true);
		final LearnerEvaluationConfiguration eval1 = simulator.readLearnerConstructionData();
		final org.w3c.dom.Element nextElement = simulator.expectNextElement(ELEM_KINDS.ELEM_INIT.name());
		final ProgressDecorator.InitialData initial = simulator.readInitialData(nextElement);
		simulator.setNextElement(nextElement);

		RPNIBlueFringeLearner learner2 = new RPNIBlueFringeLearner(null,eval1.config)
		{
			@Override
			public Pair<Integer,String> CheckWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					List<String> question, 
					@SuppressWarnings("unused")	final Object [] moreOptions)
			{
				return new Pair<Integer,String>(eval1.graph.paths.tracePath(question),null);
			}
		};
		new Test_LearnerComparator(learner2,simulator).learnMachine(initial.plus, initial.minus);
	}

	protected final static String pathToLogFiles = "/home/kirill/W_experiment/logs/b/", learner = Configuration.LEARNER.LEARNER_BLUEFRINGE.name();
	
	@Test
	public final void test11() throws FileNotFoundException
	{
		//DeterministicDirectedSparseGraph.VertexID.comparisonKind=DeterministicDirectedSparseGraph.VertexID.ComparisonKind.COMPARISON_NORM;
		check(pathToLogFiles+"2_25Inputs_75_11.xml_"+learner+"_log-100.xml");
	}
	
	@Test
	public final void test12() throws FileNotFoundException
	{
		//DeterministicDirectedSparseGraph.VertexID.comparisonKind=DeterministicDirectedSparseGraph.VertexID.ComparisonKind.COMPARISON_NORM;
		check(pathToLogFiles+"3_25Inputs_75_12.xml_"+learner+"_log-100.xml");
	}
	
	@Test
	public final void test1() throws FileNotFoundException
	{
		//DeterministicDirectedSparseGraph.VertexID.comparisonKind=DeterministicDirectedSparseGraph.VertexID.ComparisonKind.COMPARISON_LEXICOGRAPHIC_ORIG;
		//copyLogIntoAnotherLog(pathToLogFiles+"0_25Inputs_75_1.xml_log-100.xml");
		check(pathToLogFiles+"0_25Inputs_75_1.xml_"+learner+"_log-100.xml");
	}
	@Test
	public final void test10() throws FileNotFoundException
	{
		//DeterministicDirectedSparseGraph.VertexID.comparisonKind=DeterministicDirectedSparseGraph.VertexID.ComparisonKind.COMPARISON_LEXICOGRAPHIC_ORIG;
		//copyLogIntoAnotherLog(pathToLogFiles+"0_25Inputs_75_1.xml_log-100.xml");
		check(pathToLogFiles+"1_25Inputs_75_10.xml_"+learner+"_log-100.xml");
	}
}
