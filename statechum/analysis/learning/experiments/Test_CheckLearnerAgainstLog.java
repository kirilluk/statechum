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

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.Pair;
import statechum.analysis.learning.RPNILearner;
import statechum.analysis.learning.RPNIUniversalLearner;
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
@RunWith(Parameterized.class)
public class Test_CheckLearnerAgainstLog 
{
	/** This method is a useful troubleshooting aid - the log it creates should be
	 * identical to that we are playing, but if something is wrong, it is easier
	 * to spot problems in a log than via a debugger.
	 
	public void copyLogIntoAnotherLog(String logFileName)
	{
		try {
			final LearnerSimulator simulator = new LearnerSimulator(new java.io.FileInputStream(logFileName),true);
			final LearnerEvaluationConfiguration evalData = simulator.readLearnerConstructionData();
			final org.w3c.dom.Element nextElement = simulator.expectNextElement(ELEM_KINDS.ELEM_INIT.name());
			final ProgressDecorator.InitialData initial = simulator.readInitialData(nextElement);
			simulator.setNextElement(nextElement);
	
			RPNILearner learner2 = new RPNIUniversalLearner(null,null,evalData.config)
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
	}*/
	
	public void check(String logFileName) throws FileNotFoundException
	{
		// now a simulator to a learner
		final LearnerSimulator simulator = new LearnerSimulator(new java.io.FileInputStream(logFileName),true);
		final LearnerEvaluationConfiguration evalData = simulator.readLearnerConstructionData();
		final org.w3c.dom.Element nextElement = simulator.expectNextElement(ELEM_KINDS.ELEM_INIT.name());
		final ProgressDecorator.InitialData initial = simulator.readInitialData(nextElement);
		simulator.setNextElement(nextElement);

		// Now we need to choose learner parameters based on the kind of file we are given
		// (given the pace of Statechum evolution, I cannot expect all the correct options
		// to be stored in log files).
		if (logFileName.contains(Configuration.LEARNER.LEARNER_BLUEFRINGE_MAY2008.name()))
		{
			evalData.config.setUseAmber(false);evalData.config.setUseSpin(false);
			evalData.config.setSpeculativeQuestionAsking(false);
		}
		else 		
			if (logFileName.contains(Configuration.LEARNER.LEARNER_BLUEAMBER_MAY2008.name()))
			{
				evalData.config.setUseAmber(true);evalData.config.setUseSpin(false);
				evalData.config.setSpeculativeQuestionAsking(false);
			}
			else Assert.fail("unknown type of log file");
		
		RPNILearner learner2 = new RPNIUniversalLearner(null,null,evalData.config)
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
		new Test_LearnerComparator(learner2,simulator).learnMachine(initial.plus, initial.minus);
	}

	protected final static String pathToLogFiles = "resources/nonsvn/logs";
	
	@Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		for(File f:new File(pathToLogFiles).listFiles(new FileFilter(){

			public boolean accept(File pathName) {
				return pathName.canRead() && pathName.isFile() &&
				pathName.getAbsolutePath().contains(".xml_");
			}}))
			result.add(new Object[]{f});
		
		return result;
	}

	protected final File logFileToProcess;
	
	public Test_CheckLearnerAgainstLog(File file)
	{
		logFileToProcess = file;
	}
	
	@Test
	public final void testAgainstLog() throws FileNotFoundException
	{
		check(logFileToProcess.getAbsolutePath());
	}
}
