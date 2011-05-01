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
package statechum.analysis.learning;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.junit.After;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.ArrayOperations;
import statechum.Configuration;
import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.StatechumXML;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.DeterministicDirectedSparseGraph.VertexID.ComparisonKind;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.observers.LearnerSimulator;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.observers.RecordProgressDecorator;
import statechum.analysis.learning.observers.Test_LearnerComparator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;

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
	 * to spot problems in a log than via a debugger. Additionally, this routine can be used 
	 * to compress or uncompress an existing log.  
	 */
	public void copyLogIntoAnotherLog(String logFileName)
	{
		try {
			Configuration config = Configuration.getDefaultConfiguration().copy();
			final LearnerSimulator simulator = new LearnerSimulator(new java.io.FileInputStream(logFileName),true);
			final LearnerEvaluationConfiguration evalData = simulator.readLearnerConstructionData(config);
			final org.w3c.dom.Element nextElement = simulator.expectNextElement(StatechumXML.ELEM_INIT.name());
			final ProgressDecorator.InitialData initial = simulator.readInitialData(nextElement);
			simulator.setNextElement(nextElement);
	
			RPNILearner learner2 = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,evalData.config,null,null))
			{
				@Override
				public Pair<Integer,String> CheckWithEndUser(
						@SuppressWarnings("unused")	LearnerGraph model,
						List<Label> question,	@SuppressWarnings("unused") int answerForNoRestart, 
						@SuppressWarnings("unused") List<Boolean> acceptedElements,
						@SuppressWarnings("unused") PairScore pairBeingMerged,
						@SuppressWarnings("unused")	final Object [] moreOptions)
				{
					return new Pair<Integer,String>(evalData.graph.paths.tracePathPrefixClosed(question),null);
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
	
	/** The actual test method - 
	 * <ul><li>loads the configuration from log.</li>
	 * <li>creates a learner based on the configuration and runs it.</li>
	 * <li>checks most arguments and return values of most methods called by a learner against values recorded in the log.</li>
	 * </ul> 
	 * @param logFileName log to play
	 * @throws FileNotFoundException if log is not where it should be.
	 */
	public void check(String logFileName) throws FileNotFoundException
	{
		// now a simulator to a learner
		if (logFileName.contains(Configuration.LEARNER.LEARNER_BLUEFRINGE_DEC2007.name()))
			VertexID.comparisonKind = ComparisonKind.COMPARISON_LEXICOGRAPHIC_ORIG;// this is a major change in a configuration of learners, affecting the comparison between vertex IDs 

		final LearnerSimulator simulator = new LearnerSimulator(new java.io.FileInputStream(logFileName),true);
		Configuration config = Configuration.getDefaultConfiguration().copy();
		config.setLegacyXML(false);
		final LearnerEvaluationConfiguration evalData = simulator.readLearnerConstructionData(config);
		
		// Now we need to choose learner parameters based on the kind of file we are given
		// (given the pace of Statechum evolution, I cannot expect all the correct options
		// to be stored in log files).
		if (logFileName.contains(Configuration.LEARNER.LEARNER_BLUEFRINGE_MAY2008.name()))
		{
			evalData.config.setUseAmber(false);evalData.config.setUseLTL(false);
			evalData.config.setSpeculativeQuestionAsking(false);
			evalData.config.setIgnoreDepthInTheChoiceOfRepresentatives(true);evalData.config.setIgnoreVertexAttributesInLogReplay(true);
		}
		else 		
			if (logFileName.contains(Configuration.LEARNER.LEARNER_BLUEAMBER_MAY2008.name()))
			{
				evalData.config.setUseAmber(true);evalData.config.setUseLTL(false);
				evalData.config.setSpeculativeQuestionAsking(false);
				evalData.config.setIgnoreDepthInTheChoiceOfRepresentatives(true);evalData.config.setIgnoreVertexAttributesInLogReplay(true);
			}
			else 
				if (logFileName.contains(Configuration.LEARNER.LEARNER_BLUEFRINGE_DEC2007.name()))
				{// we'd like to make sure that the initial configuration is loaded with the correct configuration values.
					evalData.config.setInitialIDvalue(1);
					VertexID.comparisonKind = ComparisonKind.COMPARISON_LEXICOGRAPHIC_ORIG;
					evalData.config.setUseAmber(false);evalData.config.setUseLTL(false);
					evalData.config.setSpeculativeQuestionAsking(false);
					evalData.config.setDefaultInitialPTAName("Init");
					evalData.config.setIgnoreDepthInTheChoiceOfRepresentatives(true);evalData.config.setIgnoreVertexAttributesInLogReplay(true);
				}
				else
					Assert.fail("unknown type of log file");

		final org.w3c.dom.Element nextElement = simulator.expectNextElement(StatechumXML.ELEM_INIT.name());
		final ProgressDecorator.InitialData initial = simulator.readInitialData(nextElement);
		if (evalData.labelDetails != null) 
		{ 
			initial.plus.addAll(evalData.labelDetails.getSPlus());initial.minus.addAll(evalData.labelDetails.getSMinus());
		}
		simulator.setNextElement(nextElement);

		Learner learner2 = null;

		if (logFileName.contains(Configuration.LEARNER.LEARNER_BLUEFRINGE_DEC2007.name()))
		{// have to patch the learner.
			
			learner2 = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,evalData.config,null,null)) 
			{
				/* (non-Javadoc)
				 * @see statechum.analysis.learning.observers.DummyLearner#init(java.util.Collection, java.util.Collection)
				 */
				@Override
				public LearnerGraph init(Collection<List<Label>> plus,	Collection<List<Label>> minus) 
				{
					return super.init(plus, minus);
				}

				/* (non-Javadoc)
				 * @see statechum.analysis.learning.RPNIUniversalLearner#ComputeQuestions(statechum.analysis.learning.PairScore, statechum.analysis.learning.rpnicore.LearnerGraph, statechum.analysis.learning.rpnicore.LearnerGraph)
				 */
				@Override
				public List<List<Label>> ComputeQuestions(PairScore pair,	LearnerGraph original, LearnerGraph tempNew) 
				{
					return ArrayOperations.sort(ComputeQuestions.computeQS_origReduced(pair,original,tempNew));
				}

				/* (non-Javadoc)
				 * @see statechum.analysis.learning.RPNIUniversalLearner#MergeAndDeterminize(statechum.analysis.learning.rpnicore.LearnerGraph, statechum.analysis.learning.StatePair)
				 */
				@Override
				public LearnerGraph MergeAndDeterminize(LearnerGraph original,	StatePair pair) 
				{
					return MergeStates.mergeAndDeterminize(original,pair);
				}
				
				@Override
				public Pair<Integer,String> CheckWithEndUser(
						@SuppressWarnings("unused")	LearnerGraph model,
						List<Label> question, @SuppressWarnings("unused") int answerForNoRestart,
						@SuppressWarnings("unused") List<Boolean> acceptedElements,
						@SuppressWarnings("unused") PairScore pairBeingMerged,
						@SuppressWarnings("unused")	final Object [] moreOptions)
					{
						return new Pair<Integer,String>(evalData.graph.paths.tracePathPrefixClosed(question),null);
					}
				
				@Override
				public void AugumentPTA_and_QuestionPTA(LearnerGraph pta, RestartLearningEnum ptaKind,
						List<Label> sequence, boolean accepted, JUConstants newColour)
				{
					topLevelListener.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
				}
			};
		    
		}
		else
		if (logFileName.contains(Configuration.LEARNER.LEARNER_BLUEFRINGE_MAY2008.name()) ||
				logFileName.contains(Configuration.LEARNER.LEARNER_BLUEAMBER_MAY2008.name())
		 )
		{// have to patch the learner.
			
			learner2 = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,evalData.config,null,null)) 
			{
				/* (non-Javadoc)
				 * @see statechum.analysis.learning.observers.DummyLearner#init(java.util.Collection, java.util.Collection)
				 */
				@Override
				public LearnerGraph init(Collection<List<Label>> plus,	Collection<List<Label>> minus) 
				{
					tentativeAutomaton.initPTA_1();		
					tentativeAutomaton.paths.augmentPTA(minus, false,false);
					tentativeAutomaton.paths.augmentPTA(plus, true,false);
					return tentativeAutomaton;
				}

				@Override
				public Pair<Integer,String> CheckWithEndUser(
						@SuppressWarnings("unused")	LearnerGraph model,
						List<Label> question, @SuppressWarnings("unused") int answerForNoRestart,
						@SuppressWarnings("unused") List<Boolean> acceptedElements,
						@SuppressWarnings("unused") PairScore pairBeingMerged,
						@SuppressWarnings("unused")	final Object [] moreOptions)
						{
							return new Pair<Integer,String>(evalData.graph.paths.tracePathPrefixClosed(question),null);
						}
			};
		}
		
		// There is a reason why we should ignore vertex names (WMethod.VERTEX_COMPARISON_KIND.NONE): 
		// they are generated by GD to appear 
		// as close to the original graph as possible, but we are comparing the result of reconstruction
		// of the current graph by GD against the one generated by the learner. The following example shows
		// why simply insisting on the correct names will not work:
		// 
		//	B-a5->D\nB-a19->A-a5->C
		// if vertices A B are merged, paths A-a5->D and B-a5->C are collapsed,
		// hence the outcome is A-a19->A-a5->C   because we have to merge A,B and C,D,
		// where A and C respectively are winners.
		// GD comparison of the two graphs determines that we need to remove the part
		// of the first graph starting with a19 transition and just loop a19 around B.
		// This way, states B and D are preserved and the outcome of reconstruction is  B-a19->B-a5->D
		// which is bisimular to the expected graph but state names are different.
		//
		// It is worth noting that since the above was written GD has been modified to generate
		// an identical graph by relabelling states. If this one is utilised, the outcomes
		// of learning should be identical.
		
		new Test_LearnerComparator(learner2,simulator,!evalData.config.isIgnoreVertexAttributesInLogReplay()).learnMachine(initial.plus, initial.minus);
		if (logFileName.contains(Configuration.LEARNER.LEARNER_BLUEFRINGE_DEC2007.name()))
			VertexID.comparisonKind = ComparisonKind.COMPARISON_NORM;// reset this one if needed.
	}

	protected final static String pathToLogFiles = "resources/logs";
	
	@Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		for(File f:new File(pathToLogFiles).listFiles(new FileFilter(){
			public @Override boolean accept(File pathName) {
				return pathName.canRead() && pathName.isFile() &&
				pathName.getAbsolutePath().contains(".xml_")
				//&&	pathName.getAbsolutePath().contains("2_25Inputs_75_1.xml_LEARNER_BLUEFRINGE_MAY2008")
				;
			}}))
			result.add(new Object[]{f});
		
		return result;
	}

	public static String parametersToString(File f)
	{
		return f.getName();
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
	
	@After
	public final void afterTest()
	{
		VertexID.comparisonKind = ComparisonKind.COMPARISON_NORM;
	}
}
