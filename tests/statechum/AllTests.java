/* Copyright (c) 2006, 2007, 2008 The University of Sheffield
 * 
 * This file is part of StateChum
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
package statechum;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import statechum.analysis.learning.rpnicore.testLTLToBa;

/*
Important: when running via IntelliJ Idea, the console will not show the progress of initialisation until
a custom property editable.java.test.console=true is set (Help->Edit Custom Properties).
 */


/**
 * @author Kirill
 *
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
    statechum.AllTests.BasicTests.class,
    statechum.AllTests.GraphTests.class,
    statechum.AllTests.LearnerTests.class,
    statechum.AllTests.ObserversTests.class,
    statechum.AllTests.LinearTests.class,
    statechum.AllTests.SmtTests.class,
    statechum.analysis.Erlang.ErlangTests.class,
    //statechum.AllTests.TestErlangWithDifferentOTP.class,
    statechum.AllTests.LengthyTests.class
})
public class AllTests {

    @RunWith(Suite.class)
    @Suite.SuiteClasses({
        statechum.analysis.learning.TestPTAConstruction.class,
        statechum.analysis.learning.TestRpniLearner.class,
        testLTLToBa.class,
        statechum.analysis.learning.rpnicore.TestAugmentUsingIFTHEN.class,
        statechum.analysis.learning.rpnicore.TestAugmentUsingIFTHEN.TestQuestionPTA.class,
        statechum.analysis.learning.rpnicore.TestRandomPathGenerator.class,
        statechum.analysis.learning.TestLoadAnswers.class,
        statechum.model.testset.TestPTA_computePrecisionRecall.class,
        statechum.analysis.learning.TestLearnerWithLabelRefinementViaPta.class
    }) // commas after the last entry compile from within Eclipse but not from ant
    public static class LearnerTests {// all tests are included in the annotation.
    }

    @RunWith(Suite.class)
    @Suite.SuiteClasses({
        statechum.analysis.learning.observers.TestRecordProgressDecorator.class,
        statechum.analysis.learning.observers.TestRecordProgressDecorator.TestSequenceDumping.class,
        statechum.analysis.learning.observers.TestWriteReadPair.class,
        statechum.analysis.learning.observers.TestWriteReadInit.class,
        statechum.analysis.learning.observers.TestWriteReadLearnerEvaluation.class,
        statechum.analysis.learning.observers.TestWriteReadAugmentPta.class,
        statechum.analysis.learning.observers.TestGraphSeries.class,
        statechum.analysis.learning.observers.TestRecorderIntegration.class,
        statechum.analysis.learning.observers.TestAutoAnswers.class
    })
    public static class ObserversTests {// all tests are included in the annotation.
    }

    @RunWith(Suite.class)
    @Suite.SuiteClasses({
        statechum.TestConfiguration.class,
        collections.TestMapWithSearch.class,
        collections.TestMapWithSearchMisc.class,
        statechum.analysis.learning.rpnicore.TestEqualityComparisonAndHashCode.class,
        statechum.analysis.learning.rpnicore.TestFSMParser.class,
        statechum.analysis.learning.rpnicore.TestFSMAlgo.class,
        statechum.analysis.learning.rpnicore.TestLearnerGraphND.class,
        statechum.analysis.learning.rpnicore.TestEquivalenceChecking.class,
        statechum.analysis.learning.rpnicore.TestRejectManipulation.class,
        statechum.apps.TestQSMTool.class,
        statechum.apps.TestQSMTool.TestInvalidTraces.class,
        statechum.analysis.learning.experiments.TestPaperUAS.class,
        collections.TestArrayOperations.class,
        statechum.analysis.learning.rpnicore.TestNextID.class,
        statechum.analysis.learning.rpnicore.TestGraphBasicAlgorithms.class,
        statechum.analysis.learning.rpnicore.TestGraphConstruction.class,
        statechum.analysis.learning.rpnicore.TestGraphConstructionWithDifferentConf.class,
        statechum.analysis.learning.experiments.PairSelection.TestWekaPairClassifier.class,
        statechum.analysis.learning.experiments.TestSGE_ExperimentRunner.class,
        statechum.analysis.learning.rpnicore.TestParserDot.class
    })
    public static class BasicTests {// all tests are included in the annotation.
    }

    @RunWith(Suite.class)
    @Suite.SuiteClasses({
        statechum.analysis.learning.TestEVGraphGeneration.class,
        statechum.analysis.learning.TestPathTracing.class,
        statechum.model.testset.TestPrefixRemovingCollection.class,
        statechum.model.testset.TestPTASequenceEngine.class,
        statechum.analysis.learning.rpnicore.TestMiscTransformFunctions.class,
        statechum.analysis.learning.rpnicore.TestTransform.class,
        statechum.analysis.learning.rpnicore.TestWMethodUniversal.class,
        statechum.analysis.learning.rpnicore.TestWMethod.class,
        statechum.analysis.learning.experiments.TestForestFireGenerator.class,
        statechum.analysis.learning.TestDrawGraphs.class,
        statechum.analysis.learning.experiments.mutation.TestGraphMutator.class,
        statechum.analysis.learning.experiments.mutation.TestDiffExperiments.class,
        statechum.analysis.learning.experiments.mutation.TestExperimentResult.class,
        statechum.analysis.learning.experiments.PairSelection.TestMarkovLearner.class,
    	statechum.analysis.learning.TestStateMerging.class,
        statechum.TestProgressIndicator.class
    })
    public static class GraphTests {// all tests are included in the annotation.
    }

    @RunWith(Suite.class)
    @Suite.SuiteClasses({
        statechum.analysis.learning.linear.TestLinear.class,
        statechum.analysis.learning.linear.TestSolverRandomly.class,
        statechum.analysis.learning.linear.TestSolver.class,
        statechum.analysis.learning.linear.TestMatrixComputationWithMultipleThreads.class,
        statechum.analysis.learning.linear.TestGD.class,
        statechum.analysis.learning.linear.TestGD_Multithreaded.class,
        statechum.analysis.learning.linear.TestGD_Multithreaded_NonArray.class,
        statechum.analysis.learning.linear.TestGD_MultipleCasesOfRenaming.class,
        statechum.apps.TestVisualDemo.class
    })
    public static class LinearTests {// all tests are included in the annotation.
    }

    @RunWith(Suite.class)
    @SuiteClasses({
        statechum.analysis.learning.TestSmt.class,
        statechum.analysis.learning.smt.TestSmtLabelRepresentation.class,
        statechum.analysis.learning.smt.TestSmtLabelRepresentation.TestFeaturesOfAbstractStates.class,
        statechum.analysis.learning.smt.TestLabelParser.class,
        statechum.analysis.learning.smt.TestSmtLabelRepresentation.TestChecksInTwoContexts.class
    })
    public static class SmtTests {// all tests are included in the annotation.
    }
/*
    @RunWith(ParameterizedSuite.class)
    @SuiteClasses({
        statechum.analysis.Erlang.TestErlangStartupFailure.class,
        statechum.analysis.Erlang.TestErlangModule.class,
        statechum.analysis.Erlang.TestErlangRunner.class,
        statechum.analysis.Erlang.TestErlangParser.class,
        statechum.analysis.Erlang.TestErlangParser.TestParseBitStrFail.class,
        statechum.analysis.Erlang.TestErlangParser.TestParseDoubleFail.class,
        statechum.analysis.Erlang.TestErlangParser.TestParseInvalidCharsInAtomFail.class,
        statechum.analysis.learning.TestErlangOracleLearner.class,
        statechum.analysis.Erlang.Signatures.TestTypes.class,
        statechum.analysis.Erlang.TestErlangGraphs.class,
        statechum.analysis.Erlang.TestSynapseAuxiliaryFunctions.class,
        statechum.analysis.Erlang.TestSynapse.class
    })
    public static class TestErlangWithDifferentOTP
    {
    	final private String otpPath;
    	
    	@org.junit.runners.Parameterized.Parameters
    	public static Collection<Object[]> data() 
    	{
    		Collection<Object []> result = new LinkedList<Object []>();
    		String runtimes = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.ERLANG_OTP_RUNTIMES);
    		if (runtimes == null)
    			throw new IllegalArgumentException("ERLANG_OTP_RUNTIMES has not been set, it should contain a list of paths to Erlang runtimes, separated with commas");
    		for(String otp:GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.ERLANG_OTP_RUNTIMES).split(", *"))
    			result.add(new Object[]{otp});
    		return result;
    	}
    	
    	public void resetErlang()
    	{
       		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.ERLANGHOME, otpPath);
    		ErlangRuntime.getDefaultRuntime().killErlang();  		
    	}

    	public TestErlangWithDifferentOTP(String path)
    	{
    		otpPath = path;
    	}
    	
    	public void initSeries()
    	{
    		resetErlang();
    	}
    	
		@ParametersToString
    	public static String parametersToString(String otpPath)
    	{
    		new TestErlangWithDifferentOTP(otpPath).resetErlang();
    		ErlangRuntime.getDefaultRuntime().startRunner();
    		String value = "OTP:"+((OtpErlangString)ErlangRuntime.getDefaultRuntime().createNewRunner().evaluateString("string:substr(erlang:system_info(otp_release),1,3)")).stringValue();
    		return value;
    	}   	
    }
    */
    @RunWith(Suite.class)
    @SuiteClasses({
    	statechum.analysis.learning.experiments.TestPaperUAS_Large.class,
    	statechum.analysis.learning.TestStateMerging.TestRandomFSMMergers.class,
        statechum.analysis.learning.experiments.PairSelection.TestLearnFromTracesUsingMarkov.class,
        statechum.analysis.learning.experiments.mutation.TestDiffExperimentWithLogs.class,
        statechum.analysis.learning.TestLearnerFromLargePTA.class,
        statechum.analysis.learning.experiments.TestExperimentRunner.class,
        statechum.analysis.learning.rpnicore.TestCloneWithDifferentConf.class,
        statechum.analysis.learning.Test_CheckLearnerAgainstLog.class,
    	statechum.analysis.learning.linear.TestGD_ExistingGraphs.class,
        statechum.analysis.learning.linear.TestGD_ExistingGraphsUsingTestSet.class,
        statechum.analysis.learning.linear.TestGD_ExistingGraphsND.class,
        statechum.analysis.learning.linear.TestGD_ExistingGraphsNDUsingTestSet.class
    })
    public static class LengthyTests {// all tests are included in the annotation.
    }
  
}
