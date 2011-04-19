/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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
package testsuites;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

/**
 * @author Kirill
 *
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
    testsuites.AllTests.BasicTests.class,
    testsuites.AllTests.GraphTests.class,
    testsuites.AllTests.LearnerTests.class,
    testsuites.AllTests.LinearTests.class,
    testsuites.AllTests.SmtTests.class,
    testsuites.AllTests.ErlangTests.class,
    statechum.analysis.learning.Test_CheckLearnerAgainstLog.class
})
public class AllTests {

    @RunWith(Suite.class)
    @Suite.SuiteClasses({
        statechum.analysis.learning.TestPTAConstruction.class,
        statechum.analysis.learning.TestRpniLearner.class,
        statechum.analysis.learning.rpnicore.TestLTL_to_ba.class,
        statechum.analysis.learning.rpnicore.TestAugmentUsingIFTHEN.class,
        statechum.analysis.learning.rpnicore.TestAugmentUsingIFTHEN.TestQuestionPTA.class,
        statechum.analysis.learning.rpnicore.TestRandomPathGenerator.class,
        statechum.analysis.learning.TestLoadAnswers.class,
        statechum.analysis.learning.experiments.TestExperimentRunner.class,
        statechum.model.testset.TestPTA_computePrecisionRecall.class,
        testsuites.AllTests.ObserversTests.class
    }) // commas after the last entry compile from within Eclipse but not from ant
    public static class LearnerTests {// all tests are included in the annotation.
    }

    @RunWith(Suite.class)
    @Suite.SuiteClasses({
        statechum.analysis.learning.observers.TestRecordProgressDecorator.class,
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
        statechum.analysis.learning.rpnicore.TestEqualityComparisonAndHashCode.class,
        statechum.analysis.learning.rpnicore.TestFSMParser.class,
        statechum.analysis.learning.rpnicore.TestFSMAlgo.class,
        statechum.analysis.learning.rpnicore.TestLearnerGraphND.class,
        statechum.analysis.learning.rpnicore.TestEquivalenceChecking.class,
        statechum.analysis.learning.rpnicore.TestRejectManipulation.class,
        statechum.apps.TestQSMTool.class,
        statechum.TestArrayOperations.class,
        statechum.analysis.learning.rpnicore.TestNextID.class,
        statechum.analysis.learning.rpnicore.TestGraphBasicAlgorithms.class,
        statechum.analysis.learning.rpnicore.TestGraphConstruction.class,
        statechum.analysis.learning.rpnicore.TestGraphConstructionWithDifferentConf.class,
        statechum.analysis.learning.rpnicore.TestCloneWithDifferentConf.class
    })
    public static class BasicTests {// all tests are included in the annotation.
    }

    @RunWith(Suite.class)
    @Suite.SuiteClasses({
        statechum.analysis.learning.TestEVGraphGeneration.class,
        statechum.analysis.learning.TestPathTracing.class,
        statechum.model.testset.TestPrefixRemovingCollection.class,
        statechum.model.testset.TestPTASequenceEngine.class,
        statechum.analysis.learning.rpnicore.TestTransform.class,
        statechum.analysis.learning.rpnicore.TestWMethodUniversal.class,
        statechum.analysis.learning.rpnicore.TestWMethod.class,
        statechum.analysis.learning.experiments.TestForestFireGenerator.class
    })
    public static class GraphTests {// all tests are included in the annotation.
    }

    @RunWith(Suite.class)
    @Suite.SuiteClasses({
        statechum.analysis.learning.rpnicore.TestLinear.class,
        statechum.analysis.learning.rpnicore.TestSolverRandomly.class,
        statechum.analysis.learning.rpnicore.TestSolver.class,
        statechum.analysis.learning.rpnicore.TestLinearWithMultipleThreads.class,
        statechum.analysis.learning.rpnicore.TestGD.class,
        statechum.analysis.learning.rpnicore.TestGD_Multithreaded.class,
        statechum.analysis.learning.rpnicore.TestGD_MultipleCasesOfRenaming.class,
        statechum.analysis.learning.rpnicore.TestGD_ExistingGraphs.class,
        statechum.analysis.learning.rpnicore.TestGD_ExistingGraphsUsingTestSet.class,
        statechum.analysis.learning.rpnicore.TestGD_ExistingGraphsND.class,
        statechum.analysis.learning.rpnicore.TestGD_ExistingGraphsNDUsingTestSet.class,
        statechum.apps.TestVisualDemo.class
    })
    public static class LinearTests {// all tests are included in the annotation.
    }

    @RunWith(Suite.class)
    @SuiteClasses({
        statechum.analysis.learning.TestSmt.class,
        statechum.analysis.learning.rpnicore.TestSmtLabelRepresentation.class,
        statechum.analysis.learning.rpnicore.TestSmtLabelRepresentation.TestFeaturesOfAbstractStates.class,
        statechum.analysis.learning.rpnicore.TestLabelParser.class,
        statechum.analysis.learning.rpnicore.TestSmtLabelRepresentation.TestChecksInTwoContexts.class
    })
    public static class SmtTests {// all tests are included in the annotation.
    }

    @RunWith(Suite.class)
    @SuiteClasses({
        statechum.analysis.Erlang.ErlangModuleTest.class,
        statechum.analysis.Erlang.ErlangRunnerTest.class,
        statechum.analysis.learning.TestErlangOracleLearner.class
    })
    public static class ErlangTests {// all tests are included in the annotation.
    }
}
