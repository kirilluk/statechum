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
import statechum.analysis.learning.linear.TestGDMultipleCasesOfRenaming;
import statechum.analysis.learning.linear.TestGDMultithreaded;
import statechum.analysis.learning.linear.TestGDMultithreadedNonArray;
import statechum.model.testset.TestPTAComputePrecisionRecall;

/** This is separate from AllTests because I want to be able to run just FastTests from "ant test" when testing on Iceberg.
 * 
 * @author Kirill
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
    statechum.AllTests.HarmonyTests.class,
    statechum.FastTests.BasicTests.class,
    statechum.FastTests.GraphTests.class,
    statechum.FastTests.LearnerTests.class,
    statechum.FastTests.ObserversTests.class,
    statechum.FastTests.LinearTests.class,
    statechum.FastTests.SmtTests.class,
    statechum.analysis.Erlang.ErlangTests.class,
})
public class FastTests {

    @RunWith(Suite.class)
    @Suite.SuiteClasses({
        statechum.analysis.learning.TestPTAConstruction.class,
        statechum.analysis.learning.TestRpniLearner.class,
        statechum.analysis.learning.rpnicore.TestLTLToBa.class,
        statechum.analysis.learning.rpnicore.TestAugmentUsingIFTHEN.class,
        statechum.analysis.learning.rpnicore.TestAugmentUsingIFTHEN.TestQuestionPTA.class,
        statechum.analysis.learning.rpnicore.TestRandomPathGenerator.class,
        statechum.analysis.learning.TestLoadAnswers.class,
        TestPTAComputePrecisionRecall.class,
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
        statechum.analysis.learning.rpnicore.TestParserDot.class,
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
        statechum.analysis.learning.experiments.PairSelection.TestMiscLearningRoutines.class,
        statechum.analysis.learning.experiments.TestSGE_ExperimentRunner.class
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
        statechum.analysis.learning.rpnicore.TestMergeStatistics.class,
        statechum.analysis.learning.rpnicore.TestTransform.class,
        statechum.analysis.learning.rpnicore.TestWMethodUniversal.class,
        statechum.analysis.learning.rpnicore.TestWMethod.class,
        statechum.analysis.learning.experiments.TestForestFireGenerator.class,
        statechum.analysis.learning.TestDrawGraphs.class,
        statechum.analysis.learning.experiments.mutation.TestGraphMutator.class,
        statechum.analysis.learning.experiments.mutation.TestDiffExperiments.class,
        statechum.analysis.learning.experiments.mutation.TestExperimentResult.class,
        statechum.analysis.learning.experiments.PairSelection.TestMarkovLearner.class,
        statechum.analysis.learning.rpnicore.TestNDScoring.class,
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
        TestGDMultithreaded.class,
        TestGDMultithreadedNonArray.class,
        TestGDMultipleCasesOfRenaming.class,
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
}
