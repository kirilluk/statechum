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

package statechum;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * @author Kirill
 *
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
	statechum.AllTests.LearnerTests.class,
	statechum.AllTests.FSMTests.class,
	statechum.AllTests.WmethodTests.class,
	statechum.AllTests.BasicGraphTests.class,
	statechum.analysis.learning.rpnicore.TestTransform.class,
	statechum.TestArrayOperations.class
})
public class AllTests {
	@RunWith(Suite.class)
	@Suite.SuiteClasses({
		statechum.analysis.learning.TestPTAConstruction.class,
		statechum.analysis.learning.TestRpniLearner.class,
		statechum.analysis.learning.rpnicore.TestRandomPathGenerator.class,
		statechum.analysis.learning.TestLoadAnswers.class,
		statechum.analysis.learning.experiments.TestAbstractExperiment.class,
		statechum.model.testset.TestPTA_computePrecisionRecall.class
	}) // commas after the last entry compile from within Eclipse but not from ant
	public static class LearnerTests {
	}
	
	@RunWith(Suite.class)
	@Suite.SuiteClasses({
		statechum.analysis.learning.TestFSMParser.class,
		statechum.analysis.learning.TestFSMAlgo.class
	})
	public static class FSMTests {
	}

	@RunWith(Suite.class)
	@Suite.SuiteClasses({
		statechum.model.testset.TestPrefixRemovingCollection.class,
		statechum.model.testset.TestPTASequenceEngine.class,
		statechum.model.testset.TestWMethod.class
	})
	public static class WmethodTests {
	}

	@RunWith(Suite.class)
	@Suite.SuiteClasses({
		statechum.analysis.learning.rpnicore.TestGraphBasicAlgorithms.class,
		statechum.analysis.learning.TestGraphGeneration.class,
		statechum.analysis.learning.TestPathTracing.class,
		statechum.analysis.learning.rpnicore.TestGraphConstruction.class,
		statechum.analysis.learning.rpnicore.TestGraphConstructionWithDifferentConf.class
	})
	public static class BasicGraphTests {
	}
}
