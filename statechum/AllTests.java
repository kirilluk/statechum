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
	statechum.AllTests.BasicTests.class,
	statechum.AllTests.GraphTests.class
})
public class AllTests {
	@RunWith(Suite.class)
	@Suite.SuiteClasses({
		statechum.analysis.learning.TestPTAConstruction.class,
		statechum.analysis.learning.TestRpniLearner.class,
		statechum.analysis.learning.TestLoadAnswers.class,
		statechum.xmachine.model.testset.TestPTA_computePrecisionRecall.class,
		statechum.AllTests.ObserversTests.class
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
		statechum.analysis.learning.observers.TestRecorderIntegration.class
	})
	public static class ObserversTests {// all tests are included in the annotation.
	}
	
	@RunWith(Suite.class)
	@Suite.SuiteClasses({
		statechum.analysis.learning.TestFSMParser.class,
		statechum.analysis.learning.TestFSMAlgo.class
	})
	public static class BasicTests {// all tests are included in the annotation.
	}

	@RunWith(Suite.class)
	@Suite.SuiteClasses({
		statechum.analysis.learning.TestGraphGeneration.class,
		statechum.xmachine.model.testset.TestPrefixRemovingCollection.class,
		statechum.xmachine.model.testset.TestPTATestSequenceEngine.class,
		statechum.analysis.learning.TestTransform322.class,
		statechum.xmachine.model.testset.TestWMethod.class
	})
	public static class GraphTests {// all tests are included in the annotation.
	}

}
