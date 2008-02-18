/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of statechum.

statechum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Foobar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * @author Kirill
 * 
 */
@RunWith(Suite.class)
@Suite.SuiteClasses( { statechum.AllTests.LearnerTests.class,
		statechum.AllTests.FSMTests.class,
		statechum.AllTests.WmethodTests.class,
		statechum.AllTests.BasicGraphTests.class,
		statechum.TestArrayOperations.class })
public class AllTests {
	@RunWith(Suite.class)
	@Suite.SuiteClasses( {
			statechum.analysis.learning.TestPTAConstruction.class,
			statechum.analysis.learning.TestRpniLearner.class,
			statechum.analysis.learning.TestLoadAnswers.class,
			statechum.xmachine.model.testset.TestPTA_computePrecisionRecall.class, })
	public static class LearnerTests {
	}

	@RunWith(Suite.class)
	@Suite.SuiteClasses( { statechum.analysis.learning.TestFSMParser.class,
			statechum.analysis.learning.TestFSMAlgo.class, })
	public static class FSMTests {
	}

	@RunWith(Suite.class)
	@Suite.SuiteClasses( {
			statechum.xmachine.model.testset.TestPrefixRemovingCollection.class,
			statechum.xmachine.model.testset.TestPTATestSequenceEngine.class,
			statechum.xmachine.model.testset.TestWMethod.class, })
	public static class WmethodTests {
	}

	@RunWith(Suite.class)
	@Suite.SuiteClasses( {
			statechum.analysis.learning.TestGraphBasicAlgorithms.class,
			statechum.analysis.learning.TestGraphGeneration.class, })
	public static class BasicGraphTests {
	}
}
