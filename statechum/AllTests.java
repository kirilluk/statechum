/**
 * 
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
	statechum.AllTests.BasicGraphTests.class
})
public class AllTests {
	@RunWith(Suite.class)
	@Suite.SuiteClasses({
		statechum.analysis.learning.TestPTAConstruction.class,
		statechum.analysis.learning.TestRpniLearner.class,
		statechum.analysis.learning.TestLoadAnswers.class,
		statechum.xmachine.model.testset.TestPTA_computePrecisionRecall.class,
	})
	public static class LearnerTests {
		
	}
	
	@RunWith(Suite.class)
	@Suite.SuiteClasses({
		statechum.analysis.learning.TestFSMParser.class,
		statechum.analysis.learning.TestFSMAlgo.class,
	})
	public static class FSMTests {
		
	}

	@RunWith(Suite.class)
	@Suite.SuiteClasses({
		statechum.xmachine.model.testset.TestPrefixRemovingCollection.class,
		statechum.xmachine.model.testset.TestPTATestSequenceEngine.class,
		statechum.xmachine.model.testset.TestWMethod.class,
	})
	public static class WmethodTests {
		
	}

	@RunWith(Suite.class)
	@Suite.SuiteClasses({
		statechum.analysis.learning.TestGraphBasicAlgorithms.class,
		statechum.analysis.learning.TestGraphGeneration.class,
	})
	public static class BasicGraphTests {
		
	}
}
