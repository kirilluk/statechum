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
import statechum.analysis.learning.TestCheckLearnerAgainstLog;
import statechum.analysis.learning.experiments.TestPaperUASLarge;
import statechum.analysis.learning.linear.TestGDExistingGraphs;
import statechum.analysis.learning.linear.TestGDExistingGraphsND;
import statechum.analysis.learning.linear.TestGDExistingGraphsUsingTestSet;

/*
Important: when running via IntelliJ Idea, the console will not show the progress of initialisation until
a custom property editable.java.test.console=true is set (Help->Edit Custom Properties).
 */


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
    statechum.FastTests.class,
    //statechum.AllTests.TestErlangWithDifferentOTP.class,
    statechum.AllTests.LengthyTests.class
})
public class AllTests {
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
//            harmony.collections.HarmonyAbstractMapTest.class,
//            harmony.collections.HarmonyHashMapTest.class,
//            harmony.collections.HarmonyTreeMapTest.class,
//            harmony.collections.LinkedHashMapTest.class,
//            statechum.collections.TreeMapWithSearchTest.class
    })
    public static class HarmonyTests {// of these tests, only HarmonyAbstractMap and HarmonyHashMap need to be tested.
    }

    @RunWith(Suite.class)
    @SuiteClasses({
    	TestPaperUASLarge.class,
    	statechum.analysis.learning.TestStateMerging.TestRandomFSMMergers.class,
        statechum.analysis.learning.experiments.PairSelection.TestLearnFromTracesUsingMarkov.class,
        statechum.analysis.learning.experiments.PairSelection.TestMarkov_i2c.class,
        statechum.analysis.learning.experiments.mutation.TestDiffExperimentWithLogs.class,
        statechum.analysis.learning.TestLearnerFromLargePTA.class,
        statechum.analysis.learning.experiments.TestExperimentRunner.class,
        statechum.analysis.learning.rpnicore.TestCloneWithDifferentConf.class,
        TestCheckLearnerAgainstLog.class,
    	TestGDExistingGraphs.class,
        TestGDExistingGraphsUsingTestSet.class,
        TestGDExistingGraphsND.class,
        statechum.analysis.learning.linear.TestGDExistingGraphsNDUsingTestSet.class
    })
    public static class LengthyTests {// all tests are included in the annotation.
    }


}
