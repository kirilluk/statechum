/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

/**
 *
 * @author ramsay
 */
@RunWith(Suite.class)
@SuiteClasses({
    statechum.analysis.Erlang.ErlangTestSuite.ErlangTests.class
})
public class ErlangTestSuite {

    @RunWith(Suite.class)
    @SuiteClasses({
        //statechum.analysis.Erlang.TestErlangModule.class,
        //statechum.analysis.Erlang.TestErlangRunner.class,
        statechum.analysis.learning.TestErlangOracleLearner.class
    })
    public static class ErlangTests {// all tests are included in the annotation.
    }
}
