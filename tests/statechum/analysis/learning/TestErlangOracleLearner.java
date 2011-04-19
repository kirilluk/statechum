/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.learning;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import org.junit.Test;
import java.util.LinkedList;
import org.junit.Assert;

/**
 *
 * @author ramsay
 */
public class TestErlangOracleLearner {

    /*
     * Test that we can start an Erlang process without locking the whole system...
     */
    @Test
    public final void testStartErlang() {
        ErlangOracleLearner.startErlang();
        Assert.assertNotNull(ErlangOracleLearner.erlangProcess);
        ErlangOracleLearner.killErlang();
    }
    /*
     * Test the askErlang function, which should call the Erlang tracer process to request a first_failure evaluation
     */

    @Test
    public final void testAskErlang1() {
        ErlangOracleLearner.startErlang();
        String module = "locker";
        String wrapper = "gen_server_wrapper";
        OtpErlangList testTrace = new OtpErlangList(
                new OtpErlangObject[]{
                    // [{init,[]},{cast,stop}]
                    new OtpErlangTuple(
                    new OtpErlangObject[]{
                        new OtpErlangAtom("init"),
                        new OtpErlangList(new OtpErlangObject[0])
                    }),
                    new OtpErlangTuple(
                    new OtpErlangObject[]{
                        new OtpErlangAtom("cast"),
                        new OtpErlangAtom("stop")
                    })
                });
        OtpErlangTuple response = ErlangOracleLearner.askErlang(module, wrapper, testTrace);
        Assert.assertNotNull(response);
        // {ok,[{init,[]},{cast,stop}], [{"locker.8",1},{"locker.29",1},{"locker.34",1}]}
        OtpErlangTuple expected = new OtpErlangTuple(
                new OtpErlangObject[]{
                    new OtpErlangAtom("ok"),
                    testTrace,
                    new OtpErlangList(new OtpErlangObject[]{
                        new OtpErlangTuple(new OtpErlangObject[]{
                            new OtpErlangString("locker.8"),
                            new OtpErlangInt(1)
                        }),
                        new OtpErlangTuple(new OtpErlangObject[]{
                            new OtpErlangString("locker.29"),
                            new OtpErlangInt(1)
                        }),
                        new OtpErlangTuple(new OtpErlangObject[]{
                            new OtpErlangString("locker.34"),
                            new OtpErlangInt(1)
                        })
                    })
                });

        Assert.assertEquals(expected, response);
        ErlangOracleLearner.killErlang();
    }
}
