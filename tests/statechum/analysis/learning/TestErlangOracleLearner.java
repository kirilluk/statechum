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
import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.Configuration.LABELKIND;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;

/**
 *
 * @author ramsay
 */
public class TestErlangOracleLearner {

	@Test
	public void testLockerLearning()
	{
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		//learnerConfig.config.setScoreForAutomergeUponRestart(1);
		learnerConfig.config.setLabelKind(LABELKIND.LABEL_ERLANG);
		learnerConfig.config.setErlangModuleName("locker");
		learnerConfig.config.setErlangSourceFile("ErlangExamples/locker/locker.erl");
		ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
		
		learner.GenerateInitialTraces();
		LearnerGraph locker = learner.learnMachine();
		Assert.assertEquals(6,locker.getStateNumber());
		Assert.assertEquals(12,locker.pathroutines.computeAlphabet().size());
		Assert.assertEquals(56,locker.pathroutines.countEdges());
	}
	
	@Test
	public void testLockerLearningWithoutOutputMatching()
	{
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		learnerConfig.config.setLabelKind(LABELKIND.LABEL_ERLANG);
		learnerConfig.config.setErlangModuleName("locker");
		learnerConfig.config.setErlangSourceFile("ErlangExamples/locker/locker.erl");
		learnerConfig.config.setUseErlangOutputs(false);
		ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
		
		learner.GenerateInitialTraces();
		LearnerGraph locker = learner.learnMachine();
		Assert.assertEquals(4,locker.getStateNumber());
		Assert.assertEquals(7,locker.pathroutines.computeAlphabet().size());
		Assert.assertEquals(21,locker.pathroutines.countEdges());
	}
	
    /*
     * Test the askErlang function, which should call the Erlang tracer process to request a first_failure evaluation
     */
    public final void testAskErlang1() {
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
        OtpErlangTuple response = null;//ErlangOracleLearner.askErlang(module, wrapper, testTrace);
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
    }
}
