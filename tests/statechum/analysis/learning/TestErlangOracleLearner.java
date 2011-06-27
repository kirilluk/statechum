/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.learning;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Helper;
import statechum.Configuration.LABELKIND;
import statechum.Label;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.apps.ErlangQSMOracle;
import statechum.apps.QSMTool;

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
	
	@Test
	public void testLearningFromErlangTraceFile()
	{
        QSMTool tool = new QSMTool() {
            @Override
			public void runExperiment() {
                setSimpleConfiguration(learnerInitConfiguration.config, active, k);
                try {
					ErlangModule.loadModule(learnerInitConfiguration.config.getErlangSourceFile());
				} catch (IOException e) {
					Helper.throwUnchecked("failed to load module from "+learnerInitConfiguration.config.getErlangModuleName(), e);
				}
               	Set<List<Label>> Plus = null, Minus = null;
                Plus = ErlangQSMOracle.convertTracesToErl(sPlus, learnerInitConfiguration.config);
				Minus = ErlangQSMOracle.convertTracesToErl(sMinus, learnerInitConfiguration.config);
					
				RPNIUniversalLearner learner = new ErlangOracleLearner(null, learnerInitConfiguration);
				LearnerGraph outcome = learner.learnMachine(Plus, Minus);
				LearnerGraph expectedGraph = new LearnerGraph(learnerInitConfiguration.config);
				Label lblInit = AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",init,AnyWibble}", learnerInitConfiguration.config),
				lblLock = AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",call,lock}", learnerInitConfiguration.config),
				lblUnlock = AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",call,unlock}", learnerInitConfiguration.config),
				lblCast = AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",cast,AnyWibble}", learnerInitConfiguration.config),
				lblRead = AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",call, read}", learnerInitConfiguration.config),
				lblWrite = AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",call,{write,AnyWibble}}", learnerInitConfiguration.config);
				
				expectedGraph.paths.augmentPTA(Arrays.asList(new Label[]{lblInit,lblLock}), true, false, null);
				expectedGraph.paths.augmentPTA(Arrays.asList(new Label[]{lblLock}), false, false, null);
				CmpVertex 
					init = expectedGraph.getInit(),
					P1001 = expectedGraph.getVertex(Arrays.asList(new Label[]{lblInit})),
					P1003 = expectedGraph.getVertex(Arrays.asList(new Label[]{lblInit,lblLock})),
					reject = expectedGraph.getVertex(Arrays.asList(new Label[]{lblLock}));
				expectedGraph.addTransition(expectedGraph.transitionMatrix.get(init),lblWrite,reject);
				expectedGraph.addTransition(expectedGraph.transitionMatrix.get(init),lblCast,reject);
				expectedGraph.addTransition(expectedGraph.transitionMatrix.get(init),lblRead,reject);
				expectedGraph.addTransition(expectedGraph.transitionMatrix.get(init),lblUnlock,reject);
				
				expectedGraph.addTransition(expectedGraph.transitionMatrix.get(P1001),lblCast,P1001);
				expectedGraph.addTransition(expectedGraph.transitionMatrix.get(P1001),lblInit,reject);
				expectedGraph.addTransition(expectedGraph.transitionMatrix.get(P1001),lblUnlock,reject);
				
				expectedGraph.addTransition(expectedGraph.transitionMatrix.get(P1003),lblCast,P1003);
				expectedGraph.addTransition(expectedGraph.transitionMatrix.get(P1003),lblLock,reject);

				Assert.assertNull(WMethod.checkM_and_colours(expectedGraph,outcome,WMethod.VERTEX_COMPARISON_KIND.NONE));
            }
        };
        tool.loadConfig("resources/earlier_failure.txt");
        tool.process("config debugMode false");
        tool.runExperiment();
        
	}
}
