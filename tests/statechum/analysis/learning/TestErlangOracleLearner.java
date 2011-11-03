/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.learning;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.Configuration.EXPANSIONOFANY;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Helper;
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
	@Before
	public void beforeTest()
	{
		ErlangModule.flushRegistry();
	}
	
	@Test
	public void testLockerLearning()
	{
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(
				ErlangModule.setupErlangConfiguration(new File("ErlangExamples/locker/locker.erl")));
		learnerConfig.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
		//learnerConfig.config.setScoreForAutomergeUponRestart(1);
		ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
		learner.GenerateInitialTraces();
		LearnerGraph locker = learner.learnMachine();
		Assert.assertEquals(6,locker.getStateNumber());
		Assert.assertEquals(11,locker.pathroutines.computeAlphabet().size());
		Assert.assertEquals(51,locker.pathroutines.countEdges());
	}
	
	@Test
	public void testLockerLearningWithoutOutputMatching()
	{
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(
				ErlangModule.setupErlangConfiguration(new File("ErlangExamples/locker/locker.erl")));
		learnerConfig.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
		learnerConfig.config.setUseErlangOutputs(false);
		ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
		
		learner.GenerateInitialTraces();
		LearnerGraph locker = learner.learnMachine();
		Assert.assertEquals(4,locker.getStateNumber());
		Assert.assertEquals(6,locker.pathroutines.computeAlphabet().size());
		Assert.assertEquals(18,locker.pathroutines.countEdges());
	}
	
	@Test
	public void testExporterLearning()
	{
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(
				ErlangModule.setupErlangConfiguration(new File("ErlangExamples/exporter/exporter.erl")));
		learnerConfig.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
		learnerConfig.config.setUseErlangOutputs(true);
		ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
		learner.GenerateInitialTraces();
		LearnerGraph exporter = learner.learnMachine();
		Assert.assertEquals(6,exporter.getStateNumber());
		Assert.assertEquals(7,exporter.pathroutines.computeAlphabet().size());
		Assert.assertEquals(34,exporter.pathroutines.countEdges());
	}

	@Test
	public void testLearningFromErlangTraceFile()
	{
        QSMTool tool = new QSMTool() {
            @Override
			public void runExperiment() {
                setSimpleConfiguration(learnerInitConfiguration.config, active, k);
                learnerInitConfiguration.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
                try {
					ErlangModule.loadModule(learnerInitConfiguration.config);
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
	
	public static final String lockerFile = "ErlangExamples/locker/locker.erl";
	
	@Test
	public void testLearningFromErlangTraceFile2() throws IOException
	{
		Configuration config = ErlangModule.setupErlangConfiguration(new File(lockerFile));
        config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
		ErlangModule mod = ErlangModule.loadModule(config);
		Set<ErlangLabel> alphabetA = new TreeSet<ErlangLabel>();alphabetA.addAll(mod.behaviour.getAlphabet());
		ErlangModule.flushRegistry();
		config = ErlangModule.setupErlangConfiguration(new File(lockerFile));
        config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
		ErlangModule modSame = ErlangModule.loadModule(config);
		Assert.assertTrue(alphabetA.equals(modSame.behaviour.getAlphabet()));// check that the same alphabet will be loaded second time.
		ErlangModule.flushRegistry();
		
		ErlangOracleLearner learner = ErlangQSMOracle.createLearner(null,"resources/earlier_failure2.txt");
		Set<ErlangLabel> alphabetFull = new TreeSet<ErlangLabel>();alphabetFull.addAll(ErlangModule.findModule("locker").behaviour.getAlphabet());
		alphabetFull.removeAll(alphabetA);
		Assert.assertEquals(3,alphabetFull.size());
		Assert.assertTrue(alphabetFull.contains(AbstractLearnerGraph.generateNewLabel("{call, lock ,{ok,locked}}", learner.config)));
		Assert.assertTrue(alphabetFull.contains(AbstractLearnerGraph.generateNewLabel("{call, read ,-1}", learner.config)));
		Assert.assertTrue(alphabetFull.contains(AbstractLearnerGraph.generateNewLabel("{call, unlock ,{ok,unlocked}}", learner.config)));
	}
}
