/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.learning;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
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
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.Label;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.Erlang.ErlangRunner;
import statechum.analysis.learning.observers.LearningConvergenceObserver;
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
	public final String ErlangExamples = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_ERLANGEXAMPLES);
	
	@Before
	public void beforeTest()
	{
		ErlangModule.flushRegistry();
		ErlangRunner.getRunner().killErlang();
	}
	
	@Test
	public void testLockerLearning()
	{
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(
				ErlangModule.setupErlangConfiguration(new File(ErlangExamples,"locker/locker.erl")));
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
	public void testLockerLearning_withRestartCounter()
	{
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(
				ErlangModule.setupErlangConfiguration(new File(ErlangExamples,"locker/locker.erl")));
		learnerConfig.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
		learnerConfig.config.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		//learnerConfig.config.setScoreForAutomergeUponRestart(1);
		ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
		Learner learnerAndObserver = new LearningConvergenceObserver(learner);
		learner.GenerateInitialTraces();
		LearnerGraph locker = learnerAndObserver.learnMachine();
		Assert.assertEquals(6,locker.getStateNumber());
		Assert.assertEquals(11,locker.pathroutines.computeAlphabet().size());
		Assert.assertEquals(51,locker.pathroutines.countEdges());
		
		List<Double> observedConvergence = ((LearningConvergenceObserver)learnerAndObserver).progressObserved;
		Assert.assertEquals(2,observedConvergence.size());
		Assert.assertEquals(0.1276595744680851,observedConvergence.get(0), Configuration.fpAccuracy);
		Assert.assertEquals(1,observedConvergence.get(1), Configuration.fpAccuracy);
	}
	
	@Test
	public void testLockerLearningWithoutOutputMatching()
	{
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(
				ErlangModule.setupErlangConfiguration(new File(ErlangExamples,"locker/locker.erl")));
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
				ErlangModule.setupErlangConfiguration(new File(ErlangExamples,"exporter/exporter.erl")));
		learnerConfig.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
		learnerConfig.config.setUseErlangOutputs(true);learnerConfig.config.setErlangCompileIntoBeamDirectory(true);
		ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
		learner.GenerateInitialTraces();
		LearnerGraph exporter = learner.learnMachine();
		Assert.assertEquals(6,exporter.getStateNumber());
		System.out.println(exporter.pathroutines.computeAlphabet());
		System.out.println(exporter.transitionMatrix);
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
                learnerInitConfiguration.config.setErlangCompileIntoBeamDirectory(true);
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
        createLogFileFromExistingLog("earlier_failure.txt");
        tool.loadConfig(modifiedLogFile);
        tool.process("config debugMode false");
        tool.runExperiment();
        
	}
	
	public final String lockerFile = ErlangExamples + "/locker/locker.erl",
			modifiedLogFile = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP)+File.separator+"modifiedLogFile";
			 	
	/** Copies a log file under a different name and replaces the name of the input file by a different name. */
	protected void createLogFileFromExistingLog(String existingName)
	{
		String name = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.RESOURCES)+File.separator+existingName;
		final String stringToReplace = " ErlangExamples/locker/locker.erl";
		boolean replacedAlready = false;
		BufferedReader input = null;
		BufferedWriter output = null;
		try
		{
			input = new BufferedReader(new FileReader(name));
			output = new BufferedWriter(new FileWriter(modifiedLogFile));
			String line = input.readLine();
		
			while(line != null)
			{
				if (line.contains(stringToReplace))
				{
					if (replacedAlready) throw new IllegalArgumentException("second mentioning of the file among traces");
					line=line.replace(stringToReplace, " "+lockerFile);
						replacedAlready = true;
				}
				output.write(line);output.write('\n');
				line = input.readLine();
			}
			input.close();output.close();
			if (!replacedAlready)
				throw new IllegalArgumentException("file was not mentioned among traces");
		}
		catch(IOException ex)
		{
			Helper.throwUnchecked("failed to copy "+name+" to "+modifiedLogFile, ex);
		}
		finally
		{
			if (input != null)
				try {
					input.close();
				} catch (IOException e) {
					// ignored
				}
			if (output != null)
				try {
					output.close();
				} catch (IOException e) {
					// ignored
				}
		}
	}
	@Test
	public void testLearningFromErlangTraceFile2() throws IOException
	{
		Configuration config = ErlangModule.setupErlangConfiguration(new File(lockerFile));
        config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
        config.setErlangCompileIntoBeamDirectory(true);
		ErlangModule mod = ErlangModule.loadModule(config);
		Set<ErlangLabel> alphabetA = new TreeSet<ErlangLabel>();alphabetA.addAll(mod.behaviour.getAlphabet());
		ErlangModule.flushRegistry();
		config = ErlangModule.setupErlangConfiguration(new File(lockerFile));
		config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);config.setErlangCompileIntoBeamDirectory(true);
		ErlangModule modSame = ErlangModule.loadModule(config);
		Assert.assertTrue(alphabetA.equals(modSame.behaviour.getAlphabet()));// check that the same alphabet will be loaded second time.
		ErlangModule.flushRegistry();
		
		createLogFileFromExistingLog("earlier_failure2.txt");
		ErlangOracleLearner learner = ErlangQSMOracle.createLearner(null,modifiedLogFile);
		Set<ErlangLabel> alphabetFull = new TreeSet<ErlangLabel>();alphabetFull.addAll(ErlangModule.findModule("locker").behaviour.getAlphabet());
		alphabetFull.removeAll(alphabetA);
		Assert.assertEquals(3,alphabetFull.size());
		Assert.assertTrue(alphabetFull.contains(AbstractLearnerGraph.generateNewLabel("{call, lock ,{ok,locked}}", learner.config)));
		Assert.assertTrue(alphabetFull.contains(AbstractLearnerGraph.generateNewLabel("{call, read ,-1}", learner.config)));
		Assert.assertTrue(alphabetFull.contains(AbstractLearnerGraph.generateNewLabel("{call, unlock ,{ok,unlocked}}", learner.config)));
	}
}
