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
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import statechum.Configuration;
import statechum.Configuration.EXPANSIONOFANY;
import statechum.Configuration.LABELKIND;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.Label;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.Erlang.ErlangRuntime;
import statechum.analysis.Erlang.Synapse;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.observers.LearningConvergenceObserver;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.apps.ErlangQSMOracle;
import statechum.apps.QSMTool;
import statechum.model.testset.PTASequenceEngine;

/**
 *
 * @author ramsay
 */
public class TestErlangOracleLearner {
	public final String ErlangExamples = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_ERLANGEXAMPLES);
	protected Configuration config = Configuration.getDefaultConfiguration().copy();
	
	@Before
	public void beforeTest()
	{
		ErlangModule.flushRegistry();
		ErlangRuntime.getDefaultRuntime().killErlang();
		
		ErlangRuntime.getDefaultRuntime().startRunner();config.setErlangMboxName(ErlangRuntime.getDefaultRuntime().createNewRunner().getRunnerName());
	}

	@After
	public void afterTest()
	{
		ErlangModule.flushRegistry();
		ErlangRuntime.getDefaultRuntime().killErlang();
	}
	
	public void testLockerLearning(Configuration configToUse)
	{
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(configToUse);ErlangModule.setupErlangConfiguration(learnerConfig.config,new File(ErlangExamples,"locker/locker.erl"));
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
	public void testLockerLearning()
	{
		testLockerLearning(config);
	}
/*
	@Test
	public void testLockerLearningBenchmark_15sec()
	{
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(config);ErlangModule.setupErlangConfiguration(learnerConfig.config,new File(ErlangExamples,"locker/locker.erl"));
		learnerConfig.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
		//learnerConfig.config.setScoreForAutomergeUponRestart(1);
		final ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
		learner.GenerateInitialTraces();
		final LearnerGraph locker = learner.learnMachine();
		Map<CmpVertex,List<Label>> paths=locker.pathroutines.computeShortPathsToAllStates();
		int maxLen=0;List<Label> pathOfInterest=null;
		for(List<Label> l:paths.values())
			if (l.size() > maxLen)
			{
				maxLen = l.size();pathOfInterest=l;
			}
		
		final List<Label> path = pathOfInterest;

		final java.util.concurrent.atomic.AtomicBoolean b=new java.util.concurrent.atomic.AtomicBoolean(true);
		final java.util.concurrent.atomic.AtomicInteger counter = new java.util.concurrent.atomic.AtomicInteger(0);
		final long time = 15000;
		new Thread(new Runnable() {
			
			@Override
			public void run() {
				try {
					Thread.sleep(time);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
				b.set(false);
			}
		}).start();
		while(b.get())
		{
			learner.CheckWithEndUser(locker, path, 0, null, null, null);counter.addAndGet(1);
		}
		System.out.println("Got "+(counter.get()*1000/time)+" queries per sec");
	}
*/
	
	@Test
	public void testLockerLearning_withRestartCounter()
	{
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(config);ErlangModule.setupErlangConfiguration(learnerConfig.config,new File(ErlangExamples,"locker/locker.erl"));
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
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(config);ErlangModule.setupErlangConfiguration(learnerConfig.config,new File(ErlangExamples,"locker/locker.erl"));
		learnerConfig.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
		learnerConfig.config.setUseErlangOutputs(false);
		ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
		
		learner.GenerateInitialTraces();
		LearnerGraph locker = learner.learnMachine();
		Assert.assertEquals(4,locker.getStateNumber());
		Assert.assertEquals(6,locker.pathroutines.computeAlphabet().size());
		Assert.assertEquals(18,locker.pathroutines.countEdges());
	}

	public void testExporterLearning(Configuration configToUse)
	{
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(configToUse);ErlangModule.setupErlangConfiguration(learnerConfig.config,new File(ErlangExamples,"exporter/exporter.erl"));
		learnerConfig.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
		learnerConfig.config.setUseErlangOutputs(true);learnerConfig.config.setErlangCompileIntoBeamDirectory(true);
		ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
		learner.GenerateInitialTraces();
		LearnerGraph exporter = learner.learnMachine();
		Assert.assertEquals(6,exporter.getStateNumber());
		Assert.assertEquals(7,exporter.pathroutines.computeAlphabet().size());
		Assert.assertEquals(34,exporter.pathroutines.countEdges());
	}
	
	@Test
	public void testExporterLearning()
	{
		testExporterLearning(config);
	}

	/** Runs multiple locker and exporter learners concurrently. 
	 * @throws ExecutionException 
	 * @throws InterruptedException */
	@Test
	public void testConcurrentLearning()
	{
		final int ThreadNumber = ExperimentRunner.getCpuNumber();	
		ExecutorService executorService = Executors.newFixedThreadPool(ThreadNumber);

		// Stores tasks to complete.
		CompletionService<Integer> runner = new ExecutorCompletionService<Integer>(executorService);
		int taskNumber = 5;
		try
		{
			PTASequenceEngine initialTracesLocker = null, initialTracesExporter = null;
			
			{
				ErlangRuntime newRuntime = new ErlangRuntime();newRuntime.startRunner();
				//testLockerLearning(cfg);
				{
					Configuration cfg = config.copy();cfg.setErlangMboxName(newRuntime.createNewRunner().getRunnerName());
					LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(cfg);ErlangModule.setupErlangConfiguration(learnerConfig.config,new File(ErlangExamples,"locker/locker.erl"));
					learnerConfig.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
					//learnerConfig.config.setScoreForAutomergeUponRestart(1);
					ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
					initialTracesLocker = learner.GenerateInitialTraces(5);
				}
				
				{
					Configuration cfg = config.copy();cfg.setErlangMboxName(newRuntime.createNewRunner().getRunnerName());
					LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(cfg);ErlangModule.setupErlangConfiguration(learnerConfig.config,new File(ErlangExamples,"exporter/exporter.erl"));
					learnerConfig.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
					learnerConfig.config.setUseErlangOutputs(true);learnerConfig.config.setErlangCompileIntoBeamDirectory(true);
					ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
					initialTracesExporter = learner.GenerateInitialTraces(5);
				}
			}
			
			final PTASequenceEngine initialTracesLockerFinal = initialTracesLocker, initialTracesExporterFinal = initialTracesExporter;
			for(int i=0;i< taskNumber;++i)
			{
				runner.submit(new Callable<Integer>(){
	
					@Override
					public Integer call() throws Exception {
						ErlangRuntime newRuntime = new ErlangRuntime();newRuntime.startRunner();
						Configuration cfg = config.copy();cfg.setErlangMboxName(newRuntime.createNewRunner().getRunnerName());

						LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(cfg);ErlangModule.setupErlangConfiguration(learnerConfig.config,new File(ErlangExamples,"locker/locker.erl"));
						learnerConfig.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
						//learnerConfig.config.setScoreForAutomergeUponRestart(1);
						ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
						learner.init(initialTracesLockerFinal, 0, 0);
						
						Assert.assertEquals(237,learner.getTentativeAutomaton().getStateNumber());
						Assert.assertEquals(11,learner.getTentativeAutomaton().pathroutines.computeAlphabet().size());
						Assert.assertEquals(236,learner.getTentativeAutomaton().pathroutines.countEdges());
						LearnerGraph locker = learner.learnMachine();
						Assert.assertEquals(6,locker.getStateNumber());
						Assert.assertEquals(11,locker.pathroutines.computeAlphabet().size());
						Assert.assertEquals(51,locker.pathroutines.countEdges());
						
						return 0;
					}});
				runner.submit(new Callable<Integer>(){
	
					@Override
					public Integer call() throws Exception {
						ErlangRuntime newRuntime = new ErlangRuntime();newRuntime.startRunner();
						Configuration cfg = config.copy();cfg.setErlangMboxName(newRuntime.createNewRunner().getRunnerName());
						
						LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(cfg);ErlangModule.setupErlangConfiguration(learnerConfig.config,new File(ErlangExamples,"exporter/exporter.erl"));
						learnerConfig.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
						learnerConfig.config.setUseErlangOutputs(true);learnerConfig.config.setErlangCompileIntoBeamDirectory(true);
						ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
						learner.init(initialTracesExporterFinal,0,0);
						LearnerGraph exporter = learner.learnMachine();
						Assert.assertEquals(6,exporter.getStateNumber());
						Assert.assertEquals(7,exporter.pathroutines.computeAlphabet().size());
						Assert.assertEquals(34,exporter.pathroutines.countEdges());
						
						return 0;
					}});
			}
			
			for(int i=0;i< taskNumber;++i)
			{
				Assert.assertEquals(0,runner.take().get().intValue());
				Assert.assertEquals(0,runner.take().get().intValue());
			}
		}
		catch(Exception ex)
		{
			Helper.throwUnchecked("concurrent test failed", ex);
		}
		finally
		{
			executorService.shutdown();
		}
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
				Label lblInit = AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",init,AnyWibble}", learnerInitConfiguration.config,learnerInitConfiguration.getLabelConverter()),
				lblLock = AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",call,lock}", learnerInitConfiguration.config,learnerInitConfiguration.getLabelConverter()),
				lblUnlock = AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",call,unlock}", learnerInitConfiguration.config,learnerInitConfiguration.getLabelConverter()),
				lblCast = AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",cast,AnyWibble}", learnerInitConfiguration.config,learnerInitConfiguration.getLabelConverter()),
				lblRead = AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",call, read}", learnerInitConfiguration.config,learnerInitConfiguration.getLabelConverter()),
				lblWrite = AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",call,{write,AnyWibble}}", learnerInitConfiguration.config,learnerInitConfiguration.getLabelConverter());
				
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
		ErlangModule.setupErlangConfiguration(config,new File(lockerFile));
        config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);
        config.setErlangCompileIntoBeamDirectory(true);
		ErlangModule mod = ErlangModule.loadModule(config);
		Set<ErlangLabel> alphabetA = new TreeSet<ErlangLabel>();alphabetA.addAll(mod.behaviour.getAlphabet());
		ErlangModule.flushRegistry();

		ErlangModule.setupErlangConfiguration(config,new File(lockerFile));
		config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);config.setErlangCompileIntoBeamDirectory(true);
		ErlangModule modSame = ErlangModule.loadModule(config);
		Assert.assertTrue(alphabetA.equals(modSame.behaviour.getAlphabet()));// check that the same alphabet will be loaded second time.
		ErlangModule.flushRegistry();
		
		createLogFileFromExistingLog("earlier_failure2.txt");
		ErlangOracleLearner learner = ErlangQSMOracle.createLearner(null,modifiedLogFile);
		Set<ErlangLabel> alphabetFull = new TreeSet<ErlangLabel>();alphabetFull.addAll(ErlangModule.findModule("locker").behaviour.getAlphabet());
		alphabetFull.removeAll(alphabetA);
		Assert.assertEquals(3,alphabetFull.size());
		Assert.assertTrue(alphabetFull.contains(AbstractLearnerGraph.generateNewLabel("{call, lock ,{ok,locked}}", learner.config,learner.getLabelConverter())));
		Assert.assertTrue(alphabetFull.contains(AbstractLearnerGraph.generateNewLabel("{call, read ,-1}", learner.config,learner.getLabelConverter())));
		Assert.assertTrue(alphabetFull.contains(AbstractLearnerGraph.generateNewLabel("{call, unlock ,{ok,unlocked}}", learner.config,learner.getLabelConverter())));
	}
	
	@Test
	/** Tests that types of functions can be overridden, rather important for testing using OTP15 where typer works rather better than in OTP16. */
	public void testLearnFrequencyServer() throws IOException
	{
		LearnerEvaluationConfiguration learnerConfig = new LearnerEvaluationConfiguration(config);
		ErlangModule.setupErlangConfiguration(learnerConfig.config,new File(ErlangExamples+"/frequency/frequencyBroken.erl"));
		learnerConfig.config.setErlangAlphabetAnyElements(EXPANSIONOFANY.ANY_WIBBLE);learnerConfig.config.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		learnerConfig.config.setErlangInitialTraceLength(3);
		learnerConfig.config.setErlangStripModuleNamesFromFunctionsInNonGenModules(true);
		
		ErlangModule mod = ErlangModule.loadModule(learnerConfig.config);
		OtpErlangTuple startFunType=mod.sigTypes.get("frequencyBroken:start/0");
		Map<String,OtpErlangTuple> override = new TreeMap<String,OtpErlangTuple>();
		override.put("frequencyBroken:start/0", new OtpErlangTuple(new OtpErlangObject[]{startFunType.elementAt(0),startFunType.elementAt(1),startFunType.elementAt(2),startFunType.elementAt(3),
				ErlangLabel.parseText("{'Func',[],[],{'Any',[]}}")}));
		mod.rebuildSigs(learnerConfig.config, override);
		mod.behaviour.generateAlphabet(config);
		ErlangOracleLearner learner = new ErlangOracleLearner(null,learnerConfig);
		LearnerGraph frequencyE = new LearnerGraph(learnerConfig.config);
		learner.init(learner.GenerateInitialTraces(learnerConfig.config.getErlangInitialTraceLength()),0,0);
		
		LearnerGraph frequencyRaw = learner.learnMachine();
		Synapse.StatechumProcess.convertLabelsToStrings(frequencyRaw, frequencyE);
		config.setLabelKind(LABELKIND.LABEL_STRING);config.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		LearnerGraph frequency = new LearnerGraph(config);Synapse.StatechumProcess.parseStatemachine(Synapse.StatechumProcess.constructFSM(frequencyE), frequency, null, true);
		//System.out.println(Synapse.StatechumProcess.constructFSM(frequencyE).elementAt(2));
		LearnerGraph freq2=new LearnerGraph(config);
		Synapse.StatechumProcess.parseStatemachine(ErlangLabel.parseText("{statemachine,['P1000','N1000','P1001','P1002','P1005'],["+
"{'P1000','allocate/0,[],{ok,\\'AnyWibble\\'}','N1000'},"+
"{'P1000','allocate/0,[],{ok,\\'WibbleA\\'}','N1000'},"+
"{'P1000','allocate/0,[],{ok,\\'WobbleA\\'}','N1000'},"+
"{'P1000','deallocate/1,[\\'AnyWibble\\'],\\'AnyWibble\\'','N1000'},"+
"{'P1000','deallocate/1,[\\'AnyWibble\\'],ok','N1000'},"+
"{'P1000','init/0,[],{reply,ok}','N1000'},"+
"{'P1000','init/0,[],{reply,{ok,\\'AnyWibble\\'}}','N1000'},"+
"{'P1000','start/0,[],\\'AnyWibble\\'','N1000'},"+
"{'P1000','start/0,[],true','P1001'},"+
"{'P1000','stop/0,[],\\'AnyWibble\\'','N1000'},"+
"{'P1000','stop/0,[],ok','N1000'},"+
"{'P1001','allocate/0,[],{ok,\\'AnyWibble\\'}','N1000'},"+
"{'P1001','allocate/0,[],{ok,\\'WibbleA\\'}','P1002'},"+
"{'P1001','allocate/0,[],{ok,\\'WobbleA\\'}','N1000'},"+
"{'P1001','deallocate/1,[\\'AnyWibble\\'],\\'AnyWibble\\'','N1000'},"+
"{'P1001','deallocate/1,[\\'AnyWibble\\'],ok','P1001'},"+
"{'P1001','init/0,[],{reply,ok}','N1000'},"+
"{'P1001','init/0,[],{reply,{ok,\\'AnyWibble\\'}}','N1000'},"+
"{'P1001','start/0,[],\\'AnyWibble\\'','N1000'},"+
"{'P1001','start/0,[],true','N1000'},"+
"{'P1001','stop/0,[],\\'AnyWibble\\'','N1000'},"+
"{'P1001','stop/0,[],ok','P1000'},"+
"{'P1002','allocate/0,[],{ok,\\'AnyWibble\\'}','N1000'},"+
"{'P1002','allocate/0,[],{ok,\\'WibbleA\\'}','N1000'},"+
"{'P1002','allocate/0,[],{ok,\\'WobbleA\\'}','P1005'},"+
"{'P1002','deallocate/1,[\\'AnyWibble\\'],\\'AnyWibble\\'','N1000'},"+
"{'P1002','deallocate/1,[\\'AnyWibble\\'],ok','P1002'},"+
"{'P1002','init/0,[],{reply,ok}','N1000'},"+
"{'P1002','init/0,[],{reply,{ok,\\'AnyWibble\\'}}','N1000'},"+
"{'P1002','start/0,[],\\'AnyWibble\\'','N1000'},"+
"{'P1002','start/0,[],true','N1000'},"+
"{'P1002','stop/0,[],\\'AnyWibble\\'','N1000'},"+
"{'P1002','stop/0,[],ok','P1000'},"+
"{'P1005','allocate/0,[],{ok,\\'AnyWibble\\'}','N1000'},"+
"{'P1005','allocate/0,[],{ok,\\'WibbleA\\'}','N1000'},"+
"{'P1005','allocate/0,[],{ok,\\'WobbleA\\'}','N1000'},"+
"{'P1005','init/0,[],{reply,ok}','N1000'},"+
"{'P1005','init/0,[],{reply,{ok,\\'AnyWibble\\'}}','N1000'},"+
"{'P1005','start/0,[],\\'AnyWibble\\'','N1000'},"+
"{'P1005','start/0,[],true','N1000'},"+
"{'P1005','stop/0,[],\\'AnyWibble\\'','N1000'},"+
"{'P1005','stop/0,[],ok','P1000'}"+
"],"+
"'P1000',"+
"['init/0,[],{reply,{ok,\\'AnyWibble\\'}}','deallocate/1,[\\'AnyWibble\\'],\\'AnyWibble\\'','stop/0,[],ok','stop/0,[],\\'AnyWibble\\'','start/0,[],\\'AnyWibble\\'','allocate/0,[],{ok,\\'WibbleA\\'}','start/0,[],true','deallocate/1,[\\'AnyWibble\\'],ok','init/0,[],{reply,ok}','allocate/0,[],{ok,\\'WobbleA\\'}','allocate/0,[],{ok,\\'AnyWibble\\'}']"+
				"}"),
				freq2,learnerConfig.getLabelConverter(),true);
		
		DifferentFSMException ex = WMethod.checkM(frequency, freq2);
		if (ex != null)
			Assert.assertNull(ex.getMessage(),ex);
		Assert.assertEquals(frequency.getStateNumber(),freq2.getStateNumber());
	}
}
