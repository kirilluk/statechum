package statechum.analysis.Erlang;

import static statechum.Helper.checkForCorrectException;

import java.io.File;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.experiments.ExperimentRunner;

public class TestSynapse {

	protected ErlangRuntime erlRuntime = null;
	
	/** This one is used to number work directories so that different tests do not affect each other. Unfortunately, the numbering is sequential hence it is not known which test corresponds to which number. */
	protected static int number = 0;
	
	/** URL of the writable directory to be used for tests. */
	public File testDir = null;
	
	/** URL of the locker example. */
	public final File erlangLocker = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_ERLANGEXAMPLES),"locker"+File.separator+"locker.erl");
	
	protected ErlangRunner runner;
	
	File output = null;

	@Before
	public void beforeTest()
	{
		erlRuntime = new ErlangRuntime();erlRuntime.setTimeout(500);erlRuntime.startRunner();runner = erlRuntime.createNewRunner();
		testDir = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP),"__TestErlangRunner__"+(number++));
		output = new File(testDir,"test.erl");
		createTestDir();
	}
	
	public void createTestDir()
	{
		if (!testDir.isDirectory())
		{
			Assert.assertTrue("could not create "+testDir.getAbsolutePath(),testDir.mkdir());
		}
	}

	@After
	public void afterTest()
	{
		erlRuntime.killErlang();
		zapTestDir();
	}
	
	public void zapTestDir()
	{
		ExperimentRunner.zapDir(testDir);
	}

	public TestSynapse() {
		
	}
	
	@Test
	public void testEunitSynapseTests()
	{
		Assert.assertEquals("'ok'",ErlangLabel.dumpErlangObject(runner.evaluateString("testsynapselauncher:test()")));		
	}
	
	/** No statechum running */
	@Test
	public void testFindSynapseFailure()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.dumpErlangObject(runner.evaluateString("synapselauncher:find_statechum()"));
		}},RuntimeException.class,"{spawn_executable,\"aa\"}");
	}

	/** Invalid options before launching. */
	@Test
	public void testRunSynapseLaunchFailure0()
	{
		String java = (System.getProperty("java.home")+File.separator+"bin/java").replace(File.separatorChar,'/');
		Assert.assertTrue(
				runner.evaluateString("process_flag(trap_exit, true),"+
						"spawn_link(fun() -> synapselauncher:startStatechum([{'Java','"+java+"'},{'JavaOptionsList',[{'-DOtpConnection.trace','0'}] },{'AccumulateOutput','false'},[{'pp','qq'}] ]) end)," // this will fail if we cannot start Erlang
						+ "Response = receive Arg -> Arg end,"
						+ "process_flag(trap_exit, false),Response").toString().contains("Tuple is not key-value pair"));
	}

	/** Invalid executable */
	@Test
	public void testRunSynapseLaunchFailure1()
	{
		Assert.assertTrue(
			runner.evaluateString("process_flag(trap_exit, true),"+
					"spawn_link(fun() -> synapselauncher:startStatechum([{'Java','aa'},{'JavaOptionsList',[{'-DOtpConnection.trace','0'}] },{'AccumulateOutput','false'} ]) end)," // this will fail if we cannot start Erlang
					+ "Response = receive Arg -> Arg end,"
					+ "process_flag(trap_exit, false),Response").toString().contains("spawn_executable,aa"));
	}

	/** Invalid options to executable */
	@Test
	public void testRunSynapseLaunchFailure2()
	{
		String java = (System.getProperty("java.home")+File.separator+"bin/java").replace(File.separatorChar,'/');
		Assert.assertTrue(
				runner.evaluateString("process_flag(trap_exit, true),"+
						"spawn_link(fun() -> synapselauncher:startStatechum([{'Java','"+java+"'},{'JavaOptionsList',[{'-DOtpConnection.trace','0'},{'pp','qq'}] },{'AccumulateOutput','false'} ]) end)," // this will fail if we cannot start Erlang
						+ "Response = receive Arg -> Arg end,"
						+ "process_flag(trap_exit, false),Response").toString().contains("Timeout waiting for node"));
	}
	
	
	@Test
	public void testRunSynapse1()
	{
		String java = (System.getProperty("java.home")+File.separator+"bin/java").replace(File.separatorChar,'/');
		String response = ErlangLabel.dumpErlangObject(runner.evaluateString("synapselauncher:startStatechum([{'Java','"+java+"'},{'JavaOptionsList',[{'-DOtpConnection.trace','0'}] },{'AccumulateOutput','true'}]),"
				+ "synapselauncher:find_statechum()!terminate," //io:format(\"waiting for response~n\"),"
				+ "receive Arg -> Arg end"));
		Assert.assertTrue(response.contains("Synapse started"));Assert.assertTrue(response.contains("Synapse terminated"));
	}
}
