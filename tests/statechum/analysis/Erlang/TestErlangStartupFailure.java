package statechum.analysis.Erlang;

import static statechum.Helper.checkForCorrectException;

import java.io.File;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper.whatToRun;

public class TestErlangStartupFailure 
{
	protected ErlangRuntime erlRuntime = null;
	
	/** This one is used to number work directories so that different tests do not affect each other. Unfortunately, the numbering is sequential hence it is not known which test corresponds to which number. */
	protected static int number = 0;

	/** URL of the writable directory to be used for tests. */
	public File testDir = null;
	
	/** URL of the locker example. */
	public final File erlangLocker = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_ERLANGEXAMPLES),"locker"+File.separator+"locker.erl");
	
	File output = null;
	
	@Before
	public void beforeTest()
	{
		erlRuntime = new ErlangRuntime();erlRuntime.setTimeout(100);
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

	@Test
	public void testStartErlangFailure1a()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			erlRuntime.startErlang("halt", 0);
		}},IllegalArgumentException.class,"timeout");
	}
	
	@Test
	public void testStartErlangFailure1b()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			erlRuntime.startErlang("halt", 1000);
		}},IllegalArgumentException.class,"timeout");
	}
	
	@Test
	public void testStartErlangFailure2a()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			erlRuntime.startErlang("error", 0);
		}},IllegalArgumentException.class,"timeout");
	}
	
	@Test
	public void testStartErlangFailure2b()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			erlRuntime.startErlang("error", 1000);
		}},IllegalArgumentException.class,"timeout");
	}
	
	@Test
	public void testErlangResponseTimeout1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			erlRuntime.startErlang("noserver", 0);// this one makes a call to echo and then to listprocesses and hence times out if the server does not start
			//makeCall(erl);
		}},IllegalArgumentException.class,"timeout");
	}
	
	@Test
	public void testCreationOfRunnerFailure()
	{
		// Check that initially no node can be created.
		checkForCorrectException(new whatToRun() { public @Override void run() {
			erlRuntime.createNewRunner();
		}},IllegalArgumentException.class,"is not running");
		
		
		// Now attempt to start Erlang fails
		checkForCorrectException(new whatToRun() { public @Override void run() {
			erlRuntime.startErlang("error", 1000);
		}},IllegalArgumentException.class,"timeout");
		
		
		// Still no runner
		checkForCorrectException(new whatToRun() { public @Override void run() {
			erlRuntime.createNewRunner();
		}},IllegalArgumentException.class,"is not running");
	}
	
	@Test
	public void testCreationOfRunnerFailureAfterDeathOfRuntime()
	{
		// Success creating a runner
		erlRuntime.startRunner();
		// Can obtain runner
		final ErlangRunner runner = erlRuntime.createNewRunner();
		Assert.assertNotNull(runner);
		
		// Kill Erlang
		erlRuntime.killErlang();
		
		// No new runner can be created
		checkForCorrectException(new whatToRun() { public @Override void run() {
			erlRuntime.createNewRunner();
		}},IllegalArgumentException.class,"is not running");
		
		// Communication using old runner fails.
		checkForCorrectException(new whatToRun() { public @Override void run() {
			runner.evaluateString("25");
		}},IllegalArgumentException.class,"closed");
	}
}
