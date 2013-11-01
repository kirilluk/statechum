package statechum.analysis.Erlang;

import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.experiments.ExperimentRunner;

public class TestSynapse {

	protected ErlangRunner erl = null;
	
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
		erl = new ErlangRunner();erl.setTimeout(500);
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
		erl.killErlang();
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
		Assert.assertEquals("'ok'",ErlangLabel.dumpErlangObject(erl.evaluateString("testsynapselauncher:test()")));		
	}
	
	@Test
	public void testRunSynapse1()
	{
		String java = (System.getProperty("java.home")+File.separator+"bin/java").replace(File.separatorChar,'/');
		Assert.assertEquals("\"completed\"",ErlangLabel.dumpErlangObject(erl.evaluateString("synapselauncher:launch(\""+java+"\",\".\",[{'-DOtpConnection.trace','0'}],false)")));
	}
}
