/* Copyright (c) 2013 The University of Sheffield.
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */
package statechum.analysis.Erlang;

import static statechum.Helper.checkForCorrectException;

import java.io.File;
import java.util.Collection;
import java.util.LinkedList;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.ParameterizedWithName;
import org.junit.runners.ParameterizedWithName.ParametersToString;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.experiments.ExperimentRunner;

@RunWith(ParameterizedWithName.class)
public class TestSynapse {

	protected ErlangRuntime erlRuntime = null, erlPingRuntime = null;
	
	/** This one is used to number work directories so that different tests do not affect each other. Unfortunately, the numbering is sequential hence it is not known which test corresponds to which number. */
	protected static int number = 0;
	
	/** URL of the writable directory to be used for tests. */
	public File testDir = null;
	
	/** URL of the locker example. */
	public final File erlangLocker = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_ERLANGEXAMPLES),"locker"+File.separator+"locker.erl");
	
	protected ErlangRunner runner, pingRunner;
	
	File output = null;

	@Before
	public void beforeTest()
	{
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.ERLANG_SHORTNODENAME, Boolean.toString(useShortNames));// resets the effect of a failed short name test.
		erlRuntime = new ErlangRuntime();erlRuntime.setTimeout(500);erlRuntime.startRunner();runner = erlRuntime.createNewRunner();
		erlPingRuntime = new ErlangRuntime();erlPingRuntime.setTimeout(500);erlPingRuntime.startRunner();pingRunner = erlPingRuntime.createNewRunner();
		
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
		if (erlRuntime != null) erlRuntime.killErlang();if (erlPingRuntime != null) erlPingRuntime.killErlang();
		zapTestDir();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.ERLANG_SHORTNODENAME, "false");// resets the effect of a failed short name test.
	}
	
	public void zapTestDir()
	{
		ExperimentRunner.zapDir(testDir);
	}

	protected final boolean useShortNames;
	protected final String javaOptions;
	
	public TestSynapse(boolean shortNames) {
		useShortNames = shortNames;
		String shortNameOption=null;
		if (useShortNames)
			shortNameOption = "{'-DERLANG_SHORTNODENAME','true'},";
		else
			shortNameOption="{'-DERLANG_SHORTNODENAME','false'},";
		
		javaOptions=shortNameOption+"{'-DOtpConnection.trace','0'}";
	}

	
	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		result.add(new Object[]{new Boolean(false)});
		result.add(new Object[]{new Boolean(true)});
		
		return result;
	}

	@ParametersToString
	public static String parametersToString(Boolean b)
	{
		if (b)
			return "Synapse short node names";

		return "Synapse long node names";
	}

	
	@Test
	public void testEunitSynapseLauncherTests()
	{
		Assert.assertEquals("'ok'",ErlangLabel.dumpErlangObject(runner.evaluateString("testsynapselauncher:test()")));		
	}
	
	@Test
	public void testEunitSynapseTests1()
	{
		runner.setResponseTimeout(20000);
		startSynapse();
		Assert.assertEquals("'ok'",ErlangLabel.dumpErlangObject(runner.evaluateString("testsynapse:test()")));		
	}
	
	@Test
	public void testEunitSynapseTests1_fsmparse()
	{
		runner.setResponseTimeout(20000);
		startSynapse();
		Assert.assertEquals("'ok'",ErlangLabel.dumpErlangObject(runner.evaluateString("test_fsmparse:test()")));		
	}
	
	@Test
	public void testEunitSynapseTests1_fsmlearn()
	{
		runner.setResponseTimeout(20000);
		startSynapse();
		Assert.assertEquals("'ok'",ErlangLabel.dumpErlangObject(runner.evaluateString("test_fsmlearn:test()")));		
	}
	
	@Test
	public void testEunitSynapseTests1_diff()
	{
		runner.setResponseTimeout(20000);
		startSynapse();
		Assert.assertEquals("'ok'",ErlangLabel.dumpErlangObject(runner.evaluateString("test_diff:test()")));		
	}
	
	@Test
	public void testEunitSynapseTests1_parsetypemap()
	{
		runner.setResponseTimeout(20000);
		startSynapse();
		Assert.assertEquals("'ok'",ErlangLabel.dumpErlangObject(runner.evaluateString("test_parsetypemap:test()")));		
	}
	
	@Test
	public void testEunitSynapseTests2()
	{
		runner.setResponseTimeout(30000);
		startSynapse();
		Assert.assertEquals("'ok'",ErlangLabel.dumpErlangObject(runner.evaluateString("testsynapse_learn:test()")));		
	}
	
	/** No statechum running */
	@Test
	public void testFindSynapseFailure()
	{
		Assert.assertEquals(new OtpErlangAtom("not_started"),runner.evaluateString("synapselauncher:find_statechum()"));
	}

	/** Invalid options before launching. */
	@Test
	public void testRunSynapseLaunchFailure0()
	{
		String java = (System.getProperty("java.home")+File.separator+"bin/java").replace(File.separatorChar,'/');
		Assert.assertTrue(
				runner.evaluateString("process_flag(trap_exit, true),"+
						"spawn_link(fun() -> synapselauncher:startStatechum([{'Java','"+java+"'},{'JavaOptionsList',["+javaOptions+"]},{'AccumulateOutput','false'},[{'pp','qq'}] ]) end)," // this will fail if we cannot start Erlang
						+ "Response = receive Arg -> Arg end,"
						+ "process_flag(trap_exit, false),Response").toString().contains("Tuple is not key-value pair"));
	}

	/** Invalid executable */
	@Test
	public void testRunSynapseLaunchFailure1()
	{
		Assert.assertTrue(
			runner.evaluateString("process_flag(trap_exit, true),"+
					"spawn_link(fun() -> synapselauncher:startStatechum([{'Java','aa'},{'JavaOptionsList',["+javaOptions+"] },{'AccumulateOutput','false'} ]) end)," // this will fail if we cannot start Erlang
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
						"spawn_link(fun() -> synapselauncher:startStatechum([{'Java','"+java+"'},{'JavaOptionsList',["+javaOptions+",{'pp','qq'}] },{'AccumulateOutput','false'} ]) end)," // this will fail if we cannot start Erlang
						+ "Response = receive Arg -> Arg end,"
						+ "process_flag(trap_exit, false),Response").toString().contains("Timeout waiting for node"));
	}
	
	@Test
	public void testRunSynapseLaunchFailure3()
	{
		String java = (System.getProperty("java.home")+File.separator+"bin/java").replace(File.separatorChar,'/');
		String response = runner.evaluateString("process_flag(trap_exit, true),"+
				"spawn_link(fun() -> synapselauncher:startStatechum([{'Java','"+java+"'},{'JavaOptionsList',["+javaOptions+",{'-DSYNAPSE_TERMINATE','true'}] },{'AccumulateOutput','false'} ]) end)," // this will fail if we cannot start Erlang
				+ "Response = receive Arg -> Arg end,"
				+ "process_flag(trap_exit, false),Response").toString();
		Assert.assertTrue(
				response.contains("Timeout waiting for echo response"));
	}

	@Test
	public void testRunSynapse1()
	{
		String java = (System.getProperty("java.home")+File.separator+"bin/java").replace(File.separatorChar,'/');
		String response = ErlangLabel.dumpErlangObject(runner.evaluateString("synapselauncher:startStatechum([{'Java','"+java+"'},{'JavaOptionsList',["+javaOptions+"] },{'AccumulateOutput','true'}]),"
				+ "synapselauncher:find_statechum()!terminate," //io:format(\"waiting for response~n\"),"
				+ "receive Arg -> Arg end"));
		Assert.assertTrue(response.contains("Synapse started"));Assert.assertTrue(response.contains("Synapse terminated"));
	}
	
	
	@Test
	public void testRunSynapse2a_longnames() throws InterruptedException
	{
		String java = (System.getProperty("java.home")+File.separator+"bin/java").replace(File.separatorChar,'/');
		String synapseNode = ErlangLabel.dumpErlangObject(runner.evaluateString("synapselauncher:startStatechum([{'Java','"+java+"'},{'JavaOptionsList',["+javaOptions+"] },{'AccumulateOutput','false'}]),"
				+ "Ref=make_ref(),"
				+ "synapselauncher:find_statechum()!{self(),Ref,getNodeName},"
				+ "receive {Ref,ok,Value} -> "
				+ "Value end"));
		Assert.assertTrue(pingNode(synapseNode));
		
		erlRuntime.killErlang();
		int i=0;
		while(pingNode(synapseNode) && i < 100) // wait for Synapse to terminate
		{
			Thread.sleep(100);
			++i;
		}
		Assert.assertFalse(pingNode(synapseNode));// ensure Synapse has terminated
	}
	
	/** Tests that termination of a parent Erlang process kills Java. 
	 * @throws InterruptedException */
	@Test
	public void testRunSynapse2b() throws InterruptedException
	{
		String java = (System.getProperty("java.home")+File.separator+"bin/java").replace(File.separatorChar,'/');
		OtpErlangTuple pid_node = (OtpErlangTuple)runner.evaluateString("OurPid=self(),Pid = spawn(fun () -> synapselauncher:startStatechum([{'Java','"+java+"'},{'JavaOptionsList',["+javaOptions+"] },{'AccumulateOutput','false'}]),OurPid!ok,receive stop -> ok end end),"
				+ "receive ok -> ok end,"
				+ "Ref=make_ref(),"
				+ "synapselauncher:find_statechum()!{self(),Ref,getNodeName},"
				+ "receive {Ref,ok,Value} -> "
				+ "{Pid,Value} end");
		Assert.assertEquals(2,pid_node.arity());
		String synapseNode = ErlangLabel.dumpErlangObject(pid_node.elementAt(1));
		OtpErlangPid pid = (OtpErlangPid)pid_node.elementAt(0);// PID of the parent process
		
		// Check that Synapse is running.
		Assert.assertTrue(pingNode(synapseNode));
		
		// Terminate the parent process
		runner.thisMbox.send(pid, new OtpErlangAtom("stop"));// send message to parent process asking it to terminate
		
		int i=0;
		while(pingNode(synapseNode) && i < 100) // wait for Synapse to terminate
		{
			Thread.sleep(100);
			++i;
		}
		Assert.assertFalse(pingNode(synapseNode));// ensure Synapse has terminated
	}
	
	/** Tests that termination of a process linked to a worker kill the worker.
	 * @throws InterruptedException */
	@Test
	public void testRunSynapse2c() throws InterruptedException
	{
		String java = (System.getProperty("java.home")+File.separator+"bin/java").replace(File.separatorChar,'/');
		runner.evaluateString("synapselauncher:startStatechum([{'Java','"+java+"'},{'JavaOptionsList',["+javaOptions+"] },{'AccumulateOutput','true'}]),"+
				"OurPid=self(),Ref=make_ref(),Pid = spawn(fun () -> synapselauncher:find_statechum()!{self(),Ref,getStatechumWorker},receive {Ref,WorkerPid} -> "
				+"WorkerPid!{Ref,echo},receive {Ref,workerok} ->ok,throw(worker_parent_failed)" // check that worker is ok and then make an abnormal termination. Nnow the worker should terminate and this is to appear on standard output.
				+" end end end)" //
				);
		Thread.sleep(500);// This gives time to Erlang to propagate an error to the Java node.
		String response = ErlangLabel.dumpErlangObject(runner.evaluateString("synapselauncher:find_statechum()!terminate," //io:format(\"waiting for response~n\"),"
				+ "receive Arg -> Arg end"));
		Assert.assertTrue(response.contains("Node exited com.ericsson.otp.erlang.OtpErlangExit: {{nocatch,worker_parent_failed}"));
	}
	
	/** Pings a node provided.
	 *  The argument should have quotes around it unless it can be parsed as an atom by Erlang.
	 *  
	 * @param node node name
	 * @return true if ping returned success, false otherwise.
	 */
	protected boolean pingNode(String node)
	{// we cannot ping Java nodes from Java, it seems but can ping them from Erlang. Do this here. 
		return Boolean.parseBoolean( ((OtpErlangAtom)pingRunner.evaluateString("case net_adm:ping("+node+") of pong -> true; pang -> false end")).atomValue());
	}
	
	@Test
	public void testRunSynapse3() throws InterruptedException
	{// ,{'Cookie','"+ErlangNode.getErlangNode().getNode().cookie()+"'}
		String java = (System.getProperty("java.home")+File.separator+"bin/java").replace(File.separatorChar,'/');
		String synapseNode = ErlangLabel.dumpErlangObject(runner.evaluateString("synapselauncher:startStatechum([{'Java','"+java+"'},{'JavaOptionsList',["+javaOptions+"] },{'AccumulateOutput','true'}]),"
				+ "Ref=make_ref(),"
				+ "synapselauncher:find_statechum()!{self(),Ref,getNodeName},"
				+ "receive {Ref,ok,Value} -> "
				+ "Value end"));
		Assert.assertTrue(pingNode(synapseNode));
		
		String response = ErlangLabel.dumpErlangObject(runner.evaluateString("synapselauncher:find_statechum()!terminate," //io:format(\"waiting for response~n\"),"
				+ "receive Arg -> Arg end"));
		Assert.assertTrue(response.contains("Synapse started"));Assert.assertTrue(response.contains("Synapse terminated"));
		int i=0;
		while(pingNode(synapseNode) && i < 100) // wait for Synapse to terminate
		{
			Thread.sleep(100);
			++i;
		}
		Assert.assertFalse(pingNode(synapseNode));// ensure Synapse has terminated
	}
	
	
	/** Starts Synapse and returns the associated pid. */
	public OtpErlangPid startSynapse()
	{
		return startSynapse(false);
	}
	
	/** Starts Synapse and returns the associated pid. */
	public OtpErlangPid startSynapse(boolean accumulateOutput)
	{
		String java = (System.getProperty("java.home")+File.separator+"bin/java").replace(File.separatorChar,'/');
		String ErlangHome = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.ERLANGHOME), ErlangHomeSetting="";
		if (ErlangHome != null)
			ErlangHomeSetting=",{'-DERLANGHOME','"+ErlangHome.replace('\\', '/')+"'}";
		return (OtpErlangPid)runner.evaluateString("synapselauncher:startStatechum([{'Java','"+java+"'},{'JavaOptionsList',["+javaOptions+ErlangHomeSetting+"] },{'AccumulateOutput','" + new Boolean(accumulateOutput).toString().toLowerCase()+"'}]),"
				+ "Ref=make_ref(),"
				+ "synapselauncher:find_statechum()");		
	}
	
	/** Same as above but we do not accumulate output. 
	 * @throws InterruptedException */
	@Test
	public void testRunSynapse4() throws InterruptedException
	{
		String java = (System.getProperty("java.home")+File.separator+"bin/java").replace(File.separatorChar,'/');
		String synapseNode = ErlangLabel.dumpErlangObject(runner.evaluateString("synapselauncher:startStatechum([{'Java','"+java+"'},{'JavaOptionsList',["+javaOptions+"] },{'AccumulateOutput','false'}]),"
				+ "Ref=make_ref(),"
				+ "synapselauncher:find_statechum()!{self(),Ref,getNodeName},"
				+ "receive {Ref,ok,Value} -> "
				+ "Value end"));
		Assert.assertTrue(pingNode(synapseNode));// check that Synapse is up
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			runner.evaluateString("synapselauncher:find_statechum()!terminate,"
				+ "receive Arg -> Arg end");
		}},IllegalArgumentException.class,"timeout waiting for a response");// when we do not collect output, no response is sent.
		
		int i=0;
		while(pingNode(synapseNode) && i < 100) // wait for Synapse to terminate
		{
			Thread.sleep(100);
			++i;
		}
		Assert.assertFalse(pingNode(synapseNode));// ensure Synapse has terminated
	}
	
	/** Start Synapse, send an invalid command. */
	@Test
	public void testRunSynapse5()
	{
		startSynapse();
		
		Assert.assertEquals("ok",runner.evaluateString("Ref=make_ref(),synapselauncher:find_statechum()!{self(),Ref,junk},receive {Ref,invalidcommand_or_missing_args} -> ok end").toString());
	}

	/** Start Synapse, send a command an invalid format, get no response. */
	@Test
	public void testRunSynapse6()
	{
		startSynapse();
		
		Assert.assertEquals("true",runner.evaluateString("Ref=make_ref(),synapselauncher:find_statechum()!{junk,self(),Ref,Ref},receive A -> false after 1000 -> true end").toString());
	}

	/** Start Synapse, start worker, check echo. */
	@Test
	public void testRunSynapse7()
	{
		startSynapse();
		
		Assert.assertEquals("ok",runner.evaluateString("Ref=make_ref(),synapselauncher:find_statechum()!{self(),Ref,getStatechumWorker},receive {Ref,Pid} -> ARef = make_ref(),Pid!{ARef,echo},receive {ARef,workerok} -> ok end end").toString());
	}
	
	/** Start Synapse, start worker, check invalid command. */
	@Test
	public void testRunSynapse8()
	{
		startSynapse();
		
		Assert.assertEquals("ok",runner.evaluateString("Ref=make_ref(),synapselauncher:find_statechum()!{self(),Ref,getStatechumWorker},receive {Ref,Pid} -> ARef = make_ref(),Pid!{ARef,junk},receive {ARef,invalidcommand_or_missing_args} -> ok end end").toString());
	}
	
	/** Start Synapse, start worker, stop worker. */
	@Test
	public void testRunSynapse9()
	{
		startSynapse(true);
		
		System.out.println(runner.evaluateString("Ref=make_ref(),synapselauncher:find_statechum()!{self(),Ref,getStatechumWorker},receive {Ref,Pid} -> ARef = make_ref(),Pid!{ARef,terminate},receive after 1000 -> ok end end"));
	}
	
}
