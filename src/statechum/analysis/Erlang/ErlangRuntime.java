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

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.ExperimentRunner.HandleProcessIO;

public class ErlangRuntime {
	/** Erlang machine in which we run most stuff. */
	protected Process erlangProcess = null;
	
	/** Monitors the above machine and dumps its out and err to the console. */
	protected Thread stdDumper = null;

	protected String traceRunnerNode;
	
	public static final int timeBetweenChecks = 100;


	/**
	 * Runs the supplied process and returns output and error streams in an
	 * exception if the process returned a non-zero error code. Upon success, returns the standard output from the process.
	 * 
	 * @param p
	 *            process to run.
	 */
	public static String dumpProcessOutputOnFailure(String name, Process p) {
		final StringBuffer err = new StringBuffer(), out = new StringBuffer(),stdOut = new StringBuffer();
		ExperimentRunner.dumpStreams(p, timeBetweenChecks,
				new HandleProcessIO() {

					@Override
					public void OnHeartBeat() {// no prodding is done for a
												// short-running converter.
					}

					@Override
					public void StdErr(StringBuffer b) {
						err.append(b);
					}

					@Override
					public void StdOut(StringBuffer b) {
						out.append(b);stdOut.append(b);
					}
				});
		try {
			p.waitFor();
		} catch (InterruptedException e) {
			// assumed we have been asked to terminate
		}

		if (p.exitValue() != 0)
			throw new IllegalArgumentException("Failure running " + name + "\n"
					+ err + (err.length() > 0 ? "\n" : "") + out);
		
		return stdOut.toString();
	}

	/** How long to wait for a server to start. */
	protected int serverTimeout = 500;

	/** Sets new timeout - very useful to increase it for testing. */
	public void setTimeout(int time) {
		serverTimeout = time;
	}

	/** The default constructor is public to permit multiple Erlang runtimes to be started. */
	public ErlangRuntime()
	{
	}

	protected static ErlangRuntime defaultRuntime = null;
	
	/** Most of the time, one would not need to start multiple runtime instances, or use custom parameters. For this purpose, a single runner is good enough and it can be obtained by calling the method below. */
	public static ErlangRuntime getDefaultRuntime()
	{
		if (defaultRuntime == null) 
		{
			defaultRuntime = new ErlangRuntime();defaultRuntime.startRunner();
		}
		return defaultRuntime;
	}
	
	/** Starts the runner with default arguments. */
	public void startRunner()
	{
		startErlang(ErlangRunner.genServerDefault, 0);
	}
	
	/** Creates a runner that can be used to communicate with the runtime associated with this node. If this runtime is not associated with Erlang runner, creates this runner. */
	public synchronized ErlangRunner createNewRunner()
	{
		if (erlangProcess == null)
			throw new IllegalArgumentException("Erlang is not running");
		ErlangRunner newRunner = new ErlangRunner(traceRunnerNode);newRunner.initRunner();return newRunner;
	}

	/** Contains OTP version. */
	public static String platformDescription="";

	/**
	 * Starts Erlang in the background,
	 * 
	 * @param runnerMode
	 *            "tracerunner" means production use, noserver,halt,error are
	 *            used for testing.
	 * @param delay
	 *            how long to wait after launching the server - used only for
	 *            testing to ensure server terminates before our loop starts.
	 */
	public synchronized void startErlang(String runnerMode, long delay) {
		proclist = null;
		final boolean displayErlangOutput = Boolean.parseBoolean(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.ERLANGOUTPUT_ENABLED));
		try {
			if (erlangProcess == null) 
			{
				if (displayErlangOutput)
				{
					System.out.print("Starting Erlang...");System.out.flush();
				}
				
				final long startTime = System.currentTimeMillis();

				ErlangNode.initNodeParameters(null, null);
				
				String tracerunnerProgram = "tracerunner.erl";
				// It is very important that there is an '@' part to the node name: without it, Erlang adds a host name by default so the actual node name is different from the one supplied via -sname to the process and the node does not respond to the name without '@'.
				traceRunnerNode = "tracerunner" + "_"+ System.nanoTime()+ "_" + "@" + "127.0.0.1";// + InetAddress.getLocalHost().getHostName().replaceAll("\\..*", "");// eliminates everything starting with the first dot, important on MacOS. 
				// now we simply evaluate "halt()." which starts epmd if
				// necessary and we can check along the way that we can run
				// Erlang at all.
				Process p = Runtime.getRuntime().exec(
						new String[] { ErlangRunner.getErlangBin() + "erl",
								"-eval", "halt().", "-name", traceRunnerNode,
								"-noshell", "-setcookie", ErlangNode.getErlangNode().cookie() }, null
								);//getErlangBeamDirectory());
				dumpProcessOutputOnFailure(
						"testing that Erlang can be run at all", p);
				
				// Thanks to https://blog.kempkens.io/posts/erlang-17-0-supporting-deprecated-types-without-removing-warnings_as_errors/
				// Strings converted to lists of numbers to avoid using quotes that are not compatible between MacOS and Windows (quotes need backslash on Windows, but there should be none for MacOS). Using dollar sign is probably also bad since it may be expanded. 
				p = Runtime.getRuntime().exec(
						new String[] { ErlangRunner.getErlangBin() + "erl",
								"-eval", "io:format([126,115],[ [95 | [ erlang:system_info(otp_release) | [45 | erlang:system_info(system_architecture)]]]]),halt().", "-name", traceRunnerNode,
								"-noshell", "-setcookie", ErlangNode.getErlangNode().cookie() }, null);
				platformDescription=dumpProcessOutputOnFailure("extraction of OTP version string", p);

				// After the first Erlang node is created (by passing -sname to Erlang), epmd appears and we can create a node.
				ErlangNode.getErlangNode().createNode();

				// The compilation phase could a few seconds but only needs to
				// be done once after installation of Statechum. Here we only compile the core part of Statechum, the rest is compiled by the runtime 
				// because compile options depend on the version of Erlang in use and this is only known to the runtime.
				for (String str : new String[] { tracerunnerProgram,
						"tracer3.erl", "export_wrapper.erl", "gen_event_wrapper.erl",
						"gen_fsm_wrapper.erl", "gen_server_wrapper.erl" })
				ErlangRunner.compileErl(new File(ErlangRunner.getErlangFolder(), str), null,true);

				// Now build environment variables to ensure that dialyzer will
				// find a directory to put its plt file in.
				// This is actually a dialyzer bug which manifests itself on
				// Windows -
				// there is no need for dialyzer_options:build(Opts) to call
				// dialyzer_plt:get_default_plt()
				// before build_options(Opts, DefaultOpts1).
				List<String> envpList = new LinkedList<String>();
				
				Map<String,String> newValues = new TreeMap<String,String>();
				newValues.put("HOME",new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_ERLANGBEAM)).getAbsolutePath());
				newValues.put("ERL_MAX_PORTS","1024");// to limit the size of each Erlang instance to a few meg from a few hundred meg

				for (Entry<String, String> entry : System.getenv().entrySet())
					if (!newValues.containsKey(entry.getKey()))
						envpList.add(entry.getKey() + "=" + entry.getValue());
				
				for(Entry<String, String> entry : newValues.entrySet())
					envpList.add(entry.getKey() + "=" + entry.getValue());

				final Process processWithErlangRuntime = Runtime
						.getRuntime()
						.exec(new String[] {
								ErlangRunner.getErlangBin() + "erl",
								"-pa",ErlangRunner.getErlangBeamDirectory().getAbsolutePath(),
								// the easiest way to substitute our module in place
								// of the original Erlang's one, otherwise I'd
								// have to rely on tracerunner:compileAndLoad
								"-run", "tracerunner", "start", ErlangNode.getErlangNode().getName(),
								runnerMode, "-name", traceRunnerNode,
								"-noshell", "-setcookie", ErlangNode.getErlangNode().cookie() },
								envpList.toArray(new String[0]));
								//getErlangBeamDirectory());
				stdDumper = new Thread(new Runnable() {

					@Override
					public void run() {
						ExperimentRunner.dumpStreams(processWithErlangRuntime,
								timeBetweenChecks, new HandleProcessIO() {

									@Override
									public void OnHeartBeat() {
										// no prodding is done - we are
										// being prodded by Erlang instead.
									}

									@Override
									public void StdErr(StringBuffer b) {
										if (displayErlangOutput)
											System.out.print("[ERLANG] " + b.toString());
									}

									@Override
									public void StdOut(StringBuffer b) {
										if (displayErlangOutput)
											System.out.print("[ERLERR] " + b.toString());
									}
								});
					}
				});
				stdDumper.setDaemon(true);
				stdDumper.start();
				if (delay > 0)
					Thread.sleep(delay);
				// At this point, the process may have not yet started or
				// already terminated, it is easy to find out which of the
				// two has happened by doing
				int timeout = serverTimeout;
				while (!ErlangNode.getErlangNode().getNode().ping(traceRunnerNode, 10) && timeout > 0) {
					try {
						processWithErlangRuntime.exitValue();
						// process terminated, record this as a failure
						timeout = 0;
					} catch (IllegalThreadStateException e) {
						// process not yet terminated, hence we keep waiting
						--timeout;
					}
				}
				if (timeout <= 0) 
				{
					final long endTime = System.currentTimeMillis();
					throw new IllegalArgumentException("timeout waiting for a server to start after " + (endTime - startTime) + "ms");
				}
				ErlangRunner runner = new ErlangRunner(traceRunnerNode);
				runner.forceReady();// cannot call createNewRunner because that one is supposed only to be used after we have successfully started Erlang runtime.
				timeout = serverTimeout;
				OtpErlangObject response = null;
				
				// Now we verify that communication with the runtime is possible because genserver may not have started yet and our messages will disappear into the void. 
				// We attempt to send messages with a short response time and check that we get what we expect.
				do
				{
					try
					{
						response = runner.call(new OtpErlangObject[]{new OtpErlangAtom("echo2Notuple"),new OtpErlangAtom("aaa")},10);
						if (!(response instanceof OtpErlangAtom) || !((OtpErlangAtom)response).atomValue().equals("ok_aaa"))
							timeout = 0;// force failure upon an invalid response
					}
					catch(IllegalArgumentException ex)
					{
						// response remains null
					}
					--timeout;
				}
				while(timeout > 0 && response == null);
				if (timeout <= 0) 
				{
					final long endTime = System.currentTimeMillis();
					throw new IllegalArgumentException("timeout waiting for a server's genserver to start after " + (endTime - startTime) + "ms");
				}
				
				// Erlang started, the next step is to train message queue since given that we may have sent a number of messages above, responses to them may just begin to trickle.
				while(null != runner.thisMbox.receive(100))
				{// drain the queue
				}
				
				for(G_PROPERTIES folderKind:new G_PROPERTIES[]{G_PROPERTIES.PATH_ERLANGTYPER,G_PROPERTIES.PATH_ERLANGSYNAPSE})
					for (File f : new File(GlobalConfiguration.getConfiguration().getProperty(folderKind)).listFiles())
						if (ErlangRunner.validName(f.getName()))
							ErlangRunner.compileErl(f, runner, true);
				
				// seems like success, set the value of ErlangProcess
				erlangProcess = processWithErlangRuntime;
				proclist = runner.listProcesses();runner.close();
			}
			//final long endTime = System.currentTimeMillis();
			//System.out.println("Started. " + (endTime - startTime) + "ms");
		} catch (IOException e) {
			killErlang();
			Helper.throwUnchecked("failed to start Erlang", e);
		} catch (InterruptedException e1) {
			killErlang();
			Helper.throwUnchecked("terminating as requested", e1);
		}
		catch (OtpErlangDecodeException e) {
			killErlang();
			Helper.throwUnchecked("decode exception", e);
		}
		catch (OtpErlangExit e) {
			killErlang();
			Helper.throwUnchecked("exlang exit", e);
		}
	}

	/**
	 * Stores the processes that were extant just after the tracerunner starts.
	 * The killProcesses method will kill anything not in this list, which
	 * should mean anything started by traces.
	 */
	protected OtpErlangList proclist;

	public void killErlang() {
		if (erlangProcess != null) 
		{
			ErlangRunner.closeAll(traceRunnerNode);
			final boolean displayErlangOutput = Boolean.parseBoolean(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.ERLANGOUTPUT_ENABLED));
			if (displayErlangOutput)
			{
				System.out.print("Stopping Erlang...");
				System.out.flush();
			}
			erlangProcess.destroy();
			try { erlangProcess.waitFor(); } 
			catch (InterruptedException e) {
				statechum.Helper.throwUnchecked("wait for Erlang to terminate aborted", e);
			}
			erlangProcess = null;
			stdDumper.interrupt();// if this one is sleeping, interrupt will wake it and it will terminate since
									// Erlang is already no more ...
			try { stdDumper.join();	} 
			catch (InterruptedException e) {
				Helper.throwUnchecked("Interrupt waiting for erlang out/err dumper to terminate",e);
			}// wait for that to happen.
			stdDumper = null;
		}
	}

}
