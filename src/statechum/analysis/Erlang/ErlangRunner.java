/* Copyright (c) 2011 The University of Sheffield.
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
 * 
 * 
 * In order to run Erlang on Win32, the following VM args can be used:
 * -ea -DVIZ_CONFIG=kirill_office -Dthreadnum=2 -Djava.library.path="linear/.libs;smt/.libs" -Xmx1500m -DERLANGHOME="D:\Program~1\erl5.8.2"
 * The "~1" is important: without it experimentRunner passes the wrong arguments to its nested jvm because 
 * ManagementFactory.getRuntimeMXBean().getInputArguments() splits the command line into a number of parts.
 *  
 * Running on Debian x86_64 requires something like:
 * -ea -DVIZ_CONFIG=kirill_home -Dthreadnum=2 -Djava.library.path=linear/.libs:smt/.libs -Xmx2500m -DLTL2BA=/usr/local/bin/ltl2ba
 * 
 * Building Erlang R14B03 on MacOS X 10.5,
 * ./configure --enable-darwin-64bit --enable-m64-build --prefix=/usr/local/soft/Erlang_R14B03 
 * This generates apple-gcc command-line options which are not accepted by 
 * "normal" gcc since Apple does not contribute patches to their features. Compile
 * may fail because am_sse2_fnegate_mask is not defined - it needs to be included
 * in the file with atoms and defined as #define am_sse2_fnegate_mask make_atom(984) or whatever the last value is.
 * 
 * If erlang is installed somewhere not on the standard path (e.g. /usr/local/bin) then this
 * needs to be added to the path seen by Eclipse (or NetBeans, or whatever). This is NOT the
 * same as the path seen from the terminal. To change the path seen by apps you must create
 * ~/.MacOSX/environment.plist - this is populated with key-value items that are added to the
 * environment when an app loads. The simplest content is:
 * 
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>PATH</key>
	<string>/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin</string>
</dict>
</plist>
 * 
 *
 * As documented in many places online such as, 
 * http://concurrently-chaotic.blogspot.co.uk/2012/03/erlang-vm-memory-size-and-erlmaxports.html
 * Erlang has a habit of allocating a lot of memory for possible ports,
 * this was usually a small number (such as 1024), but was recently increased, leading to Erlang eating 250Meg
 * per instance (as shown by pmap -d on Linux).
 * 
 * I use the command 
 * {Dh,Dd}=instrument:descr(instrument:memory_data()),lists:foreach(fun(T)->{Tname,_,Tsize,Tpid}=T,if Tsize>200000 -> io:format("~p ~p ~p~n",[Tname,Tsize,Tpid]); true -> ok end end,Dd).
 * to list all the "worst offenders", but one can see it in the list of allocators (ll_alloc is the one that ate all this and in another
 * lists that shows the size of the 'static' allocated memory). Solution: ERL_MAX_PORTS.
 * 
 * 
 */

package statechum.analysis.Erlang;

import java.io.File;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper;
import statechum.AttributeMutator.GETMETHOD_KIND;

import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.ExperimentRunner.HandleProcessIO;

/**
 * Manages the Erlang process running Erlang oracle. The idea is to start one
 * when an instance of the runner is created and let it die when Java process
 * terminates.
 */
public class ErlangRunner {

	/**
	 * Given the name of Erlang file, this method strips out the extension and
	 * returns the result. The name is supposed to be a valid file name.
	 * 
	 * @param fileName
	 *            File name to remove the extension from
	 * @return stripped file name.
	 */
	public static String getErlName(String fileName) {
		if (fileName == null)
			return null;
		String trimmedName = fileName.trim();
		int pos = trimmedName.lastIndexOf('.');
		if (pos <= 0 || !trimmedName.substring(pos).equals(ERL.ERL.toString()))
			return null;
		return trimmedName.substring(0, pos);
	}

	public static boolean validName(String fileName) {
		return getErlName(fileName) != null;
	}

	public static enum ERL {
		ERL(".erl", false), BEAM(".beam", false), PLT(".plt", false), MOD("",
				true), NOEXT("", false);

		private ERL(String textualName, boolean strip) {
			stringRepresentation = textualName;
			stripPath = strip;
		}

		private final String stringRepresentation;
		private final boolean stripPath;

		public boolean getStrip() {
			return stripPath;
		}

		@Override
		public String toString() {
			return stringRepresentation;
		}
	}

	/**
	 * Given a file name, converts it to the appropriate extension. Throws
	 * IllegalArgumentException if this fails.
	 * 
	 * @param file
	 *            file to convert
	 * @param ext
	 *            extension to add
	 * @param separateBeamDir whether output files are expected to be located in a separate directory.
	 */
	public static String getName(File file, ERL ext,boolean separateBeamDir) {
		String nameToProcess = null;
		if (ext.getStrip())
			nameToProcess = file.getName();
		else
		{
			if (separateBeamDir)
				nameToProcess = ErlangRunner.getErlangBeamDirectory()+File.separator+file.getName();
			else
				nameToProcess = file.getAbsolutePath();
		}

		String moduleName = getErlName(nameToProcess);
		if (moduleName == null)
			throw new IllegalArgumentException("Invalid module "
					+ nameToProcess);

		return moduleName + ext.toString();
	}

	/** Obtains a binary directory for an Erlang executable. */
	public static String getErlangBin() {
		String erlangBin = GlobalConfiguration.getConfiguration().getProperty(
				GlobalConfiguration.G_PROPERTIES.ERLANGHOME);
		if (erlangBin != null)
			erlangBin = erlangBin + File.separator + "bin" + File.separator;
		else
			erlangBin = "";
		return erlangBin;
	}

	/** Erlang machine in which we run most stuff. */
	protected Process erlangProcess = null;
	
	/** Monitors the above machine and dumps its out and err to the console. */
	protected Thread stdDumper = null;

	protected String traceRunnerNode, ourNode;

	protected static final ErlangRunner staticRunner = new ErlangRunner();

	/** Returns a static instance of this class. */
	static public ErlangRunner getRunner() {
		return staticRunner;
	}

	/** Returns the directory where Beam, plt and other files will be placed. 
	 * This is a writable location not necessarily the same as the one containing Erlang source files.
	 * 
	 * @return binary directory
	 */
	static public File getErlangBeamDirectory()
	{
		File beamDir = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_ERLANGBEAM));
		if (!beamDir.isDirectory())
			if (!beamDir.mkdir())
				throw new IllegalArgumentException("Erlang output directory "+beamDir.getAbsolutePath()+" cannot be created");
		return beamDir;
	}
	
	/** Returns the directory where .erl files from Erlang integration are stored. 
	 * 
	 * @return binary directory
	 */
	static public String getErlangFolder()
	{
		return GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_ERLANGFOLDER);
	}

	/**
	 * Compiles the supplied file into .beam if .erl has been modified after an
	 * existing .beam (date of last change is 0 if file does not exist).
	 * 
	 * @param whatToCompile
	 *            file to compile
	 * @param useRunner
	 *            ask Erlang compiler to perform the compile - no need to launch
	 *            compiler as a separate process.
	 * @param compileIntoBeamDirectory whether the compiled file should be placed in a separate directory defined using {@link G_PROPERTIES#PATH_ERLANGBEAM} 
	 * @throws IOException
	 *             if something goes wrong.
	 */
	public static void compileErl(File whatToCompile, ErlangRunner useRunner, boolean compileIntoBeamDirectory)
			throws IOException {
		String erlFileName = getName(whatToCompile, ERL.ERL,false);
		if (whatToCompile.getParentFile() == null)
			throw new IllegalArgumentException(
					"File does not have a parent directory " + whatToCompile);
		File beamDirectory = compileIntoBeamDirectory?getErlangBeamDirectory():whatToCompile.getAbsoluteFile().getParentFile();

		if (!whatToCompile.canRead())
			throw new IOException("file " + erlFileName + " does not exist");
		if (whatToCompile.lastModified() > new File(getName(whatToCompile, ERL.BEAM,compileIntoBeamDirectory)).lastModified()) 
		{
			if (useRunner == null) {
				Process p = Runtime.getRuntime().exec(
						new String[] { ErlangRunner.getErlangBin() + "erlc",
								"+debug_info", erlFileName }, null, beamDirectory);
				dumpProcessOutputOnFailure("erlc " + whatToCompile.getName(), p);
			} else 
			{
				useRunner.call(new OtpErlangObject[] {
								new OtpErlangAtom("compile"),
								new OtpErlangList(
										new OtpErlangObject[] { new OtpErlangAtom(
												erlFileName) }),
								new OtpErlangAtom("erlc"),
								new OtpErlangAtom(beamDirectory.getAbsolutePath()) },
								"cannot compile ");
			}
		}
	}

	public static final int timeBetweenChecks = 100;

	/**
	 * Runs the supplied process and returns output and error streams in an
	 * exception if the process returned a non-zero error code. Upon success, no
	 * output is produced.
	 * 
	 * @param p
	 *            process to run.
	 */
	public static void dumpProcessOutputOnFailure(String name, Process p) {
		final StringBuffer err = new StringBuffer(), out = new StringBuffer();
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
						out.append(b);
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
	}

	/** How long to wait for a server to start. */
	protected int serverTimeout = 7200;

	/** Sets new timeout - very useful to increase it for testing. */
	public void setTimeout(int time) {
		serverTimeout = time;
	}

	/*
	 * Store the processes that were extant just after the tracerunner starts.
	 * The killProcesses method will kill anything not in this list, which
	 * should mean anything started by traces.
	 */
	protected OtpErlangList proclist;

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
	public void startErlang(String runnerMode, long delay) {
		proclist = null;
		final boolean displayErlangOutput = Boolean.parseBoolean(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.ERLANGOUTPUT_ENABLED));
		try {
			if (displayErlangOutput)
			{
				System.out.print("Starting Erlang...");System.out.flush();
			}
			
			final long startTime = System.currentTimeMillis();
			if (erlangProcess == null) {
				String tracerunnerProgram = "tracerunner.erl";
				String uniqueID = "_"+ System.nanoTime()+ "_"+ ManagementFactory.getRuntimeMXBean().getName().replace('@', '_').replace('.', '_') + "@localhost";
				traceRunnerNode = "tracerunner" + uniqueID;
				ourNode = "java" + uniqueID;
				// now we simply evaluate "halt()." which starts epmd if
				// necessary and we can check along the way that we can run
				// Erlang at all.
				Process p = Runtime.getRuntime().exec(
						new String[] { ErlangRunner.getErlangBin() + "erl",
								"-eval", "halt().", "-sname", traceRunnerNode,
								"-noshell", "-setcookie", uniqueID }, null
								);//getErlangBeamDirectory());
				dumpProcessOutputOnFailure(
						"testing that Erlang can be run at all", p);

				// The compilation phase could a few seconds but only needs to
				// be done once after installation of Statechum
				for (String str : new String[] { tracerunnerProgram,
						"tracer3.erl", "export_wrapper.erl", "gen_event_wrapper.erl",
						"gen_fsm_wrapper.erl", "gen_server_wrapper.erl" })
				compileErl(new File(getErlangFolder(), str), null,true);
				for (File f : new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_ERLANGTYPER)).listFiles())
					if (ErlangRunner.validName(f.getName()))
						ErlangRunner.compileErl(f, null, true);

				// Based on
				// http://erlang.org/pipermail/erlang-questions/2010-March/050226.html
				OtpNode self = new OtpNode(ourNode, uniqueID); // identify self
				thisMbox = self.createMbox("thisMbox");

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

				erlangProcess = Runtime
						.getRuntime()
						.exec(new String[] {
								ErlangRunner.getErlangBin() + "erl",
								"-pa",getErlangBeamDirectory().getAbsolutePath(),
								// the easiest way to substitute our module in place
								// of the original Erlang's one, otherwise I'd
								// have to rely on tracerunner:compileAndLoad
								"-run", "tracerunner", "start", ourNode,
								runnerMode, "-sname", traceRunnerNode,
								"-noshell", "-setcookie", uniqueID },
								envpList.toArray(new String[0]));
								//getErlangBeamDirectory());
				stdDumper = new Thread(new Runnable() {

					@Override
					public void run() {
						ExperimentRunner.dumpStreams(erlangProcess,
								timeBetweenChecks, new HandleProcessIO() {

									@Override
									public void OnHeartBeat() {
										// no prodding is done - we are
										// being prodded by Erlang instead.
									}

									@Override
									public void StdErr(StringBuffer b) {
										if (displayErlangOutput)
											System.out.print("[ERLANG] "
													+ b.toString());
									}

									@Override
									public void StdOut(StringBuffer b) {
										if (displayErlangOutput)
											System.out.print("[ERLERR] "
													+ b.toString());
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
				while (!self.ping(traceRunnerNode, 500) && timeout > 0) {
					try {
						erlangProcess.exitValue();
						// process terminated, record this as a failure
						timeout = 0;
					} catch (IllegalThreadStateException e) {
						// process not yet terminated, hence we keep waiting
						--timeout;
					}
				}
				ourBox = new OtpErlangTuple(new OtpErlangObject[] {
						thisMbox.self(), self.createRef() });
				if (timeout <= 0) {
					final long endTime = System.currentTimeMillis();
					throw new IllegalArgumentException(
							"timeout waiting for a server to start after "
									+ (endTime - startTime) + "ms");
				}
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
		proclist = listProcesses();
	}

	/** An argument to $gen_call */
	protected OtpErlangTuple ourBox = null;
	/** The process on the Java side which communicates with Erlang process. */
	protected OtpMbox thisMbox = null;

	public static final OtpErlangAtom okAtom = new OtpErlangAtom("ok");
	public static final OtpErlangAtom timeoutAtom = new OtpErlangAtom("timeout");

	/** Passes Statechum configuration to Erlang. */
	public void configurationToErlang(Configuration config) {
		Class<? extends Configuration> clazz = config.getClass();
		List<OtpErlangTuple> nameToValue = new LinkedList<OtpErlangTuple>();

		for (Field var : clazz.getDeclaredFields()) {
			// based on constructArgList of AttributeMutator

			if (var.getType() != clazz && var.getName() != "$VRc"// added by eclemma (coverage analysis)
					&& !java.lang.reflect.Modifier.isFinal(var.getModifiers())) {
				String varName = var.getName();
				Method getter = statechum.AttributeMutator.getMethod(clazz,
						GETMETHOD_KIND.FIELD_GET, var);
				Object outcome = null;
				try {
					outcome = getter.invoke(config, new Object[] {});
				} catch (Exception e) {
					Helper.throwUnchecked("cannot invoke method " + getter
							+ " on " + clazz, e);
				}

				if (outcome != null) {// it is null for some enums
					OtpErlangObject value = null;
					if (outcome.getClass().equals(Boolean.class))
						value = new OtpErlangBoolean(
								((Boolean) outcome).booleanValue());
					else if (outcome.getClass().equals(Double.class))
						value = new OtpErlangDouble(
								((Double) outcome).doubleValue());
					else if (outcome.getClass().equals(String.class))
						value = new OtpErlangAtom((String) outcome);
					else if (outcome.getClass().equals(Integer.class))
						value = new OtpErlangInt(((Integer) outcome).intValue());
					else if (outcome.getClass().equals(Long.class))
						value = new OtpErlangLong(((Long) outcome).longValue());
					else
						value = new OtpErlangAtom(outcome.toString());

					nameToValue.add(new OtpErlangTuple(new OtpErlangObject[] {
							new OtpErlangAtom(varName), value }));
				}
			}
		}

		call(new OtpErlangObject[] { new OtpErlangAtom("setConfiguration"),
				new OtpErlangList(nameToValue.toArray(new OtpErlangObject[0])) },
				"setConfiguration");
	}

	/** Extracts an attribute value from Erlang. */
	public OtpErlangObject getAttr(String name) {
		return call(
				new OtpErlangObject[] { new OtpErlangAtom("getAttr"),
						new OtpErlangAtom(name) }, "getAttr " + name)
				.elementAt(1);
	}

	/**
	 * If an exception is thrown via Erlang's throw mechanism, it is packed as
	 * this exception.
	 */
	static class ErlangThrownException extends RuntimeException {
		private final OtpErlangTuple errorMessage;
		public static final OtpErlangAtom ErrCannotLoadPlt = new OtpErlangAtom(
				"cannotLoadPlt");

		public ErlangThrownException(OtpErlangTuple decodedResponse,
				String errText) {
			super(errText);
			errorMessage = decodedResponse;
		}

		/**
		 * Returns an atom for the error code, throws illegalcast exception if
		 * the format an error message is unexpected.
		 */
		public OtpErlangAtom getError() {
			return (OtpErlangAtom) errorMessage.elementAt(1);
		}

		/**
		 * ID for serialization.
		 */
		private static final long serialVersionUID = -1273285579412902110L;

		public static final String keyword = "throw";
	}

	/**
	 * Makes a call and waits for a response.
	 * 
	 * @param args
	 *            components of the tuple to pass a server
	 * @return Response from the server so long as the server returns { ok,
	 *         Response }. Upon a failure {error, Failure }, an error message is
	 *         thrown as RuntimeException with Failure included in it.
	 */
	public OtpErlangTuple call(OtpErlangObject[] args, String errorMessage) {
		OtpErlangTuple result = null;
		OtpErlangObject response = call(args, 30000);

		if (response instanceof OtpErlangAtom) {
			if (!response.equals(okAtom)) {
				throw new RuntimeException(errorMessage + " : error "
						+ response);
			}

			// success, but null response
		} else if (response instanceof OtpErlangTuple) {
			OtpErlangTuple decodedResponse = (OtpErlangTuple) response;
			if (decodedResponse.arity() == 0)
				throw new RuntimeException(errorMessage
						+ " : unexpectedly short response (arity "
						+ decodedResponse.arity() + ") " + response);
			OtpErlangObject decodedResponseCode = decodedResponse.elementAt(0);
			if (!(decodedResponseCode instanceof OtpErlangAtom))
				throw new RuntimeException(errorMessage
						+ " : unexpected type in response tuple "
						+ decodedResponse);
			if (!decodedResponseCode.equals(okAtom)) {
				if (((OtpErlangAtom) decodedResponseCode).atomValue().equals(
						ErlangThrownException.keyword))
					throw new ErlangThrownException(decodedResponse,
							errorMessage + " : has thrown " + decodedResponse);
				throw new RuntimeException(errorMessage + " : error "
						+ decodedResponse);

			}
			result = decodedResponse;
		} else
			throw new RuntimeException(errorMessage
					+ " : unexpected response type " + response);

		return result;
	}

	/** Parses an evaluates a supplied text using Erlang. */
	public OtpErlangObject evaluateString(String text) {
		OtpErlangTuple outcome = call(new OtpErlangObject[] {
				new OtpErlangAtom("evaluateTerm"),
				new OtpErlangString(text + ".") }, "evaluation of " + text);
		return outcome.elementAt(1);
	}

	/**
	 * Makes a call and waits for a response for the specified duration,
	 * infinitely if timeout is zero.
	 * 
	 * @param args
	 *            components of the tuple to pass a server
	 * @param timeout
	 *            how long to wait for a response.
	 * @return Response from the server or null on a timeout
	 */
	public OtpErlangObject call(OtpErlangObject[] args, int timeout) {
		if (erlangProcess == null) {
			startErlang("tracerunner", 0);
		}
		thisMbox.send("tracecheckServer", traceRunnerNode, new OtpErlangTuple(
				new OtpErlangObject[] { new OtpErlangAtom("$gen_call"), ourBox,
						new OtpErlangTuple(args) }));
		OtpErlangTuple msg = null;
		try {
			if (timeout > 0)
				msg = (OtpErlangTuple) thisMbox.receive(timeout);
			else
				msg = (OtpErlangTuple) thisMbox.receive();
		} catch (OtpErlangExit e) {
			Helper.throwUnchecked("exlang exit", e);
		} catch (OtpErlangDecodeException e) {
			Helper.throwUnchecked("decode exception", e);
		}
		if (msg == null)
			throw new IllegalArgumentException("timeout waiting for a response");
		return msg.elementAt(1);
	}

	public void killErlang() {
		if (erlangProcess != null) {
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
			ourBox = null;
			thisMbox = null;
			stdDumper.interrupt();// if this one is sleeping, interrupt will wake it and it will terminate since
									// Erlang is already no more ...
			try { stdDumper.join();	} 
			catch (InterruptedException e) {
				Helper.throwUnchecked("Interrupt waiting for erlang out/err dumper to terminate",e);
			}// wait for that to happen.
			stdDumper = null;
		}
	}

	public OtpErlangList listProcesses() {
		OtpErlangList outcome = null;
		OtpErlangTuple tup = call(new OtpErlangObject[] { new OtpErlangAtom(
				"processes") }, "Failed to get process list.");
		if ((tup.arity() == 2) && (tup.elementAt(0).equals(okAtom))) {
			outcome = (OtpErlangList) tup.elementAt(1);
		} else
			throw new RuntimeException("Er, why did I get this?: "
					+ tup.elementAt(0) + "(" + tup.arity() + ")");
		
		return outcome;
	}

	public OtpErlangTuple killProcesses() {
		OtpErlangTuple outcome = null;
		if (proclist != null) {
			outcome = call(new OtpErlangObject[] {
					new OtpErlangAtom("killProcesses"), proclist },
					"Failed to kill processes.");
		} 
		
		return outcome;
	}
}
