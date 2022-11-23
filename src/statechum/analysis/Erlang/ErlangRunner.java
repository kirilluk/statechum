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
 * Erlang 24.3.3 requires updated JInterface.jar because connections from the old one are rejected by the
 * Erlang runtime.
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
 * OtpErlang library for communication with Erlang from Java: there are two versions of it, old one (OtpErlang/14/OtpErlang.jar) and new one (OtpErlang/14/OtpErlang.jar).
 * They are not interchangeable: the old one will not work with new version of Erlang (connection will be rejected) and the new one will not work with old Erlang.
 * The '14' refers to the Otp it came from; it is known to support versions 14-18. The new one is known to support version 24 it originates from. It would be possible
 * to load one of them at run time using a custom classloader but it is not convenient for development. Hence at present this is left to the end user to decide which
 * of the two to use. Customized typer is also version-specific and the relevant version of it is compiled depending on the detected version of Otp runtime.
 */

package statechum.analysis.Erlang;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper;
import statechum.AttributeMutator.GETMETHOD_KIND;

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
		return getName(file,ext,separateBeamDir?ErlangRunner.getErlangBeamDirectory():null);
	}
	
	/**
	 * Given a file name, converts it to the appropriate extension. Throws
	 * IllegalArgumentException if this fails.
	 * 
	 * @param file
	 *            file to convert
	 * @param ext
	 *            extension to add
	 * @param beamDir where to place the output files, if null places them in the source directory.
	 */
	public static String getName(File file, ERL ext,File beamDir) {
		String nameToProcess = null;
		if (ext.getStrip())
			nameToProcess = file.getName();
		else
		{
			if (beamDir != null)
				nameToProcess = beamDir+File.separator+file.getName();
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


	/** Maps different processes on our node. */
	protected static final Map<String,ErlangRunner> nameToRunnerMap = new HashMap<String,ErlangRunner>();
	protected static int runnerNumber = 0;
	protected final String runnerMBox;

	/** The default server process to handle requests. */
	public static final String genServerDefault = "tracecheckServer";
	
	protected String genServerToCall = genServerDefault;
	
	/** Returns the name of this box. */
	public String getRunnerName()
	{
		return runnerMBox;
	}
	
	/** Delegates to {@link OtpMbox}. */
	public OtpErlangPid self()
	{
		return thisMbox.self();
	}

	/** Constructs a mailbox (an equivalent to a process) on this Erlang node. 
	 * 
	 *	@param nodeWithTraceCheckServer is the name of the node that will receive messages when Statechum needs to check validity of specific traces or collect coverage information. 
	 */
	public ErlangRunner(String nodeWithTraceCheckServer)
	{
		if (nodeWithTraceCheckServer == null || nodeWithTraceCheckServer.isEmpty())
			throw new IllegalArgumentException("invalid tracerunner node name");
		synchronized(nameToRunnerMap)
		{
			traceRunnerNode = nodeWithTraceCheckServer;
			runnerMBox = "erlangRunner_"+(runnerNumber++);nameToRunnerMap.put(runnerMBox, this);
			thisMbox = ErlangNode.getErlangNode().getNode().createMbox(runnerMBox);
			ourBox = new OtpErlangTuple(new OtpErlangObject[] {
					thisMbox.self(), ErlangNode.getErlangNode().getNode().createRef() });
		}
	}

	/** Only used by the runtime to force runner to use the default genserver (within tracerunner), used to send commands such as 'startrunner' starting a new otp server on a module of interest. */
	void forceReady()
	{
		mboxOpen = true;
	}
	
	/** This one has to be called after construction of the runner. */
	public void initRunner()
	{
		// Now create another genserver to handle requests from this runner. Upon failure, the box remains closed.
		try
		{
			mboxOpen = true;
			call(new OtpErlangObject[] { new OtpErlangAtom("startrunner"), new OtpErlangAtom(getRunnerName()) }, "Failed to start a new runner.");
			genServerToCall = getRunnerName();
		}
		catch(IllegalArgumentException ex)
		{// if anything fails, eliminate this runner
			close();
			throw(ex);// rethrow
		}
	}
	
	/** Closes the mailbox, causing the linked processes to receive an exception. */
	public synchronized void close()
	{
		synchronized(nameToRunnerMap)
		{
			if (mboxOpen && !genServerToCall.equals(genServerDefault))
				try
				{
					call(new OtpErlangObject[] { new OtpErlangAtom("terminate") }, 1000);// a short timeout here since we do not care whether we are successful. Dormant processes are not too bad; timeouts on closing mboxes are possibly worse if we are closing many inboxes following unexpected termination of Erlang runtime. 
				}
				catch(IllegalArgumentException ex)
				{// if anything fails, ignore this
				}
			nameToRunnerMap.remove(this);
			thisMbox.close();
			mboxOpen = false;
		}
	}
	
	/** Closes all mailboxes associated with the specific trace runner node. */
	public static void closeAll(String traceRunnerNode)
	{
		synchronized(nameToRunnerMap)
		{
			List<ErlangRunner> whatToRemove = new LinkedList<ErlangRunner>();
			for(Map.Entry<String,ErlangRunner> r:nameToRunnerMap.entrySet())
				if (r.getValue().traceRunnerNode.equals(traceRunnerNode))
					whatToRemove.add(r.getValue());
			for(ErlangRunner r:whatToRemove)
				r.close();
		}
	}
	
	/** Returns a static instance of this class. */
	static public ErlangRunner getRunner(String mboxToUse) {
		return nameToRunnerMap.get(mboxToUse);
	}
	
	/** Returns the directory where Beam, plt and other files will be placed, taking the current platform into account.
	 * This is a writable location not necessarily the same as the one containing Erlang source files.
	 * 
	 * @return binary directory
	 */
	static public File getErlangBeamDirectory()
	{
		File beamDir = new File(MessageFormat.format(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_ERLANGBEAM),ErlangRuntime.platformDescription));
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
	public static void compileErl(File whatToCompile, ErlangRunner useRunner, boolean compileIntoBeamDirectory)	throws IOException
	{
		File beamDirectory = compileIntoBeamDirectory?getErlangBeamDirectory():null;
		compileErl(whatToCompile,useRunner,null, beamDirectory);
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
	 * @param whereToPlaceBeam where to place the compiled file. If null, will use the file name of the Erlang module to determine where to place the compiled .beam file.
	 * @throws IOException
	 *             if something goes wrong.
	 */
	public static void compileErl(File whatToCompile, ErlangRunner useRunner, OtpErlangList compileFlags, File whereToPlaceBeam) throws IOException
	{
		String erlFileName = getName(whatToCompile, ERL.ERL,null);
		if (whatToCompile.getParentFile() == null)
			throw new IllegalArgumentException(
					"File does not have a parent directory " + whatToCompile);

		if (!whatToCompile.canRead())
			throw new IOException("file " + erlFileName + " does not exist");
		
		File beamDirectory = whereToPlaceBeam != null? whereToPlaceBeam:whatToCompile.getAbsoluteFile().getParentFile();
		
		if (whatToCompile.lastModified() > new File(getName(whatToCompile, ERL.BEAM,whereToPlaceBeam)).lastModified()) 
		{
			if (useRunner == null) {
				if (compileFlags != null)
					throw new IllegalArgumentException("Cannot compile an Erlang file with flags that are intended to be handled by OTP calling erlc");
				Process p = Runtime.getRuntime().exec(
						new String[] { ErlangRunner.getErlangBin() + "erlc",
								"+debug_info", erlFileName }, null, beamDirectory);
				ErlangRuntime.dumpProcessOutputOnFailure("erlc " + whatToCompile.getName(), p);
			} else 
			{
				useRunner.call(new OtpErlangObject[] {
								new OtpErlangAtom("compile"),
								new OtpErlangList(
										new OtpErlangObject[] { new OtpErlangAtom(
												erlFileName) }),
								compileFlags == null? new OtpErlangList():compileFlags,
								new OtpErlangAtom(beamDirectory.getAbsolutePath()) },
								"cannot compile ");
			}
		}
	}

	/** An argument to $gen_call */
	protected final OtpErlangTuple ourBox;
	/** The process on the Java side which communicates with Erlang process. */
	protected final OtpMbox thisMbox;
	/** The node where we expect our tracerunnerServer to run. */
	protected final String traceRunnerNode;

	/** Whether messagebox is valid. Becomes invalid after being closed. */
	protected boolean mboxOpen = false;
	
	public static final OtpErlangAtom okAtom = new OtpErlangAtom("ok");

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
	
	/** Using the supplied Erlang tuple, updates a supplied configuration. 
	 * 
	 * @param obj details to make to the pair.
	 * @return null if everything went well or a text of an error message (mostly java-related) if it went wrong.
	 */
	public static String updateConfiguration(Configuration config, OtpErlangObject obj)
	{
		String outcome = null;
		try
		{
			OtpErlangList list = (OtpErlangList) obj;
			for(OtpErlangObject p:list.elements())
			{
				OtpErlangTuple pair = (OtpErlangTuple) p;
				if (pair.arity() != 2)
					throw new IllegalArgumentException("key-value pair is not a pair, got "+p);
				config.assignValue(pair.elementAt(0).toString(), ((OtpErlangAtom)pair.elementAt(1)).atomValue(), true);
			}
		}
		catch(Throwable ex)
		{
			System.out.println(ex);
			outcome = ex.getMessage();
		}
		
		return outcome;
	}

	/** Extracts an attribute value from Erlang. */
	public OtpErlangObject getAttr(String name) {
		return call(
				new OtpErlangObject[] { new OtpErlangAtom("getAttr"),new OtpErlangAtom(name) }, "getAttr " + name)
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

	protected int responseTimeout = 10000;
	
	public void setResponseTimeout(int newValue)
	{
		responseTimeout = newValue;
	}
	
	public int getResponseTimeout()
	{
		return responseTimeout;
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
		OtpErlangObject response = call(args, responseTimeout);

		if (response instanceof OtpErlangAtom) 
		{
			if (!response.equals(okAtom)) 
				throw new IllegalArgumentException(errorMessage + " : error " + response);

			// success, but null response
		} 
		else if (response instanceof OtpErlangTuple) {
			OtpErlangTuple decodedResponse = (OtpErlangTuple) response;
			if (decodedResponse.arity() == 0)
				throw new IllegalArgumentException(errorMessage	+ " : unexpectedly short response (arity " + decodedResponse.arity() + ") " + response);
			OtpErlangObject decodedResponseCode = decodedResponse.elementAt(0);
			if (!(decodedResponseCode instanceof OtpErlangAtom))
				throw new IllegalArgumentException(errorMessage	+ " : unexpected type in response tuple " + decodedResponse);
			if (!decodedResponseCode.equals(okAtom)) 
			{
				if (((OtpErlangAtom) decodedResponseCode).atomValue().equals(ErlangThrownException.keyword))
					throw new ErlangThrownException(decodedResponse, errorMessage + " : has thrown " + decodedResponse);
				throw new IllegalArgumentException(errorMessage + " : error " + decodedResponse);

			}
			result = decodedResponse;
		} else
			throw new IllegalArgumentException(errorMessage + " : unexpected response type " + response);

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
	public synchronized OtpErlangObject call(OtpErlangObject[] args, int timeout) {
		if (!mboxOpen)
			throw new IllegalArgumentException("messagebox is closed");
		// The first argument is the registered process name, the second is the node name, followed by the tuple of arguments.
		thisMbox.send(genServerToCall, traceRunnerNode, new OtpErlangTuple(
				new OtpErlangObject[] { new OtpErlangAtom("$gen_call"), ourBox,	new OtpErlangTuple(args) }));
		OtpErlangTuple msg = null;
		try {
			if (timeout > 0)
				msg = (OtpErlangTuple) thisMbox.receive(timeout);
			else
				msg = (OtpErlangTuple) thisMbox.receive();
		} catch (OtpErlangExit e) {
			close();// kill this process
			Helper.throwUnchecked("exlang exit", e);
		} catch (OtpErlangDecodeException e) {
			Helper.throwUnchecked("decode exception", e);
		}
		if (msg == null)
			throw new IllegalArgumentException("timeout waiting for a response");
		return msg.elementAt(1);
	}
	
	/** Obtains a list of processes in Erlang. */
	public OtpErlangList listProcesses() 
	{
		OtpErlangList outcome = null;
		OtpErlangTuple tup = call(new OtpErlangObject[] { new OtpErlangAtom(
				"processes") }, "Failed to get process list.");
		if (tup != null && (tup.arity() == 2) && (tup.elementAt(0).equals(okAtom))) {
			outcome = (OtpErlangList) tup.elementAt(1);
		} else
			if (tup == null)
				throw new IllegalArgumentException("NULL response from call to get process list");
			else
				throw new IllegalArgumentException("Unexpected response from a call to get a process list: "
					+ tup.elementAt(0) + "(" + tup.arity() + ")");
		
		return outcome;
	}

	public OtpErlangTuple killProcesses(OtpErlangList proclist) 
	{
		OtpErlangTuple outcome = null;
		if (proclist != null) 
		{
			outcome = call(new OtpErlangObject[] {
					new OtpErlangAtom("killProcesses"), proclist },
					"Failed to kill processes.");
		}
		
		return outcome;
	}
}
