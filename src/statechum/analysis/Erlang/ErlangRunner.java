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
 */

package statechum.analysis.Erlang;

import java.io.File;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.ExperimentRunner.HandleProcessIO;
import statechum.apps.ErlangQSMOracle;

/** Manages the Erlang process running Erlang oracle. 
 * The idea is to start one when an instance of the runner is created and let it die 
 * when Java process terminates. 
 */
public class ErlangRunner 
{

	/** Given the name of Erlang file, this method strips out the extension and returns the result.
	 * The name is supposed to be a valid file name.
	 * 
	 * @param fileName File name to remove the extension from
	 * @return stripped file name.
	 */
	protected static String getErlName(String fileName)
	{
		if (fileName == null) return null;
		String trimmedName = fileName.trim();
		int pos = trimmedName.lastIndexOf('.');
		if (pos <= 0 || !trimmedName.substring(pos).equals(ERL.ERL.toString()))
			return null;
		return trimmedName.substring(0, pos);
	}

	public static boolean validName(String fileName)
	{
		return getErlName(fileName) != null;
	}
	
	public static enum ERL
	{
		ERL(".erl",false),BEAM(".beam",false),PLT(".plt",false),MOD("",true),NOEXT("",false);
		
		private ERL(String textualName, boolean strip)
		{
			stringRepresentation = textualName;stripPath = strip;
		}
		
		private final String stringRepresentation;
		private final boolean stripPath;
		
		public boolean getStrip()
		{
			return stripPath;
		}
		
		@Override
		public String toString()
		{
			return stringRepresentation;
		}
	}
	
	/** Given a file name, converts it to the appropriate extension.
	 * Throws IllegalArgumentException if this fails.
	 *  @param file file to convert
	 *  @param ext extension to add
	 */
	public static String getName(File file,ERL ext)
	{
		String nameToProcess = ext.getStrip()?file.getName():file.getAbsolutePath();
		String moduleName = getErlName(nameToProcess);
		if (moduleName == null) throw new IllegalArgumentException("Invalid module "+nameToProcess);
		
		return moduleName + ext.toString();
	}

	/** Obtains a binary directory for an Erlang executable. */
	public static String getErlangBin()
	{
		String erlangBin = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.ERLANGHOME);
		if (erlangBin != null) erlangBin = erlangBin+File.separator+"bin"+File.separator;else erlangBin = "";
		return erlangBin;
	}

	/** Erlang machine in which we run most stuff. */
    protected Process erlangProcess = null;
    /** Monitors the above machine and dumps its out and err to the console. */
    protected Thread stdDumper = null;
    
    protected String traceRunnerNode,ourNode;

    protected static final ErlangRunner staticRunner = new ErlangRunner();
    
    /** Returns a static instance of this class. */
    static public ErlangRunner getRunner() 
    {
    	return staticRunner;
    }
    
    /** Compiles the supplied file into .beam if .erl has been modified after an existing .beam (date of last change is 0 if file does not exist).
     * 
     * @param whatToCompile file to compile
     * @param useRunner ask Erlang compiler to perform the compile - no need to launch compiler as a separate process.
     * @throws IOException if something goes wrong.
     */
    public static void compileErl(File whatToCompile, ErlangRunner useRunner) throws IOException  
    {
    	String erlFileName = getName(whatToCompile,ERL.ERL);
    	File parentFile = whatToCompile.getParentFile();
    	if (parentFile == null) throw new IllegalArgumentException("File does not have a parent directory "+whatToCompile);

    	if (!whatToCompile.canRead()) throw new IOException("file "+erlFileName+" does not exist");
        if (whatToCompile.lastModified() > new File(getName(whatToCompile,ERL.BEAM)).lastModified())
        {
        	if (useRunner == null)
        	{
		       Process p = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin()+"erlc","+debug_info",erlFileName}, null, parentFile);
		       dumpProcessOutputOnFailure("erlc "+whatToCompile.getName(),p);
        	}
        	else
        	{
        		useRunner.call(new OtpErlangObject[]{new OtpErlangAtom("compile"),
        				new OtpErlangList(new OtpErlangObject[]{new OtpErlangAtom(erlFileName)}),
        				new OtpErlangAtom("erlc"),new OtpErlangAtom(parentFile.getAbsolutePath())
        		},"cannot compile ");
        	}
        }
    }
    
    public static final int timeBetweenChecks = 500;
    
    public static final File ErlangFolder = new File("ErlangOracle");
    
    /** Runs the supplied process and returns output and error streams in an exception if the process
     * returned a non-zero error code. Upon success, no output is produced.
     * 
     * @param p process to run.
     */
    public static void dumpProcessOutputOnFailure(String name,Process p) {
    	final StringBuffer err=new StringBuffer(),out=new StringBuffer(); 
        ExperimentRunner.dumpStreams(p, timeBetweenChecks, new HandleProcessIO() {

            @Override
            public void OnHeartBeat() {// no prodding is done for a short-running converter.
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
            ;
        }
        
        if (p.exitValue() != 0)
        	throw new IllegalArgumentException("Failure running "+name+"\n"+err+(err.length()>0?"\n":"")+out);
    }

    /** How long to wait for a server to start. */
    protected int serverTimeout = 10;
    
    /** Sets new timeout - very useful to increase it for testing. */
    public void setTimeout(int time)
    {
    	serverTimeout = time;
    }
    
    /** Starts Erlang in the background,
     * 
     * @param runnerMode "tracerunner" means production use, noserver,halt,error are used for testing.
     * @param delay how long to wait after launching the server - used only for testing to ensure server terminates before our loop starts.
     */
    public void startErlang(String runnerMode, long delay) 
    {
    	try
    	{
            if (erlangProcess == null) {
            	String tracerunnerProgram = "tracerunner.erl";
            	String uniqueID = "_"+System.nanoTime()+"_"+ManagementFactory.getRuntimeMXBean().getName().replace('@', '_')+"@localhost";
            	traceRunnerNode="tracerunner"+uniqueID;ourNode = "java"+uniqueID;
            	// now we simply evaluate "halt()." which starts epmd if necessary and we can check along the way that we can run Erlang at all.
            	Process p=Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin() + "erl","-eval","halt().","-sname",traceRunnerNode,"-noshell","-setcookie",uniqueID}, null, new File(ErlangQSMOracle.ErlangFolder));
                dumpProcessOutputOnFailure("testing that Erlang can be run at all",p);
                
                // The compilation phase could a few seconds but only needs to be done once after installation of Statechum
                compileErl(new File(ErlangQSMOracle.ErlangFolder,tracerunnerProgram),null);
                for (File f : new File(ErlangQSMOracle.ErlangTyper).listFiles())
                    if (ErlangRunner.validName(f.getName()))
                        ErlangRunner.compileErl(f,null);

                // Based on http://erlang.org/pipermail/erlang-questions/2010-March/050226.html
                OtpNode self = new OtpNode(ourNode, uniqueID); // identify self
                thisMbox = self.createMbox("thisMbox");
                
                // Now build environment variables to ensure that dialyzer will find a directory to put its plt file in.
                // This is actually an bug of dialyzer which manifests itself on Windows - 
                // there is no need for dialyzer_options:build(Opts) to call dialyzer_plt:get_default_plt()
                // before build_options(Opts, DefaultOpts1).
    	        List<String> envpList = new LinkedList<String>();
    	        for(Entry<String,String> entry:System.getenv().entrySet())
    	        	if (!entry.getKey().equals("HOME"))
    	        		envpList.add(entry.getKey()+"="+entry.getValue());
    	        envpList.add("HOME="+new File(ErlangQSMOracle.ErlangFolder).getAbsolutePath());
    	         
                erlangProcess = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin() + "erl",
                		"-pa",new File(ErlangQSMOracle.ErlangFolder).getAbsolutePath(),
                		"-pa",new File(ErlangQSMOracle.ErlangTyper).getAbsolutePath(),// the easiest way to substitute our module in place of the original Erlang's one, otherwise I'd have to rely on tracerunner:compileAndLoad
                		"-run","tracerunner","start",ourNode,runnerMode,"-sname",traceRunnerNode,"-noshell","-setcookie",uniqueID}, envpList.toArray(new String[0]), new File(ErlangQSMOracle.ErlangFolder));
                stdDumper = new Thread(new Runnable() {
					
					@Override
					public void run() {
					       ExperimentRunner.dumpStreams(erlangProcess, timeBetweenChecks, new HandleProcessIO() {

					            @Override
					            public void OnHeartBeat() {// no prodding is done - we are being prodded by Erlang instead.
					            }

					            @Override
					            public void StdErr(StringBuffer b) {
					                System.out.print("[ERLANG] "+b.toString());
					            }

					            @Override
					            public void StdOut(StringBuffer b) {
					                System.out.print("[ERLERR] "+b.toString());
					            }
					        });
					}
				});
                stdDumper.setDaemon(true);stdDumper.start();
                if (delay > 0) Thread.sleep(delay);
                // At this point, the process may have not yet started or already terminated, it is easy to find out which of the
                // two has happened by doing
                int timeout = serverTimeout;
                while(!self.ping(traceRunnerNode, 500) && timeout > 0)
                {
                	try
                	{
                		erlangProcess.exitValue();
                		// process terminated, record this as a failure
                		timeout = 0;
                	}
                	catch(IllegalThreadStateException e)
                	{// process not yet terminated, hence we keep waiting
                		--timeout;
                	}
                }
                ourBox = new OtpErlangTuple(new OtpErlangObject[]{thisMbox.self(),self.createRef()});
                if (timeout <= 0) throw new IllegalArgumentException("timeout waiting for a server to start");
            }
    	}
    	catch(IOException e)
    	{
    		killErlang();
    		Helper.throwUnchecked("failed to start Erlang", e);
    	}
    	catch(InterruptedException e1)
    	{
    		killErlang();
    		Helper.throwUnchecked("terminating as requested", e1);
    	}
    }

    /** An argument to $gen_call */
    protected OtpErlangTuple ourBox = null;
    /** The process on the Java side which communicates with Erlang process. */ 
    protected OtpMbox thisMbox = null;
    
    public static final OtpErlangAtom okAtom = new OtpErlangAtom("ok");
    
    /** Makes a call and waits for a response.
     * 
     * @param args components of the tuple to pass a server
     * @return Response from the server so long as the server returns { ok, Response }. 
     * Upon a failure {error, Failure }, an error message is thrown as RuntimeException with Failure included in it.
     */
    public OtpErlangTuple call(OtpErlangObject[]args, String errorMessage)
    {
    	OtpErlangTuple result = null;
    	OtpErlangObject response = call(args,0);
    	
    	if (response instanceof OtpErlangAtom)
    	{
    		if (!response.equals(okAtom)) 
    			throw new RuntimeException(errorMessage+" : error "+response+" but the server did not say more");
    		
    		// success, but null response
    	}
    	else if (response instanceof OtpErlangTuple)
    	{
    		OtpErlangTuple decodedResponse = (OtpErlangTuple) response;
    		if (decodedResponse.arity() == 0) throw new RuntimeException(errorMessage+" : unexpectedly short response (arity "+decodedResponse.arity()+") "+response);
    		OtpErlangObject decodedResponseCode = decodedResponse.elementAt(0);
    		if (!(decodedResponseCode instanceof OtpErlangAtom))
    				throw new RuntimeException(errorMessage+" : unexpected type in response tuple "+decodedResponse);
    		if (!decodedResponseCode.equals(okAtom))
    		{
    			if (decodedResponse.arity() == 2)
    				// try to beautify the error
    				throw new RuntimeException(errorMessage+" : error "+decodedResponse.elementAt(1).toString());
				throw new RuntimeException(errorMessage+" : error "+decodedResponse);
    			
    		}
    		result = decodedResponse;
    	}
    	else throw new RuntimeException(errorMessage+" : unexpected response type "+response);
    	
    	return result;
    }
    
    /** Parses an evaluates a supplied text using Erlang. */
	public OtpErlangObject evaluateString(String text)
	{
		OtpErlangTuple outcome = call(new OtpErlangObject[]{new OtpErlangAtom("evaluateTerm"), new OtpErlangString(text+".")},
				"evaluation of "+text);
		return outcome.elementAt(1);
	}
    
   /** Makes a call and waits for a response for the specified duration, infinitely if timeout is zero.
     * 
     * @param args components of the tuple to pass a server
     * @param timeout how long to wait for a response.
     * @return Response from the server or null on a timeout
     */
    public OtpErlangObject call(OtpErlangObject[]args,int timeout)
    {
    	if (erlangProcess == null) startErlang("tracerunner",0);
        thisMbox.send("tracecheckServer", traceRunnerNode, new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom("$gen_call"),
        		ourBox,
        		new OtpErlangTuple(args)})
        );
        OtpErlangTuple msg = null;
		try {
			if (timeout > 0)
				msg = (OtpErlangTuple)thisMbox.receive(timeout);
			else
				msg = (OtpErlangTuple)thisMbox.receive();
		} catch (OtpErlangExit e) {
			Helper.throwUnchecked("exlang exit", e);
		} catch (OtpErlangDecodeException e) {
			Helper.throwUnchecked("decode exception", e);
		}
		if (msg == null) throw new IllegalArgumentException("timeout waiting for a response");
        return msg.elementAt(1);
    }
    
    public void killErlang() 
    {
        if (erlangProcess != null) 
        {
            erlangProcess.destroy();
            try {
                erlangProcess.waitFor();
            } catch (InterruptedException e) {
                statechum.Helper.throwUnchecked("wait for Erlang to terminate aborted", e);
            }
            erlangProcess = null;ourBox = null;thisMbox = null;
            stdDumper.interrupt();// if this one is sleeping, interrupt will wake it and it will terminate since Erlang is already no more ...
            try {
				stdDumper.join();
			} catch (InterruptedException e) {
				Helper.throwUnchecked("Interrupt waiting for erlang out/err dumper to terminate", e);
			}// wait for that to happen.
            stdDumper = null;
        }
    }

}
