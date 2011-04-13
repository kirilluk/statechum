package statechum.analysis.Erlang;

import java.io.File;
import java.io.IOException;
import java.lang.management.ManagementFactory;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpLocalNode;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpMsg;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.ExperimentRunner.HandleProcessIO;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.apps.ErlangApplicationLoader;
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
	public static String getErlName(String fileName)
	{
		if (fileName == null) return null;
		String trimmedName = fileName.trim();
		int pos = trimmedName.lastIndexOf('.');
		if (pos <= 0 || !trimmedName.substring(pos).equals(".erl"))
			return null;
		return trimmedName.substring(0, pos);
	}

	/** Obtains a binary directory for an Erlang executable. */
	public static String getErlangBin()
	{
		String erlangBin = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.ERLANGHOME);
		if (erlangBin != null) erlangBin = erlangBin+File.separator+"bin"+File.separator;else erlangBin = "";
		return erlangBin;
	}

    protected Process erlangProcess = null;
    protected String traceRunnerNode,ourNode;

    public static void main(String arg[])
    {
    	ErlangRunner r = new ErlangRunner();
    	r.startErlang();
    }
   
    public static void compileErl(File whatToCompile) throws IOException  
    {
        Process p = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin()+"erlc","+debug_info",whatToCompile.getName()}, null, whatToCompile.getParentFile());
        ErlangApplicationLoader.dumpProcessOutputOnFailure("erlc "+whatToCompile.getName(),p);
    }
    
    public void startErlang() {
        try {
            if (erlangProcess == null) {
            	String tracerunnerProgram = "tracerunner.erl";
            	String uniqueID = "_"+System.nanoTime()+"_"+ManagementFactory.getRuntimeMXBean().getName().replace('@', '_')+"@localhost";
            	traceRunnerNode="tracerunner"+uniqueID;ourNode = "java"+uniqueID;
            	// now we simply evaluate "halt()." which starts epmd if necessary and we can check along the way that we can run Erlang at all.
            	Process p=Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin() + "erl","-eval","halt().","-sname",traceRunnerNode,"-noshell","-setcookie",uniqueID}, null, new File(ErlangQSMOracle.ErlangFolder));
                ErlangApplicationLoader.dumpProcessOutputOnFailure("testing that Erlang can be run at all",p);
                
                if (new File(ErlangQSMOracle.ErlangFolder,tracerunnerProgram).lastModified() > new File(ErlangQSMOracle.ErlangFolder,getErlName(tracerunnerProgram)+".beam").lastModified())
                	compileErl(new File(ErlangQSMOracle.ErlangFolder,tracerunnerProgram));
                
                
                // Based on http://erlang.org/pipermail/erlang-questions/2010-March/050226.html
                OtpNode self = new OtpNode(ourNode, uniqueID); // identify self
                OtpMbox thisMbox = self.createMbox("thisMbox");
                
                erlangProcess = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin() + "erl","-run","tracerunner","start",ourNode,"-sname",traceRunnerNode,"-noshell","-setcookie",uniqueID}, null, new File(ErlangQSMOracle.ErlangFolder));
                
                // At this point, the process may have not yet started or already terminated, it is easy to find out which of the
                // two has happened by doing
                int timeout = 10;
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
                
                if (timeout <= 0) throw new IllegalArgumentException("timeout waiting for a server to start");
                thisMbox.send("tracecheckServer", traceRunnerNode, new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom("$gen_call"),
                		new OtpErlangTuple(new OtpErlangObject[]{thisMbox.self(),self.createRef()}),
                		new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom("runTrace"),new OtpErlangAtom("someData")})})
                );
                OtpErlangTuple msg = (OtpErlangTuple)thisMbox.receive();
                OtpErlangTuple response = (OtpErlangTuple)msg.elementAt(1);
                System.out.println(response);
                System.out.println();
            }
        } catch (Exception e) {
        	e.printStackTrace();
            killErlang();
        }
    }

    public void killErlang() 
    {
        if (erlangProcess != null) {
            erlangProcess.destroy();
             try {
                erlangProcess.waitFor();
            } catch (InterruptedException e) {
                statechum.Helper.throwUnchecked("wait for Erlang to terminate aborted", e);
            }
            erlangProcess = null;
        }
    }

}
