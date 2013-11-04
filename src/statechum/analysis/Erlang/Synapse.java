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

import statechum.apps.QSMTool;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNodeStatus;

public class Synapse implements Runnable {

	final OtpMbox thisMbox;
	final String nodeToRunTracesIn;
	
	public Synapse(String ourNode, String cookie, String mailBox, String partnerNode) 
	{
		ErlangNode.initNodeParameters(ourNode, cookie);ErlangNode.getErlangNode().createNode();
		
		thisMbox = ErlangNode.getErlangNode().getNode().createMbox(mailBox);// we do not use ErlangRunner here because runner is supposed to communicate with our tracerunner but the object of communication is user code in Erlang that called us. 
		nodeToRunTracesIn = partnerNode;
	}

	public static void main(String[] args) 
	{
		final String erlangNode = args[3];
		Synapse s = new Synapse(args[0],args[1],args[2],erlangNode);
		/*
		Thread watchdog = new Thread(new Runnable(){

			@Override
			public void run() {
				for(;;) {
					if (!ErlangNode.getErlangNode().getNode().ping(erlangNode, 500))
						System.exit(-1);// forcefully kill Java because Erlang has terminated.
				}
			}});
		
		watchdog.setDaemon(true);watchdog.run();*/
		
		ErlangNode.getErlangNode().getNode().registerStatusHandler(new OtpNodeStatus(){
			public void remoteStatus(java.lang.String node, boolean up, @SuppressWarnings("unused") java.lang.Object info)
			{
				if (node.equals(erlangNode) && up == false)
					System.exit(-1);// forcefully kill Java because Erlang has terminated.
			}
			
			public void localStatus(java.lang.String node, boolean up, @SuppressWarnings("unused") java.lang.Object info)
			{
				if (node.equals(erlangNode) && up == false)
					System.exit(-1);// forcefully kill Java because Erlang has terminated.
			}
		});
		
		s.run();
	}

	public void reply(OtpErlangPid where, OtpErlangRef ref, OtpErlangAtom responseCode, OtpErlangObject data)
	{
		OtpErlangObject [] whatToSend = null;
		if (data != null)
			whatToSend = new OtpErlangObject[]{ref,responseCode,data};
		else
			whatToSend = new OtpErlangObject[]{ref,responseCode};
		
		thisMbox.send(where, new OtpErlangTuple(whatToSend));
	}
	
	public static final OtpErlangAtom 
		msgEcho = new OtpErlangAtom("echo"), // used both for testing and to check that Statechum process responds to messages
		msgTerminate = new OtpErlangAtom("terminate"), // terminates Statechum if sent to the supervisor process or individual tasks when sent to them.
		msgRunTask = new OtpErlangAtom("runTask"), // starts a learner. The response is a pid of the said learner which can receive configuration messages and those to start learning of visualisation.
		msgComputeDiff =  new OtpErlangAtom("computeDiff"), 
		msgDisplayDiff = new OtpErlangAtom("displayDiff"), 
		msgDisplayFSM = new OtpErlangAtom("displayFSM"),
		
		msgOk = new OtpErlangAtom("ok"),// a response suggesting that command completed successfully
		msgInvalidCommand = new OtpErlangAtom("invalidcommand_or_missing_args"),// returned from tasks to indicate that either the command was unrecognised or the number of arguments to it was wrong (usually there is just one argument).
		msgFailure = new OtpErlangAtom("failure"),// indicates a failure of either Statechum supervisor process or a task
				msgStarted = new OtpErlangAtom("started"),
						msgUpdateConfiguration = new OtpErlangAtom("updateConfiguration"),// used to modify configuration used by the task, the argument should be a list of tuples.
	msgLine = new OtpErlangAtom("line")
		
		;
	
	
	/** Messages have a fixed structure, PID,Ref,Atom,Data. */
	@Override
	public void run() {
		System.out.println("synapse started.");
		/*
		ErlangNode.getErlangNode().getNode().registerStatusHandler(new OtpNodeStatus(){
			public void remoteStatus(java.lang.String node, boolean up, java.lang.Object info)
			{
				
			}
			
			public void localStatus(java.lang.String node, boolean up, java.lang.Object info)
			{
				
			}
		});
		 */
		for(;;)
		{
			try {
				OtpErlangObject msg=thisMbox.receive();
				if (!(msg instanceof OtpErlangTuple))
					System.out.println("invalid message received, expected tuple, got "+msg);
				OtpErlangTuple message = (OtpErlangTuple)msg;
				if (message.arity() != 3)
					System.out.println("invalid tuple received, expected four elements, got "+msg);
				
				if (!(message.elementAt(2) instanceof OtpErlangAtom))
					System.out.println("invalid request received, expected an atom, got "+message.elementAt(2));
				OtpErlangAtom command = (OtpErlangAtom) message.elementAt(2);
				if (!(message.elementAt(1) instanceof OtpErlangRef))
					System.out.println("invalid request received, expected a ref, got "+message.elementAt(1));
				OtpErlangRef ref = (OtpErlangRef) message.elementAt(1);
				if (!(message.elementAt(0) instanceof OtpErlangPid))
					System.out.println("invalid request received, expected a pid, got "+message.elementAt(0));
				OtpErlangPid pid = (OtpErlangPid) message.elementAt(0);
				
				if (command.equals(msgEcho))
				{// used to establish a link between a supervisor on the Erlang side and that of the Java side
					thisMbox.link(pid);
					reply(pid,ref,msgOk,null);
				}
				else
				if (command.equals(msgTerminate))
					break;
				else
					if (command.equals(msgRunTask))
					{
						Thread worker = new Thread(new StatechumProcess(pid, thisMbox.self(), ref,nodeToRunTracesIn));
						worker.setDaemon(true);
						worker.start();
					}
					else 
						reply(pid,ref,msgInvalidCommand,null);
				
			} catch (OtpErlangDecodeException e) 
			{// communication failure, hence terminate. 
				System.out.println("communication failure "+e);
			}
			catch(OtpErlangExit nodeExited)
			{
				System.out.println("Node exited "+nodeExited);
				ErlangRunner.closeAll(nodeToRunTracesIn);
			} // do nothing, assuming we've been asked to terminate
		}
		thisMbox.close();
		System.out.println("terminated");
	}

	public static class StatechumProcess extends QSMTool implements Runnable
	{
		protected static long mboxNumber=0;

		/** Process corresponding to this thread. */
		protected final OtpMbox mbox;
		
		/** Erlang process we are communicating with. */
		protected final OtpErlangPid erlangPartner, supervisor;
		
		/** Reference to include in the first response. */
		protected final OtpErlangRef refFirstResponse;
		
		/** Name of this process. */
		protected final String mboxName = "statechumProcess_"+(mboxNumber++);  
		
		/** Node with tracerunner. */
		protected final String nodeWithTraceRunner;
		
		public StatechumProcess(OtpErlangPid erlangPid, OtpErlangPid supervisorPid, OtpErlangRef refArg, String nodeWithTraceRunnerArg)
		{
			refFirstResponse = refArg;
			mbox = ErlangNode.getErlangNode().getNode().createMbox(mboxName);erlangPartner = erlangPid;supervisor = supervisorPid;
			nodeWithTraceRunner = nodeWithTraceRunnerArg;
		}
		
		@Override
		public void run() 
		{
			ErlangRunner runner =  nodeWithTraceRunner.isEmpty()? ErlangRuntime.getDefaultRuntime().createNewRunner():new ErlangRunner(nodeWithTraceRunner);
			runner.initRunner();
			try {
				mbox.link(erlangPartner);mbox.link(runner.self());mbox.link(supervisor);// ensure a link with both tracerunner and our supervisor
				learnerInitConfiguration.config.setErlangMboxName(runner.getRunnerName());learnerInitConfiguration.config.setUseExternalErlangRuntime(true);
				
				// sends our PID. The idea is that the initial call goes to the Synapses' main mailbox (with the name chosen by the Erlang process that started it) 
				// and the response is the PID of the runner that should be used with the specific Statechum thread. 
				// This involves setting up configuration, traces and running either a learner or fsmdiff.
				mbox.send(erlangPartner,new OtpErlangTuple(new OtpErlangObject[]{refFirstResponse,mbox.self()}));
				for(;;)
				{
						OtpErlangObject msg=mbox.receive();
						if (!(msg instanceof OtpErlangTuple))
							System.out.println("invalid message received, expected tuple, got "+msg);
						OtpErlangTuple message = (OtpErlangTuple)msg;
						if (message.arity() < 2)
							System.out.println("invalid tuple received, expected at least two elements, got "+msg);
						
						if (!(message.elementAt(1) instanceof OtpErlangAtom))
							System.out.println("invalid request received, expected an atom, got "+message.elementAt(1));
						OtpErlangAtom command = (OtpErlangAtom) message.elementAt(1);
						if (!(message.elementAt(0) instanceof OtpErlangRef))
							System.out.println("invalid request received, expected a ref, got "+message.elementAt(0));
						OtpErlangRef ref = (OtpErlangRef) message.elementAt(0);
						if (command.equals(msgEcho))
							mbox.send(erlangPartner,new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk}));
						if (command.equals(msgTerminate))
							break;
						else
							if (command.equals(msgUpdateConfiguration) && message.arity() != 3)
							{
								String outcome = ErlangRunner.updateConfiguration(learnerInitConfiguration.config, message.elementAt(2));
								if (outcome == null)
									mbox.send(erlangPartner,new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk}));
								else
									mbox.send(erlangPartner,new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(outcome)}));// report an error.
							}
							else
								if (command.equals(msgLine) && message.arity() != 3)
								{
									process(message.elementAt(2).toString());
								}
								else
									if (command.equals(msgRunTask))
									{
									}
									else 
										mbox.send(erlangPartner,new OtpErlangTuple(new OtpErlangObject[]{ref,msgInvalidCommand}));
				}
						
			} catch (OtpErlangDecodeException e) 
			{// communication failure, hence terminate. 
				System.out.println("communication failure "+e);
			}
			catch(OtpErlangExit nodeExited)
			{
				System.out.println("Node exited "+nodeExited);
			} // do nothing, assuming we've been asked to terminate
			runner.close();
		}
	}
	
}
