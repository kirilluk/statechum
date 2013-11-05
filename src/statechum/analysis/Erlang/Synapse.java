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

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Label;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.apps.QSMTool;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
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

		ErlangNode.getErlangNode().getNode().registerStatusHandler(new OtpNodeStatus(){
			@Override
			public void remoteStatus(java.lang.String node, boolean up, @SuppressWarnings("unused") java.lang.Object info)
			{
				if (node.equals(erlangNode) && up == false)
					System.exit(-1);// forcefully kill Java because Erlang has terminated.
			}
			
			@Override
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
		msgGetNodeName = new OtpErlangAtom("getNodeName"),// reports the name of this node.
		msgGetStatechumWorker = new OtpErlangAtom("getStatechumWorker"), // starts a learner. The response is a pid of the said learner which can receive configuration messages and those to start learning of visualisation.
		msgComputeDiff =  new OtpErlangAtom("computeDiff"), 
		msgDisplayDiff = new OtpErlangAtom("displayDiff"), 
		msgDisplayFSM = new OtpErlangAtom("displayFSM"),
		msgLearn = new OtpErlangAtom("learn"),
		msgTraces = new OtpErlangAtom("traces"),
		msgGetTraces = new OtpErlangAtom("getTraces"),
		msgLoadFSM = new OtpErlangAtom("loadFSM"),
		msgGetFSM = new OtpErlangAtom("getFSM"),
		
		msgOk = new OtpErlangAtom("ok"),// a response suggesting that command completed successfully
		msgWorkerOk = new OtpErlangAtom("workerok"),// a response suggesting that command completed successfully by the worker
		msgInvalidCommand = new OtpErlangAtom("invalidcommand_or_missing_args"),// returned from tasks to indicate that either the command was unrecognised or the number of arguments to it was wrong (usually there is just one argument).
		msgFailure = new OtpErlangAtom("failure"),// indicates a failure of either Statechum supervisor process or a task
		msgStarted = new OtpErlangAtom("started"),
		msgUpdateConfiguration = new OtpErlangAtom("updateConfiguration"),// used to modify configuration used by the task, the argument should be a list of tuples.
		msgLine = new OtpErlangAtom("line")
		
		;
	
	
	/** Messages have a fixed structure, PID,Ref,Atom,Data. */
	@Override
	public void run() {
		if (System.getProperty("SYNAPSE_TERMINATE") != null)
		{// only used for testing
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				// assume we were asked to terminate
			}
			System.exit(-1);
		}
		
		System.out.println("Synapse started.");
		for(;;)
		{
			try {
				OtpErlangObject msg=thisMbox.receive();
				if (!(msg instanceof OtpErlangTuple))
					System.out.println("invalid message received, expected tuple, got "+msg);
				OtpErlangTuple message = (OtpErlangTuple)msg;
				if (message.arity() != 3)
					System.out.println("invalid tuple received, expected three elements, got "+msg);
				
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
					reply(pid,ref,msgOk,thisMbox.self());
				}
				else
				if (command.equals(msgGetNodeName))
				{
					reply(pid,ref,msgOk,new OtpErlangAtom(ErlangNode.getErlangNode().getName()));
				}
				else
				if (command.equals(msgTerminate))
					break;
				else
					if (command.equals(msgGetStatechumWorker))
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
				//System.out.println("Node exited "+nodeExited);
				ErlangRunner.closeAll(nodeToRunTracesIn);
			} // do nothing, assuming we've been asked to terminate
		}
		thisMbox.close();
		System.out.println("Synapse terminated");
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
		
		protected void parseTraces(OtpErlangObject traces)
		{
			OtpErlangList listOfTraces = (OtpErlangList)traces;
			for(OtpErlangObject entryObj:listOfTraces)
			{
				OtpErlangTuple entry = (OtpErlangTuple)entryObj;
				if (entry.arity() != 2)
					throw new IllegalArgumentException("invalid trace: more than a pair of pos/neg and data");
				
				boolean positive = false;
				OtpErlangAtom traceType = (OtpErlangAtom)entry.elementAt(0);
				if (traceType.atomValue().equals("pos"))
					positive = true;
				else
					if (traceType.atomValue().equals("neg"))
						positive = false;
					else
						throw new IllegalArgumentException("invalid trace: got "+entry.elementAt(0)+" instead of pos/neg");
				
				OtpErlangList traceData = (OtpErlangList)entry.elementAt(1);
				List<Label> data = new LinkedList<Label>();
				for(OtpErlangObject traceElement:traceData)
					data.add(AbstractLearnerGraph.generateNewLabel( ((OtpErlangAtom)traceElement).atomValue(), learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()));
				
				if (positive) sPlus.add(data);else sMinus.add(data);
			}		
		}

		/** Given a representation of FSM in a form of Erlang tuple, builds the corresponding graph.
		 * 
		 * @param obj FSM to parse
		 * @param gr graph to fill in (whatever was in there will be replaced by the data loaded from the graph, this is necessary since we do not know the specific type to instantiate here).
		 * @param converter label converter
		 */
		public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void parseStatemachine(OtpErlangObject obj,AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> gr, ConvertALabel converter)
		{
//		0	  states :: list(state()),
//		1	  transitions :: list(transition()),
//		2	  initial_state :: state(),
//		3	  alphabet :: list(event())
			OtpErlangTuple machine = (OtpErlangTuple)obj;
			if (machine.arity() != 5)
				throw new IllegalArgumentException("expected 5 components in FSM");
			if (!((OtpErlangAtom)machine.elementAt(0)).atomValue().equals("statemachine"))
				throw new IllegalArgumentException("first element of a record should be \"statemachine\"");
			OtpErlangList states = (OtpErlangList)machine.elementAt(1),transitions = (OtpErlangList)machine.elementAt(2),alphabet = (OtpErlangList)machine.elementAt(4);
			OtpErlangAtom initial_state = (OtpErlangAtom)machine.elementAt(3);

			if (states.arity() == 0)
				throw new IllegalArgumentException("empty automaton");
			
			gr.initEmpty();
			
			for(OtpErlangObject st:states)
			{
				String state = ((OtpErlangAtom)st).atomValue();if (state.isEmpty()) throw new IllegalArgumentException("empty state name");
				gr.transitionMatrix.put(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID( state ), gr.config),gr.createNewRow());
			}
			
			if (states.arity() != gr.transitionMatrix.size())
				throw new IllegalArgumentException("repeated states in the list of states");
			
			Map<OtpErlangAtom,Label> atomToLabel = new HashMap<OtpErlangAtom,Label>();
			for(OtpErlangObject l:alphabet)
			{
				String label = ((OtpErlangAtom)l).atomValue();if (label.isEmpty()) throw new IllegalArgumentException("empty label");
				atomToLabel.put((OtpErlangAtom)l,AbstractLearnerGraph.generateNewLabel( label,gr.config,converter));
			}
			for(OtpErlangObject transitionObj:transitions)
			{
				OtpErlangTuple transition = (OtpErlangTuple) transitionObj;
				if (transition.arity() != 3)
					throw new IllegalArgumentException("expected 3 components in transition "+transition);
				OtpErlangAtom from = (OtpErlangAtom)transition.elementAt(0),label = (OtpErlangAtom)transition.elementAt(1),to = (OtpErlangAtom)transition.elementAt(2);
				CmpVertex fromState = gr.findVertex(VertexID.parseID(from.atomValue())), toState = gr.findVertex(VertexID.parseID(to.atomValue()));
				if (fromState == null)
					throw new IllegalArgumentException("invalid source state "+from.atomValue());
				if (toState == null)
					throw new IllegalArgumentException("invalid target state"+to.atomValue());
				if (!atomToLabel.containsKey(label))
					throw new IllegalArgumentException("unknown label");
				gr.addTransition(gr.transitionMatrix.get(fromState),atomToLabel.get(label),toState);
			}
			
			gr.setInit(gr.findVertex(VertexID.parseID(initial_state.atomValue())));
			if (gr.getInit() == null)
				throw new IllegalArgumentException("missing initial state");
		}
		
		/** Turns the supplied graph into an Erlang tuple. 
		 * @param gr graph to convert. 
		 */
		public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> OtpErlangTuple constructFSM(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> gr)
		{
			List<OtpErlangObject> statesList = new LinkedList<OtpErlangObject>(), transitions = new LinkedList<OtpErlangObject>();
			Set<OtpErlangAtom> alphabet = new HashSet<OtpErlangAtom>();
			
			for(Entry<CmpVertex,Map<Label,TARGET_TYPE>> entry:gr.transitionMatrix.entrySet()) 
			{
				statesList.add(new OtpErlangAtom(entry.getKey().getStringId()));
				for(Entry<Label,TARGET_TYPE> transition:entry.getValue().entrySet())
				{
					OtpErlangAtom label = new OtpErlangAtom(transition.getKey().toErlangTerm());
					alphabet.add(label);
					for(CmpVertex target:gr.getTargets(transition.getValue()))
						transitions.add(new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom(entry.getKey().getStringId()), label, new OtpErlangAtom(target.getStringId())}));
				}
			}
			
			return new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom("statemachine"),new OtpErlangList(statesList.toArray(new OtpErlangObject[0])),
					new OtpErlangList(transitions.toArray(new OtpErlangObject[0])),
					new OtpErlangAtom(gr.getInit().getStringId()),new OtpErlangList(alphabet.toArray(new OtpErlangObject[0])),
			});
		}
		
		@Override
		public void run() 
		{
			ErlangRunner runner =  null;
			try {
				//ErlangRunner runner =  nodeWithTraceRunner.isEmpty()? ErlangRuntime.getDefaultRuntime().createNewRunner():new ErlangRunner(nodeWithTraceRunner);runner.initRunner();
				//mbox.link(runner.self());learnerInitConfiguration.config.setErlangMboxName(runner.getRunnerName());learnerInitConfiguration.config.setUseExternalErlangRuntime(true);

				mbox.link(erlangPartner);mbox.link(supervisor);// ensure a link with both tracerunner and our supervisor
				
				// sends our PID. The idea is that the initial call goes to the Synapses' main mailbox (with the name chosen by the Erlang process that started it) 
				// and the response is the PID of the runner that should be used with the specific Statechum thread. 
				// This involves setting up configuration, traces and running either a learner or fsmdiff.
				mbox.send(erlangPartner,new OtpErlangTuple(new OtpErlangObject[]{refFirstResponse,mbox.self()}));
				for(;;)
				{
						OtpErlangObject msg=mbox.receive();
						if (!(msg instanceof OtpErlangTuple))
							System.out.println("invalid message received by worker, expected tuple, got "+msg);
						OtpErlangTuple message = (OtpErlangTuple)msg;
						if (message.arity() < 2)
							System.out.println("invalid tuple received by worker, expected at least two elements, got "+msg);
						
						if (!(message.elementAt(1) instanceof OtpErlangAtom))
							System.out.println("invalid request received by worker, expected an atom, got "+message.elementAt(1));
						OtpErlangAtom command = (OtpErlangAtom) message.elementAt(1);
						if (!(message.elementAt(0) instanceof OtpErlangRef))
							System.out.println("invalid request received by worker, expected a ref, got "+message.elementAt(0));
						OtpErlangRef ref = (OtpErlangRef) message.elementAt(0);
						if (command.equals(msgEcho))
							mbox.send(erlangPartner,new OtpErlangTuple(new OtpErlangObject[]{ref,msgWorkerOk}));
						if (command.equals(msgTerminate))
						{
							//System.out.println("worker terminated");
							break;
						}
						else
							if (command.equals(msgUpdateConfiguration) && message.arity() == 3)
							{
								String outcome = ErlangRunner.updateConfiguration(learnerInitConfiguration.config, message.elementAt(2));
								if (outcome == null)
									mbox.send(erlangPartner,new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk}));
								else
									mbox.send(erlangPartner,new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(outcome)}));// report an error.
							}
							else
								if (command.equals(msgLine) && message.arity() == 3)
								{
									process(message.elementAt(2).toString());
								}
								else
									if (command.equals(msgTraces) && message.arity() == 3)
									{
										OtpErlangObject outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk});
										try
										{
											parseTraces(message.elementAt(2));
										}
										catch(Throwable ex)
										{
											outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
										}
										mbox.send(erlangPartner,outcome);
									}
									else
									if (command.equals(msgGetTraces) && message.arity() == 2)
									{
										mbox.send(erlangPartner,new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,new OtpErlangString(sPlus.toString()),new OtpErlangString(sMinus.toString())}));
									}
									else
									if (command.equals(msgLoadFSM) && message.arity() == 3)
									{
										OtpErlangObject outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk});
										try
										{
											learnerInitConfiguration.graph = new LearnerGraph(learnerInitConfiguration.config);
											parseStatemachine(message.elementAt(2), learnerInitConfiguration.graph, learnerInitConfiguration.getLabelConverter());
										}
										catch(Throwable ex)
										{
											System.out.println(ex.getMessage());ex.printStackTrace();
											outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
										}
										mbox.send(erlangPartner,outcome);
									}
									else
										if (command.equals(msgGetFSM) && message.arity() == 2)
										{
											OtpErlangObject outcome = null;
											try
											{
												outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,constructFSM(learnerInitConfiguration.graph)});
											}
											catch(Throwable ex)
											{
												outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
											}
											mbox.send(erlangPartner,outcome);
										}
										else
									if (command.equals(msgLearn))
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
				//System.out.println("Node exited "+nodeExited);
			} // do nothing, assuming we've been asked to terminate
			finally
			{
				if (runner != null) { runner.close();runner = null; }
			}
		}
	}
	
}
