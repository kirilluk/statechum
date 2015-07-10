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
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicLong;

import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.DeterministicDirectedSparseGraph.VertID.VertKind;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Configuration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.StringLabel;
import statechum.analysis.learning.ErlangOracleLearner;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.Visualiser.LayoutOptions;
import statechum.analysis.learning.experiments.PaperUAS;
import statechum.analysis.learning.experiments.PairSelection.ASE2014.EDSM_MarkovLearner;
import statechum.analysis.learning.experiments.PairSelection.Cav2014.KTailsReferenceLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceDiff;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceLanguageBCR;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ReferenceLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.mutation.DiffExperiments;
import statechum.analysis.learning.linear.DifferenceVisualiser;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.apps.QSMTool;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNodeStatus;

import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

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
		if(args.length < 3) {
		    System.err.println("Usage: statechum.analysis.Erlang.Synapse <ourNode> <cookie> <mailBox> <partnerNode>");
		    System.exit(99);
		}
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
		msgGetTraces = new OtpErlangAtom("getTraces"),
		msgLoadFSM = new OtpErlangAtom("loadFSM"),
		msgTestMapParsing = new OtpErlangAtom("testMapParsing"),
		msgGetFSM = new OtpErlangAtom("getFSM"),
		msgTestLoadFSM = new OtpErlangAtom("testLoadFSM"),
		msgTestDiffParsing = new OtpErlangAtom("testDiffParsing"),
		msgSetReds = new OtpErlangAtom("setReds"),
	
		msgComputeDiff =  new OtpErlangAtom("computeDiff"), 
		msgDisplayDiff = new OtpErlangAtom("displayDiff"), 
		msgDisplayFSM = new OtpErlangAtom("displayFSM"),
		msgLearnErlang = new OtpErlangAtom("learnErlang"),
		msgAddTypeInformation = new OtpErlangAtom("addTypeInformation"),
		msgExtractTypeInformation = new OtpErlangAtom("extractTypeInformation"),
		msgPurgeModuleInformation = new OtpErlangAtom("purgeModuleInformation"),
		msgLearnSicco = new OtpErlangAtom("learnSicco"),
		msgCompareWithOthers = new OtpErlangAtom("compareWithOthers"),// Args: referenceGraph, LearntGraph
		msgLearnEDSM = new OtpErlangAtom("learnEDSM"),
		msgLearnEDSMMARKOV = new OtpErlangAtom("learn"),
		msgLearnKTails = new OtpErlangAtom("learnKTails"),
		msgTraces = new OtpErlangAtom("traces"),

		msgStop = new OtpErlangAtom("stop"), // sent in order to make a learner terminate its learning process. workers respond with {Ref,workerok} to it.
		msgStatus = new OtpErlangAtom("status"), // sent a status message
		msgNotification = new OtpErlangAtom("step"), // sent as an indication of a progress
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
				{
					System.out.println("terminating upon request");
					break;
				}
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
		
		/** Overrides to function descriptions. */
		protected final Map<String,OtpErlangTuple> overrides = new TreeMap<String,OtpErlangTuple>();
		
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

		public static Map<VertID,VertID> parseMap(OtpErlangObject obj)
		{
			Map<VertID,VertID> outcome = new TreeMap<VertID,VertID>();
			updateMap(obj,outcome);
			return outcome;
		}
		
		public static void updateMap(OtpErlangObject obj, Map<VertID,VertID> outcome)
		{
			OtpErlangList map = (OtpErlangList)obj;
			for(OtpErlangObject entry:map)
			{
				OtpErlangTuple entryTuple = (OtpErlangTuple)entry;
				if (entryTuple.arity() != 2)
					throw new IllegalArgumentException("invalid tuple "+entryTuple);
				
				String stateA = ((OtpErlangAtom)entryTuple.elementAt(0)).atomValue();if (stateA.isEmpty()) throw new IllegalArgumentException("empty first state name");
				String stateB = ((OtpErlangAtom)entryTuple.elementAt(1)).atomValue();if (stateB.isEmpty()) throw new IllegalArgumentException("empty second state name");
				outcome.put(VertexID.parseID( stateA ),VertexID.parseID( stateB ) );
			}
		}
		
		public static OtpErlangList mapToObject(Map<VertID,VertID> map)
		{
			List<OtpErlangObject> outcome = new LinkedList<OtpErlangObject>();
			for(Entry<VertID,VertID> entry:map.entrySet())
			{
				outcome.add(new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom(entry.getKey().getStringId()),new OtpErlangAtom(entry.getValue().getStringId())}));
			}
			return new OtpErlangList(outcome.toArray(new OtpErlangObject[0]));
		}

		/** Given a representation of FSM in a form of Erlang tuple, builds the corresponding graph.
		 * 
		 * @param obj FSM to parse
		 * @param gr graph to fill in (whatever was in there will be replaced by the data loaded from the graph, this is necessary since we do not know the specific type to instantiate here).
		 * @param converter label converter
		 * @param checkStates if true, will check that all source and target states have been defined earlier. If false, will add them if needed.
		 */
		public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void parseStatemachine(OtpErlangObject obj,AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> gr, ConvertALabel converter, boolean checkStates)
		{
//		1	  states :: list(state()),
//		2	  transitions :: list(transition()),
//		3	  initial_state :: state(),
//		4	  alphabet :: list(event())
			OtpErlangTuple machine = (OtpErlangTuple)obj;
			if (machine.arity() != 5)
				throw new IllegalArgumentException("expected 5 components in FSM");
			if (!((OtpErlangAtom)machine.elementAt(0)).atomValue().equals("statemachine"))
				throw new IllegalArgumentException("first element of a record should be \"statemachine\"");
			OtpErlangList states = (OtpErlangList)machine.elementAt(1),transitions = (OtpErlangList)machine.elementAt(2),alphabet = (OtpErlangList)machine.elementAt(4);
			OtpErlangAtom initial_state = (OtpErlangAtom)machine.elementAt(3);
			
			parseStatemachine(states,transitions,alphabet,initial_state,gr,converter,checkStates);
		}
		
		/** Given a list of states in the supplied object, assigns red colour to the matching states in the supplied graph.
		 *  
		 * @param obj list of red states
		 * @param gr graph to modify
		 */
		public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void setReds(OtpErlangObject obj,AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> gr)
		{
			for(OtpErlangObject st:(OtpErlangList)obj)
			{
				String state = ((OtpErlangAtom)st).atomValue();if (state.isEmpty()) throw new IllegalArgumentException("empty state name");
				CmpVertex v=gr.findVertex(VertexID.parseID(state));
				if (v == null) throw new IllegalArgumentException(state+" state not found");
				v.setColour(JUConstants.RED);
			}
		}
		
		public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void parseStatemachine(OtpErlangList states,OtpErlangList transitions, OtpErlangList alphabet, OtpErlangAtom initial_state,
				AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> gr, ConvertALabel converter, boolean checkStates)
		{
			if (states.arity() == 0 && checkStates)
				throw new IllegalArgumentException("empty automaton");
			
			gr.initEmpty();
			
			for(OtpErlangObject st:states)
			{
				String state = ((OtpErlangAtom)st).atomValue();if (state.isEmpty()) throw new IllegalArgumentException("empty state name");
				CmpVertex vertex = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID( state ), gr.config);
				vertex.setAccept(vertex.getKind() != VertKind.NEGATIVE);
				gr.transitionMatrix.put(vertex,gr.createNewRow());
			}
			
			if (states.arity() != gr.transitionMatrix.size())
				throw new IllegalArgumentException("repeated states in the list of states");
			
			Map<OtpErlangObject,Label> objectToLabel = new HashMap<OtpErlangObject,Label>();
			for(OtpErlangObject l:alphabet)
			{
				String label = ((OtpErlangAtom)l).atomValue();if (label.isEmpty()) throw new IllegalArgumentException("empty label");
				objectToLabel.put(l,AbstractLearnerGraph.generateNewLabel( label,gr.config,converter));
			}
			for(OtpErlangObject transitionObj:transitions)
			{
				OtpErlangTuple transition = (OtpErlangTuple) transitionObj;
				if (transition.arity() != 3)
					throw new IllegalArgumentException("expected 3 components in transition "+transition);
				OtpErlangAtom from = (OtpErlangAtom)transition.elementAt(0),label = (OtpErlangAtom)transition.elementAt(1),to = (OtpErlangAtom)transition.elementAt(2);
				CmpVertex fromState = gr.findVertex(VertexID.parseID(from.atomValue())), toState = gr.findVertex(VertexID.parseID(to.atomValue()));
				if (fromState == null)
					if (!checkStates)
					{
						String state = from.atomValue();if (state.isEmpty()) throw new IllegalArgumentException("empty source state");
						fromState = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID( state), gr.config );
						gr.transitionMatrix.put(fromState,gr.createNewRow());
					}
					else
							throw new IllegalArgumentException("invalid source state "+from.atomValue());
				
				if (toState == null)
					if (!checkStates)
					{
						String state = to.atomValue();if (state.isEmpty()) throw new IllegalArgumentException("empty target state");
						toState = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID( state), gr.config );
						gr.transitionMatrix.put(toState,gr.createNewRow());
					}
					else
						throw new IllegalArgumentException("invalid target state"+to.atomValue());
				
				if (!objectToLabel.containsKey(label))
				{
					if (!checkStates)
					{
						String l = label.atomValue();if (l.isEmpty()) throw new IllegalArgumentException("empty label");
						objectToLabel.put(label,AbstractLearnerGraph.generateNewLabel( l,gr.config,converter));
					}
					else
						throw new IllegalArgumentException("unknown label "+label);
				}
				gr.addTransition(gr.transitionMatrix.get(fromState),objectToLabel.get(label),toState);
			}
			
			if (initial_state != null)
			{// do not set the initial state if we are not asked to
				gr.setInit(gr.findVertex(VertexID.parseID(initial_state.atomValue())));
				if (gr.getInit() == null)
				{
					if (!checkStates)
					{
						String state = initial_state.atomValue();if (state.isEmpty()) throw new IllegalArgumentException("empty initial state");
						CmpVertex initState = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID( state), gr.config );gr.transitionMatrix.put(initState,gr.createNewRow());
						gr.setInit(initState);
					}
					else
						throw new IllegalArgumentException("missing initial state");
				}
			}
			gr.setIDNumbers();gr.invalidateCache();
		}
		
		/** Given an Erlang tuple, extract names of states to be ignored and adds them to the options. If any cast goes wrong, throws a cast exception.
		 * 
		 * @param options layout options where to add names
		 * @param stateNamesAsObject where to get the names from 
		 */
		public static void setStateNamesToBeIgnored(LayoutOptions options, OtpErlangObject stateNamesAsObject)
		{
			
			OtpErlangList statesToBeIgnored = (OtpErlangList)stateNamesAsObject;
			if (statesToBeIgnored.arity() > 0 && options != null)
			{
				if (options.ignoredStates == null)
					options.ignoredStates = new TreeSet<String>();
				for(OtpErlangObject obj:statesToBeIgnored)
					options.ignoredStates.add( ((OtpErlangAtom)obj).atomValue() );
			}
			
		}
		
		/** Turns the supplied graph into an Erlang tuple. 
		 * @param gr graph to convert. 
		 */
		public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> OtpErlangTuple constructFSM(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> gr)
		{
			List<OtpErlangObject> statesList = new LinkedList<OtpErlangObject>(), transitions = new LinkedList<OtpErlangObject>();
			Map<String,OtpErlangObject> alphabet = new TreeMap<String,OtpErlangObject>();
			
			for(Entry<CmpVertex,Map<Label,TARGET_TYPE>> entry:gr.transitionMatrix.entrySet()) 
			{
				statesList.add(new OtpErlangAtom(entry.getKey().getStringId()));
				for(Entry<Label,TARGET_TYPE> transition:entry.getValue().entrySet())
				{
					String lblStr = transition.getKey().toErlangTerm();OtpErlangAtom lblAtom = new OtpErlangAtom(lblStr);
					alphabet.put(lblStr,lblAtom);
					for(CmpVertex target:gr.getTargets(transition.getValue()))
						transitions.add(new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom(entry.getKey().getStringId()), lblAtom, new OtpErlangAtom(target.getStringId())}));
				}
			}
			return new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom("statemachine"),new OtpErlangList(statesList.toArray(new OtpErlangObject[0])),
					new OtpErlangList(transitions.toArray(new OtpErlangObject[0])),
					new OtpErlangAtom(gr.getInit().getStringId()),new OtpErlangList(alphabet.values().toArray(new OtpErlangObject[alphabet.size()])),
			});
		}

		public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void convertLabelsToStrings(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> grFrom, AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> grTo)
		{
    		AbstractLearnerGraph.interpretLabelsOnGraph(grFrom,grTo,new Transform.ConvertLabel(new ConvertALabel() {
				
				@Override
				public Label convertLabelToLabel(Label label) {
					ErlangLabel lbl = (ErlangLabel)label;
					return new StringLabel(lbl.function.getName()+"/"+lbl.function.getArity()+","+lbl.input+","+lbl.expectedOutput);
				}
			}));
		}
		
		public static class AskedToTerminateException extends RuntimeException
		{

			/**
			 * ID for serialization
			 */
			private static final long serialVersionUID = -3164183727619518185L;
		}
		
		protected void sendProgress(OtpErlangPid pid, OtpErlangRef ref, LearnerGraph graph, ErlangModule mod, AtomicLong counter)
		{
			OtpErlangObject progressDetails = null, stateNumber = new com.ericsson.otp.erlang.OtpErlangLong(graph.getStateNumber());
			if (learnerInitConfiguration.config.getSynapseSendFSMFrequency() <= 0 || 0 != (counter.incrementAndGet() % learnerInitConfiguration.config.getSynapseSendFSMFrequency()))
				progressDetails = new OtpErlangTuple(new OtpErlangObject[]{ stateNumber });
			else
			{// we need to report red states in order to be able to continue QSM-learning the graph
				List<OtpErlangObject> stateList = new LinkedList<OtpErlangObject>();
				for(CmpVertex v:graph.transitionMatrix.keySet())
					if (v.getColour() == JUConstants.RED)
						stateList.add(new OtpErlangAtom(v.getStringId()));
				
				if (mod != null)
				{
					LearnerGraph graphWithTrimmedLabels = new LearnerGraph(learnerInitConfiguration.config);
					AbstractLearnerGraph.interpretLabelsOnGraph(graph,graphWithTrimmedLabels,mod.behaviour.new ConverterModToErl());
					progressDetails = new OtpErlangTuple(new OtpErlangObject[]{ stateNumber, constructFSM(graphWithTrimmedLabels), new OtpErlangList(stateList.toArray(new OtpErlangObject[0])) });
				}
				else
					progressDetails = new OtpErlangTuple(new OtpErlangObject[]{ stateNumber, constructFSM(graph), new OtpErlangList(stateList.toArray(new OtpErlangObject[0])) });
			}
			mbox.send(pid,new OtpErlangTuple(new OtpErlangObject[]{ref,msgStatus,msgNotification, progressDetails}));
		}
		
		/** Goes through the options provided in a message, starting with the specific index and updates the graph to be visualised to respect those options. Returns the window number in which to pop the graph, negative for a test mode. 
		 * 
		 * @param message OTP tuple with options
		 * @param startFrom the starting position in the tuple to go through
		 * @param fsmPicture graph to update
		 * @return window number to use or negative for a test mode.
		 * @throws OtpErlangRangeException 
		 */
		public static int setOptions(final OtpErlangTuple message, int startFrom, DirectedSparseGraph fsmPicture) throws OtpErlangRangeException
		{
			int windowNumber = 0;
			OtpErlangObject name = message.elementAt(startFrom);
			if (name instanceof OtpErlangList && ((OtpErlangList)name).arity() == 0)
				windowNumber =-1;
			else
			{
				fsmPicture.setUserDatum(JUConstants.TITLE, ((OtpErlangAtom)name).atomValue(), UserData.SHARED);
			
				if (message.arity() > startFrom+1)
				{
					OtpErlangObject stateNamesAsObject = message.elementAt(startFrom+1);
					setStateNamesToBeIgnored(((LayoutOptions)fsmPicture.getUserDatum(JUConstants.LAYOUTOPTIONS)),stateNamesAsObject);
				}
				if (message.arity() > startFrom+2)
				{
					OtpErlangLong windowNum = ((OtpErlangLong)message.elementAt(startFrom+2));windowNumber = windowNum.intValue();
				}
				if (message.arity() > startFrom+3)
				{
					OtpErlangLong abstraction = ((OtpErlangLong)message.elementAt(startFrom+3));
					((LayoutOptions)fsmPicture.getUserDatum(JUConstants.LAYOUTOPTIONS)).componentsToPick=abstraction.intValue();
				}
			}
			return windowNumber;
		}
		
		/** Given a map of object names to their types, serialises it into an Erlang list.
		 *  
		 * @param map what to serialise
		 * @return Erlang list
		 */
		public static OtpErlangList typeMapToList(Map<String,OtpErlangTuple> map)
		{
			OtpErlangTuple mapping [] = new OtpErlangTuple[map.size()];
			int i=0;
			for(Entry<String,OtpErlangTuple> entry:map.entrySet())
				mapping[i++]=new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom(entry.getKey()),entry.getValue()});
			return new OtpErlangList(mapping);
		}
		
		/** Deserialises a map of functions to their types and updates a provided map with their values.
		 * 
		 * @param list what to deserialise
		 * @param map what to update.
		 */
		public static void updateFrom(OtpErlangList list,Map<String,OtpErlangTuple> map)
		{
			for(OtpErlangObject obj:list.elements())
			{
				if (!(obj instanceof OtpErlangTuple))
					throw new IllegalArgumentException("element of a list should be a tuple");
				OtpErlangTuple t=(OtpErlangTuple)obj;
				if (t.arity() != 2)
					throw new IllegalArgumentException("tuple should contain exactly two elements");
				
				if (!(t.elementAt(0) instanceof OtpErlangAtom))
					throw new IllegalArgumentException("type name should be an atom");
				if (!(t.elementAt(1) instanceof OtpErlangTuple))
					throw new IllegalArgumentException("type value should be a tuple");
				map.put( ((OtpErlangAtom)t.elementAt(0)).atomValue(), (OtpErlangTuple)t.elementAt(1));
			}
		}

		public OtpErlangObject computeBCR_and_structuralDifference(LearnerGraph referenceGraph, LearnerGraph learntGraph, Configuration config)
		{
			final int chunkLen = 3;
			
			ConvertALabel converter = new Transform.InternStringLabel();
			LearnerEvaluationConfiguration learnerEval = new LearnerEvaluationConfiguration(config);learnerEval.setLabelConverter(converter);
			int states = referenceGraph.getAcceptStateNumber();
			final Collection<List<Label>> testSet = PaperUAS.computeEvaluationSet(referenceGraph,states*3,PairQualityLearner.makeEven(states*referenceGraph.pathroutines.computeAlphabet().size()));
			
			DifferenceToReferenceDiff differenceStructural=DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, learntGraph, config, 1);
			DifferenceToReferenceLanguageBCR differenceBCRlearnt=DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, learntGraph,testSet);
		
			final MarkovModel m= new MarkovModel(chunkLen,true,true,false);
			LearnerGraph pta=new LearnerGraph(config);
			for(List<Label> seq:sPlus)
				pta.paths.augmentPTA(seq,true,false,null);
			for(List<Label> seq:sMinus)
				pta.paths.augmentPTA(seq,false,false,null);
			pta.clearColours();
			new MarkovClassifier(m, pta).updateMarkov(false);// construct Markov chain
			// For Markov, we do not need to learn anything at all - our Markov matrix contains enough information to classify paths and hence compare it to the reference graph.
			ConfusionMatrix mat = DiffExperiments.classifyAgainstMarkov(testSet, referenceGraph, m);
			DifferenceToReferenceLanguageBCR differenceBCRMarkov = new DifferenceToReferenceLanguageBCR(mat);
			
			return new OtpErlangTuple(new OtpErlangObject[]{
					new OtpErlangDouble(differenceStructural.getValue()),new OtpErlangDouble(differenceBCRlearnt.getValue()),new OtpErlangDouble(differenceBCRMarkov.getValue())
			});
		}
		
		protected void reportLearningProgress(LearnerGraph graph,final OtpErlangTuple message, final OtpErlangRef ref, final AtomicLong counter)
		{
			// send the notification if necessary
			if (message.arity() > 2 && message.elementAt(2) instanceof OtpErlangPid)
				sendProgress((OtpErlangPid)message.elementAt(2), ref, graph, null, counter);
			
			// check if we were asked to terminate
			try {
				OtpErlangObject messageReceived = mbox.receive(0);// do not wait, return null if anything received
				if (messageReceived != null && messageReceived instanceof OtpErlangTuple && ((OtpErlangTuple)messageReceived).arity() == 2)
				{
					OtpErlangTuple cmd = ((OtpErlangTuple)messageReceived);
					if (cmd.elementAt(0).equals(ref) && cmd.elementAt(1).equals(msgStop))
						throw new AskedToTerminateException();
				}
			} catch (OtpErlangExit e) {
				Helper.throwUnchecked("node exited", e);
			} catch (OtpErlangDecodeException e) {
				Helper.throwUnchecked("decode exception", e);
			}			
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
				
				learnerInitConfiguration.config.setErlangStripModuleNamesFromFunctionsInNonGenModules(true);
				learnerInitConfiguration.config.setGdFailOnDuplicateNames(false);
				learnerInitConfiguration.config.setErlangInitialTraceLength(3);
				for(;;)
				{
						OtpErlangObject msg=mbox.receive();
						if (!(msg instanceof OtpErlangTuple))
							System.out.println("invalid message received by worker, expected tuple, got "+msg);
						final OtpErlangTuple message = (OtpErlangTuple)msg;
						if (message.arity() < 2)
							System.out.println("invalid tuple received by worker, expected at least two elements, got "+msg);
						
						if (!(message.elementAt(1) instanceof OtpErlangAtom))
							System.out.println("invalid request received by worker, expected an atom, got "+message.elementAt(1));
						OtpErlangAtom command = (OtpErlangAtom) message.elementAt(1);
						if (!(message.elementAt(0) instanceof OtpErlangRef))
							System.out.println("invalid request received by worker, expected a ref, got "+message.elementAt(0));
						final OtpErlangRef ref = (OtpErlangRef) message.elementAt(0);

						if (command.equals(msgEcho))
							mbox.send(erlangPartner,new OtpErlangTuple(new OtpErlangObject[]{ref,msgWorkerOk}));
						if (command.equals(msgStop))
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
												parseStatemachine(message.elementAt(2), learnerInitConfiguration.graph, learnerInitConfiguration.getLabelConverter(),true);
											}
											catch(Throwable ex)
											{
												System.out.println(ex.getMessage());ex.printStackTrace();
												outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
											}
											mbox.send(erlangPartner,outcome);
										}
										else
											if (command.equals(msgTestLoadFSM) && message.arity() == 3)
											{
												OtpErlangObject outcome = null;
												try
												{
													learnerInitConfiguration.graph = new LearnerGraph(learnerInitConfiguration.config);
													parseStatemachine(message.elementAt(2), learnerInitConfiguration.graph, learnerInitConfiguration.getLabelConverter(),false);
													outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,constructFSM(learnerInitConfiguration.graph)});
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
												if (command.equals(msgSetReds) && message.arity() == 3)
												{
													OtpErlangObject outcome = null;
													try
													{
														setReds(message.elementAt(2), learnerInitConfiguration.graph);
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk});
													}
													catch(Throwable ex)
													{
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
													}
													mbox.send(erlangPartner,outcome);
												}
												else
											if (command.equals(msgTestMapParsing) && message.arity() == 3)
											{
												OtpErlangObject outcome = null;
												try
												{
													outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,mapToObject(parseMap(message.elementAt(2)))});
												}
												catch(Throwable ex)
												{
													outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
												}
												mbox.send(erlangPartner,outcome);
											}
											else
												// Arguments: ref,computeDiff,fsmA,fsmB
												// Response: ref,ok,diff
												// on error, ref,failure,text_of_the_error (as string)
												if (command.equals(msgComputeDiff) && message.arity() == 4)
												{
													OtpErlangObject outcome = null;
													try
													{
														LearnerGraphND grA = new LearnerGraphND(learnerInitConfiguration.config), grB = new LearnerGraphND(learnerInitConfiguration.config);
														Synapse.StatechumProcess.parseStatemachine(message.elementAt(2),grA,null,true);
														Synapse.StatechumProcess.parseStatemachine(message.elementAt(3),grB,null,true);
														OtpErlangObject difference  = DifferenceVisualiser.ChangesToGraph.computeGD(grA, grB, learnerInitConfiguration.config);
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,difference});
													}
													catch(Throwable ex)
													{
														ex.printStackTrace();
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
													}
													mbox.send(erlangPartner,outcome);
												}
												else
												// Args: Ref,learn, pid
												// pid is optional, where provided, progress messages are reported in a form of {Ref,'status',step}
												// in the course of learning, the learner is receptive to messages directed at its normal PID, a {Ref,terminate} command will kill it and the response will be {Ref,terminate}.
												// Response: Ref,ok,fsm
												// on error: Ref,failure,text_of_the_error (as string)
												if (command.equals(msgLearnEDSM) && message.arity() >= 2)
												{
													OtpErlangObject outcome = null;
													try
													{
														final AtomicLong counter = new AtomicLong();
														RPNIUniversalLearner learner = new RPNIUniversalLearner(null, learnerInitConfiguration) {

															@Override
															public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) 
															{
																reportLearningProgress(graph,message,ref,counter);
																// resume learning.
																return super.ChooseStatePairs(graph);
															}
														};
														learner.init(sPlus, sMinus);
														if (learnerInitConfiguration.graph != null)
														{
															learnerInitConfiguration.graph.clearColours();learnerInitConfiguration.graph.getInit().setColour(JUConstants.RED);
															LearnerGraph.copyGraphs(learnerInitConfiguration.graph,learner.getTentativeAutomaton());
														}
														LearnerGraph graphLearnt = learner.learnMachine();
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,  constructFSM(graphLearnt)});
													}
													catch(AskedToTerminateException e)
													{
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgTerminate});
													}
													catch(Throwable ex)
													{
														ex.printStackTrace();
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
													}
													mbox.send(erlangPartner,outcome);
													
												}
												else
												// Args: Ref,learn, pid
												// pid is optional, where provided, progress messages are reported in a form of {Ref,'status',step}
												// in the course of learning, the learner is receptive to messages directed at its normal PID, a {Ref,terminate} command will kill it and the response will be {Ref,terminate}.
												// Response: Ref,ok,fsm
												// on error: Ref,failure,text_of_the_error (as string)
												if (command.equals(msgLearnEDSMMARKOV) && message.arity() >= 2)
												{
													OtpErlangObject outcome = null;
													try
													{
														final AtomicLong counter = new AtomicLong();
														learnerInitConfiguration.config.setLearnerScoreMode(ScoreMode.ONLYOVERRIDE);
														LearnerGraph pta=new LearnerGraph(learnerInitConfiguration.config);
														for(List<Label> seq:sPlus)
															pta.paths.augmentPTA(seq,true,false,null);
														for(List<Label> seq:sMinus)
															pta.paths.augmentPTA(seq,false,false,null);
														final MarkovModel m= new MarkovModel(3,true,true,false);

														new MarkovClassifier(m, pta).updateMarkov(false);// construct Markov chain if asked for.
														final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();
													
														pta.clearColours();
														EDSM_MarkovLearner learner = new EDSM_MarkovLearner(learnerInitConfiguration,pta,0) {

															@Override
															public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) 
															{
																reportLearningProgress(graph,message,ref,counter);
																// resume learning.
																return super.ChooseStatePairs(graph);
															}
														};
														learner.setMarkov(m);learner.setChecker(checker);
														learner.setUseNewScoreNearRoot(false);learner.setUseClassifyPairs(false);
														learner.setDisableInconsistenciesInMergers(false);
														
														if (learnerInitConfiguration.graph != null)
														{
															learnerInitConfiguration.graph.clearColours();learnerInitConfiguration.graph.getInit().setColour(JUConstants.RED);
															LearnerGraph.copyGraphs(learnerInitConfiguration.graph,learner.getTentativeAutomaton());
														}
														LearnerGraph graphLearnt = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,  constructFSM(graphLearnt)});
													}
													catch(AskedToTerminateException e)
													{
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgTerminate});
													}
													catch(Throwable ex)
													{
														ex.printStackTrace();
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
													}
													mbox.send(erlangPartner,outcome);
													
												}
												else
												// Args: Ref,
												// Arguments: ref,compareWithOthers,fsmA,fsmB
												// Response: ref,ok,results of comparison
												// on error, ref,failure,text_of_the_error (as string)
												if (command.equals(msgCompareWithOthers) && message.arity() == 4)
												{
													OtpErlangObject outcome = null;
													try
													{
														LearnerGraph grA = new LearnerGraph(learnerInitConfiguration.config), grB = new LearnerGraph(learnerInitConfiguration.config);
														Synapse.StatechumProcess.parseStatemachine(message.elementAt(2),grA,null,true);
														Synapse.StatechumProcess.parseStatemachine(message.elementAt(3),grB,null,true);
														OtpErlangObject difference  = computeBCR_and_structuralDifference(grA, grB, learnerInitConfiguration.config);
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,difference});
													}
													catch(Throwable ex)
													{
														ex.printStackTrace();
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
													}
													mbox.send(erlangPartner,outcome);
												}
												else
												// Args: Ref,learn, pid
												// pid is optional, where provided, progress messages are reported in a form of {Ref,'status',step}
												// in the course of learning, the learner is receptive to messages directed at its normal PID, a {Ref,terminate} command will kill it and the response will be {Ref,terminate}.
												// Response: Ref,ok,fsm
												// on error: Ref,failure,text_of_the_error (as string)
												if (command.equals(msgLearnSicco) && message.arity() >= 2)
												{
													OtpErlangObject outcome = null;
													try
													{
														final AtomicLong counter = new AtomicLong();
														learnerInitConfiguration.config.setLearnerScoreMode(ScoreMode.ONLYOVERRIDE);
														LearnerGraph pta=new LearnerGraph(learnerInitConfiguration.config);
														for(List<Label> seq:sPlus)
															pta.paths.augmentPTA(seq,true,false,null);
														for(List<Label> seq:sMinus)
															pta.paths.augmentPTA(seq,false,false,null);
														pta.clearColours();
														ReferenceLearner learner = new ReferenceLearner(learnerInitConfiguration,null,pta,false) {

															@Override
															public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) 
															{
																reportLearningProgress(graph,message,ref,counter);
																// resume learning.
																return super.ChooseStatePairs(graph);
															}

															@Override
															public Pair<Integer, String> CheckWithEndUser(
																	LearnerGraph model,
																	List<Label> question,
																	int expectedForNoRestart,
																	List<Boolean> consistentFacts,
																	PairScore pairBeingMerged,
																	Object[] moreOptions) {

																return super.CheckWithEndUser(model, question, expectedForNoRestart,
																		consistentFacts, pairBeingMerged, moreOptions);
															}
															
														};

														if (learnerInitConfiguration.graph != null)
														{
															learnerInitConfiguration.graph.clearColours();learnerInitConfiguration.graph.getInit().setColour(JUConstants.RED);
															LearnerGraph.copyGraphs(learnerInitConfiguration.graph,learner.getTentativeAutomaton());
														}
														LearnerGraph graphLearnt = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,  constructFSM(graphLearnt)});
													}
													catch(AskedToTerminateException e)
													{
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgTerminate});
													}
													catch(Throwable ex)
													{
														ex.printStackTrace();
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
													}
													mbox.send(erlangPartner,outcome);
													
												}
												else
												// Args: Ref,learn, pid
												// pid is optional, where provided, progress messages are reported in a form of {Ref,'status',step}
												// in the course of learning, the learner is receptive to messages directed at its normal PID, a {Ref,terminate} command will kill it and the response will be {Ref,terminate}.
												// Response: Ref,ok,fsm
												// on error: Ref,failure,text_of_the_error (as string)
												if (command.equals(msgLearnKTails) && message.arity() >= 2)
												{
													OtpErlangObject outcome = null;
													try
													{
														final AtomicLong counter = new AtomicLong();
														learnerInitConfiguration.config.setLearnerScoreMode(ScoreMode.ONLYOVERRIDE);
														LearnerGraph pta=new LearnerGraph(learnerInitConfiguration.config);
														for(List<Label> seq:sPlus)
															pta.paths.augmentPTA(seq,true,false,null);
														for(List<Label> seq:sMinus)
															pta.paths.augmentPTA(seq,false,false,null);
														pta.clearColours();
														KTailsReferenceLearner learner = new KTailsReferenceLearner(learnerInitConfiguration,pta,true,1) {

															@Override
															public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) 
															{
																reportLearningProgress(graph,message,ref,counter);
																// resume learning.
																return super.ChooseStatePairs(graph);
															}

														};

														if (learnerInitConfiguration.graph != null)
														{
															learnerInitConfiguration.graph.clearColours();learnerInitConfiguration.graph.getInit().setColour(JUConstants.RED);
															LearnerGraph.copyGraphs(learnerInitConfiguration.graph,learner.getTentativeAutomaton());
														}
														LearnerGraph graphLearnt = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,  constructFSM(graphLearnt)});
													}
													catch(AskedToTerminateException e)
													{
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgTerminate});
													}
													catch(Throwable ex)
													{
														ex.printStackTrace();
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
													}
													mbox.send(erlangPartner,outcome);
													
												}
												else
												// Args: Ref,addTypeInformation,list of pairs containing method names and types.
												if (command.equals(msgAddTypeInformation) && message.arity() >= 3)
												{
													OtpErlangObject outcome = null;
													try
													{
														updateFrom((OtpErlangList)message.elementAt(2), overrides);
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,  typeMapToList(overrides)});
													}
													catch(Throwable ex)
													{
														ex.printStackTrace();
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
													}
													mbox.send(erlangPartner,outcome);
												}
												else
												// Args: Ref,purgeModuleInformation
												// Since using addTypeInformation followed by learnErlang causes changes to the alphabet modules we are dealing with, independence of tests requires the collection of loaded modules to be purged. This is the purpose of this function.
												if (command.equals(msgPurgeModuleInformation) && message.arity() >= 2)
												{
													OtpErlangObject outcome = null;
													try
													{
														ErlangModule.flushRegistry();
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk});
													}
													catch(Throwable ex)
													{
														ex.printStackTrace();
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
													}
													mbox.send(erlangPartner,outcome);
												}
												else
												// Args: Ref,extractTypeInformation,moduleFileFullPath.
												// Returns a list of pairs of method names and types.
												if (command.equals(msgExtractTypeInformation) && message.arity() >= 3)
												{
													OtpErlangObject outcome = null;
													try
													{
														ErlangModule.setupErlangConfiguration(learnerInitConfiguration.config, new File(((OtpErlangAtom)message.elementAt(2)).atomValue()));
														
														// we start a separate Erlang node to run the questions
														ErlangRunner erlangRunner = ErlangRuntime.getDefaultRuntime().createNewRunner();
														learnerInitConfiguration.config.setErlangMboxName(erlangRunner.getRunnerName());
														final ErlangModule mod = ErlangModule.loadModule(learnerInitConfiguration.config);
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,  typeMapToList(mod.sigTypes)});
													}
													catch(Throwable ex)
													{
														ex.printStackTrace();
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
													}
													mbox.send(erlangPartner,outcome);
												}
												else
												// Args: Ref,learnErlang,moduleFileFullPath, pid
												// pid is optional, where provided, progress messages are reported in a form of {Ref,'status',step}
												// in the course of learning, the learner is receptive to messages directed at its normal PID, a {Ref,terminate} command will kill it and the response will be {Ref,terminate}.
												// Response: Ref,ok,fsm
												// on error: Ref,failure,text_of_the_error (as string)
												if (command.equals(msgLearnErlang) && message.arity() >= 3)
												{
													OtpErlangObject outcome = null;
													try
													{
														ErlangModule.setupErlangConfiguration(learnerInitConfiguration.config, new File(((OtpErlangAtom)message.elementAt(2)).atomValue()));
														
														// we start a separate Erlang node to run the questions
														ErlangRunner erlangRunner = ErlangRuntime.getDefaultRuntime().createNewRunner();
														learnerInitConfiguration.config.setErlangMboxName(erlangRunner.getRunnerName());
														final AtomicLong counter = new AtomicLong();
														final ErlangModule mod = ErlangModule.loadModule(learnerInitConfiguration.config);
														mod.rebuildSigs(learnerInitConfiguration.config, overrides);mod.behaviour.generateAlphabet(learnerInitConfiguration.config);
														
														ErlangOracleLearner learner = new ErlangOracleLearner(null, learnerInitConfiguration) {

															@Override
															public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) 
															{
																// check if we were asked to terminate
																try {
																	if (message.arity() > 3 && message.elementAt(3) instanceof OtpErlangPid)
																		// send the notification if necessary
																		sendProgress((OtpErlangPid)message.elementAt(3), ref, graph, mod, counter);

																	OtpErlangObject messageReceived = mbox.receive(0);// do not wait, return null if anything received
																	if (messageReceived != null && messageReceived instanceof OtpErlangTuple && ((OtpErlangTuple)messageReceived).arity() == 2)
																	{
																		OtpErlangTuple cmd = ((OtpErlangTuple)messageReceived);
																		if (cmd.elementAt(0).equals(ref) && cmd.elementAt(1).equals(msgStop))
																			throw new AskedToTerminateException();
																	}
																} catch (OtpErlangExit e) {
																	Helper.throwUnchecked("node exited", e);
																} catch (OtpErlangDecodeException e) {
																	Helper.throwUnchecked("decode exception", e);
																} 
																
																// resume learning.
																return super.ChooseStatePairs(graph);
															}

															@Override
															public Pair<Integer, String> CheckWithEndUser(
																	LearnerGraph model,
																	List<Label> question,
																	int expectedForNoRestart,
																	List<Boolean> consistentFacts,
																	PairScore pairBeingMerged,
																	Object[] moreOptions) {
																return super.CheckWithEndUser(model, question, expectedForNoRestart,consistentFacts, pairBeingMerged, moreOptions);
															}
															
														};
														if (learnerInitConfiguration.config.getAskQuestions()) // only generate initial traces if we are permited to ask questions.
															learner.init(learner.GenerateInitialTraces(learnerInitConfiguration.config.getErlangInitialTraceLength()),0,0);
														System.out.println("random trace generation complete");
														LearnerGraph graphLearnt = learner.learnMachine(),
																graphWithTrimmedLabels = new LearnerGraph(learnerInitConfiguration.config);
														
														if (learnerInitConfiguration.config.getErlangStripModuleNamesFromFunctionsInNonGenModules())
															convertLabelsToStrings(graphLearnt,graphWithTrimmedLabels);
														else
															AbstractLearnerGraph.interpretLabelsOnGraph(graphLearnt,graphWithTrimmedLabels,mod.behaviour.new ConverterModToErl());
														
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,  constructFSM(graphWithTrimmedLabels)});
														erlangRunner.close();
													}
													catch(AskedToTerminateException e)
													{
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgTerminate});
													}
													catch(Throwable ex)
													{
														ex.printStackTrace();
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
													}
													mbox.send(erlangPartner,outcome);
													
												}
												else
												if (command.equals(msgTestDiffParsing) && message.arity() == 4) // this one computes a graph reflecting the differences and returns the labelling part of it as a string. Inputs are one of the original machines and the differences.
												{
													OtpErlangObject outcome = null;
													try
													{
														DirectedSparseGraph diff = DifferenceVisualiser.ChangesToGraph.computeVisualisationParameters(message.elementAt(2), message.elementAt(3));
														
														Set<String> modifiedLines = new TreeSet<String>();
														for(Object obj:diff.getEdges()) // getEdges returns edges in a JDK-dependent order, we use TreeSet to sort them so that expected values can be determined without associating them with specific versions of JDK.
														{
															StringBuffer textOfTheOutcome = new StringBuffer();
															
															textOfTheOutcome.append(obj.toString());textOfTheOutcome.append(":");textOfTheOutcome.append( ((Edge)obj).getUserDatum(JUConstants.DIFF) );
															modifiedLines.add(textOfTheOutcome.toString());
														}
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk,new OtpErlangAtom(modifiedLines.toString())});
													}
													catch(Throwable ex)
													{
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
													}
													mbox.send(erlangPartner,outcome);
												}
												else
													
												// Arguments: Ref, 'displayDiff', first graph, diff, atom with the name of the difference and (optional) list of states (as atoms) to ignore. 
												// Upon error, no notifications are sent and instead an error is reported. 
												// Note: if the difference name is an empty sequence, no graph is displayed but notifications are provided (for testing).
												if (command.equals(msgDisplayDiff) && message.arity() >= 5) 
												{
													OtpErlangObject outcome = null;
													try
													{
														DirectedSparseGraph diff = DifferenceVisualiser.ChangesToGraph.computeVisualisationParameters(message.elementAt(2), message.elementAt(3));
														int windowNumber = setOptions(message,4,diff);
														if (windowNumber >= 0)
															Visualiser.updateFrameWithPos(diff,windowNumber);

														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk});
													}
													catch(Throwable ex)
													{
														outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
													}
													mbox.send(erlangPartner,outcome);
												}
												else
													// Arguments: Ref, 'displayFSM', graph, atom with the name of the difference and (optional) list of states (as atoms) to ignore. 
													// Upon error, no notifications are sent and instead an error is reported. 
													// Note: if the difference name is an empty sequence, no graph is displayed but notifications are provided (for testing).
													if (command.equals(msgDisplayFSM) && message.arity() >= 4) 
													{
														OtpErlangObject outcome = null;
														try
														{
															Configuration config = Configuration.getDefaultConfiguration().copy();
															LearnerGraphND machine = new LearnerGraphND(config);Synapse.StatechumProcess.parseStatemachine(message.elementAt(2),machine,null,true);
															DirectedSparseGraph fsmPicture = machine.pathroutines.getGraph();
															if (!fsmPicture.containsUserDatumKey(JUConstants.LAYOUTOPTIONS))
																fsmPicture.addUserDatum(JUConstants.LAYOUTOPTIONS,new LayoutOptions(), UserData.SHARED);
															int windowNumber = setOptions(message,3,fsmPicture);
															if (windowNumber >= 0)
																Visualiser.updateFrameWithPos(fsmPicture,windowNumber);

															outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgOk});
														}
														catch(Throwable ex)
														{
															ex.printStackTrace();
															outcome = new OtpErlangTuple(new OtpErlangObject[]{ref,msgFailure,new OtpErlangList(ex.getMessage())});
														}
														mbox.send(erlangPartner,outcome);
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
			catch(Throwable e)
			{
				e.printStackTrace();
			}
			finally
			{
				if (runner != null) { runner.close();runner = null; }
			}
		}
	}
	
}
