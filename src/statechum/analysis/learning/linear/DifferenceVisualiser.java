package statechum.analysis.learning.linear;

import java.awt.Color;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicEdge;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.Erlang.Synapse;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.linear.GD.ChangesRecorder;
import statechum.analysis.learning.linear.GD.LearnerGraphMutator;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.PathRoutines.EdgeAnnotation;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

public class DifferenceVisualiser {

	private DifferenceVisualiser() {
	}
	
	public static <TARGET_A_TYPE,TARGET_B_TYPE, CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>>
		DirectedSparseGraph computeVisualGD(AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> grA, AbstractLearnerGraph<TARGET_B_TYPE,CACHE_B_TYPE> grB, Configuration config)
	{
		GD<TARGET_A_TYPE,TARGET_B_TYPE,CACHE_A_TYPE,CACHE_B_TYPE> gd = new GD<TARGET_A_TYPE,TARGET_B_TYPE,CACHE_A_TYPE,CACHE_B_TYPE>();
	
		ChangesToGraph recorder = new ChangesToGraph();
		gd.computeGD(grA, grB, ExperimentRunner.getCpuNumber(),recorder,config);
			
		return recorder.getDifferenceGraph(grA.pathroutines.getGraph());
	}
	
//	0  statemachinedifference,{
//	1	  added_transitions :: list(transition()),
//	2	  deleted_transitions :: list(transition()),
//	3	  added_states :: list(state()),
//	4	  deleted_states :: list(state()),
//	5	  name_mapping_1 :: list({state(),state()}),
//	6	 initial :: atom

// 0  statemachine
//	1	  states :: list(state()),
//	2	  transitions :: list(transition()),
//	3	  initial_state :: state(),
//	4	  alphabet :: list(event())

	
	public static class ChangesToGraph extends ChangesRecorder
	{
		/** This one does not permit changes to be passed to subsequent listeners because we intend to load and save this one from Erlang data structures. */
		protected ChangesToGraph() {
			super(null);
		}
	
		
		
		/** Loads the differences from Erlang structure
		 * 
		 * @param obj where to load from
		 */
		public static ChangesToGraph load(OtpErlangObject obj)
		{
			ChangesToGraph recorder = new ChangesToGraph();
			OtpErlangTuple difference = (OtpErlangTuple)obj;
			if (difference.arity() != 7)
				throw new IllegalArgumentException("expected 7 components in diff");
			if (!((OtpErlangAtom)difference.elementAt(0)).atomValue().equals("statemachinedifference"))
				throw new IllegalArgumentException("first element of a record should be \"statemachinedifference\"");
			OtpErlangList addedTransitions = (OtpErlangList)difference.elementAt(1),removedTransitions = (OtpErlangList)difference.elementAt(2),addedStates = (OtpErlangList)difference.elementAt(3);
			OtpErlangAtom initial_state = (OtpErlangAtom)difference.elementAt(6);			
			
			Synapse.StatechumProcess.parseStatemachine(addedStates,addedTransitions,new OtpErlangList(new OtpErlangObject[0]),initial_state,recorder.added,null,false);
			Synapse.StatechumProcess.parseStatemachine(new OtpErlangList(new OtpErlangObject[0]),removedTransitions,new OtpErlangList(new OtpErlangObject[0]),null,recorder.removed,null,false);
			recorder.relabelling.clear();
			Synapse.StatechumProcess.updateMap(difference.elementAt(5),recorder.relabelling);
			return recorder;
		}

		/** 
		 * Constructs a graph for visualisation of the differences between the two machines. Takes the diff object and the original machine (otherwise it does not know what remained unchanged).
		 * 
		 * @param origMachineObject machine against which this diff was computed
		 * @param differences differences computed earlier and reported by {@link ChangesToGraph#computeGD}
		 * @return graph with colouring to reflect the changes.
		 */
		public static DirectedSparseGraph computeVisualisationParameters(OtpErlangObject origMachineObject, OtpErlangObject differences)
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();
			LearnerGraphND origMachine = new LearnerGraphND(config);Synapse.StatechumProcess.parseStatemachine(origMachineObject,origMachine,null,true);
			
			ChangesToGraph recorder=load(differences);
			return recorder.getDifferenceGraph(origMachine.pathroutines.getGraph());
		}
		
		public <TARGET_A_TYPE,CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>> void applyDiff(AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> grA, Configuration config)
		{
			LearnerGraphMutator<TARGET_A_TYPE,CACHE_A_TYPE> graphPatcher = new LearnerGraphMutator<TARGET_A_TYPE,CACHE_A_TYPE>(grA,config,null);
			for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entry:removed.transitionMatrix.entrySet())
				for(Entry<Label,List<CmpVertex>> transition:entry.getValue().entrySet())
					for(CmpVertex target:removed.getTargets(transition.getValue()))
						graphPatcher.removeTransition(entry.getKey(), transition.getKey(), target);

			for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entry:added.transitionMatrix.entrySet())
			{
				if (entry.getValue().isEmpty())
					graphPatcher.addVertex(entry.getKey());
				else
					for(Entry<Label,List<CmpVertex>> transition:entry.getValue().entrySet())
						for(CmpVertex target:added.getTargets(transition.getValue()))
							graphPatcher.addTransition(entry.getKey(), transition.getKey(), target);
			}
			grA.setInit(added.getInit());
			graphPatcher.removeDanglingStates();
		}
		

		protected Set<CmpVertex> extraVertices = new TreeSet<CmpVertex>();
		
		@Override
		public void addVertex(CmpVertex vertex) {
			super.addVertex(vertex);
			extraVertices.add(vertex);
		}

		
		/** Computes a difference between two graphs and reports as an Erlang-compatible tuple.
		 * 
		 * @param grA first graph (does not matter whether deterministic or not).
		 * @param grB second graph (does not matter whether deterministic or not).
		 * @param config configuration to use in comparisons
		 * @return difference as an Erlang statemachinedifference structure.
		 */
		public static <TARGET_A_TYPE,TARGET_B_TYPE, CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>>
		OtpErlangObject computeGD(AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> grA, AbstractLearnerGraph<TARGET_B_TYPE,CACHE_B_TYPE> grB, Configuration config)
		{
			GD<TARGET_A_TYPE,TARGET_B_TYPE,CACHE_A_TYPE,CACHE_B_TYPE> gd = new GD<TARGET_A_TYPE,TARGET_B_TYPE,CACHE_A_TYPE,CACHE_B_TYPE>();
	
			ChangesToGraph recorder = new ChangesToGraph();
			gd.computeGD(grA, grB, ExperimentRunner.getCpuNumber(),recorder,config);
			
			
			LearnerGraphND origMachine = new LearnerGraphND(grA,config), finalMachineWithoutRelabelling = new LearnerGraphND(grA,config);
			//Synapse.StatechumProcess.parseStatemachine(origMachineObject,origMachine,null,true);
			//Synapse.StatechumProcess.parseStatemachine(origMachineObject,finalMachineWithoutRelabelling,null,true);
			
			// now we apply the patch to the graph, the outcome will be in terms of the original vertices which is why it is easiest to apply a patch rather than use the second graph and figure out the relationship between the vertices.
			recorder.applyDiff(finalMachineWithoutRelabelling,config);
			
			Set<CmpVertex> verticesAdded = new TreeSet<CmpVertex>(), verticesRemoved = new TreeSet<CmpVertex>();
			verticesAdded.addAll(finalMachineWithoutRelabelling.transitionMatrix.keySet());verticesAdded.removeAll(origMachine.transitionMatrix.keySet());verticesAdded.addAll(recorder.extraVertices);
			verticesRemoved.addAll(origMachine.transitionMatrix.keySet());verticesRemoved.removeAll(finalMachineWithoutRelabelling.transitionMatrix.keySet());
			List<OtpErlangAtom> addedList = new LinkedList<OtpErlangAtom>(), removedList = new LinkedList<OtpErlangAtom>();
			for(CmpVertex a:verticesAdded) addedList.add(new OtpErlangAtom(a.getStringId()));for(CmpVertex a:verticesRemoved) removedList.add(new OtpErlangAtom(a.getStringId()));
			
			OtpErlangTuple addedTuple = Synapse.StatechumProcess.constructFSM(recorder.added), removedTuple = Synapse.StatechumProcess.constructFSM(recorder.removed);
			OtpErlangObject map = Synapse.StatechumProcess.mapToObject(recorder.relabelling);
			
			// Important: here I rely on the order of element in the statemachine tuple in order to build a difference tuple.
			return new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom("statemachinedifference"),
				addedTuple.elementAt(2),removedTuple.elementAt(2),new OtpErlangList(addedList.toArray(new OtpErlangObject[0])),new OtpErlangList(removedList.toArray(new OtpErlangObject[0])),map,addedTuple.elementAt(3)});
		}
		
		/** Adds a vertex corresponding to the provided one to the graph.
		 * 
		 * @param graphToUpdate where to add
		 * @param vertex what to add.
		 */
		protected DeterministicVertex addVertex(DirectedSparseGraph graphToUpdate, CmpVertex vertex)
		{
			DeterministicVertex vert = new DeterministicVertex(vertex.getStringId());
			
			vert.setAccept(vertex.isAccept());
			vert.setColour(vertex.getColour());
			vert.setHighlight(vertex.isHighlight());
			graphToUpdate.addVertex(vert);return vert;
		}
		
		protected void updateAnnotationsWithColour(DirectedSparseGraph graphToUpdate, LearnerGraphND graph,EdgeAnnotation transitionAnnotation, Color color)
		{
			Map<StatePair,DeterministicEdge> pairsToNewEdges = new HashMap<StatePair,DeterministicEdge>();
			
			for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entry:graph.transitionMatrix.entrySet())
			{
				CmpVertex from = entry.getKey();
				DeterministicVertex fromVertex = DeterministicDirectedSparseGraph.findVertexNamed(from,graphToUpdate);
				if (fromVertex == null) fromVertex = addVertex(graphToUpdate, from);
				
				Map<Label,Map<String,Color>> lbl = transitionAnnotation.get(from.getStringId());
				if (lbl == null)
				{
					lbl = new TreeMap<Label,Map<String,Color>>();transitionAnnotation.put(from.getStringId(), lbl);
				}

				for(Entry<Label,List<CmpVertex>> transition:entry.getValue().entrySet())
					for(CmpVertex target:transition.getValue())
					{
						DeterministicVertex targetVertex = DeterministicDirectedSparseGraph.findVertexNamed(target,graphToUpdate);
						if (targetVertex == null) targetVertex = addVertex(graphToUpdate, target);
						DeterministicEdge e = pairsToNewEdges.get(new StatePair(from,target));
						if (e == null)
						{
								e = new DeterministicEdge(fromVertex,targetVertex);
								Set<Label> labels = new TreeSet<Label>();labels.add(transition.getKey()); 
								e.addUserDatum(JUConstants.LABEL, labels, UserData.CLONE);e.addUserDatum(JUConstants.DIFF, color, UserData.SHARED);
								graphToUpdate.addEdge(e);
								pairsToNewEdges.put(new StatePair(from,target),e);
						}
						else
							((Set<Label>)e.getUserDatum(JUConstants.LABEL)).add(transition.getKey());
						

						Map<String,Color> targetToColour = lbl.get(transition.getKey());
						if (targetToColour == null)
						{// this is the first annotation for the specific target state
							targetToColour = new TreeMap<String,Color>();lbl.put(transition.getKey(),targetToColour);
						}
						targetToColour.put(target.getStringId(),color);
					}
			}
		}
		
		protected void removeFromGraph(DirectedSparseGraph graphToUpdate, LearnerGraphND graph)
		{
			for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entry:graph.transitionMatrix.entrySet())
			{
				CmpVertex from = entry.getKey();
				DeterministicVertex fromVertex = DeterministicDirectedSparseGraph.findVertexNamed(from,graphToUpdate);
				if (fromVertex == null)
					throw new IllegalArgumentException("source state in diff is not known, looking at "+from);
				
				for(Entry<Label,List<CmpVertex>> transition:entry.getValue().entrySet())
					for(CmpVertex target:transition.getValue())
					{
						DeterministicVertex targetVertex = DeterministicDirectedSparseGraph.findVertexNamed(target,graphToUpdate);
						if (targetVertex == null)
							throw new IllegalArgumentException("target state in diff is not known, looking at "+target);
						Edge e=DeterministicDirectedSparseGraph.findEdge(fromVertex, targetVertex);
						if (e == null)
							throw new IllegalArgumentException("edge in diff was not found");
						
						Set<Label> labels = ((Set<Label>)e.getUserDatum(JUConstants.LABEL));
						labels.remove(transition.getKey());if (labels.isEmpty()) graphToUpdate.removeEdge(e);
					}
			}
		}
		
		/** Constructs a graph corresponding to changes recorded. Not anywhere as sophisticated as that in showGD but good enough for basic use. 
		 * 
		 * @param origGraph the source graph, as a Jung graph to permit both non-deterministic and deterministic graphs to be handled.
		 * @return constructed graph
		 */
		public DirectedSparseGraph getDifferenceGraph(DirectedSparseGraph origGraph)
		{
			final EdgeAnnotation transitionAnnotation = new EdgeAnnotation();
			DirectedSparseGraph outcome = origGraph;//new DirectedSparseGraph();
			for(Object constraint:outcome.getEdgeConstraints())
				if (constraint instanceof org.apache.commons.collections.functors.NotPredicate)
				{
					outcome.getEdgeConstraints().remove(constraint);break;
				}
			removeFromGraph(outcome, removed);
			updateAnnotationsWithColour(outcome, added, transitionAnnotation, Color.GREEN);
			updateAnnotationsWithColour(outcome, removed, transitionAnnotation, Color.RED);
			DeterministicDirectedSparseGraph.findInitial(outcome).removeUserDatum(JUConstants.INITIAL);
			DeterministicDirectedSparseGraph.findVertexNamed(added.getInit(),outcome).addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
			Visualiser.LayoutOptions options = new Visualiser.LayoutOptions();options.showDIFF=true;
			outcome.addUserDatum(JUConstants.LAYOUTOPTIONS,options, UserData.SHARED);
			return outcome;
		}


	}
}
