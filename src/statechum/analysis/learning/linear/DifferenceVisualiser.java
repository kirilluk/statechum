package statechum.analysis.learning.linear;

import java.awt.Color;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicEdge;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.linear.GD.ChangesRecorder;
import statechum.analysis.learning.linear.GD.PatchGraph;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.PathRoutines.EdgeAnnotation;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

public class DifferenceVisualiser {

	public DifferenceVisualiser() {
	}
	
	public static <TARGET_A_TYPE,TARGET_B_TYPE, CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>>
		DirectedSparseGraph computeVisualGD(AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> grA, AbstractLearnerGraph<TARGET_B_TYPE,CACHE_B_TYPE> grB, Configuration config)
	{
		GD<TARGET_A_TYPE,TARGET_B_TYPE,CACHE_A_TYPE,CACHE_B_TYPE> gd = new GD<TARGET_A_TYPE,TARGET_B_TYPE,CACHE_A_TYPE,CACHE_B_TYPE>();
	
			ChangesToGraph recorder = new ChangesToGraph(null);
			gd.computeGD(grA, grB, ExperimentRunner.getCpuNumber(),recorder,config);
			
			return recorder.getGraph(grA.pathroutines.getGraph());
	}
	
	public static class ChangesToGraph extends ChangesRecorder
	{

		public ChangesToGraph(PatchGraph nextInStack) {
			super(nextInStack);
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
			System.out.println("started with "+graph);
			
			for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entry:graph.transitionMatrix.entrySet())
			{
				CmpVertex from = entry.getKey();
				DeterministicVertex fromVertex = DeterministicDirectedSparseGraph.findVertexNamed(from,graphToUpdate);
				if (fromVertex == null)
					throw new IllegalArgumentException("source state not known");
				
				for(Entry<Label,List<CmpVertex>> transition:entry.getValue().entrySet())
					for(CmpVertex target:transition.getValue())
					{
						DeterministicVertex targetVertex = DeterministicDirectedSparseGraph.findVertexNamed(target,graphToUpdate);
						if (targetVertex == null)
							throw new IllegalArgumentException("target state not known");
						Edge e=DeterministicDirectedSparseGraph.findEdge(fromVertex, targetVertex);
						if (e == null)
							throw new IllegalArgumentException("edge not found");
						
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
		public DirectedSparseGraph getGraph(DirectedSparseGraph origGraph)
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
