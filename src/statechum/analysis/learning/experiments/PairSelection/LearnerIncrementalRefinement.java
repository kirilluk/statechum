/* Copyright (c) 2015 The University of Sheffield
 * 
 * This file is part of StateChum.
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
package statechum.analysis.learning.experiments.PairSelection;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.TreeSet;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;
import statechum.Pair;
import statechum.StringLabel;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.collections.ArrayMapWithSearch;
import statechum.collections.HashMapWithSearch;

public class LearnerIncrementalRefinement 
{

	static class LabelListPair
	{
		public final A_Label whatToSplit;
		public final List<Label> positives = new LinkedList<Label>(), negatives = new LinkedList<Label>();
		public final int distanceFromInconsistency;
		
		public LabelListPair(A_Label l,int d)
		{
			whatToSplit = l;
			distanceFromInconsistency = d;
		}
	}
	
	/** Map from vertices in an initial PTA to sequences of labels leading back from them to the initial state. */
	Map<VertID,ArrayList<Label>> labelsLeadingToThisInHardFacts;
	
	/** This is an initialisation routine, taking an initial concrete PTA and constructing the relevant structure.
	 */
	void constructLabelsLeadingToStates()
	{
		int coreGraphStateNumber = initialPta.getStateNumber();
		labelsLeadingToThisInHardFacts = initialPta.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<VertID, ArrayList<Label>>(coreGraphStateNumber):new HashMapWithSearch<VertID,ArrayList<Label>>(coreGraphStateNumber);
		Map<CmpVertex,List<Label>> cmap = initialPta.pathroutines.computeShortPathsToAllStates();
		for(Map.Entry<CmpVertex, List<Label>> entry:cmap.entrySet())
		{
			ArrayList<Label> pathToVert = new ArrayList<Label>();pathToVert.addAll(entry.getValue());Collections.reverse(pathToVert);
			labelsLeadingToThisInHardFacts.put(entry.getKey().getOrigState(), pathToVert);
			if (!entry.getKey().isAccept())
				rejectStates.add(entry.getKey());
		}
	}
	
	/** The graph at the current iteration, using abstract labels. */
	LearnerGraph graph;
	
	/** The initial PTA, using concrete labels. */
	LearnerGraph initialPta;
	
	/** Collection of reject vertices, we keep them separately because we'd like mergers to be unaffected by rejects, however we still need them in order to be able to split labels and reject clearly invalid mergers. */
	Set<VertID> rejectStates;
	
	/**  Abstract labels. */
	public static class A_Label extends StringLabel
	{
		public final Set<Label> concreteLabels = new TreeSet<Label>();
		
		public A_Label(String l) 
		{
			super(l);
		}
	}
	
	// Maps concrete to abstract labels.
	Map<Label,A_Label> concreteToAbstract;
	
	public void abstractLabels()
	{
		Map<String,A_Label> prefixToLabel = new TreeMap<String,A_Label>();
		for(Label l:initialPta.pathroutines.computeAlphabet())
		{
			String[] str = ((StringLabel)l).toString().split("/");
			A_Label abstractLabel = prefixToLabel.get(str[0]);
			if (abstractLabel == null)
			{
				abstractLabel = new A_Label(str[0]);prefixToLabel.put(str[0],abstractLabel);
			}
			
			concreteToAbstract.put(l, abstractLabel);abstractLabel.concreteLabels.add(l);
		}
		
	}
	
	/** Given <i>mergedVertices</i>, describing vertices to be merged, looks for inconsistencies. If any are detected, constructs a prioritised list of abstract labels to be split.
	 * 
	 * @param mergedVertices vertices returned by a generalised merger routine. 
	 */
	public List<LabelListPair> constructListOfConcreteLabels(Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices)
	{
        final Map<VertID,Collection<VertID>> hardOrig = graph.learnerCache.getMergedToHardFacts();
		final List<LabelListPair> abstractLabelsToSplit = new ArrayList<LabelListPair>();
		for(AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:mergedVertices)
		{// we iterate through all outgoing transition, aiming to identify inconsistencies. Labels that lead to both accept and reject-states are candidates for splitting.
			
			for(Map.Entry<Label,ArrayList<CmpVertex>> lbl_targets:eqClass.getOutgoing().entrySet())
			{
				ArrayList<CmpVertex> Targets = lbl_targets.getValue();
			
	            Collection<VertID> hardVertices = new LinkedList<VertID>();
	            boolean inconsistency = false;
				for(CmpVertex v:Targets)
			    {
			        if (hardOrig != null && hardOrig.containsKey(v))
			        {
			            hardVertices.addAll(hardOrig.get(v));
			        }
			        else
			            hardVertices.add(v);
			    }
				
				// note that lbl_targets.getKey() returns an abstract label, however we are looking at concrete labels here. These are contained in the labelToProximity map: VertID->{Label,proximity}
				boolean firstState = rejectStates.contains(hardVertices.iterator().next());
				for(VertID v:hardVertices)
					if (rejectStates.contains(v) != firstState)
						inconsistency = true;
				
				if (inconsistency)
				{
					int distance=0;

					Map<A_Label,LabelListPair> lblToWhatToSplit = new TreeMap<A_Label,LabelListPair>();
					int pathsAtRoot = 0;// the number of traces we are leading from the inconsistent merge to the initial state, leading in lock-step. If this falls to 1 because all others reached the initial state, we stop.
					while(pathsAtRoot < hardVertices.size()-1)
					{
						pathsAtRoot = 0;
						for(VertID v:hardVertices)
						{
							ArrayList<Label> pathToRoot = labelsLeadingToThisInHardFacts.get(v);
							if (distance >= pathToRoot.size())
							{// reached an end of this path
								pathsAtRoot++;break;
							}
							Label lbl=pathToRoot.get(distance);
							A_Label abstractLabel = concreteToAbstract.get(lbl);
							LabelListPair pair = lblToWhatToSplit.get(abstractLabel);
							if (pair == null)
							{
								pair = new LabelListPair(abstractLabel,distance);lblToWhatToSplit.put(abstractLabel,pair);
							}
							if (rejectStates.contains(v))
								pair.negatives.add(lbl);
							else
								pair.positives.add(lbl);
						}
						distance++;
					}
					
					for(Map.Entry<A_Label, LabelListPair> possibleSplit:lblToWhatToSplit.entrySet())
						if (possibleSplit.getValue().positives.size() > 0 && possibleSplit.getValue().negatives.size() > 0)
							abstractLabelsToSplit.add(possibleSplit.getValue());
				}
			}
			
			// Now we have gone through all the merge decisions and identified where splits can be made. Sort them in terms of the order from those closest to 
		}
		
		return abstractLabelsToSplit;
	}

	
	public Map<CmpVertex,Set<A_Label>> determineLabelsLeadingToStates()
	{
		//for(Map.Entry<CmpVertex, >)
		return null;
	}
	/** Given our map from concrete to abstract labels  
	 * and a current graph with a designated vertex, it adds all paths from an initial (concrete) PTA to it, starting from <i>ptaVertex</i>. Used both for construction of an 
	 * initial PTA and for unfolding paths where labels are refined.
	 *
	 * @param startingVertex vertex in the graph to start augmenting from
	 * @param ptaVertex vertex in an initial PTA to be 'merged' into the graph of interest.
	 */
	void AugmentPTAWithAbstractedTrace(CmpVertex startingVertex,VertID ptaVertex)
	{
		JUConstants newColour = null;
		
		int coreGraphStateNumber = graph.getStateNumber();
		Queue<Pair<CmpVertex,CmpVertex>> fringe = new LinkedList<Pair<CmpVertex,CmpVertex>>();
		Map<CmpVertex,CmpVertex> statesInFringe = initialPta.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,CmpVertex>(coreGraphStateNumber):new HashMapWithSearch<CmpVertex,CmpVertex>(coreGraphStateNumber);// in order not to iterate through the list all the time.
		fringe.add(new Pair<CmpVertex,CmpVertex>(startingVertex,initialPta.findVertex(ptaVertex)));
		while(!fringe.isEmpty())
		{
			Pair<CmpVertex,CmpVertex> currentPair = fringe.remove();
			Map<Label,CmpVertex> targetsGraph = initialPta.transitionMatrix.get(currentPair.firstElem), targetsInitialPTA = graph.transitionMatrix.get(currentPair.secondElem);
			
			if(targetsInitialPTA != null && !targetsInitialPTA.isEmpty())
				for(Entry<Label,CmpVertex> labelstate:targetsInitialPTA.entrySet())
				for(CmpVertex target:initialPta.getTargets(labelstate.getValue()))
				{
					A_Label abstractLabel = concreteToAbstract.get(labelstate.getKey());
					CmpVertex targetStateInGraph = targetsGraph.get(abstractLabel);
					if (targetStateInGraph == null)
					{
						targetStateInGraph = AbstractLearnerGraph.generateNewCmpVertex(graph.nextID(true),graph.config);targetsGraph.put(labelstate.getKey(), targetStateInGraph);
						// same case for both splitting and constructing an initial graph.
					}
					else
					{// we have a matched transition between an initial PTA and our graph of interest. If the one in the graph is currently being split off, we need to mark it as being split off rather than proceed to follow the graph.
					// If the entered state only has our incoming transitions, we do not need to split them off
						
					}
					fringe.offer(new Pair<CmpVertex,CmpVertex>(targetStateInGraph,target));
				}
		}
		
		List<Label> sequence = null;
		boolean accepted = true;
		CmpVertex currentState = startingVertex, prevState = null;
		Iterator<Label> inputIt = sequence.iterator();
		Label lastInput = null;
		int position = 0;
		while(inputIt.hasNext() && currentState != null)
		{
			if (!currentState.isAccept())
			{// not the last state and the already-reached state is not accept, while all prefixes of reject sequences should be accept ones. 
				currentState.setHighlight(true);
				throw new IllegalArgumentException("incompatible "+
						(accepted?"accept":"reject")+" labelling: "+sequence.subList(0, position)+" when trying to append "+sequence);
			}
			prevState = currentState;
                        lastInput = inputIt.next();++position;
			
			currentState = graph.transitionMatrix.get(prevState).get(lastInput);
		}
		
		if (currentState == null)
		{// the supplied path does not exist in PTA, the first non-existing vertex is from state prevState with label lastInput

			synchronized (AbstractLearnerGraph.syncObj) 
			{
				while(inputIt.hasNext())
				{
					prevState = graph.addVertex(prevState, true, lastInput);prevState.setColour(newColour);
					prevState.setColour(newColour);prevState.setDepth(position++);
					lastInput = inputIt.next();
				}
				// at this point, we are at the end of the sequence. Last vertex is prevState and last input if lastInput
				CmpVertex newVertex = graph.addVertex(prevState, accepted, lastInput);
				newVertex.setColour(newColour);newVertex.setDepth(position++);
			}
		}
	}
	// Given a complete split of a label, constructs a revised graph
	public LearnerGraph constructSplit(LabelListPair possibleSplit)
	{
		A_Label l_pos = new A_Label(possibleSplit.whatToSplit.label+"p"), l_neg = new A_Label(possibleSplit.whatToSplit.label+"n");
		l_pos.concreteLabels.addAll(possibleSplit.positives);l_neg.concreteLabels.addAll(possibleSplit.negatives);
		for(Label v:possibleSplit.positives)
			concreteToAbstract.put(v, l_pos);
		for(Label v:possibleSplit.negatives)
			concreteToAbstract.put(v, l_neg);
		
		
		for(Entry<CmpVertex,Map<Label,CmpVertex>> transition:graph.getTransitionMatrix().entrySet())
			for(Map.Entry<Label,CmpVertex> lblToVertex:transition.getValue().entrySet())
			{
				if (lblToVertex.getKey() == possibleSplit.whatToSplit)
				{
					}
			}
		
		return null;
	}
	
	public int computePairCompatibilityScore(StatePair pairToMerge, Collection<StatePair> pairsToMerge, Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices) 
	{
		int computedScore = graph.pairscores.computePairCompatibilityScore_general(pairToMerge,pairsToMerge, mergedVertices);
		return computedScore;
	}
}
