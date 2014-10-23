package statechum.analysis.learning.experiments.PairSelection;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.StringLabel;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;

public class LearnerWithLabelRefinement extends ASE2014.EDSM_MarkovLearner 
{
	
	public static class PrevVertexAndOutgoingLabel
	{
		public PrevVertexAndOutgoingLabel(CmpVertex prevState, AbstractLabel lastInput) {
			vertPTA = prevState;label = lastInput;
		}
		public final AbstractLabel label;
		public final CmpVertex vertPTA;
	}

	/** Describes an abstract version of a label that has a string representation appearing like a real label and methods to manipulate abstract labels. */
	public static class AbstractLabel extends StringLabel
	{
		/**
		 * ID for serialisation.
		 */
		private static final long serialVersionUID = -8106697725431488206L;			

		private final int abstractionInstance;
		private int lastChildNumber;
		
		private final AbstractLabel labelThisOneWasSplitFrom;
		
		//protected Map<Label> labelToAbstractLabel;
		
		/** This one is only used when labels are passed from Erlang to Java. All subsequent labels are obtained by splitting existing ones. 
		 */
		public AbstractLabel(String l) 
		{
			super(l);
			abstractionInstance = 0;// the parent of all possible abstractions.
			labelThisOneWasSplitFrom = null;// the top-level label
		}

		protected AbstractLabel(AbstractLabel parent, int abstractionNum) 
		{
			super(parent.getTopLevelName()+","+abstractionNum);
			assert abstractionNum > 0;
			abstractionInstance = abstractionNum;labelThisOneWasSplitFrom = parent;
			labelsThisoneabstracts.addAll(parent.labelsThisoneabstracts);
		}
		
		public String getTopLevelName()
		{
			if (abstractionInstance != 0)
				return labelThisOneWasSplitFrom.getTopLevelName();
			
			return label;
			
		}
		
		public int getNextAbstractionInstance()
		{
			if (abstractionInstance != 0)
				return labelThisOneWasSplitFrom.getNextAbstractionInstance();
			
			return ++lastChildNumber;
		}
		
		/** This one is used to resolve inconsistencies, by preserving only elements of the SplitFrom label that are not in contradiction to transitions from the state of interest.
		 * The asymmetry is due to us wanting to merge a path into an existing graph, where splitting labels that are already present raises all sort of problems but the path
		 * being merged is part of a tree that is therefore easy to split.
		 * 
		 *  Since the same label could be used multiple times in different part of a graph, it is imperative to create a new instance of it for the purpose of splitting.
		 */
		public AbstractLabel splitLabel(AbstractLabel SplitFrom, Map<Label,CmpVertex> stateToSplit, CmpVertex source)
		{
			int nextID = getNextAbstractionInstance();
			AbstractLabel outcome = new AbstractLabel(SplitFrom,nextID);
			
			for(Map.Entry<Label, CmpVertex> entry:stateToSplit.entrySet())
				if (source == entry.getValue())
					outcome.labelsThisoneabstracts.removeAll(((AbstractLabel)entry.getKey()).labelsThisoneabstracts);

			//assert !outcome.labelsThisoneabstracts.isEmpty() : "inconsistency around the state " + stateToSplit;
			return outcome;
		}
		
		/** In a number of cases, we make a series of mergers, eventually leading to an inconsistency. In those cases we have to backtrack, following a trail that has to be created.
		 *  A-a-> B-a->C-a->D-a->E
		 *  P-ab->Q-ab->R-a->S-a->#T here we split at Q where ab is branches off. Note that since this is the last point of inconsistency
		 *  before the last state, the positive path ab-ab-a-a is included in the a-a-a-a (if it were not included, we would have encountered an inconsistency later).
		 *  This permits us to just remove elements that are in contradiction from the label of interest.
		 *  
		 *  Returns true if it were possible to avoid inconsistencies by splitting and false otherwise.
		 */
		protected boolean backtrackAnSplit(LearnerGraph graph, List<PrevVertexAndOutgoingLabel> trail, CmpVertex endState)
		{
			CmpVertex currState = endState;
			Iterator<PrevVertexAndOutgoingLabel> iter = trail.iterator();
			while(iter.hasNext())
			{
				PrevVertexAndOutgoingLabel elem = iter.next();
				Map<Label,CmpVertex> transitions = graph.transitionMatrix.get(elem.vertPTA);
				AbstractLabel splitOutcome = splitLabel(elem.label, transitions,currState);
				if (!splitOutcome.labelsThisoneabstracts.isEmpty())
				{// split successful, replace the label. Simply updating the ID would be more efficient but this ID could be used by a label-target map therefore making this map inconsistent.
					transitions.remove(elem.label);transitions.put(elem.label, currState);
					return true;
				}
				currState = elem.vertPTA;
			}
			
			return false;
		}
		
		private final Set<Label> labelsThisoneabstracts = new TreeSet<Label>();
		
		private boolean labelsIntersect(AbstractLabel B)
		{
			Set<Label> labels = new TreeSet<Label>();labels.addAll(labelsThisoneabstracts);labels.retainAll(B.labelsThisoneabstracts);return labels.isEmpty();
		}

		@Override
		public int compareTo(Label o) 
		{
			if (!(o instanceof AbstractLabel))
				throw new IllegalArgumentException("Cannon compare an abstract label with a non-abstract label");
			return super.compareTo(o);
		}

		@Override
		public boolean equals(Object obj) 
		{
			if (!(obj instanceof AbstractLabel))
				throw new IllegalArgumentException("Cannon compare an abstract label with a non-abstract label");
			return super.equals(obj);
		}
	}
	
	
	
	Map<Label,Map<Label,Integer>> similarityBetweenLabels;

	public LearnerWithLabelRefinement(LearnerEvaluationConfiguration evalCnf, LearnerGraph argInitialPTA, int threshold) 
	{
		super(evalCnf, argInitialPTA, threshold);
	}

	@Override
	public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
	{
		return super.MergeAndDeterminize(original, pair);
	}

	// A-abc->B
	// A-de ->C
	// 
	// A->adf->? 
	// It seems that the most appropriate way is to split the label based on its intersection with the labels in the graph. Since I may need to backtrack,
	// this has to be implemented in the form construct-commit where construction builds a PTA like that in the score computation and this is subsequently
	// applied as in merge states. The first pass could update the PTA (or copy it) and resolve inconsistencies. The second pass would subsequently merge it.
	//
	// Perhaps f should stay with a or d if that is closer to f using the similarity metric. 
	//
	// Where used as part of score computation, I need to split labels as part of construction of maps of labels to add to existing states. Augmentation is hence best 
	// implemented as a pseudo-merge where I'm turning a path into a tree and then merging it into our PTA.
	public int computePairCompatibilityScore_internal(StatePair origPair,Map<CmpVertex,List<CmpVertex>> mergedVertices) 
	{
		mergedVertices.clear();// for every vertex of the model, gives a set of PTA vertices which were joined to it, for those of them which lead to a new (PTA-only) state
			// note that PTA states may easily be merged with other PTA states, in which case they will feature as keys of this set.
		
		int score = -1;// compatibility score between states in the pair
		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue containing pairs to be explored
		Queue<Boolean> currentRedFromPta = new LinkedList<Boolean>();// FIFO queue containing true if the red node comes from a branch of a PTA which has been previously already merged into the machine
		currentExplorationBoundary.add(origPair);currentRedFromPta.add(false);
		
		while(!currentExplorationBoundary.isEmpty())
		{
			StatePair currentPair = currentExplorationBoundary.remove();Boolean redFromPta = currentRedFromPta.remove();
			boolean RedAndBlueToBeMerged = false;// this one is set to true if states in the current pair have to be merged. 
			// This will be so for all state pairs where a blue node can 
			// make moves which the red one cannot match. The term "merged" does not refer to whether 
			// two nodes are actually merged - they have to be anyway, however if there are sequences of 
			// nodes with identical moves, PTA nodes do not contribute to anything - we only need
			// to consider those which branch. mergedVertices is only updated when we find a blue vertex which 
			// can accept input a red node cannot accept. 

			if (!AbstractLearnerGraph.checkCompatible(currentPair.getQ(),currentPair.getR(),coregraph.pairCompatibility))
				return -1;// incompatible states
			if (!redFromPta.booleanValue())
				++score;
			Map<Label,CmpVertex> targetBlue = coregraph.transitionMatrix.get(currentPair.getQ());

			for(Entry<Label,CmpVertex> blueEntry:targetBlue.entrySet())
			{
				CmpVertex nextRedState = coregraph.transitionMatrix.get(currentPair.getR()).get(blueEntry.getKey());
				if (nextRedState != null)
				{// both states can make a transition - this would be the case of "non-determinism" for Merge&Determinize
					boolean newRedFromPta = redFromPta;
					
					// PTA does not have loops, but the original automaton has
					// and one of those loops is not on the transition diagram, namely the one related to B=A
					if (nextRedState == origPair.getQ())
					{
						nextRedState = origPair.getR(); // emulates the new loop
						newRedFromPta = coregraph.config.getLearnerScoreMode() != Configuration.ScoreMode.COMPATIBILITY; // and since the original score computation algorithm cannot do this, we pretend to be unable to do this either
						// The problem is that since we effectively merge the
						// states at this point, a loop introduced by merging
						// adjacent states may suck many PTA states into it, 
						// so that two transitions which would not normally be
						// near each other will be merged. For this reason, it
						// is possible that our score computation will deliver
						// a higher value that the conventional matching 
						// (where in the considered situation we'll be 
						// matching PTA with itself and PTA may be sparse).
					}

					if (coregraph.config.getScoreCompatibilityScoreComputationBugEmulation())
						redFromPta = newRedFromPta;
					StatePair nextStatePair = new StatePair(blueEntry.getValue(),nextRedState);
					currentExplorationBoundary.offer(nextStatePair);currentRedFromPta.offer(newRedFromPta);
				}
				else
				{// the current red state cannot make a transition, perhaps PTA states associated with it can
					nextRedState = coregraph.pairscores.findNextRed(mergedVertices,currentPair.getR(),blueEntry.getKey());
					if (nextRedState != null)
					{// both states can make a transition - this would be the case of "non-determinism" for Merge&Determinize
					 // The red state is the one originally from a previously-merged PTA branch, so here we are merging PTA with itself. 

						// Since we are merging PTA with itself and PTA does not have loops, we cannot reenter the original blue state. Moreover,
						// since we called findNextRed, we are looking at transitions from the PTA states. For this reason, we cannot enter the 
						// blue state since PTA does not have loops.
						assert nextRedState != origPair.getQ() : "inconsistent PTA";
						
						StatePair nextStatePair = new StatePair(blueEntry.getValue(),nextRedState);
						currentExplorationBoundary.offer(nextStatePair);currentRedFromPta.offer(coregraph.config.getLearnerScoreMode() != Configuration.ScoreMode.COMPATIBILITY);// from now on, no increments to the score
					}
					else
					{
						// If the blue can make a move, but the red one cannot, it means that the blue vertex has 
						// transitions with labels which are not contained in the set of labels on 
						// transitions from the red state. For this reason, we have to merge the current blue vertex with the current red one.
						RedAndBlueToBeMerged = true;
						
						// there is no point exploring further since the transition leaving the blue state is not matched to any red one.
					}
				}
			}
			
			if (RedAndBlueToBeMerged)
			{// if the current pair of states is to be merged, do it (i.e. record them as merged).
				List<CmpVertex> redMerged = mergedVertices.get(currentPair.getR());
				if (redMerged == null)
				{
					redMerged = new LinkedList<CmpVertex>();mergedVertices.put(currentPair.getR(), redMerged);
				}
				redMerged.add(currentPair.getQ());
			}
		}
		return score;
	}

	
	
	@Override
	public void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind, List<Label> sequence, boolean accepted, JUConstants newColour) 
	{
		CmpVertex currentState = coregraph.getInit(), prevState = null;
		Iterator<Label> inputIt = sequence.iterator();
		List<PrevVertexAndOutgoingLabel> trail = new ArrayList<PrevVertexAndOutgoingLabel>();
		Label lastInput = null;
		int position = 0;
		while(inputIt.hasNext() && currentState != null)
		{
			if (!currentState.isAccept())
			{// it is possible that this inconsistency is due to overgeneralisation, split labels as appropriate.
				
				
				currentState.setHighlight(true);
				throw new IllegalArgumentException("incompatible "+
						(accepted?"accept":"reject")+" labelling: "+sequence.subList(0, position)+" when trying to append "+sequence);
			}
			prevState = currentState;lastInput = inputIt.next();++position;
			currentState = coregraph.transitionMatrix.get(prevState).get(lastInput);
			trail.add(new PrevVertexAndOutgoingLabel(prevState,(AbstractLabel)lastInput));
		}

		if (currentState == null)
		{// the supplied path does not exist in PTA, the first non-existing vertex is from state prevState with label lastInput

			synchronized (AbstractLearnerGraph.syncObj) 
			{
				while(inputIt.hasNext())
				{
					prevState = coregraph.addVertex(prevState, true, lastInput);prevState.setColour(newColour);
					prevState.setColour(newColour);prevState.setDepth(position++);
					lastInput = inputIt.next();
				}
				// at this point, we are at the end of the sequence. Last vertex is prevState and last input if lastInput
				CmpVertex newVertex = coregraph.addVertex(prevState, accepted, lastInput);
				newVertex.setColour(newColour);newVertex.setDepth(position++);
			}
			
		}
		else
		{// we reached the end of the string to add to the PTA, with currentState being the current PTA state.
				if (currentState.isAccept() != accepted)
				{
					currentState.setHighlight(true);
					throw new IllegalArgumentException("incompatible "+(accepted?"accept":"reject")+" labelling: "+sequence.subList(0, position));
				}
		}
	
		coregraph.learnerCache.invalidate();
	}

	@Override
	public void AugumentPTA_and_QuestionPTA(LearnerGraph pta, RestartLearningEnum ptaKind, List<Label> sequence, boolean accepted, JUConstants newColour) 
	{
		super.AugumentPTA_and_QuestionPTA(pta, ptaKind, sequence, accepted, newColour);
	}
}
