package statechum.analysis.learning.experiments.PairSelection;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairOfPaths;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.MarkovClassifierLG;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.TrueFalseCounter;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.apps.QSMTool;
import statechum.collections.ArrayMapWithSearchPos;

public class LearningSupportRoutines
{
	/** Looks for a labels each of which is only used on transitions entering a specific state.
	 * <p/>
	 * It also looks for labels that are only present on a single transition in a graph, hence uniquely identifying both their source and their target states.
	 * <p/>
	 * Assumes that all states are reachable from an initial state, otherwise it may decide that some labels are not unique to 
	 * the initial state while in reality they are unique to all states that are reachable from it.
	 * 
	 * @param graph the graph where to look for such labels.
	 * @return a map from labels to pairs of states, where the first element is the state the label of interest is uniqueFrom and the second is the state it is uniqueTo. The first of the two can be null, the second cannot.
	 * <p>Justification: if a label is uniqueTo, it can be used on a number of transitions to the state of interest. Each of such transitions may have a different source state. Where it is uniqueFrom, this is a 
	 * label that is only used on a single transition in a graph, hence it is also uniqueTo. UniqueTo can only be uniqueFrom if it is only used on a single transition.
	 */
	public static Map<Label,Pair<CmpVertex,CmpVertex> > uniqueIntoState(LearnerGraph graph)
	{
		Set<Label> deadLabels = new HashSet<Label>();
		
		Map<Label, Pair<CmpVertex,CmpVertex> > labelToStatePair = new TreeMap<Label,Pair<CmpVertex,CmpVertex>>();
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
			for(Entry<Label,CmpVertex> target:entry.getValue().entrySet())
				if (!deadLabels.contains(target.getKey()))
				{// the label is not already recorded as leading to multiple different states.
					Pair<CmpVertex,CmpVertex> recordedSourceTarget = labelToStatePair.get(target.getKey());
					if (recordedSourceTarget == null)
						// first time we've seen this label in use
						labelToStatePair.put(target.getKey(),new Pair<CmpVertex,CmpVertex>(entry.getKey(),target.getValue()));
					else
						if (recordedSourceTarget.secondElem != target.getValue())
						{
							// record the label as leading to multiple states
							deadLabels.add(target.getKey());
							labelToStatePair.remove(target.getKey());
						}
						else
							// a different state leading to the same target, clear the first element of the pair since the label of interest is not longer uniqueFrom
							labelToStatePair.put(target.getKey(),new Pair<CmpVertex,CmpVertex>(null,recordedSourceTarget.secondElem));
				}
		
		return labelToStatePair;
	}
	
	/** Identifies labels that are unique from specific states.
	 *  
	 * @param graph graph in which to calculate those labels
	 * @return map from a label to a vertex that can be identified by a transition with a label of interest. 
	 */
	public static Map<Label,CmpVertex> uniqueFromState(LearnerGraph graph)
	{
		Set<Label> deadLabels = new HashSet<Label>();
		
		Map<Label,CmpVertex> labelToState = new TreeMap<Label,CmpVertex>();
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
		{
			CmpVertex state = entry.getKey(); 
			for(Entry<Label,CmpVertex> target:entry.getValue().entrySet())
				if (!deadLabels.contains(target.getKey()))
				{// the label is not already recorded as present on transitions from different states.
					CmpVertex recordedState = labelToState.get(target.getKey());
					if (recordedState == null)
						// first time we've seen this label in use
						labelToState.put(target.getKey(),state);
					else
						if (recordedState != state)
						{
							// record the label as leading to multiple states
							deadLabels.add(target.getKey());
							labelToState.remove(target.getKey());
						}
				}
		}
		
		return labelToState;
	}

	/** Finds a label that uniquely identifies the initial state. 
	 */ 
	public static Label uniqueFromInitial(LearnerGraph graph)
	{
		if (graph.getInit() == null)
			return null;
		Set<Label> liveLabels = new HashSet<Label>();liveLabels.addAll(graph.transitionMatrix.get(graph.getInit()).keySet());
		
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
			if (entry.getKey() != graph.getInit()) liveLabels.removeAll(entry.getValue().keySet());
		
		if (liveLabels.isEmpty())
			return null;
		
		return liveLabels.iterator().next();
	}
	
	/** All label starting from this prefix are going to be merged. */
	public static final String prefixOfMandatoryMergeTransition = "toMerge", pairwiseAutomata = "pairwiseConstraints";
	
	public static void addIfThenForMandatoryMerge(LearnerEvaluationConfiguration initialData, Collection<Label> dataOnUniqueTransitions)
	{
		if (initialData.ifthenSequences == null)
			initialData.ifthenSequences = new LinkedList<String>();
		
		int transitionNumber = 1;
		for(Label l:dataOnUniqueTransitions)
		{
			String lbl = l.toString(), mandatory = prefixOfMandatoryMergeTransition+"_"+transitionNumber+"_"+lbl;
			initialData.ifthenSequences.add(QSMTool.cmdIFTHENAUTOMATON + " Mandatory_"+transitionNumber+"_via_"+lbl+" A- !"+lbl+" || "+mandatory+" ->A-"+lbl+"->B - "+lbl+" ->B / B- !"+lbl+" || "+mandatory+" ->A / B == THEN == C / C-"+mandatory+"->D");
			++transitionNumber;
		}
	}
	
	/** Assuming we know which pairs of labels are not supposed to be merged, this function construts if-then constraints to prevent such mergers.
	 * This is used in the SmallvsHugeExperiment. 
	 */
	public static void addIfThenForPairwiseConstraints(LearnerEvaluationConfiguration initialData, Map<Label,Set<Label>> pairwiseConstraints)
	{
		if (initialData.ifthenSequences == null)
			initialData.ifthenSequences = new LinkedList<String>();
		
		for(Entry<Label,Set<Label>> entry:pairwiseConstraints.entrySet())
			if (!entry.getValue().isEmpty())
			{
				String lbl = entry.getKey().toString();
				StringBuffer rejects = new StringBuffer();
				for(Label l:entry.getValue())
				{
					rejects.append("/ C -");rejects.append(l);rejects.append("-#D");
				}
				initialData.ifthenSequences.add(QSMTool.cmdIFTHENAUTOMATON + " "+ pairwiseAutomata+"_"+lbl+" A- !"+lbl+" ->A-"+lbl+"->B - "+lbl+" ->B / B- !"+lbl+" ->A "+rejects.toString()+"/ B == THEN == C");
			}
	}
	
	/** Given a name of a vertex in a PTA, prints a path in it from the supplied initial state of length around 10 in the direction of the root state. Will not give expected results if the graph is not a PTA. 
	 * Primarily intended for troubleshooting.
	 * 
	 * @param graph graph in which to process a vertex
	 * @param vert vertex of interest.
	 */
	public static void printTraceLeadingTo(LearnerGraph graph, String vert)
	{
		LearnerGraphND ptaInverse = new LearnerGraphND(graph.config);
		AbstractPathRoutines.buildInverse(graph, LearnerGraphND.ignoreNone, ptaInverse);
		CmpVertex v=ptaInverse.findVertex(vert);
		for(int i=0;i<10;++i)
		{
			Map<Label,List<CmpVertex>> transitions = ptaInverse.transitionMatrix.get(v);
			if (transitions.size() > 1)
			{
				System.out.println(transitions);break;
			}
			List<CmpVertex> next = transitions.entrySet().iterator().next().getValue();
			if (next.size() > 1)
			{
				System.out.println(next);break;
			}
			CmpVertex nextState = next.iterator().next();
			System.out.println(v+"-"+transitions.entrySet().iterator().next().getKey()+"->"+nextState);
			v  = nextState;
		}
		
	}

	/** Whenever a transition is encountered with the supplied label, we replace it with a transition to a new state and record what the original state was. 
	 * The collection of pairs initial-original is then returned. 
	 * Important: this method modifies the supplied graph because it needs to add vertices. 
	 * For instance, A-a->B-b->C-a->D using label "a" as unique is turned into A-a->N1 / N2-a->B-b-C-a->N3 / N4-a->D (with a set of {N2,N4,A} returned). 
	 * The need to split states accounts for multiple transitions from the same state:
	 * Init ... A-a->B / A-b->B would turn into A-a->N1 / A-b->B / N2-a->B (with a set of {N2,Init} returned).
	 * The key advantage is that after merger of the returned vertices, the outcome is a tree, hence can be used as a normal PTA with a non-generalised merger.
	 * The problem with this is that with long sequences, EDSM has to be used with a threshold greater than 1. In fact, it is the case most of the time and hence this
	 * idea does not really encourage mergers as much as it should. I could possibly improve on this by adding a new element of an alphabet so that each transition
	 * with a unique label will be followed by a long sequence of such transitions; after the learning is complete, transitions with the extra element of an alphabet 
	 * could be removed. This idea is not currently pursued because the generalised learner seems as fast as PTA learner and just about as good.  
	 */
	public static List<CmpVertex> constructPairsToMergeWithOutgoing(LearnerGraph pta, Label uniqueFromInitial)
	{
		List<CmpVertex> sourceStates = new LinkedList<CmpVertex>(), statesOfInterest = new LinkedList<CmpVertex>();
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:pta.transitionMatrix.entrySet())
			if (entry.getKey().isAccept())
				for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
					if (transition.getValue().isAccept() && transition.getKey().equals(uniqueFromInitial))
					{
						sourceStates.add(entry.getKey());
					}

		for(CmpVertex vert:sourceStates)
			if (vert != pta.getInit())
			{
				CmpVertex newSource = AbstractLearnerGraph.generateNewCmpVertex(pta.nextID(true), pta.config);
				Map<Label,CmpVertex> row = pta.createNewRow();
				pta.transitionMatrix.put(newSource,row);row.put(uniqueFromInitial, pta.transitionMatrix.get(vert).get(uniqueFromInitial));
				statesOfInterest.add(newSource);
				
				CmpVertex tailState = AbstractLearnerGraph.generateNewCmpVertex(pta.nextID(true), pta.config);
				Map<Label,CmpVertex> tailRow = pta.createNewRow();
				pta.transitionMatrix.put(tailState,tailRow);pta.transitionMatrix.get(vert).put(uniqueFromInitial,tailState);
			}
			else
				statesOfInterest.add(vert);
		
		return statesOfInterest;
	}
	
	public static LearnerGraph mergeStatesForUnique(LearnerGraph pta, Label unique)
	{
		boolean buildAuxInfo = false;
		List<StatePair> pairs = new LinkedList<StatePair>();
		LearnerGraph sourcePta = new LearnerGraph(pta,pta.config);
		List<CmpVertex> whatToMerge = constructPairsToMergeWithOutgoing(sourcePta,unique);
		for(CmpVertex vert:whatToMerge)
			pairs.add(new StatePair(sourcePta.getInit(),vert));
		List<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new ArrayList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		
		if (sourcePta.pairscores.computePairCompatibilityScore_general(null, pairs, verticesToMerge,buildAuxInfo) < 0)
			throw new IllegalArgumentException("failed to merge states corresponding to a unique outgoing transition "+unique);
		LearnerGraph outcome = MergeStates.mergeCollectionOfVertices(sourcePta, null, verticesToMerge, null, buildAuxInfo);
		outcome.pathroutines.updateDepthLabelling();
		return outcome;
	}
	
	public static int makeEven(int number)
	{
		if (number % 2 == 0)
			return number;
		return number + 1;
	}
	
	/** Checks if there are transitions from the supplied pair that are worth merging, by checking for the existence of transitions with "mandatory merge" labels. */
	public static boolean checkForMerge(PairScore pair, LearnerGraph tentativeGraph)
	{
		Set<Label> labelsOfInterest = null;
		for(Entry<Label,CmpVertex> entry:tentativeGraph.transitionMatrix.get(pair.getQ()).entrySet()) // iterate over the smaller set
			if (entry.getKey().toString().startsWith(LearningSupportRoutines.prefixOfMandatoryMergeTransition))
			{
				if (labelsOfInterest == null) labelsOfInterest = new TreeSet<Label>();
				labelsOfInterest.add(entry.getKey());
			}
		if (labelsOfInterest == null)
			return false;// nothing of interest found.
		
		for(Entry<Label,CmpVertex> entry:tentativeGraph.transitionMatrix.get(pair.getR()).entrySet())
			if (entry.getKey().toString().startsWith(LearningSupportRoutines.prefixOfMandatoryMergeTransition) && labelsOfInterest.contains(entry.getKey()))
				return true;
		
		return false;
	}
	
	/** Given a reference graph, identifies pairs of labels that cannot be taken in a sequence, from any state. If a pair is possible from some states and not others, it will not be included. 
	 * This is subsequently used to construct if-then automata.
	 */
	public static Map<Label,Set<Label>> computeInfeasiblePairs(LearnerGraph tentativeGraph)
	{
		Map<Label,Set<Label>> labelToSet = new TreeMap<Label,Set<Label>>();
		Set<Label> alphabet = tentativeGraph.pathroutines.computeAlphabet();
		for(Label lbl:alphabet)
		{
			Set<Label> labels = new TreeSet<Label>();labels.addAll(alphabet);labelToSet.put(lbl,labels);
		}
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:tentativeGraph.transitionMatrix.entrySet())
			for(Entry<Label,CmpVertex> firstTransition:entry.getValue().entrySet())
				labelToSet.get(firstTransition.getKey()).removeAll(tentativeGraph.transitionMatrix.get(firstTransition.getValue()).keySet());
			
		return labelToSet;
	}
	
	public static long computeScoreBasedOnMandatoryMerge(PairScore pair,LearnerGraph tentativeGraph,Collection<Label> labelsLeadingToStatesToBeMerged,Collection<Label> labelsLeadingFromStatesToBeMerged)
	{
		List<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new ArrayList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsList = null;
		if (labelsLeadingToStatesToBeMerged.isEmpty() && labelsLeadingFromStatesToBeMerged.size() == 1)
			pairsList = buildVerticesToMergeForPathsFrom(tentativeGraph, labelsLeadingFromStatesToBeMerged.iterator().next());// this is the special case where labels to is empty and labels from is a singleton, as is the case for the small_vs_huge experiment.
		else
			pairsList = buildVerticesToMerge(tentativeGraph,labelsLeadingToStatesToBeMerged,labelsLeadingFromStatesToBeMerged);
		if (pairsList.isEmpty())
			return pair.getScore();
		
		if (
				!pair.getQ().isAccept() || !pair.getR().isAccept() || // if any is a negative, it can always be merged.
				tentativeGraph.pairscores.computePairCompatibilityScore_general(pair, pairsList, verticesToMerge, false) >= 0 // the pair does not contradict mandatory merge.
		)
		return pair.getScore();
		
		return -1;// cannot be merged
	}
	
	/** Returns a subset of pairs that are not in contradiction with mandatory merge constraints.
	 *  
	 * @param pairs pairs to merge
	 * @return the outcome of merging.
	 */
	public static List<PairScore> filterPairsBasedOnMandatoryMerge(List<PairScore> pairs, LearnerGraph tentativeGraph,Collection<Label> labelsLeadingToStatesToBeMerged,Collection<Label> labelsLeadingFromStatesToBeMerged)
	{
		List<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new ArrayList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsList = buildVerticesToMerge(tentativeGraph,labelsLeadingToStatesToBeMerged,labelsLeadingFromStatesToBeMerged);
		if (pairsList.isEmpty())
			return pairs;
		
		List<PairScore> outcome = new ArrayList<PairScore>();
		for(PairScore p:pairs)
			if (
					!p.getQ().isAccept() || !p.getR().isAccept() || // if any is a negative, it can always be merged.
					tentativeGraph.pairscores.computePairCompatibilityScore_general(p, pairsList, verticesToMerge, false) >= 0 // the pair does not contradict mandatory merge.
			)
			outcome.add(p);
		return outcome;
	}
	
	public static int signum(long value)
	{
		if (value>0)
			return 1;
		else
			if (value < 0)
				return -1;
		return 0;
	}
	
	
	/** Returns the best pair according to the ordering associated with individual pairs.
	 */
	public static PairScore pickPairQSMLike(Collection<PairScore> pairs)
	{
		ArrayList<PairScore> pairsSorted = new ArrayList<PairScore>(pairs);
		Collections.sort(pairsSorted, new Comparator<PairScore>(){

			@Override
			public int compare(PairScore o1, PairScore o2) {
				long scoreDiff = o1.getScore() - o2.getScore();
				if (scoreDiff != 0)
					return signum(scoreDiff);
				
				return o1.compareTo(o2);// other than by score, we sort using vertex IDs
			}});
		return pairsSorted.get(pairsSorted.size()-1);
	}

	public static long getThreadTime()
	{
		// thanks to http://stackoverflow.com/questions/14664897/measure-java-short-time-running-thread-execution-time
		return java.lang.management.ManagementFactory.getThreadMXBean().getThreadCpuTime(Thread.currentThread().getId());
	}

	/** Given a collection of labels, identifies states that transitions with those labels lead to. For each label, 
	 * there will be a set of states that is supposed to be merged. 
	 * It is important to point out that only positive states are taken into account, there are frequent 
	 * cases where a transition cannot be repeated, hence all transitions with this label will lead to the same state in the dataset,
	 * except for a transition from that very state that is often to be rejected.
	 *  
	 * @param tentativeGraph graph to process
	 * @param transitionsToTheSameState labels that are supposed to lead to the same state
	 * @param transitionsFromTheSameState labels that are supposed to uniquely identify a state
	 * @return a collection of pairs of state that are supposed to be merged.
	 */
	public static List<StatePair> buildVerticesToMerge(LearnerGraph tentativeGraph, Collection<Label> transitionsToTheSameState,Collection<Label> transitionsFromTheSameState)
	{
		List<StatePair> pairsList = new ArrayList<StatePair>();
		if (transitionsToTheSameState.isEmpty() && transitionsFromTheSameState.isEmpty() )
			return pairsList;
		
		Map<Label,Collection<CmpVertex>> labelToStates = 
				tentativeGraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY? new ArrayMapWithSearchPos<Label,Collection<CmpVertex>>() : new TreeMap<Label,Collection<CmpVertex>>();
		Map<Label,Collection<CmpVertex>> labelFromStates = 
				tentativeGraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY? new ArrayMapWithSearchPos<Label,Collection<CmpVertex>>() : new TreeMap<Label,Collection<CmpVertex>>();
					
		for(Label lbl:transitionsToTheSameState) labelToStates.put(lbl,new ArrayList<CmpVertex>());
		for(Label lbl:transitionsFromTheSameState) labelFromStates.put(lbl,new ArrayList<CmpVertex>());
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:tentativeGraph.transitionMatrix.entrySet())
			if (entry.getKey().isAccept())
				for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
				{
					Collection<CmpVertex> statesToMerge = labelToStates.get(transition.getKey());
					if (statesToMerge != null && transition.getValue().isAccept()) statesToMerge.add(transition.getValue());

					Collection<CmpVertex> sourceStatesToMerge = labelFromStates.get(transition.getKey());
					if (sourceStatesToMerge != null && transition.getValue().isAccept()) sourceStatesToMerge.add(entry.getKey());
				}
		
		for(Collection<CmpVertex> vertices:labelToStates.values())
		{
			CmpVertex prevVertex = null;
			for(CmpVertex v:vertices)
			{
				if (prevVertex != null)
					pairsList.add(new StatePair(prevVertex,v));
				prevVertex = v;
			}
		}
		for(Collection<CmpVertex> vertices:labelFromStates.values())
		{
			CmpVertex prevVertex = null;
			for(CmpVertex v:vertices)
			{
				if (prevVertex != null)
					pairsList.add(new StatePair(prevVertex,v));
				prevVertex = v;
			}
		}
		
		return pairsList;
	}

	/** A specialised version of {@link LearningSupportRoutines#buildVerticesToMerge(LearnerGraph, Collection, Collection)} 
	 * that only supports constructing pairs of states where one has a transition with a specific outgoing label. 
	 */
	public static List<StatePair> buildVerticesToMergeForPathsFrom(LearnerGraph tentativeGraph, Label transitionFromTheSameState)
	{
		List<StatePair> pairsList = new ArrayList<StatePair>();
		
		List<CmpVertex> statesToMerge = new ArrayList<CmpVertex>();
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:tentativeGraph.transitionMatrix.entrySet())
			if (entry.getKey().isAccept())
			{
				CmpVertex targetState = entry.getValue().get(transitionFromTheSameState);
				if (targetState != null && targetState.isAccept())
					statesToMerge.add(entry.getKey());
			}
		CmpVertex prevVertex = null;
		for(CmpVertex v:statesToMerge)
		{
			if (prevVertex != null)
				pairsList.add(new StatePair(prevVertex,v));
			prevVertex = v;
		}
		
		return pairsList;
	}

	
	
	
	/** Given a graph and a collection of pairs, this one uses the correct graph to split the collection into "correct" pairs that correspond to the same state in the correct graph and "wrong" pairs which states
	 * are not merged in the correct graph.
	 *  
	 * @param graph the graph to consider
	 * @param correctGraph states that should be merged
	 * @param pairs pairs to consider
	 * @param correct collection into which correct ones will be added. 
	 * @param wrong collection where the wrong ones will be added.
	 * @return the index of the first pair in the supplied list of pairs that is deemed correct.
	 */
	public static int SplitSetOfPairsIntoRightAndWrong(LearnerGraph graph, LearnerGraph correctGraph, Collection<PairScore> pairs, Collection<PairScore> correctPairs, Collection<PairScore> wrongPairs)
	{
		Set<CmpVertex> statesOfInterest = new HashSet<CmpVertex>();
		for(PairScore pair:pairs)
		{
			statesOfInterest.add(pair.getQ());statesOfInterest.add(pair.getR());
		}
		Map<CmpVertex,LinkedList<Label>> stateToPath = PairOfPaths.convertSetOfStatesToPaths(graph, statesOfInterest);

		
		int firstCorrectPair = JUConstants.intUNKNOWN, cnt=0;
		for(PairScore p:pairs)
		{
			CmpVertex blue = correctGraph.getVertex(stateToPath.get(p.getQ()));if (blue != null && !blue.isAccept()) blue = null;
			CmpVertex red = correctGraph.getVertex(stateToPath.get(p.getR()));if (red != null && !red.isAccept()) red = null;
			if (blue == red)
			{
				// it would be right to merge this pair.
				correctPairs.add(p);
				if (firstCorrectPair == JUConstants.intUNKNOWN)
					firstCorrectPair = cnt;
			}
			else
				// not appropriate to merge this pair.
				wrongPairs.add(p);
			
			++cnt;
		}
		return firstCorrectPair;
	}

	/** Records scores of pairs that are correctly classified and misclassified. Only considers pairs with scores below 150 in order to have a graph that will fit in a paper. */
	static void updateStatistics( Map<Long,TrueFalseCounter> pairQuality, LearnerGraph tentativeGraph, LearnerGraph referenceGraph, Collection<PairScore> pairsToConsider)
	{
		if (!pairsToConsider.isEmpty() && pairQuality != null)
		{
			List<PairScore> correctPairs = new ArrayList<PairScore>(pairsToConsider.size()), wrongPairs = new ArrayList<PairScore>(pairsToConsider.size());
			LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, referenceGraph, pairsToConsider, correctPairs, wrongPairs);

			for(PairScore pair:pairsToConsider)
			{
				if (pair.getQ().isAccept() && pair.getR().isAccept() && pair.getScore() < 150)
					synchronized(pairQuality)
					{
						TrueFalseCounter counter = pairQuality.get(pair.getScore());
						if (counter == null)
						{
							counter = new TrueFalseCounter();pairQuality.put(pair.getScore(),counter);
						}
						if (correctPairs.contains(pair))
							counter.trueCounter++;
						else
							counter.falseCounter++;
					}
			}
		}
	}
	
	public static void updateGraph(final RBoxPlot<Long> gr_PairQuality, Map<Long,TrueFalseCounter> pairQuality)
	{
		if (gr_PairQuality != null)
		{
			for(Entry<Long,TrueFalseCounter> entry:pairQuality.entrySet())
				gr_PairQuality.add(entry.getKey(), 100*entry.getValue().trueCounter/((double)entry.getValue().trueCounter+entry.getValue().falseCounter));
		}		
	}
	
 	/** Given a graph, removes all negatives and returns the outcome.
 	 * 
 	 * @param args
 	 * @throws Exception
 	 */
 	public static LearnerGraph removeAllNegatives(LearnerGraph initialPTA)
 	{
		LearnerGraph ptaTmp = new LearnerGraph(initialPTA,initialPTA.config);
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:initialPTA.transitionMatrix.entrySet())
		{
			if (!entry.getKey().isAccept())
				ptaTmp.transitionMatrix.remove(entry.getKey());
			else
				for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
					if (!transition.getValue().isAccept())
						ptaTmp.transitionMatrix.get(entry.getKey()).remove(transition.getKey());
		}
		return ptaTmp;
 	}

 	
 	/** Removes reject states if there are any accept-states.
 	 * This is used at the end of the learning process where all inputs that are not present are assumed to be rejected. 
 	 * If learning failed due to excessive number of red states, the outcome is a machine with a single reject-state. This is preserved by this routine.
 	 */
 	public static LearnerGraph removeRejects(LearnerGraph learningOutcome)
 	{
 		if (learningOutcome.getAcceptStateNumber() == 0)
 			return learningOutcome;
 		LearnerGraph actualAutomaton = new LearnerGraph(learningOutcome.config);
 		AbstractPathRoutines.removeRejectStates(learningOutcome,actualAutomaton);
 		return actualAutomaton;
 	}

 	/** Returns a string padded to the specified width with the supplied character.
 	 * 
 	 * @param whatToPad
 	 * @param ch character to pad with
 	 * @param length the length to pad to
 	 * @return
 	 */
 	public static String padString(String whatToPad, char ch, int length)
 	{
 		StringBuffer buf = new StringBuffer();
 		for(int i=0;i<length-whatToPad.length();++i)
 			buf.append(ch);
 		buf.append(whatToPad);
 		return buf.toString();
 	}

 	/** Removes spaces at the beginning and end of string. */
 	public static String removeSpaces(String text) 
 	{
 		return text.replaceAll("^\\s*", "").replaceAll("\\s*$", "");
 	}
 	
 	/** The purpose of the routine below is to change
 	 * "     : Intel(R) Xeon(R) CPU           X5670  @ 2.93GHz"
 	 * into 
 	 * "Intel(R) Xeon(R) CPU X5670 @ 2.93GHz"
 	 * 
 	 * @param text text to process
 	 * @return shortened version
 	 */
 	public static String removeColonsAndSpaces(String text)
 	{
 		String withoutSpacesAtTheFrontAndEnd =  removeSpaces(text);
 		
 		if (withoutSpacesAtTheFrontAndEnd.startsWith(":"))
 			withoutSpacesAtTheFrontAndEnd = removeSpaces(withoutSpacesAtTheFrontAndEnd.substring(1));
 		return withoutSpacesAtTheFrontAndEnd.replaceAll("\\s+", " ");
 	}
 	
 	public static String [] removeSpaces(String []text)
 	{
 		if (text.length == 0)
 			return text;
 		
 		String [] outcome = new String[text.length];for(int i=0;i<text.length;++i) outcome[i]=removeSpaces(text[i]);
 		return outcome;
 	}
 	
 	public static double getFreqCorrectionValue()
 	{
		String globalScaling = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING);
		if (globalScaling.isEmpty())
			throw new IllegalArgumentException("Experiment is using timeouts but SGE_EXECUTIONTIME_SCALING scaling is not set. Use SGE_ExperimentRunner.configureCPUFreqNormalisation()");

		return Double.parseDouble(globalScaling);
 	}
 	
 	/** In some cases, learning does not start from an initial vertex, therefore it is possible for an initial vertex in an outcome of inference to be incorrectly labelled.
 	 * A good example is where the initial vertex has an outgoing transition and some other vertex has a transition with the same label to the same state. In this case, it is not possible
 	 * to tell from the outcome of inference which of the two should be labelled initial so we match the whole of the initial PTA to all states in the inferred model in order to check which 
 	 * of the states looks like an initial one. EDSM-PTA scoring is used because a significant amount of matches are expected and the initial PTA is a tree. 
 	 * 
 	 * @param graph
 	 * @param whichInitialVertex
 	 * @return
 	 */
 	public static CmpVertex findBestMatchForInitialVertexInGraph(LearnerGraph graph, LearnerGraph ptaWithInitialState)
 	{
 		LearnerGraph grCombined = new LearnerGraph(graph,graph.config);
 		Collection<CmpVertex> verts = new LinkedList<CmpVertex>();verts.addAll(grCombined.transitionMatrix.keySet());
 		CmpVertex otherRoot = AbstractPathRoutines.addToGraph(grCombined, ptaWithInitialState, null);
 		CmpVertex currBest = null;
 		long bestScore = -1;
 		for(CmpVertex v:verts)
 		{
 			long score = grCombined.pairscores.computeStateScore(new StatePair(otherRoot, v));
 			if (score > bestScore)
 			{
 				bestScore = score;currBest = v;
 			}
 		}
 		return currBest;
 	}
	/** PTA is supposed to be built using walks over a reference graph. If these are random walks, it is possible that some transitions will not be covered. 
	 * For the learning purposes, this is significant because this could make some states more easily identifiable.
	 *  
	 * @param pta walks through the reference graph
	 * @param reference graph to trim 
	 * @return trimmed copy of the reference graph.
	 */
	public static Map<CmpVertex,Set<Label>> identifyUncoveredTransitions(LearnerGraph pta,LearnerGraph reference)
	{
		Map<CmpVertex,Set<Label>> outcome = new TreeMap<CmpVertex,Set<Label>>();
		StatePair reference_pta = new StatePair(reference.getInit(),pta.getInit());
		LinkedList<StatePair> pairsToExplore = new LinkedList<StatePair>();pairsToExplore.add(reference_pta);
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:reference.transitionMatrix.entrySet())
			outcome.put(entry.getKey(), new TreeSet<Label>(entry.getValue().keySet()));
		Set<CmpVertex> visitedInTree = new HashSet<CmpVertex>();
		while(!pairsToExplore.isEmpty())
		{
			reference_pta = pairsToExplore.pop();
			Map<Label,CmpVertex> transitions=pta.transitionMatrix.get(reference_pta.secondElem);
			outcome.get(reference_pta.firstElem).removeAll(transitions.keySet());
			for(Entry<Label,CmpVertex> target:transitions.entrySet())
				if (target.getValue().isAccept())
				{
					if (visitedInTree.contains(target.getValue()))
						throw new IllegalArgumentException("PTA is not a tree");
					visitedInTree.add(target.getValue());
					CmpVertex nextGraphState = reference.transitionMatrix.get(reference_pta.firstElem).get(target.getKey());
					if (nextGraphState == null)
						throw new IllegalArgumentException("coverage has more transitions than the original graph");
					pairsToExplore.add(new StatePair(nextGraphState, target.getValue()));
				}
		}
		
		return outcome;
	}
	
	/** Takes a supplied automaton and removes all transitions that have not been covered by a supplied PTA.
	 * 
	 * @param pta contains covered transitions
	 * @param reference all of the transitions.
	 * @return trimmed reference graph
	 */
	public static LearnerGraph trimUncoveredTransitions(LearnerGraph pta,LearnerGraph reference)
	{
		Configuration shallowCopy = reference.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph outcome = new LearnerGraph(shallowCopy);AbstractLearnerGraph.copyGraphs(reference, outcome);

		for(Entry<CmpVertex,Set<Label>> entry:identifyUncoveredTransitions(pta, reference).entrySet())
		{
			Map<Label,CmpVertex> map = outcome.transitionMatrix.get(entry.getKey());
			for(Label lbl:entry.getValue()) map.remove(lbl);
		}
		
		return outcome;
	}

	/** Uses sideways predictions in order to identify more states to be merged. */
	public static Collection<Set<CmpVertex>> mergeBasedOnInversePredictions(MarkovClassifierLG ptaClassifier,LearnerGraph referenceGraph,final Collection<List<Label>> pathsOfInterest)
	{/*
		Map<CmpVertex,LearnerGraph> pathsFromEachStateInGraph = PairQualityLearner.constructPathsFromEachState(pta,directionForwardOrInverse);
		ConsistencyChecker checker = new MarkovUniversalLearner.DifferentPredictionsInconsistency();
		List<StatePair> pairsList = PairQualityLearner.buildVerticesToMergeForPath(pathsFromEachStateInGraph,directionForwardOrInverse,pathsOfInterest);
		LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		int score = pta.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
		LearnerGraph merged = MergeStates.mergeCollectionOfVertices(pta, null, verticesToMerge);// after merging all paths of interest, we get this graph.
		final Collection<List<Label>> pathsToMerge2=identifyPathsToMerge(merged,referenceGraph,m,!directionForwardOrInverse);
		/*
		m.updateMarkov(merged,predictForwardOrSideways,false);// now we construct sideways learner ...
		m.constructMarkovTentative(graph,predictForwardOrSideways);// ... and use it to add more transitions.
		*/
		MarkovModel inverseModel = new MarkovModel(ptaClassifier.model.getChunkLen(),ptaClassifier.model.pathsOrSets,true,!ptaClassifier.model.directionForwardOrInverse,false);
		MarkovClassifierLG cl = new MarkovClassifierLG(inverseModel,ptaClassifier.graphD,null);cl.updateMarkov(false);
		Collection<Set<CmpVertex>> verticesToMergeUsingSideways=cl.buildVerticesToMergeForPaths(pathsOfInterest);
		return verticesToMergeUsingSideways;
	}
	
	public static LearnerGraph checkIfSingleStateLoopsCanBeFormed(MarkovClassifierLG ptaClassifier,LearnerGraph referenceGraph,final Collection<List<Label>> pathsOfInterest)
	{
		List<StatePair> pairsList = ptaClassifier.buildVerticesToMergeForPath(pathsOfInterest);
		LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		ptaClassifier.graphD.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge, false);
		LearnerGraph merged = MergeStates.mergeCollectionOfVertices(ptaClassifier.graphD, null, verticesToMerge, null, false);// after merging all paths of interest, we get this graph.
		ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistency();
		final long genScoreThreshold = 1;
		int nrOfMergers=0;
		List<StatePair> pairsToMerge = new LinkedList<StatePair>();
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:merged.transitionMatrix.entrySet())
			for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
				if (merged.transitionMatrix.get(transition.getValue()).containsKey(transition.getKey()))
				{// we have a potential loop
					PairScore p = new PairScore(entry.getKey(),transition.getValue(),0,0);
					ArrayList<PairScore> pairOfInterest = new ArrayList<PairScore>(1);pairOfInterest.add(p);
					List<PairScore> correctPairs = new ArrayList<PairScore>(1), wrongPairs = new ArrayList<PairScore>(1);
					LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(ptaClassifier.graphD, referenceGraph, pairOfInterest, correctPairs, wrongPairs);
					
					verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					long genScore = ptaClassifier.graphD.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge, false);
					LearnerGraph mergedForThisPair = MergeStates.mergeCollectionOfVertices(ptaClassifier.graphD, null, verticesToMerge, null, false);
					long value = MarkovClassifier.computeInconsistency(mergedForThisPair, null, ptaClassifier.model, checker, false);
					
					boolean decidedToMerge= (value == 0 && genScore >= genScoreThreshold);
					if (decidedToMerge)
					{
						pairsToMerge.add(p);
						++nrOfMergers;
					}
					
					if ( !wrongPairs.isEmpty() && decidedToMerge)
							//(wrongPairs.isEmpty() && value > 0 || genScore < genScoreThreshold) ||  (!wrongPairs.isEmpty() && value == 0 && genScore >= genScoreThreshold))
					{
						System.out.println( p.toString()+(wrongPairs.isEmpty()?"valid, ":"invalid:")+value+ "(score "+genScore+")");
						System.out.println( "R: " + ptaClassifier.graphD.transitionMatrix.get(p.getR())+" B: "+ptaClassifier.graphD.transitionMatrix.get(p.getQ()));
					}
				}
		System.out.println("mergers identified: "+nrOfMergers);
		/*
		long genScore = graph.pairscores.computePairCompatibilityScore_general(null, pairsToMerge, verticesToMerge);
		LearnerGraph mergedForAllPairs = MergeStates.mergeCollectionOfVertices(graph, null, verticesToMerge);*/
		return merged;
	}
	
	public void constructMapFromLabelsToStateGroups(LearnerGraph tentativeGraph, Collection<Label> transitionsFromTheSameState)
	{
		Map<Label,Collection<CmpVertex>> labelToStates = 
				tentativeGraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY? new ArrayMapWithSearchPos<Label,Collection<CmpVertex>>() : new TreeMap<Label,Collection<CmpVertex>>();
		Map<Label,Collection<CmpVertex>> labelFromStates = 
				tentativeGraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY? new ArrayMapWithSearchPos<Label,Collection<CmpVertex>>() : new TreeMap<Label,Collection<CmpVertex>>();
					
		for(Label lbl:transitionsFromTheSameState) labelFromStates.put(lbl,new ArrayList<CmpVertex>());
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:tentativeGraph.transitionMatrix.entrySet())
			if (entry.getKey().isAccept())
				for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
				{
					Collection<CmpVertex> statesToMerge = labelToStates.get(transition.getKey());
					if (statesToMerge != null && transition.getValue().isAccept()) statesToMerge.add(transition.getValue());

					Collection<CmpVertex> sourceStatesToMerge = labelFromStates.get(transition.getKey());
					if (sourceStatesToMerge != null && transition.getValue().isAccept()) sourceStatesToMerge.add(entry.getKey());
				}
		
	}
		
	protected static boolean checkSeqIsUnique(LearnerGraph referenceGraph, List<Label> seq)
	{
		boolean outcome = false;
		int count=0;
		for(CmpVertex v:referenceGraph.transitionMatrix.keySet())
		{
			if (referenceGraph.getVertex(v,seq) != null)
			{
				++count;
				if (count > 1)
					break;
			}
		}
		if (count == 1)
			outcome = true;
		
		return outcome;
	}
	
	/** Given a graph and a collection of sequences, extracts a set of states from the graph where each state is uniquely identified by one of the sequences
	 * 
	 * @param referenceGraph graph of interest
	 * @param collectionOfUniqueSeq sequences to check from all states of this graph
	 * @param correctlyIdentified collection of the vertices of the graph that are uniquely identified using the supplied sequences.
	 * @param incorrectSequences a subset of sequences passed in <i>collectionOfUniqueSeq</i> that do not identify states uniquely.
	 */
	public static void statesIdentifiedUsingUniques(LearnerGraph referenceGraph, Collection<List<Label>> collectionOfUniqueSeq, Set<CmpVertex> correctlyIdentified,Collection<List<Label>> incorrectSequences)
	{
		for(List<Label> seq:collectionOfUniqueSeq)
		{
			int count=0;CmpVertex unique = null;
			for(CmpVertex v:referenceGraph.transitionMatrix.keySet())
			{
				if (referenceGraph.getVertex(v,seq) != null)
				{
					++count;unique=v;
					if (count > 1)
						break;
				}
			}
			if (count == 1)
			{
				if (correctlyIdentified != null) correctlyIdentified.add(unique);
			}
			else if (count > 1)
			{
				if (incorrectSequences != null) incorrectSequences.add(seq);
			}
		}
	}
	
	/** Given a graph and a collection of sequences, extracts a set of states from the graph where each state accepts one of the supplied sequences. Where multiple states accept one of the sequences,
	 * all of those states are returned. <b>There is no expectation of uniqueness</b>.
	 * 
	 * @param referenceGraph graph of interest
	 * @param collectionOfSeq sequences to check from all states of this graph
	 * @return collection of the vertices of the graph that are uniquely identified using the supplied sequences.
	 */
	public static Collection<CmpVertex> statesIdentifiedUsingSequences(LearnerGraph referenceGraph, Collection<List<Label>> collectionOfSeq)
	{
		Set<CmpVertex> statesUniquelyIdentified = new TreeSet<CmpVertex>();
		for(List<Label> seq:collectionOfSeq)
		{
			for(CmpVertex v:referenceGraph.transitionMatrix.keySet())
				if (referenceGraph.getVertex(v,seq) != null)
					statesUniquelyIdentified.add(v);
		}
		return statesUniquelyIdentified;
	}
	
	public static CmpVertex checkSeqUniqueOutgoing(LearnerGraph referenceGraph, List<Label> seq)
	{
		CmpVertex vertexOfInterest = null;
		for(CmpVertex v:referenceGraph.transitionMatrix.keySet())
		{
			CmpVertex currTarget = referenceGraph.getVertex(v,seq);
			if (currTarget != null)
			{
				if (vertexOfInterest != null)
					return null;

				vertexOfInterest = v;
			}
		}
		
		return vertexOfInterest;
	}
	
	
	/** Identifies states <i>steps</i> away from the root state and labels the first of them red and others blue. The colour of all other states is removed. When the learner
	 * starts, the exploration begins not from the root state as per blue fringe but from the marked red state. 
	 * The aim is to permit Markov predictive power to be used on arbitrary states, 
	 * without this we cannot predict anything in the vicinity of the root state which has no incoming transitions unless pre-merge is used. 
	 */ 
	public static void labelStatesAwayFromRoot(LearnerGraph graph, int steps)
	{
		graph.clearColours();graph.getInit().setColour(null);
		
		Set<CmpVertex> visited = new HashSet<CmpVertex>();
		Collection<CmpVertex> frontLine = new LinkedList<CmpVertex>(), nextLine = new LinkedList<CmpVertex>(), previousFrontLine = null;
		
		frontLine.add(graph.getInit());visited.add(graph.getInit());
		for(int line=0;line < steps;++line)
		{
			for(CmpVertex vert:frontLine)
				for(CmpVertex next:graph.transitionMatrix.get(vert).values())
					if (!visited.contains(next))
					{
						nextLine.add(next);visited.add(next);
					}
			
			previousFrontLine = frontLine;frontLine = nextLine;nextLine=new LinkedList<CmpVertex>();
		}
		for(CmpVertex blue:frontLine) blue.setColour(JUConstants.BLUE);
		if (frontLine.isEmpty())
			throw new IllegalArgumentException("no states beyond the steps");
		graph.additionalExplorationRoot = previousFrontLine;
		frontLine.iterator().next().setColour(JUConstants.RED);
	}


	
	public static void showInconsistenciesForDifferentMergers(LearnerGraph referenceGraph,MarkovClassifierLG ptaClassifier, Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA)
	{
		LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		int genScore = ptaClassifier.graphD.pairscores.computePairCompatibilityScore_general(null, constructPairsToMergeBasedOnSetsToMerge(ptaClassifier.graphD.transitionMatrix.keySet(),verticesToMergeBasedOnInitialPTA), verticesToMerge, false);
		LearnerGraph graph = MergeStates.mergeCollectionOfVertices(ptaClassifier.graphD, null, verticesToMerge, null, false);
		
		Set<CmpVertex> tr=graph.transform.trimGraph(10, graph.getInit()).transitionMatrix.keySet();
		ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistency();

		//WaveBlueFringe.constructPairsToMergeBasedOnSetsToMerge(graph.transitionMatrix.keySet(),verticesToMergeBasedOnInitialPTA);		
		for(CmpVertex v0:tr)
			for(CmpVertex v1:tr)
				if (v0 != v1)
				{
					PairScore p = new PairScore(v0,v1,0,0);
					ArrayList<PairScore> pairOfInterest = new ArrayList<PairScore>(1);pairOfInterest.add(p);
					List<PairScore> correctPairs = new ArrayList<PairScore>(1), wrongPairs = new ArrayList<PairScore>(1);
					LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(graph, referenceGraph, pairOfInterest, correctPairs, wrongPairs);
					
					verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					genScore = graph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge, false);
					LearnerGraph merged = MergeStates.mergeCollectionOfVertices(graph, null, verticesToMerge, null, false);
					long value = MarkovClassifier.computeInconsistency(merged, null, ptaClassifier.model, checker, false);
					if ( (wrongPairs.isEmpty() && value > 0) ||  (!wrongPairs.isEmpty() && value == 0))
					{
						System.out.println( p.toString()+(wrongPairs.isEmpty()?"valid, ":"invalid:")+value+ "(score "+genScore+")");
						System.out.println( "R: " + graph.transitionMatrix.get(p.getR())+" B: "+graph.transitionMatrix.get(p.getQ()));
					}
				}
		
		System.out.println("finished dumping inconsistencies");
	}


	/** Given a collection of collections of paths, such that target state of each sequence in the inner collection is to be merged, computes a collection of pairs of states that are to be merged. */ 
	public static List<StatePair> getVerticesToMergeFor(LearnerGraph graph,List<List<List<Label>>> pathsToMerge)
	{
		List<StatePair> listOfPairs = new LinkedList<StatePair>();
		for(List<List<Label>> lotOfPaths:pathsToMerge)
		{
			CmpVertex firstVertex = graph.getVertex(lotOfPaths.get(0));
			for(List<Label> seq:lotOfPaths)
				listOfPairs.add(new StatePair(firstVertex,graph.getVertex(seq)));
		}
		return listOfPairs;
	}
	
	/** Given a collection of vertices to merge, constructs a collection of pairs to merge. */
	public static Collection<StatePair> constructPairsToMergeBasedOnSetsToMerge(Set<CmpVertex> validStates, Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA)
	{
		List<StatePair> pairsList = new LinkedList<StatePair>();
		for(Set<CmpVertex> groupOfStates:verticesToMergeBasedOnInitialPTA)
		{
			Set<CmpVertex> validStatesInGroup = new TreeSet<CmpVertex>();validStatesInGroup.addAll(groupOfStates);validStatesInGroup.retainAll(validStates);
			if (validStatesInGroup.size() > 1)
			{
				CmpVertex v0=validStatesInGroup.iterator().next();
				for(CmpVertex v:validStatesInGroup)
				{
					if (v != v0)
						pairsList.add(new StatePair(v0,v));
					v0=v;
				}
			}
		}
		return pairsList;
	}
}
