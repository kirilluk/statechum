package statechum.analysis.learning;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.MarkovModel.MarkovMatrixEngine;
import statechum.analysis.learning.MarkovModel.MarkovOutcome;
import statechum.analysis.learning.MarkovModel.UpdatablePairInteger;
import statechum.analysis.learning.MarkovModel.MarkovMatrixEngine.PredictionForSequence;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.model.testset.PTAExploration;
import statechum.model.testset.PTASequenceEngine;

public class MarkovClassifierLG extends MarkovClassifier<CmpVertex,LearnerGraphCachedData> 
{
	public final LearnerGraph graphD;
	
	public MarkovClassifierLG(MarkovModel m, LearnerGraph gr, LearnerGraphND grInverse) 
	{
		super(m, gr, grInverse);graphD = gr;
	}

	/** Given a graph, it uses the supplied collection of labels in order to identify states to merge, constructs a merge and counts the number of inconsistencies between the Markov-predicted vertices and the actual ones.
	 * The large number of arguments reflect the extent to which this process can be customised. 
	 * 
	 * @param paths paths to use.
	 * @param checker Consistency checker to use for predictions, usually based on a static method from {@link MarkovOutcome}.
	 * @return how inconsistent predictions are compared to the actual graph. Always non-negative.
	 */
	public long computeInconsistencyForMergingPaths(Collection<List<Label>> paths,  ConsistencyChecker checker)
	{
		long outcome = 0;
		
		LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsList = buildVerticesToMergeForPath(paths);
		if (!pairsList.isEmpty())
		{
			int score = graphD.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge, false);
			if (score < 0)
				outcome = dREJECT;
			else
			{
				LearnerGraph merged = MergeStates.mergeCollectionOfVertices(graphD, null, verticesToMerge, null, false);
				outcome = computeInconsistency(merged,null, model,checker,false);
			}
		}
		
		return outcome;
	}

	/** Given a graph, it uses the supplied collection of labels in order to identify states to merge, constructs a merge and counts the number of inconsistencies between the Markov-predicted vertices and the actual ones.
	 * The large number of arguments reflect the extent to which this process can be customised. 
	 * <p>
	 * This is a special version of {@link #computeInconsistencyForMergingPath(Collection)} for a single path.
	 * 
	 * @param path a single path to use.
	 * @param checker Consistency checker to use for predictions, usually based on a static method from {@link MarkovOutcome}.
	 * @return how inconsistent predictions are compared to the actual graph. Always non-negative.
	 */
	public long computeInconsistencyForMergingPath(List<Label> path, ConsistencyChecker checker)
	{
		Collection<List<Label>> paths=new LinkedList<List<Label>>();paths.add(path);
		return computeInconsistencyForMergingPaths(paths,checker);
	}
	
	/** This function is predicts transitions from each state and then adds them to the supplied graph. 
	 * Uses floating-point calculations as to whether to add or not.
	 * <br/>
	 * Can only be used to predict transition forward since inverse expects disconnected states to be added and QSM is not expected to deal with such states. We could certainly attempt to merge them somewhere but this seems to make little sense.
	 *  
	 * @param predictForwardOrSideways whether to make predictions forward or sideways
	 * @param highThreshold if the predicted probability of a transition is above this value, it seems plausible to add this transition.
	 * @param lowThreshold if the predicted probability of a transition is below this value, it is believed that the impact of this transition is insignificant.
	 */
	@SuppressWarnings("unchecked")
	public LearnerGraph Markov_tentative(double highThreshold, double lowThreshold)
	{
		if (!model.directionForwardOrInverse)
			throw new IllegalArgumentException("predictions are only supported in the forward direction, not inverse");
		
		class UpdatablePairDouble 
		{
			public double firstElem, secondElem;
			public UpdatablePairDouble(double a, double b) {
				firstElem=a;secondElem=b;
			}
			
			@Override
			public String toString()
			{
				return "(pos: "+firstElem+", neg: "+secondElem+")";
			}
		}
		
		/** Maps states to a function associating labels to a sum of probabilities from a Markov matrix. Such a sum is supposed to predict the likelyhood of a transition with this label from every state. */
		Map<CmpVertex,Map<Label,UpdatablePairInteger>> state_outgoing_occurence=new HashMap<CmpVertex,Map<Label,UpdatablePairInteger>>();
		/** Maps states to a function associating labels to a probability of a transition with the label of interest from a state of interest. Computed from {@link MarkovUniversalLearner#state_outgoing_occurence}. */
		Map<CmpVertex,Map<Label,UpdatablePairDouble>> state_outgoing=new HashMap<CmpVertex,Map<Label,UpdatablePairDouble>>();

		final Configuration shallowCopy = graphD.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph outcome = new LearnerGraph(shallowCopy);
		LearnerGraph.copyGraphs(graphD, outcome);
		final Set<Label> allElementsOfAlphabet = graphD.learnerCache.getAlphabet(); 
		// mapping map to store all paths leave each state in different length
    	for(CmpVertex vert:graphD.transitionMatrix.keySet())
    	{
    		if(vert.isAccept() )
            {
		        final Map<Label,UpdatablePairDouble> outgoing_labels_probabilities=new HashMap<Label,UpdatablePairDouble>();
		        final Map<Label,UpdatablePairInteger> outgoing_labels_occurrences=new HashMap<Label,UpdatablePairInteger>();
		        final UpdatablePairInteger sum=new UpdatablePairInteger(0,0);
		        WalkThroughAllPathsOfSpecificLength(graphToUseForPrediction,vert,model.getPredictionLen(),new ForEachCollectionOfPaths() 
		        {
					@Override
					public void handlePath(List<Label> pathToNewState) 
					{
	    				List<Label> partOfTraceUsedInMarkovPredictions=new ArrayList<Label>(pathToNewState.size());partOfTraceUsedInMarkovPredictions.addAll(pathToNewState);
    					if (predictionGraphInverted)
    						Collections.reverse(partOfTraceUsedInMarkovPredictions);
    					Map<Label,PTASequenceEngine.Node> lastElementToPrediction = model.markovMatrix.getMapFromLabelsToPredictions(partOfTraceUsedInMarkovPredictions);
	    				for(Label label:allElementsOfAlphabet)
	    				{
	    					PredictionForSequence prediction = MarkovMatrixEngine.getPredictionIfExists(lastElementToPrediction,label);

	    					UpdatablePairInteger occurrence_of_label_predicted_form_Markov=prediction == null?null:prediction.occurrence;

	    					if(outgoing_labels_occurrences.containsKey(label))
	    					{
	    						UpdatablePairInteger labels_occurence= outgoing_labels_occurrences.get(label);
	    						sum.add(labels_occurence);
	    						labels_occurence.add(occurrence_of_label_predicted_form_Markov);											 
	    					}
	    					else
	    					{
	    						outgoing_labels_occurrences.put(label, occurrence_of_label_predicted_form_Markov);
	    						sum.add(occurrence_of_label_predicted_form_Markov);
	    					}	
	    				}
					}
				});

			    for(Entry<Label,UpdatablePairInteger> labelValue:outgoing_labels_occurrences.entrySet())
			    	outgoing_labels_probabilities.put(labelValue.getKey(),new UpdatablePairDouble(labelValue.getValue().firstElem/sum.firstElem, labelValue.getValue().secondElem/sum.secondElem));

			    state_outgoing.put(vert, outgoing_labels_probabilities);
			    state_outgoing_occurence.put(vert, outgoing_labels_occurrences);
			}

    		// in this part the tree is extended depending on what we learnt from traces.
    	 	for(Entry<CmpVertex, Map<Label, UpdatablePairDouble>> outgoing:state_outgoing.entrySet())
    	 	{
    	 		CmpVertex currrent_state_to_explore_outgoing= outgoing.getKey();
    	 		Map<Label, UpdatablePairDouble> list_of_outgoing = outgoing.getValue();
    	 		for(Entry<Label, UpdatablePairDouble> out:list_of_outgoing.entrySet())
    	 		{
    	 			Map<Label, CmpVertex> already_outgoing = graphD.transitionMatrix.get(currrent_state_to_explore_outgoing);
    	 			assert already_outgoing!=null : "state "+currrent_state_to_explore_outgoing+" is not mentioned in the transition diagram";

    	 			if(!already_outgoing.containsKey(out.getKey()))
    	 			{  	   						
    	 				if(out.getValue().firstElem >  highThreshold && out.getValue().secondElem <= lowThreshold && currrent_state_to_explore_outgoing.isAccept()==true)
    	 				{  
    	 					if(!outcome.transitionMatrix.get(currrent_state_to_explore_outgoing).keySet().contains(out.getKey()))
    	 					{
    	 						extendWithLabel(outcome,currrent_state_to_explore_outgoing, true, out.getKey());
    	 					}     					      
    	 				} 

    	 				if(out.getValue().secondElem >  highThreshold && out.getValue().firstElem <= lowThreshold && currrent_state_to_explore_outgoing.isAccept()==true)
    	 				{  
    	 					if(!outcome.transitionMatrix.get(currrent_state_to_explore_outgoing).keySet().contains(out.getKey()))
    	 					{
    	 						extendWithLabel(outcome,currrent_state_to_explore_outgoing, false, out.getKey());
    	 					}     					      
    	 				} 	 					
    	 			}					   
    	 		}          	       	      
    	 	}
    	}
    	
    	return outcome;
	}

	/** Extends the supplied graph with transitions in the forward direction.
	 * 
	 * @param what
	 * @param prevState
	 * @param isAccept
	 * @param input
	 */
	public void extendWithLabel(LearnerGraph what, CmpVertex prevState, boolean isAccept, Label input)
	{
		CmpVertex newVertex = AbstractLearnerGraph.generateNewCmpVertex(what.nextID(isAccept),what.config);
		assert !what.transitionMatrix.containsKey(newVertex);
		newVertex.setAccept(isAccept);
		what.transitionMatrix.put(newVertex, what.createNewRow());
		what.addTransition(what.transitionMatrix.get(prevState),input,newVertex);
	}


	/** This function is predicts transitions from each state and then adds them to the supplied graph. Uses predictions from the model to add transitions without a second thought.
	 * <br/>
	 * Can only be used to predict transition forward since inverse expects disconnected states to be added and QSM is not expected to deal with such states. We could certainly attempt to merge them somewhere but this seems to make little sense.
	 * <ul>
	 * <li>
	 * Where <i>predictForward</i> is true, we are predicting transitions based on paths leading to the state of interest. Parameter <i>Inverse_Graph</i> should be the (non-deterministic) inverse of <i>graph</i>.
	 * </li>
	 * <li> 
	 * Where <i>predictForward</i> is false, we are predicting transitions based on paths leading from the state of interest (sideways predictions). Parameter <i>Inverse_Graph</i> should be the same as <i>graph</i>.
	 * </li>
	 * </ul>
	 * <em>directionForwardOrInverse</em> determines whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and all its target states could be merged together.
	 * @return the graph with predicted transitions added to it.
	 */
	public LearnerGraph constructMarkovTentative()
	{
		if (!model.directionForwardOrInverse)
			throw new IllegalArgumentException("predictions are only supported in the forward direction, not inverse");

		/** Maps states to a function associating labels to a probability of a transition with the label of interest from a state of interest. Computed from {@link MarkovUniversalLearner#state_outgoing_occurence}. */
		Map<CmpVertex,Map<Label,MarkovOutcome>> state_outgoing=predictTransitions();

		final Configuration shallowCopy = graphD.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph graphWithPredictedTransitions = new LearnerGraph(shallowCopy);
		LearnerGraph.copyGraphs(graphD, graphWithPredictedTransitions);

		// in this part the tree is extended depend on their outgoing transition probabilities
	 	for(Entry<CmpVertex, Map<Label, MarkovOutcome>> outgoing:state_outgoing.entrySet())
	 	{
	 		CmpVertex currrent_state_to_explore_outgoing= outgoing.getKey();
	 		Map<Label, MarkovOutcome> list_of_outgoing = outgoing.getValue();
	 		for(Entry<Label, MarkovOutcome> out:list_of_outgoing.entrySet())
	 		{
	 			Map<Label, CmpVertex> already_outgoing = graphD.transitionMatrix.get(currrent_state_to_explore_outgoing);
	 			assert already_outgoing!=null : "state "+currrent_state_to_explore_outgoing+" is not mentioned in the transition diagram";

	 			if(!already_outgoing.containsKey(out.getKey()) && out.getValue() != MarkovOutcome.failure)
	 			{  	   
 					if(!graphWithPredictedTransitions.transitionMatrix.get(currrent_state_to_explore_outgoing).keySet().contains(out.getKey()))
 						extendWithLabel(graphWithPredictedTransitions,currrent_state_to_explore_outgoing, out.getValue().isPositive, out.getKey());
	 			}					   
	 		}          	       	      
	 	}

      return graphWithPredictedTransitions;
	}

	/**
	 * Uses a supplied consistency checker to find paths that uniquely identify states. The supplied consistency checker is used to verify consistency after states deemed identical are merged.
	 * @param checker Consistency checker to use for predictions, usually based on a static method from {@link MarkovOutcome}.
	 * @param useAverageOfMax if true, takes an average, divides by divisor and uses this value; for false, uses a maximal value and divides that.
	 * @param divisor permits one to select a subset of paths that are not often used.
	 * @param WLength The length of sequences to check from every state. The usual starting value is 1 which is a guess, based the observation of behaviour of graphs with large alphabet size. We have no way to tell whether paths of this length are going to separate states or not.
	 * @return paths to uniquely identify states.
	 */
	public List<List<Label>> identifyPathsToMerge(final ConsistencyChecker checker, boolean useAverageOfMax,int divisor, final int WLength)
	{
		if (model.getChunkLen() < 2)
			throw new IllegalArgumentException("not enough data for a first-order Markov model");
		
		updateMarkov(false);
		long scoreAfterBigMerge=-1;
		List<List<Label>> whatToMerge = Collections.emptyList();

		final AtomicLong maxCount = new AtomicLong(0), sumInPta = new AtomicLong(0), pathsExplored = new AtomicLong(0);
		PTAExploration<Boolean> exploration = new PTAExploration<Boolean>(model.markovMatrix) {
			@Override
			public Boolean newUserObject() {
				return null;
			}

			@Override
			public void nodeEntered(PTAExplorationNode currentNode,	LinkedList<PTAExplorationNode> pathToInit) 
			{
				PredictionForSequence prediction = (PredictionForSequence)currentNode.getState();
				if (pathToInit.size() == WLength && prediction.prediction == MarkovOutcome.positive)
				{
					long countInPTA=prediction.occurrence.firstElem;
					if (countInPTA > maxCount.longValue())
						maxCount.set(countInPTA);
					sumInPta.addAndGet(countInPTA);pathsExplored.incrementAndGet();
				}
			}

			@Override
			public void leafEntered(PTAExplorationNode currentNode,	LinkedList<PTAExplorationNode> pathToInit) 
			{
				nodeEntered(currentNode, pathToInit);
			}

			@Override
			public void nodeLeft(@SuppressWarnings("unused") PTAExplorationNode currentNode, @SuppressWarnings("unused") LinkedList<PTAExplorationNode> pathToInit) 
			{
				// nothing to do here.
			}

		};
		exploration.walkThroughAllPaths();
		// paths that are common are likely to be present from a number of different states and as such not very good for discriminating between them.
		final long valueAverage = pathsExplored.get() > 0?sumInPta.get()/pathsExplored.get():0;
		final long countForInfrequentPaths = useAverageOfMax?valueAverage/divisor:maxCount.get()/divisor;
		final Map<Long,List<List<Label>>> thresholdToInconsistency = new TreeMap<Long,List<List<Label>>>();
		exploration = new PTAExploration<Boolean>(model.markovMatrix) {
			@Override
			public Boolean newUserObject() {
				return null;
			}

			@Override
			public void nodeEntered(PTAExplorationNode currentNode,	LinkedList<PTAExplorationNode> pathToInit) 
			{
				PredictionForSequence prediction = (PredictionForSequence)currentNode.getState();
				if (pathToInit.size() == WLength && prediction.prediction == MarkovOutcome.positive)
				{
					long countInPTA=prediction.occurrence.firstElem;
					if (countInPTA < countForInfrequentPaths) 
					{
						LinkedList<Label> path = new LinkedList<Label>();for(PTAExplorationNode elem:pathToInit) path.addFirst(elem.getInput());
						long value = computeInconsistencyForMergingPath(path, checker);
						if (value >= 0)
						{
							List<List<Label>> pathsForThisInconsistency = thresholdToInconsistency.get(value);
							if (pathsForThisInconsistency == null)
							{
								pathsForThisInconsistency = new LinkedList<List<Label>>();thresholdToInconsistency.put(value, pathsForThisInconsistency); 
							}
							pathsForThisInconsistency.add(path);
						}
					}
				}
			}

			@Override
			public void leafEntered(PTAExplorationNode currentNode,	LinkedList<PTAExplorationNode> pathToInit) 
			{
				nodeEntered(currentNode, pathToInit);
			}

			@Override
			public void nodeLeft(@SuppressWarnings("unused") PTAExplorationNode currentNode, @SuppressWarnings("unused")	LinkedList<PTAExplorationNode> pathToInit) 
			{
				// nothing to do here.
			}

		};
		exploration.walkThroughAllPaths();
		
		if (!thresholdToInconsistency.isEmpty())
		{// Now evaluate the most consistent element in the map and merge all paths associated with it. 
		 // In reality, there would be many other elements that might be feasible, however we'd like 
		 // not to get this one wrong and the way to do it is to be cautious. 
		 // At present, we seem to be getting around 10% of these wrong.
			whatToMerge = thresholdToInconsistency.entrySet().iterator().next().getValue();
			List<StatePair> pairsList = buildVerticesToMergeForPath(whatToMerge);
			scoreAfterBigMerge = dREJECT;
			LearnerGraph merged = null;
			if (//thresholdToInconsistency.entrySet().iterator().next().getKey() == 0 && 
					!pairsList.isEmpty())
			{
				LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
				int score = graphD.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge, false);
				if (score < 0)
					scoreAfterBigMerge = dREJECT;
				else
				{
					merged = MergeStates.mergeCollectionOfVertices(graphD, null, verticesToMerge, null, false);
					scoreAfterBigMerge = computeInconsistency(merged, null, model, checker,false);
				}
			}
			if (scoreAfterBigMerge < 0)
				whatToMerge = Collections.emptyList();
		}
		return whatToMerge;
	}

	/** Given the collection of paths and a way to tell which states to merge, computes which states to merge and uses the reference graph to check for validity. Returns true if a merged graph would be valid.
	 * 
	 * @param trimmedReference reference graph
	 * @param graph graph in which to identify states to merge
	 * @param whatToMerge paths to check. 
	 * @return true if a merge will be valid and false otherwise.
	 */
	public static boolean checkMergeValidity(LearnerGraph trimmedReference, LearnerGraph graph, MarkovModel model, Collection<List<Label>> whatToMerge)
	{
		Map<CmpVertex,LinkedList<Label>> graphToPath=PairOfPaths.convertSetOfStatesToPaths(graph,graph.transitionMatrix.keySet());
		assert graphToPath != null;
		boolean valid = true;
		MarkovClassifierLG cl = new MarkovClassifierLG(model,graph,null);
		for(Set<CmpVertex> set:cl.buildVerticesToMergeForPaths(whatToMerge))
		{
			CmpVertex expected = trimmedReference.getVertex(graphToPath.get(set.iterator().next()));
			for(CmpVertex v:set)
				if (trimmedReference.getVertex(graphToPath.get(v)) != expected)
				{
					valid = false;//System.out.println("INVALID MERGE: "+set);
					break;
				}
			
			if (!valid)
				break;
		}
		return valid;
	}
	
}
