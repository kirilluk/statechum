/* Copyright (c) 2013 The University of Sheffield.
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
package statechum.analysis.learning;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.Helper;
import statechum.Label;
import statechum.Trace;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.MarkovModel.DifferentPredictionsInconsistencyNoBlacklisting;
import statechum.analysis.learning.MarkovModel.FrontLineElem;
import statechum.analysis.learning.MarkovModel.MarkovOutcome;
import statechum.analysis.learning.MarkovModel.UpdatablePairInteger;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;

/** An instance of this class holds all the necessary parameters in order to make it possible to predict transitions and/or check inconsistencies using a Markov model. Depending on the kind of model passed to it, 
 * it will be making appropriate predictions.
 */
public class MarkovClassifier 
{
	/** Markov model being used in this classifier. Also determines the direction of prediction and whether forward or sideways. */
	protected final MarkovModel model;
	/** The graph in which we are making predictions.*/
	protected final LearnerGraph graph;

	/** Contains paths to be supplied to Markov for making predictions. This is computed using {@link PairQualityLearner#constructPathsFromEachState(LearnerGraph, boolean)} and is expected to be cached between multiple invocations of method in an instance of this class. */
	Map<CmpVertex,LearnerGraph> graphsToUseForPrediction;  
	
	/** Contains paths to be used for consistency checking. This is computed using {@link PairQualityLearner#constructPathsFromEachState(LearnerGraph, boolean)} and is expected to be cached between multiple invocations of method in an instance of this class. */
	Map<CmpVertex,LearnerGraph> graphsToCheckForConsistency;  
	
	/** Consistency checker to use for predictions, usually based on a static method from {@link MarkovOutcome}. */
	final ConsistencyChecker checker;
	
	/** Given a graph, constructs a set of deterministic graphs for each vertex of it. When used to invert a graph graph, this makes it possible to quickly check which paths lead to each of the states.
	 * The specific method is not using an inverse because we'd like to be able to use the same method for both forward and inverse path checking. 
	 *   
	 * @param graph graph to consider
	 * @param directionForward if true, will use the supplied graph unchanged; if false, will invert the supplied graph
	 * @return an association between vertices and deterministic graphs. Where built as an inverse of the original graph at each of its vertices, each graph can be large but this does not have to be so
	 * even in the worst case since we can trim all graphs at the number of transitions corresponding to the length of the paths at the point of construction. 
	 */
	public static Map<CmpVertex,LearnerGraph> constructPathsFromEachState(LearnerGraph graph, boolean directionForward)
	{
		LearnerGraphND forwardOrInverseGraph = null;
		
		if (directionForward)
		{
			forwardOrInverseGraph = new LearnerGraphND(graph,graph.config);
		}
		else
		{
			Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
			forwardOrInverseGraph = new LearnerGraphND(shallowCopy);
			AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,forwardOrInverseGraph);  // do the inverse to the tentative graph
		}

		Map<CmpVertex,LearnerGraph> outcome = new TreeMap<CmpVertex,LearnerGraph>();
		for(CmpVertex v:graph.transitionMatrix.keySet())
			try {
				outcome.put(v,forwardOrInverseGraph.pathroutines.buildDeterministicGraph(v));
			} catch (IncompatibleStatesException e) {
				Helper.throwUnchecked("inverse was impossible to build - this should not happen", e);
			}
		return outcome;
	}

	/** Decisions to invert or not are based on the following:
	 * <table>
	 * <tr><td>predictForwardOrSideways</td><td>directionForwardOrInverse</td><td>Decision</td></tr>
	 * <tr><td>T</td><td>T</td><td>graphsToUseForPrediction=inverse<br/>graphsToCheckForConsistency=<b>forward</b></td></tr>
	 * <tr><td>T</td><td>F</td><td>graphsToUseForPrediction=<b>forward</b><br/>graphsToCheckForConsistency=inverse</td></tr>
	 * <tr><td>F</td><td>T</td><td>graphsToUseForPrediction=<b>forward</b><br/>graphsToCheckForConsistency=<b>forward</b></td></tr>
	 * <tr><td>F</td><td>F</td><td>graphsToUseForPrediction=inverse<br/>graphsToCheckForConsistency=inverse</td></tr>
	 * </table>
	 * @param m model to use
	 * @param gr graph to make predictions in
	 * @param c consistency checker to use for predictions. 
	 */
	public MarkovClassifier(MarkovModel m, LearnerGraph gr, ConsistencyChecker c)
	{
		model = m;graph = gr;checker=c;
		graphsToUseForPrediction=constructPathsFromEachState(graph,model.predictForwardOrSideways != model.directionForwardOrInverse);
		graphsToCheckForConsistency=constructPathsFromEachState(graph,model.directionForwardOrInverse);
	}
	
	/** This function is predicts transitions from each state and then adds them to the supplied graph.
	 *  
	 * @param tentativeAutomaton tentative Automaton 
	 * @param predictForwardOrSideways whether to make predictions forward or sideways
	 * @param highThreshold if the predicted probability of a transition is above this value, it seems plausible to add this transition.
	 * @param lowThreshold if the predicted probability of a transition is below this value, it is believed that the impact of this transition is insignificant.
	 */
	public LearnerGraph Markov_tentative(LearnerGraph tentativeAutomaton, boolean predictForwardOrSideways, double highThreshold, double lowThreshold)
	{
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

		final Configuration shallowCopy = tentativeAutomaton.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph outcome = new LearnerGraph(shallowCopy);
		LearnerGraph.copyGraphs(tentativeAutomaton, outcome);
		Set<Label> allElementsOfAlphabet = tentativeAutomaton.learnerCache.getAlphabet(); 
		List<Label> pathToNewState=new LinkedList<Label>();
		// mapping map to store all paths leave each state in different length
    	for(CmpVertex vert:tentativeAutomaton.transitionMatrix.keySet())
    	{
    		if(vert.isAccept() )
            {
		        Map<Label,UpdatablePairDouble> outgoing_labels_probabilities=new HashMap<Label,UpdatablePairDouble>();
		        Map<Label,UpdatablePairInteger> outgoing_labels_occurrences=new HashMap<Label,UpdatablePairInteger>();
				LinkedList<FrontLineElem> ListofFrontElem=new LinkedList<FrontLineElem>();
				LinkedList<FrontLineElem> frontline = new LinkedList<FrontLineElem>();
				LearnerGraph graphForPrediction = graphsToUseForPrediction.get(vert);
	            FrontLineElem e=new FrontLineElem(new LinkedList<Label>(),graphForPrediction.getInit());
			    frontline.add(e);
			    UpdatablePairInteger sum=new UpdatablePairInteger(0,0);
			    while(!frontline.isEmpty())
			    {
			    	e=frontline.pop();
			    	
					Map<Label,CmpVertex> transitions = graphForPrediction.transitionMatrix.get(e.currentState);
			    	for(Object lblObj:transitions.keySet())					
			    	{
			    		Label lbl = (Label)lblObj;
			    		for(CmpVertex target:graphForPrediction.getTargets(transitions.get(lbl)))
			    		{
			    			pathToNewState=new ArrayList<Label>();
			    			pathToNewState.addAll(e.pathToFrontLine);
			    			pathToNewState.add(lbl);
			    			if(pathToNewState.size()==model.getPredictionLen())
			    			{
			    				Trace path_chunk_minus_one=new Trace(pathToNewState,true);
			    				for(Label label:allElementsOfAlphabet)
			    				{
			    					Trace Predicted_trace= new Trace();Predicted_trace.getList().addAll(path_chunk_minus_one.getList());
			    					Collections.reverse(Predicted_trace.getList());
			    					Predicted_trace.add(label);

			    					UpdatablePairInteger occurrence_of_predicted_form_Markov=model.occurrenceMatrix.get(Predicted_trace);

			    					if(outgoing_labels_occurrences.containsKey(label))
			    					{
			    						UpdatablePairInteger labels_ocuurence= outgoing_labels_occurrences.get(label);
			    						sum.add(labels_ocuurence);
			    						labels_ocuurence.add(occurrence_of_predicted_form_Markov);											 
			    						outgoing_labels_occurrences.put(label, labels_ocuurence);
			    					}
			    					else
			    					{
			    						outgoing_labels_occurrences.put(label, occurrence_of_predicted_form_Markov);
			    						sum.add(occurrence_of_predicted_form_Markov);
			    					}	
			    				}
			    			}
			    			else
			    			{// not reached the maximal length of paths to explore
			    				frontline.add(new FrontLineElem(pathToNewState,target));
			    			}
			    		}
			    		ListofFrontElem.add(e);
			    	}
			    }

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
    	 			Map<Label, CmpVertex> already_outgoing = tentativeAutomaton.transitionMatrix.get(currrent_state_to_explore_outgoing);
    	 			assert already_outgoing!=null : "state "+currrent_state_to_explore_outgoing+" is not mentioned in the transition diagram";

    	 			if(!already_outgoing.containsKey(out.getKey()))
    	 			{  	   						
    	 				if(out.getValue().firstElem >  highThreshold && out.getValue().secondElem <= lowThreshold && currrent_state_to_explore_outgoing.isAccept()==true)
    	 				{  
    	 					if(!outcome.transitionMatrix.get(currrent_state_to_explore_outgoing).keySet().contains(out.getKey()))
    	 					{
    	 						MarkovModel.extendWithLabel(outcome,currrent_state_to_explore_outgoing, true, out.getKey());
    	 					}     					      
    	 				} 

    	 				if(out.getValue().secondElem >  highThreshold && out.getValue().firstElem <= lowThreshold && currrent_state_to_explore_outgoing.isAccept()==true)
    	 				{  
    	 					if(!outcome.transitionMatrix.get(currrent_state_to_explore_outgoing).keySet().contains(out.getKey()))
    	 					{
    	 						MarkovModel.extendWithLabel(outcome,currrent_state_to_explore_outgoing, false, out.getKey());
    	 					}     					      
    	 				} 	 					
    	 			}					   
    	 		}          	       	      
    	 	}
    	}
    	
    	return outcome;
	}

	/** Uses the supplied Markov matrix to predict transitions from a specific state, passed as an argument. The choice of direction is <em>not</em> a choice between predicting transitions leaving a state based on those surrounding that state v.s
	 * predicting transitions entering a state based on those surrounding it. It is rather a choice of classifier to make predictions, the one that looks at history and decides what is to follow and the one looking at surrounding transitions and
	 * making decisions based on that.  
	 * <ul>
	 * <li>
	 * Where <i>predictForwardOrSideways</i> is true, we are predicting transitions based on paths leading to the state of interest. Parameter <i>Inverse_Graph</i> should be the (non-deterministic) inverse of <i>graph</i>.
	 * </li>
	 * <li> 
	 * Where <i>predictForwardOrSideways</i> is false, we are predicting transitions based on paths leading from the state of interest (sideways predictions). Parameter <i>Inverse_Graph</i> should be the same as <i>graph</i> and 
	 * <i>pathBeyondCurrentState</i> should be null because once we predicted one transition, there are no further transitions from that state, hence no further transitions can be predicted sideways.
	 * </li>
	 * </ul>
	 * <em>predictForwardOrSideways</em> <i>true</i> if this is to predict forward (usual Markov) or <i>false</i> for sideways. 
	 * @param vert state of interest
	 * @param pathBeyondCurrentState labels that are assumed to be at the tail of all paths leading to a state of interest. 
	 * Used in predictions where we are considering a PTA rooted at some real states. Each path in this PTA can the be passed as <i>pathBeyondCurrentState</i>. 
	 * @param chunkLength length of paths to consider (before the <i>pathBeyondCurrentState</i> component).
	 * @param pathsOfInterest paths considered for prediction. Ignored if <i>null</i>.
	 * Each such path had an outgoing label added and possibly <i>pathBeyondCurrentState</i> appended to it before being passed into Markov and the summary of the outcomes of such predictions is returned by this method.
	 * @return map from labels to predictions.
	 */
	public <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> Map<Label, MarkovOutcome> predictTransitionsFromState( 
			CmpVertex vert, List<Label> pathBeyondCurrentState, int chunkLength,Collection<List<Label>> pathsOfInterest)
	{
		assert vert.isAccept();
		int lengthOfPathBeyond = pathBeyondCurrentState == null?0:pathBeyondCurrentState.size();
		if (lengthOfPathBeyond+1 > chunkLength)
			throw new IllegalArgumentException("supplied pathBeyondCurrentState is too long and does not permit exploration");
		if (!model.predictForwardOrSideways && lengthOfPathBeyond>0)
			throw new IllegalArgumentException("sideways predictions cannot be made by extension of earlier sideways predictions");

		Set<Label> failureLabels = new TreeSet<Label>();
		Map<Label,MarkovOutcome> outgoing_labels_probabilities=new HashMap<Label,MarkovOutcome>();
		LinkedList<FrontLineElem> frontline = new LinkedList<FrontLineElem>();
		LearnerGraph graphToUseInPredictions = graphsToUseForPrediction.get(vert);
        FrontLineElem e=new FrontLineElem(new LinkedList<Label>(),graphToUseInPredictions.getInit());
        if (e.currentState.isAccept())
        	frontline.add(e);
        
	    while(!frontline.isEmpty())
	    {
	    	e=frontline.pop();	
			if(e.pathToFrontLine.size()==chunkLength-1-lengthOfPathBeyond)
			{
				if (pathsOfInterest != null)
					pathsOfInterest.add(e.pathToFrontLine);

				for(Label label:graph.getCache().getAlphabet())
				{
					if (!failureLabels.contains(label))
					{// if the labels is not already recorded as being inconsistently predicted
						MarkovOutcome predictedFromEalierTrace = outgoing_labels_probabilities.get(label);
    					Trace Predicted_trace = new Trace();
    					if (model.predictForwardOrSideways)
    					{
    						for(int i=e.pathToFrontLine.size()-1;i>=0;--i) Predicted_trace.add(e.pathToFrontLine.get(i));if (pathBeyondCurrentState != null) Predicted_trace.getList().addAll(pathBeyondCurrentState);
    					}
    					else
    					{
    						Predicted_trace.getList().addAll(e.pathToFrontLine);
    					}
    					Predicted_trace.add(label);
    					
    					MarkovOutcome predicted_from_Markov=model.predictionsMatrix.get(Predicted_trace);
						MarkovOutcome outcome = MarkovOutcome.reconcileOpinions_PosNeg_Overrides_Null(predictedFromEalierTrace, predicted_from_Markov);
						if (outcome != predictedFromEalierTrace)
						{// we learnt something new, be it a new value (or a non-null value) or a failure, record it
							if (outcome == MarkovOutcome.failure)
							{
								failureLabels.add(label);outgoing_labels_probabilities.remove(label);
							}
							else
								outgoing_labels_probabilities.put(label, outcome);
						}
					}
				}
			}
			else
			{// not reached the maximal length of paths to explore
		    	for(Entry<Label, CmpVertex> entry: graphToUseInPredictions.transitionMatrix.get(e.currentState).entrySet())				
		    	{		
		    		ArrayList<Label> pathToUseWithMarkovToPredictOutgoing=new ArrayList<Label>(chunkLength);
		    		pathToUseWithMarkovToPredictOutgoing.addAll(e.pathToFrontLine);
		    		pathToUseWithMarkovToPredictOutgoing.add(entry.getKey());

		    		for(CmpVertex target:graphToUseInPredictions.getTargets(entry.getValue()))
		    			if (target.isAccept())
		    				frontline.add(new FrontLineElem(pathToUseWithMarkovToPredictOutgoing,target));
		    	}
			}
	    }

	    return outgoing_labels_probabilities;
	}
	
	/** Updates Markov. This is useful where we have added something to the original PTA and need to update Markov. Crucial for learning of Markov for sideways inference, where we cannot learn from the original
	 * traces and have to delay Markov construction to the time where PTA is built.
	 * <p>
	 * Note that computing Markov using incoming/outgoing paths of length 0 is just a distribution of letters, tagged with pos/neg/fail. 
	 * It is not based on earlier knowledge hence could be built either forwards or sideways in the same way.
	 *
	 * @param vert state to predict for
	 * @param alphabet alphabet of the graph of interest
	 * @param chunkLength how many steps to make a prediction for.
	 */
	public <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void updateMarkov(CmpVertex vert, Collection<Label> alphabet, int chunkLength)
	{
		List<List<Label>> markovPathsToUpdate = new LinkedList<List<Label>>();
		predictTransitionsFromState(vert,null,chunkLength,markovPathsToUpdate);

	    // Now we iterate through all the labels and update entries in markovEntriesToUpdate depending on the outcome.
	    for(Label lbl:alphabet)
	    {
	    	CmpVertex target = graph.transitionMatrix.get(vert).get(lbl);
	    	if (target != null) // there is a transition with the considered label, hence update Markov
		    	for(List<Label> pathToUseWithMarkovToPredictOutgoing:markovPathsToUpdate)
		    	{
		    		Trace predictedTrace= new Trace();
					if (model.predictForwardOrSideways)
					{
						for(int i=pathToUseWithMarkovToPredictOutgoing.size()-1;i>=0;--i) predictedTrace.add(pathToUseWithMarkovToPredictOutgoing.get(i));
					}
					else
					{
						predictedTrace.getList().addAll(pathToUseWithMarkovToPredictOutgoing);
					}
					predictedTrace.add(lbl);
					
					MarkovOutcome newValue = null;
					UpdatablePairInteger p=model.occurrenceMatrix.get(predictedTrace);if (p == null) { p=new UpdatablePairInteger(0, 0);model.occurrenceMatrix.put(predictedTrace,p); }

					if (target.isAccept())
					{
						newValue=MarkovOutcome.positive;p.add(1, 0);
					}
					else
					{
						newValue=MarkovOutcome.negative;p.add(0, 1);
					}
					
					model.predictionsMatrix.put(predictedTrace, MarkovOutcome.reconcileOpinions_PosNeg_Overrides_Null(model.predictionsMatrix.get(predictedTrace),newValue));
		    	}
	    }
	}

	/** Determines how consistent a graph is compared to the data in the Markov model.
 	 * <em>directionForwardOrInverse<em> whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and all its target states could be merged together.
	 */
	public <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> long computeConsistency()
	{
		long accumulatedInconsistency = 0;
		for(CmpVertex v:graph.transitionMatrix.keySet()) if (v.isAccept()) accumulatedInconsistency+=checkFanoutInconsistency(v);
		return accumulatedInconsistency;
	}
	
	/** Implementations of this interface are used to check for consistency between Markov predictions and actual mergers. For instance, we could have a transition with a specific label predicted from a state where there is no transition
	 * with such a label or a positive transition is predicted whereas a negative transition is present. Another case is where no transition is predicted whereas a transition is present.
	 * <ul>
	 * <li>
	 * Whenever {@link #consistent(MarkovOutcome, MarkovOutcome)} returns false, an inconsistencies counter is incremented. 
	 * </li>
	 * <li> 
	 * There are two ways to check inconsistencies, either by implementing {@link #labelConsistent(MarkovOutcome, MarkovOutcome)} to always return <i>true</i>, or by making it return false for the first inconsistency 
	 * which will then stop exploration for the inconsistent label.
	 * </li>
	 * </ul> 
	 */
	public interface ConsistencyChecker
	{
		/** Returns an alphabet to use for a specific vertex. This would usually return a collection of labels on transitions from that state but may also be used to return an entire alphabet in order to check that not only that all the 
		 * existing transitions are not predicted as non-existing but also that all those that do not exist are not predicted as those that are to exist. The latter kind of check is useful on states where we expect all outgoing transitions to
		 * be correctly identified. 
		 *  
		 * @param graph graph which to process 
		 * @param v vertex for which to compute an alphabet. 
		 */
		public Collection<Label> obtainAlphabet(LearnerGraph graph,CmpVertex v);
		
		/** 
		 * Given two outcomes, returns true if they are considered consistent and false otherwise.
		 */
		public boolean consistent(MarkovOutcome actual,MarkovOutcome predicted);
		
		/**
		 * Given two outcomes, returns a new value of the prediction to be associated with the label. Can return {@link MarkovOutcome#failure} if the label is to be labelled as inconsistent and excluded from any other comparisons. 
		 * With this returning {@link MarkovOutcome#failure}, we can have multiple inconsistencies per label, associated to 
		 * different paths leading to a state of interest (or different paths leading from it) and hence different Markov predictions. 
		 */
		public MarkovOutcome labelConsistent(MarkovOutcome actual,MarkovOutcome predicted);
	}
	
	/** Uses the supplied Markov matrix to check if predicted transitions from specific states match those that actually exist.
	 * <ul>
	 * <li>
	 * Where <i>predictForwardOrSideways</i> is true, we are predicting transitions based on paths leading to the state of interest. Parameter <i>Inverse_Graph</i> should be the (non-deterministic) inverse of <i>graph</i>.
	 * </li>
	 * <li> 
	 * Where <i>predictForwardOrSideways</i> is false, we are predicting transitions based on paths leading from the state of interest (sideways predictions). Parameter <i>Inverse_Graph</i> should be the same as <i>graph</i> and 
	 * <i>pathBeyondCurrentState</i> should be null because once we predicted one transition, there are no further transitions from that state, hence no further transitions can be predicted sideways.
	 * </li>
	 * </ul>
	 * Requires Markov matrix to contain prefix-closed set of traces, in order to check for paths that have not been seen at all and hence ignored (otherwise they will be counted as inconsistencies that is perhaps not right). 
	 * <br/>
	 * <em>predictForwardOrSideways</em> <i>true</i> if this is to predict forward (usual Markov) or <i>false</i> for sideways. 
	 * <em>directionForwardOrInverse</em> whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and all its target states could be merged together.
	 *
	 * @param vert state of interest
	 */
	public long checkFanoutInconsistency(CmpVertex vert)
	{
		assert vert.isAccept();
		LearnerGraph graphToUseInChecks = graphsToUseForPrediction.get(vert);
		Collection<Label> outgoingLabels = checker.obtainAlphabet(graphToUseInChecks,graphToUseInChecks.getInit());
		Map<Label,MarkovOutcome> outgoing_labels_value=new HashMap<Label,MarkovOutcome>();
		//for(Label l:alphabet) outgoing_labels_probabilities.put(l, UpdatableOutcome.unknown);
		long inconsistencies = 0;

		for(Entry<Label,CmpVertex> entry:graph.transitionMatrix.get(vert).entrySet())
		{
			outgoing_labels_value.put(entry.getKey(),entry.getValue().isAccept()?MarkovOutcome.positive:MarkovOutcome.negative);
		}
		LinkedList<FrontLineElem> frontline = new LinkedList<FrontLineElem>();
		
		LearnerGraph graphToUseInPredictions = graphsToUseForPrediction.get(vert);
        FrontLineElem e=new FrontLineElem(new LinkedList<Label>(),graphToUseInPredictions.getInit());
	    frontline.add(e);
	    while(!frontline.isEmpty())
	    {
	    	e=frontline.pop();	
	    	for(Entry<Label,CmpVertex> entry: graphToUseInPredictions.transitionMatrix.get(e.currentState).entrySet())					
	    	{		
	    		for(CmpVertex target:graphToUseInPredictions.getTargets(entry.getValue()))
	    		{
	    			ArrayList<Label> pathToUseWithMarkovToPredictOutgoing=new ArrayList<Label>(model.getChunkLen());
	    			pathToUseWithMarkovToPredictOutgoing.addAll(e.pathToFrontLine);
	    			pathToUseWithMarkovToPredictOutgoing.add(entry.getKey());
	    			if(pathToUseWithMarkovToPredictOutgoing.size()==model.getPredictionLen())
	    			{
	    				List<Label> encounteredPartOfTrace = new ArrayList<Label>();
	    				
    					if (model.predictForwardOrSideways)
    					{
    						for(int i=pathToUseWithMarkovToPredictOutgoing.size()-1;i>=0;--i) encounteredPartOfTrace.add(pathToUseWithMarkovToPredictOutgoing.get(i));
    					}
    					else
    					{
    						encounteredPartOfTrace.addAll(pathToUseWithMarkovToPredictOutgoing);
    					}
    					if (model.occurrenceMatrix.containsKey(new Trace(encounteredPartOfTrace, true))) // we skip everything where a path was not seen in PTA.
		    				for(Label label:outgoingLabels)
		    				{
	    						MarkovOutcome labels_occurrence= outgoing_labels_value.get(label);
	    						if (labels_occurrence != MarkovOutcome.failure)
	    						{
			    					Trace traceToCheck = new Trace();traceToCheck.getList().addAll(encounteredPartOfTrace);
			    					traceToCheck.add(label);
		
			    					MarkovOutcome predicted_from_Markov=model.predictionsMatrix.get(traceToCheck);
			    					if (predicted_from_Markov != MarkovOutcome.failure)
			    					{// if training data does not lead to a consistent outcome for this label because chunk length is too small, not much we can do, but otherwise we are here and can make use of the data
			    						if (!checker.consistent(labels_occurrence, predicted_from_Markov))
			    						{
			    							++inconsistencies;// record inconsistency
			    							//System.out.println("inconsistency at state "+vert+" because path "+Predicted_trace+" is Markov-predicted as "+predicted_from_Markov+" but earlier value is "+labels_occurrence+" total inconsistencies: "+inconsistencies);
			    						}
		    							outgoing_labels_value.put(label,checker.labelConsistent(labels_occurrence, predicted_from_Markov));// record the outcome composed of both Markov and label. If a failure is recorded, we subsequently do not look at this label.
			    					}
	    						}
		    				}
	    			}
	    			else
	    			{// not reached the maximal length of paths to explore
	    				frontline.add(new FrontLineElem(pathToUseWithMarkovToPredictOutgoing,target));
	    			}
	    		}
	    	}
	    }

	    return inconsistencies;
	}
	
	/** This function is predicts transitions from each state.
	 * <ul>
	 * <li>
	 * Where <i>predictForward</i> is true, we are predicting transitions based on paths leading to the state of interest. Parameter <i>Inverse_Graph</i> should be the (non-deterministic) inverse of <i>graph</i>.
	 * </li>
	 * <li> 
	 * Where <i>predictForward</i> is false, we are predicting transitions based on paths leading from the state of interest (sideways predictions). Parameter <i>Inverse_Graph</i> should be the same as <i>graph</i>.
	 * </li>
	 * </ul>
	 * @return a list of possible of outgoing transitions from each state
	 */
	public Map<CmpVertex, Map<Label, MarkovOutcome>> predictTransitions()
	{
		/** Maps states to a function associating labels to a probability of a transition with the label of interest from a state of interest. Computed from {@link MarkovUniversalLearner#state_outgoing_occurence}. */
		Map<CmpVertex,Map<Label,MarkovOutcome>> state_outgoing=new HashMap<CmpVertex,Map<Label,MarkovOutcome>>();

		final Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);

    	for(CmpVertex vert:graph.transitionMatrix.keySet())
    		if(vert.isAccept() )
            {
        	   Map<Label,MarkovOutcome> outgoing_labels_probabilities=predictTransitionsFromState(vert,null,model.getChunkLen(),null);
			   if (!outgoing_labels_probabilities.isEmpty())
			    	state_outgoing.put(vert, outgoing_labels_probabilities);
			}
    	return state_outgoing;
	}	
	
	/** Where we get a specific figure reflecting the number of inconsistencies, it would depend on the number of states, size of an alphabet and graph topology. This computes a normalised inconsistency as a logarithm of a ratio
	 * of the inconsistency encountered and the maximal one. 
	 * <p>
	 * Where predictions are being made inverse rather than forward, an appropriate graph/Markov have to be used.
	 * <p>
	 * The implementation follows {@link MarkovModel#predictTransitions(LearnerGraph, boolean)}.
	 * 
	 * <em>predictForwardOrSideways</em> how to make predictions.
	 * <em>directionForwardOrInverse</em> whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and all its target states could be merged together.
	 * @return inconsistency
	 */
	public double countPossibleInconsistencies()
	{
		double countOfPossibleInconsistencies = 0;
		long actualInconsistency = computeConsistency();
		Collection<List<Label>> collectionOfPaths = new ArrayList<List<Label>>();
    	for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
    		if(entry.getKey().isAccept() )
            {
    			// it would be more efficient if I passed a mock of a collection instead of an actual one but for small graphs it does not matter.
    			predictTransitionsFromState(entry.getKey(),null,model.getChunkLen(),collectionOfPaths);
    			countOfPossibleInconsistencies+=collectionOfPaths.size()*entry.getValue().size();collectionOfPaths.clear();
            }
    	return actualInconsistency/countOfPossibleInconsistencies;
	}

	public List<List<Label>> identifyPathsToMerge(LearnerGraph referenceGraph)
	{
		if (model.getChunkLen() < 2)
			throw new IllegalArgumentException("not enough data for a first-order Markov model");
		
		boolean predictForwardOrSideways = true;model.updateMarkov(graph, predictForwardOrSideways, false);
		int attemptToUpdateMarkov=0;
		long scoreAfterBigMerge=-1,earlierScoreAfterBigMerge=-1;
		int WLength = 1;
		
		DifferentPredictionsInconsistencyNoBlacklisting checker = new DifferentPredictionsInconsistencyNoBlacklisting();
		
		List<List<Label>> whatToMerge = null;
		//do
		{
			long maxCount = 0;earlierScoreAfterBigMerge=scoreAfterBigMerge;
			System.out.println("Traces in Markov: "+model.predictionsMatrix.size());
			for(Entry<Trace,MarkovOutcome> entry:model.predictionsMatrix.entrySet())
				if (entry.getKey().getList().size() == WLength && entry.getValue() == MarkovOutcome.positive)
				{
					long countInPTA=m.getOccurrence(predictForwardOrSideways).get(entry.getKey()).firstElem;
					if (countInPTA > maxCount)
						maxCount = countInPTA;
				}
			
			
			Map<Long,List<List<Label>>> thresholdToInconsistency = new TreeMap<Long,List<List<Label>>>();
			
			for(Entry<Trace,MarkovOutcome> markovEntry:model.predictionsMatrix.entrySet())
				if (markovEntry.getKey().getList().size() == WLength && markovEntry.getValue() == MarkovOutcome.positive)
				{
					List<Label> path = markovEntry.getKey().getList();
					long countInPTA=model.occurrenceMatrix.get(markovEntry.getKey()).firstElem;
					if (countInPTA < maxCount/2)
					{
						//Collection<List<Label>> pathsToIdentifyStates = new LinkedList<List<Label>>();pathsToIdentifyStates.add(path);checker.setUniquePaths(pathsToIdentifyStates);
						long value = MarkovModel.computeInconsistencyForMergingPath(graph,pathsFromEachStateInGraph,predictForwardOrSideways,directionForwardOrInverse,path,m,checker);
						if (value >= 0)
						{
							List<List<Label>> labelsForThisInconsistency = thresholdToInconsistency.get(value);
							if (labelsForThisInconsistency == null)
							{
								labelsForThisInconsistency = new LinkedList<List<Label>>();thresholdToInconsistency.put(value, labelsForThisInconsistency); 
							}
							labelsForThisInconsistency.add(path);
						}
					}
				}
			//System.out.println(thresholdToInconsistency.entrySet().iterator().next());
			for(double threshold:thresholdToInconsistency.keySet())
			{
				Set<List<Label>> smallValueUniques = new HashSet<List<Label>>();
				for(Entry<Long,List<List<Label>>> entry:thresholdToInconsistency.entrySet())
					if (entry.getKey() <= threshold)
						smallValueUniques.addAll(entry.getValue());
					else
						break;
				LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
				List<StatePair> pairsList = PairQualityLearner.buildVerticesToMergeForPath(smallValueUniques);
				scoreAfterBigMerge = MarkovModel.dREJECT;
				LearnerGraph merged = null;
				if (!pairsList.isEmpty())
				{
					int score = graph.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
					if (score < 0)
						scoreAfterBigMerge = MarkovModel.dREJECT;
					else
					{
						merged = MergeStates.mergeCollectionOfVertices(graph, null, verticesToMerge);
						//checker.setUniquePaths(smallValueUniques);
						scoreAfterBigMerge = MarkovModel.computeInconsistency(merged, predictForwardOrSideways, m, checker);
					}
				}
				
				if (merged != null && referenceGraph!=null)
				{
					System.out.print("After big merge ("+threshold+"): "+scoreAfterBigMerge+" inconsistencies, "+merged.getStateNumber()+" states, originally "+graph.getStateNumber()+ " ");
					System.out.println(checkMergeValidity(referenceGraph,graph,pathsFromEachStateInGraph,smallValueUniques)?"VALID":"INVALID");
				}
			}

			{
				whatToMerge = thresholdToInconsistency.entrySet().iterator().next().getValue();
				List<StatePair> pairsList = PairQualityLearner.buildVerticesToMergeForPath(pathsFromEachStateInGraph,directionForwardOrInverse,whatToMerge);
				scoreAfterBigMerge = MarkovModel.dREJECT;
				LearnerGraph merged = null;
				if (!pairsList.isEmpty())
				{
					LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					int score = graph.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
					if (score < 0)
						scoreAfterBigMerge = MarkovModel.dREJECT;
					else
					{
						merged = MergeStates.mergeCollectionOfVertices(graph, null, verticesToMerge);
						scoreAfterBigMerge = MarkovModel.computeInconsistency(merged, predictForwardOrSideways, m, checker);
					}
				}
				
				if (merged != null && referenceGraph!=null)
				{
					System.out.print("Iteration "+attemptToUpdateMarkov+" : "+scoreAfterBigMerge+" inconsistencies, "+merged.getStateNumber()+" states, originally "+graph.getStateNumber()+ " ");
					System.out.println(checkMergeValidity(referenceGraph,graph,pathsFromEachStateInGraph,whatToMerge)?"VALID":"INVALID");

					m.updateMarkov(merged,predictForwardOrSideways,false);
					++attemptToUpdateMarkov;
				}
			}
		}
		//while(scoreAfterBigMerge > earlierScoreAfterBigMerge);
		System.out.println("FOR THE CORRECT GRAPH INCONSISTENCIES ARE: "+MarkovModel.computeInconsistency(referenceGraph, predictForwardOrSideways, m, checker));
		return whatToMerge;
	}
}
