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
import java.util.concurrent.atomic.AtomicLong;

import statechum.Configuration;
import statechum.Label;
import statechum.Trace;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.MarkovModel.MarkovOutcome;
import statechum.analysis.learning.MarkovModel.UpdatablePairInteger;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation;

/** An instance of this class holds all the necessary parameters in order to make it possible to predict transitions and/or check inconsistencies using a Markov model. Depending on the kind of model passed to it, 
 * it will be making appropriate predictions.
 */
public class MarkovClassifier 
{
	public static final double fREJECT = -1;
	public static final long dREJECT = -1;

	/** Markov model being used in this classifier. Also determines the direction of prediction and whether forward or sideways. */
	public final MarkovModel model;
	/** The graph in which we are making predictions.*/
	public final LearnerGraph graph;
	
	/** Contains paths to be supplied to Markov for making predictions. The specific kind of the graph depends on the direction in which we are doing predictions. Could be either a deterministic or a non-deterministic graph.
	 * <br/>Should be immutable and contain the same states and transition labels as {@link MarkovClassifier#graph}. This is used both in exploration and construction of an alphabet.
	 */
	@SuppressWarnings("rawtypes")
	public final AbstractLearnerGraph graphToUseForPrediction;
	
	/** True if the graph used for predictions is an inverse, in this case all paths we obtain from it are best inverted before lookup in Markov model. 
	 * For efficiency, we could have obviously invert paths in the model but the current setup makes it easier to understand and we need to copy the 
	 * graphs anyway which is be accomplished as fast as inversion. 
	 * <br/>
	 * For the graph to check for consistency, we do not need a special variable - it is <em>model.directionForwardOrInverse</em>.
	 */
	public final boolean predictionGraphInverted;

	/** Contains paths to be used for consistency checking. The specific kind of the graph depends on the direction in which we are doing predictions. Could be either a deterministic or a non-deterministic graph. 
	 * <br/>Should be immutable and contain the same states and transition labels as {@link MarkovClassifier#graph}. This is used both in exploration and construction of an alphabet.
	 */
	@SuppressWarnings("rawtypes")
	public final AbstractLearnerGraph graphToCheckForConsistency;  
	

	/** Navigates a path from the supplied state and either returns 
	 * true if it is a valid path.
	 * 
	 * @param path path to traverse
	 * @param startState the state to start from
	 * @return true if path exists.
	 */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> boolean tracePath(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph, List<Label> path, CmpVertex startState)
	{
		return tracePath_internal(graph,path,0,startState);
	}
	
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> boolean tracePath_internal(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph, List<Label> path, int startPos, CmpVertex startState)
	{
		CmpVertex current = startState;
		if (current == null)
			return false;// if we start from null (such as not found) state, fail immediately.
		if (startPos >= path.size())
			return startState.isAccept();
		if (!startState.isAccept())
			return false;// we are only considering prefix-closed paths here, hence if we hit a reject state on the way, reject a path.
		
		Label label=path.get(startPos);
		Map<Label, TARGET_TYPE> existingTrans = graph.transitionMatrix.get(current);
		TARGET_TYPE collectionOfTargets = existingTrans != null?existingTrans.get(label):null;
		if (collectionOfTargets == null)
			// cannot make a move
			return false;
			
		// now iterate through possible target states
		for(Object vert:graph.getTargets(collectionOfTargets))
		{
			boolean value = tracePath_internal(graph,path,startPos+1,(CmpVertex)vert);
			if (value)
				return true;
		}
			
		// go to the end without finding a path, report a failure.
		return false;
	}
	
	/** Obtains the graph that can be used in calls of {@link #checkFanoutInconsistency(AbstractLearnerGraph, boolean, LearnerGraph, CmpVertex, int)} and many others.
	 * 
	 * @param graph what to invert.
	 * @return inverted graph
	 */
	public static LearnerGraphND computeInverseGraph(LearnerGraph graph)
	{
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND inverseGraph = new LearnerGraphND(shallowCopy);inverseGraph.initEmpty();
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,inverseGraph);  // do the inverse to the tentative graph
		return inverseGraph;
	}
	
	/** Obtains the graph that can be used in calls of {@link #checkFanoutInconsistency(AbstractLearnerGraph, boolean, LearnerGraph, CmpVertex, int)} and many others.
	 * Returns an inverse when <i>predictForward</i> is true and <i>graph</i> otherwise.
	 * @param graph what to compute an inverse of
	 * @param constructInverseOrForward whether to invert
	 * @return either an inverse or the original graph
	 */
	@SuppressWarnings("rawtypes")
	public static AbstractLearnerGraph computeInverseGraph(LearnerGraph graph,boolean constructInverseOrForward)
	{
		AbstractLearnerGraph inverseGraph = null;
		if (constructInverseOrForward)
		{
			inverseGraph = computeInverseGraph(graph);
		}
		else
			inverseGraph = graph;

		return inverseGraph;
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
	 */
	public MarkovClassifier(MarkovModel m, LearnerGraph gr)
	{
		model = m;graph = gr;
		predictionGraphInverted = model.predictForwardOrSideways == model.directionForwardOrInverse;
		graphToUseForPrediction=computeInverseGraph(graph,predictionGraphInverted);
		assert graph.transitionMatrix.keySet().equals(graphToUseForPrediction.transitionMatrix.keySet());
		graphToCheckForConsistency=computeInverseGraph(graph,!model.directionForwardOrInverse);
		assert graph.transitionMatrix.keySet().equals(graphToCheckForConsistency.transitionMatrix.keySet());
	}
	
	interface ForEachCollectionOfPaths
	{
		/** This one is called for each path in an explored graph. */
		public void handlePath(List<Label> path);
	}
	
	public static class FrontLineElem
	{
		public final List<Label> pathToFrontLine;
		public final CmpVertex currentState;
		
		public FrontLineElem(List<Label> path, CmpVertex vert) {
			pathToFrontLine=path;
			currentState=vert;
		}
		
	}

	/** Explores all positive states up to the specified length, calling the supplied callback for each of them. Can be used on both deterministic and non-deterministic graphs. 
	 * In the non-deterministic case, could report the same path multiple times.
	 * 
	 * @param graph graph to explore
	 * @param vert vertex to start with
	 * @param pathLength length of paths to explore
	 * @param callback what to call for each discovered path.
	 */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void WalkThroughAllPathsOfSpecificLength(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph, CmpVertex vert,int pathLength,ForEachCollectionOfPaths callback)
	{
		LinkedList<FrontLineElem> frontline = new LinkedList<FrontLineElem>();
        FrontLineElem e=new FrontLineElem(new LinkedList<Label>(),vert);
        Set<List<Label>> pathsEncountered = new HashSet<List<Label>>();
	    if (vert.isAccept()) frontline.add(e);
	    while(!frontline.isEmpty())
	    {
	    	e=frontline.pop();
	    	
			if(e.pathToFrontLine.size()==pathLength)
			{
				if (!pathsEncountered.contains(e.pathToFrontLine))
				{
					pathsEncountered.add(e.pathToFrontLine);
					callback.handlePath(e.pathToFrontLine);
				}
			}
			else
			{// not reached the maximal length of paths to explore
				Map<Label,TARGET_TYPE> transitions = graph.transitionMatrix.get(e.currentState);
				for(Label lbl:transitions.keySet())					
				{
					for(CmpVertex target:graph.getTargets(transitions.get(lbl)))
		    			if (target.isAccept())
			    		{
			    			List<Label> pathToNewState=new ArrayList<Label>();
			    			pathToNewState.addAll(e.pathToFrontLine);pathToNewState.add(lbl);
		    					frontline.add(new FrontLineElem(pathToNewState,target));
			    		}
			    }
	    	}
	    }
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
		
		LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsList = buildVerticesToMergeForPath(paths);
		if (!pairsList.isEmpty())
		{
			int score = graph.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
			if (score < 0)
				outcome = dREJECT;
			else
			{
				LearnerGraph merged = MergeStates.mergeCollectionOfVertices(graph, null, verticesToMerge);
				outcome = computeInconsistency(merged,model,checker,false);
			}
		}
		
		return outcome;
	}
	
	/** Given the markov model in this classifier and a graph, this method obtains inconsistency for the supplied graph. This is implemented by creating another classifier with the same parameters but a supplied graph as an argument. */
	public static long computeInconsistency(LearnerGraph gr, MarkovModel model,  ConsistencyChecker checker, boolean displayTrace)
	{
		MarkovClassifier cl = new MarkovClassifier(model, gr);return cl.computeConsistency(checker,displayTrace);
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
		public Collection<Label> obtainAlphabet(@SuppressWarnings("rawtypes") AbstractLearnerGraph graph,CmpVertex v);
		
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
	
	public static class InconsistencyNullVsPredicted implements ConsistencyChecker
	{
		@Override
		public MarkovOutcome labelConsistent(MarkovOutcome actual, MarkovOutcome predicted) 
		{
			return consistent(actual,predicted)?actual:MarkovOutcome.failure;
		}
		
		@Override
		public boolean consistent(MarkovOutcome actual, MarkovOutcome predicted) 
		{
			if (actual == MarkovOutcome.failure || predicted == MarkovOutcome.failure)
				return false;
			
			if (actual == null && predicted != null)
				return false;
			
			if (actual != null && predicted != null && actual != predicted)
				return false;
			
			return true;
		}

		@SuppressWarnings({ "rawtypes", "unchecked" })
		@Override
		public Collection<Label> obtainAlphabet(AbstractLearnerGraph graph,@SuppressWarnings("unused") CmpVertex v) {
			return graph.getCache().getAlphabet();
		}
	}
	
	@SuppressWarnings("rawtypes")
	public static class DifferentPredictionsInconsistency implements ConsistencyChecker
	{
		@Override
		public boolean consistent(MarkovOutcome actual, MarkovOutcome predicted) {
			return labelConsistent(actual, predicted) != MarkovOutcome.failure;
		}

		@Override
		public MarkovOutcome labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) 
		{
			return MarkovOutcome.ensureConsistencyBetweenOpinions(actual,predicted);
		}

		@SuppressWarnings("unchecked")
		@Override
		public Collection<Label> obtainAlphabet(AbstractLearnerGraph graph,CmpVertex v) {
			return ((Map)graph.transitionMatrix.get(v)).keySet();
		}
		
	}
	
	/** This one counts all inconsistencies but does not blacklist any label. */
	@SuppressWarnings("rawtypes")
	public static class DifferentPredictionsInconsistencyNoBlacklisting implements ConsistencyChecker
	{
		@Override
		public boolean consistent(MarkovOutcome actual, MarkovOutcome predicted) {
			return MarkovOutcome.ensureConsistencyBetweenOpinions(actual,predicted) != MarkovOutcome.failure;
		}

		@SuppressWarnings("unused")
		@Override
		public MarkovOutcome labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) 
		{
			return actual;
		}

		@SuppressWarnings("unchecked")
		@Override
		public Collection<Label> obtainAlphabet(AbstractLearnerGraph graph,CmpVertex v) {
			return ((Map)graph.transitionMatrix.get(v)).keySet();
		}
		
	}

	/** Similar to {@link MarkovModel.DifferentPredictionsInconsistencyNoBlacklisting} except that uses an entire alphabet for states that are singled out by any of the unique paths. 
	 * These are the states that should have all outgoing transitions added to them.
	 */
	public static class ConsistencyCheckerForIdentificationOfUniques extends DifferentPredictionsInconsistencyNoBlacklisting
	{
		Collection<List<Label>> uniquePaths = null;
		public void setUniquePaths(Collection<List<Label>> pathsToIdentifyStates) 
		{
			uniquePaths = pathsToIdentifyStates;
		}
		
		@SuppressWarnings({ "rawtypes", "unchecked" })
		@Override
		public Collection<Label> obtainAlphabet(AbstractLearnerGraph graph,CmpVertex v) 
		{
			Collection<Label> outcome = ((Map)graph.transitionMatrix.get(v)).keySet(); 
			if (uniquePaths != null)
				for(List<Label> path:uniquePaths)
					if (tracePath(graph,path, v))
					{
						outcome = graph.getCache().getAlphabet();break;
					}
			return outcome;
		}
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

		final Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph outcome = new LearnerGraph(shallowCopy);
		LearnerGraph.copyGraphs(graph, outcome);
		final Set<Label> allElementsOfAlphabet = graph.learnerCache.getAlphabet(); 
		// mapping map to store all paths leave each state in different length
    	for(CmpVertex vert:graph.transitionMatrix.keySet())
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
	    				Trace path_chunk_minus_one=new Trace(pathToNewState,true);
	    				for(Label label:allElementsOfAlphabet)
	    				{
	    					Trace Predicted_trace= new Trace();Predicted_trace.getList().addAll(path_chunk_minus_one.getList());
	    					if (predictionGraphInverted)
	    						Collections.reverse(Predicted_trace.getList());
	    					Predicted_trace.add(label);

	    					UpdatablePairInteger occurrence_of_label_predicted_form_Markov=model.occurrenceMatrix.get(Predicted_trace);

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
    	 			Map<Label, CmpVertex> already_outgoing = graph.transitionMatrix.get(currrent_state_to_explore_outgoing);
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

		final Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph graphWithPredictedTransitions = new LearnerGraph(shallowCopy);
		LearnerGraph.copyGraphs(graph, graphWithPredictedTransitions);

		// in this part the tree is extended depend on their outgoing transition probabilities
	 	for(Entry<CmpVertex, Map<Label, MarkovOutcome>> outgoing:state_outgoing.entrySet())
	 	{
	 		CmpVertex currrent_state_to_explore_outgoing= outgoing.getKey();
	 		Map<Label, MarkovOutcome> list_of_outgoing = outgoing.getValue();
	 		for(Entry<Label, MarkovOutcome> out:list_of_outgoing.entrySet())
	 		{
	 			Map<Label, CmpVertex> already_outgoing = graph.transitionMatrix.get(currrent_state_to_explore_outgoing);
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
	@SuppressWarnings("unchecked")
	public Map<Label, MarkovOutcome> predictTransitionsFromState(CmpVertex vert, final List<Label> pathBeyondCurrentState, int chunkLength,final Collection<List<Label>> pathsOfInterest)
	{
		assert vert.isAccept();
		int lengthOfPathBeyond = pathBeyondCurrentState == null?0:pathBeyondCurrentState.size();
		if (lengthOfPathBeyond+1 > chunkLength)
			throw new IllegalArgumentException("supplied pathBeyondCurrentState is too long and does not permit exploration");
		if (!model.predictForwardOrSideways && lengthOfPathBeyond>0)
			throw new IllegalArgumentException("sideways predictions cannot be made by extension of earlier sideways predictions");

		final Set<Label> failureLabels = new TreeSet<Label>();
		final Map<Label,MarkovOutcome> outgoing_labels_probabilities=new HashMap<Label,MarkovOutcome>();
        WalkThroughAllPathsOfSpecificLength(graphToUseForPrediction,vert,chunkLength-1-lengthOfPathBeyond,new ForEachCollectionOfPaths() 
        {
			@Override
			public void handlePath(List<Label> pathToNewState) 
			{
				if (pathsOfInterest != null)
					pathsOfInterest.add(pathToNewState);

				for(Label label:graph.getCache().getAlphabet())
				{
					if (!failureLabels.contains(label))
					{// if the labels is not already recorded as being inconsistently predicted
						MarkovOutcome predictedFromEalierTrace = outgoing_labels_probabilities.get(label);
    					Trace Predicted_trace = new Trace();
    					if (predictionGraphInverted)
    					{
    						for(int i=pathToNewState.size()-1;i>=0;--i) Predicted_trace.add(pathToNewState.get(i));if (pathBeyondCurrentState != null) Predicted_trace.getList().addAll(pathBeyondCurrentState);
    					}
    					else
    					{
    						Predicted_trace.getList().addAll(pathToNewState);
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
	    });

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
	@SuppressWarnings("unchecked")
	public <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void updateMarkov(CmpVertex vert, int chunkLength)
	{
		List<List<Label>> markovPathsToUpdate = new LinkedList<List<Label>>();
		predictTransitionsFromState(vert,null,chunkLength,markovPathsToUpdate);

	    // Now we iterate through all the labels and update entries in markovEntriesToUpdate depending on the outcome.
	    for(Label lbl:graph.getCache().getAlphabet())
	    {
	    	@SuppressWarnings("rawtypes")
			Object targets = ((Map)graphToCheckForConsistency.transitionMatrix.get(vert)).get(lbl);
	    	if (targets != null) // there are transitions with the considered label, hence update Markov
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

					boolean foundAccept = false, foundReject = false;
					for(Object vObj:graphToCheckForConsistency.getTargets(targets))
					{
						if ( ((CmpVertex)vObj).isAccept() ) foundAccept = true;
						if ( !((CmpVertex)vObj).isAccept() ) foundReject = true;
					}
					
					// By construction of an inverse graph and its immutability, it is either accept, reject, or both. getTargets will never be empty.
					
					if (foundAccept && foundReject)
						throw new IllegalArgumentException("inconsistent inverse graph: path "+predictedTrace.getList()+" is both accepted and rejected");
					
					if (foundAccept)
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
	 * @param checker Consistency checker to use for predictions, usually based on a static method from {@link MarkovOutcome}.
	 * @return true if the graph is consistent according to the supplied checker.
	 */
	public <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> long computeConsistency(ConsistencyChecker checker, boolean displayTrace)
	{
		long accumulatedInconsistency = 0;
		for(CmpVertex v:graph.transitionMatrix.keySet()) if (v.isAccept()) accumulatedInconsistency+=checkFanoutInconsistency(v,checker,displayTrace);
		return accumulatedInconsistency;
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
	 * @param checker Consistency checker to use for predictions, usually based on a static method from {@link MarkovOutcome}.
	 *
	 * @param vert state of interest
	 */
	public long checkFanoutInconsistency(final CmpVertex vert,final ConsistencyChecker checker)
	{
		return checkFanoutInconsistency(vert, checker, false);
	}
	@SuppressWarnings("unchecked")
	public long checkFanoutInconsistency(final CmpVertex vert,final ConsistencyChecker checker, final boolean displayTrace)
	{
		assert vert.isAccept();
		final Collection<Label> outgoingLabels = checker.obtainAlphabet(graphToCheckForConsistency,vert);
		final Map<Label,MarkovOutcome> outgoing_labels_value=new HashMap<Label,MarkovOutcome>();
		//for(Label l:alphabet) outgoing_labels_probabilities.put(l, UpdatableOutcome.unknown);
		final AtomicLong inconsistencies = new AtomicLong();

		for(Entry<Label,CmpVertex> entry:graph.transitionMatrix.get(vert).entrySet())
		{
			outgoing_labels_value.put(entry.getKey(),entry.getValue().isAccept()?MarkovOutcome.positive:MarkovOutcome.negative);
		}
		
		WalkThroughAllPathsOfSpecificLength(graphToUseForPrediction,vert,model.getPredictionLen(),new ForEachCollectionOfPaths() 
        {
			@Override
			public void handlePath(List<Label> pathToNewState) 
			{
				List<Label> encounteredPartOfTrace = new ArrayList<Label>();
				
				if (predictionGraphInverted)
				{
					for(int i=pathToNewState.size()-1;i>=0;--i) encounteredPartOfTrace.add(pathToNewState.get(i));
				}
				else
				{
					encounteredPartOfTrace.addAll(pathToNewState);
				}
				//System.out.println(vert.toString()+" : "+encounteredPartOfTrace+" outgoing: "+outgoingLabels);
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
	    							inconsistencies.addAndGet(1);// record inconsistency
	    							if (displayTrace) System.out.println("inconsistency at state "+vert+" because path "+traceToCheck+" is Markov-predicted as "+predicted_from_Markov+" but earlier value is "+labels_occurrence+" total inconsistencies: "+inconsistencies);
	    						}
    							outgoing_labels_value.put(label,checker.labelConsistent(labels_occurrence, predicted_from_Markov));// record the outcome composed of both Markov and label. If a failure is recorded, we subsequently do not look at this label.
	    					}
						}
    				}
    			}
        });

	    return inconsistencies.get();
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
	 * @param checker Consistency checker to use for predictions, usually based on a static method from {@link MarkovOutcome}.
	 * @return inconsistency
	 */
	public double computeRelativeInconsistency(ConsistencyChecker checker)
	{
		double outcome = 0;
		Collection<List<Label>> collectionOfPaths = new ArrayList<List<Label>>();
    	for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
    		if(entry.getKey().isAccept() )
            {
    			// it would be more efficient if I passed a mock of a collection instead of an actual one but for small graphs it does not matter.
    			long value=checkFanoutInconsistency(entry.getKey(),checker,false);
    			if (value > 0)
    			{
    				predictTransitionsFromState(entry.getKey(),null,model.getChunkLen(),collectionOfPaths);
    				//double inconsistencyforThisState=((double)value)/(collectionOfPaths.size()*entry.getValue().size());collectionOfPaths.clear();
    				//outcome += inconsistencyforThisState;
    				outcome += value;
    			}
            }
    	return outcome;
	}

	/** This function goes through the supplied graph and then adds predictions to the Markov model.
	 *  This makes it possible to update an existing model by creating a new instance of the classifier and associating it with that model. 
	 *  <br/>
	 *  Important: this method mis-counts the number of times shorter traces occur since it
	 * can see that they exist but not the number of tails they lead to. This is left in because I do not use specific values occurrence counts.
	 * @param onlyLongest if set, only add traces of <i>chunkLen</i> to Markov matrix. Where false, all prefixes are added as well.
	 */
	public void updateMarkov(boolean onlyLongest)
	{
    	for(CmpVertex vert:graph.transitionMatrix.keySet())
    		for(int len=onlyLongest?model.getChunkLen():1;len <=model.getChunkLen();++len)// this is very inefficient; we'll optimize it later if needed.
	           if(vert.isAccept())
	        	  updateMarkov(vert,len);
	}
	
	/** Given a collection of paths, constructs a collection where each path is an inverse of what it was.
	 * 
	 * @param paths paths to consider
	 * @return collected of inverted paths.
	 */
	public static Collection<List<Label>> invertPaths(Collection<List<Label>> paths)
	{
		Collection<List<Label>> pathsOfInterest = new LinkedList<List<Label>>();
		for(List<Label> p:paths)
		{
			ArrayList<Label> pathReversible=new ArrayList<Label>(p);Collections.reverse(pathReversible);pathsOfInterest.add(pathReversible);
		}
		return pathsOfInterest;
	}
	
	/** Identifies vertices that are supposed to be merged if we use the provided set of paths to identify states. The direction determines whether we look for outgoing transitions (as in W set)
	 * or incoming ones (see Rob Hierons' invertibility work).
	 * 
	 * @param paths paths to consider
	 * @return collection of pairs of states to merge. This is expected to be passed to the generalised merger.
	 */
	public List<StatePair> buildVerticesToMergeForPath(Collection<List<Label>> paths)
	{
		Collection<List<Label>> pathsOfInterest = model.directionForwardOrInverse?paths:invertPaths(paths);
		return collectionOfSetsToPairs(buildVerticesToMergeForPaths(pathsOfInterest));
	}
	
	/** Given a collection of sets of vertices, returns a collection of pairs of states to merge. This is expected to be passed to the generalised merger, {@link PairScoreComputation#computePairCompatibilityScore_general(StatePair, Collection, Collection)}. */
	public static List<StatePair> collectionOfSetsToPairs(Collection<Set<CmpVertex>> collectionOfSets)
	{
		List<StatePair> pairsList = new LinkedList<StatePair>();
		for(Collection<CmpVertex> vertices:collectionOfSets)
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

	/** Given a collection of paths, makes it possible to merge states from which the provided paths can be followed. 
	 * Where multiple paths can be followed from the same state, merges all states from which any of the paths can be followed. Depending on the input, can be used for paths in the forward direction or in the inverse one (in which case graphsToCheckForPaths
	 * should also be an inverse of a graph of interest, to match the paths being considered).
	 *  
	 * @param paths collection of sequences of labels, we will merge all states that have the same sequence leading from them.
	 * @return a number of collections of vertices to merge. Every two collections are non-intersecting but may not cover all states in the original graph.
	 */
	@SuppressWarnings("unchecked")
	public Collection<Set<CmpVertex>> buildVerticesToMergeForPaths(Collection<List<Label>> paths)
	{
		Map<Integer,Set<CmpVertex>> idToVerticesToMerge = new TreeMap<Integer,Set<CmpVertex>>();
		Map<CmpVertex,Set<Integer>> vertToPaths = new TreeMap<CmpVertex,Set<Integer>>();
		Map<Integer,List<Label>> idToPathsFromIt = new TreeMap<Integer,List<Label>>();// makes it possible to number paths
		int id=0;
		
		for(List<Label> p:paths)
		{
			idToPathsFromIt.put(id, p);
			idToVerticesToMerge.put(id, new TreeSet<CmpVertex>());
			++id;
		}
		
		for(Entry<Integer,List<Label>> path:idToPathsFromIt.entrySet())
		{
			for(CmpVertex v:graph.transitionMatrix.keySet())
				if (tracePath(graphToCheckForConsistency,path.getValue(),v))
				{
					Set<Integer> pathsForVertex = vertToPaths.get(v);
					if (pathsForVertex == null)
					{
						pathsForVertex = new TreeSet<Integer>();vertToPaths.put(v,pathsForVertex);
					}
					vertToPaths.get(v).add(path.getKey());idToVerticesToMerge.get(path.getKey()).add(v);
				}
		}
		
		// now we start merging sets until no two of them have paths in common.
		Set<Integer> pathsFromAnyOfVerts=null;
		Set<CmpVertex> verts = new TreeSet<CmpVertex>();
		
		Set<CmpVertex> vertsConsidered = new TreeSet<CmpVertex>();Collection<Set<CmpVertex>> setsConsidered=new LinkedList<Set<CmpVertex>>();
		do
		{
			pathsFromAnyOfVerts=null;verts = new TreeSet<CmpVertex>();vertsConsidered.clear();setsConsidered.clear();
			
			for(Entry<CmpVertex,Set<Integer>> entry:vertToPaths.entrySet())
				if (!vertsConsidered.contains(entry.getKey())) // we only look at vertices that were not seen before
				{
					verts = new TreeSet<CmpVertex>();
					for(Integer p:entry.getValue())
						verts.addAll(idToVerticesToMerge.get(p));
					
					for(CmpVertex v:verts)
					{
						Set<Integer> pathsForVert = vertToPaths.get(v);
						if (pathsForVert != entry.getValue())
						{// this state is different from our collection, perform the merge.
							if (pathsFromAnyOfVerts == null)
								pathsFromAnyOfVerts = new TreeSet<Integer>(entry.getValue());
							pathsFromAnyOfVerts.addAll(pathsForVert);
						}
					}
					if (pathsFromAnyOfVerts != null)
						break;
					
					{
						vertsConsidered.addAll(verts);setsConsidered.add(verts);// here we update the return value, if we get to the endwith pathsFromAnyOfVerts remaining null, we are done and setsConsidered can be returned
					}
				}
			if (pathsFromAnyOfVerts != null)
			{// had to compute a merge
				for(CmpVertex v:verts)
					vertToPaths.put(v,pathsFromAnyOfVerts);
				//for(Integer p:pathsFromAnyOfVerts)
				//	idToVerticesToMerge.put(p, verts);
			}
		}
		while(pathsFromAnyOfVerts != null);
		
		return setsConsidered;
	}

	/**
	 * Uses a supplied consistency checker to find paths that uniquely identify states. The supplied consistency checker is used to verify consistency after states deemed identical are merged.
	 * @param checker Consistency checker to use for predictions, usually based on a static method from {@link MarkovOutcome}.
	 * @return paths to uniquely identify states.
	 */
	public List<List<Label>> identifyPathsToMerge(ConsistencyChecker checker)
	{
		if (model.getChunkLen() < 2)
			throw new IllegalArgumentException("not enough data for a first-order Markov model");
		
		updateMarkov(false);
		long scoreAfterBigMerge=-1;
		int WLength = 1;
		
		//DifferentPredictionsInconsistencyNoBlacklisting checker = new DifferentPredictionsInconsistencyNoBlacklisting();
		
		List<List<Label>> whatToMerge = null;

		long maxCount = 0;
		for(Entry<Trace,MarkovOutcome> entry:model.predictionsMatrix.entrySet())
			if (entry.getKey().getList().size() == WLength && entry.getValue() == MarkovOutcome.positive)
			{
				long countInPTA=model.occurrenceMatrix.get(entry.getKey()).firstElem;
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
					long value = computeInconsistencyForMergingPath(path, checker);
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
		
		//for(double threshold:thresholdToInconsistency.keySet())
		long threshold = thresholdToInconsistency.keySet().iterator().next();
		{
			Set<List<Label>> smallValueUniques = new HashSet<List<Label>>();
			for(Entry<Long,List<List<Label>>> entry:thresholdToInconsistency.entrySet())
				if (entry.getKey() <= threshold)
					smallValueUniques.addAll(entry.getValue());
				else
					break;
			LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			List<StatePair> pairsList = buildVerticesToMergeForPath(smallValueUniques);
			scoreAfterBigMerge = dREJECT;
			LearnerGraph merged = null;
			if (!pairsList.isEmpty())
			{
				int score = graph.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
				if (score < 0)
					scoreAfterBigMerge = dREJECT;
				else
				{
					merged = MergeStates.mergeCollectionOfVertices(graph, null, verticesToMerge);
					//checker.setUniquePaths(smallValueUniques);
					scoreAfterBigMerge = computeInconsistency(merged,model,checker,false);
				}
			}
			
			//System.out.println("After big merge ("+threshold+") of "+smallValueUniques+" : "+scoreAfterBigMerge+" inconsistencies, "+merged.getStateNumber()+" states, originally "+graph.getStateNumber()+ " ");
			/*
			if (merged != null && referenceGraph!=null)
			{
				System.out.print("After big merge ("+threshold+"): "+scoreAfterBigMerge+" inconsistencies, "+merged.getStateNumber()+" states, originally "+graph.getStateNumber()+ " ");
				System.out.println(checkMergeValidity(referenceGraph,graph,smallValueUniques)?"VALID":"INVALID");
			}*/
		}

		{
			whatToMerge = thresholdToInconsistency.entrySet().iterator().next().getValue();
			List<StatePair> pairsList = buildVerticesToMergeForPath(whatToMerge);
			scoreAfterBigMerge = dREJECT;
			LearnerGraph merged = null;
			if (!pairsList.isEmpty())
			{
				LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
				int score = graph.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
				if (score < 0)
					scoreAfterBigMerge = dREJECT;
				else
				{
					merged = MergeStates.mergeCollectionOfVertices(graph, null, verticesToMerge);
					scoreAfterBigMerge = computeInconsistency(merged, model, checker,false);
				}
			}
			
			// We are confident we got it right, hence update Markov (in reality we would mess it up sometimes).
			//new MarkovClassifier(model,merged).updateMarkov(false);
			/*
			if (merged != null && referenceGraph!=null)
			{
				System.out.print("Iteration "+attemptToUpdateMarkov+" : "+scoreAfterBigMerge+" inconsistencies, "+merged.getStateNumber()+" states, originally "+graph.getStateNumber()+ " ");
				System.out.println(checkMergeValidity(referenceGraph,graph,whatToMerge)?"VALID":"INVALID");

				updateMarkov(merged,false);
				++attemptToUpdateMarkov;
			}*/
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
		MarkovClassifier cl = new MarkovClassifier(model,graph);
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
