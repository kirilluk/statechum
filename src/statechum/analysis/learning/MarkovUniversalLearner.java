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
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;
import java.util.TreeSet;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.Label;
import statechum.Pair;
import statechum.Trace;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.collections.ArrayMapWithSearch;
import statechum.collections.HashMapWithSearch;

public class MarkovUniversalLearner
{
	private Map<Trace, UpdatablePairInteger> occurrenceMatrixForward =  new HashMap<Trace,UpdatablePairInteger>(),occurrenceMatrixSideways =  new HashMap<Trace,UpdatablePairInteger>();
	private Map<Trace, MarkovOutcome> MarkovMatrixForward_F =  new HashMap<Trace,MarkovOutcome>(), MarkovMatrixSideways_F = new HashMap<Trace,MarkovOutcome>();
	private Map<Trace, MarkovOutcome> MarkovMatrixForward_I =  new HashMap<Trace,MarkovOutcome>(), MarkovMatrixSideways_I = new HashMap<Trace,MarkovOutcome>();
	
	public Map<Trace, MarkovOutcome> getMarkov(boolean forwardOrSideways, boolean forwardExplorationOrInverse)
	{
		if (forwardExplorationOrInverse)
			return forwardOrSideways? MarkovMatrixForward_F:MarkovMatrixSideways_F;
		return forwardOrSideways? MarkovMatrixForward_I:MarkovMatrixSideways_I;
	}
	
	public Map<Trace, UpdatablePairInteger> getOccurrence(boolean forward)
	{
		return forward? occurrenceMatrixForward:occurrenceMatrixSideways;
	}
	
	public int getChunkLen()
	{
		return chunk_Length;
	}
	
	private final int chunk_Length;
    private LearnerGraph Extension_Graph;

    public MarkovUniversalLearner(int chunkLen)
    {
    	if (chunkLen < 2)
    		throw new IllegalArgumentException("chunkLen should be at least 2");
    	chunk_Length = chunkLen;
    }
    
    
    public static  class MarkovOutcome 
	{
		public final boolean isPositive, isFailure, isUnknown;
		
		protected MarkovOutcome(boolean pos,boolean failure, boolean unknown)
		{
			isPositive = pos;isFailure = failure;isUnknown = unknown;
		}
		public static MarkovOutcome failure=new MarkovOutcome(false, true,false), positive = new MarkovOutcome(true, false,false), negative = new MarkovOutcome(false, false,false),unknown = new MarkovOutcome(false, false,true);

		/** Given two outcomes of a prediction of a transition (any of which could be a null), computes the expected outcome where the two predictions are reconciled.
		 *  Unknown values are treated the same way as nulls.
		 *  
		 * @param a first opinion
		 * @param b second opinion
		 * @return outcome, possibly null where both opinions are null.
		 */
		public static MarkovOutcome reconcileOpinions_PosNeg_Overrides_Null(MarkovOutcome a, MarkovOutcome b)
		{
			MarkovOutcome outcome = null;

			if (a == failure || b == failure)
				outcome = failure;
			else
			if (a != null)
			{// b could be null
				if (a != unknown)
					outcome = a;

				if (b != null)
				{
					if (b != unknown && a != b)
						outcome = failure;
				}
			}
			else
				if (b != null && b != unknown)
				// a == null, b != null
					outcome = b;

			return outcome;
		}
		
		/** Given two outcomes of a prediction of a transition (any of which could be a null), computes the expected outcome where the two predictions are reconciled.
		 *  Unknown values are treated the same way as nulls.
		 *  
		 * @param a first opinion
		 * @param b second opinion
		 * @return outcome, possibly null where both opinions are null.
		 */
		public static MarkovOutcome reconcileOpinionsAllHaveToMatch(MarkovOutcome a, MarkovOutcome b)
		{
			MarkovOutcome outcome = null;

			if (a == failure || b == failure)
				outcome = failure;
			else
			if (a != null)
			{// b could be null
				if (a != unknown)
					outcome = a;

				if (b != null)
				{
					if (b != unknown && a != b)
						outcome = failure;
				}
				else
					// b is null a is not null
					outcome = null;
			}
			else
				if (b != null)
					outcome = failure;

			return outcome;
		}
		
		/** Given two outcomes of a prediction of a transition (any of which could be a null), computes the expected outcome. Reports a failure if any difference between opinions is observed.
		 * If any of the two is unknown, the other value overrides it.
		 * <p>
		 * The significance of this is that where we make a merge, a number of states get merged and hence there will be a number of paths leading to and from a state of interest. Markov will predict outgoing transitions
		 * based on those paths, relying on an entire graph as the source of information. These predictions may or may not match actual transitions, for each actual outgoing transition (pos/neg/non-existing) we might 
		 * like to match it with the predicted one and count the number of labels where predictions from one or more paths does not match the actual data (which will also imply that predictions contradict each other). 
		 * We could instead look for consistent predictions (where all paths to or from a state lead to the same prediction) and use those to check whether they contradict the actual data. 
		 * 
		 * @param a first opinion
		 * @param b second opinion
		 * @return outcome, possibly null where both opinions are null.
		 */
		public static MarkovOutcome ensureConsistencyBetweenOpinions(MarkovOutcome a, MarkovOutcome b)
		{
			MarkovOutcome outcome = null;

			if (a == failure || b == failure)
				outcome = failure;
			else
			if (a != null)
			{// b could be null
				
				if (a == unknown)
				{// unknown is overridden by b, whatever it is, including unknown
					outcome = b;
				}/*
				if (a == negative)
				{
					outcome = a;
					
					if (b != null)
					{
						if (b != unknown && a != b)
							outcome = failure;
					}
				}*/
				else
				{
					outcome = a;
	
					if (b != null)
					{
						if (b != unknown && a != b)
							outcome = failure;
					}
					else
						outcome = failure;// null v.s. non-null & not unknown
				}
			}
			else
				if (b != null)
				{
					if (b != unknown)
						outcome = failure;
				}
			return outcome;
		}
		
		@Override
		public String toString()
		{
			return "("+(isUnknown?"unknown":(isFailure?"failure":(isPositive?"+":"-")))+")";
		}
	}
    
	public static  class UpdatablePairInteger
	{
		public int firstElem, secondElem;
		public UpdatablePairInteger(int a, int b) {
			firstElem=a;secondElem=b;
		}
		
		public UpdatablePairInteger add(int a, int b)
		{
			firstElem+=a;secondElem+=b;return this;
		}
		
		public UpdatablePairInteger add(UpdatablePairInteger d)
		{
			add(d.firstElem,d.secondElem);return this;
		}
		
		@Override
		public String toString()
		{
			return "(pos: "+firstElem+", neg: "+secondElem+")";
		}		

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + firstElem;
			result = prime * result + secondElem;
			return result;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj instanceof UpdatablePairInteger))
				return false;
			UpdatablePairInteger other = (UpdatablePairInteger) obj;
			if (firstElem != other.firstElem)
				return false;
			if (secondElem != other.secondElem)
				return false;
			return true;
		}
	}
	
	/** Constructs the tables used by the learner, from positive and negative traces. Only builds Markov in the forward direction.
	 * 
	 * @param onlyLongest if set, only add traces of <i>chunkLen</i> to Markov matrix. Where false, all prefixes are added as well.
	 */
	public  Map<Trace, MarkovOutcome> createMarkovLearner(Collection<List<Label>> pos,Collection<List<Label>> neg, boolean onlyLongest)
	{
		int traceLength = 0;
		Set<Label> alphabet = new HashSet<Label>();
		for(List<Label> p:pos) 
		{ 
			for(Label l:p) alphabet.add(l);
			traceLength+=p.size();
		}
		for(List<Label> n:neg)
		{
			for(Label l:n) alphabet.add(l);
			traceLength+=n.size();
		}
		
		if (traceLength == 0)
			throw new IllegalArgumentException("empty trace data");
		
		// going through all positive traces
		//and partitioning each positive traces into a list of events ( a list of labels based on the chunk length)
		for(List<Label> positive_trace:pos)
		{
			Trace current_positive_trace=new Trace(positive_trace, true);
			for(int i=onlyLongest?chunk_Length-1:0;i<chunk_Length;i++)
			{
				List<Trace> List_traces=get_chunks(current_positive_trace,i+1);
				for (Trace tracePos:List_traces)
					initialization(tracePos,true);
			}
		}
		
		// from negative traces initialize the Markov matrix
		for(List<Label> negative_trace:neg)
		{
			for(int i=onlyLongest?chunk_Length-1:0; i<chunk_Length; i++)
			{
				Trace trace=new Trace(negative_trace,true);
				List<Trace> List_traces=get_chunks(trace,i+1);
				int chunkNumber = List_traces.size();
				if (chunkNumber >= 1)
				{
					Trace traceNeg=List_traces.get(chunkNumber-1);
					initialization(traceNeg,false);
					for (Trace tracePos:List_traces)
						if (tracePos != traceNeg)
							initialization(tracePos,true);
				}
			}
		}
		
		Map<Trace, MarkovOutcome> MarkovMatrix = getMarkov(true,true);
		// construct a matrix from trace data, including marking of conflicting data as invalid (conflicts arise where a path is too short). 
		// A prefix of either a positive or a negative could be a failure where there are some states from which a shorter sequence is rejected but from other states a longer one is accepted. 
		Trace trace_to_account_its_probability=null;
		for (Entry<Trace, UpdatablePairInteger> e : occurrenceMatrixForward.entrySet())
		{
			trace_to_account_its_probability=e.getKey();
			UpdatablePairInteger Trace_occurence = e.getValue();
			if (Trace_occurence.firstElem > 0 && Trace_occurence.secondElem > 0)
				MarkovMatrix.put(trace_to_account_its_probability, MarkovOutcome.failure);
			else
			if (Trace_occurence.firstElem > 0) 
				MarkovMatrix.put(trace_to_account_its_probability, MarkovOutcome.positive);
				else
					if (Trace_occurence.secondElem > 0)
						MarkovMatrix.put(trace_to_account_its_probability, MarkovOutcome.negative);
		}
		return MarkovMatrix;
	}

	protected void initialization(Trace traceToMarkov , boolean positive)
	{
		UpdatablePairInteger occurrence_of_trace=occurrenceMatrixForward.get(traceToMarkov);
		if (occurrence_of_trace == null)
		{
			occurrence_of_trace = new UpdatablePairInteger(0, 0);occurrenceMatrixForward.put(traceToMarkov,occurrence_of_trace);
		}
		
		if(positive)
			occurrence_of_trace.add(1,0);
		else  // if negative
			occurrence_of_trace.add(0,1);
	}

	/** This function is predicts transitions from each state and then adds them to the supplied graph.
	 *  
	 * @param tentativeAutomaton tentative Automaton 
	 * @param predictForwardOrSideways whether to make predictions forward or sideways
	 * @param highThreshold if the predicted probability of a transition is above this value, it seems plausible to add this transition.
	 * @param lowThreshold if the predicted probability of a transition is below this value, it is believed that the impact of this transition is insignificant.
	 */
	public void Markov_tentative(LearnerGraph tentativeAutomaton, boolean predictForwardOrSideways, double highThreshold, double lowThreshold)
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
		Extension_Graph= new LearnerGraph(shallowCopy);
		LearnerGraph.copyGraphs(tentativeAutomaton, Extension_Graph);
		Set<Label> allElementsOfAlphabet = tentativeAutomaton.learnerCache.getAlphabet(); 
		List<Label> pathToNewState=new LinkedList<Label>();
		// mapping map to store all paths leave each state in different length
		@SuppressWarnings("rawtypes")
		AbstractLearnerGraph Inverse_Graph = computeInverseGraph(tentativeAutomaton, predictForwardOrSideways);
		Map<Trace, UpdatablePairInteger> occurrenceMatrix=getOccurrence(predictForwardOrSideways);
    	for(Object vertObj:Inverse_Graph.transitionMatrix.keySet())
    	{
    		CmpVertex vert = (CmpVertex)vertObj;
    		if(vert.isAccept() )
            {
		        Map<Label,UpdatablePairDouble> outgoing_labels_probabilities=new HashMap<Label,UpdatablePairDouble>();
		        Map<Label,UpdatablePairInteger> outgoing_labels_occurrences=new HashMap<Label,UpdatablePairInteger>();
				LinkedList<FrontLineElem> ListofFrontElem=new LinkedList<FrontLineElem>();
				LinkedList<FrontLineElem> frontline = new LinkedList<FrontLineElem>();
	            FrontLineElem e=new FrontLineElem(new LinkedList<Label>(),vert);
			    frontline.add(e);
			    UpdatablePairInteger sum=new UpdatablePairInteger(0,0);
			    while(!frontline.isEmpty())
			    {
			    	e=frontline.pop();
			    	
			    	@SuppressWarnings("rawtypes")
					Map transitions = (Map)Inverse_Graph.transitionMatrix.get(e.currentState);
			    	for(Object lblObj:transitions.keySet())					
			    	{
			    		Label lbl = (Label)lblObj;
			    		for(Object targetObj:Inverse_Graph.getTargets(transitions.get(lbl)))
			    		{
			    			CmpVertex target = (CmpVertex)targetObj;
			    			pathToNewState=new ArrayList<Label>();
			    			pathToNewState.addAll(e.pathToFrontLine);
			    			pathToNewState.add(lbl);
			    			if(pathToNewState.size()==chunk_Length-1)
			    			{
			    				Trace path_chunk_minus_one=new Trace(pathToNewState,true);
			    				for(Label label:allElementsOfAlphabet)
			    				{
			    					Trace Predicted_trace= new Trace();Predicted_trace.getList().addAll(path_chunk_minus_one.getList());
			    					Collections.reverse(Predicted_trace.getList());
			    					Predicted_trace.add(label);

			    					UpdatablePairInteger occurrence_of_predicted_form_Markov=occurrenceMatrix.get(Predicted_trace);

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

    		// in this part the tree is extended depend on their outgoing transition probabilities
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
    	 					if(!Extension_Graph.transitionMatrix.get(currrent_state_to_explore_outgoing).keySet().contains(out.getKey()))
    	 					{
    	 						extendWithLabel(Extension_Graph,currrent_state_to_explore_outgoing, true, out.getKey());
    	 					}     					      
    	 				} 

    	 				if(out.getValue().secondElem >  highThreshold && out.getValue().firstElem <= lowThreshold && currrent_state_to_explore_outgoing.isAccept()==true)
    	 				{  
    	 					if(!Extension_Graph.transitionMatrix.get(currrent_state_to_explore_outgoing).keySet().contains(out.getKey()))
    	 					{
    	 						extendWithLabel(Extension_Graph,currrent_state_to_explore_outgoing, false, out.getKey());
    	 					}     					      
    	 				} 	 					
    	 			}					   
    	 		}          	       	      
    	 	}
    	}
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
	 * @param MarkovMatrix matrix to use in predictions
	 * @param Inverse_Graph graph used to compute all paths of specific length leading to a state of interest
	 * @param predictForwardOrSideways <i>true</i> if this is to predict forward (usual Markov) or <i>false</i> for sideways. 
	 * @param vert state of interest
	 * @param alphabet alphabet to use in predictions.
	 * @param pathBeyondCurrentState labels that are assumed to be at the tail of all paths leading to a state of interest. 
	 * Used in predictions where we are considering a PTA rooted at some real states. Each path in this PTA can the be passed as <i>pathBeyondCurrentState</i>. 
	 * @param chunkLength length of paths to consider (before the <i>pathBeyondCurrentState</i> component).
	 * @param pathsOfInterest paths considered for prediction. Ignored if <i>null</i>.
	 * Each such path had an outgoing label added and possibly <i>pathBeyondCurrentState</i> appended to it before being passed into Markov and the summary of the outcomes of such predictions is returned by this method.
	 * @return map from labels to predictions.
	 */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> Map<Label, MarkovOutcome> predictTransitionsFromState(Map<Trace, MarkovOutcome> MarkovMatrix, AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> Inverse_Graph, boolean predictForwardOrSideways, 
			CmpVertex vert, Collection<Label> alphabet, List<Label> pathBeyondCurrentState, int chunkLength,Collection<List<Label>> pathsOfInterest)
	{
		assert vert.isAccept();
		int lengthOfPathBeyond = pathBeyondCurrentState == null?0:pathBeyondCurrentState.size();
		if (lengthOfPathBeyond+1 > chunkLength)
			throw new IllegalArgumentException("supplied pathBeyondCurrentState is too long and does not permit exploration");
		if (!predictForwardOrSideways && lengthOfPathBeyond>0)
			throw new IllegalArgumentException("sideways predictions cannot be made by extension of earlier sideways predictions");

		Set<Label> failureLabels = new TreeSet<Label>();
		Map<Label,MarkovOutcome> outgoing_labels_probabilities=new HashMap<Label,MarkovOutcome>();
		LinkedList<FrontLineElem> frontline = new LinkedList<FrontLineElem>();
        FrontLineElem e=new FrontLineElem(new LinkedList<Label>(),vert);
        if (e.currentState.isAccept())
        	frontline.add(e);
        
	    while(!frontline.isEmpty())
	    {
	    	e=frontline.pop();	
			if(e.pathToFrontLine.size()==chunkLength-1-lengthOfPathBeyond)
			{
				if (pathsOfInterest != null)
					pathsOfInterest.add(e.pathToFrontLine);

				for(Label label:alphabet)
				{
					if (!failureLabels.contains(label))
					{// if the labels is not already recorded as being inconsistently predicted
						MarkovOutcome predictedFromEalierTrace = outgoing_labels_probabilities.get(label);
    					Trace Predicted_trace = new Trace();
    					if (predictForwardOrSideways)
    					{
    						for(int i=e.pathToFrontLine.size()-1;i>=0;--i) Predicted_trace.add(e.pathToFrontLine.get(i));if (pathBeyondCurrentState != null) Predicted_trace.getList().addAll(pathBeyondCurrentState);
    					}
    					else
    					{
    						Predicted_trace.getList().addAll(e.pathToFrontLine);
    					}
    					Predicted_trace.add(label);
    					
    					MarkovOutcome predicted_from_Markov=MarkovMatrix.get(Predicted_trace);
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
		    	for(Entry<Label, TARGET_TYPE> entry: Inverse_Graph.transitionMatrix.get(e.currentState).entrySet())				
		    	{		
		    		ArrayList<Label> pathToUseWithMarkovToPredictOutgoing=new ArrayList<Label>(chunkLength);
		    		pathToUseWithMarkovToPredictOutgoing.addAll(e.pathToFrontLine);
		    		pathToUseWithMarkovToPredictOutgoing.add(entry.getKey());

		    		for(CmpVertex target:Inverse_Graph.getTargets(entry.getValue()))
		    			if (target.isAccept())
		    				frontline.add(new FrontLineElem(pathToUseWithMarkovToPredictOutgoing,target));
		    	}
			}
	    }

	    return outgoing_labels_probabilities;
	}
	
	/** Updates Markov based on predictions. This is useful where we have added something to the original PTA and need to update Markov. Crucial for learning of Markov for sideways inference, where we cannot learn from the original
	 * traces and have to delay Markov construction to the time where PTA is built.
	 * <p>
	 * Note that computing Markov using incoming/outgoing paths of length 0 is just a distribution of letters, tagged with pos/neg/fail. 
	 * It is not based on earlier knowledge hence could be built either forwards or sideways in the same way.
	 *
	 * @param graph the graph to predict transitions for
	 * @param markovMatrix the matrix used for predictions
	 * @param Inverse_Graph graph used to make predictions
	 * @param predictForwardOrSideways whether to predict forward (<i>true</i>) or sideways (<i>false</i>).
	 * @param vert state to predict for
	 * @param alphabet alphabet of the graph of interest
	 * @param chunkLength how many steps to make a prediction for.
	 */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void updateMarkov(LearnerGraph graph, Map<Trace, MarkovOutcome> markovMatrix, 
			Map<Trace, UpdatablePairInteger> occurrenceMatrix, AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> Inverse_Graph, boolean predictForwardOrSideways, CmpVertex vert, Collection<Label> alphabet, int chunkLength)
	{
		List<List<Label>> markovPathsToUpdate = new LinkedList<List<Label>>();
		predictTransitionsFromState(markovMatrix,Inverse_Graph,predictForwardOrSideways,vert,Collections.<Label>emptySet(),null,chunkLength,markovPathsToUpdate);

	    // Now we iterate through all the labels and update entries in markovEntriesToUpdate depending on the outcome.
	    for(Label lbl:alphabet)
	    {
	    	CmpVertex target = graph.transitionMatrix.get(vert).get(lbl);
	    	if (target != null) // there is a transition with the considered label, hence update Markov
		    	for(List<Label> pathToUseWithMarkovToPredictOutgoing:markovPathsToUpdate)
		    	{
		    		Trace predictedTrace= new Trace();
					if (predictForwardOrSideways)
					{
						for(int i=pathToUseWithMarkovToPredictOutgoing.size()-1;i>=0;--i) predictedTrace.add(pathToUseWithMarkovToPredictOutgoing.get(i));
					}
					else
					{
						predictedTrace.getList().addAll(pathToUseWithMarkovToPredictOutgoing);
					}
					predictedTrace.add(lbl);
					
					MarkovOutcome newValue = null;
					UpdatablePairInteger p=occurrenceMatrix.get(predictedTrace);if (p == null) { p=new UpdatablePairInteger(0, 0);occurrenceMatrix.put(predictedTrace,p); }

					if (target.isAccept())
					{
						newValue=MarkovOutcome.positive;p.add(1, 0);
					}
					else
					{
						newValue=MarkovOutcome.negative;p.add(0, 1);
					}
					
					markovMatrix.put(predictedTrace, MarkovOutcome.reconcileOpinions_PosNeg_Overrides_Null(markovMatrix.get(predictedTrace),newValue));
		    	}
	    }
	}

	/** This function predicts transitions from each state and then adds predictions to the Markov model.
	 *  
	 * @param tentativeAutomaton tentative Automaton
	 * @param predictForwardOrSideways whether to update a forward or a sideways Markov matrix.
	 * @param directionForwardOrInverse whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and all its target states could be merged together.
	 * @param onlyLongest if set, only add traces of <i>chunkLen</i> to Markov matrix. Where false, all prefixes are added as well.
	 */
	public void updateMarkov(LearnerGraph tentativeAutomaton, boolean predictForwardOrSideways, boolean directionForwardOrInverse, boolean onlyLongest)
	{
		final Configuration shallowCopy = tentativeAutomaton.config.copy();shallowCopy.setLearnerCloneGraph(false);
		Set<Label> alphabet = tentativeAutomaton.learnerCache.getAlphabet();
		Map<Trace, MarkovOutcome> MarkovMatrix = getMarkov(predictForwardOrSideways,directionForwardOrInverse);
		Map<Trace, UpdatablePairInteger> occurrenceMatrix = getOccurrence(predictForwardOrSideways);
		@SuppressWarnings("rawtypes")
		AbstractLearnerGraph Inverse_Graph = computeInverseGraph(tentativeAutomaton, predictForwardOrSideways);
    	for(CmpVertex vert:tentativeAutomaton.transitionMatrix.keySet())
    		for(int len=onlyLongest?chunk_Length:1;len <=chunk_Length;++len)// this is very inefficient; we'll optimize it later if needed.
	           if(vert.isAccept())
	        	  updateMarkov(tentativeAutomaton,MarkovMatrix,occurrenceMatrix, Inverse_Graph,predictForwardOrSideways,vert,alphabet,len);
	}

	/** Given a graph, it uses the supplied collection of labels in order to identify states to merge, constructs a merge and counts the number of inconsistencies between the Markov-predicted vertices and the actual ones.
	 * The large number of arguments reflect the extent to which this process can be customised. 
	 * 
	 * @param graph graph to handle.
	 * @param graphsToCheckForPaths this is computed using {@link PairQualityLearner#constructPathsFromEachState(LearnerGraph, boolean)} and is expected to be cached between multiple invocations of this method.  
	 * @param predictForwardOrSideways whether to perform Markov-predictions forward (good for long paths) or sideways (many paths, not necessarily short).
	 * @param directionForwardOrInverse whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and all its target states could be merged together.
	 * @param paths paths to use.
	 * @param m Markov matrix to be used in predictions in order to compute inconsistency
	 * @param checker inconsistency checker. This one may consider only a subset of labels from each state (such as those labelling outgoing transitions) and make different decision as to what to count as an inconsistency.
	 * @return how inconsistent predictions are compared to the actual graph. Always non-negative.
	 */
	public static long computeInconsistencyForMergingPaths(LearnerGraph graph, Map<CmpVertex,LearnerGraph> graphsToCheckForPaths, boolean predictForwardOrSideways, boolean directionForwardOrInverse, Collection<List<Label>> paths, MarkovUniversalLearner m, ConsistencyChecker checker)
	{
		long outcome = 0;
		
		LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsList = PairQualityLearner.buildVerticesToMergeForPath(graphsToCheckForPaths,directionForwardOrInverse,paths);
		if (!pairsList.isEmpty())
		{
			int score = graph.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
			if (score < 0)
				outcome = MarkovUniversalLearner.dREJECT;
			else
			{
				LearnerGraph merged = MergeStates.mergeCollectionOfVertices(graph, null, verticesToMerge);
				outcome = computeInconsistency(merged, predictForwardOrSideways, directionForwardOrInverse, m, checker);
			}
		}
		
		return outcome;
	}
	
	/** Given a graph, it uses the supplied collection of labels in order to identify states to merge, constructs a merge and counts the number of inconsistencies between the Markov-predicted vertices and the actual ones.
	 * The large number of arguments reflect the extent to which this process can be customised. 
	 * <p>
	 * This is a special version of {@link #computeInconsistencyForMergingPath(LearnerGraph, Map, boolean, boolean, Collection, MarkovUniversalLearner, ConsistencyChecker)} for a single path.
	 * 
	 * @param graph graph to handle.
	 * @param graphsToCheckForPaths this is computed using {@link PairQualityLearner#constructPathsFromEachState(LearnerGraph, boolean)} and is expected to be cached between multiple invocations of this method.  
	 * @param predictForward whether to perform Markov-predictions forward (good for long paths) or sideways (many paths, not necessarily short).
	 * @param directionForward whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and 
	 * all its target states could be merged together.
	 * @param path a single path to use.
	 * @param m Markov matrix to be used in predictions in order to compute inconsistency
	 * @param checker inconsistency checker. This one may consider only a subset of labels from each state (such as those labelling outgoing transitions) and make different decision as to what to count as an inconsistency.
	 * @return how inconsistent predictions are compared to the actual graph. Always non-negative.
	 */
	public static long computeInconsistencyForMergingPath(LearnerGraph graph, Map<CmpVertex,LearnerGraph> graphsToCheckForPaths, boolean predictForward, boolean directionForward, List<Label> path, MarkovUniversalLearner m, ConsistencyChecker checker)
	{
		Collection<List<Label>> paths=new LinkedList<List<Label>>();paths.add(path);
		return computeInconsistencyForMergingPaths(graph, graphsToCheckForPaths, predictForward, directionForward, paths, m, checker);
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

		@Override
		public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") LearnerGraph graph,@SuppressWarnings("unused") CmpVertex v) {
			return graph.getCache().getAlphabet();
		}
	}
	
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

		@Override
		public Collection<Label> obtainAlphabet(LearnerGraph graph,CmpVertex v) {
			return graph.transitionMatrix.get(v).keySet();
		}
		
	}
	
	/** This one counts all inconsistencies but does not blacklist any label. */
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

		@Override
		public Collection<Label> obtainAlphabet(LearnerGraph graph,CmpVertex v) {
			return graph.transitionMatrix.get(v).keySet();
		}
		
	}

	/** Obtains the graph that can be used in calls of {@link #checkFanoutInconsistency(AbstractLearnerGraph, boolean, LearnerGraph, CmpVertex, int)} and many others.
	 * Returns an inverse when <i>predictForward</i> is true and <i>graph</i> otherwise.
	 * @param graph what to compute an inverse of
	 * @param predictForward whether to invert
	 * @return either an inverse or the original graph
	 */
	@SuppressWarnings("rawtypes")
	public static AbstractLearnerGraph computeInverseGraph(LearnerGraph graph,boolean predictForward)
	{
		AbstractLearnerGraph Inverse_Graph = null;
		if (predictForward)
		{
			Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
			LearnerGraphND Inverse = new LearnerGraphND(shallowCopy);
			AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse);  // do the inverse to the tentative graph
			Inverse_Graph = Inverse;
		}
		else
			Inverse_Graph = graph;

		return Inverse_Graph;
	}
	
	/** Determines how consistent a graph is compared to the data in the Markov model.
 	 * @param directionForwardOrInverse whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and all its target states could be merged together.
	 */
	@SuppressWarnings("unchecked")
	public static long computeInconsistency(LearnerGraph graph, boolean predictForwardOrSideways, boolean directionForwardOrInverse, MarkovUniversalLearner m, ConsistencyChecker checker)
	{
		return m.computeConsistency(computeInverseGraph(graph, predictForwardOrSideways), predictForwardOrSideways, directionForwardOrInverse, graph, m.getChunkLen(),checker);
	}

	/** Determines how consistent a graph is compared to the data in the Markov model.
 	 * @param directionForwardOrInverse whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and all its target states could be merged together.
	 */
	public <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> long computeConsistency(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> Inverse_Graph, boolean predictForwardOrSideways, boolean directionForwardOrInverse, LearnerGraph graph, int chunkLength, ConsistencyChecker checker)
	{
		long accumulatedInconsistency = 0;
		for(CmpVertex v:graph.transitionMatrix.keySet()) if (v.isAccept()) accumulatedInconsistency+=checkFanoutInconsistency(Inverse_Graph,predictForwardOrSideways, directionForwardOrInverse, graph,v,chunkLength, checker);
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
	 * 
	 * @param Inverse_Graph graph used to compute all paths of specific length leading to a state of interest
	 * @param predictForwardOrSideways <i>true</i> if this is to predict forward (usual Markov) or <i>false</i> for sideways. 
	 * @param directionForwardOrInverse whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and all its target states could be merged together.
	 * @param vert state of interest
	 * @param chunkLength length of paths to consider (before the <i>pathBeyondCurrentState</i> component).
	 * @param checker consistency checker, usually based on a static method from {@link MarkovOutcome}. 
	 */
	public <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> long checkFanoutInconsistency(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> Inverse_Graph, 
			boolean predictForwardOrSideways, boolean directionForwardOrInverse, LearnerGraph graph, CmpVertex vert, int chunkLength, ConsistencyChecker checker)
	{
		assert vert.isAccept();
		Collection<Label> outgoingLabels = checker.obtainAlphabet(graph,vert);
		Map<Trace, MarkovOutcome> MarkovMatrix = getMarkov(predictForwardOrSideways,directionForwardOrInverse);
		Map<Trace, UpdatablePairInteger> occurrenceMatrix = getOccurrence(predictForwardOrSideways);
		Map<Label,MarkovOutcome> outgoing_labels_value=new HashMap<Label,MarkovOutcome>();
		//for(Label l:alphabet) outgoing_labels_probabilities.put(l, UpdatableOutcome.unknown);
		long inconsistencies = 0;

		for(Entry<Label,CmpVertex> entry:graph.transitionMatrix.get(vert).entrySet())
		{
			outgoing_labels_value.put(entry.getKey(),entry.getValue().isAccept()?MarkovOutcome.positive:MarkovOutcome.negative);
		}
		LinkedList<FrontLineElem> frontline = new LinkedList<FrontLineElem>();
        FrontLineElem e=new FrontLineElem(new LinkedList<Label>(),vert);
	    frontline.add(e);
	    while(!frontline.isEmpty())
	    {
	    	e=frontline.pop();	
	    	for(Entry<Label, TARGET_TYPE> entry: Inverse_Graph.transitionMatrix.get(e.currentState).entrySet())					
	    	{		
	    		for(CmpVertex target:Inverse_Graph.getTargets(entry.getValue()))
	    		{
	    			ArrayList<Label> pathToUseWithMarkovToPredictOutgoing=new ArrayList<Label>(chunkLength);
	    			pathToUseWithMarkovToPredictOutgoing.addAll(e.pathToFrontLine);
	    			pathToUseWithMarkovToPredictOutgoing.add(entry.getKey());
	    			if(pathToUseWithMarkovToPredictOutgoing.size()==chunkLength-1)
	    			{
	    				List<Label> encounteredPartOfTrace = new ArrayList<Label>();
	    				
    					if (predictForwardOrSideways)
    					{
    						for(int i=pathToUseWithMarkovToPredictOutgoing.size()-1;i>=0;--i) encounteredPartOfTrace.add(pathToUseWithMarkovToPredictOutgoing.get(i));
    					}
    					else
    					{
    						encounteredPartOfTrace.addAll(pathToUseWithMarkovToPredictOutgoing);
    					}
    					if (occurrenceMatrix.containsKey(new Trace(encounteredPartOfTrace, true))) // we skip everything where a path was not seen in PTA.
		    				for(Label label:outgoingLabels)
		    				{
	    						MarkovOutcome labels_occurrence= outgoing_labels_value.get(label);
	    						if (labels_occurrence != MarkovOutcome.failure)
	    						{
			    					Trace traceToCheck = new Trace();traceToCheck.getList().addAll(encounteredPartOfTrace);
			    					traceToCheck.add(label);
		
			    					MarkovOutcome predicted_from_Markov=MarkovMatrix.get(traceToCheck);
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
	 * @param tentativeAutomaton tentative Automaton 
	 * @return a list of possible of outgoing transitions from each state
	 */
	public Map<CmpVertex, Map<Label, MarkovOutcome>> predictTransitions(LearnerGraph tentativeAutomaton, boolean predictForwardOrSideways, boolean directionForwardOrInverse)
	{
		/** Maps states to a function associating labels to a probability of a transition with the label of interest from a state of interest. Computed from {@link MarkovUniversalLearner#state_outgoing_occurence}. */
		Map<CmpVertex,Map<Label,MarkovOutcome>> state_outgoing=new HashMap<CmpVertex,Map<Label,MarkovOutcome>>();

		final Configuration shallowCopy = tentativeAutomaton.config.copy();shallowCopy.setLearnerCloneGraph(false);
		Set<Label> alphabet = tentativeAutomaton.learnerCache.getAlphabet(); 
		Map<Trace, MarkovOutcome> MarkovMatrix = getMarkov(predictForwardOrSideways, directionForwardOrInverse);
		// mapping map to store all paths leave each state in different length
		@SuppressWarnings("rawtypes")
		AbstractLearnerGraph Inverse_Graph = computeInverseGraph(tentativeAutomaton, predictForwardOrSideways);
		
    	for(CmpVertex vert:tentativeAutomaton.transitionMatrix.keySet())
    		if(vert.isAccept() )
            {
        	   Map<Label,MarkovOutcome> outgoing_labels_probabilities=predictTransitionsFromState(MarkovMatrix,Inverse_Graph,predictForwardOrSideways,vert,alphabet,null,chunk_Length,null);
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
	 * The implementation follows {@link MarkovUniversalLearner#predictTransitions(LearnerGraph, boolean)}.
	 * 
	 * @param tentativeAutomaton graph for which to compute normalised inconsistency
	 * @param predictForwardOrSideways how to make predictions.
	 * @param directionForwardOrInverse whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and all its target states could be merged together.
	 * @param checker the routine to check for inconsistencies.
	 * @return inconsistency
	 */
	public double countPossibleInconsistencies(LearnerGraph tentativeAutomaton, boolean predictForwardOrSideways, boolean directionForwardOrInverse, ConsistencyChecker checker)
	{
		Set<Label> alphabet = tentativeAutomaton.learnerCache.getAlphabet(); 
		Map<Trace, MarkovOutcome> MarkovMatrix = getMarkov(predictForwardOrSideways,directionForwardOrInverse);
		// mapping map to store all paths leave each state in different length
		@SuppressWarnings("rawtypes")
		AbstractLearnerGraph inverseGraph = computeInverseGraph(tentativeAutomaton, predictForwardOrSideways);
		List<List<Label>> collectionOfPaths = new ArrayList<List<Label>>();
		double countOfPossibleInconsistencies = 0;
		long actualInconsistency = computeConsistency(inverseGraph,predictForwardOrSideways,directionForwardOrInverse,tentativeAutomaton,chunk_Length,checker);
    	for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:tentativeAutomaton.transitionMatrix.entrySet())
    		if(entry.getKey().isAccept() )
            {
    			// it would be more efficient if I passed a mock of a collection instead of an actual one but for small graphs it does not matter.
    			predictTransitionsFromState(MarkovMatrix,inverseGraph,predictForwardOrSideways,entry.getKey(),alphabet,null,chunk_Length,collectionOfPaths);
    			countOfPossibleInconsistencies+=collectionOfPaths.size()*entry.getValue().size();collectionOfPaths.clear();
            }
    	return actualInconsistency/countOfPossibleInconsistencies;
	}
	
	/** This function is predicts transitions from each state and then adds them to the supplied graph.
	 * <ul>
	 * <li>
	 * Where <i>predictForward</i> is true, we are predicting transitions based on paths leading to the state of interest. Parameter <i>Inverse_Graph</i> should be the (non-deterministic) inverse of <i>graph</i>.
	 * </li>
	 * <li> 
	 * Where <i>predictForward</i> is false, we are predicting transitions based on paths leading from the state of interest (sideways predictions). Parameter <i>Inverse_Graph</i> should be the same as <i>graph</i>.
	 * </li>
	 * </ul>
	 * @param tentativeAutomaton tentative Automaton 
	 * @param predictForwardOrSideways how to make predictions.
	 * @param directionForwardOrInverse whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and all its target states could be merged together.
	 * @return a list of possible of outgoing transitions from each state
	 */
	public Map<CmpVertex, Map<Label, MarkovOutcome>> constructMarkovTentative(LearnerGraph tentativeAutomaton, boolean predictForwardOrSideways, boolean directionForwardOrInverse)
	{
		/** Maps states to a function associating labels to a probability of a transition with the label of interest from a state of interest. Computed from {@link MarkovUniversalLearner#state_outgoing_occurence}. */
		Map<CmpVertex,Map<Label,MarkovOutcome>> state_outgoing=predictTransitions(tentativeAutomaton,predictForwardOrSideways,directionForwardOrInverse);

		final Configuration shallowCopy = tentativeAutomaton.config.copy();shallowCopy.setLearnerCloneGraph(false);
		Extension_Graph= new LearnerGraph(shallowCopy);
		LearnerGraph.copyGraphs(tentativeAutomaton, Extension_Graph);

		// in this part the tree is extended depend on their outgoing transition probabilities
	 	for(Entry<CmpVertex, Map<Label, MarkovOutcome>> outgoing:state_outgoing.entrySet())
	 	{
	 		CmpVertex currrent_state_to_explore_outgoing= outgoing.getKey();
	 		Map<Label, MarkovOutcome> list_of_outgoing = outgoing.getValue();
	 		for(Entry<Label, MarkovOutcome> out:list_of_outgoing.entrySet())
	 		{
	 			Map<Label, CmpVertex> already_outgoing = tentativeAutomaton.transitionMatrix.get(currrent_state_to_explore_outgoing);
	 			assert already_outgoing!=null : "state "+currrent_state_to_explore_outgoing+" is not mentioned in the transition diagram";

	 			if(!already_outgoing.containsKey(out.getKey()) && out.getValue() != MarkovOutcome.failure)
	 			{  	   
 					if(!Extension_Graph.transitionMatrix.get(currrent_state_to_explore_outgoing).keySet().contains(out.getKey()))
 						extendWithLabel(Extension_Graph,currrent_state_to_explore_outgoing, out.getValue().isPositive, out.getKey());
	 			}					   
	 		}          	       	      
	 	}

      return state_outgoing;
	}

	public static void extendWithLabel(LearnerGraph what, CmpVertex prevState, boolean isAccept, Label input)
	{
		CmpVertex newVertex = AbstractLearnerGraph.generateNewCmpVertex(what.nextID(isAccept),what.config);
		assert !what.transitionMatrix.containsKey(newVertex);
		newVertex.setAccept(isAccept);
		what.transitionMatrix.put(newVertex, what.createNewRow());
		what.addTransition(what.transitionMatrix.get(prevState),input,newVertex);
	}
	
	public static List<Trace> get_chunks (Trace t,int chunkLen)
	{
		List<Trace> chunks=new ArrayList<Trace>();
	   	for(int f=0; f < t.size(); f++)
	    {
	   		if(f < (t.size()-chunkLen+1))
	   		{
	   			Trace traceToMarkov=new Trace(t.getList().subList(f, f+chunkLen), true); // get trace from the path
	   			chunks.add(traceToMarkov);
	   		}
	    }
	   	return chunks;
	}
	
	public  LearnerGraph get_extension_model() 
	{
		return Extension_Graph;
	}
	
	public  void set_extension_model(LearnerGraph l) 
	{
		 Extension_Graph=l;
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
	
	public int ask_questions(PairScore Pair, LearnerGraph graph, LearnerGraph extension_Graph2, LearnerGraph referenceGraph, LearnerGraph ptaHardFacts)
	{
		int numOfquestions=0;
		Collection<List<Label>> SmartQuestions = new LinkedList<List<Label>>();
	    SmartQuestions = computeSQS(Pair, graph, extension_Graph2);// questions generator from the current pair
	    if(SmartQuestions!=null && SmartQuestions.size() > 0) 
	    {
	    	// asking those questions 
			Iterator<List<Label>> questionIt = SmartQuestions.iterator();	
			while(questionIt.hasNext())
			{
				numOfquestions++;
    			List<Label> question = questionIt.next();
    			Pair<Integer,String> answer = null;
    			assert question!=null;
    			if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.ASSERT_ENABLED)))
    				if (ptaHardFacts.paths.tracePathPrefixClosed(question) == AbstractOracle.USER_ACCEPTED) 
    				{
    					throw new IllegalArgumentException("question "+ question+ " has already been answered");
    				}
    			answer = new Pair<Integer,String>(referenceGraph.paths.tracePathPrefixClosed(question),null);

          	    System.out.println("question:"+question+" and its answer: "+answer);
    			
			    if (answer.firstElem == AbstractOracle.USER_INCOMPATIBLE)
				{
			    	graph.addToCompatibility(Pair.firstElem, Pair.secondElem, PAIRCOMPATIBILITY.INCOMPATIBLE);
				}
				else if (answer.firstElem == AbstractOracle.USER_ACCEPTED) 
				{
					synchronized (AbstractLearnerGraph.syncObj) 
					{
						if(graph.getVertex(question).isAccept()==false)
							graph.getVertex(question).setAccept(true);
					}
				} 
				else if (answer.firstElem >= 0) 
				{
					synchronized (AbstractLearnerGraph.syncObj) 
					{
						if(graph.getVertex(question).isAccept()==true)
							graph.getVertex(question).setAccept(false);	
					}
				} 
			
				else
					throw new IllegalArgumentException("unexpected user choice "+answer);
			}
	    }
	    
	    return numOfquestions;
	}
	
	
	public  Collection<List<Label>> computeSQS(PairScore pair, LearnerGraph graph, LearnerGraph extension_Graph2) 
	{
		if(pair.firstElem.isAccept()!=true && pair.secondElem.isAccept()!=true)
			return null;
		
		List<Label> predicted_form_blue_node = new ArrayList<Label>();
		List<Label> predicted_form_red_node = new ArrayList<Label>();
		Collection<List<Label>> questionAdded = new ArrayList<List<Label>>();
		Set<Label> outgoing_form_blue_node = graph.transitionMatrix.get(pair.getQ()).keySet();
		Set<Label> outgoing_form_red_node = graph.transitionMatrix.get(pair.getR()).keySet();
		if(extension_Graph2.transitionMatrix.get(pair.getQ())!=null)
			predicted_form_blue_node = new ArrayList<Label>(extension_Graph2.transitionMatrix.get(pair.getQ()).keySet());
		if(extension_Graph2.transitionMatrix.get(pair.getR())!=null)
			predicted_form_red_node = new ArrayList<Label>(extension_Graph2.transitionMatrix.get(pair.getR()).keySet());
		
		predicted_form_blue_node.removeAll(outgoing_form_blue_node);predicted_form_red_node.removeAll(outgoing_form_red_node);
		
		for(Label out_red:outgoing_form_red_node)
		{			
			if(!outgoing_form_blue_node.contains(out_red) && predicted_form_blue_node.contains(out_red))  // if outgoing transitions from a red node exist in a blue state
			{		
				Collection<List<Label>> existingPathToCurrentState = graph.pathroutines.computePathsToRed(pair.getQ());
				for(List<Label> q:existingPathToCurrentState)
				{
					List<Label> added_question= new ArrayList<Label>();
					added_question.addAll(q);
					added_question.add(out_red);
					questionAdded.add(added_question);
				}	
			}					
		}
		
		for(Label out_blue:outgoing_form_blue_node)
		{			
			if(!outgoing_form_red_node.contains(out_blue) && predicted_form_red_node.contains(out_blue))  // if outgoing transitions from a red node exist in a blue state
			{			
				Collection<List<Label>> existingPathToCurrentState = graph.pathroutines.computePathsToRed(pair.getR());
				for(List<Label> q:existingPathToCurrentState)
				{
					List<Label> added_question= new ArrayList<Label>();
					added_question.addAll(q);
					added_question.add(out_blue);
					questionAdded.add(added_question);
				}	
			}				
		}
		
		return questionAdded;
	}
	
	
	public static final double fREJECT = -1;
	public static final long dREJECT = -1;
	
	public double computeMMScoreImproved(PairScore P, LearnerGraph coregraph)
	{
		double score = 0;
		Set<Label> outgoing_from_blue_node = coregraph.transitionMatrix.get(P.getQ()).keySet();
		Set<Label> outgoing_from_red_node = coregraph.transitionMatrix.get(P.getR()).keySet();						
		Set<Label> predicted_from_blue_node = Extension_Graph.transitionMatrix.get(P.getQ()).keySet();
		Set<Label> predicted_from_red_node = Extension_Graph.transitionMatrix.get(P.getR()).keySet();
		
		Set<Label> all_outgoing = new HashSet<Label>() ;
		all_outgoing.addAll(predicted_from_red_node);
		all_outgoing.addAll(predicted_from_blue_node);
		if (all_outgoing.isEmpty())
			return 0;
		
		for(Label out_red:outgoing_from_red_node)
		{			
			if(predicted_from_blue_node.contains(out_red))  // if outgoing transitions from a red node exist in a blue state
			{
				boolean target_from_red_acceptance  = coregraph.getTransitionMatrix().get(P.getR()).get(out_red).isAccept();
				boolean target_form_blue_acceptance = Extension_Graph.getTransitionMatrix().get(P.getQ()).get(out_red).isAccept();	
	    		if(target_form_blue_acceptance  ==  target_from_red_acceptance )	
	    			score++;	
	    		else
	    			return fREJECT;
			}
			else
				return fREJECT;
		}
				
		for(Label out_blue:outgoing_from_blue_node)
		{			
			if(predicted_from_red_node.contains(out_blue))  // if outgoing transitions from a red node exist in a blue state
			{	
				boolean target_from_red_acceptance  = Extension_Graph.getTransitionMatrix().get(P.getR()).get(out_blue).isAccept();
				boolean target_form_blue_acceptance = coregraph.getTransitionMatrix().get(P.getQ()).get(out_blue).isAccept();
	    		if(target_form_blue_acceptance  ==  target_from_red_acceptance )
	    			score++;
	    		else
	    			return fREJECT;
			}
			else
				return fREJECT;
		}		
		return score+(score/all_outgoing.size());		
	}
	
	public static long computeScoreSicco(LearnerGraph original,StatePair pair)
	{
		assert pair.getQ() != pair.getR();
		assert original.transitionMatrix.containsKey(pair.firstElem);
		assert original.transitionMatrix.containsKey(pair.secondElem);
		Map<CmpVertex,List<CmpVertex>> mergedVertices = original.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,List<CmpVertex>>(original.getStateNumber()):
				new HashMapWithSearch<CmpVertex,List<CmpVertex>>(original.getStateNumber());
		Configuration shallowCopy = original.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph result = new LearnerGraph(original,shallowCopy);
		assert result.transitionMatrix.containsKey(pair.firstElem);
		assert result.transitionMatrix.containsKey(pair.secondElem);

		long pairScore = original.pairscores.computePairCompatibilityScore_internal(pair,mergedVertices);
		if (pairScore < 0)
			throw new IllegalArgumentException("elements of the pair are incompatible");

		Map<CmpVertex,Collection<Label>> labelsAdded = new TreeMap<CmpVertex,Collection<Label>>();
		
		// make a loop
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:original.transitionMatrix.entrySet())
		{
			for(Entry<Label,CmpVertex> rowEntry:entry.getValue().entrySet())
				if (rowEntry.getValue() == pair.getQ())
				{
					// the transition from entry.getKey() leads to the original blue state, record it to be rerouted.
					result.transitionMatrix.get(entry.getKey()).put(rowEntry.getKey(), pair.getR());
				}
		}
		
		Set<CmpVertex> ptaVerticesUsed = new HashSet<CmpVertex>();
		Set<Label> inputsUsed = new HashSet<Label>();

		// I iterate over the elements of the original graph in order to be able to update the target one.
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:original.transitionMatrix.entrySet())
		{
			CmpVertex vert = entry.getKey();
			Map<Label,CmpVertex> resultRow = result.transitionMatrix.get(vert);// the row we'll update
			if (mergedVertices.containsKey(vert))
			{// there are some vertices to merge with this one.
				Collection<Label> newLabelsAddedToVert = labelsAdded.get(entry.getKey());
				if (newLabelsAddedToVert == null)
				{
					newLabelsAddedToVert = new TreeSet<Label>();labelsAdded.put(entry.getKey(), newLabelsAddedToVert);
				}

				inputsUsed.clear();inputsUsed.addAll(entry.getValue().keySet());// the first entry is either a "derivative" of a red state or a branch of PTA into which we are now merging more states.
				for(CmpVertex toMerge:mergedVertices.get(vert))
				{// for every input, I'll have a unique target state - this is a feature of PTA
				 // For this reason, every if multiple branches of PTA get merged, there will be no loops or parallel edges.
				// As a consequence, it is safe to assume that each input/target state combination will lead to a new state
				// (as long as this combination is the one _not_ already present from the corresponding red state).
					boolean somethingWasAdded = false;
					for(Entry<Label,CmpVertex> input_and_target:original.transitionMatrix.get(toMerge).entrySet())
						if (!inputsUsed.contains(input_and_target.getKey()))
						{
							// We are adding a transition to state vert with label input_and_target.getKey() and target state input_and_target.getValue();
							resultRow.put(input_and_target.getKey(), input_and_target.getValue());
							
							newLabelsAddedToVert.add(input_and_target.getKey());
							
							inputsUsed.add(input_and_target.getKey());
							ptaVerticesUsed.add(input_and_target.getValue());somethingWasAdded = true;
							// Since PTA is a tree, a tree rooted at ptaVerticesUsed will be preserved in a merged automaton, however 
							// other parts of a tree could be merged into it. In this case, each time there is a fork corresponding to 
							// a step by that other chunk which the current tree cannot follow, that step will end in a tree and a root
							// of that tree will be added to ptaVerticesUsed.
						}
					assert somethingWasAdded : "RedAndBlueToBeMerged was not set correctly at an earlier stage";
				}
			}
		}
		
		if (labelsAdded.containsKey(pair.getR()) && !labelsAdded.get(pair.getR()).isEmpty())
			return -1;
		
		return 0;
	}
	
	
	public long computeMarkovScoring(PairScore pair, LearnerGraph graph, LearnerGraph extension_model, int chunkLen)
	{
		Map<CmpVertex,List<CmpVertex>> mergedVertices = graph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,List<CmpVertex>>():
				new HashMapWithSearch<CmpVertex,List<CmpVertex>>(graph.getStateNumber());
		if (!AbstractLearnerGraph.checkCompatible(pair.getR(),pair.getQ(),graph.pairCompatibility))
			return -1;
		if(pair.getR().isAccept()==false && pair.getQ().isAccept()==false)
			return 1;
		
		if(graph.pairscores.computePairCompatibilityScore_internal(pair,mergedVertices) < 0)
			return -1;		
		
		if (computeScoreSicco(graph,pair) < 0 && pair.getQ().getDepth()<chunkLen && pair.getR().getDepth() < chunkLen)
			return  -1;
		long matchscore= 0;
		assert pair.getQ() != pair.getR();
			
		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
		currentExplorationBoundary.add(pair);currentExplorationBoundary.offer(null);
			
		while(!currentExplorationBoundary.isEmpty())
		{
			StatePair currentPair = currentExplorationBoundary.remove();
			if (currentPair == null)
			{// we got to the end of a wave
				if (currentExplorationBoundary.isEmpty())
					break;// we are at the end of the last wave, stop looping.

				// mark the end of a wave.
				currentExplorationBoundary.offer(null);
			}
			else
			{
				Map<Label,CmpVertex> targetRed = graph.transitionMatrix.get(currentPair.getR()),
						targetBlue = graph.transitionMatrix.get(currentPair.getQ());
		
					for(Entry<Label,CmpVertex> redEntry:targetRed.entrySet())
					{
						CmpVertex nextBlueState = targetBlue.get(redEntry.getKey());
						if (nextBlueState != null)
						{// both states can make a transition
							if (!AbstractLearnerGraph.checkCompatible(redEntry.getValue(),nextBlueState,graph.pairCompatibility))
								return -1;// incompatible states
																		
							List<Label> outgoing_form_blue_node = new ArrayList<Label>(graph.transitionMatrix.get(currentPair.getQ()).keySet());
							List<Label> outgoing_form_red_node = new ArrayList<Label>(graph.transitionMatrix.get(currentPair.getR()).keySet());	
							List<Label> exoutgoing_form_blue_node = new ArrayList<Label>(extension_model.transitionMatrix.get(currentPair.getQ()).keySet());
							List<Label> exoutgoing_form_red_node = new ArrayList<Label>(extension_model.transitionMatrix.get(currentPair.getR()).keySet());	
							Set<Label> all_outgoing = new HashSet<Label>() ;
							all_outgoing.addAll(exoutgoing_form_blue_node);
							all_outgoing.addAll(exoutgoing_form_red_node);

							for(Label out_red:outgoing_form_red_node)
							{	
								Boolean target_from_red_acceptance  = graph.getTransitionMatrix().get(currentPair.getR()).get(out_red).isAccept();		
								if(outgoing_form_blue_node.contains(out_red))  
								{				
							    	Boolean target_form_blue_acceptance = graph.getTransitionMatrix().get(currentPair.getQ()).get(out_red).isAccept();	
							    	assert target_from_red_acceptance!=null; assert target_form_blue_acceptance!=null;			

						    		if(target_form_blue_acceptance ==  target_from_red_acceptance )		
										matchscore++;	
						    		else
							    		return -1;	
								}
								else if(exoutgoing_form_blue_node.contains(out_red) && target_from_red_acceptance.booleanValue())
								{
							    	Boolean extensiontarget_form_blue_acceptance = extension_model.getTransitionMatrix().get(currentPair.getQ()).get(out_red).isAccept();	
									if(extensiontarget_form_blue_acceptance == true && target_from_red_acceptance==true )		
										matchscore++;	
								}
								else
								{
									return -1;
								}
							}
							
							for(Label out_blue:outgoing_form_blue_node)
							{
								Boolean target_form_blue_acceptance = graph.getTransitionMatrix().get(currentPair.getQ()).get(out_blue).isAccept();	

								if(exoutgoing_form_red_node.contains(out_blue) && target_form_blue_acceptance.booleanValue())
								{
							    	Boolean extensiontarget_form_red_acceptance = extension_model.getTransitionMatrix().get(currentPair.getR()).get(out_blue).isAccept();	
									if(extensiontarget_form_red_acceptance == true && target_form_blue_acceptance ==true )		
										matchscore++;
								}
								else
								{
									return -1;
								}
								
							}
							StatePair nextStatePair = new StatePair(nextBlueState,redEntry.getValue());
							currentExplorationBoundary.offer(nextStatePair);
							}	
						
						// if the red can make a move, but the blue one cannot, ignore this case.
					}
				}
			}     
			return matchscore;
	}	
	public static Collection<CmpVertex> numOFsimilarRED(Stack<PairScore> possibleMerges)
	{
		Set<CmpVertex> reds = new HashSet<CmpVertex>();// was: new LinkedHashSet<CmpVertex>();
		for(PairScore v:possibleMerges)
			if(v.firstElem.isAccept()==v.secondElem.isAccept())
			if (v.secondElem.getColour() == JUConstants.RED )
				reds.add(v.secondElem);
		return reds;
	}
	
	public static Collection<CmpVertex> numOFsimilarBLUE(Stack<PairScore> possibleMerges)
	{
		Set<CmpVertex> blues = new HashSet<CmpVertex>();// was: new LinkedHashSet<CmpVertex>();
		for(PairScore v:possibleMerges)
			if(v.firstElem.isAccept()== v.secondElem.isAccept())
			if (v.secondElem.getColour() == JUConstants.RED )
				blues.add(v.firstElem);
		return blues;
	}

}