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
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.collections.ArrayMapWithSearch;
import statechum.collections.HashMapWithSearch;

public class MarkovUniversalLearner
{
	private Map<Trace, UpdatablePairInteger> occurrenceMatrix =  new HashMap<Trace,UpdatablePairInteger>();
	private Map<Trace, UpdatableOutcome> MarkovMatrix =  new HashMap<Trace,UpdatableOutcome>();
	
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
    
    
    public static  class UpdatableOutcome 
	{
		public final boolean isPositive, isFailure, isUnknown;
		
		protected UpdatableOutcome(boolean pos,boolean failure, boolean unknown)
		{
			isPositive = pos;isFailure = failure;isUnknown = unknown;
		}
		public static UpdatableOutcome failure=new UpdatableOutcome(false, true,false), positive = new UpdatableOutcome(true, false,false), negative = new UpdatableOutcome(false, false,false),unknown = new UpdatableOutcome(false, false,true);

		/** Given two outcomes of a prediction of a transition (any of which could be a null), computes the expected outcome where the two predictions are reconciled.
		 *  Unknown values are treated the same way as nulls.
		 *  
		 * @param a first opinion
		 * @param b second opinion
		 * @return outcome, possibly null where both opinions are null.
		 */
		public static UpdatableOutcome reconcileOpinions_PosNeg_Overrides_Null(UpdatableOutcome a, UpdatableOutcome b)
		{
			UpdatableOutcome outcome = null;

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
		public static UpdatableOutcome reconcileOpinionsAllHaveToMatch(UpdatableOutcome a, UpdatableOutcome b)
		{
			UpdatableOutcome outcome = null;

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
		 * 
		 * @param a first opinion
		 * @param b second opinion
		 * @return outcome, possibly null where both opinions are null.
		 */
		public static UpdatableOutcome ensureConsistencyBetweenOpinions(UpdatableOutcome a, UpdatableOutcome b)
		{
			UpdatableOutcome outcome = null;

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
	}
	
	/** Constructs the tables used by the learner, from positive and negative traces.
	 */
	public  Map<Trace, UpdatableOutcome> createMarkovLearner(Collection<List<Label>> pos,Collection<List<Label>> neg)
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
			Trace current_positive_trace=new Trace(positive_trace);
			for(int i=0;i<chunk_Length;i++)
			{
				List<Trace> List_traces=get_chunks(current_positive_trace,i+1);
				for (Trace tracePos:List_traces)
					initialization(tracePos,true);
			}
		}
		
		// from negative traces initialize the Markov matrix
		for(List<Label> negative_trace:neg)
		{
			for(int i=0; i<chunk_Length; i++)
			{
				Trace trace=new Trace(negative_trace);
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

		Trace trace_to_account_its_probability=null;
		for (Entry<Trace, UpdatablePairInteger> e : occurrenceMatrix.entrySet())
		{
			trace_to_account_its_probability=e.getKey();
			UpdatablePairInteger Trace_occurence = e.getValue();
			if (Trace_occurence.firstElem > 0 && Trace_occurence.secondElem > 0)
				MarkovMatrix.put(trace_to_account_its_probability, UpdatableOutcome.failure);
			else
			if (Trace_occurence.firstElem > 0) 
				MarkovMatrix.put(trace_to_account_its_probability, UpdatableOutcome.positive);
				else
					if (Trace_occurence.secondElem > 0)
						MarkovMatrix.put(trace_to_account_its_probability, UpdatableOutcome.negative);
		}
		return MarkovMatrix;
	}

	protected void initialization(Trace traceToMarkov , boolean positive)
	{
		UpdatablePairInteger occurrence_of_trace=occurrenceMatrix.get(traceToMarkov);
		if (occurrence_of_trace == null)
		{
			occurrence_of_trace = new UpdatablePairInteger(0, 0);occurrenceMatrix.put(traceToMarkov,occurrence_of_trace);
		}
		
		if(positive)
			occurrence_of_trace.add(1,0);
		else  // if negative
			occurrence_of_trace.add(0,1);
	}

	/** This function is predicts transitions from each state and then adds them to the supplied graph.
	 *  
	 * @param tentativeAutomaton tentative Automaton 
	 * @param highThreshold if the predicted probability of a transition is above this value, it seems plausible to add this transition.
	 * @param lowThreshold if the predicted probability of a transition is below this value, it is believed that the impact of this transition is insignificant.
	 */
	public void Markov_tentative(LearnerGraph tentativeAutomaton, double highThreshold, double lowThreshold)
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
		Set<Label> all_alphabets = tentativeAutomaton.learnerCache.getAlphabet(); 
		List<Label> PathToNewState=new LinkedList<Label>();
		// mapping map to store all paths leave each state in different length
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(tentativeAutomaton,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
    	for(CmpVertex vert:Inverse_Graph.transitionMatrix.keySet())
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
			    	for(Entry<Label, List<CmpVertex>> entry: Inverse_Graph.transitionMatrix.get(e.currentState).entrySet())					
			    	{		
			    		for(CmpVertex target:entry.getValue())
			    		{
			    			PathToNewState=new ArrayList<Label>();
			    			PathToNewState.addAll(e.pathToFrontLine);
			    			PathToNewState.add(entry.getKey());
			    			if(PathToNewState.size()==chunk_Length-1)
			    			{
			    				Trace path_chunk_minus_one=new Trace(PathToNewState);
			    				for(Label label:all_alphabets)
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
			    				frontline.add(new FrontLineElem(PathToNewState,target));
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
	
	
	
	public Map<Label, UpdatableOutcome> predictTransitionsFromState(LearnerGraphND Inverse_Graph, CmpVertex vert, Collection<Label> alphabet, List<Label> pathBeyondCurrentState, int chunkLength)
	{
		assert vert.isAccept();
		int lengthOfPathBeyond = pathBeyondCurrentState == null?0:pathBeyondCurrentState.size();
		if (lengthOfPathBeyond+2 > chunkLength)
			throw new IllegalArgumentException("supplied pathBeyondCurrentState is too long and does not permit exploration");
		Set<Label> failureLabels = new TreeSet<Label>();
		Map<Label,UpdatableOutcome> outgoing_labels_probabilities=new HashMap<Label,UpdatableOutcome>();
		LinkedList<FrontLineElem> frontline = new LinkedList<FrontLineElem>();
        FrontLineElem e=new FrontLineElem(new LinkedList<Label>(),vert);
	    frontline.add(e);
	    while(!frontline.isEmpty())
	    {
	    	e=frontline.pop();	
	    	for(Entry<Label, List<CmpVertex>> entry: Inverse_Graph.transitionMatrix.get(e.currentState).entrySet())					
	    	{		
	    		for(CmpVertex target:entry.getValue())
	    		{
	    			ArrayList<Label> PathToNewState=new ArrayList<Label>(chunkLength);
	    			PathToNewState.addAll(e.pathToFrontLine);
	    			PathToNewState.add(entry.getKey());
	    			if(PathToNewState.size()==chunkLength-1-lengthOfPathBeyond)
	    			{
	    				for(Label label:alphabet)
	    				{
	    					if (!failureLabels.contains(label))
	    					{// if the labels is not already recorded as being inconsistently predicted
	    						UpdatableOutcome predictedFromEalierTrace = outgoing_labels_probabilities.get(label);
		    					Trace Predicted_trace= new Trace();
		    					for(int i=PathToNewState.size()-1;i>=0;--i) Predicted_trace.add(PathToNewState.get(i));if (pathBeyondCurrentState != null) Predicted_trace.getList().addAll(pathBeyondCurrentState);
		    					Predicted_trace.add(label);
		    					
		    					UpdatableOutcome predicted_from_Markov=MarkovMatrix.get(Predicted_trace);
	    						UpdatableOutcome outcome = UpdatableOutcome.reconcileOpinions_PosNeg_Overrides_Null(predictedFromEalierTrace, predicted_from_Markov);
	    						if (outcome != predictedFromEalierTrace)
	    						{// we learnt something new, be it a new value (or a non-null value) or a failure, record it
	    							if (outcome == UpdatableOutcome.failure)
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
	    				frontline.add(new FrontLineElem(PathToNewState,target));
	    			}
	    		}
	    	}
	    }

	    return outgoing_labels_probabilities;
	}
	
	public void predictTransitionsFromStateAndUpdateMarkov(LearnerGraphND Inverse_Graph, CmpVertex vert, Collection<Label> alphabet, List<Label> pathBeyondCurrentState, int chunkLength)
	{
		assert vert.isAccept();
		int lengthOfPathBeyond = pathBeyondCurrentState == null?0:pathBeyondCurrentState.size();
		if (lengthOfPathBeyond+2 > chunkLength)
			throw new IllegalArgumentException("supplied pathBeyondCurrentState is too long and does not permit exploration");
		Set<Label> failureLabels = new TreeSet<Label>();
		Map<Label,UpdatableOutcome> outgoing_labels_probabilities=new HashMap<Label,UpdatableOutcome>();
		LinkedList<FrontLineElem> frontline = new LinkedList<FrontLineElem>();
        FrontLineElem e=new FrontLineElem(new LinkedList<Label>(),vert);
        List<List<Label>> markovEntriesToUpdate = new LinkedList<List<Label>>();
        
	    frontline.add(e);
	    while(!frontline.isEmpty())
	    {
	    	e=frontline.pop();	
	    	for(Entry<Label, List<CmpVertex>> entry: Inverse_Graph.transitionMatrix.get(e.currentState).entrySet())					
	    	{		
	    		for(CmpVertex target:entry.getValue())
	    		{
	    			ArrayList<Label> PathToNewState=new ArrayList<Label>(chunkLength);
	    			PathToNewState.addAll(e.pathToFrontLine);
	    			PathToNewState.add(entry.getKey());
	    			if(PathToNewState.size()==chunkLength-1-lengthOfPathBeyond)
	    			{
	    				for(Label label:alphabet)
	    				{
	    					if (!failureLabels.contains(label))
	    					{// if the labels is not already recorded as being inconsistently predicted
	    						UpdatableOutcome predictedFromEalierTrace = outgoing_labels_probabilities.get(label);
		    					Trace Predicted_trace= new Trace();
		    					for(int i=PathToNewState.size()-1;i>=0;--i) Predicted_trace.add(PathToNewState.get(i));if (pathBeyondCurrentState != null) Predicted_trace.getList().addAll(pathBeyondCurrentState);
		    					Predicted_trace.add(label);
		    					
		    					UpdatableOutcome predicted_from_Markov=MarkovMatrix.get(Predicted_trace);
		    					if (predicted_from_Markov != UpdatableOutcome.failure)
		    					{
			    					markovEntriesToUpdate.add(PathToNewState);
		    						UpdatableOutcome outcome = UpdatableOutcome.reconcileOpinions_PosNeg_Overrides_Null(predictedFromEalierTrace, predicted_from_Markov);
		    						if (outcome != predictedFromEalierTrace)
		    						{// we learnt something new, be it a new value (or a non-null value) or a failure, record it
		    							if (outcome == UpdatableOutcome.failure)
		    								failureLabels.add(label);
	    								outgoing_labels_probabilities.put(label, outcome);
		    						}
		    					}
	    					}
	    				}
	    			}
	    			else
	    			{// not reached the maximal length of paths to explore
	    				frontline.add(new FrontLineElem(PathToNewState,target));
	    			}
	    		}
	    	}
	    }

	    // Now we iterate through all the labels and update entries in markovEntriesToUpdate depending on the outcome.
	    for(Entry<Label,UpdatableOutcome> computedValues:outgoing_labels_probabilities.entrySet())
	    	if (computedValues.getValue() != null)
		    {// we have a definite value
		    	for(List<Label> PathToNewState:markovEntriesToUpdate)
		    	{
		    		Trace Predicted_trace= new Trace();
					for(int i=PathToNewState.size()-1;i>=0;--i) Predicted_trace.add(PathToNewState.get(i));if (pathBeyondCurrentState != null) Predicted_trace.getList().addAll(pathBeyondCurrentState);
					Predicted_trace.add(computedValues.getKey());
					
					//if (MarkovMatrix.get(Predicted_trace) != computedValues.getValue()) // the one next is good for breakpoint, otherwise this line makes no sense
					MarkovMatrix.put(Predicted_trace, computedValues.getValue());// includes failures because null means "missing value", often interpreted as "most likely, not".
		    	}
		    }
	}

	/** This function is predicts transitions from each state and then adds predictions to the Markov model.
	 *  
	 * @param tentativeAutomaton tentative Automaton 
	 */
	public void predictTransitionsAndUpdateMarkov(LearnerGraph tentativeAutomaton)
	{
		final Configuration shallowCopy = tentativeAutomaton.config.copy();shallowCopy.setLearnerCloneGraph(false);
		Set<Label> alphabet = tentativeAutomaton.learnerCache.getAlphabet(); 
		// mapping map to store all paths leave each state in different length
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(tentativeAutomaton,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
    	for(CmpVertex vert:Inverse_Graph.transitionMatrix.keySet())
           if(vert.isAccept() )
        	  predictTransitionsFromStateAndUpdateMarkov(Inverse_Graph,vert,alphabet,null,chunk_Length);
	}	

	protected static UpdatablePairInteger zero=new UpdatablePairInteger(0,0);
	
	public double computeConsistency(LearnerGraphND Inverse_Graph, LearnerGraph graph, int chunkLength)
	{
		double accumulatedInconsistency = 0;
		for(CmpVertex v:graph.transitionMatrix.keySet()) if (v.isAccept()) accumulatedInconsistency+=checkFanoutInconsistency(Inverse_Graph,graph,v,chunkLength);
		return accumulatedInconsistency;
	}
	
	public int checkFanoutInconsistency(LearnerGraphND Inverse_Graph, LearnerGraph graph, CmpVertex vert, int chunkLength)
	{
		assert vert.isAccept();
		Set<Label> outgoingLabels = graph.transitionMatrix.get(vert).keySet();
		Map<Label,UpdatableOutcome> outgoing_labels_value=new HashMap<Label,UpdatableOutcome>();
		//for(Label l:alphabet) outgoing_labels_probabilities.put(l, UpdatableOutcome.unknown);
		int inconsistencies = 0;

		for(Entry<Label,CmpVertex> entry:graph.transitionMatrix.get(vert).entrySet())
		{
			outgoing_labels_value.put(entry.getKey(),entry.getValue().isAccept()?UpdatableOutcome.positive:UpdatableOutcome.negative);
		}
		LinkedList<FrontLineElem> frontline = new LinkedList<FrontLineElem>();
        FrontLineElem e=new FrontLineElem(new LinkedList<Label>(),vert);
	    frontline.add(e);
	    while(!frontline.isEmpty())
	    {
	    	e=frontline.pop();	
	    	for(Entry<Label, List<CmpVertex>> entry: Inverse_Graph.transitionMatrix.get(e.currentState).entrySet())					
	    	{		
	    		for(CmpVertex target:entry.getValue())
	    		{
	    			ArrayList<Label> PathToNewState=new ArrayList<Label>(chunkLength);
	    			PathToNewState.addAll(e.pathToFrontLine);
	    			PathToNewState.add(entry.getKey());
	    			if(PathToNewState.size()==chunkLength-1)
	    			{
	    				for(Label label:outgoingLabels)
	    				{
    						UpdatableOutcome labels_occurrence= outgoing_labels_value.get(label);
    						if (labels_occurrence != UpdatableOutcome.failure)
    						{
		    					Trace Predicted_trace= new Trace();
		    					for(int i=PathToNewState.size()-1;i>=0;--i) Predicted_trace.add(PathToNewState.get(i));Predicted_trace.add(label);
	
		    					UpdatableOutcome predicted_from_Markov=MarkovMatrix.get(Predicted_trace);
		    					if (predicted_from_Markov != UpdatableOutcome.failure)
		    					{// if training data does not lead to a consistent outcome for this label because chunk length is too small, not much we can do, but otherwise we are here and can make use of the data
	
		    						UpdatableOutcome outcome = UpdatableOutcome.ensureConsistencyBetweenOpinions(labels_occurrence, predicted_from_Markov);
		    						if (outcome != labels_occurrence)
		    						{// we learnt something new, be it a new value (or a non-null value) or a failure, record it
		    							assert outcome == UpdatableOutcome.failure;
		    							++inconsistencies;// record inconsistency
		    							outgoing_labels_value.put(label, outcome);// record the failure
		    						}
		    						
		    					}
    						}
	    				}
	    			}
	    			else
	    			{// not reached the maximal length of paths to explore
	    				frontline.add(new FrontLineElem(PathToNewState,target));
	    			}
	    		}
	    	}
	    }

	    return inconsistencies;
	}
	
	public double checkFanoutInconsistencyDouble(LearnerGraphND Inverse_Graph, LearnerGraph graph, CmpVertex vert, int chunkLength)
	{
		assert vert.isAccept();
		Set<Label> outgoingLabels = graph.transitionMatrix.get(vert).keySet();
		Map<Label,UpdatablePairInteger> outgoing_labels_probabilities=new HashMap<Label,UpdatablePairInteger>();
		Map<Label,UpdatableOutcome> outgoing_labels_value=new HashMap<Label,UpdatableOutcome>();
		//for(Label l:alphabet) outgoing_labels_probabilities.put(l, UpdatableOutcome.unknown);
		for(Entry<Label,CmpVertex> entry:graph.transitionMatrix.get(vert).entrySet())
		{
			outgoing_labels_value.put(entry.getKey(),entry.getValue().isAccept()?UpdatableOutcome.positive:UpdatableOutcome.negative);
			outgoing_labels_probabilities.put(entry.getKey(),new UpdatablePairInteger(0, 0));
		}
		LinkedList<FrontLineElem> frontline = new LinkedList<FrontLineElem>();
        FrontLineElem e=new FrontLineElem(new LinkedList<Label>(),vert);
	    frontline.add(e);
	    while(!frontline.isEmpty())
	    {
	    	e=frontline.pop();	
	    	for(Entry<Label, List<CmpVertex>> entry: Inverse_Graph.transitionMatrix.get(e.currentState).entrySet())					
	    	{		
	    		for(CmpVertex target:entry.getValue())
	    		{
	    			ArrayList<Label> PathToNewState=new ArrayList<Label>(chunkLength);
	    			PathToNewState.addAll(e.pathToFrontLine);
	    			PathToNewState.add(entry.getKey());
	    			if(PathToNewState.size()==chunkLength-1)
	    			{
	    				for(Label label:outgoingLabels)
	    				{
    						UpdatableOutcome labels_occurrence= outgoing_labels_value.get(label);
	
	    					Trace Predicted_trace= new Trace();
	    					for(int i=PathToNewState.size()-1;i>=0;--i) Predicted_trace.add(PathToNewState.get(i));Predicted_trace.add(label);

	    					UpdatableOutcome predicted_from_Markov=MarkovMatrix.get(Predicted_trace);
	    					if (predicted_from_Markov != UpdatableOutcome.failure)
	    					{// if training data does not lead to a consistent outcome for this label because chunk length is too small, not much we can do, but otherwise we are here and can make use of the data

	    						UpdatablePairInteger counters = outgoing_labels_probabilities.get(label);counters.secondElem++;
	    						UpdatableOutcome outcome = UpdatableOutcome.ensureConsistencyBetweenOpinions(labels_occurrence, predicted_from_Markov);
	    						if (outcome != labels_occurrence)
	    						{// we learnt something new, be it a new value (or a non-null value) or a failure, record it
	    							assert outcome == UpdatableOutcome.failure;
    								++counters.firstElem;// record inconsistency
	    						}
	    						
	    					}
	    				}
	    			}
	    			else
	    			{// not reached the maximal length of paths to explore
	    				frontline.add(new FrontLineElem(PathToNewState,target));
	    			}
	    		}
	    	}
	    }

	    double inconsistencies = 0;
	    for(UpdatablePairInteger p:outgoing_labels_probabilities.values())
	    	if (p.secondElem > 0)
	    		if (p.firstElem > 0)
	    			inconsistencies+=p.firstElem/(double)p.secondElem;
	    return inconsistencies;
	}

	/** This function is predicts transitions from each state and then adds them to the supplied graph.
	 *  
	 * @param tentativeAutomaton tentative Automaton 
	 * @return a list of possible of outgoing transitions from each state
	 */
	public Map<CmpVertex, Map<Label, UpdatableOutcome>> predictTransitions(LearnerGraph tentativeAutomaton)
	{
		/** Maps states to a function associating labels to a probability of a transition with the label of interest from a state of interest. Computed from {@link MarkovUniversalLearner#state_outgoing_occurence}. */
		Map<CmpVertex,Map<Label,UpdatableOutcome>> state_outgoing=new HashMap<CmpVertex,Map<Label,UpdatableOutcome>>();

		final Configuration shallowCopy = tentativeAutomaton.config.copy();shallowCopy.setLearnerCloneGraph(false);
		Set<Label> alphabet = tentativeAutomaton.learnerCache.getAlphabet(); 
		// mapping map to store all paths leave each state in different length
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(tentativeAutomaton,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
    	for(CmpVertex vert:Inverse_Graph.transitionMatrix.keySet())
           if(vert.isAccept() )
            {
        	   Map<Label,UpdatableOutcome> outgoing_labels_probabilities=predictTransitionsFromState(Inverse_Graph,vert,alphabet,null,chunk_Length);
			   if (!outgoing_labels_probabilities.isEmpty())
			    	state_outgoing.put(vert, outgoing_labels_probabilities);
			}
    	return state_outgoing;
	}	
	
	/** This function is predicts transitions from each state and then adds them to the supplied graph.
	 *  
	 * @param tentativeAutomaton tentative Automaton 
	 * @return a list of possible of outgoing transitions from each state
	 */
	public Map<CmpVertex, Map<Label, UpdatableOutcome>> constructMarkovTentative(LearnerGraph tentativeAutomaton)
	{
		/** Maps states to a function associating labels to a probability of a transition with the label of interest from a state of interest. Computed from {@link MarkovUniversalLearner#state_outgoing_occurence}. */
		Map<CmpVertex,Map<Label,UpdatableOutcome>> state_outgoing=predictTransitions(tentativeAutomaton);

		final Configuration shallowCopy = tentativeAutomaton.config.copy();shallowCopy.setLearnerCloneGraph(false);
		Extension_Graph= new LearnerGraph(shallowCopy);
		LearnerGraph.copyGraphs(tentativeAutomaton, Extension_Graph);

    		// in this part the tree is extended depend on their outgoing transition probabilities
    	 	for(Entry<CmpVertex, Map<Label, UpdatableOutcome>> outgoing:state_outgoing.entrySet())
    	 	{
    	 		CmpVertex currrent_state_to_explore_outgoing= outgoing.getKey();
    	 		Map<Label, UpdatableOutcome> list_of_outgoing = outgoing.getValue();
    	 		for(Entry<Label, UpdatableOutcome> out:list_of_outgoing.entrySet())
    	 		{
    	 			Map<Label, CmpVertex> already_outgoing = tentativeAutomaton.transitionMatrix.get(currrent_state_to_explore_outgoing);
    	 			assert already_outgoing!=null : "state "+currrent_state_to_explore_outgoing+" is not mentioned in the transition diagram";

    	 			if(!already_outgoing.containsKey(out.getKey()) && out.getValue() != UpdatableOutcome.failure)
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
	   			Trace traceToMarkov=new Trace(t.getList().subList(f, f+chunkLen)); // get trace from the path
	   			chunks.add(traceToMarkov);
	   		}
	    }
	   	return chunks;
	}
	
	public Map<Trace, UpdatablePairInteger> get_Markov_model_occurence() 
	{
		return occurrenceMatrix;
	}
	
	public Map<Trace, UpdatableOutcome> get_Markov_model() 
	{
		return MarkovMatrix;
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
	
	
	public static final double REJECT = -1;
	
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
	    			return REJECT;
			}
			else
				return REJECT;
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
	    			return REJECT;
			}
			else
				return REJECT;
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
	
	public static Collection<CmpVertex> numOFsimilarBLUEBLUE(Stack<PairScore> possibleMerges)
	{
		Set<CmpVertex> blues = new HashSet<CmpVertex>();// was: new LinkedHashSet<CmpVertex>();
		for(PairScore v:possibleMerges)
			if(v.firstElem.isAccept()== v.secondElem.isAccept())
			if (v.secondElem.getColour() == JUConstants.RED )
				blues.add(v.firstElem);
		return blues;
	}

}