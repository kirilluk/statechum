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
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;

import statechum.Configuration;
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
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSetAutomaton;

public class MarkovUniversalLearner
{
	private Map<Trace, UpdatablePairInteger> occurrenceMatrix =  new HashMap<Trace,UpdatablePairInteger>();
	private Map<Trace, UpdatablePairDouble> MarkovMatrix =  new HashMap<Trace,UpdatablePairDouble>();
	
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
    
    
    public static  class UpdatablePairDouble 
	{
		public double firstElem, secondElem;
		public UpdatablePairDouble(double a, double b) {
			firstElem=a;secondElem=b;
		}
		
		public UpdatablePairDouble add(double a, double b)
		{
			firstElem+=a;secondElem+=b;return this;
		}
		
		public UpdatablePairDouble add(UpdatablePairDouble d)
		{
			add(d.firstElem,d.secondElem);return this;
		}
		
		public double getProbabilityOfAccept(boolean accept)
		{
			return accept?firstElem:secondElem;
		}
		
		@Override
		public String toString()
		{
			return "(pos: "+firstElem+", neg: "+secondElem+")";
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
	
	public static final UpdatablePairDouble failure = new UpdatablePairDouble(-1, -1);
	
	/** Constructs the tables used by the learner, from positive and negative traces.
	 */
	public  Map<Trace, UpdatablePairDouble> createMarkovLearner(Collection<List<Label>> pos,Collection<List<Label>> neg)
	{
		// initialize the occurrence matrix with all elements of the alphabet
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
		/*
		PTASequenceEngine engine = new PTASequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTASequenceEngine.SequenceSet allPathsOfLengthi = engine.new SequenceSet();allPathsOfLengthi.setIdentity(); 
		for(int i=0;i<chunk_Length;++i)
		{
			allPathsOfLengthi=allPathsOfLengthi.crossWithSet(alphabet);
			for(List<Label> seq:engine.getData())
				occurrenceMatrix.put(new Trace(seq), new UpdatablePairInteger(0,0));
		}
		*/
		
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
			Trace pretrace=new Trace(trace_to_account_its_probability.getList().subList(0,trace_to_account_its_probability.size()-1));  // get its prefix
			// counting the number of times of occurrence the random generated path
			int prefix_occurence=trace_to_account_its_probability.size()>1?occurrenceMatrix.get(pretrace).firstElem : traceLength;
			UpdatablePairInteger Trace_occurence = e.getValue();
			if (Trace_occurence.firstElem > 0 && Trace_occurence.secondElem > 0)
				MarkovMatrix.put(trace_to_account_its_probability, failure);
			else
			if (Trace_occurence.firstElem > 0 || Trace_occurence.secondElem > 0)
				MarkovMatrix.put(trace_to_account_its_probability, new UpdatablePairDouble(Trace_occurence.firstElem/(double)prefix_occurence,Trace_occurence.secondElem/(double)prefix_occurence));
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
	public Map<CmpVertex, Map<Label, UpdatablePairDouble>> Markov_tentative(LearnerGraph tentativeAutomaton, double highThreshold, double lowThreshold)
	{
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
      return state_outgoing;
	}
	
	
	public Map<Label, UpdatablePairDouble> predictTransitionsFromState(LearnerGraphND Inverse_Graph, CmpVertex vert, Collection<Label> alphabet, int chunkLength)
	{
		assert vert.isAccept();

		Map<Label,UpdatablePairDouble> outgoing_labels_probabilities=new HashMap<Label,UpdatablePairDouble>();
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
	    				for(Label label:alphabet)
	    				{
	    					Trace Predicted_trace= new Trace();
	    					for(int i=PathToNewState.size()-1;i>=0;--i) Predicted_trace.add(PathToNewState.get(i));Predicted_trace.add(label);

	    					UpdatablePairDouble predicted_from_Markov=MarkovMatrix.get(Predicted_trace);
	    					if (predicted_from_Markov != null && predicted_from_Markov != failure)
	    					{
		    					if(outgoing_labels_probabilities.containsKey(label))
		    					{
		    						UpdatablePairDouble labels_ocuurence= outgoing_labels_probabilities.get(label);
		    						labels_ocuurence.firstElem=Math.max(labels_ocuurence.firstElem,predicted_from_Markov.firstElem);											 
		    						labels_ocuurence.secondElem=Math.max(labels_ocuurence.secondElem,predicted_from_Markov.secondElem);											 
		    					}
		    					else
		    						outgoing_labels_probabilities.put(label, predicted_from_Markov);
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
	
	protected static UpdatablePairDouble zero=new UpdatablePairDouble(0,0);
	
	public int checkFanoutInconsistency(LearnerGraphND Inverse_Graph, LearnerGraph graph, CmpVertex vert, Collection<Label> alphabet, int chunkLength)
	{
		assert vert.isAccept();

		int inconsistentElements = 0;
		Map<Label,UpdatablePairDouble> outgoing_labels_probabilities=new HashMap<Label,UpdatablePairDouble>();
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
	    				for(Label label:alphabet)
	    				{
	    					Trace Predicted_trace= new Trace();
	    					for(int i=PathToNewState.size()-1;i>=0;--i) Predicted_trace.add(PathToNewState.get(i));Predicted_trace.add(label);

    						UpdatablePairDouble predicted_from_Markov=MarkovMatrix.get(Predicted_trace);
	    					if (predicted_from_Markov != failure)
	    					{// if training data does not lead to a consistent outcome for this label because chunk length is too small, not much we can do, but otherwise we are here and can make use of the data

	    						if (predicted_from_Markov == null)
		    					// the current path does not expect a specific label to follow, check our records, we may have seen other paths predict it.
		    						predicted_from_Markov = zero;

	    						UpdatablePairDouble labels_occurrence= outgoing_labels_probabilities.get(label);
	    						if (labels_occurrence == null)
	    						{
	    							if (vert.getDepth() < chunkLength-1)
	    							{// we cannot predict anything for states close to root unless we've created a loop (which also happens). Hence assume that for states near root, the present fanout is the one expected.
	    								CmpVertex targetState = graph.transitionMatrix.get(vert).get(label);
	    								if (targetState == null)
	    									labels_occurrence = new UpdatablePairDouble(0,0);// no transition with this label, set prediction to zero
	    								else
	    									if (targetState.isAccept()) labels_occurrence = new UpdatablePairDouble(1,0);// there is already a transition to an accept state, record it.
	    									else
	    										labels_occurrence = new UpdatablePairDouble(0,1);// a reject-transition
	    							}
	    							else
	    								labels_occurrence = new UpdatablePairDouble(predicted_from_Markov.firstElem, predicted_from_Markov.secondElem);
	    							
	    							outgoing_labels_probabilities.put(label, labels_occurrence);
	    						}
	
	    						if (labels_occurrence != failure)
	    						{// if there is not inconsistency already detected for this label
		
		    						if (
		    								(predicted_from_Markov.firstElem > 0 && labels_occurrence.firstElem <= 0) ||
		    								(predicted_from_Markov.firstElem <= 0 && labels_occurrence.firstElem > 0)
		    							)
		    						{
		    							outgoing_labels_probabilities.put(label, failure);// one is predicted, the other one is not, report as a failure
		    							++inconsistentElements;
		    						}
		    						else
		    						{
			    						labels_occurrence.firstElem=Math.max(labels_occurrence.firstElem,predicted_from_Markov.firstElem);											 
			    						labels_occurrence.secondElem=Math.max(labels_occurrence.secondElem,predicted_from_Markov.secondElem);
			    						
			    						if (labels_occurrence.firstElem > 0 && labels_occurrence.secondElem > 0)
			    						{
			    							outgoing_labels_probabilities.put(label, failure);// inconsistent prediction, report as a failure
			    							++inconsistentElements;
			    						}
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

	    return inconsistentElements;
	}
	
	
	
	
	/** This function is predicts transitions from each state and then adds them to the supplied graph.
	 *  
	 * @param tentativeAutomaton tentative Automaton 
	 * @return a list of possible of outgoing transitions from each state
	 */
	public Map<CmpVertex, Map<Label, UpdatablePairDouble>> predictTransitions(LearnerGraph tentativeAutomaton)
	{
		/** Maps states to a function associating labels to a probability of a transition with the label of interest from a state of interest. Computed from {@link MarkovUniversalLearner#state_outgoing_occurence}. */
		Map<CmpVertex,Map<Label,UpdatablePairDouble>> state_outgoing=new HashMap<CmpVertex,Map<Label,UpdatablePairDouble>>();

		final Configuration shallowCopy = tentativeAutomaton.config.copy();shallowCopy.setLearnerCloneGraph(false);
		Set<Label> alphabet = tentativeAutomaton.learnerCache.getAlphabet(); 
		// mapping map to store all paths leave each state in different length
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(tentativeAutomaton,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
    	for(CmpVertex vert:Inverse_Graph.transitionMatrix.keySet())
           if(vert.isAccept() )
            {
        	   Map<Label,UpdatablePairDouble> outgoing_labels_probabilities=predictTransitionsFromState(Inverse_Graph,vert,alphabet,chunk_Length);
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
	public Map<CmpVertex, Map<Label, UpdatablePairDouble>> constructMarkovTentative(LearnerGraph tentativeAutomaton, double highThreshold, double lowThreshold)
	{
		/** Maps states to a function associating labels to a probability of a transition with the label of interest from a state of interest. Computed from {@link MarkovUniversalLearner#state_outgoing_occurence}. */
		Map<CmpVertex,Map<Label,UpdatablePairDouble>> state_outgoing=predictTransitions(tentativeAutomaton);

		final Configuration shallowCopy = tentativeAutomaton.config.copy();shallowCopy.setLearnerCloneGraph(false);
		Extension_Graph= new LearnerGraph(shallowCopy);
		LearnerGraph.copyGraphs(tentativeAutomaton, Extension_Graph);

    		// in this part the tree is extended depend on their outgoing transition probabilities
    	 	for(Entry<CmpVertex, Map<Label, UpdatablePairDouble>> outgoing:state_outgoing.entrySet())
    	 	{
    	 		CmpVertex currrent_state_to_explore_outgoing= outgoing.getKey();
    	 		Map<Label, UpdatablePairDouble> list_of_outgoing = outgoing.getValue();
    	 		for(Entry<Label, UpdatablePairDouble> out:list_of_outgoing.entrySet())
    	 		{
    	 			Map<Label, CmpVertex> already_outgoing = tentativeAutomaton.transitionMatrix.get(currrent_state_to_explore_outgoing);
    	 			assert already_outgoing!=null : "state "+currrent_state_to_explore_outgoing+" is not mentioned in the transition diagram";

    	 			if(!already_outgoing.containsKey(out.getKey()) && out.getValue() != failure)
    	 			{  	   
//    	 				if (Math.abs(out.getValue().firstElem-out.getValue().secondElem) < 0.2)
//    	 					System.out.println("similar values for pos and neg");
//    	 				
    	 				if(out.getValue().firstElem >  highThreshold && out.getValue().secondElem <= lowThreshold && currrent_state_to_explore_outgoing.isAccept()==true)
    	 				{  
    	 					if(!Extension_Graph.transitionMatrix.get(currrent_state_to_explore_outgoing).keySet().contains(out.getKey()))
    	 						extendWithLabel(Extension_Graph,currrent_state_to_explore_outgoing, true, out.getKey());
    	 				} 

    	 				if(out.getValue().secondElem >  highThreshold && out.getValue().firstElem <= lowThreshold && currrent_state_to_explore_outgoing.isAccept()==true)
    	 				{  
    	 					if(!Extension_Graph.transitionMatrix.get(currrent_state_to_explore_outgoing).keySet().contains(out.getKey()))
    	 						extendWithLabel(Extension_Graph,currrent_state_to_explore_outgoing, false, out.getKey());
    	 				} 	 					
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
	
	public Map<Trace, UpdatablePairDouble> get_Markov_model() 
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
	
	class FrontLineElem
	{
		final List<Label> pathToFrontLine;
		final CmpVertex currentState;
		
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
	
	public double computeMMScore(PairScore P, LearnerGraph coregraph , Map<CmpVertex, Map<Label, UpdatablePairDouble>> l,@SuppressWarnings("unused") LearnerGraph extensiongraph)
	{
		double score = 0;
		Set<Label> outgoing_form_blue_node = coregraph.transitionMatrix.get(P.getQ()).keySet();
		Set<Label> outgoing_form_red_node = coregraph.transitionMatrix.get(P.getR()).keySet();
		Set<Label> predicted_form_blue_node= l.get(P.getQ()).keySet();
		Set<Label> predicted_form_red_node= l.get(P.getR()).keySet();
		
		Set<Label> all_outgoing = new HashSet<Label>() ;
		all_outgoing.addAll(predicted_form_red_node);
		all_outgoing.addAll(predicted_form_blue_node);		

		for(Label out_red:outgoing_form_red_node)
		{			
			if(outgoing_form_blue_node.contains(out_red))  // if outgoing transitions from a red node exist in a blue state
			{				
				boolean target_from_red_acceptance  = coregraph.getTransitionMatrix().get(P.getR()).get(out_red).isAccept();
		    	boolean target_form_blue_acceptance = coregraph.getTransitionMatrix().get(P.getQ()).get(out_red).isAccept();	

	    		if(target_form_blue_acceptance ==  target_from_red_acceptance )		
					score++;	
	    		else
		    		return REJECT;	
			}
			else if(predicted_form_blue_node.contains(out_red))  // if outgoing transitions from a red node exist in a blue state
			{// We are here only if an existing transition has matched the predicted rather than the real transition
				boolean target_from_red_acceptance  = coregraph.getTransitionMatrix().get(P.getR()).get(out_red).isAccept();
		    	boolean target_form_blue_acceptance = coregraph.getTransitionMatrix().get(P.getQ()).get(out_red).isAccept();	
			 
				if(target_form_blue_acceptance ==  target_from_red_acceptance )
					score+=l.get(P.getQ()).get(out_red).getProbabilityOfAccept(target_form_blue_acceptance);// since accept-transition has been predicted, from the current state, we can count on Markov matrix being adequately populated. 
			}
			
		}
				
		for(Label out_blue:outgoing_form_blue_node)
		{			
			if(outgoing_form_red_node.contains(out_blue))  // if outgoing transitions from a red node exist in a blue state
			{				
				boolean target_from_red_acceptance  = coregraph.getTransitionMatrix().get(P.getR()).get(out_blue).isAccept();
		    	boolean target_form_blue_acceptance = coregraph.getTransitionMatrix().get(P.getQ()).get(out_blue).isAccept();
		    	
		    	if(target_form_blue_acceptance ==  target_from_red_acceptance )		
					score++;	
	    		else
		    		return REJECT;
			}
			else if(predicted_form_red_node.contains(out_blue))  // if outgoing transitions from a red node exist in a blue state
			{// we are here only if an existing transition has matched the predicted rather than the real transition
				boolean target_from_red_acceptance  = coregraph.getTransitionMatrix().get(P.getR()).get(out_blue).isAccept();
		    	boolean target_form_blue_acceptance = coregraph.getTransitionMatrix().get(P.getQ()).get(out_blue).isAccept();	
		    	if(target_form_blue_acceptance ==  target_from_red_acceptance )		
		    		score+=l.get(P.getR()).get(out_blue).getProbabilityOfAccept(target_form_blue_acceptance);
			}
			
		}
		
		return score;		
	}
	
	public double computeMMScoreImproved(PairScore P, LearnerGraph coregraph)
	{
		double score = 0;
		Set<Label> outgoing_from_blue_node = coregraph.transitionMatrix.get(P.getQ()).keySet();
		Set<Label> outgoing_from_red_node = coregraph.transitionMatrix.get(P.getR()).keySet();						
		Set<Label> predicted_from_blue_node = Extension_Graph.transitionMatrix.get(P.getQ()).keySet();
		Set<Label> predicted_from_red_node = Extension_Graph.transitionMatrix.get(P.getR()).keySet();
		
		predicted_from_blue_node.removeAll(outgoing_from_blue_node);
		predicted_from_red_node.removeAll(outgoing_from_red_node);
	
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
	/*
	public static Double get_high_Threshold(int numoutgoing)
	{
		double high=0.0;
		if(numoutgoing!=0)
			high= 1/numoutgoing;
		else
			high=0.7;
		return high;
	}
	*/
	/*
	  public static void main(String[] args) throws IncompatibleStatesException 
	  {
//		  String specGraph = "q0-initialise->q1-connect->q2-login->q3-setfiletype->q4-rename->q6-storefile->q5-setfiletype->q4-storefile->q7-appendfile->q5-setfiletype->q4\nq3-makedir->q8-makedir->q8-logout->q16-disconnect->q17\nq3-changedir->q9-listnames->q10-delete->q10-changedir->q9\nq10-appendfile->q11-logout->q16\nq3-storefile->q11\nq3-listfiles->q13-retrievefile->q13-logout->q16\nq13-changedir->q14-listfiles->q13\nq6-logout->q16\nq5-logout->q16";
//		  LearnerGraph graph = FsmParser.buildLearnerGraph(specGraph, "Target", learnerInitConfiguration.config,null);
   	      
		  
		  LearnerGraph referenceGraph = null;
		  MachineGenerator mg = new MachineGenerator(5, 400 , (int)Math.round((double)5/5));mg.setGenerateConnected(true);
			final int alphabet = 2*5;

		 referenceGraph = mg.nextMachine(alphabet,1, learnerInitConfiguration.config, null).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
				
		 Set<Label> all_events_in_graph = referenceGraph.learnerCache.getAlphabet();         	  	    

	  	  RandomPathGenerator rpg = new RandomPathGenerator(referenceGraph, new Random(10), 10 ,null);// the seed for Random should be the same for each file
		  rpg.generatePosNeg(10,1);
		  final PTASequenceEngine  samples= rpg.getAllSequences(0);
		  PTASequenceEngine.FilterPredicate posPredicate = samples.getFSM_filterPredicate();
		  PTASequenceEngine.FilterPredicate negPredicate = new FilterPredicate() 
		  {
			  FilterPredicate origFilter = samples.getFSM_filterPredicate();
		      public @Override boolean shouldBeReturned(Object name) 
		      {
		        	return !origFilter.shouldBeReturned(name);
			  }
	      };	
	    
	      Set<List<Label>> sPlus = new HashSet<List<Label>>();
	      Set<List<Label>> sMinus = new HashSet<List<Label>>();
	      // generating random positive and negative samples and add them to the markov matrix to start learning from.
		  sPlus.addAll(rpg.getExtraSequences(0).getData());
		  sMinus.addAll(samples.getData(negPredicate));
		  MarkovUniversalLearner MUL=new MarkovUniversalLearner(null, learnerEvaluatorConfig);
		  MUL.MarkovMatrix=MUL.CreatingMarkovLearner(sPlus, sMinus, all_events_in_graph);
		  Learner l_inner;
		  l_inner = new RPNIUniversalLearner(null,learnerEvaluatorConfig);	
		  LearnerGraph o = l_inner.init(sPlus, sMinus);		
		  MUL.Markov_tentative(o, MUL.get_Markov_model(), MUL.get_Markov_model_occurence());
		LearnerGraph learntGraph = l_inner.learnMachine();	
	      System.out.println(MUL.MarkovMatrix);

	  }*/
}