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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;
import statechum.Trace;

/** Describes a non-probabilistic Markov model, where for every path we know either that, 
 * <ul>
 * <li>the path was never encountered or</li>
 * <li>the path was encountered and there is a specific set of elements of alphabet that followed it.</li>
 * </ul>
 *  
 * The idea is to use the supplied Markov matrix to predict transitions from a specific state, passed as an argument. The choice of direction is <em>not</em> a choice between predicting transitions leaving a state based on those surrounding that state v.s
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
 */
public class MarkovModel
{
	/** Contains the number of times a specific path was encountered. Would usually be prefix-closed by construction. This property is used both to identify if a particular path was never seen*/
	public final Map<Trace, UpdatablePairInteger> occurrenceMatrix =  new HashMap<Trace,UpdatablePairInteger>();
	/** The model, effectively an boolean representation of <em>numberOfOccurrences</em>. */
	public final Map<Trace, MarkovOutcome> predictionsMatrix =  new HashMap<Trace,MarkovOutcome>();
	
	/** Returns the maximal length of paths in either of the two matrices. */
	public int getChunkLen()
	{
		return chunk_Length;
	}
	
	public int getPredictionLen()
	{
		return chunk_Length-1;
	}
	
	private final int chunk_Length;

	public final boolean predictForwardOrSideways,directionForwardOrInverse;
	
    public MarkovModel(int chunkLen,boolean argPredictForwardOrSideways,boolean argDirectionForwardOrInverse)
    {
    	if (chunkLen < 2)
    		throw new IllegalArgumentException("chunkLen should be at least 2");
    	chunk_Length = chunkLen;predictForwardOrSideways = argPredictForwardOrSideways;directionForwardOrInverse = argDirectionForwardOrInverse;
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
	
	/** Constructs the tables used by the learner, from positive and negative traces. Only builds Markov model in the direction of traces.
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
				List<Trace> List_traces=splitTrace(current_positive_trace,i+1);
				for (Trace tracePos:List_traces)
					updateOccurrenceMatrix(tracePos,true);
			}
		}
		
		// from negative traces initialize the Markov matrix
		for(List<Label> negative_trace:neg)
		{
			for(int i=onlyLongest?chunk_Length-1:0; i<chunk_Length; i++)
			{
				Trace trace=new Trace(negative_trace,true);
				List<Trace> List_traces=splitTrace(trace,i+1);
				int chunkNumber = List_traces.size();
				if (chunkNumber >= 1)
				{
					Trace traceNeg=List_traces.get(chunkNumber-1);
					updateOccurrenceMatrix(traceNeg,false);
					for (Trace tracePos:List_traces)
						if (tracePos != traceNeg)
							updateOccurrenceMatrix(tracePos,true);
				}
			}
		}
		
		// Construct a matrix from trace data, including marking of conflicting data as invalid (conflicts arise where a path is too short). 
		// A prefix of either a positive/ a negative/ a failure (where there are some states from which a shorter sequence is rejected but from other states a longer one is accepted). 
		Trace trace_to_account_its_probability=null;
		for (Entry<Trace, UpdatablePairInteger> e : occurrenceMatrix.entrySet())
		{
			trace_to_account_its_probability=e.getKey();
			UpdatablePairInteger Trace_occurence = e.getValue();
			if (Trace_occurence.firstElem > 0 && Trace_occurence.secondElem > 0)
				predictionsMatrix.put(trace_to_account_its_probability, MarkovOutcome.failure);
			else
			if (Trace_occurence.firstElem > 0) 
				predictionsMatrix.put(trace_to_account_its_probability, MarkovOutcome.positive);
				else
					if (Trace_occurence.secondElem > 0)
						predictionsMatrix.put(trace_to_account_its_probability, MarkovOutcome.negative);
		}
		return predictionsMatrix;
	}

	protected void updateOccurrenceMatrix(Trace traceToMarkov, boolean positive)
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

	


	public static List<Trace> splitTrace (Trace t,int chunkLen)
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
	
}