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
	Map<Trace, UpdatablePairInteger> occurrenceMatrix =  new HashMap<Trace,UpdatablePairInteger>();
	/** The model, effectively an boolean representation of <em>numberOfOccurrences</em>. */
	Map<Trace, MarkovOutcome> predictionsMatrix =  new HashMap<Trace,MarkovOutcome>();
	
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
	
    public MarkovModel(int chunkLen,boolean argDirectionForwardOrInverse,boolean argPredictForwardOrSideways)
    {
    	if (chunkLen < 2)
    		throw new IllegalArgumentException("chunkLen should be at least 2");
    	chunk_Length = chunkLen;predictForwardOrSideways = argDirectionForwardOrInverse;directionForwardOrInverse = argPredictForwardOrSideways;
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

	

	/** This function predicts transitions from each state and then adds predictions to the Markov model.
	 *  
	 * <em>predictForwardOrSideways</em> whether to update a forward or a sideways Markov matrix.
	 * <em>directionForwardOrInverse</em> whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and all its target states could be merged together.
	 * @param tentativeAutomaton tentative Automaton
	 * @param onlyLongest if set, only add traces of <i>chunkLen</i> to Markov matrix. Where false, all prefixes are added as well.
	 */
	public void updateMarkov(LearnerGraph tentativeAutomaton, boolean onlyLongest)
	{
		final Configuration shallowCopy = tentativeAutomaton.config.copy();shallowCopy.setLearnerCloneGraph(false);
		Set<Label> alphabet = tentativeAutomaton.learnerCache.getAlphabet();
    	for(CmpVertex vert:tentativeAutomaton.transitionMatrix.keySet())
    		for(int len=onlyLongest?chunk_Length:1;len <=chunk_Length;++len)// this is very inefficient; we'll optimize it later if needed.
	           if(vert.isAccept())
	        	  updateMarkov(tentativeAutomaton,MarkovMatrix,occurrenceMatrix, Inverse_Graph,predictForwardOrSideways,vert,alphabet,len);
	}

	/** Given a graph, it uses the supplied collection of labels in order to identify states to merge, constructs a merge and counts the number of inconsistencies between the Markov-predicted vertices and the actual ones.
	 * The large number of arguments reflect the extent to which this process can be customised. 
	 * 
	 * @param graph graph to handle.
	 * @param graphsToCheckForPaths 
	 * @param predictForwardOrSideways whether to perform Markov-predictions forward (good for long paths) or sideways (many paths, not necessarily short).
	 * @param directionForwardOrInverse whether to merge states identified with the supplied outgoing transitions or those that the supplied transitions lead into. For instance, one might frequently have a <i>reset</i> transition and all its target states could be merged together.
	 * @param paths paths to use.
	 * @param m Markov matrix to be used in predictions in order to compute inconsistency
	 * @param checker inconsistency checker. This one may consider only a subset of labels from each state (such as those labelling outgoing transitions) and make different decision as to what to count as an inconsistency.
	 * @return how inconsistent predictions are compared to the actual graph. Always non-negative.
	 */
	public static long computeInconsistencyForMergingPaths(LearnerGraph graph, Map<CmpVertex,LearnerGraph> graphsToCheckForPaths, boolean predictForwardOrSideways, boolean directionForwardOrInverse, Collection<List<Label>> paths, MarkovModel m, ConsistencyChecker checker)
	{
		long outcome = 0;
		
		LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsList = PairQualityLearner.buildVerticesToMergeForPath(graphsToCheckForPaths,directionForwardOrInverse,paths);
		if (!pairsList.isEmpty())
		{
			int score = graph.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
			if (score < 0)
				outcome = MarkovModel.dREJECT;
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
	 * This is a special version of {@link #computeInconsistencyForMergingPath(LearnerGraph, Map, boolean, boolean, Collection, MarkovModel, ConsistencyChecker)} for a single path.
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
	public static long computeInconsistencyForMergingPath(LearnerGraph graph, Map<CmpVertex,LearnerGraph> graphsToCheckForPaths, boolean predictForward, boolean directionForward, List<Label> path, MarkovModel m, ConsistencyChecker checker)
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
	
	/** This function is predicts transitions from each state and then adds them to the supplied graph.
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
	public LearnerGraph constructMarkovTentative(LearnerGraph tentativeAutomaton)
	{
		/** Maps states to a function associating labels to a probability of a transition with the label of interest from a state of interest. Computed from {@link MarkovUniversalLearner#state_outgoing_occurence}. */
		Map<CmpVertex,Map<Label,MarkovOutcome>> state_outgoing=predictTransitions(tentativeAutomaton);

		final Configuration shallowCopy = tentativeAutomaton.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph graphWithPredictedTransitions = new LearnerGraph(shallowCopy);
		LearnerGraph.copyGraphs(tentativeAutomaton, graphWithPredictedTransitions);

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
 					if(!graphWithPredictedTransitions.transitionMatrix.get(currrent_state_to_explore_outgoing).keySet().contains(out.getKey()))
 						extendWithLabel(graphWithPredictedTransitions,currrent_state_to_explore_outgoing, out.getValue().isPositive, out.getKey());
	 			}					   
	 		}          	       	      
	 	}

      return graphWithPredictedTransitions;
	}

	public static void extendWithLabel(LearnerGraph what, CmpVertex prevState, boolean isAccept, Label input)
	{
		CmpVertex newVertex = AbstractLearnerGraph.generateNewCmpVertex(what.nextID(isAccept),what.config);
		assert !what.transitionMatrix.containsKey(newVertex);
		newVertex.setAccept(isAccept);
		what.transitionMatrix.put(newVertex, what.createNewRow());
		what.addTransition(what.transitionMatrix.get(prevState),input,newVertex);
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