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

import java.lang.reflect.Array;
import java.util.*;

import statechum.DeterministicDirectedSparseGraph;
import statechum.Label;
import statechum.Trace;
import statechum.analysis.learning.MarkovModel.MarkovMatrixEngine.PredictionForSequence;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.collections.ArrayMapWithSearchPos;
import statechum.model.testset.PTAExploration;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSetAutomaton;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;

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
 * Where <i>predictForwardOrSideways</i> is true and <i>directionForwardOrInverse</i> is true, 
 * we are predicting transitions based on paths leading to the state of interest. Parameter <i>Inverse_Graph</i> should be the (non-deterministic) inverse of <i>graph</i>.
 * </li>
 * <li>
 * Where <i>predictForwardOrSideways</i> is true and <i>directionForwardOrInverse</i> is false, means that we are predicting transition based on paths from a state of interest forwards. Hence prediction graph is the normal (forward) graph. 
 * </li>
 * <li> 
 * Where <i>predictForwardOrSideways</i> is false and <i>directionForwardOrInverse</i> is true, we are predicting transitions based on paths leading from the state of interest (sideways predictions). Parameter <i>Inverse_Graph</i> should be the same as <i>graph</i> and 
 * <i>pathBeyondCurrentState</i> should be null because once we predicted one transition, there are no further transitions from that state, hence no further transitions can be predicted sideways.
 * </li>
 * <li> 
 * Where <i>predictForwardOrSideways</i> is false and <i>directionForwardOrInverse</i> is false, we are predicting transitions leading to the state of interest based on paths leading to the state of interest.
 * </li>
 * </ul>
 */
public class MarkovModel
{
	public static class MarkovMatrixEngine extends statechum.model.testset.PTASequenceEngine
	{
		public static class PredictionForSequence
		{
			final public UpdatablePairInteger occurrence;
			public MarkovOutcome prediction;
			
			public PredictionForSequence()
			{
				occurrence = new UpdatablePairInteger(0, 0);
				prediction = null;// no value
			}
		}

		public static class PredictionStatePTAAutomaton extends PTASequenceSetAutomaton
		{
			@Override
			public Object getTheOnlyState() 
			{
				return new PredictionForSequence();// it is important to return a new instance every time it is asked for, because otherwise we'll end up sharing instances that is not right.
			}
		}
		
		public MarkovMatrixEngine(boolean useMatrix)
		{
			super(useMatrix);init(new PredictionStatePTAAutomaton());
		}
		
		
		/** Used to obtain a map from labels to predictions, takes a prefix of a trace and returns a map from the last element of that trace to a node associated with predicted elements. 
		 * 
		 * @param sequenceWithoutLastElement sequence to use for predictions.
		 * @return map from labels to predictions, encapsulated inside a node, use {@link PTASequenceEngine.Node#getState()} to get the associated prediction.
		 * The currently commented-out method setState() can be used to set it.
		 */
		public Map<Label,PTASequenceEngine.Node> getMapFromLabelsToPredictions(List<Label> sequenceWithoutLastElement)
		{
			PTASequenceEngine.Node currentNode = getNodeFromSequence(sequenceWithoutLastElement);
			if (currentNode == null)
				return null;
			
			return pta.get(currentNode);
		}
		
		/** Obtains predictions and occurrence. */
		public static PredictionForSequence getPredictionIfExists(Map<Label,PTASequenceEngine.Node> map, Label element)
		{
			if (map == null)
				return null;
			PTASequenceEngine.Node node= map.get(element);
			if (node == null)
				return null;
			return (PredictionForSequence)node.getState();
		}
		
		/** Given a sequence, obtains a prediction for it. */
		public PredictionForSequence getPrediction(List<Label> sequence)
		{
			PTASequenceEngine.Node currentNode = getNodeFromSequence(sequence);
			if (currentNode == null)
				return null;
			return (PredictionForSequence)currentNode.getState();
		}
		
		/** Obtains predictions, initialises a new one if absent. */
		public PredictionForSequence getPredictionAndCreateNewOneIfNecessary(List<Label> sequence)
		{
			SequenceSet set = MarkovMatrixEngine.this.new SequenceSet();set.setIdentity();
			PTASequenceEngine.Node currentNode = set.crossWithSequence(sequence).getTheOnlyElement();
			/*if (!(currentNode.getState() instanceof PredictionForSequence))
					currentNode.setState(new PredictionForSequence());*/
			return (PredictionForSequence)currentNode.getState();
		}

		/** A very specific implementation for 'forward inconsistency' computation that is intended to be efficient. */
		protected  <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>>
		long computeForwardInconsistency(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> reverse, AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> forward,
										 Collection<CmpVertex> collectionStateReverse, CmpVertex consideredStateForward,
										 int step, Map<Label,PTASequenceEngine.Node> row, boolean penaliseMissingPaths) {
			long inconsistency = 0;

			if (step <= 0) {// got to the end of path in reverse graph, take transitions going forward from the graph to be evaluated and check each of them against the Markov PTA.
				if (penaliseMissingPaths || row != null) {// if row is null, it means that a path entering a state of interest
					// (consideredStateForward) does exist in the Markov table. If penaliseMissingPaths is true, we penalise this,
					// if not, we ignore such missing paths.
					for (Map.Entry<Label, TARGET_TYPE> entryForward : forward.transitionMatrix.get(consideredStateForward).entrySet()) {
						PTASequenceEngine.Node nextNode = row == null ? null : row.get(entryForward.getKey());
						PredictionForSequence curPrediction = (PredictionForSequence) (nextNode == null ? null : nextNode.getState());
						if (curPrediction == null || !curPrediction.prediction.isPositive)// if not predicted or predicated as negative, increase inconsistency
							++inconsistency;
					}
				}
			}
			else // if not at the end of a walk, recurse
			{
				// Using LinkedHashMap gives better performance compared to TreeMap
//				Map<Label,List<CmpVertex>> labelToTargets = new TreeMap<>();// for prefix len 3, 40 states and 20% density this gives 275 sec.
				Map<Label,List<CmpVertex>> labelToTargets = new LinkedHashMap<>();// for prefix len 3, 40 states and 20% density this gives 280 sec, same case for prefix len 4 is 984 sec.
//				Map<Label,List<CmpVertex>> labelToTargets = new ArrayMapWithSearchPos<>(reverse.transitionMatrix.size());// for prefix len 3, 40 states and 20% density this gives 380 sec v.s. LinkedHashSet which gives 280 sec.
				for (CmpVertex curReverse : collectionStateReverse)
					for (Map.Entry<Label, TARGET_TYPE> entry : reverse.transitionMatrix.get(curReverse).entrySet())
						labelToTargets.computeIfAbsent(entry.getKey(), k -> new LinkedList<>()).addAll(reverse.getTargets(entry.getValue()));

				for(Map.Entry<Label,List<CmpVertex>> entry : labelToTargets.entrySet()) {
						PTASequenceEngine.Node nextNode = row == null ? null : row.get(entry.getKey());
						inconsistency += computeForwardInconsistency(reverse, forward, entry.getValue(),
								consideredStateForward, step - 1, nextNode == null ? null : pta.get(nextNode),penaliseMissingPaths);
				}
			}
			return inconsistency;
		}

		public  <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>>
		long computeForwardInconsistency(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> reverse, AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> forward,
										 CmpVertex curState, int step, boolean penaliseMissingPaths) {
			// Using LinkedList below avoids a good amount of time spent inside ArrayList.iterator
			return computeForwardInconsistency(reverse,forward, new LinkedList<>(Arrays.asList(curState)),curState,step,pta.get(init), penaliseMissingPaths);
		}
	}


	public final MarkovMatrixEngine markovMatrix;
	
	/** Contains the number of times a specific path was encountered. Would usually be prefix-closed by construction. 
	 * This property is used both to identify if a particular path was never seen. 
	 * Commented out because it is replaced by a PTA representation of the map.
	 */
	//public final Map<Trace, UpdatablePairInteger> occurrenceMatrix =  new HashMap<Trace,UpdatablePairInteger>();
	
	/** The model, effectively an boolean representation of <em>numberOfOccurrences</em>. 
	 * Commented out because it is replaced by a PTA representation of the map.
	 */
	//public final Map<Trace, MarkovOutcome> predictionsMatrix =  new HashMap<Trace,MarkovOutcome>();
	
	/** Returns the maximal length of paths in either of the two matrices. */
	public int getChunkLen()
	{
		return chunkLength;
	}
	
	public int getPredictionLen()
	{
		return chunkLength-1;
	}
	
	private final int chunkLength;

	private boolean predictionFromOnlySequencesForward;

	boolean getPredictionFromOnlySequencesForward()
	{
		return predictionFromOnlySequencesForward;
	}

	public final boolean predictForwardOrSideways,directionForwardOrInverse;

	/** True if the graph used for predictions is an inverse, in this case all paths we obtain from it are best inverted before lookup in Markov model. 
	 * For efficiency, we could have obviously invert paths in the model but the current setup makes it easier to understand and we need to copy the 
	 * graphs anyway which is be accomplished as fast as inversion.
	 * <p/? 
	 * Decisions to invert or not are based on the following:
	 * <table>
	 * <tr><td>predictForwardOrSideways</td><td>directionForwardOrInverse</td><td>Decision</td></tr>
	 * <tr><td>T</td><td>T</td><td>graphsToUseForPrediction=inverse<br/>graphsToCheckForConsistency=<b>forward</b></td></tr>
	 * <tr><td>T</td><td>F</td><td>graphsToUseForPrediction=<b>forward</b><br/>graphsToCheckForConsistency=inverse</td></tr>
	 * <tr><td>F</td><td>T</td><td>graphsToUseForPrediction=<b>forward</b><br/>graphsToCheckForConsistency=<b>forward</b></td></tr>
	 * <tr><td>F</td><td>F</td><td>graphsToUseForPrediction=inverse<br/>graphsToCheckForConsistency=inverse</td></tr>
	 * </table>
	 */
	public final boolean predictionGraphInverted;
	
	/** If true, we are looking at sequences of transitions to/from a state of interest. 
	 * If false, we are looking for sets of labels on transitions into/out of a state of interest. Both are 
	 * represented as paths because we need to do a lookup in a collection of paths and numbering of labels 
	 * permits elements such sets to be represented as sequences.
	 */
	public final boolean pathsOrSets;

	/** Constructs an instance of a Markov model which will subsequently be populated with data by calling update.
	 *
	 * @param chunkLen length of paths used for prediction + 1. For instance, a value of 2 means predicting transitions based on one transition.
	 * @param argPathsOrSets if true, predicts transitions based on presence of paths. If false, predicts based on collections of transitions (such as on an incoming fan-in)
	 * @param argPredictForwardOrSideways if true, predicts transitions leaving a state based on those incoming (or the other way around). For false, predicts outgoing transitions based on existing outgoing transitions (or same for incoming transitions).
	 * @param argDirectionForwardOrInverse whether to predict outgoing transitions (value true) or incoming transitions (value false)
	 * @param PTAUseMatrix whether to use a matrix to store PTA storing Markov information. Matrix is great memory-wise for millions of elements (and not as good performance-wise) which is probably unlikely in this case hence most of the time it will be false.
	 */
    public MarkovModel(int chunkLen,final boolean argPathsOrSets, boolean argPredictForwardOrSideways,boolean argDirectionForwardOrInverse, boolean PTAUseMatrix)
    {
    	if (chunkLen < 2)
    		throw new IllegalArgumentException("chunkLen should be at least 2");
    	chunkLength = chunkLen;pathsOrSets = argPathsOrSets;predictForwardOrSideways = argPredictForwardOrSideways;directionForwardOrInverse = argDirectionForwardOrInverse;
    	predictionGraphInverted = predictForwardOrSideways == directionForwardOrInverse;
    	markovMatrix = new MarkovMatrixEngine(PTAUseMatrix);
    }
    
    @Override
    public String toString()
    {
    	return "MarkovModel("+chunkLength+"-"+(directionForwardOrInverse?"forward":"backward")+","+ (pathsOrSets?("paths"+(predictForwardOrSideways?"forward":"sideways")):"sets")+")";
    }
    
    /** Used to record outcomes of Markov computations. Its primary use are the three values and static routines to make decisions between them. */
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
		 * The significance of this is that where we make a merge, a number of states get merged and hence there will 
		 * be a number of paths leading to and from a state of interest. Markov will predict outgoing transitions
		 * based on those paths, relying on an entire graph as the source of information. These predictions may or 
		 * may not match actual transitions, for each actual outgoing transition (pos/neg/non-existing) we might 
		 * like to match it with the predicted one and count the number of labels where predictions from one or 
		 * more paths does not match the actual data (which will also imply that predictions contradict each other). 
		 * We could instead look for consistent predictions (where all paths to or from a state lead to the same 
		 * prediction) and use those to check whether they contradict the actual data. 
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
				}
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
            return secondElem == other.secondElem;
        }
	}

	/** Constructs Markov matrix with sequences that are a reverse of a 'normal' Markov matrix. The intention is to use such a matrix
	 * with the computeForwardInconsistency method, expecting them. The intended benefit is much better performance since it is
	 * optimized for the specific use case.
	 *
	 * @param pos collection of positive sequences
	 * @param onlyLongest if set, only add traces of <i>chunkLen</i> to Markov matrix. Where false, all prefixes are added as well.
	 */
	public void createMarkovFromPositiveDataAndGenerateInversePredictions(Collection<List<Label>> pos, boolean onlyLongest) {
		if (!predictForwardOrSideways || !directionForwardOrInverse || !pathsOrSets)
			throw new IllegalArgumentException("This Markov matrix can only be used to predict events forward by looking at past sequences");

		predictionFromOnlySequencesForward = true;// sequences will be stored reversed in the PTA

		// going through all positive traces
		//and partitioning each positive traces into a list of events ( a list of labels based on the chunk length)
		for(List<Label> positive_trace:pos)
		{
			Trace current_positive_trace=new Trace(positive_trace, true);
			for(int i=onlyLongest?chunkLength-1:0;i<chunkLength;i++)
			{
				List<Trace> List_traces=splitTrace(current_positive_trace,i+1);
				for (Trace tracePos:List_traces) {
					List<Label> sequence = new ArrayList<>(tracePos.getList().subList(0, tracePos.getList().size()-1));
					Collections.reverse(sequence);
					sequence.add(tracePos.getList().get(tracePos.getList().size()-1));
					markovMatrix.getPredictionAndCreateNewOneIfNecessary(sequence).occurrence.add(1,0);
				}
			}
		}

		convertOccurrenceMatrixToPTA();
	}

	/** Constructs the tables used by the learner, from positive and negative traces. Only builds Markov model in the direction of traces.
	 * 
	 * @param onlyLongest if set, only add traces of <i>chunkLen</i> to Markov matrix. Where false, all prefixes are added as well.
	 */
	public void createMarkovLearner(Collection<List<Label>> pos,Collection<List<Label>> neg, boolean onlyLongest)
	{
		predictionFromOnlySequencesForward = false;// sequences will be stored as-is in the PTA

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
			for(int i=onlyLongest?chunkLength-1:0;i<chunkLength;i++)
			{
				List<Trace> List_traces=splitTrace(current_positive_trace,i+1);
				for (Trace tracePos:List_traces)
					updateOccurrenceMatrix(tracePos,true);
			}
		}
		
		// from negative traces initialize the Markov matrix
		for(List<Label> negative_trace:neg)
		{
			for(int i=onlyLongest?chunkLength-1:0; i<chunkLength; i++)
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

		convertOccurrenceMatrixToPTA();
	}

	private void convertOccurrenceMatrixToPTA() {
		// Construct a matrix from trace data, including marking of conflicting data as invalid (conflicts arise where a path is too short).
		// A prefix of either a positive/ a negative/ a failure (where there are some states from which a shorter
		// sequence is rejected but from other states a longer one is accepted. This is detected because with onlyLongest being false,
		// all strict prefixes of a trace (plus whole trace if positive) will be added as positives
		// so if there was a shorter trace labelled as a negative, there will be a both a positive counter
		// and a negative one above zero leading to a failure-prediction).

		PTAExploration<Boolean> exploration = new PTAExploration<Boolean>(markovMatrix) {
			@Override
			public Boolean newUserObject() {
				return null;
			}

			@Override
			public void nodeEntered(PTAExplorationNode currentNode, @SuppressWarnings("unused")	LinkedList<PTAExplorationNode> pathToInit)
			{
				PredictionForSequence prediction = (PredictionForSequence)currentNode.getState();
				if (prediction.occurrence.firstElem > 0 && prediction.occurrence.secondElem > 0)
					prediction.prediction = MarkovOutcome.failure;
				else
				if (prediction.occurrence.firstElem > 0)
					prediction.prediction = MarkovOutcome.positive;
				else
				if (prediction.occurrence.secondElem > 0)
					prediction.prediction = MarkovOutcome.negative;
			}

			@Override
			public void leafEntered(PTAExplorationNode currentNode,	LinkedList<PTAExplorationNode> pathToInit)
			{
				nodeEntered(currentNode, pathToInit);
			}

			@Override
			public void nodeLeft(@SuppressWarnings("unused") PTAExplorationNode currentNode,	@SuppressWarnings("unused")	LinkedList<PTAExplorationNode> pathToInit)
			{
				// nothing to do here.
			}

		};
		exploration.walkThroughAllPaths();
	}

	/** Predictions are used to predict labels following a prefix (both positive and negative predictions).
	 * This function reports values stored in the markov matrix as far as predictions are concerned.
	 * It does not care for whether these values contain reverse strings followed by prediction characters
	 * such as when getPredictionFromOnlySequencesForward() is true.
	 */
	public Map<List<Label>, MarkovOutcome> computePredictionMatrix()
	{
		final Map<List<Label>, MarkovOutcome> outcome = new LinkedHashMap<List<Label>,MarkovOutcome>();
		PTAExploration<Boolean> exploration = new PTAExploration<Boolean>(markovMatrix) {
			@Override
			public Boolean newUserObject() {
				return null;
			}

			@Override
			public void nodeEntered(PTAExplorationNode currentNode, LinkedList<PTAExplorationNode> pathToInit) 
			{
				PredictionForSequence prediction = (PredictionForSequence)currentNode.getState();
				LinkedList<Label> path = new LinkedList<Label>();
				if (predictionGraphInverted)
					for(PTAExplorationNode elem:pathToInit) path.addFirst(elem.getInput());
				else
					for(PTAExplorationNode elem:pathToInit) path.addLast(elem.getInput());
				
				if (prediction.prediction != null)
					outcome.put(path, prediction.prediction);
			}

			@Override
			public void leafEntered(PTAExplorationNode currentNode,	LinkedList<PTAExplorationNode> pathToInit) 
			{
				nodeEntered(currentNode, pathToInit);
			}

			@Override
			public void nodeLeft(@SuppressWarnings("unused") PTAExplorationNode currentNode,	@SuppressWarnings("unused")	LinkedList<PTAExplorationNode> pathToInit) 
			{
				// nothing to do here.
			}

		};
		exploration.walkThroughAllPaths();
		return outcome;
	}

	/** Occurrences count the number of labels following a prefix (both positive and negative).
	 * This function reports values stored in the markov matrix as far as occurrences are concerned.
	 */
	public Map<List<Label>, UpdatablePairInteger> computeOccurrenceMatrix()
	{
		final Map<List<Label>, UpdatablePairInteger> outcome = new LinkedHashMap<List<Label>,UpdatablePairInteger>();
		PTAExploration<Boolean> exploration = new PTAExploration<Boolean>(markovMatrix) {
			@Override
			public Boolean newUserObject() {
				return null;
			}

			@Override
			public void nodeEntered(PTAExplorationNode currentNode, LinkedList<PTAExplorationNode> pathToInit) 
			{
				PredictionForSequence prediction = (PredictionForSequence)currentNode.getState();
				LinkedList<Label> path = new LinkedList<Label>();
				if (predictionGraphInverted)
					for(PTAExplorationNode elem:pathToInit) path.addFirst(elem.getInput());
				else
					for(PTAExplorationNode elem:pathToInit) path.addLast(elem.getInput());

				if (prediction.prediction != null)
					outcome.put(path, prediction.occurrence);
			}

			@Override
			public void leafEntered(PTAExplorationNode currentNode,	LinkedList<PTAExplorationNode> pathToInit) 
			{
				nodeEntered(currentNode, pathToInit);
			}

			@Override
			public void nodeLeft(@SuppressWarnings("unused") PTAExplorationNode currentNode,	@SuppressWarnings("unused")	LinkedList<PTAExplorationNode> pathToInit) 
			{
				// nothing to do here.
			}

		};
		exploration.walkThroughAllPaths();
		return outcome;
	}

	protected void updateOccurrenceMatrix(Trace traceToMarkov, boolean positive)
	{
		UpdatablePairInteger occurrence_of_trace=markovMatrix.getPredictionAndCreateNewOneIfNecessary(traceToMarkov.getList()).occurrence;
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

	/** Markov matrix is usually built from traces, however in order to evaluate whether an automaton can be
	 * realistically learnt it helps to assume that a very big PTA will be computed (based on all possible paths of prefix-length)
	 * and then we can compute inconsistency against this graph. It will usually be non-zero because many paths would not
	 * uniquely identify states. If this inconsistency is large, this means we do not have a reliable 'homing' to
	 * an automaton of interest. We could also compare this inconsistency with that of other (random) automata and if it is
	 * similar, conclude that an automaton with a particular structure cannot effectively be learnt using Markov heuristic.
	 *
	 * @param gr automaton from which to compute Markov Matrix, assuming the case that all chunkLen-long transitions were present in a PTA.
	 */
	public void buildMarkovMatrixFromAutomaton(LearnerGraph gr) {
		if (!predictForwardOrSideways || !directionForwardOrInverse || !pathsOrSets)
			throw new IllegalArgumentException("This Markov matrix can only be used to predict events forward by looking at past sequences");

		predictionFromOnlySequencesForward = true;// sequences will be stored reversed in the PTA

		for(CmpVertex vert:gr.transitionMatrix.keySet()) {
			if (!vert.isAccept())
				throw new IllegalArgumentException("All states should be accept-states");
			buildMarkovMatrixFromAutomaton(gr, vert);
		}

		convertOccurrenceMatrixToPTA();
	}

	protected void buildMarkovMatrixFromAutomaton(LearnerGraph gr, CmpVertex startingState) {
		int currentExplorationDepth = 1;// when we look at transitions from the initial pair of states, this is depth 1.

		Queue<CmpVertex> currentExplorationBoundary = new LinkedList<>();// FIFO queue
		Queue<List<Label>> currentPathBoundary = new LinkedList<>();

		if (currentExplorationDepth <= chunkLength) {
			currentExplorationBoundary.add(startingState);

			currentPathBoundary.offer(new LinkedList<>());
//			PTASequenceEngine.Node currentNode = set.crossWithSequence(sequence).getTheOnlyElement();
		}
		currentExplorationBoundary.offer(null);
		currentPathBoundary.offer(null);


		while (true) // we'll do a break at the end of the last wave
		{
			CmpVertex currentState = currentExplorationBoundary.remove();
			List<Label> currentPath = currentPathBoundary.remove();

			if (currentState == null) {// we got to the end of a wave
				if (currentExplorationBoundary.isEmpty())
					break;// we are at the end of the last wave, stop looping.

				// mark the end of a wave.
				currentExplorationBoundary.offer(null);
				currentPathBoundary.offer(null);
				currentExplorationDepth++;
			} else {
				Map<Label, CmpVertex> transitionsFromState = gr.transitionMatrix.get(currentState);

				// if our current depth is less than the one to explore, make subsequent steps.
				for (Map.Entry<Label, CmpVertex> transition : transitionsFromState.entrySet()) {
					List<Label> newPath = new LinkedList<>(currentPath);newPath.add(transition.getKey());
					if (currentExplorationDepth < chunkLength) {
						currentExplorationBoundary.offer(transition.getValue());
						currentPathBoundary.offer(newPath);
					}
					else {
						Collections.reverse(newPath);
						markovMatrix.getPredictionAndCreateNewOneIfNecessary(newPath).occurrence.add(1, 0);
					}
				}
			}
		}
	}
}