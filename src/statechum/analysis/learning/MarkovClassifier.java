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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicLong;

import statechum.Configuration;
import statechum.Label;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.MarkovModel.MarkovMatrixEngine;
import statechum.analysis.learning.MarkovModel.MarkovMatrixEngine.PredictionForSequence;
import statechum.analysis.learning.MarkovModel.MarkovOutcome;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.collections.ArrayMapWithSearch;
import statechum.collections.ArrayMapWithSearchPos;
import statechum.model.testset.PTASequenceEngine;

/** An instance of this class holds all the necessary parameters in order to make it possible to predict transitions 
 * and/or check inconsistencies using a Markov model. Depending on the kind of model passed to it, 
 * it will be making appropriate predictions.
 */
public class MarkovClassifier<TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
{
	public static final double fREJECT = -1;
	public static final long dREJECT = -1;

	/** Markov model being used in this classifier. Also determines the direction of prediction and whether forward or sideways. */
	public final MarkovModel model;
	/** The graph in which we are making predictions.*/
	
	final AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph;
	
	/** Contains paths to be supplied to Markov for making predictions. The specific kind of the graph 
	 * depends on the direction in which we are doing predictions. Could be either a deterministic or 
	 * a non-deterministic graph.
	 * <br/>Should be immutable and contain the same states and transition labels as 
	 * {@link MarkovClassifier#graph}. This is used both in exploration and construction of an alphabet.
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

	/** Contains paths to be used for consistency checking. The specific kind of the graph depends on 
	 * the direction in which we are doing predictions. Could be either a deterministic or a non-deterministic graph. 
	 * <br/>Should be immutable and contain the same states and transition labels as {@link MarkovClassifier#graph}. 
	 * This is used both in exploration and construction of an alphabet.
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
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> LearnerGraphND computeInverseGraph(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph)
	{
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);shallowCopy.setMaxAcceptStateNumber(graph.vertPositiveID);shallowCopy.setMaxRejectStateNumber(graph.vertNegativeID);
		LearnerGraphND inverseGraph = new LearnerGraphND(shallowCopy);
		inverseGraph.initEmpty();
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,inverseGraph);  // do the inverse to the tentative graph
		return inverseGraph;
	}
	
	/** Obtains the graph that can be used in calls of {@link #checkFanoutInconsistency(AbstractLearnerGraph, boolean, LearnerGraph, CmpVertex, int)} and many others.
	 * Returns an inverse when <i>predictForward</i> is true and <i>graph</i> otherwise.
	 * @param graph what to compute an inverse of
	 * @param constructInverseOrForward whether to invert
	 * @return either an inverse or the original graph. Since either of them can be a deterministic or a non-deterministic graph, the return type is just an archetype and we do not include template parameters.
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static AbstractLearnerGraph computeInverseGraph(AbstractLearnerGraph graph,AbstractLearnerGraph grInverseOrNull, boolean constructInverseOrForward)
	{
		AbstractLearnerGraph inverseGraph = null;
		if (constructInverseOrForward)
		{
			if (grInverseOrNull != null)
				inverseGraph = grInverseOrNull;
			else
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
	 * @param inverseGR inverse of graph gr, if available. If not, pass null here and it will be computed.
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public MarkovClassifier(MarkovModel m, AbstractLearnerGraph gr, AbstractLearnerGraph grInverse)
	{
		model = m;graph = gr;
		predictionGraphInverted = model.predictForwardOrSideways == model.directionForwardOrInverse;
		graphToUseForPrediction=computeInverseGraph(graph,grInverse,predictionGraphInverted);
		assert graph.transitionMatrix.keySet().equals(graphToUseForPrediction.transitionMatrix.keySet());
		graphToCheckForConsistency=computeInverseGraph(graph,grInverse,!model.directionForwardOrInverse);
		assert graph.transitionMatrix.keySet().equals(graphToCheckForConsistency.transitionMatrix.keySet());
	}

	public interface ForEachCollectionOfPaths
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

	public static class FrontLineSet
	{
		/** A set is represented by a list because elements of this set have a total order. */
		public final List<Label> currentSet;
		public final int currentPosition;
		public final int maxElement;
		
		public FrontLineSet(List<Label> set, int position, int elem) {
			currentSet=set;
			currentPosition = position;maxElement=elem;
		}
		
	}

	/** Explores all positive states up to the specified length, calling the supplied callback for each of them. Can be used on both deterministic and non-deterministic graphs. 
	 * In the non-deterministic case, could report the same path multiple times.
	 * 
	 * @param graph graph to explore
	 * @param vert vertex to start with
	 * @param pathLength length of paths to explore
	 * @param pathsOrSets if true, we are looking at sequences of transitions to/from a state of interest. 
	 * If false, we are looking for sets of labels on transitions into/out of a state of interest. Both are 
	 * represented as paths because we need to do a lookup in a collection of paths and numbering of labels 
	 * permits elements such sets to be represented as sequences.
	 * @param callback what to call for each discovered path.
	 */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void WalkThroughAllPathsOfSpecificLength(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph, CmpVertex vert,int pathLength,final boolean pathsOrSets,ForEachCollectionOfPaths callback)
	{
		if (pathsOrSets)
			WalkThroughAllPathsOfSpecificLength_Sequences(graph,vert,pathLength,callback);
		else
			WalkThroughAllPathsOfSpecificLength_Sets(graph,vert,pathLength,callback);
	}

	/** Explores all sets of labels on transitions to/from a supplied state, calling the supplied callback for each of them. Can be used on both deterministic and non-deterministic graphs. 
	 * In the non-deterministic case, could report the same path multiple times.
	 * 
	 * @param graph graph to explore
	 * @param vert vertex to start with
	 * @param pathLength length of paths to explore
	 * @param callback what to call for each discovered path.
	 */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void WalkThroughAllPathsOfSpecificLength_Sets(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph, CmpVertex vert,int pathLength,ForEachCollectionOfPaths callback)
	{
		// considering that there is a total order on the labels, pick the smallest first, then the next one etc.
		Label[] elements = graph.transitionMatrix.get(vert).keySet().toArray(new Label[]{});
		if (elements.length < pathLength)
			return;// cannot build complete paths
		if (pathLength == 0)
			callback.handlePath(new ArrayList<Label>());
		else
		{
			LinkedList<FrontLineSet> frontLine = new LinkedList<FrontLineSet>();
			frontLine.add(new FrontLineSet(new ArrayList<Label>(elements.length),0,0));
			while(!frontLine.isEmpty())
			{
				FrontLineSet setSoFar = frontLine.pop();
				int lastPlusOne = elements.length+setSoFar.currentPosition-pathLength+1;// we need to ensure there is enough elements left for a complete (pathLength-length) entry. 
				for(int i=setSoFar.maxElement;i<lastPlusOne;++i)
				{
					List<Label> nextSet = new ArrayList<Label>(setSoFar.currentSet);
					nextSet.add(elements[i]);
					if (setSoFar.currentPosition == pathLength-1)
						callback.handlePath(nextSet);
					else
						// not reached the maximal length of paths to explore
						frontLine.offer(new FrontLineSet(nextSet,setSoFar.currentPosition+1,i+1));
				}
			}
		}
	}
	
	/** Explores all positive states up to the specified length, calling the supplied callback for each of them. Can be used on both deterministic and non-deterministic graphs. 
	 * In the non-deterministic case, would report each path only once.
	 * 
	 * @param graph graph to explore
	 * @param vert vertex to start with
	 * @param pathLength length of paths to explore
	 * @param callback what to call for each discovered path.
	 */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void WalkThroughAllPathsOfSpecificLength_Sequences(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph, CmpVertex vert,int pathLength,ForEachCollectionOfPaths callback)
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
				{// this accounts for multiple identical paths from a state in a non-deterministic graph
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
			    			List<Label> pathToNewState=new ArrayList<Label>(pathLength+2);// +2 is to avoid potential array reallocation, some versions of JDK reallocate when an array is full without waiting for a next call to add. 
			    			pathToNewState.addAll(e.pathToFrontLine);pathToNewState.add(lbl);
	    					frontline.add(new FrontLineElem(pathToNewState,target));
			    		}
			    }
	    	}
	    }
	}

	/** Used to check if the supplied vertex cannot have anything predicted for it because there is no path of length "prediction length" leading to it. This usually happens for root states. */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> boolean checkIfThereIsPathOfSpecificLength(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph, CmpVertex vert,int pathLength)
	{
		LinkedList<FrontLineElem> frontline = new LinkedList<FrontLineElem>();
        FrontLineElem e=new FrontLineElem(new LinkedList<Label>(),vert);
	    if (vert.isAccept()) frontline.add(e);
	    while(!frontline.isEmpty())
	    {
	    	e=frontline.pop();
	    	
			if(e.pathToFrontLine.size()==pathLength)
				return true;
			// not reached the maximal length of paths to explore
			Map<Label,TARGET_TYPE> transitions = graph.transitionMatrix.get(e.currentState);
			for(Label lbl:transitions.keySet())					
			{
				for(CmpVertex target:graph.getTargets(transitions.get(lbl)))
	    			if (target.isAccept())
		    		{
		    			List<Label> pathToNewState=new ArrayList<Label>(pathLength+1);// +1 is to avoid potential array reallocation
		    			pathToNewState.addAll(e.pathToFrontLine);pathToNewState.add(lbl);
    					frontline.add(new FrontLineElem(pathToNewState,target));
		    		}
		    }
	    }
	    
	    return false;// did not encounter a single path of requested length
	}
	
	/** Given a collection of vertices that is to be merged, computes the inconsistency of the outcome of a merger. This is defined as the difference in inconsistency scores between the merged
	 * graph and the original graph. This routine aims to avoid computation of an inconsistency of complete graphs, instead looking only at those vertices that are affected by mergers and the vicinity of them.
	 * The merged graph should be constructed by merging vertices in verticesToMerge, otherwise merged vertices would not be available as part of elements of {@link AMEquivalenceClass} and we'll crash.
	 *
	 * @param coregraph the original graph
	 * @param inverseGraph the inverse of the original graph
	 * @param verticesToMerge vertices to merge in the original graph, computed by the generalised scoring routine.
	 * @param m Markov model used to compute inconsistencies.
	 * @param origClassifier the classifier used on the original graph. 
	 */
	public static long computeInconsistencyOfAMerger(LearnerGraph coregraph, LearnerGraphND inverseGraph,  List<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge,
			Map<CmpVertex,Long> origInconsistencies, MarkovModel m, MarkovClassifier<CmpVertex,LearnerGraphCachedData> origClassifier, ConsistencyChecker checker)
	{
		Set<CmpVertex> affectedVerticesInMergedGraph = new LinkedHashSet<CmpVertex>(),affectedVerticesInOrigGraph = new LinkedHashSet<CmpVertex>(),influentialVerticesInOrigGraph = new LinkedHashSet<CmpVertex>();
		for(EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass:verticesToMerge)
			if (eqClass.getStates().size() > 1)
				// only look at vertices affected by mergers, it is this property that permits a call to computePairCompatibilityScore_general with the last argument (fullMergedVertices) set to false.
				affectedVerticesInOrigGraph.addAll(eqClass.getStates());
			
		computeClosure(coregraph,affectedVerticesInOrigGraph,m.getPredictionLen());// here we expect predictions to be based on a few past vertices, hence computing closure in the forward direction.
		influentialVerticesInOrigGraph.addAll(affectedVerticesInOrigGraph);
		computeClosure(inverseGraph,influentialVerticesInOrigGraph,m.getPredictionLen());
		LearnerGraph merged = MergeStates.mergeCollectionOfVertices(coregraph, null, verticesToMerge, influentialVerticesInOrigGraph,false);// by the virtue of using a small set of
		// vertices influentialVerticesInOrigGraph to build a graph, both construction of a merged graph and construction of the inverse of this merged graph should be fast 
		// they are currently contributing a lot of time to the runtime cost of running the learner).
		
		for(EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass:verticesToMerge)
			if (eqClass.getStates().size() > 1)
				affectedVerticesInMergedGraph.add(eqClass.getMergedVertex());// we have to do this separately to the computation of affected vertices in the original graph because merged vertices are not constructed (that is, getMergedVertex() returns null) until the merged graph is built.
		
		computeClosure(merged,affectedVerticesInMergedGraph,m.getPredictionLen());
//System.out.println("coregraph: "+coregraph.getStateNumber()+" states, merged graph: "+merged.getStateNumber()+" states, affected: "+affectedVerticesInOrigGraph.size()+" affected in merged: "+affectedVerticesInMergedGraph.size());
		long origInconsistencyRelativeToChanges = 0;
		for(CmpVertex v:affectedVerticesInOrigGraph)
			if (v.isAccept()) // we only consider prefix-closed languages where there are never any outgoing transitions from reject-states and hence no potential for inconsistencies.
			{
				if (origInconsistencies.containsKey(v))
					origInconsistencyRelativeToChanges+=origInconsistencies.get(v);
				else
				{
					long inconsistency = origClassifier.checkFanoutInconsistency(v,checker,false);
					origInconsistencies.put(v,inconsistency);// cache the inconsistency of the original graph. This will be reused across numerous invocations of computeInconsistencyOfAMerger on the same original graph.
					origInconsistencyRelativeToChanges+=inconsistency;
				}
			}
		MarkovClassifier<CmpVertex,LearnerGraphCachedData> cl = new MarkovClassifier<CmpVertex,LearnerGraphCachedData>(m, merged, null);
		long mergedInconsistencyRelativeToChanges = cl.computeConsistencyForSpecificVertices(checker,affectedVerticesInMergedGraph,false);
		return mergedInconsistencyRelativeToChanges - origInconsistencyRelativeToChanges;
	}
	
	/** Given a collection of vertices that is to be merged, computes the 
	 * inconsistency of the outcome of a merger, using a number of different models and classifiers. 
	 * This makes it possible to make predictions both forward, inverse in sideways at the same time. 
	 * This is defined as the difference in inconsistency scores between the merged
	 * graph and the original graph. This routine aims to avoid computation of an 
	 * inconsistency of complete graphs, instead looking only at those vertices that 
	 * are affected by mergers and the vicinity of them. The merged graph should be 
	 * constructed by merging vertices in verticesToMerge, otherwise merged vertices 
	 * would not be available as part of elements of {@link AMEquivalenceClass} and 
	 * we'll crash.
	 *
	 * @param coregraph the original graph
	 * @param inverseGraph the inverse of the original graph
	 * @param verticesToMerge vertices to merge in the original graph, computed by the generalised scoring routine.
	 * @param m Markov model used to compute inconsistencies.
	 * @param origClassifier the classifier used on the original graph.
	 * @param maxPredictionLength is the maximal number of transitions in either direction of the vertices of interest that may affect the scores. This is computed as a maximal value of <tt>m.getPredictionLen()</tt>  
	 */
	public static long [] computeInconsistencyOfAMergerWithMultipleClassifiers(LearnerGraph coregraph, LearnerGraphND inverseGraph,  List<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge,
			Map<CmpVertex,Long> origInconsistencies, MarkovModel [] m, MarkovClassifier<CmpVertex,LearnerGraphCachedData> [] origClassifier, ConsistencyChecker [] checker, int maxPredictionLength)
	{
		assert m.length == origClassifier.length;
		assert origClassifier.length == checker.length;
		Set<CmpVertex> affectedVerticesInMergedGraphAll = new LinkedHashSet<CmpVertex>(), affectedVerticesInMergedGraphBackward = new LinkedHashSet<CmpVertex>(),
				affectedVerticesInOrigGraphForward = new LinkedHashSet<CmpVertex>(),affectedVerticesInOrigGraphBackward = new LinkedHashSet<CmpVertex>(),
						influentialVerticesInOrigGraphAll = new LinkedHashSet<CmpVertex>(),influentialVerticesInOrigGraphBackward = new LinkedHashSet<CmpVertex>();

		for(EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass:verticesToMerge)
			if (eqClass.getStates().size() > 1)
				// only look at vertices affected by mergers, it is this property that permits a call to computePairCompatibilityScore_general with the last argument (fullMergedVertices) set to false.
				affectedVerticesInOrigGraphForward.addAll(eqClass.getStates());
		affectedVerticesInOrigGraphBackward.addAll(affectedVerticesInOrigGraphForward);
		
		computeClosure(coregraph,affectedVerticesInOrigGraphForward,maxPredictionLength);// here we expect predictions to be based on a few past vertices, hence computing closure in the forward direction.
		computeClosure(inverseGraph,affectedVerticesInOrigGraphBackward,maxPredictionLength);// here we expect predictions to be based on a few past vertices, hence computing closure in the backward direction.
		
		// now we collect together all the vertices that may affect scores of the vertices directly or indirectly affected by mergers.
		// (vertices directly or indirectly affected by mergers are affectedVerticesInOrigGraph; vertices that may affect affectedVerticesInOrigGraph are influentialVerticesInOrigGraph).
		influentialVerticesInOrigGraphAll.addAll(affectedVerticesInOrigGraphForward);influentialVerticesInOrigGraphBackward.addAll(affectedVerticesInOrigGraphBackward);affectedVerticesInOrigGraphBackward = null;
		
		computeClosure(inverseGraph,influentialVerticesInOrigGraphAll,maxPredictionLength);
		computeClosure(coregraph,influentialVerticesInOrigGraphBackward,maxPredictionLength);influentialVerticesInOrigGraphAll.addAll(affectedVerticesInOrigGraphBackward);
		// at this point, affectedVerticesInOrigGraphBackward is not needed and affectedVerticesInOrigGraph is created to store all of the affected vertices.
		Set<CmpVertex> affectedVerticesInOrigGraph = affectedVerticesInOrigGraphForward;affectedVerticesInOrigGraphForward.addAll(affectedVerticesInOrigGraphBackward);
		affectedVerticesInOrigGraphForward = null;affectedVerticesInOrigGraphBackward = null;
		LearnerGraph merged = MergeStates.mergeCollectionOfVertices(coregraph, null, verticesToMerge, influentialVerticesInOrigGraphAll,false);// by the virtue of using a small set of
		// vertices influentialVerticesInOrigGraph to build a graph, both construction of a merged graph and construction of the inverse of this merged graph should be fast 
		// they are currently contributing a lot of time to the runtime cost of running the learner).
		
		for(EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass:verticesToMerge)
			if (eqClass.getStates().size() > 1)
				affectedVerticesInMergedGraphAll.add(eqClass.getMergedVertex());// we have to do this separately to the computation of affected vertices in the original graph because merged vertices are not constructed (that is, getMergedVertex() returns null) until the merged graph is built.
		affectedVerticesInMergedGraphBackward.addAll(affectedVerticesInOrigGraphForward);
		
		computeClosure(merged,affectedVerticesInMergedGraphAll,maxPredictionLength);
		LearnerGraphND mergedInverse=MarkovClassifier.computeInverseGraph(merged);
		computeClosure(mergedInverse,affectedVerticesInMergedGraphBackward,maxPredictionLength);
		affectedVerticesInMergedGraphAll.addAll(affectedVerticesInMergedGraphBackward);
		
		long [] outcome = new long[m.length];
		
//System.out.println("coregraph: "+coregraph.getStateNumber()+" states, merged graph: "+merged.getStateNumber()+" states, affected: "+affectedVerticesInOrigGraph.size()+" affected in merged: "+affectedVerticesInMergedGraph.size());
		for(int i=0;i<m.length;++i)
		{
			long origInconsistencyRelativeToChanges = 0;
			for(CmpVertex v:affectedVerticesInOrigGraph)
			{// here we cannot filter for only accept-states because evaluation of inconsistencies backwards would look at transitions entering reject-states.
				if (origInconsistencies.containsKey(v))
					origInconsistencyRelativeToChanges+=origInconsistencies.get(v);
				else
				{
					long inconsistency = origClassifier[i].checkFanoutInconsistency(v,checker[i],false);
					origInconsistencies.put(v,inconsistency);// cache the inconsistency of the original graph. This will be reused across numerous invocations of computeInconsistencyOfAMerger on the same original graph.
					origInconsistencyRelativeToChanges+=inconsistency;
				}
			}
			MarkovClassifier<CmpVertex,LearnerGraphCachedData> cl = new MarkovClassifier<CmpVertex,LearnerGraphCachedData>(m[i], merged, mergedInverse);// this chooses between the forward and inverse graphs depending on the parameters of markov model, passed as the first argument.
			long mergedInconsistencyRelativeToChanges = cl.computeConsistencyForSpecificVertices(checker[i],affectedVerticesInMergedGraphAll,false);
			outcome[i] = mergedInconsistencyRelativeToChanges - origInconsistencyRelativeToChanges;
		}
		return outcome;
	}

	/** Walks all paths of the specified distance and states encountered are added to the provided set.
	 * 
	 * @param coregraph graph to explore
	 * @param affectedVerticesInGraph where to accumulate encountered vertices
	 * @param distance how far to explore
	 */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>>
	 void computeClosure(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> coregraph, Set<CmpVertex> affectedVerticesInGraph, int distance)
	{
		if (distance <= 0)
			return;// nothing to do.
		
		final Queue<CmpVertex> currentExplorationBoundary = new LinkedList<CmpVertex>();// FIFO queue
		final Map<CmpVertex,Integer> visited = new HashMap<CmpVertex,Integer>();
		for(CmpVertex v:affectedVerticesInGraph) 
		{ 
			visited.put(v, 0);currentExplorationBoundary.offer(v); 
		}
		
		CmpVertex explorationElement = null;
		while(!currentExplorationBoundary.isEmpty())
		{
			explorationElement = currentExplorationBoundary.remove();
			int exploredDistance = visited.get(explorationElement)+1;
			for(Entry<Label,TARGET_TYPE> transitionND:coregraph.transitionMatrix.get(explorationElement).entrySet())
				for(CmpVertex targetState:coregraph.getTargets(transitionND.getValue()))
				{
					Integer distanceSeen = visited.get(targetState);
					
					if (distanceSeen == null || distanceSeen > exploredDistance)
					{
						visited.put(targetState,exploredDistance);// record the new or revised distance
						affectedVerticesInGraph.add(targetState);// ensure we record that this vertex has to be explored as part of computation.
						
						if (exploredDistance < distance) // only explore from the found element if we did not reach the limit.
							currentExplorationBoundary.offer(targetState);// ensure we explore this element later in our breadth-first search.
					}
				}
		}
	}
	
	/** Given the markov model in this classifier and a graph, this method obtains inconsistency for the supplied graph. This is implemented by creating another classifier with the same parameters but a supplied graph as an argument. */
	@SuppressWarnings("rawtypes")
	public static long computeInconsistency(AbstractLearnerGraph gr, AbstractLearnerGraph grInverse,MarkovModel model,  ConsistencyChecker checker, boolean displayTrace)
	{
		MarkovClassifier cl = new MarkovClassifier(model, gr, grInverse);return cl.computeConsistency(checker,displayTrace);
	}

	/** Implementations of this interface are used to check for consistency between Markov predictions and actual mergers. For instance, we could have a transition with a specific label predicted from a state where there is no transition
	 * with such a label or a positive transition is predicted whereas a negative transition is present. Another case is where no transition is predicted whereas a transition is present in an actual graph.
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
		/** Returns an alphabet to use for a specific vertex. This would usually return a collection of labels on transitions from that state but may 
		 * also be used to return an entire alphabet in order to check that not only that all the existing transitions are not predicted as 
		 * non-existing but also that all those that do not exist are not predicted as those that are to exist. The latter kind of check is 
		 * useful on states where we expect all outgoing transitions to be correctly identified. 
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
		 * Given two outcomes, returns a new value of the prediction to be associated with the label. 
		 * Can return {@link MarkovOutcome#failure} if the label is to be labelled as inconsistent and excluded from any other comparisons. 
		 * With this returning {@link MarkovOutcome#failure}, we can have multiple inconsistencies per label, associated with 
		 * different paths leading to a state of interest (or different paths leading from it for predictions backwards) and hence different Markov predictions. 
		 */
		public MarkovOutcome labelConsistent(MarkovOutcome actual,MarkovOutcome predicted);
		
		/**
		 * Inconsistencies are based on whether predicted paths are matched by the actual ones. 
		 * Path prediction is based on availability of predictionlen-path in a Markov matrix.
		 * If a path is not found, all paths from the current state are seen as not predicted, 
		 * hence where we only consider positive paths all paths from the current state appear 
		 * inconsistent with predictions. Returning false here avoids such a problem.  
		 */
		public boolean considerPathsWithPrefixMissingInMarkov();
		
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
		public Collection<Label> obtainAlphabet(AbstractLearnerGraph graph,@SuppressWarnings("unused") CmpVertex v) {
			return graph.getCache().getAlphabet();
		}

		@Override
		public boolean considerPathsWithPrefixMissingInMarkov() {
			return false;
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

		@Override
		public Collection<Label> obtainAlphabet(AbstractLearnerGraph graph,CmpVertex v) {
			return ((Map)graph.transitionMatrix.get(v)).keySet();
		}

		@Override
		public boolean considerPathsWithPrefixMissingInMarkov() {
			return false;
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

		@Override
		public Collection<Label> obtainAlphabet(AbstractLearnerGraph graph,CmpVertex v) {
			return ((Map)graph.transitionMatrix.get(v)).keySet();
		}

		@Override
		public boolean considerPathsWithPrefixMissingInMarkov() {
			return false;
		}
	}

	
	/** This one counts all inconsistencies but does not blacklist any label. */
	public static class DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes extends DifferentPredictionsInconsistencyNoBlacklisting
	{
		
		@Override
		public boolean considerPathsWithPrefixMissingInMarkov() {
			return true;
		}
	}

	
	/** This one counts all inconsistencies but does not blacklist any label. */
	public static class DifferentPredictionsInconsistencyAcrossAllElementsOfAlphabet implements ConsistencyChecker
	{
		@Override
		public boolean consistent(MarkovOutcome actual, MarkovOutcome predicted) 
		{
			return MarkovOutcome.ensureConsistencyBetweenOpinions(actual,predicted) != MarkovOutcome.failure;
		}

		@SuppressWarnings("unused")
		@Override
		public MarkovOutcome labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) 
		{
			return actual;
		}

		@Override
		public Collection<Label> obtainAlphabet(AbstractLearnerGraph graph,@SuppressWarnings("unused") CmpVertex v) {
			return graph.getCache().getAlphabet();
		}

		@Override
		public boolean considerPathsWithPrefixMissingInMarkov() {
			return false;
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
	 * @param pathsOfInterest paths considered for prediction, filled in by this method.. Ignored if <i>null</i>.
	 * @param pathsOrSets if true, we are looking at sequences of transitions to/from a state of interest. 
	 * If false, we are looking for sets of labels on transitions into/out of a state of interest. Both are 
	 * represented as paths because we need to do a lookup in a collection of paths and numbering of labels 
	 * permits elements such sets to be represented as sequences.
	 * @param pathsOfInterest paths considered for prediction. Ignored if <i>null</i>.
	 * Each such path had an outgoing label added and possibly <i>pathBeyondCurrentState</i> appended to it before being passed into Markov and the summary of the outcomes of such predictions is returned by this method.
	 * @return map from labels to predictions.
	 */
	@SuppressWarnings("unchecked")
	public Map<Label, MarkovOutcome> predictTransitionsFromState(CmpVertex vert, final List<Label> pathBeyondCurrentState, int chunkLength,final boolean pathsOrSets, final Collection<List<Label>> pathsOfInterest)
	{
		assert vert.isAccept();
		int lengthOfPathBeyond = pathBeyondCurrentState == null?0:pathBeyondCurrentState.size();
		if (lengthOfPathBeyond+1 > chunkLength)
			throw new IllegalArgumentException("supplied pathBeyondCurrentState is too long and does not permit exploration");
		if (!model.predictForwardOrSideways && lengthOfPathBeyond>0)
			throw new IllegalArgumentException("sideways predictions cannot be made by extension of earlier sideways predictions");

		final Set<Label> failureLabels = new TreeSet<Label>();
		final Map<Label,MarkovOutcome> outgoing_labels_probabilities=
				graph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY? new ArrayMapWithSearchPos<Label,MarkovOutcome>() : new HashMap<Label,MarkovOutcome>();
        WalkThroughAllPathsOfSpecificLength(graphToUseForPrediction,vert,chunkLength-1-lengthOfPathBeyond, pathsOrSets,new ForEachCollectionOfPaths() 
        {
			@Override
			public void handlePath(List<Label> pathToNewState) 
			{
				if (pathsOfInterest != null)
					pathsOfInterest.add(pathToNewState);

				List<Label> partOfTraceUsedInMarkovPredictions=new ArrayList<Label>(pathToNewState.size());
				if (predictionGraphInverted)
				{
					for(int i=pathToNewState.size()-1;i>=0;--i) partOfTraceUsedInMarkovPredictions.add(pathToNewState.get(i));if (pathBeyondCurrentState != null) partOfTraceUsedInMarkovPredictions.addAll(pathBeyondCurrentState);
				}
				else
				{
					partOfTraceUsedInMarkovPredictions.addAll(pathToNewState);if (pathBeyondCurrentState != null) partOfTraceUsedInMarkovPredictions.addAll(pathBeyondCurrentState);
				}
				Map<Label,PTASequenceEngine.Node> lastElementToPrediction = model.markovMatrix.getMapFromLabelsToPredictions(partOfTraceUsedInMarkovPredictions);

				for(Label label:graph.getCache().getAlphabet())
				{
					if (!failureLabels.contains(label))
					{// if the labels is not already recorded as being inconsistently predicted
						MarkovOutcome predictedFromEalierTrace = outgoing_labels_probabilities.get(label);
    					
    					PredictionForSequence prediction = MarkovMatrixEngine.getPredictionIfExists(lastElementToPrediction, label);
    					MarkovOutcome predicted_from_Markov= prediction!=null?prediction.prediction:null;
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
	 * @param pathsOrSets if true, we are looking at sequences of transitions to/from a state of interest. 
	 * If false, we are looking for sets of labels on transitions into/out of a state of interest. Both are 
	 * represented as paths because we need to do a lookup in a collection of paths and numbering of labels 
	 * permits elements such sets to be represented as sequences.
	 */
	public void updateMarkov(CmpVertex vert, int chunkLength, final boolean pathsOrSets)
	{
		List<List<Label>> markovPathsToUpdate = new LinkedList<List<Label>>();
		predictTransitionsFromState(vert,null,chunkLength,pathsOrSets,markovPathsToUpdate);

	    // Now we iterate through all the labels and update entries in markovEntriesToUpdate depending on the outcome.
	    for(Label lbl:graph.getCache().getAlphabet())
	    {
	    	@SuppressWarnings("rawtypes")
			Object targets = ((Map)graphToCheckForConsistency.transitionMatrix.get(vert)).get(lbl);
	    	if (targets != null) // there are transitions with the considered label, hence update Markov
	    	{
		    	for(List<Label> pathToUseWithMarkovToPredictOutgoing:markovPathsToUpdate)
		    	{
					List<Label> pathToUpdateInMarkov=new ArrayList<Label>(pathToUseWithMarkovToPredictOutgoing.size());
					if (predictionGraphInverted)
					{
						for(int i=pathToUseWithMarkovToPredictOutgoing.size()-1;i>=0;--i) pathToUpdateInMarkov.add(pathToUseWithMarkovToPredictOutgoing.get(i));
					}
					else
					{
						pathToUpdateInMarkov.addAll(pathToUseWithMarkovToPredictOutgoing);
					}
					
					pathToUpdateInMarkov.add(lbl);
					
					MarkovOutcome newValue = null;
					PredictionForSequence prediction = model.markovMatrix.getPredictionAndCreateNewOneIfNecessary(pathToUpdateInMarkov);
					
					boolean foundAccept = false, foundReject = false;
					for(Object vObj:graphToCheckForConsistency.getTargets(targets))
					{
						if ( ((CmpVertex)vObj).isAccept() ) foundAccept = true;
						if ( !((CmpVertex)vObj).isAccept() ) foundReject = true;
					}
					
					// By construction of an inverse graph and its immutability, it is either accept, reject, or both. getTargets will never be empty.
					
					if (foundAccept && foundReject)
						throw new IllegalArgumentException("inconsistent inverse graph: path "+pathToUpdateInMarkov+" is both accepted and rejected");
					
					if (foundAccept)
					{
						newValue=MarkovOutcome.positive;prediction.occurrence.add(1, 0);
					}
					else
					{
						newValue=MarkovOutcome.negative;prediction.occurrence.add(0, 1);
					}
					
					prediction.prediction=MarkovOutcome.reconcileOpinions_PosNeg_Overrides_Null(prediction.prediction,newValue);
		    	}
	    	}
	    }
	}

	/** Determines how consistent a graph is compared to the data in the Markov model.
	 * @param checker Consistency checker to use for predictions, usually based on a static method from {@link MarkovOutcome}.
	 * @param displayTrace whether to display details of the computation performed
	 * @return true the number of inconsistencies
	 */
	public long computeConsistency(ConsistencyChecker checker, boolean displayTrace)
	{
		long accumulatedInconsistency = 0;
		for(CmpVertex v:graph.transitionMatrix.keySet()) if (v.isAccept()) accumulatedInconsistency+=checkFanoutInconsistency(v,checker,displayTrace);
		return accumulatedInconsistency;
	}

	/** Determines how consistent a graph is compared to the data in the Markov model.
	 * @param checker Consistency checker to use for predictions, usually based on a static method from {@link MarkovOutcome}.
	 * @param statesToConsider state to limit the exploration to. This is aimed to speedup computation of inconsistencies as an outcome of EDSM mergers, where only a subset of states are affected and we hence do not wish to run the computation across an entire machine.
	 * @param displayTrace whether to display details of the computation performed
	 * @return the number of inconsistencies.
	 */
	public long computeConsistencyForSpecificVertices(ConsistencyChecker checker, Collection<CmpVertex> statesOfInterest, boolean displayTrace)
	{
		long accumulatedInconsistency = 0;
		for(CmpVertex v:statesOfInterest)
			if (v.isAccept())
			{
				long inconsistency = checkFanoutInconsistency(v,checker,displayTrace);
				accumulatedInconsistency+=inconsistency;
			}
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
		if (!vert.isAccept())
			return 0;// reject-vertices cannot have outgoing transitions (we are considering prefix-closed languages) and hence a score of zero makes good sense.
		
		final Collection<Label> outgoingLabels = checker.obtainAlphabet(graphToCheckForConsistency,vert);
		final Map<Label,MarkovOutcome> outgoing_labels_value=new HashMap<Label,MarkovOutcome>();
		//for(Label l:alphabet) outgoing_labels_probabilities.put(l, UpdatableOutcome.unknown);
		final AtomicLong inconsistencies = new AtomicLong();

		for(Entry<Label, TARGET_TYPE> entry:graph.transitionMatrix.get(vert).entrySet())
			for(CmpVertex transitionTarget:graph.getTargets(entry.getValue()))
			{
				outgoing_labels_value.put(entry.getKey(),transitionTarget.isAccept()?MarkovOutcome.positive:MarkovOutcome.negative);
			}
		
		WalkThroughAllPathsOfSpecificLength(graphToUseForPrediction,vert,model.getPredictionLen(),model.pathsOrSets,new ForEachCollectionOfPaths() 
        {
			@Override
			public void handlePath(List<Label> pathToNewState) 
			{
				List<Label> partOfTraceUsedInMarkovPredictions = new ArrayList<Label>();
				
				if (predictionGraphInverted)
				{
					for(int i=pathToNewState.size()-1;i>=0;--i) partOfTraceUsedInMarkovPredictions.add(pathToNewState.get(i));
				}
				else
				{
					partOfTraceUsedInMarkovPredictions.addAll(pathToNewState);
				}
				
				Map<Label,PTASequenceEngine.Node> mapFromLastLabelToNodes = model.markovMatrix.getMapFromLabelsToPredictions(partOfTraceUsedInMarkovPredictions);
				
				if (checker.considerPathsWithPrefixMissingInMarkov() || mapFromLastLabelToNodes != null) // we skip everything where a path was not seen in PTA unless we are asked to consider all such paths.
    				for(Label label:outgoingLabels)
    				{
						MarkovOutcome labels_occurrence= outgoing_labels_value.get(label);
						if (labels_occurrence != MarkovOutcome.failure)
						{
	    					PredictionForSequence prediction = MarkovMatrixEngine.getPredictionIfExists(mapFromLastLabelToNodes, label);
	    					MarkovOutcome predicted_from_Markov=prediction == null?null:prediction.prediction;
	    					if (predicted_from_Markov != MarkovOutcome.failure)
	    					{// if training data does not lead to a consistent outcome for this label because chunk length is too small, not much we can do, but otherwise we are here and can make use of the data
	    						if (!checker.consistent(labels_occurrence, predicted_from_Markov))
	    						{
	    							inconsistencies.addAndGet(1);// record inconsistency
	    							if (displayTrace) System.out.println("inconsistency at state "+vert+" because path "+partOfTraceUsedInMarkovPredictions+" followed by " + label + " is Markov-predicted as "+predicted_from_Markov+" but earlier value is "+labels_occurrence+" total inconsistencies: "+inconsistencies);
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
		Map<CmpVertex,Map<Label,MarkovOutcome>> state_outgoing=
				graph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY? 
						new ArrayMapWithSearch<CmpVertex,Map<Label,MarkovOutcome>>(graph.vertPositiveID,graph.vertNegativeID) : 
							new HashMap<CmpVertex,Map<Label,MarkovOutcome>>(graph.vertPositiveID+graph.vertNegativeID);

    	for(CmpVertex vert:graph.transitionMatrix.keySet())
    		if(vert.isAccept() )
            {
        	   Map<Label,MarkovOutcome> outgoing_labels_probabilities=predictTransitionsFromState(vert,null,model.getChunkLen(),model.pathsOrSets,null);
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
    	for(Entry<CmpVertex,Map<Label,TARGET_TYPE>> entry:graph.transitionMatrix.entrySet())
     		if(entry.getKey().isAccept() )
            {
    			// it would be more efficient if I passed a mock of a collection instead of an actual one but for small graphs it does not matter.
    			long value=checkFanoutInconsistency(entry.getKey(),checker,false);
    			if (value > 0)
    			{
    				predictTransitionsFromState(entry.getKey(),null,model.getChunkLen(),model.pathsOrSets,collectionOfPaths);
    				int outgoingPaths = 0;
    				for(Entry<Label,TARGET_TYPE> transition:entry.getValue().entrySet())
    					outgoingPaths += graph.getTargets(transition.getValue()).size();
    						
    				double inconsistencyforThisState=((double)value)/(collectionOfPaths.size()*outgoingPaths);collectionOfPaths.clear();
    				outcome += inconsistencyforThisState;
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
	        	  updateMarkov(vert,len, model.pathsOrSets);
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
	 * Where multiple paths can be followed from the same state, merges all states from which any of the paths can be followed. Depending on the input, can be used for paths in the forward 
	 * direction or in the inverse one (in which case graphsToCheckForPaths should also be an inverse of a graph of interest, to match the paths being considered).
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
					// now we record which paths leave each vertex and separately which vertices are at root of which paths. If there are A-a-> , A-b-> , B-a-> , C-b->, 
					// vertToPaths will have IDs of a and b from A, just a from B and b from C;
					// idToVerticesToMerge will map A,B to a and C,A to B. We then need to ensure that A,B are merged together and since both A,C have b in common, C is also merged into A,B.
					vertToPaths.get(v).add(path.getKey());idToVerticesToMerge.get(path.getKey()).add(v);
				}
		}
		
		// now we start merging sets of vertices until no two of them have paths in common.
		Set<Integer> pathsFromAnyOfVerts=null;
		Set<CmpVertex> verts = new TreeSet<CmpVertex>();
		
		Set<CmpVertex> vertsConsidered = new TreeSet<CmpVertex>();Collection<Set<CmpVertex>> setsConsidered=new LinkedList<Set<CmpVertex>>();
		do
		{
			pathsFromAnyOfVerts=null;verts.clear();vertsConsidered.clear();setsConsidered.clear();
			
			for(Entry<CmpVertex,Set<Integer>> entry:vertToPaths.entrySet())
				if (!vertsConsidered.contains(entry.getKey())) // we only look at vertices that were not seen before on this iteration of merging
				{
					verts = new TreeSet<CmpVertex>();
					for(Integer p:entry.getValue())
						verts.addAll(idToVerticesToMerge.get(p));// these are all the vertices that have path p from them, we will now merge sets of paths for all of them to form pathsFromAnyOfVerts
					
					for(CmpVertex v:verts)
					{
						Set<Integer> pathsForVert = vertToPaths.get(v);
						if (pathsForVert != entry.getValue())
						{// this state v is different from the currently considered state entry.getKey(), perform the merge. Comparison by reference is possible due to 'put(v,pathsFromAnyOfVerts)' below that makes sure that states with identical sets of paths are not merged.
							if (pathsFromAnyOfVerts == null)
								pathsFromAnyOfVerts = new TreeSet<Integer>(entry.getValue());
							pathsFromAnyOfVerts.addAll(pathsForVert);
						}
					}
					// pathsFromAnyOfVerts now contains all paths from all the vertices in the collection verts.
					if (pathsFromAnyOfVerts != null)
						break;// new paths have been found, in the loop below we use pathsFromAnyOfVerts as the collection of paths from any state in verts.
					
					vertsConsidered.addAll(verts);setsConsidered.add(verts);// here we update the return value, if we get to the end with pathsFromAnyOfVerts remaining null, we are done and setsConsidered can be returned
				}
			if (pathsFromAnyOfVerts != null)
			{// had to compute a merge of outgoing transitions for all the states in verts.
				for(CmpVertex v:verts)
					vertToPaths.put(v,pathsFromAnyOfVerts);
			}
		}
		while(pathsFromAnyOfVerts != null);
		
		return setsConsidered;
	}

	
	/** Given a graph, computes the proportion of states that can be identified using singleton sequences.
	 * 
	 * @param referenceGraph reference graph
	 * @param whatToMerge paths to check. 
	 * @return proportion of vertices that can be identified by singletons, actually identified with the provided paths.
	 */
	public static double calculateFractionOfStatesIdentifiedBySingletons(LearnerGraph referenceGraph)
	{
		if (referenceGraph.getStateNumber() == 0)
			throw new IllegalArgumentException("empty reference graph");
		
		Set<CmpVertex> uniquelyIdentifiableVertices = new TreeSet<CmpVertex>();
		
		for(Label l:referenceGraph.getCache().getAlphabet())
		{
			CmpVertex vertexIdentified = LearningSupportRoutines.checkSeqUniqueOutgoing(referenceGraph,Arrays.asList(new Label[]{l}));
			if(vertexIdentified != null)
				uniquelyIdentifiableVertices.add(vertexIdentified);
		}
		
		return (double)uniquelyIdentifiableVertices.size()/referenceGraph.getStateNumber();
	}
	
	/** Given the collection of paths and a way to tell which states to merge, computes a proportion of states that could be identified. 
	 * Returns negative if any sequence in the supplied collection exists from more than a single state in a reference graph.
	 * 
	 * @param referenceGraph reference graph
	 * @param whatToMerge paths to check. 
	 * @return proportion of vertices that can be identified by singletons, actually identified with the provided paths.
	 */
	public static double calculateFractionOfIdentifiedStates(LearnerGraph referenceGraph, Collection<List<Label>> whatToMerge)
	{
		if (referenceGraph.getStateNumber() == 0)
			throw new IllegalArgumentException("empty reference graph");
		
		Set<CmpVertex> identifiedVertices = new TreeSet<CmpVertex>();
		
		for(List<Label> l:whatToMerge)
		{
			CmpVertex vertexIdentified = LearningSupportRoutines.checkSeqUniqueOutgoing(referenceGraph,l);
			if (vertexIdentified != null)
				identifiedVertices.add(vertexIdentified);
		}
		
		return (double)identifiedVertices.size()/referenceGraph.getStateNumber();
	}

	/** Given that this classified is instantiate with a reference graph, determines the ration of correct predictions by this classifier to the total number of predictions. 
	 * 
	 * @return fraction of Markov's predictions that are correct.
	 */
	public statechum.Pair<Double,Double> evaluateCorrectnessOfMarkov()
	{
		double outcomePrecision = 0, outcomeRecall = 0;
		
		long correctPredictions=0, numberOfPredictions=0;
		long numberOfExistingPredicted=0;
		for(Entry<CmpVertex,Map<Label,TARGET_TYPE>> entry:graph.transitionMatrix.entrySet())
			if (entry.getKey().isAccept())
			{
				Map<Label, MarkovOutcome> predictions = predictTransitionsFromState(entry.getKey(), null, model.getChunkLen(), model.pathsOrSets, null);
				for(Entry<Label,MarkovOutcome> prediction:predictions.entrySet())
				{
					TARGET_TYPE targetList = entry.getValue().get(prediction.getKey()); 
					assert prediction.getValue() != MarkovOutcome.failure;
					++numberOfPredictions;
					for(CmpVertex target:graph.getTargets(targetList))
					{
						if (prediction.getValue() == MarkovOutcome.positive)
						{
							if(target != null && target.isAccept())
								++correctPredictions;
						}
						if (prediction.getValue() == MarkovOutcome.negative)
						{
							if (target == null || !target.isAccept())
								++correctPredictions;
						}
					}
				}
				
				
				for(Entry<Label,TARGET_TYPE> existingTransition:entry.getValue().entrySet())
				{
					MarkovOutcome predictedTarget = predictions.get(existingTransition.getKey());
					for(CmpVertex existing:graph.getTargets(existingTransition.getValue()))
					{
						if (existing.isAccept() && predictedTarget == MarkovOutcome.positive)
							++numberOfExistingPredicted;
						if (!existing.isAccept() && predictedTarget == MarkovOutcome.negative)
							++numberOfExistingPredicted;
					}
				}
			}
		if (numberOfPredictions > 0) outcomePrecision = (double)correctPredictions/numberOfPredictions;
		int edgeNumber = graph.pathroutines.countEdges();
		if (edgeNumber > 0) outcomeRecall = (double)numberOfExistingPredicted/edgeNumber;
		return new statechum.Pair<Double, Double>(outcomePrecision, outcomeRecall);
	}
}
