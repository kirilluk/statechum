/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * StateChum. If not, see <http://www.gnu.org/licenses/>.
 */

package statechum.analysis.learning.rpnicore;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.TreeMap;
import java.util.Map.Entry;

import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.observers.DummyLearner;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.Smt;
import statechum.model.testset.PTASequenceEngine;

public class SmtLearnerDecorator extends DummyLearner 
{
	protected final LabelRepresentation lbl;
	protected final Smt solver;
	
	public SmtLearnerDecorator(Learner learner,LabelRepresentation labels) {
		super(learner);lbl=labels;
		
		Smt.loadLibrary();Smt.closeStdOut();solver = new Smt();
	}

	/* (non-Javadoc)
	 * @see statechum.analysis.learning.observers.DummyLearner#AugmentPTA(statechum.analysis.learning.rpnicore.LearnerGraph, statechum.analysis.learning.observers.Learner.RestartLearningEnum, java.util.List, boolean, statechum.JUConstants)
	 */
	@Override
	public void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind,
			List<String> sequence, boolean accepted, JUConstants newColour) {

		decoratedLearner.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
		if (ptaKind == RestartLearningEnum.restartHARD)
			lbl.AugmentAbstractStates(solver,sequence, pta,accepted);
	}

	/* (non-Javadoc)
	 * @see statechum.analysis.learning.observers.DummyLearner#CheckWithEndUser(statechum.analysis.learning.rpnicore.LearnerGraph, java.util.List, int, java.lang.Object[])
	 */
	@Override
	public Pair<Integer, String> CheckWithEndUser(LearnerGraph graph,
			List<String> question, int responseForNoRestart, int lengthInHardFacts, Object[] options) 
	{
		int smtAnswer = lbl.CheckWithEndUser(solver, question);
		System.err.println("question: "+question+" expected for no restart: "+responseForNoRestart+" smt returned: "+smtAnswer);
		if (smtAnswer != responseForNoRestart)
			return new Pair<Integer, String>(smtAnswer,null);
		
		return decoratedLearner.CheckWithEndUser(graph, question, responseForNoRestart, lengthInHardFacts, options);
	}

	/** A map from merged vertices to collections of original vertices they correspond to.
	 */
	TreeMap<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> vertexToEqClass = null; 

	/** The number of states in the original PTA. Only used for consistency checking. */
	int vertexTotal = 0;
	
	/** Whether the vertex to eqClass map has already been built and we did not go through any merge or restart. */
	boolean vertexToEqClassUpToDate = false;
	
	/** Each time a merge happens, we need to rebuild a map from merged vertices to collections 
	 * of original vertices they correspond to. This is the purpose of this method.
	 */
	protected void updateVertexToEqClassMap(LearnerGraph graph)
	{
		if (vertexToEqClassUpToDate && vertexToEqClass != null)
			return;
		
		// First, we build a collection of states of the original PTA which correspond to the each merged vertex.
		TreeMap<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> newVertexToEqClass = new TreeMap<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		if (graph.learnerCache.getMergedStates() == null)
		{// the case when we get here for the first time or right after a reset
			int i=0;vertexTotal = graph.getStateNumber();
			for(CmpVertex vertex:graph.transitionMatrix.keySet())
			{
				AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(i++,graph);
				try {
					eqClass.addFrom(vertex, null);
				} catch (IncompatibleStatesException e) {
					Helper.throwUnchecked("failed to construct an AMEquivalenceClass with a single node", e);
				}

				newVertexToEqClass.put(eqClass.getRepresentative(),eqClass);
			}
		}
		else // after a previous successful merge 
		for(AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:graph.learnerCache.getMergedStates())
		{
			AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> combinedEqClass = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(eqClass.getNumber(),graph);
			for(CmpVertex state:eqClass.getStates())
				try 
				{
					combinedEqClass.mergeWith(vertexToEqClass.get(state));
				} catch (IncompatibleStatesException e) {
					Helper.throwUnchecked("failed to construct a collection of states which have previously been merged successfully", e);
				}
			newVertexToEqClass.put(eqClass.getMergedVertex(),combinedEqClass);
		}
		vertexToEqClass = newVertexToEqClass;
		
		
		if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.ASSERT)))
		{
			int vertexEncountered = 0;
			Map<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> vertexToCollection = new TreeMap<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			for(Entry<CmpVertex,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> eqClass:vertexToEqClass.entrySet())
			{
				for(CmpVertex vert:eqClass.getValue().getStates())
				{
					++vertexEncountered;
					AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> existingClass = vertexToCollection.get(vert);
					if (existingClass != null)
						throw new IllegalArgumentException("classes "+existingClass+" and "+eqClass.getValue()+" share vertex "+vert);
					vertexToCollection.put(vert,eqClass.getValue());
				}
			}
			if (vertexTotal != vertexEncountered)
				throw new IllegalArgumentException("after a merge, the number of states used from the original PTA is "+vertexEncountered+" but the initial PTA has "+vertexTotal+" of them");
		}
	}
	
	/* (non-Javadoc)
	 * @see statechum.analysis.learning.observers.DummyLearner#ChooseStatePairs(statechum.analysis.learning.rpnicore.LearnerGraph)
	 */
	@Override
	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) 
	{
		decoratedLearner.ChooseStatePairs(graph);
		
		// Now go through the pairs and update the scores.
		for(int i=0;i<graph.pairsAndScores.size();++i)
		{
			PairScore pair = graph.pairsAndScores.get(i);
			Iterator<CmpVertex> 
				stateA_iter = vertexToEqClass.get(pair.firstElem).getStates().iterator(),
				stateB_iter = vertexToEqClass.get(pair.secondElem).getStates().iterator();
			boolean finished = false, statesIntersect = false;// using these two variables I can choose whether to check for intersection or non-intersection.
			while(stateA_iter.hasNext() && !finished)
			{
				CmpVertex stateA = stateA_iter.next();
				while(stateB_iter.hasNext() && !finished)
					if (lbl.abstractStatesCompatible(solver, stateA.getID(), stateB_iter.next().getID()))
					{
						finished = true;statesIntersect = true;
					}
			}
			
			if (statesIntersect)
			{
				System.err.println("bumped score for "+new StatePair(pair.firstElem, pair.secondElem));
				graph.pairsAndScores.set(i, new PairScore(pair.firstElem, pair.secondElem,
						pair.getScore()+1,pair.getAnotherScore()));
			}
			else
				System.err.println("no bump for "+new StatePair(pair.firstElem, pair.secondElem));
		}
		return graph.pairscores.getSortedPairsAndScoresStackFromUnsorted();
	}

	@Override
	public LearnerGraph init(Collection<List<String>> plus,	Collection<List<String>> minus) 
	{
		LearnerGraph result= decoratedLearner.init(plus, minus);
		lbl.idToState.clear();
		lbl.mapVerticesToAbstractStates(result);return result;
	}

	@Override
	public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
	{
		LearnerGraph result= decoratedLearner.init(engine, plusSize, minusSize);
		lbl.idToState.clear();
		lbl.mapVerticesToAbstractStates(result);return result;
	}

	/**
	 * @see statechum.analysis.learning.observers.DummyLearner#Restart(statechum.analysis.learning.observers.Learner.RestartLearningEnum)
	 */
	@Override
	public void Restart(RestartLearningEnum mode) 
	{
		decoratedLearner.Restart(mode);
		if (mode == RestartLearningEnum.restartHARD || mode == RestartLearningEnum.restartSOFT)
		{
			vertexToEqClass = null;vertexToEqClassUpToDate = false;
		}
		else
		if (mode == RestartLearningEnum.restartNONE)
			vertexToEqClassUpToDate = false;
	}

	/**
	 * @see statechum.analysis.learning.observers.DummyLearner#AddConstraints(statechum.analysis.learning.rpnicore.LearnerGraph, statechum.analysis.learning.rpnicore.LearnerGraph, java.lang.StringBuffer)
	 */
	@Override
	public boolean AddConstraints(LearnerGraph graph, LearnerGraph outcome,	StringBuffer counterExampleHolder) 
	{
		return decoratedLearner.AddConstraints(graph, outcome, counterExampleHolder);
	}
}
