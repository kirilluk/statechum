/* Copyright (c) 2016 The University of Sheffield.
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
package statechum.analysis.learning.experiments.MarkovEDSM;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.collections.ArrayMapWithSearchPos;

public class MarkovHelperClassifier 
{
	MarkovClassifier [] cl=null;
	LearnerGraphND inverseGraph = null;
	long comparisonsPerformed = 0;

	public final MarkovParameters markovParameters;

	Map<CmpVertex,Long> inconsistenciesPerVertex = null;

	public LearnerGraph coregraph = null;
	
	protected MarkovModel [] markovModel;
	protected ConsistencyChecker [] markovConsistencyChecker;

	public MarkovHelperClassifier(MarkovParameters pars)
	{
		markovParameters = pars;
	}

	public void setMarkovAndChecker(MarkovModel []m,ConsistencyChecker []c) 
	{
		markovModel=m;markovConsistencyChecker=c;
		if (m.length != c.length)
			throw new IllegalArgumentException("the length of models should match that of checkers");
		cl = new MarkovClassifier[m.length];
	}

	/** The following routine is to be called by a user integrating (mixing) this class into a learner. */
	public void initComputation(LearnerGraph graph) 
	{
		coregraph = graph;

		inverseGraph = (LearnerGraphND)MarkovClassifier.computeInverseGraph(coregraph,null,true);
		for(int i=0;i<markovModel.length;++i)
			cl[i] = new MarkovClassifier(markovModel[i], coregraph, inverseGraph);
		inconsistenciesPerVertex =
				coregraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY && (coregraph.getStateNumber() > coregraph.config.getThresholdToGoHash() || coregraph.config.getAlwaysUseTheSameMatrixType())?
						new ArrayMapWithSearchPos<CmpVertex,Long>(coregraph.getStateNumber()):
						new TreeMap<CmpVertex,Long>();
	}

	public long [] computeInconsistency(PairScore p)
	{
		long [] outcome = new long[markovModel.length];
		if(p.getQ().isAccept()==false && p.getR().isAccept()==false)
			return outcome;// return zeroes
		List<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();//coregraph.getStateNumber()+1);// to ensure arraylist does not reallocate when we fill in the last element
		int genScore = coregraph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge, false);
		if (genScore >= 0)
		{			
			outcome = MarkovClassifier.computeInconsistencyOfAMergerWithMultipleClassifiers(coregraph, inverseGraph, verticesToMerge, inconsistenciesPerVertex, markovModel, cl, markovConsistencyChecker, markovParameters.chunkLen-1);
		}		
		else
			for(int i=0;i<markovModel.length;++i)
				outcome[i] = genScore;
		return outcome;
	}
	
	public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(CmpVertex currentRed) 
	{
		return	WaveBlueFringe.obtainSurroundingTransitions(coregraph,inverseGraph,currentRed);
	}
}
