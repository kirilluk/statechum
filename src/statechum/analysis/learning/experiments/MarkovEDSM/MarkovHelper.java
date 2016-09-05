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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import statechum.Label;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.MarkovClassifierLG;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.LearnerThatCanClassifyPairs;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.collections.ArrayMapWithSearchPos;

/** This class makes it possible to integrate Markov computations into any desired learner. In this way, it is used for both Markov experiment and Weka experiment. */
public class MarkovHelper
{
	LearnerGraph extendedGraph = null;
	MarkovClassifierLG cl=null;
	LearnerGraphND inverseGraph = null;
	long comparisonsPerformed = 0;

	public final MarkovParameters markovParameters;
	
	Map<CmpVertex,Long> inconsistenciesPerVertex = null;

	public LearnerGraph coregraph = null;

	protected MarkovModel Markov;
	protected ConsistencyChecker checker;
	
	public MarkovHelper(MarkovParameters pars)
	{
		markovParameters = pars;
	}
	
	public void setMarkov(MarkovModel m) {
		Markov=m;
	}

	public void setChecker(ConsistencyChecker c) {
		checker=c;
	}

	/** This method orders the supplied pairs in the order of best to merge to worst to merge. 
	 * We do not simply return the best pair because the next step is to check whether pairs we think are right are classified correctly.
	 * <p/> 
	 * Pairs are supposed to be the ones from {@link LearnerThatCanClassifyPairs#filterPairsBasedOnMandatoryMerge(Stack, LearnerGraph)} where all those not matching mandatory merge conditions are not included.
	 * Inclusion of such pairs will not affect the result but it would be pointless to consider such pairs.
	 * @param extension_graph 
	 * @param learnerGraph 
	 * @param pairs 
	 */
	public List<PairScore> classifyPairs(Collection<PairScore> pairs, LearnerGraph graph, LearnerGraph extension_graph)
	{
		boolean allPairsNegative = true;
		for(PairScore p:pairs)
		{
			assert p.getScore() >= 0;
			
			if (p.getQ().isAccept() || p.getR().isAccept()) // if any are rejects, add with a score of zero, these will always work because accept-reject pairs will not get here and all rejects can be merged.
			{
				allPairsNegative = false;break;
			}
		}
		ArrayList<PairScore> possibleResults = new ArrayList<PairScore>(pairs.size()),nonNegPairs = new ArrayList<PairScore>(pairs.size());
		if (allPairsNegative)
			possibleResults.addAll(pairs);
		else
		{
			for(PairScore p:pairs)
			{
				assert p.getScore() >= 0;
				if (!p.getQ().isAccept() || !p.getR().isAccept()) // if any are rejects, add with a score of zero, these will always work because accept-reject pairs will not get here and all rejects can be merged.
					possibleResults.add(new WaveBlueFringe.PairScoreWithDistance(p,0));
				else
					nonNegPairs.add(p);// meaningful pairs, will check with the classifier
			}

			for(PairScore p:nonNegPairs)
			{
				double d = MarkovScoreComputation.computeMMScoreImproved(p,graph, extension_graph);
				if(d >= 0.0)
					possibleResults.add(new WaveBlueFringe.PairScoreWithDistance(p, d));
			}
				
			Collections.sort(possibleResults, new Comparator<PairScore>(){

				@Override
				public int compare(PairScore o1, PairScore o2) {
					int outcome = (int) Math.signum( ((WaveBlueFringe.PairScoreWithDistance)o2).getDistanceScore() - ((WaveBlueFringe.PairScoreWithDistance)o1).getDistanceScore());  
					if (outcome != 0)
						return outcome;
					return o2.compareTo(o1);
				}}); 
		}				
		return possibleResults;
	}

	/** The following routine is to be called by a user integrating (mixing) this class into a learner. */
	public void initComputation(LearnerGraph graph) 
	{
		coregraph = graph;
				 				
		inverseGraph = (LearnerGraphND)MarkovClassifier.computeInverseGraph(coregraph,null,true);
		cl = new MarkovClassifierLG(Markov, coregraph, inverseGraph);
	    extendedGraph = null;// this will be built when it is needed and value stored until next call to initComputation.
		inconsistenciesPerVertex =
				coregraph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY && (coregraph.getStateNumber() > coregraph.config.getThresholdToGoHash() || coregraph.config.getAlwaysUseTheSameMatrixType())?
						new ArrayMapWithSearchPos<CmpVertex,Long>(coregraph.getStateNumber()):
						new TreeMap<CmpVertex,Long>();
	}
	
	public long onlyComputeInconsistency(PairScore p)
	{
		if(p.getQ().isAccept()==false && p.getR().isAccept()==false)
			return 0;
		List<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();//coregraph.getStateNumber()+1);// to ensure arraylist does not reallocate when we fill in the last element
		int genScore = coregraph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge, false);
		long score= genScore;
		if (genScore >= 0)
		{			
			score = MarkovClassifier.computeInconsistencyOfAMerger(coregraph, inverseGraph, verticesToMerge, inconsistenciesPerVertex, Markov, cl, checker);
		}		
		return score;
	}
	
	public long computeScoreBasedOnInconsistencies(PairScore p) 
	{
		if(p.getQ().isAccept()==false && p.getR().isAccept()==false)
			return 0;
		++comparisonsPerformed;
		long currentInconsistency = 0;
		List<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();//coregraph.getStateNumber()+1);// to ensure arraylist does not reallocate when we fill in the last element
		int genScore = coregraph.pairscores.computePairCompatibilityScore_general(p, null, verticesToMerge, false);
		long score= genScore;
		if (genScore >= 0)
		{			
			currentInconsistency = MarkovClassifier.computeInconsistencyOfAMerger(coregraph, inverseGraph, verticesToMerge, inconsistenciesPerVertex, Markov, cl, checker);
			
			score=Math.round(genScore-markovParameters.weightOfInconsistencies*currentInconsistency);
			
			if (markovParameters.useNewScoreNearRoot && genScore <= 1) // could do with 2 but it does not make a difference.
			{
				if (!MarkovClassifier.checkIfThereIsPathOfSpecificLength(inverseGraph,p.getR(),Markov.getPredictionLen()) ||
						!MarkovClassifier.checkIfThereIsPathOfSpecificLength(inverseGraph,p.getQ(),Markov.getPredictionLen()))
				{
					if (extendedGraph == null)
						extendedGraph = cl.constructMarkovTentative();
					score = //(long)MarkovScoreComputation.computeMMScoreImproved(p,coregraph, extendedGraph);
						MarkovScoreComputation.computenewscore(p, extendedGraph);// use a different score computation in this case
				}
			}
		}
		return score;
	}

	public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(CmpVertex currentRed) 
	{
		return	WaveBlueFringe.obtainSurroundingTransitions(coregraph,inverseGraph,currentRed);
	}
}