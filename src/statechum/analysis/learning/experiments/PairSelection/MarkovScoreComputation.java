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

package statechum.analysis.learning.experiments.PairSelection;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.Stack;

import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Configuration;
import statechum.JUConstants;
import statechum.Label;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.MarkovModel.MarkovOutcome;
import statechum.analysis.learning.experiments.PairSelection.MarkovPassivePairSelection.PairScoreWithDistance;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.collections.ArrayMapWithSearch;
import statechum.collections.HashMapWithSearch;

public class MarkovScoreComputation 
{

	public static final  int pairchoiceMIN=-2;
	public static final int pairchoiceMAX=3;
	  
	    /** Counts the number with the same score at the top of the stack. */
	     public static int countChoices_old(Stack<PairScore> stack)
	     {
//	    	 System.out.println("abdullah= "+stack.toString());
	         if (stack.isEmpty())
	                 return 0;
	         int outcome = 1;
	         long top = stack.peek().getScore();
	         
	         int i=stack.size()-2;
	         while(i>=0)
	         {
	                 long curr = stack.get(i).getScore();--i;
	                 if (curr != top)
	                         break;
	                 ++outcome;
	         }
	         return outcome;
	     }
	     
	     public static int countChoices(Stack<PairScore> stack)
	     {
	         if (stack.isEmpty())
	                 return 0;
	         int outcome = 1;
	         PairScore top = stack.peek();
	         
	        int i=stack.size()-2;
	         while(i>=0)
	         {
	                 PairScore curr = stack.get(i);--i;
//	                 if (curr.getScore() != top.getScore() || curr.getR() != top.getR() )
		                 if (curr.getScore() != top.getScore())

	                         break;
	                 ++outcome;
	         }
	         return outcome;
	     }
	     
	     public static int countChoicesOther(Stack<PairScore> stack)
	     {
	         if (stack.isEmpty())
	                 return 0;
	         int outcome = 1;
	         PairScore top = stack.peek();
	         
	        int i=stack.size()-2;
	         while(i>=0)
	         {
	                 PairScore curr = stack.get(i);--i;
//	                 if (curr.getScore() != top.getScore() || curr.getR() != top.getR() )
		                 if (curr.getAnotherScore() != top.getAnotherScore())

	                         break;
	                 ++outcome;
	         }
	         return outcome;
	     }
	     
	     public static Stack<PairScore> getChoices(Stack<PairScore> stack)
	     {
	         if (stack.isEmpty())
	                 return null;
	         Stack<PairScore> outcome = new Stack<PairScore>();
	         PairScore top = stack.peek();
	         outcome.add(top);
	         int i=stack.size()-2;
	         while(i>=0)
	         {
	                 PairScore curr = stack.get(i);--i;
	                 if (curr.getScore() != top.getScore())
	                         break;
	                 outcome.add(curr);
	         }
	         return outcome;
	     }
	     
	     public static Stack<PairScore> getChoicesother(Stack<PairScore> stack)
	     {
	         if (stack.isEmpty())
	                 return null;
	         Stack<PairScore> outcome = new Stack<PairScore>();
	         PairScore top = stack.peek();
	         outcome.add(top);
	         int i=stack.size()-2;
	         while(i>=0)
	         {
	                 PairScore curr = stack.get(i);--i;
	                 if (curr.getAnotherScore() != top.getAnotherScore())
	                         break;
	                 outcome.add(curr);
	         }
	         return outcome;
	     }
	     
	     public static Stack<PairScore> getChoicesother_improved(ArrayList<PairScore> possibleResults)
	     {
	    	 PairScore bestPair=null;
			 for(PairScore P:possibleResults)
			 {
				if(bestPair == null || P.getAnotherScore() > bestPair.getAnotherScore())
					bestPair=P;
				
//				if(bestPair == null || P.getDistanceScore() > bestPair.getDistanceScore())
//					bestPair=P;
			 }
	         Stack<PairScore> outcome = new Stack<PairScore>();
	         PairScore top = bestPair;
	         outcome.add(top);
	         int i=possibleResults.size()-2;
	         while(i>=0)
	         {
                 PairScore curr = possibleResults.get(i);--i;
                 if (curr.getAnotherScore() == top.getAnotherScore() && !outcome.contains(curr))
                	 outcome.add(curr);
//                 if (curr.getDistanceScore() == top.getDistanceScore())
//                	 outcome.add(curr);
	         }
	         return outcome;
	     }
	     
	     public static Stack<PairScoreWithDistance> getChoicesother_distance(ArrayList<PairScoreWithDistance> possibleResults)
	     {
	    	 PairScoreWithDistance bestPair=null;
			 for(PairScoreWithDistance P:possibleResults)
			 {				
				if(bestPair == null || P.getDistanceScore() > bestPair.getDistanceScore())
					bestPair=P;
			 }
	         Stack<PairScoreWithDistance> outcome = new Stack<PairScoreWithDistance>();
	         PairScoreWithDistance top = bestPair;
	         outcome.add(top);
	         int i=possibleResults.size()-2;
	         while(i>=0)
	         {
	        	 PairScoreWithDistance curr = possibleResults.get(i);--i;
                 if (curr.getDistanceScore() == top.getDistanceScore() && !outcome.contains(curr))
                	 outcome.add(curr);
	         }
	         return outcome;
	     }
	     
	     public static Stack<PairScore> getChoicesother_Score(ArrayList<PairScore> possibleResults)
	     {
	    	 PairScore bestPair=null;
			 for(PairScore P:possibleResults)
			 {				
				if(bestPair == null || P.getScore() > bestPair.getScore())
					bestPair=P;
			 }
	         Stack<PairScore> outcome = new Stack<PairScore>();
	         PairScore top = bestPair;
	         outcome.add(top);
	         int i=possibleResults.size()-2;
	         while(i>=0)
	         {
                 PairScore curr = possibleResults.get(i);--i;
                 if (curr.getScore() == top.getScore() && !outcome.contains(curr))
                	 outcome.add(curr);
	         }
	         return outcome;
	     }
	     
	     
	     public static int countChoices_new(Stack<PairScore> stack)
	     {
	         if (stack.isEmpty())
	                 return 0;
	         int outcome = 1;
	         PairScore top = stack.peek();
	         
	        int i=stack.size()-2;
	         while(i>=0)
	         {
	                 PairScore curr = stack.get(i);--i;
	                 if (curr.getScore() != top.getScore())
	                         break;
	                 ++outcome;
	         }
	         return outcome;
	     }
	     
	     public static PairScore ordercountChoices(Stack<PairScoreWithDistance> stack)
	     {
	    	 ArrayList<PairScoreWithDistance> toptop = new ArrayList<PairScoreWithDistance>();
	    	 PairScoreWithDistance top = stack.peek();	       
	         int i=stack.size()-1;
	         while(i>=0)
	         {
	        	 PairScoreWithDistance curr = stack.get(i);--i;
                 if (curr.getScore() != top.getScore())
	        		 break;  
            	 toptop.add(curr);                       
	         }
	         PairScoreWithDistance high_pair = top;
	         for(PairScoreWithDistance t:toptop)
	         {
	        	if(t!=top)
	        	{
	        		System.out.println("Distance Score of high= "+high_pair.getDistanceScore());
	        		System.out.println("Distance Score of t= "+t.getDistanceScore());
					if( t.getDistanceScore() > high_pair.getDistanceScore())
						high_pair=t;
	        	}
	         }
	         return high_pair;
	     }
	     
	     public static PairScoreWithDistance ordercountChoices_improved(Stack<PairScoreWithDistance> stack)
	     {
	    	 Stack<PairScoreWithDistance> toptop = new Stack<PairScoreWithDistance>();
	    	 PairScoreWithDistance top = stack.peek();	       
	         int i=stack.size()-1;
	         while(i>=0)
	         {
	        	 PairScoreWithDistance curr = stack.get(i);--i;
                 if (curr.getScore() != top.getScore())
	        		 break;  
            	 toptop.add(curr);                       
	         }
	         PairScoreWithDistance high_pair = top;
	         for(PairScoreWithDistance t:toptop)
	         {
	        	if(t!=top)
	        	{
	        		 if(t.getDistanceScore() > high_pair.getDistanceScore())
	        			 high_pair=t;
	        	}
	         }
	        			 		
	         return high_pair;
	     }
	     
	     public static Stack<PairScore> possibleAtTop(Stack<PairScore> stack)
	     {
	    	 Stack<PairScore> toptop = new Stack<PairScore>();
	         PairScore top = stack.peek();	       
	         int i=stack.size()-1;
	         while(i>=0)
	         {
	        	 PairScore curr = stack.get(i);--i;
                 if (curr.getScore() == top.getScore())
            	  toptop.addElement(curr);                       
	         }
        	 
	         return toptop;
	     }
	     
//	     public static PairScore ordercountChoices_new(Stack<PairScore> stack)
//	     {
//	    	 ArrayList<PairScore> toptop = new ArrayList<PairScore>();
//	         PairScore top = stack.peek();	       
//	         int i=stack.size()-1;
//	         while(i>=0)
//	         {
//	        	 PairScore curr = stack.get(i);--i;
//                 if (curr.getScore() != top.getScore())
//	        		 break;  
//            	 toptop.add(curr);                       
//	         }
//        	 PairScore high_pair = top;
//	         for(PairScore t:toptop)
//	         {
//	        	if(t!=top)
//	        	{
//	        		System.out.println("Distance Score of high= "+high_pair.getDistanceScore());
//	        		System.out.println("Distance Score of t= "+t.getDistanceScore());
//	        		
//					if(Similarities_incoming(high_pair.getR(), high_pair.getQ()) < Similarities_incoming(t.getR(), t.getQ()) )
//						high_pair=t;
//	        	}
//	         }
//	         return high_pair;
//	     }
	     

	     /** Returns the pair corresponding to the smallest or largest machine for one of the pairs with the same score at the top of the stack. */
	      public static PairScore selectPairMinMax(LearnerGraph graph, Stack<PairScore> stack, int pairChoice)
	      {
	          PairScore top = stack.peek();
	          int value = MergeStates.mergeAndDeterminize(graph, top).getStateNumber();
	          int i=stack.size()-2;
	          while(i>=0)
	          {
	                  PairScore pair = stack.get(i);--i;
	                  if (pair.getScore() != top.getScore()) break;
	                  int stateNumber = MergeStates.mergeAndDeterminize(graph, pair).getStateNumber();
	                  switch(pairChoice)
	                  {
	                  case pairchoiceMIN:
	                          if (stateNumber < value)
	                          {
	                                  top=pair;value=stateNumber;
	                          }
	                          break;
	                  case pairchoiceMAX:
	                          if (stateNumber > value)
	                          {
	                                  top=pair;value=stateNumber;
	                          }
	                          break;
	                  default:
	                          throw new IllegalArgumentException("invalid argument "+pairChoice);
	                  }
	          }
	          return top;
	      }
	  
	     /** Picks a pair at the top of the stack at random. */
	      public static PairScore selectPairAtRandom(Stack<PairScore> stack, Random rnd)
	      {
	          PairScore top = stack.get(stack.size()-1-rnd.nextInt(countChoices(stack)));
	          assert top.getScore() == stack.peek().getScore();
	          return top;
	      }
	      
		public Stack<PairScore> chooseStatePairs_Markov(LearnerGraph coregraph)
		{
			coregraph.pairsAndScores.clear();
			Set<CmpVertex> reds = new LinkedHashSet<CmpVertex>();
			for(CmpVertex v:coregraph.transitionMatrix.keySet())
				if (v.getColour() == JUConstants.RED)
					reds.add(v);

			Queue<CmpVertex> currentExplorationBoundary = new LinkedList<CmpVertex>();// FIFO queue
			currentExplorationBoundary.addAll(reds);
			List<CmpVertex> BlueStatesConsideredSoFar = new LinkedList<CmpVertex>();
			while(!currentExplorationBoundary.isEmpty())
			{
				CmpVertex currentRed = currentExplorationBoundary.remove();
				for(Entry<Label,CmpVertex> BlueEntry:coregraph.transitionMatrix.get(currentRed).entrySet())
					if (BlueEntry.getValue().getColour() == null || 
							BlueEntry.getValue().getColour() == JUConstants.BLUE)
					{// the next vertex is not marked red, hence it has to become blue
						CmpVertex currentBlueState = BlueEntry.getValue();
												
						int numberOfCompatiblePairs = 0;
						for(CmpVertex oldRed:reds)
						{
							PairScore pair = coregraph.pairscores.obtainPair(currentBlueState,oldRed,null);
							if (pair.getScore() >= coregraph.config.getGeneralisationThreshold())
							{
								coregraph.pairsAndScores.add(pair);
								++numberOfCompatiblePairs;
							}
						}
						
						if (numberOfCompatiblePairs == 0)
						{// mark this blue node as red. 
							CmpVertex newRedNode = currentBlueState;
							newRedNode.setColour(JUConstants.RED);
							reds.add(newRedNode);currentExplorationBoundary.add(newRedNode);
							BlueStatesConsideredSoFar.remove(newRedNode);
							
							// All future blue nodes will use this revised set of red states; the fact that
							// it is added to the exploration boundary ensures that it is considered when looking for more blue states.
							// Note that previously-considered blue states were not compared to this one (because it was blue before),
							// however previously-introduced red were - we're using the up-to-date reds set above.
							// For this reason, all we have to do is iterate over the old blue states and compare them to the
							// current one; none of those states may become red as a consequence since they are not 
							// red already, i.e. there is an entry about them in PairsAndScores
							for(CmpVertex oldBlue:BlueStatesConsideredSoFar)
							{
								PairScore pair = coregraph.pairscores.obtainPair(oldBlue,newRedNode,null);
								if (pair.getScore() >= coregraph.config.getGeneralisationThreshold())
								{
									coregraph.pairsAndScores.add(pair);
								}
							}
						}
						else
						{// This node is a blue node and remains blue unlike the case above when it could become red.
							BlueStatesConsideredSoFar.add(BlueEntry.getValue());// add a blue one
							currentBlueState.setColour(JUConstants.BLUE);
						}							
					}
			}

			return coregraph.pairscores.getSortedPairsAndScoresStackFromUnsorted();
		}
		
		
		public double computePairMarkovScore_new(LearnerGraph coregraph, StatePair pair) 
		{
			if (!AbstractLearnerGraph.checkCompatible(pair.getR(),pair.getQ(),coregraph.pairCompatibility))
				return -1;
			double matchscore=0;
			assert pair.getQ() != pair.getR();
			boolean foundKTail = false;
			
			Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
			currentExplorationBoundary.add(pair);currentExplorationBoundary.offer(null);
			
			while(!foundKTail)
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
					Map<Label,CmpVertex> targetRed = coregraph.transitionMatrix.get(currentPair.getR()),
						targetBlue = coregraph.transitionMatrix.get(currentPair.getQ());
		
					for(Entry<Label,CmpVertex> redEntry:targetRed.entrySet())
					{
						CmpVertex nextBlueState = targetBlue.get(redEntry.getKey());
						if (nextBlueState != null)
						{// both states can make a transition
							if (!AbstractLearnerGraph.checkCompatible(redEntry.getValue(),nextBlueState,coregraph.pairCompatibility))
								return -1;// incompatible states
							
//							if(RPNIUniversalLearner.state_outgoing.get(currentPair.getQ())==null || RPNIUniversalLearner.state_outgoing.get(currentPair.getQ()).get(redEntry.getKey())==null)
								 matchscore++;
							
//							else if(RPNIUniversalLearner.state_outgoing.get(currentPair.getQ()).get(redEntry.getKey()) > 0.0)
//					                 matchscore++;
							
							
							StatePair nextStatePair = new StatePair(nextBlueState,redEntry.getValue());
							currentExplorationBoundary.offer(nextStatePair);
							}								
						// if the red can make a move, but the blue one cannot, ignore this case.
					}
				}
			}     
			return matchscore;
		}
 
	
	public MarkovScoreComputation() {
	}

	public static double computeMMScoreImproved(PairScore P, MarkovClassifier cl)
	{
		double score = 0;
		LearnerGraph Extension_Graph = cl.constructMarkovTentative();
		LearnerGraph coregraph = cl.graph;
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
	    			return MarkovClassifier.fREJECT;
			}
			else
				return MarkovClassifier.fREJECT;
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
	    			return MarkovClassifier.fREJECT;
			}
			else
				return MarkovClassifier.fREJECT;
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


	/** The purpose of this method is to match predicted transitions between the supplied states. Imagine two states with a pair of Markov-predicted transitions. These transitions may happen to lead to compatible states
	 * (in other words, both predicted as positive or negative). We can make a subsequent prediction, in which we assume that such predicted transitions are valid and predict those after them. Where these "second-step" transitions 
	 *  match, increment scores.
	 * 
	 * @param graph graph which transitions are going to be predicted and compared
	 * @param origInverse inverse graph that is used to construct all paths leading to states of interest
	 * @param predictForward whether to make predictions either forward or sideways
	 * @param markov prediction engine
	 * @param red first state from which to predict transitions
	 * @param blue second state from which to predict transitions
	 * @param alphabet alphabet to use, passed to {@link MarkovModel#predictTransitionsFromState}. 
	 * @param pathLenBeyondCurrentState path already predicted by the time this method is called. Initially empty and updated for each recursive call of {@link MarkovPassivePairSelection#comparePredictedFanouts(LearnerGraph, LearnerGraphND, MarkovModel, CmpVertex, CmpVertex, Set, List, int)}. 
	 * @param stepNumber how many waves of transitions to generate
	 * @return number of matching transitions 
	 */
	protected static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> long comparePredictedFanouts(MarkovClassifier cl, CmpVertex red, CmpVertex blue, List<Label> pathLenBeyondCurrentState,int stepNumber)
	{
		if (!red.isAccept() || !blue.isAccept())
			return 0;
		
		long scoreCurrentFanout = 0, score = 0;
		Map<Label,MarkovOutcome> outgoing_red_probabilities=cl.predictTransitionsFromState(red,pathLenBeyondCurrentState,cl.model.getChunkLen(),null);
		Map<Label,MarkovOutcome> outgoing_blue_probabilities=cl.predictTransitionsFromState(blue,pathLenBeyondCurrentState,cl.model.getChunkLen(),null);
		for(Entry<Label,MarkovOutcome> entry:outgoing_red_probabilities.entrySet())
		{
			MarkovOutcome outcomeBlue = outgoing_blue_probabilities.get(entry.getKey());
			if (outcomeBlue == null && entry.getValue() == MarkovOutcome.negative) 
				++scoreCurrentFanout; // red negative, blue absent, hence the two are consistent
			if (outcomeBlue == entry.getValue()) // or if the two are consistent
			{
				if (stepNumber > 1)
				{
					LinkedList<Label> pathBeyond = new LinkedList<Label>(pathLenBeyondCurrentState);pathBeyond.add(entry.getKey());
					score+=comparePredictedFanouts(cl,red,blue,pathBeyond,stepNumber-1);
				}
				++scoreCurrentFanout;
			}
		}
			
		for(Entry<Label,MarkovOutcome> entry:outgoing_blue_probabilities.entrySet())
		{
			MarkovOutcome outcomeRed = outgoing_red_probabilities.get(entry.getKey());
			if (outcomeRed == null && entry.getValue() == MarkovOutcome.negative) 
				++scoreCurrentFanout; // blue negative, red absent, hence the two are consistent
			if (outcomeRed == entry.getValue()) // or if the two are consistent
			{
				if (stepNumber > 1)
				{
					LinkedList<Label> pathBeyond = new LinkedList<Label>(pathLenBeyondCurrentState);pathBeyond.add(entry.getKey());
					score+=comparePredictedFanouts(cl,red,blue,pathBeyond,stepNumber-1);
				}
				++scoreCurrentFanout;
			}
		}
		
		if (scoreCurrentFanout*4 < (outgoing_red_probabilities.size()+outgoing_blue_probabilities.size())*3)
			scoreCurrentFanout = 0;
		else
			scoreCurrentFanout+=score;
		return scoreCurrentFanout;
	}

	/** This one does a merger and then looks at the states where something was added to, checking differences between actual transitions and Markov predictions.
	 * 
	 * @param cl the graph and a Markov model where a pair is to be merged
	 * @param pair pair of states to merge
	 * @return the score reflecting the number of inconsistencies between predictions and actual transitions. Even where a merger is correct, the number of inconsistencies could be above zero due to either 
	 * <ul>
	 * <li>insufficient data from which the orignal Markov predictions are built or </li>
	 * <li> due to coarse nature of 
	 * predictions (only short paths are considered for predictions of subsequent transitions).</li>
	 * </ul>
	 */
	public static long computeScoreBasedOnMarkov(MarkovClassifier cl, StatePair pair)
	{
		assert pair.getQ() != pair.getR();
		assert cl.graph.transitionMatrix.containsKey(pair.firstElem);
		assert cl.graph.transitionMatrix.containsKey(pair.secondElem);
		Map<CmpVertex,List<CmpVertex>> mergedVertices = cl.graph.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?
				new ArrayMapWithSearch<CmpVertex,List<CmpVertex>>(cl.graph.getStateNumber()):
				new HashMapWithSearch<CmpVertex,List<CmpVertex>>(cl.graph.getStateNumber());
		Configuration shallowCopy = cl.graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph result = new LearnerGraph(cl.graph,shallowCopy);
		assert result.transitionMatrix.containsKey(pair.firstElem);
		assert result.transitionMatrix.containsKey(pair.secondElem);

		long pairScore = cl.graph.pairscores.computePairCompatibilityScore_internal(pair,mergedVertices);
		if (pairScore < 0)
			throw new IllegalArgumentException("elements of the pair are incompatible");

		if ((pair.getR().getDepth() < cl.model.getChunkLen()-1 || pair.getQ().getDepth() < cl.model.getChunkLen()-1) && pairScore <= 0)
			return Long.MIN_VALUE;// block mergers into the states for which no statistical information is available if there are not common transitions.

		Map<CmpVertex,Collection<Label>> labelsAdded = new TreeMap<CmpVertex,Collection<Label>>();

		Collection<Label> redLabelsAdded = new TreeSet<Label>();labelsAdded.put(pair.getR(), redLabelsAdded);
		redLabelsAdded.addAll(result.transitionMatrix.get(pair.getR()).keySet());

		
		// make a loop
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:cl.graph.transitionMatrix.entrySet())
		{
			for(Entry<Label,CmpVertex> rowEntry:entry.getValue().entrySet())
				if (rowEntry.getValue() == pair.getQ())
				{
					// the transition from entry.getKey() leads to the original blue state, record it to be rerouted.
					result.transitionMatrix.get(entry.getKey()).put(rowEntry.getKey(), pair.getR());

					Collection<Label> newLabelsAdded = labelsAdded.get(entry.getKey());
					if (newLabelsAdded == null)
					{
						newLabelsAdded = new TreeSet<Label>();labelsAdded.put(entry.getKey(), newLabelsAdded);
					}
					newLabelsAdded.add(rowEntry.getKey());

				}
		}
		
		Set<CmpVertex> ptaVerticesUsed = new HashSet<CmpVertex>();
		Set<Label> inputsUsed = new HashSet<Label>();

		// I iterate over the elements of the original graph in order to be able to update the target one.
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:cl.graph.transitionMatrix.entrySet())
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
					for(Entry<Label,CmpVertex> input_and_target:cl.graph.transitionMatrix.get(toMerge).entrySet())
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
		
		// Now we have a graph with all the transitions added (but old ones are not removed, no point doing this). Check if there are any new inconsistencies with 
		// transitions in the vicinity of the added ones. For instance, where a path has been folded in with some transitions sticking out, those new ones
		// may be inconsistent with predictions, based on the transitions in the red part of the graph.

		// mapping map to store all paths leave each state in different length
		double tentativeScore=0;
		@SuppressWarnings("rawtypes")
		ConsistencyChecker checker = new MarkovClassifier.InconsistencyNullVsPredicted();
		MarkovClassifier resultClassifier = new MarkovClassifier(cl.model,result);
		for(Entry<CmpVertex,Collection<Label>> entry:labelsAdded.entrySet())
			if (!entry.getValue().isEmpty())
			{
				double numberOfInconsistencies = resultClassifier.checkFanoutInconsistency(entry.getKey(),checker);
				tentativeScore-=numberOfInconsistencies;
			}

		return (long)tentativeScore;
	}

	public static long computeScoreUsingMarkovFanouts(MarkovClassifier cl, StatePair p)
	{
		long currentScore=0;//comparePredictedFanouts(cl,p.getR(),p.getQ(),new LinkedList<Label>(),2);
		// The one below compares states based on actual outgoing transitions, the one above only uses Markov predictions, current outgoing are taken into account when I count inconsistencies.
		Map<Label,CmpVertex> transitionsFromBlue = cl.graph.transitionMatrix.get(p.getQ());
		for(Entry<Label,CmpVertex> outgoing:cl.graph.transitionMatrix.get(p.getR()).entrySet())
		{
			CmpVertex targetFromBlue = transitionsFromBlue.get(outgoing.getKey());
			if (targetFromBlue != null)
			{// we have matching outgoing transitions
				currentScore+=MarkovScoreComputation.comparePredictedFanouts(cl,outgoing.getValue(),targetFromBlue,new LinkedList<Label>(),2);
			}
		}
		
		return currentScore;
	}
}
