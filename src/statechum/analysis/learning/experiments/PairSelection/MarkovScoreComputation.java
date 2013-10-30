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
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.Stack;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.JUConstants;
import statechum.Label;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PairSelection.MarkovPassivePairSelection.PairScoreWithDistance;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;

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
							PairScore pair = coregraph.pairscores.obtainPair(currentBlueState,oldRed);
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
								PairScore pair = coregraph.pairscores.obtainPair(oldBlue,newRedNode);
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

}
