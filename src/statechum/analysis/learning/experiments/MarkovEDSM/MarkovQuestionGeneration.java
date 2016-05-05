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
package statechum.analysis.learning.experiments.MarkovEDSM;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import statechum.GlobalConfiguration;
import statechum.Label;
import statechum.Pair;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;

public class MarkovQuestionGeneration {
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
	

}
