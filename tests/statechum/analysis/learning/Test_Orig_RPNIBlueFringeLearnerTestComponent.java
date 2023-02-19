/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
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

import java.awt.Frame;
import java.util.*;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;
import static statechum.DeterministicDirectedSparseGraph.findEdge;

public class Test_Orig_RPNIBlueFringeLearnerTestComponent extends Test_Orig_RPNIBlueFringeLearner {
	

	public Test_Orig_RPNIBlueFringeLearnerTestComponent(Frame parent, Configuration c, ConvertALabel conv)
	{
		super(parent,c,conv);
	}
	
	@Override
	public LearnerGraph learnMachine(Collection<List<Label>> argSPlus, Collection<List<Label>> argSMinus) 	{
		this.sPlus = argSPlus;
		this.sMinus = argSMinus;
		DirectedSparseGraph model = createAugmentedPTA(sPlus, sMinus);// KIRR: node labelling is done by createAugmentedPTA 
		DeterministicDirectedSparseGraph.findInitial(model).setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
		setChanged();

		Stack<OrigStatePair> possibleMerges = chooseStatePairs(model, sPlus, sMinus);
		while(!possibleMerges.isEmpty()){
			OrigStatePair pair = possibleMerges.pop();
			DirectedSparseGraph temp = mergeAndDeterminize(model, pair);
			pair.getQ().setUserDatum(JUConstants.HIGHLIGHT, pair, UserData.SHARED);
			pair.getR().setUserDatum(JUConstants.HIGHLIGHT, pair, UserData.SHARED);// since this copy of the graph will really not be used, changes to it are immaterial at this stage 
			setChanged();
			List<List<Label>> questions = new LinkedList<List<Label>>();
			doneEdges = new HashSet<DirectedSparseEdge>();
			int score = computeScore(model, pair);
			if(shouldAskQuestions(score)){
				questions = generateQuestions(model, temp, pair);
				// questions = trimSet(questions); // KIRR: unnecessary by construction of questions
			}
			
			boolean restartLearning = false;// whether we need to rebuild a PTA and restart learning.
			
			Iterator<List<Label>> questionIt = questions.iterator();
			while(questionIt.hasNext()){
				List<Label> question = questionIt.next();
				boolean accepted = DeterministicDirectedSparseGraph.isAccept(pair.getQ());
				Pair<Integer,String> answer = CheckWithEndUser(new LearnerGraph(model,Configuration.getDefaultConfiguration()),question, AbstractOracle.USER_CANCELLED,null, null,new Object [] {"Test"});
				if (answer.firstElem == AbstractOracle.USER_CANCELLED)
				{
					System.out.println("CANCELLED");
					return null;
				}
				Vertex tempVertex = getVertex(temp, question);
				if(answer.firstElem == AbstractOracle.USER_ACCEPTED){
					sPlus.add(question);
					
					if(!DeterministicDirectedSparseGraph.isAccept(tempVertex))
					{
							restartLearning = true;break;
					}
				}
				else if(answer.firstElem >= 0){
					assert answer.firstElem < question.size();
					LinkedList<Label> subAnswer = new LinkedList<Label>();subAnswer.addAll(question.subList(0, answer.firstElem+1));sMinus.add(subAnswer);
					// sMinus.add(question.subList(0, answer+1)); // KIRR: without a `proper' collection in the set, I cannot serialise the sets into XML

					if((answer.firstElem==question.size()-1)&&!DeterministicDirectedSparseGraph.isAccept(tempVertex)){
						continue;
					}
					assert accepted == true;
					restartLearning = true;
					break;
				}
				else if (answer.firstElem == AbstractOracle.USER_ACCEPTED-1){
					// sPlus = this.parentFrame.addTest(sPlus);
					if(sPlus == null)
						return new LearnerGraph(model,Configuration.getDefaultConfiguration());
					if(!containsSubString(sPlus, question))
						return learnMachine(sPlus, sMinus);
				}
				
			}
			
			
			if (restartLearning)
			{// restart learning
				model = createAugmentedPTA(sPlus, sMinus);// KIRR: node labelling is done by createAugmentedPTA 
				DeterministicDirectedSparseGraph.findInitial(model).setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
				setChanged();				
			}
			else
				// keep going with the existing model
				model = temp;
			
			possibleMerges = chooseStatePairs(model, sPlus, sMinus);
		}
		updateGraph(new LearnerGraph(model,Configuration.getDefaultConfiguration()),null);
		return new LearnerGraph(model,Configuration.getDefaultConfiguration());
	}
	
	
	
	@SuppressWarnings("unchecked")
	public static DeterministicVertex getTempRed(DirectedSparseGraph model, DeterministicVertex r, DirectedSparseGraph temp){
		DijkstraShortestPath p = new DijkstraShortestPath(model);
		List<Edge> pathToRed = p.getPath(DeterministicDirectedSparseGraph.findInitial(model), r);
		Set<List<Label>> pathToRedStrings = new LinkedHashSet<List<Label>>();
		DeterministicVertex tempRed = null;
		if(!pathToRed.isEmpty()){
			pathToRedStrings = getPaths(pathToRed);
			List<Label> prefixString = (List<Label>)pathToRedStrings.toArray()[0];
			tempRed = getVertex(temp, prefixString);
		}
		else
			tempRed = DeterministicDirectedSparseGraph.findInitial(temp);
		return tempRed;
	}
	
	@SuppressWarnings("unchecked")
	protected List<List<Label>> generateQuestions(DirectedSparseGraph model, DirectedSparseGraph temp, OrigStatePair pair){
		DeterministicVertex q = pair.getQ();
		DeterministicVertex r = pair.getR();
		if(q==null || r ==null)
			return new LinkedList<List<Label>>();
		DijkstraShortestPath p = new DijkstraShortestPath(temp);
		DeterministicVertex tempRed = getTempRed(model, r, temp);
		DeterministicVertex tempInit = DeterministicDirectedSparseGraph.findInitial(temp);
		Set<List<Label>> prefixes = new LinkedHashSet<List<Label>>();
		if(!tempRed.equals(tempInit)){
			List<Edge> prefixEdges = p.getPath(tempInit, tempRed);
			prefixes = getPaths(prefixEdges);
		}
		Set<List<Label>> suffixes = computeSuffixes(tempRed, temp);
		List<List<Label>> questions =new LinkedList<List<Label>>();
		questions.addAll(mergePrefixWithSuffixes(prefixes, suffixes));
		Edge loopEdge = findEdge(tempRed, tempRed);
		if(loopEdge!=null){
			Collection<Label> looplabels = (Collection<Label>)loopEdge.getUserDatum(JUConstants.LABEL);
			questions.addAll(mergePrefixWithSuffixes(prefixes, looplabels,suffixes));
		}
		
		DirectedSparseGraph questionPrefixes = augmentPTA(DeterministicDirectedSparseGraph.initialise(), questions, true);
		Iterator<DeterministicVertex> questionIt = getEndPoints(questionPrefixes).iterator();
		p = new DijkstraShortestPath(questionPrefixes);
		questions =new LinkedList<List<Label>>();
		DeterministicVertex init = DeterministicDirectedSparseGraph.findInitial(questionPrefixes);
		while(questionIt.hasNext()){
			List<Edge> edgePath = p.getPath(init, questionIt.next());
			Set<List<Label>> pathsToPoint = getPaths(edgePath);
			if(pathsToPoint.isEmpty())
				continue;
			List<Label> pathToPoint = (List<Label>)getPaths(edgePath).toArray()[0];
			DeterministicVertex tempV = getVertex(temp, pathToPoint);
			DeterministicVertex v = getVertex(model, pathToPoint);
			if(v == null)
				questions.add(pathToPoint);
			else if(Test_Orig_RPNIBlueFringeLearner.different(new OrigStatePair(v, tempV)))
				questions.add(pathToPoint);
			
		}
		return questions;
	}
	
	@SuppressWarnings("unchecked")
	public static List<List<Label>> mergePrefixWithSuffixes(Set<List<Label>> sp, Collection<List<Label>> suffixes){
		ArrayList<List<Label>> questions = new ArrayList<List<Label>>();
		Object[] prefixArray = null;
		int iterations = sp.size();
		if(sp.isEmpty()){
			iterations++;
		}
		else
			prefixArray = sp.toArray();
		for(int i=0;i<iterations;i++){
			List<Label> prefix = null;
			if(!sp.isEmpty())
				prefix = (List<Label>)prefixArray[i];
			Iterator<List<Label>> suffixIt = suffixes.iterator();
			while(suffixIt.hasNext()){
				List<Label> suffix = suffixIt.next();
				List<Label> newQuestion = new LinkedList<Label>();
				if(prefix != null)
					newQuestion.addAll(prefix);
				newQuestion.addAll(suffix);
				questions.add(newQuestion);
			}
		}
		return questions;
	}
	
	@SuppressWarnings("unchecked")
	public static List<List<Label>> mergePrefixWithSuffixes(Collection<List<Label>> sp, Collection<Label> loopLabels, Collection<List<Label>> suffixes){
		List<List<Label>> questions = new LinkedList<List<Label>>();
		Object[] prefixArray = null;
		int iterations = sp.size();
		if(sp.isEmpty()){
			iterations++;
		}
		else
			prefixArray = sp.toArray();
		for(int i=0;i<iterations;i++){
			List<Label> prefix = null;
			if(!sp.isEmpty())
				prefix = (List<Label>)prefixArray[i];
			Iterator<List<Label>> suffixIt = suffixes.iterator();
			while(suffixIt.hasNext()){
				List<Label> suffix = suffixIt.next();
				Iterator<Label> loopLabelIt = loopLabels.iterator();
				while(loopLabelIt.hasNext()){
					Label loopLabel = loopLabelIt.next();
					List<Label> newQuestion = new LinkedList<Label>();
					if(prefix != null)
						newQuestion.addAll(prefix);
					newQuestion.add(loopLabel);
					newQuestion.addAll(suffix);
					questions.add(newQuestion);
				}
			}
		}
		return questions;
	}
	
	public static Set<List<Label>> computeSuffixes(DeterministicVertex v, DirectedSparseGraph model){
		Set<List<Label>> returnSet = new HashSet<List<Label>>();
		DijkstraShortestPath p = new DijkstraShortestPath(model);
		Iterator<DirectedSparseEdge> edgeIt = model.getEdges().iterator();
		while(edgeIt.hasNext()){
			DirectedSparseEdge e = edgeIt.next();
			List<Edge> sp = null;
			sp = p.getPath(v, e.getSource());
			if(sp!=null){
				if(!sp.isEmpty()){
					sp.add(e);
					Set<List<Label>> paths = getPaths(sp);
					returnSet.addAll(paths);
				}
				else if(e.getSource().equals(v)) { //&&(e.getDest().equals(v))){ // KIRR: BUG FIXED
					sp.add(e);
					Set<List<Label>> paths = getPaths(sp);
					returnSet.addAll(paths);
				}
			}
			
		}
		return returnSet;
	}
	
	private static Set<DeterministicVertex> getEndPoints(DirectedSparseGraph g){
		Set<DeterministicVertex> returnSet = new HashSet<DeterministicVertex>();
		Iterator<DeterministicVertex> vertexIt = g.getVertices().iterator();
		while(vertexIt.hasNext()){
			DeterministicVertex v = vertexIt.next();
			if(v.getSuccessors().isEmpty())
				returnSet.add(v);
		}
		return returnSet;
	}
	
	private static boolean containsSubString(Collection<List<Label>> sPlusArg, List<Label> question){
		Iterator<List<Label>> stringIt = sPlusArg.iterator();
		Label first = question.get(0);
		int length = question.size();
		while(stringIt.hasNext()){
			List<Label> list = stringIt.next();
			for(int i=0;i<list.size();i++){
				if(list.get(i).equals(first)){
					if(list.subList(i, i+length).equals(question))
						return true;
				}
			}
		}
		return false;
	}
		
	
}
