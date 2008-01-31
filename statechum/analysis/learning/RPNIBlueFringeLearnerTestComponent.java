package statechum.analysis.learning;

import java.awt.Frame;
import java.util.*;

import statechum.JUConstants;
import static statechum.analysis.learning.RPNIBlueFringeLearner.isAccept;

import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;

public class RPNIBlueFringeLearnerTestComponent extends RPNIBlueFringeLearner {
	

	public RPNIBlueFringeLearnerTestComponent(Frame parentFrame){
		super(parentFrame);
	}
	
	
	
	public DirectedSparseGraph learnMachine(DirectedSparseGraph model, Collection<List<String>> sPlus, Collection<List<String>> sMinus) 	{
		this.sPlus = sPlus;
		this.sMinus = sMinus;
		model = createAugmentedPTA(model, sPlus, sMinus);// KIRR: node labelling is done by createAugmentedPTA 
		findInitial(model).setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
		setChanged();

		Stack possibleMerges = chooseStatePairs(model, sPlus, sMinus);
		while(!possibleMerges.isEmpty()){
			StatePair pair = (StatePair)possibleMerges.pop();
			DirectedSparseGraph temp = mergeAndDeterminize(model, pair);
			pair.getQ().setUserDatum(JUConstants.HIGHLIGHT, pair, UserData.SHARED);
			pair.getR().setUserDatum(JUConstants.HIGHLIGHT, pair, UserData.SHARED);// since this copy of the graph will really not be used, changes to it are immaterial at this stage 
			setChanged();
			List<List<String>> questions = new ArrayList<List<String>>();
			doneEdges = new HashSet();
			int score = computeScore(model, pair);
			if(score<this.certaintyThreshold&&score>minCertaintyThreshold){
				questions = generateQuestions(model, temp, pair);
				// questions = trimSet(questions); // KIRR: unnecessary by construction of questions
			}
			
			boolean restartLearning = false;// whether we need to rebuild a PTA and restart learning.
			
			Iterator<List<String>> questionIt = questions.iterator();
			while(questionIt.hasNext()){
				List<String> question = questionIt.next();
				boolean accepted = isAccept(pair.getQ());
				int answer = checkWithEndUser(model,question, new Object [] {"Test"});
				this.questionCounter++;
				if (answer == USER_CANCELLED)
				{
					System.out.println("CANCELLED");
					return null;
				}
				Vertex tempVertex = getVertex(temp, question);
				if(answer == USER_ACCEPTED){
					sPlus.add(question);
					//System.out.println(setByAuto+question.toString()+ " <yes>");
					
					if(!isAccept(tempVertex))
					{
							restartLearning = true;break;
					}
				}
				else if(answer >= 0){
					assert answer < question.size();
					LinkedList<String> subAnswer = new LinkedList<String>();subAnswer.addAll(question.subList(0, answer+1));sMinus.add(subAnswer);
					// sMinus.add(question.subList(0, answer+1)); // KIRR: without a `proper' collection in the set, I cannot serialise the sets into XML

					//System.out.println(setByAuto+question.toString()+ " <no> at position "+answer+", element "+question.get(answer));
					if((answer==question.size()-1)&&!isAccept(tempVertex)){
						continue;
					}
					else{
						assert accepted == true;
						restartLearning = true;break;
					}
				}
				else if (answer == USER_ACCEPTED-1){
					// sPlus = this.parentFrame.addTest(sPlus);
					if(sPlus == null)
						return model;
					if(!containsSubString(sPlus, question))
						return learnMachine(initialise(), sPlus, sMinus);
				}
				
			}
			
			
			if (restartLearning)
			{// restart learning
				model = createAugmentedPTA(initialise(), sPlus, sMinus);// KIRR: node labelling is done by createAugmentedPTA 
				findInitial(model).setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
				setChanged();				
			}
			else
				// keep going with the existing model
				model = temp;
			
			possibleMerges = chooseStatePairs(model, sPlus, sMinus);
		}
		updateGraph(model);
		return model;
	}
	
	
	
	public static Vertex getTempRed(DirectedSparseGraph model, Vertex r, DirectedSparseGraph temp){
		DijkstraShortestPath p = new DijkstraShortestPath(model);
		List<Edge> pathToRed = p.getPath(findInitial(model), r);
		Set<List<String>> pathToRedStrings = new HashSet<List<String>>();
		Vertex tempRed = null;
		if(!pathToRed.isEmpty()){
			pathToRedStrings = getPaths(pathToRed);
			List<String> prefixString = (List<String>)pathToRedStrings.toArray()[0];
			tempRed = getVertex(temp, prefixString);
		}
		else
			tempRed = findInitial(temp);
		return tempRed;
	}
	
	protected List<List<String>> generateQuestions(DirectedSparseGraph model, DirectedSparseGraph temp, StatePair pair){
		Vertex q = pair.getQ();
		Vertex r = pair.getR();
		if(q==null || r ==null)
			return new ArrayList<List<String>>();
		DijkstraShortestPath p = new DijkstraShortestPath(temp);
		Vertex tempRed = getTempRed(model, r, temp);
		Vertex tempInit = findInitial(temp);
		Set<List<String>> prefixes = new HashSet<List<String>>();
		if(!tempRed.equals(tempInit)){
			List<Edge> prefixEdges = p.getPath(tempInit, tempRed);
			prefixes = getPaths(prefixEdges);
		}
		Set<List<String>> suffixes = computeSuffixes(tempRed, temp);
		List<List<String>> questions =new ArrayList<List<String>>();
		questions.addAll(mergePrefixWithSuffixes(prefixes, suffixes));
		Edge loopEdge = findEdge(tempRed, tempRed);
		if(loopEdge!=null){
			Collection<String> looplabels = (Collection<String>)loopEdge.getUserDatum(JUConstants.LABEL);
			questions.addAll(mergePrefixWithSuffixes(prefixes, looplabels,suffixes));
		}
		
		DirectedSparseGraph questionPrefixes = augmentPTA(initialise(), questions, true);
		Iterator<Vertex> questionIt = getEndPoints(questionPrefixes).iterator();
		p = new DijkstraShortestPath(questionPrefixes);
		questions =new ArrayList<List<String>>();
		Vertex init = findInitial(questionPrefixes);
		while(questionIt.hasNext()){
			List<Edge> edgePath = p.getPath(init, questionIt.next());
			Set<List<String>> pathsToPoint = getPaths(edgePath);
			if(pathsToPoint.isEmpty())
				continue;
			List<String> pathToPoint = (List<String>)getPaths(edgePath).toArray()[0];
			Vertex tempV = getVertex(temp, pathToPoint);
			Vertex v = getVertex(model, pathToPoint);
			if(v == null)
				questions.add(pathToPoint);
			else if(different(new StatePair(v, tempV)))
				questions.add(pathToPoint);
			
		}
		return questions;
	}
	
	public static List<List<String>> mergePrefixWithSuffixes(Set<List<String>> sp, Collection<List<String>> suffixes){
		ArrayList<List<String>> questions = new ArrayList<List<String>>();
		Object[] prefixArray = null;
		int iterations = sp.size();
		if(sp.isEmpty()){
			iterations++;
		}
		else
			prefixArray = sp.toArray();
		for(int i=0;i<iterations;i++){
			List<String> prefix = null;
			if(!sp.isEmpty())
				prefix = (List<String>)prefixArray[i];
			Iterator<List<String>> suffixIt = suffixes.iterator();
			while(suffixIt.hasNext()){
				List<String> suffix = suffixIt.next();
				List<String> newQuestion = new ArrayList<String>();
				if(prefix != null)
					newQuestion.addAll(prefix);
				newQuestion.addAll(suffix);
				questions.add(newQuestion);
			}
		}
		return questions;
	}
	
	public static List<List<String>> mergePrefixWithSuffixes(Collection<List<String>> sp, Collection<String> loopLabels, Collection<List<String>> suffixes){
		ArrayList<List<String>> questions = new ArrayList<List<String>>();
		Object[] prefixArray = null;
		int iterations = sp.size();
		if(sp.isEmpty()){
			iterations++;
		}
		else
			prefixArray = sp.toArray();
		for(int i=0;i<iterations;i++){
			List<String> prefix = null;
			if(!sp.isEmpty())
				prefix = (List<String>)prefixArray[i];
			Iterator<List<String>> suffixIt = suffixes.iterator();
			while(suffixIt.hasNext()){
				List<String> suffix = suffixIt.next();
				Iterator<String> loopLabelIt = loopLabels.iterator();
				while(loopLabelIt.hasNext()){
					String loopLabel = loopLabelIt.next();
					List<String> newQuestion = new ArrayList<String>();
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
	
	public static Set<List<String>> computeSuffixes(Vertex v, DirectedSparseGraph model){
		Set<List<String>> returnSet = new HashSet<List<String>>();
		DijkstraShortestPath p = new DijkstraShortestPath(model);
		Iterator<DirectedSparseEdge> edgeIt = model.getEdges().iterator();
		while(edgeIt.hasNext()){
			DirectedSparseEdge e = edgeIt.next();
			List<Edge> sp = null;
			sp = p.getPath(v, e.getSource());
			if(sp!=null){
				if(!sp.isEmpty()){
					sp.add(e);
					Set<List<String>> paths = getPaths(sp);
					returnSet.addAll(paths);
				}
				else if(e.getSource().equals(v)) { //&&(e.getDest().equals(v))){ // KIRR: BUG FIXED
					sp.add(e);
					Set<List<String>> paths = getPaths(sp);
					returnSet.addAll(paths);
				}
			}
			
		}
		return returnSet;
	}
	
	private static Set<Vertex> getEndPoints(DirectedSparseGraph g){
		Set<Vertex> returnSet = new HashSet<Vertex>();
		Iterator<Vertex> vertexIt = g.getVertices().iterator();
		while(vertexIt.hasNext()){
			Vertex v = vertexIt.next();
			if(v.getSuccessors().isEmpty())
				returnSet.add(v);
		}
		return returnSet;
	}
	
	private boolean containsSubString(Collection<List<String>> sPlus, List<String> question){
		Iterator<List<String>> stringIt = sPlus.iterator();
		String first = question.get(0);
		int length = question.size();
		while(stringIt.hasNext()){
			ArrayList list = (ArrayList)stringIt.next();
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
