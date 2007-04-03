package statechum.analysis.learning;

import java.awt.Frame;
import java.util.*;

import statechum.JUConstants;

import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;

public class RPNIBlueFringeLearnerTestComponent extends RPNIBlueFringeLearner {
	private HashSet<ArrayList> scoreDistributions = new HashSet<ArrayList>();
	private int certaintyThreshold = 100;

	public RPNIBlueFringeLearnerTestComponent(Frame parentFrame){
		super(parentFrame);
	}
	
	
	
	public DirectedSparseGraph learnMachine(DirectedSparseGraph model, Set<List<String>> sPlus, Set<List<String>> sMinus)throws InterruptedException{
		this.sPlus = sPlus;
		this.sMinus = sMinus;
		model = createAugmentedPTA(model, sPlus, sMinus);
		numberVertices(model);
		Vertex init = findVertex("property", "init",model);
		init.setUserDatum("colour", "red", UserData.SHARED);
		setChanged();
		Stack possibleMerges = chooseStatePairs(model, sPlus, sMinus);
		while(!possibleMerges.isEmpty()){
			StatePair pair = (StatePair)possibleMerges.pop();
			DirectedSparseGraph temp = mergeAndDeterminize((Graph)model.copy(), pair);
			assert compatible(temp, sPlus, sMinus);
			pair.getQ().setUserDatum("pair", pair, UserData.SHARED);
			pair.getR().setUserDatum("pair", pair, UserData.SHARED);// since this copy of the graph will really not be used, changes to it are immaterial at this stage 
			setChanged();
			List<List<String>> questions = new ArrayList<List<String>>();
			if(computeScore(model, pair)<=this.certaintyThreshold){
				questions = generateQuestions(model, temp, pair);
				questions = trimSet(questions);
			}
			Iterator<List<String>> questionIt = questions.iterator();
			while(questionIt.hasNext()){
				List<String> question = questionIt.next();
				String accepted = pair.getQ().getUserDatum(JUConstants.ACCEPTED).toString();
				int answer = checkWithEndUser(model,question, new Object [] {"Test"});
				this.questionCounter++;
				if (answer == USER_CANCELLED)
				{
					System.out.println("CANCELLED");
					return null;
				}
				if(answer == USER_ACCEPTED){
					sPlus.add(question);
					//System.out.println(setByAuto+question.toString()+ " <yes>");
					Vertex tempVertex = getVertex(temp, question);
					if(tempVertex.getUserDatum(JUConstants.ACCEPTED).toString().equals("false"))
							return learnMachine(initialise(), sPlus, sMinus);
				}
				else if(answer >= 0){
					assert answer < question.size();
					sMinus.add(question.subList(0, answer+1));
					//System.out.println(setByAuto+question.toString()+ " <no> at position "+answer+", element "+question.get(answer));
					if(getVertex(temp, question).getUserDatum(JUConstants.ACCEPTED).toString().equals("false")){
						continue;
					}
					assert accepted.equals("true");
					return learnMachine(initialise(), sPlus, sMinus);
				}
				else if (answer == USER_ACCEPTED-1){
					// sPlus = this.parentFrame.addTest(sPlus);
					if(sPlus == null)
						return model;
					if(!containsSubString(sPlus, question))
						return learnMachine(initialise(), sPlus, sMinus);
				}
				
			}
			model = temp;
			possibleMerges = chooseStatePairs(model, sPlus, sMinus);
		}
		updateGraph(model);
		return model;
	}
	
	
	
	private Vertex getTempRed(DirectedSparseGraph model, Vertex r, DirectedSparseGraph temp){
		DijkstraShortestPath p = new DijkstraShortestPath(model);
		List<Edge> pathToRed = p.getPath(findVertex("property", "init",model), r);
		Set<List<String>> pathToRedStrings = new HashSet<List<String>>();
		Vertex tempRed = null;
		if(!pathToRed.isEmpty()){
			pathToRedStrings = getPaths(pathToRed);
			List<String> prefixString = (List<String>)pathToRedStrings.toArray()[0];
			tempRed = getVertex(temp, prefixString);
		}
		else
			tempRed = findVertex("property", "init", temp);
		return tempRed;
	}
	
	protected List<List<String>> generateQuestions(DirectedSparseGraph model, DirectedSparseGraph temp, StatePair pair){
		Vertex q = pair.getQ();
		Vertex r = pair.getR();
		if(q==null || r ==null)
			return new ArrayList<List<String>>();
		DijkstraShortestPath p = new DijkstraShortestPath(temp);
		Vertex tempRed = getTempRed(model, r, temp);
		Vertex tempInit = findVertex("property", "init", temp);
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
		Vertex init = findVertex("property", "init",questionPrefixes);
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
				else if(e.getSource().equals(v)&&(e.getDest().equals(v))){
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
	
	private void printScoreDistributions(){
		Iterator<ArrayList> listIt = scoreDistributions.iterator();
		while(listIt.hasNext()){
			List l = listIt.next();
			for(int i=0;i<l.size();i++){
				Integer score = (Integer)l.get(i);
				System.out.print(score);
				if(i<l.size()-1)
					System.out.print(",");
			}
			System.out.println();
		}
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
		
	private Stack chooseStatePairs(DirectedSparseGraph g, Set<List<String>> sPlus, Set<List<String>> sMinus){
		ArrayList scores = new ArrayList();
		Stack<Vertex> blueStack = new Stack<Vertex>();
		blueStack.addAll(computeBlue(g));
		TreeMap<Integer,Vector<StatePair> > scoreToPair = new TreeMap<Integer,Vector<StatePair> >();// maps scores to pairs which have those scores
		while(!blueStack.isEmpty()){
			TreeMap<Integer,Vector<StatePair> > singleSet = new TreeMap<Integer,Vector<StatePair> >();
			Vertex blueVertex = blueStack.pop();
			Stack<Vertex> redStack = new Stack<Vertex>();
			redStack.addAll(findVertices("colour", "red", g));
			while(!redStack.isEmpty()){
				Vertex redVertex = redStack.pop();
				StatePair pair = new StatePair(blueVertex, redVertex);
				doneEdges = new HashSet();
				Integer score = new Integer(computeScore(g,pair));
				if(score.intValue()<this.generalisationThreshold)
					continue;
				DirectedSparseGraph temp = mergeAndDeterminize((Graph)g.copy(), pair);
				if(compatible(temp, sPlus, sMinus)){
					// singleSet maps scores to pairs which have those scores
					if(singleSet.get(score) == null){
						// nothing yet with this score
						Vector<StatePair> s = new Vector<StatePair>();
						s.add(pair);
						singleSet.put(score, s);
					}
					else{
						// already some pairs with this score
						Vector<StatePair> s = singleSet.get(score);
						s.add(pair);
						singleSet.put(score, s);
					}	
				}
			}
			if(singleSet.isEmpty()){// no new pairs found, marking the current blue vertex as red.
				blueVertex.setUserDatum("colour", "red", UserData.SHARED);
				blueStack.clear();
				scoreToPair.clear();
				blueStack.addAll(computeBlue(g));
			}
			else{
				Iterator<Integer> keyIt = singleSet.keySet().iterator();
				while(keyIt.hasNext()){
					Object key = keyIt.next();
					Vector<StatePair> s = scoreToPair.get(key);
					if(s!=null){
						s.addAll(singleSet.get(key));
						Collections.sort(s);
					}
					else
					{
						Vector<StatePair> value = singleSet.get(key);
						Collections.sort(value);
						scoreToPair.put((Integer)key, value);
					}
				}
			}
		}
		scores.addAll(scoreToPair.keySet());
		scoreDistributions.add(scores);
		return createOrderedStack(scoreToPair);
	}



	public void setCertaintyThreshold(int certaintyThreshold) {
		this.certaintyThreshold = certaintyThreshold;
	}
}
