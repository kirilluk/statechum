package statechum.analysis.learning;

import java.awt.Frame;
import java.util.*;

import statechum.JUConstants;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;

public class RPNIBlueFringeLearnerTestComponent extends RPNIBlueFringeLearner {
	private HashSet<ArrayList> scoreDistributions = new HashSet<ArrayList>();

	public RPNIBlueFringeLearnerTestComponent(Frame parentFrame){
		super(parentFrame);
	}
	
	public DirectedSparseGraph learnMachine(DirectedSparseGraph model, Set<List<String>> sPlus, Set<List<String>> sMinus, int threshold){
		model = createAugmentedPTA(model, sPlus, sMinus);
		numberVertices(model);
		Vertex init = findVertex("property", "init",model);
		init.setUserDatum("colour", "red", UserData.SHARED);
		updateGraph(model);
		Stack possibleMerges = chooseStatePairs(model, sPlus, sMinus, threshold);
		while(!possibleMerges.isEmpty()){
			StatePair pair = (StatePair)possibleMerges.pop();
			DirectedSparseGraph temp = mergeAndDeterminize((Graph)model.copy(), pair);
			assert compatible(temp, sPlus, sMinus);
			pair.getQ().setUserDatum("pair", pair, UserData.SHARED);
			pair.getR().setUserDatum("pair", pair, UserData.SHARED);
			List<List<String>> questions = generateQuestions(model, pair);
			questions = trimSet(questions);
			Iterator<List<String>> questionIt = questions.iterator();
			while(questionIt.hasNext()){
				List<String> question = questionIt.next();
				String accepted = pair.getQ().getUserDatum(JUConstants.ACCEPTED).toString();
				updateGraph(model);
				int answer = checkWithEndUser(question, new Object [] {"Test"});
				pair.getQ().removeUserDatum("pair");
				pair.getR().removeUserDatum("pair");
				if(answer == USER_ACCEPTED){
					sPlus.add(question);
					System.out.println(question.toString()+ " <yes>");
				}
				else if(answer >= 0){
					assert answer < question.size();
					sMinus.add(question.subList(0, answer+1));
					System.out.println(question.toString()+ " <no> at position "+answer+", element "+question.get(answer));
					assert accepted.equals("true");
					return learnMachine(initialise(), sPlus, sMinus, threshold);
				}
				else if (answer == USER_ACCEPTED-1){
					// sPlus = this.parentFrame.addTest(sPlus);
					if(sPlus == null)
						return model;
					if(!containsSubString(sPlus, question))
						return learnMachine(initialise(), sPlus, sMinus, threshold);
				}
			}
			model = temp;
			possibleMerges = chooseStatePairs(model, sPlus, sMinus, threshold);
		}
		updateGraph(model);
		System.out.println("finished");
		//printScoreDistributions();
		return model;
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
	
	private boolean containsSubString(Set<List<String>> sPlus, List<String> question){
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
		
	private Stack chooseStatePairs(DirectedSparseGraph g, Set<List<String>> sPlus, Set<List<String>> sMinus, int threshold){
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
				if(score.intValue()<threshold)
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
		scoreDistributions.add(scores);
		return createOrderedStack(scoreToPair);
	}
}
