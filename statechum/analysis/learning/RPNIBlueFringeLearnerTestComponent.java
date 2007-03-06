package statechum.analysis.learning;

import java.util.*;

import javax.swing.*;
import statechum.analysis.learning.profileStringExtractor.*;
import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.algorithms.shortestpath.*;

public class RPNIBlueFringeLearnerTestComponent extends RPNIBlueFringeLearner implements Learner {
	private SplitFrame parentFrame;
	private HashSet scoreDistributions = new HashSet();
	
	
	public RPNIBlueFringeLearnerTestComponent(SplitFrame parentFrame){
		super();
		this.parentFrame = parentFrame;
	}
	
	public DirectedSparseGraph learnMachine(DirectedSparseGraph model, Set sPlus, Set sMinus, int threshold){
		model = augmentPTA(model, sMinus, false);
		model = augmentPTA(model, sPlus, true);
		numberVertices(model);
		Vertex init = findVertex("property", "init",model);
		init.setUserDatum("colour", "red", UserData.SHARED);
		Stack possibleMerges = chooseStatePairs(model, sPlus, sMinus, threshold);
		while(!possibleMerges.isEmpty()){
			StatePair pair = (StatePair)possibleMerges.pop();
			DirectedSparseGraph temp = mergeAndDeterminize((Graph)model.copy(), pair);
			if(compatible(temp, sPlus, sMinus)){
				pair.getQ().setUserDatum("pair", pair, UserData.SHARED);
				pair.getR().setUserDatum("pair", pair, UserData.SHARED);
				updateGraph(model);
				Set<List<String>> questions = generateQuestions(model, pair);
				questions = trimSet(questions);
				Iterator<List<String>> questionIt = questions.iterator();
				while(questionIt.hasNext()){
					List<String> question = questionIt.next();
					String accepted = pair.getQ().getUserDatum("accepted").toString();
					updateGraph(model);
					int answer = checkWithEndUser(question);
					pair.getQ().removeUserDatum("pair");
					pair.getR().removeUserDatum("pair");
					if(answer == 0){
						sPlus.add(question);
						System.out.println(getShortenedQuestion(question)+ " <yes>");
					}
					else if(answer == 1){
						sMinus.add(question);
						System.out.println(getShortenedQuestion(question)+ " <no>");
						if(accepted.equals("true")){
							return learnMachine(initialise(), sPlus, sMinus, threshold);
						}
					}
					else if (answer == 2){
						sPlus = this.parentFrame.addTest(sPlus);
						if(sPlus == null)
							return model;
						if(!containsSubString(sPlus, question))
							return learnMachine(initialise(), sPlus, sMinus, threshold);
					}
				}
				model = temp;
			}
			possibleMerges = chooseStatePairs(model, sPlus, sMinus, threshold);
		}
		updateGraph(model);
		System.out.println("finished");
		//printScoreDistributions();
		return model;
	}
	
	private void printScoreDistributions(){
		Iterator listIt = scoreDistributions.iterator();
		while(listIt.hasNext()){
			List l = (List)listIt.next();
			for(int i=0;i<l.size();i++){
				Integer score = (Integer)l.get(i);
				System.out.print(score);
				if(i<l.size()-1)
					System.out.print(",");
			}
			System.out.println();
		}
	}
	
	private boolean containsSubString(Set sPlus, List<String> question){
		Iterator stringIt = sPlus.iterator();
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
	
	
	private String getShortenedQuestion(List question){
		String questionString = new String();
		int counter=1;
		for(int i=0;i<question.size();i++){
			if(i==0){
				questionString = questionString.concat((String)question.get(i)+", ");
			}
			else{
				String current = (String)question.get(i);
				if(current.equals(question.get(i-1))){
					counter++;
					continue;
				}
				else{
					if(counter>1){
						questionString = questionString.substring(0, questionString.length()-2);
						questionString = questionString.concat("(*"+counter+"), ");
						counter = 1;
					}
					questionString = questionString.concat(question.get(i)+", ");
				}
			}
		}
		return questionString;
	}
	
	private int checkWithEndUser(List question){
		String questionString = getShortenedQuestion(question);
		JFrame jf = new JFrame();
		int answer;
		Object[] options = {"Yes",
                "No",
                "Test"};
		answer = JOptionPane.showOptionDialog(jf,
                questionString, "Valid input string?",
                JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE,
                null,options, options[0]);
		return answer;
	}

	
	private Stack chooseStatePairs(DirectedSparseGraph g, Set sPlus, Set sMinus, int threshold){
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
				Iterator keyIt = singleSet.keySet().iterator();
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
