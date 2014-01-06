/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

statechum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/
package statechum.analysis.learning.observers;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.Reader;
import java.util.List;

import statechum.Configuration;
import statechum.Label;
import statechum.Pair;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.RPNILearner;
import statechum.analysis.learning.StoredAnswers;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.apps.QSMTool;

/**
 * @author kirill
 *
 */
public class AutoAnswers extends DummyLearner {

	protected String howAnswerWasObtained = "";
	
	/** Stores recorded answers.
	 */
	protected AbstractOracle ans = null;
	
	/** Makes it possible to answer questions automatically.
	 *  
	 * @param a the class holding stored answers.
	 */
	public void setAnswers(AbstractOracle a)
	{
		ans = a;
	}
	
	protected void setAutoOracle(Configuration config)
	{
		if (config.getAutoAnswerFileName().length() > 0)
			try {
				loadAnswers(new FileReader(config.getAutoAnswerFileName()),config);
			} catch (FileNotFoundException e) {
				// does not matter - ans remains null
			}
	}

	public AutoAnswers(Learner learner) {
		super(learner);
	}

	void loadAnswers(Reader from, Configuration config)
	{
		ans = new StoredAnswers(config,getLabelConverter());
		try {
			((StoredAnswers)ans).setAnswers(from);
		} catch (Exception e) {
			ans = null;
		}		
	}
	
	@Override
	public Pair<Integer, String> CheckWithEndUser(LearnerGraph graph, List<Label> question, int responseForNoRestart, List<Boolean> acceptedElements, PairScore pairBeingMerged, Object[] options) 
	{
		if (ans == null) setAutoOracle(graph.config);
		Pair<Integer,String> answer = null;
		
		if (ans != null)
		{// first, attempt auto	
			answer = ans.getAnswer(question);
			howAnswerWasObtained = RPNILearner.QUESTION_AUTO;
			
			if (answer != null && answer.firstElem == AbstractOracle.USER_INCOMPATIBLE)
			{// verify that the answer obtained is consistent with the request
				String currentPairText = AutoAnswers.pairToString(pairBeingMerged);
				if (answer.secondElem != null && !answer.secondElem.equals(currentPairText))
					throw new IllegalArgumentException("Autoanswers recorded "+answer.secondElem+" as incompatible but the current pair is "+currentPairText);
			}
		}
		
		if (answer == null)
		{// auto did not provide an answer, pass the question further
			answer = decoratedLearner.CheckWithEndUser(graph, question, responseForNoRestart, acceptedElements, pairBeingMerged, options);
			howAnswerWasObtained = RPNILearner.QUESTION_USER;// we expect to be last in the chain, but do not really care.
		}
		
		if (answer != null)
		{
			
			if (answer.firstElem >= 0)
				// rejected
				System.out.println(howAnswerWasObtained+" "+RPNILearner.questionToString(question)+ " <no> at position "+answer.firstElem+", element "+question.get(answer.firstElem));
			else
			if (answer.firstElem == AbstractOracle.USER_ACCEPTED)
				System.out.println(howAnswerWasObtained+" "+RPNILearner.questionToString(question)+ " <yes>");
			else
			if (answer.firstElem == AbstractOracle.USER_LTL && answer.secondElem != null)
				System.out.println(howAnswerWasObtained+" "+RPNILearner.questionToString(question)+ " <"+QSMTool.cmdLTL+"> "+answer.secondElem);
			else
			if (answer.firstElem == AbstractOracle.USER_IFTHEN && answer.secondElem != null)
				System.out.println(howAnswerWasObtained+" "+RPNILearner.questionToString(question)+ " <"+QSMTool.cmdIFTHENAUTOMATON+"> "+answer.secondElem);
			else
			if (answer.firstElem == AbstractOracle.USER_INCOMPATIBLE)
				System.out.println(howAnswerWasObtained+" "+RPNILearner.questionToString(question)+" "+RPNILearner.QUESTION_INCOMPATIBLE+" "+pairToString(pairBeingMerged));
			else
			if (answer.firstElem == AbstractOracle.USER_IGNORED)
				System.out.println(howAnswerWasObtained+" "+RPNILearner.questionToString(question)+" "+RPNILearner.QUESTION_IGNORE);
			else
			if (answer.firstElem == AbstractOracle.USER_NEWTRACE && answer.secondElem != null)
				System.out.println(howAnswerWasObtained+" "+RPNILearner.questionToString(question)+" "+RPNILearner.QUESTION_NEWTRACE+" "+answer.secondElem);

		}
		return answer;
	}
	
	/** Given a state pair, returns a string representing that pair - used to store/load a pair of incompatible elements. 
	 * @param pair the pair to convert 
	 */
	public static String pairToString(PairScore pair)
	{
		return pair.firstElem.getStringId()+" "+pair.secondElem.getStringId();
	}
}
