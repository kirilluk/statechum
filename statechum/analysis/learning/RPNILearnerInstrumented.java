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
package statechum.analysis.learning;

import java.awt.Frame;
import java.io.FileReader;
import java.util.List;

import statechum.Configuration;
import statechum.Pair;
import statechum.analysis.learning.rpnicore.LearnerGraph;

/**
 * @author kirill
 *
 */
public abstract class RPNILearnerInstrumented extends RPNILearner {
	protected int questionCounter = 0;
	

	public RPNILearnerInstrumented(Frame parent, Configuration c) {
		super(parent, c);
	}

	protected int counterRestarted = 0;
	
	/** Returns the number of times learner had to restart. */
	public int getRestarts()
	{
		return counterRestarted;
	}

	public int getQuestionCounter() {
		return questionCounter;
	}

	public void setQuestionCounter(int questionCnt) {
		this.questionCounter = questionCnt;
	}

	/** Stores recorded answers. Ideally, this would be a collection of oracles, each
	 * of which would be asked in turn to provide answers. In practice, I need to know
	 * if autoanswer is on or not - if it is on, I need to display answers to questions
	 * so that these answers can be pasted into a file. If not, they should not be displayed
	 * because this would slow down batch processing.
	 * <p>
	 * The proper way to perform this stack is via observers overriding the
	 * <em>CheckWithEndUser</em> method. 
	 */
	protected AbstractOracle ans = null;
	
	protected String howAnswerWasObtained = "";

	/** Makes it possible to answer questions automatically.
	 *  
	 * @param a the class holding stored answers.
	 */
	public void setAnswers(AbstractOracle a)
	{
		ans = a;
	}
		
	protected Pair<Integer,String> handleAutoAnswer(List<String> question)
	{
		howAnswerWasObtained = QUESTION_USER;
		Pair<Integer,String> AutoAnswer = ans == null? null:ans.getAnswer(question);
		if (AutoAnswer != null)
		{
			howAnswerWasObtained = QUESTION_AUTO;
			return AutoAnswer;
		}
		
		return null;
	}
	
	protected void setAutoOracle()
	{
		if (config.getAutoAnswerFileName().length() > 0)
		{
			ans = new StoredAnswers();
			try {
				((StoredAnswers)ans).setAnswers(new FileReader(config.getAutoAnswerFileName()));
			} catch (Exception e) {
				ans = null;
			}
		}
	}

	/** Displays a tentative graph and asks user a supplied question. 
	 * Options are to be shown as choices in addition to yes/element_not_accepted. 
	 */
	@Override
	public Pair<Integer,String> CheckWithEndUser(LearnerGraph model,List<String> question, final Object [] moreOptions)
	{
		Pair<Integer,String> autoAnswer = handleAutoAnswer(question);if (autoAnswer != null) return autoAnswer;
		return super.CheckWithEndUser(model, question, moreOptions);
	}
}
