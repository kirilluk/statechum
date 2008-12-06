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

import java.util.List;

import statechum.Pair;
import statechum.analysis.learning.rpnicore.LearnerGraph;

/** Counts the number of hard/soft restarts and the number of questions asked.
 * 
 * @author kirill
 *
 */
public class QuestionAndRestartCounter extends DummyLearner 
{

	public QuestionAndRestartCounter(Learner learner) {
		super(learner);
	}
	
	protected int questionCounter = 0;
	
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

	/* (non-Javadoc)
	 * @see statechum.analysis.learning.observers.DummyLearner#CheckWithEndUser(statechum.analysis.learning.rpnicore.LearnerGraph, java.util.List, java.lang.Object[])
	 */
	@Override
	public Pair<Integer, String> CheckWithEndUser(LearnerGraph graph,
			List<String> question, int responseForNoRestart, int lengthInHardFacts, Object[] options) {
		++questionCounter;
		return super.CheckWithEndUser(graph, question, responseForNoRestart, lengthInHardFacts, options);
	}
	
	/* (non-Javadoc)
	 * @see statechum.analysis.learning.observers.DummyLearner#Restart(statechum.analysis.learning.observers.Learner.RestartLearningEnum)
	 */
	@Override
	public void Restart(RestartLearningEnum mode) {
		super.Restart(mode);
		if (mode != RestartLearningEnum.restartNONE) ++counterRestarted;
	}
	
}
