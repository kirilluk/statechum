/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
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

import java.util.Collection;
import java.util.List;

import statechum.Label;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.model.testset.PTASequenceEngine;

/** This cannot be integrated into RPNIBlueFringeLearner because learner
 * has to be observable and we cannot do multiple inheritance.
 */
public abstract class LearnerDecorator implements Learner {
	
	protected Learner decoratedLearner;
	
	public LearnerDecorator(Learner learner){
		this.decoratedLearner = learner;if (learner != null) learner.setTopLevelListener(this);
	}
	
	/** Propagates the request from a listener higher-up to our learner (or a lower-level listener).
	 * 
	 * @param top the current top of the listener stack
	 */
	@Override 
	public void setTopLevelListener(Learner top)
	{
		if (decoratedLearner != null)
			decoratedLearner.setTopLevelListener(top);
	}
	
	@Override 
	public LearnerGraph learnMachine()
	{
		return decoratedLearner.learnMachine();
	}

	@Override 
	public LearnerGraph learnMachine(Collection<List<Label>> plus, Collection<List<Label>> minus)
	{
		init(plus,minus);
		return decoratedLearner.learnMachine();
	}
	
	@Override 
	public LearnerGraph learnMachine(PTASequenceEngine engine, int plusSize, int minusSize)
	{
		init(engine, plusSize, minusSize);
		return decoratedLearner.learnMachine();
	}

}
