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

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.xmachine.model.testset.PTATestSequenceEngine;

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
	public void setTopLevelListener(Learner top)
	{
		if (decoratedLearner != null)
			decoratedLearner.setTopLevelListener(top);
	}
	
	public DirectedSparseGraph learnMachine()
	{
		return decoratedLearner.learnMachine();
	}

	public DirectedSparseGraph learnMachine(Collection<List<String>> plus, Collection<List<String>> minus)
	{
		init(plus,minus);
		return decoratedLearner.learnMachine();
	}
	
	public DirectedSparseGraph learnMachine(PTATestSequenceEngine engine, int plusSize, int minusSize)
	{
		init(engine, plusSize, minusSize);
		return decoratedLearner.learnMachine();
	}

}
