/* Copyright (c) 2016 The University of Sheffield
 
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
import java.util.Stack;

import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.model.testset.PTASequenceEngine;

/** Assume there is a learner that builds a graph, it does not have to be an EDSM-style learner. 
 * Given that many experiments expect a learner to conform to the {@link Learner} interface, 
 * we have to wrap the non-EDSM learner so that it looks like one. This is provided by this class, 
 * where constructGraph delegates inference to any chosen learner.
 */
public abstract class LearnerReturningOriginalGraph implements Learner
{
	public LearnerReturningOriginalGraph() 
	{
	}
	
	public abstract LearnerGraph constructGraph();
	
	@Override
	public LearnerGraph learnMachine() 
	{
		return constructGraph();
	}

	@SuppressWarnings("unused")
	@Override
	public void setTopLevelListener(Learner top) 
	{
		// does nothing
	}

	@SuppressWarnings("unused")
	@Override
	public LearnerGraph learnMachine(PTASequenceEngine engine, int plusSize, int minusSize) 
	{
		return constructGraph();
	}

	@SuppressWarnings("unused")
	@Override
	public LearnerGraph learnMachine(Collection<List<Label>> plus, Collection<List<Label>> minus) 
	{
		return constructGraph();
	}

	@Override
	public String getResult() 
	{
		return null;
	}

	@SuppressWarnings("unused")
	@Override
	public LearnerGraph init(Collection<List<Label>> plus, Collection<List<Label>> minus) 
	{
		return null;
	}

	@SuppressWarnings("unused")
	@Override
	public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
	{
		return null;
	}

	@SuppressWarnings("unused")
	@Override
	public LearnerGraph init(LearnerGraph initPta) 
	{
		return null;
	}
	
	@SuppressWarnings("unused")
	@Override
	public Stack<PairScore> ChooseStatePairs(LearnerGraph gr) 
	{
		throw new UnsupportedOperationException("not implemented in this learner");
	}

	@SuppressWarnings("unused")
	@Override
	public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
	{
		throw new UnsupportedOperationException("not implemented in this learner");
	}

	@SuppressWarnings("unused")
	@Override
	public List<List<Label>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) 
	{
		throw new UnsupportedOperationException("not implemented in this learner");
	}

	@SuppressWarnings("unused")
	@Override
	public List<List<Label>> RecomputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) 
	{
		throw new UnsupportedOperationException("not implemented in this learner");
	}

	@SuppressWarnings("unused")
	@Override
	public Pair<Integer, String> CheckWithEndUser(LearnerGraph gr, List<Label> question, int expectedAccept,
			List<Boolean> acceptedElements, PairScore pairBeingMerged, Object[] options) 
	{
		throw new UnsupportedOperationException("not implemented in this learner");
	}

	@SuppressWarnings("unused")
	@Override
	public void Restart(RestartLearningEnum mode) 
	{
		// does nothing
	}

	@SuppressWarnings("unused")
	@Override
	public void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind, List<Label> sequence, boolean accepted, JUConstants newColour) 
	{
		// does nothing
	}

	@SuppressWarnings("unused")
	@Override
	public boolean AddConstraints(LearnerGraph gr, LearnerGraph resultHolder, StringBuffer counterExampleHolder) 
	{
		throw new UnsupportedOperationException("not implemented in this learner");
	}

	@Override
	public ConvertALabel getLabelConverter() 
	{
		throw new UnsupportedOperationException("not implemented in this learner");
	}

}
