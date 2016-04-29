/* Copyright (c) 2013 The University of Sheffield.
 * 
 * This file is part of StateChum.
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */

package statechum.analysis.learning;

import java.util.Stack;

import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;

public class AbstractionRefinement extends ASE2014.EDSM_MarkovLearner
{

	public AbstractionRefinement(LearnerEvaluationConfiguration learnerInitConfiguration,LearnerGraph ptaToUseForInference, int i) 
	{
		super(learnerInitConfiguration,ptaToUseForInference,i,-1);
	}

	@Override
	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) 
	{
		return null;
	}

}
