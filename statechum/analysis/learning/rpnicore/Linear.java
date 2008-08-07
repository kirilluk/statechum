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

package statechum.analysis.learning.rpnicore;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.LearnerGraphND.DDRH_default;
import statechum.analysis.learning.rpnicore.LearnerGraphND.DetermineDiagonalAndRightHandSide;

public class Linear {
	final LearnerGraph coregraph;
	
	/** Associates this object to LinearGraph it is using for data to operate on. 
	 * Important: the constructor should not access any data in LinearGraph 
	 * because it is usually invoked during the construction phase of LinearGraph 
	 * when no data is yet available.
	 */
	Linear(LearnerGraph g)
	{
		coregraph =g;
	}
	
	/** Highlights all negative states and make all the normal vertices accept-vertices. */
	public void moveRejectToHighlight()
	{
		for(CmpVertex v:coregraph.transitionMatrix.keySet())
		{
			v.setHighlight(!v.isAccept());v.setAccept(true);
		}
	}
	
	/** Acts as an oracle, comparing two graphs and returning compatibility score. 
	 *
	 * @param forceAccept if true, assumes that all states are accept-states
	 */
	public double getSimilarity(LearnerGraph gr, boolean forceAccept, int ThreadNumber)
	{
		Configuration copyConfig = (Configuration)coregraph.config.clone();copyConfig.setLearnerCloneGraph(true);
		LearnerGraph copy = coregraph.copy(copyConfig);
		CmpVertex grInit = Transform.addToGraph(copy, gr);
		if (forceAccept) for(CmpVertex vert:copy.transitionMatrix.keySet()) vert.setAccept(true);
		copy.learnerCache.invalidate();
		LearnerGraphND ndGraph = new LearnerGraphND(coregraph,LearnerGraphND.ignoreRejectStates,false);
		assert ndGraph.getStatesToNumber().containsKey(copy.init);
		assert ndGraph.getStatesToNumber().containsKey(grInit);
		return ndGraph.computeStateCompatibility(ThreadNumber,DDRH_default.class)[ndGraph.vertexToIntNR(copy.init, grInit)]; 
	}

	
	/** Acts as an oracle, comparing two graphs are returning compatibility score, however
	 * reject states are not ignored. Accept/reject decisions are copied into highlight
	 * and then objects of the supplied class are used to compute numbers. 
	 */
	public double getSimilarityWithNegatives(LearnerGraph gr, int ThreadNumber, 
			final Class<? extends DetermineDiagonalAndRightHandSide> ddrh)
	{
		Configuration copyConfig = (Configuration)coregraph.config.clone();copyConfig.setLearnerCloneGraph(true);
		LearnerGraph copy = coregraph.copy(copyConfig);
		CmpVertex grInit = Transform.addToGraph(copy, gr);
		copy.linear.moveRejectToHighlight();
		copy.learnerCache.invalidate();
		LearnerGraphND ndGraph = new LearnerGraphND(coregraph,LearnerGraphND.ignoreRejectStates,false);
		double result = ndGraph.computeStateCompatibility(ThreadNumber,ddrh)[ndGraph.vertexToIntNR(copy.init, grInit)];
		
		return result;
	}

}
