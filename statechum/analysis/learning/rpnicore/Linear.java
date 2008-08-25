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
	public static double getSimilarity(LearnerGraph reference, LearnerGraph learnt, boolean forceAccept, int ThreadNumber)
	{
		Configuration copyConfig = reference.config.copy();copyConfig.setLearnerCloneGraph(true);
		LearnerGraph copy = reference.copy(copyConfig);
		CmpVertex grInit = Transform.addToGraph(copy, learnt, null);
		if (forceAccept) for(CmpVertex vert:copy.transitionMatrix.keySet()) vert.setAccept(true);
		copy.learnerCache.invalidate();
		LearnerGraphND ndGraph = new LearnerGraphND(copy,LearnerGraphND.ignoreNone,false);
		assert ndGraph.getStatesToNumber().containsKey(copy.init);
		assert ndGraph.getStatesToNumber().containsKey(grInit);
		return ndGraph.computeStateCompatibility(ThreadNumber,DDRH_default.class)[ndGraph.vertexToIntNR(copy.init, grInit)]; 
	}

	/** Computes similarity via GD. */
	public static GD.ChangesCounter getSimilarityGD(LearnerGraph reference, LearnerGraph learnt, int ThreadNumber) 
	{
		GD.ChangesCounter counter = new GD.ChangesCounter(reference,learnt,null);
		GD gd = new GD();
		// I need to remove reject-states because the learnt machine ends up collecting a huge
		// number of negative edges implicit in the original one but since we're learning
		// inherently incomplete systems, it is not clear how to compare
		LearnerGraph reducedReference = Transform.removeRejectStates(reference, reference.config), 
			reducedLearnt = Transform.removeRejectStates(learnt, reference.config);
		gd.computeGD(reducedReference, reducedLearnt, ThreadNumber, counter);
		return counter;
	}
	
	/** Computes similarity via GD and returns details. */
	public static String getSimilarityGD_details(LearnerGraph reference, LearnerGraph learnt, int ThreadNumber) 
	{
		GD.ChangesCounter counter = new GD.ChangesCounter(reference,learnt,null);
		GD gd = new GD();
		// I need to remove reject-states because the learnt machine ends up collecting a huge
		// number of negative edges implicit in the original one but since we're learning
		// inherently incomplete systems, it is not clear how to compare
		LearnerGraph reducedReference = Transform.removeRejectStates(reference, reference.config), 
			reducedLearnt = Transform.removeRejectStates(learnt, reference.config);
		gd.computeGD(reducedReference, reducedLearnt, ThreadNumber, counter);
		return reducedReference.countEdges()+"+"+counter.getAdded()+"-"+counter.getRemoved()+"="+reducedLearnt.countEdges();
		//return Double.toString(counter.getCompressionRate());
	}
}
