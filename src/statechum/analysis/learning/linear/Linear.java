/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
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

package statechum.analysis.learning.linear;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.linear.GDLearnerGraph.DDRH_default;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;

public class Linear {
	final LearnerGraph coregraph;
	
	/** Associates this object to LinearGraph it is using for data to operate on. 
	 * Important: the constructor should not access any data in LinearGraph 
	 * because it is usually invoked during the construction phase of LinearGraph 
	 * when no data is yet available.
	 */
	public Linear(LearnerGraph g)
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
		LearnerGraph copy = new LearnerGraph(reference,copyConfig);
		CmpVertex grInit = AbstractPathRoutines.addToGraph(copy, learnt, null);
		if (forceAccept) for(CmpVertex vert:copy.transitionMatrix.keySet()) vert.setAccept(true);
		copy.learnerCache.invalidate();
		GDLearnerGraph ndGraph = new GDLearnerGraph(copy,LearnerGraphND.ignoreNone,false);
		assert ndGraph.getStatesToNumber().containsKey(copy.getInit());
		assert ndGraph.getStatesToNumber().containsKey(grInit);
		return ndGraph.computeStateCompatibility(ThreadNumber,DDRH_default.class)[ndGraph.vertexToIntNR(copy.getInit(), grInit)]; 
	}

	/** Computes similarity via GD. */
	public static GD.ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> getSimilarityGD(LearnerGraph reference, LearnerGraph learnt, int ThreadNumber) 
	{
		GD.ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new GD.ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(reference,learnt,null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		// I need to remove reject-states because the learnt machine ends up collecting a huge
		// number of negative edges implicit in the original one but since we're learning
		// inherently incomplete systems, it is not clear how to compare
		LearnerGraph reducedReference = new LearnerGraph(reference.config);
		AbstractPathRoutines.removeRejectStates(reference, reducedReference);
		LearnerGraph reducedLearnt = new LearnerGraph(reference.config);
		AbstractPathRoutines.removeRejectStates(learnt,reducedLearnt);
		gd.computeGD(reducedReference, reducedLearnt, ThreadNumber, counter,reference.config);
		return counter;
	}
	
	/** Computes similarity via GD and returns details. */
	public static String getSimilarityGD_details(LearnerGraph reference, LearnerGraph learnt, int ThreadNumber) 
	{
		GD.ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new GD.ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(reference,learnt,null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		// I need to remove reject-states because the learnt machine ends up collecting a huge
		// number of negative edges implicit in the original one but since we're learning
		// inherently incomplete systems, it is not clear how to compare
		LearnerGraph reducedReference = new LearnerGraph(reference.config);
		AbstractPathRoutines.removeRejectStates(reference, reducedReference);
		LearnerGraph reducedLearnt = new LearnerGraph(reference.config);
		AbstractPathRoutines.removeRejectStates(learnt,reducedLearnt);
		gd.computeGD(reducedReference, reducedLearnt, ThreadNumber, counter,reference.config);
		return reducedReference.pathroutines.countEdges()+"+"+counter.getAdded()+"-"+counter.getRemoved()+"="+reducedLearnt.pathroutines.countEdges();
		//return Double.toString(counter.getCompressionRate());
	}
}
