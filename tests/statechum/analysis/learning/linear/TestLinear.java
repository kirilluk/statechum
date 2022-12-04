/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

package statechum.analysis.learning.linear;

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Test;

import cern.colt.bitvector.BitVector;
import cern.colt.function.IntComparator;

import statechum.Configuration;
import statechum.Configuration.GDScoreComputationAlgorithmEnum;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Label;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph.StatesToConsider;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.linear.GDLearnerGraph.DetermineDiagonalAndRightHandSide;
import statechum.analysis.learning.linear.GDLearnerGraph.HandleRow;
import statechum.analysis.learning.linear.GDLearnerGraph.StateBasedRandom;
import statechum.collections.MapWithSearch;

public class TestLinear {

	final Configuration configMain = Configuration.getDefaultConfiguration();
	final ConvertALabel converter = null;
	
	int cnt = 0;
	
	public final void benchmarkArrayTransformations()
	{
		int[] buf = new int[8];
		Random rnd = new Random(0);
		for(int i=0;i<buf.length;++i) buf[i]=buf.length-i;
		for(int reorder=0;reorder<buf.length/3;++reorder)
			buf[rnd.nextInt(buf.length)]=buf[rnd.nextInt(buf.length)];

		int[] tmpBuf =new int[buf.length];
		int stateNumber = 1000;
		int[] Ai = new int[stateNumber*(stateNumber+1)/2*buf.length];
		double[] Ax = new double[Ai.length];
		int pos=0;
		for(int tmp=0;tmp<stateNumber*(stateNumber+1)/2;++tmp)
		{
			System.arraycopy(buf, 0, tmpBuf, 0, buf.length);		
			cern.colt.Sorting.quickSort(tmpBuf, 0, buf.length, (o1, o2) -> {
				++cnt;
				return o1-o2;
			});
			
			Ai[pos++]=tmpBuf[0];
			int prev = tmpBuf[0];
			for(int i=1;i<buf.length;++i)
				if(tmpBuf[i]!=prev)
				{
					prev=tmpBuf[i];
					Ax[pos]=-0.9;
					Ai[pos++]=prev;
				}
				else Ax[pos]+=-0.9;
			
		}
		System.out.println("cmp calls: "+cnt+" pos="+pos+" out of "+Ai.length);
	}
	
	LearnerGraph grLoadDistribution=buildLearnerGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7",configMain,converter);
	GDLearnerGraph grLoadDistributionND = new GDLearnerGraph(grLoadDistribution,LearnerGraphND.ignoreRejectStates, false);

	/** Tests how well the workload is distributed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWorkLoadDistribution0_1()
	{
		GDLearnerGraph.partitionWorkLoadTriangular(0,grLoadDistributionND.getStateNumber());
	}

	/** Tests how well the workload is distributed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWorkLoadDistribution0_2a()
	{
		GDLearnerGraph.partitionWorkLoadTriangular(-1,grLoadDistributionND.getStateNumber());
	}

	/** Tests how well the workload is distributed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWorkLoadDistribution0_2b()
	{
		GDLearnerGraph.partitionWorkLoadLinear(-1,grLoadDistributionND.getStateNumber());
	}

	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution1()
	{
		Assert.assertArrayEquals(new int[]{0,4},GDLearnerGraph.partitionWorkLoadTriangular(1,grLoadDistributionND.getStateNumber()));
		Assert.assertArrayEquals(new int[]{0,4},GDLearnerGraph.partitionWorkLoadLinear(1,grLoadDistributionND.getStateNumber()));
	}

	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution2()
	{
		Assert.assertArrayEquals(new int[]{0,1,2,3,4},GDLearnerGraph.partitionWorkLoadTriangular(4,grLoadDistributionND.getStateNumber()));
		Assert.assertArrayEquals(new int[]{0,1,2,3,4},GDLearnerGraph.partitionWorkLoadLinear(4,grLoadDistributionND.getStateNumber()));
	}
	
	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution3()
	{
		Assert.assertArrayEquals(new int[]{0,2,4},GDLearnerGraph.partitionWorkLoadTriangular(2,grLoadDistributionND.getStateNumber()));
		Assert.assertArrayEquals(new int[]{0,2,4},GDLearnerGraph.partitionWorkLoadLinear(2,grLoadDistributionND.getStateNumber()));
	}
	
	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution4()
	{
		Assert.assertArrayEquals(new int[]{0,2,3,4},GDLearnerGraph.partitionWorkLoadTriangular(3,grLoadDistributionND.getStateNumber()));
		Assert.assertArrayEquals(new int[]{0,1,2,4},GDLearnerGraph.partitionWorkLoadLinear(3,grLoadDistributionND.getStateNumber()));
	}
	
	@Test
	public final void testWorkLoadDistributionUsingFilter1()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7",configMain,converter);
		Assert.assertArrayEquals(new int[]{0,2,3,4,4},GDLearnerGraph.partitionWorkLoadTriangular(4, gr.transitionMatrix, new LearnerGraphND.ignoreNoneClass()));
	}	
	
	@Test
	public final void testWorkLoadDistributionUsingFilter2()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7",configMain,converter);
		Assert.assertArrayEquals(new int[]{0,4},GDLearnerGraph.partitionWorkLoadTriangular(1, gr.transitionMatrix, new LearnerGraphND.ignoreNoneClass()));
	}	

	@Test
	public final void testWorkLoadDistributionUsingFilter3()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7",configMain,converter);
		Assert.assertArrayEquals(new int[]{0,2,3,4,4,4},GDLearnerGraph.partitionWorkLoadTriangular(5, gr.transitionMatrix, new LearnerGraphND.ignoreNoneClass()));
	}
	
	@Test
	public final void testWorkLoadDistributionUsingFilter4()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7",configMain,converter);
		Assert.assertArrayEquals(new int[]{0,2,3,4,4,4},GDLearnerGraph.partitionWorkLoadTriangular(5, gr.transitionMatrix, new LearnerGraphND.StatesToConsider() {
// identical values mean that it will be the last thread that will run on row 3 and over; thread 0 will start at 0 and all other threads will be doing nothing.
			@Override
			public boolean stateToConsider(CmpVertex vert) {
				return !vert.toString().equals("B");
			}
			
		}));
	}
	
	@Test
	public final void testWorkLoadDistributionUsingFilter5()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7",configMain,converter);
		Assert.assertArrayEquals(new int[]{4,4,4,4,4,4},GDLearnerGraph.partitionWorkLoadTriangular(5, gr.transitionMatrix, new LearnerGraphND.StatesToConsider() {

			@Override
			public boolean stateToConsider(@SuppressWarnings("unused") CmpVertex vert) {
				return false;
			}
			
		}));
	}
	
	@Test
	public final void testWorkLoadDistributionUsingFilter6()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A-e->E-f->F-g->G","testWorkLoadDistributionUsingFilter6",configMain,converter);
		Assert.assertArrayEquals(new int[]{0,3,4,5,7},GDLearnerGraph.partitionWorkLoadTriangular(4, gr.transitionMatrix, new LearnerGraphND.StatesToConsider() {

			@Override
			public boolean stateToConsider(CmpVertex vert) {
				return !vert.toString().equals("C");
			}
			
		}));
	}
	
	/** Gives the same results as testWorkLoadDistributionUsingFilter6 essentially because we are counting active rows rather than absolute ones. */ 
	@Test
	public final void testWorkLoadDistributionUsingFilter7()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A-e->E-f->F-g->G","testWorkLoadDistributionUsingFilter6",configMain,converter);
		Assert.assertArrayEquals(new int[]{0,3,4,5,7},GDLearnerGraph.partitionWorkLoadTriangular(4, gr.transitionMatrix, new LearnerGraphND.StatesToConsider() {

			@Override
			public boolean stateToConsider(CmpVertex vert) {
				return !vert.toString().equals("E");
			}
			
		}));
	}
	
	@Test
	public final void testWorkLoadDistributionUsingFilter8()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A-e->E-f->F-g->G","testWorkLoadDistributionUsingFilter6",configMain,converter);
		Assert.assertArrayEquals(new int[]{0,5,7},GDLearnerGraph.partitionWorkLoadTriangular(2, gr.transitionMatrix, new LearnerGraphND.StatesToConsider() {

			@Override
			public boolean stateToConsider(CmpVertex vert) {
				return !vert.toString().equals("E");
			}
			
		}));
	}
	
	
	/** Tests the workload distribution. */
	@Test
	public final void testWorkLoadDistribution5()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7",configMain,converter);
		final int ThreadNumber=4;
		for(int i=0;i< ThreadNumber;++i) AbstractPathRoutines.addToGraph(gr, grLoadDistribution,null);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		Assert.assertArrayEquals(new int[]{0,9,14,17,20},GDLearnerGraph.partitionWorkLoadTriangular(ThreadNumber,ndGraph.getStateNumber()));
		Assert.assertArrayEquals(new int[]{0,5,10,15,20},GDLearnerGraph.partitionWorkLoadLinear(ThreadNumber,ndGraph.getStateNumber()));
	}
	
	/** Tests the workload distribution. */
	@Test
	public final void testPerformRowTasks_A_1()
	{
		LearnerGraph gr=new LearnerGraph(configMain);gr.getInit().setAccept(false);
		StatesToConsider filter = LearnerGraphND.ignoreRejectStates;
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,filter, false);
		int ThreadNumber=4;
		Assert.assertArrayEquals(new int[]{0,0,0,0,0},GDLearnerGraph.partitionWorkLoadTriangular(ThreadNumber,ndGraph.getStateNumber()));
		Assert.assertArrayEquals(new int[]{0,0,0,0,0},GDLearnerGraph.partitionWorkLoadLinear(ThreadNumber,ndGraph.getStateNumber()));

		final Map<Integer,Integer> threadToRowNumber = Collections.synchronizedMap(new TreeMap<Integer,Integer>());
		
		List<HandleRow<List<CmpVertex>>> handlerList = new LinkedList<HandleRow<List<CmpVertex>>>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
			handlerList.add(new HandleRow<List<CmpVertex>>()
			{
				@Override
				public void init(@SuppressWarnings("unused") int threadNo) {
					// No per-thread initialisation is needed.
				}
	
				@Override
				public void handleEntry(@SuppressWarnings("unused") Entry<CmpVertex, MapWithSearch<Label,Label, List<CmpVertex>>> entryA, int threadNo)
				{
					Integer newValue = threadToRowNumber.get(threadNo);
					if (newValue == null)
					{
						newValue = Integer.valueOf(0);
					}
					threadToRowNumber.put(threadNo,newValue+1);
				}
				
			});
		GDLearnerGraph.performRowTasks(handlerList, ThreadNumber, ndGraph.matrixForward.transitionMatrix, filter,
				GDLearnerGraph.partitionWorkLoadTriangular(ThreadNumber, ndGraph.matrixForward.transitionMatrix.size()));
		Assert.assertEquals(0, threadToRowNumber.size());
		//Assert.assertEquals(1, threadToRowNumber.values().iterator().next().intValue());
	}
	
	/** Tests the workload distribution. */
	@Test
	public final void testPerformRowTasks_A_2()
	{
		LearnerGraph gr=new LearnerGraph(configMain);
		StatesToConsider filter = LearnerGraphND.ignoreRejectStates;
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,filter, false);
		int ThreadNumber=4;
		Assert.assertArrayEquals(new int[]{0,0,0,0,1},GDLearnerGraph.partitionWorkLoadTriangular(ThreadNumber,ndGraph.getStateNumber()));

		final Map<Integer,Integer> threadToRowNumber = Collections.synchronizedMap(new TreeMap<Integer,Integer>());
		
		List<HandleRow<List<CmpVertex>>> handlerList = new LinkedList<HandleRow<List<CmpVertex>>>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
			handlerList.add(new HandleRow<List<CmpVertex>>()
			{
				@Override
				public void init(@SuppressWarnings("unused") int threadNo) {
					// No per-thread initialisation is needed.
				}
	
				@Override
				public void handleEntry(@SuppressWarnings("unused") Entry<CmpVertex, MapWithSearch<Label,Label, List<CmpVertex>>> entryA, int threadNo)
				{
					Integer newValue = threadToRowNumber.get(threadNo);
					if (newValue == null)
					{
						newValue = Integer.valueOf(0);
					}
					threadToRowNumber.put(threadNo,newValue+1);
				}
				
			});
		GDLearnerGraph.performRowTasks(handlerList, ThreadNumber,ndGraph.matrixForward.transitionMatrix,filter,
				GDLearnerGraph.partitionWorkLoadTriangular(ThreadNumber, ndGraph.matrixForward.transitionMatrix.size()));
		Assert.assertEquals(1, threadToRowNumber.size());
		Assert.assertEquals(1, threadToRowNumber.values().iterator().next().intValue());
		
	}
	
	/** Tests the workload distribution. */
	@Test
	public final void testPerformRowTasks_A_3()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B-a-#C\nA-b-#D\nA-c-#E\nB-b-#F\nB-c-#G","testPerformRowTasks_A_3",configMain,converter);
		StatesToConsider filter = LearnerGraphND.ignoreRejectStates;
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,filter, false);
		int ThreadNumber=4;

		final Map<Integer,Integer> threadToRowNumber = Collections.synchronizedMap(new TreeMap<Integer,Integer>());
		
		List<HandleRow<List<CmpVertex>>> handlerList = new LinkedList<HandleRow<List<CmpVertex>>>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
			handlerList.add(new HandleRow<List<CmpVertex>>()
			{
				@Override
				public void init(@SuppressWarnings("unused") int threadNo) {
					// No per-thread initialisation is needed.
				}
	
				@Override
				public void handleEntry(@SuppressWarnings("unused") Entry<CmpVertex, MapWithSearch<Label,Label, List<CmpVertex>>> entryA, int threadNo)
				{
					Integer newValue = threadToRowNumber.get(threadNo);
					if (newValue == null)
					{
						newValue = Integer.valueOf(0);
					}
					threadToRowNumber.put(threadNo,newValue+1);
				}
				
			});
		Assert.assertArrayEquals(new int[]{0,0,1,1,2},GDLearnerGraph.partitionWorkLoadTriangular(ThreadNumber,ndGraph.getStateNumber()));
		GDLearnerGraph.performRowTasks(handlerList, ThreadNumber, ndGraph.matrixInverse.transitionMatrix,filter,
				GDLearnerGraph.partitionWorkLoadTriangular(ThreadNumber, ndGraph.matrixInverse.transitionMatrix.size()));
		Assert.assertEquals(2, threadToRowNumber.size());
		int counterOfAllUsedRows=0;
		for(Integer numberOfRows:threadToRowNumber.values()) counterOfAllUsedRows+=numberOfRows;
		Assert.assertEquals(2, counterOfAllUsedRows);// 2 is the number of states which were not ignored
		
		threadToRowNumber.clear();
		Assert.assertArrayEquals(new int[]{0,3,4,6,7},GDLearnerGraph.partitionWorkLoadTriangular(ThreadNumber,gr.getStateNumber()));
		GDLearnerGraph.performRowTasks(handlerList, ThreadNumber, ndGraph.matrixForward.transitionMatrix,filter,
				GDLearnerGraph.partitionWorkLoadTriangular(ThreadNumber, ndGraph.matrixForward.transitionMatrix.size()));
		Assert.assertEquals(1, threadToRowNumber.size());// only one thread gets to do anything because A and B are within its scope.
		counterOfAllUsedRows=0;
		for(Integer numberOfRows:threadToRowNumber.values()) counterOfAllUsedRows+=numberOfRows;
		Assert.assertEquals(2, counterOfAllUsedRows);// 2 is the number of states which were not ignored
	}
	
	public static final int PAIR_INCOMPATIBLE=GDLearnerGraph.PAIR_INCOMPATIBLE, PAIR_OK=GDLearnerGraph.PAIR_OK;
	
	@Test
	public final void testNumberNonNegativeElementsA()
	{
		int [] arrayA=new int[]{PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE};
		Assert.assertEquals(0,GDLearnerGraph.numberNonNegativeElements(arrayA));
		Assert.assertArrayEquals(new int[]{PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE},arrayA);
	}
	@Test
	public final void testNumberNonNegativeElementsB()
	{
		int [] arrayB=new int[]{PAIR_INCOMPATIBLE,6,7,PAIR_INCOMPATIBLE,8};
		Assert.assertEquals(0,GDLearnerGraph.numberNonNegativeElements(arrayB));
		Assert.assertArrayEquals(new int[]{PAIR_INCOMPATIBLE,6,7,PAIR_INCOMPATIBLE,8},arrayB);
	}

	@Test
	public final void testNumberNonNegativeElementsC()
	{
		int [] arrayC=new int[]{PAIR_INCOMPATIBLE,PAIR_OK,PAIR_OK,PAIR_INCOMPATIBLE,PAIR_OK};
		Assert.assertEquals(3,GDLearnerGraph.numberNonNegativeElements(arrayC));
		Assert.assertArrayEquals(new int[]{PAIR_INCOMPATIBLE,0,1,PAIR_INCOMPATIBLE,2},arrayC);
	}
	
	@Test
	public final void testNumberNonNegativeElementsD()
	{
		int [] arrayD=new int[]{PAIR_INCOMPATIBLE,6,PAIR_OK,PAIR_INCOMPATIBLE,8,PAIR_INCOMPATIBLE};
		Assert.assertEquals(1,GDLearnerGraph.numberNonNegativeElements(arrayD));
		Assert.assertArrayEquals(new int[]{PAIR_INCOMPATIBLE,6,0,PAIR_INCOMPATIBLE,8,PAIR_INCOMPATIBLE},arrayD);
	}
	
	@Test
	public final void testIntersects()
	{
		BitVector A=new BitVector(100),B=new BitVector(100);
		Assert.assertFalse(GDLearnerGraph.intersects(A, B));
		A.set(10);
		Assert.assertFalse(GDLearnerGraph.intersects(A, B));
		B.set(10);
		Assert.assertTrue(GDLearnerGraph.intersects(A, B));
		A.clear(10);
		A.set(71);A.set(72);B.set(72);
		Assert.assertTrue(GDLearnerGraph.intersects(A, B));
		A.clear(72);
		Assert.assertFalse(GDLearnerGraph.intersects(A, B));
	}
	
	/** A helper to call matches on both permutations of states to check if the result is the same.
	 * 
	 * @param gr graph to consider
	 * @param matrixND the matrix to call a matcher on
	 * @param matcher matcher to use
	 * @param A name of the first state
	 * @param B name of the second state
	 */
	private final void getMatcherValue(LearnerGraph gr,GDLearnerGraph gdlearnerGraph,LearnerGraphND matrixND, DetermineDiagonalAndRightHandSide matcher, String A,String B)
	{
		matcher.compute(gr.findVertex(A),gr.findVertex(B),
				matrixND.transitionMatrix.get(gr.findVertex(A)),matrixND.transitionMatrix.get(gr.findVertex(B)));

		double rightHand = matcher.getRightHandSide(), diag = matcher.getDiagonal();
		// Now check that matcher is stateless by computing the same in the reverse order.
		matcher.compute(gr.findVertex(B),gr.findVertex(A),
				matrixND.transitionMatrix.get(gr.findVertex(B)),matrixND.transitionMatrix.get(gr.findVertex(A)));

		Assert.assertEquals("right-hand side",matcher.getRightHandSide(),rightHand,Configuration.fpAccuracy);
		Assert.assertEquals("right-hand side",matcher.getDiagonal(),diag,Configuration.fpAccuracy);
		
		// Now copy the matcher and do another computation, with A and B reversed.
		DetermineDiagonalAndRightHandSide anotherMather = null;
		try {// based on http://forums.sun.com/thread.jspa?threadID=767974
			anotherMather = matcher.getClass().getDeclaredConstructor(new Class[]{GDLearnerGraph.class}).newInstance(new Object[]{gdlearnerGraph});
		} catch (Exception e) {
			Assert.fail("Unexpected exception cloning a matcher: "+e);
		}
		anotherMather.compute(gr.findVertex(B),gr.findVertex(A),
				matrixND.transitionMatrix.get(gr.findVertex(B)),matrixND.transitionMatrix.get(gr.findVertex(A)));

		Assert.assertEquals("right-hand side",anotherMather.getRightHandSide(),rightHand,Configuration.fpAccuracy);
		Assert.assertEquals("right-hand side",anotherMather.getDiagonal(),diag,Configuration.fpAccuracy);
	}
	
	@Test
	public final void testCountMatchingOutgoing1()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->B\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1", configMain,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default(); 
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"A","Q");
		Assert.assertEquals(2,matcher.getRightHandSide(),Configuration.fpAccuracy);
	}

	@Test
	public final void testCountMatchingOutgoing2()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->B\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1", configMain,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default(); 
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"A","A");
		Assert.assertEquals(3,matcher.getRightHandSide(),Configuration.fpAccuracy);
	}

	/** States with incompatible children will get a reduced score. */
	@Test
	public final void testCountMatchingOutgoing3a()
	{
		LearnerGraph gr=buildLearnerGraph("A-a-#B\nA-b->B1\nA-c->C\nQ-a->R-a->R\nQ-b->S", "testCountMatchingOutgoing3a", configMain,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default(); 
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher ,"A","Q");
		// A and Q have 3 outgoing transitions, 2 are matched.
		// target pair B1-S has a score of 0, B-R has a score of -1.
		Assert.assertEquals(2,matcher.getRightHandSide(),Configuration.fpAccuracy);
		Assert.assertEquals(3*2,matcher.getDiagonal(),Configuration.fpAccuracy);
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher ,"B","R");
		Assert.assertEquals(PAIR_INCOMPATIBLE,matcher.getRightHandSide(),Configuration.fpAccuracy);
		Assert.assertEquals(1*2,matcher.getDiagonal(),Configuration.fpAccuracy);
	}

	/** Directly incompatible states. */
	@Test
	public final void testCountMatchingOutgoing3b()
	{
		LearnerGraph gr=buildLearnerGraph("A-a-#B\nA-b->B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing3b", configMain,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreNone, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default(); 
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher ,"A","B");
		Assert.assertEquals(matcher.getDiagonal()*PAIR_INCOMPATIBLE,matcher.getRightHandSide()*2,Configuration.fpAccuracy);
		Assert.assertEquals(3*2,matcher.getDiagonal(),Configuration.fpAccuracy);
	}

	@Test
	public final void testCountMatchingOutgoing3c()
	{
		Configuration config = configMain.copy();
		LearnerGraph gr=buildLearnerGraph("A-a-#B\nA-b->B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing3c", config,converter);
		gr.linear.moveRejectToHighlight();
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default();
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"A","Q");
		// A and Q have 3 outgoing and 2 matched.
		// the score for the first pair is -1, for another one it is 1, hence the outcome is zero
		Assert.assertEquals(2,matcher.getRightHandSide(),Configuration.fpAccuracy);
		Assert.assertEquals(3*2,matcher.getDiagonal(),Configuration.fpAccuracy);
	}
	
	@Test
	public final void testCountMatchingOutgoing3d()
	{
		Configuration config = configMain.copy();
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->B1\nA-c->C\nQ-a->R\nQ-b->Q-c->S", "testCountMatchingOutgoing3d", config,converter);
		gr.linear.moveRejectToHighlight();
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default();
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"A","Q");
		// A and Q have 3 outgoing and 3 matched.
		// the score for the first pair is -1, for the other ones it is 1, hence the outcome is 1
		Assert.assertEquals(3,matcher.getRightHandSide(),Configuration.fpAccuracy);
		Assert.assertEquals(3*2,matcher.getDiagonal(),Configuration.fpAccuracy);
	}
	@Test
	public final void testCountMatchingOutgoing3e()
	{
		Configuration config = configMain.copy();
		LearnerGraph gr=buildLearnerGraph("A-a-#B\nA-b->B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing3c", config,converter);
		gr.linear.moveRejectToHighlight();
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_highlight();
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"A","Q");
		// A and Q have 3 outgoing and 2 matched.
		Assert.assertEquals(1,matcher.getRightHandSide(),Configuration.fpAccuracy);
	}

	@Test
	public final void testCountMatchingOutgoing3f()
	{
		Configuration config = configMain.copy();
		LearnerGraph gr=buildLearnerGraph("A-a->B\nA-b->B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing3d", config,converter);
		gr.linear.moveRejectToHighlight();
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_highlight();
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"A","Q");
		// A and Q have 3 outgoing and 3 matched.
		// the score for the first pair is -1, for the other ones it is 1, hence the outcome is 1
		Assert.assertEquals(2,matcher.getRightHandSide(),Configuration.fpAccuracy);
	}

	@Test
	public final void testCountMatchingOutgoing4a()
	{
		LearnerGraph gr=buildLearnerGraph("A-a-#B\nA-b-#B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1", configMain,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default();
		// 3 outgoing and 2 matching reject, but we count them as positives
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"A","Q");
		Assert.assertEquals(2,matcher.getRightHandSide(),Configuration.fpAccuracy);
	}

	@Test
	public final void testCountMatchingOutgoing4b()
	{
		Configuration config = configMain.copy();
		LearnerGraph gr=buildLearnerGraph("A-a-#B\nA-b-#B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1", config,converter);
		gr.linear.moveRejectToHighlight();
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_highlight();
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"A","Q");
		Assert.assertEquals(0,matcher.getRightHandSide(),Configuration.fpAccuracy);
	}

	@Test
	public final void testCountMatchingOutgoing5a()
	{
		LearnerGraph gr=buildLearnerGraph("A-a-#B\nA-b-#B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1", configMain,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default();
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"A","C");
		Assert.assertEquals(0,matcher.getRightHandSide(),Configuration.fpAccuracy);
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"S","C");
		Assert.assertEquals(0,matcher.getRightHandSide(),Configuration.fpAccuracy);
	}

	@Test
	public final void testCountMatchingOutgoing5b()
	{
		Configuration config = configMain.copy();
		LearnerGraph gr=buildLearnerGraph("A-a-#B\nA-b-#B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1", config,converter);
		gr.linear.moveRejectToHighlight();
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_highlight();
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"A","C");
		Assert.assertEquals(0,matcher.getRightHandSide(),Configuration.fpAccuracy);
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"S","C");
		Assert.assertEquals(0,matcher.getRightHandSide(),Configuration.fpAccuracy);
	}


	@Test
	public final void testCountMatchingOutgoing6a()
	{
		LearnerGraph gr=buildLearnerGraph("A-a-#B\nA-b-#B1\nQ-a->R", "testCountMatchingOutgoing1", configMain,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default();
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"A","R");
		Assert.assertEquals(0,matcher.getRightHandSide(),Configuration.fpAccuracy);
	}
	
	@Test
	public final void testCountMatchingOutgoing6b()
	{
		LearnerGraph gr=buildLearnerGraph("A-a-#B\nA-b-#B1\nQ-a->R", "testCountMatchingOutgoing1", configMain,converter);
		gr.linear.moveRejectToHighlight();
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_highlight();
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"A","R");
		Assert.assertEquals(0,matcher.getRightHandSide(),Configuration.fpAccuracy);
	}
	
	/** These are tests how the matcher handles a case of non-deterministism. 
	 * Accomplished by building a normal graph and then running the matcher
	 * on the inverse of it (I cannot build a non-deterministic graph directly
	 * because LearnerGraph has to be deterministic and I do not wish to
	 * manually build a non-deterministic transition matrix). 
	 */
	@Test
	public final void testCountMatchingOutgoing_nd1()
	{
		LearnerGraph gr=buildLearnerGraph("A1-a->C\nA2-a->C\nA3-a->C<-b-G\nB1-a->D<-a-B2\nE-b->D<-b-F", "testCountMatchingOutgoing_nd1", configMain,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default();
		getMatcherValue(gr,ndGraph,ndGraph.matrixInverse, matcher,"C","D");
		Assert.assertEquals(8,matcher.getRightHandSide(),Configuration.fpAccuracy);
		Assert.assertEquals(8*2,matcher.getDiagonal(),Configuration.fpAccuracy);
	}
	
	@Test
	public final void testCountMatchingOutgoing_nd2a()
	{
		LearnerGraph gr=buildLearnerGraph("A1-a->C\nA2-a->C\nA3-a->C<-b-G\nB1-a->D<-a-B2\nE-b->D<-b-F\n"
				+"N-c->C", "testCountMatchingOutgoing_nd2a", configMain,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default();
		getMatcherValue(gr,ndGraph,ndGraph.matrixInverse, matcher,"C","D");
		Assert.assertEquals(8,matcher.getRightHandSide(),Configuration.fpAccuracy);
		Assert.assertEquals(9*2,matcher.getDiagonal(),Configuration.fpAccuracy);
	}
	
	@Test
	public final void testCountMatchingOutgoing_nd2b()
	{
		LearnerGraph gr=buildLearnerGraph("A1-a->C\nA2-a->C\nA3-a->C<-b-G\nB1-a->D<-a-B2\nE-b->D<-b-F\n"
				+"N-c->C\n"
				+"M-d->D"
				, "testCountMatchingOutgoing_nd2b", configMain,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default();
		getMatcherValue(gr,ndGraph,ndGraph.matrixInverse, matcher,"C","D");
		Assert.assertEquals(8,matcher.getRightHandSide(),Configuration.fpAccuracy);
		Assert.assertEquals(10*2,matcher.getDiagonal(),Configuration.fpAccuracy);
	}
	
	@Test
	public final void testCountMatchingOutgoing_nd3a()
	{
		LearnerGraph gr=buildLearnerGraph("A1-a->C\nA2-a->C\nA3-a->C<-b-G\nB1-a->D<-a-B2\nE-b->D<-b-F\n"
				+"N-c->C\n"
				+"N-a->C", "testCountMatchingOutgoing_nd3a", configMain,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default();
		getMatcherValue(gr,ndGraph,ndGraph.matrixInverse, matcher,"C","D");
		Assert.assertEquals(10,matcher.getRightHandSide(),Configuration.fpAccuracy);
		Assert.assertEquals(11*2,matcher.getDiagonal(),Configuration.fpAccuracy);
	}
	
	@Test
	public final void testCountMatchingOutgoing_nd3b()
	{
		LearnerGraph gr=buildLearnerGraph("A1-a->C\nA2-a->C\nA3-a->C<-b-G\nB1-a->D<-a-B2\nE-b->D<-b-F\n"
				+"N-c->C<-f-U\n"
				+"N-a->C\nS-r->D", "testCountMatchingOutgoing_nd3b", configMain,converter);
		//Visualiser.updateFrame(gr, null);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_default();
		getMatcherValue(gr,ndGraph,ndGraph.matrixInverse, matcher,"C","D");
		Assert.assertEquals(10,matcher.getRightHandSide(),Configuration.fpAccuracy);
		Assert.assertEquals(13*2,matcher.getDiagonal(),Configuration.fpAccuracy);
	}
	
	final String NDGraph = "D-f->D-c->C-b->B-a->A\nF-d->F-b->E-a->A\nH-e->H-c->G-b->E";
	
	@Test
	public final void testCount_computeGraphs()
	{
		Configuration config = configMain.copy();
		config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(2);
		config.setGdScoreComputationAlgorithm_RandomWalk_ExtraLength(0);
		LearnerGraph gr=buildLearnerGraph(NDGraph, "testCount_computeGraphs", config,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, true);
		//Visualiser.updateFrame(ndGraph.matrixInverse, null);
		//RandomPathGenerator.diameter(ndGraph.matrixInverse);
		ndGraph.computeWalkSequences(new StateBasedRandom(80), 1);
		
		Assert.assertNull(WMethod.checkM(buildLearnerGraph("A-a->BE-b->CFG-d->F-d->F / CFG-c->DH-f->D-f->D / DH-e->H-e->H","testCount_computeGraphs_A",config,converter), 
				ndGraph.stateToCorrespondingGraph.get(gr.findVertex(VertexID.parseID("A"))).graph));
		Assert.assertNull(WMethod.checkM(buildLearnerGraph("B-b->C-c->D-f->D","testCount_computeGraphs_B",config,converter), 
				ndGraph.stateToCorrespondingGraph.get(gr.findVertex(VertexID.parseID("B"))).graph));
		Assert.assertNull(WMethod.checkM(buildLearnerGraph("C-c->D-f->D","testCount_computeGraphs_C",config,converter), 
				ndGraph.stateToCorrespondingGraph.get(gr.findVertex(VertexID.parseID("C"))).graph));
		Assert.assertNull(WMethod.checkM(buildLearnerGraph("D-f->D","testCount_computeGraphs_D",config,converter), 
				ndGraph.stateToCorrespondingGraph.get(gr.findVertex(VertexID.parseID("D"))).graph));
		Assert.assertNull(WMethod.checkM(buildLearnerGraph("E-b->FG-d->F-d->F / FG-c->H-e->H","testCount_computeGraphs_E",config,converter), 
				ndGraph.stateToCorrespondingGraph.get(gr.findVertex(VertexID.parseID("E"))).graph));
		Assert.assertNull(WMethod.checkM(buildLearnerGraph("G-c->H-e->H","testCount_computeGraphs_G",config,converter), 
				ndGraph.stateToCorrespondingGraph.get(gr.findVertex(VertexID.parseID("G"))).graph));
		Assert.assertNull(WMethod.checkM(buildLearnerGraph("H-e->H","testCount_computeGraphs_H",config,converter), 
				ndGraph.stateToCorrespondingGraph.get(gr.findVertex(VertexID.parseID("H"))).graph));
		Assert.assertNull(WMethod.checkM(buildLearnerGraph("F-d->F","testCount_computeGraphs_F",config,converter), 
				ndGraph.stateToCorrespondingGraph.get(gr.findVertex(VertexID.parseID("F"))).graph));
	}
	
	@Test
	public final void testCount_computeBCR_Walks()
	{
		Configuration config = configMain.copy();
		config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(200);
		config.setGdScoreComputationAlgorithm_RandomWalk_ExtraLength(0);
		config.setGdScoreComputationAlgorithm_RandomWalk_PathLength(4);
		LearnerGraph gr=buildLearnerGraph(NDGraph, "testCount_computeGraphs", config,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, true);
		ndGraph.computeWalkSequences(new StateBasedRandom(80), 1);
		
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_BCR();
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"C","D");
		Assert.assertEquals(4.76190476190,matcher.getRightHandSide(),Configuration.fpAccuracy);
		Assert.assertEquals(2*2,matcher.getDiagonal(),Configuration.fpAccuracy);

		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"E","B");
		Assert.assertEquals(6.374807987711213,matcher.getRightHandSide(),Configuration.fpAccuracy);
		Assert.assertEquals(2*2,matcher.getDiagonal(),Configuration.fpAccuracy);
	}

	@Test
	public final void testCount_computeBCR_TestSet()
	{
		Configuration config = configMain.copy();
		config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		LearnerGraph gr=buildLearnerGraph(NDGraph, "testCount_computeGraphs", config,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,LearnerGraphND.ignoreRejectStates, true);
		ndGraph.computeWalkSequences(new StateBasedRandom(80), 1);
		
		DetermineDiagonalAndRightHandSide matcher = ndGraph.new DDRH_BCR();
		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"C","D");
		Assert.assertEquals(100./3.,matcher.getRightHandSide(),Configuration.fpAccuracy);
		Assert.assertEquals(2*2,matcher.getDiagonal(),Configuration.fpAccuracy);

		getMatcherValue(gr,ndGraph,ndGraph.matrixForward, matcher,"E","B");
		Assert.assertEquals(100,matcher.getRightHandSide(),Configuration.fpAccuracy);
		Assert.assertEquals(2*2,matcher.getDiagonal(),Configuration.fpAccuracy);
	}

	/** Adds reject vertices with names starting with a given prefix, and 
	 * the suffix sequentially increasing 0..number.
	 * The first reject vertex is just the prefix, to add a single state 
	 * with just a prefix, call
	 * <code>
	 * addRejectVertices(gr, prefix,-1);
	 * </code>
	 * @param gr graph to update
	 * @param prefix the prefix for all reject vertices
	 * @param number number of elements to add
	 */
	public static void addRejectVertices(LearnerGraph gr,String prefix, int number)
	{
		for(int i=-1;i<number;++i)
		{
			VertexID id = VertexID.parseID(prefix+(i>=0?i:""));if (gr.findVertex(id) != null) throw new IllegalArgumentException("vertex "+id+" already exists");
			CmpVertex newVertex = AbstractLearnerGraph.generateNewCmpVertex(id, gr.config);newVertex.setAccept(false);
			gr.transitionMatrix.put(newVertex, gr.createNewRow());
		}
	}
	
	@Test
	public final void testAddRejectVertices0()
	{
		LearnerGraph gr=buildLearnerGraph("A00-a->B-a->C","testAddRejectVertices",configMain,converter);
		addRejectVertices(gr, "A0", -1);
		LearnerGraph expectedResult = buildLearnerGraph("A-a->B-a->C\nA0-a->A","testAddRejectVertices_result",configMain,converter);
		for(Entry<CmpVertex,MapWithSearch<Label,Label,CmpVertex>> entry:expectedResult.transitionMatrix.entrySet()) if (entry.getKey().getStringId().contains("A0")) entry.getValue().clear();
		Assert.assertNull(WMethod.checkM(expectedResult, gr));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void testAddRejectVertices0_fail1()
	{
		LearnerGraph gr=buildLearnerGraph("A00-a->B-a->C","testAddRejectVertices",configMain,converter);
		addRejectVertices(gr, "A0", 1);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void testAddRejectVertices0_fail2()
	{
		LearnerGraph gr=buildLearnerGraph("A00-a->B-a->C","testAddRejectVertices",configMain,converter);
		addRejectVertices(gr, "B", 0);
	}
	
	@Test
	public final void testAddRejectVertices1()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B-a->C","testAddRejectVertices",configMain,converter);
		addRejectVertices(gr, "QQ0", 4);
		LearnerGraph expectedResult = buildLearnerGraph("A-a->B-a->C\nQQ0-a->A\nQQ00-a->A\nQQ01-a->A\nQQ02-a->A\nQQ03-a->A\n","testAddRejectVertices_result",configMain,converter);
		for(Entry<CmpVertex,MapWithSearch<Label,Label,CmpVertex>> entry:expectedResult.transitionMatrix.entrySet()) if (entry.getKey().getStringId().contains("QQ0")) entry.getValue().clear();
		Assert.assertNull(WMethod.checkM(expectedResult, gr));
	}
	
	@Test
	public final void testAddRejectVertices2()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B-a->C","testAddRejectVertices",configMain,converter);
		addRejectVertices(gr, "QQ0", -10);
		Assert.assertEquals(gr.transitionMatrix.keySet(), buildLearnerGraph("A-a->B-a->C","testAddRejectVertices",configMain,converter).transitionMatrix.keySet());
	}
	
	@Test
	public final void testAddRejectVertices3()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->B-a->C","testAddRejectVertices",configMain,converter);
		addRejectVertices(gr, "QQ0", 1);
		LearnerGraph expectedResult = buildLearnerGraph("A-a->B-a->C\nQQ0-a->A\nQQ00-a->A","testAddRejectVertices_result3",configMain,converter);
		for(Entry<CmpVertex,MapWithSearch<Label,Label,CmpVertex>> entry:expectedResult.transitionMatrix.entrySet()) if (entry.getKey().getStringId().contains("QQ0")) entry.getValue().clear();
		Assert.assertNull(WMethod.checkM(expectedResult, gr));
	}

	/** Tests that conversion between numerical state pairs and back works. */
	@Test
	public final void testNumberToState_and_Back()
	{
		LearnerGraph gr=buildLearnerGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R\nC-f-#T\nC-e->G-a-#K\nG-b->S-a-#U","TestFindIncompatibleStatesB",configMain,converter);
		StatesToConsider filter = LearnerGraphND.ignoreRejectStates;
		GDLearnerGraph ndGraph = new GDLearnerGraph(gr,filter, false);
		for(CmpVertex A:gr.transitionMatrix.keySet())
			if (filter.stateToConsider(A))
				for(CmpVertex B:gr.transitionMatrix.keySet())
					if (filter.stateToConsider(B))
					{
						PairScore received1 = ndGraph.getPairScore(ndGraph.vertexToIntNR(A,B), 1, 2);
						Assert.assertTrue(
								received1.equals(new PairScore(A,B,1,2)) ||
								received1.equals(new PairScore(B,A,1,2))
						);
						
						PairScore received2 = ndGraph.getPairScore(ndGraph.vertexToIntNR(B,A), 1, 2);
						Assert.assertTrue(
								received2.equals(new PairScore(A,B,1,2)) ||
								received2.equals(new PairScore(B,A,1,2))
						);
					}
	}
	

}
