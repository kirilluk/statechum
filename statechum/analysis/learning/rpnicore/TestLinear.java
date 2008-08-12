/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

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

import static statechum.analysis.learning.TestFSMAlgo.buildGraph;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Test;

import cern.colt.bitvector.BitVector;
import cern.colt.function.IntComparator;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.LearnerGraph.StatesToConsider;
import statechum.analysis.learning.rpnicore.LearnerGraphND.DetermineDiagonalAndRightHandSide;
import statechum.analysis.learning.rpnicore.LearnerGraphND.HandleRow;

public class TestLinear {

	int cnt = 0;
	
	public final void benchmarkArrayTransformations()
	{
		int buf[] = new int[8];
		Random rnd = new Random(0);
		for(int i=0;i<buf.length;++i) buf[i]=buf.length-i;
		for(int reorder=0;reorder<buf.length/3;++reorder)
			buf[rnd.nextInt(buf.length)]=buf[rnd.nextInt(buf.length)];

		int tmpBuf[]=new int[buf.length];
		int stateNumber = 1000;
		int Ai[] = new int[stateNumber*(stateNumber+1)/2*buf.length];
		double Ax[] = new double[Ai.length];
		int pos=0;
		for(int tmp=0;tmp<stateNumber*(stateNumber+1)/2;++tmp)
		{
			System.arraycopy(buf, 0, tmpBuf, 0, buf.length);		
			cern.colt.Sorting.quickSort(tmpBuf, 0, buf.length, new IntComparator() {

				public int compare(int o1, int o2) {
					++cnt;
					return o1-o2;
				}});
			
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
	
	LearnerGraph grLoadDistribution=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7"),Configuration.getDefaultConfiguration());
	LearnerGraphND grLoadDistributionND = new LearnerGraphND(grLoadDistribution,LearnerGraphND.ignoreRejectStates, false);

	/** Tests how well the workload is distributed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWorkLoadDistribution0_1()
	{
		LearnerGraphND.partitionWorkLoadTriangular(0,grLoadDistributionND.getStateNumber());
	}

	/** Tests how well the workload is distributed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWorkLoadDistribution0_2a()
	{
		LearnerGraphND.partitionWorkLoadTriangular(-1,grLoadDistributionND.getStateNumber());
	}

	/** Tests how well the workload is distributed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWorkLoadDistribution0_2b()
	{
		LearnerGraphND.partitionWorkLoadLinear(-1,grLoadDistributionND.getStateNumber());
	}

	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution1()
	{
		Assert.assertArrayEquals(new int[]{0,4},LearnerGraphND.partitionWorkLoadTriangular(1,grLoadDistributionND.getStateNumber()));
		Assert.assertArrayEquals(new int[]{0,4},LearnerGraphND.partitionWorkLoadLinear(1,grLoadDistributionND.getStateNumber()));
	}

	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution2()
	{
		Assert.assertArrayEquals(new int[]{0,1,2,3,4},LearnerGraphND.partitionWorkLoadTriangular(4,grLoadDistributionND.getStateNumber()));
		Assert.assertArrayEquals(new int[]{0,1,2,3,4},LearnerGraphND.partitionWorkLoadLinear(4,grLoadDistributionND.getStateNumber()));
	}
	
	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution3()
	{
		Assert.assertArrayEquals(new int[]{0,2,4},LearnerGraphND.partitionWorkLoadTriangular(2,grLoadDistributionND.getStateNumber()));
		Assert.assertArrayEquals(new int[]{0,2,4},LearnerGraphND.partitionWorkLoadLinear(2,grLoadDistributionND.getStateNumber()));
	}
	
	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution4()
	{
		Assert.assertArrayEquals(new int[]{0,2,3,4},LearnerGraphND.partitionWorkLoadTriangular(3,grLoadDistributionND.getStateNumber()));
		Assert.assertArrayEquals(new int[]{0,1,2,4},LearnerGraphND.partitionWorkLoadLinear(3,grLoadDistributionND.getStateNumber()));
	}
	
	/** Tests the workload distribution. */
	@Test
	public final void testWorkLoadDistribution5()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7"),Configuration.getDefaultConfiguration());
		final int ThreadNumber=4;
		for(int i=0;i< ThreadNumber;++i) Transform.addToGraph(gr, grLoadDistribution,null);
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		Assert.assertArrayEquals(new int[]{0,9,14,17,20},LearnerGraphND.partitionWorkLoadTriangular(ThreadNumber,ndGraph.getStateNumber()));
		Assert.assertArrayEquals(new int[]{0,5,10,15,20},LearnerGraphND.partitionWorkLoadLinear(ThreadNumber,ndGraph.getStateNumber()));
	}
	
	/** Tests the workload distribution. */
	@Test
	public final void testPerformRowTasks_A_1()
	{
		LearnerGraph gr=new LearnerGraph(Configuration.getDefaultConfiguration());gr.init.setAccept(false);
		StatesToConsider filter = LearnerGraphND.ignoreRejectStates;
		LearnerGraphND ndGraph = new LearnerGraphND(gr,filter, false);
		int ThreadNumber=4;
		Assert.assertArrayEquals(new int[]{0,0,0,0,0},LearnerGraphND.partitionWorkLoadTriangular(ThreadNumber,ndGraph.getStateNumber()));
		Assert.assertArrayEquals(new int[]{0,0,0,0,0},LearnerGraphND.partitionWorkLoadLinear(ThreadNumber,ndGraph.getStateNumber()));

		final Map<Integer,Integer> threadToRowNumber = new TreeMap<Integer,Integer>();  
		
		List<HandleRow<List<CmpVertex>>> handlerList = new LinkedList<HandleRow<List<CmpVertex>>>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
			handlerList.add(new HandleRow<List<CmpVertex>>()
			{
				public void init(@SuppressWarnings("unused") int threadNo) {}
	
				public void handleEntry(@SuppressWarnings("unused") Entry<CmpVertex, Map<String, List<CmpVertex>>> entryA, int threadNo) 
				{
					Integer newValue = threadToRowNumber.get(threadNo);
					if (newValue == null)
					{
						newValue = new Integer(0);
					}
					threadToRowNumber.put(threadNo, newValue+1);
				}
				
			});
		LearnerGraphND.performRowTasks(handlerList, ThreadNumber, ndGraph.matrixForward, filter,
				LearnerGraphND.partitionWorkLoadTriangular(ThreadNumber, ndGraph.matrixForward.size()));
		Assert.assertEquals(0, threadToRowNumber.size());
		//Assert.assertEquals(1, threadToRowNumber.values().iterator().next().intValue());
	}
	
	/** Tests the workload distribution. */
	@Test
	public final void testPerformRowTasks_A_2()
	{
		LearnerGraph gr=new LearnerGraph(Configuration.getDefaultConfiguration());
		StatesToConsider filter = LearnerGraphND.ignoreRejectStates;
		LearnerGraphND ndGraph = new LearnerGraphND(gr,filter, false);
		int ThreadNumber=4;
		Assert.assertArrayEquals(new int[]{0,0,0,0,1},LearnerGraphND.partitionWorkLoadTriangular(ThreadNumber,ndGraph.getStateNumber()));

		final Map<Integer,Integer> threadToRowNumber = new TreeMap<Integer,Integer>();  
		
		List<HandleRow<List<CmpVertex>>> handlerList = new LinkedList<HandleRow<List<CmpVertex>>>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
			handlerList.add(new HandleRow<List<CmpVertex>>()
			{
				public void init(@SuppressWarnings("unused") int threadNo) {}
	
				public void handleEntry(@SuppressWarnings("unused") Entry<CmpVertex, Map<String, List<CmpVertex>>> entryA, int threadNo) 
				{
					Integer newValue = threadToRowNumber.get(threadNo);
					if (newValue == null)
					{
						newValue = new Integer(0);
					}
					threadToRowNumber.put(threadNo, newValue+1);
				}
				
			});
		LearnerGraphND.performRowTasks(handlerList, ThreadNumber,ndGraph.matrixForward,filter,
				LearnerGraphND.partitionWorkLoadTriangular(ThreadNumber, ndGraph.matrixForward.size()));
		Assert.assertEquals(1, threadToRowNumber.size());
		Assert.assertEquals(1, threadToRowNumber.values().iterator().next().intValue());
		
	}
	
	/** Tests the workload distribution. */
	@Test
	public final void testPerformRowTasks_A_3()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nA-c-#E\nB-b-#F\nB-c-#G","testPerformRowTasks_A_3"),Configuration.getDefaultConfiguration());
		StatesToConsider filter = LearnerGraphND.ignoreRejectStates;
		LearnerGraphND ndGraph = new LearnerGraphND(gr,filter, false);
		int ThreadNumber=4;


		final Map<Integer,Integer> threadToRowNumber = new TreeMap<Integer,Integer>();  
		
		List<HandleRow<List<CmpVertex>>> handlerList = new LinkedList<HandleRow<List<CmpVertex>>>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
			handlerList.add(new HandleRow<List<CmpVertex>>()
			{
				public void init(@SuppressWarnings("unused") int threadNo) {}
	
				public void handleEntry(@SuppressWarnings("unused") Entry<CmpVertex, Map<String, List<CmpVertex>>> entryA, int threadNo) 
				{
					Integer newValue = threadToRowNumber.get(threadNo);
					if (newValue == null)
					{
						newValue = new Integer(0);
					}
					threadToRowNumber.put(threadNo, newValue+1);
				}
				
			});
		Assert.assertArrayEquals(new int[]{0,0,1,1,2},LearnerGraphND.partitionWorkLoadTriangular(ThreadNumber,ndGraph.getStateNumber()));
		LearnerGraphND.performRowTasks(handlerList, ThreadNumber, ndGraph.matrixInverse,filter,
				LearnerGraphND.partitionWorkLoadTriangular(ThreadNumber, ndGraph.matrixInverse.size()));
		Assert.assertEquals(2, threadToRowNumber.size());
		int counterOfAllUsedRows=0;
		for(Integer numberOfRows:threadToRowNumber.values()) counterOfAllUsedRows+=numberOfRows;
		Assert.assertEquals(2, counterOfAllUsedRows);// 2 is the number of states which were not ignored
		
		threadToRowNumber.clear();
		Assert.assertArrayEquals(new int[]{0,3,4,6,7},LearnerGraphND.partitionWorkLoadTriangular(ThreadNumber,gr.getStateNumber()));
		LearnerGraphND.performRowTasks(handlerList, ThreadNumber, ndGraph.matrixForward,filter,
				LearnerGraphND.partitionWorkLoadTriangular(ThreadNumber, ndGraph.matrixForward.size()));
		Assert.assertEquals(1, threadToRowNumber.size());// only one thread gets to do anything because A and B are within its scope.
		counterOfAllUsedRows=0;
		for(Integer numberOfRows:threadToRowNumber.values()) counterOfAllUsedRows+=numberOfRows;
		Assert.assertEquals(2, counterOfAllUsedRows);// 2 is the number of states which were not ignored
	}
	
	/** Tests that the transition matrices are built correctly by LearnerGraphND */
	@Test
	public final void TestDomainCompatibility1()
	{
		checkConsideringIgnoredStates("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R\nC-f-#T","TestFindIncompatibleStatesB",
				LearnerGraphND.ignoreRejectStatesClass.class,new String[]{"A","Q","C","D"});
	}
	
	/** Tests that the transition matrices are built correctly by LearnerGraphND */
	@Test
	public final void TestDomainCompatibility2()
	{
		checkConsideringIgnoredStates("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R\nC-f-#T","TestFindIncompatibleStatesB",
				LearnerGraphND.ignoreNoneClass.class,new String[]{"A","Q","C","D","R","T"});
	}
	
	/** Tests that the transition matrices are built correctly by LearnerGraphND */
	@Test
	public final void TestDomainCompatibility3()
	{
		checkConsideringIgnoredStates("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R\nC-f-#T","TestFindIncompatibleStatesB",
				LearnerGraphND.ignoreZeroClass.class,new String[]{"A","C","D"});
	}
	
	/** A helper method to create an instance of a filter. The trouble with filters is that
	 * some need an instance of LearnerGraph and others do not. We hence try all of them.
	 * 
	 * @return an instance of filter.
	 */
	public static StatesToConsider createInstanceOfFilter(Class<? extends StatesToConsider> filterClass,LearnerGraph gr)
	{
		StatesToConsider filter = null;
		try {
			java.lang.reflect.Constructor<? extends StatesToConsider> constructor = filterClass.getConstructor(new Class []{});
			filter = constructor.newInstance(new Object[]{});
		} catch (Exception e) {
			// if we failed, filter stays at null, we'll try again.
		}
		
		if (filter == null)
			try {
				java.lang.reflect.Constructor<? extends StatesToConsider> constructor = filterClass.getConstructor(new Class []{LearnerGraph.class});
				filter = constructor.newInstance(new Object[]{gr});
			} catch (Exception e) {
				Assert.fail("failed to create an instance of a filter");
			}
		return filter;
	}
	
	/** Tests that main sets built for the supplied graph honour ignored states.
	 * 
	 * @param graph graph to consider
	 * @param graphName graph name
	 * @param filter which states to filter out
	 * @param expectedIgnored which states are expected to remain after filtering.
	 */
	private final void checkConsideringIgnoredStates(String graph, String graphName, Class<? extends StatesToConsider> filterClass, String [] expectedIgnored)
	{
		LearnerGraph gr=new LearnerGraph(buildGraph(graph,graphName),Configuration.getDefaultConfiguration());
		StatesToConsider filter = createInstanceOfFilter(filterClass, gr);
		for(boolean direction:new boolean[]{false,true})
		{
			LearnerGraphND ndGraph = new LearnerGraphND(gr,filter, direction);
			Map<CmpVertex,Integer> state_to_int_map = new TreeMap<CmpVertex,Integer>();
			CmpVertex [] numberToStateNoReject = gr.buildStateToIntegerMap(filter,state_to_int_map);
			Assert.assertEquals(state_to_int_map.size(), numberToStateNoReject.length);
			Collection<CmpVertex> states_int = state_to_int_map.keySet();
			Assert.assertEquals(ndGraph.getStateNumber(),state_to_int_map.values().size());
			Assert.assertArrayEquals(states_int.toArray(),numberToStateNoReject);
			
			Set<CmpVertex> expectedIgnoredStates = new TreeSet<CmpVertex>();for(String st:expectedIgnored) expectedIgnoredStates.add(gr.findVertex(st));
			Assert.assertEquals(states_int,expectedIgnoredStates);
			
			// The forward matrix is not filtered
			checkSource_Target_are_expected(ndGraph.matrixForward,gr.transitionMatrix.keySet());
			checkSource_Target_are_expected(ndGraph.matrixInverse,expectedIgnoredStates);
		}
	}
	
	private final void checkSource_Target_are_expected(Map<CmpVertex,Map<String,List<CmpVertex>>> data,
			Set<CmpVertex> expected)
	{
		Assert.assertEquals("source states mismatch",expected, data.keySet());
		Set<CmpVertex> targetSet = new HashSet<CmpVertex>();
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:data.entrySet())
			for(List<CmpVertex> list:entry.getValue().values())
				targetSet.addAll(list);
		// target states are supposed to be a subset of the main set of states, hence a subset of the expected states
		targetSet.removeAll(expected);
		Assert.assertTrue("target states mismatch",targetSet.isEmpty());
	}
	
	public static final int PAIR_INCOMPATIBLE=LearnerGraphND.PAIR_INCOMPATIBLE, PAIR_OK=LearnerGraphND.PAIR_OK;
	
	@Test
	public final void testNumberNonNegativeElementsA()
	{
		int [] arrayA=new int[]{PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE};
		Assert.assertEquals(0,LearnerGraphND.numberNonNegativeElements(arrayA));
		Assert.assertArrayEquals(new int[]{PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE},arrayA);
	}
	@Test
	public final void testNumberNonNegativeElementsB()
	{
		int [] arrayB=new int[]{PAIR_INCOMPATIBLE,6,7,PAIR_INCOMPATIBLE,8};
		Assert.assertEquals(0,LearnerGraphND.numberNonNegativeElements(arrayB));
		Assert.assertArrayEquals(new int[]{PAIR_INCOMPATIBLE,6,7,PAIR_INCOMPATIBLE,8},arrayB);
	}

	@Test
	public final void testNumberNonNegativeElementsC()
	{
		int [] arrayC=new int[]{PAIR_INCOMPATIBLE,PAIR_OK,PAIR_OK,PAIR_INCOMPATIBLE,PAIR_OK};
		Assert.assertEquals(3,LearnerGraphND.numberNonNegativeElements(arrayC));
		Assert.assertArrayEquals(new int[]{PAIR_INCOMPATIBLE,0,1,PAIR_INCOMPATIBLE,2},arrayC);
	}
	
	@Test
	public final void testNumberNonNegativeElementsD()
	{
		int [] arrayD=new int[]{PAIR_INCOMPATIBLE,6,PAIR_OK,PAIR_INCOMPATIBLE,8,PAIR_INCOMPATIBLE};
		Assert.assertEquals(1,LearnerGraphND.numberNonNegativeElements(arrayD));
		Assert.assertArrayEquals(new int[]{PAIR_INCOMPATIBLE,6,0,PAIR_INCOMPATIBLE,8,PAIR_INCOMPATIBLE},arrayD);
	}
	
	@Test
	public final void testIntersects()
	{
		BitVector A=new BitVector(100),B=new BitVector(100);
		Assert.assertFalse(LearnerGraphND.intersects(A, B));
		A.set(10);
		Assert.assertFalse(LearnerGraphND.intersects(A, B));
		B.set(10);
		Assert.assertTrue(LearnerGraphND.intersects(A, B));
		A.clear(10);
		A.set(71);A.set(72);B.set(72);
		Assert.assertTrue(LearnerGraphND.intersects(A, B));
		A.clear(72);
		Assert.assertFalse(LearnerGraphND.intersects(A, B));
	}
	
	/** A helper to call matches on both permutations of states to check if the result is the same.
	 * 
	 * @param gr graph to consider
	 * @param matrixND the matrix to call a matcher on
	 * @param matcher matcher to use
	 * @param A name of the first state
	 * @param B name of the second state
	 */
	private final void getMatcherValue(LearnerGraph gr,Map<CmpVertex,Map<String,List<CmpVertex>>> matrixND, DetermineDiagonalAndRightHandSide matcher, String A,String B)
	{
		matcher.compute(matrixND.get(gr.findVertex(A)),matrixND.get(gr.findVertex(B)));
		int rightHand = matcher.getRightHandSide(), diag = matcher.getDiagonal();
		// Now check that matcher is stateless by computing the same in the reverse order.
		matcher.compute(matrixND.get(gr.findVertex(B)),matrixND.get(gr.findVertex(A)));
		Assert.assertEquals("right-hand side",matcher.getRightHandSide(),rightHand);
		Assert.assertEquals("right-hand side",matcher.getDiagonal(),diag);
		
		
		// Now copy the matcher and do another computation, with A and B reversed.
		DetermineDiagonalAndRightHandSide anotherMather = null;
		try {
			anotherMather = matcher.getClass().newInstance();
		} catch (Exception e) {
			Assert.fail("Unexpected exception cloning a matcher: "+e);
		}
		anotherMather.compute(matrixND.get(gr.findVertex(B)),matrixND.get(gr.findVertex(A)));
		Assert.assertEquals("right-hand side",anotherMather.getRightHandSide(),rightHand);
		Assert.assertEquals("right-hand side",anotherMather.getDiagonal(),diag);
	}
	
	@Test
	public final void testCountMatchingOutgoing1()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B\nA-b->B\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), Configuration.getDefaultConfiguration());
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_default(); 
		getMatcherValue(gr,ndGraph.matrixForward, matcher,"A","Q");
		Assert.assertEquals(2,matcher.getRightHandSide());
	}

	@Test
	public final void testCountMatchingOutgoing2()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B\nA-b->B\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), Configuration.getDefaultConfiguration());
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_default(); 
		getMatcherValue(gr,ndGraph.matrixForward, matcher,"A","A");
		Assert.assertEquals(3,matcher.getRightHandSide());
	}

	@Test
	public final void testCountMatchingOutgoing3a()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b->B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing3a"), Configuration.getDefaultConfiguration());
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_default(); 
		getMatcherValue(gr,ndGraph.matrixForward, matcher ,"A","Q");
		Assert.assertEquals(matcher.getDiagonal()*PAIR_INCOMPATIBLE,matcher.getRightHandSide());
	}

	@Test
	public final void testCountMatchingOutgoing3b()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b->B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing3b"), config);
		gr.linear.moveRejectToHighlight();
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_highlight();
		getMatcherValue(gr,ndGraph.matrixForward, matcher,"A","Q");
		Assert.assertEquals(1,matcher.getRightHandSide());
	}

	@Test
	public final void testCountMatchingOutgoing4a()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b-#B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), Configuration.getDefaultConfiguration());
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_default();
		getMatcherValue(gr,ndGraph.matrixForward, matcher,"A","Q");
		Assert.assertEquals(matcher.getDiagonal()*PAIR_INCOMPATIBLE,matcher.getRightHandSide());
	}

	@Test
	public final void testCountMatchingOutgoing4b()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b-#B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), config);
		gr.linear.moveRejectToHighlight();
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_highlight();
		getMatcherValue(gr,ndGraph.matrixForward, matcher,"A","Q");
		Assert.assertEquals(0,matcher.getRightHandSide());
	}

	@Test
	public final void testCountMatchingOutgoing5a()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b-#B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), Configuration.getDefaultConfiguration());
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_default();
		getMatcherValue(gr,ndGraph.matrixForward, matcher,"A","C");
		Assert.assertEquals(0,matcher.getRightHandSide());
		getMatcherValue(gr,ndGraph.matrixForward, matcher,"S","C");
		Assert.assertEquals(0,matcher.getRightHandSide());
	}

	@Test
	public final void testCountMatchingOutgoing5b()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b-#B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), config);
		gr.linear.moveRejectToHighlight();
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_highlight();
		getMatcherValue(gr,ndGraph.matrixForward, matcher,"A","C");
		Assert.assertEquals(0,matcher.getRightHandSide());
		getMatcherValue(gr,ndGraph.matrixForward, matcher,"S","C");
		Assert.assertEquals(0,matcher.getRightHandSide());
	}


	@Test
	public final void testCountMatchingOutgoing6a()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b-#B1\nQ-a->R", "testCountMatchingOutgoing1"), Configuration.getDefaultConfiguration());
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_default();
		getMatcherValue(gr,ndGraph.matrixForward, matcher,"A","R");
		Assert.assertEquals(0,matcher.getRightHandSide());
	}
	
	@Test
	public final void testCountMatchingOutgoing6b()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b-#B1\nQ-a->R", "testCountMatchingOutgoing1"), Configuration.getDefaultConfiguration());
		gr.linear.moveRejectToHighlight();
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_highlight();
		getMatcherValue(gr,ndGraph.matrixForward, matcher,"A","R");
		Assert.assertEquals(0,matcher.getRightHandSide());
	}
	
	/** These are tests how the matcher handles a case of non-deterministism. 
	 * Accomplished by building a normal graph and then running the matcher
	 * on the inverse of it (I cannot build a non-determinstic graph directly
	 * because LearnerGraph has to be deterministic and I do not wish to
	 * manually build the non-determinstic transition matrix). 
	 */
	@Test
	public final void testCountMatchingOutgoing_nd1()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A1-a->C\nA2-a->C\nA3-a->C<-b-G\nB1-a->D<-a-B2\nE-b->D<-b-F", "testCountMatchingOutgoing_nd1"), Configuration.getDefaultConfiguration());
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_default();
		getMatcherValue(gr,ndGraph.matrixInverse, matcher,"C","D");
		Assert.assertEquals(8,matcher.getRightHandSide());
		Assert.assertEquals(8,matcher.getDiagonal());
	}
	
	@Test
	public final void testCountMatchingOutgoing_nd2a()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A1-a->C\nA2-a->C\nA3-a->C<-b-G\nB1-a->D<-a-B2\nE-b->D<-b-F\n"
				+"N-c->C", "testCountMatchingOutgoing_nd2a"), Configuration.getDefaultConfiguration());
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_default();
		getMatcherValue(gr,ndGraph.matrixInverse, matcher,"C","D");
		Assert.assertEquals(8,matcher.getRightHandSide());
		Assert.assertEquals(9,matcher.getDiagonal());
	}
	
	@Test
	public final void testCountMatchingOutgoing_nd2b()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A1-a->C\nA2-a->C\nA3-a->C<-b-G\nB1-a->D<-a-B2\nE-b->D<-b-F\n"
				+"N-c->C\n"
				+"M-d->D"
				, "testCountMatchingOutgoing_nd2b"), Configuration.getDefaultConfiguration());
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_default();
		getMatcherValue(gr,ndGraph.matrixInverse, matcher,"C","D");
		Assert.assertEquals(8,matcher.getRightHandSide());
		Assert.assertEquals(10,matcher.getDiagonal());
	}
	
	@Test
	public final void testCountMatchingOutgoing_nd3a()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A1-a->C\nA2-a->C\nA3-a->C<-b-G\nB1-a->D<-a-B2\nE-b->D<-b-F\n"
				+"N-c->C\n"
				+"N-a->C", "testCountMatchingOutgoing_nd3a"), Configuration.getDefaultConfiguration());
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_default();
		getMatcherValue(gr,ndGraph.matrixInverse, matcher,"C","D");
		Assert.assertEquals(10,matcher.getRightHandSide());
		Assert.assertEquals(11,matcher.getDiagonal());
	}
	
	@Test
	public final void testCountMatchingOutgoing_nd3b()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A1-a->C\nA2-a->C\nA3-a->C<-b-G\nB1-a->D<-a-B2\nE-b->D<-b-F\n"
				+"N-c->C<-f-U\n"
				+"N-a->C\nS-r->D", "testCountMatchingOutgoing_nd3b"), Configuration.getDefaultConfiguration());
		LearnerGraphND ndGraph = new LearnerGraphND(gr,LearnerGraphND.ignoreRejectStates, false);
		DetermineDiagonalAndRightHandSide matcher = new LearnerGraphND.DDRH_default();
		getMatcherValue(gr,ndGraph.matrixInverse, matcher,"C","D");
		Assert.assertEquals(10,matcher.getRightHandSide());
		Assert.assertEquals(13,matcher.getDiagonal());
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
			VertexID id = new VertexID(prefix+(i>=0?i:""));if (gr.findVertex(id) != null) throw new IllegalArgumentException("vertex already exists");
			CmpVertex newVertex = LearnerGraph.generateNewCmpVertex(id, gr.config);newVertex.setAccept(false);
			gr.transitionMatrix.put(newVertex, new TreeMap<String,CmpVertex>());
		}
	}
	
	@Test
	public final void testAddRejectVertices0()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A00-a->B-a->C","testAddRejectVertices"),Configuration.getDefaultConfiguration());
		addRejectVertices(gr, "A0", -1);
		LearnerGraph expectedResult = new LearnerGraph(buildGraph("A-a->B-a->C\nA0-a->A","testAddRejectVertices_result"),Configuration.getDefaultConfiguration());
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:expectedResult.transitionMatrix.entrySet()) if (entry.getKey().getID().toString().contains("A0")) entry.getValue().clear();
		WMethod.checkM(expectedResult, gr);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void testAddRejectVertices0_fail1()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A00-a->B-a->C","testAddRejectVertices"),Configuration.getDefaultConfiguration());
		addRejectVertices(gr, "A0", 1);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void testAddRejectVertices0_fail2()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A00-a->B-a->C","testAddRejectVertices"),Configuration.getDefaultConfiguration());
		addRejectVertices(gr, "B", 0);
	}
	
	@Test
	public final void testAddRejectVertices1()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a->C","testAddRejectVertices"),Configuration.getDefaultConfiguration());
		addRejectVertices(gr, "QQ0", 4);
		LearnerGraph expectedResult = new LearnerGraph(buildGraph("A-a->B-a->C\nQQ0-a->A\nQQ00-a->A\nQQ01-a->A\nQQ02-a->A\nQQ03-a->A\n","testAddRejectVertices_result"),Configuration.getDefaultConfiguration());
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:expectedResult.transitionMatrix.entrySet()) if (entry.getKey().getID().toString().contains("QQ0")) entry.getValue().clear();
		WMethod.checkM(expectedResult, gr);
	}
	
	@Test
	public final void testAddRejectVertices2()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a->C","testAddRejectVertices"),Configuration.getDefaultConfiguration());
		addRejectVertices(gr, "QQ0", -10);
		Assert.assertEquals(gr.transitionMatrix.keySet(), new LearnerGraph(buildGraph("A-a->B-a->C","testAddRejectVertices"),Configuration.getDefaultConfiguration()).transitionMatrix.keySet());
	}
	
	@Test
	public final void testAddRejectVertices3()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a->C","testAddRejectVertices"),Configuration.getDefaultConfiguration());
		addRejectVertices(gr, "QQ0", 1);
		LearnerGraph expectedResult = new LearnerGraph(buildGraph("A-a->B-a->C\nQQ0-a->A\nQQ00-a->A","testAddRejectVertices_result3"),Configuration.getDefaultConfiguration());
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:expectedResult.transitionMatrix.entrySet()) if (entry.getKey().getID().toString().contains("QQ0")) entry.getValue().clear();
		WMethod.checkM(expectedResult, gr);
	}

	/** Tests that conversion between numerical state pairs and back works. */
	@Test
	public final void testNumberToState_and_Back()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R\nC-f-#T\nC-e->G-a-#K\nG-b->S-a-#U","TestFindIncompatibleStatesB"),Configuration.getDefaultConfiguration());
		StatesToConsider filter = LearnerGraphND.ignoreRejectStates;
		LearnerGraphND ndGraph = new LearnerGraphND(gr,filter, false);
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
