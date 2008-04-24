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
import java.util.TreeMap;
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
import statechum.analysis.learning.rpnicore.Linear.HandleRow;

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

	/** Tests how well the workload is distributed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWorkLoadDistribution0_1()
	{
		grLoadDistribution.linear.partitionWorkLoad(0);
	}

	/** Tests how well the workload is distributed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWorkLoadDistribution0_2()
	{
		grLoadDistribution.linear.partitionWorkLoad(-1);
	}

	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution1()
	{
		Assert.assertArrayEquals(new int[]{0,4},grLoadDistribution.linear.partitionWorkLoad(1));
	}

	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution2()
	{
		Assert.assertArrayEquals(new int[]{0,2,3,4,4},grLoadDistribution.linear.partitionWorkLoad(4));
	}
	
	/** Tests the workload distribution. */
	@Test
	public final void testWorkLoadDistribution3()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7"),Configuration.getDefaultConfiguration());
		for(int i=0;i< 4;++i) Transform.addToGraph(gr, grLoadDistribution);
		Assert.assertArrayEquals(new int[]{0,10,14,17,20},gr.linear.partitionWorkLoad(4));
	}
	
	/** Tests the workload distribution. */
	@Test
	public final void testWorkLoadDistribution_A_1()
	{
		LearnerGraph gr=new LearnerGraph(Configuration.getDefaultConfiguration());gr.init.setAccept(false);
		int ThreadNumber=4;
		Assert.assertArrayEquals(new int[]{0,0,0,0,1},gr.linear.partitionWorkLoad(ThreadNumber));

		final Map<Integer,Integer> threadToRowNumber = new TreeMap<Integer,Integer>();  
		
		List<HandleRow> handlerList = new LinkedList<HandleRow>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
			handlerList.add(new HandleRow()
			{
				public void init(@SuppressWarnings("unused") int threadNo) {}
	
				public void handleEntry(@SuppressWarnings("unused") Entry<CmpVertex, Map<String, CmpVertex>> entryA, int threadNo) 
				{
					Integer newValue = threadToRowNumber.get(threadNo);
					if (newValue == null)
					{
						newValue = new Integer(0);
					}
					threadToRowNumber.put(threadNo, newValue+1);
				}
				
			});
		gr.linear.performRowTasks(handlerList, ThreadNumber);
		Assert.assertEquals(1, threadToRowNumber.size());
		Assert.assertEquals(1, threadToRowNumber.values().iterator().next().intValue());
		
	}
	
	/** Tests the workload distribution. */
	@Test
	public final void testWorkLoadDistribution_A_2()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nA-c-#E\nB-b-#F\nB-c-#G","testAddToBuffer7"),Configuration.getDefaultConfiguration());
		int ThreadNumber=4;

		Assert.assertArrayEquals(new int[]{0,1,1,1,7},gr.linear.partitionWorkLoad(ThreadNumber));

		final Map<Integer,Integer> threadToRowNumber = new TreeMap<Integer,Integer>();  
		
		List<HandleRow> handlerList = new LinkedList<HandleRow>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
			handlerList.add(new HandleRow()
			{
				public void init(@SuppressWarnings("unused") int threadNo) {}
	
				public void handleEntry(@SuppressWarnings("unused") Entry<CmpVertex, Map<String, CmpVertex>> entryA, int threadNo) 
				{
					Integer newValue = threadToRowNumber.get(threadNo);
					if (newValue == null)
					{
						newValue = new Integer(0);
					}
					threadToRowNumber.put(threadNo, newValue+1);
				}
				
			});
		gr.linear.performRowTasks(handlerList, ThreadNumber);
		Assert.assertEquals(2, threadToRowNumber.size());
		int counterOfAllUsedRows=0;
		for(Integer numberOfRows:threadToRowNumber.values()) counterOfAllUsedRows+=numberOfRows;
		Assert.assertEquals(gr.getStateNumber(), counterOfAllUsedRows);
		
	}
	
	/** Tests that if all pairs are not compatible, this is preserved. */
	@Test
	public final void TestDomainCompatibility()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R\nC-f-#T","TestFindIncompatibleStatesB"),Configuration.getDefaultConfiguration());
		CmpVertex [] numberToStateNoReject = new CmpVertex[gr.learnerCache.getAcceptStateNumber()];
		Map<CmpVertex,Integer> state_to_int_map = gr.wmethod.buildStateToIntegerMap(false,numberToStateNoReject);
		Collection<CmpVertex> states_int = state_to_int_map.keySet();
		Assert.assertEquals(gr.learnerCache.getAcceptStateNumber(),state_to_int_map.values().size());
		Assert.assertArrayEquals(states_int.toArray(),numberToStateNoReject);
		Collection<CmpVertex> states_sorta = new HashSet<CmpVertex>();states_sorta.addAll(gr.learnerCache.getSortaInverse().keySet());
		states_sorta.removeAll(states_int);Assert.assertTrue(states_sorta.isEmpty());// verify that states_sorta does not contain more source states than we expect
		states_sorta.clear();
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:gr.learnerCache.getSortaInverse().entrySet())
			for(List<CmpVertex> list:entry.getValue().values())
				states_sorta.addAll(list);
		states_sorta.removeAll(states_int);Assert.assertTrue(states_sorta.isEmpty());// verify that states_sorta does not contain more target states than we expect
	}
	
	public static final int PAIR_INCOMPATIBLE=Linear.PAIR_INCOMPATIBLE, PAIR_OK=Linear.PAIR_OK;
	
	@Test
	public final void testNumberNonNegativeElementsA()
	{
		int [] arrayA=new int[]{PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE};
		Assert.assertEquals(0,Linear.numberNonNegativeElements(arrayA));
		Assert.assertArrayEquals(new int[]{PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE},arrayA);
	}
	@Test
	public final void testNumberNonNegativeElementsB()
	{
		int [] arrayB=new int[]{PAIR_INCOMPATIBLE,6,7,PAIR_INCOMPATIBLE,8};
		Assert.assertEquals(0,Linear.numberNonNegativeElements(arrayB));
		Assert.assertArrayEquals(new int[]{PAIR_INCOMPATIBLE,6,7,PAIR_INCOMPATIBLE,8},arrayB);
	}

	@Test
	public final void testNumberNonNegativeElementsC()
	{
		int [] arrayC=new int[]{PAIR_INCOMPATIBLE,PAIR_OK,PAIR_OK,PAIR_INCOMPATIBLE,PAIR_OK};
		Assert.assertEquals(3,Linear.numberNonNegativeElements(arrayC));
		Assert.assertArrayEquals(new int[]{PAIR_INCOMPATIBLE,0,1,PAIR_INCOMPATIBLE,2},arrayC);
	}
	
	@Test
	public final void testNumberNonNegativeElementsD()
	{
		int [] arrayD=new int[]{PAIR_INCOMPATIBLE,6,PAIR_OK,PAIR_INCOMPATIBLE,8,PAIR_INCOMPATIBLE};
		Assert.assertEquals(1,Linear.numberNonNegativeElements(arrayD));
		Assert.assertArrayEquals(new int[]{PAIR_INCOMPATIBLE,6,0,PAIR_INCOMPATIBLE,8,PAIR_INCOMPATIBLE},arrayD);
	}
	
	@Test
	public final void testIntersects()
	{
		BitVector A=new BitVector(100),B=new BitVector(100);
		Assert.assertFalse(Linear.intersects(A, B));
		A.set(10);
		Assert.assertFalse(Linear.intersects(A, B));
		B.set(10);
		Assert.assertTrue(Linear.intersects(A, B));
		A.clear(10);
		A.set(71);A.set(72);B.set(72);
		Assert.assertTrue(Linear.intersects(A, B));
		A.clear(72);
		Assert.assertFalse(Linear.intersects(A, B));
	}
	
	@Test
	public final void testCountMatchingOutgoing1()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B\nA-b->B\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), Configuration.getDefaultConfiguration());
		Assert.assertEquals(2,gr.linear.countMatchingOutgoing(gr.transitionMatrix.get(gr.findVertex("A")),gr.transitionMatrix.get(gr.findVertex("Q"))));
	}

	@Test
	public final void testCountMatchingOutgoing2()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B\nA-b->B\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), Configuration.getDefaultConfiguration());
		Assert.assertEquals(3,gr.linear.countMatchingOutgoing(gr.transitionMatrix.get(gr.findVertex("A")),gr.transitionMatrix.get(gr.findVertex("A"))));
	}

	@Test
	public final void testCountMatchingOutgoing3a()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b->B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), Configuration.getDefaultConfiguration());
		Assert.assertEquals(1,gr.linear.countMatchingOutgoing(gr.transitionMatrix.get(gr.findVertex("A")),gr.transitionMatrix.get(gr.findVertex("Q"))));
	}

	@Test
	public final void testCountMatchingOutgoing3b()
	{
		Configuration config = (Configuration)Configuration.getDefaultConfiguration().clone();config.setLinearPairScoreInterpretHighlightAsNegative(true);
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b->B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), config);
		gr.linear.highlightNegatives();
		Assert.assertEquals(-1,gr.linear.countMatchingOutgoing(gr.transitionMatrix.get(gr.findVertex("A")),gr.transitionMatrix.get(gr.findVertex("Q"))));
	}

	@Test
	public final void testCountMatchingOutgoing4a()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b-#B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), Configuration.getDefaultConfiguration());
		Assert.assertEquals(0,gr.linear.countMatchingOutgoing(gr.transitionMatrix.get(gr.findVertex("A")),gr.transitionMatrix.get(gr.findVertex("Q"))));
	}

	@Test
	public final void testCountMatchingOutgoing4b()
	{
		Configuration config = (Configuration)Configuration.getDefaultConfiguration().clone();config.setLinearPairScoreInterpretHighlightAsNegative(true);
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b-#B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), config);
		gr.linear.highlightNegatives();
		Assert.assertEquals(-1,gr.linear.countMatchingOutgoing(gr.transitionMatrix.get(gr.findVertex("A")),gr.transitionMatrix.get(gr.findVertex("Q"))));
	}

	@Test
	public final void testCountMatchingOutgoing5a()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b-#B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), Configuration.getDefaultConfiguration());
		Assert.assertEquals(0,gr.linear.countMatchingOutgoing(gr.transitionMatrix.get(gr.findVertex("A")),gr.transitionMatrix.get(gr.findVertex("C"))));
		Assert.assertEquals(0,gr.linear.countMatchingOutgoing(gr.transitionMatrix.get(gr.findVertex("S")),gr.transitionMatrix.get(gr.findVertex("C"))));
	}

	@Test
	public final void testCountMatchingOutgoing5b()
	{
		Configuration config = (Configuration)Configuration.getDefaultConfiguration().clone();config.setLinearPairScoreInterpretHighlightAsNegative(true);
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b-#B1\nA-c->C\nQ-a->R\nQ-b->S", "testCountMatchingOutgoing1"), config);
		gr.linear.highlightNegatives();
		Assert.assertEquals(0,gr.linear.countMatchingOutgoing(gr.transitionMatrix.get(gr.findVertex("A")),gr.transitionMatrix.get(gr.findVertex("C"))));
		Assert.assertEquals(0,gr.linear.countMatchingOutgoing(gr.transitionMatrix.get(gr.findVertex("S")),gr.transitionMatrix.get(gr.findVertex("C"))));
		Assert.assertEquals(0,gr.linear.countMatchingOutgoing(gr.transitionMatrix.get(gr.findVertex("C")),gr.transitionMatrix.get(gr.findVertex("A"))));
		Assert.assertEquals(0,gr.linear.countMatchingOutgoing(gr.transitionMatrix.get(gr.findVertex("C")),gr.transitionMatrix.get(gr.findVertex("S"))));
	}


	@Test
	public final void testCountMatchingOutgoing6()
	{
		LearnerGraph gr=new LearnerGraph(TestFSMAlgo.buildGraph("A-a-#B\nA-b-#B1\nQ-a->R", "testCountMatchingOutgoing1"), Configuration.getDefaultConfiguration());
		Assert.assertEquals(0,gr.linear.countMatchingOutgoing(gr.transitionMatrix.get(gr.findVertex("A")),gr.transitionMatrix.get(gr.findVertex("R"))));
	}
	
	
	/** Adds reject vertices with names starting with a given prefix, and 
	 * the suffix sequentially increasing 0..number.
	 * The first reject vertex is just the prefix.
	 * 
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
		for(CmpVertex A:gr.transitionMatrix.keySet())
			if (A.isAccept())
				for(CmpVertex B:gr.transitionMatrix.keySet())
					if (B.isAccept())
					{
						PairScore received1 = gr.linear.getPairScore(gr.wmethod.vertexToIntNR(A,B), 1, 2);
						Assert.assertTrue(
								received1.equals(new PairScore(A,B,1,2)) ||
								received1.equals(new PairScore(B,A,1,2))
						);
						
						PairScore received2 = gr.linear.getPairScore(gr.wmethod.vertexToIntNR(B,A), 1, 2);
						Assert.assertTrue(
								received2.equals(new PairScore(A,B,1,2)) ||
								received2.equals(new PairScore(B,A,1,2))
						);
					}
	}
	

}
