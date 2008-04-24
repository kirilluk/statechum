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
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Assert;
import org.junit.Test;

import cern.colt.bitvector.BitVector;
import cern.colt.function.IntComparator;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
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
				public void init(int threadNo) {}
	
				public void handleEntry(Entry<CmpVertex, Map<String, CmpVertex>> entryA, int threadNo) 
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
		System.out.println(gr.transitionMatrix.keySet());
		Assert.assertArrayEquals(new int[]{0,1,1,1,7},gr.linear.partitionWorkLoad(ThreadNumber));

		final Map<Integer,Integer> threadToRowNumber = new TreeMap<Integer,Integer>();  
		
		List<HandleRow> handlerList = new LinkedList<HandleRow>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
			handlerList.add(new HandleRow()
			{
				public void init(int threadNo) {}
	
				public void handleEntry(Entry<CmpVertex, Map<String, CmpVertex>> entryA, int threadNo) 
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
		int cnt=0;
		for(Integer numberOfRows:threadToRowNumber.values()) cnt+=numberOfRows;
		Assert.assertEquals(gr.getStateNumber(), cnt);
		
	}
	
	/** Tests that if all pairs are not compatible, this is preserved. */
	@Test
	public final void TestDomainCompatibility()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R\nC-f-#T","TestFindIncompatibleStatesB"),Configuration.getDefaultConfiguration());
		Collection<CmpVertex> states = gr.transitionMatrix.keySet();
		Collection<CmpVertex> states_int = gr.learnerCache.getStateToNumber().keySet();Assert.assertEquals(states, states_int);
		Collection<CmpVertex> states_sorta = gr.learnerCache.getSortaInverse().keySet();Assert.assertEquals(states, states_sorta);
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
}
