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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.Linear.HandleRow;
import cern.colt.matrix.DoubleFactory1D;
import cern.colt.matrix.DoubleFactory2D;
import cern.colt.matrix.DoubleMatrix1D;
import cern.colt.matrix.DoubleMatrix2D;

@RunWith(Parameterized.class)
public class TestLinearWithMultipleThreads {
	final int ThreadNumber;
	final double k=Configuration.getDefaultConfiguration().getAttenuationK();
	
	public TestLinearWithMultipleThreads(int th)
	{
		ThreadNumber = th;
	}
	
	@Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		for(int i=1;i<8;++i)
			result.add(new Object[]{new Integer(i)});
		
		return result;
	}
	
	protected DoubleMatrix1D getExpectedMatrix1DSlowly(LearnerGraph gr)
	{
		int size=gr.getStateNumber()*(gr.getStateNumber()+1)/2;
		DoubleMatrix1D result = DoubleFactory1D.dense.make(size);
		
		for(Entry<CmpVertex,Map<String,CmpVertex>> entryA:gr.transitionMatrix.entrySet())
		{
			// Now iterate through states
			Iterator<Entry<CmpVertex,Map<String,CmpVertex>>> stateB_It = gr.transitionMatrix.entrySet().iterator();
			while(stateB_It.hasNext())
			{
				Entry<CmpVertex,Map<String,CmpVertex>> stateB = stateB_It.next();

				int currentStatePair = gr.wmethod.vertexToInt(stateB.getKey(),entryA.getKey());
				result.setQuick(currentStatePair, gr.linear.countMatchingOutgoing(entryA.getValue(),stateB.getValue()));
				if (stateB.getKey().equals(entryA.getKey())) break; // we only process a triangular subset.
			}
		}
		
		return result;
	}

	protected DoubleMatrix2D getExpectedMatrix2DSlowly(LearnerGraph gr)
	{
		int size=gr.getStateNumber()*(gr.getStateNumber()+1)/2;
		DoubleMatrix2D result = DoubleFactory2D.sparse.make(size,size,0);
		for(Entry<CmpVertex,Map<String,CmpVertex>> entryA:gr.transitionMatrix.entrySet())
		{
			// Now iterate through states
			Iterator<Entry<CmpVertex,Map<String,CmpVertex>>> stateB_It = gr.transitionMatrix.entrySet().iterator();
			while(stateB_It.hasNext())
			{
				Entry<CmpVertex,Map<String,CmpVertex>> stateB = stateB_It.next();

				int currentStatePair = gr.wmethod.vertexToInt(entryA.getKey(),stateB.getKey());
				
				int outgoingMatched = 0;
				for(Entry<String,CmpVertex> targetsA:entryA.getValue().entrySet())
				{
					CmpVertex toB = stateB.getValue().get(targetsA.getKey());
					if (toB != null)
					{
						++outgoingMatched;
						int targetStatePair = gr.wmethod.vertexToInt(targetsA.getValue(), toB);
						result.setQuick(currentStatePair,targetStatePair,result.getQuick(currentStatePair, targetStatePair)-gr.config.getAttenuationK());
					}
				}
				int totalOutgoing = entryA.getValue().size()+gr.transitionMatrix.get(stateB.getKey()).size()-outgoingMatched;
				if (totalOutgoing == 0) totalOutgoing = 1;// if neither element of a pair of states has an outgoing transition, force the identity to ensure that the solution will be zero. 
				result.setQuick(currentStatePair,currentStatePair,result.getQuick(currentStatePair, currentStatePair)+totalOutgoing);
				
				if (stateB.getKey().equals(entryA.getKey())) break; // we only process a triangular subset.
			}
		}
		
		return result;
	}
	
	
	/** Tests matrix construction for a supplied graph and matrix builder. */
	protected void checkBuildMatrix(LearnerGraph gr, DoubleMatrix2D expectedAx, DoubleMatrix1D expectedB)
	{
		LSolver solver = gr.linear.buildMatrix(ThreadNumber);
		DoubleMatrix2D Ax=solver.toDoubleMatrix2D();
		if (expectedAx != null) Assert.assertEquals(expectedAx, Ax);Assert.assertEquals(getExpectedMatrix2DSlowly(gr),Ax);
		DoubleMatrix1D b=solver.toDoubleMatrix1D();
		if (expectedB != null) Assert.assertEquals(expectedB, b);Assert.assertEquals(getExpectedMatrix1DSlowly(gr),b);
		solver.solveExternally();// check if we have a solution, just in case it fails.
	/*
		// Now check consistency.
		gr.config.setAttenuationK_testOnly(1);DoubleMatrix2D Ax1 = gr.linear.buildMatrix(ThreadNumber).toDoubleMatrix2D();
		gr.config.setAttenuationK(0);DoubleMatrix2D Ax0 = gr.linear.buildMatrix(ThreadNumber).toDoubleMatrix2D();
		DoubleMatrix1D one = DoubleFactory1D.dense.make(Ax1.rows(), 1), a=DoubleFactory1D.dense.make(Ax.rows(), 0);
		
		// check A(1)*one >= 0
		Ax1.zMult(one, a);for(int i=0;i<a.size();++i) Assert.assertTrue(a.getQuick(i)>=0);
		
		// check (A(1)-A(0))*one = b
		Ax1.assign(Ax0, cern.jet.math.Functions.minus);
		Ax1.zMult(one, a);for(int i=0;i<a.size();++i) Assert.assertTrue(a.getQuick(i) == -b.getQuick(i));
		
		// Finally, we check that neither states are incompatible (there are no reject states so there should not be any)
		int pairNumber [] = new int[gr.getStateNumber()*(gr.getStateNumber()+1)/2];
		gr.linear.findIncompatiblePairs(pairNumber, ThreadNumber);Linear.numberNonNegativeElements(pairNumber);
		for(int i=0;i<pairNumber.length;++i) pairNumber[i]=i;
		*/
	}
	
	@Test
	public final void testBuildMatrix0()
	{
		LearnerGraph gr=new LearnerGraph(Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr, DoubleFactory2D.dense.make(new double[][]{new double[]{1}}), DoubleFactory1D.dense.make(new double[]{0}));
	}

	@Test
	public final void testBuildMatrix1()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a->B-b->A",	"testAddToBuffer1"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr, DoubleFactory2D.dense.make(new double[][]{
				new double[]{1,0,-k},
				new double[]{0,2,-k},
				new double[]{-k,0,2-k}
				}), DoubleFactory1D.dense.make(new double[]{1,1,2}));
	}
	
	@Test
	public final void testBuildMatrix2()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->A-b->B",	"testAddToBuffer1"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr, null,null);
		/*DoubleFactory2D.dense.make(new double[][]{
				new double[]{1,0,-k},
				new double[]{0,2,-k},
				new double[]{-k,0,2-k}
				}), DoubleFactory1D.dense.make(new double[]{1,2,1}));*/
	}

	@Test
	public final void testBuildMatrix3()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nC-a->D",	"testAddToBuffer2"),Configuration.getDefaultConfiguration());
		final int size =4*5/2; 
		DoubleMatrix2D matrix=DoubleFactory2D.sparse.identity(size);
		DoubleMatrix1D row=DoubleFactory1D.dense.make(size, 0);
		matrix.setQuick(0, 2, -k);row.setQuick(0, 1);
		matrix.setQuick(5, 9, -k);row.setQuick(5, 1);
		matrix.setQuick(3, 7, -k);row.setQuick(3, 1);
		
		checkBuildMatrix(gr,matrix,row);
	}

	@Test
	public final void testBuildMatrix4()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C",	"testAddToBuffer3"),Configuration.getDefaultConfiguration());
		final int size =4*5/2; 
		DoubleMatrix2D matrix=DoubleFactory2D.sparse.identity(size);
		DoubleMatrix1D row=DoubleFactory1D.dense.make(size, 0);
		matrix.setQuick(0, 0, 2);matrix.setQuick(0, 2, -k);matrix.setQuick(0, 5, -k);row.setQuick(0, 2);
		matrix.setQuick(1, 1, 2);
		matrix.setQuick(3, 3, 2);
		matrix.setQuick(6, 6, 2);
	
		matrix.setQuick(9, 5, -k);row.setQuick(9, 1);
		matrix.setQuick(6, 4, -k);row.setQuick(6, 1);
				
		checkBuildMatrix(gr,matrix,row);
	}
	
	@Test
	public final void testBuildMatrix5()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C","testAddToBuffer4"),Configuration.getDefaultConfiguration());
		final int size =4*5/2; 
		DoubleMatrix2D matrix=DoubleFactory2D.sparse.identity(size);
		DoubleMatrix1D row=DoubleFactory1D.dense.make(size, 0);
		matrix.setQuick(0, 0, 2);matrix.setQuick(0, 2, -k);matrix.setQuick(0, 5, -k);row.setQuick(0, 2);
		matrix.setQuick(1, 1, 2);
		matrix.setQuick(3, 3, 2);
		matrix.setQuick(6, 6, 2);
	
		matrix.setQuick(9, 5, -2*k);row.setQuick(9, 2);
		matrix.setQuick(6, 4, -k);matrix.setQuick(6, 5, -k);row.setQuick(6, 2);
		matrix.setQuick(9, 9, 2);
		matrix.setQuick(8, 8, 2);
		matrix.setQuick(7, 7, 2);
		matrix.setQuick(6, 6, 2);
		
		checkBuildMatrix(gr,matrix,row);
	}

	@Test
	public final void testBuildMatrix6()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C\nD-c->A","testAddToBuffer5"),Configuration.getDefaultConfiguration());
		final int size =4*5/2; 
		DoubleMatrix2D matrix=DoubleFactory2D.sparse.identity(size);
		DoubleMatrix1D row=DoubleFactory1D.dense.make(size, 0);
		matrix.setQuick(0, 0, 2);matrix.setQuick(0, 2, -k);matrix.setQuick(0, 5, -k);row.setQuick(0, 2);
		matrix.setQuick(1, 1, 2);
		matrix.setQuick(3, 3, 2);
		matrix.setQuick(6, 6, 2);
	
		matrix.setQuick(9, 5, -2*k);matrix.setQuick(9, 0, -k);row.setQuick(9, 3);
		matrix.setQuick(6, 4, -k);matrix.setQuick(6, 5, -k);row.setQuick(6, 2);
		matrix.setQuick(9, 9, 3);
		matrix.setQuick(8, 8, 3);
		matrix.setQuick(7, 7, 3);
		matrix.setQuick(6, 6, 3);
		
		checkBuildMatrix(gr,matrix,row);
	}

	@Test
	public final void testBuildMatrix7()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nA-c->C\nD-a->C\nD-b->C\nD-c->A","testAddToBuffer7"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);

		final int size =4*5/2; 
		DoubleMatrix2D matrix=DoubleFactory2D.sparse.identity(size);
		DoubleMatrix1D row=DoubleFactory1D.dense.make(size, 0);
		matrix.setQuick(0, 0, 3);matrix.setQuick(0, 2, -k);matrix.setQuick(0, 5, -2*k);row.setQuick(0, 3);
		matrix.setQuick(1, 1, 3);
		matrix.setQuick(3, 3, 3);
		matrix.setQuick(6, 6, 3);
	
		matrix.setQuick(9, 5, -2*k);matrix.setQuick(9, 0, -k);row.setQuick(9, 3);
		matrix.setQuick(6, 4, -k);matrix.setQuick(6, 5, -k);matrix.setQuick(6, 3, -k);row.setQuick(6, 3);
		matrix.setQuick(9, 9, 3);
		matrix.setQuick(8, 8, 3);
		matrix.setQuick(7, 7, 3);
		matrix.setQuick(6, 6, 3);
		
		checkBuildMatrix(gr,matrix,row);
	}
	
	@Test
	public final void testBuildMatrix8()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nA-c->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);

		final int size =4*5/2; 
		DoubleMatrix2D matrix=DoubleFactory2D.sparse.identity(size);
		DoubleMatrix1D row=DoubleFactory1D.dense.make(size, 0);
		matrix.setQuick(0, 0, 3);matrix.setQuick(0, 2, -k);matrix.setQuick(0, 5, -2*k);row.setQuick(0, 3);
		matrix.setQuick(1, 1, 3);
		matrix.setQuick(3, 3, 3);
		matrix.setQuick(6, 6, 3);
	
		matrix.setQuick(9, 5, -3*k);matrix.setQuick(9, 0, -k);row.setQuick(9, 4);
		matrix.setQuick(6, 4, -k);matrix.setQuick(6, 5, -k);matrix.setQuick(6, 3, -k);row.setQuick(6, 3);
		matrix.setQuick(9, 9, 4);
		matrix.setQuick(8, 8, 4);
		matrix.setQuick(7, 7, 4);
		matrix.setQuick(6, 6, 4);
		
		checkBuildMatrix(gr,matrix,row);
	}
		
	@Test
	public final void testBuildMatrix9()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a->B-b->A","testAddToBuffer8"),Configuration.getDefaultConfiguration());
		gr.linear.buildMatrix(ThreadNumber);
		final double k = gr.config.getAttenuationK(); 
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
			"mat(1,1)=1.0;","mat(1,3)=-"+k+";",// AA
			"mat(3,3)="+(2.0-k)+";","mat(3,1)=-"+k+";", // BB
			"mat(2,2)="+2.0+";","mat(2,3)=-"+k+";"}));// AB
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix10()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testEstimation1"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix11()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A","testEstimation2"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix12()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c->R","testEstimation2"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix13()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c->R-a->F","testEstimation2"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);
	}

	static final int PAIR_INCOMPATIBLE = Linear.PAIR_INCOMPATIBLE, PAIR_OK=Linear.PAIR_OK;
	
	@Test(expected=IllegalArgumentException.class)
	public final void findIncompatiblePairs_fail()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c->R-a->F","testEstimation2"),Configuration.getDefaultConfiguration());
		gr.linear.findIncompatiblePairs(new int[]{},ThreadNumber);
	}

	/** Tests that if all pairs are not compatible, this is preserved. */
	@Test
	public final void TestFindIncompatibleStatesA()
	{
		int PAIR_INCOMPATIBLE = Linear.PAIR_INCOMPATIBLE;
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c->R-a->F","testEstimation2"),Configuration.getDefaultConfiguration());
		final int size=gr.getStateNumber()*(gr.getStateNumber()+1)/2;
		int pairs[]=new int[size];for(int i=0;i<pairs.length;++i) pairs[i]=PAIR_INCOMPATIBLE;
		gr.linear.findIncompatiblePairs(pairs,ThreadNumber);for(int i=0;i<pairs.length;++i) Assert.assertEquals(PAIR_INCOMPATIBLE,pairs[i]);
	}

	/** Tests that if all pairs are not compatible, this is preserved. */
	@Test
	public final void TestFindIncompatibleStatesB()
	{
		int PAIR_INCOMPATIBLE = Linear.PAIR_INCOMPATIBLE;
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R","TestFindIncompatibleStatesB"),Configuration.getDefaultConfiguration());
		final int size=gr.getStateNumber()*(gr.getStateNumber()+1)/2;
		int pairs[]=new int[size];for(int i=0;i<pairs.length;++i) pairs[i]=PAIR_INCOMPATIBLE;
		gr.linear.findIncompatiblePairs(pairs,ThreadNumber);
		
		CmpVertex R=gr.findVertex("R");
		for(int i=0;i<pairs.length;++i)
		{
			boolean reject = false;
			for(CmpVertex v:gr.transitionMatrix.keySet())
				if (gr.wmethod.vertexToInt(R, v) == i)
					reject = true;
			Assert.assertEquals(reject? Linear.PAIR_REJECT:PAIR_INCOMPATIBLE,pairs[i]);
		}
	}

	protected Map<Integer,StatePair> reverseMap = null;
	
	private class StringPair extends Pair<String,String>{
		public StringPair(String a, String b) {
			super(a, b);
		}}
	
	/** Uses computePairCompatibilityScore_general to identify incompatible pairs of states. */
	protected static Set<StatePair> buildSetOfIncompatiblePairsSlowly(final LearnerGraph gr,int ThreadNumber)
	{
		final Set<StatePair> result = new LinkedHashSet<StatePair>();
		
		List<HandleRow> handlerList = new LinkedList<HandleRow>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
		handlerList.add(new HandleRow()
		{
			public void init(int threadNo)
			{
			}
			
			public void handleEntry(Entry<CmpVertex, Map<String, CmpVertex>> entryA, int threadNo) 
			{
				if (entryA.getKey().isAccept())
				{// reject-states are ignored.
					
					// Now iterate through states
					Iterator<Entry<CmpVertex,Map<String,List<CmpVertex>>>> stateB_It = gr.learnerCache.getSortaInverse().entrySet().iterator();
					while(stateB_It.hasNext())
					{
						Entry<CmpVertex,Map<String,List<CmpVertex>>> stateB = stateB_It.next();
						if (stateB.getKey().isAccept())
						{// reject-states are ignored.
							StatePair currentPair = new StatePair(entryA.getKey(),stateB.getKey());
							if (gr.pairscores.computePairCompatibilityScore_general(currentPair, new LinkedList<Collection<CmpVertex>>()) < 0)
								synchronized(result)
								{
									result.add(currentPair);
								}
						}// reject-states are ignored.
						
						if (stateB.getKey().equals(entryA.getKey())) break; // we only process a triangular subset.
					}// iterating through states (stateB)
				}// reject-states are ignored.
			}
		});
		gr.linear.performRowTasks(handlerList, ThreadNumber);
		return result;
	}

	protected final void findIncompatibleTestHelper(LearnerGraph gr,List<StringPair> incompatibles_list)
	{
		int PAIR_INCOMPATIBLE = Linear.PAIR_INCOMPATIBLE;
		HashSet<StatePair> incompatibles = new HashSet<StatePair>();
		for(StringPair p:incompatibles_list)
		{
			incompatibles.add(new StatePair(gr.findVertex(p.firstElem),gr.findVertex(p.secondElem)));
			incompatibles.add(new StatePair(gr.findVertex(p.secondElem),gr.findVertex(p.firstElem)));
		}
		Set<StatePair> incompatiblePairs = new HashSet<StatePair>();
		for(StatePair s:buildSetOfIncompatiblePairsSlowly(gr,ThreadNumber))
		{
			incompatiblePairs.add(s);incompatiblePairs.add(new StatePair(s.secondElem,s.firstElem));
		}
		// incompatiblePairs these are the pairs which cannot be merged which may be a larger set than 
		// the set of clearly incompatible pairs. The difference is that when pairs are merged, more traces
		// are created, hence it is possible that some of those new traces will be in conflict.
		// Example: states A3 and A2 in findIncompatibleStates6()
		HashSet<StatePair> pairs_extra = new HashSet<StatePair>();pairs_extra.addAll(incompatibles);pairs_extra.removeAll(incompatiblePairs);
		Assert.assertTrue("compatible pairs included :"+pairs_extra,pairs_extra.isEmpty());
		//System.out.println(incompatiblePairs);

		final int size=gr.getStateNumber()*(gr.getStateNumber()+1)/2;
		final int highNumber = 10000;
		int pairs[]=new int[size];for(int i=0;i<pairs.length;++i) pairs[i]=highNumber;
		reverseMap = new HashMap<Integer,StatePair>();
		for(CmpVertex A:gr.transitionMatrix.keySet())
			for(CmpVertex B:gr.transitionMatrix.keySet())
				reverseMap.put(gr.wmethod.vertexToInt(A, B), new StatePair(A,B));
		Assert.assertEquals(size,reverseMap.size());
		
		gr.linear.buildMatrix(ThreadNumber);
		gr.linear.findIncompatiblePairs(pairs,ThreadNumber);
		
		int cnt=0;
		for(int i=0;i<pairs.length;++i)
		{
			StatePair pair = reverseMap.get(i);
			if (!pair.firstElem.isAccept() || !pair.secondElem.isAccept())
				Assert.assertEquals("pair ("+pair.firstElem+","+pair.secondElem+") should not have been touched", 
						Linear.PAIR_REJECT,pairs[i]);// reject-states should be labelled as such.
			else
			if (incompatibles.contains(pair))
				Assert.assertEquals("pair ("+pair.firstElem+","+pair.secondElem+") should be marked as incompatible",PAIR_INCOMPATIBLE,pairs[i]);// this pair is not compatible
			else
				Assert.assertEquals("invalid counter for pair ("+pair.firstElem+","+pair.secondElem+")", 
						cnt++,pairs[i]);// pairs should have monotonically increasing numbers
				
		}
	}

	/** Tests that if A is not compatible to D, this is recorded. */
	@Test
	public final void TestFindIncompatibleStates1()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R","TestFindIncompatibleStates1"),Configuration.getDefaultConfiguration());
		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","D")
		}));
	}

	/** Tests that if A is not compatible to C and D, this is recorded. */
	@Test
	public final void TestFindIncompatibleStates2()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a-#Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c-#R","TestFindIncompatibleStates2"),Configuration.getDefaultConfiguration());

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","D"),new StringPair("A","C")
		}));
	}

	@Test
	public final void TestFindIncompatibleStates3()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A3-a->A2-a->A1-a->A-b-#R\nB3-a->B2-a->B1-a->B-b->D","TestFindIncompatibleStates3"),Configuration.getDefaultConfiguration());

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B"),new StringPair("A1","B1"),new StringPair("A2","B2"),new StringPair("A3","B3")
		}));
	}

	@Test
	public final void TestFindIncompatibleStates4()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("B1-b->C\n"+
				"A3-a->A2-a->A1-a->A-b-#R\nB3-a->B2-a->B1-a->B-b->D","TestFindIncompatibleStates4"),Configuration.getDefaultConfiguration());

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B"),new StringPair("A1","B1"),new StringPair("A2","B2"),new StringPair("A3","B3"),
				new StringPair("A","B1"),new StringPair("A1","B2"),new StringPair("A2","B3")
		}));
	}

	@Test
	public final void TestFindIncompatibleStates5()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("B3-b->B3\n"+
				"B1-b->C\n"+
				"A3-a->A2-a->A1-a->A-b-#R\nB3-a->B2-a->B1-a->B-b->D","TestFindIncompatibleStates5"),Configuration.getDefaultConfiguration());

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B"),new StringPair("A1","B1"),new StringPair("A2","B2"),new StringPair("A3","B3"),
				new StringPair("A","B1"),new StringPair("A1","B2"),new StringPair("A2","B3"),
				new StringPair("A","B3")
		}));
	}

	@Test
	public final void TestFindIncompatibleStates6()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("B3-b->B3\n"+"A3-b->A3\n"+
				"B1-b->C\n"+
				"A3-a->A2-a->A1-a->A-b-#R\nB3-a->B2-a->B1-a->B-b->D","TestFindIncompatibleStates6"),Configuration.getDefaultConfiguration());

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B"),new StringPair("A1","B1"),new StringPair("A2","B2"),new StringPair("A3","B3"),
				new StringPair("A","B1"),new StringPair("A1","B2"),new StringPair("A2","B3"),
				new StringPair("A","B3"),
				new StringPair("A3","A")
		}));
	}


	@Test
	public final void TestFindIncompatibleStates7()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->E-a->C-a->A-b-#R\nB-a->H-a->G-a->F-a->D-a->B-b->Q"
				,"TestFindIncompatibleStates7"),Configuration.getDefaultConfiguration());

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B"),new StringPair("A","D"),new StringPair("A","F"),new StringPair("A","G"),new StringPair("A","H"),
				new StringPair("C","B"),new StringPair("C","D"),new StringPair("C","F"),new StringPair("C","G"),new StringPair("C","H"),
				new StringPair("E","B"),new StringPair("E","D"),new StringPair("E","F"),new StringPair("E","G"),new StringPair("E","H")
		}));
	}
	
	@Test
	public final void TestFindIncompatibleStates8()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("E-a->C-a->A-b-#R\nE-c->C-c->E\nB-c->H-c->G-c->F-c->D-c->B-b->Q\n"
				,"TestFindIncompatibleStates8"),Configuration.getDefaultConfiguration());

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B")
		}));
	}
	
	@Test
	public final void TestFindIncompatibleStates9()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("D-a->B\n"+
				"E-a->C-a->A-b-#R\nE-c->C-c->E\nB-c->H-c->G-c->F-c->D-c->B-b->Q\n"
				,"TestFindIncompatibleStates9"),Configuration.getDefaultConfiguration());

		findIncompatibleTestHelper(gr, Arrays.asList(new StringPair[]{
				new StringPair("A","B"),
				new StringPair("C","B"),new StringPair("C","D"),new StringPair("C","F"),new StringPair("C","G"),new StringPair("C","H"),
				new StringPair("E","B"),new StringPair("E","D"),new StringPair("E","F"),new StringPair("E","G"),new StringPair("E","H")
		}));
	}
	
	@Test
	public final void TestComputeStateCompatibility1()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a->C"
				,"TestComputeStateCompatibility1"),Configuration.getDefaultConfiguration());
		DoubleMatrix1D result = DoubleFactory1D.dense.make(gr.linear.computeStateCompatibility(ThreadNumber));
		Assert.assertTrue(DoubleFactory1D.dense.make(new double[]{1+k,1,1,0,0,0}).equals(result));
	}	
	
	@Test
	public final void TestComputeStateCompatibility2()
	{
		final int PAIR_REJECT = Linear.PAIR_REJECT, PAIR_INCOMPATIBLE=Linear.PAIR_INCOMPATIBLE; 
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a->C-a-#D"
				,"TestComputeStateCompatibility1"),Configuration.getDefaultConfiguration());
		DoubleMatrix1D result = DoubleFactory1D.dense.make(gr.linear.computeStateCompatibility(ThreadNumber));
		DoubleMatrix1D expected=DoubleFactory1D.dense.make(new double[]{1+k,PAIR_INCOMPATIBLE,1,PAIR_INCOMPATIBLE,PAIR_INCOMPATIBLE,0,PAIR_REJECT,PAIR_REJECT,PAIR_REJECT,PAIR_REJECT});
		Assert.assertTrue(expected.equals(result));
	}	
	
}
