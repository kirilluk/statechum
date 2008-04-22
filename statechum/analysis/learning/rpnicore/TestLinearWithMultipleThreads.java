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
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
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
		gr.linear.prepareForLinear();
		ExternalSolver solver = gr.linear.buildMatrix(ThreadNumber);
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
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix4()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C",	"testAddToBuffer3"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);
	}
	
	@Test
	public final void testBuildMatrix5()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C","testAddToBuffer4"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix6()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C\nD-c->A","testAddToBuffer5"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix7()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nA-c->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);
/*
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();Transform ad = new Transform(gr);ad.prepareForLinear();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("D"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(7,7)=4.0;","mat(7,5)=-"+ad.valueK+";","mat(7,6)=-"+2*ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
		*/
	}
		
	@Test
	public final void testBuildMatrix8()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a->B-b->A","testAddToBuffer8"),Configuration.getDefaultConfiguration());
		gr.linear.prepareForLinear();
		gr.linear.buildMatrix(ThreadNumber);
		final double k = gr.config.getAttenuationK(); 
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
			"mat(1,1)=1.0;","mat(1,3)=-"+k+";",// AA
			"mat(3,3)="+(2.0-k)+";","mat(3,1)=-"+k+";", // BB
			"mat(2,2)="+2.0+";","mat(2,3)=-"+k+";"}));// AB
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix9()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testEstimation1"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix10()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A","testEstimation2"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix11()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c->R","testEstimation2"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);
	}

	@Test
	public final void testBuildMatrix12()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A-c->R-a->F","testEstimation2"),Configuration.getDefaultConfiguration());
		checkBuildMatrix(gr,null,null);
	}

}
