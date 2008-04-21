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
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Test;

import cern.colt.function.IntComparator;
import cern.colt.list.IntArrayList;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;

// FIXME: needs to be parameterised for a number of different threads.
public class TestLinear {
	final int ThreadNumber = 1;
	@Test
	public final void testAddToBuffer0()
	{
		LearnerGraph gr=new LearnerGraph(Configuration.getDefaultConfiguration());
		Transform ad = new Transform(gr);ad.prepareForLinear();
		ExternalSolver solver = ad.buildMatrix(ThreadNumber);
	}

	@Test
	public final void testAddToBuffer1()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->A-b->B",	"testAddToBuffer1"),Configuration.getDefaultConfiguration());
	}
	
	@Test
	public final void testAddToBuffer2()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nC-a->D",	"testAddToBuffer2"),Configuration.getDefaultConfiguration());
	}

	@Test
	public final void testAddToBuffer3()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C",	"testAddToBuffer3"),Configuration.getDefaultConfiguration());
	}
	
	@Test
	public final void testAddToBuffer4()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C","testAddToBuffer4"),Configuration.getDefaultConfiguration());
	}

	@Test
	public final void testAddToBuffer5()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C\nD-c->A","testAddToBuffer5"),Configuration.getDefaultConfiguration());
	}
	
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
	
	@Test
	public final void testAddToBuffer7()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nA-c->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7"),Configuration.getDefaultConfiguration());
/*
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();Transform ad = new Transform(gr);ad.prepareForLinear();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("D"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(7,7)=4.0;","mat(7,5)=-"+ad.valueK+";","mat(7,6)=-"+2*ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
		*/
	}
	
	
	@Test
	public final void testAddToBuffer8_1()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a->B-b->A","testAddToBuffer8"),Configuration.getDefaultConfiguration());
		Transform ad = new Transform(gr);ad.prepareForLinear();
		ad.buildMatrix(ThreadNumber);
		final double k = gr.config.getAttenuationK(); 
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
			"mat(1,1)=1.0;","mat(1,3)=-"+k+";",// AA
			"mat(3,3)="+(2.0-k)+";","mat(3,1)=-"+k+";", // BB
			"mat(2,2)="+2.0+";","mat(2,3)=-"+k+";"}));// AB
		//Assert.assertEquals(expected, testAdder.getResult());
	}
	
	LearnerGraph grLoadDistribution=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7"),Configuration.getDefaultConfiguration());

	/** Tests how well the workload is distributed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWorkLoadDistribution0_1()
	{
		grLoadDistribution.transform.partitionWorkLoad(0);
	}

	/** Tests how well the workload is distributed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWorkLoadDistribution0_2()
	{
		grLoadDistribution.transform.partitionWorkLoad(-1);
	}

	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution1()
	{
		Assert.assertArrayEquals(new int[]{0,4},grLoadDistribution.transform.partitionWorkLoad(1));
	}

	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution2()
	{
		Assert.assertArrayEquals(new int[]{0,2,3,4,4},grLoadDistribution.transform.partitionWorkLoad(4));
	}
	
	/** Tests the workload distribution. */
	@Test
	public final void testWorkLoadDistribution3()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7"),Configuration.getDefaultConfiguration());
		for(int i=0;i< 4;++i) Transform.addToGraph(gr, grLoadDistribution);
		Assert.assertArrayEquals(new int[]{0,10,14,17,20},gr.transform.partitionWorkLoad(4));
	}
	
	@Test
	public final void testEstimation1()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testEstimation1"),Configuration.getDefaultConfiguration());
		// AA: 3
	}

	@Test
	public final void testEstimation2()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A","testEstimation2"),Configuration.getDefaultConfiguration());
		// ACD
		//A3
		//C11
	}
	
	@Test
	public final void testEstimation3()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A","testEstimation2"),Configuration.getDefaultConfiguration());
		// ACD
		//A3
		//C11
		//D314
	}
}
