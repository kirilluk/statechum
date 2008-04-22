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
import java.util.Random;

import org.junit.Assert;
import org.junit.Test;

import cern.colt.function.IntComparator;

import statechum.Configuration;

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
}
