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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import statechum.ArrayOperations;
import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.AddTransitions.AddToMatrix;

import static statechum.analysis.learning.TestFSMAlgo.buildGraph;
import static statechum.analysis.learning.rpnicore.AddTransitions.HammingDistance;

public class TestAddTransitions {
	@Test(expected=IllegalArgumentException.class)
	public final void testHammingDistance0()
	{
		HammingDistance(
				Arrays.asList(new Boolean[]{true}), Arrays.asList(new Boolean[]{}));
	}

	@Test 
	public final void testHammingDistance1()
	{
		Assert.assertEquals(0, HammingDistance(
				Arrays.asList(new Boolean[]{}),
				Arrays.asList(new Boolean[]{})
		));
	}
	
	@Test 
	public final void testHammingDistance2()
	{
		Assert.assertEquals(0, HammingDistance(
				Arrays.asList(new Boolean[]{false}),
				Arrays.asList(new Boolean[]{false})
		));
	}
	
	@Test 
	public final void testHammingDistance3()
	{
		Assert.assertEquals(1, HammingDistance(
				Arrays.asList(new Boolean[]{false}),
				Arrays.asList(new Boolean[]{true})
		));
	}
	
	@Test 
	public final void testHammingDistance4()
	{
		Assert.assertEquals(1, HammingDistance(
				Arrays.asList(new Boolean[]{true,false,false}),
				Arrays.asList(new Boolean[]{true,true,false})
		));
	}
	
	@Test 
	public final void testHammingDistance5()
	{
		Assert.assertEquals(3, HammingDistance(
				Arrays.asList(new Boolean[]{true,true,false}),
				Arrays.asList(new Boolean[]{false,false,true})
		));
	}

	private LearnerGraph g = new LearnerGraph(buildGraph("A-a->A-b->B",	"testToBooleans"),Configuration.getDefaultConfiguration());
	private StringBuffer resultDescr = new StringBuffer();	
	private List<List<String>> buildListList(String [][]list_of_seq)
	{
		List<List<String>> result = new LinkedList<List<String>>();
		for(String []seq:list_of_seq)
			result.add(Arrays.asList(seq));
		return result;
	}
	
	@Test
	public final void testToBooleans0()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{},
				AddTransitions.wToBooleans(g,g.findVertex("A"),
				buildListList(new String [][]{})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}
	
	@Test
	public final void testToBooleans1()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{true,true},
				AddTransitions.wToBooleans(g,g.findVertex("A"),
				buildListList(new String [][]{
						new String[] {"a"},
						new String[] {"b"}
				})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}

	@Test
	public final void testToBooleans2()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{true,false},
				AddTransitions.wToBooleans(g,g.findVertex("A"),
				buildListList(new String [][]{
						new String[] {"a"},
						new String[] {"c"}
				})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}

	@Test
	public final void testToBooleans3()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{true,false,false,true},
				AddTransitions.wToBooleans(g,g.findVertex("A"),
				buildListList(new String [][]{
						new String[] {"a"},
						new String[] {"b","b"},
						new String[] {"a","a","a","c"},
						new String[] {"a","a","a","b"},
				})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}

	@Test
	public final void testToBooleans4()
	{
		boolean outcome = ArrayOperations.cmp(
				new Boolean[]{false,false,false,false},
				AddTransitions.wToBooleans(g,g.findVertex("B"),
				buildListList(new String [][]{
						new String[] {"a"},
						new String[] {"b","b"},
						new String[] {"a","a","a","c"},
						new String[] {"a","a","a","b"},
				})
				).toArray(), resultDescr);
		Assert.assertTrue(resultDescr.toString(),outcome);
	}
	
	@Test
	public final void testComputeHamming()
	{
		Assert.assertEquals("Hamming distances min: 1 max: 1", new AddTransitions(g).ComputeHamming(false));
	}
	
	static class TestMatrixEntryAdder implements AddToMatrix
	{
		private final Set<String> result = new HashSet<String>();
		
		public void addMapping(Integer A, Integer B, double value) {
			StringBuffer buffer = new StringBuffer();
			AddTransitions.addAssignement(buffer, A, B, value);result.add(buffer.toString());
		}
		
		public Set<String> getResult()
		{
			return result;
		}
	}
	
	@Test
	public final void testAddToBuffer1()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->A-b->B",	"testAddToBuffer1"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();AddTransitions ad = new AddTransitions(gr);ad.populatePairToNumber();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("B"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(2,2)=2.0;"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
	
	@Test
	public final void testAddToBuffer2()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nC-a->D",	"testAddToBuffer2"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();AddTransitions ad = new AddTransitions(gr);ad.populatePairToNumber();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("C"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(4,4)=1.0;","mat(4,8)=-"+ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}

	@Test
	public final void testAddToBuffer3()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C",	"testAddToBuffer3"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();AddTransitions ad = new AddTransitions(gr);ad.populatePairToNumber();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("D"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(7,7)=2.0;","mat(7,5)=-"+ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
	
	@Test
	public final void testAddToBuffer4()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C","testAddToBuffer4"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();AddTransitions ad = new AddTransitions(gr);ad.populatePairToNumber();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("D"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(7,7)=2.0;","mat(7,5)=-"+ad.valueK+";","mat(7,6)=-"+ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}

	@Test
	public final void testAddToBuffer5()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C\nD-c->A","testAddToBuffer5"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();AddTransitions ad = new AddTransitions(gr);ad.populatePairToNumber();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("D"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(7,7)=3.0;","mat(7,5)=-"+ad.valueK+";","mat(7,6)=-"+ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
	
	@Test
	public final void testAddToBuffer6()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C\nD-c->A","testAddToBuffer6"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();AddTransitions ad = new AddTransitions(gr);ad.populatePairToNumber();
		ad.addToBuffer(testAdder, gr.findVertex("D"), gr.findVertex("D"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(10,10)=3.0;","mat(10,1)=-"+ad.valueK+";","mat(10,6)=-"+2*ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
	
	@Test
	public final void testAddToBuffer7()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();AddTransitions ad = new AddTransitions(gr);ad.populatePairToNumber();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("D"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(7,7)=4.0;","mat(7,5)=-"+ad.valueK+";","mat(7,6)=-"+2*ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
	
	
	@Test
	public final void testAddToBuffer8()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a->B-b->A","testAddToBuffer8"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();AddTransitions ad = new AddTransitions(gr);ad.populatePairToNumber();
		ad.buildMatrix(testAdder);
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
			/* AA */ "mat(1,1)=1.0;","mat(1,3)=-"+ad.valueK+";",
			/* BB */ "mat(3,3)="+(2.0-ad.valueK)+";","mat(3,1)=-"+ad.valueK+";",
			/* AB */ "mat(2,2)="+2.0+";","mat(2,3)=-"+ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
}
