/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

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
import java.util.Random;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import statechum.ArrayOperations;
import statechum.Configuration;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.TestFSMAlgo;

public class TestRandomPathGenerator {
	private Configuration config = Configuration.getDefaultConfiguration();

	@Test
	public void test_diameter0()
	{
		Assert.assertEquals(0, RandomPathGenerator.diameter(
				new LearnerGraph(config)));
	}

	
	@Test
	public void test_diameter1()
	{
		Assert.assertEquals(0, RandomPathGenerator.diameter(
				new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A","test_diameter1"),config)));
	}

	@Test
	public void test_diameter2()
	{
		Assert.assertEquals(3, RandomPathGenerator.diameter(
				new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->A-c-#C\nB-b->D-c->E","test_diameter1"),config)));
	}

	/** One of the test graphs. Should not contain reject states since these methods assume that all states are accepts. */
	private LearnerGraph simpleGraph = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B\nB-b->D-c->E","test_generateRandomWalk1"),config);
	
	@Test
	public void test_generateRandomWalk1()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		Assert.assertEquals(Arrays.asList(new String[]{"a","b"}), generator.generateRandomWalk(2,2, true));
		Assert.assertEquals(Arrays.asList(new String[]{"a","b","c"}), generator.generateRandomWalk(3,3, true));
	}

	@Test(expected=IllegalArgumentException.class)
	public void test_generateRandomWalk2()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		List<String> path = generator.generateRandomWalk(2,3, true);
		Assert.assertEquals(Arrays.asList(new String[]{"a","b"}), path);
		generator.allSequences.add(path);
		generator.generateRandomWalk(2,2, true);// no more paths of this length
	}

	@Test
	public void test_generateRandomWalk3a()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		Assert.assertNull(generator.generateRandomWalk(20,20, true));// no paths of this length
	}
	
	@Test
	public void test_generateRandomWalk3b()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		Assert.assertNull(generator.generateRandomWalk(20,20, false));// no paths of this length
	}
	
	private void generateSeq(int length, int count,RandomPathGenerator generator,Object [][]expectedSeq)
	{
		generateSeq(length, length,count,generator,expectedSeq);
	}
	
	private void generateSeq(int length, int prefixLength,int count,RandomPathGenerator generator,Object [][]expectedSeq)
	{
		Set<List<String>> expected = new HashSet<List<String>>();
		for(Object []seq:expectedSeq)
		{
			List<String> sequence = new LinkedList<String>();for(int i=0;i<seq.length;++i) sequence.add((String)seq[i]);
			expected.add(sequence);
		}
		for(int i=0;i<count;++i) 
		{
			List<String> path = generator.generateRandomWalk(length, prefixLength, false);generator.allSequences.add(path);
		}
		Set<List<String>> actualA = new HashSet<List<String>>();actualA.addAll(generator.allSequences.getData());
		Assert.assertEquals(expected, actualA);
		Assert.assertNull(generator.generateRandomWalk(length, prefixLength, false));
		Set<List<String>> actualB = new HashSet<List<String>>();actualB.addAll(generator.allSequences.getData());
		Assert.assertEquals(expected, actualB);
	}
	
	@Test
	public void test_generateRandomWalk4()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		generateSeq(1,2,generator,new String[][]{new String[]{"c"},new String[]{"b"}});
	}
	
	@Test
	public void test_generateRandomWalk5a()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		generateSeq(2,2, 2,generator,new String[][]{new String[]{"a","c"},new String[]{"a","a"}});
	}	

	@Test
	public void test_generateRandomWalk5b()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		generateSeq(2,1, 1,generator,new String[][]{new String[]{"a","c"}});
	}	

	@Test
	public void test_generateRandomWalk5c()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		generateSeq(2,1, 1,generator,new String[][]{new String[]{"a","c"}});// this could be new String[]{"a","c"} just as well
	}	

	@Test
	public void test_generateRandomWalk6()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		generateSeq(4,3,generator,
				ArrayOperations.flatten(new Object[]{"a","b","c",new String[]{"a","b","c"}}));
	}	

	@Test
	public void test_generateRandomWalk7()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B\nB-b->D-a->D-c->E-a->E","test_generateRandomWalk7"),config);
		RandomPathGenerator generator = new RandomPathGenerator(graph,new Random(0),0);
		generateSeq(4,3,generator,
				ArrayOperations.flatten(new Object[]{new Object[]{// the first Object[] means we are talking
						// of a sequence, the second Object[] means that the first sequence contains alternatives,
						// defined in the second Object, the two sequences below denote these alternatives.
						new Object[]{"a","b","a","b"},
						new Object[]{"a","b","c",new String[]{"b","c"}}
				}}));
	}	

	@Test
	public void test_generateRandomWalk8a()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->B\nB-b->D-a->D-c->E-a->E","test_generateRandomWalk8"),config);
		RandomPathGenerator generator = new RandomPathGenerator(graph,new Random(0),0);
		generateSeq(4,4, 7,generator,
				ArrayOperations.flatten(new Object[]{new Object[]{// the first Object[] means we are talking
						// of a sequence, the second Object[] means that the first sequence contains alternatives,
						// defined in the second Object, the two sequences below denote these alternatives.
						new Object[]{"a","b","a","b"},
						new Object[]{"b","a","b","b"},
						new Object[]{"b","b","a",new String[]{"a","c"}},
						new Object[]{"b","b","b","c"},
						new Object[]{"a","b","c",new String[]{"b","c"}}
				}}));
	}
	
	@Test
	public void test_generateRandomWalk8b()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->B\nB-b->D-a->D-c->E-a->E","test_generateRandomWalk8"),config);
		RandomPathGenerator generator = new RandomPathGenerator(graph,new Random(0),0);
		generateSeq(4,3, 5,generator,
				ArrayOperations.flatten(new Object[]{new Object[]{// the first Object[] means we are talking
						// of a sequence, the second Object[] means that the first sequence contains alternatives,
						// defined in the second Object, the two sequences below denote these alternatives.
						new Object[]{"a","b","a","b"},
						new Object[]{"b","a","b","b"},
						new Object[]{"b","b","a",new String[]{"c"}},// "c" can also be "a"
						new Object[]{"b","b","b","c"},
						new Object[]{"a","b","c",new String[]{"b"}}// "b" can also be "c"
				}}));
	}	
	
	@Test(expected=IllegalArgumentException.class)
	public void test_generateRandomWalk_tooshort1()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		generator.generateRandomWalk(-1,1, true);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void test_generateRandomWalk_tooshort2()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		generator.generateRandomWalk(0,0, true);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void test_generateRandomWalk_tooshort3()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		generator.generateRandomWalk(0,0, false);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void test_generateRandomWalk_tooshort4()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		generator.generateRandomWalk(2,3, false);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void test_generateRandomWalk_tooshort5()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		generator.generateRandomWalk(2,-1, false);
	}
	
	@Test
	public void test_generatePosNeg1()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->B\nB-b->D-a->D-c->E-a->E-c->A\nB-c->B\nA-q->A\nA-t->A\nA-r->A\nE-f->F-d->F-e->F","test_generatePosNeg1"),config);
		Assert.assertEquals(4,RandomPathGenerator.diameter(graph));
		RandomPathGenerator generator = new RandomPathGenerator(graph,new Random(0),8);
		final int chunkNumber = 3, posOrNegPerChunk = 3;
		generator.generatePosNeg(posOrNegPerChunk*2,chunkNumber);
		Collection<List<String>> previousChunkNeg = null;
		Collection<List<String>> previousChunkPos = null;

		for(int i=0;i<chunkNumber;++i)
		{
			{
				Collection<List<String>> currentNeg = generator.getAllSequences(i).getData();
				Assert.assertEquals("chunk "+i+" (neg) should be of length "+(posOrNegPerChunk*(i+1))+" but it was "+currentNeg.size(),(posOrNegPerChunk*(i+1)), currentNeg.size());
				for(List<String> s:currentNeg)
					Assert.assertTrue("path "+s+" should not exist",graph.paths.tracePath(s) >=0);
				if (previousChunkNeg != null) currentNeg.containsAll(previousChunkNeg);
				previousChunkNeg = currentNeg;
			}
			{
				Collection<List<String>> currentPos = generator.getExtraSequences(i).getData();
				Assert.assertEquals("chunk "+i+" (pos) should be of length "+(posOrNegPerChunk*(i+1))+" but it was "+currentPos.size(),(posOrNegPerChunk*(i+1)), currentPos.size());
				for(List<String> s:currentPos)
					Assert.assertTrue("path "+s+" should exist",graph.paths.tracePath(s) == RPNIBlueFringeLearner.USER_ACCEPTED);
				if (previousChunkPos != null) currentPos.containsAll(previousChunkPos);
				previousChunkPos = currentPos;
			}
		}
		Assert.assertEquals(chunkNumber*posOrNegPerChunk, previousChunkNeg.size());
		Assert.assertEquals(chunkNumber*posOrNegPerChunk, previousChunkPos.size());
	}
	
	@Test
	public void test_generateRandomPosNeg1()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->B\nB-b->D-a->D-c->E-a->E-c->A\nB-c->B\nA-q->A\nA-t->A\nA-r->A\nE-f->F-d->F-e->F\nA-g->A-h->A","test_generateRandomPosNeg1"),config);
		Assert.assertEquals(4,RandomPathGenerator.diameter(graph));
		RandomPathGenerator generator = new RandomPathGenerator(graph,new Random(0),8);
		final int chunkNumber = 4, posOrNegPerChunk = 3;
		generator.generateRandomPosNeg(posOrNegPerChunk*2,chunkNumber);
		//System.out.println("fudges: "+generator.getFudgeDetails());
		
		Collection<List<String>> previousChunk = null;

		for(int i=0;i<chunkNumber;++i)
		{
			Collection<List<String>> current = generator.getAllSequences(i).getData();
			Assert.assertEquals("chunk "+i+" (neg) should be of length "+(2*posOrNegPerChunk*(i+1))+" but it was "+current.size(),(2*posOrNegPerChunk*(i+1)), current.size());
			int positive = 0,negative=0;
			for(List<String> s:current)
				if(graph.paths.tracePath(s) >=0) ++negative;else ++positive;
			Assert.assertEquals(posOrNegPerChunk*(i+1), positive);
			Assert.assertEquals(posOrNegPerChunk*(i+1), negative);
			if (previousChunk != null) current.containsAll(previousChunk);
			previousChunk= current;
		}
		Assert.assertEquals(chunkNumber*posOrNegPerChunk*2, previousChunk.size());
	}
	
	/** A slightly more difficult automaton than the above. */
	@Test
	public void test_generateRandomPosNeg2()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->B\nB-b->D-a->D-c->E-a->E-c->A\nB-c->B\nA-q->A\nA-t->A\nA-r->A\nE-f->F-d->F","test_generateRandomPosNeg1"),config);
		Assert.assertEquals(4,RandomPathGenerator.diameter(graph));
		RandomPathGenerator generator = new RandomPathGenerator(graph,new Random(0),4);
		final int chunkNumber = 4, posOrNegPerChunk = 3;
		generator.generateRandomPosNeg(posOrNegPerChunk*2,chunkNumber);
		//System.out.println("fudges: "+generator.getFudgeDetails());
		
		Collection<List<String>> previousChunk = null;

		for(int i=0;i<chunkNumber;++i)
		{
			Collection<List<String>> current = generator.getAllSequences(i).getData();
			Assert.assertEquals("chunk "+i+" (neg) should be of length "+(2*posOrNegPerChunk*(i+1))+" but it was "+current.size(),(2*posOrNegPerChunk*(i+1)), current.size());
			int positive = 0,negative=0;
			for(List<String> s:current)
				if(graph.paths.tracePath(s) >=0) ++negative;else ++positive;
			Assert.assertEquals(posOrNegPerChunk*(i+1), positive);
			Assert.assertEquals(posOrNegPerChunk*(i+1), negative);
			if (previousChunk != null) current.containsAll(previousChunk);
			previousChunk= current;
		}
		Assert.assertEquals(chunkNumber*posOrNegPerChunk*2, previousChunk.size());
	}
	
	
}
