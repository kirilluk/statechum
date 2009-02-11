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
import org.junit.Before;
import org.junit.Test;

import statechum.ArrayOperations;
import statechum.Configuration;
import statechum.analysis.learning.AbstractOracle;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;
import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;

public class TestRandomPathGenerator {
	private Configuration config = null;

	@Before
	public void InitConfig()
	{
		config = Configuration.getDefaultConfiguration().copy();
		simpleGraph = new LearnerGraph(FsmParser.buildGraph("A-a->B\nB-b->D-c->E","test_generateRandomWalk1"),config);
	}
	
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
				new LearnerGraph(FsmParser.buildGraph("A-a->A","test_diameter1"),config)));
	}

	@Test
	public void test_diameter2()
	{
		Assert.assertEquals(3, RandomPathGenerator.diameter(
				new LearnerGraph(FsmParser.buildGraph("A-a->B-a->A-c-#C\nB-b->D-c->E","test_diameter1"),config)));
	}

	/** One of the test graphs. Should not contain reject states since these methods assume that all states are accepts. */
	private LearnerGraph simpleGraph = null;
	
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
		Set<List<String>> actualA = new HashSet<List<String>>();actualA.addAll(generator.allSequences.getData(PTASequenceEngine.truePred));
		Assert.assertEquals(expected, actualA);
		Assert.assertNull(generator.generateRandomWalk(length, prefixLength, false));
		Set<List<String>> actualB = new HashSet<List<String>>();actualB.addAll(generator.allSequences.getData(PTASequenceEngine.truePred));
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
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B\nB-b->D-a->D-c->E-a->E","test_generateRandomWalk7"),config);
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
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->B\nB-b->D-a->D-c->E-a->E","test_generateRandomWalk8"),config);
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
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->B\nB-b->D-a->D-c->E-a->E","test_generateRandomWalk8"),config);
		RandomPathGenerator generator = new RandomPathGenerator(graph,new Random(0),0);
		generateSeq(4,3, 5,generator,
				ArrayOperations.flatten(new Object[]{new Object[]{// the first Object[] means we are talking
						// of a sequence, the second Object[] means that the first sequence contains alternatives,
						// defined in the second Object, the two sequences below denote these alternatives.
						new Object[]{"a","b","a","b"},
						new Object[]{"b","a","b","b"},
						new Object[]{"b","b","a",new String[]{"c"}},// "c" can also be "a"
						new Object[]{"b","b","b","c"},
						new Object[]{"a","b","c",new String[]{"c"}}// "c" can also be "b"
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
	
	@Test(expected=IllegalArgumentException.class)
	public void test_generateRandomWalk_tooshort_PosNeg_fail1()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),-100);
		generator.generatePosNeg(10, 10);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void test_generateRandomWalk_tooshort_PosNeg_fail2()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		generator.generatePosNeg(11, 10);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void test_generateRandomWalk_tooshort_Random_fail1()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),-100);
		generator.generateRandomPosNeg(10, 10);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void test_generateRandomWalk_tooshort_Random_fail2()
	{
		RandomPathGenerator generator = new RandomPathGenerator(simpleGraph,new Random(0),0);
		generator.generateRandomPosNeg(11, 10);
	}
	
	public RandomPathGenerator generatePosNegTestHelper(String automaton, String automatonName,
			final int chunkNumber,final int posOrNegPerChunk)
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph(automaton,automatonName),config);
		Assert.assertEquals(4,RandomPathGenerator.diameter(graph));
		final RandomPathGenerator generator = new RandomPathGenerator(graph,new Random(0),8);
		generator.generatePosNeg(posOrNegPerChunk*2,chunkNumber);
		Collection<List<String>> previousChunkNeg = null;
		Collection<List<String>> previousChunkPos = null;

		Assert.assertEquals(chunkNumber, generator.getChunkNumber());
		
		for(int i=0;i<chunkNumber;++i)
		{
			{
				Collection<List<String>> currentNeg = generator.getAllSequences(i).getData(PTASequenceEngine.truePred);
				Assert.assertEquals("chunk "+i+" (neg) should be of length "+(posOrNegPerChunk*(i+1))+" but it was "+currentNeg.size(),(posOrNegPerChunk*(i+1)), currentNeg.size());
				for(List<String> s:currentNeg)
					Assert.assertTrue("path "+s+" should not exist",graph.paths.tracePath(s) >=0);
				Assert.assertEquals(0, generator.getAllSequences(i).getData().size());// all seq reject ones
				if (previousChunkNeg != null) currentNeg.containsAll(previousChunkNeg);
				previousChunkNeg = currentNeg;
			}
			{
				Collection<List<String>> currentPos = generator.getExtraSequences(i).getData(PTASequenceEngine.truePred);
				Assert.assertEquals("chunk "+i+" (pos) should be of length "+(posOrNegPerChunk*(i+1))+" but it was "+currentPos.size(),(posOrNegPerChunk*(i+1)), currentPos.size());
				Assert.assertEquals((posOrNegPerChunk*(i+1)), generator.getExtraSequences(i).getData().size());// all seq accept ones
				for(List<String> s:currentPos)
					Assert.assertTrue("path "+s+" should exist",graph.paths.tracePath(s) == AbstractOracle.USER_ACCEPTED);
				if (previousChunkPos != null) currentPos.containsAll(previousChunkPos);
				previousChunkPos = currentPos;
			}
		}
		
		checkForCorrectException(new whatToRun() { public void run() {
			generator.getAllSequences(-1);
		}},IllegalArgumentException.class,"is out of range");
		
		checkForCorrectException(new whatToRun() { public void run() {
			generator.getExtraSequences(-1);
		}},IllegalArgumentException.class,"is out of range");

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getAllSequences(chunkNumber);
		}},IllegalArgumentException.class,"is out of range");

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getExtraSequences(chunkNumber);
		}},IllegalArgumentException.class,"is out of range");

		Assert.assertEquals(chunkNumber*posOrNegPerChunk, previousChunkNeg.size());
		Assert.assertEquals(chunkNumber*posOrNegPerChunk, previousChunkPos.size());
		return generator;
	}

	@Test
	public void test_generatePosNeg1()
	{
		RandomPathGenerator generator = generatePosNegTestHelper("A-b->A-a->B\nB-b->D-a->D-c->E-a->E-c->A\nB-c->B\nA-q->A\nA-t->A\nA-r->A\nE-f->F-d->F-e->F","test_generatePosNeg1",
				3,3);
		Assert.assertFalse(generator.getFudgeDetails().toString().contains("FAILED"));
	}

	@Test
	public void test_generatePosNeg2()
	{
		RandomPathGenerator generator = generatePosNegTestHelper("A-b->A-a->B\nB-b->D-a->D-c->E-a->E-c->A\nB-c->B\nA-q->A\nA-t->A\nA-r->A\nE-f->F-d->F-e->F","test_generatePosNeg1",
				3,30);
		Assert.assertTrue(generator.getFudgeDetails().toString().contains("FAILED"));
	}

	@Test(expected=IllegalArgumentException.class)
	public void test_generatePosNeg_fail()
	{
		config.setRandomPathAttemptFudgeThreshold(1);
		generatePosNegTestHelper("A-b->A-a->B\nB-b->D-a->D-c->E-a->E-c->A\nB-c->B\nA-q->A\nA-t->A\nA-r->A\nE-f->F-d->F-e->F","test_generatePosNeg1",
				3,30);
	}

	public RandomPathGenerator generateRandomPosNegHelper(String automaton, String automatonName,final int chunkNumber,int posOrNegPerChunk)
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph(automaton,automatonName),config);
		Assert.assertEquals(4,RandomPathGenerator.diameter(graph));
		final RandomPathGenerator generator = new RandomPathGenerator(graph,new Random(0),8);
		generator.generateRandomPosNeg(posOrNegPerChunk*2,chunkNumber);
		
		Collection<List<String>> previousChunk = null;

		Assert.assertEquals(chunkNumber, generator.getChunkNumber());

		for(int i=0;i<chunkNumber;++i)
		{
			final PTASequenceEngine currentPTA = generator.getAllSequences(i);
			Collection<List<String>> currentSequences = currentPTA.getData(PTASequenceEngine.truePred);
			Assert.assertEquals("chunk "+i+" (neg) should be of length "+(2*posOrNegPerChunk*(i+1))+" but it was "+currentSequences.size(),(2*posOrNegPerChunk*(i+1)), currentSequences.size());
			int positive = 0,negative=0;
			PTASequenceEngine positivePTA = currentPTA.filter(currentPTA.getFSM_filterPredicate());
			PTASequenceEngine negativePTA = currentPTA.filter(new FilterPredicate() {
				FilterPredicate origFilter = currentPTA.getFSM_filterPredicate();
				public boolean shouldBeReturned(Object name) {
					return !origFilter.shouldBeReturned(name);
				}
			});
			for(List<String> s:currentSequences)
				if(graph.paths.tracePath(s) >=0) 
				{
					++negative;
					Assert.assertFalse(positivePTA.containsSequence(s));Assert.assertTrue(negativePTA.containsSequence(s));
				}
				else
				{
					++positive;
					Assert.assertTrue(positivePTA.containsSequence(s));Assert.assertFalse(negativePTA.containsSequence(s));
				}
			Assert.assertEquals(posOrNegPerChunk*(i+1), positive);Assert.assertEquals(positive,positivePTA.getData(PTASequenceEngine.truePred).size());
			Assert.assertEquals(positive,positivePTA.getData().size());// all seq are accept ones
			Assert.assertEquals(posOrNegPerChunk*(i+1), negative);Assert.assertEquals(negative,negativePTA.getData(PTASequenceEngine.truePred).size());
			Assert.assertEquals(0,negativePTA.getData().size());// all seq are reject ones
			if (previousChunk != null) currentSequences.containsAll(previousChunk);
			previousChunk= currentSequences;
		}

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getAllSequences(-1);
		}},IllegalArgumentException.class,"is out of range");
		
		checkForCorrectException(new whatToRun() { public void run() {
			generator.getExtraSequences(-1);
		}},IllegalArgumentException.class,"is out of range");

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getAllSequences(chunkNumber);
		}},IllegalArgumentException.class,"is out of range");

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getExtraSequences(chunkNumber);
		}},IllegalArgumentException.class,"is out of range");
		
		Assert.assertEquals(chunkNumber*posOrNegPerChunk*2, previousChunk.size());
		return generator;
	}
	
	@Test
	public void test_generateRandomPosNeg1()
	{
		RandomPathGenerator generator = generateRandomPosNegHelper("A-b->A-a->B\nB-b->D-a->D-c->E-a->E-c->A\nB-c->B\nA-q->A\nA-t->A\nA-r->A\nE-f->F-d->F-e->F\nA-g->A-h->A","test_generateRandomPosNeg1",
				4,3);
		Assert.assertFalse(generator.getFudgeDetails().toString().contains("FAILED"));
	}
	
	/** A slightly more difficult automaton than the above. */
	@Test
	public void test_generateRandomPosNeg2()
	{
		RandomPathGenerator generator = generateRandomPosNegHelper("A-b->A-a->B\nB-b->D-a->D-c->E-a->E-c->A\nB-c->B\nA-q->A\nA-t->A\nA-r->A\nE-f->F-d->F","test_generateRandomPosNeg2",
				18,12);
		Assert.assertTrue(generator.getFudgeDetails().toString().contains("FAILED"));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void test_generateRandomPosNeg_fail1()
	{
		config.setRandomPathAttemptFudgeThreshold(1);
		generateRandomPosNegHelper("A-b->A-a->B\nB-b->D-a->D-c->E-a->E-c->A\nB-c->B\nA-q->A\nA-t->A\nA-r->A\nE-f->F-d->F","test_generateRandomPosNeg2",
				18,12);
	}

	@Test
	public void test_generatePosNeg_failA()
	{
		final int chunkNumber = 18,posOrNegPerChunk=12;
				
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->B\nB-b->D-a->D-c->E-a->E-c->A\nB-c->B\nA-q->A\nA-t->A\nA-r->A\nE-f->F-d->F","test_generateRandomPosNeg2"),config);
		final RandomPathGenerator generator = new RandomPathGenerator(graph,new Random(0),8);
		Assert.assertEquals(0, generator.getChunkNumber());
		generator.generatePosNeg(posOrNegPerChunk*2,chunkNumber);
		Assert.assertEquals(chunkNumber, generator.getChunkNumber());
		config.setRandomPathAttemptFudgeThreshold(1);
		
		try	{ generator.generatePosNeg(posOrNegPerChunk*2,chunkNumber);Assert.fail("exception not thrown"); }
		catch(IllegalArgumentException ex) { Assert.assertEquals(0, generator.getChunkNumber()); }

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getAllSequences(-1);
		}},IllegalArgumentException.class,"is out of range");
		
		checkForCorrectException(new whatToRun() { public void run() {
			generator.getExtraSequences(-1);
		}},IllegalArgumentException.class,"is out of range");

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getAllSequences(0);
		}},IllegalArgumentException.class,"is out of range");

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getExtraSequences(0);
		}},IllegalArgumentException.class,"is out of range");

		config.setRandomPathAttemptFudgeThreshold(10);
		generator.generatePosNeg(posOrNegPerChunk*2,chunkNumber);
		Assert.assertEquals(chunkNumber, generator.getChunkNumber());
		
		checkForCorrectException(new whatToRun() { public void run() {
			generator.getAllSequences(-1);
		}},IllegalArgumentException.class,"is out of range");
		
		checkForCorrectException(new whatToRun() { public void run() {
			generator.getExtraSequences(-1);
		}},IllegalArgumentException.class,"is out of range");

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getAllSequences(chunkNumber);
		}},IllegalArgumentException.class,"is out of range");

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getExtraSequences(chunkNumber);
		}},IllegalArgumentException.class,"is out of range");

		generator.getAllSequences(chunkNumber/2);generator.getExtraSequences(chunkNumber/2);
	}

	@Test
	public void test_generateRandomPosNeg_failA()
	{
		final int chunkNumber = 18,posOrNegPerChunk=12;
				
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-b->A-a->B\nB-b->D-a->D-c->E-a->E-c->A\nB-c->B\nA-q->A\nA-t->A\nA-r->A\nE-f->F-d->F","test_generateRandomPosNeg2"),config);
		final RandomPathGenerator generator = new RandomPathGenerator(graph,new Random(0),8);
		Assert.assertEquals(0, generator.getChunkNumber());
		generator.generateRandomPosNeg(posOrNegPerChunk*2,chunkNumber);
		Assert.assertEquals(chunkNumber, generator.getChunkNumber());
		config.setRandomPathAttemptFudgeThreshold(1);
		
		try	{ generator.generateRandomPosNeg(posOrNegPerChunk*2,chunkNumber);Assert.fail("exception not thrown"); }
		catch(IllegalArgumentException ex) { Assert.assertEquals(0, generator.getChunkNumber()); }

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getAllSequences(-1);
		}},IllegalArgumentException.class,"is out of range");
		
		checkForCorrectException(new whatToRun() { public void run() {
			generator.getExtraSequences(-1);
		}},IllegalArgumentException.class,"is out of range");

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getAllSequences(0);
		}},IllegalArgumentException.class,"is out of range");

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getExtraSequences(0);
		}},IllegalArgumentException.class,"is out of range");
		
		config.setRandomPathAttemptFudgeThreshold(10);
		generator.generateRandomPosNeg(posOrNegPerChunk*2,chunkNumber);
		Assert.assertEquals(chunkNumber, generator.getChunkNumber());
		
		checkForCorrectException(new whatToRun() { public void run() {
			generator.getAllSequences(-1);
		}},IllegalArgumentException.class,"is out of range");
		
		checkForCorrectException(new whatToRun() { public void run() {
			generator.getExtraSequences(-1);
		}},IllegalArgumentException.class,"is out of range");

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getAllSequences(chunkNumber);
		}},IllegalArgumentException.class,"is out of range");

		checkForCorrectException(new whatToRun() { public void run() {
			generator.getExtraSequences(chunkNumber);
		}},IllegalArgumentException.class,"is out of range");
		
		generator.getAllSequences(chunkNumber/2);generator.getExtraSequences(chunkNumber/2);
	}
	
}
