package statechum.xmachine.model.testset;

import static statechum.analysis.learning.TestFSMAlgo.buildSet;

import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import junit.framework.Assert;

import org.junit.Test;

public class TestPrefixRemovingCollection 
{
	@Test 
	public final void testPrefixRemovingCollection0()
	{
		PTASequenceSet c = new PTASequenceSet();
		Assert.assertEquals(0, c.size());
		Assert.assertFalse(c.getData().iterator().hasNext());
		Assert.assertTrue(c.getData().isEmpty());
	}
	
	@Test 
	public final void testPrefixRemovingCollection1()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<String>());
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testPrefixRemovingCollection2()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<String>());
		c.addSequence(new LinkedList<String>());
		c.addSequence(new LinkedList<String>());
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testPrefixRemovingCollection3()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<String>());
		c.addSequence(Arrays.asList(new String[]{"a"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a"}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testPrefixRemovingCollection4()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<String>());
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{"a","a"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","a"}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testPrefixRemovingCollection5()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<String>());
		c.addSequence(Arrays.asList(new String[]{"a","a"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","a"}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testPrefixRemovingCollection6()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<String>());
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{}));
		c.addSequence(Arrays.asList(new String[]{"a","a"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","a"}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testPrefixRemovingCollection7a()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<String>());
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{}));
		c.addSequence(Arrays.asList(new String[]{"a","b"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testPrefixRemovingCollection7b()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<String>());
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{}));
		c.addSequence(Arrays.asList(new String[]{"a","b"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testPrefixRemovingCollection8()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<String>());
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{}));
		c.addSequence(Arrays.asList(new String[]{"a","b"}));
		c.addSequence(Arrays.asList(new String[]{"a","c","d"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		c.addSequence(Arrays.asList(new String[]{"a"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
				new String[]{"a","c","d"}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(2, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testPrefixRemovingCollection9()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addAll(buildSet(new String[][] {
			new String[]{"a"},
			new String[]{},
			new String[]{"a","b"},
			new String[]{"a","c","d"},
			new String[]{"a"},
			new String[]{"a"}}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
				new String[]{"a","c","d"}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(2, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testPrefixRemovingCollection10()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addAll(buildSet(new String[][] {
			new String[]{"a"},
			new String[]{},
			new String[]{"a","b"}}));
		
		PTASequenceSet d = new PTASequenceSet();
		d.addAll(buildSet(new String[][] {		
			new String[]{"a","c","d"},
			new String[]{"a"},
			new String[]{"a"}}));
		c.addAll(d);
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
				new String[]{"a","c","d"}
		}),actual = new HashSet<List<String>>();actual.addAll(c.getData());
		Assert.assertEquals(2, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
	
	@Test 
	public final void testPrefixRemovingCollection11()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addAll(buildSet(new String[][] {
			new String[]{"a"},
			new String[]{},
			new String[]{"a","b"},
			new String[]{"a","c","d"},
			new String[]{"a"},
			new String[]{"a"}}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
				new String[]{"a","c","d"}
		}),actual = new HashSet<List<String>>();actual.addAll(c);
		Assert.assertEquals(2, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
	}
}
