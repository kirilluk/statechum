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

package statechum.model.testset;

import statechum.Configuration;
import statechum.Label;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.model.testset.PTASequenceSet;
import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;

import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

public class TestPrefixRemovingCollection 
{
	final Configuration config = Configuration.getDefaultConfiguration();
	final ConvertALabel converter = null;
	
	@Test 
	public final void testPrefixRemovingCollection0()
	{
		PTASequenceSet c = new PTASequenceSet();
		Assert.assertEquals(0, c.size());
		Assert.assertFalse(c.getData().iterator().hasNext());
		Assert.assertTrue(c.getData().isEmpty());

		Assert.assertTrue(c.containsAll(c));

		Assert.assertTrue(c.contains(new LinkedList<Label>()));
		Assert.assertFalse(c.contains(labelList(new String[]{"a"})));
	}
	
	@Test 
	public final void testPrefixRemovingCollection1()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<Label>());
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{}
		},config,converter),actual = new HashSet<List<Label>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
		Assert.assertFalse(c.getData().isEmpty());

		Assert.assertTrue(c.contains(new LinkedList<Label>()));
		Assert.assertFalse(c.contains(labelList(new String[]{"a"})));

		Assert.assertTrue(c.containsAll(expected));
		Assert.assertTrue(c.containsAll(c));
	}
	
	@Test 
	public final void testPrefixRemovingCollection2()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<Label>());
		c.addSequence(new LinkedList<Label>());
		c.addSequence(new LinkedList<Label>());
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{}
		},config,converter),actual = new HashSet<List<Label>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertFalse(c.getData().isEmpty());
		Assert.assertTrue(expected.equals(actual));

		Assert.assertTrue(c.contains(new LinkedList<Label>()));
		Assert.assertFalse(c.contains(labelList(new String[]{"a"})));
		
		Assert.assertTrue(c.containsAll(expected));
		Assert.assertTrue(c.containsAll(c));
	}
	
	/** Converts arrays of labels to lists of labels using config - it does not really matter which configuration is used 
	 * because all of them start from a default one and do not modify label type.
	 * 
	 * @param labels what to convert
	 * @return the outcome of conversion.
	 */
	protected List<Label> labelList(String [] labels)
	{
		return AbstractLearnerGraph.buildList(Arrays.asList(labels),config,converter);
	}
	
	@Test 
	public final void testPrefixRemovingCollection3()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<Label>());
		c.addSequence(labelList(new String[]{"a"}));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"a"}
		},config,converter),actual = new HashSet<List<Label>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
		Assert.assertFalse(c.getData().isEmpty());

		Assert.assertTrue(c.contains(new LinkedList<Label>()));
		Assert.assertTrue(c.contains(labelList(new String[]{"a"})));
		Assert.assertFalse(c.contains(labelList(new String[]{"a","a"})));
		
		Assert.assertTrue(c.containsAll(expected));
		Assert.assertTrue(c.containsAll(c));
	}
	
	@Test 
	public final void testPrefixRemovingCollection4()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<Label>());
		c.addSequence(labelList(new String[]{"a"}));
		c.addSequence(labelList(new String[]{"a","a"}));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"a","a"}
		},config,converter),actual = new HashSet<List<Label>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
		Assert.assertFalse(c.getData().isEmpty());

		Assert.assertTrue(c.contains(new LinkedList<Label>()));
		Assert.assertTrue(c.contains(labelList(new String[]{"a"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","a"})));
		Assert.assertFalse(c.contains(labelList(new String[]{"a","a","a"})));
		
		Assert.assertTrue(c.containsAll(expected));
		Assert.assertTrue(c.containsAll(c));
	}
	
	@Test 
	public final void testPrefixRemovingCollection5()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<Label>());
		c.addSequence(labelList(new String[]{"a","a"}));
		c.addSequence(labelList(new String[]{"a"}));
		c.addSequence(labelList(new String[]{}));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"a","a"}
		},config,converter),actual = new HashSet<List<Label>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
		Assert.assertFalse(c.getData().isEmpty());

		Assert.assertTrue(c.contains(new LinkedList<Label>()));
		Assert.assertTrue(c.contains(labelList(new String[]{"a"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","a"})));
		Assert.assertFalse(c.contains(labelList(new String[]{"a","a","a"})));
		
		Assert.assertTrue(c.containsAll(expected));
		Assert.assertTrue(c.containsAll(c));
	}
	
	@Test 
	public final void testPrefixRemovingCollection6()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<Label>());
		c.addSequence(labelList(new String[]{"a"}));
		c.addSequence(labelList(new String[]{}));
		c.addSequence(labelList(new String[]{"a","a"}));
		c.addSequence(labelList(new String[]{"a"}));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"a","a"}
		},config,converter),actual = new HashSet<List<Label>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
		Assert.assertFalse(c.getData().isEmpty());

		Assert.assertTrue(c.contains(new LinkedList<Label>()));
		Assert.assertTrue(c.contains(labelList(new String[]{"a"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","a"})));
		Assert.assertFalse(c.contains(labelList(new String[]{"a","a","a"})));
		
		Assert.assertTrue(c.containsAll(expected));
		Assert.assertTrue(c.containsAll(c));
	}
	
	@Test 
	public final void testPrefixRemovingCollection7a()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<Label>());
		c.addSequence(labelList(new String[]{"a"}));
		c.addSequence(labelList(new String[]{}));
		c.addSequence(labelList(new String[]{"a","b"}));
		c.addSequence(labelList(new String[]{"a"}));
		c.addSequence(labelList(new String[]{"a"}));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
		},config,converter),actual = new HashSet<List<Label>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
		Assert.assertFalse(c.getData().isEmpty());

		Assert.assertTrue(c.contains(new LinkedList<Label>()));
		Assert.assertTrue(c.contains(labelList(new String[]{"a"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","b"})));
		Assert.assertFalse(c.contains(labelList(new String[]{"a","b","a"})));
		
		Assert.assertTrue(c.containsAll(expected));
		Assert.assertTrue(c.containsAll(c));
	}
	
	@Test 
	public final void testPrefixRemovingCollection7b()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<Label>());
		c.addSequence(labelList(new String[]{"a"}));
		c.addSequence(labelList(new String[]{}));
		c.addSequence(labelList(new String[]{"a","b"}));
		c.addSequence(labelList(new String[]{"a"}));
		c.addSequence(labelList(new String[]{"a"}));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
		},config,converter),actual = new HashSet<List<Label>>();actual.addAll(c.getData());
		Assert.assertEquals(1, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
		Assert.assertFalse(c.getData().isEmpty());

		Assert.assertTrue(c.contains(new LinkedList<Label>()));
		Assert.assertTrue(c.contains(labelList(new String[]{"a"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","b"})));
		Assert.assertFalse(c.contains(labelList(new String[]{"a","b","a"})));
		
		Assert.assertTrue(c.containsAll(expected));
		Assert.assertTrue(c.containsAll(c));
	}
	
	@Test 
	public final void testPrefixRemovingCollection8()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addSequence(new LinkedList<Label>());
		c.addSequence(labelList(new String[]{"a"}));
		c.addSequence(labelList(new String[]{}));
		c.addSequence(labelList(new String[]{"a","b"}));
		c.addSequence(labelList(new String[]{"a","c","d"}));
		c.addSequence(labelList(new String[]{"a"}));
		c.addSequence(labelList(new String[]{"a"}));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
				new String[]{"a","c","d"}
		},config,converter),actual = new HashSet<List<Label>>();actual.addAll(c.getData());
		Assert.assertEquals(2, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
		Assert.assertFalse(c.getData().isEmpty());

		Assert.assertTrue(c.contains(new LinkedList<Label>()));
		Assert.assertTrue(c.contains(labelList(new String[]{"a"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","b"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","c"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","c","d"})));
		Assert.assertFalse(c.contains(labelList(new String[]{"a","b","a"})));
		
		Assert.assertTrue(c.containsAll(expected));
		Assert.assertTrue(c.containsAll(c));
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
			new String[]{"a"}},config,converter));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
				new String[]{"a","c","d"}
		},config,converter),actual = new HashSet<List<Label>>();actual.addAll(c.getData());
		Assert.assertEquals(2, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
		Assert.assertFalse(c.getData().isEmpty());

		Assert.assertTrue(c.contains(new LinkedList<Label>()));
		Assert.assertTrue(c.contains(labelList(new String[]{"a"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","b"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","c"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","c","d"})));
		Assert.assertFalse(c.contains(labelList(new String[]{"a","b","a"})));
		
		Assert.assertTrue(c.containsAll(expected));
		Assert.assertTrue(c.containsAll(c));
	}
	
	@Test 
	public final void testPrefixRemovingCollection10()
	{
		PTASequenceSet c = new PTASequenceSet();
		c.addAll(buildSet(new String[][] {
			new String[]{"a"},
			new String[]{},
			new String[]{"a","b"}},config,converter));
		
		PTASequenceSet d = new PTASequenceSet();
		d.addAll(buildSet(new String[][] {		
			new String[]{"a","c","d"},
			new String[]{"a"},
			new String[]{"a"}},config,converter));
		c.addAll(d);
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
				new String[]{"a","c","d"}
		},config,converter),actual = new HashSet<List<Label>>();actual.addAll(c.getData());
		Assert.assertEquals(2, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
		Assert.assertFalse(c.getData().isEmpty());

		Assert.assertTrue(c.contains(new LinkedList<Label>()));
		Assert.assertTrue(c.contains(labelList(new String[]{"a"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","b"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","c"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","c","d"})));
		Assert.assertFalse(c.contains(labelList(new String[]{"a","b","a"})));
		
		Assert.assertTrue(c.containsAll(expected));
		Assert.assertTrue(c.containsAll(c));
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
			new String[]{"a"}},config,converter));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"a","b"},
				new String[]{"a","c","d"}
		},config,converter),actual = new HashSet<List<Label>>();actual.addAll(c);
		Assert.assertEquals(2, c.getData().size());
		Assert.assertTrue(expected.equals(actual));
		Assert.assertFalse(c.getData().isEmpty());

		Assert.assertTrue(c.contains(new LinkedList<Label>()));
		Assert.assertTrue(c.contains(labelList(new String[]{"a"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","b"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","c"})));
		Assert.assertTrue(c.contains(labelList(new String[]{"a","c","d"})));
		Assert.assertFalse(c.contains(labelList(new String[]{"a","b","a"})));
		
		Assert.assertTrue(c.containsAll(expected));
		Assert.assertTrue(c.containsAll(c));
	}
}
