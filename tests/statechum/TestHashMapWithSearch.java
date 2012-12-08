package statechum;

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;

import java.io.IOException;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.collections.HashMapWithSearch;

public class TestHashMapWithSearch {
	
	int key[]=new int[50000];//179177];
	double value[]=new double[key.length];
	
	@Before
	public void BeforeTest()
	{
		Random rnd = new Random(0);
		for(int i=0;i<key.length;++i)
		{
			key[i]=rnd.nextInt();value[i]=rnd.nextDouble();
		}
	}
	
	protected static <V>  Set<V> collectionAsSet(Collection<V> c) 
	{
		Set<V> outcome = new TreeSet<V>();outcome.addAll(c);return outcome;
	}
	
	private  <K,V>  void compareForEquality(Map<K,V> realMap,Map<K,V> ourMap)
	{
		Assert.assertTrue(realMap.equals(ourMap));
		Assert.assertTrue(ourMap.equals(realMap));
		Assert.assertEquals(realMap.hashCode(), ourMap.hashCode());
		Assert.assertEquals(realMap.toString(),ourMap.toString());
		Assert.assertEquals(realMap.size(),ourMap.size());

		Assert.assertTrue(ourMap.keySet().equals(realMap.keySet()));
		Assert.assertTrue(realMap.keySet().equals(ourMap.keySet()));

		Set<V> realValues = new TreeSet<V>();realValues.addAll(realMap.values());
		Assert.assertTrue(realValues.equals(ourMap.values()));
		//Assert.assertTrue(ourMap.values().equals(realValues));// this one is very slow because we have to go through the entire list for each element of the collection, giving it a quadratic performance in the number of elements.
		if (realMap.isEmpty())
		{
			Assert.assertTrue(ourMap.isEmpty());Assert.assertEquals(0,ourMap.size());
			Assert.assertTrue(ourMap.values().isEmpty());Assert.assertEquals(0,ourMap.size());
		}
		
		Assert.assertEquals(realMap.keySet().hashCode(), ourMap.keySet().hashCode());
		Assert.assertEquals(realMap.keySet().toString(),ourMap.keySet().toString());
		Assert.assertEquals(ourMap.size(),ourMap.keySet().size());

		Assert.assertEquals(realValues.hashCode(), ourMap.values().hashCode());
		Assert.assertEquals(realMap.values().toString(), ourMap.values().toString());
		Assert.assertEquals(ourMap.size(),ourMap.values().size());

		Assert.assertTrue(ourMap.entrySet().equals(realMap.entrySet()));
		Assert.assertTrue(realMap.entrySet().equals(ourMap.entrySet()));
		Assert.assertEquals(realMap.entrySet().hashCode(), ourMap.entrySet().hashCode());
		Assert.assertEquals(realMap.entrySet().toString(),ourMap.entrySet().toString());
		Assert.assertEquals(ourMap.size(),ourMap.entrySet().size());
	}
	
	@Test
	public void TestHashMapWithSearch1()
	{
		LinkedHashMap<Integer,Double> realMap = new LinkedHashMap<Integer,Double>();
		HashMapWithSearch<Integer,Double> ourMap = new HashMapWithSearch<Integer,Double>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY);
		Assert.assertEquals(ourMap.keySet(),realMap.keySet());
		
		Set<Double> realValues = new TreeSet<Double>();realValues.addAll(realMap.values());
		Assert.assertEquals(realValues,collectionAsSet(ourMap.values()));
		Assert.assertEquals(collectionAsSet(ourMap.values()),realValues);
		Assert.assertEquals(ourMap.entrySet(),realMap.entrySet());

		Assert.assertTrue(ourMap.isEmpty());
		for(int i=0;i<key.length;++i)
		{
			realMap.put(key[i], value[i]);ourMap.put(key[i], value[i]);
		}

		compareForEquality(realMap,ourMap);
	}
	
	/** Similar to above but where entries are changed. */
	@Test
	public void TestHashMapWithSearch_update()
	{
		LinkedHashMap<Integer,Double> realMap = new LinkedHashMap<Integer,Double>();
		HashMapWithSearch<Integer,Double> ourMap = new HashMapWithSearch<Integer,Double>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY);
		for(int i=0;i<key.length;++i)
		{
			realMap.put(key[i], value[i]);ourMap.put(key[i], value[i]);
		}

		Random rnd = new Random(1);
		for(int i=0;i<10;++i)
		{
			int idx = rnd.nextInt(key.length);
			Assert.assertEquals(value[idx],ourMap.get(key[idx]));
			Assert.assertEquals(value[idx],realMap.get(key[idx]));

			double val = rnd.nextDouble();
			realMap.put(key[idx],val);ourMap.put(key[idx],val);value[idx]=val;

			compareForEquality(realMap,ourMap);
		}
	}
	
	
	/** Similar to above but where entries are changed. */
	@Test
	public void TestHashMapWithSearch_remove()
	{
		LinkedHashMap<Integer,Double> realMap = new LinkedHashMap<Integer,Double>();
		HashMapWithSearch<Integer,Double> ourMap = new HashMapWithSearch<Integer,Double>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY);
		for(int i=0;i<key.length;++i)
		{
			realMap.put(key[i], value[i]);ourMap.put(key[i], value[i]);
		}

		Random rnd = new Random(1);
		for(int i=0;i<20;++i)
		{
			int idx = rnd.nextInt(key.length);
			
			if (realMap.containsKey(key[idx]))
			{
				Assert.assertEquals(value[idx],realMap.get(key[idx]));
				realMap.remove(key[idx]);ourMap.remove(key[idx]);
			}
			else
				Assert.assertFalse(ourMap.containsKey(key[idx]));
			
			compareForEquality(realMap,ourMap);
		}
	}
	
	/** Similar to above but where entries are changed. */
	@Test
	public void TestHashMapWithSearch_add_remove()
	{
		LinkedHashMap<Integer,Double> realMap = new LinkedHashMap<Integer,Double>();
		HashMapWithSearch<Integer,Double> ourMap = new HashMapWithSearch<Integer,Double>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY);
		for(int i=0;i<key.length;++i)
		{
			realMap.put(key[i], value[i]);ourMap.put(key[i], value[i]);
		}

		Random rnd = new Random(1);
		for(int i=0;i<20;++i)
		{
			int idx = rnd.nextInt(key.length);
			
			int rndKey = rnd.nextInt();double val = rnd.nextDouble();
			if (realMap.containsKey(key[idx]))
			{
				Assert.assertEquals(value[idx],realMap.get(key[idx]));
				key[idx]=rndKey;value[idx]=val;
				realMap.put(key[idx],value[idx]);ourMap.put(key[idx],value[idx]);
			}
			else
				Assert.assertFalse(ourMap.containsKey(key[idx]));
			
			compareForEquality(realMap,ourMap);
		}
	}
	
	/** Similar to above but where entries are changed. */
	@Test
	public void TestHashMapWithSearch_putAll()
	{
		LinkedHashMap<Integer,Double> realMap = new LinkedHashMap<Integer,Double>();
		HashMapWithSearch<Integer,Double> ourMap = new HashMapWithSearch<Integer,Double>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY);
		for(int i=0;i<key.length;++i)
		{
			realMap.put(key[i], value[i]);ourMap.put(key[i], value[i]);
		}

		HashMapWithSearch<Integer,Double> ourMap2 = new HashMapWithSearch<Integer,Double>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY);
		ourMap2.putAll(ourMap);
		LinkedHashMap<Integer,Double> realMap2 = new LinkedHashMap<Integer,Double>();
		realMap2.putAll(realMap);
		compareForEquality(realMap,ourMap);
		compareForEquality(realMap2,ourMap2);
		
		
		
		Random rnd = new Random(1);
		for(int i=0;i<20;++i)
		{
			int idx = rnd.nextInt(key.length);
			
			int rndKey = rnd.nextInt();double val = rnd.nextDouble();
			if (realMap.containsKey(key[idx]))
			{
				Assert.assertEquals(value[idx],realMap.get(key[idx]));
				key[idx]=rndKey;value[idx]=val;
				realMap.put(key[idx],value[idx]);ourMap.put(key[idx],value[idx]);
			}
			else
				Assert.assertFalse(ourMap.containsKey(key[idx]));
		}
		
		// Now place all values from the second map to the first one.
		compareForEquality(realMap,ourMap);
		compareForEquality(realMap2,ourMap2);
		
		realMap2.putAll(realMap);ourMap2.putAll(ourMap);
		compareForEquality(realMap,ourMap);
		compareForEquality(realMap2,ourMap2);
	}
	
	/** Similar to above but where entries are changed. */
	@Test
	public void TestHashMapWithSearch_clear()
	{
		LinkedHashMap<Integer,Double> realMap = new LinkedHashMap<Integer,Double>();
		HashMapWithSearch<Integer,Double> ourMap = new HashMapWithSearch<Integer,Double>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY);
		for(int i=0;i<key.length;++i)
		{
			realMap.put(key[i], value[i]);ourMap.put(key[i], value[i]);
		}
		compareForEquality(realMap,ourMap);
		ourMap.clear();realMap.clear();		
		compareForEquality(realMap,ourMap);
		
	}
		
	/** Similar to above but where entries almost all entries are removed. */
	@Test
	public void TestHashMapWithSearch_remove2()
	{
		LinkedHashMap<Integer,Double> realMap = new LinkedHashMap<Integer,Double>();
		HashMapWithSearch<Integer,Double> ourMap = new HashMapWithSearch<Integer,Double>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY);
		int cntMissing = 0;
		
		for(int i=0;i<key.length;++i)
		{
			realMap.put(key[i], value[i]);ourMap.put(key[i], value[i]);
		}

		Random rnd = new Random(1);
		for(int i=0;i<20000;++i)
		{
			int idx = rnd.nextInt(key.length);
			
			if (realMap.containsKey(key[idx]))
			{
				Assert.assertEquals(value[idx],realMap.get(key[idx]));
				realMap.remove(key[idx]);ourMap.remove(key[idx]);
			}
			else
			{
				++cntMissing;
				Assert.assertFalse(ourMap.containsKey(key[idx]));
			}
			
		}
		
		compareForEquality(realMap,ourMap);
		
		if (cntMissing < 1000)
			Assert.fail("too few checks for containsKey");
	}

	@Test
	public void TestContainsValue()
	{
		LinkedHashMap<Integer,Double> realMap = new LinkedHashMap<Integer,Double>();
		HashMapWithSearch<Integer,Double> ourMap = new HashMapWithSearch<Integer,Double>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY);
		int cntMissing = 0;
		
		for(int i=0;i<key.length;++i)
		{
			realMap.put(key[i], value[i]);ourMap.put(key[i], value[i]);
		}

		Random rnd = new Random(1);
		for(int i=0;i<20000;++i)
		{
			int idx = rnd.nextInt(key.length);
			
			if (realMap.containsValue(value[idx]))
			{
				Assert.assertEquals(value[idx],realMap.get(key[idx]));
				realMap.remove(key[idx]);ourMap.remove(key[idx]);
			}
			else
			{
				++cntMissing;
				Assert.assertFalse(ourMap.containsValue(value[idx]));
			}
			
		}
		
		compareForEquality(realMap,ourMap);
		
		if (cntMissing < 1000)
			Assert.fail("too few checks for containsKey");
	}
	
	@Test
	public void testKeySetModification()
	{
		HashMapWithSearch<Integer,Double> ourMap = new HashMapWithSearch<Integer,Double>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY);
		ourMap.put(3,4.5);ourMap.put(2, 9.);
		TreeSet<Integer> keys = new TreeSet<Integer>();keys.addAll(ourMap.keySet());
		Assert.assertEquals("[2, 3]",keys.toString());
		ourMap.keySet().remove(2);
		keys = new TreeSet<Integer>();keys.addAll(ourMap.keySet());
		Assert.assertEquals("[3]",keys.toString());
		
	}

	@Test
	public void testInvalidModification2()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException, IOException,IncompatibleStatesException 
			{
				new HashMapWithSearch<Integer,Integer>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY).values().remove(45);
			}
		}, UnsupportedOperationException.class, "modification");
	}

	@Test
	public void testInvalidModification3a()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException, IOException,IncompatibleStatesException 
			{
				new HashMapWithSearch<Integer,Integer>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY).keySet().clear();
			}
		}, UnsupportedOperationException.class, "modification");
	}

	@Test
	public void testInvalidModification3b()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException, IOException,IncompatibleStatesException 
			{
				new HashMapWithSearch<Integer,Integer>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY).values().clear();
			}
		}, UnsupportedOperationException.class, "modification");
	}

	@Test
	public void testInvalidModification4()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException, IOException,IncompatibleStatesException 
			{
				new HashMapWithSearch<Integer,Integer>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY).entrySet().clear();
			}
		}, UnsupportedOperationException.class, "modification");
	}

	@Test
	public void testInvalidModification5()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException, IOException,IncompatibleStatesException 
			{
				HashMapWithSearch<Integer,Integer> h = new HashMapWithSearch<Integer,Integer>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY);h.put(33, 44);
				h.entrySet().iterator().remove();
			}
		}, UnsupportedOperationException.class, "modification");
	}

	@Test
	public void testFindByID1()
	{
		LearnerGraph g = buildLearnerGraph("A-a->A-b->B",	"testToBooleans",Configuration.getDefaultConfiguration());
		Assert.assertEquals("A",g.findVertex(VertexID.parseID("A")).getID().getStringId());
		Assert.assertEquals("B",g.findVertex(VertexID.parseID("B")).getID().getStringId());
		Assert.assertNull(g.findVertex(VertexID.parseID("AB")));
	}

}
