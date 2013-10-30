package collections;

import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.ParameterizedWithName;
import org.junit.runners.ParameterizedWithName.ParametersToString;

import statechum.DeterministicDirectedSparseGraph.VertID.VertKind;
import statechum.Helper;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper.whatToRun;
import statechum.collections.ArrayMapWithSearchPos;
import statechum.collections.ArrayMapWithSearch;
import statechum.collections.ArrayOperations;
import statechum.collections.ConvertibleToInt;
import statechum.collections.HashMapWithSearch;
import statechum.collections.MapWithSearch;

@RunWith(ParameterizedWithName.class)
public class TestMapWithSearch 
{
	private static final int COLLECTIONSIZE = 3000;
	
	CInteger key[]=null;
	long value[]=null;
		
	private Random rndForKeys = new Random(0);
	
	private final int MAXNUMBEROFUNIQUEKEYS = 1 << 17;
	
	/** Generates a small key, so that it will fit in our array. */
	int randomKey()
	{
		int outcome = rndForKeys.nextInt(MAXNUMBEROFUNIQUEKEYS);
		
		if (collectionCreator.supportsNegatives() && rndForKeys.nextBoolean())
			outcome-=outcome;
		
		return outcome;
	}
	
	public void initKeysAndValues()
	{
		key=new CInteger[COLLECTIONSIZE];value=new long[key.length];
		Set<Integer> existingKeys = new HashSet<Integer>(MAXNUMBEROFUNIQUEKEYS);
		for(int i=0;i<key.length;++i)
		{
			int newKey = randomKey();
			while(existingKeys.contains(newKey))// to ensure that keys do not repeat
				newKey = randomKey();
			existingKeys.add(newKey);
			key[i]=new CInteger(newKey);value[i]=rndForKeys.nextLong();
		}
	}
	
	@After
	public void afterTest()
	{
		key=null;value=null;
	}
	
	private final MapCreator collectionCreator;
	
	public TestMapWithSearch(MapCreator collection, @SuppressWarnings("unused") String desc)
	{
		collectionCreator = collection;
	}
	
	public static abstract class MapCreator
	{
		@SuppressWarnings("rawtypes")
		public abstract MapWithSearch createMap();
		
		public boolean supportsNegatives()
		{
			return true;
		}
		
	}
	
	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		result.add(new Object[]{new MapCreator() {
			
			@SuppressWarnings("rawtypes")
			@Override
			public MapWithSearch createMap() {
				return new HashMapWithSearch(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY);
			}
		},"HashMapWithSearch"});
		result.add(new Object[]{new MapCreator() {
			
			@SuppressWarnings("rawtypes")
			@Override
			public MapWithSearch createMap() {
				return new ArrayMapWithSearch();
			}
		},"ArrayMapWithSearch"});
		result.add(new Object[]{new MapCreator() {
			
			@SuppressWarnings("rawtypes")
			@Override
			public MapWithSearch createMap() {
				return new ArrayMapWithSearchPos();
			}
			@Override
			public boolean supportsNegatives()
			{
				return false;
			}
		},"ArrayMapWithSearchPos"});
		return result;
	}
	
	@ParametersToString
	public static String parametersToString(@SuppressWarnings("unused") MapCreator creator, String desc)
	{
		return "tests for "+desc;
	}
	
	@SuppressWarnings("unchecked")
	protected <K extends ConvertibleToInt,V> MapWithSearch<K,V> createOurMap()
	{/*
		MapWithSearch<K,V> outcome = null;
		try {
			outcome = (MapWithSearch<K,V>)collectionToUse.newInstance();
		} catch (Exception e) {
			Helper.throwUnchecked("failed to create an instance of a collection", e);
		}
		return outcome;*/
		return collectionCreator.createMap();
	}
	
	protected static <V>  Set<V> collectionAsSet(Collection<V> c) 
	{
		Set<V> outcome = new TreeSet<V>();outcome.addAll(c);return outcome;
	}
	
	static <K,V>  void compareForEquality(Map<K,V> realMap,Map<K,V> ourMap)
	{
		Assert.assertTrue(realMap.equals(ourMap));
		Assert.assertTrue(ourMap.equals(realMap));
		Assert.assertEquals(realMap.hashCode(), ourMap.hashCode());
		//Assert.assertEquals(realMap.toString(),ourMap.toString());// this may fail where the order of elements is different between the two maps, hence effectively delegated to comparing copies of the two maps into maps with identical ordering. 
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
		//Assert.assertEquals(realMap.keySet().toString(),ourMap.keySet().toString());// this may fail where the order of elements is different between the two maps, hence effectively delegated to comparing copies of the two maps into maps with identical ordering.
		Assert.assertEquals(ourMap.size(),ourMap.keySet().size());

		Assert.assertEquals(realValues.hashCode(), ourMap.values().hashCode());
		//Assert.assertEquals(realMap.values().toString(), ourMap.values().toString());// this may fail where the order of elements is different between the two maps, hence effectively delegated to comparing copies of the two maps into maps with identical ordering.
		Assert.assertEquals(ourMap.size(),ourMap.values().size());

		Assert.assertTrue(ourMap.entrySet().equals(realMap.entrySet()));
		Assert.assertTrue(realMap.entrySet().equals(ourMap.entrySet()));
		Assert.assertEquals(realMap.entrySet().hashCode(), ourMap.entrySet().hashCode());
		//Assert.assertEquals(realMap.entrySet().toString(),ourMap.entrySet().toString());// this may fail where the order of elements is different between the two maps, hence effectively delegated to comparing copies of the two maps into maps with identical ordering.
		Assert.assertEquals(ourMap.size(),ourMap.entrySet().size());
		
		TreeMap<K,V> copyOfOurMap=new TreeMap<K,V>(),copyOfRealMap=new TreeMap<K,V>();
		copyOfOurMap.putAll(ourMap);copyOfRealMap.putAll(realMap);
		Assert.assertEquals(copyOfRealMap,copyOfOurMap);
		Assert.assertEquals(copyOfRealMap.toString(),copyOfOurMap.toString());
	}
	
	public static class AttemptIterator<G> implements Iterator<G>
	{
		private final Iterator<G> real, ours;
		
		public AttemptIterator(Iterator<G> r,Iterator<G> o)
		{
			real = r;ours = o;
		}
		
		@Override
		public boolean hasNext() {
			boolean has = real.hasNext();
			Assert.assertEquals(has,ours.hasNext());return has;
		}

		@Override
		public G next() {
			G r = null, o = null;
			boolean rEx=false,oEx=false;
			try
			{
				r=real.next();
			}
			catch(NoSuchElementException e)
			{
				rEx = true;
			}
			
			try
			{
				o=ours.next();
			}
			catch(NoSuchElementException e)
			{
				oEx = true;
			}
			
			if (rEx)
			{
				Assert.assertTrue("exception not thrown by our collection",oEx);
				throw new NoSuchElementException();
			}

			Assert.assertFalse("unexpected exception by our collection",oEx);
			Assert.assertEquals(r,o);
			return r;
		}

		@Override
		public void remove() {
			real.remove();ours.remove();
		}
		
		@Override
		public String toString()
		{
			String r = real.toString(), o = ours.toString();
			Assert.assertEquals(r,o);return r;
		}
	} // AttemptIterator<K>

	public static class AttemptCollection<K> implements Collection<K> 
	{
		private final Collection<K> real,ours; 
		
		public AttemptCollection(Collection<K> r,Collection<K> o)
		{
			real = r;ours = o;
		}
		
		@Override
		public int size() {
			int s=real.size();
			Assert.assertEquals(s,ours.size());return s;
		}

		@Override
		public boolean isEmpty() {
			boolean em = real.isEmpty();
			Assert.assertEquals(em,ours.isEmpty());return em;
		}

		@Override
		public boolean contains(Object obj) {
			boolean c = real.contains(obj);
			Assert.assertEquals(c, ours.contains(obj));return c;
		}

		@Override
		public Iterator<K> iterator() {
			return new AttemptIterator<K>(real.iterator(), ours.iterator());
		}

		@Override
		public Object[] toArray() 
		{
			Object []oR = real.toArray(), oO = ours.toArray();
			StringBuffer errMsgBuffer = new StringBuffer();
			boolean outcome = ArrayOperations.cmp(oR,oO,errMsgBuffer);
			Assert.assertTrue(errMsgBuffer.toString(), outcome);
			return oR;
		}

		@Override
		public <T> T[] toArray(T[] a) 
		{
			T []copyOfA = Arrays.copyOf(a, a.length);
			T []oR = real.toArray(a), oO = ours.toArray(copyOfA);
			StringBuffer errMsgBuffer = new StringBuffer();
			boolean outcome = ArrayOperations.cmp(a,copyOfA,errMsgBuffer);
			Assert.assertTrue(errMsgBuffer.toString(), outcome);
			errMsgBuffer = new StringBuffer();
			outcome = ArrayOperations.cmp(oR,oO,errMsgBuffer);
			Assert.assertTrue(errMsgBuffer.toString(), outcome);
			return oR;
		}

		@Override
		public boolean add(@SuppressWarnings("unused") K elem) 
		{
			throw new UnsupportedOperationException("no support for add in a Set");
		}

		@Override
		public boolean remove(Object obj) 
		{
			boolean result = real.remove(obj);
			Assert.assertEquals(result,ours.remove(obj));return result;
		}

		@Override
		public boolean containsAll(Collection<?> c) 
		{
			boolean contains = real.containsAll(c);
			Assert.assertEquals(contains,ours.containsAll(c));return contains;
		}

		@Override
		public boolean addAll(@SuppressWarnings("unused") Collection<? extends K> c) {
			throw new UnsupportedOperationException("no support for addAll in a Set");
		}

		@Override
		public boolean retainAll(Collection<?> c) {
			boolean contains = real.retainAll(c);
			Assert.assertEquals(contains,ours.retainAll(c));return contains;
		}

		@Override
		public boolean removeAll(Collection<?> c) {
			boolean contains = real.removeAll(c);
			Assert.assertEquals(contains,ours.removeAll(c));return contains;
		}

		@Override
		public void clear() {
			real.clear();ours.clear();
		}

		@Override
		public String toString()
		{
			String r = real.toString(), o = ours.toString();
			Assert.assertEquals(r,o);return r;
		}
	} // AttemptCollection<K>
	
	public static class AttemptSet<K> extends AttemptCollection<K> implements Set<K> 
	{

		public AttemptSet(Collection<K> r, Collection<K> o) 
		{
			super(r, o);
		}
	} // AttemptSet<K>

	public static class Attempt<K,V>  implements Map<K,V>
	{
		private final Map<K,V> real,ours;
		
		public Map<K,V> getOurs()
		{
			return ours;
		}
		
		public void compareForEquality()
		{
			TestMapWithSearch.compareForEquality(real,ours);
		}
		
		public Attempt(Map<K,V> r, Map<K,V> o)
		{
			real=r;ours=o;
		}
		
		@Override
		public int size() {
			int r=real.size();
			Assert.assertEquals(r, ours.size());return r;
		}

		@Override
		public boolean isEmpty() {
			boolean r = real.isEmpty();
			Assert.assertEquals(r, ours.isEmpty());return r;
		}

		@Override
		public boolean containsKey(Object key) {
			boolean r = real.containsKey(key);
			Assert.assertEquals(r,ours.containsKey(key));return r;
		}

		@Override
		public boolean containsValue(Object value) {
			boolean r = real.containsValue(value);
			Assert.assertEquals(r,ours.containsValue(value));return r;
		}

		@Override
		public V get(Object key) {
			V r = real.get(key);
			Assert.assertEquals(r,ours.get(key));return r;
		}

		@Override
		public V put(K key, V value) {
			V r = real.put(key, value),o=ours.put(key, value);
			Assert.assertEquals(r, o);
			return r;
		}

		@Override
		public V remove(Object key) {
			V r = real.remove(key),o=ours.remove(key);
			Assert.assertEquals(r, o);
			return r;
		}

		@Override
		public void putAll(Map<? extends K, ? extends V> m) {
			real.putAll(m);ours.putAll(m);
		}

		@Override
		public void clear() {
			real.clear();ours.clear();
		}
		
		
		@Override
		public Set<K> keySet() 
		{
			final Set<K> r = real.keySet(), o = ours.keySet();
			return new AttemptSet<K>(r,o);
		}

		@Override
		public Collection<V> values() 
		{
			final Collection<V> r = real.values(), o = ours.values();
			return new AttemptCollection<V>(r,o);
		}

		@Override
		public Set<java.util.Map.Entry<K, V>> entrySet() 
		{
			Set<java.util.Map.Entry<K, V>> r=real.entrySet(),o=ours.entrySet();
			return new AttemptSet<java.util.Map.Entry<K, V>>(r,o);
		}

		@Override
		public String toString()
		{
			String r = real.toString(), o = ours.toString();
			Assert.assertEquals(r,o);return r;
		}
	}
	
	/** An integer that reports its value via {@link ConvertibleToInt}.
	 */
	public static class CInteger implements ConvertibleToInt, Comparable<CInteger>
	{
		private final int value;
		
		public CInteger(int v)
		{
			value = v;
		}
		
		@Override
		public int toInt() {
			return value;
		}

		/* This one should directly compare the two values, not compute a difference between them, because in the case where the values are close
		 * to the boundary for what fits into an int, the difference will overflow. 
		 */
		@Override
		public int compareTo(CInteger o) 
		{
			if (value < o.value)
				return -1;
			if (value > o.value)
				return 1;
			return 0;
		}
		
		@Override
		public String toString()
		{
			return Integer.toString(value);
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + value;
			return result;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj instanceof CInteger))
				return false;
			CInteger other = (CInteger) obj;
			if (value != other.value)
				return false;
			return true;
		}
	}
	
	private <K extends ConvertibleToInt,V> Attempt<K,V> createMap()
	{
		MapWithSearch<K,V> ours = createOurMap(); 
		return new Attempt<K,V>(new TreeMap<K,V>(),ours);
	}
	
	private <K extends ConvertibleToInt,V> Attempt<K,V> createMapOne(K k,V v)
	{
		MapWithSearch<K,V> ours = createOurMap(); 
		Attempt<K,V> outcome = new Attempt<K,V>(new TreeMap<K,V>(),ours);
		outcome.put(k,v);return outcome;
	}
	
	private <K extends ConvertibleToInt,V> Attempt<K,V> createMapTwo(K kOne,V vOne, K kTwo, V vTwo)
	{
		MapWithSearch<K,V> ours = createOurMap(); 
		Attempt<K,V> outcome = new Attempt<K,V>(new TreeMap<K,V>(),ours);
		outcome.put(kOne,vOne);outcome.put(kTwo,vTwo);return outcome;
	}
	
	public static final CInteger keyOne = new CInteger(22), keyTwo = new CInteger(44);
	public static final Long valueOne = 9L, valueTwo=7L;
	
	private <K,V> void checkUnsupportedOperationsOnEntrySet(final Map<K,V> map)
	{
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.entrySet().iterator().remove();
		}},UnsupportedOperationException.class,"modification of iterator");
		
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.entrySet().add(null);
		}},UnsupportedOperationException.class,"modification of entry set");

		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.entrySet().addAll(null);
		}},UnsupportedOperationException.class,"modification of entry set");

		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.entrySet().remove(null);
		}},UnsupportedOperationException.class,"modification of entry set");
		
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.entrySet().removeAll(map.entrySet());
		}},UnsupportedOperationException.class,"modification of entry set");
		
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.entrySet().retainAll(map.entrySet());
		}},UnsupportedOperationException.class,"modification of entry set");
		
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.entrySet().clear();
		}},UnsupportedOperationException.class,"modification of entry set");		
	}
	
	private <K,V> void checkUnsupportedOperationsOnValueSet(final Map<K,V> map)
	{
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.values().iterator().remove();
		}},UnsupportedOperationException.class,"modification of iterator");
		
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.values().add(null);
		}},UnsupportedOperationException.class,"modification of value set");

		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.values().addAll(null);
		}},UnsupportedOperationException.class,"modification of value set");

		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.values().remove(null);
		}},UnsupportedOperationException.class,"modification of value set");
		
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.values().removeAll(null);
		}},UnsupportedOperationException.class,"modification of value set");
		
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.values().retainAll(null);
		}},UnsupportedOperationException.class,"modification of value set");
		
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.values().clear();
		}},UnsupportedOperationException.class,"modification of value set");
	}
	
	private <K,V> void checkUnsupportedOperationsOnKeySet(final Map<K,V> map)
	{
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.keySet().add(null);
		}},UnsupportedOperationException.class,"modification of key set");
		
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.keySet().addAll(null);
		}},UnsupportedOperationException.class,"modification of key set");
	}
	
	private <K,V> void checkArrayWithNoPairs(final Attempt<K,V> map)
	{
		Assert.assertEquals(0,map.size());Assert.assertTrue(map.isEmpty());
		Assert.assertEquals("{}",map.toString());
		map.compareForEquality();
		{// entry set
			Assert.assertEquals("[]",map.entrySet().toString());
			Assert.assertTrue(map.entrySet().isEmpty());Assert.assertEquals(0,map.entrySet().size());

			{// the two collections have entry sets of different types, hence we cannot meaningfully compare them other then via toString.
				Object data []= map.getOurs().entrySet().toArray();
				Assert.assertEquals(0,data.length);
			}
			{
				Object data10[]= new Object[10];
				Object data10returned[]=map.getOurs().entrySet().toArray(data10);Assert.assertSame(data10,data10returned);
				for(int i=0;i<data10.length;++i) Assert.assertNull(data10[i]);
			}
			
			{
				Object data1[]= new Object[1];
				Object data10returned[]=map.getOurs().entrySet().toArray(data1);Assert.assertSame(data1,data10returned);
				for(int i=0;i<data1.length;++i) Assert.assertNull(data1[i]);
			}
			
			{// this one will not fit in an array
				Object data0[]= new Object[0];
				Object data0returned[]=map.getOurs().entrySet().toArray(data0);Assert.assertSame(data0, data0returned);
				Assert.assertEquals(0,data0returned.length);
			}
			
			checkUnsupportedOperationsOnEntrySet(map.getOurs());
		}
		
		{// values
			Assert.assertEquals("[]",map.values().toString());
			Assert.assertTrue(map.values().isEmpty());Assert.assertEquals(0,map.values().size());
			Assert.assertEquals(0,map.values().toArray().length);
			
			{
				Object data10[]= new Object[10];
				Object data10returned[]=map.values().toArray(data10);Assert.assertSame(data10,data10returned);
				for(int i=0;i<data10.length;++i) Assert.assertNull(data10[i]);
			}
			
			{
				Object data1[]= new Object[1];
				Object data1returned[]=map.values().toArray(data1);Assert.assertSame(data1,data1returned);
				for(int i=0;i<data1.length;++i) Assert.assertNull(data1[i]);
			}
			
			{
				Object data0[]= new Object[0];
				Object data0returned[]=map.values().toArray(data0);Assert.assertSame(data0, data0returned);
			}

			checkUnsupportedOperationsOnValueSet(map.getOurs());
		}
		{// keys
			Assert.assertEquals("[]",map.keySet().toString());
			Assert.assertTrue(map.keySet().isEmpty());Assert.assertEquals(0,map.keySet().size());

			Assert.assertEquals(0,map.keySet().toArray().length);

			{
				Object data10[]= new Object[10];
				Object data10returned[]=map.keySet().toArray(data10);Assert.assertSame(data10,data10returned);
				for(int i=0;i<data10.length;++i) Assert.assertNull(data10[i]);
			}
			
			{
				Object data1[]= new Object[1];
				Object data1returned[]=map.keySet().toArray(data1);Assert.assertSame(data1,data1returned);
				for(int i=0;i<data1.length;++i) Assert.assertNull(data1[i]);
			}
			{
				Object data0[]= new Object[0];
				Object data0returned[]=map.keySet().toArray(data0);Assert.assertSame(data0, data0returned);
			}

			checkUnsupportedOperationsOnKeySet(map.getOurs());
		}
		map.compareForEquality();
	}
	
	private <K,V> void checkArrayWithOnePair(final Attempt<K,V> map, K keyArg, V valueArg)
	{
		Assert.assertEquals(1,map.size());Assert.assertFalse(map.isEmpty());
		Assert.assertEquals("{"+keyArg+"="+valueArg+"}",map.toString());
		map.compareForEquality();
		{// entry set
			Assert.assertEquals("["+keyArg+"="+valueArg+"]",map.entrySet().toString());
			Assert.assertFalse(map.entrySet().isEmpty());Assert.assertEquals(1,map.entrySet().size());

			{// the two collections have entry sets of different types, hence we cannot meaningfully compare them other then via toString.
				Object data []= map.getOurs().entrySet().toArray();
				Assert.assertEquals(1,data.length);Assert.assertEquals(keyArg+"="+valueArg,data[0].toString());
			}
			
			{
				Object data10[]= new Object[10];
				Object data10returned[]=map.getOurs().entrySet().toArray(data10);Assert.assertSame(data10,data10returned);
				Assert.assertEquals(keyArg+"="+valueArg,data10[0].toString());
				for(int i=1;i<data10.length;++i) Assert.assertNull(data10[i]);
			}
			
			{
				Object data1[]= new Object[1];
				Object data10returned[]=map.getOurs().entrySet().toArray(data1);Assert.assertSame(data1,data10returned);
				Assert.assertEquals(keyArg+"="+valueArg,data1[0].toString());
				for(int i=1;i<data1.length;++i) Assert.assertNull(data1[i]);
			}
			
			{// this one will not fit in an array
				Object data0[]= new Object[0];
				Object data0returned[]=map.getOurs().entrySet().toArray(data0);Assert.assertNotSame(data0, data0returned);
				Assert.assertEquals(1,data0returned.length);
				Assert.assertEquals(keyArg+"="+valueArg,data0returned[0].toString());
			}
			
			checkUnsupportedOperationsOnEntrySet(map.getOurs());
		}

		{// values
			Assert.assertEquals("["+valueArg+"]",map.values().toString());
			Assert.assertFalse(map.values().isEmpty());Assert.assertEquals(1,map.values().size());

			{
				Object returnedArray[]=map.values().toArray();
				Assert.assertEquals(1,returnedArray.length);
				Assert.assertEquals(valueArg,returnedArray[0]);
			}

			{
				Object data10[]= new Object[10];
				Object data10returned[]=map.values().toArray(data10);Assert.assertSame(data10,data10returned);
				Assert.assertEquals(valueArg,data10[0]);
				for(int i=1;i<data10.length;++i) Assert.assertNull(data10[i]);
			}
			
			{
				Object data1[]= new Object[1];
				Object data1returned[]=map.values().toArray(data1);Assert.assertSame(data1,data1returned);
				Assert.assertEquals(valueArg,data1[0]);
				for(int i=1;i<data1.length;++i) Assert.assertNull(data1[i]);
			}
			
			{
				Object data0[]= new Object[0];
				Object data0returned[]=map.values().toArray(data0);Assert.assertNotSame(data0, data0returned);
				Assert.assertEquals(1,data0returned.length);
				Assert.assertEquals(valueArg,data0returned[0]);
			}

			checkUnsupportedOperationsOnValueSet(map.getOurs());
		}
		
		{// keys
			Assert.assertEquals("["+keyArg+"]",map.keySet().toString());
			Assert.assertFalse(map.keySet().isEmpty());Assert.assertEquals(1,map.keySet().size());
			
			{
				Object returnedArray[]=map.keySet().toArray();
				Assert.assertEquals(1,returnedArray.length);
				Assert.assertEquals(keyArg,returnedArray[0]);
			}

			{
				Object data10[]= new Object[10];
				Object data10returned[]=map.keySet().toArray(data10);Assert.assertSame(data10,data10returned);
				Assert.assertEquals(keyArg,data10[0]);
				for(int i=1;i<data10.length;++i) Assert.assertNull(data10[i]);
			}
			
			{
				Object data1[]= new Object[1];
				Object data1returned[]=map.keySet().toArray(data1);Assert.assertSame(data1,data1returned);
				Assert.assertEquals(keyArg,data1[0]);
				for(int i=1;i<data1.length;++i) Assert.assertNull(data1[i]);
			}
			{
				Object data0[]= new Object[0];
				Object data0returned[]=map.keySet().toArray(data0);Assert.assertNotSame(data0, data0returned);
				Assert.assertEquals(1,data0returned.length);
				Assert.assertEquals(keyArg,data0returned[0]);
			}

			checkUnsupportedOperationsOnKeySet(map.getOurs());
		}
		map.compareForEquality();
	}
	
	private static void checkFirstTwoElementsAsString(Object data[], String elem1, String elem2)
	{
		Assert.assertTrue(data[0].toString().equals(elem1) && data[1].toString().equals(elem2) ||
				data[1].toString().equals(elem1) && data[0].toString().equals(elem2));
	}
	
	private static void checkFirstTwoElements(Object data[], Object elem1, Object elem2)
	{
		Assert.assertTrue(data[0].equals(elem1) && data[1].equals(elem2) ||
				data[1].equals(elem1) && data[0].equals(elem2));
	}
	
	/** This one assumes that the two keys provided are in the same order as we'd expect to get them from the map when enumerating entries. */
	private <K,V> void checkArrayWithTwoPairs(final Attempt<K,V> map, K keyArgOne, V valueArgOne, K keyArgTwo, V valueArgTwo)
	{
		Assert.assertEquals(2,map.size());Assert.assertFalse(map.isEmpty());
		Assert.assertTrue("GOT: "+map.getOurs().toString()+" expected something like "+"{"+keyArgOne+"="+valueArgOne+", "+keyArgTwo+"="+valueArgTwo+"}",
				map.getOurs().toString().equals("{"+keyArgOne+"="+valueArgOne+", "+keyArgTwo+"="+valueArgTwo+"}") ||
				map.getOurs().toString().equals("{"+keyArgTwo+"="+valueArgTwo+", "+keyArgOne+"="+valueArgOne+"}"));
		map.compareForEquality();
		
		{// entry set
			String elem1AsString = keyArgOne+"="+valueArgOne, elem2AsString = keyArgTwo+"="+valueArgTwo;
			
			Assert.assertFalse(map.entrySet().isEmpty());Assert.assertEquals(2,map.entrySet().size());
			Assert.assertTrue(map.getOurs().entrySet().toString().equals("["+keyArgOne+"="+valueArgOne+", "+keyArgTwo+"="+valueArgTwo+"]") ||
					map.getOurs().entrySet().toString().equals("["+keyArgTwo+"="+valueArgTwo+", "+ keyArgOne+"="+valueArgOne+"]"));
			{// the two collections have entry sets of different types, hence we cannot meaningfully compare them other then via toString.
				Object data []= map.getOurs().entrySet().toArray();
				Assert.assertEquals(2,data.length);
				checkFirstTwoElementsAsString(data, elem1AsString, elem2AsString);
			}
			
			{
				Object data10[]= new Object[10];
				Object data10returned[]=map.getOurs().entrySet().toArray(data10);Assert.assertSame(data10,data10returned);
				checkFirstTwoElementsAsString(data10, elem1AsString, elem2AsString);
				for(int i=2;i<data10.length;++i) Assert.assertNull(data10[i]);
			}
			
			{
				Object data2[]= new Object[2];
				Object data2returned[]=map.getOurs().entrySet().toArray(data2);Assert.assertSame(data2,data2returned);
				checkFirstTwoElementsAsString(data2, elem1AsString, elem2AsString);
				for(int i=2;i<data2.length;++i) Assert.assertNull(data2[i]);
			}
			
			{// this one will not fit in an array
				Object data1[]= new Object[1];
				Object data1returned[]=map.getOurs().entrySet().toArray(data1);Assert.assertNotSame(data1, data1returned);
				Assert.assertEquals(2,data1returned.length);
				checkFirstTwoElementsAsString(data1returned, elem1AsString, elem2AsString);
			}
			
			{// this one will not fit in an array
				Object data0[]= new Object[0];
				Object data0returned[]=map.getOurs().entrySet().toArray(data0);Assert.assertNotSame(data0, data0returned);
				Assert.assertEquals(2,data0returned.length);
				checkFirstTwoElementsAsString(data0returned, elem1AsString, elem2AsString);
			}
			
			checkUnsupportedOperationsOnEntrySet(map.getOurs());
		}
		
		{// values
			Assert.assertTrue(map.getOurs().values().toString().equals("["+valueArgOne+", "+valueArgTwo+"]") ||
					map.getOurs().values().toString().equals("["+valueArgTwo+", "+valueArgOne+"]"));
			Assert.assertFalse(map.values().isEmpty());Assert.assertEquals(2,map.values().size());

			{
				Object returnedArray[]=map.getOurs().values().toArray();
				Assert.assertEquals(2,returnedArray.length);
				checkFirstTwoElements(returnedArray,valueArgOne,valueArgTwo);
			}
			
			{
				Object data10[]= new Object[10];
				Object data10returned[]=map.getOurs().values().toArray(data10);Assert.assertSame(data10,data10returned);
				checkFirstTwoElements(data10,valueArgOne,valueArgTwo);
				for(int i=2;i<data10.length;++i) Assert.assertNull(data10[i]);
			}
			
			{
				Object data2[]= new Object[2];
				Object data2returned[]=map.getOurs().values().toArray(data2);Assert.assertSame(data2,data2returned);
				checkFirstTwoElements(data2,valueArgOne,valueArgTwo);
				for(int i=2;i<data2.length;++i) Assert.assertNull(data2[i]);
			}
			
			{// this one will not fit in an array
				Object data1[]= new Object[1];
				Object data1returned[]=map.getOurs().values().toArray(data1);Assert.assertNotSame(data1, data1returned);
				Assert.assertEquals(2,data1returned.length);
				checkFirstTwoElements(data1returned,valueArgOne,valueArgTwo);
			}

			{// this one will not fit in an array
				Object data0[]= new Object[0];
				Object data0returned[]=map.getOurs().values().toArray(data0);Assert.assertNotSame(data0, data0returned);
				Assert.assertEquals(2,data0returned.length);
				checkFirstTwoElements(data0returned,valueArgOne,valueArgTwo);
			}

			checkUnsupportedOperationsOnValueSet(map.getOurs());
		}
		
		{// keys
			Assert.assertTrue(map.getOurs().keySet().toString().equals("["+keyArgOne+", "+keyArgTwo+"]") ||
					map.getOurs().keySet().toString().equals("["+keyArgTwo+", "+keyArgOne+"]"));
			Assert.assertFalse(map.keySet().isEmpty());Assert.assertEquals(2,map.keySet().size());
			
			{
				Object returnedArray[]=map.getOurs().keySet().toArray();
				Assert.assertEquals(2,returnedArray.length);
				checkFirstTwoElements(returnedArray,keyArgOne,keyArgTwo);
			}
			{
				Object data10[]= new Object[10];
				Object data10returned[]=map.getOurs().keySet().toArray(data10);Assert.assertSame(data10,data10returned);
				checkFirstTwoElements(data10,keyArgOne,keyArgTwo);
				for(int i=2;i<data10.length;++i) Assert.assertNull(data10[i]);
			}
			
			{
				Object data2[]= new Object[2];
				Object data2returned[]=map.getOurs().keySet().toArray(data2);Assert.assertSame(data2,data2returned);
				checkFirstTwoElements(data2,keyArgOne,keyArgTwo);
				for(int i=2;i<data2.length;++i) Assert.assertNull(data2[i]);
			}

			{// this one will not fit in an array
				Object data1[]= new Object[1];
				Object data1returned[]=map.getOurs().keySet().toArray(data1);Assert.assertNotSame(data1, data1returned);
				Assert.assertEquals(2,data1returned.length);
				checkFirstTwoElements(data1returned,keyArgOne,keyArgTwo);
			}

			{// this one will not fit in an array
				Object data0[]= new Object[0];
				Object data0returned[]=map.getOurs().keySet().toArray(data0);Assert.assertNotSame(data0, data0returned);
				Assert.assertEquals(2,data0returned.length);
				checkFirstTwoElements(data0returned,keyArgOne,keyArgTwo);
			}

			checkUnsupportedOperationsOnKeySet(map.getOurs());
		}
		map.compareForEquality();
	}
	
	@Test
	public void testZeroSize1()
	{
		final Attempt<CInteger,Long> map = createMap();
		Assert.assertEquals(0, map.size());
		
		Assert.assertFalse(map.containsKey("at"));
		Assert.assertFalse(map.containsKey(new CInteger(33)));
		Assert.assertFalse(map.containsKey(new CInteger(-6)));
		
		Assert.assertFalse(map.containsValue("at"));
		Assert.assertFalse(map.containsValue(new CInteger(33)));
		Assert.assertFalse(map.containsValue(new CInteger(-6)));

		Assert.assertNull(map.remove("aa"));
		Assert.assertNull(map.remove(new CInteger(-7)));
		Assert.assertNull(map.remove(new CInteger(7)));
		
		checkArrayWithNoPairs(map);
		Assert.assertEquals(null,map.put(new CInteger(55), 77L));
		Assert.assertEquals(1, map.size());
		checkArrayWithOnePair(map,new CInteger(55),new Long(77L));
	}
	
	@Test
	public void testZeroSize2()
	{
		final Attempt<VertexID,Long> map = createMap();
		Assert.assertNull( ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(VertexID.parseID("P20")));
		Assert.assertNull( ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(VertexID.parseID("N20")));
		Assert.assertNull( ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(null));
	}
	
	@Test
	public void testZeroSize3()
	{
		final Attempt<VertexID,Long> map = createMap();
		Assert.assertTrue( ((MapWithSearch<VertexID,Long>)map.getOurs()).getTreeEntrySet().isEmpty() );
		Assert.assertTrue( ((MapWithSearch<VertexID,Long>)map.getOurs()).getPotentiallyOrderedEntrySet(true).isEmpty() );
		Assert.assertTrue( ((MapWithSearch<VertexID,Long>)map.getOurs()).getPotentiallyOrderedEntrySet(false).isEmpty() );
		Assert.assertTrue( ((MapWithSearch<VertexID,Long>)map.getOurs()).getPotentiallyOrderedKeySet(true).isEmpty() );
		Assert.assertTrue( ((MapWithSearch<VertexID,Long>)map.getOurs()).getPotentiallyOrderedKeySet(false).isEmpty() );
	}
	
	@Test
	public void testZeroSize_EntrySet()
	{
		final Attempt<CInteger,Long> map = createMap();
		Assert.assertEquals(0, map.entrySet().size());
		Assert.assertFalse(map.entrySet().contains("aa"));
		Assert.assertFalse(map.entrySet().contains(new CInteger(33)));
		Assert.assertFalse(map.entrySet().contains(new CInteger(-7)));
		Assert.assertTrue(map.entrySet().containsAll(map.entrySet()));// contains all on an empty set returns true
		Assert.assertFalse(map.entrySet().containsAll(createMapOne(keyOne, valueOne).entrySet()));
		checkArrayWithNoPairs(map);

		Iterator<Entry<CInteger,Long>> iter = map.entrySet().iterator();
		Assert.assertFalse(iter.hasNext());
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}
		iter = map.entrySet().iterator();
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testZeroSize_Values()
	{
		final Attempt<CInteger,Long> map = createMap();
		Assert.assertEquals(0, map.values().size());
		Assert.assertFalse(map.values().contains("aa"));
		Assert.assertFalse(map.values().contains(new CInteger(33)));
		Assert.assertFalse(map.values().contains(new CInteger(-7)));
		Assert.assertTrue(map.values().containsAll(map.entrySet()));// contains all on an empty set returns true
		Assert.assertFalse(map.values().containsAll(Arrays.asList(new Long[]{new Long(0)})));
		checkArrayWithNoPairs(map);

		Iterator<Long> iter = map.values().iterator();
		Assert.assertFalse(iter.hasNext());
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}
		iter = map.values().iterator();
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testZeroSize_KeySet1()
	{
		final Attempt<CInteger,Long> map = createMap();
		checkArrayWithNoPairs(map);
		Assert.assertEquals(0, map.keySet().size());
		Assert.assertFalse(map.keySet().contains("aa"));
		Assert.assertFalse(map.keySet().contains(new CInteger(33)));
		Assert.assertFalse(map.keySet().contains(new CInteger(-7)));
		Assert.assertTrue(map.keySet().containsAll(map.keySet()));// contains all on an empty set returns true

		checkArrayWithNoPairs(map);
		
		Iterator<CInteger> iter = map.keySet().iterator();
		Assert.assertFalse(iter.hasNext());
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}
		iter = map.keySet().iterator();
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}
	}
	
	@Test
	public void testZeroSize_KeySet2()
	{
		final Attempt<CInteger,Long> map = createMap();
		Assert.assertFalse(map.keySet().remove("aa"));
		Assert.assertFalse(map.keySet().remove(new CInteger(-7)));
		Assert.assertFalse(map.keySet().remove(new CInteger(7)));
		
		checkArrayWithNoPairs(map);

		Assert.assertFalse(map.keySet().removeAll(Arrays.asList(new CInteger[]{})));
		checkArrayWithNoPairs(map);
	}
		
	@Test
	public void testZeroSize_KeySet3()
	{
		final Attempt<CInteger,Long> map = createMap();
		Assert.assertFalse(map.keySet().removeAll(createMap().keySet()));
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testZeroSize_KeySet4()
	{
		final Attempt<CInteger,Long> map = createMap();
		map.keySet().clear();
		checkArrayWithNoPairs(map);
	}

	@Test
	public void testZeroSize_KeySet5()
	{
		final Attempt<CInteger,Long> map = createMap();
		Assert.assertFalse(map.keySet().remove("aa"));
		Assert.assertFalse(map.keySet().remove(new CInteger(-7)));
		Assert.assertFalse(map.keySet().remove(new CInteger(7)));
		
		checkArrayWithNoPairs(map);

		Assert.assertFalse(map.keySet().retainAll(createMapOne(keyOne,valueOne).keySet()));
		checkArrayWithNoPairs(map);
	}

	@Test
	public void testSizeOne1()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		Assert.assertEquals(1, map.size());
		checkArrayWithOnePair(map, keyOne, valueOne);
		Assert.assertFalse(map.containsKey(new CInteger(33)));
		Assert.assertFalse(map.containsKey(new CInteger(-6)));
		Assert.assertTrue(map.containsKey(keyOne));
		
		Assert.assertFalse(map.containsValue("at"));
		Assert.assertFalse(map.containsValue(new CInteger(33)));
		Assert.assertFalse(map.containsValue(new CInteger(-6)));
		Assert.assertTrue(map.containsValue(valueOne));

		Assert.assertEquals(valueOne,map.get(keyOne));
		Assert.assertNull(map.get(new CInteger(33)));
		Assert.assertNull(map.get(new CInteger(-6)));

		Assert.assertNull(map.remove(new CInteger(-7)));
		Assert.assertNull(map.remove(new CInteger(7)));
		checkArrayWithOnePair(map, keyOne, valueOne);
		
		Assert.assertEquals("{22=9}",map.toString());
		Assert.assertEquals(null,map.put(new CInteger(55), 77L));
		Assert.assertEquals(2, map.size());
		checkArrayWithTwoPairs(map, keyOne, valueOne, new CInteger(55), new Long(77L));
	}

	@Test
	public void testSizeOne2()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		
		Assert.assertEquals(valueOne,map.remove(keyOne));
		checkArrayWithNoPairs(map);
		Assert.assertEquals(null,map.put(new CInteger(55), 77L));
		Assert.assertEquals(1, map.size());
		Assert.assertEquals(new Long(77L),map.get(new CInteger(55)));
		checkArrayWithOnePair(map,new CInteger(55),new Long(77L));
	}

	@Test
	public void testSizeOne3()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		
		Assert.assertEquals(valueOne,map.remove(new CInteger(keyOne.toInt())));
		Assert.assertFalse(map.containsKey(keyOne));Assert.assertFalse(map.containsValue(valueOne));Assert.assertEquals(null,map.get(keyOne));

		Assert.assertEquals(null,map.put(keyOne, 77L));
		Assert.assertEquals(new Long(77L),map.get(new CInteger(keyOne.toInt())));Assert.assertTrue(map.containsValue(new Long(77L)));Assert.assertTrue(map.containsKey(keyOne));
		checkArrayWithOnePair(map,keyOne,new Long(77L));
		
		Assert.assertEquals(new Long(77L),map.put(keyOne, 88L));
		Assert.assertEquals(new Long(88L),map.get(new CInteger(keyOne.toInt())));Assert.assertTrue(map.containsValue(new Long(88L)));Assert.assertTrue(map.containsKey(keyOne));
		checkArrayWithOnePair(map,keyOne,new Long(88L));

		Assert.assertEquals(new Long(88L),map.remove(new CInteger(keyOne.toInt())));
		Assert.assertFalse(map.containsKey(keyOne));Assert.assertFalse(map.containsValue(valueOne));Assert.assertEquals(null,map.get(keyOne));
		checkArrayWithNoPairs(map);
		
		Assert.assertEquals(null,map.put(keyOne, 77L));
		Assert.assertEquals(new Long(77L),map.get(new CInteger(keyOne.toInt())));Assert.assertTrue(map.containsValue(new Long(77L)));Assert.assertTrue(map.containsKey(keyOne));
		checkArrayWithOnePair(map,keyOne,new Long(77L));
	}

	@Test
	public void testSizeOne4()
	{
		VertexID vert = VertexID.parseID("P21");
		final Attempt<VertexID,Long> map = createMapOne(vert,valueOne);
		Assert.assertNull( ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(VertexID.parseID("P20")));
		Assert.assertNull( ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(null));
		Assert.assertNull( ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(VertexID.parseID("N20")));
		Assert.assertSame(vert, ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(vert));
		Assert.assertSame(vert, ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(VertexID.parseID("P21")));
	}
	
	public static class CIntegerWithReverseOrder extends CInteger
	{

		public CIntegerWithReverseOrder(int v) {
			super(v);
		}

		@Override
		public int compareTo(CInteger o) {
			return -super.compareTo(o);
		}
		
	}
	
	@Test
	public void testSizeOne5()
	{
		final Attempt<CIntegerWithReverseOrder,Long> map = createMapOne(new CIntegerWithReverseOrder(89),valueOne);
		MapWithSearch<CIntegerWithReverseOrder,Long> ourMap = (MapWithSearch<CIntegerWithReverseOrder,Long>)map.getOurs();
		for(Set<java.util.Map.Entry<CIntegerWithReverseOrder, Long>> entrySet:new Set[]{ourMap.entrySet(),ourMap.getPotentiallyOrderedEntrySet(false),ourMap.getPotentiallyOrderedEntrySet(true)})
		{
			Iterator<java.util.Map.Entry<CIntegerWithReverseOrder, Long>> iter = entrySet.iterator();
			java.util.Map.Entry<CIntegerWithReverseOrder, Long> entry = iter.next();Assert.assertFalse(iter.hasNext());
			Assert.assertEquals(new CIntegerWithReverseOrder(89),entry.getKey());Assert.assertEquals(valueOne,entry.getValue());
		}
		
		Assert.assertEquals(new CIntegerWithReverseOrder(89),ourMap.getPotentiallyOrderedKeySet(true).iterator().next());
		Assert.assertEquals(new CIntegerWithReverseOrder(89),ourMap.getPotentiallyOrderedKeySet(false).iterator().next());
	}
	
	@Test
	public void testSizeOne_EntrySet1()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);

		Assert.assertFalse(map.entrySet().contains("aa"));
		Assert.assertFalse(map.entrySet().contains(new CInteger(33)));
		Assert.assertFalse(map.entrySet().contains(new CInteger(-7)));
		Assert.assertTrue(map.entrySet().containsAll(createMapOne(keyOne,valueOne).entrySet()));// contains all on an the set of pairs from the same set should return true
		Assert.assertTrue(map.entrySet().containsAll(map.entrySet()));// contains all on an the set of pairs from the same set should return true
		Assert.assertFalse(map.entrySet().containsAll(createMapTwo(keyOne,valueOne,keyTwo,valueTwo).entrySet()));
		checkArrayWithOnePair(map, keyOne, valueOne);

		Iterator<Entry<CInteger,Long>> iter = map.entrySet().iterator();
		Assert.assertTrue(iter.hasNext());
		Entry<CInteger,Long> entry = iter.next();
		Assert.assertEquals(keyOne, entry.getKey());Assert.assertEquals(valueOne, entry.getValue());
		
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}

		iter = map.entrySet().iterator();iter.next();
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}
		checkArrayWithOnePair(map, keyOne, valueOne);
	}
	
	@Test
	public void testSizeOne_EntrySet2()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		Iterator<Entry<CInteger,Long>> iter = map.entrySet().iterator();
		Assert.assertTrue(iter.hasNext());
		Entry<CInteger,Long> entry = iter.next();
		entry.setValue(valueTwo);// this one sets the entry of the real map
		map.getOurs().entrySet().iterator().next().setValue(valueTwo);// and this one sets our map
		checkArrayWithOnePair(map, keyOne, valueTwo);
	}
	
	@Test
	public void testSizeOne_Values()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);

		Assert.assertEquals(1, map.values().size());
		Assert.assertFalse(map.values().contains("aa"));
		Assert.assertFalse(map.values().contains(new CInteger(33)));
		Assert.assertFalse(map.values().contains(new CInteger(-7)));
		Assert.assertTrue(map.values().containsAll(map.values()));// contains all on an empty set returns true
		Assert.assertFalse(map.values().containsAll(Arrays.asList(new Long[]{new Long(0)})));
		Assert.assertFalse(map.values().containsAll(Arrays.asList(new Long[]{valueOne,new Long(0)})));
		Assert.assertTrue(map.values().containsAll(Arrays.asList(new Long[]{valueOne,valueOne})));
		Assert.assertTrue(map.values().containsAll(createMapOne(keyOne,valueOne).values()));// contains all on an empty set returns true
		checkArrayWithOnePair(map, keyOne, valueOne);

		Iterator<Long> iter = map.values().iterator();
		Assert.assertTrue(iter.hasNext());
		Long retrievedValue = iter.next();
		Assert.assertEquals(valueOne,retrievedValue);
		
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}

		iter = map.values().iterator();iter.next();
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}
		checkArrayWithOnePair(map, keyOne, valueOne);
	}
	
	@Test
	public void testSizeOne_KeySet1()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		Assert.assertFalse(map.keySet().contains(new CInteger(33)));
		Assert.assertFalse(map.keySet().contains(new CInteger(-7)));
		Assert.assertTrue(map.keySet().containsAll(map.keySet()));// contains all on an empty set returns true
		Assert.assertTrue(map.keySet().containsAll(createMapOne(keyOne,valueOne).keySet()));// contains all on an empty set returns true
		Assert.assertFalse(map.keySet().containsAll(Arrays.asList(new CInteger[]{keyOne,keyTwo})));
		Assert.assertTrue(map.keySet().containsAll(Arrays.asList(new CInteger[]{keyOne,keyOne})));
		Assert.assertTrue(map.keySet().containsAll(Arrays.asList(new CInteger[]{})));
				
		Iterator<CInteger> iter = map.keySet().iterator();
		Assert.assertTrue(iter.hasNext());
		CInteger retrievedValue = iter.next();
		Assert.assertEquals(keyOne,retrievedValue);
		
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}

		iter = map.keySet().iterator();iter.next();
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}
		checkArrayWithOnePair(map, keyOne, valueOne);
	}
	
	@Test
	public void testSizeOne_KeySetRemove()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);

		Assert.assertFalse(map.keySet().remove(new CInteger(-7)));
		Assert.assertFalse(map.keySet().remove(new CInteger(7)));
		checkArrayWithOnePair(map, keyOne, valueOne);
		Assert.assertTrue(map.keySet().remove(keyOne));// remove the only pair
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testSizeOne_KeySetRemoveAll1()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		Assert.assertFalse(map.keySet().removeAll(Arrays.asList(new CInteger[]{})));
		checkArrayWithOnePair(map, keyOne, valueOne);
	}
	
	@Test
	public void testSizeOne_KeySetRemoveAll2()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		Assert.assertTrue(map.keySet().removeAll(createMapOne(keyOne,valueOne).keySet()));
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testSizeOne_KeySetRemoveAll3()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		Assert.assertFalse(map.keySet().removeAll(Arrays.asList(new CInteger[]{new CInteger(4),new CInteger(5)})));
		checkArrayWithOnePair(map, keyOne, valueOne);
	}
	
	@Test
	public void testSizeOne_KeySetRetainAll1()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		Assert.assertFalse(map.keySet().retainAll(createMapOne(keyOne,valueOne).keySet()));
		checkArrayWithOnePair(map, keyOne, valueOne);
	}
	
	@Test
	public void testSizeOne_KeySetRetainAll2()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		Assert.assertTrue(map.keySet().retainAll(Arrays.asList(new CInteger[]{new CInteger(9)})));
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testSizeOne_KeySetRetainAll3()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		Assert.assertTrue(map.keySet().retainAll(Arrays.asList(new CInteger[]{})));
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testSizeOne_KeySetClear()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		map.keySet().clear();
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testSizeOne_KeySetContainsAll1()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		Assert.assertTrue(map.keySet().containsAll(Arrays.asList(new CInteger[]{keyOne})));
		checkArrayWithOnePair(map, keyOne, valueOne);
	}
	
	@Test
	public void testSizeOne_KeySetContainsAll2()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		Assert.assertFalse(map.keySet().containsAll(Arrays.asList(new CInteger[]{keyOne,new CInteger(11)})));
		checkArrayWithOnePair(map, keyOne, valueOne);
	}
	
	@Test
	public void testSizeOne_KeySetContainsAll3()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		Assert.assertTrue(map.keySet().containsAll(Arrays.asList(new CInteger[]{})));
		checkArrayWithOnePair(map, keyOne, valueOne);
	}
	
	@Test
	public void testSizeOne_KeySetContains()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		Assert.assertTrue(map.keySet().contains(keyOne));
		Assert.assertFalse(map.keySet().contains(new CInteger(0)));
		checkArrayWithOnePair(map, keyOne, valueOne);
	}
	
	@Test
	public void testSizeOne_KeySetIteratorRemove1()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.getOurs().keySet().iterator().remove();
		}},IllegalStateException.class,"next was not yet called");
	}
	
	@Test
	public void testSizeOne_KeySetIteratorRemove2()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		final Iterator<CInteger> iter = map.keySet().iterator();
		iter.next();iter.remove();	
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testSizeOne_KeySetIteratorRemove3()
	{
		final Attempt<CInteger,Long> map = createMapOne(keyOne,valueOne);
		final Iterator<CInteger> iter = map.keySet().iterator();
		iter.next();iter.remove();	
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.getOurs().keySet().iterator().remove();
		}},IllegalStateException.class,"next was not yet called");
	}
	
	@Test
	public void testNull1()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.getOurs().put(null, new Long(33));
		}},IllegalArgumentException.class,"key cannot be null");
	}
	
	@Test
	public void testNull2()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.getOurs().put(keyOne, null);
		}},IllegalArgumentException.class,"value cannot be null");
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.getOurs().putAll(null);
		}},NullPointerException.class,null);
	}
	
	@Test
	public void testNull3()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertNull(map.getOurs().get(null));
		Assert.assertFalse(map.getOurs().containsKey(null));
		Assert.assertFalse(map.getOurs().containsValue(null));
	}
	
	@Test
	public void testContains()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertFalse(map.keySet().contains(new CInteger(33)));
		Assert.assertFalse(map.keySet().contains(new CInteger(-7)));
		Assert.assertTrue(map.keySet().contains(keyOne));
		Assert.assertTrue(map.keySet().contains(keyTwo));
		Assert.assertFalse(map.getOurs().keySet().contains(null));
		Assert.assertTrue(map.keySet().containsAll(map.keySet()));// contains all on an empty set returns true
		Assert.assertTrue(map.keySet().containsAll(createMapTwo(keyOne,valueOne, keyTwo, valueTwo).keySet()));// contains all on an empty set returns true
		Assert.assertTrue(map.keySet().containsAll(createMapOne(keyOne,valueOne).keySet()));
		Assert.assertFalse(map.keySet().containsAll(Arrays.asList(new CInteger[]{new CInteger(0),keyOne})));
		Assert.assertTrue(map.keySet().containsAll(Arrays.asList(new CInteger[]{keyOne,keyOne})));
		Assert.assertFalse(map.getOurs().keySet().containsAll(Arrays.asList(new CInteger[]{keyOne,null,keyOne})));
		checkArrayWithTwoPairs(map, keyOne, valueOne, keyTwo, valueTwo);
	}
	
	@Test
	public void testSizeTwo1()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		checkArrayWithTwoPairs(map, keyOne, valueOne, keyTwo, valueTwo);
		Assert.assertFalse(map.containsKey(new CInteger(33)));
		Assert.assertFalse(map.containsKey(new CInteger(-6)));
		Assert.assertTrue(map.containsKey(keyOne));
		Assert.assertTrue(map.containsKey(keyTwo));
		
		Assert.assertFalse(map.containsValue("at"));
		Assert.assertFalse(map.containsValue(new CInteger(33)));
		Assert.assertFalse(map.containsValue(new CInteger(-6)));
		Assert.assertTrue(map.containsValue(valueOne));
		Assert.assertTrue(map.containsValue(valueTwo));

		Assert.assertEquals(valueOne,map.get(keyOne));
		Assert.assertEquals(valueTwo,map.get(keyTwo));
		Assert.assertNull(map.get(new CInteger(33)));
		Assert.assertNull(map.get(new CInteger(-6)));

		Assert.assertNull(map.remove(new CInteger(-7)));
		Assert.assertNull(map.remove(new CInteger(7)));
		checkArrayWithTwoPairs(map, keyOne, valueOne, keyTwo, valueTwo);
		
		checkArrayWithTwoPairs(map, keyOne, valueOne, keyTwo, valueTwo);
		Assert.assertEquals(null,map.put(new CInteger(55), 77L));
		Assert.assertEquals(3, map.size());
		Assert.assertEquals(new Long(77L),map.put(new CInteger(55), 78L));
	}

	@Test
	public void testSizeTwo2()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		
		Assert.assertEquals(valueOne,map.remove(keyOne));
		checkArrayWithOnePair(map, keyTwo, valueTwo);
		Assert.assertEquals(null,map.put(new CInteger(55), 77L));
		Assert.assertEquals(2, map.size());
		Assert.assertEquals(new Long(77L),map.get(new CInteger(55)));
		checkArrayWithTwoPairs(map,keyTwo, valueTwo, new CInteger(55),new Long(77L));
	}

	@Test
	public void testSizeTwo3()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		checkArrayWithTwoPairs(map,keyOne, valueOne, keyTwo,valueTwo);
		
		Assert.assertEquals(valueOne,map.remove(new CInteger(keyOne.toInt())));
		Assert.assertFalse(map.containsKey(keyOne));Assert.assertFalse(map.containsValue(valueOne));Assert.assertEquals(null,map.get(keyOne));
		checkArrayWithOnePair(map, keyTwo, valueTwo);
		
		Assert.assertEquals(null,map.put(keyOne, 77L));
		Assert.assertEquals(new Long(77L),map.get(new CInteger(keyOne.toInt())));Assert.assertTrue(map.containsValue(new Long(77L)));Assert.assertTrue(map.containsKey(keyOne));
		checkArrayWithTwoPairs(map,keyOne,new Long(77L), keyTwo, valueTwo);
		
		Assert.assertEquals(new Long(77L),map.put(keyOne, 88L));
		Assert.assertEquals(new Long(88L),map.get(new CInteger(keyOne.toInt())));Assert.assertTrue(map.containsValue(new Long(88L)));Assert.assertTrue(map.containsKey(keyOne));
		checkArrayWithTwoPairs(map,keyOne,new Long(88L), keyTwo, valueTwo);

		Assert.assertEquals(new Long(88L),map.remove(new CInteger(keyOne.toInt())));
		Assert.assertFalse(map.containsKey(keyOne));Assert.assertFalse(map.containsValue(valueOne));Assert.assertEquals(null,map.get(keyOne));
		checkArrayWithOnePair(map, keyTwo, valueTwo);
		
		Assert.assertEquals(null,map.put(keyOne, 77L));
		Assert.assertEquals(new Long(77L),map.get(new CInteger(keyOne.toInt())));Assert.assertTrue(map.containsValue(new Long(77L)));Assert.assertTrue(map.containsKey(keyOne));
		checkArrayWithTwoPairs(map,keyOne,new Long(77L), keyTwo, valueTwo);
	}

	@Test
	public void testSizeTwo4()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		checkArrayWithTwoPairs(map,keyOne, valueOne, keyTwo,valueTwo);
		
		Assert.assertEquals(valueOne,map.remove(new CInteger(keyOne.toInt())));
		Assert.assertFalse(map.containsKey(keyOne));Assert.assertFalse(map.containsValue(valueOne));Assert.assertEquals(null,map.get(keyOne));
		checkArrayWithOnePair(map, keyTwo, valueTwo);
		map.remove(keyTwo);
		checkArrayWithNoPairs(map);
		map.remove(keyTwo);
		map.put(keyTwo, valueTwo);
		checkArrayWithOnePair(map, keyTwo, valueTwo);
	}

	@Test
	public void testSizeTwo5()
	{
		final Attempt<CInteger,Long> mapA = createMapOne(keyOne,valueOne), mapB=createMapOne(keyTwo, valueTwo);
		mapA.putAll(mapB);
		checkArrayWithTwoPairs(mapA,keyOne, valueOne, keyTwo,valueTwo);checkArrayWithOnePair(mapB, keyTwo, valueTwo);

		mapA.putAll(mapB);// another attempt
		checkArrayWithTwoPairs(mapA,keyOne, valueOne, keyTwo,valueTwo);checkArrayWithOnePair(mapB, keyTwo, valueTwo);

		final Attempt<CInteger,Long> mapEmpty = createMap();
		mapA.putAll(mapEmpty);// put an empty map
		checkArrayWithNoPairs(mapEmpty);checkArrayWithTwoPairs(mapA,keyOne, valueOne, keyTwo,valueTwo);checkArrayWithOnePair(mapB, keyTwo, valueTwo);

		mapEmpty.putAll(mapA);
		checkArrayWithTwoPairs(mapA,keyOne, valueOne, keyTwo,valueTwo);checkArrayWithTwoPairs(mapEmpty,keyOne, valueOne, keyTwo,valueTwo);checkArrayWithOnePair(mapB, keyTwo, valueTwo);

		Assert.assertEquals(valueOne, mapA.remove(keyOne));
		checkArrayWithOnePair(mapA,keyTwo,valueTwo);checkArrayWithTwoPairs(mapEmpty,keyOne, valueOne, keyTwo,valueTwo);checkArrayWithOnePair(mapB, keyTwo, valueTwo);
		mapB.clear();
		checkArrayWithOnePair(mapA,keyTwo,valueTwo);checkArrayWithTwoPairs(mapEmpty,keyOne, valueOne, keyTwo,valueTwo);checkArrayWithNoPairs(mapB);
		Assert.assertNull(mapB.remove(keyOne));		
	}
	
	@Test
	public void testSizeTwo6()
	{
		VertexID vert = VertexID.parseID("P21"), vert2 = VertexID.parseID("P99");
		final Attempt<VertexID,Long> map = createMapTwo(vert,valueOne,vert2,valueTwo);
		Assert.assertNull( ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(VertexID.parseID("P20")));
		Assert.assertNull( ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(null));
		Assert.assertSame(vert, ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(vert));
		Assert.assertSame(vert, ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(VertexID.parseID("P21")));

		Assert.assertNull( ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(VertexID.parseID("P98")));
		Assert.assertNull( ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(VertexID.parseID("N98")));
		Assert.assertNull( ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(null));
		Assert.assertSame(vert2, ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(vert2));
		Assert.assertSame(vert2, ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(VertexID.parseID("P99")));
	}
	
	@Test
	public void testSizeTwo7()
	{
		final Attempt<CIntegerWithReverseOrder,Long> map = createMapTwo(new CIntegerWithReverseOrder(89),valueOne,new CIntegerWithReverseOrder(6),valueTwo);
		MapWithSearch<CIntegerWithReverseOrder,Long> ourMap = (MapWithSearch<CIntegerWithReverseOrder,Long>)map.getOurs();

		for(Set<java.util.Map.Entry<CIntegerWithReverseOrder, Long>> entrySet:new Set[]{ourMap.entrySet(),ourMap.getPotentiallyOrderedEntrySet(false)})
		{
			Iterator<java.util.Map.Entry<CIntegerWithReverseOrder, Long>> iter = entrySet.iterator();
			java.util.Map.Entry<CIntegerWithReverseOrder, Long> entry = iter.next();
			// Here we accept any order because we've not asked for an ordered collection
			if (new CIntegerWithReverseOrder(6).equals(entry.getKey()))
			{
				Assert.assertEquals(valueTwo,entry.getValue());
				entry = iter.next();
				Assert.assertEquals(new CIntegerWithReverseOrder(89),entry.getKey());Assert.assertEquals(valueOne,entry.getValue());
			}
			else
			{
				Assert.assertEquals(new CIntegerWithReverseOrder(89),entry.getKey());Assert.assertEquals(valueOne,entry.getValue());
				entry = iter.next();
				Assert.assertEquals(new CIntegerWithReverseOrder(6),entry.getKey());Assert.assertEquals(valueTwo,entry.getValue());
			}
			Assert.assertFalse(iter.hasNext());
		}
		
		{
			Iterator<java.util.Map.Entry<CIntegerWithReverseOrder, Long>> iter = ourMap.getPotentiallyOrderedEntrySet(true).iterator();
			java.util.Map.Entry<CIntegerWithReverseOrder, Long> entry = iter.next();
			Assert.assertEquals(new CIntegerWithReverseOrder(89),entry.getKey());Assert.assertEquals(valueOne,entry.getValue());
			entry = iter.next();
			Assert.assertEquals(new CIntegerWithReverseOrder(6),entry.getKey());Assert.assertEquals(valueTwo,entry.getValue());
			Assert.assertFalse(iter.hasNext());
		}
		
		StringBuffer outcome = new StringBuffer();boolean success = false;
		if ( ourMap.getPotentiallyOrderedKeySet(false).iterator().next().equals(new CIntegerWithReverseOrder(6)))
		{// Here we accept any order because we've not asked for an ordered collection
			success = ArrayOperations.cmp(new CIntegerWithReverseOrder[]{new CIntegerWithReverseOrder(6), new CIntegerWithReverseOrder(89)}, ourMap.getPotentiallyOrderedKeySet(false).toArray(), outcome);
			Assert.assertTrue(outcome.toString(),success);
		}
		else
		{
			success = ArrayOperations.cmp(new CIntegerWithReverseOrder[]{new CIntegerWithReverseOrder(89), new CIntegerWithReverseOrder(6)}, ourMap.getPotentiallyOrderedKeySet(false).toArray(), outcome);
			Assert.assertTrue(outcome.toString(),success);
		}
		success = ArrayOperations.cmp(new CIntegerWithReverseOrder[]{new CIntegerWithReverseOrder(89), new CIntegerWithReverseOrder(6)}, ourMap.getPotentiallyOrderedKeySet(true).toArray(), outcome);
		Assert.assertTrue(outcome.toString(),success);
	}

	@Test
	public void testSizeTwo_EntrySet1()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);

		Assert.assertFalse(map.entrySet().contains("aa"));
		Assert.assertFalse(map.entrySet().contains(new CInteger(33)));
		Assert.assertFalse(map.entrySet().contains(new CInteger(-7)));
		Assert.assertTrue(map.entrySet().containsAll(createMapTwo(keyOne,valueOne, keyTwo, valueTwo).entrySet()));// contains all on an the set of pairs from the same set should return true
		Assert.assertTrue(map.entrySet().containsAll(createMapOne(keyOne,valueOne).entrySet()));
		Assert.assertFalse(map.entrySet().containsAll(createMapTwo(keyOne,valueOne, keyTwo, new Long(6)).entrySet()));
		Assert.assertTrue(map.entrySet().containsAll(map.entrySet()));// contains all on an the set of pairs from the same set should return true
		
		{// now check with an invalid entry
			List<Map.Entry<CInteger,Long>> someEntries = new LinkedList<Map.Entry<CInteger,Long>>();
			someEntries.addAll(map.getOurs().entrySet());someEntries.add(null);
			Assert.assertFalse(map.entrySet().containsAll(someEntries));
			
			Assert.assertTrue(map.entrySet().containsAll(someEntries.subList(0, 1)));
		}

		checkArrayWithTwoPairs(map,keyOne, valueOne, keyTwo,valueTwo);

		Iterator<Entry<CInteger,Long>> iter = map.entrySet().iterator();
		Assert.assertTrue(iter.hasNext());
		Entry<CInteger,Long> entry = iter.next();
		Assert.assertEquals(keyOne, entry.getKey());Assert.assertEquals(valueOne, entry.getValue());
		entry = iter.next();
		Assert.assertEquals(keyTwo, entry.getKey());Assert.assertEquals(valueTwo, entry.getValue());
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}

		iter = map.entrySet().iterator();iter.next();iter.next();
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}
		checkArrayWithTwoPairs(map,keyOne, valueOne, keyTwo,valueTwo);
	}
	
	@Test
	public void testSizeTwo_EntrySet2()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Iterator<Entry<CInteger,Long>> iter = map.entrySet().iterator();
		Assert.assertTrue(iter.hasNext());
		Entry<CInteger,Long> entry = iter.next();entry.setValue(new Long(11L));
		iter.next().setValue(new Long(64L));//  the above sets or real map, now we set ours,
		Iterator<Entry<CInteger,Long>> iterOurs = map.getOurs().entrySet().iterator();
		iterOurs.next().setValue(new Long(11L));iterOurs.next().setValue(new Long(64L));
		checkArrayWithTwoPairs(map,keyOne, new Long(11L), keyTwo,new Long(64L));
	}
	
	@Test
	public void testSizeTwo_Values()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);

		Assert.assertFalse(map.values().contains(new CInteger(33)));
		Assert.assertFalse(map.values().contains(new CInteger(-7)));
		Assert.assertTrue(map.values().containsAll(map.values()));// contains all on an empty set returns true
		Assert.assertTrue(map.values().containsAll(createMapOne(keyOne,valueOne).values()));// contains all on an empty set returns true
		Assert.assertFalse(map.values().containsAll(Arrays.asList(new Long[]{valueOne,new Long(0)})));
		Assert.assertTrue(map.values().containsAll(Arrays.asList(new Long[]{valueOne,valueOne})));
		checkArrayWithTwoPairs(map,keyOne,valueOne, keyTwo, valueTwo);

		Iterator<Long> iter = map.values().iterator();
		Assert.assertTrue(iter.hasNext());
		Long retrievedValue = iter.next();
		Assert.assertEquals(valueOne,retrievedValue);
		retrievedValue = iter.next();
		Assert.assertEquals(valueTwo,retrievedValue);
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}

		iter = map.values().iterator();iter.next();iter.next();
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}
		checkArrayWithTwoPairs(map,keyOne,valueOne, keyTwo, valueTwo);
	}
	
	@Test
	public void testSizeTwo_KeySet1()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);

		Iterator<CInteger> iter = map.keySet().iterator();
		Assert.assertTrue(iter.hasNext());
		CInteger retrievedValue = iter.next();
		Assert.assertEquals(keyOne,retrievedValue);
		retrievedValue = iter.next();
		Assert.assertEquals(keyTwo,retrievedValue);
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}

		iter = map.keySet().iterator();iter.next();iter.next();
		try	{ iter.next();fail("exception not thrown");	} catch(NoSuchElementException e){/* exception if all is good */}
		checkArrayWithTwoPairs(map,keyOne,valueOne, keyTwo, valueTwo);
	}
	
	@Test
	public void testSizeTwo_KeySetRemove()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);

		Assert.assertFalse(map.keySet().remove(new CInteger(-7)));
		Assert.assertFalse(map.keySet().remove(new CInteger(7)));
		checkArrayWithTwoPairs(map,keyOne,valueOne, keyTwo, valueTwo);
		
		Assert.assertTrue(map.keySet().remove(keyOne));// remove one pair
		checkArrayWithOnePair(map, keyTwo, valueTwo);
		Assert.assertFalse(map.getOurs().keySet().remove(null));// remove invalid element
		checkArrayWithOnePair(map, keyTwo, valueTwo);
	}
	
	@Test
	public void testSizeTwo_KeySetRemoveAll1()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertFalse(map.keySet().removeAll(Arrays.asList(new CInteger[]{})));
		checkArrayWithTwoPairs(map,keyOne,valueOne, keyTwo, valueTwo);
	}
	
	@Test
	public void testSizeTwo_KeySetRemoveAll2()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertTrue(map.keySet().removeAll(createMapOne(keyOne,valueOne).keySet()));
		checkArrayWithOnePair(map, keyTwo, valueTwo);
	}
	
	@Test
	public void testSizeTwo_KeySetRemoveAll3()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertTrue(map.keySet().removeAll(Arrays.asList(new CInteger[]{keyOne,null})));
		checkArrayWithOnePair(map, keyTwo, valueTwo);
	}
	
	@Test
	public void testSizeTwo_KeySetRemoveAll4()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertFalse(map.keySet().removeAll(Arrays.asList(new CInteger[]{new CInteger(4),new CInteger(5)})));
		checkArrayWithTwoPairs(map,keyOne,valueOne, keyTwo, valueTwo);
	}
	
	@Test
	public void testSizeTwo_KeySetRetainAll1()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertTrue(map.keySet().retainAll(createMapOne(keyOne,valueOne).keySet()));
		checkArrayWithOnePair(map, keyOne, valueOne);
	}
	
	@Test
	public void testSizeTwo_KeySetRetainAll2()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertTrue(map.keySet().retainAll(Arrays.asList(new CInteger[]{new CInteger(9)})));
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testSizeTwo_KeySetRetainAll3()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertTrue(map.keySet().retainAll(Arrays.asList(new CInteger[]{})));
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testSizeTwo_KeySetRetainAll4()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertFalse(map.keySet().retainAll(Arrays.asList(new CInteger[]{keyOne,keyTwo})));
		checkArrayWithTwoPairs(map,keyOne,valueOne, keyTwo, valueTwo);
	}
	
	@Test
	public void testSizeTwo_KeySetRetainAll5()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertTrue(map.keySet().retainAll(Arrays.asList(new CInteger[]{null})));
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testSizeTwo_KeySetClear()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		map.keySet().clear();
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testSizeTwo_KeySetContainsAll1()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertTrue(map.keySet().containsAll(Arrays.asList(new CInteger[]{keyOne})));
		checkArrayWithTwoPairs(map,keyOne,valueOne, keyTwo, valueTwo);
	}
	
	@Test
	public void testSizeTwo_KeySetContainsAll2()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertFalse(map.keySet().containsAll(Arrays.asList(new CInteger[]{keyOne,new CInteger(11),keyTwo})));
		checkArrayWithTwoPairs(map,keyOne,valueOne, keyTwo, valueTwo);
	}
	
	@Test
	public void testSizeTwo_KeySetContainsAll3()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertTrue(map.keySet().containsAll(Arrays.asList(new CInteger[]{})));
		checkArrayWithTwoPairs(map,keyOne,valueOne, keyTwo, valueTwo);
	}
	
	@Test
	public void testSizeTwo_KeySetContains()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		Assert.assertTrue(map.keySet().contains(keyOne));
		Assert.assertFalse(map.keySet().contains(new CInteger(0)));
		checkArrayWithTwoPairs(map,keyOne,valueOne, keyTwo, valueTwo);
	}
	
	@Test
	public void testSizeTwo_KeySetIteratorRemove1()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.getOurs().keySet().iterator().remove();
		}},IllegalStateException.class,"next was not yet called");
	}
	
	@Test
	public void testSizeTwo_KeySetIteratorRemove2()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		final Iterator<CInteger> iter = map.keySet().iterator();
		iter.next();iter.remove();	
		checkArrayWithOnePair(map, keyTwo, valueTwo);
		iter.next();iter.remove();
		checkArrayWithNoPairs(map);
	}
	
	@Test
	public void testSizeTwo_KeySetIteratorRemove3()
	{
		final Attempt<CInteger,Long> map = createMapTwo(keyOne,valueOne, keyTwo, valueTwo);
		final Iterator<CInteger> iter = map.keySet().iterator();
		iter.next();iter.remove();	
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			map.getOurs().keySet().iterator().remove();
		}},IllegalStateException.class,"next was not yet called");
	}
	
	
	@Test
	public void TestMapWithSearch0a()
	{
		int [] collectionOfKeys = new int[]{
				1000,
				1000,
				1001,
				0,
				1002,
				1003}; 
		final Attempt<CInteger,Long> map = createMap();
		for(int i=0;i<collectionOfKeys.length;++i)
		{
			map.put(new CInteger(collectionOfKeys[i]), new Long(1000+i));
			for(int j=0;j <= i;++j)
				Assert.assertEquals(new Long(1000+i),map.get(new CInteger(collectionOfKeys[i])));
		}
	}
	
	@Test
	public void TestMapWithSearch0b()
	{
		VertexID [] collectionOfKeys = new VertexID[]{new VertexID(VertKind.POSITIVE,0),new VertexID(VertKind.POSITIVE,1),new VertexID(VertKind.POSITIVE,3),new VertexID(VertKind.POSITIVE,4)}; 
		final Attempt<VertexID,Long> map = createMap();
		for(int i=0;i<collectionOfKeys.length;++i)
		{
			map.put(collectionOfKeys[i], new Long(1000+i));
			for(int j=i+1;j < collectionOfKeys.length;++j)
			{
				Assert.assertFalse("j="+j+", key "+collectionOfKeys[j]+" should not exist",map.containsKey(collectionOfKeys[j]));
				Assert.assertNull(map.remove(collectionOfKeys[j]));
				Assert.assertNull( ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(collectionOfKeys[j]) );
			}
			for(int j=0;j <= i;++j)
			{
				Assert.assertEquals(new Long(1000+i),map.get(collectionOfKeys[i]));
				Assert.assertSame(collectionOfKeys[j], ((MapWithSearch<VertexID,Long>)map.getOurs()).findElementById(new VertexID(collectionOfKeys[j])) );
			}
		}
	}
	@Test
	public void TestMapWithSearch1()
	{
		initKeysAndValues();
		LinkedHashMap<CInteger,Long> realMap = new LinkedHashMap<CInteger,Long>();
		MapWithSearch<CInteger,Long> ourMap = createOurMap();
		Assert.assertEquals(ourMap.keySet(),realMap.keySet());
		
		Set<Long> realValues = new TreeSet<Long>();realValues.addAll(realMap.values());
		Assert.assertEquals(realValues,collectionAsSet(ourMap.values()));
		Assert.assertEquals(collectionAsSet(ourMap.values()),realValues);
		Assert.assertEquals(ourMap.entrySet(),realMap.entrySet());

		Assert.assertTrue(ourMap.isEmpty());
		for(int i=0;i<key.length;++i)
		{
			Assert.assertNull(realMap.put(key[i], value[i]));Assert.assertNull(ourMap.put(key[i], value[i]));
		}

		compareForEquality(realMap,ourMap);
	}
	
	/** Similar to above but where entries are changed. */
	@Test
	public void TestMapWithSearch_update()
	{
		initKeysAndValues();
		LinkedHashMap<CInteger,Long> realMap = new LinkedHashMap<CInteger,Long>();
		MapWithSearch<CInteger,Long> ourMap = createOurMap();
		for(int i=0;i<key.length;++i)
		{
			Assert.assertNull(realMap.put(key[i], value[i]));Assert.assertNull(ourMap.put(key[i], value[i]));
		}

		Random rnd = new Random(1);
		
		for(int i=0;i<10;++i)
		{
			int idx = rnd.nextInt(key.length);
			Assert.assertEquals(new Long(value[idx]),ourMap.get(key[idx]));
			Assert.assertEquals(new Long(value[idx]),realMap.get(key[idx]));

			long val = rnd.nextLong();
			Assert.assertEquals(new Long(value[idx]),realMap.put(key[idx],val));
			Assert.assertEquals(new Long(value[idx]),ourMap.put(key[idx],val));
			value[idx]=val;
			for(int j=0;j<key.length;++j)
				Assert.assertEquals("difference at index "+j,new Long(value[j]),ourMap.get(key[j]));
			compareForEquality(realMap,ourMap);
		}
	}
	
	
	/** Similar to above but where entries are changed. */
	@Test
	public void TestMapWithSearch_remove()
	{
		initKeysAndValues();
		LinkedHashMap<CInteger,Long> realMap = new LinkedHashMap<CInteger,Long>();
		MapWithSearch<CInteger,Long> ourMap = createOurMap();
		for(int i=0;i<key.length;++i)
		{
			realMap.put(key[i], value[i]);
			ourMap.put(key[i], value[i]);
			Assert.assertEquals(new Long(value[0]),ourMap.get(key[0]));
		}
		for(int i=0;i<key.length;++i)
			Assert.assertEquals("failure at index "+i+" with key "+key[i]+", expected "+value[i]+" got "+ourMap.get(key[i]),new Long(value[i]),ourMap.get(key[i]));
		
		
		Random rnd = new Random(1);

		for(int i=0;i<20;++i)
		{
			int idx = rnd.nextInt(key.length);
			
			if (realMap.containsKey(key[idx]))
			{
				Assert.assertEquals(new Long(value[idx]),realMap.get(key[idx]));
				realMap.remove(key[idx]);ourMap.remove(key[idx]);
			}
			else
				Assert.assertFalse(ourMap.containsKey(key[idx]));
			
			compareForEquality(realMap,ourMap);
		}
	}
	
	/** Similar to above but where entries are changed. */
	@Test
	public void TestMapWithSearch_add_remove()
	{
		initKeysAndValues();
		LinkedHashMap<CInteger,Long> realMap = new LinkedHashMap<CInteger,Long>();
		MapWithSearch<CInteger,Long> ourMap = createOurMap();
		for(int i=0;i<key.length;++i)
		{
			realMap.put(key[i], value[i]);ourMap.put(key[i], value[i]);
		}

		Random rnd = new Random(1);

		for(int i=0;i<20;++i)
		{
			int idx = rnd.nextInt(key.length);
			
			CInteger rndKey = new CInteger(randomKey());long val = rnd.nextLong();
			if (realMap.containsKey(key[idx]))
			{
				Assert.assertEquals(new Long(value[idx]),realMap.get(key[idx]));
				Assert.assertEquals(new Long(value[idx]),ourMap.get(key[idx]));
				key[idx]=rndKey;value[idx]=val;
				Long origValue = realMap.put(key[idx],value[idx]);
				if (origValue == null)
					Assert.assertNull(ourMap.put(key[idx],value[idx]));
				else
					Assert.assertEquals(origValue,ourMap.put(key[idx],value[idx]));
			}
			else
				Assert.assertFalse(ourMap.containsKey(key[idx]));
			
			compareForEquality(realMap,ourMap);
		}
	}
	
	/** Similar to above but where entries are changed. */
	@Test
	public void TestMapWithSearch_putAll()
	{
		initKeysAndValues();
		LinkedHashMap<CInteger,Long> realMap = new LinkedHashMap<CInteger,Long>();
		MapWithSearch<CInteger,Long> ourMap = createOurMap();
		for(int i=0;i<key.length;++i)
		{
			realMap.put(key[i], value[i]);ourMap.put(key[i], value[i]);
		}

		MapWithSearch<CInteger,Long> ourMap2 = createOurMap();
		ourMap2.putAll(ourMap);
		LinkedHashMap<CInteger,Long> realMap2 = new LinkedHashMap<CInteger,Long>();
		realMap2.putAll(realMap);
		compareForEquality(realMap,ourMap);
		compareForEquality(realMap2,ourMap2);
		
		Random rnd = new Random(1);

		for(int i=0;i<20;++i)
		{
			int idx = rnd.nextInt(key.length);
			
			CInteger rndKey = new CInteger(randomKey());long val = rnd.nextLong();
			if (realMap.containsKey(key[idx]))
			{
				Assert.assertEquals(new Long(value[idx]),realMap.get(key[idx]));
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
	public void TestMapWithSearch_clear()
	{
		initKeysAndValues();
		LinkedHashMap<CInteger,Long> realMap = new LinkedHashMap<CInteger,Long>();
		MapWithSearch<CInteger,Long> ourMap = createOurMap();
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
	public void TestMapWithSearch_remove2()
	{
		initKeysAndValues();
		LinkedHashMap<CInteger,Long> realMap = new LinkedHashMap<CInteger,Long>();
		MapWithSearch<CInteger,Long> ourMap = createOurMap();
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
				Assert.assertEquals(new Long(value[idx]),realMap.get(key[idx]));
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
		initKeysAndValues();
		LinkedHashMap<CInteger,Long> realMap = new LinkedHashMap<CInteger,Long>();
		MapWithSearch<CInteger,Long> ourMap = createOurMap();
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
				Assert.assertEquals(new Long(value[idx]),realMap.get(key[idx]));
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
		MapWithSearch<CInteger,Long> ourMap = createOurMap();
		ourMap.put(new CInteger(3),5L);ourMap.put(new CInteger(2), 9L);
		TreeSet<CInteger> keys = new TreeSet<CInteger>();keys.addAll(ourMap.keySet());
		Assert.assertEquals("[2, 3]",keys.toString());
		ourMap.keySet().remove(new CInteger(2));
		keys = new TreeSet<CInteger>();keys.addAll(ourMap.keySet());
		Assert.assertEquals("[3]",keys.toString());	
	}

	@Test
	public void testInvalidModification2()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException 
			{
				createOurMap().values().remove(45);
			}
		}, UnsupportedOperationException.class, "modification");
	}

	@Test
	public void testInvalidModification3b()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException 
			{
				createOurMap().values().clear();
			}
		}, UnsupportedOperationException.class, "modification");
	}

	@Test
	public void testInvalidModification4()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException
			{
				createOurMap().entrySet().clear();
			}
		}, UnsupportedOperationException.class, "modification");
	}

}
