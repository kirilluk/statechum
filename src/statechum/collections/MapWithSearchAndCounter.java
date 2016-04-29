/* Copyright (c) 2012 The University of Sheffield
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */

package statechum.collections;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.Pair;

/** This is a facade to anything like {@link ArrayMapWithSearch} in that it keeps track of the number of data values to be stored or removed. As such, it consumes more memory
 * (which is not important since such collections are not used inside others) but offers an efficient way to determine the number of positive and negative entries. Therefore,
 * we will quickly know when to switch to HashMap without having to iterate through the collection. 
 * 
 * @author kirill
 *
 */
public class MapWithSearchAndCounter<K,V> implements MapWithSearch<K, V> {
	int positiveCount = 0, negativeCount = 0;
	boolean parentExpectsConvertibleToInt = false;
	
	protected MapWithSearch<K, V> parent = null;
	
	public MapWithSearchAndCounter(MapWithSearch<K, V> map) 
	{
		parent = map;parentExpectsConvertibleToInt = parent.expectsConvertibleToInt();
	}

	
	@Override
	public void clear() {
		if (isEmpty())
			return;
		positiveCount = 0;negativeCount = 0;
		parent.clear();
	}

	@Override
	public V put(K k, V v) {
		if (k == null)
			throw new IllegalArgumentException("Keys cannot be null");
		if (v == null)
			throw new IllegalArgumentException("Values cannot be null");
		V prevValue = parent.put(k, v);

		if (prevValue == null)
		{// did not have a previous value.
			if (parentExpectsConvertibleToInt)
			{
				if (((ConvertibleToInt) k).toInt() >= 0)
					positiveCount+=1;
				else
					negativeCount+=1;
			}
			else
				positiveCount++;
		}
		
		return prevValue;
	}

	@Override
	public V remove(Object k) {
		if (k == null)
			throw new IllegalArgumentException("Keys cannot be null");

		V prevValue = parent.remove(k);
		if (prevValue != null)
		{// had a previous value.
			if (parentExpectsConvertibleToInt)
			{
				if (((ConvertibleToInt) k).toInt() >= 0)
					positiveCount-=1;
				else
					negativeCount-=1;
			}
			else
				positiveCount--;
		}
		
		return prevValue;
	}

	@Override
	public int size() {
		return negativeCount + positiveCount;
	}

	@Override
	public boolean isEmpty() {
		return negativeCount == 0 && positiveCount == 0;
	}

	/** Returns the number of positive and negative values in the collection. */
	public Pair<Integer,Integer> getPosNeg()
	{
		return new Pair<Integer,Integer>(positiveCount,negativeCount);
	}


	@Override
	public boolean containsKey(Object key) {
		return parent.containsKey(key);
	}


	@Override
	public boolean containsValue(Object value) {
		return parent.containsValue(value);
	}


	@Override
	public V get(Object key) {
		return parent.get(key);
	}


	@Override
	public void putAll(Map<? extends K, ? extends V> m) {
		for(java.util.Map.Entry<? extends K, ? extends V> entry:m.entrySet())
			put(entry.getKey(),entry.getValue());
	}


	@Override
	public Set<K> keySet() {
		final Set<K> returnedKeys = parent.keySet();
		final MapWithSearchAndCounter<K, V> map = this;
		return new Set<K>(){

			/* (non-Javadoc)
			 * @see java.lang.Object#hashCode()
			 */
			@Override
			public int hashCode() {
				return returnedKeys.hashCode();
			}

			/* (non-Javadoc)
			 * @see java.lang.Object#equals(java.lang.Object)
			 */
			@Override
			public boolean equals(Object obj) {
				return returnedKeys.equals(obj);
			}

			/* (non-Javadoc)
			 * @see java.lang.Object#toString()
			 */
			@Override
			public String toString() {
				return returnedKeys.toString();
			}

			@Override
			public int size() {
				return map.size();
			}

			@Override
			public boolean isEmpty() {
				return map.isEmpty();
			}

			@Override
			public boolean contains(Object o) {
				return returnedKeys.contains(o);
			}

			@Override
			public Iterator<K> iterator() {
				return returnedKeys.iterator();
			}

			@Override
			public Object[] toArray() {
				return returnedKeys.toArray();
			}

			@Override
			public <T> T[] toArray(T[] a) {
				return returnedKeys.toArray(a);
			}

			@Override
			public boolean add(@SuppressWarnings("unused") K e) {
				throw new UnsupportedOperationException("adding to a collection of states is not supported");
			}

			@Override
			public boolean remove(Object o) {
				return map.remove(o) != null;
			}

			@Override
			public boolean containsAll(Collection<?> c) {
				return returnedKeys.containsAll(c);
			}

			@Override
			public boolean addAll(@SuppressWarnings("unused") Collection<? extends K> c) {
				throw new UnsupportedOperationException("adding to a collection of states is not supported");
			}

			@Override
			public boolean retainAll(@SuppressWarnings("unused") Collection<?> c) {
				throw new UnsupportedOperationException("removing some keys is not supported");
			}

			@Override
			public boolean removeAll(Collection<?> c) {
				boolean returnValue = false;
				for(Object elem:c)
					returnValue |=remove(elem);
				return returnValue;
			}

			@Override
			public void clear() {
				map.clear();
			}
			
		};
	}


	@Override
	public Collection<V> values() {
		final Collection<V> returnedValues = parent.values();
		final MapWithSearchAndCounter<K, V> map = this;
		return new Collection<V>()	{

			/* (non-Javadoc)
			 * @see java.lang.Object#hashCode()
			 */
			@Override
			public int hashCode() {
				return returnedValues.hashCode();
			}

			/* (non-Javadoc)
			 * @see java.lang.Object#equals(java.lang.Object)
			 */
			@Override
			public boolean equals(Object obj) {
				return returnedValues.equals(obj);
			}

			/* (non-Javadoc)
			 * @see java.lang.Object#toString()
			 */
			@Override
			public String toString() {
				return returnedValues.toString();
			}

			@Override
			public int size() {
				return map.size();
			}

			@Override
			public boolean isEmpty() {
				return map.isEmpty();
			}

			@Override
			public boolean contains(Object o) {
				return returnedValues.contains(o);
			}

			@Override
			public Iterator<V> iterator() {
				return returnedValues.iterator();
			}

			@Override
			public Object[] toArray() {
				return returnedValues.toArray();
			}

			@Override
			public <T> T[] toArray(T[] a) {
				return returnedValues.toArray(a);
			}

			@Override
			public boolean add(@SuppressWarnings("unused") V e) {
				throw new UnsupportedOperationException("adding transitions by manipulating values() of transition matrix is not supported");
			}

			@Override
			public boolean remove(@SuppressWarnings("unused") Object o) {
				throw new UnsupportedOperationException("removing transitions by manipulating values() of transition matrix is not supported");
			}

			@Override
			public boolean containsAll(Collection<?> c) {
				return returnedValues.containsAll(c);
			}

			@Override
			public boolean addAll(@SuppressWarnings("unused") Collection<? extends V> c) {
				throw new UnsupportedOperationException("adding transitions by manipulating values() of transition matrix is not supported");
			}

			@Override
			public boolean removeAll(@SuppressWarnings("unused") Collection<?> c) {
				throw new UnsupportedOperationException("removing transitions by manipulating values() of transition matrix is not supported");
			}

			@Override
			public boolean retainAll(@SuppressWarnings("unused") Collection<?> c) {
				throw new UnsupportedOperationException("manipulating values() of transition matrix is not supported");
			}

			@Override
			public void clear() {
				map.clear();
			}
	
		};
	}


	@Override
	public Set<java.util.Map.Entry<K, V>> entrySet() {
		final Set<java.util.Map.Entry<K, V>> returnedEntries = parent.entrySet();
		final MapWithSearchAndCounter<K, V> map = this;
		return new Set<java.util.Map.Entry<K, V>>()
		{

			/* (non-Javadoc)
			 * @see java.lang.Object#hashCode()
			 */
			@Override
			public int hashCode() {
				return returnedEntries.hashCode();
			}

			/* (non-Javadoc)
			 * @see java.lang.Object#equals(java.lang.Object)
			 */
			@Override
			public boolean equals(Object obj) {
				return returnedEntries.equals(obj);
			}

			/* (non-Javadoc)
			 * @see java.lang.Object#toString()
			 */
			@Override
			public String toString() {
				return returnedEntries.toString();
			}

			@Override
			public int size() {
				return map.size();
			}

			@Override
			public boolean isEmpty() {
				return map.isEmpty();
			}

			@Override
			public boolean contains(Object o) {
				return returnedEntries.contains(o);
			}

			@Override
			public Iterator<Entry<K,V>> iterator() {
				return returnedEntries.iterator();
			}

			@Override
			public Object[] toArray() {
				return returnedEntries.toArray();
			}

			@Override
			public <T> T[] toArray(T[] a) {
				return returnedEntries.toArray(a);
			}

			@Override
			public boolean add(@SuppressWarnings("unused") java.util.Map.Entry<K, V> e) {
				throw new UnsupportedOperationException("adding transitions by manipulating entries of transition matrix is not supported");
			}

			@Override
			public boolean remove(@SuppressWarnings("unused") Object o) {
				throw new UnsupportedOperationException("removing transitions by manipulating entries of transition matrix is not supported");
			}

			@Override
			public boolean containsAll(Collection<?> c) {
				return returnedEntries.containsAll(c);
			}

			@Override
			public boolean addAll(@SuppressWarnings("unused") Collection<? extends java.util.Map.Entry<K, V>> c) {
				throw new UnsupportedOperationException("adding transitions by manipulating entries of transition matrix is not supported");
			}

			@Override
			public boolean retainAll(@SuppressWarnings("unused") Collection<?> c) {
				throw new UnsupportedOperationException("removing transitions by manipulating entries of transition matrix is not supported");
			}

			@Override
			public boolean removeAll(@SuppressWarnings("unused") Collection<?> c) {
				throw new UnsupportedOperationException("removing transitions by manipulating entries of transition matrix is not supported");
			}

			@Override
			public void clear() {
				map.clear();
			}
			
		};
	}


	@Override
	public K findElementById(VertID id) {
		return parent.findElementById(id);
	}


	@Override
	public Set<java.util.Map.Entry<K, V>> getTreeEntrySet() {
		return parent.getTreeEntrySet();
	}


	@Override
	public Set<java.util.Map.Entry<K, V>> getPotentiallyOrderedEntrySet(boolean ordered) {
		return parent.getPotentiallyOrderedEntrySet(ordered);
	}


	@Override
	public Set<K> getPotentiallyOrderedKeySet(boolean ordered) {
		return parent.getPotentiallyOrderedKeySet(ordered);
	}

	/** Delegates to the parent class.
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return parent.hashCode();
	}


	/** Delegates to the parent class.
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		return parent.equals(obj);
	}


	/** Delegates to the parent class.
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return parent.toString();
	}


	@Override
	public boolean expectsConvertibleToInt() {
		return parentExpectsConvertibleToInt;
	}
}
