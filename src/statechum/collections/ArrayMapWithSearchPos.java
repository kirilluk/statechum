/* Copyright (c) 2012 Neil Walkinshaw and Kirill Bogdanov
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

import java.util.AbstractSet;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeMap;

import statechum.DeterministicDirectedSparseGraph.VertID;

/**
 * A map backed by an array. Expects key to be convertible to an integer and be non-negative.
 * If a map is a singleton, it is made an instance of a special class to save space. If a singleton
 * is expanded with new elements added and then those elements are removed, it does not revert to
 * a singleton for performance reasons. Such a behaviour is not necessary for the pattern of behaviour
 * utilised by the learner. In principle, it can be implemented so that when the number of elements drops to 
 * the array could be thrown away and reverted to a single pair.
 * <br>
 * Ordinarily, this would have been derived from {@link AbstractMap}, however in the interest of memory use
 * this is not done as it would bring in all the attributes of {@link AbstractMap} that we aim to avoid.
 * <br>
 * Important: this collection does not track modifications, so it will never throw {@link ConcurrentModificationException}
 * but instead may return completely wrong values. This is done to reduce memory footprint when storing
 * tens of millions of entries, some of which could be instances of this class.
 * 
 * @author kirill
 */
public class ArrayMapWithSearchPos<K extends ConvertibleToInt,V> implements MapWithSearch<K, V> {
	/** This is either
	 * <ul> 
	 * <li>null for an empty map,</li> 
	 * <li>a key (implementing {@link ConvertibleToInt}) for a map with a single element or</li> 
	 * <li>an array of keys and elements. The reason we record keys is to do with garbage
	 * collection: if a key is not recorded, it may be collected. Where we maintain a global 
	 * registry of all keys, we might actually want it to be collected but it will not be.
	 * For these reasons, it'll be an array twice the size of the number of elements.</li>
	 * </ul>
	 */
	Object array_or_key;
	
	/** This is either
	 * <ul> 
	 * <li>null for an empty map,</li> 
	 * <li>a value for a value where a map contains a single element,</li> 
	 * <li>unused otherwise. In principle, it can be used to count the number of non-nulls
	 * in the collection, but in this case I would have to keep a static collection of Integer-objects
	 * for different lengths (otherwise it'll consume +20 bytes or I'll have to introduce
	 * another attribue).</li>
	 * <li>child classes may use it for any other purpose.</li>
	 * </ul>. 
	 */
	V value;
	
	public ArrayMapWithSearchPos()
	{}
	
	public ArrayMapWithSearchPos(int currentSize)
	{
		int currentOffset = currentSize*CELLS_PER_ELEM;
		Object [] data = new Object[CELLS_PER_ELEM+currentOffset];
		array_or_key = data;
	}
	
	/** The new size for the array on each resize is determined by adding the current size divided by this number. */
	public static final int GROWTHRATE_DIV = 1;

	protected static final int CELLS_PER_ELEM=2; 

	@Override
	public int size() {
		if (array_or_key == null)
			return 0;
		if (array_or_key instanceof ConvertibleToInt)
			return 1;
		
		int count = 0;
		Object[] array = (Object[])array_or_key;
		for(int i=1;i<array.length;i+=CELLS_PER_ELEM)
			if (array[i] != null) ++count;
		
		return count;
	}

	@Override
	public boolean isEmpty() {
		return size() == 0;
	}

	@Override
	public boolean containsKey(Object key) {
		return get(key) != null;
	}

	@Override
	public boolean containsValue(Object v) {
		if (v == null || array_or_key == null)
			return false;
		if (array_or_key instanceof ConvertibleToInt)
			return value.equals(v);
		
		Object [] array = (Object[])array_or_key;
		for(int i=1;i<array.length;i+=CELLS_PER_ELEM)
			if (array[i] != null && array[i].equals(v))
				return true;
		return false;
	}

	@SuppressWarnings("unchecked")
	@Override
	public V get(Object k) {
		if (k == null)
			return null;
		if (array_or_key == null)
			return null;
		int kIndex = ((ConvertibleToInt)k).toInt();
		if (array_or_key instanceof ConvertibleToInt)
		{
			if ( ((ConvertibleToInt)array_or_key).toInt() == kIndex)
				return value;
			
			return null;
		}
		Object[] array = (Object[])array_or_key;
		int offset = kIndex*CELLS_PER_ELEM;
		if (kIndex < 0 || offset >= array.length)
			return null;
		return (V)array[1+offset];
	}

	@SuppressWarnings("unchecked")
	@Override
	public V put(K k, V v) {
		V outcome = null;
        if (k == null)
            throw new IllegalArgumentException("key cannot be null for ArrayMapWithSearch");
        if (v == null)
            throw new IllegalArgumentException("value cannot be null for ArrayMapWithSearch");
 		int kIndex = ((ConvertibleToInt)k).toInt();
		if (kIndex < 0)
			throw new IllegalArgumentException("negative indices are not supported");

		if (array_or_key == null)
		{
			array_or_key=k;value=v;
			// outcome remains null
		}
		else
		if (array_or_key instanceof ConvertibleToInt)
		{
			int currentIndex = ((ConvertibleToInt)array_or_key).toInt();
			if (currentIndex == kIndex)
			{// replacing the current value
				outcome = value;value=v;
			}
			else
			{// create a collection
				int currentOffset = currentIndex*CELLS_PER_ELEM, kOffset = kIndex*CELLS_PER_ELEM;
				Object[] data = new Object[CELLS_PER_ELEM+Math.max(kOffset,currentOffset)];
				data[currentOffset]=array_or_key;data[1+currentOffset]=value;
				data[kOffset]=k;data[1+kOffset]=v;
				array_or_key = data;
				
				// outcome remains null
			}
		}
		else
		{// collection already exists
			Object[] array= (Object[])array_or_key;
			int currentLength = array.length;
			int offset = kIndex*CELLS_PER_ELEM;
			
			if ( offset >= currentLength)
			{// resize upwards
				int newSize = currentLength + Math.max( offset, currentLength/GROWTHRATE_DIV);
				array = new Object[newSize];
				System.arraycopy(array_or_key, 0, array, 0, currentLength);
				array_or_key = array;
			}
			
			outcome = (V)array[1+offset];array[offset]=k;array[1+offset]=v;
		}
		
		return outcome;
	}

	// Remove or contains: null/invalid/existing/non-existing
	// Add: key is null/invalid/existing/non-existing
	// Add: value is null/existing/non-existing
	// Values: null/one/two/multiple with repeated values
	// Keyset: null/one/two/multiple
	// Initial collection for add/remove: empty/one/two/multiple
	@SuppressWarnings("unchecked")
	@Override
	public V remove(Object k) {
		V outcome = null;
		if (k == null || array_or_key == null)
			return null;
		int kIndex = ((ConvertibleToInt)k).toInt();
		if (array_or_key instanceof ConvertibleToInt)
		{
			if ( ((ConvertibleToInt)array_or_key).toInt() == kIndex)
			{// remove the only mapping
				outcome = value;
				clear();
			}
		}
		else
		{// we've got an array
			Object[] array = (Object[])array_or_key;
			int offset = kIndex*CELLS_PER_ELEM;
			if (kIndex >= 0 && offset < array.length)
			{
				outcome = (V)array[1+offset];
				array[offset]=null;array[1+offset]=null;
			}
		}
		return outcome;
	}

	@Override
	public void putAll(Map<? extends K, ? extends V> m) {
		for(java.util.Map.Entry<? extends K, ? extends V> entry:m.entrySet())
			put(entry.getKey(),entry.getValue());
	}

	@Override
	public void clear() {
		array_or_key = null;value = null;
	}

	@Override
	public Set<K> keySet() {
		return new AbstractSet<K>(){

			@Override
			public Iterator<K> iterator() {
				return new AnIterator<K>(){
					K lastValue = null;
					
					@SuppressWarnings("unchecked")
					@Override
					public K next() {
						K outcome = null;
						if (!hasNext())// in case we're storing an array, this call will move index to the next non-null if there is any. 
							throw new NoSuchElementException();
						
						if (array_or_key instanceof ConvertibleToInt)
						{
							nextIndex = -1;// this is the only element, force next to negative
							outcome = (K)array_or_key;							
						}
						else
						{
							outcome = (K) ((Object[])array_or_key)[nextIndex];
							nextIndex+=CELLS_PER_ELEM;// move to the next element, we'll be looking for non-nulls in the next call to hasNext()							
						}
						lastValue = outcome;
						return outcome;
					}
					
					@Override
					public void remove()
					{
						if (lastValue == null)
							throw new IllegalStateException("next was not yet called or was already called");
						ArrayMapWithSearchPos.this.remove(lastValue);
						lastValue = null;
					}
				};
			}

	        @Override
			public boolean contains(Object o) {
	            return containsKey(o);
	        }
	        
	        @Override
	        public boolean remove(Object o) {
	            return ArrayMapWithSearchPos.this.remove(o) != null;
	        }
	        
	        @Override
			public void clear() {
	        	ArrayMapWithSearchPos.this.clear();
	        }

			/* (non-Javadoc)
			 * @see java.util.AbstractCollection#add(java.lang.Object)
			 */
			@Override
			public boolean add(@SuppressWarnings("unused") K e) {
				throw new UnsupportedOperationException("modification of key set is not allowed for ArrayMapWithSearch");
			}

			/* (non-Javadoc)
			 * @see java.util.AbstractCollection#addAll(java.util.Collection)
			 */
			@Override
			public boolean addAll(@SuppressWarnings("unused") Collection<? extends K> c) {
				throw new UnsupportedOperationException("modification of key set is not allowed for ArrayMapWithSearch");
			}

			@Override
			public int size() {
				return ArrayMapWithSearchPos.this.size();
			}
		};
	}

	@Override
	public Collection<V> values() {
		return new AbstractSet<V>(){

			@Override
			public Iterator<V> iterator() {
				return new AnIterator<V>(){
					
					@SuppressWarnings("unchecked")
					@Override
					public V next() {
						V outcome = null;
						if (!hasNext())// in case we're storing an array, this call will move index to the next non-null if there is any. 
							throw new NoSuchElementException();
						
						if (array_or_key instanceof ConvertibleToInt)
						{
							nextIndex = -1;// this is the only element, force next to negative
							outcome = value;							
						}
						else
						{
							outcome = (V) ((Object[])array_or_key)[1+nextIndex];
							nextIndex+=CELLS_PER_ELEM;// move to the next element, we'll be looking for non-nulls in the next call to hasNext()							
						}
						return outcome;
					}
				};
			}

			@Override
			public int size() {
				return ArrayMapWithSearchPos.this.size();
			}
	       @Override
			public boolean contains(Object o) {
	            return containsValue(o);
	        }
	        @Override
			public void clear() {
	        	throw new UnsupportedOperationException("modification of value set is not allowed for ArrayMapWithSearch");
	        }
	        @Override
	 		public boolean remove(@SuppressWarnings("unused") Object o) {
	         	throw new UnsupportedOperationException("modification of value set is not allowed for ArrayMapWithSearch");
	         }
	        @Override
	        public boolean removeAll(@SuppressWarnings("unused") Collection<?> c) {
	         	throw new UnsupportedOperationException("modification of value set is not allowed for ArrayMapWithSearch");
	        }
	        @Override
	        public boolean retainAll(@SuppressWarnings("unused") Collection<?> c) {
	         	throw new UnsupportedOperationException("modification of value set is not allowed for ArrayMapWithSearch");
	        }
			/* (non-Javadoc)
			 * @see java.util.AbstractCollection#add(java.lang.Object)
			 */
			@Override
			public boolean add(@SuppressWarnings("unused") V e) {
	         	throw new UnsupportedOperationException("modification of value set is not allowed for ArrayMapWithSearch");
			}
			/* (non-Javadoc)
			 * @see java.util.AbstractCollection#addAll(java.util.Collection)
			 */
			@Override
			public boolean addAll(@SuppressWarnings("unused") Collection<? extends V> c) {
	         	throw new UnsupportedOperationException("modification of value set is not allowed for ArrayMapWithSearch");
			}
		};
	}

	@Override
	public String toString()
	{
		StringBuffer outcome = new StringBuffer();outcome.append('{');
		boolean first = true;
		for(java.util.Map.Entry<K, V> entry:entrySet())
		{
			if (!first) outcome.append(", ");else first=false;
			outcome.append(entry.toString());
		}
		outcome.append('}');return outcome.toString();
	}
	
	abstract class AnIterator<IT> implements Iterator<IT>
	{
		int nextIndex = 0;// the first value in an array
		@Override
		public boolean hasNext() {
			if (array_or_key == null)
				return false;
			if (!(array_or_key instanceof ConvertibleToInt))
				updateIndexToNextElement();// we've got an array, position the index to the next non-null
			
			return nextIndex >= 0;
		}

		private void updateIndexToNextElement()
		{
			if (nextIndex >= 0 && array_or_key != null && !(array_or_key instanceof ConvertibleToInt))
			{
				Object[] array=(Object[])array_or_key;
				while(nextIndex < array.length && array[nextIndex] == null)
					nextIndex+=CELLS_PER_ELEM;
				if (nextIndex >= array.length)
					nextIndex = -1;// failed to find another element
			}
		}

		@Override
		public void remove() {
            throw new UnsupportedOperationException("modification of iterator is not allowed for ArrayMapWithSearch");
		}
	}
	
	abstract class Entry<A,B> implements java.util.Map.Entry<A,B>
	{
		@Override
		public int hashCode()
		{
			return getKey().hashCode() ^ getValue().hashCode();
		}
		
		@Override
		public boolean equals(Object obj)
		{
			if (this == obj)
				return true;
			if (!(obj instanceof java.util.Map.Entry))
				return false;
			@SuppressWarnings("unchecked")
			java.util.Map.Entry<A,B> o = (java.util.Map.Entry<A,B>)obj;
			return o.getKey().equals(o.getKey()) && o.getValue().equals(o.getValue());
		}
		
		@Override
		public String toString()
		{
			return getKey().toString()+"="+getValue().toString();
		}
	}
	
	@Override
	public Set<java.util.Map.Entry<K, V>> entrySet() {
		return new AbstractSet<Map.Entry<K,V>>(){

			@Override
			public Iterator<java.util.Map.Entry<K, V>> iterator() {
				return new AnIterator<java.util.Map.Entry<K, V>>(){
					
					@Override
					public Entry<K, V> next() {
						Entry<K, V> outcome = null;
						if (!hasNext())// in case we're storing an array, this call will move index to the next non-null if there is any. 
							throw new NoSuchElementException();
						
						if (array_or_key instanceof ConvertibleToInt)
						{
							nextIndex = -1;// this is the only element, force next to negative
							outcome = new Entry<K, V>()
							{

								@SuppressWarnings("unchecked")
								@Override
								public K getKey() {
									return (K)array_or_key;
								}

								@SuppressWarnings("cast")
								@Override
								public V getValue() {
									return (V)value;
								}

								@Override
								public V setValue(V v) {
									V returnValue = value;
									value = v;
									return returnValue;
								}};
						}
						else
						{
							outcome = new Entry<K, V>()
							{
								private final int currentIndex = nextIndex;
								
								@SuppressWarnings("unchecked")
								@Override
								public K getKey() {
									Object[] array=(Object[])array_or_key;
									return (K)array[currentIndex]; 
								}

								@SuppressWarnings("unchecked")
								@Override
								public V getValue() {
									Object[] array=(Object[])array_or_key;
									return (V)array[currentIndex+1]; 
								}

								@SuppressWarnings("unchecked")
								@Override
								public V setValue(V v) {
									Object[] array=(Object[])array_or_key;
									V returnValue = (V)array[currentIndex+1];
									array[currentIndex+1]=v;
									return returnValue;
								}};
							nextIndex+=CELLS_PER_ELEM;// move to the next element, we'll be looking for non-nulls in the next call to hasNext()
						}
						
						return outcome;
					}
				};
			}

			@Override
			public int size() {
				return ArrayMapWithSearchPos.this.size();
			}

			/* (non-Javadoc)
			 * @see java.util.AbstractSet#removeAll(java.util.Collection)
			 */
			@Override
			public boolean removeAll(@SuppressWarnings("unused") Collection<?> c) {
	            throw new UnsupportedOperationException("modification of entry set is not allowed for ArrayMapWithSearch");
			}

			/* (non-Javadoc)
			 * @see java.util.AbstractCollection#add(java.lang.Object)
			 */
			@Override
			public boolean add(@SuppressWarnings("unused") java.util.Map.Entry<K, V> e) {
	            throw new UnsupportedOperationException("modification of entry set is not allowed for ArrayMapWithSearch");
			}

			/* (non-Javadoc)
			 * @see java.util.AbstractCollection#remove(java.lang.Object)
			 */
			@Override
			public boolean remove(@SuppressWarnings("unused") Object o) {
	            throw new UnsupportedOperationException("modification of entry set is not allowed for ArrayMapWithSearch");
			}

			/* (non-Javadoc)
			 * @see java.util.AbstractCollection#addAll(java.util.Collection)
			 */
			@Override
			public boolean addAll(@SuppressWarnings("unused") Collection<? extends java.util.Map.Entry<K, V>> c) {
	            throw new UnsupportedOperationException("modification of entry set is not allowed for ArrayMapWithSearch");
			}

			/* (non-Javadoc)
			 * @see java.util.AbstractCollection#retainAll(java.util.Collection)
			 */
			@Override
			public boolean retainAll(@SuppressWarnings("unused") Collection<?> c) {
	            throw new UnsupportedOperationException("modification of entry set is not allowed for ArrayMapWithSearch");
			}

			/* (non-Javadoc)
			 * @see java.util.AbstractCollection#clear()
			 */
			@Override
			public void clear() {
	            throw new UnsupportedOperationException("modification of entry set is not allowed for ArrayMapWithSearch");
			}};
	}

	@SuppressWarnings("unchecked")
	@Override
	public K findElementById(VertID id) {
		if (id == null)
			return null;
		if (array_or_key == null)
			return null;
		int kIndex = id.toInt();
		if (array_or_key instanceof ConvertibleToInt)
		{
			if ( ((ConvertibleToInt)array_or_key).toInt() == kIndex)
				return (K)array_or_key;
			
			return null;
		}
		Object[] array = (Object[])array_or_key;
		int offset = kIndex*CELLS_PER_ELEM;
		if (kIndex < 0 || offset >= array.length)
			return null;
		return (K)array[offset];
	}

	@Override
	public Set<java.util.Map.Entry<K, V>> getTreeEntrySet() {
		TreeMap<K,V> treeMap = new TreeMap<K, V>();treeMap.putAll(this);return treeMap.entrySet();
	}

	@Override
	public Set<java.util.Map.Entry<K, V>> getPotentiallyOrderedEntrySet(boolean ordered) 
	{
		if (!ordered)
			return entrySet();
		return getTreeEntrySet();
	}

	@Override
	public Set<K> getPotentiallyOrderedKeySet(boolean ordered) 
	{
		if (!ordered)
			return keySet();
		
		TreeMap<K,V> treeMap = new TreeMap<K, V>();treeMap.putAll(this);return treeMap.keySet();
	}

	/** The hash code is the sum of hash codes of elements, for compatibility with Java collections.
	 */
	@Override
	public int hashCode() {
		int result = 0;
		for(java.util.Map.Entry<K, V> entry:entrySet())
			result+=entry.hashCode();
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
		if (!(obj instanceof Map))
			return false;
		@SuppressWarnings("unchecked")
		Map<K,V> other = (Map<K,V>) obj;
		if (size() != other.size())
			return false;
		for(java.util.Map.Entry<K, V> entry:entrySet())
			if (!entry.getValue().equals(other.get(entry.getKey())))
				return false;
		
		return true;
	}

	@Override
	public boolean expectsConvertibleToInt() {
		return true;
	}

}
