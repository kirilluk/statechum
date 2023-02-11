/*
 * Copyright (c) 2000, 2010, Oracle and/or its affiliates. All rights reserved.
 * ORACLE PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 *
 *
 * This file is essentially an adapted HashMapWithSearch/HashMap
*/

package statechum.collections;

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeSet;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.DeterministicDirectedSparseGraph.VertexID;


public class HashMapWithSearch<K,V> implements MapWithSearch<K,V>
{
	private static int collisionNumber=0;
	
	public static int getCollisions()
	{
		return collisionNumber;
	}
	
    /**
     * The default initial capacity - MUST be a power of two.
     */
    public static final int DEFAULT_INITIAL_CAPACITY = 16;

    /**
     * The maximum capacity, used if a higher value is implicitly specified
     * by either of the constructors with arguments.
     * MUST be a power of two <= 1<<30.
     */
    static final int MAXIMUM_CAPACITY = 1 << 30;

    /**
     * The load factor used when none specified in constructor.
     */
    static final float DEFAULT_LOAD_FACTOR = 0.75f;

    /**
     * The table, resized as necessary. Length MUST Always be a power of two.
     */
    transient Entry<K,V>[] table;

    /**
     * The number of key-value mappings contained in this map.
     */
    transient int size;

    /**
     * The next size value at which to resize (capacity * load factor).
     * @serial
     */
    int threshold;

    /**
     * The load factor for the hash table.
     *
     * @serial
     */
    final float loadFactor;

    /** A view, from {@link HashSet}. */
    private transient Set<Map.Entry<K,V>> entrySet = null;
    
    /** A view, from {@link TreeSet}. */
    private transient Set<Map.Entry<K,V>> entrySetOrdered = null;
    
    
    /** A view, from {@link HashSet}. */
    transient volatile Set<K>        keySet = null;
    
    /** A view, from {@link TreeSet}. */
    transient volatile Set<K>        keySetOrdered = null;
    
    transient volatile Collection<V> values = null;
    
    /**
     * The number of times this HashMap has been structurally modified
     * Structural modifications are those that change the number of mappings in
     * the HashMap or otherwise modify its internal structure (e.g.,
     * rehash).  This field is used to make iterators on Collection-views of
     * the HashMap fail-fast.  (See ConcurrentModificationException).
     */
    transient int modCount;

    /**
     * The default threshold of map capacity above which alternative hashing is
     * used for String keys. Alternative hashing reduces the incidence of
     * collisions due to weak hash code calculation for String keys.
     * <p/>
     * This value may be overridden by defining the system property
     * {@code jdk.map.althashing.threshold}. A property value of {@code 1}
     * forces alternative hashing to be used at all times whereas
     * {@code -1} value ensures that alternative hashing is never used.
     */
    static final int ALTERNATIVE_HASHING_THRESHOLD_DEFAULT = Integer.MAX_VALUE;

    /**
     * The head of the doubly linked list.
     */
    transient Entry<K,V> header;

    /** When used to store a map from a state {@link CmpVertex} to something else, the initial size is best chosen based on the maximal number of states.
     * 
     * @param stateNumber maximal expected number of states
     */
    public HashMapWithSearch(@SuppressWarnings("unused") int stateNumber)
    {
    	this(DEFAULT_INITIAL_CAPACITY, DEFAULT_LOAD_FACTOR);
    	//this(stateNumber, DEFAULT_LOAD_FACTOR);
    }
    
    /**
     * Constructs an empty <tt>HashMap</tt> with the specified initial
     * capacity and load factor.
     *
     * @param  initialCapacity the initial capacity
     * @param  loadFactorArg      the load factor
     * @throws IllegalArgumentException if the initial capacity is negative
     *         or the load factor is non-positive
     */
    protected HashMapWithSearch(int initialCapacityArg, float loadFactorArg) {
        if (initialCapacityArg < 0)
            throw new IllegalArgumentException("Illegal initial capacity: " +
                                               initialCapacityArg);
        int initialCapacity = initialCapacityArg;
        if (initialCapacity > MAXIMUM_CAPACITY)
            initialCapacity = MAXIMUM_CAPACITY;
        if (loadFactorArg <= 0 || Float.isNaN(loadFactorArg))
            throw new IllegalArgumentException("Illegal load factor: " + loadFactorArg);

        // Find a power of 2 >= initialCapacity
        int capacity = 1;
        while (capacity < initialCapacity)
            capacity <<= 1;

        this.loadFactor = loadFactorArg;
        threshold = (int)Math.min(capacity * loadFactorArg, MAXIMUM_CAPACITY + 1);
        table = new Entry[capacity];
        init();
    }

    
    
    /**
     * Called by clone before any entries are inserted into the map.  Initializes
     * the chain.
     */
    void init() {
        header = new Entry<K,V>(-1, null, null, null);
        header.before = header.after = header;
    }

    /**
     * Transfers all entries to new table array.  This method is called
     * by superclass resize.  It is overridden for performance, as it is
     * faster to iterate using our linked list.
     */
    void transfer(Entry<K,V>[] newTable, boolean rehash) {
        int newCapacity = newTable.length;
        for (Entry<K,V> e = header.after; e != header; e = e.after) {
            if (rehash)
                e.hash = (e.key == null) ? 0 : hash(e.key);
            int index = indexFor(e.hash, newCapacity);
            e.next = newTable[index];
            newTable[index] = e;
        }
    }

    /**
     * Returns <tt>true</tt> if this map contains a mapping for the
     * specified key.
     *
     * @param   key   The key whose presence in this map is to be tested
     * @return <tt>true</tt> if this map contains a mapping for the specified
     * key.
     */
    @Override
	public boolean containsKey(Object key) {
        return getEntry(key) != null;
    }

    /**
     * Returns the value to which the specified key is mapped,
     * or {@code null} if this map contains no mapping for the key.
     *
     * <p>More formally, if this map contains a mapping from a key
     * {@code k} to a value {@code v} such that {@code (key==null ? k==null :
     * key.equals(k))}, then this method returns {@code v}; otherwise
     * it returns {@code null}.  (There can be at most one such mapping.)
     *
     * <p>A return value of {@code null} does not <i>necessarily</i>
     * indicate that the map contains no mapping for the key; it's also
     * possible that the map explicitly maps the key to {@code null}.
     * The {@link #containsKey containsKey} operation may be used to
     * distinguish these two cases.
     */
    @Override
	public V get(Object key) {
        Entry<K,V> e = getEntry(key);
        if (e == null)
            return null;
        e.recordAccess(this);
        return e.value;
    }

	@Override
	public K findElementById(VertID id) {
		Entry<K,V> e = getEntry(id);
        if (e == null)
            return null;
        e.recordAccess(this);
        return e.key;
	
	}

	/**
     * Removes all of the mappings from this map.
     * The map will be empty after this call returns.
     */
    @Override
	public void clear() {
        modCount++;Arrays.fill(table,null);
        /*Entry<K,V>[] tab = table;
        for (int i = 0; i < tab.length; i++)
            tab[i] = null;*/
        size = 0;

        header.before = header.after = header;
    }

    /**
     * Entry from {@link HashMapWithSearch}.
     */
    public static class Entry<K,V> implements Map.Entry<K, V>
    {
        final K key;
        V value;
        Entry<K,V> next;
        int hash;

        /**
         * Creates new entry.
         */
        Entry(int h, K k, V v, Entry<K,V> n) {
            value = v;
            next = n;
            key = k;
            hash = h;
        }

        @Override
		public final K getKey() {
            return key;
        }

        @Override
		public final V getValue() {
            return value;
        }

        /** Important: this will not invalidate any iterator over values stored in the collection, contrary
         * to what it is supposed to do. 
         */
        @Override
		public final V setValue(V newValue) {
            V oldValue = value;
            value = newValue;
            return oldValue;
        }

        @Override
		public final boolean equals(Object o) {
            if (!(o instanceof Map.Entry))
                return false;
            @SuppressWarnings("unchecked")
			Map.Entry<K,V> e = (Map.Entry<K,V>)o;
            Object k1 = getKey();
            Object k2 = e.getKey();
            if (k1 == k2 || (k1 != null && k1.equals(k2))) {
                Object v1 = getValue();
                Object v2 = e.getValue();
                if (v1 == v2 || (v1 != null && v1.equals(v2)))
                    return true;
            }
            return false;
        }

        @Override
		public final int hashCode() {
            return (key==null   ? 0 : key.hashCode()) ^
                   (value==null ? 0 : value.hashCode());
        }

        @Override
		public final String toString() {
            return getKey() + "=" + getValue();
        }

        
        // These fields comprise the doubly linked list used for iteration.
        Entry<K,V> before;
		Entry<K,V> after;

        /**
         * Removes this entry from the linked list.
         */
        private void remove() {
            before.after = after;
            after.before = before;
        }

        /**
         * Inserts this entry before the specified existing entry in the list.
         */
        void addBefore(Entry<K,V> existingEntry) {
            after  = existingEntry;
            before = existingEntry.before;
            before.after = this;
            after.before = this;
        }

        /**
         * Dummy method, a remnant from that of {@link HashMapWithSearch}
         */
        void recordAccess(@SuppressWarnings("unused") HashMapWithSearch<K,V> m) {
        }

        void recordRemoval(@SuppressWarnings("unused") HashMapWithSearch<K,V> m) {
            remove();
        }
    }    
    
    abstract class HashMapWithSearchAbstractIterator<T> implements Iterator<T> 
    {
        /**
         * The modCount value that the iterator believes that the backing
         * List should have.  If this expectation is violated, the iterator
         * has detected concurrent modification.
         */
        int expectedModCount = modCount;

        @Override
		public void remove() {
            throw new UnsupportedOperationException("modification of iterator is not allowed for HashMapWithSearch");
        }

        /** Checks whether the collection has been unexpectedly modified. Throws an exception, otherwise returns as normal. */ 
        protected void nextEntryCheck() 
        {
            if (modCount != expectedModCount)
                throw new ConcurrentModificationException();
        }
    }
    
    abstract class HashMapWithSearchIterator<T> extends HashMapWithSearchAbstractIterator<T> 
    {
        Entry<K,V> nextEntry    = header.after;

        @Override
		public boolean hasNext() {
            return nextEntry != header;
        }

        Map.Entry<K,V> nextEntry() {
        	nextEntryCheck();
        	
            if (!hasNext())
                throw new NoSuchElementException();

            Entry<K,V> e = nextEntry;
            nextEntry = e.after;
            return e;
        }
    }

    abstract class HashMapWithSearchOrderedIterator<T> extends HashMapWithSearchAbstractIterator<T> 
    {
    	final Iterator<K> keyIterator;
    	
    	HashMapWithSearchOrderedIterator()
    	{
    		TreeSet<K> localKeySet = new TreeSet<K>();
    		localKeySet.addAll(HashMapWithSearch.this.keySet());keyIterator = localKeySet.iterator();
    	}
    	
        @Override
		public boolean hasNext()
        {
            return keyIterator.hasNext();
        }

        Map.Entry<K,V> nextEntry() 
        {
        	nextEntryCheck();
        	
            if (!hasNext())
                throw new NoSuchElementException();

            return getEntry(keyIterator.next());
        }    	
    }
    
    class KeyIterator extends HashMapWithSearchIterator<K> {
    	K lastValue = null;
        @Override
		public K next() {
        	lastValue = nextEntry().getKey();
        	return lastValue;
        }

		/* (non-Javadoc)
		 * @see statechum.collections.HashMapWithSearch.HashMapWithSearchAbstractIterator#remove()
		 */
		@Override
		public void remove() {
			if (lastValue == null)
				throw new IllegalStateException("next was not yet called or was already called");
			HashMapWithSearch.this.remove(lastValue);lastValue = null;
			expectedModCount = modCount;// to permit more than a single remove to take place consecutively
		}
        
        
    }

    class KeyOrderedIterator extends HashMapWithSearchOrderedIterator<K> {
        @Override
		public K next() { return nextEntry().getKey(); }
    }

    class EntryIterator extends HashMapWithSearchIterator<Map.Entry<K,V>> {
        @Override
		public Map.Entry<K,V> next() { return nextEntry(); }
    }

    class EntryOrderedIterator extends HashMapWithSearchOrderedIterator<Map.Entry<K,V>> {
        @Override
		public Map.Entry<K,V> next() { return nextEntry(); }
    }

    class ValueIterator extends HashMapWithSearchIterator<V> {
        @Override
		public V next() { return nextEntry().getValue(); }
    }

    // These Overrides alter the behaviour of superclass view iterator() methods
    Iterator<K> newKeyIterator()   { return new KeyIterator();   }
    Iterator<K> newKeyOrderedIterator()   { return new KeyOrderedIterator();   }
    Iterator<Map.Entry<K,V>> newEntryIterator() { return new EntryIterator(); }
    Iterator<Map.Entry<K,V>> newEntryOrderedIterator() { return new EntryOrderedIterator(); }
    Iterator<V> newValueIterator()   { return new ValueIterator();   }

    /**
     * This override alters behaviour of superclass put method. It causes newly
     * allocated entry to get inserted at the end of the linked list and
     * removes the eldest entry if appropriate.
     */
    void addEntry(int hashArg, K key, V value, int bucketIndexArg) {
    	int hash=hashArg,bucketIndex=bucketIndexArg;    			
    	if ((size >= threshold) && (null != table[bucketIndex])) {
            resize(2 * table.length);
            hash = (null != key) ? hash(key) : 0;
            bucketIndex = indexFor(hash, table.length);
        }

        createEntry(hash, key, value, bucketIndex);
    }

    /**
     * This differs in {@link HashMapWithSearch#addEntry} from {@link HashMap#addEntry} in that it doesn't resize the
     * table or remove the eldest entry.
     */
    void createEntry(int hash, K key, V value, int bucketIndex) {
        Entry<K,V> old = table[bucketIndex];
        Entry<K,V> e = new Entry<K,V>(hash, key, value, old);
        table[bucketIndex] = e;
        e.addBefore(header);
        size++;
    }
    
    
    // The following part is from HashMap
    
    /**
     * Retrieve object hash code and applies a supplemental hash function to the
     * result hash, which defends against poor quality hash functions.  This is
     * critical because HashMap uses power-of-two length hash tables, that
     * otherwise encounter collisions for hashCodes that do not differ
     * in lower bits.
     */
    final int hash(Object k) {
        int h = 0;
/*
        if (k instanceof VertexID || k instanceof CmpVertex)
        	return k.hashCode();
        */
        h ^= k.hashCode();

        // This function ensures that hashCodes that differ only by
        // constant multiples at each bit position have a bounded
        // number of collisions (approximately 8 at default load factor).
        
        
        h ^= (h >>> 20) ^ (h >>> 12);
        return h ^ (h >>> 7) ^ (h >>> 4);
    }

    /**
     * Returns index for hash code h.
     */
    static int indexFor(int h, int length) {
        return h & (length-1);
    }

    /**
     * Returns the number of key-value mappings in this map.
     *
     * @return the number of key-value mappings in this map
     */
    @Override
	public int size() {
        return size;
    }

    /**
     * Returns <tt>true</tt> if this map contains no key-value mappings.
     *
     * @return <tt>true</tt> if this map contains no key-value mappings
     */
    @Override
	public boolean isEmpty() {
        return size == 0;
    }

    
    /**
     * Returns the entry associated with the specified key in the
     * HashMap.  Returns null if the HashMap contains no mapping
     * for the key.
     */
    final Entry<K,V> getEntry(Object key) {
        int hash = (key == null) ? 0 : hash(key);
        for (Entry<K,V> e = table[indexFor(hash, table.length)];
             e != null;
             e = e.next) {
            Object k;
            if (e.hash == hash &&
                ((k = e.key) == key || (key != null && 
                	(	key.equals(k) || (key instanceof VertexID && k instanceof CmpVertex && ((CmpVertex)k).equals(key)) )  )))
                return e;
            ++collisionNumber;
        }
        return null;
    }


    /**
     * Associates the specified value with the specified key in this map.
     * If the map previously contained a mapping for the key, the old
     * value is replaced.
     *
     * @param key key with which the specified value is to be associated
     * @param value value to be associated with the specified key
     * @return the previous value associated with <tt>key</tt>, or
     *         <tt>null</tt> if there was no mapping for <tt>key</tt>.
     *         (A <tt>null</tt> return can also indicate that the map
     *         previously associated <tt>null</tt> with <tt>key</tt>.)
     */
    @Override
	public V put(K key, V value) {
        if (key == null)
            throw new IllegalArgumentException("key cannot be null for HashMapWithSearch");
        if (value == null)
            throw new IllegalArgumentException("value cannot be null for HashMapWithSearch");
        
        int hash = hash(key);
        int i = indexFor(hash, table.length);
        for (Entry<K,V> e = table[i]; e != null; e = e.next) {
            Object k;
            if (e.hash == hash && ((k = e.key) == key || key.equals(k))) {
                V oldValue = e.value;
                e.value = value;
                e.recordAccess(this);
                return oldValue;
            }
        }

        modCount++;
        addEntry(hash, key, value, i);
        return null;
    }

    /**
     * Rehashes the contents of this map into a new array with a
     * larger capacity.  This method is called automatically when the
     * number of keys in this map reaches its threshold.
     *
     * If current capacity is MAXIMUM_CAPACITY, this method does not
     * resize the map, but sets threshold to Integer.MAX_VALUE.
     * This has the effect of preventing future calls.
     *
     * @param newCapacity the new capacity, MUST be a power of two;
     *        must be greater than current capacity unless current
     *        capacity is MAXIMUM_CAPACITY (in which case value
     *        is irrelevant).
     */
    void resize(int newCapacity) {
        Entry<K,V>[] oldTable = table;
        int oldCapacity = oldTable.length;
        if (oldCapacity == MAXIMUM_CAPACITY) {
            threshold = Integer.MAX_VALUE;
            return;
        }

        @SuppressWarnings("unchecked")
		Entry<K,V>[] newTable = new Entry[newCapacity];
        transfer(newTable, false);
        table = newTable;
        threshold = (int)Math.min(newCapacity * loadFactor, MAXIMUM_CAPACITY + 1);
    }
  
    /**
     * Returns a {@link Set} view of the mappings contained in this map.
     * The set is backed by the map, so changes to the map are
     * reflected in the set. No changes via this set are permitted.
     *
     * @return a set view of the mappings contained in this map
     */
    @Override
	public Set<Map.Entry<K,V>> entrySet() {
        return entrySet0();
    }

    private Set<Map.Entry<K,V>> entrySet0() {
        Set<Map.Entry<K,V>> es = entrySet;
        return es != null ? es : (entrySet = new EntrySet());
    }

    private class EntrySet extends AbstractSet<Map.Entry<K,V>> {
    	
    	public EntrySet() {
			// does not do anything
		}
    	
		/** Every time an iterator is created we make a copy of modification count which makes it possible to detect modifications in the process of enumeration. */
        @Override
		public Iterator<Map.Entry<K,V>> iterator() {
            return newEntryIterator();
        }
        @Override
		public boolean contains(Object o) {
            if (!(o instanceof Map.Entry))
                return false;
            @SuppressWarnings("unchecked")
			Map.Entry<K,V> e = (Map.Entry<K,V>) o;
            Entry<K,V> candidate = getEntry(e.getKey());
            return candidate != null && candidate.equals(e);
        }
                
        @Override
		public boolean remove(@SuppressWarnings("unused") Object o) {
            throw new UnsupportedOperationException("modification of entry set is not allowed for HashMapWithSearch");
        }
        @Override
		public int size() {
            return size;
        }
        @Override
		public void clear() {
            throw new UnsupportedOperationException("modification of entry set is not allowed for HashMapWithSearch");
        }
        @Override
        public boolean removeAll(@SuppressWarnings("unused") Collection<?> c) {
         	throw new UnsupportedOperationException("modification of entry set is not allowed for HashMapWithSearch");
        }

		/* (non-Javadoc)
		 * @see java.util.AbstractCollection#add(java.lang.Object)
		 */
		@Override
		public boolean add(@SuppressWarnings("unused") java.util.Map.Entry<K, V> e) {
         	throw new UnsupportedOperationException("modification of entry set is not allowed for HashMapWithSearch");
		}

		/* (non-Javadoc)
		 * @see java.util.AbstractCollection#addAll(java.util.Collection)
		 */
		@Override
		public boolean addAll(@SuppressWarnings("unused") Collection<? extends java.util.Map.Entry<K, V>> c) {
         	throw new UnsupportedOperationException("modification of entry set is not allowed for HashMapWithSearch");
		}

		/* (non-Javadoc)
		 * @see java.util.AbstractCollection#retainAll(java.util.Collection)
		 */
		@Override
		public boolean retainAll(@SuppressWarnings("unused") Collection<?> c) {
        	throw new UnsupportedOperationException("modification of entry set is not allowed for HashMapWithSearch");
		}
     }

    /**
     * Returns a {@link Set} view of the mappings contained in this map.
     * The set is backed by the map, so changes to the map are
     * reflected in the set. No changes via this set are permitted.
     *
     * @return a set view of the mappings contained in this map
     */
	@Override
	public Set<java.util.Map.Entry<K, V>> getTreeEntrySet() {
		return entrySetOrdered0();
	}

    private Set<Map.Entry<K,V>> entrySetOrdered0() {
        Set<Map.Entry<K,V>> es = entrySetOrdered;
        return es != null ? es : (entrySetOrdered = new EntrySet()
        {
            @Override
    		public Iterator<Map.Entry<K,V>> iterator() {
                return newEntryOrderedIterator();
            }
        });
    }

    /** Copy of the one from HashMapWithSearch
     * 
     * Returns <tt>true</tt> if this map maps one or more keys to the
     * specified value.
     *
     * @param value value whose presence in this map is to be tested
     * @return <tt>true</tt> if this map maps one or more keys to the
     *         specified value
     */
    @Override
	public boolean containsValue(Object value) {
        // Overridden to take advantage of faster iterator
        if (value==null) {
            for (Entry<K,V> e = header.after; e != header; e = e.after)
                if (e.value==null)
                    return true;
        } else {
            for (Entry<K,V> e = header.after; e != header; e = e.after)
                if (value.equals(e.value))
                    return true;
        }
        return false;
    }

    /**
     * Removes the mapping for the specified key from this map if present.
     *
     * @param  key key whose mapping is to be removed from the map
     * @return the previous value associated with <tt>key</tt>, or
     *         <tt>null</tt> if there was no mapping for <tt>key</tt>.
     *         (A <tt>null</tt> return can also indicate that the map
     *         previously associated <tt>null</tt> with <tt>key</tt>.)
     */
    @Override
    public V remove(Object key) {
        Entry<K,V> e = removeEntryForKey(key);
        return (e == null ? null : e.value);
    }

    /**
     * Removes and returns the entry associated with the specified key
     * in the HashMap.  Returns null if the HashMap contains no mapping
     * for this key.
     */
    final Entry<K,V> removeEntryForKey(Object key) {
        int hash = (key == null) ? 0 : hash(key);
        int i = indexFor(hash, table.length);
        Entry<K,V> prev = table[i];
        Entry<K,V> e = prev;

        while (e != null) {
            Entry<K,V> next = e.next;
            Object k;
            if (e.hash == hash &&
                ((k = e.key) == key || (key != null && key.equals(k)))) {
                modCount++;
                size--;
                if (prev == e)
                    table[i] = next;
                else
                    prev.next = next;
                e.recordRemoval(this);
                return e;
            }
            prev = e;
            e = next;
        }

        return e;
    }


    /**
     * Copies all of the mappings from the specified map to this map.
     * These mappings will replace any mappings that this map had for
     * any of the keys currently in the specified map.
     *
     * @param m mappings to be stored in this map
     * @throws NullPointerException if the specified map is null
     */
    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        int numKeysToBeAdded = m.size();
        if (numKeysToBeAdded == 0)
            return;

        /*
         * Expand the map if the map if the number of mappings to be added
         * is greater than or equal to threshold.  This is conservative; the
         * obvious condition is (m.size() + size) >= threshold, but this
         * condition could result in a map with twice the appropriate capacity,
         * if the keys to be added overlap with the keys already in this map.
         * By using the conservative calculation, we subject ourself
         * to at most one extra resize.
         */
        if (numKeysToBeAdded > threshold) {
            int targetCapacity = (int)(numKeysToBeAdded / loadFactor + 1);
            if (targetCapacity > MAXIMUM_CAPACITY)
                targetCapacity = MAXIMUM_CAPACITY;
            int newCapacity = table.length;
            while (newCapacity < targetCapacity)
                newCapacity <<= 1;
            if (newCapacity > table.length)
                resize(newCapacity);
        }

        for (Map.Entry<? extends K, ? extends V> e : m.entrySet())
            put(e.getKey(), e.getValue());
    }


    /**
     * Returns a {@link Set} view of the keys contained in this map.
     * The set is backed by the map, so changes to the map are
     * reflected in the set, and vice-versa.  If the map is modified
     * while an iteration over the set is in progress (except through
     * the iterator's own <tt>remove</tt> operation), the results of
     * the iteration are undefined.  The set supports element removal,
     * which removes the corresponding mapping from the map, via the
     * <tt>Iterator.remove</tt>, <tt>Set.remove</tt>,
     * <tt>removeAll</tt>, <tt>retainAll</tt>, and <tt>clear</tt>
     * operations.  It does not support the <tt>add</tt> or <tt>addAll</tt>
     * operations.
     */
    @Override
	public Set<K> keySet() {
        Set<K> ks = keySet;
        return (ks != null ? ks : (keySet = new KeySet()));
    }

    private class KeySet extends AbstractSet<K> {
        public KeySet() {
		}

		@Override
		public Iterator<K> iterator() {
            return newKeyIterator();
        }
        
        @Override
		public int size() {
            return size;
        }
        
        @Override
		public boolean contains(Object o) {
            return containsKey(o);
        }
        
        @Override
        public boolean remove(Object o) {
            return HashMapWithSearch.this.removeEntryForKey(o) != null;
        }
        
        @Override
		public void clear() {
        	HashMapWithSearch.this.clear();
        }

		/* (non-Javadoc)
		 * @see java.util.AbstractCollection#add(java.lang.Object)
		 */
		@Override
		public boolean add(@SuppressWarnings("unused") K e) {
			throw new UnsupportedOperationException("modification of key set is not allowed for HashMapWithSearch");
		}

		/* (non-Javadoc)
		 * @see java.util.AbstractCollection#addAll(java.util.Collection)
		 */
		@Override
		public boolean addAll(@SuppressWarnings("unused") Collection<? extends K> c) {
			throw new UnsupportedOperationException("modification of key set is not allowed for HashMapWithSearch");
		}

   }

    @Override
	public Set<K> getPotentiallyOrderedKeySet(boolean ordered) {
		if (!ordered)
			return keySet();
		
        Set<K> ks = keySetOrdered;
        return (ks != null ? ks : (keySetOrdered = new KeySet()
        {
    		@Override
    		public Iterator<K> iterator() {
                return HashMapWithSearch.this.newKeyOrderedIterator();
            }
        }));
    }

    /**
     * Returns a {@link Collection} view of the values contained in this map.
     * The collection is backed by the map, so changes to the map are
     * reflected in the collection, and vice-versa.  If the map is
     * modified while an iteration over the collection is in progress
     * (except through the iterator's own <tt>remove</tt> operation),
     * the results of the iteration are undefined.  The collection
     * supports element removal, which removes the corresponding
     * mapping from the map, via the <tt>Iterator.remove</tt>,
     * <tt>Collection.remove</tt>, <tt>removeAll</tt>,
     * <tt>retainAll</tt> and <tt>clear</tt> operations.  It does not
     * support the <tt>add</tt> or <tt>addAll</tt> operations.
     */
    @Override
	public Collection<V> values() {
        Collection<V> vs = values;
        return (vs != null ? vs : (values = new Values()));
    }

    final class Values extends AbstractSet<V> {
        @Override
		public Iterator<V> iterator() {
            return new ValueIterator();
        }
        @Override
		public int size() {
            return size;
        }
        @Override
		public boolean contains(Object o) {
            return containsValue(o);
        }
        @Override
		public void clear() {
        	throw new UnsupportedOperationException("modification of value set is not allowed for HashMapWithSearch");
        }
        @Override
 		public boolean remove(@SuppressWarnings("unused") Object o) {
         	throw new UnsupportedOperationException("modification of value set is not allowed for HashMapWithSearch");
         }
        @Override
        public boolean removeAll(@SuppressWarnings("unused") Collection<?> c) {
         	throw new UnsupportedOperationException("modification of value set is not allowed for HashMapWithSearch");
        }
        @Override
        public boolean retainAll(@SuppressWarnings("unused") Collection<?> c) {
         	throw new UnsupportedOperationException("modification of value set is not allowed for HashMapWithSearch");
        }
		/* (non-Javadoc)
		 * @see java.util.AbstractCollection#add(java.lang.Object)
		 */
		@Override
		public boolean add(@SuppressWarnings("unused") V e) {
         	throw new UnsupportedOperationException("modification of value set is not allowed for HashMapWithSearch");
		}
		/* (non-Javadoc)
		 * @see java.util.AbstractCollection#addAll(java.util.Collection)
		 */
		@Override
		public boolean addAll(@SuppressWarnings("unused") Collection<? extends V> c) {
         	throw new UnsupportedOperationException("modification of value set is not allowed for HashMapWithSearch");
		}
    }
    
    /** A copy of the {@link AbstractMap#equals(Object)} method.
     * <p>
     * Compares the specified object with this map for equality.  Returns
     * <tt>true</tt> if the given object is also a map and the two maps
     * represent the same mappings.  More formally, two maps <tt>m1</tt> and
     * <tt>m2</tt> represent the same mappings if
     * <tt>m1.entrySet().equals(m2.entrySet())</tt>.  This ensures that the
     * <tt>equals</tt> method works properly across different implementations
     * of the <tt>Map</tt> interface.
     *
     * <p>This implementation first checks if the specified object is this map;
     * if so it returns <tt>true</tt>.  Then, it checks if the specified
     * object is a map whose size is identical to the size of this map; if
     * not, it returns <tt>false</tt>.  If so, it iterates over this map's
     * <tt>entrySet</tt> collection, and checks that the specified map
     * contains each mapping that this map contains.  If the specified map
     * fails to contain such a mapping, <tt>false</tt> is returned.  If the
     * iteration completes, <tt>true</tt> is returned.
     *
     * @param o object to be compared for equality with this map
     * @return <tt>true</tt> if the specified object is equal to this map
     */
    @Override
	public boolean equals(Object o) {
        if (o == this)
            return true;

        if (!(o instanceof Map))
            return false;
        @SuppressWarnings("unchecked")
		Map<K,V> m = (Map<K,V>) o;
        if (m.size() != size())
            return false;

        try {
            Iterator<Map.Entry<K,V>> i = entrySet().iterator();
            while (i.hasNext()) {
                Map.Entry<K,V> e = i.next();
                K key = e.getKey();
                V value = e.getValue();
                if (value == null) {
                    if (!(m.get(key)==null && m.containsKey(key)))
                        return false;
                } else {
                    if (!value.equals(m.get(key)))
                        return false;
                }
            }
        } catch (ClassCastException unused) {
            return false;
        } catch (NullPointerException unused) {
            return false;
        }

        return true;
    }

    /** A copy of the {@link AbstractMap#equals(Object)} method.
     * <p>
     * Returns the hash code value for this map.  The hash code of a map is
     * defined to be the sum of the hash codes of each entry in the map's
     * <tt>entrySet()</tt> view.  This ensures that <tt>m1.equals(m2)</tt>
     * implies that <tt>m1.hashCode()==m2.hashCode()</tt> for any two maps
     * <tt>m1</tt> and <tt>m2</tt>, as required by the general contract of
     * {@link Object#hashCode}.
     *
     * <p>This implementation iterates over <tt>entrySet()</tt>, calling
     * {@link Map.Entry#hashCode hashCode()} on each element (entry) in the
     * set, and adding up the results.
     *
     * @return the hash code value for this map
     * @see Map.Entry#hashCode()
     * @see Object#equals(Object)
     * @see Set#equals(Object)
     */
    @Override
	public int hashCode() {
        int h = 0;
        Iterator<Map.Entry<K,V>> i = entrySet().iterator();
        while (i.hasNext())
            h += i.next().hashCode();
        return h;
    }

    /** A copy of the {@link AbstractMap#equals(Object)} method.
     * <p>
     * Returns a string representation of this map.  The string representation
     * consists of a list of key-value mappings in the order returned by the
     * map's <tt>entrySet</tt> view's iterator, enclosed in braces
     * (<tt>"{}"</tt>).  Adjacent mappings are separated by the characters
     * <tt>", "</tt> (comma and space).  Each key-value mapping is rendered as
     * the key followed by an equals sign (<tt>"="</tt>) followed by the
     * associated value.  Keys and values are converted to strings as by
     * {@link String#valueOf(Object)}.
     *
     * @return a string representation of this map
     */
    @Override
	public String toString() {
        Iterator<Map.Entry<K,V>> i = entrySet().iterator();
        if (! i.hasNext())
            return "{}";

        StringBuilder sb = new StringBuilder();
        sb.append('{');
        for (;;) {
            Map.Entry<K,V> e = i.next();
            K key = e.getKey();
            V value = e.getValue();
            sb.append(key   == this ? "(this Map)" : key);
            sb.append('=');
            sb.append(value == this ? "(this Map)" : value);
            if (! i.hasNext())
                return sb.append('}').toString();
            sb.append(',').append(' ');
        }
    }

	@Override
	public Set<java.util.Map.Entry<K, V>> getPotentiallyOrderedEntrySet(boolean ordered) {
		if (ordered)
			return getTreeEntrySet();
		
		return entrySet();
	}

	@Override
	public boolean expectsConvertibleToInt() {
		return false;
	}
}

