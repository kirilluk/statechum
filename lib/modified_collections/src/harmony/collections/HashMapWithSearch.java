/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package harmony.collections;

import statechum.collections.MapWithSearch;

import java.util.*;

/**
 * HashMapWithSearch is a variant of HarmonyHashMap. Its entries are kept in a
 * doubly-linked list. The iteration order is, by default, the order in which
 * keys were inserted. Reinserting an already existing key doesn't change the
 * order. A key is existing if a call to {@code containsKey} would return true.
 * <p>
 * If the three argument constructor is used, and {@code order} is specified as
 * {@code true}, the iteration will be in the order that entries were accessed.
 * The access order gets affected by put(), get(), putAll() operations, but not
 * by operations on the collection views.
 * <p>
 * Null elements are allowed, and all the optional map operations are supported.
 * <p>
 * <b>Note:</b> The implementation of {@code HashMapWithSearch} is not synchronized.
 * If one thread of several threads accessing an instance modifies the map
 * structurally, access to the map needs to be synchronized. For
 * insertion-ordered instances a structural modification is an operation that
 * removes or adds an entry. Access-ordered instances also are structurally
 * modified by put(), get() and putAll() since these methods change the order of
 * the entries. Changes in the value of an entry are not structural changes.
 * <p>
 * The Iterator that can be created by calling the {@code iterator} method
 * throws a {@code ConcurrentModificationException} if the map is structurally
 * changed while an iterator is used to iterate over the elements. Only the
 * {@code remove} method that is provided by the iterator allows for removal of
 * elements during iteration. It is not possible to guarantee that this
 * mechanism works in all cases of unsynchronized concurrent modification. It
 * should only be used for debugging purposes.
 *
 * @since 1.4
 */
 
// The origin of this file is Apache Harmony SVN repository,
// location: classlib/modules/luni/src/main/java/java/util
// checked out Dec 3, 2022.

public class HashMapWithSearch<I, K extends  I, V> extends HarmonyHashMap<K, V> implements Map<K, V>, MapWithSearch<I, K, V> {

    public static final int DEFAULT_INITIAL_CAPACITY = 16;

    private static final long serialVersionUID = 3801124242820219131L;

    private final boolean accessOrder;

    transient private LinkedHashMapEntry<K, V> head, tail;

    /**
     * Constructs a new empty {@code HashMapWithSearch} instance.
     */
    public HashMapWithSearch() {
        super();
        accessOrder = false;
        head = null;
    }

    /**
     * Constructs a new {@code HashMapWithSearch} instance with the specified
     * capacity.
     * 
     * @param s
     *            the initial capacity of this map.
     * @throws IllegalArgumentException
     *                if the capacity is less than zero.
     */
    public HashMapWithSearch(int s) {
        super(s);
        accessOrder = false;
        head = null;
    }

    /**
     * Constructs a new {@code HashMapWithSearch} instance with the specified
     * capacity and load factor.
     * 
     * @param s
     *            the initial capacity of this map.
     * @param lf
     *            the initial load factor.
     * @throws IllegalArgumentException
     *             when the capacity is less than zero or the load factor is
     *             less or equal to zero.
     */
    public HashMapWithSearch(int s, float lf) {
        super(s, lf);
        accessOrder = false;
        head = null;
        tail = null;
    }

    /**
     * Constructs a new {@code HashMapWithSearch} instance with the specified
     * capacity, load factor and a flag specifying the ordering behavior.
     * 
     * @param s
     *            the initial capacity of this hash map.
     * @param lf
     *            the initial load factor.
     * @param order
     *            {@code true} if the ordering should be done based on the last
     *            access (from least-recently accessed to most-recently
     *            accessed), and {@code false} if the ordering should be the
     *            order in which the entries were inserted.
     * @throws IllegalArgumentException
     *             when the capacity is less than zero or the load factor is
     *             less or equal to zero.
     */
    public HashMapWithSearch(int s, float lf, boolean order) {
        super(s, lf);
        accessOrder = order;
        head = null;
        tail = null;
    }

    /**
     * Constructs a new {@code HashMapWithSearch} instance containing the mappings
     * from the specified map. The order of the elements is preserved.
     * 
     * @param m
     *            the mappings to add.
     */
    public HashMapWithSearch(Map<? extends K, ? extends V> m) {
        accessOrder = false;
        head = null;
        tail = null;
        putAll(m);
    }

    @Override
    public boolean expectsConvertibleToInt() {
        return false;
    }

    /** Returns a key object associated with the provided key. */
    @Override
    public K findKey(I id) {
        LinkedHashMapEntry<K, V> m;
        if (id == null) {
            m = (LinkedHashMapEntry<K, V>) findNullKeyEntry();
        } else {
            int hash = id.hashCode();
            int index = (hash & 0x7FFFFFFF) % elementData.length;
            m = (LinkedHashMapEntry<K, V>) findNonNullKeyEntry(id, index, hash);
        }
        if (m == null) {
            return null;
        }
        if (accessOrder && tail != m) {
            LinkedHashMapEntry<K, V> p = m.chainBackward;
            LinkedHashMapEntry<K, V> n = m.chainForward;
            n.chainBackward = p;
            if (p != null) {
                p.chainForward = n;
            } else {
                head = n;
            }
            m.chainForward = null;
            m.chainBackward = tail;
            tail.chainForward = m;
            tail = m;
        }
        return m.key;
    }

    @Override
    public Set<Map.Entry<K, V>> getTreeEntrySet() {
        return entrySetOrdered();
    }

    @Override
    public Set<Map.Entry<K, V>> getPotentiallyOrderedEntrySet(boolean ordered) {
        if (ordered)
            return getTreeEntrySet();

        return entrySet();
    }

    @Override
    public Set<K> getPotentiallyOrderedKeySet(boolean ordered) {
        if (ordered)
            return new TreeMapWithSearch<>(this).keySet();

        return keySet();
    }

    private static class AbstractMapIterator<I, K extends  I, V>  {
        int expectedModCount;
        LinkedHashMapEntry<K, V>  futureEntry;
        LinkedHashMapEntry<K, V>  currentEntry;
        final HashMapWithSearch<I, K, V> associatedMap;

        AbstractMapIterator(HashMapWithSearch<I, K, V> map) {
            expectedModCount = map.modCount;
            futureEntry = map.head;
            associatedMap = map;
        }

        public boolean hasNext() {
            return (futureEntry != null);
        }

        final void checkConcurrentMod() throws ConcurrentModificationException {
            if (expectedModCount != associatedMap.modCount) {
                throw new ConcurrentModificationException();
            }
        }

        final void makeNext() {
            checkConcurrentMod();
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            currentEntry = futureEntry;
            futureEntry = futureEntry.chainForward;
        }

        public void remove() {
            checkConcurrentMod();
            if (currentEntry==null) {
                throw new IllegalStateException();
            }
            associatedMap.removeEntry(currentEntry);
            LinkedHashMapEntry<K, V> lhme =  currentEntry;
            LinkedHashMapEntry<K, V> p = lhme.chainBackward;
            LinkedHashMapEntry<K, V> n = lhme.chainForward;
            HashMapWithSearch<I, K, V> lhm = associatedMap;
            if (p != null) {
                p.chainForward = n;
                if (n != null) {
                    n.chainBackward = p;
                } else {
                    lhm.tail = p;
                }
            } else {
                lhm.head = n;
                if (n != null) {
                    n.chainBackward = null;
                } else {
                    lhm.tail = null;
                }
            }
            currentEntry = null;
            expectedModCount++;
        }
    }

    private static abstract class IteratorNoModification<I, K extends I, V> extends AbstractMapIterator<I, K, V> {

        public IteratorNoModification(HashMapWithSearch<I, K, V> map) {
            super(map);
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException("modification of iterator is not allowed for HashMapWithSearch");
        }
    }

    private static class EntryIterator <I, K extends I, V> extends IteratorNoModification<I, K, V> implements Iterator<Map.Entry<K, V>> {

        EntryIterator (HashMapWithSearch<I, K, V> map) {
            super(map);
        }

        public Map.Entry<K, V> next() {
            makeNext();
            return currentEntry;
        }
    }

    private static class KeyIterator <I, K extends I, V> extends AbstractMapIterator<I, K, V> implements Iterator<K> {

        KeyIterator (HashMapWithSearch<I, K, V> map) {
            super(map);
        }

        public K next() {
            makeNext();
            return currentEntry.key;
        }

        @Override
        public void remove() {
            if (currentEntry == null)
                throw new IllegalStateException("next was not yet called");
            super.remove();
            //expectedModCount = modCount;// to permit more than a single remove to take place consecutively
        }

    }

    private static class ValueIterator <I, K extends I, V> extends IteratorNoModification<I, K, V> implements Iterator<V> {

        ValueIterator (HashMapWithSearch<I, K, V> map) {
            super(map);
        }

        public V next() {
            makeNext();
            return currentEntry.value;
        }
    }

    static final class LinkedHashMapEntrySet<IT, KT extends  IT, VT> extends HashMapEntrySet<KT, VT> {
        public LinkedHashMapEntrySet(HashMapWithSearch<IT, KT, VT> lhm) {
            super(lhm);
        }

        @Override
        public Iterator<Map.Entry<KT, VT>> iterator() {

            return new EntryIterator<>((HashMapWithSearch<IT, KT, VT>) HarmonyHashMap());
        }

        @Override
        public boolean remove(@SuppressWarnings("unused") Object o) {
            throw new UnsupportedOperationException("modification of entry set is not allowed for HashMapWithSearch");
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
        public boolean add(Map.Entry<KT, VT> ktvtEntry) {
            throw new UnsupportedOperationException("modification of entry set is not allowed for HashMapWithSearch");
        }

        /* (non-Javadoc)
         * @see java.util.AbstractCollection#addAll(java.util.Collection)
         */
        @Override
        public boolean addAll(Collection<? extends Map.Entry<KT, VT>> c) {
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

    static final class LinkedHashMapEntry<K, V> extends HarmonyHashMap.Entry<K, V> {
        LinkedHashMapEntry<K, V> chainForward, chainBackward;

        LinkedHashMapEntry(K theKey, V theValue) {
            super(theKey, theValue);
            chainForward = null;
            chainBackward = null;
        }

        LinkedHashMapEntry(K theKey, int hash) {
            super(theKey, hash);
            chainForward = null;
            chainBackward = null;
        }

        @Override
        @SuppressWarnings("unchecked")
        public Object clone() {
            LinkedHashMapEntry<K, V> entry = (LinkedHashMapEntry<K, V>) super
                    .clone();
            entry.chainBackward = chainBackward;
            entry.chainForward = chainForward;
            LinkedHashMapEntry<K, V> lnext = (LinkedHashMapEntry<K, V>) entry.next;
            if (lnext != null) {
                entry.next = (LinkedHashMapEntry<K, V>) lnext.clone();
            }
            return entry;
        }
    }

    @Override
    public boolean containsValue(Object value) {
        LinkedHashMapEntry<K, V> entry = head;
        if (null == value) {
            while (null != entry) {
                if (null == entry.value) {
                    return true;
                }
                entry = entry.chainForward;
            }
        } else {
            while (null != entry) {
                if (value.equals(entry.value)) {
                    return true;
                }
                entry = entry.chainForward;
            }
        }
        return false;
    }

    /**
     * Create a new element array
     * 
     * @param s size of the array to create
     * @return Reference to the element array
     */
    @Override
    @SuppressWarnings("unchecked")
    HarmonyHashMap.Entry<K, V>[] newElementArray(int s) {
        return new LinkedHashMapEntry[s];
    }

    /**
     * Returns the value of the mapping with the specified key.
     * 
     * @param key
     *            the key.
     * @return the value of the mapping with the specified key, or {@code null}
     *         if no mapping for the specified key is found.
     */
    @Override
    public V get(Object key) {
        LinkedHashMapEntry<K, V> m;
        if (key == null) {
            m = (LinkedHashMapEntry<K, V>) findNullKeyEntry();
        } else {
            int hash = key.hashCode();
            int index = (hash & 0x7FFFFFFF) % elementData.length;
            m = (LinkedHashMapEntry<K, V>) findNonNullKeyEntry(key, index, hash);
        }
        if (m == null) {
            return null;
        }
        if (accessOrder && tail != m) {
            LinkedHashMapEntry<K, V> p = m.chainBackward;
            LinkedHashMapEntry<K, V> n = m.chainForward;
            n.chainBackward = p;
            if (p != null) {
                p.chainForward = n;
            } else {
                head = n;
            }
            m.chainForward = null;
            m.chainBackward = tail;
            tail.chainForward = m;
            tail = m;
        }
        return m.value;
    }

    /*
     * @param key @param index @return Entry
     */
    @Override
    HarmonyHashMap.Entry<K, V> createEntry(K key, int index, V value) {
        LinkedHashMapEntry<K, V> m = new LinkedHashMapEntry<>(key, value);
        m.next = elementData[index];
        elementData[index] = m;
        linkEntry(m);
        return m;
    }

    HarmonyHashMap.Entry<K, V> createHashedEntry(K key, int index, int hash) {
        LinkedHashMapEntry<K, V> m = new LinkedHashMapEntry<>(key, hash);
        m.next = elementData[index];
        elementData[index] = m;
        linkEntry(m);
        return m;
    }

    /**
     * Maps the specified key to the specified value.
     * 
     * @param key
     *            the key.
     * @param value
     *            the value.
     * @return the value of any previous mapping with the specified key or
     *         {@code null} if there was no such mapping.
     */
    @Override
    public V put(K key, V value) {
        if (key == null)
            throw new IllegalArgumentException("key cannot be null for HashMapWithSearch");
        if (value == null)
            throw new IllegalArgumentException("value cannot be null for HashMapWithSearch");

        V result = putImpl(key, value);

        if (removeEldestEntry(head)) {
            remove(head.key);
        }

        return result;
    }

    V putImpl(K key, V value) {
        LinkedHashMapEntry<K, V> m;
        if (elementCount == 0) {
            head = tail = null;
        }
        if (key == null) {
            m = (LinkedHashMapEntry<K, V>) findNullKeyEntry();
            if (m == null) {
                modCount++;
                // Check if we need to remove the oldest entry. The check
                // includes accessOrder since an accessOrder HashMapWithSearch does
                // not record the oldest member in 'head'.
                if (++elementCount > threshold) {
                    rehash();
                }
                m = (LinkedHashMapEntry<K, V>) createHashedEntry(null, 0, 0);
            } else {
                linkEntry(m);
            }
        } else {
            int hash = key.hashCode();
            int index = (hash & 0x7FFFFFFF) % elementData.length;
            m = (LinkedHashMapEntry<K, V>) findNonNullKeyEntry(key, index, hash);
            if (m == null) {
                modCount++;
                if (++elementCount > threshold) {
                    rehash();
                    index = (hash & 0x7FFFFFFF) % elementData.length;
                }
                m = (LinkedHashMapEntry<K, V>) createHashedEntry(key, index,
                        hash);
            } else {
                linkEntry(m);
            }
        }

        V result = m.value;
        m.value = value;
        return result;
    }

    /*
     * @param m
     */
    void linkEntry(LinkedHashMapEntry<K, V> m) {
        if (tail == m) {
            return;
        }

        if (head == null) {
            // Check if the map is empty
            head = tail = m;
            return;
        }

        // we need to link the new entry into either the head or tail
        // of the chain depending on if the HashMapWithSearch is accessOrder or not
        LinkedHashMapEntry<K, V> p = m.chainBackward;
        LinkedHashMapEntry<K, V> n = m.chainForward;
        if (p == null) {
            if (n != null) {
                // The entry must be the head but not the tail
                if (accessOrder) {
                    head = n;
                    n.chainBackward = null;
                    m.chainBackward = tail;
                    m.chainForward = null;
                    tail.chainForward = m;
                    tail = m;
                }
            } else {
                // This is a new entry
                m.chainBackward = tail;
                m.chainForward = null;
                tail.chainForward = m;
                tail = m;
            }
            return;
        }

        if (n == null) {
            // The entry must be the tail so we can't get here
            return;
        }

        // The entry is neither the head nor tail
        if (accessOrder) {
            p.chainForward = n;
            n.chainBackward = p;
            m.chainForward = null;
            m.chainBackward = tail;
            tail.chainForward = m;
            tail = m;
        }
    }

    /**
     * Returns a set containing all of the mappings in this map. Each mapping is
     * an instance of {@link Map.Entry}. As the set is backed by this map,
     * changes in one will be reflected in the other.
     * 
     * @return a set of the mappings.
     */
    @Override
    public Set<Map.Entry<K, V>> entrySet() {
        return new LinkedHashMapEntrySet<>(this);
    }

    /**
     * Returns a set containing all of the mappings in this map, ordered as if they were in a TreeMap. Each mapping is
     * an instance of {@link Map.Entry}. The mappings are decoupled from the content of this hash in that changes to either of the
     * two will not affect another.
     *
     * @return a set of the mappings.
     */
    public Set<Map.Entry<K, V>> entrySetOrdered() {
        return new TreeMapWithSearch<>(this).entrySet();
    }

    /**
     * Returns a set of the keys contained in this map. The set is backed by
     * this map so changes to one are reflected by the other. The set does not
     * support adding.
     * 
     * @return a set of the keys.
     */
    @Override
    public Set<K> keySet() {
        if (keySet == null) {
            keySet = new AbstractSet<>() {
                @Override
                public boolean contains(Object object) {
                    return containsKey(object);
                }

                @Override
                public int size() {
                    return HashMapWithSearch.this.size();
                }

                @Override
                public void clear() {
                    HashMapWithSearch.this.clear();
                }

                @Override
                public boolean remove(Object key) {
                    if (containsKey(key)) {
                        HashMapWithSearch.this.remove(key);
                        return true;
                    }
                    return false;
                }

                @Override
                public Iterator<K> iterator() {
                    return new KeyIterator<>(HashMapWithSearch.this);
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
            };
        }
        return keySet;
    }

    /**
     * Returns a collection of the values contained in this map. The collection
     * is backed by this map so changes to one are reflected by the other. The
     * collection supports remove, removeAll, retainAll and clear operations,
     * and it does not support add or addAll operations.
     * <p>
     * This method returns a collection which is the subclass of
     * AbstractCollection. The iterator method of this subclass returns a
     * "wrapper object" over the iterator of map's entrySet(). The size method
     * wraps the map's size method and the contains method wraps the map's
     * containsValue method.
     * <p>
     * The collection is created when this method is called for the first time
     * and returned in response to all subsequent calls. This method may return
     * different collections when multiple concurrent calls occur, since no
     * synchronization is performed.
     *
     * @return a collection of the values contained in this map.
     */
    @Override
    public Collection<V> values() {
        if (valuesCollection == null) {
            valuesCollection = new AbstractSet<>() {
                @Override
                public boolean contains(Object object) {
                    return containsValue(object);
                }

                @Override
                public int size() {
                    return HashMapWithSearch.this.size();
                }

//                @Override
//                public void clear() {
//                    HashMapWithSearch.this.clear();
//                }

                @Override
                public Iterator<V> iterator() {
                    return new ValueIterator<>(HashMapWithSearch.this);
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
            };
        }
        return valuesCollection;
    }

    /**
     * Removes the mapping with the specified key from this map.
     * 
     * @param key
     *            the key of the mapping to remove.
     * @return the value of the removed mapping or {@code null} if no mapping
     *         for the specified key was found.
     */
    @Override
    public V remove(Object key) {
        LinkedHashMapEntry<K, V> m = (LinkedHashMapEntry<K, V>) removeEntry(key);
        if (m == null) {
            return null;
        }
        LinkedHashMapEntry<K, V> p = m.chainBackward;
        LinkedHashMapEntry<K, V> n = m.chainForward;
        if (p != null) {
            p.chainForward = n;
        } else {
            head = n;
        }
        if (n != null) {
            n.chainBackward = p;
        } else {
            tail = p;
        }
        return m.value;
    }

    /**
     * This method is queried from the put and putAll methods to check if the
     * eldest member of the map should be deleted before adding the new member.
     * If this map was created with accessOrder = true, then the result of
     * removeEldestEntry is assumed to be false.
     * 
     * @param eldest
     *            the entry to check if it should be removed.
     * @return {@code true} if the eldest member should be removed.
     */
    protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
        return false;
    }

    /**
     * Removes all elements from this map, leaving it empty.
     * 
     * @see #isEmpty()
     * @see #size()
     */
    @Override
    public void clear() {
        super.clear();
        head = tail = null;
    }
}
