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
 
// The origin of this file is Apache Harmony SVN repository,
// location: classlib/modules/luni/src/main/java/java/util
// checked out Dec 3, 2022.
package harmony.collections;

import statechum.collections.MapWithSearch;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.*;

/**
 * TreeMapWithSearch is an implementation of SortedMap. All optional operations (adding
 * and removing) are supported. The values can be any objects. The keys can be
 * any objects which are comparable to each other either using their natural
 * order or a specified Comparator.
 *
 * @since 1.2
 */
public class TreeMapWithSearch<I, K extends I, V> extends HarmonyAbstractMap<K, V> implements SortedMap<K, V>, MapWithSearch<I,K,V>,
                                                        Cloneable, Serializable {
    private static final long serialVersionUID = 919286545866124006L;

    transient int size;

    private Comparator<? super I> comparator;

    transient int modCount;

    transient Set<Entry<K, V>> entrySet;

    transient Node<I, K, V> root;

    @Override
    public boolean expectsConvertibleToInt() {
        return false;
    }

    /** The implementation is a clone of the get() method but modified to return a key rather than the value. */
    @Override
    public K findKey(I key) {
        if (key == null)
            return null;// we never store null keys.

        Comparable<I> object = comparator == null ? toComparable(key) : null;
        Node<I, K, V> node = root;
        while (node != null) {
            K[] keys = node.keys;
            int left_idx = node.left_idx;
            int result = cmp(object, key, keys[left_idx]);
            if (result < 0) {
                node = node.left;
            } else if (result == 0) {
                return node.keys[left_idx];
            } else {
                int right_idx = node.right_idx;
                if (left_idx != right_idx) {
                    result = cmp(object, key, keys[right_idx]);
                }
                if (result > 0) {
                    node = node.right;
                } else if (result == 0) {
                    return node.keys[right_idx];
                } else { /*search in node*/
                    int low = left_idx + 1, mid = 0, high = right_idx - 1;
                    while (low <= high) {
                        mid = (low + high) >>> 1;
                        result = cmp(object, key, keys[mid]);
                        if (result > 0) {
                            low = mid + 1;
                        } else if (result == 0) {
                            return node.keys[mid];
                        } else {
                            high = mid - 1;
                        }
                    }
                    return null;
                }
            }
        }
        return null;
    }

    @Override
    public Set<Entry<K, V>> getTreeEntrySet() {
        return entrySet();
    }

    @Override
    public Set<Entry<K, V>> getPotentiallyOrderedEntrySet(boolean ordered) {
        return entrySet();
    }

    @Override
    public Set<K> getPotentiallyOrderedKeySet(boolean ordered) {
        return keySet();
    }

    class MapEntry implements Map.Entry<K, V>, Cloneable {
		
		final int offset;
		final Node<I, K, V> node;
		final K key;
		
	    MapEntry(Node<I, K, V> node, int offset) {
	    	this.node = node;
	    	this.offset = offset;
	    	key = node.keys[offset];
	    }

	    @Override
	    public Object clone() {
	        try {
	            return super.clone();
	        } catch (CloneNotSupportedException e) {
	            return null;
	        }
	    }

	    @Override
	    public boolean equals(Object object) {
	        if (this == object) {
	            return true;
	        }
	        if (object instanceof Map.Entry) {
	            Map.Entry<?, ?> entry = (Map.Entry<?, ?>) object;	            
	            V value = getValue();
	            return (key == null ? entry.getKey() == null : key.equals(entry
	                    .getKey()))
	                    && (value == null ? entry.getValue() == null : value
	                            .equals(entry.getValue()));
	        }
	        return false;
	    }

	    public K getKey() {
	        return key;
	    }

	    public V getValue() {
	    	if (node.keys[offset] == key) {
	    		return node.values[offset];
	    	}
	    	if (containsKey(key)) {
	    		return get(key);
	    	}
	    	throw new IllegalStateException();
	    }

	    @Override
	    public int hashCode() {
	    	V value = getValue();
	        return (key == null ? 0 : key.hashCode())
	                ^ (value == null ? 0 : value.hashCode());
	    }

	    public V setValue(V object) {
	    	if (node.keys[offset] == key) {
	    		V res = node.values[offset];
	    		node.values[offset] = object;
	    		return res;
	    	}
	    	if (containsKey(key)) {
	    		return put(key, object);
	    	}
	    	throw new IllegalStateException();
	    }

	    @Override
	    public String toString() {
	        return key + "=" + getValue();
	    }
	}

    static class Node <I, K extends I,V> implements Cloneable {
        static final int NODE_SIZE = 64;
        Node<I, K, V> prev, next;
        Node<I, K, V> parent, left, right;
        V[] values;
        K[] keys;
        int left_idx = 0;
        int right_idx = -1;
        int size = 0;
        boolean color;

        @SuppressWarnings("unchecked")
        public Node() {
            keys = (K[]) new Object[NODE_SIZE];
            values = (V[]) new Object[NODE_SIZE];
        }

        @SuppressWarnings("unchecked")
        Node<I, K, V> clone(Node<I, K, V> parent) throws CloneNotSupportedException {
            Node<I, K, V> clone = (Node<I, K, V>) super.clone();
            clone.keys   = (K[]) new Object[NODE_SIZE];
            clone.values = (V[]) new Object[NODE_SIZE];
            System.arraycopy(keys,   0, clone.keys,   0, keys.length);
            System.arraycopy(values, 0, clone.values, 0, values.length);
            clone.left_idx  = left_idx;
            clone.right_idx = right_idx;
            clone.parent = parent;
            if (left != null) {
                clone.left = left.clone(clone);
            }
            if (right != null) {
                clone.right = right.clone(clone);
            }
            clone.prev = null;
            clone.next = null;
            return clone;
        }
    }

    @SuppressWarnings("unchecked")
     private static <I> Comparable<I> toComparable(I obj) {
        if (obj == null) {
            throw new NullPointerException();
        }
        return (Comparable<I>) obj;
    }

    static class AbstractMapIterator <I,K extends I,V> {
        TreeMapWithSearch<I, K, V> backingMap;
        int expectedModCount;
        Node<I, K, V> node;
        Node<I, K, V> lastNode;
        int offset;
        int lastOffset;

        AbstractMapIterator(TreeMapWithSearch<I, K, V> map, Node<I, K, V> startNode, int startOffset) {
            backingMap = map;
            expectedModCount = map.modCount;
            node = startNode;
            offset = startOffset;
        }

        AbstractMapIterator(TreeMapWithSearch<I, K, V> map, Node<I, K, V> startNode) {
            this(map, startNode, startNode != null ?
                                 startNode.right_idx - startNode.left_idx : 0);
        }

        AbstractMapIterator(TreeMapWithSearch<I, K, V> map) {
            this(map, minimum(map.root));
        }

        public boolean hasNext() {
            return node != null;
        }

        final void makeNext() {
            if (expectedModCount != backingMap.modCount) {
                throw new ConcurrentModificationException();
            } else if (node == null) {
                throw new NoSuchElementException();
            }
            lastNode = node;
            lastOffset = offset;
            if (offset != 0) {
                offset--;
            } else {
                node = node.next;
                if (node != null) {
                    offset = node.right_idx - node.left_idx;
                }
            }
        }

        public void remove() {
            if (expectedModCount == backingMap.modCount) {
                if (lastNode != null) {
                    int idx = lastNode.right_idx - lastOffset;
                    backingMap.removeFromIterator(lastNode, idx);
                    lastNode = null;
                    expectedModCount++;
                } else {
                    throw new IllegalStateException();
                }
            } else {
                throw new ConcurrentModificationException();
            }
        }
    }
    private static abstract class IteratorNoModification<I, K extends I, V> extends AbstractMapIterator<I, K, V> {

        IteratorNoModification(TreeMapWithSearch<I, K, V> map, Node<I, K, V> startNode, int startOffset) {
            super(map, startNode, startOffset);
        }

        IteratorNoModification(TreeMapWithSearch<I, K, V> map, Node<I, K, V> startNode) {
            super(map, startNode);
        }

        IteratorNoModification(TreeMapWithSearch<I, K, V> map) {
            super(map);
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException("modification of iterator is not allowed for HashMapWithSearch");
        }
    }
    static class UnboundedEntryIterator <I, K extends I, V> extends IteratorNoModification<I, K, V>
                                            implements Iterator<Map.Entry<K, V>> {

        UnboundedEntryIterator(TreeMapWithSearch<I, K, V> map, Node<I, K, V> startNode, int startOffset) {
            super(map, startNode, startOffset);
        }

        UnboundedEntryIterator(TreeMapWithSearch<I, K, V> map) {
            super(map);
        }

        public Map.Entry<K, V> next() {
            makeNext();
            int idx = lastNode.right_idx - lastOffset;
            return backingMap.new MapEntry(lastNode, idx);
        }
    }

    static class UnboundedKeyIterator <I, K extends I, V> extends AbstractMapIterator<I, K, V>
                                                          implements Iterator<K> {

        UnboundedKeyIterator(TreeMapWithSearch<I, K, V> map, Node<I, K, V> startNode, int startOffset) {
            super(map, startNode, startOffset);
        }

        UnboundedKeyIterator(TreeMapWithSearch<I, K, V> map) {
            super(map);
        }

        public K next() {
            makeNext();
            return lastNode.keys[lastNode.right_idx - lastOffset];
        }

        @Override
        public void remove() {
            if (lastNode == null)
                throw new IllegalStateException("next was not yet called");
            super.remove();
            //expectedModCount = modCount;// to permit more than a single remove to take place consecutively
        }
    }

    static class UnboundedValueIterator <I, K extends I, V> extends IteratorNoModification<I, K, V>
                                                          implements Iterator<V> {

        UnboundedValueIterator(TreeMapWithSearch<I, K, V> map, Node<I, K, V> startNode, int startOffset) {
            super(map, startNode, startOffset);
        }

        UnboundedValueIterator(TreeMapWithSearch<I, K, V> map) {
            super(map);
        }

        public V next() {
            makeNext();
            return lastNode.values[lastNode.right_idx - lastOffset];
        }
    }

    static class BoundedMapIterator <I, K extends I, V> extends AbstractMapIterator<I, K, V> {

        Node<I, K, V> finalNode;
        int finalOffset;

        BoundedMapIterator(Node<I, K, V> startNode, int startOffset, TreeMapWithSearch<I, K, V> map,
                           Node<I, K, V> finalNode, int finalOffset) {
            super(map, finalNode==null? null : startNode, startOffset);
            this.finalNode = finalNode;
            this.finalOffset = finalOffset;
        }

        BoundedMapIterator(Node<I, K, V> startNode, TreeMapWithSearch<I, K, V> map,
                           Node<I, K, V> finalNode, int finalOffset) {
            this(startNode, startNode != null ?
                            startNode.right_idx - startNode.left_idx : 0,
                            map, finalNode, finalOffset);
        }

        BoundedMapIterator(Node<I, K, V> startNode, int startOffset,
                           TreeMapWithSearch<I, K, V> map, Node<I, K, V> finalNode) {
            this(startNode, startOffset, map, finalNode,
                         finalNode.right_idx - finalNode.left_idx);
        }

        void makeBoundedNext() {
            makeNext();
            if (lastNode == finalNode && lastOffset == finalOffset) {
                node = null;
            }
        }
    }

    static class BoundedEntryIterator <I, K extends I, V> extends BoundedMapIterator<I, K, V>
                                          implements Iterator<Map.Entry<K, V>> {

        public BoundedEntryIterator(Node<I, K, V> startNode, int startOffset, TreeMapWithSearch<I, K, V> map,
                                    Node<I, K, V> finalNode, int finalOffset) {
            super(startNode, startOffset, map, finalNode, finalOffset);
        }

        public Map.Entry<K, V> next() {
            makeBoundedNext();
            int idx = lastNode.right_idx - lastOffset;
            return backingMap.new MapEntry(lastNode, idx);
        }
    }

    static class BoundedKeyIterator <I, K extends I, V> extends BoundedMapIterator<I, K, V>
                                                     implements Iterator<K> {

        public BoundedKeyIterator(Node<I, K, V> startNode, int startOffset, TreeMapWithSearch<I, K, V> map,
                                  Node<I, K, V> finalNode, int finalOffset) {
            super(startNode, startOffset, map, finalNode, finalOffset);
        }

        public K next() {
            makeBoundedNext();
            return lastNode.keys[lastNode.right_idx - lastOffset];
        }
    }

    static class BoundedValueIterator <I, K extends I, V> extends BoundedMapIterator<I, K, V>
                                                       implements Iterator<V> {

        public BoundedValueIterator(Node<I, K, V> startNode, int startOffset, TreeMapWithSearch<I, K, V> map,
                                    Node<I, K, V> finalNode, int finalOffset) {
            super(startNode, startOffset, map, finalNode, finalOffset);
        }

        public V next() {
            makeBoundedNext();
            return lastNode.values[lastNode.right_idx - lastOffset];
        }
    }

    static final class SubMap <I, K extends I,V> extends HarmonyAbstractMap<K, V>
                                 implements SortedMap<K, V>, Serializable {
        private static final long serialVersionUID = -6520786458950516097L;

        private final TreeMapWithSearch<I, K, V> backingMap;

        boolean hasStart, hasEnd;
        K startKey, endKey;
        transient Set<Map.Entry<K, V>> entrySet = null;
        transient int firstKeyModCount = -1;
        transient int lastKeyModCount = -1;
        transient Node<I, K, V> firstKeyNode;
        transient int firstKeyIndex;
        transient Node<I, K, V> lastKeyNode;
        transient int lastKeyIndex;

        SubMap(K start, TreeMapWithSearch<I, K, V> map) {
            backingMap = map;
            hasStart = true;
            startKey = start;
        }

        SubMap(K start, TreeMapWithSearch<I, K, V> map, K end) {
            backingMap = map;
            hasStart = hasEnd = true;
            startKey = start;
            endKey = end;
        }

        SubMap(TreeMapWithSearch<I, K, V> map, K end) {
            backingMap = map;
            hasEnd = true;
            endKey = end;
        }

        private void checkRange(K key) {
            Comparator<? super K> cmp = backingMap.comparator;
            if (cmp == null) {
                Comparable<K> object = toComparable(key);
                if (hasStart && object.compareTo(startKey) < 0) {
                    throw new IllegalArgumentException();
                }
                if (hasEnd && object.compareTo(endKey) > 0) {
                    throw new IllegalArgumentException();
                }
            } else {
                if (hasStart
                    && backingMap.comparator().compare(key, startKey) < 0) {
                    throw new IllegalArgumentException();
                }
                if (hasEnd && backingMap.comparator().compare(key, endKey) > 0) {
                    throw new IllegalArgumentException();
                }
            }
        }

        private boolean isInRange(K key) {
            Comparator<? super K> cmp = backingMap.comparator;
            if (cmp == null) {
                Comparable<K> object = toComparable(key);
                if (hasStart && object.compareTo(startKey) < 0) {
                    return false;
                }
                if (hasEnd && object.compareTo(endKey) >= 0) {
                    return false;
                }
            } else {
                if (hasStart && cmp.compare(key, startKey) < 0) {
                    return false;
                }
                if (hasEnd && cmp.compare(key, endKey) >= 0) {
                    return false;
                }
            }
            return true;
        }

        private boolean checkUpperBound(K key) {
            if (hasEnd) {
                Comparator<? super K> cmp = backingMap.comparator;
                if (cmp == null) {
                    return (toComparable(key).compareTo(endKey) < 0);
                }
                return (cmp.compare(key, endKey) < 0);
            }
            return true;
        }

        private boolean checkLowerBound(K key) {
            if (hasStart) {
                Comparator<? super K> cmp = backingMap.comparator;
                if (cmp == null) {
                    return (toComparable(key).compareTo(startKey) >= 0);
                }
                return (cmp.compare(key, startKey) >= 0);
            }
            return true;
        }

        public Comparator<? super K> comparator() {
            return backingMap.comparator();
        }

        @SuppressWarnings("unchecked")
        @Override
        public boolean containsKey(Object key) {
            if (isInRange((K) key)) {
                return backingMap.containsKey(key);
            }
            return false;
        }

        @Override
         public void clear() {
            keySet().clear();
        }

        @Override
         public boolean containsValue(Object value) {
            Iterator<V> it = values().iterator();
            if (value != null) {
                while (it.hasNext()) {
                    if (value.equals(it.next())) {
                        return true;
                    }
                }
            } else {
                while (it.hasNext()) {
                    if (it.next() == null) {
                        return true;
                    }
                }
            }
            return false;
        }

        @Override
        public Set<Map.Entry<K, V>> entrySet() {
            if (entrySet == null) {
                entrySet = new SubMapEntrySet<>(this);
            }
            return entrySet;
        }

        private void setFirstKey() {
            if (firstKeyModCount == backingMap.modCount) {
                return;
            }
            Comparable<I> object = backingMap.comparator == null ?
                                   toComparable(startKey) : null;
            K key = startKey;
            Node<I, K, V> node = backingMap.root;
            Node<I, K, V> foundNode = null;
            int foundIndex = -1;
            TOP_LOOP:
            while (node != null) {
                K[] keys = node.keys;
                int left_idx = node.left_idx;
                int result = backingMap.cmp(object, key, keys[left_idx]);
                if (result < 0) {
                    foundNode = node;
                    foundIndex = left_idx;
                    node = node.left;
                } else if (result == 0) {
                    foundNode = node;
                    foundIndex = left_idx;
                    break;
                } else {
                    int right_idx = node.right_idx;
                    if (left_idx != right_idx) {
                        result = backingMap.cmp(object, key, keys[right_idx]);
                    }
                    if (result > 0) {
                        node = node.right;
                    } else if (result == 0) {
                        foundNode = node;
                        foundIndex = right_idx;
                        break;
                    } else { /*search in node*/
                        foundNode = node;
                        foundIndex = right_idx;
                        int low = left_idx + 1, mid = 0, high = right_idx - 1;
                        while (low <= high) {
                            mid = (low + high) >>> 1;
                            result = backingMap.cmp(object, key, keys[mid]);
                            if (result > 0) {
                                low = mid + 1;
                            } else if (result == 0) {
                                foundNode = node;
                                foundIndex = mid;
                                break TOP_LOOP;
                            } else {
                                foundNode = node;
                                foundIndex = mid;
                                high = mid - 1;
                            }
                        }
                        break TOP_LOOP;
                    }
                }
            }
            if (foundNode != null && !checkUpperBound(foundNode.keys[foundIndex])) {
                foundNode = null;
            }
            firstKeyNode = foundNode;
            firstKeyIndex = foundIndex;
            firstKeyModCount = backingMap.modCount;
        }

        public K firstKey() {
            if (backingMap.size > 0) {
                if (!hasStart) {
                    Node<I, K, V> node = minimum(backingMap.root);
                    if (node != null && checkUpperBound(node.keys[node.left_idx])) {
                        return node.keys[node.left_idx];
                    }
                } else {
                    setFirstKey();
                    if (firstKeyNode != null) {
                        return firstKeyNode.keys[firstKeyIndex];
                    }
                }
            }
            throw new NoSuchElementException();
        }


        @SuppressWarnings("unchecked")
        @Override
        public V get(Object key) {
            if (isInRange((K) key)) {
                return backingMap.get(key);
            }
            return null;
        }

        public SortedMap<K, V> headMap(K endKey) {
            checkRange(endKey);
            if (hasStart) {
                return new SubMap<>(startKey, backingMap, endKey);
            }
            return new SubMap<>(backingMap, endKey);
        }

        @Override
        public boolean isEmpty() {
            if (hasStart) {
                setFirstKey();
                return firstKeyNode == null;
            } else {
                setLastKey();
                return lastKeyNode == null;
            }
        }

        @Override
        public Set<K> keySet() {
            if (keySet == null) {
                keySet = new SubMapKeySet<>(this);
            }
            return keySet;
        }

        private void setLastKey() {
            if (lastKeyModCount == backingMap.modCount) {
                return;
            }
            Comparable<I> object = backingMap.comparator == null ?
                                   toComparable(endKey) : null;
            K key = endKey;
            Node<I, K, V> node = backingMap.root;
            Node<I, K, V> foundNode = null;
            int foundIndex = -1;
            TOP_LOOP:
            while (node != null) {
                K[] keys = node.keys;
                int left_idx = node.left_idx;
                int result = backingMap.cmp(object, key, keys[left_idx]);
                if (result <= 0) {
                    node = node.left;
                } else {
                    int right_idx = node.right_idx;
                    if (left_idx != right_idx) {
                        result = backingMap.cmp(object, key, keys[right_idx]);
                    }
                    if (result > 0) {
                        foundNode = node;
                        foundIndex = right_idx;
                        node = node.right;
                    } else if (result == 0) {
                        if (node.left_idx == node.right_idx) {
                            foundNode = node.prev;
                            if (foundNode != null) {
                                foundIndex = foundNode.right_idx - 1;
                            }
                        } else {
                            foundNode = node;
                            foundIndex = right_idx - 1;
                        }
                        break;
                    } else { /*search in node*/
                        foundNode = node;
                        foundIndex = left_idx;
                        int low = left_idx + 1, mid = 0, high = right_idx - 1;
                        while (low <= high) {
                            mid = (low + high) >>> 1;
                            result = backingMap.cmp(object, key, keys[mid]);
                            if (result > 0) {
                                foundNode = node;
                                foundIndex = mid;
                                low = mid + 1;
                            } else if (result == 0) {
                                foundNode = node;
                                foundIndex = mid - 1;
                                break TOP_LOOP;
                            } else {
                                high = mid - 1;
                            }
                        }
                        break TOP_LOOP;
                    }
                }
            }
            if (foundNode != null && !checkLowerBound(foundNode.keys[foundIndex])) {
                foundNode = null;
            }
            lastKeyNode = foundNode;
            lastKeyIndex = foundIndex;
            lastKeyModCount = backingMap.modCount;
        }

        public K lastKey() {
            if (backingMap.size > 0) {
                if (!hasEnd) {
                    Node<I, K, V> node = maximum(backingMap.root);
                    if (node != null && checkLowerBound(node.keys[node.right_idx])) {
                        return node.keys[node.right_idx];
                    }
                } else {
                    setLastKey();
                    if (lastKeyNode != null) {
                        return lastKeyNode.keys[lastKeyIndex];
                    }
                }
            }
            throw new NoSuchElementException();
        }


        @Override
        public V put(K key, V value) {
            if (isInRange(key)) {
                return backingMap.put(key, value);
            }
            throw new IllegalArgumentException();
        }

        @SuppressWarnings("unchecked")
        @Override
        public V remove(Object key) {
            if (isInRange((K) key)) {
                return backingMap.remove(key);
            }
            return null;
        }

        public SortedMap<K, V> subMap(K startKey, K endKey) {
            checkRange(startKey);
            checkRange(endKey);
            Comparator<? super K> c = backingMap.comparator();
            if (c == null) {
                if (toComparable(startKey).compareTo(endKey) <= 0) {
                    return new SubMap<>(startKey, backingMap, endKey);
                }
            } else {
                if (c.compare(startKey, endKey) <= 0) {
                    return new SubMap<>(startKey, backingMap, endKey);
                }
            }
            throw new IllegalArgumentException();
        }

        public SortedMap<K, V> tailMap(K startKey) {
            checkRange(startKey);
            if (hasEnd) {
                return new SubMap<>(startKey, backingMap, endKey);
            }
            return new SubMap<>(startKey, backingMap);
        }

        @Override
        public Collection<V> values() {
            if (valuesCollection == null) {
                valuesCollection = new SubMapValuesCollection<>(this);
            }
            return valuesCollection;
        }

        public int size() {
            Node<I, K, V> from, to;
            int fromIndex, toIndex;
            if (hasStart) {
                setFirstKey();
                from = firstKeyNode;
                fromIndex = firstKeyIndex;
            } else {
                from = minimum(backingMap.root);
                fromIndex = from == null ? 0 : from.left_idx;
            }
            if (from == null) {
                return 0;
            }
            if (hasEnd) {
                setLastKey();
                to = lastKeyNode;
                toIndex = lastKeyIndex;
            } else {
                to = maximum(backingMap.root);
                toIndex = to == null ? 0 : to.right_idx;
            }
            if (to == null) {
                return 0;
            }
            if (from == to) {
                return toIndex - fromIndex + 1;
            }
            int sum = 0;
            while (from != to) {
                sum += (from.right_idx - fromIndex + 1);
                from = from.next;
                fromIndex = from.left_idx;
            }
            return sum + toIndex - fromIndex + 1;
        }

        private void readObject(ObjectInputStream stream) throws IOException,
                                                                 ClassNotFoundException {
            stream.defaultReadObject();
            firstKeyModCount = -1;
            lastKeyModCount = -1;
        }
    }

    static class SubMapEntrySet <I,K extends I,V> extends AbstractSet<Map.Entry<K, V>>
                                                implements Set<Map.Entry<K, V>> {
        SubMap<I, K, V> subMap;

        SubMapEntrySet(SubMap<I, K, V> map) {
            subMap = map;
        }

        @Override
        public boolean isEmpty() {
            return subMap.isEmpty();
        }

        public Iterator<Map.Entry<K, V>> iterator() {
            Node<I, K, V> from;
            int fromIndex;
            if (subMap.hasStart) {
                subMap.setFirstKey();
                from = subMap.firstKeyNode;
                fromIndex = subMap.firstKeyIndex;
            } else {
                from = minimum(subMap.backingMap.root);
                fromIndex = from != null ? from.left_idx : 0;
            }
            if (!subMap.hasEnd) {
                return new UnboundedEntryIterator<>(subMap.backingMap, from, from == null ? 0 : from.right_idx - fromIndex);
            }
            subMap.setLastKey();
            Node<I, K, V> to = subMap.lastKeyNode;
            int toIndex = subMap.lastKeyIndex;
            return new BoundedEntryIterator<>(from, from == null ? 0 : from.right_idx - fromIndex, subMap.backingMap, to, to == null ? 0 : to.right_idx - toIndex);
        }

        @Override
        public int size() {
            return subMap.size();
        }

        @SuppressWarnings("unchecked")
        @Override
        public boolean contains(Object object) {
            if (object instanceof Map.Entry) {
                Map.Entry<K, V> entry = (Map.Entry<K, V>) object;
                K key = entry.getKey();
                if (subMap.isInRange(key)) {
                    V v1 = subMap.get(key), v2 = entry.getValue();
                    return v1 == null ? ( v2 == null && subMap.containsKey(key) ) : v1.equals(v2);
                }
            }
            return false;
        }

        @Override
        public boolean remove(Object object) {
            if (contains(object)) {
                @SuppressWarnings("unchecked") Map.Entry<K, V> entry = (Map.Entry<K, V>) object;
                K key = entry.getKey();
                subMap.remove(key);
                return true;
            }
            return false;
        }
    }

    static class SubMapKeySet <I,K extends I,V> extends AbstractSet<K> implements Set<K> {
        SubMap<I, K, V> subMap;

        SubMapKeySet(SubMap<I, K, V> map) {
            subMap = map;
        }

        @Override
        public boolean contains(Object object) {
            return subMap.containsKey(object);
        }

        @Override
        public boolean isEmpty() {
            return subMap.isEmpty();
        }

        @Override
        public int size() {
            return subMap.size();
        }

        @Override
        public boolean remove(Object object) {
            if (subMap.containsKey(object)) {
                subMap.remove(object);
                return true;
            }
            return false;
        }

        public Iterator<K> iterator() {
            Node<I, K, V> from;
            int fromIndex;
            if (subMap.hasStart) {
                subMap.setFirstKey();
                from = subMap.firstKeyNode;
                fromIndex = subMap.firstKeyIndex;
            } else {
                from = minimum(subMap.backingMap.root);
                fromIndex = from != null ? from.left_idx : 0;
            }
            if (!subMap.hasEnd) {
                return new UnboundedKeyIterator<>(subMap.backingMap, from,
                        from == null ? 0 : from.right_idx - fromIndex);
            }
            subMap.setLastKey();
            Node<I, K, V> to = subMap.lastKeyNode;
            int toIndex = subMap.lastKeyIndex;
            return new BoundedKeyIterator<>(from,
                    from == null ? 0 : from.right_idx - fromIndex, subMap.backingMap, to,
                    to == null ? 0 : to.right_idx - toIndex);
        }
    }

    static class SubMapValuesCollection <I,K extends I,V> extends AbstractCollection<V> {
        SubMap<I, K, V> subMap;

        public SubMapValuesCollection(SubMap<I, K, V> subMap) {
            this.subMap = subMap;
        }

        @Override
        public boolean isEmpty() {
            return subMap.isEmpty();
        }

        @Override
        public Iterator<V> iterator() {
            Node<I, K, V> from;
            int fromIndex;
            if (subMap.hasStart) {
                subMap.setFirstKey();
                from = subMap.firstKeyNode;
                fromIndex = subMap.firstKeyIndex;
            } else {
                from = minimum(subMap.backingMap.root);
                fromIndex = from != null ? from.left_idx : 0;
            }
            if (!subMap.hasEnd) {
                return new UnboundedValueIterator<>(subMap.backingMap, from,
                        from == null ? 0 : from.right_idx - fromIndex);
            }
            subMap.setLastKey();
            Node<I, K, V> to = subMap.lastKeyNode;
            int toIndex = subMap.lastKeyIndex;
            return new BoundedValueIterator<>(from,
                    from == null ? 0 : from.right_idx - fromIndex, subMap.backingMap, to,
                    to == null ? 0 : to.right_idx - toIndex);
        }

        @Override
        public int size() {
            return subMap.size();
        }
    }

    /**
     * Constructs a new empty {@code TreeMapWithSearch} instance.
     */
    public TreeMapWithSearch() {
    }

    /**
     * Constructs an empty {@code TreeMapWithSearch} instance.
     * @param size ignored
     */
    public TreeMapWithSearch(int size) {
    }

    /**
     * Constructs a new empty {@code TreeMapWithSearch} instance with the specified
     * comparator.
     *
     * @param comparator
     *            the comparator to compare keys with.
     */
    public TreeMapWithSearch(Comparator<? super I> comparator) {
        this.comparator = comparator;
    }

    /**
     * Constructs a new {@code TreeMapWithSearch} instance containing the mappings from
     * the specified map and using natural ordering.
     *
     * @param map
     *            the mappings to add.
     * @throws ClassCastException
     *             if a key in the specified map does not implement the
     *             Comparable interface, or if the keys in the map cannot be
     *             compared.
     */
    public TreeMapWithSearch(Map<? extends K, ? extends V> map) {
        putAll(map);
    }

    Node<I, K, V> addToLast(Node<I, K, V> last, K key, V value) {
        if (last == null) {
            root = last = createNode(key, value);
            size = 1;
        } else if (last.size == Node.NODE_SIZE) {
            Node<I, K, V> newNode = createNode(key, value);
            attachToRight(last, newNode);
            balance(newNode);
            size++;
            last = newNode;
        } else {
            appendFromRight(last, key, value);
            size++;
        }
        return last;
    }

    /**
     * Removes all mappings from this TreeMapWithSearch, leaving it empty.
     *
     * @see Map#isEmpty()
     * @see #size()
     */
    @Override
    public void clear() {
        root = null;
        size = 0;
        modCount++;
    }

    /**
     * Returns a new {@code TreeMapWithSearch} with the same mappings, size and comparator
     * as this instance.
     *
     * @return a shallow copy of this instance.
     * @see java.lang.Cloneable
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object clone() {
        try {
            TreeMapWithSearch<I, K, V> clone = (TreeMapWithSearch<I, K, V>) super.clone();
            clone.entrySet = null;
            if (root != null) {
                clone.root = root.clone(null);
                // restore prev/next chain
                Node<I, K, V> node = minimum(clone.root);
                while (true) {
                    Node<I, K, V> nxt = successor(node);
                    if (nxt == null) {
                        break;
                    }
                    nxt.prev = node;
                    node.next = nxt;
                    node = nxt;
                }
            }
            return clone;
        } catch (CloneNotSupportedException e) {
            return null;
        }
    }

    static private <I,K extends I, V> Node<I, K, V> successor(Node<I, K, V> x) {
        if (x.right != null) {
            return minimum(x.right);
        }
        Node<I, K, V> y = x.parent;
        while (y != null && x == y.right) {
            x = y;
            y = y.parent;
        }
        return y;
    }

    /**
     * Returns the comparator used to compare elements in this map.
     *
     * @return the comparator or {@code null} if the natural ordering is used.
     */
    public Comparator<? super K> comparator() {
        return comparator;
    }

    /**
     * Returns whether this map contains the specified key.
     *
     * @param key
     *            the key to search for.
     * @return {@code true} if this map contains the specified key,
     *         {@code false} otherwise.
     * @throws ClassCastException
     *             if the specified key cannot be compared with the keys in this
     *             map.
     * @throws NullPointerException
     *             if the specified key is {@code null} and the comparator
     *             cannot handle {@code null} keys.
     */
    @Override
    public boolean containsKey(Object key) {
        if (key == null)
            return false;// we never store null keys

        Comparable<I> object = comparator == null ? toComparable((K) key) : null;
        K keyK = (K) key;
        Node<I, K, V> node = root;
        while (node != null) {
            K[] keys = node.keys;
            int left_idx = node.left_idx;
            int result = cmp(object, keyK, keys[left_idx]);
            if (result < 0) {
                node = node.left;
            } else if (result == 0) {
                return true;
            } else {
                int right_idx = node.right_idx;
                if (left_idx != right_idx) {
                    result = cmp(object, keyK, keys[right_idx]);
                }
                if (result > 0) {
                    node = node.right;
                } else if (result == 0) {
                    return true;
                } else { /*search in node*/
                    int low = left_idx + 1, mid = 0, high = right_idx - 1;
                    while (low <= high) {
                        mid = (low + high) >>> 1;
                        result = cmp(object, keyK, keys[mid]);
                        if (result > 0) {
                            low = mid + 1;
                        } else if (result == 0) {
                            return true;
                        } else {
                            high = mid - 1;
                        }
                    }
                    return false;
                }
            }
        }
        return false;
    }

    /**
     * Returns whether this map contains the specified value.
     *
     * @param value
     *            the value to search for.
     * @return {@code true} if this map contains the specified value,
     *         {@code false} otherwise.
     */
    @Override
    public boolean containsValue(Object value) {
        if (root == null) {
            return false;
        }
        if (value == null)
            return false;// we never store null values

        Node<I, K, V> node = minimum(root);
        if (value != null) {
            while (node != null) {
                int to = node.right_idx;
                V[] values = node.values;
                for (int i = node.left_idx; i <= to; i++) {
                    if (value.equals(values[i])) {
                        return true;
                    }
                }
                node = node.next;
            }
        } else {
            while (node != null) {
                int to = node.right_idx;
                V[] values = node.values;
                for (int i = node.left_idx; i <= to; i++) {
                    if (values[i] == null) {
                        return true;
                    }
                }
                node = node.next;
            }
        }
        return false;
    }

    /**
     * Returns a set containing all of the mappings in this map. Each mapping is
     * an instance of {@link Map.Entry}. As the set is backed by this map,
     * changes in one will be reflected in the other. It does not support adding
     * operations.
     *
     * @return a set of the mappings.
     */
    @Override
    public Set<Map.Entry<K, V>> entrySet() {
        if (entrySet == null) {
            entrySet = new AbstractSet<>() {
                @Override
                public int size() {
                    return size;
                }

//                @Override
//                public void clear() {
//                    TreeMapWithSearch.this.clear();
//                }

                @SuppressWarnings("unchecked")
                @Override
                public boolean contains(Object object) {
                    if (object instanceof Map.Entry) {
                        Map.Entry<K, V> entry = (Map.Entry<K, V>) object;
                        K key = entry.getKey();
                        Object v1 = TreeMapWithSearch.this.get(key), v2 = entry.getValue();
                        return v1 == null ? (v2 == null && TreeMapWithSearch.this.containsKey(key)) : v1.equals(v2);
                    }
                    return false;
                }

//                @Override
//                public boolean remove(Object object) {
//                    if (contains(object)) {
//                        Map.Entry<K, V> entry = (Map.Entry<K, V>) object;
//                        K key = entry.getKey();
//                        TreeMapWithSearch.this.remove(key);
//                        return true;
//                    }
//                    return false;
//                }

                @Override
                public Iterator<Map.Entry<K, V>> iterator() {
                    return new UnboundedEntryIterator<>(TreeMapWithSearch.this);
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
                public boolean add(Map.Entry<K, V> ktvtEntry) {
                    throw new UnsupportedOperationException("modification of entry set is not allowed for HashMapWithSearch");
                }

                /* (non-Javadoc)
                 * @see java.util.AbstractCollection#addAll(java.util.Collection)
                 */
                @Override
                public boolean addAll(Collection<? extends Map.Entry<K, V>> c) {
                    throw new UnsupportedOperationException("modification of entry set is not allowed for HashMapWithSearch");
                }

                /* (non-Javadoc)
                 * @see java.util.AbstractCollection#retainAll(java.util.Collection)
                 */
                @Override
                public boolean retainAll(@SuppressWarnings("unused") Collection<?> c) {
                    throw new UnsupportedOperationException("modification of entry set is not allowed for HashMapWithSearch");
                }
            };
        }
        return entrySet;
    }

    /**
     * Returns the first key in this map.
     *
     * @return the first key in this map.
     * @throws NoSuchElementException
     *                if this map is empty.
     */
    public K firstKey() {
        if (root != null) {
            Node<I, K, V> node = minimum(root);
            return node.keys[node.left_idx];
        }
        throw new NoSuchElementException();
    }


    /**
     * Returns the value of the mapping with the specified key.
     *
     * @param key
     *            the key.
     * @return the value of the mapping with the specified key.
     * @throws ClassCastException
     *             if the key cannot be compared with the keys in this map.
     * @throws NullPointerException
     *             if the key is {@code null} and the comparator cannot handle
     *             {@code null}.
     */
    @Override
    public V get(Object key) {
        if (key == null)
            return null;// we never store null keys

        Comparable<I> object = comparator == null ? toComparable((K) key) : null;
        K keyK = (K) key;
        Node<I, K, V> node = root;
        while (node != null) {
            K[] keys = node.keys;
            int left_idx = node.left_idx;
            int result = cmp(object, keyK, keys[left_idx]);
            if (result < 0) {
                node = node.left;
            } else if (result == 0) {
                return node.values[left_idx];
            } else {
                int right_idx = node.right_idx;
                if (left_idx != right_idx) {
                    result = cmp(object, keyK, keys[right_idx]);
                }
                if (result > 0) {
                    node = node.right;
                } else if (result == 0) {
                    return node.values[right_idx];
                } else { /*search in node*/
                    int low = left_idx + 1, mid = 0, high = right_idx - 1;
                    while (low <= high) {
                        mid = (low + high) >>> 1;
                        result = cmp(object, keyK, keys[mid]);
                        if (result > 0) {
                            low = mid + 1;
                        } else if (result == 0) {
                            return node.values[mid];
                        } else {
                            high = mid - 1;
                        }
                    }
                    return null;
                }
            }
        }
        return null;
    }

    private int cmp(Comparable<I> object, I key1, I key2) {
        return object != null ?
               object.compareTo(key2) : comparator.compare(key1, key2);
    }

    /**
     * Returns a sorted map over a range of this sorted map with all keys that
     * are less than the specified {@code endKey}. Changes to the returned
     * sorted map are reflected in this sorted map and vice versa.
     * <p>
     * Note: The returned map will not allow an insertion of a key outside the
     * specified range.
     *
     * @param endKey
     *            the high boundary of the range specified.
     * @return a sorted map where the keys are less than {@code endKey}.
     * @throws ClassCastException
     *             if the specified key cannot be compared with the keys in this
     *             map.
     * @throws NullPointerException
     *             if the specified key is {@code null} and the comparator
     *             cannot handle {@code null} keys.
     * @throws IllegalArgumentException
     *             if this map is itself a sorted map over a range of another
     *             map and the specified key is outside of its range.
     */
    public SortedMap<K, V> headMap(K endKey) {
        // Check for errors
        if (comparator == null) {
            toComparable(endKey).compareTo(endKey);
        } else {
            comparator.compare(endKey, endKey);
        }
        return new SubMap<>(this, endKey);
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
                    return TreeMapWithSearch.this.containsKey(object);
                }

                @Override
                public int size() {
                    return TreeMapWithSearch.this.size;
                }

                @Override
                public void clear() {
                    TreeMapWithSearch.this.clear();
                }

                @Override
                public boolean remove(Object object) {
                    if (contains(object)) {
                        TreeMapWithSearch.this.remove(object);
                        return true;
                    }
                    return false;
                }

                @Override
                public Iterator<K> iterator() {
                    return new UnboundedKeyIterator<>(TreeMapWithSearch.this);
                }

                /* (non-Javadoc)
                 * @see java.util.AbstractSet#add(java.lang.Object)
                 */
                @Override
                public boolean add(@SuppressWarnings("unused") K e) {
                    throw new UnsupportedOperationException("modification of key set is not allowed for TreeMapWithSearch");
                }

                /* (non-Javadoc)
                 * @see java.util.AbstractSet#addAll(java.util.Collection)
                 */
                @Override
                public boolean addAll(@SuppressWarnings("unused") Collection<? extends K> c) {
                    throw new UnsupportedOperationException("modification of key set is not allowed for TreeMapWithSearch");
                }
            };
        }
        return keySet;
    }

    /**
     * Returns the last key in this map.
     *
     * @return the last key in this map.
     * @throws NoSuchElementException
     *             if this map is empty.
     */
    public K lastKey() {
        if (root != null) {
            Node<I, K, V> node = maximum(root);
            return node.keys[node.right_idx];
        }
        throw new NoSuchElementException();
    }

    static <I,K extends I,V> Node<I, K, V> minimum(Node<I, K, V> x) {
        if (x == null) {
            return null;
        }
        while (x.left != null) {
            x = x.left;
        }
        return x;
    }

    static <I,K extends I,V> Node<I, K, V> maximum(Node<I, K, V> x) {
        if (x == null) {
            return null;
        }
        while (x.right != null) {
            x = x.right;
        }
        return x;
    }

    /**
     * Maps the specified key to the specified value.
     *
     * @param key
     *            the key.
     * @param value
     *            the value.
     * @return the value of any previous mapping with the specified key or
     *         {@code null} if there was no mapping.
     * @throws ClassCastException
     *             if the specified key cannot be compared with the keys in this
     *             map.
     * @throws NullPointerException
     *             if the specified key is {@code null} and the comparator
     *             cannot handle {@code null} keys.
     */
    @Override
    public V put(K key, V value) {
        if (key == null)
            throw new IllegalArgumentException("key cannot be null for TreeMapWithSearch");
        if (value == null)
            throw new IllegalArgumentException("value cannot be null for TreeMapWithSearch");

        if (root == null) {
            root = createNode(key, value);
            size = 1;
            modCount++;
            return null;
        }
        Comparable<I> object = comparator == null ? toComparable(key) : null;
        Node<I, K, V> node = root;
        Node<I, K, V> prevNode = null;
        int result = 0;
        while (node != null) {
            prevNode = node;
            K[] keys = node.keys;
            int left_idx = node.left_idx;
            result = cmp(object, (K) key, keys[left_idx]);
            if (result < 0) {
                node = node.left;
            } else if (result == 0) {
                V res = node.values[left_idx];
                node.values[left_idx] = value;
                return res;
            } else {
                int right_idx = node.right_idx;
                if (left_idx != right_idx) {
                    result = cmp(object, key, keys[right_idx]);
                }
                if (result > 0) {
                    node = node.right;
                } else if (result == 0) {
                    V res = node.values[right_idx];
                    node.values[right_idx] = value;
                    return res;
                } else { /*search in node*/
                    int low = left_idx + 1, mid = 0, high = right_idx - 1;
                    while (low <= high) {
                        mid = (low + high) >>> 1;
                        result = cmp(object, key, keys[mid]);
                        if (result > 0) {
                            low = mid + 1;
                        } else if (result == 0) {
                            V res = node.values[mid];
                            node.values[mid] = value;
                            return res;
                        } else {
                            high = mid - 1;
                        }
                    }
                    result = low;
                    break;
                }
            }
        } /* while */
/*
          if(node == null) {
             if(prevNode==null) {
                - case of empty Tree
             } else {
                result < 0 - prevNode.left==null - attach here
                result > 0 - prevNode.right==null - attach here
             }
          } else {
             insert into node.
             result - index where it should be inserted.
          }
        */
        size++;
        modCount++;
        if (node == null) {
            if (prevNode == null) {
                // case of empty Tree
                root = createNode(key, value);
            } else if (prevNode.size < Node.NODE_SIZE) {
                // there is a place for insert
                if (result < 0) {
                    appendFromLeft(prevNode, key, value);
                } else {
                    appendFromRight(prevNode, key, value);
                }
            } else {
                // create and link
                Node<I, K, V> newNode = createNode(key, value);
                if (result < 0) {
                    attachToLeft(prevNode, newNode);
                } else {
                    attachToRight(prevNode, newNode);
                }
                balance(newNode);
            }
        } else {
            // insert into node.
            // result - index where it should be inserted.
            if (node.size < Node.NODE_SIZE) { // insert and ok
                int left_idx = node.left_idx;
                int right_idx = node.right_idx;
                if (left_idx == 0 || ((right_idx != Node.NODE_SIZE - 1) && (right_idx - result <= result - left_idx))) {
                    int right_idxPlus1 = right_idx + 1;
                    System.arraycopy(node.keys,   result, node.keys,   result + 1, right_idxPlus1 - result);
                    System.arraycopy(node.values, result, node.values, result + 1, right_idxPlus1 - result);
                    node.right_idx = right_idxPlus1;
                    node.keys[result] = key;
                    node.values[result] = value;
                } else {
                    int left_idxMinus1 = left_idx - 1;
                    System.arraycopy(node.keys,   left_idx, node.keys,   left_idxMinus1, result - left_idx);
                    System.arraycopy(node.values, left_idx, node.values, left_idxMinus1, result - left_idx);
                    node.left_idx = left_idxMinus1;
                    node.keys[result - 1] = key;
                    node.values[result - 1] = value;
                }
                node.size++;
            } else {
                // there are no place here
                // insert and push old pair
                Node<I, K, V> previous = node.prev;
                Node<I, K, V> nextNode = node.next;
                boolean removeFromStart;
                boolean attachFromLeft = false;
                Node<I, K, V> attachHere = null;
                if (previous == null) {
                    if (nextNode != null && nextNode.size < Node.NODE_SIZE) {
                        // move last pair to next
                        removeFromStart = false;
                    } else {
                        // next node doesn't exist or full
                        // left==null
                        // drop first pair to new node from left
                        removeFromStart = true;
                        attachFromLeft = true;
                        attachHere = node;
                    }
                } else if (nextNode == null) {
                    if (previous.size < Node.NODE_SIZE) {
                        // move first pair to prev
                        removeFromStart = true;
                    } else {
                        // right == null;
                        // drop last pair to new node from right
                        removeFromStart = false;
                        attachFromLeft = false;
                        attachHere = node;
                    }
                } else {
                    if (previous.size < Node.NODE_SIZE) {
                        if (nextNode.size < Node.NODE_SIZE) {
                            // choose prev or next for moving
                            removeFromStart = previous.size < nextNode.size;
                        } else {
                            // move first pair to prev
                            removeFromStart = true;
                        }
                    } else {
                        if (nextNode.size < Node.NODE_SIZE) {
                            // move last pair to next
                            removeFromStart = false;
                        } else {
                            // prev & next are full
                            // if node.right!=null then node.next.left==null
                            // if node.left!=null then node.prev.right==null
                            if (node.right == null) {
                                attachHere = node;
                                attachFromLeft = false;
                                removeFromStart = false;
                            } else {
                                attachHere = nextNode;
                                attachFromLeft = true;
                                removeFromStart = false;
                            }
                        }
                    }
                }
                K movedKey;
                V movedValue;
                if (removeFromStart) {
                    // node.left_idx == 0
                    movedKey = node.keys[0];
                    movedValue = node.values[0];
                    int resMunus1 = result - 1;
                    System.arraycopy(node.keys,   1, node.keys,   0, resMunus1);
                    System.arraycopy(node.values, 1, node.values, 0, resMunus1);
                    node.keys  [resMunus1] = key;
                    node.values[resMunus1] = value;
                } else {
                    // node.right_idx == Node.NODE_SIZE - 1
                    movedKey   = node.keys[Node.NODE_SIZE - 1];
                    movedValue = node.values[Node.NODE_SIZE - 1];
                    System.arraycopy(node.keys,   result, node.keys,   result + 1, Node.NODE_SIZE - 1 - result);
                    System.arraycopy(node.values, result, node.values, result + 1, Node.NODE_SIZE - 1 - result);
                    node.keys[result] = key;
                    node.values[result] = value;
                }
                if (attachHere == null) {
                    if (removeFromStart) {
                        appendFromRight(previous, movedKey, movedValue);
                    } else {
                        appendFromLeft(nextNode, movedKey, movedValue);
                    }
                } else {
                    Node<I, K, V> newNode = createNode(movedKey, movedValue);
                    if (attachFromLeft) {
                        attachToLeft(attachHere, newNode);
                    } else {
                        attachToRight(attachHere, newNode);
                    }
                    balance(newNode);
                }
            }
        }
        return null;
    }

    private void appendFromLeft(Node<I, K, V> node, K keyObj, V value) {
        if (node.left_idx == 0) {
            int new_right = node.right_idx + 1;
            System.arraycopy(node.keys,   0, node.keys,   1, new_right);
            System.arraycopy(node.values, 0, node.values, 1, new_right);
            node.right_idx = new_right;
        } else {
            node.left_idx--;
        }
        node.size++;
        node.keys[node.left_idx] = keyObj;
        node.values[node.left_idx] = value;
    }

    private void attachToLeft(Node<I, K, V> node, Node<I, K, V> newNode) {
        newNode.parent = node;
        // node.left==null - attach here
        node.left = newNode;
        Node<I, K, V> predecessor = node.prev;
        newNode.prev = predecessor;
        newNode.next = node;
        if (predecessor != null) {
            predecessor.next = newNode;
        }
        node.prev = newNode;
    }

    /* add pair into node; existence free room in the node should be checked
     * before call
     */
    private void appendFromRight(Node<I, K, V> node, K keyObj, V value) {
        if (node.right_idx == Node.NODE_SIZE - 1) {
            int left_idx = node.left_idx;
            int left_idxMinus1 = left_idx - 1;
            System.arraycopy(node.keys,   left_idx, node.keys,   left_idxMinus1, Node.NODE_SIZE - left_idx);
            System.arraycopy(node.values, left_idx, node.values, left_idxMinus1, Node.NODE_SIZE - left_idx);
            node.left_idx = left_idxMinus1;
        } else {
            node.right_idx++;
        }
        node.size++;
        node.keys[node.right_idx] = keyObj;
        node.values[node.right_idx] = value;
    }

    private void attachToRight(Node<I, K, V> node, Node<I, K, V> newNode) {
        newNode.parent = node;
        // - node.right==null - attach here
        node.right = newNode;
        newNode.prev = node;
        Node<I, K, V> successor = node.next;
        newNode.next = successor;
        if (successor != null) {
            successor.prev = newNode;
        }
        node.next = newNode;
    }

    private Node<I, K, V> createNode(K keyObj, V value) {
        Node<I, K, V> node = new Node<>();
        node.keys[0] = keyObj;
        node.values[0] = value;
        node.left_idx = 0;
        node.right_idx = 0;
        node.size = 1;
        return node;
    }

    void balance(Node<I, K, V> x) {
        Node<I, K, V> y;
        x.color = true;
        while (x != root && x.parent.color) {
            if (x.parent == x.parent.parent.left) {
                y = x.parent.parent.right;
                if (y != null && y.color) {
                    x.parent.color = false;
                    y.color = false;
                    x.parent.parent.color = true;
                    x = x.parent.parent;
                } else {
                    if (x == x.parent.right) {
                        x = x.parent;
                        leftRotate(x);
                    }
                    x.parent.color = false;
                    x.parent.parent.color = true;
                    rightRotate(x.parent.parent);
                }
            } else {
                y = x.parent.parent.left;
                if (y != null && y.color) {
                    x.parent.color = false;
                    y.color = false;
                    x.parent.parent.color = true;
                    x = x.parent.parent;
                } else {
                    if (x == x.parent.left) {
                        x = x.parent;
                        rightRotate(x);
                    }
                    x.parent.color = false;
                    x.parent.parent.color = true;
                    leftRotate(x.parent.parent);
                }
            }
        }
        root.color = false;
    }

    private void rightRotate(Node<I, K, V> x) {
        Node<I, K, V> y = x.left;
        x.left = y.right;
        if (y.right != null) {
            y.right.parent = x;
        }
        y.parent = x.parent;
        if (x.parent == null) {
            root = y;
        } else {
            if (x == x.parent.right) {
                x.parent.right = y;
            } else {
                x.parent.left = y;
            }
        }
        y.right = x;
        x.parent = y;
    }


    private void leftRotate(Node<I, K, V> x) {
        Node<I, K, V> y = x.right;
        x.right = y.left;
        if (y.left != null) {
            y.left.parent = x;
        }
        y.parent = x.parent;
        if (x.parent == null) {
            root = y;
        } else {
            if (x == x.parent.left) {
                x.parent.left = y;
            } else {
                x.parent.right = y;
            }
        }
        y.left = x;
        x.parent = y;
    }


    /**
     * Copies all the mappings in the given map to this map. These mappings will
     * replace all mappings that this map had for any of the keys currently in
     * the given map.
     *
     * @param map
     *            the map to copy mappings from.
     * @throws ClassCastException
     *             if a key in the specified map cannot be compared with the
     *             keys in this map.
     * @throws NullPointerException
     *             if a key in the specified map is {@code null} and the
     *             comparator cannot handle {@code null} keys.
     */
    @Override
    public void putAll(Map<? extends K, ? extends V> map) {
        super.putAll(map);
    }

    /**
     * Removes the mapping with the specified key from this map.
     *
     * @param key
     *            the key of the mapping to remove.
     * @return the value of the removed mapping or {@code null} if no mapping
     *         for the specified key was found.
     * @throws ClassCastException
     *             if the specified key cannot be compared with the keys in this
     *             map.
     * @throws NullPointerException
     *             if the specified key is {@code null} and the comparator
     *             cannot handle {@code null} keys.
     */
    @Override
    public V remove(Object key) {
        if (size == 0) {
            return null;
        }
        Comparable<I> object = comparator == null ? toComparable((K) key) : null;
        K keyK = (K) key;
        Node<I, K, V> node = root;
        while (node != null) {
            K[] keys = node.keys;
            int left_idx = node.left_idx;
            int result = cmp(object, keyK, keys[left_idx]);
            if (result < 0) {
                node = node.left;
            } else if (result == 0) {
                V value = node.values[left_idx];
                removeLeftmost(node);
                return value;
            } else {
                int right_idx = node.right_idx;
                if (left_idx != right_idx) {
                    result = cmp(object, keyK, keys[right_idx]);
                }
                if (result > 0) {
                    node = node.right;
                } else if (result == 0) {
                    V value = node.values[right_idx];
                    removeRightmost(node);
                    return value;
                } else { /*search in node*/
                    int low = left_idx + 1, mid = 0, high = right_idx - 1;
                    while (low <= high) {
                        mid = (low + high) >>> 1;
                        result = cmp(object, keyK, keys[mid]);
                        if (result > 0) {
                            low = mid + 1;
                        } else if (result == 0) {
                            V value = node.values[mid];
                            removeMiddleElement(node, mid);
                            return value;
                        } else {
                            high = mid - 1;
                        }
                    }
                    return null;
                }
            }
        }
        return null;
    }

    void removeLeftmost(Node<I, K, V> node) {
        int index = node.left_idx;
        if (node.size == 1) {
            deleteNode(node);
        } else if (node.prev != null && (Node.NODE_SIZE - 1 - node.prev.right_idx) > node.size) {
            // move all to prev node and kill it
            Node<I, K, V> prev = node.prev;
            int size = node.right_idx - index;
            System.arraycopy(node.keys,   index + 1, prev.keys,   prev.right_idx + 1, size);
            System.arraycopy(node.values, index + 1, prev.values, prev.right_idx + 1, size);
            prev.right_idx += size;
            prev.size += size;
            deleteNode(node);
        } else if (node.next != null && (node.next.left_idx) > node.size) {
            // move all to next node and kill it
            Node<I, K, V> next = node.next;
            int size = node.right_idx - index;
            int next_new_left = next.left_idx - size;
            next.left_idx = next_new_left;
            System.arraycopy(node.keys,   index + 1, next.keys,   next_new_left, size);
            System.arraycopy(node.values, index + 1, next.values, next_new_left, size);
            next.size += size;
            deleteNode(node);
        } else {
            node.keys[index] = null;
            node.values[index] = null;
            node.left_idx++;
            node.size--;
            Node<I, K, V> prev = node.prev;
            if (prev != null && prev.size == 1) {
                node.size++;
                node.left_idx--;
                node.keys  [node.left_idx] = prev.keys  [prev.left_idx];
                node.values[node.left_idx] = prev.values[prev.left_idx];
                deleteNode(prev);
            }
        }
        modCount++;
        size--;
    }

    void removeRightmost(Node<I, K, V> node) {
        int index = node.right_idx;
        if (node.size == 1) {
            deleteNode(node);
        } else if (node.prev != null && (Node.NODE_SIZE - 1 - node.prev.right_idx) > node.size) {
            // move all to prev node and kill it
            Node<I, K, V> prev = node.prev;
            int left_idx = node.left_idx;
            int size = index - left_idx;
            System.arraycopy(node.keys,   left_idx, prev.keys,   prev.right_idx + 1, size);
            System.arraycopy(node.values, left_idx, prev.values, prev.right_idx + 1, size);
            prev.right_idx += size;
            prev.size += size;
            deleteNode(node);
        } else if (node.next != null && (node.next.left_idx) > node.size) {
            // move all to next node and kill it
            Node<I, K, V> next = node.next;
            int left_idx = node.left_idx;
            int size = index - left_idx;
            int next_new_left = next.left_idx - size;
            next.left_idx = next_new_left;
            System.arraycopy(node.keys,   left_idx, next.keys,   next_new_left, size);
            System.arraycopy(node.values, left_idx, next.values, next_new_left, size);
            next.size += size;
            deleteNode(node);
        } else {
            node.keys[index] = null;
            node.values[index] = null;
            node.right_idx--;
            node.size--;
            Node<I, K, V> next = node.next;
            if (next != null && next.size == 1) {
                node.size++;
                node.right_idx++;
                node.keys[node.right_idx]   = next.keys[next.left_idx];
                node.values[node.right_idx] = next.values[next.left_idx];
                deleteNode(next);
            }
        }
        modCount++;
        size--;
    }

    void removeMiddleElement(Node<I, K, V> node, int index) {
        // this function is called iff index if some middle element;
        // so node.left_idx < index < node.right_idx
        // condition above assume that node.size > 1
        if (node.prev != null && (Node.NODE_SIZE - 1 - node.prev.right_idx) > node.size) {
            // move all to prev node and kill it
            Node<I, K, V> prev = node.prev;
            int left_idx = node.left_idx;
            int size = index - left_idx;
            System.arraycopy(node.keys,   left_idx, prev.keys,   prev.right_idx + 1, size);
            System.arraycopy(node.values, left_idx, prev.values, prev.right_idx + 1, size);
            prev.right_idx += size;
            size = node.right_idx - index;
            System.arraycopy(node.keys,   index + 1, prev.keys,   prev.right_idx + 1, size);
            System.arraycopy(node.values, index + 1, prev.values, prev.right_idx + 1, size);
            prev.right_idx += size;
            prev.size += (node.size - 1);
            deleteNode(node);
        } else if (node.next != null && (node.next.left_idx) > node.size) {
            // move all to next node and kill it
            Node<I, K, V> next = node.next;
            int left_idx = node.left_idx;
            int next_new_left = next.left_idx - node.size + 1;
            next.left_idx = next_new_left;
            int size = index - left_idx;
            System.arraycopy(node.keys,   left_idx, next.keys,   next_new_left, size);
            System.arraycopy(node.values, left_idx, next.values, next_new_left, size);
            next_new_left += size;
            size = node.right_idx - index;
            System.arraycopy(node.keys,   index + 1, next.keys,   next_new_left, size);
            System.arraycopy(node.values, index + 1, next.values, next_new_left, size);
            next.size += (node.size - 1);
            deleteNode(node);
        } else {
            int moveFromRight = node.right_idx - index;
            int left_idx = node.left_idx;
            int moveFromLeft = index - left_idx ;
            if (moveFromRight <= moveFromLeft) {
                System.arraycopy(node.keys,   index + 1, node.keys,   index, moveFromRight);
                System.arraycopy(node.values, index + 1, node.values, index, moveFromRight);
                Node<I, K, V> next = node.next;
                if (next != null && next.size == 1) {
                    node.keys  [node.right_idx] = next.keys  [next.left_idx];
                    node.values[node.right_idx] = next.values[next.left_idx];
                    deleteNode(next);
                } else {
                    node.keys  [node.right_idx] = null;
                    node.values[node.right_idx] = null;
                    node.right_idx--;
                    node.size--;
                }
            } else {
                System.arraycopy(node.keys,   left_idx , node.keys,   left_idx  + 1, moveFromLeft);
                System.arraycopy(node.values, left_idx , node.values, left_idx + 1, moveFromLeft);
                Node<I, K, V> prev = node.prev;
                if (prev != null && prev.size == 1) {
                    node.keys  [left_idx ] = prev.keys  [prev.left_idx];
                    node.values[left_idx ] = prev.values[prev.left_idx];
                    deleteNode(prev);
                } else {
                    node.keys  [left_idx ] = null;
                    node.values[left_idx ] = null;
                    node.left_idx++;
                    node.size--;
                }
            }
        }
        modCount++;
        size--;
    }

    void removeFromIterator(Node<I, K, V> node, int index) {
        if (node.size == 1) {
            // it is safe to delete the whole node here.
            // iterator already moved to the next node;
            deleteNode(node);
        } else {
            int left_idx = node.left_idx;
            if (index == left_idx) {
                Node<I, K, V> prev = node.prev;
                if (prev != null && prev.size == 1) {
                    node.keys  [left_idx] = prev.keys  [prev.left_idx];
                    node.values[left_idx] = prev.values[prev.left_idx];
                    deleteNode(prev);
                } else {
                    node.keys  [left_idx] = null;
                    node.values[left_idx] = null;
                    node.left_idx++;
                    node.size--;
                }
            } else if (index == node.right_idx) {
                node.keys  [index] = null;
                node.values[index] = null;
                node.right_idx--;
                node.size--;
            } else {
                int moveFromRight = node.right_idx - index;
                int moveFromLeft = index - left_idx;
                if (moveFromRight <= moveFromLeft) {
                    System.arraycopy(node.keys,   index + 1, node.keys,   index, moveFromRight );
                    System.arraycopy(node.values, index + 1, node.values, index, moveFromRight );
                    node.keys  [node.right_idx] = null;
                    node.values[node.right_idx] = null;
                    node.right_idx--;
                    node.size--;
                } else {
                    System.arraycopy(node.keys,   left_idx, node.keys,   left_idx+ 1, moveFromLeft);
                    System.arraycopy(node.values, left_idx, node.values, left_idx+ 1, moveFromLeft);
                    node.keys  [left_idx] = null;
                    node.values[left_idx] = null;
                    node.left_idx++;
                    node.size--;
               }
            }
        }
        modCount++;
        size--;
    }

    private void deleteNode(Node<I, K, V> node) {
        if (node.right == null) {
            if (node.left != null) {
                attachToParent(node, node.left);
           } else {
                attachNullToParent(node);
            }
            fixNextChain(node);
        } else if(node.left == null) { // node.right != null
            attachToParent(node, node.right);
            fixNextChain(node);
        } else {
            // Here node.left!=nul && node.right!=null
            // node.next should replace node in tree
            // node.next!=null by tree logic.
            // node.next.left==null by tree logic.
            // node.next.right may be null or non-null
            Node<I, K, V> toMoveUp = node.next;
            fixNextChain(node);
            if(toMoveUp.right==null){
                attachNullToParent(toMoveUp);
            } else {
                attachToParent(toMoveUp, toMoveUp.right);
            }
            // Here toMoveUp is ready to replace node
            toMoveUp.left = node.left;
            if (node.left != null) {
            	node.left.parent = toMoveUp;
            }
            toMoveUp.right = node.right;
            if (node.right != null) {
            	node.right.parent = toMoveUp;
            }
            attachToParentNoFixup(node,toMoveUp);
            toMoveUp.color = node.color;
        }
    }

    private void attachToParentNoFixup(Node<I, K, V> toDelete, Node<I, K, V> toConnect) {
        // assert toConnect!=null
        Node<I,K,V> parent = toDelete.parent;
        toConnect.parent = parent;
        if (parent == null) {
            root = toConnect;
        } else if (toDelete == parent.left) {
            parent.left = toConnect;
        } else {
            parent.right = toConnect;
        }
    }

    private void attachToParent(Node<I, K, V> toDelete, Node<I, K, V> toConnect) {
        // assert toConnect!=null
        attachToParentNoFixup(toDelete,toConnect);
        if (!toDelete.color) {
            fixup(toConnect);
        }
    }

    private void attachNullToParent(Node<I, K, V> toDelete) {
        Node<I, K, V> parent = toDelete.parent;
        if (parent == null) {
            root = null;
        } else {
            if (toDelete == parent.left) {
                parent.left = null;
            } else {
                parent.right = null;
            }
            if (!toDelete.color) {
                fixup(parent);
            }
        }
    }

    private void fixNextChain(Node<I, K, V> node) {
        if (node.prev != null) {
            node.prev.next = node.next;
        }
        if (node.next != null) {
            node.next.prev = node.prev;
        }
    }

    private void fixup(Node<I, K, V> x) {
        Node<I, K, V> w;
        while (x != root && !x.color) {
            if (x == x.parent.left) {
                w = x.parent.right;
                if (w == null) {
                    x = x.parent;
                    continue;
                }
                if (w.color) {
                    w.color = false;
                    x.parent.color = true;
                    leftRotate(x.parent);
                    w = x.parent.right;
                    if (w == null) {
                        x = x.parent;
                        continue;
                    }
                }
                if ((w.left == null || !w.left.color)
                    && (w.right == null || !w.right.color)) {
                    w.color = true;
                    x = x.parent;
                } else {
                    if (w.right == null || !w.right.color) {
                        w.left.color = false;
                        w.color = true;
                        rightRotate(w);
                        w = x.parent.right;
                    }
                    w.color = x.parent.color;
                    x.parent.color = false;
                    w.right.color = false;
                    leftRotate(x.parent);
                    x = root;
                }
            } else {
                w = x.parent.left;
                if (w == null) {
                    x = x.parent;
                    continue;
                }
                if (w.color) {
                    w.color = false;
                    x.parent.color = true;
                    rightRotate(x.parent);
                    w = x.parent.left;
                    if (w == null) {
                        x = x.parent;
                        continue;
                    }
                }
                if ((w.left == null || !w.left.color)
                    && (w.right == null || !w.right.color)) {
                    w.color = true;
                    x = x.parent;
                } else {
                    if (w.left == null || !w.left.color) {
                        w.right.color = false;
                        w.color = true;
                        leftRotate(w);
                        w = x.parent.left;
                    }
                    w.color = x.parent.color;
                    x.parent.color = false;
                    w.left.color = false;
                    rightRotate(x.parent);
                    x = root;
                }
            }
        }
        x.color = false;
    }


    /**
     * Returns the number of mappings in this map.
     *
     * @return the number of mappings in this map.
     */
    @Override
    public int size() {
        return size;
    }

    /**
     * Returns a sorted map over a range of this sorted map with all keys
     * greater than or equal to the specified {@code startKey} and less than the
     * specified {@code endKey}. Changes to the returned sorted map are
     * reflected in this sorted map and vice versa.
     * <p>
     * Note: The returned map will not allow an insertion of a key outside the
     * specified range.
     *
     * @param startKey
     *            the low boundary of the range (inclusive).
     * @param endKey
     *            the high boundary of the range (exclusive),
     * @return a sorted map with the key from the specified range.
     * @throws ClassCastException
     *             if the start or end key cannot be compared with the keys in
     *             this map.
     * @throws NullPointerException
     *             if the start or end key is {@code null} and the comparator
     *             cannot handle {@code null} keys.
     * @throws IllegalArgumentException
     *             if the start key is greater than the end key, or if this map
     *             is itself a sorted map over a range of another sorted map and
     *             the specified range is outside of its range.
     */
    public SortedMap<K, V> subMap(K startKey, K endKey) {
        if (comparator == null) {
            if (toComparable(startKey).compareTo(endKey) <= 0) {
                return new SubMap<>(startKey, this, endKey);
            }
        } else {
            if (comparator.compare(startKey, endKey) <= 0) {
                return new SubMap<>(startKey, this, endKey);
            }
        }
        throw new IllegalArgumentException();
    }

    /**
     * Returns a sorted map over a range of this sorted map with all keys that
     * are greater than or equal to the specified {@code startKey}. Changes to
     * the returned sorted map are reflected in this sorted map and vice versa.
     * <p>
     * Note: The returned map will not allow an insertion of a key outside the
     * specified range.
     *
     * @param startKey
     *            the low boundary of the range specified.
     * @return a sorted map where the keys are greater or equal to
     *         {@code startKey}.
     * @throws ClassCastException
     *             if the specified key cannot be compared with the keys in this
     *             map.
     * @throws NullPointerException
     *             if the specified key is {@code null} and the comparator
     *             cannot handle {@code null} keys.
     * @throws IllegalArgumentException
     *             if this map itself a sorted map over a range of another map
     *             and the specified key is outside of its range.
     */
    @SuppressWarnings({"ResultOfMethodCallIgnored", "EqualsWithItself"})
    public SortedMap<K, V> tailMap(K startKey) {
        // Check for errors
        if (comparator == null) {
            toComparable(startKey).compareTo(startKey);
        } else {
            comparator.compare(startKey, startKey);
        }
        return new SubMap<>(startKey, this);
    }

    /**
     * Returns a collection of the values contained in this map. The collection
     * is backed by this map so changes to one are reflected by the other. The
     * collection supports remove, removeAll, retainAll and clear operations,
     * and it does not support add or addAll operations.
     * <p>
     * This method returns a collection which is the subclass of
     * AbstractCollection. The iterator method of this subclass returns a
     * "wrapper object" over the iterator of map's entrySet(). The {@code size}
     * method wraps the map's size method and the {@code contains} method wraps
     * the map's containsValue method.
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
                    return size;
                }

//                @Override
//                public void clear() {
//                    TreeMapWithSearch.this.clear();
//                }

                @Override
                public void clear() {
                    throw new UnsupportedOperationException("modification of value set is not allowed for TreeMapWithSearch");
                }
                @Override
                public boolean remove(@SuppressWarnings("unused") Object o) {
                    throw new UnsupportedOperationException("modification of value set is not allowed for TreeMapWithSearch");
                }
                @Override
                public boolean removeAll(@SuppressWarnings("unused") Collection<?> c) {
                    throw new UnsupportedOperationException("modification of value set is not allowed for TreeMapWithSearch");
                }
                @Override
                public boolean retainAll(@SuppressWarnings("unused") Collection<?> c) {
                    throw new UnsupportedOperationException("modification of value set is not allowed for TreeMapWithSearch");
                }

                /* (non-Javadoc)
                 * @see java.util.AbstractSet#add(java.lang.Object)
                 */
                @Override
                public boolean add(@SuppressWarnings("unused") V e) {
                    throw new UnsupportedOperationException("modification of value set is not allowed for TreeMapWithSearch");
                }

                /* (non-Javadoc)
                 * @see java.util.AbstractSet#addAll(java.util.Collection)
                 */
                @Override
                public boolean addAll(@SuppressWarnings("unused") Collection<? extends V> c) {
                    throw new UnsupportedOperationException("modification of value set is not allowed for TreeMapWithSearch");
                }

                @Override
                public Iterator<V> iterator() {
                    return new UnboundedValueIterator<>(TreeMapWithSearch.this);
                }
            };
        }
        return valuesCollection;
    }

    private void writeObject(ObjectOutputStream stream) throws IOException {
        stream.defaultWriteObject();
        stream.writeInt(size);
        if (size > 0) {
            Node<I, K, V> node = minimum(root);
            while (node != null) {
                int to = node.right_idx;
                for (int i = node.left_idx; i <= to; i++) {
                    stream.writeObject(node.keys[i]);
                    stream.writeObject(node.values[i]);
                }
                node = node.next;
            }
        }
    }

    @SuppressWarnings("unchecked")
    private void readObject(ObjectInputStream stream) throws IOException,
                                                          ClassNotFoundException {
        stream.defaultReadObject();
        int size = stream.readInt();
        Node<I, K, V> lastNode = null;
        for (int i = 0; i < size; i++) {
            lastNode = addToLast(lastNode, (K) stream.readObject(), (V) stream.readObject());
        }
    }
}
