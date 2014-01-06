/*
 * Copyright (c) 2000, 2010, Oracle and/or its affiliates. All rights reserved.
 * ORACLE PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 *
 *
 * This file is essentially an adapted LinkedHashMap/HashMap
*/

package statechum.collections;

import java.util.Set;
import java.util.TreeMap;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;


public class TreeMapWithSearch<K,V> extends TreeMap<K,V> implements MapWithSearch<K,V>
{
	/**
	 * Number for serialisation.
	 */
	private static final long serialVersionUID = 5025142577937498737L;
	
	private static int collisionNumber=0;
	
	public TreeMapWithSearch(@SuppressWarnings("unused") int arg)
	{
		super();
	}
	
	public static int getCollisions()
	{
		return collisionNumber;
	}

	/* This one can be implemented by creating another map, but such a map will have to be updated every time the main map changes, such as when put/putAll or even remove() operation is carried out on a member of keySet(). 
	 * Since this class is supposed to only be used on small automata, we do a linear search.
	 * 
	 * @see statechum.MapWithSearch#findElementById(statechum.DeterministicDirectedSparseGraph.VertexID)
	 */
	@Override
	public K findElementById(VertID id) {
		for(K el:keySet())
			if (el.equals(id) || (el instanceof CmpVertex && ((CmpVertex)el).equals(id)))
				return el;
		return null;
	}

	@Override
	public Set<java.util.Map.Entry<K, V>> getTreeEntrySet() {
		return entrySet();
	}

	@Override
	public Set<java.util.Map.Entry<K, V>> getPotentiallyOrderedEntrySet(@SuppressWarnings("unused") boolean ordered) {
		return entrySet();
	}

	@Override
	public Set<K> getPotentiallyOrderedKeySet(@SuppressWarnings("unused") boolean ordered) {
		return keySet();
	}
}

