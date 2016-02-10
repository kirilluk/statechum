/* Copyright (c) 2006-2012 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
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

import java.util.Map;
import java.util.Set;

import statechum.DeterministicDirectedSparseGraph.VertID;

/** This one makes it possible to search a map for an element with a specific ID and returns an element of its domain that matches that ID.
 * 
 */
public interface MapWithSearch<K,V> extends Map<K, V>
{
	/** Returns true if the collection expects to store elements that implement {@link ConvertibleToInt} interface. */
	boolean expectsConvertibleToInt();
	
	/** Searches a map for an element with a specific ID and returns an element of its domain that matches that ID. */
	K findElementById(VertID id);
	
	/** Returns elements in the same order as they would have been returned by TreeMap, for testing only. */
	Set<Map.Entry<K, V>> getTreeEntrySet();

	/** The same as {@link #entrySet()} if the argument is false. 
	 * Will return elements in the same order as they would have been returned by TreeMap when the argument is true (very slow hence for testing only).
	 * Returning elements in the same order is needed for testing where the outcome should not depend on the hash function.  
	 */
	Set<Map.Entry<K, V>> getPotentiallyOrderedEntrySet(boolean ordered);
	
	/** The same as {@link #entrySet()} if the argument is false. 
	 * Will return elements in the same order as they would have been returned by TreeMap when the argument is true (very slow hence for testing only).
	 * Returning elements in the same order is needed for testing where the outcome should not depend on the hash function.  
	 */
	Set<K> getPotentiallyOrderedKeySet(boolean ordered);	

}

