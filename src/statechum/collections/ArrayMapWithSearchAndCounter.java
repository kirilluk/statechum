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

import statechum.Pair;

/** This is different from the parent class {@link ArrayMapWithSearch} in that it keeps track of the number of data values to be stored or removed. As such, it consumes more memory
 * (which is not important since such collections are not used inside others) but offers an efficient way to determine the number of positive and negative entries. Therefore,
 * we will quickly know when to switch to HashMap without having to iterate through the collection. 
 * 
 * @author kirill
 *
 */
public class ArrayMapWithSearchAndCounter<K extends ConvertibleToInt,V> extends ArrayMapWithSearch<K, V> {

	int positiveCount = 0, negativeCount = 0;
	
	public ArrayMapWithSearchAndCounter() {
	}

	public ArrayMapWithSearchAndCounter(int posSize,int negSize)
	{
		super(posSize,negSize);
	}
	
	@Override
	public void clear() {
		if (isEmpty())
			return;
		positiveCount = 0;negativeCount = 0;
		super.clear();
	}

	@Override
	public V put(K k, V v) {
		if (k == null)
			throw new IllegalArgumentException("Keys cannot be null");
		if (v == null)
			throw new IllegalArgumentException("Values cannot be null");
		V prevValue = super.put(k, v);

		if (prevValue == null)
		{// did not have a previous value.
			if (k.toInt() >= 0)
				positiveCount+=1;
			else
				negativeCount+=1;
		}
		
		return prevValue;
	}

	@Override
	public V remove(Object k) {
		V prevValue = super.remove(k);
		if (prevValue != null)
		{// had a previous value.
			if ( ((ConvertibleToInt) k).toInt() >= 0)
				positiveCount-=1;
			else
				negativeCount-=1;
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
}
