/*
 * Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * StateChum. If not, see <http://www.gnu.org/licenses/>.
 */ 

package statechum;

/** A generic superclass for all pairs. 
 * 
 * @author kirr
 *
 * @param <A>
 * @param <B>
 */
public class Pair<A extends Comparable<? super A>,B extends Comparable<? super B>> implements Comparable<Pair<A,B>>
{
	public final A firstElem;
	public final B secondElem;
	
	public Pair(A a,B b)
	{
		firstElem = a;secondElem = b;
	}

	@Override
	public String toString()
	{
		return firstElem+" , "+secondElem;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int PRIME = 31;
		int result = 1;
		result = PRIME * result + ((firstElem == null) ? 0 : firstElem.hashCode());
		result = PRIME * result + ((secondElem == null) ? 0 : secondElem.hashCode());
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
		if (!(obj instanceof Pair))
			return false;
		@SuppressWarnings("unchecked")
		final Pair<A,B> other = (Pair<A,B>) obj;
		if (firstElem == null) {
			if (other.firstElem != null) {
				return false;
                        }
		} else if (!firstElem.equals(other.firstElem)) {
			return false;
                }
		if (secondElem == null) {
			if (other.secondElem != null) {
				return false;
                        }
		} else if (!secondElem.equals(other.secondElem)) {
			return false;
                }
		return true;

	}

	@Override
	public int compareTo(Pair<A,B> o) {
		int firstComp = 0;
		
		if (firstElem == null)
		{
			if (o.firstElem != null)
				firstComp = -1;
		}
		else
		{
			if (o.firstElem == null)
				firstComp = 1;
			else
				firstComp = firstElem.compareTo(o.firstElem);
		}
		
		if(firstComp != 0)
			return firstComp;
		
		int secondComp = 0;
		
		if (secondElem == null)
		{
			if (o.secondElem != null)
				secondComp = -1;
		}
		else
		{
			if (o.secondElem == null)
				secondComp = 1;
			else
				secondComp = secondElem.compareTo(o.secondElem);
		}
		
		return secondComp;
	}
}
