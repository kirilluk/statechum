package statechum;

/** A generic superclass for all pairs. Cannot implement Comparable because 
 * Comparable has to be implemented with different generic 
 * parameters by derived classes.
 * 
 * @author kirr
 *
 * @param <A>
 * @param <B>
 */
public class Pair<A extends Comparable<A>,B extends Comparable<B>> {
	public final A firstElem;
	public final B secondElem;
	
	public Pair(A a,B b)
	{
		firstElem = a;secondElem = b;
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
		final Pair other = (Pair) obj;
		if (firstElem == null) {
			if (other.firstElem != null)
				return false;
		} else if (!firstElem.equals(other.firstElem))
			return false;
		if (secondElem == null) {
			if (other.secondElem != null)
				return false;
		} else if (!secondElem.equals(other.secondElem))
			return false;
		return true;
	}

	public int compareTo(Pair<A, B> o) {
		int firstComp = firstElem.compareTo(o.firstElem);
		
		if(firstComp != 0)
			return firstComp; 
		return secondElem.compareTo(o.secondElem);
	}
}
