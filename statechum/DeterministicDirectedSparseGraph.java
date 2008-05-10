package statechum;

import java.util.Random;

import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class DeterministicDirectedSparseGraph {

	public static abstract class CmpVertex extends DirectedSparseVertex implements Comparable
	{}
	
	/** The extension of the vertex where all operations are ID-based, for performance. */
	public static class DeterministicVertex extends CmpVertex
	{
		protected String label = null;
		protected int hashCode = super.hashCode();
		
		public DeterministicVertex(String string) {
			super();addUserDatum(JUConstants.LABEL, string, UserData.SHARED);
		}

		public DeterministicVertex() {
			super();
		}

		/* (non-Javadoc)
		 * @see edu.uci.ics.jung.utils.UserDataDelegate#addUserDatum(java.lang.Object, java.lang.Object, edu.uci.ics.jung.utils.UserDataContainer.CopyAction)
		 */
		@Override
		public void addUserDatum(Object key, Object datum, CopyAction copyAct) {
			if (key == JUConstants.LABEL)
			{
				label = (String)datum;
				hashCode = datum.hashCode();
			}
			super.addUserDatum(key, datum, copyAct);
		}

		/* (non-Javadoc)
		 * @see edu.uci.ics.jung.utils.UserDataDelegate#setUserDatum(java.lang.Object, java.lang.Object, edu.uci.ics.jung.utils.UserDataContainer.CopyAction)
		 */
		@Override
		public void setUserDatum(Object key, Object datum, CopyAction copyAct) {
			if (key == JUConstants.LABEL)
			{
				label = (String)datum;
				hashCode = datum.hashCode();
			}
			super.setUserDatum(key, datum, copyAct);
		}

		@Override
		public int hashCode()
		{
			return hashCode;
		}
		
		/* (non-Javadoc)
		 * @see edu.uci.ics.jung.graph.impl.AbstractSparseVertex#toString()
		 */
		@Override
		public String toString() {
			if (label != null)
				return label;
			else
				return super.toString();
		}

		public int compareTo(Object o) {
			assert o != null;
			assert o instanceof DeterministicVertex : "an attempt to compare "+toString()+" with a non-CmpVertx "+o.toString();
			DeterministicVertex v=(DeterministicVertex)o;
			if (this == v) return 0;
			return label.compareTo(v.label);
			//if (v.id == this.id) return 0;
			//return (this.id < v.id)? -1:1;
		}
	}

	public static class DeterministicEdge extends DirectedSparseEdge
	{

		public DeterministicEdge(CmpVertex from, CmpVertex to) {
			super(from, to);
		}

		/* (non-Javadoc)
		 * @see edu.uci.ics.jung.graph.impl.AbstractSparseVertex#toString()
		 */
		@Override
		public String toString() {
			return getSource().toString()+"-"+getUserDatum(JUConstants.LABEL).toString()+"->"+getDest().toString();
		}

		/* (non-Javadoc)
		 * @see edu.uci.ics.jung.graph.impl.AbstractElement#hashCode()
		 */
		@Override
		public int hashCode() {
			final int PRIME1 = 31;
			return getDest().hashCode()*PRIME1+getSource().hashCode();
		}
		
	}
	
	/** Returns an array of sequential numbers 0..howMany, randomly permuted. 
	 * 
	 * @param howMany how many numbers to return (and hence the length of the array.
	 * @param seed the seed for the random generator
	 * @return the array of numbers.
	 */ 
	public static int [] getNonRepeatingNumbers(int howMany, int seed)
	{
    	int newNumbers[] = new int[howMany];
    	for(int i=0;i < howMany;++i) newNumbers[i]=i;
    	Random rnd = new Random(seed);
    	for(int i=0;i < howMany;++i) 
    	{ 
    		int a = rnd.nextInt(howMany),b=rnd.nextInt(howMany);
    		int value = newNumbers[a];newNumbers[a]=newNumbers[b];newNumbers[b]=value;
    	}
    	
    	return newNumbers;
	}
}
