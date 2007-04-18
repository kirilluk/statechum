package statechum;

import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;

public class DeterministicDirectedSparseGraph {

	public static abstract class CmpVertex extends DirectedSparseVertex implements Comparable
	{}
	
	/** The extension of the vertex where all operations are ID-based, for performance. */
	public static class DeterministicVertex extends CmpVertex
	{

		/* (non-Javadoc)
		 * @see edu.uci.ics.jung.graph.impl.AbstractSparseVertex#toString()
		 */
		@Override
		public String toString() {
			if (containsUserDatumKey(JUConstants.LABEL))
				return getUserDatum(JUConstants.LABEL).toString();
			else
				return super.toString();
		}

		public int compareTo(Object o) {
			assert o != null;
			assert o instanceof DeterministicVertex : "an attempt to compare "+toString()+" with a non-CmpVertx "+o.toString();
			DeterministicVertex v=(DeterministicVertex)o; 
			if (v.id == this.id) return 0;
			return (this.id < v.id)? -1:1;
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
}
