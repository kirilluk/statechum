package statechum;

import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;

public class DeterministicDirectedSparseGraph {

	public static class DeterministicVertex extends DirectedSparseVertex
	{

		/* (non-Javadoc)
		 * @see edu.uci.ics.jung.graph.impl.AbstractSparseVertex#toString()
		 */
		@Override
		public String toString() {
			return getUserDatum(JUConstants.LABEL).toString();
		}

		/* (non-Javadoc)
		 * @see edu.uci.ics.jung.graph.impl.AbstractElement#hashCode()
		 */
		@Override
		public int hashCode() {
			Object label = getUserDatum(JUConstants.LABEL);
			return label.hashCode();
		}
	
	}

	public static class DeterministicEdge extends DirectedSparseEdge
	{

		public DeterministicEdge(Vertex from, Vertex to) {
			super(from, to);
			assert from instanceof DeterministicVertex && to instanceof DeterministicVertex;
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
