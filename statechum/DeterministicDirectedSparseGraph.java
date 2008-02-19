/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum;

import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class DeterministicDirectedSparseGraph {

	public static abstract class CmpVertex extends DirectedSparseVertex
			implements Comparable {
	}

	/**
	 * The extension of the vertex where all operations are ID-based, for
	 * performance.
	 */
	public static class DeterministicVertex extends CmpVertex {
		protected String label = null;

		protected int hashCode = super.hashCode();

		public DeterministicVertex(String string) {
			super();
			addUserDatum(JUConstants.LABEL, string, UserData.SHARED);
		}

		public DeterministicVertex() {
			super();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see edu.uci.ics.jung.utils.UserDataDelegate#addUserDatum(java.lang.Object,
		 *      java.lang.Object,
		 *      edu.uci.ics.jung.utils.UserDataContainer.CopyAction)
		 */
		@Override
		public void addUserDatum(Object key, Object datum, CopyAction copyAct) {
			if (key == JUConstants.LABEL) {
				label = (String) datum;
				hashCode = datum.hashCode();
			}
			super.addUserDatum(key, datum, copyAct);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see edu.uci.ics.jung.utils.UserDataDelegate#setUserDatum(java.lang.Object,
		 *      java.lang.Object,
		 *      edu.uci.ics.jung.utils.UserDataContainer.CopyAction)
		 */
		@Override
		public void setUserDatum(Object key, Object datum, CopyAction copyAct) {
			if (key == JUConstants.LABEL) {
				label = (String) datum;
				hashCode = datum.hashCode();
			}
			super.setUserDatum(key, datum, copyAct);
		}

		@Override
		public int hashCode() {
			return hashCode;
		}

		/*
		 * (non-Javadoc)
		 * 
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
			assert o instanceof DeterministicVertex : "an attempt to compare "
					+ toString() + " with a non-CmpVertx " + o.toString();
			DeterministicVertex v = (DeterministicVertex) o;
			if (this == v)
				return 0;
			return label.compareTo(v.label);
			// if (v.id == this.id) return 0;
			// return (this.id < v.id)? -1:1;
		}
	}

	public static class DeterministicEdge extends DirectedSparseEdge {

		public DeterministicEdge(CmpVertex from, CmpVertex to) {
			super(from, to);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see edu.uci.ics.jung.graph.impl.AbstractSparseVertex#toString()
		 */
		@Override
		public String toString() {
			return getSource().toString() + "-"
					+ getUserDatum(JUConstants.LABEL).toString() + "->"
					+ getDest().toString();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see edu.uci.ics.jung.graph.impl.AbstractElement#hashCode()
		 */
		@Override
		public int hashCode() {
			final int PRIME1 = 31;
			return getDest().hashCode() * PRIME1 + getSource().hashCode();
		}

	}
}
