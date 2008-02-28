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

import statechum.analysis.learning.RPNIBlueFringeLearner;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class DeterministicDirectedSparseGraph {

	public interface CmpVertex extends Comparable<CmpVertex> {
		/** Returns a name of this vertex. */
		String getName();

		/** Returns true if this is an accept vertex and false for a reject one. */
		boolean isAccept();
		/** Makes this state an accept/reject one. */
		void setAccept(boolean accept);

		/** Returns the colour of this vertex. */
		JUConstants getColour();
		/** Sets the colour of this vertex. null removes the colour. */
		void setColour(JUConstants colour);

		/** Determines whether this vertex is to be highlighted. */
		boolean isHighlight();
		/** Sets the mark on this vertex. */
		void setHighlight(boolean hightlight);
	}

	/**
	 * The extension of the vertex where all operations are ID-based, for
	 * performance.
	 */
	public static class DeterministicVertex extends DirectedSparseVertex implements Comparable<CmpVertex>, CmpVertex {
		protected String label = null;

		protected int hashCode = super.hashCode();

		public DeterministicVertex(String string) {
			super();
			addUserDatum(JUConstants.LABEL, string, UserData.SHARED);
		}

		private DeterministicVertex() {
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

			return super.toString();
		}

		public int compareTo(CmpVertex o) {
			assert o != null;
			assert o instanceof DeterministicVertex : "an attempt to compare "
					+ toString() + " with a non-DeterministicVertex " + o.toString();
			DeterministicVertex v = (DeterministicVertex) o;
			if (this == v)
				return 0;
			return label.compareTo(v.label);
		}
		
		/** Compares this vertex with a different one, based on label alone.
		 */

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (!(obj instanceof DeterministicVertex))
				return false;
			
			final DeterministicVertex other = (DeterministicVertex) obj;
			
			if (label == null)
				return other.label == null;
			else
				return label.equals(other.label);
		}
		
		public String getName() {
			return label;
		}

		public boolean isAccept() {
			return RPNIBlueFringeLearner.isAccept(this);
		}

		public void setAccept(boolean accept) 
		{
			addUserDatum(JUConstants.ACCEPTED, accept, UserData.SHARED);
		}

		public JUConstants getColour() 
		{
			return (JUConstants)getUserDatum(JUConstants.COLOUR);
		}

		public void setColour(JUConstants colour) 
		{
			removeUserDatum(JUConstants.COLOUR);
			if (colour != null)
			{
				if (colour != JUConstants.RED && colour != JUConstants.BLUE)
					throw new IllegalArgumentException("colour "+colour+" is not a valid colour (vertex "+getName()+")");
				addUserDatum(JUConstants.COLOUR, colour, UserData.SHARED);
			}
		}

		public void setHighlight(boolean hightlight) {
			if (hightlight)
				addUserDatum(JUConstants.HIGHLIGHT, "whatever", UserData.SHARED);
			else
				removeUserDatum(JUConstants.HIGHLIGHT);
		}

		public boolean isHighlight() {
			return containsUserDatumKey(JUConstants.HIGHLIGHT);
		}
	}

	public static class DeterministicEdge extends DirectedSparseEdge {

		public DeterministicEdge(DeterministicVertex from, DeterministicVertex to) {
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
