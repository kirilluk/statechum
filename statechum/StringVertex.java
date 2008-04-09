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

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;


public class StringVertex implements CmpVertex {
	protected JUConstants colour = null;
	protected boolean accept = true,highlight = false;
	final protected VertexID vertexId;

	public StringVertex(String nameArg)
	{
		vertexId = new DeterministicDirectedSparseGraph.VertexID(nameArg);
	}
	
	public StringVertex(VertexID nameArg)
	{
		vertexId = nameArg;
	}
	
	public JUConstants getColour() {
		return colour;
	}

	public VertexID getID() {
		return vertexId;
	}
	
	public boolean isAccept() {
		return accept;
	}

	public boolean isHighlight() {
		return highlight;
	}

	public void setAccept(boolean a) {
		accept = a;
	}

	public void setColour(JUConstants c) {
		if (c != null && c != JUConstants.RED && c != JUConstants.BLUE)
			throw new IllegalArgumentException("colour "+colour+" is not a valid colour (vertex "+getID().toString()+")");
		colour = c;
	}

	public void setHighlight(boolean h) {
		highlight = h;
	}

	/** The ordering is based on names only ignoring whether this is an
	 * accept or a reject vertex. This is necessary if we wish to adjust
	 * an order of traversal in experiments. In cases where accepts or
	 * rejects should appear earlier/later, the <i>nextID</i> method
	 * will generate the appropriate number. 
	 */
	public int compareTo(CmpVertex o) {
		assert o != null;
/*		if (!(o instanceof CmpVertex))
			throw new IllegalArgumentException("an attempt to compare "
				+ toString() + " with a non-CmpVertex " + o.getName());*/
		CmpVertex v = o;
		if (this == v)
			return 0;
		return vertexId.compareTo(v.getID());
	}

	@Override
	public int hashCode() {
		int labelHashCode = vertexId == null?super.hashCode():vertexId.hashCode();
		if (!isAccept())
			labelHashCode = ~labelHashCode;
		
		return labelHashCode;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
			return true;
		if (!(obj instanceof CmpVertex))
			return false;
		
		final CmpVertex other = (CmpVertex) obj;
		if (isAccept() != other.isAccept())
			return false;
		
		if (vertexId == null)
			return other.getID() == null;
		
		return vertexId.equals(other.getID());
		
	}

	@Override
	public String toString()
	{
		return vertexId == null?"NULL":vertexId.toString();
	}
}
