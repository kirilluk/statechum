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
package statechum.analysis.learning;

import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;

public class StringVertex implements CmpVertex {
	protected JUConstants colour;
	protected boolean accept,highlight;
	final protected String label;

	public StringVertex(String nameArg)
	{
		label = nameArg;accept = true;highlight=false;colour=null;
	}
	
	private StringVertex()
	{
		label = null;
	}
	
	public JUConstants getColour() {
		return colour;
	}

	public String getName() {
		return label;
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
		colour = c;
	}

	public void setHighlight(boolean h) {
		highlight = h;
	}

	public int compareTo(CmpVertex o) {
		assert o != null;
		assert o instanceof StringVertex : "an attempt to compare "
				+ toString() + " with a non-StringVertex " + o.toString();
		StringVertex v = (StringVertex) o;
		if (this == v)
			return 0;
		return label.compareTo(v.label);
	}

	@Override
	public int hashCode() {
		return label == null?super.hashCode():label.hashCode();
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
			return true;
		if (!(obj instanceof StringVertex))
			return false;
		
		final StringVertex other = (StringVertex) obj;
		
		if (label == null)
			return other.label == null;
		
		return label.equals(other.label);
		
	}

	@Override
	public String toString()
	{
		return label == null?"NULL":label;
	}
}
