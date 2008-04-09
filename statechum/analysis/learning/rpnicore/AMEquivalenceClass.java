/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of Statechum.

Statechum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Statechum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Statechum.  If not, see <http://www.gnu.org/licenses/>.
*/
package statechum.analysis.learning.rpnicore;

import java.util.Collection;
import java.util.Iterator;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;

public class AMEquivalenceClass
{
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + mergedVertex.hashCode();
		result = prime * result + vertices.hashCode();
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
		if (!(obj instanceof AMEquivalenceClass))
			return false;
		final AMEquivalenceClass other = (AMEquivalenceClass) obj;
		if (!mergedVertex.equals(other.mergedVertex))
			return false;
		return vertices.equals(other.vertices);
	}

	public AMEquivalenceClass(Collection<CmpVertex> eqclass)
	{
		if (eqclass == null || eqclass.isEmpty()) throw new IllegalArgumentException("cannot create an empty equivalence class");
		vertices = eqclass;mergedVertex=eqclass.iterator().next();
	}
	
	/** The merged vertex. */
	CmpVertex mergedVertex;
	
	/** Vertices in the original graph corresponding to the merged vertex. */
	Collection<CmpVertex> vertices;

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuffer result = new StringBuffer("["+mergedVertex.getID().toString()+"->{");
		Iterator<CmpVertex> vertIter = vertices.iterator();
		result.append(vertIter.next().getID().toString());
		while(vertIter.hasNext()) result.append(',').append(vertIter.next().getID().toString());
		result.append("}]");
		return result.toString();
	}
}
