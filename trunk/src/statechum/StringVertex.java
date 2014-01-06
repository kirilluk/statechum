/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.DeterministicDirectedSparseGraph.VertexID;

final public class StringVertex extends VertexID implements CmpVertex
{
	/**
	 * ID for serialisation
	 */
	private static final long serialVersionUID = -3577238342791987067L;
	
	protected JUConstants colour = null;
	protected boolean accept = true,highlight = false;
	private VertID origState = null;
	private int depth=JUConstants.intUNKNOWN;
	
	public StringVertex(String nameArg)
	{
		super(VertexID.parseID(nameArg));
	}
	
	public StringVertex(VertID nameArg)
	{
		super(nameArg);
	}
	
	@Override 
	public JUConstants getColour() {
		return colour;
	}

	@Override 
	public boolean isAccept() {
		return accept;
	}

	@Override 
	public boolean isHighlight() {
		return highlight;
	}

	@Override 
	public void setAccept(boolean a) {
		accept = a;
	}

	@Override 
	public void setColour(JUConstants c) {
		if (c != null && c != JUConstants.RED && c != JUConstants.BLUE && c != JUConstants.AMBER && c != JUConstants.GRAY && c != JUConstants.INF_AMBER)
			throw new IllegalUserDataException("colour "+colour+" is not a valid colour (vertex "+super.toString()+")");
		colour = c;
	}

	@Override 
	public void setHighlight(boolean h) {
		highlight = h;
	}

	/** The ordering is based on names only ignoring whether this is an
	 * accept or a reject vertex. This is necessary if we wish to adjust
	 * an order of traversal in experiments. In cases where accepts or
	 * rejects should appear earlier/later, the <i>nextID</i> method
	 * will generate the appropriate number. 
	 */
	@Override 
	public int compareTo(VertID o) 
	{
		assert o != null;
/*		if (!(o instanceof CmpVertex))
			throw new IllegalArgumentException("an attempt to compare "
				+ toString() + " with a non-CmpVertex " + o.getName());*/
		if (this == o)
			return 0;
		return super.compareTo(o);
	}

	@Override
	public String toString()
	{
		String origName = "";if (getOrigState() != null) origName=" <"+getOrigState()+">";
		String strDepth="";if(getDepth()!=JUConstants.intUNKNOWN) strDepth=" depth="+getDepth();
		String strColour="";if(getColour()!=null) strColour=" colour="+getColour();
		return super.toString()+origName+strDepth+strColour;
	}

	@Override 
	public int getDepth() {
		return depth;
	}

	@Override 
	public VertID getOrigState() {
		return origState;
	}

	@Override 
	public void setDepth(int argDepth) {
		depth = argDepth;
	}

	@Override 
	public void setOrigState(VertID newState) {
		if (newState == null) origState = null;
		else origState = new VertexID(newState);
	}
}
