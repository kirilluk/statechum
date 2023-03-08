/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */ 

package statechum;

public enum JUConstants {
	LABEL("label"),
	ACCEPTED("accepted"),// used to mark accept-states
	TITLE("title"),
	INITIAL("init"),// (boolean) whether a vertex is an initial state
	HIGHLIGHT("highlight"),// (boolean) whether a vertex should be highlighted on a display
	ORIGSTATE("origstate"),// (VertexID) the name of a vertex in hard facts which corresponds to this vertex
	DEPTH("depth"),// (int) how far the original state (ORIGSTATE) was from the root state
	
	STATS("STATS"),
	COLOUR("colour"),
	DIFF("DIFF"),// information used to facilitate coloring of edges 
	NONE("none"),// used to denote absence of any colour rather than null in AMEquivalenceClass 
	RED("red"),BLUE("blue"),AMBER("amber"),GRAY("gray"),INF_AMBER("inf_amber"),
	JUNKVERTEX("junk"),// used for testing that searching for a property that does not exist returns a null vertex.
	EDGE("edge"),VERTEX("vertex"), // used for labelling vertices
    LAYOUTOPTIONS("layoutOptions") // settings affecting the preferred layout such as whether negative nodes are to be shown
	;
	private final String stringRepresentation;
	
	public enum VERTEXLABEL
	{
		ACCEPT, // vertex is an accept one
		REJECT, // vertex is reject
		NOLABEL // unknown - it could be either of the above.
	}
	
	public enum PAIRCOMPATIBILITY
	{
		INCOMPATIBLE(-20), // used to designate a pair of states as incompatible
		MERGED(-21), // used to designate a pair of states as those which have been 
			// merged before, hence we might wish to merge them without asking user any questions 
		 	// as long as the pair of states is compatible. Such as merger will have to be sandwiched
			// between checkers and the bottom of the stack where autoanswers and user answers lie.
		THEN(-22); // used in the if-then FSM constraints. If a "match" state of an "if" machine is entered, then
			// the "then" machine is unrolled starting from the corresponding state.

		private final int pairConstant;
		
		public int getInteger()
		{
			return pairConstant;
		}

		PAIRCOMPATIBILITY(int value)
		{
			pairConstant = value;
		}
		
		/** Converts state compatibility integer value to an equivalent JUConstants one. 
		 * 
		 * @param compat value to convert
		 * @return resulting value
		 */
		public static PAIRCOMPATIBILITY compatibilityToJUConstants(long compat)
		{
			PAIRCOMPATIBILITY result;
			if (compat == PAIRCOMPATIBILITY.INCOMPATIBLE.getInteger()) result = PAIRCOMPATIBILITY.INCOMPATIBLE;
			else
			if (compat == PAIRCOMPATIBILITY.MERGED.getInteger()) result = PAIRCOMPATIBILITY.MERGED;
			else
			if (compat == PAIRCOMPATIBILITY.THEN.getInteger()) result = PAIRCOMPATIBILITY.THEN;
			else
				throw new IllegalArgumentException(compat+" is not a valid compatibility value");
			return result;
		}
	}
	
	public static final int intUNKNOWN = -1;

	JUConstants(String textualName)
	{
		stringRepresentation = textualName;
	}
	
	@Override
	public String toString()
	{
		return stringRepresentation;
	}
}
