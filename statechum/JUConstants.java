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

public enum JUConstants {
	LABEL("label"),
	ACCEPTED("accepted"),
	TITLE("title"),
	INITIAL("init"),// whether a vertex is an initial state
	HIGHLIGHT("highlight"),
	STATS("STATS"),
	COLOUR("colour"),
	RED("red"),BLUE("blue"),AMBER("amber"),
	JUNKVERTEX("junk");// used for testing that searching for a property that does not exist returns a null vertex.
	
	private String stringRepresentation;
	
	JUConstants(String textualName)
	{
		stringRepresentation = textualName;
	}
	
	public String toString()
	{
		return stringRepresentation;
	}
}
