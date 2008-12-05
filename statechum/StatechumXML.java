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

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public enum StatechumXML 
{
	/** GD XML tags. */
	gdGD("GD"), gdAdded("gdAdded"), gdRemoved("gdRemoved"), gdRelabelling("gdRelabelling"),

	/** Observer-related. */
	ELEM_ANSWER, ELEM_QUESTIONS, ATTR_QUESTIONS, ELEM_PAIRS, ELEM_STATECHUM_TESTTRACE, 
		ATTR_QUESTION, ATTR_TESTSET, ATTR_FAILEDPOS, ATTR_LTL, ELEM_PAIR, ELEM_SEQ, ATTR_SEQ, ATTR_Q, ATTR_R, ATTR_SCORE, ATTR_OTHERSCORE, ELEM_RESTART, ATTR_KIND, 
		ELEM_EVALUATIONDATA,ATTR_GRAPHKIND, ELEM_INIT, ELEM_MERGEANDDETERMINIZE, ATTR_LEARNINGOUTCOME, 
		ATTR_POSITIVE_SIZE, ATTR_POSITIVE_SEQUENCES, ATTR_NEGATIVE_SIZE, ATTR_NEGATIVE_SEQUENCES,
		ELEM_LTL,ELEM_AUGMENTPTA, ATTR_ACCEPT, ATTR_COLOUR, ELEM_PROGRESSINDICATOR, ELEM_LABELDETAILS, ATTR_GRAPHNUMBER, ATTR_WITHCONSTRAINTS,

	/** Graphml namespace */
	graphmlNS("gml"),
	
	/** Graphml top-level node tag if a name space is not used. */
	graphmlNodeName("graphml"),
	
	/** Graphml top-level node tag. */
	graphmlNodeNameNS(graphmlNS+":"+graphmlNodeName),
	
	/** Graphml uri */
	graphlmURI("http://graphml.graphdrawing.org/xmlns/graphml");	
	
	
	private final String stringRepresentation;
	
	StatechumXML(String textualName) {stringRepresentation = textualName;}
	StatechumXML() {stringRepresentation = null;}
	
	@Override
	public String toString()
	{
		if (stringRepresentation == null)
			return super.name();
		
		return stringRepresentation;
	}
	
	/** Given an element, this method returns all direct descendants with the specified tag. 
	 */
	public static NodeList getChildWithTag(Element elem, String tag)
	{
		Node resultA = null, resultB = null; int counter=0;
		NodeList children = elem.getChildNodes();
		if (children.getLength() > 0)
			for(int i=0;i< children.getLength();++i)
				if (tag.equals(children.item(i).getNodeName()))
				{// found an element with an expected tag.
					if (resultA == null)
						resultA = children.item(i);
					else
						if (resultB == null)
							resultB = children.item(i);
						
					++counter;
				}
		
		final int finalCounter = counter;final Node finalNodeA = resultA, finalNodeB = resultB;
		
		return new NodeList() {

			public int getLength() {
				return finalCounter;
			}

			public Node item(int index) {
				if (index == 0)
					return finalNodeA;// the first element if available
				if (index == 1)
					return finalNodeB;// the second element if available
				throw new IllegalArgumentException("only "+finalCounter+" elements in a collectin but requested element "+index);
			}};
	}
}
