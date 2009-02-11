/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
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

package statechum.analysis.learning.rpnicore;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

import statechum.DeterministicDirectedSparseGraph;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.DeterministicEdge;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph.PairCompatibility;


public class FsmParser 
{
	public static final int LABEL=0;
	public static final int LARROW=1;
	public static final int RARROW=2;
	public static final int LARROWREJ=3;
	public static final int RARROWREJ=4;
	public static final int DASH =5;
	public static final int EQUIV = 6;
	public static final int NEWL =7;
	
	private final String text;
	private final Matcher lexer;

	public FsmParser(String whatToParse)
	{
		text = "\n"+whatToParse;
		lexer = Pattern.compile("([^\n #\\055<>\\|=]+)|( *<\\055+ *)|( *\\055+> *)|( *#\\055+ *)|( *\\055+# *)|( *\\055+ *)|( *=+ *)|( *[\n\\|] *)").matcher(text);
	}
	
	protected boolean isFinished()
	{
		return lexer.regionStart() == lexer.regionEnd();
	}
	
	private String lastMatch = null;
	
	protected void throwException(String errMsg)
	{
		throw new IllegalArgumentException(errMsg+" starting from "+text.substring(lexer.regionStart()));
	}
	
	protected int getNextMatch()
	{
		if (!lexer.lookingAt())
			throwException("failed to lex");
		
		int i=1;
		for(;i<lexer.groupCount()+1 && lexer.group(i) == null;++i);
		if (i == lexer.groupCount()+1)
			throwException("failed to lex (group number is out of boundary)");

		lastMatch = lexer.group(i);
		lexer.region(lexer.end(i),lexer.regionEnd());
		return i-1;// to bring it to 0..max from 1..max+1
	}
	
	protected String getMatch()
	{
		return lastMatch;
	}
	
	public void parse(TransitionReceiver receiver)
	{
		String currentState = null;
		do {					
			int left = getNextMatch();// this is expected to be a label of the left state; 
				// we start with a newline hence the first thing that happens is a fallthrough 
				// to a label of the first state.
			if (left == NEWL)
			{
				while(left == NEWL && !isFinished())
					left = getNextMatch();
				if (left == NEWL && isFinished())
					break;// finished parsing
				if (left != FsmParser.LABEL)
					throwException("state name expected");// there should be a state name after a newline
				currentState = getMatch();
				left=getNextMatch();
			}
			
			if (left != FsmParser.LARROW && left != FsmParser.LARROWREJ && left != FsmParser.DASH && left != FsmParser.EQUIV)
				throwException("a left arrow or a dash expected here");
			
			if (getNextMatch() != FsmParser.LABEL)
				throwException("label expected");
			String label = getMatch();
			JUConstants.PAIRCOMPATIBILITY pairRelation = null;
			int right = getNextMatch();
			if (left == FsmParser.LARROW || left == FsmParser.LARROWREJ)
			{
				if (right != FsmParser.DASH)
					throwException("a dash was expected here");
			}
			else
				if (left == FsmParser.DASH)
				{
					if (right != FsmParser.RARROW && right != FsmParser.RARROWREJ)
						throwException("a right-arrow was expected here");
				}
				else
				{
					assert left == FsmParser.EQUIV;
					if (right != FsmParser.EQUIV)
						throwException("equiv on the left-hand side should be accompanied by on on the right");
					pairRelation = JUConstants.PAIRCOMPATIBILITY.valueOf(label);
				}
			
			if (getNextMatch() != FsmParser.LABEL)
				throwException("state name expected");
			String anotherState = getMatch();
			
			if (left == FsmParser.LARROW)
				receiver.accept(anotherState, currentState, label);
			else
				if (left == FsmParser.LARROWREJ)
					receiver.reject(anotherState, currentState, label);
				else
					if (left == FsmParser.DASH)
					{
						if (right == FsmParser.RARROW)
							receiver.accept(currentState, anotherState, label);
						else
							receiver.reject(currentState, anotherState, label);
					}
					else // left == FsmParser.EQUIV
						{
							assert left == FsmParser.EQUIV;
							receiver.pairCompatibility(currentState,pairRelation,anotherState);
						}
			currentState = anotherState;
		} while(!isFinished());
		
	}


	/** Given a textual representation of an fsm, builds a corresponding Jung graph
	 * 
	 * @param fsm the textual representation of an FSM
	 * @param name graph name, to be displayed as the caption of the Jung window.
	 * @return Jung graph for it
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public final static DirectedSparseGraph buildGraph(String fsm,String name)
	{
		final Map<String,DeterministicVertex> existingVertices = new HashMap<String,DeterministicVertex>();
		final Map<StatePair,DeterministicEdge> existingEdges = new HashMap<StatePair,DeterministicEdge>();
		
		final DirectedSparseGraph g = new DirectedSparseGraph();
		g.setUserDatum(JUConstants.TITLE, name,UserData.SHARED);

		new FsmParser(fsm).parse(new TransitionReceiver()
		{
			public void put(String from, String to, String label, boolean accept) {
				DeterministicVertex fromVertex = existingVertices.get(from), toVertex = existingVertices.get(to);
				
				if (fromVertex == null)
				{
					fromVertex = new DeterministicDirectedSparseGraph.DeterministicVertex(from);
					if (existingVertices.isEmpty())
						fromVertex.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
					fromVertex.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
					existingVertices.put(from, fromVertex);
					g.addVertex(fromVertex);
				}
				else
					if (!Boolean.valueOf(fromVertex.getUserDatum(JUConstants.ACCEPTED).toString()))
						throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+from);

				if (from.equals(to))
				{
					if (!accept) throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+to);
					toVertex = fromVertex;
				}
				else
					if (toVertex == null)
					{
						toVertex = new DeterministicDirectedSparseGraph.DeterministicVertex(to);
						toVertex.removeUserDatum(JUConstants.ACCEPTED); // in case we've got a reject loop in the same state
						toVertex.addUserDatum(JUConstants.ACCEPTED, accept, UserData.SHARED);
						existingVertices.put(to, toVertex);
						g.addVertex(toVertex);
					}
					else
						if (DeterministicDirectedSparseGraph.isAccept(toVertex) != accept)
							throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+to);
				
				StatePair pair = new StatePair(fromVertex,toVertex);
				DeterministicEdge edge = existingEdges.get(pair);
				if (edge == null)
				{
					edge = new DeterministicDirectedSparseGraph.DeterministicEdge(fromVertex,toVertex);
					edge.addUserDatum(JUConstants.LABEL, new HashSet<String>(), UserData.CLONE);
					g.addEdge(edge);existingEdges.put(pair,edge);
				}
				
				Set<String> labels = (Set<String>)edge.getUserDatum(JUConstants.LABEL);
				labels.add(label);
			}

			public void accept(String from, String to, String label) {
				put(from,to,label,true);
			}
			public void reject(String from, String to, String label) {
				put(from,to,label,false);
			}

			public void pairCompatibility(String stateA, PAIRCOMPATIBILITY pairRelation, String stateB) {
				PairCompatibility<Vertex> pairCompatibility = (PairCompatibility<Vertex>)g.getUserDatum(JUConstants.PAIR_COMPATIBILITY);
				if (pairCompatibility == null)
				{
					pairCompatibility = new PairCompatibility<Vertex>();
					g.addUserDatum(JUConstants.PAIR_COMPATIBILITY, pairCompatibility, UserData.SHARED);
				}
				if (!existingVertices.containsKey(stateA))
					throw new IllegalArgumentException("unknown vertex "+stateA);
				if (!existingVertices.containsKey(stateB))
					throw new IllegalArgumentException("unknown vertex "+stateB);
				pairCompatibility.addToCompatibility(existingVertices.get(stateA), existingVertices.get(stateB), pairRelation);
				
			}
		});

		if (GlobalConfiguration.getConfiguration().isGraphTransformationDebug(g))
		{
			Visualiser.updateFrame(g, null);System.out.println("******** PROCESSING "+name+" **********\n");
		}
		return g;
	}
}