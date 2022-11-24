/* Copyright (c) 2022 The University of Sheffield
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

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Label;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;



public class FsmParserDot<TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> {

	protected int pos;

	protected void throwException(String errMsg) {
		if (text.length() < pos)
			throw new IllegalArgumentException(errMsg+" at end of input");
		if (pos > 0)
			throw new IllegalArgumentException(errMsg + " starting from " + text.substring(pos-1));
		throw new IllegalArgumentException(errMsg);
	}

	String text;

	boolean isFinished() {
		return pos >= text.length();
	}

	char lastChar;
	boolean returnLast = false;
	char nextChar() {
		if (returnLast) {
			returnLast = false;
			return lastChar;
		}

		if (isFinished())
			throwException("attempting to get next token");
		lastChar = text.charAt(pos++);
		return lastChar;
	}

	void unget() {
		returnLast = true;
	}

	final AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph;
	final Configuration config;
	ConvertALabel conv;

	/** Given a textual representation of an fsm, builds a corresponding graph
	 *
	 * @param whatToParse the textual representation of an FSM in the DOT language (<a href="https://www.graphviz.org/doc/info/lang.html">...</a>)
	 * @param name graph name.
	 * @param conf configuration to use for node creation.
	 * @param converter label converter, ignored if null.
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public FsmParserDot(String whatToParse, String name,Configuration conf,final AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> gr,final ConvertALabel converter)
	{
		assert conf.getTransitionMatrixImplType() != STATETREE.STATETREE_ARRAY || conv != null : "converter has to be set for an ARRAY transition matrix";
		text = whatToParse;
		pos=0;
		graph = gr;config = conf;conv = converter;
		graph.setName(name);
	}

	public String parseQuoted() {
		StringBuilder result = new StringBuilder();
		char ch = nextChar();
		boolean escapeChar = false;
		while(ch != '"' || escapeChar) {
			if (escapeChar) {
				if (ch == '\n' || ch == 'n')
					result.append('\n');
				else if (ch == '\\')
					result.append('\\');
				else if (ch == '"')
					result.append('"');
				else
					throwException("Invalid escape character");
				escapeChar = false;
			}
			else {// character that was not escaped
				if (ch == '\\')
					escapeChar = true;
				else
					if (ch == '\n')
						throwException("string cannot contain a newline");
					else
						result.append(ch);
			}

			ch = nextChar();
		}
		return result.toString();
	}

	public static boolean isTextChar(char ch) {
		return isDigit(ch) || ch == '_' || (ch >='a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
	}

	public String parseText() {
		StringBuilder result = new StringBuilder();
		char ch = nextChar();
		if (isDigit(ch))
			throwException("ID cannot start with a digit");
		if (!isTextChar(ch))
			throwException("Invalid starting character for ID");
		result.append(ch);
		while(!isFinished()) {
			ch = nextChar();
			if (isTextChar(ch))
				result.append(ch);
			else {
				unget();
				break;
			}
		}
		return result.toString();
	}

	static boolean isDigit(char ch) {
		return ch >= '0' && ch <= '9';
	}
	public String parseNumber() {
		boolean seenDot = false;
		StringBuilder value = new StringBuilder();
		char ch = nextChar();
		if (ch == '-') {
			value.append('-');
			ch = nextChar();
		}

		if (ch == '.') {
			seenDot = true;
			value.append('0');value.append('.');
			ch = nextChar();
		}

		if (!isDigit(ch))
			throwException("invalid number: should contain a digit");

		do {
			value.append(ch);

			if (isFinished())
				return value.toString();
			ch=nextChar();

			if (ch == '.') {
				if (seenDot)
					throwException("multiple dots in a number");
				seenDot = true;value.append(ch);
				if (isFinished())
					return value.toString();
				ch=nextChar();
			} else
				if (!isDigit(ch) && isTextChar(ch))
					throwException("text character cannot be part of a number");
		} while(isDigit(ch));

		unget();// make sure we retrieve the character last seen.
		return value.toString();
	}

	public void skipWhitespace() {
		while(!isFinished()) {
			char ch = nextChar();
			if (ch != ' ' && ch != '\n') {
				unget();break;
			}
		}
	}

	/** Parses an ID tocken. */
	public String parseID() {
		String result = null;
		skipWhitespace();
		char ch = nextChar();

		if (ch == '\"')
			result = parseQuoted();
		else if (ch == '-' || ch == '.' || isDigit(ch)) {
			unget();
			result = parseNumber();
		} else if (isTextChar(ch)) {
			unget();
			result = parseText();
		} else
			throwException("invalid character");

		return result;
	}

	public void parseArrow() {
		if (nextChar() != '-')
			throwException("arrow should begin with '-'");
		if (nextChar() != '>')
			throwException("arrow should end with '>'");
	}

	public Map<String,String> parseOptions() {
		Map<String,String> options=new TreeMap<>();
		if (nextChar() != '[')
			throwException("options should begin with '['");

		skipWhitespace();
		while(nextChar() != ']') {
			unget();
			String key = parseID();
			skipWhitespace();
			if (nextChar() != '=')
				throwException("option "+key+" should have a value");
			String value = parseID();
			options.put(key,value);

			skipWhitespace();
			char ch = nextChar();
			if (ch == ']')
				unget();
			else if (ch != ';' && ch != ',')
				throwException("invalid ending for the option "+key+"="+value);
			else
				skipWhitespace();
		}

		return options;
	}

	public String getLabel(Map<String,String> options) {
		String value = options.get("label");
		if (value == null)
			throwException("missing label option");
		return value;
	}

	public void createVertex(String from) {
		if (null != graph.transitionMatrix.findElementById(VertexID.parseID(from)))
			throwException("State "+from+" already defined");
		CmpVertex vert = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(from), config);
		graph.transitionMatrix.put(vert, graph.createNewRow());
		vert.setAccept(true);
	}

	public CmpVertex vertexForName(String name) {
		CmpVertex vertexFound = graph.transitionMatrix.findElementById(VertexID.parseID(name));
		if (null == vertexFound)
			throwException("State "+name+" not defined");
		return vertexFound;
	}

	public void createTransition(String from, String to, String label) {
		CmpVertex fromVertex = vertexForName(from);
		CmpVertex toVertex = vertexForName(to);
		Label lbl = AbstractLearnerGraph.generateNewLabel(label, config, conv);
		graph.addTransition(graph.transitionMatrix.get(fromVertex),lbl,toVertex);
	}


	public void parseGraph() {
		skipWhitespace();

		if (!"digraph".equals(parseText()))
			throwException("The graph should be labelled as directed graph");

		skipWhitespace();
		if (nextChar() != '{')
			throwException("The graph description should be enclosed in curly braces");
		skipWhitespace();
		char ch = nextChar();
		unget();
		while(ch != '}')
		{
			String currentNode = parseID();
			skipWhitespace();
			Map<String, String> options;
			ch = nextChar();
			if (ch == '-') {// assume an edge
				unget();
				parseArrow();
				skipWhitespace();
				String target = parseID();
				skipWhitespace();
				options = new TreeMap<>();
				ch = nextChar();
				if (ch == '[') {
					unget();
					options = parseOptions();
					skipWhitespace();
					ch = nextChar();
				}
				if (ch != ';' && ch != '\n' && ch != '}')
					throwException("invalid character at the end of transition description");
				if (currentNode.equals("__start0") && options.isEmpty()) {
					if (graph.getInit() != null)
						throwException("multiple initial state declaration");
					graph.setInit(vertexForName(target));
				}
				else
					createTransition(currentNode, target, getLabel(options));

				skipWhitespace();
			} else if (ch == '[') {
				unget();
				options = parseOptions();
				createVertex(getLabel(options));
				skipWhitespace();
				ch = nextChar();
				if (ch != ';' && ch != '\n' && ch != '}')
					throwException("invalid character at the end of node description");
			} else
				if (ch == ';' || ch == '\n' || ch == '}')
					createVertex(currentNode);
				else
					throwException("invalid character at the end of transition description");

			ch=nextChar();unget();
		}
		skipWhitespace();
		if (!isFinished())
			throwException("Extra text at the end of graph");
	}

	/** Given a textual representation of an fsm, builds a corresponding deterministic graph
	 * 
	 * @param fsm the textual representation of an FSM
	 * @param name graph name, to be displayed as the caption of the Jung window.
	 * @param conv label converter, ignored if null.
	 * @return LearnerGraph graph for it
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public static LearnerGraph buildLearnerGraph(String fsm, String name, Configuration config, final ConvertALabel conv)
	{
		LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
		new FsmParserDot<CmpVertex,LearnerGraphCachedData>(fsm,name,config,graph,conv).parseGraph();
		return graph;
	}
		
	/** Given a textual representation of an fsm, builds a corresponding non-deterministic learner graph
	 * 
	 * @param fsm the textual representation of an FSM
	 * @param name graph name, to be displayed as the caption of the Jung window.
	 * @param conv label converter, ignored if null.
	 * @return LearnerGraphND graph for it
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public static LearnerGraphND buildLearnerGraphND(String fsm, String name, Configuration config, final ConvertALabel conv)
	{
		LearnerGraphND graph = new LearnerGraphND(config);graph.initEmpty();
		new FsmParserDot<List<CmpVertex>,LearnerGraphNDCachedData>(fsm,name,config,graph,conv).parseGraph();
		return graph;
	}
}
