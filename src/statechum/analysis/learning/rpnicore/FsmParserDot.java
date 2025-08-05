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
import statechum.LabelInputOutput;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

import java.util.Map;
import java.util.TreeMap;

import static statechum.Configuration.LABELKIND.LABEL_ATOMICPAIRS;
import static statechum.analysis.learning.rpnicore.FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND;
import static statechum.analysis.learning.rpnicore.FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0;

public class FsmParserDot<TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> {

	protected int pos;

	protected void throwException(String errMsg) {
		if (text.length() < pos)
			throw new IllegalArgumentException(errMsg + " at end of input");
		if (pos > 0)
			throw new IllegalArgumentException(errMsg + " starting from " + text.substring(pos - 1));
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
			throwException("Premature end of input");
		lastChar = text.charAt(pos++);
		return lastChar;
	}

	void unget() {
		returnLast = true;
	}

	void unget2() {
		if (pos < 2)
			throwException("cannot move back 2 chars when less than 2 were consumed");
		pos-=2;
		returnLast = false;
	}

	final AbstractLearnerGraph<TARGET_TYPE, CACHE_TYPE> graph;
	final Configuration config;
	final ConvertALabel conv;

	final boolean allowPartialAutomata;

	public enum HOW_TO_FIND_INITIAL_STATE {FIRST_FOUND,USE_START0}

	final HOW_TO_FIND_INITIAL_STATE initial_state_locator;
	/**
	 * Given a textual representation of an fsm, builds a corresponding graph. Graph name is extracted from dot graph name.
	 *
	 * @param whatToParse     the textual representation of an FSM in the DOT language (<a href="https://www.graphviz.org/doc/info/lang.html">...</a>)
	 * @param conf            configuration to use for node creation.
	 * @param converter       label converter, ignored if null.
	 * @param allowPartialAutomata whether transitions with specific string can be seen as error-transitions
	 * @param start0          When USE_FIRST_FOUND_STATE, will be using the first encountered state as an initial state. When USE_START0, will use __start0 to on a transition pointing to the initial state (and __start0 is not a state in our automaton but a placeholder).
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public FsmParserDot(String whatToParse, Configuration conf, final AbstractLearnerGraph<TARGET_TYPE, CACHE_TYPE> gr, final ConvertALabel converter,
						boolean allowPartialAutomata, HOW_TO_FIND_INITIAL_STATE start0) {
		assert conf.getTransitionMatrixImplType() != STATETREE.STATETREE_ARRAY || converter != null : "converter has to be set for an ARRAY transition matrix";
		text = whatToParse;
		pos = 0;
		graph = gr;
		config = conf;
		conv = converter;
		this.allowPartialAutomata = allowPartialAutomata;
		initial_state_locator = start0;
	}

	/**
	 * Given a textual representation of a fsm, builds a corresponding graph
	 *
	 * @param whatToParse the textual representation of an FSM in the DOT language (<a href="https://www.graphviz.org/doc/info/lang.html">...</a>)
	 * @param conf        configuration to use for node creation.
	 * @param converter   label converter, ignored if null.
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public FsmParserDot(String whatToParse, Configuration conf, final AbstractLearnerGraph<TARGET_TYPE, CACHE_TYPE> gr, final ConvertALabel converter) {
		this(whatToParse, conf,gr,converter,true, FIRST_FOUND);
	}

	public String parseQuoted() {
		StringBuilder result = new StringBuilder();
		char ch = nextChar();
		boolean escapeChar = false;
		while (ch != '"' || escapeChar) {
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
			} else {// character that was not escaped
				if (ch == '\\')
					escapeChar = true;
				else if (ch == '\n')
					throwException("string cannot contain a newline");
				else
					result.append(ch);
			}

			ch = nextChar();
		}
		return result.toString();
	}

	public static boolean isTextChar(char ch) {
		return isDigit(ch) || ch == '_' || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
	}

	public String parseText() {
		StringBuilder result = new StringBuilder();
		char ch = nextChar();
		if (isDigit(ch))
			throwException("ID cannot start with a digit");
		if (!isTextChar(ch))
			throwException("Invalid starting character for ID");
		result.append(ch);
		while (!isFinished()) {
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
			value.append('0');
			value.append('.');
			ch = nextChar();
		}

		if (!isDigit(ch))
			throwException("invalid number: should contain a digit");

		do {
			value.append(ch);

			if (isFinished())
				return value.toString();
			ch = nextChar();

			if (ch == '.') {
				if (seenDot)
					throwException("multiple dots in a number");
				seenDot = true;
				value.append(ch);
				// we should not be at the end of input here because a number is part of node name and a graph should be enclosed in curly braces thus after the last node there is should be at least '}'.
				ch = nextChar();
			} else if (!isDigit(ch) && isTextChar(ch))
				throwException("text character cannot be part of a number");
		} while (isDigit(ch));

		unget();// make sure we retrieve the character last seen.
		return value.toString();
	}

	public void skipWhitespace() {
		while (!isFinished()) {
			char ch = nextChar();
			if (ch != ' ' && ch != '\t' && ch != '\n') {
				unget();
				break;
			}
		}
	}
	public void skipSeparatorAndWhitespace() {
		boolean foundSeparator = false;
		while (!isFinished()) {
			char ch = nextChar();
			if (!foundSeparator && (ch == ';' || ch == ','))
				foundSeparator = true;
			else
				if (ch != ' ' && ch != '\t' && ch != '\n') {// if we found a character that is not a whitespace and we've already ignored a separator (if any), stop.
					unget();
					break;
				}
		}
	}
	/**
	 * Parses an ID tocken.
	 */
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

	public Map<String, String> parseOptions() {
		Map<String, String> options = new TreeMap<>();
		if (nextChar() != '[')
			throwException("options should begin with '['");

		skipWhitespace();
		while (nextChar() != ']') {
			unget();
			String key = parseID();
			skipWhitespace();
			if (nextChar() != '=')
				throwException("option " + key + " should have a value");
			String value = parseID();
			options.put(key, value);

			skipSeparatorAndWhitespace();
		}

		return options;
	}

	public String getLabel(Map<String, String> options) {
		String value = options.get("label");
		if (value == null)
			throwException("Missing label option");
		return value;
	}

	public void createVertex(String from) {
		if (null != graph.transitionMatrix.findKey(VertexID.parseID(from)))
			throwException("State " + from + " already defined");
		CmpVertex vert = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(from), config);
		graph.transitionMatrix.put(vert, graph.createNewRow());
		vert.setAccept(true);
	}

	public CmpVertex vertexForName(String name) {
		String nameOfVertexInGraph = id_to_label.get(name);
		if (null == nameOfVertexInGraph)
			throwException("State " + name + " not defined");
		if (nameOfVertexInGraph == null)
			throw new IllegalArgumentException("Graph name cannot be null");
		CmpVertex vertexFound = graph.transitionMatrix.findKey(VertexID.parseID(nameOfVertexInGraph));
		assert null != vertexFound;
		return vertexFound;
	}

	public void createTransition(String from, String to, Label label) {
		CmpVertex fromVertex = vertexForName(from);
		CmpVertex toVertex = vertexForName(to);

		graph.addTransition(graph.transitionMatrix.get(fromVertex), label, toVertex);
	}

	Map<String,String> id_to_label = new TreeMap<>();

	/** Given a current state, sets it as initial state if we have no initial state and configuration defines
	 * the first encountered state to be marked as initial state.
	 *
	 * @param currentNode current state
	 */
	protected void createInitialStateIfNeeded(String currentNode) {
		if (graph.getInit() == null && initial_state_locator == FIRST_FOUND)
			graph.setInit(vertexForName(currentNode));
	}

	public void parseGraph() {
		skipWhitespace();

		if (!"digraph".equals(parseText()))
			throwException("The graph should be labelled as directed graph");

		skipWhitespace();
		graph.setName(parseID());graph.inputsFilteredOutOnLoad.clear();
		skipWhitespace();
		if (nextChar() != '{')
			throwException("The graph description should be enclosed in curly braces");
		skipWhitespace();
		char ch = nextChar();
		unget();
		while (ch != '}') {
			String currentNode = parseID();
			skipWhitespace();
			Map<String, String> options;
			ch = nextChar();
			if (ch == '-') {// it is either an edge or another node declaration that is a negative number.
				char ch_next=nextChar();
				if (isDigit(ch_next) || ch_next == '.') {
					// we have a node declaration followed by something else. Record the current node as such.
					createVertex(currentNode);
					id_to_label.put(currentNode,currentNode);
					unget2();// this restarts parsing on the next node.
					createInitialStateIfNeeded(currentNode);
				}
				else {// we have what might be an arrow
					if (ch_next != '>')
						throwException("arrow should end with '>'");

					skipWhitespace();
					String target = parseID();
					skipWhitespace();
					options = new TreeMap<>();
					ch = nextChar();
					unget();
					if (ch == '[') {
						options = parseOptions();
					}

					if (currentNode.equals("__start0") && options.isEmpty() && initial_state_locator == USE_START0) {
						// This is the special case where __start0 is not an actual state in our automaton but a placeholder
						// such that a transition from this state leads to an initial state. This is why we remove the original
						// state (vertexForName(currentNode)) because it is only a placeholder.
						if (graph.getInit() != null)
							throwException("multiple initial states declaration");
						graph.setInit(vertexForName(target));
						graph.transitionMatrix.remove(vertexForName(currentNode));
					} else {
						String lbl = getLabel(options);// will throw exception if label is not provided in options.
						switch(config.getLabelKind()) {
							case LABEL_INPUT_OUTPUT:
							case LABEL_ATOMICPAIRS:
								LabelInputOutput label = new LabelInputOutput(lbl,allowPartialAutomata,config.getLabelKind() == LABEL_ATOMICPAIRS);
								if (!label.isErrorTransition())
									createTransition(currentNode, target, label);
								else
									graph.inputsFilteredOutOnLoad.add(label);
								break;
							default:
								createTransition(currentNode, target, AbstractLearnerGraph.generateNewLabel(lbl, config, conv));
								break;
						}
					}
				}

			} else if (ch == '[') {// this is a state declaration, process options
				unget();
				options = parseOptions();
				String lbl = getLabel(options);
				createVertex(lbl);
				id_to_label.put(currentNode,lbl);
				createInitialStateIfNeeded(currentNode);
			} else {// this is a state declaration without options
				unget();
				createVertex(currentNode);
				id_to_label.put(currentNode,currentNode);
				createInitialStateIfNeeded(currentNode);
			}

			skipSeparatorAndWhitespace();
			ch = nextChar();unget();
		}
		nextChar();// skip '}'
		skipWhitespace();
		if (!isFinished())
			throwException("Extra text at the end of graph");
	}

	/**
	 * Given a textual representation of an fsm, builds a corresponding deterministic graph
	 *
	 * @param fsm  the textual representation of an FSM
	 * @param config configuration for the automaton, including kind of labels
	 * @param conv label converter, ignored if null.
	 * @param allowPartialAutomata whether to permit partial automat to be built
	 * @param how_to_find_initial_state selects algorithm to identify an initial state
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public static LearnerGraph buildLearnerGraph(String fsm, Configuration config, final ConvertALabel conv, boolean allowPartialAutomata,HOW_TO_FIND_INITIAL_STATE how_to_find_initial_state ) {
		Configuration conf = config.copy();
		LearnerGraph graph = new LearnerGraph(conf);
		graph.initEmpty();
        new FsmParserDot<>(fsm, config, graph, conv, allowPartialAutomata, how_to_find_initial_state).parseGraph();
		return graph;
	}

	/**
	 * Given a textual representation of an fsm, builds a corresponding non-deterministic learner graph
	 *
	 * @param fsm  the textual representation of an FSM
	 * @param config configuration for the automaton, including kind of labels
	 * @param conv label converter, ignored if null.
	 * @param allowPartialAutomata whether to permit partial automat to be built
	 * @param how_to_find_initial_state selects algorithm to identify an initial state
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public static LearnerGraphND buildLearnerGraphND(String fsm, Configuration config, final ConvertALabel conv, boolean allowPartialAutomata,HOW_TO_FIND_INITIAL_STATE how_to_find_initial_state) {
		Configuration conf = config.copy();
		LearnerGraphND graph = new LearnerGraphND(conf);
		graph.initEmpty();
        new FsmParserDot<>(fsm, config, graph, conv, allowPartialAutomata, how_to_find_initial_state).parseGraph();
		return graph;
	}

}
