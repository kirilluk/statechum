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

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import statechum.Configuration;
import statechum.Helper;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.ExperimentRunner.HandleProcessIO;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;

/** This one runs LTL2BA and parses its output.
 * <p>
 * Data from LTL2BA has to arrive in the following format:
 * <pre>
never {
accept_init :
	if
	:: (!close) -> goto accept_S1
	:: (1) -> goto accept_S4
	:: (!save) -> goto accept_S2
	:: (!save && edit) -> goto accept_S3
	fi;
accept_S1 :
	if
	:: (!close) -> goto accept_S1
	:: (1) -> goto accept_S4
	fi;
accept_S4 :
	if
	:: (!save && !edit && !close) -> goto accept_S4
	:: (!save && !edit && !close && load) -> goto accept_S1
	fi;
accept_S2 :
	if
	:: (!save) -> goto accept_S2
	:: (!save && edit) -> goto accept_S3
	fi;
accept_S3 :
	if
	:: (!save) -> goto accept_S3
	:: (1) -> goto accept_S2
	fi;
}
</pre>
 *
 * Another example is below (note that "if" statements can be replaced with "skip"
 * !(((edit) R (!save)) && G(save -> X((edit) R (!save))))
<pre>
never {
T0_init :
	if
	:: (!edit) || (save) -> goto T0_S1
	:: (save) -> goto accept_all
	:: (1) -> goto T1_S2
	fi;
T0_S1 :
	if
	:: (!edit) -> goto T0_S1
	:: (save) -> goto accept_all
	fi;
T1_S2 :
	if
	:: (1) -> goto T1_S2
	:: (save) -> goto T0_S1
	fi;
accept_all :
	skip
}
</pre>
 * This automatically excludes a few words from appearing
 * as event labels.
 * 
 * @author kirill
 *
 */
public class LTL_to_ba {
	/** What is forbidden in LTL formulas - whether we're dealing with purely safety properties
	 * is checked in the process of parsing of the output of LTL2BA. 
	 */
	public static final Pattern ltlForbiddenWords;
	
	static
	{
		StringBuffer expr = new StringBuffer();
		boolean first = true;
		for(String str:new String[]{":","/\\*","\\*/"}) // this one is to ensure that parsing is regular.
		{
			if (!first) expr.append("|");else first=false;
			expr.append("(.*");expr.append(str);expr.append(".*)");
		}
		ltlForbiddenWords = Pattern.compile(expr.toString());
	}
	
	/** Refer to the description of dumpStreams in ExperimentRunner class for details on this. */
	protected static int timeBetweenHearbeats=20;

	/** Concatenates LTL and checks for forbidden words. */
	public static StringBuffer concatenateLTL(Collection<String> ltl)
	{
		StringBuffer ltlCombined = new StringBuffer();
		boolean first = true;
		for(String str:ltl)
		{
			String formula = str.trim();
			if (ltlForbiddenWords.matcher(formula).find())
				throw new IllegalArgumentException("expression "+formula+" contains a forbidden word");
			if (formula.length() > 0)
			{
				if (!first) ltlCombined.append(" || ");else first=false;
				ltlCombined.append(formula);
			}
		}
		return ltlCombined;
	}
	
	public static final String 
		baStart="never", 
		baError = "ltl2ba",baSimpleComment="\\s*/\\*.*\\*/\\n*";
	
	private final Configuration config;
	protected final LearnerGraphND matrixFromLTL;
	
	public LearnerGraphND getLTLgraph()
	{
		return matrixFromLTL;
	}
	
	/** Constructs class which will use LTL to augment the supplied graph.
	 * 
	 * @param cnf configuration to use
	 * @param from graph to fold LTL into
	 */
	public LTL_to_ba(Configuration cnf)
	{
		config = cnf;
		matrixFromLTL = new LearnerGraphND(config);
	}
		
	private static final int lexSTART =8;
	private static final int lexEND =11;
	private static final int lexFALSE =9;
	private static final int lexCOMMENT =10;
	private static final int lexIF =6;
	private static final int lexFI =7;
	private static final int lexSTATE = 4;
	private static final int lexSTATELABEL = 5;
	private static final int lexTRANSITION = 1;
	private static final int lexTRANSITIONLABEL =2;
	private static final int lexTRANSITIONTARGET =3;
	private static final int lexSKIP = 12;

	protected static class Lexer 
	{
		private String text = null;
		private Matcher lexer = null;
	
		private String lastMatch = null;
		private int lastMatchNumber = -1;
		
		/** Constructs the lexer.
		 * 
		 * @param grammar grammar to deal with
		 * @param textToAnalyse what to lexically analyse
		 */
		public Lexer(String grammar, String textToAnalyse)
		{
			text = textToAnalyse;
			lexer = Pattern.compile(grammar).matcher(text);
		}
		
		public void throwException(String errMsg)
		{
			throw new IllegalArgumentException(errMsg+" starting from \""+text.substring(lexer.regionStart())+"\"");
		}
		
		public int getMatchType()
		{
			int result = -1;
			if(lastMatchNumber >=0)
				lexer.region(lexer.end(lastMatchNumber),lexer.regionEnd());
	
			if (lexer.regionStart() < lexer.regionEnd())
			{// not run out of elements
				if (!lexer.lookingAt())
					throwException("failed to lex");
	
				int i=1;
				while(i<lexer.groupCount()+1 && lexer.group(i) == null) ++i;
				if (i == lexer.groupCount()+1)
					throwException("failed to lex (group number is out of boundary)");
		
				lastMatch = lexer.group(i);
				lastMatchNumber = i;result = i;
			}
			return result;
		}
		
		public String getMatch()
		{
			return lastMatch;
		}
		
		public String group(int i)
		{
			return lexer.group(i);
		}
	}
	
	/** Maps names of vertices to the corresponding "real" vertices. */
	private Map<String,CmpVertex> verticesUsed = new HashMap<String,CmpVertex>(); 
	
	/** Adds a state to the graph and the transition matrix.
	 * If a state already exists, it is returned.
	 *  
	 * @param name for a state
	 * @return the vertex corresponding to it. 
	 */
	private CmpVertex addState(String name)
	{
		CmpVertex vert = verticesUsed.get(name);

		if (vert == null)
		{// add new vertex
			vert = AbstractLearnerGraph.generateNewCmpVertex(new VertexID(name), config);
			vert.setAccept(name.startsWith("accept_"));vert.setColour(LearnerGraphND.ltlColour);
			matrixFromLTL.transitionMatrix.put(vert,matrixFromLTL.createNewRow());
			verticesUsed.put(name, vert);
		}
		return vert;
	}
	
	/** The name of the initial state given by ltl2ba. */
	public static final String initStateName = "init";
	
	/** Parses the output of LTL2BA, with the aim to extract a buchi automaton.
	 * Throws an {@link IllegalArgumentException} if something goes wrong.
	 * Non-deterministic automata extracted from LTL is stored in the <em>matrix</em> array.
	 *  
	 * @param output
	 */
	public void parse(String whatToParse)
	{
		Lexer lexer = new Lexer(
			"(\\s*::\\s*([^\\-]+)\\s+\\->\\s*goto\\s+(\\w+)\\s+)|"+// if conditional
				"(\\s*(\\w+)\\s*:\\s)"+"|"+// state name
				"(\\s*if\\s*)"+"|"+ // if opening statement, 6
				"(\\s*fi\\s*;\\s*)"+"|"+ // if closing statement, 7
				"("+baStart+"\\s*\\{\\s*)"+"|"+ // start, 8
				"(\\s*false\\s*;\\s+)"+"|"+ // false statement, 9
				"("+baSimpleComment+")"+"|"+ // comment, 10
				"(\\s*\\}\\s*)"+"|"+ // end of the claim, 11
				"(\\s*skip\\s*)" // skip statement, 12
				,whatToParse+"\n");
		int currentMatch = lexer.getMatchType();
		if (currentMatch == lexSTATE && baError.equals(lexer.group(lexSTATELABEL)))
			lexer.throwException("syntax error reported by ltl2ba");
		if (currentMatch != lexSTART)
			 lexer.throwException("failed to find the start of automaton");
		currentMatch = lexer.getMatchType();
		int state = lexSTART;
		CmpVertex currentState = null;
		while(currentMatch>=0)
		{
			if (currentMatch != lexCOMMENT)
			switch(state)
			{
			case lexSTART:
				switch(currentMatch)
				{
				case lexSTATE:
					state = lexIF;
					currentState=addState(lexer.group(lexSTATELABEL));break;
				default:
					lexer.throwException("unexpected token type "+currentMatch);
				}
				break;
			case lexEND:
				if (currentMatch != lexEND)
					lexer.throwException("expected end "+currentMatch);
				state = -1;// we should not get any more tokens
				break;
			case lexSTATE:
				switch(currentMatch)
				{
				case lexSTATE:
					state = lexIF;
					currentState=addState(lexer.group(lexSTATELABEL));break;
				case lexEND:
					state = -1;break;// we should not get any more tokens
				default:
					lexer.throwException("unexpected lexSTATE token type "+currentMatch);
				}
				break;
			case lexIF:
				switch(currentMatch)
				{
				case lexFALSE:
					state = lexSTATE;// expect next if
					break;
				case lexSKIP:
					addTransitionsBetweenStates(currentState,"1",currentState);
					state = lexSTATE;
					break;
				case lexIF:
					state = lexTRANSITION;break;
				default:
					lexer.throwException("expected if, skip or false"+currentMatch);
				}
				break;
			case lexTRANSITION:
				switch(currentMatch)
				{
				case lexTRANSITION:
					addTransitionsBetweenStates(currentState,lexer.group(lexTRANSITIONLABEL),addState(lexer.group(lexTRANSITIONTARGET)));
					break;
				case lexFI:
						state = lexSTATE;break;
				default:
					lexer.throwException("unexpected lexTRANSITION token type "+currentMatch);
				}
				break;
			default:
				lexer.throwException("unexpected state "+state);
			}
				
			currentMatch = lexer.getMatchType();
		}
		
		matrixFromLTL.findInitialState(initStateName);
	}
	
	/** Takes a SPIN transition label and a target state and adds transitions from the given state to the target state,
	 * interpreting the expression on the label.
	 * 
	 * @param currentState the current state
	 * @param transitionLabel the SPIN label
	 * @param targetState target state
	 */
	protected void addTransitionsBetweenStates(CmpVertex currentState, String transitionLabel, CmpVertex targetState)
	{
		Map<String,List<CmpVertex>> row = matrixFromLTL.transitionMatrix.get(currentState);
		for(String currLabel:interpretString(transitionLabel))
		{
			List<CmpVertex> targetList = row.get(currLabel);
			if (targetList == null)
			{
				targetList = new LinkedList<CmpVertex>();row.put(currLabel, targetList);
			}
			targetList.add(targetState);
		}
	}
	
	/** Alphabet of a graph we'd like to augment with LTL. */
	protected Set<String> alphabet = null;
	
	enum OPERATION { AND,OR,NEG,ASSIGN }
	
	public static final int exprOpen = 1;
	public static final int exprClose =2;
	public static final int exprAND = 3;
	public static final int exprOR = 4;
	public static final int exprNEG = 5;
	public static final int exprWord = 6;
	public static final int exprWordText = 7;
	
	/** Lexical analyser for embedded context-free expressions. */
	protected Lexer lexExpr = null;

	protected void buildExprLexer(String data)
	{
		lexExpr = new Lexer("(\\s*\\(\\s*)|(\\s*\\)\\s*)|(\\s*&&\\s*)|(\\s*\\|\\|\\s*)|(\\s*!\\s*)|(\\s*(\\w+)\\s*)",data);
	}
	
	protected Set<String> interpretString(String data)
	{
		buildExprLexer(data+")");
		Set<String> result = interpretExpression();
		if (lexExpr.getMatchType() >=0)
			throw new IllegalArgumentException("extra tokens at the end of expression");
		return result;
	}
	
	/** Given an expression with brackets, && and ||, this one interprets it as a set of labels. */
	protected Set<String> interpretExpression()
	{
		int currentMatch = lexExpr.getMatchType();
		if (currentMatch < 0 || currentMatch == exprClose)
			throw new IllegalArgumentException("unexpected end of expression");
		
		boolean expectWord = true;// this means that we are waiting for a word
		
		Set<String> currentValue = new TreeSet<String>();// the outcome of the left-hand side.

		OPERATION currentOperation = OPERATION.ASSIGN;
		while(currentMatch >= 0 && currentMatch != exprClose)
		{
			switch(currentMatch)
			{
			case exprOpen: // embedded expression
				if (!expectWord)
					throw new IllegalArgumentException("expected binary operation instead of "+lexExpr.getMatch());
				performOperation(currentValue, currentOperation, interpretExpression());
				expectWord = false;
				break;
			case exprAND:
				if (expectWord)
					throw new IllegalArgumentException("expected word instead of "+lexExpr.getMatch());
				currentOperation = OPERATION.AND;expectWord = true;
				break;
			case exprOR:
				if (expectWord)
					throw new IllegalArgumentException("expected word instead of "+lexExpr.getMatch());
				currentOperation = OPERATION.OR;expectWord = true;
				break;
			case exprNEG:
				if (!expectWord)
					throw new IllegalArgumentException("expected binary operation instead of "+lexExpr.getMatch());
				Set<String> tmp = new TreeSet<String>();performOperation(tmp,OPERATION.NEG,interpretUnary());
				performOperation(currentValue, currentOperation, tmp);
				currentOperation = OPERATION.ASSIGN;expectWord = false;
				break;
			case exprWord:
				if (!expectWord)
					throw new IllegalArgumentException("expected binary operation instead of "+lexExpr.getMatch());
				performOperation(currentValue, currentOperation, interpretInputLabel(lexExpr.group(exprWordText)));
				currentOperation = OPERATION.ASSIGN;expectWord = false;
				break;
			default:
				throw new IllegalArgumentException("invalid token "+currentMatch+", looking at "+lexExpr.getMatch());
			}
			currentMatch = lexExpr.getMatchType();
		}
		return currentValue;
	}
	
	protected Set<String> interpretUnary()
	{
		int currentMatch = lexExpr.getMatchType();
		Set<String> currentValue = new TreeSet<String>();// the outcome of the left-hand side.
		switch(currentMatch)
		{
		case exprOpen: // expression in braces
			performOperation(currentValue, OPERATION.ASSIGN, interpretExpression());
			return currentValue;
		case exprNEG:
			performOperation(currentValue, OPERATION.NEG, interpretUnary());
			break;
		case exprWord:
			performOperation(currentValue, OPERATION.ASSIGN, interpretInputLabel(lexExpr.group(exprWordText)));
			break;
		default:
			throw new IllegalArgumentException("invalid token "+currentMatch+", looking at "+lexExpr.getMatch());
		}
		return currentValue;
	}
	
	/** The notation for each label in a BA is one of the following:
	 * "label", "1".
	 * 
	 * @param left the left-hand side and the receiver of the outcome of the operation.
	 * @param oper the operation to perform between the supplied sets,
	 * <em>null</em> means assignment of the right-hand side to the left-hand. 
	 * @param right the right-hand side
	 */
	protected void performOperation(Set<String> left, OPERATION oper, Set<String> right)
	{
		switch(oper)
		{
		case ASSIGN:
			left.clear();left.addAll(right);
			break;
		case NEG:
			left.clear();left.addAll(alphabet);left.removeAll(right);
			break;
		case AND:
			left.retainAll(right);break;
		case OR:
			left.addAll(right);break;
		}
	}
	
	/** The notation for each label in a BA is one of the following:
	 * "label", "1".
	 * 
	 * @param label label to interpret, using an alphabet.
	 * @return result of interpretation.
	 * 
	 */
	protected Set<String> interpretInputLabel(String label)
	{
		if (label.length() == 0)
			throw new IllegalArgumentException("empty label");
		
		Set<String> result = new TreeSet<String>();
		
		if (label.equals("1")) 
			result.addAll(alphabet);
		else
		{
			if (!alphabet.contains(label))
				throw new IllegalArgumentException("unrecognised label "+label);
			result.add(label);
		}
		return result;
	}
	
	/** Runs a supplied ltl formula through ltl2ba.
	 * The graph returned by ltl2ba is stored in the internal matrix. 
	 *
	 * @param ltl formula to run
	 */
	protected void runLTL2BA(String ltl)
	{
		final StringBuffer converterOutput = new StringBuffer();
		try 
		{
			final Process ltlconverter = Runtime.getRuntime().exec(new String[]{"ltl2ba", "-f",ltl});// run LTL2BA
			ExperimentRunner.dumpStreams(ltlconverter,timeBetweenHearbeats,new HandleProcessIO() {

			public void OnHeartBeat() {// no prodding is done for a short-running converter.
			}

			public void StdErr(StringBuffer b) {
				System.err.print(b.toString());
			}

			public void StdOut(StringBuffer b) {
				converterOutput.append(b);
			}});
			ltlconverter.waitFor();
		} catch (IOException e1) {
			statechum.Helper.throwUnchecked("failed to run ltl2ba", e1);
		} catch (InterruptedException e) {
			statechum.Helper.throwUnchecked("wait for ltl2ba to terminate aborted", e);
		}
		
		parse(converterOutput.toString());
	}

	/** Takes a collection of LTL formulae and builds the corresponding FSM,
	 * assuming the properties are all safety ones.
	 * 
	 * @param ltl formulas to run
	 * @param graph in order to correctly interpret symbols used by ltl2ba 
	 * such as "1", we need to be aware of the alphabet of an FSM being built. 
	 * This information is extracted from the supplied graph.
	 * @throws IncompatibleStatesException 
	 */
	public void ltlToBA(Collection<String> ltl, LearnerGraph graph)
	{
		if (graph != null)
			alphabet = graph.pathroutines.computeAlphabet();
		runLTL2BA(concatenateLTL(ltl).toString());
		for(CmpVertex v:matrixFromLTL.transitionMatrix.keySet())
			if (!v.isAccept())
				throw new IllegalArgumentException("not all states are accept-states");
	}
	
}
