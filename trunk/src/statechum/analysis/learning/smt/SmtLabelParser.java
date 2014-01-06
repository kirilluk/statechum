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
package statechum.analysis.learning.smt;

import java.util.LinkedList;
import java.util.List;

import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;
import statechum.analysis.learning.smt.SmtLabelRepresentation.CompositionOfFunctions;

public class SmtLabelParser {
	public static final int exprOpen = 1;
	public static final int exprClose =2;
	public static final int exprWord = 3;
	public static final int exprWordText = 4;
	public static final int exprENDL = 5;
	
	/** Lexical analyser for embedded context-free expressions. */
	protected Lexer lexExpr = null;

	List<String> operations = new LinkedList<String>();
	List<CompositionOfFunctions> arguments = new LinkedList<CompositionOfFunctions>();
	
	private void buildLexer(String data)
	{
		lexExpr = new Lexer("(\\s*\\(\\s*)|(\\s*\\)\\s*)|(\\s*([^\n\\(\\)\\s]+)\\s*)|(\\s*\n\\s*)",data);
	}
	
	/** Given a composite trace, this method converts it into a sequence of operations
	 * and the corresponding arguments.   
	 */
	public void interpretTrace(String data,FunctionArgumentsHandler functionArgumentsHandler)
	{
		buildLexer(data);
		interpretLabelExpression(functionArgumentsHandler);
	}
	
	/** Given an expression representing a function, this method pulls out functions of interest
	 * and associates a fresh variable with each parameter and with the outcome of invoking a
	 * function.
	 * 
	 * @param function function to process
	 * @return the function with all the additions.
	 */
	public CompositionOfFunctions interpretPrePostCondition(String data,FunctionArgumentsHandler functionArgumentsHandler)
	{
		if (data == null) return CompositionOfFunctions.createEmptySecondPhase();// returns the second phase CompositionOfFunctions here.
		buildLexer(data);
		CompositionOfFunctions result = functionArgumentsHandler.getComposition(interpretFunctionalExpression(functionArgumentsHandler,true));
		if (lexExpr.getMatch() != null) // if we did not stop parsing the expression due to the end of text
			throw new IllegalArgumentException("extra text at the end of expression "+lexExpr.getText());
		return result;
	}
	
	
	/** Given an expression with brackets interprets it as label(args). 
	 * Calls the supplied callback
	 * with every label or function identified and the arguments.
	 * 
	 * @param parseLabel
	 */
	protected void interpretLabelExpression(FunctionArgumentsHandler functionArgumentsHandler)
	{
		int currentMatch = lexExpr.getMatchType();
		if (currentMatch < 0)
			throw new IllegalArgumentException("unexpected end of expression");
		
		String lastLabel = null;
		CompositionOfFunctions lastArgs = null;
		boolean argumentsAlreadyParsed = false;
		while(currentMatch >= 0)
		{
			switch(currentMatch)
			{
			case exprOpen: // embedded expression
				if (argumentsAlreadyParsed)
					throw new IllegalArgumentException("multiple groups of arguments for label "+lastLabel);
				if (lastLabel == null)
					throw new IllegalArgumentException("arguments without a label");
				lastArgs=functionArgumentsHandler.getComposition(interpretFunctionalExpression(functionArgumentsHandler,true));
				if (lexExpr.getMatch() == null) // if we stopped parsing the expression due to the end of text
					throw new IllegalArgumentException("unexpected end of input in "+lexExpr.getText());
				argumentsAlreadyParsed = true;
				break;
			case exprWord:
				if (lastLabel != null)
				{// record what we've got so far
					operations.add(lastLabel);arguments.add(lastArgs==null?
							CompositionOfFunctions.createEmptySecondPhase():lastArgs);
						// if there are no args to a function, we have to create an empty composition; interpretLabelExpression
						// is always called during construction of the second version of CompositionOfFunctions,
						// hence an empty one has to be built using createEmptySecondPhase.
				}
				lastLabel = lexExpr.group(exprWordText);
				lastArgs = null;argumentsAlreadyParsed = false;
				break;
			case exprClose:
				throw new IllegalArgumentException("unexpected closing brace when parsing "+lexExpr.getText());
			default:
				throw new IllegalArgumentException("invalid token "+currentMatch+", looking at "+lexExpr.getMatch());
			}
			currentMatch = lexExpr.getMatchType();
		}
		
		if (lastLabel != null)
		{// record the last label
			operations.add(lastLabel);arguments.add(lastArgs==null?
					CompositionOfFunctions.createEmptySecondPhase():lastArgs);
		}
	}
	
	
	public interface FunctionArgumentsHandler
	{
		/** Called for each detected function.
		 * 
		 * @param functionName the name of the function
		 * @param args arguments of this function.
		 * @return if non-null, the name of a variable to represent the result of computing this function;
		 * null return value means that function and its arguments should be passed unchanged through interpretFunctionalExpression.
		 */
		public String HandleLowLevelFunction(String functionName, List<String> args);
		
		/** Given the text of the term built by calling <em>HandleLowLevelFunction</em>, 
		 * returns the outcome of parsing.
		 */
		public CompositionOfFunctions getComposition(String text);

		/** Whenever we start processing next label or a completely different term, this object needs to be reset. */
		public void reset();
	}
	
	/** Given an expression with brackets interprets it as (func args). 
	 * If passed a non-null maps, populates it with arguments to functions of interest.
	 * <p>
	 * We also need to associate each function of interest with traces utilising their
	 * arguments.
	 * 
	 * @param functionArgumentsHandler the callback used to process arguments of functions
	 * @param topLevel if true, we are interpreting a top-level expression where there is no function name 
	 * and only arguments are present.
	 */
	protected String interpretFunctionalExpression(FunctionArgumentsHandler functionArgumentsHandler, boolean topLevel)
	{
		int currentMatch = lexExpr.getMatchType();
		if (currentMatch < 0)
			throw new IllegalArgumentException("unexpected end of expression when parsing "+lexExpr.getText());
	
		if (topLevel) functionArgumentsHandler.reset();
		boolean expectFunction = !topLevel;// this means that we are waiting for a function name in (funcName arg1 arg2 ... ).
		String function = null;
		List<String> args = new LinkedList<String>(); 
		while(currentMatch >= 0 && currentMatch != exprClose)
		{
			switch(currentMatch)
			{
			case exprOpen: // embedded expression
				if (expectFunction)
					throw new IllegalArgumentException("missing name of a function");
				// expression has the form of  ... ( nested expr ) ... 
				args.add(interpretFunctionalExpression(functionArgumentsHandler,false));
				break;
			case exprWord:
				if (expectFunction)
				{
					function = lexExpr.group(exprWordText);expectFunction = false;
					if (function.startsWith(SmtLabelRepresentation.functionArg) || function.indexOf(SmtLabelRepresentation.delimiter) != -1)
						throw new IllegalArgumentException("invalid function name "+function);
				}
				else
				{// literal term rather than expression
					String argString = lexExpr.group(exprWordText);
					if (argString.startsWith(SmtLabelRepresentation.functionArg))
						throw new IllegalArgumentException("invalid argument name "+argString);
					args.add(argString);
				}
				break;
			case exprENDL:
				break;// ignore this.
			default:
				throw new IllegalArgumentException("invalid token "+currentMatch+", looking at "+lexExpr.getMatch());
			}
			currentMatch = lexExpr.getMatchType();
		}
		if (!topLevel && currentMatch < 0)
			throw new IllegalArgumentException("unexpected end of input when parsing "+lexExpr.getText());
		if (!topLevel && function == null)
			throw new IllegalArgumentException("missing function name when parsing "+lexExpr.getText());
		StringBuffer result = new StringBuffer();
		String outcome = null;
		if (!topLevel) outcome = functionArgumentsHandler.HandleLowLevelFunction(function, args);
		if (outcome == null)
		{
			boolean first = false;if (!topLevel) result.append('(');
			if (function != null) result.append(function);else first = true;
			for(String arg:args) { if (first) first=false;else result.append(' ');result.append(arg); }
			if (!topLevel) result.append(')');
		}
		else
			result.append(outcome);
		
		return result.toString();
	}
	
}
