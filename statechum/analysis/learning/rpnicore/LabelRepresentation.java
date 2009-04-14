/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 *
 * This file is part of StateChum.
 * 
 * statechum is free software: you can redistribute it and/or modify
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
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.Pair;
import statechum.StatechumXML;
import statechum.Configuration.SMTGRAPHDOMAINCONSISTENCYCHECK;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.LabelParser.FunctionArgumentsHandler;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.Smt;
import statechum.apps.QSMTool;

/** Transition labels may have complex behaviours associated with them and
 * SMT can be used to check
 * 
 * @author kirill
 *
 */
public class LabelRepresentation 
{
	/** Constraints declaration of memory and input variables, together with constraints
	 * on input variables to ensure that design-for-test conditions are met are stored as a precondition of MEM0
	 * A postcondition of MEM0 describes the initial memory value.
	 */
	public static final String INITMEM="MEM0";
	
	protected Label init = null;

	/** If true, this means that all abstract states corresponding to accept-states will be included in the 
	 * Yices context.
	 */
	protected boolean usingLowLevelFunctions = false;
	
	/** This collection contains names and kinds of all variables. Used to check that preconditions
	 * do not refer to output variables or new values of memory. Such a constraint is necessary
	 * in order to check for determinism correctly (we are limiting arguments of operations to
	 * old values of inputs/outputs; if preconditions refer to outputs, our
	 * check is limited to outcomes where outputs are the same as those we've seen, hence we are
	 * only looking at a subset of possible behaviours and may miss non-determinism if it occurs in 
	 * a wider context). 
	 */
	protected Map<String,VARTYPE> variables = new HashMap<String,VARTYPE>();
	
	public enum VARTYPE { VAR_INPUT, VAR_OUTPUT, VAR_MEMORY };
	/** Eliminates spaces at the beginning and end of the supplied 
	 * string. If not empty, the outcome is appended to the buffer provided. 
	 * This is needed because a in number 
	 * of cases we cannot simply include an empty string into Yices expression - 
	 * we have to know that the sting is empty and put "true".
	 * For this reason, empty values are designated with nulls and this method makes it possible to maintain them
	 * until a non-empty string is appended. Usage:
	 * <pre>
	 * String text = null;
	 * ...
	 * text = appendToString(string,text);
	 * </pre>
	 * 
	 * @param str what to append
	 * @param where to append
	 * @param result, which could be null 
	 */
	public static String appendToString(String str,String buffer)
	{
		String result = buffer;
		if (str == null) return result; // nothing to do
		String trimmed = str.trim();if (trimmed.length() == 0) return result;// nothing to do
		
		if (buffer == null) result = trimmed;else { result= result+ENDL+trimmed; }
		return result;
	}

	/** Represents a function used by preconditions, postconditions and i/o. */
	public static class LowLevelFunction implements Comparable<LowLevelFunction>
	{
		protected String name;
		
		/** This is a constraint used to limit the range of arguments and the return value of this function.
		 * For example,
		 * <pre>
		 * (and (< frg0 4) (= frg2 (+ frg1 frg0)))
		 * </pre>
		 * where expressions <em>frg0</em> is the return value of this function and
		 * <em>frg<b>i</b></em> (where <em><b>i</b></em> is in the range of <em>1..arity</em>) refer 
		 * to the arguments of this function.
		 */
		protected String constraint= null;
		
		/** Variable declarations for the arguments of this function - for uninterpreted functions
		 * this is not necessary but we have to introduce variables for them, hence the type has 
		 * to be provided to Yices.
		 */
		protected String varDeclaration = null;
		
		protected int arity = 0;
		
		/** Whether arguments to this function should be constrained to terms used in existing traces. */ 
		protected boolean constrainArgsToTraces = false;

		public LowLevelFunction(String functionName)
		{
			name = functionName;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result
					+ ((constraint == null) ? 0 : constraint.hashCode());
			result = prime * result + ((name == null) ? 0 : name.hashCode());
			result = prime
					* result
					+ ((varDeclaration == null) ? 0 : varDeclaration.hashCode());
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
			if (!(obj instanceof LowLevelFunction))
				return false;
			LowLevelFunction other = (LowLevelFunction) obj;
			if (constraint == null) {
				if (other.constraint != null)
					return false;
			} else if (!constraint.equals(other.constraint))
				return false;
			if (name == null) {
				if (other.name != null)
					return false;
			} else if (!name.equals(other.name))
				return false;
			if (varDeclaration == null) {
				if (other.varDeclaration != null)
					return false;
			} else if (!varDeclaration.equals(other.varDeclaration))
				return false;
			return true;
		}

		public String getName() {
			return name;
		}

		public int compareTo(LowLevelFunction o) {
			return name.compareTo(o.name);
		}
	}

	public static final String blockVARS="VARS",blockVARDECLS="VAR DECL",blockVALUES="POSSIBLE VALUES",blockDATATRACES="KNOWN DATA TRACES"; 
	
	/** Given a block of text, surrounds it in begin-end if it is not empty.
	 * What is important is that in case anything is appended, it is separated from the text
	 * before with a new line.
	 * 
	 * @param block text to surround
	 * @param title the title to give to the surrounding text
	 * @return surrounded text
	 */
	public static String encloseInBeginEndIfNotEmpty(String block,String title)
	{
		if (block == null || block.length() == 0) return "";
		
		return ENDL+";; BEGIN "+title+ENDL+block+ENDL+";; END "+title+ENDL;
	}

	/** The purpose of this class is to record the results of parsing preconditions, postconditions and i/o. 
	 * It is constructed following three steps,
	 * <ol>
	 * <li><em>text</em> is built by parsing preconditions, postconditions and io.</li>
	 * <li>After parsing <em>text</em>, it is rebuilt and appropriate functions are substituted using
	 * new variables.</li>
	 * <li>After traces are all added to an initial PTA, the variables used in those traces 
	 * (with substituted _M and _N) are added to this composition.</li>
	 * </ol> 
	 */
	public static class CompositionOfFunctions
	{
		protected final String text,relabelledText,finalText;
		protected final String varDeclarations;
		protected final Map<LowLevelFunction,Collection<String>> variablesUsedForArgs;
		
		/** This constructor is only used in order to build the first version of this composition.
		 */
		public CompositionOfFunctions(String argText)
		{
			text = argText;varDeclarations=null;relabelledText=null;finalText=null;variablesUsedForArgs = null;
		}
		
		/** This constructor is only used in order to build the second version of this composition.
		 */
		public CompositionOfFunctions(String data, String decls, String vars, Map<LowLevelFunction,Collection<String>> argVariablesUsedForArgs)
		{
			assert data != null;
			text = data;varDeclarations = encloseInBeginEndIfNotEmpty(decls,blockVARDECLS);relabelledText = text+encloseInBeginEndIfNotEmpty(vars,blockVARS);variablesUsedForArgs = argVariablesUsedForArgs;
			finalText = null;
		}

		/** This constructor is only used in order to build the third version of this composition.
		 */
		public CompositionOfFunctions(CompositionOfFunctions composition,String whatToAdd)
		{// both the original and relabelled text is preserved, not least in order to rebuild compositions when hard facts are changed when sequences are added to it.
			text = composition.text;relabelledText = composition.relabelledText;varDeclarations = composition.varDeclarations;variablesUsedForArgs = null;
			finalText = composition.relabelledText+encloseInBeginEndIfNotEmpty(whatToAdd,blockVALUES);
			assert varDeclarations != null;
			assert finalText != null;
		}
		
		public String getCondition() 
		{
			if (finalText == null)
				return relabelledText;
			return finalText;
		}

		@Override
		public String toString()
		{
			String result = getCondition();
			if (result == null)
				result = text;
			return result;
		}
		
		/** Constructs an instance of an empty second phase of this composition. */
		public static CompositionOfFunctions createEmptySecondPhase() {
			return new CompositionOfFunctions("","","",new TreeMap<LowLevelFunction,Collection<String>>());
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result
					+ ((finalText == null) ? 0 : finalText.hashCode());
			result = prime
					* result
					+ ((relabelledText == null) ? 0 : relabelledText.hashCode());
			result = prime * result + ((text == null) ? 0 : text.hashCode());
			result = prime
					* result
					+ ((varDeclarations == null) ? 0 : varDeclarations
							.hashCode());
			result = prime
					* result
					+ ((variablesUsedForArgs == null) ? 0
							: variablesUsedForArgs.hashCode());
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
			if (!(obj instanceof CompositionOfFunctions))
				return false;
			CompositionOfFunctions other = (CompositionOfFunctions) obj;
			if (finalText == null) {
				if (other.finalText != null)
					return false;
			} else if (!finalText.equals(other.finalText))
				return false;
			if (relabelledText == null) {
				if (other.relabelledText != null)
					return false;
			} else if (!relabelledText.equals(other.relabelledText))
				return false;
			if (text == null) {
				if (other.text != null)
					return false;
			} else if (!text.equals(other.text))
				return false;
			if (varDeclarations == null) {
				if (other.varDeclarations != null)
					return false;
			} else if (!varDeclarations.equals(other.varDeclarations))
				return false;
			if (variablesUsedForArgs == null) {
				if (other.variablesUsedForArgs != null)
					return false;
			} else if (!variablesUsedForArgs.equals(other.variablesUsedForArgs))
				return false;
			return true;
		}
	}

	/** Represents a label on a transition. */
	public static class Label
	{
		protected String name;
		protected CompositionOfFunctions pre = new CompositionOfFunctions(null), post = new CompositionOfFunctions(null);
		
		public Label(String labelName)
		{
			name = appendToString(labelName, null);
			if (name == null)
				throw new IllegalArgumentException("invalid label name");
		}
		
		public Label(String labelName, CompositionOfFunctions preCondition, CompositionOfFunctions postCondition)
		{
			this(labelName);pre=preCondition;post=postCondition;
		}
	
		/** Returns the name of this label. */
		public String getName()
		{
			return name;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((name == null) ? 0 : name.hashCode());
			result = prime * result + ((post == null) ? 0 : post.hashCode());
			result = prime * result + ((pre == null) ? 0 : pre.hashCode());

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
			if (!(obj instanceof Label))
				return false;
			Label other = (Label) obj;
			if (name == null) {
				if (other.name != null)
					return false;
			} else if (!name.equals(other.name))
				return false;
			if (post == null) {
				if (other.post != null)
					return false;
			} else if (!post.equals(other.post))
				return false;
			if (pre == null) {
				if (other.pre != null)
					return false;
			} else if (!pre.equals(other.pre))
				return false;
			return true;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return getName();
		}
	}

	static public final char delimiter='@';
	static public final String varNewSuffix = delimiter+"N",varOldSuffix=delimiter+"M", functionArg = "frg";
	public static final String delimiterString=""+delimiter;
	
	/** Given a string with variables, renumbers them according to the number passed. This is used
	 * to generate memory and input variables corresponding to different states/labels on a path.
	 * 
	 * @param constraint what to renumber
	 * @param num how far on a path we are.
	 * @param previous the way to refer to previous values of variables (so that a postcondition can use them).
	 * The values <em>num</em> and <em>previous</em> cannot be the same since in the course of constructing
	 * data traces and collecting arguments to functions, we expect each entry to reference unique variables.
	 * The uniqueness is determined by function name and use count within each of PRE,POST and IO; only one
	 * PRE,POST or IO can be present at each point in a sequence and their unique numbers is a pair of _M and _N.
	 * 
	 * @return a constraint with renumbered variables. 
	 */
	protected static String toCurrentMem(String constraint, int num, int previous)
	{
		if (constraint == null || constraint.length() == 0)
			return "true";
		//assert num != previous;// when I'd like to compare the effect of an operation on a state, I may have a loopback transition and hence compare with itself.
		
		String result = constraint;
		if (num != JUConstants.intUNKNOWN)
			result = result.replaceAll(varNewSuffix,delimiterString+num);
		else
			if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.SMTWARNINGS)))
			{// Consistency checking
				if (result.contains(varNewSuffix))
					throw new IllegalArgumentException("current number should be valid in "+constraint);
			}

		if (previous != JUConstants.intUNKNOWN)
			result = result.replaceAll(varOldSuffix,delimiterString+previous);
		else
			if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.SMTWARNINGS)))
			{// Consistency checking
				if (result.contains(varOldSuffix))
					throw new IllegalArgumentException("previous number should be valid in "+constraint);
			}
		return result;
	}

	/** Given a constraint, makes all variables in it match the current state number and appends the result
	 * to the provided string buffer.
	 * 
	 * @param what text to convert
	 * @param stateNumber state number
	 * @param oldStateNumber previous state number
	 * @return true if something was appended (<em>what</em> is non-null and has a positive length).
	 */
	protected static boolean addStringRepresentation(String what, int stateNumber, int oldStateNumber, StringBuffer result)
	{
		boolean outcome = false;
		if (what != null && what.length()>0)
		{
			result.append(toCurrentMem(what,stateNumber,oldStateNumber));result.append(ENDL);
			outcome = true;
		}
		return outcome;
	}

	/** Maps names of labels to their representation - a learner does not need to know the details, albeit
	 * perhaps we should've incorporated an abstract label into our transition structure.
	 * The three maps correspond to different phases of construction of the final map. 
	 */
	protected Map<String,Label> labelMapConstructionOfOperations = null,
		labelMapConstructionOfDataTraces = null, labelMapFinal = null;
	
	/** Maps names of low-level functions to their representation. 
	 */
	protected Map<String,LowLevelFunction> functionMap = new TreeMap<String,LowLevelFunction>();
	
	/** This one stores the text from which label descriptions have been loaded. */
	private List<String> originalText = new LinkedList<String>();
	
	public Element storeToXML(Document doc)
	{
		Element labelText = doc.createElement(StatechumXML.ELEM_LABELDETAILS.name());
		StringWriter labelDetails = new StringWriter();ProgressDecorator.writeInputSequence(labelDetails, originalText);
		labelText.setTextContent(labelDetails.toString());
		return labelText;
	}
	
	public void loadXML(Element elem)
	{
		parseCollection(ProgressDecorator.readInputSequence(new StringReader( elem.getTextContent()),-1));		
	}

	/** Represents a trace with arguments. */
	public static class TraceWithData
	{
		protected boolean accept = true;
		protected List<String> traceDetails = null;
		protected List<CompositionOfFunctions> arguments = null;
	}
	
	protected final Collection<TraceWithData> traces = new LinkedList<TraceWithData>();
	
	/** Extracts positive sequences from the collection. */
	public Collection<List<String>> getSPlus() 
	{
		final Collection<List<String>> sPlus = new LinkedList<List<String>>();
		for(TraceWithData trace:traces)
			if (trace.accept)
				sPlus.add(trace.traceDetails);
		return sPlus; 
	}
	
	/** Extracts negative sequences from the collection. */
	public Collection<List<String>> getSMinus() 
	{ 
		final Collection<List<String>> sMinus = new LinkedList<List<String>>();
		for(TraceWithData trace:traces)
			if (!trace.accept)
				sMinus.add(trace.traceDetails);
		return sMinus; 
	}
	
	public enum VARIABLEUSE { PRE,POST,IO}; 
	
	/** Generates a new variable name. 
	 * 
	 * @param functionName this variable will be this function's argument or value. 
	 * @param useNumber Each time a function is used we create a set of fresh variables for its 
	 * 	arguments and value, this is the number representing the number of times this function
	 * has been used in a particular element of a trace.
	 * @param useKind how this variable is expected to be used (to ensure that for each position
	 * of each data trace, an IO, a PRE and a POST will have disjoint variable IDs). 
	 * @param position 
	 * <ul>
	 * <li>zero means an outcome of a function, </li>
	 * <li>positive is the position of an argument 
	 * the generated variable name will represent and</li> 
	 * <li><em>JUConstants.intUNKNOWN</em> means the positional argument 
	 * will not be generated (but the separator before it will be).</li>
	 * </ul>
	 * @return the generated variable name.
	 */
	public static String generateFreshVariable(String functionName, VARIABLEUSE useKind, int useNumber,  int position)
	{
		return functionArg+delimiter+functionName+delimiter+useKind.name()+delimiter+useNumber+varOldSuffix+
			(useKind == VARIABLEUSE.IO?delimiterString+"IO":varNewSuffix)+delimiter+
			(position != JUConstants.intUNKNOWN?Integer.toString(position):"");
	}

	/** Maps low level functions to variables associated with arguments and return values of these functions. */ 
	final Map<LowLevelFunction,Collection<String>> functionToVariables = new TreeMap<LowLevelFunction,Collection<String>>();
	
	/** Contains declarations and assertions associated with known traces and values of arguments 
	 * of uninterpreted functions used in those traces.  
	 */
	String knownTraces;
	
	/** Called for each detected function.
	 * 
	 * @param functionName the name of the function
	 * @param args arguments of this function.
	 * @return if non-null, the name of a variable to represent the result of computing this function;
	 * null return value means that function and its arguments should be passed unchanged through interpretFunctionalExpression.
	 */
	public class FunctionVariablesHandler implements FunctionArgumentsHandler
	{
		/** How many times a function has been used within a precondition, postcondition or i/o. */
		final Map<LowLevelFunction,Integer> functionToUseCounter = new TreeMap<LowLevelFunction,Integer>();
		final StringBuffer additionalVariables = new StringBuffer(),additionalDeclarations = new StringBuffer();
		protected final Map<LowLevelFunction,Collection<String>> variablesUsedForArgs = new TreeMap<LowLevelFunction,Collection<String>>();
		
		public void reset()
		{
			additionalVariables.setLength(0);additionalDeclarations.setLength(0);
			functionToUseCounter.clear();variablesUsedForArgs.clear();
		}
		
		/** All variables introduced using this handler will share this kind. */
		private final VARIABLEUSE useKind;
		
		public FunctionVariablesHandler(VARIABLEUSE kind)
		{
			useKind = kind;reset();
		}
		
		public String HandleLowLevelFunction(String functionName,List<String> args) 
		{
			String result = null;
			
			// First, we check that the supplied function does not refer to variables it is not allowed to refer to. */
			for(String arg:args)
			{// TODO: to test these checks
				char first = arg.charAt(0);
				if (first < '0' && first > '9' && !arg.startsWith(functionArg))
				{// must be a user-declared identifier
					boolean next = false, prev=false;
					if (arg.endsWith(varOldSuffix))
					{
						prev = true;arg.substring(0, arg.length()-varOldSuffix.length());
					}
					else
						if (arg.endsWith(varNewSuffix))
						{
							next = true;arg.substring(0, arg.length()-varNewSuffix.length());
						}
					VARTYPE type = variables.get(arg);
					if (type == null)
						throw new IllegalArgumentException("undeclared variable "+arg+" used in function "+functionName);
					
					if ((type == VARTYPE.VAR_INPUT || type == VARTYPE.VAR_OUTPUT) && prev) 
						throw new IllegalArgumentException("i/o variables should only be used with "+varNewSuffix+" suffix");
					
					if (useKind == VARIABLEUSE.PRE && 
							(type == VARTYPE.VAR_OUTPUT || (type == VARTYPE.VAR_MEMORY && next)))
						throw new IllegalArgumentException("preconditions cannot refer to new values of memory or outputs");
				}
			}
			
			LowLevelFunction func = functionMap.get(functionName);
			if (func != null)
			{// this is a function of interest to us
				if (args.size() != func.arity)
					throw new IllegalArgumentException("function "+functionName+" should take "+func.arity+" arguments instead of "+args);
				assert func.varDeclaration != null;
				
				int useCounter = 0;
				Integer currentUseCounter = functionToUseCounter.get(func);
				if (currentUseCounter != null) useCounter = currentUseCounter.intValue();
				
				additionalVariables.append("(= ");String outcomeVariable = generateFreshVariable(func.getName(), useKind, useCounter, 0);
				additionalVariables.append(outcomeVariable);
				additionalVariables.append(" (");additionalVariables.append(func.getName());
				int argNumber = 1;
				while(argNumber <= args.size())
				{
					additionalVariables.append(' ');additionalVariables.append(generateFreshVariable(func.getName(), useKind, useCounter, argNumber++));
				}
				additionalVariables.append("))");
				
				argNumber = 1;
				for(String arg:args)
				{
					additionalVariables.append("(= ");
					additionalVariables.append(generateFreshVariable(func.getName(), useKind, useCounter, argNumber++));
					additionalVariables.append(' ');additionalVariables.append(arg);
					additionalVariables.append(')');
				}
				additionalVariables.append(ENDL);
				if (func.constraint != null)
				{// there is a constraint associated with this function
					additionalVariables.append(func.constraint.replace(functionArg+delimiterString, generateFreshVariable(func.getName(), useKind, useCounter, JUConstants.intUNKNOWN)));
					additionalVariables.append(ENDL);
				}
				additionalDeclarations.append(func.varDeclaration.replace(functionArg+delimiterString, generateFreshVariable(func.getName(), useKind, useCounter, JUConstants.intUNKNOWN)));
				additionalDeclarations.append(ENDL);
				result = outcomeVariable;
				// variables introduced for every use of this function are recorded in the map 
				Collection<String> knownValues = variablesUsedForArgs.get(func);
				if (knownValues == null)
				{
					knownValues = new LinkedList<String>();variablesUsedForArgs.put(func,knownValues);
				}
				knownValues.add(generateFreshVariable(func.getName(), useKind, useCounter, JUConstants.intUNKNOWN));
				useCounter++;
				functionToUseCounter.put(func, useCounter);// stores the value to use next time.
				
			}
			return result;
		}
		
		public CompositionOfFunctions getComposition(String text)
		{
			return new CompositionOfFunctions(text,additionalDeclarations.toString(),additionalVariables.toString(),variablesUsedForArgs);
		}
	}
	
	/** Loads labels and traces from a collection of strings, populating both internal 
	 * data and the supplied collection of traces. 
	 */
	public void parseCollection(Collection<String> data)
	{
		LabelParser parser = new LabelParser();
		for(String text:data)
			if (!text.startsWith(QSMTool.cmdLowLevelFunction) && 
				!text.startsWith(QSMTool.cmdDataTrace) &&
				!text.startsWith(QSMTool.cmdOperation) &&
				!text.startsWith(QSMTool.cmdComment) &&
				!text.startsWith(QSMTool.cmdVarInput))
				throw new IllegalArgumentException("invalid command "+text);

		for(String text:data) 
			if (text.startsWith(QSMTool.cmdLowLevelFunction))
			{
				parseFunction(text.substring(QSMTool.cmdLowLevelFunction.length()).trim());
			}
		
		for(LowLevelFunction function:functionMap.values())
		{
			for(int i=0;i<= function.arity;++i) // note the <= here, this is because we have arity arguments and the return value
			{
				if (function.varDeclaration == null)
					throw new IllegalArgumentException("types of return value and arguments is missing for function "+function.getName());
				if (!function.varDeclaration.contains(functionArg+delimiterString+i))
					throw new IllegalArgumentException("the variable declaration of "+function.getName()+" is missing a declaration for "+(i>0?"argument "+i:"the return value"));
			}
			
			if (function.arity > 0 && function.constrainArgsToTraces)
				usingLowLevelFunctions = true;
		}
		
		if (labelMapConstructionOfOperations != null)
			throw new IllegalArgumentException("operations already built");
		labelMapConstructionOfOperations = new TreeMap<String,Label>();
		for(String text:data) 
			if (text.startsWith(QSMTool.cmdVarInput))
			{
				parseVarDeclaration(text.substring(QSMTool.cmdVarInput.length()).trim());
			}

		for(String text:data) 
			if (text.startsWith(QSMTool.cmdOperation))
			{
				parseLabel(text.substring(QSMTool.cmdOperation.length()).trim());
			}

		// Assign an initial memory value.
		init = labelMapConstructionOfOperations.get(INITMEM);if (init == null) throw new IllegalArgumentException("missing initial memory value");
		if ((init.pre.text != null && init.pre.text.contains(varOldSuffix)) || 
				(init.post.text != null && init.post.text.contains(varOldSuffix)))
			throw new IllegalArgumentException(init.getName()+" should not refer to "+varOldSuffix);

		assert labelMapConstructionOfDataTraces == null;
		labelMapConstructionOfDataTraces = new TreeMap<String,Label>();
		for(Label label:labelMapConstructionOfOperations.values())
		{
			label.pre  =  parser.interpretPrePostCondition(label.pre.text, new FunctionVariablesHandler(VARIABLEUSE.PRE));
			label.post =  parser.interpretPrePostCondition(label.post.text, new FunctionVariablesHandler(VARIABLEUSE.POST));
			labelMapConstructionOfDataTraces.put(label.getName(), label);// changes to pre/post may change the ordering in the map, hence we rebuild the map.
		}

		for(String text:data) 
			if (text.startsWith(QSMTool.cmdDataTrace))
			{
				parseTrace(text.substring(QSMTool.cmdOperation.length()).trim());
			}

		for(String text:data)
			originalText.add(text);
	}
	
	/** Given a string of text, parses it as a low-level function. */
	public void parseFunction(String text)
	{
		if (text == null || text.length() == 0) return;// ignore empty input
		
		StringTokenizer tokenizer = new StringTokenizer(text);
		if (!tokenizer.hasMoreTokens()) return;// ignore empty input
		String functionName = tokenizer.nextToken();
		if (!tokenizer.hasMoreTokens()) throw new IllegalArgumentException("expected details for function "+functionName);
		FUNC_DATA kind = null;String prepost = tokenizer.nextToken();
		try
		{
			kind=FUNC_DATA.valueOf(prepost);
		}
		catch(IllegalArgumentException ex)
		{
			throw new IllegalArgumentException("expected "+Arrays.toString(FUNC_DATA.values())+" but got: "+prepost);
		}
		if (!tokenizer.hasMoreTokens()) throw new IllegalArgumentException("expected specification for function "+functionName+" "+prepost);
		StringBuffer labelSpec = new StringBuffer(tokenizer.nextToken());
		while(tokenizer.hasMoreTokens())
		{
			labelSpec.append(' ');labelSpec.append(tokenizer.nextToken());
		}
		
		LowLevelFunction func = functionMap.get(functionName);
		if (func == null)
		{
			func = new LowLevelFunction(functionName);functionMap.put(func.getName(),func);
		}
		switch(kind)
		{
		case CONSTRAINT:
			func.constraint = appendToString(labelSpec.toString(), func.constraint);break;
		case DECL:
			func.varDeclaration = appendToString(labelSpec.toString(),func.varDeclaration);break;
		case ARITY:
			if (func.arity > 0) throw new IllegalArgumentException("the arity of "+functionName+" is already known");
			try
			{
				func.arity = Integer.parseInt(labelSpec.toString());
			}
			catch(NumberFormatException ex)
			{
				throw new IllegalArgumentException("the arity of "+functionName+" is "+labelSpec.toString()+" which is not a number");
			}
			if (func.arity < 0)
				throw new IllegalArgumentException("the arity of "+functionName+" is an invalid number");
			if (func.arity == 0)
				if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.SMTWARNINGS)))
					System.err.println("WARNING: function "+func.getName()+" has a zero arity and hence arguments cannot be constrained to values used"); 
			break;
		case CONSTRAINARGS:
			func.constrainArgsToTraces = Boolean.parseBoolean(labelSpec.toString());
			break;
		}
	
	}
	
	/** Given a string of text, parses it as a declaration of an input variable. */
	public void parseVarDeclaration(String text)
	{
		if (text == null || text.length() == 0) return;// ignore empty input
		
		StringTokenizer tokenizer = new StringTokenizer(text);
		if (!tokenizer.hasMoreTokens()) return;// ignore empty input
		String varName = tokenizer.nextToken();
		if (!tokenizer.hasMoreTokens()) throw new IllegalArgumentException("expected details for variable "+varName);
		VARTYPE kind = null;String prepost = tokenizer.nextToken();
		try
		{
			kind=VARTYPE.valueOf(prepost);
		}
		catch(IllegalArgumentException ex)
		{
			throw new IllegalArgumentException("expected "+Arrays.toString(VARTYPE.values())+" but got: "+prepost);
		}
		
		if (variables.containsKey(varName))
			throw new IllegalArgumentException("declaration of variable "+varName+" already exists");
		variables.put(varName, kind);
	}
	
	/** Given a string of text, parses it as a label. */
	public void parseLabel(String text)
	{
		if (text == null || text.length() == 0) return;// ignore empty input
		
		StringTokenizer tokenizer = new StringTokenizer(text);
		if (!tokenizer.hasMoreTokens()) return;// ignore empty input
		String labelName = tokenizer.nextToken();
		if (!tokenizer.hasMoreTokens()) throw new IllegalArgumentException("expected details for label "+labelName);
		OP_DATA kind = null;String prepost = tokenizer.nextToken();
		try
		{
			kind=OP_DATA.valueOf(prepost);
		}
		catch(IllegalArgumentException ex)
		{
			throw new IllegalArgumentException("expected "+Arrays.toString(OP_DATA.values())+" but got: "+prepost);
		}
		if (!tokenizer.hasMoreTokens()) throw new IllegalArgumentException("expected specification for label "+labelName);
		StringBuffer labelSpec = new StringBuffer(tokenizer.nextToken());
		while(tokenizer.hasMoreTokens())
		{
			labelSpec.append(' ');labelSpec.append(tokenizer.nextToken());
		}
		
		Label lbl = labelMapConstructionOfOperations.get(labelName);
		if (lbl == null)
		{
			lbl = new Label(labelName);labelMapConstructionOfOperations.put(lbl.getName(),lbl);
		}
		switch(kind)
		{
		case PRE:
			lbl.pre = new CompositionOfFunctions(appendToString(labelSpec.toString(), lbl.pre.text));break;
		case POST:
			lbl.post = new CompositionOfFunctions(appendToString(labelSpec.toString(), lbl.post.text));break;
		}
	}
	
	/** Given a string of text, parses it as a trace with variable values. */
	public void parseTrace(String text)
	{
		if (text == null || text.length() == 0) return;// ignore empty input
		
		String traceType = text.substring(0, 1);
		TraceWithData trace = new TraceWithData();
		if (traceType.equals("+"))
			trace.accept = true;
		else
			if (traceType.equals("-"))
				trace.accept = false;
			else
				throw new IllegalArgumentException("invalid data trace type "+traceType+", only \"+\" or \"-\" is allowed");
		
		LabelParser parser = new LabelParser();
		parser.interpretTrace(text.substring(1).trim(),new FunctionVariablesHandler(VARIABLEUSE.IO));
		trace.traceDetails = parser.operations;
		trace.arguments = parser.arguments;
		traces.add(trace);
	}
	
	public enum OP_DATA { PRE,POST }
	public enum FUNC_DATA { ARITY, DECL, CONSTRAINT, CONSTRAINARGS }
	
	public LabelRepresentation()
	{
	}
	
	/** Whenever we generate paths, new variables have to be introduced. By incrementing this variable,
	 * new ones can be generated.
	 */
	protected int currentNumber = 0;
	
	public static final String 
		commentForNewSeq = ";; sequence", 
		commentForLabel = ";; label ", 
		commentForTransition = ";; transition ",
		assertString="(assert ",
		commentForInit = ";; INIT";
	public static final char ENDL = '\n';
	
	/** Given a path in a graph returns an expression which can be used to check whether that path can be followed. 
	 * Note that if a path cannot be followed, this means that either the precondition is not satisfied or that
	 * a postcondition cannot be satisfied.
	 * <p>
	 * The supplied path cannot contain newlines.
	 */
	public synchronized Pair<String,String> getConjunctionForPath(List<Label> path, List<CompositionOfFunctions> inputOutput)
	{
		if (labelMapFinal == null) throw new IllegalArgumentException("construction incomplete");
		if (inputOutput != null && inputOutput.size() != path.size())
			throw new IllegalArgumentException("mismatched length of path and parameters");

		StringBuffer axiom = new StringBuffer(), varDeclaration = new StringBuffer();
		axiom.append(commentForNewSeq);
		Iterator<Label> pathIterator = path.iterator();
		Iterator<CompositionOfFunctions> ioIterator = inputOutput == null?null:inputOutput.iterator();
		axiom.append('[');
		boolean first = true;
		while(pathIterator.hasNext())
		{
			if (first) first = false;else axiom.append(',');
			axiom.append(pathIterator.next().toString());
			if (ioIterator != null)
			{
				String args = ioIterator.next().getCondition();if (args.length()>0) { axiom.append('(');axiom.append(args);axiom.append(')'); }
			}
		}
		axiom.append(']');axiom.append(ENDL);
		
		// We always have to add variable declarations (even for empty paths) because
		// they may be used in operations involving the abstract state we are returning here.
		for(int i=0;i<=path.size();++i) 
			if (init.pre.text != null)
			{
				varDeclaration.append(toCurrentMem(init.pre.getCondition(), i+currentNumber,JUConstants.intUNKNOWN));varDeclaration.append(ENDL);
			}

		boolean pathNotEmpty = false;

		// Now walk through the path generating constraints.
		axiom.append("(and");axiom.append(ENDL);axiom.append(commentForInit);axiom.append(ENDL);

		pathNotEmpty |= addStringRepresentation(init.post.getCondition(),currentNumber,JUConstants.intUNKNOWN,axiom);

		int i=0;
		ioIterator = inputOutput == null?null:inputOutput.iterator();
		for(Label currentLabel:path) 
		{
			axiom.append(commentForLabel+currentLabel.getName());axiom.append(ENDL);
			int previousNumber = i+currentNumber;++i;
			if (ioIterator != null)
				pathNotEmpty |= addStringRepresentation(ioIterator.next().getCondition(),i+currentNumber,previousNumber,axiom);
			pathNotEmpty |= addStringRepresentation(currentLabel.pre.getCondition(),JUConstants.intUNKNOWN,previousNumber,axiom);
			pathNotEmpty |= addStringRepresentation(currentLabel.post.getCondition(),i+currentNumber,previousNumber,axiom);
		}
		axiom.append(")");
		currentNumber+=path.size()+1;
		
		axiom.append(ENDL);
		return new Pair<String,String>(varDeclaration.toString(), pathNotEmpty?axiom.toString():commentForNewSeq+path+ENDL+"true"+ENDL);
	}
	
	public static String getAssertionFromVarAndAxiom(String variableDeclarations, String abstractState)
	{
		return variableDeclarations+assertString+"(and "+ENDL+abstractState+ENDL+"))"+ENDL;
	}
	
	public class AbstractState
	{
		/** Graph vertex corresponding to this state. Very useful for testing. */
		public final CmpVertex vertex;
		
		/** Previous abstract state. Makes it possible to reconstruct a path 
		 * from which the particular abstract state was built (and hence the axiom is
		 * merely a cached value. 
		 */
		public final AbstractState previousState;
		
		/** Axiom so far. */
		public final String abstractState;
		
		/** Variable declarations to support the axiom. */
		public final String variableDeclarations;
		
		/** The number corresponding to this abstract state. */
		public final int stateNumber;

		/** The transition leading to this state, making it (in conjuction with lastIO) possible to compute a postcondition
		 * for an arbitrarily-chosen abstract state number. 
		 * This would be null for an initial state. 
		 */
		public final Label lastLabel;
		
		/** Arguments to the last label. Can be null if lastLabel is not null, but will never 
		 * be non-null if lastLabel is null. */
		public final CompositionOfFunctions lastIO;
		
		/** Constructs an abstract state for the initial state.
		 * 
		 * @param v state
		 * @param num number to give to this state.
		 */
		public AbstractState(CmpVertex initState, int num)
		{
			stateNumber = num;vertex = initState;previousState = null;lastLabel = null;lastIO = null;
			// Initial memory value.
			variableDeclarationsThisState = toCurrentMem(init.pre.getCondition(), num, JUConstants.intUNKNOWN);
			variableDeclarations = variableDeclarationsThisState;
			abstractStateThisState = commentForInit+ENDL+toCurrentMem(init.post.getCondition(), num, num);
			abstractState = abstractStateThisState;
		}
		
		/** Reflects the contribution of this state to the global condition. */
		final String variableDeclarationsThisState, abstractStateThisState;
		
		/** Creating a clone of an abstract state is easy: it is enough to simply create another instance 
		 * of this AbstractState using the recorded arguments.  
		 * @param v DFA vertex this abstract state is to be associated with.
		 * @param argPreviousState an abstract state from which this state has been entered by invoking <em>step</em>. 
		 * @param arglastLabel the operation used to enter this abstract state from the previous one.
		 * @param arglastIO the arguments to the operation used to enter this abstract state from the previous one.
		 * @param N the number to give to variables in order to express "current state". Previous state is obtained 
		 * from the number associated with the previous state provided as an argument <em>argPreviousState</em>.
		 */
		public AbstractState(CmpVertex v,AbstractState argPreviousState, Label arglastLabel, CompositionOfFunctions arglastIO, int num)
		{
			if (argPreviousState == null || arglastLabel == null) throw new IllegalArgumentException("previous state or label cannot be null"); 
			vertex=v;previousState=argPreviousState;lastLabel=arglastLabel;lastIO = arglastIO;

			stateNumber = num;
			variableDeclarationsThisState = (lastIO == null || lastIO.varDeclarations.length()==0?"":toCurrentMem(lastIO.varDeclarations,num,previousState.stateNumber)+ENDL)+
				toCurrentMem(init.pre.getCondition()+lastLabel.pre.varDeclarations+lastLabel.post.varDeclarations,num,previousState.stateNumber);
			variableDeclarations = previousState.variableDeclarations+ENDL+variableDeclarationsThisState;

			abstractStateThisState = (lastIO == null || lastIO.getCondition().length()==0?"":toCurrentMem(lastIO.getCondition(),num,previousState.stateNumber)+ENDL)+
				toCurrentMem(lastLabel.pre.getCondition(),JUConstants.intUNKNOWN,previousState.stateNumber)+ENDL+
				toCurrentMem(lastLabel.post.getCondition(),num,previousState.stateNumber);
			abstractState = previousState.abstractState+ENDL+commentForLabel+lastLabel.getName()+ENDL+abstractStateThisState;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result
					+ ((abstractState == null) ? 0 : abstractState.hashCode());
			result = prime * result
				+ ((lastLabel == null) ? 0 : lastLabel.hashCode());
			result = prime * result
				+ ((lastIO == null) ? 0 : lastIO.hashCode());
			result = prime * result + stateNumber;
			result = prime
					* result
					+ ((variableDeclarations == null) ? 0
							: variableDeclarations.hashCode());
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
			if (!(obj instanceof AbstractState))
				return false;
			AbstractState other = (AbstractState) obj;
			if (abstractState == null) {
				if (other.abstractState != null)
					return false;
			} else if (!abstractState.equals(other.abstractState))
				return false;
			if (lastLabel == null) {
				if (other.lastLabel != null)
					return false;
			} else if (!lastLabel.equals(other.lastLabel))
				return false;
			if (lastIO == null) {
				if (other.lastIO != null)
					return false;
			} else if (!lastIO.equals(other.lastIO))
				return false;
			if (stateNumber != other.stateNumber)
				return false;
			if (variableDeclarations == null) {
				if (other.variableDeclarations != null)
					return false;
			} else if (!variableDeclarations.equals(other.variableDeclarations))
				return false;
			return true;
		}
	}

	/** Each time a merge happens, we need to rebuild a map from merged vertices to collections 
	 * of original vertices they correspond to. This is the purpose of this method.
	 * <p>
	 * If <em>previousMap</em> is null, the current map is updated with vertices not 
	 * mentioned in the map (or built anew if it does not exist).
	 * <p>
	 * The typical use for this method is to call it first with an argument of the previous graph
	 * and subsequently with a null to add abstract states to the graph states added using IF-THEN
	 * automata.
	 * <p>
	 * There is no waste in using CmpVertex-vertices because they are part of the initial PTA and
	 * hence kept in memory anyway.
	 */
	public void buildVertexToAbstractStateMap(LearnerGraph coregraph, LearnerGraph previousGraph, boolean updateValuesUsedOnTraces)
	{
		Map<CmpVertex,Collection<LabelRepresentation.AbstractState>> previousMap = previousGraph == null?null:previousGraph.getVertexToAbstractState();
		
		// First, we build a collection of states of the original PTA which correspond to the each merged vertex.
		Map<CmpVertex,Collection<LabelRepresentation.AbstractState>> newVertexToEqClass = new TreeMap<CmpVertex,Collection<LabelRepresentation.AbstractState>>();
		
		if (previousMap == null)
		{// either the case when we get here for the first time (as well as right after a reset)
		 // or when vertices have been added to the graph and we need to update the map.
			if (coregraph.getVertexToAbstractState() == null)
				addAbstractStatesFromTraces(coregraph);
			
			newVertexToEqClass = coregraph.getVertexToAbstractState();// we are updating the map here.
			int elementCounter = currentNumber;

			Queue<CmpVertex> fringe = new LinkedList<CmpVertex>();
			Set<CmpVertex> statesInFringe = new HashSet<CmpVertex>();// in order not to iterate through the list all the time.
			fringe.add(coregraph.init);statesInFringe.add(coregraph.init);
			
			// Entry for the initial state is always added by addAbstractStatesFromTraces, 
			// hence no need to add it here.			

			while(!fringe.isEmpty())
			{// based on computeShortPathsToAllStates in AbstractPathRoutines
				CmpVertex currentState = fringe.remove();
				Collection<AbstractState> currentAbstractStates=newVertexToEqClass.get(currentState);
				Map<String,CmpVertex> targets = coregraph.transitionMatrix.get(currentState);
				if(targets != null && !targets.isEmpty())
					for(Entry<String,CmpVertex> labelstate:targets.entrySet())
					{
						CmpVertex target = labelstate.getValue();
						if (!statesInFringe.contains(target) && target.isAccept()) // TODO: to check that everything else will work ok with rejects ignored, as they should be.
						{// the new state has not yet been visited, so we may need to add an entry the 
						 // collection of AbstractStates associated with it.
							Collection<AbstractState> targetDataStates = newVertexToEqClass.get(target);
							if (targetDataStates == null)
							{// build a collection of target abstract states if none are known.
								targetDataStates = new LinkedList<AbstractState>();
								newVertexToEqClass.put(target, targetDataStates);
								for(AbstractState currentAbstractState:currentAbstractStates)
								{
									Label currentLabel=labelMapConstructionOfDataTraces.get(labelstate.getKey());if (currentLabel == null) throw new IllegalArgumentException("unknown label "+labelstate.getKey());
									AbstractState abstractState = new AbstractState(target,currentAbstractState, currentLabel,null,elementCounter++);
									targetDataStates.add(abstractState);
									tracesVars.append(abstractState.variableDeclarationsThisState);tracesVars.append(ENDL);
									traceAxioms.append(abstractState.abstractStateThisState);
								}
							}
							fringe.offer(target);
							statesInFringe.add(target);
						}
					}
			}
			
			currentNumber = elementCounter;
			
			if (updateValuesUsedOnTraces)
			{
				knownTraces = encloseInBeginEndIfNotEmpty(tracesVars.toString()+ENDL+assertString+"(and "+traceAxioms.toString()+"))",blockDATATRACES);
				
				/* Now we go through all the recorded variables and updates pre and post-conditions so that
				 * they refer to the recorded values.
				 */
				
				labelMapFinal = new TreeMap<String,Label>();
				for(Label label:labelMapConstructionOfDataTraces.values())
				{
					label.pre  =  addKnownValuesToPrePost(label.pre);
					label.post =  addKnownValuesToPrePost(label.post);
					labelMapFinal.put(label.getName(), label);// changes to pre/post may change the ordering in the map, hence we rebuild the map.
				}
			}
			labelMapConstructionOfOperations=null;
		}
		else // after a previous successful merge 
			for(AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:coregraph.learnerCache.getMergedStates())
				if (eqClass.getMergedVertex().isAccept())
				{// TODO: to test this
					List<AbstractState> combinedAbstractStates = new LinkedList<AbstractState>();
					for(CmpVertex state:eqClass.getStates())
						combinedAbstractStates.addAll(previousMap.get(state));
					newVertexToEqClass.put(eqClass.getMergedVertex(),combinedAbstractStates);
				}
		coregraph.vertexToAbstractState = newVertexToEqClass;
		
		if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.SMTWARNINGS)))
		{// Consistency checking
			Map<CmpVertex,CmpVertex> vertexToCollection = new TreeMap<CmpVertex,CmpVertex>();
			for(Entry<CmpVertex,Collection<LabelRepresentation.AbstractState>> eqClass:coregraph.vertexToAbstractState.entrySet())
			{
			 // Checking that vertices associated with abstract states from different merged vertices do not intersect.
			 // This is done by building a map associating DFA vertices recorded in abstract states
			 // with the DFA states they are associated with. If there are abstract states with the same 
			 // DFA vertex associated to different DFA states, this is to be reported.
				for(LabelRepresentation.AbstractState abstractState:eqClass.getValue())
				{
					CmpVertex existingClass = vertexToCollection.get(abstractState.vertex);
					if (existingClass != null && existingClass != eqClass.getKey())
						throw new IllegalArgumentException("classes with DFA state "+abstractState.vertex+" are associated to both "+eqClass.getKey()+" and "+existingClass);
					vertexToCollection.put(abstractState.vertex,eqClass.getKey());
				}
			}
			
			{// Checking that all accept-vertices from the current graph have a merged vertex corresponding to them.
			 // This should be true by construction of the graph; the only way we may get here is where a vertex
			 // is not reachable from an initial state.
				Set<CmpVertex> verticesInGraph = new TreeSet<CmpVertex>();
				for(CmpVertex vert:coregraph.transitionMatrix.keySet())
					if (vert.isAccept()) verticesInGraph.add(vert);
				verticesInGraph.removeAll(coregraph.getVertexToAbstractState().keySet());
				if (!verticesInGraph.isEmpty())
					throw new IllegalArgumentException("vertices such as "+verticesInGraph+" are not in the vertex to collection map (unreachable state?)");
			}
			
			{// Checking that domain of getVertexToAbstractState is contained within the set of vertices of our graph.
				Set<CmpVertex> verticesInCollection = new TreeSet<CmpVertex>();verticesInCollection.addAll(coregraph.getVertexToAbstractState().keySet());
				verticesInCollection.removeAll(coregraph.transitionMatrix.keySet());
				if (!verticesInCollection.isEmpty())
					throw new IllegalArgumentException("vertices from the vertex to collection map "+verticesInCollection+" do not feature in the graph");
			}
			
			// The computed number of vertices will not always be equal to the number of vertices in an 
			// original PTA because graphs can be updated using IF-THEN automata. This is the reason
			// why this check is not performed.
		}
	}
	
	/** Given a composition and a pair of numbers, associates these numbers with all variables
	 * of the supplied composition and adds the outcome to variables stored in this object.
	 * 
	 * @param composition what to process.
	 * @param num current number (aka _N).
	 * @param previous previous number (aka _M).
	 */
	protected void populateVarsUsedForArgs(CompositionOfFunctions composition, int num, int previous)
	{
		if (composition.variablesUsedForArgs != null)
			for(Entry<LowLevelFunction,Collection<String>> entry:composition.variablesUsedForArgs.entrySet())
			{
				Collection<String> vars = functionToVariables.get(entry.getKey());
				if (vars == null)
				{
					vars = new LinkedList<String>();functionToVariables.put(entry.getKey(),vars);
				}
				for(String var:entry.getValue())
					vars.add(toCurrentMem(var, num, previous));
			}
	}

	/** Used to build global constraints. */
	StringBuffer tracesVars = null, traceAxioms = null; 

	/** Goes through the collection of traces and adds them to the graph provided.
	 * In addition, this method populates the map from functions to variables used.
	 * 
	 * @param gr graph to process.
	 */
	public void addAbstractStatesFromTraces(LearnerGraph gr)
	{
		if (gr.getVertexToAbstractState() != null)
			throw new IllegalArgumentException("data traces should not be added to a graph with existing abstract states");
		functionToVariables.clear();
		tracesVars = new StringBuffer();traceAxioms = new StringBuffer(); 
		int elementCounter = currentNumber;
		gr.vertexToAbstractState = new TreeMap<CmpVertex,Collection<LabelRepresentation.AbstractState>>();
		AbstractState initialAbstractState = new AbstractState(gr.init,elementCounter++);
		gr.vertexToAbstractState.put(gr.init,Arrays.asList(new AbstractState[]{initialAbstractState}));

		populateVarsUsedForArgs(init.post, initialAbstractState.stateNumber, initialAbstractState.stateNumber);
		// Add details of the current abstract state to what we know of supplied data traces.
		/*String header = commentForInit+" initialisation"+ENDL;
		tracesVars.append(header);traceAxioms.append(header);*/
		tracesVars.append(initialAbstractState.variableDeclarationsThisState);tracesVars.append(ENDL);
		traceAxioms.append(initialAbstractState.abstractStateThisState);

		for(TraceWithData trace:traces)
		{
			assert trace.traceDetails.size() == trace.arguments.size();
			if (!trace.traceDetails.isEmpty())
			{// a non-empty trace - empty ones are ignored here because they do not make it possible to add new abstract states
				Iterator<String> operationIterator = trace.traceDetails.iterator();
				Iterator<CompositionOfFunctions> argumentsIterator = trace.arguments.iterator();
				AbstractState abstractState = initialAbstractState;

				Label currentLabel = labelMapConstructionOfDataTraces.get(operationIterator.next()); 
				CompositionOfFunctions currentIO = argumentsIterator.next();
				CmpVertex currentState = gr.init;
				
				while(operationIterator.hasNext())
				{
					CmpVertex previousState = currentState;
					currentState = gr.transitionMatrix.get(currentState).get(currentLabel.getName());

					populateVarsUsedForArgs(currentIO, JUConstants.intUNKNOWN, abstractState.stateNumber);
					populateVarsUsedForArgs(currentLabel.pre, elementCounter, abstractState.stateNumber);
					populateVarsUsedForArgs(currentLabel.post, elementCounter, abstractState.stateNumber);
					abstractState = new AbstractState(currentState,abstractState,currentLabel,currentIO,elementCounter);
					
					Collection<AbstractState> abstractStatesForDFAState = gr.vertexToAbstractState.get(currentState);
					if (abstractStatesForDFAState == null)
					{
						abstractStatesForDFAState = new LinkedList<AbstractState>();gr.vertexToAbstractState.put(currentState, abstractStatesForDFAState);
					}
					abstractStatesForDFAState.add(abstractState);

					// Add details of the current abstract state to what we know of supplied data traces.
					/*header = commentForLabel+previousState+"-"+currentLabel.getName()+
						(currentIO != null?"("+toCurrentMem(currentIO.text,JUConstants.intUNKNOWN, abstractState.stateNumber)+")":"")+
						"->"+currentState+ENDL;
					tracesVars.append(header);traceAxioms.append(header);*/
					tracesVars.append(abstractState.variableDeclarationsThisState);tracesVars.append(ENDL);
					traceAxioms.append(abstractState.abstractStateThisState);
					
					currentLabel = labelMapConstructionOfDataTraces.get(operationIterator.next());
					currentIO = argumentsIterator.next();
					elementCounter++;
				}

				if (trace.accept)
				{// positive trace, handle the target state as usual; for negative states all conditions are ignored
				 // because the last element of a trace should be unsatisfiable when taken together with the last
				 // but one abstract state. This last element may be internally unsatisfiable, hence we'd like not
				 // to introduce unsatisfiable constraints on arguments this may bring.
					CmpVertex previousState = currentState;
					currentState = gr.transitionMatrix.get(currentState).get(currentLabel.getName());

					populateVarsUsedForArgs(currentIO, JUConstants.intUNKNOWN, abstractState.stateNumber);
					populateVarsUsedForArgs(currentLabel.pre, elementCounter, abstractState.stateNumber);
					populateVarsUsedForArgs(currentLabel.post, elementCounter, abstractState.stateNumber);
					abstractState = new AbstractState(currentState,abstractState,currentLabel,currentIO,elementCounter);

					Collection<AbstractState> abstractStatesForDFAState = gr.vertexToAbstractState.get(currentState);
					if (abstractStatesForDFAState == null)
					{
						abstractStatesForDFAState = new LinkedList<AbstractState>();gr.vertexToAbstractState.put(currentState, abstractStatesForDFAState);
					}
					abstractStatesForDFAState.add(abstractState);

					// Add details of the current abstract state to what we know of supplied data traces.
					/*header = commentForLabel+previousState+"-"+currentLabel.getName()+
						(currentIO != null?"("+toCurrentMem(currentIO.text,JUConstants.intUNKNOWN, abstractState.stateNumber)+")":"")+
						"->"+currentState+ENDL;
					tracesVars.append(header);traceAxioms.append(header);*/
					tracesVars.append(abstractState.variableDeclarationsThisState);tracesVars.append(ENDL);
					traceAxioms.append(abstractState.abstractStateThisState);
					elementCounter++;
				}
				
			}
		}
		
		currentNumber = elementCounter;
	}
	
	public CompositionOfFunctions addKnownValuesToPrePost(CompositionOfFunctions composition)
	{
		StringBuffer additionalVariables = new StringBuffer();
		if (composition.variablesUsedForArgs != null && !composition.variablesUsedForArgs.isEmpty()) // if there are any variables in need of handling, go through them.
		{
			for(Entry<LowLevelFunction,Collection<String>> entry:composition.variablesUsedForArgs.entrySet())
			{// Now we use the variables recorded in order to record that for each use of each function
			 // in this composition, the tuple of values can be equal to each value passed to this function
			 // in the recorded traces.
			
				for(String funcVar:entry.getValue())
				{// for each use of this function
					additionalVariables.append(";; ");additionalVariables.append(funcVar);additionalVariables.append(0);additionalVariables.append(ENDL);
					additionalVariables.append("(or ");
					for(String knownVar:functionToVariables.get(entry.getKey()))
					{// constructing the equality of tuples
						additionalVariables.append("(and ");
						for(int i=1;i<=entry.getKey().arity;++i)
						{// equality of individual elements of a tuple.
							additionalVariables.append("(= ");
							additionalVariables.append(funcVar);additionalVariables.append(i);
							additionalVariables.append(' ');
							additionalVariables.append(knownVar);additionalVariables.append(i);
							additionalVariables.append(")");
							// this one checks that _M and _N have been already expanded
							assert !knownVar.contains(varNewSuffix) && !knownVar.contains(varOldSuffix);
						}
						additionalVariables.append(")");additionalVariables.append(ENDL);
					}
					additionalVariables.append(")");additionalVariables.append(ENDL);
				}
			}
		}
		return new CompositionOfFunctions(composition,additionalVariables.toString());
	}
		
	/** Given PTA computes a path which can be used as an axiom in later computations. 
	 * In addition to the construction of axioms, the method populates the map associating
	 * vertex identifiers to abstract states which would be reached in the supplied graph
	 * when those states are reached. 

	public synchronized String constructPathAxioms(LearnerGraph gr)
	{
		buildVertexToAbstractStateMap(gr, null);
		StringBuffer variableDeclarations = new StringBuffer(), lemmas = new StringBuffer();
		for(Entry<CmpVertex,Collection<AbstractState>> entry:gr.learnerCache.getVertexToAbstractState().entrySet())
			for(AbstractState previousAbstractState:entry.getValue())
			{
				for(Entry<String,CmpVertex> transition:gr.transitionMatrix.get(entry.getKey()).entrySet())
				{
					// Now we generate path axioms using state IDs 
					Label currentLabel=labelMap.get(transition.getKey());// if a label is not known, it will be buildVertexToEqClassMap to throw an exception.
					
					// This one clones the state and builds a path to the new state with new ids of variables.
					AbstractState prevState = new AbstractState(previousAbstractState.vertex,previousAbstractState.previousState,previousAbstractState.lastLabel);
					final int currentStateNumber = currentNumber-1;// using the last number utilised to build a clone of the previousAbstractState
					variableDeclarations.append(prevState.variableDeclarations);
					variableDeclarations.append(toCurrentMem(init.pre, currentStateNumber, currentStateNumber));variableDeclarations.append(ENDL);
					
					String textPre = "", textPost = "";
					if (currentLabel.pre != null)
						textPre = " "+toCurrentMem(currentLabel.pre,prevState.stateNumber,prevState.stateNumber);
					if (currentLabel.post != null)
						textPost = " "+toCurrentMem(currentLabel.post,currentStateNumber, prevState.stateNumber);
					++currentNumber;
					
					if (entry.getKey().isAccept())
					{// a path to the current state exists
						if (currentLabel.pre != null || currentLabel.post != null)
						{
							// now we need to add a condition that p => pre next and post next
							lemmas.append(commentForTransition+entry.getKey()+"("+(prevState.stateNumber)+")-"+currentLabel.getName()+"->"+
									transition.getValue()+"("+currentStateNumber+")");lemmas.append(ENDL);
							
							lemmas.append(assertString);lemmas.append("(implies");lemmas.append(ENDL);lemmas.append(prevState.abstractState);
							lemmas.append("\t(and");lemmas.append(textPre);lemmas.append(textPost);lemmas.append(")))");lemmas.append(ENDL);
						}
						else
						{
							if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.SMTWARNINGS)))
							{// if a precondition is empty, we cannot really generate a useful lemma, hence ignore this case but give a warning.
								System.out.println("label "+currentLabel+" does not have a pre- or post-condition");
							}
						}
					}
					else
					{// the transition to the last node cannot be taken.
						if (currentLabel.pre != null)
						{
							// now we need to add a condition that p => ¬pre next
							lemmas.append(commentForTransition+entry.getKey()+"("+(prevState.stateNumber)+")-"+currentLabel.getName()+"-#"+
									transition.getValue()+"("+currentStateNumber+")");lemmas.append(ENDL);
									
							lemmas.append(assertString);lemmas.append("(implies");lemmas.append(ENDL);lemmas.append(prevState.abstractState);
							lemmas.append("\t(not");lemmas.append(textPre);lemmas.append(")))");lemmas.append(ENDL);
						} 
						else
							if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.SMTWARNINGS)))
							{// if a precondition is empty, we cannot really generate a useful lemma, hence ignore this case but give a warning.
								System.out.println("label "+currentLabel+" leading to a reject-state does not have a precondition hence it is not possible to generate a lemma");
							}
					}
				}
			}
		StringBuffer outcome = new StringBuffer(variableDeclarations);outcome.append(lemmas);
		outcome.append(";; END OF PATH AXIOMS");outcome.append(ENDL);
		return outcome.toString();
	}
	*/
	
	/** Checks if abstract states corresponding to the supplied vertices are compatible,
	 * assuming that abstract states are themselves satisfiable. 
	 */
	public boolean abstractStatesCompatible(AbstractState Aarg, AbstractState Barg)
	{
		AbstractState A=Aarg,B=Barg;
		if (A.lastLabel == null && B.lastLabel == null)
			return true;// both states are initial states.
		
		if (B.lastLabel == null)
		{// If both states are not null, we make B's last label not null by swapping it with A
			AbstractState C = A;A = B;B = C;
		}
		
		// We make a clone of B in such a way that the new number is the same as that of A,
		// as a consequence, renumberedB and A share the same new state.
		AbstractState renumberedB = new AbstractState(B.vertex,B.previousState,B.lastLabel,B.lastIO,A.stateNumber);
		
		String varDecl = "";
		if (!usingLowLevelFunctions)
		{
			// Since renumberedB and A share the state number, we'll have duplicate variable declarations.
			// The two paths will have a common prefix (the initial state is always common),
			// this is why we have to throw away declarations associated with this prefix from a combined declaration.
			int Bnumber = B.stateNumber, Anumber = A.stateNumber;
			AbstractState Bcurr = B, Acurr = A;
			while(Bnumber != Anumber)
			{
				if (Bnumber > Anumber)
					Bcurr = Bcurr.previousState;
				else
					Acurr = Acurr.previousState;
				
				Anumber = Acurr.stateNumber;Bnumber = Bcurr.stateNumber;
			}
			
			// after the two numbers converge, Acurr is the point of forking.
			assert A.variableDeclarations.startsWith(Acurr.variableDeclarations);
			assert B.variableDeclarations.startsWith(Acurr.variableDeclarations);
			varDecl = B.variableDeclarations+A.variableDeclarations.substring(Acurr.variableDeclarations.length());
		}
		String assertion = 
				A.abstractState+ENDL+
				renumberedB.abstractState;
		return checkSatisfiability(varDecl,assertion);
	}
	
	/** The solver to be used. */
	private Smt smtSolver = null;
	
	public Smt getSolver()
	{
		if (smtSolver != null)
			return smtSolver;
		Smt.loadLibrary();Smt.closeStdOut();
		smtSolver = new Smt();
		if (usingLowLevelFunctions) smtSolver.loadData(knownTraces);
		return smtSolver;
	}
	
	/** Checks that <ul>
	 * <li>all states correspond to satisfiable abstract states.</li>
	 * <li>all transitions either can or cannot be taken, from all abstract states corresponding 
	 * to each DFA state.</li>
	 * <li>for each abstract state, there is at most one possible outgoing transition. Intersection
	 * of the preconditions of transitions to reject-states does not matter.
	 * </li>
	 * </ul>
	 * Returns IllegalArgumentException if any checked condition is not satisfied, null otherwise.
	 * 
	 * The choice of conditions to check is selected using the supplied configuration.
	 */
	public synchronized IllegalArgumentException checkConsistency(LearnerGraph graph,Configuration whatToCheck)
	{
		if (labelMapFinal == null) throw new IllegalArgumentException("construction incomplete");

		int variableNumber = currentNumber;// we do not intend to change currentNumber since all checks made here are transient.
		for(Entry<CmpVertex,Collection<AbstractState>> entry:graph.getVertexToAbstractState().entrySet())
		{
			if (whatToCheck.getSmtGraphDomainConsistencyCheck() == SMTGRAPHDOMAINCONSISTENCYCHECK.ALLABSTRACTSTATESEXIST)
				for(AbstractState state:entry.getValue())
					if (entry.getKey().isAccept() != checkSatisfiability(!usingLowLevelFunctions?state.variableDeclarations:"",state.abstractState))
						return new IllegalArgumentException("state "+entry.getKey()+" has an abstract state inconsistent with the accept condition");

			for(Entry<String,CmpVertex> transition:graph.transitionMatrix.get(entry.getKey()).entrySet())
			{
				Label label = labelMapFinal.get(transition.getKey());
				if (whatToCheck.getSmtGraphDomainConsistencyCheck() == SMTGRAPHDOMAINCONSISTENCYCHECK.TRANSITIONSFROMALLORNONE ||
						whatToCheck.getSmtGraphDomainConsistencyCheck() == SMTGRAPHDOMAINCONSISTENCYCHECK.DETERMINISM)
				{
					// for each transition we check that its precondition is satisfied (or not satisfied) from all abstract states
					Collection<AbstractState> previouslyConsideredAbstractStates = graph.learnerCache.getAbstractStateToLabelPreviouslyChecked().get(label);
					for(AbstractState state:entry.getValue())
						if (!previouslyConsideredAbstractStates.contains(state))
						{
							String varDeclaration = state.variableDeclarations+ENDL+
								toCurrentMem(init.pre.text, variableNumber, variableNumber+1)+ENDL;
							
							String statement = "(and "+state.abstractState+ENDL+
								toCurrentMem(labelMapFinal.get(transition.getKey()).pre.getCondition(), variableNumber, variableNumber+1)+")"+ENDL;
		
							if (transition.getValue().isAccept() != checkSatisfiability(varDeclaration,statement)) 
								return new IllegalArgumentException("from state "+entry.getKey()+" transition "+transition.getKey()+" to "+transition.getValue()+" has an accept condition incompatible with the SMT solution");
							
							varDeclaration = state.variableDeclarations+ENDL+
							toCurrentMem(init.pre.text, variableNumber, variableNumber+1)+ENDL;
	
							if (whatToCheck.getSmtGraphDomainConsistencyCheck() == SMTGRAPHDOMAINCONSISTENCYCHECK.TRANSITIONSFROMALLORNONE &&
									transition.getValue().isAccept())
							{// for each transition we may need to check that the xmachine is deterministic.
								for(Entry<String,CmpVertex> otherTransition:graph.transitionMatrix.get(entry.getKey()).entrySet())
									if (otherTransition != transition && otherTransition.getValue().isAccept())
									{
										StringBuffer variableBuffer = new StringBuffer();
										variableBuffer.append(state.abstractState);variableBuffer.append(ENDL);
										variableBuffer.append(toCurrentMem(init.pre.getCondition(), variableNumber, variableNumber+1));variableBuffer.append(ENDL);
										variableBuffer.append(toCurrentMem(init.pre.getCondition(), variableNumber, variableNumber+2));variableBuffer.append(ENDL);
										
										StringBuffer statementBuffer = new StringBuffer();
										statementBuffer.append("(and ");statementBuffer.append(ENDL);
										statementBuffer.append(state.abstractState);statementBuffer.append(ENDL);
										statementBuffer.append(toCurrentMem(labelMapFinal.get(transition.getKey()).pre.getCondition(), variableNumber, variableNumber+1));statementBuffer.append(ENDL);
										statementBuffer.append(toCurrentMem(labelMapFinal.get(otherTransition.getKey()).pre.getCondition(), variableNumber, variableNumber+2));statementBuffer.append(ENDL);
										statementBuffer.append(')');
										if (checkSatisfiability(variableBuffer.toString(), statementBuffer.toString()))
											return new IllegalArgumentException("Non-deterministic choice from state "+entry.getKey()+" transitions "+transition.getKey()+" and "+otherTransition.getKey());
									}
							}
						}
					
					
				}				

			}
		}
		return null;
	}

	/** Checks if a path condition corresponding to an abstract state is satisfiable.
	 * @return false if a path leading to the supplied state is not satisfiable. 
	 */
	protected boolean checkSatisfiability(String variableDeclarations,String condition)
	{
		Smt solver = getSolver();
		solver.pushContext();
		String whatToCheck = LabelRepresentation.getAssertionFromVarAndAxiom(variableDeclarations,condition);
		//System.err.println("CHECK: "+whatToCheck);
		solver.loadData(whatToCheck);
		boolean outcome = solver.check();
		solver.popContext();return outcome;		
	}
	
	/** Extracts an ID of a supplied vertex. */
	public static VertexID getID(CmpVertex vertex)
	{
		if (vertex.getOrigState() != null) return vertex.getOrigState();
		return vertex.getID();
	}
	
	/** Similar to CheckWithEndUser, but works via SMT. 
	 */
	public int CheckWithEndUser(List<String> question)
	{
		int pos = -1;
		List<Label> partialPath = new LinkedList<Label>();
		for(String label:question)
		{
			++pos;
			Label lbl=labelMapFinal.get(label);if (lbl == null) throw new IllegalArgumentException("unknown label "+label);
			partialPath.add(lbl);
			Pair<String,String> pair = getConjunctionForPath(partialPath,null);
			boolean outcome = checkSatisfiability(pair.firstElem,pair.secondElem);
			if (!outcome) return pos;
		}
		return AbstractOracle.USER_ACCEPTED;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + currentNumber;
		result = prime * result
				+ ((functionMap == null) ? 0 : functionMap.hashCode());
		result = prime
				* result
				+ ((functionToVariables == null) ? 0 : functionToVariables
						.hashCode());
		result = prime * result
			+ ((knownTraces == null) ? 0 : knownTraces.hashCode());
		result = prime
				* result
				+ ((labelMapConstructionOfDataTraces == null) ? 0
						: labelMapConstructionOfDataTraces.hashCode());
		result = prime
				* result
				+ ((labelMapConstructionOfOperations == null) ? 0
						: labelMapConstructionOfOperations.hashCode());
		result = prime * result
				+ ((labelMapFinal == null) ? 0 : labelMapFinal.hashCode());
		result = prime * result
				+ ((originalText == null) ? 0 : originalText.hashCode());
		result = prime * result + ((traces == null) ? 0 : traces.hashCode());
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
		if (!(obj instanceof LabelRepresentation))
			return false;
		LabelRepresentation other = (LabelRepresentation) obj;
		if (currentNumber != other.currentNumber)
			return false;
		if (functionMap == null) {
			if (other.functionMap != null)
				return false;
		} else if (!functionMap.equals(other.functionMap))
			return false;
		if (functionToVariables == null) {
			if (other.functionToVariables != null)
				return false;
		} else if (!functionToVariables.equals(other.functionToVariables))
			return false;
		if (knownTraces == null) {
			if (other.knownTraces != null)
				return false;
		} else if (!knownTraces.equals(other.knownTraces))
			return false;
		if (labelMapConstructionOfDataTraces == null) {
			if (other.labelMapConstructionOfDataTraces != null)
				return false;
		} else if (!labelMapConstructionOfDataTraces
				.equals(other.labelMapConstructionOfDataTraces))
			return false;
		if (labelMapConstructionOfOperations == null) {
			if (other.labelMapConstructionOfOperations != null)
				return false;
		} else if (!labelMapConstructionOfOperations
				.equals(other.labelMapConstructionOfOperations))
			return false;
		if (labelMapFinal == null) {
			if (other.labelMapFinal != null)
				return false;
		} else if (!labelMapFinal.equals(other.labelMapFinal))
			return false;
		if (originalText == null) {
			if (other.originalText != null)
				return false;
		} else if (!originalText.equals(other.originalText))
			return false;
		if (traces == null) {
			if (other.traces != null)
				return false;
		} else if (!traces.equals(other.traces))
			return false;
		return true;
	}
	

}

