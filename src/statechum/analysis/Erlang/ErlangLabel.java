/* Copyright (c) 2011 The University of Sheffield.
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
package statechum.analysis.Erlang;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import statechum.Label;
import statechum.analysis.Erlang.Signatures.FuncSignature;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;

/**
 *
 * @author ramsay
 */
public class ErlangLabel extends OtpErlangTuple implements Label {

    /**
     * ID for serialization.
     */
    private static final long serialVersionUID = 5192814547774363649L;
    public final int arity;
    public final FuncSignature function;
    public final OtpErlangObject input, expectedOutput;

    public ErlangLabel(FuncSignature operator, OtpErlangObject inputArgs) {
        super(new OtpErlangObject[]{
                    new OtpErlangAtom(operator.getQualifiedName()),
                    inputArgs
                });
        arity = 2;
        function = operator;
        input = inputArgs;
        expectedOutput = null;
        alphaNum = dumpErlangObject(this);
    }

    @Override
    public String toString() {
        String result = function.toString() + "(" + input + ")";
        if (expectedOutput != null) {
            result = result + " == " + expectedOutput;
        }
        return result;
    }
    
    private final String alphaNum; 
    
	@Override
	public String toErlangTerm() {
		 return alphaNum;
	}

	public static String dumpErlangObject(OtpErlangObject obj)
	{
		StringBuffer buffer = new StringBuffer();
		Class cls = obj.getClass();
		ErlangParserComponent dumper=classToDumper.get(cls);
		while(dumper == null)
		{
			cls = cls.getSuperclass();
			if (cls == null)
				throw new IllegalArgumentException("cannot dump object of type "+obj.getClass());
				
			dumper=classToDumper.get(cls);
		}
		dumper.dump(obj, buffer);
		return buffer.toString();
	}
	
	public ErlangLabel(FuncSignature operator, OtpErlangObject inputArgs, OtpErlangObject expectedOutputArgs) {
        super(new OtpErlangObject[]{
                    new OtpErlangAtom(operator.getQualifiedName()),
                    inputArgs,
                    expectedOutputArgs
                });
        arity = 3;
        function = operator;
        input = inputArgs;
        expectedOutput = expectedOutputArgs;
		alphaNum = dumpErlangObject(this);
    }

    @Override
    public int compareTo(Label other) {
        if (!(other instanceof ErlangLabel)) {
            throw new IllegalArgumentException("Comparing an ErlangLabel to something thats not an ErlangLabel");
        }

        return toErlangTerm().compareTo( ((ErlangLabel)other).toErlangTerm() );
    }


    /* No idea what this means, hence cannot fix
    public ErlangLabel replaceAll(String from, String to) {
    String[] resultContent = new String[arity];
    for (int i = 0; i < arity; i++) {
    resultContent[i] = this.elementAt(i).toString().replaceAll(from, to);
    }
    // Only arity 2 or arity 3 elements should exist...
    // This would be better handled by having two subclasses that represent the different types of Alphabet element. Maybe Ill do that tomorrow.
    if (arity == 2) {
    return new ErlangLabel(resultContent[0], resultContent[1]);
    } else {
    return new ErlangLabel(resultContent[0], resultContent[1], resultContent[2]);

    }
    }
     */
    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + arity;
        result = prime * result
                + ((expectedOutput == null) ? 0 : expectedOutput.hashCode());
        result = prime * result
                + ((function == null) ? 0 : function.hashCode());
        result = prime * result + ((input == null) ? 0 : input.hashCode());
        return result;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        return toErlangTerm().equals(((Label)obj).toErlangTerm());
    }
    

    /** Implemented by differet components of Erlang<->text parser. */
    public static interface ErlangParserComponent
    {
    	/** Turns the text of this component into text 
    	 * which can be subsequently parsed back. For instance,
    	 * string are quoted and so are atoms, hence no type conversion 
    	 * is necessary.
    	 */
    	public void dump(OtpErlangObject obj,StringBuffer resultHolder);
    	
    	/** Parses text using the provided parser and returns an Erlang 
    	 * term which corresponds to what lexer observes.
    	 *  
    	 * @param lex lexer to use
    	 * @return Erlang term
    	 */
    	public OtpErlangObject parseObject(Lexer lex);
    }

	public static class ErlangTuple implements ErlangParserComponent
    {
    	private static final ErlangTuple singleton = new ErlangTuple();
    	public static ErlangTuple getSingleton()
    	{
    		return singleton;
    	}

    	@Override
		public void dump(OtpErlangObject arg,StringBuffer resultHolder)
    	{
    		OtpErlangTuple tuple = (OtpErlangTuple)arg;
    		
    		resultHolder.append("{");
    		boolean first = true;
    		for(OtpErlangObject obj:tuple.elements())
    		{
    			if (!first)	resultHolder.append(',');else first = false;
    			classToDumper.get(obj.getClass()).dump(obj,resultHolder);
    		}
    		resultHolder.append("}");
    	}

		@Override
		public OtpErlangObject parseObject(Lexer lexer) {
			assert lexer.getLastMatchType() == erlTupleBegin;
			
			List<OtpErlangObject> tupleComponents = new LinkedList<OtpErlangObject>();
			
			// Parser state.
			boolean expectComma = false, pullNextToken = true;
			int currentMatch = lexer.getMatchType();
			while(currentMatch != erlTupleEnd)
			{// parsing ErlangTuple
				if (currentMatch < 0)
					throw new IllegalArgumentException("unexpected end of tuple");

				switch(currentMatch)
				{
				case erlTupleBegin:
				case erlListBegin:
				case erlAtomQuote:
				case erlString:
					if (expectComma)
						throw new IllegalArgumentException("expecting comma in parsing tuple, looking at "+lexer.getMatch());
					tupleComponents.add(tokenToParser.get(currentMatch).parseObject(lexer));
					expectComma = true;
					break;
				case erlPositiveNumber:
				case erlNegativeNumber:
				case erlText:
					if (expectComma)
						throw new IllegalArgumentException("expecting comma in parsing tuple");
					tupleComponents.add(tokenToParser.get(currentMatch).parseObject(lexer));
					expectComma = true;pullNextToken = false;
					break;
				case erlComma:
					if (!expectComma)
						throw new IllegalArgumentException("unexpected comma in parsing tuple, looking at "+lexer.getMatch());
					expectComma = false;
					break;
				default:
					throw new IllegalArgumentException("invalid token type "+currentMatch+" in parsing tuple, looking at "+lexer.getMatch());
				}
				if (pullNextToken)
					currentMatch = lexer.getMatchType();
				else
				{// use the last token but pull the next one next time 'round
					currentMatch = lexer.getLastMatchType();pullNextToken = true;
				}
			}
			return new OtpErlangTuple(tupleComponents.toArray(new OtpErlangObject[0]));
		}
    	
    }
    
    public static class ErlangList implements ErlangParserComponent
    {
    	private static final ErlangList singleton = new ErlangList();
    	public static ErlangList getSingleton()
    	{
    		return singleton;
    	}

    	@Override
		public void dump(OtpErlangObject arg,StringBuffer resultHolder)
    	{
    		OtpErlangList list = (OtpErlangList)arg;
    		
    		resultHolder.append("[");
    		boolean first = true;
    		for(OtpErlangObject obj:list.elements())
    		{
    			if (!first)	resultHolder.append(',');else first = false;
    			classToDumper.get(obj.getClass()).dump(obj,resultHolder);
    		}
    		resultHolder.append("]");
    	}
    	
		@Override
		public OtpErlangObject parseObject(Lexer lexer) {
			assert lexer.getLastMatchType() == erlListBegin;
			
			List<OtpErlangObject> listComponents = new LinkedList<OtpErlangObject>();
			
			// Parser state.
			boolean expectComma = false, pullNextToken = true;
			int currentMatch = lexer.getMatchType();
			while(currentMatch != erlListEnd)
			{// parsing ErlangList
				if (currentMatch < 0)
					throw new IllegalArgumentException("unexpected end of list");

				switch(currentMatch)
				{
				case erlTupleBegin:
				case erlListBegin:
				case erlAtomQuote:
				case erlString:
					if (expectComma)
						throw new IllegalArgumentException("expecting comma in parsing list");
					listComponents.add(tokenToParser.get(currentMatch).parseObject(lexer));
					expectComma = true;
					break;
				case erlPositiveNumber:
				case erlNegativeNumber:
				case erlText:
					if (expectComma)
						throw new IllegalArgumentException("expecting comma in parsing list");
					listComponents.add(tokenToParser.get(currentMatch).parseObject(lexer));
					expectComma = true;pullNextToken = false;
					break;
				case erlComma:
					if (!expectComma)
						throw new IllegalArgumentException("unexpected comma in parsing list, looking at "+lexer.getMatch());
					expectComma = false;
					break;
				default:
					throw new IllegalArgumentException("invalid token type "+currentMatch+" in parsing list, looking at "+lexer.getMatch());
				}
				if (pullNextToken)
					currentMatch = lexer.getMatchType();
				else
				{// use the last token but pull the next one next time 'round
					currentMatch = lexer.getLastMatchType();pullNextToken = true;
				}
			}
			return new OtpErlangList(listComponents.toArray(new OtpErlangObject[0]));
		}
    }
    
    public static class ErlangQuotedAtom implements ErlangParserComponent
    {
    	private static final ErlangQuotedAtom singleton = new ErlangQuotedAtom();
    	public static ErlangQuotedAtom getSingleton()
    	{
    		return singleton;
    	}

    	protected static final Set<Character> whatToQuoteForAtom = new HashSet<Character>();
    	
    	static
    	{
    		whatToQuoteForAtom.add('\'');whatToQuoteForAtom.add('\\');
    	}
    	
    	@Override
		public void dump(OtpErlangObject arg,StringBuffer resultHolder)
    	{
    		OtpErlangAtom atom = (OtpErlangAtom)arg;
    		resultHolder.append('\'');
    		stringToText(atom.atomValue(),whatToQuoteForAtom,resultHolder);
    		resultHolder.append('\'');
    	}
    	
		@Override
		public OtpErlangObject parseObject(Lexer lexer) 
		{
			assert lexer.getLastMatchType() == erlAtomQuote;
			StringBuffer atomText = new StringBuffer();
			
			// parser state
			boolean expectedChar = false, finished = false;
			
			while(!finished)
			{
				int currentMatch = lexer.getMatchType();
				if (currentMatch < 0)
					throw new IllegalArgumentException("unexpected end of atom");

				switch(currentMatch)
				{// parsing ErlangAtom
				case erlTupleBegin:
				case erlTupleEnd:
				case erlListBegin:
				case erlListEnd:
				case erlString:
					atomText.append(lexer.getMatch());expectedChar = false;
					break;
				case erlBackslash:
					if (expectedChar)
					{// this char will be quoted
						atomText.append(lexer.getMatch());expectedChar = false;
					}
					else
						expectedChar = true;
					break;					
				case erlAtomQuote:
					if (expectedChar)
					{// this char will be quoted
						atomText.append(lexer.getMatch());expectedChar = false;
					}
					else
						finished = true;
					break;
				case erlPositiveNumber:
				case erlNegativeNumber:
				case erlText:
				case erlComma:
				case erlSpaces:
					if (expectedChar)
						// first character quoted - we do not support special chars in atoms because they are not supposed to be quoted anyway.
						throw new IllegalArgumentException("atom parser: character "+lexer.getMatch().charAt(0)+" is not supposed to be prefixed by backslash");
					
					atomText.append(lexer.getMatch());
					break;
				default:
					throw new IllegalArgumentException("invalid token type "+currentMatch+" in parsing atom, looking at "+lexer.getMatch());
				}
			}
				
			return new OtpErlangAtom(atomText.toString());
		}
   }
    
    public static class ErlangUnquotedAtom extends ErlangQuotedAtom
    {
    	private static final ErlangUnquotedAtom singleton = new ErlangUnquotedAtom();
    	public static ErlangUnquotedAtom getSingleton()
    	{
    		return singleton;
    	}

    	@Override
		public OtpErlangObject parseObject(Lexer lexer) 
		{
			assert lexer.getLastMatchType() == erlText;
			StringBuffer atomText = new StringBuffer();
			
			// parser state
			boolean finished = false;
			int currentMatch = lexer.getLastMatchType();
			while(!finished && currentMatch >= 0)
			{
				switch(currentMatch)
				{// parsing ErlangAtom
				case erlString:
				case erlAtomQuote:
				case erlBackslash:
						throw new IllegalArgumentException("unquoted atom parser: "+lexer.getMatch()+" is never allowed in an atom without quotes");
				case erlPositiveNumber:
				case erlNegativeNumber:
				case erlText:
					atomText.append(lexer.getMatch());break;
				case erlTupleBegin:
				case erlListBegin:
				case erlTupleEnd:
				case erlListEnd:
				case erlSpaces:
				case erlComma:
					finished = true;break;
				default:
					throw new IllegalArgumentException("invalid token type "+currentMatch+" in parsing unquoted atom, looking at "+lexer.getMatch());
				}
				if (!finished) currentMatch = lexer.getMatchType();
			}
			assert atomText.length() > 0;
				
			return new OtpErlangAtom(atomText.toString());
		}
   }
    
    public static class ErlangString implements ErlangParserComponent
    {
    	private static final ErlangString singleton = new ErlangString();
    	public static ErlangString getSingleton()
    	{
    		return singleton;
    	}

    	protected static final Set<Character> whatToQuoteForString = new HashSet<Character>();
    	
     	static
    	{
     		whatToQuoteForString.add('\"');whatToQuoteForString.add('\\');
    	}
   
     	@Override
		public void dump(OtpErlangObject arg,StringBuffer resultHolder)
    	{
    		OtpErlangString atom = (OtpErlangString)arg;
    		resultHolder.append('\"');
    		stringToText(atom.stringValue(),whatToQuoteForString,resultHolder);
    		resultHolder.append('\"');
    	}

		@Override
		public OtpErlangObject parseObject(Lexer lexer) 
		{
			assert lexer.getLastMatchType() == erlString;
			StringBuffer stringText = new StringBuffer();
			
			// parser state
			boolean expectedChar = false, finished = false;
			
			while(!finished)
			{
				int currentMatch = lexer.getMatchType();
				if (currentMatch < 0)
					throw new IllegalArgumentException("unexpected end of string");

				switch(currentMatch)
				{// parsing ErlangString
				case erlTupleBegin:
				case erlTupleEnd:
				case erlListBegin:
				case erlListEnd:
				case erlAtomQuote:
					stringText.append(lexer.getMatch());expectedChar = false;
					break;
				case erlBackslash:
					if (expectedChar)
					{// this char will be quoted
						stringText.append(lexer.getMatch());expectedChar = false;
					}
					else
						expectedChar = true;
					break;
				case erlString:
					if (expectedChar)
					{// this char will be quoted
						stringText.append(lexer.getMatch());expectedChar = false;
					}
					else
						finished = true;
					break;
				case erlPositiveNumber:
				case erlNegativeNumber:
				case erlText:
				case erlComma:
				case erlSpaces:
					if (expectedChar)
						// first character quoted - we do not support special chars in string because they are not supposed to be quoted anyway.
						throw new IllegalArgumentException("string parser: character "+lexer.getMatch().charAt(0)+" is not supposed to be prefixed by backslash");
					
					stringText.append(lexer.getMatch());break;
				default:
					throw new IllegalArgumentException("invalid token type "+currentMatch+" in parsing tuple, looking at "+lexer.getMatch());
				}
			}
				
			return new OtpErlangString(stringText.toString());
		}
    }
    
    public static class ErlangLong implements ErlangParserComponent
    {
    	private static final ErlangLong singleton = new ErlangLong();
    	public static ErlangLong getSingleton()
    	{
    		return singleton;
    	}

    	@Override
		public void dump(OtpErlangObject arg,StringBuffer resultHolder)
    	{
       		OtpErlangLong longValue = (OtpErlangLong)arg;
       		resultHolder.append(longValue.longValue());
    	}   	
		@Override
		public OtpErlangObject parseObject(Lexer lexer) 
		{
			assert lexer.getLastMatchType() == erlPositiveNumber || lexer.getLastMatchType() == erlNegativeNumber;
			long outcome = Long.parseLong(lexer.getMatch().trim());
			
			int currentMatch = lexer.getMatchType();
			if (currentMatch >= 0)
				switch(currentMatch)
				{
				case erlPositiveNumber:
				case erlNegativeNumber:
				case erlText:
					throw new IllegalArgumentException("invalid token type "+currentMatch+" in parsing erlang number, looking at "+lexer.getMatch());
				}
			OtpErlangObject result = null;
			if (outcome > Integer.MAX_VALUE || outcome < Integer.MIN_VALUE)
				result = new OtpErlangLong(outcome);
			else
				result = new OtpErlangInt((int)outcome);
			
			return result;
		}
   }
    
    public static class ErlangInt implements ErlangParserComponent
    {
    	private ErlangInt() {}
    	
    	private static final ErlangInt singleton = new ErlangInt();
    	public static ErlangInt getSingleton()
    	{
    		return singleton;
    	}
    	
       	@Override
		public void dump(OtpErlangObject arg,StringBuffer resultHolder)
    	{
       		OtpErlangInt longValue = (OtpErlangInt)arg;
       		resultHolder.append(longValue.longValue());
    	}
       	
		@Override
		public OtpErlangObject parseObject(@SuppressWarnings("unused") Lexer lexer) 
		{
			throw new UnsupportedOperationException("all numbers are supposed to be parsed as long numbers and subsequently converted to int if they are small enough");
		}
   }
    
    public static void stringToText(String str,Set<Character> whatToQuote,StringBuffer result)
    {
    	for(int i=0;i<str.length();++i)
    	{
    		char ch = str.charAt(i);
    		if (whatToQuote.contains(ch))
    			result.append('\\');
    		result.append(ch);
    	}    			
    }
    
	public static final int erlTupleBegin = 1;
	public static final int erlTupleEnd = 2;
	public static final int erlListBegin = 3;
	public static final int erlListEnd = 4;
	public static final int erlAtomQuote = 5;
	public static final int erlString = 6;
	public static final int erlPositiveNumber = 7;
	public static final int erlNegativeNumber = 8;
	public static final int erlBackslash = 9;
	public static final int erlComma = 10;
	public static final int erlSpaces = 11;
    public static final int erlText = 12;
    
    public static Lexer buildLexer(String whatToParse)
    {
		return new Lexer(
				"(\\s*\\{\\s*)|" + // erlTupleBegin
				"(\\s*}\\s*)|" + // erlTupleEnd
				"(\\s*\\[\\s*)|" + // erlListBegin
				"(\\s*]\\s*)|" + // erlListEnd
				"(\\s*\'\\s*)|" + // erlAtomQuote
				"(\\s*\"\\s*)|" + // erlString
				"(\\s*\\d+\\s*)|" +// erlPositiveNumber
				"(\\s*-\\d+\\s*)|" +// erlNegativeNumber
				"(\\\\)|" + // erlBackslash
				"( *, *)|"+// erlComma
				"( +)|"+// erlSpaces
				"([^\\\\\"\',\\[\\]{} ]+)" // erlText, together with spaces but none of the special characters above.
				,whatToParse);

    }
    
    /** Given a string containing the first term of the expression to parse, parses the text and returns the
     * corresponding Erlang term.
     *  
     * @param str label to parse
     * @return the outcome.
     */
    public static OtpErlangObject parseFirstTermInText(Lexer lexer)
    {
		int currentMatch = lexer.getMatchType();
		while(currentMatch == erlSpaces)
		{// parsing ErlangTuple
			
			currentMatch = lexer.getMatchType();
		}
		if (currentMatch < 0)
			throw new IllegalArgumentException("empty term");
		
		OtpErlangObject result = null;
		switch(currentMatch)
		{
		case erlTupleBegin:
		case erlListBegin:
		case erlAtomQuote:
		case erlString:
			result = tokenToParser.get(currentMatch).parseObject(lexer);
			currentMatch = lexer.getMatchType();
			break;
		case erlPositiveNumber:
		case erlNegativeNumber:
		case erlText:
			result = tokenToParser.get(currentMatch).parseObject(lexer);
			currentMatch = lexer.getLastMatchType();
			break;
		default:
			throw new IllegalArgumentException("invalid token type "+currentMatch+" in parsing erlang term, looking at "+lexer.getMatch());
		}
		while(currentMatch == erlSpaces)
		{// parsing ErlangTuple
			
			currentMatch = lexer.getMatchType();
		}
		assert result != null;
		return result;
    }
    
    /** Given a string containing the whole of the expression to parse, parses the text and returns the
     * corresponding Erlang term.
     *  
     * @param str text to parse
     * @return the outcome.
     */
    public static OtpErlangObject parseText(String str)
    {
    	Lexer lexer = buildLexer(str);
    	OtpErlangObject result = parseFirstTermInText(lexer);
		if (lexer.getLastMatchType() >= 0)// did not get to the end of string
			throw new IllegalArgumentException("unexpected characters at the end of string to parse, looking at "+lexer.getMatch());
		return result;
    }
   
    protected final static Map<Class<? extends OtpErlangObject>,ErlangParserComponent> classToDumper; 
    static {
    	classToDumper =new HashMap<Class<? extends OtpErlangObject>,ErlangParserComponent>();
    	classToDumper.put(OtpErlangTuple.class,ErlangTuple.getSingleton());
    	classToDumper.put(OtpErlangList.class,ErlangList.getSingleton());
    	classToDumper.put(OtpErlangAtom.class,ErlangQuotedAtom.getSingleton());
    	classToDumper.put(OtpErlangString.class,ErlangString.getSingleton());
    	classToDumper.put(OtpErlangLong.class,ErlangLong.getSingleton());
    	classToDumper.put(OtpErlangInt.class,ErlangInt.getSingleton());
    }
    protected final static Map<Integer,ErlangParserComponent> tokenToParser;
    static {
    	tokenToParser = new TreeMap<Integer,ErlangParserComponent>();
    	
    	tokenToParser.put(erlTupleBegin, ErlangTuple.getSingleton());
    	tokenToParser.put(erlListBegin, ErlangList.getSingleton());
    	tokenToParser.put(erlAtomQuote, ErlangQuotedAtom.getSingleton());
    	tokenToParser.put(erlString, ErlangString.getSingleton());
    	tokenToParser.put(erlPositiveNumber, ErlangLong.getSingleton());
    	tokenToParser.put(erlNegativeNumber, ErlangLong.getSingleton());
    	tokenToParser.put(erlText, ErlangUnquotedAtom.getSingleton());
    }    
    
    /** Given a string containing the whole of the expression to parse, parses the text and returns the
     * corresponding Erlang label.
     *  
     * @param str label to parse
     * @return the outcome.
     */
    public static Label parseLabel(String str)
    {
    	OtpErlangObject obj = parseText(str);
    	if (!(obj instanceof OtpErlangTuple))
    		throw new IllegalArgumentException("expected Erlang label, parsed "+obj);
    	OtpErlangTuple tuple = (OtpErlangTuple)obj;
    	
    	if (tuple.arity() < 2 || tuple.arity() > 3)
    		throw new IllegalArgumentException("expected Erlang label, got tuple of arity "+tuple.arity()+" in "+obj);
    	
    	// map of names to signatures is to be stored in OtpErlangModule or somewhere, hence ErlangTrace cannot have parse as a static method.
    	
    	return null;
    }
}
