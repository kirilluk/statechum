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

import java.math.BigInteger;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBitstr;
import com.ericsson.otp.erlang.OtpErlangBoolean;
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
    /** A function might be called wibble:handle_call/3 but we have to use the name of "call" when making a call to Erlang. */
    public final String callName;

    public ErlangLabel(FuncSignature operator, String shortName, OtpErlangObject inputArgs) {
        super(new OtpErlangObject[]{
                    new OtpErlangAtom(shortName),
                    inputArgs
                });
        arity = 2;
        function = operator;callName = shortName;
        input = inputArgs;
        expectedOutput = null;
        alphaNum = buildFunctionSignatureAsString();
    }

    protected String buildFunctionSignatureAsString()
    {// {File, LineNo, F, A,fun_to_Statechum(erl_types:t_fun(ArgType, RetType),Info#info.recMap)}
    	StringBuffer resultHolder = new StringBuffer();
    	resultHolder.append('{');resultHolder.append(function.toErlangTerm());resultHolder.append(',');
    	ErlangLabel.ErlangString.getSingleton().dump(callName,resultHolder);resultHolder.append(",");
    	resultHolder.append(arity);resultHolder.append(',');
       	resultHolder.append(dumpErlangObject(input));
       	if (expectedOutput != null) {resultHolder.append(',');resultHolder.append(dumpErlangObject(expectedOutput)); }
       	resultHolder.append('}');
       	return resultHolder.toString();
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
		ErlangParserComponent dumper = classToDumper.get(cls);
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
	
	public ErlangLabel(FuncSignature operator, String shortName, OtpErlangObject inputArgs, OtpErlangObject expectedOutputArgs) {
        super(new OtpErlangObject[]{
                    new OtpErlangAtom(shortName),
                    inputArgs,
                    expectedOutputArgs
                });
        arity = 3;
        function = operator;callName = shortName;
        input = inputArgs;
        expectedOutput = expectedOutputArgs;
		alphaNum = buildFunctionSignatureAsString();
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
				case erlBitStrBegin:
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
				case erlBitStrBegin:
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
				{// parsing quoted ErlangAtom
				case erlTupleBegin:
				case erlTupleEnd:
				case erlListBegin:
				case erlListEnd:
				case erlBitStrBegin:
				case erlBitStrEnd:
				case erlString:
				case erlGT:
				case erlLT:
				case erlCol:
				case erlMinus:
				case erlSlash:
				case erlSpaces:
				case erlComma:
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
    
    /** Parsing is done by the UnquotedAtom class since booleans are unquoted. */
    public static class ErlangBoolean implements ErlangParserComponent
    {
    	private static final ErlangBoolean singleton = new ErlangBoolean();
    	public static ErlangBoolean getSingleton()
    	{
    		return singleton;
    	}
    	
    	public static final String True = "true", False = "false";
    	
		@Override
		public void dump(OtpErlangObject obj, StringBuffer resultHolder) {
			if ( ((OtpErlangBoolean)obj).booleanValue() )
				resultHolder.append(True);
			else
				resultHolder.append(False);
		}
		
		@Override
		public OtpErlangObject parseObject(@SuppressWarnings("unused") Lexer lex) {
			throw new UnsupportedOperationException("parsing is supposed to be done by ErlangUnquotedAtom");
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
				{// parsing Unquoted ErlangAtom
				case erlString:
				case erlAtomQuote:
				case erlBackslash:
				case erlGT: // Erlang interprets statements with these as expressions and attempts to evaluate them, we simply ban 'em
				case erlLT:// Erlang interprets statements with these as expressions and attempts to evaluate them, we simply ban 'em
				case erlCol:
				case erlMinus:
				case erlSlash:
					throw new IllegalArgumentException("unquoted atom parser: "+lexer.getMatch()+" is never allowed in an atom without quotes");
				case erlPositiveNumber:
				case erlNegativeNumber:
				case erlText:
					atomText.append(lexer.getMatch());break;
				case erlTupleBegin:
				case erlListBegin:
				case erlTupleEnd:
				case erlListEnd:
				case erlBitStrBegin:
				case erlBitStrEnd:
				case erlSpaces:
				case erlComma:
					finished = true;break;
				default:
					throw new IllegalArgumentException("invalid token type "+currentMatch+" in parsing unquoted atom, looking at "+lexer.getMatch());
				}
				if (!finished) currentMatch = lexer.getMatchType();
			}
			assert atomText.length() > 0;
			
			OtpErlangObject outcome = null;
			if (atomText.toString().equals(ErlangBoolean.True))
				outcome = new OtpErlangBoolean(true);
			else
			if (atomText.toString().equals(ErlangBoolean.False))
				outcome = new OtpErlangBoolean(false);
			else
				outcome = new OtpErlangAtom(atomText.toString());
			
			return outcome;
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
    		dump(atom.stringValue(),resultHolder);
     	}

     	/** Similar to the above but for normal strings. */
     	public void dump(String str, StringBuffer resultHolder)
     	{
       		resultHolder.append('\"');
    		stringToText(str,whatToQuoteForString,resultHolder);
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
				case erlBitStrBegin:
				case erlBitStrEnd:
				case erlAtomQuote:
				case erlGT:
				case erlLT:
				case erlCol:
				case erlMinus:
				case erlSlash:
				case erlComma:
				case erlSpaces:
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
    
    public static class ErlangBitString implements ErlangParserComponent
    {
    	private static final ErlangBitString singleton = new ErlangBitString();
    	public static ErlangBitString getSingleton()
    	{
    		return singleton;
    	}
		@Override
		public void dump(OtpErlangObject obj, StringBuffer resultHolder) {
			OtpErlangBitstr bitStr = (OtpErlangBitstr) obj;
			resultHolder.append("<< ");
			byte [] bitStringData = bitStr.binaryValue();
			if (bitStr.size() != bitStringData.length)
				throw new IllegalArgumentException("the number of bits in the string is not divisible by 8, conversion cannot be accurately performed");
			boolean first = true;
			for(byte b:bitStringData)
			{
				if(!first) resultHolder.append(", ");else first = false;
				resultHolder.append( b & 0xff);
			}
			resultHolder.append(">>");
		}
		
		public static enum ParseState { NUM_OR_END, SCOLON_COMMA_END, SIZE, SLASH_COMMA_END,
				TYPE, MINUS_COMMA_END, UCOLON, UNIT, COMMA_END, NUM, FINISHED };
		
		public static final Set<String> typeStringsIgnored;
		
		static
		{
			Set<String> strings = new TreeSet<String>();
			strings.add("integer");
			typeStringsIgnored = Collections.unmodifiableSet(strings);
		}

		protected class SegmentParameters
		{
			// default values
			int unit = 1, size = 8;
			boolean littleEndian = false;
			
			final BigInteger number;
			int lengthOfNumber;
			
			/** Only used to detect inconsistent type declarations. */
			boolean isSigned = false, isUnsigned = false, isBig = false, isLittle = false;
			
			public SegmentParameters(String text)
			{
				number=new BigInteger(text);lengthOfNumber = number.bitCount();
			}
						
			/** Handles all type strings exceptin Unit which requires parser assistance. */
			public void processTypeAttribute(String type)
			{
				if (type.equals("little"))
				{
					if (isBig)
						throw new IllegalArgumentException("type mismatched: already big-endian");
					isLittle = true;littleEndian = true;
				}
				else
				if (type.equals("big"))
				{
					if (isLittle)
						throw new IllegalArgumentException("type mismatched: already little-endian");
					isBig = true;littleEndian = false;
				}
				else
				if (type.equals("signed"))
				{
					if (isUnsigned)
						throw new IllegalArgumentException("type mismatched: already unsigned");
					isSigned = true;
				}
				else
				if (type.equals("unsigned"))
				{
					if (isSigned)
						throw new IllegalArgumentException("type mismatched: already signed");
					isUnsigned = true;
				}
				else
					if (!typeStringsIgnored.contains(type))
						throw new IllegalArgumentException("unknown type specifier "+type);
			}
			
			/** Given the byte from our big number and the length of the big number, 
			 * it updates our last byte, data and returns the remaining length of the big number.
			 * @param byteToAdd byte to store
			 * @param len current length of big number
			 * @return the remaining length of big number
			 */
			protected int addBigEndianByte(byte byteToAdd, int lenArg)
			{
				int len = lenArg;
				int bitsUsedInLeftmostByte = len%8 == 0? 8:len%8;
				int bitsSpareMinusBitsNeeded = (8-(totalLen%8))-bitsUsedInLeftmostByte;
				if (bitsSpareMinusBitsNeeded < 0)
				{// last byte: UUUUU...
				 //  new byte: ...XXXXXX, bitsSpareMinusBitsNeeded is what will not fit in the last byte
					int rightShift =-bitsSpareMinusBitsNeeded; 
					// Masking is from http://stackoverflow.com/questions/11088/what-is-the-best-way-to-work-around-the-fact-that-all-java-bytes-are-signed
					lastByte |= (byte)((byteToAdd >> rightShift) &((1 << (8-rightShift))-1));
					data.add(lastByte);
					lastByte = (byte)( byteToAdd << (8-rightShift) );// make the last byte big-endian
					
					int bitsAdded = bitsUsedInLeftmostByte;
					totalLen+=bitsAdded;len-=bitsAdded;
				}
				else
				{// enough space in the last byte
				 // last byte: UUU.....
				 //  new byte: ......XX, bitsSpareMinusBitsNeeded is what will remain in the last byte
					lastByte |= (byte)(byteToAdd << bitsSpareMinusBitsNeeded);
					int bitsAdded = bitsUsedInLeftmostByte;
					totalLen+=bitsAdded;len-=bitsAdded;
					if (totalLen % 8 == 0)// last byte is full
					{
						data.add(lastByte);lastByte=0;
					}
				}
				
				return len;
			}
			
			/** Appends the byte representation of the data in this segment to the collection. */
			public void appendNumbers()
			{
				// data in the last byte is always big-endian
				int len = unit*size; 
				if (len == 0)
					return;// empty number, do nothing
				if (lengthOfNumber > len)
					throw new IllegalArgumentException("Number with "+lengthOfNumber+" bits will not fit inside "+len+"-bit segment");
				BigInteger num = number.mod(new BigInteger(new byte[]{1}).shiftLeft(len));
				byte []integerData = new byte[len % 8 > 0?1+len/8:len/8];
				
				// the number of bytes in this array may be arbitrary, possibly greater or lower
				// than integerData.length, copying ensures that we have as many bytes there
				// as we expect.
				byte []numberData = num.toByteArray();
				for(int i=0;i<Math.min(integerData.length, numberData.length);++i)
					integerData[integerData.length-1-i]=numberData[numberData.length-1-i];
				
				num.toByteArray();
				if (!littleEndian)
				{// dumping big-endian
					for(int i=0;i<integerData.length;++i)
					{
						byte currentByte=integerData[i];
						len = addBigEndianByte(currentByte,len);
					}
				}
				else
				{// little-endian case
					for(int i=integerData.length-1;i>0;--i)
					{// not enough (or just enough) space to fit in the last byte (we are trying to fit an entire byte in it).
						byte currentByte=integerData[i];
						int bitsSpare = (8-(totalLen%8));
						int bitsUsed = 8-bitsSpare;
						lastByte |= (byte)( (currentByte >> bitsUsed) & ((1 << bitsSpare)-1));
						data.add(lastByte);
						lastByte = (byte)( (currentByte << bitsSpare ) );// the spillover goes in the low bits of the next byte - we are little-endian
						totalLen+=8;len-=8;
					}
					// last byte
					byte currentByte=integerData[0];
					addBigEndianByte(currentByte, len);
				}
			}
		}
		
		List<Byte> data = new LinkedList<Byte>();
		byte lastByte=0;
		int totalLen = 0;
		
		@Override
		public OtpErlangObject parseObject(Lexer lexer) 
		{// I need a new instance of this object for each bit string, hence have to create it here (on demand).
			return new ErlangBitString().parseObjectInternal(lexer);
		}
		public OtpErlangObject parseObjectInternal(Lexer lexer) 
		{
			assert lexer.getLastMatchType() == erlBitStrBegin;
			
			// parser state
			boolean pullNextToken = true;
			ParseState state = ParseState.NUM_OR_END;
			
			SegmentParameters currentSegment = null;
			while(state != ParseState.FINISHED)
			{
				int currentMatch =-1;
				if (pullNextToken)
					currentMatch = lexer.getMatchType();
				else
				{// use the last token but pull the next one next time 'round
					currentMatch = lexer.getLastMatchType();pullNextToken = true;
				}
				
				if (currentMatch < 0)
					throw new IllegalArgumentException("unexpected end of bit string");

				switch(currentMatch)
				{// parsing quoted ErlangAtom
				case erlBitStrEnd:
					switch(state)
					{
					case NUM_OR_END:
					case SCOLON_COMMA_END:
					case SLASH_COMMA_END:
					case MINUS_COMMA_END:
					case COMMA_END:
						if (currentSegment != null) currentSegment.appendNumbers();
						if (totalLen % 8 != 0) data.add(lastByte);
						state = ParseState.FINISHED;break;
					default:
						throw new IllegalArgumentException("BitStr parser: got "+lexer.getMatch()+" in "+state+ " state");
					}
					break;
				case erlCol:
					switch(state)
					{
					case SCOLON_COMMA_END:
						state = ParseState.SIZE;
						break;
					case UCOLON:
						state = ParseState.UNIT;
						break;
					default:
						throw new IllegalArgumentException("BitStr parser: got "+lexer.getMatch()+" in "+state+ " state");
					}
					break;
				case erlMinus:
					if (state != ParseState.MINUS_COMMA_END)
						throw new IllegalArgumentException("BitStr parser: dash can only be used after a type, looking at "+lexer.getMatch());
					state = ParseState.TYPE;
					break;
				case erlSpaces:
					break;// ignore this
				case erlComma:
					switch(state)
					{
					case SCOLON_COMMA_END:
					case SLASH_COMMA_END:
					case MINUS_COMMA_END:
					case COMMA_END:
						state = ParseState.NUM;
						break;
					default:
						throw new IllegalArgumentException("BitStr parser: got "+lexer.getMatch()+" in "+state+ " state");
					}
					break;
				case erlSlash:
					if (state != ParseState.SLASH_COMMA_END)
						throw new IllegalArgumentException("BitStr parser: slash can only be used after NUMBER:SIZE declaration, looking at "+lexer.getMatch());
					state = ParseState.TYPE;
					break;
				case erlPositiveNumber:
				case erlNegativeNumber:
					switch(state)
					{
					case NUM:
					case NUM_OR_END:
						// NUMBER:unit/... reading this NUMBER
						if (currentSegment != null) currentSegment.appendNumbers();
						currentSegment = new SegmentParameters(lexer.getMatch().trim());
						state = ParseState.SCOLON_COMMA_END;
						break;
					case SIZE:
						currentSegment.size = Integer.parseInt(lexer.getMatch().trim());
						state = ParseState.SLASH_COMMA_END;
						break;
					case UNIT:
						currentSegment.unit = Integer.parseInt(lexer.getMatch().trim());
						state = ParseState.COMMA_END;
						break;
					default:
						throw new IllegalArgumentException("BitStr parser: got "+lexer.getMatch()+" in "+state+ " state");
					}
					break;
				case erlText:
					if (state != ParseState.TYPE)
						throw new IllegalArgumentException("BitStr parser: type is unexpected in this position, looking at "+lexer.getMatch());
					if (lexer.getMatch().equals("unit"))
						state = ParseState.UCOLON;
					else
					{
						currentSegment.processTypeAttribute(lexer.getMatch());
						state = ParseState.MINUS_COMMA_END;
					}
					break;
				default:
					throw new IllegalArgumentException("invalid token type "+currentMatch+" in parsing atom, looking at "+lexer.getMatch());
				}
			}
			
			if (totalLen % 8 != 0) 
				throw new IllegalArgumentException("the length of bit string should be divisible by 8");
			byte []bitData = new byte[data.size()];
			int i=0;for(Byte b:data) bitData[i++]=b.byteValue();
			return new OtpErlangBitstr(bitData);
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
    
    protected static void stringToText(String str,Set<Character> whatToQuote,StringBuffer result)
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
	public static final int erlBitStrBegin = 5;
	public static final int erlBitStrEnd = 6;
	public static final int erlAtomQuote = 7;
	public static final int erlString = 8;
	public static final int erlPositiveNumber = 9;
	public static final int erlNegativeNumber = 10;
	public static final int erlBackslash = 11;
	public static final int erlComma = 12;
	public static final int erlSpaces = 13;
	public static final int erlGT = 14;
	public static final int erlLT = 15;
	public static final int erlCol = 16;
	public static final int erlMinus = 17;
	public static final int erlSlash = 18;
    public static final int erlText = 19;
    
    public static Lexer buildLexer(String whatToParse)
    {
		return new Lexer(
				"(\\s*\\{\\s*)|" + // erlTupleBegin
				"(\\s*}\\s*)|" + // erlTupleEnd
				"(\\s*\\[\\s*)|" + // erlListBegin
				"(\\s*]\\s*)|" + // erlListEnd
				"(\\s*<<\\s*)|" + // erlBitStrBegin
				"(\\s*>>\\s*)|" + // erlBitStrEnd
				"(\\s*\'\\s*)|" + // erlAtomQuote
				"(\\s*\"\\s*)|" + // erlString
				"(\\s*\\d+\\s*)|" +// erlPositiveNumber
				"(\\s*-\\d+\\s*)|" +// erlNegativeNumber
				"(\\\\)|" + // erlBackslash
				"( *, *)|"+// erlComma
				"( +)|"+// erlSpaces
				"(>)|"+// erlGT
				"(<)|"+// erlLT
				"(:)|"+// erlCol
				"(-)|"+// erlMinus
				"(/)|"+// erlSlash
				"([^\\\\\"\',\\[\\]{}<>:\\-/ ]+)" // erlText, together with spaces but none of the special characters above.
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
		case erlBitStrBegin:
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
    	classToDumper.put(OtpErlangBoolean.class,ErlangBoolean.getSingleton());
    	classToDumper.put(OtpErlangString.class,ErlangString.getSingleton());
    	classToDumper.put(OtpErlangLong.class,ErlangLong.getSingleton());
    	classToDumper.put(OtpErlangInt.class,ErlangInt.getSingleton());
    	classToDumper.put(OtpErlangBitstr.class,ErlangBitString.getSingleton());
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
    	tokenToParser.put(erlBitStrBegin,ErlangBitString.getSingleton());
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
    	// moreover, we have no idea where to get a function for this label  - the first element of the tuple is not enough :/  
    	return null;
    }
    
    /** Given a string containing the whole of the expression to parse, parses the text and returns the
     * corresponding Erlang label.
     *  
     * @param str label to parse
     * @return the outcome.
     */
    public static List<Label> parseTrace(String str)
    {
    	OtpErlangObject obj = parseText("["+str+"]");
    	assert obj instanceof OtpErlangList;
    	OtpErlangList list = (OtpErlangList)obj;
    	List<Label> outcome = new LinkedList<Label>();
    	/*for(OtpErlangObject o:list.elements())
    		outcome.add(o);
    	// need module information 
    		*
    		*/
    	
    	
    	return outcome;
    }
}
